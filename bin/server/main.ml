(* Raw fraud-detection HTTP server.

   No framework: a single-threaded epoll event loop, fixed per-connection
   buffers, pipelined HTTP/1.1 parsing, pre-rendered responses, and the mmap'd
   exact KD index (Fraud.Knn). The hot path allocates almost nothing, so under
   the 0.45-CPU cgroup cap it stays under quota and avoids the CFS throttling
   stalls that gave the old httpaf+Lwt server a ~45ms p99.

   epoll (not Unix.select) is used because select rebuilds fd_sets and
   allocates lists every call, capping single-core throughput.

   Topology: each worker binds :9999 with SO_REUSEPORT; run N workers (fork) so
   the kernel load-balances connections across them with no proxy hop. *)

module K = Fraud.Knn
module E = Fraud.Epoll

let dim = K.dim

(* ---- config ---- *)
let port = ref 9999
let index_path = ref "/app/index.bin"
let socket_path = ref ""
let fd_uds = ref ""          (* SEQPACKET unix socket: receive client fds from the LB *)
let reuseport = ref true
let workers = ref 1
let warmup = ref 0
let spin_us = ref 0          (* busy-poll: spin epoll_wait(0) up to this many µs before blocking *)

(* ---- pre-rendered responses ---- *)
let http_resp body =
  Bytes.of_string
    (Printf.sprintf
       "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
       (String.length body) body)

let fraud_responses =
  Array.init 6 (fun c ->
    let approved = if c < 3 then "true" else "false" in
    let score = [| "0"; "0.2"; "0.4"; "0.6"; "0.8"; "1" |].(c) in
    http_resp (Printf.sprintf "{\"approved\":%s,\"fraud_score\":%s}" approved score))

let ready_resp =
  Bytes.of_string "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 2\r\n\r\nok"

let bad_resp =
  Bytes.of_string "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"

(* ---- connection state ---- *)
let buf_cap = 16384

(* fd -> conn lookup is a flat array indexed by the raw fd int (kernel hands out
   the lowest free fd, so values stay small under the rinha load + fd limits).
   This kills the per-request [Hashtbl.find_opt] allocation of a fresh [Some]. *)
let max_fds = 65536

type conn = {
  fd : Unix.file_descr;
  ifd : int;
  inbuf : Bytes.t;
  mutable inlen : int;
  mutable outbuf : Bytes.t;
  mutable outpos : int;
  mutable outlen : int;
  mutable want_write : bool;
}

let new_conn fd ifd = {
  fd; ifd;
  inbuf = Bytes.create buf_cap;
  inlen = 0;
  outbuf = Bytes.create 4096;
  outpos = 0;
  outlen = 0;
  want_write = false;
}

let[@inline] lc c = if c >= 'A' && c <= 'Z' then Char.chr (Char.code c + 32) else c

(* ---- per-worker serve loop ---- *)
let serve () =
  let idx = K.load ~path:!index_path in
  Printf.eprintf "[server] loaded index n=%d nodes=%d\n%!"
    idx.K.n (Array.length idx.K.node_left);
  let scratch = K.create_scratch () in
  let q = Array.make dim 0 in

  if !warmup > 0 then begin
    let n = idx.K.n in
    for i = 0 to !warmup - 1 do
      let src = (i * 2654435761) land max_int mod (max 1 n) in
      for d = 0 to dim - 1 do
        q.(d) <- Bigarray.Array1.unsafe_get idx.K.vecs (src * dim + d)
      done;
      ignore (K.fraud_count_with scratch idx q ~exact:false)
    done;
    Printf.eprintf "[server] warmed up %d queries\n%!" !warmup
  end;

  let fd_pass = !fd_uds <> "" in
  let listen_fd, label =
    if fd_pass then begin
      (* SEQPACKET unix socket: the LB connects and passes client fds over it *)
      (try Unix.unlink !fd_uds with _ -> ());
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_SEQPACKET 0 in
      Unix.bind fd (Unix.ADDR_UNIX !fd_uds);
      Unix.listen fd 8;
      (try Unix.chmod !fd_uds 0o666 with _ -> ());
      fd, "fd-uds:" ^ !fd_uds
    end
    else if !socket_path <> "" then begin
      (try Unix.unlink !socket_path with _ -> ());
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Unix.bind fd (Unix.ADDR_UNIX !socket_path);
      Unix.listen fd 1024;
      (try Unix.chmod !socket_path 0o666 with _ -> ());
      fd, "unix:" ^ !socket_path
    end else begin
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      if !reuseport then (try Unix.setsockopt fd Unix.SO_REUSEPORT true with _ -> ());
      Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, !port));
      Unix.listen fd 1024;
      fd, Printf.sprintf "tcp::%d" !port
    end
  in
  Unix.set_nonblock listen_fd;
  let lfd_int = E.int_of_fd listen_fd in
  let epfd = E.create1 () in
  E.add epfd lfd_int E.in_;
  Printf.eprintf "[server] listening on %s (epoll)\n%!" label;

  let conns : conn option array = Array.make max_fds None in
  let ctrls : bool array = Array.make max_fds false in

  let close_conn c =
    (try E.del epfd c.ifd with _ -> ());
    (try Unix.close c.fd with _ -> ());
    Array.unsafe_set conns c.ifd None
  in

  let ensure_out c need =
    let cap = Bytes.length c.outbuf in
    if c.outlen + need > cap then begin
      let ncap = ref (cap * 2) in
      while c.outlen + need > !ncap do ncap := !ncap * 2 done;
      let nb = Bytes.create !ncap in
      Bytes.blit c.outbuf 0 nb 0 c.outlen;
      c.outbuf <- nb
    end
  in
  let append_resp c (r : Bytes.t) =
    let len = Bytes.length r in
    ensure_out c len;
    Bytes.blit r 0 c.outbuf c.outlen len;
    c.outlen <- c.outlen + len
  in

  (* flush pending output; returns false if the conn died *)
  let flush c =
    let alive = ref true and again = ref true in
    while !again && c.outpos < c.outlen do
      match Unix.write c.fd c.outbuf c.outpos (c.outlen - c.outpos) with
      | 0 -> again := false
      | n -> c.outpos <- c.outpos + n
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> again := false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | exception _ -> alive := false; again := false
    done;
    if not !alive then false
    else begin
      if c.outpos >= c.outlen then begin
        c.outpos <- 0; c.outlen <- 0;
        if c.want_write then (c.want_write <- false; (try E.modify epfd c.ifd E.in_ with _ -> ()))
      end else if not c.want_write then
        (c.want_write <- true; (try E.modify epfd c.ifd (E.in_ lor E.out) with _ -> ()));
      true
    end
  in

  (* handle one request at inbuf[start..]; consumed bytes, or -1 need, -2 close *)
  let handle_request c start =
    let buf = c.inbuf in
    let len = c.inlen - start in
    let sub off = start + off in
    let he =
      let i = ref (sub 3) and res = ref (-1) in
      while !res < 0 && !i < c.inlen do
        if Bytes.unsafe_get buf !i = '\n'
           && Bytes.unsafe_get buf (!i - 1) = '\r'
           && Bytes.unsafe_get buf (!i - 2) = '\n'
           && Bytes.unsafe_get buf (!i - 3) = '\r'
        then res := !i + 1 else incr i
      done;
      !res
    in
    if he < 0 then (if len > buf_cap - 1 then -2 else -1)
    else begin
      let get o = Bytes.unsafe_get buf (sub o) in
      if get 0 = 'G' then (append_resp c ready_resp; he - start)
      else if get 0 = 'P' then begin
        let cl =
          let needle = "content-length:" in
          let nl = String.length needle in
          let i = ref start and found = ref (-1) in
          while !found < 0 && !i + nl <= he do
            let m = ref true and j = ref 0 in
            while !m && !j < nl do
              if lc (Bytes.unsafe_get buf (!i + !j)) <> String.unsafe_get needle !j then m := false;
              incr j
            done;
            if !m then begin
              let p = ref (!i + nl) in
              while !p < he && (Bytes.unsafe_get buf !p = ' ' || Bytes.unsafe_get buf !p = '\t') do incr p done;
              let v = ref 0 and any = ref false in
              while !p < he && (let ch = Bytes.unsafe_get buf !p in ch >= '0' && ch <= '9') do
                v := !v * 10 + (Char.code (Bytes.unsafe_get buf !p) - Char.code '0'); any := true; incr p
              done;
              found := if !any then !v else 0
            end;
            incr i
          done;
          !found
        in
        if cl < 0 then (append_resp c bad_resp; he - start)
        else begin
          let body_end = he + cl in
          if c.inlen < body_end then -1
          else begin
            (* zero-alloc: parse the body slice in place into the reused q *)
            Fraud.Detect.vectorize_q_into (Bytes.unsafe_to_string buf) he q;
            let count = K.fraud_count_with scratch idx q ~exact:false in
            append_resp c (Array.unsafe_get fraud_responses count);
            body_end - start
          end
        end
      end
      else (append_resp c bad_resp; he - start)
    end
  in

  let handle_readable c =
    let alive = ref true and again = ref true in
    (* Drain the socket. A short read (n < space) means the kernel buffer is
       empty, so we stop without the extra confirm-read that would just raise
       EAGAIN — saving one syscall + one exception alloc per request. Level-
       triggered epoll re-fires if more data arrives later. *)
    while !again do
      let space = buf_cap - c.inlen in
      match Unix.read c.fd c.inbuf c.inlen space with
      | 0 -> alive := false; again := false
      | n -> c.inlen <- c.inlen + n;
             if n < space || c.inlen >= buf_cap then again := false
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> again := false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | exception _ -> alive := false; again := false
    done;
    if not !alive && c.inlen = 0 then close_conn c
    else begin
      let pos = ref 0 and stop = ref false and dead = ref false in
      while not !stop && !pos < c.inlen do
        let consumed = handle_request c !pos in
        if consumed = -1 then stop := true
        else if consumed = -2 then (dead := true; stop := true)
        else pos := !pos + consumed
      done;
      if !dead then close_conn c
      else begin
        if !pos > 0 then begin
          let rem = c.inlen - !pos in
          if rem > 0 then Bytes.blit c.inbuf !pos c.inbuf 0 rem;
          c.inlen <- rem
        end;
        if not (flush c) then close_conn c
        else if not !alive then close_conn c   (* peer closed after final req *)
      end
    end
  in

  (* [configured]=true means the fd already carries O_NONBLOCK + TCP_NODELAY.
     Fds passed by the LB do: the LB accept4()s with SOCK_NONBLOCK and sets
     TCP_NODELAY, and both survive SCM_RIGHTS — O_NONBLOCK lives on the shared
     open file description, TCP_NODELAY on the shared socket — so re-setting them
     here is two dead syscalls per accepted connection. Only the direct-accept
     path (TCP/unix) hands us a raw fd that still needs configuring. *)
  let register_client ?(configured = false) fd =
    if not configured then begin
      Unix.set_nonblock fd;
      (try Unix.setsockopt fd Unix.TCP_NODELAY true with _ -> ())
    end;
    let ifd = E.int_of_fd fd in
    if ifd >= max_fds then (try Unix.close fd with _ -> ())
    else
      (* Register with epoll FIRST; only record the conn if that succeeds, so a
         failed epoll_ctl never leaves a live fd in [conns] with no waiter. *)
      match (try E.add epfd ifd E.in_; true with _ -> false) with
      | true -> Array.unsafe_set conns ifd (Some (new_conn fd ifd))
      | false -> (try Unix.close fd with _ -> ())
  in

  (* TCP / unix-stream mode: accept client connections directly. *)
  let accept_all () =
    let again = ref true in
    while !again do
      match Unix.accept ~cloexec:true listen_fd with
      | (fd, _) -> register_client fd
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> again := false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | exception _ -> again := false
    done
  in

  (* fd-pass mode: accept the LB's SEQPACKET control connection(s). *)
  let accept_ctrls () =
    let again = ref true in
    while !again do
      match Unix.accept ~cloexec:true listen_fd with
      | (fd, _) ->
        Unix.set_nonblock fd;
        let ifd = E.int_of_fd fd in
        if ifd >= max_fds then (try Unix.close fd with _ -> ())
        else
          (* epoll-add before marking the ctrl slot, same ordering as clients *)
          (match (try E.add epfd ifd E.in_; true with _ -> false) with
           | true -> Array.unsafe_set ctrls ifd true
           | false -> (try Unix.close fd with _ -> ()))
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> again := false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | exception _ -> again := false
    done
  in

  (* fd-pass mode: drain passed client fds off a control connection. *)
  let recv_clients ctrl_ifd =
    let again = ref true in
    while !again do
      let r = E.recv_fd ctrl_ifd in
      if r >= 0 then register_client ~configured:true (E.fd_of_int r)
      else if r = -1 then again := false
      else begin
        (try E.del epfd ctrl_ifd with _ -> ());
        (try Unix.close (E.fd_of_int ctrl_ifd) with _ -> ());
        Array.unsafe_set ctrls ctrl_ifd false;
        again := false
      end
    done
  in

  let maxev = 1024 in
  let out_fds = Array.make maxev 0 and out_evs = Array.make maxev 0 in
  let spin = !spin_us in
  while true do
    let n = E.wait_spin epfd out_fds out_evs spin in
    for i = 0 to n - 1 do
      let fd = Array.unsafe_get out_fds i in
      let ev = Array.unsafe_get out_evs i in
      if fd = lfd_int then (if fd_pass then accept_ctrls () else accept_all ())
      else if fd_pass && Array.unsafe_get ctrls fd then recv_clients fd
      else match Array.unsafe_get conns fd with
        | None -> ()
        | Some c ->
          if ev land (E.err lor E.hup) <> 0 && ev land E.in_ = 0 then close_conn c
          else begin
            if ev land E.in_ <> 0 then handle_readable c;
            if ev land E.out <> 0 then
              (match Array.unsafe_get conns fd with
               | Some c -> if not (flush c) then close_conn c
               | None -> ())  (* handle_readable may have closed it *)
          end
    done
  done

(* ---- entry ---- *)
let main () =
  let speclist = [
    "--port", Arg.Set_int port, "TCP port (default 9999)";
    "--socket", Arg.Set_string socket_path, "unix socket path (HTTP over unix, overrides TCP)";
    "--fd-uds", Arg.Set_string fd_uds, "SEQPACKET unix socket: receive client fds from the LB";
    "--index", Arg.Set_string index_path, "path to index.bin";
    "--no-reuseport", Arg.Clear reuseport, "disable SO_REUSEPORT";
    "--workers", Arg.Set_int workers, "SO_REUSEPORT worker processes (default 1)";
    "--warmup", Arg.Set_int warmup, "warmup query count before serving";
    "--spin-us", Arg.Set_int spin_us, "busy-poll spin budget in µs before blocking (default 0 = off)";
  ] in
  Arg.parse speclist (fun _ -> ()) "fraud-server";

  (match Sys.getenv_opt "INDEX_PATH" with Some p when p <> "" -> index_path := p | _ -> ());
  (match Sys.getenv_opt "FD_UDS" with Some p when p <> "" -> fd_uds := p | _ -> ());
  (match Sys.getenv_opt "API_WORKERS" with Some w -> (try workers := int_of_string w with _ -> ()) | _ -> ());
  (match Sys.getenv_opt "API_WARMUP_QUERIES" with Some w -> (try warmup := int_of_string w with _ -> ()) | _ -> ());
  (match Sys.getenv_opt "EPOLL_SPIN_US" with Some w -> (try spin_us := int_of_string w with _ -> ()) | _ -> ());

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  let n = max 1 !workers in
  for _ = 2 to n do
    match Unix.fork () with
    | 0 -> serve ()
    | _ -> ()
  done;
  serve ()

let () = main ()
