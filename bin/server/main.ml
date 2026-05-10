(* Fraud detection HTTP server — mmap'd index.bin. *)

open Lwt.Infix

let nprobe = 4
let port = ref 9999
let index_path = ref "/app/index.bin"
let socket_path = ref ""

let load_index path : Fraud.Index.t * Fraud.Index.scorer =
  let t0 = Unix.gettimeofday () in
  let h, v = Fraud.Index_io.load_mmap path in
  let idx = Fraud.Index.of_segments
    ~vecs:v.vecs ~n:h.n ~labels:v.labels
    ~centroids:v.centroids ~c:h.c ~cell_offsets:v.cell_offsets in
  let scorer = Fraud.Index.create_scorer ~max_nprobe:nprobe in
  Printf.printf "[server] mmapped index n=%d c=%d in %.3fs from %s\n%!"
    h.n h.c (Unix.gettimeofday () -. t0) path;
  idx, scorer

let respond_string reqd ?(status = `OK) ?(content_type = "text/plain") body =
  let headers = Httpaf.Headers.of_list [
    "content-type", content_type;
    "content-length", string_of_int (String.length body);
  ] in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp body

(* Pre-rendered response strings, indexed by fraud count 0..k_neighbors.
   fraud_score = frauds / 5, so the only six possible bodies are
   approved=true for 0/1/2 and approved=false for 3/4/5. Building these
   once at boot kills per-request String/Buffer allocation. *)
let fraud_response_for : Httpaf.Response.t array =
  let headers = Httpaf.Headers.of_list [
    "content-type", "application/json";
  ] in
  Array.init 6 (fun frauds ->
    let body = Printf.sprintf "{\"approved\":%s,\"fraud_score\":%g}"
      (if frauds < 3 then "true" else "false")
      (float_of_int frauds /. 5.0)
    in
    let h = Httpaf.Headers.add headers "content-length"
              (string_of_int (String.length body)) in
    Httpaf.Response.create ~headers:h `OK)

let fraud_response_body : string array =
  Array.init 6 (fun frauds ->
    Printf.sprintf "{\"approved\":%s,\"fraud_score\":%g}"
      (if frauds < 3 then "true" else "false")
      (float_of_int frauds /. 5.0))

let request_handler (index, scorer) _client_addr (reqd : Httpaf.Reqd.t) : unit =
  let req = Httpaf.Reqd.request reqd in
  match req.meth, req.target with
  | `GET, "/ready" ->
    respond_string reqd "ok"
  | `POST, "/fraud-score" ->
    let body_r = Httpaf.Reqd.request_body reqd in
    let buf = Buffer.create 1024 in
    let rec on_read bs ~off ~len =
      Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
      Httpaf.Body.schedule_read body_r ~on_read ~on_eof
    and on_eof () =
      try
        let v = Fraud.Detect.vectorize_str (Buffer.contents buf) in
        let score = Fraud.Index.fraud_score_with scorer index v ~nprobe in
        let frauds = int_of_float (score *. 5.0 +. 0.5) in
        let frauds = if frauds < 0 then 0 else if frauds > 5 then 5 else frauds in
        Httpaf.Reqd.respond_with_string reqd
          fraud_response_for.(frauds) fraud_response_body.(frauds)
      with e ->
        let msg = Printexc.to_string e in
        respond_string reqd ~status:`Bad_request ("error: " ^ msg)
    in
    Httpaf.Body.schedule_read body_r ~on_read ~on_eof
  | _ ->
    respond_string reqd ~status:`Not_found "not found"

let error_handler _client_addr ?request:_ _err start_response =
  let body = start_response Httpaf.Headers.empty in
  Httpaf.Body.write_string body "internal error";
  Httpaf.Body.close_writer body

(* Standalone client mode used as the docker healthcheck: connects to the
   given unix socket, sends GET /ready, exits 0 if response starts with
   "HTTP/1.1 200" else 1. Lives in the same binary so the runtime image
   doesn't need curl/socat. *)
let healthcheck_mode path =
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let ok =
    try
      Unix.connect fd (Unix.ADDR_UNIX path);
      let req = "GET /ready HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" in
      let _ = Unix.write_substring fd req 0 (String.length req) in
      let buf = Bytes.create 64 in
      let n = Unix.read fd buf 0 (Bytes.length buf) in
      n >= 12 && Bytes.sub_string buf 0 12 = "HTTP/1.1 200"
    with _ -> false
  in
  (try Unix.close fd with _ -> ());
  exit (if ok then 0 else 1)

let main () =
  let healthcheck = ref "" in
  let speclist = [
    "--port",   Arg.Set_int port,           "TCP port (default 9999, ignored if --socket given)";
    "--socket", Arg.Set_string socket_path, "Unix socket path (skips TCP listener)";
    "--index",  Arg.Set_string index_path,  "path to index.bin (default /app/index.bin)";
    "--healthcheck", Arg.Set_string healthcheck, "probe given unix socket and exit 0/1";
  ] in
  Arg.parse speclist (fun _ -> ()) "fraud-server";

  if !healthcheck <> "" then healthcheck_mode !healthcheck;

  let env_socket = try Sys.getenv "SOCKET_PATH" with Not_found -> "" in
  if !socket_path = "" && env_socket <> "" then socket_path := env_socket;

  let bundle = load_index !index_path in

  let listen_addr, listen_label =
    if !socket_path <> "" then begin
      (try Unix.unlink !socket_path with Unix.Unix_error _ -> ());
      Unix.(ADDR_UNIX !socket_path), "unix:" ^ !socket_path
    end else
      Unix.(ADDR_INET (inet_addr_any, !port)), Printf.sprintf "tcp::%d" !port
  in
  let inner_handler =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler bundle)
      ~error_handler
  in
  (* Disable Nagle on every accepted TCP connection. Unix sockets ignore
     TCP_NODELAY (setsockopt fails with ENOPROTOOPT) so we swallow the
     error. *)
  let connection_handler client_addr fd =
    (try
       Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
     with _ -> ());
    inner_handler client_addr fd
  in
  Lwt_main.run begin
    Lwt_io.establish_server_with_client_socket listen_addr connection_handler
    >>= fun _server ->
    if !socket_path <> "" then begin
      (* nginx in another container needs to read+write the socket. *)
      try Unix.chmod !socket_path 0o666 with _ -> ()
    end;
    Printf.printf "[server] listening on %s\n%!" listen_label;
    fst (Lwt.wait ())
  end

let () = main ()
