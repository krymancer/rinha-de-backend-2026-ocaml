(* Fraud detection HTTP server — mmap'd index.bin. *)

open Lwt.Infix

let nprobe = 4
let port = ref 9999
let index_path = ref "/app/index.bin"

(* Touch every page of the four mmap'd Bigarrays so the kernel pulls them into
   page cache before we start serving. Without this the first ~thousand
   requests pay cold-fault penalties on test boxes with slow flash. *)
let prefault_index (v : Fraud.Index_io.mmap_views) =
  let touch_f32 (ba : (float, Bigarray.float32_elt, Bigarray.c_layout)
                       Bigarray.Array1.t) =
    let n = Bigarray.Array1.dim ba in
    let acc = ref 0.0 in
    let i = ref 0 in
    while !i < n do
      acc := !acc +. Bigarray.Array1.unsafe_get ba !i;
      i := !i + 1024
    done;
    !acc
  in
  let touch_chr (ba : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
                       Bigarray.Array1.t) =
    let n = Bigarray.Array1.dim ba in
    let acc = ref 0 in
    let i = ref 0 in
    while !i < n do
      acc := !acc lxor Char.code (Bigarray.Array1.unsafe_get ba !i);
      i := !i + 4096
    done;
    !acc
  in
  let _ = touch_f32 v.centroids in
  let _ = touch_f32 v.vecs in
  let _ = touch_chr v.labels in
  ()

let load_index path : Fraud.Index.t * Fraud.Index.scorer =
  let t0 = Unix.gettimeofday () in
  let h, v = Fraud.Index_io.load_mmap path in
  prefault_index v;
  let idx = Fraud.Index.of_segments
    ~vecs:v.vecs ~n:h.n ~labels:v.labels
    ~centroids:v.centroids ~c:h.c ~cell_offsets:v.cell_offsets in
  let scorer = Fraud.Index.create_scorer ~max_nprobe:nprobe in
  Printf.printf "[server] mmapped+prefaulted index n=%d c=%d in %.3fs from %s\n%!"
    h.n h.c (Unix.gettimeofday () -. t0) path;
  idx, scorer

let respond_string reqd ?(status = `OK) ?(content_type = "text/plain") body =
  let headers = Httpaf.Headers.of_list [
    "content-type", content_type;
    "content-length", string_of_int (String.length body);
  ] in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp body

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
        let json = Yojson.Safe.from_string (Buffer.contents buf) in
        let v = Fraud.Detect.vectorize json in
        let score = Fraud.Index.fraud_score_with scorer index v ~nprobe in
        let approved = score < 0.6 in
        let body =
          Printf.sprintf "{\"approved\":%s,\"fraud_score\":%g}"
            (if approved then "true" else "false") score
        in
        respond_string reqd ~content_type:"application/json" body
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

let main () =
  let speclist = [
    "--port",  Arg.Set_int port,           "port (default 9999)";
    "--index", Arg.Set_string index_path,  "path to index.bin (default /app/index.bin)";
  ] in
  Arg.parse speclist (fun _ -> ()) "fraud-server";

  let bundle = load_index !index_path in

  let listen_addr = Unix.(ADDR_INET (inet_addr_any, !port)) in
  let connection_handler =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler bundle)
      ~error_handler
  in
  Lwt_main.run begin
    Lwt_io.establish_server_with_client_socket listen_addr connection_handler
    >>= fun _server ->
    Printf.printf "[server] listening on :%d\n%!" !port;
    fst (Lwt.wait ())
  end

let () = main ()
