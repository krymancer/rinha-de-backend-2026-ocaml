(* Fraud detection HTTP server — mmap'd index.bin. *)

open Lwt.Infix

let nprobe = 8
let port = ref 9999
let index_path = ref "/app/index.bin"

let load_index path : Fraud.Index.t =
  let t0 = Unix.gettimeofday () in
  let h, v = Fraud.Index_io.load_mmap path in
  let idx = Fraud.Index.of_segments
    ~vecs:v.vecs ~n:h.n ~labels:v.labels
    ~centroids:v.centroids ~c:h.c ~cell_offsets:v.cell_offsets in
  Printf.printf "[server] mmapped index n=%d c=%d in %.3fs from %s\n%!"
    h.n h.c (Unix.gettimeofday () -. t0) path;
  idx

let respond_string reqd ?(status = `OK) ?(content_type = "text/plain") body =
  let headers = Httpaf.Headers.of_list [
    "content-type", content_type;
    "content-length", string_of_int (String.length body);
  ] in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp body

let request_handler index _client_addr (reqd : Httpaf.Reqd.t) : unit =
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
        let score = Fraud.Index.fraud_score index v ~nprobe in
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

  let index = load_index !index_path in

  let listen_addr = Unix.(ADDR_INET (inet_addr_any, !port)) in
  let connection_handler =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler index)
      ~error_handler
  in
  Lwt_main.run begin
    Lwt_io.establish_server_with_client_socket listen_addr connection_handler
    >>= fun _server ->
    Printf.printf "[server] listening on :%d\n%!" !port;
    fst (Lwt.wait ())
  end

let () = main ()
