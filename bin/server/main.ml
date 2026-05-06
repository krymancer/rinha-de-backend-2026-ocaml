(* Fraud detection HTTP server.
   Currently builds synthetic 3M-vec IVF index at startup (real data wiring next). *)

open Lwt.Infix

let n = 3_000_000
let dim = 14
let nprobe = 8
let port = ref 9999
let n_arg = ref n

let build_synth_index () : Fraud.Index.t =
  let t0 = Unix.gettimeofday () in
  Printf.printf "building synth %d vecs ...\n%!" !n_arg;
  let vs = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (!n_arg * dim) in
  let st = Random.State.make [| 42 |] in
  for i = 0 to !n_arg * dim - 1 do
    Bigarray.Array1.unsafe_set vs i (Random.State.float st 1.0)
  done;
  let labels = Bytes.create !n_arg in
  let st2 = Random.State.make [| 7 |] in
  for i = 0 to !n_arg - 1 do
    Bytes.unsafe_set labels i
      (if Random.State.float st2 1.0 < 0.05 then '\001' else '\000')
  done;
  Printf.printf "data ready in %.2fs, building IVF ...\n%!" (Unix.gettimeofday () -. t0);
  let t1 = Unix.gettimeofday () in
  let idx = Fraud.Index.build vs !n_arg labels in
  Printf.printf "IVF built in %.2fs (total %.2fs)\n%!"
    (Unix.gettimeofday () -. t1) (Unix.gettimeofday () -. t0);
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
    "--port", Arg.Set_int port, "port (default 9999)";
    "--n", Arg.Set_int n_arg, "synth vec count (default 3M)";
  ] in
  Arg.parse speclist (fun _ -> ()) "fraud-server";

  let index = build_synth_index () in

  let listen_addr = Unix.(ADDR_INET (inet_addr_any, !port)) in
  let connection_handler =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler index)
      ~error_handler
  in
  Lwt_main.run begin
    Lwt_io.establish_server_with_client_socket listen_addr connection_handler
    >>= fun _server ->
    Printf.printf "listening on :%d\n%!" !port;
    fst (Lwt.wait ())
  end

let () = main ()
