(* Stream-parse the rinha references.json format:
   [ {"vector":[f0,...,f13],"label":"fraud"|"legit"}, ... ]

   We do not build the full Yojson tree in memory.  Instead we scan raw
   bytes: skip whitespace / outer '[' / ',' / ']', then find one record
   substring delimited by matching '{' '}', and feed that to Yojson.Safe.
   Each record is small (<200 bytes), so per-record parse is cheap. *)

type label = [ `Fraud | `Legit ]

type source =
  | File of string
  | Channel of In_channel.t
  | Stdin

let source_to_chan = function
  | File path -> In_channel.open_text path
  | Channel ic -> ic
  | Stdin -> In_channel.stdin

module Buf = struct
  type t = {
    ic : In_channel.t;
    mutable data : Bytes.t;
    mutable pos : int;
    mutable len : int;
    mutable eof : bool;
  }
  let make ic = { ic; data = Bytes.create 65536; pos = 0; len = 0; eof = false }
  let refill b =
    if not b.eof then begin
      let n = In_channel.input b.ic b.data 0 (Bytes.length b.data) in
      b.pos <- 0; b.len <- n;
      if n = 0 then b.eof <- true
    end
  let peek b =
    if b.pos >= b.len then refill b;
    if b.eof then None else Some (Bytes.unsafe_get b.data b.pos)
  let advance b = b.pos <- b.pos + 1
end

let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let skip_ws_and b sep =
  let rec loop () =
    match Buf.peek b with
    | None -> ()
    | Some c when is_ws c || c = sep -> Buf.advance b; loop ()
    | Some _ -> ()
  in
  loop ()

(* Read one balanced JSON object beginning at peek='{'. String literals
   handled so braces inside them don't confuse the depth counter. *)
let read_object b =
  (match Buf.peek b with
   | Some '{' -> ()
   | _ -> failwith "Refs_reader: expected '{'");
  let out = Buffer.create 200 in
  Buffer.add_char out '{';
  Buf.advance b;
  let depth = ref 1 in
  let in_str = ref false in
  let escape = ref false in
  while !depth > 0 do
    match Buf.peek b with
    | None -> failwith "Refs_reader: EOF inside record"
    | Some c ->
      Buffer.add_char out c;
      Buf.advance b;
      if !escape then escape := false
      else if !in_str then begin
        if c = '\\' then escape := true
        else if c = '"' then in_str := false
      end
      else begin
        if c = '"' then in_str := true
        else if c = '{' then incr depth
        else if c = '}' then decr depth
      end
  done;
  Buffer.contents out

let parse_record s : float array * label =
  let j = Yojson.Safe.from_string s in
  let vec_j = Detect.field j "vector" in
  let label_j = Detect.field j "label" in
  let label =
    match Detect.to_string label_j with
    | "fraud" -> `Fraud
    | "legit" -> `Legit
    | other -> failwith (Printf.sprintf "Refs_reader: unknown label %s" other)
  in
  let xs = Detect.to_list vec_j in
  if List.length xs <> 14 then
    failwith (Printf.sprintf "Refs_reader: expected 14 dims, got %d" (List.length xs));
  let arr = Array.make 14 0.0 in
  List.iteri (fun i v -> arr.(i) <- Detect.to_float v) xs;
  arr, label

let fold (f : 'a -> float array * label -> 'a) (acc0 : 'a) (src : source) : 'a =
  let ic = source_to_chan src in
  let b = Buf.make ic in
  let close () =
    match src with
    | File _ -> In_channel.close ic
    | Channel _ | Stdin -> ()
  in
  Fun.protect ~finally:close (fun () ->
    skip_ws_and b ' ';
    (match Buf.peek b with
     | Some '[' -> Buf.advance b
     | _ -> failwith "Refs_reader: expected '[' at start");
    let acc = ref acc0 in
    let rec loop () =
      skip_ws_and b ',';
      match Buf.peek b with
      | None -> failwith "Refs_reader: EOF before ']'"
      | Some ']' -> Buf.advance b
      | Some '{' ->
        let s = read_object b in
        let rec_ = parse_record s in
        acc := f !acc rec_;
        loop ()
      | Some c -> failwith (Printf.sprintf "Refs_reader: unexpected char %C" c)
    in
    loop ();
    !acc)
