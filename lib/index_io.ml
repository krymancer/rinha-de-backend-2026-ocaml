(* Binary format and mmap helpers for index.bin. *)

let magic : int32 = 0x49564631l   (* "IVF1" little-endian *)
let version : int32 = 1l
let header_size = 4096
let page_size = 4096

type header = {
  n : int;
  c : int;
  dim : int;
  nprobe_default : int;
  centroids_off : int;
  cell_offsets_off : int;
  vecs_off : int;
  labels_off : int;
  file_size : int;
}

let[@inline] align_up x a = (x + a - 1) / a * a

let plan_layout ~n ~c ~dim ~nprobe_default =
  let centroids_off = header_size in
  let cell_offsets_off = centroids_off + c * dim * 4 in
  let vecs_off = align_up (cell_offsets_off + (c + 1) * 8) page_size in
  let labels_off = vecs_off + n * dim * 4 in
  let file_size = labels_off + n in
  { n; c; dim; nprobe_default;
    centroids_off; cell_offsets_off; vecs_off; labels_off; file_size }

let write_u32_le oc v =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 v;
  Out_channel.output_bytes oc b

let write_u64_le oc v =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int v);
  Out_channel.output_bytes oc b

let write_zeros oc n =
  let chunk = Bytes.make 4096 '\000' in
  let remaining = ref n in
  while !remaining >= 4096 do
    Out_channel.output_bytes oc chunk;
    remaining := !remaining - 4096
  done;
  if !remaining > 0 then
    Out_channel.output_bytes oc (Bytes.sub chunk 0 !remaining)

(* Write a Bigarray.float32 Array1 as raw little-endian bytes. *)
let write_f32_array oc (ba : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t) =
  let n = Bigarray.Array1.dim ba in
  let buf = Bytes.create 4 in
  for i = 0 to n - 1 do
    let bits = Int32.bits_of_float (Bigarray.Array1.unsafe_get ba i) in
    Bytes.set_int32_le buf 0 bits;
    Out_channel.output_bytes oc buf
  done

let write_i64_array oc (ba : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t) =
  let n = Bigarray.Array1.dim ba in
  let buf = Bytes.create 8 in
  for i = 0 to n - 1 do
    Bytes.set_int64_le buf 0 (Bigarray.Array1.unsafe_get ba i);
    Out_channel.output_bytes oc buf
  done

let save ~path ~header ~centroids ~cell_offsets ~vecs ~labels =
  let oc = Out_channel.open_bin path in
  let close () = Out_channel.close oc in
  Fun.protect ~finally:close (fun () ->
    (* Header *)
    write_u32_le oc magic;
    write_u32_le oc version;
    write_u64_le oc header.n;
    write_u32_le oc (Int32.of_int header.c);
    write_u32_le oc (Int32.of_int header.dim);
    write_u32_le oc (Int32.of_int header.nprobe_default);
    write_u32_le oc 0l;                              (* pad *)
    write_u64_le oc header.centroids_off;
    write_u64_le oc header.cell_offsets_off;
    write_u64_le oc header.vecs_off;
    write_u64_le oc header.labels_off;
    write_u64_le oc header.file_size;
    let written = 4 + 4 + 8 + 4 + 4 + 4 + 4 + 8 * 5 in
    write_zeros oc (header_size - written);

    (* Centroids — already at centroids_off = header_size *)
    write_f32_array oc centroids;
    let pos = header.centroids_off + header.c * header.dim * 4 in
    assert (pos = header.cell_offsets_off);

    (* Cell offsets *)
    write_i64_array oc cell_offsets;
    let pos = pos + (header.c + 1) * 8 in
    write_zeros oc (header.vecs_off - pos);

    (* Vecs *)
    write_f32_array oc vecs;
    let pos = header.vecs_off + header.n * header.dim * 4 in
    assert (pos = header.labels_off);

    (* Labels *)
    Out_channel.output_bytes oc (Bytes.sub labels 0 header.n))

let read_exact ic n =
  let b = Bytes.create n in
  really_input ic b 0 n;
  b

let load_header path =
  let ic = In_channel.open_bin path in
  let close () = In_channel.close ic in
  Fun.protect ~finally:close (fun () ->
    let h = read_exact ic header_size in
    let m = Bytes.get_int32_le h 0 in
    if m <> magic then failwith "Index_io.load_header: bad magic";
    let v = Bytes.get_int32_le h 4 in
    if v <> version then failwith "Index_io.load_header: bad version";
    let n              = Int64.to_int (Bytes.get_int64_le h 8) in
    let c              = Int32.to_int (Bytes.get_int32_le h 16) in
    let dim            = Int32.to_int (Bytes.get_int32_le h 20) in
    let nprobe_default = Int32.to_int (Bytes.get_int32_le h 24) in
    (* skip pad u32 at offset 28 *)
    let centroids_off    = Int64.to_int (Bytes.get_int64_le h 32) in
    let cell_offsets_off = Int64.to_int (Bytes.get_int64_le h 40) in
    let vecs_off         = Int64.to_int (Bytes.get_int64_le h 48) in
    let labels_off       = Int64.to_int (Bytes.get_int64_le h 56) in
    let file_size        = Int64.to_int (Bytes.get_int64_le h 64) in
    { n; c; dim; nprobe_default;
      centroids_off; cell_offsets_off; vecs_off; labels_off; file_size })

type mmap_views = {
  fd : Unix.file_descr;
  centroids    : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  cell_offsets : (int64, Bigarray.int64_elt,  Bigarray.c_layout) Bigarray.Array1.t;
  vecs         : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  labels       : (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let load_mmap path =
  let header = load_header path in
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let map_f32 ~pos ~len =
    Unix.map_file fd ~pos:(Int64.of_int pos)
      Bigarray.float32 Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  let map_i64 ~pos ~len =
    Unix.map_file fd ~pos:(Int64.of_int pos)
      Bigarray.int64 Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  let map_chr ~pos ~len =
    Unix.map_file fd ~pos:(Int64.of_int pos)
      Bigarray.char Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  let centroids    = map_f32 ~pos:header.centroids_off    ~len:(header.c * header.dim) in
  let cell_offsets = map_i64 ~pos:header.cell_offsets_off ~len:(header.c + 1) in
  let vecs         = map_f32 ~pos:header.vecs_off         ~len:(header.n * header.dim) in
  let labels       = map_chr ~pos:header.labels_off       ~len:header.n in
  header, { fd; centroids; cell_offsets; vecs; labels }
