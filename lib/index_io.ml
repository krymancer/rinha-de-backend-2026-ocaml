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

let load_header _path : header = failwith "Index_io.load_header: not yet implemented"

type mmap_views = {
  fd : Unix.file_descr;
  centroids    : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  cell_offsets : (int64, Bigarray.int64_elt,  Bigarray.c_layout) Bigarray.Array1.t;
  vecs         : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  labels       : (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let load_mmap _path : header * mmap_views =
  failwith "Index_io.load_mmap: not yet implemented"
