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

(* placeholders, filled in subsequent tasks *)
let save ~path:_ ~header:_ ~centroids:_ ~cell_offsets:_ ~vecs:_ ~labels:_ =
  failwith "Index_io.save: not yet implemented"

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
