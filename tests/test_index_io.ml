open Fraud

let test_plan_layout_basic () =
  let h = Index_io.plan_layout ~n:3_000_000 ~c:1024 ~dim:14 ~nprobe_default:8 in
  Alcotest.(check int) "n" 3_000_000 h.n;
  Alcotest.(check int) "c" 1024 h.c;
  Alcotest.(check int) "dim" 14 h.dim;
  Alcotest.(check int) "nprobe_default" 8 h.nprobe_default;
  Alcotest.(check int) "centroids_off is page-aligned" 0 (h.centroids_off mod 4096);
  Alcotest.(check int) "vecs_off is page-aligned" 0 (h.vecs_off mod 4096);
  Alcotest.(check bool) "centroids before cell_offsets"
    true (h.centroids_off < h.cell_offsets_off);
  Alcotest.(check bool) "cell_offsets before vecs"
    true (h.cell_offsets_off < h.vecs_off);
  Alcotest.(check bool) "vecs before labels"
    true (h.vecs_off < h.labels_off);
  Alcotest.(check int) "file_size matches labels segment end"
    (h.labels_off + h.n) h.file_size

let with_tmp_file f =
  let path = Filename.temp_file "idx_" ".bin" in
  let r =
    try f path
    with e -> (try Sys.remove path with _ -> ()); raise e
  in
  (try Sys.remove path with _ -> ());
  r

let mk_centroids ~c ~dim =
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (c * dim) in
  for i = 0 to c * dim - 1 do
    Bigarray.Array1.set ba i (float_of_int i *. 0.001)
  done;
  ba

let mk_offsets ~c ~n =
  let ba = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout (c + 1) in
  let per = n / c in
  for i = 0 to c - 1 do
    Bigarray.Array1.set ba i (Int64.of_int (i * per))
  done;
  Bigarray.Array1.set ba c (Int64.of_int n);
  ba

let mk_vecs ~n ~dim =
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * dim) in
  for i = 0 to n * dim - 1 do
    Bigarray.Array1.set ba i (float_of_int i *. 0.0001)
  done;
  ba

let mk_labels ~n =
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set b i (if i mod 7 = 0 then '\001' else '\000')
  done;
  b

let test_save_writes_correct_size () =
  with_tmp_file (fun path ->
    let n = 1000 and c = 8 and dim = 14 in
    let header = Index_io.plan_layout ~n ~c ~dim ~nprobe_default:4 in
    let centroids    = mk_centroids ~c ~dim in
    let cell_offsets = mk_offsets ~c ~n in
    let vecs         = mk_vecs ~n ~dim in
    let labels       = mk_labels ~n in
    Index_io.save ~path ~header ~centroids ~cell_offsets ~vecs ~labels;
    let st = Unix.stat path in
    Alcotest.(check int) "file size matches header.file_size"
      header.file_size st.st_size)

let () =
  Alcotest.run "index_io" [
    "layout", [
      Alcotest.test_case "plan_layout 3M/1024/14" `Quick test_plan_layout_basic;
    ];
    "save", [
      Alcotest.test_case "save writes correct size" `Quick test_save_writes_correct_size;
    ];
  ]
