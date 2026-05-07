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

let test_save_load_header_roundtrip () =
  with_tmp_file (fun path ->
    let n = 1000 and c = 8 and dim = 14 in
    let header = Index_io.plan_layout ~n ~c ~dim ~nprobe_default:4 in
    Index_io.save ~path ~header
      ~centroids:(mk_centroids ~c ~dim)
      ~cell_offsets:(mk_offsets ~c ~n)
      ~vecs:(mk_vecs ~n ~dim)
      ~labels:(mk_labels ~n);
    let h2 = Index_io.load_header path in
    Alcotest.(check int) "n" header.n h2.n;
    Alcotest.(check int) "c" header.c h2.c;
    Alcotest.(check int) "dim" header.dim h2.dim;
    Alcotest.(check int) "nprobe_default" header.nprobe_default h2.nprobe_default;
    Alcotest.(check int) "centroids_off" header.centroids_off h2.centroids_off;
    Alcotest.(check int) "cell_offsets_off" header.cell_offsets_off h2.cell_offsets_off;
    Alcotest.(check int) "vecs_off" header.vecs_off h2.vecs_off;
    Alcotest.(check int) "labels_off" header.labels_off h2.labels_off;
    Alcotest.(check int) "file_size" header.file_size h2.file_size)

let test_load_header_bad_magic () =
  with_tmp_file (fun path ->
    let oc = Out_channel.open_bin path in
    Out_channel.output_string oc (String.make 4096 '\000');
    Out_channel.close oc;
    Alcotest.check_raises "bad magic raises"
      (Failure "Index_io.load_header: bad magic")
      (fun () -> ignore (Index_io.load_header path)))

let test_load_mmap_roundtrip () =
  with_tmp_file (fun path ->
    let n = 1000 and c = 8 and dim = 14 in
    let header = Index_io.plan_layout ~n ~c ~dim ~nprobe_default:4 in
    let centroids = mk_centroids ~c ~dim in
    let offs      = mk_offsets ~c ~n in
    let vecs      = mk_vecs ~n ~dim in
    let lbls      = mk_labels ~n in
    Index_io.save ~path ~header ~centroids
      ~cell_offsets:offs ~vecs ~labels:lbls;
    let h2, views = Index_io.load_mmap path in
    Alcotest.(check int) "header n" n h2.n;
    (* Spot-check a few centroid values *)
    Alcotest.(check (float 1e-6)) "centroid[0]"
      (Bigarray.Array1.get centroids 0)
      (Bigarray.Array1.get views.centroids 0);
    Alcotest.(check (float 1e-6)) "centroid[c*dim-1]"
      (Bigarray.Array1.get centroids (c * dim - 1))
      (Bigarray.Array1.get views.centroids (c * dim - 1));
    (* Spot-check vecs *)
    Alcotest.(check (float 1e-6)) "vec[0]"
      (Bigarray.Array1.get vecs 0)
      (Bigarray.Array1.get views.vecs 0);
    Alcotest.(check (float 1e-6)) "vec[n*dim-1]"
      (Bigarray.Array1.get vecs (n * dim - 1))
      (Bigarray.Array1.get views.vecs (n * dim - 1));
    (* Cell offsets *)
    Alcotest.(check int64) "offs[0]"
      (Bigarray.Array1.get offs 0)
      (Bigarray.Array1.get views.cell_offsets 0);
    Alcotest.(check int64) "offs[c]"
      (Bigarray.Array1.get offs c)
      (Bigarray.Array1.get views.cell_offsets c);
    (* Labels *)
    Alcotest.(check char) "lbl[0]"
      (Bytes.get lbls 0)
      (Bigarray.Array1.get views.labels 0);
    Alcotest.(check char) "lbl[n-1]"
      (Bytes.get lbls (n - 1))
      (Bigarray.Array1.get views.labels (n - 1));
    Unix.close views.fd)

let test_build_produces_sorted_layout () =
  let n = 256 and dim = 14 in
  let vs = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * dim) in
  let st = Random.State.make [| 99 |] in
  for i = 0 to n * dim - 1 do
    Bigarray.Array1.set vs i (Random.State.float st 1.0)
  done;
  let lbls = Bytes.make n '\000' in
  let idx = Index.build ~c:4 ~iters:3 ~sample:n vs n lbls in
  Alcotest.(check int) "n" n idx.n;
  Alcotest.(check int) "c" 4 idx.c;
  Alcotest.(check int) "cell_offsets length" 5 (Bigarray.Array1.dim idx.cell_offsets);
  Alcotest.(check int64) "first offset = 0" 0L (Bigarray.Array1.get idx.cell_offsets 0);
  Alcotest.(check int64) "last offset = n" (Int64.of_int n) (Bigarray.Array1.get idx.cell_offsets 4)

let () =
  Alcotest.run "index_io" [
    "layout", [
      Alcotest.test_case "plan_layout 3M/1024/14" `Quick test_plan_layout_basic;
    ];
    "save", [
      Alcotest.test_case "save writes correct size" `Quick test_save_writes_correct_size;
    ];
    "load_header", [
      Alcotest.test_case "save/load round-trip" `Quick test_save_load_header_roundtrip;
      Alcotest.test_case "rejects bad magic"     `Quick test_load_header_bad_magic;
    ];
    "load_mmap", [
      Alcotest.test_case "round-trip mmap views" `Quick test_load_mmap_roundtrip;
    ];
    "index", [
      Alcotest.test_case "build produces sorted cell-major layout"
        `Quick test_build_produces_sorted_layout;
    ];
  ]
