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

let () =
  Alcotest.run "index_io" [
    "layout", [
      Alcotest.test_case "plan_layout 3M/1024/14" `Quick test_plan_layout_basic;
    ];
  ]
