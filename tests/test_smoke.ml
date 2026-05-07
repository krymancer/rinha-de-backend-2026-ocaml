let test_alcotest_runs () =
  Alcotest.(check int) "trivial" 1 1

let () =
  Alcotest.run "smoke" [
    "smoke", [
      Alcotest.test_case "alcotest works" `Quick test_alcotest_runs;
    ];
  ]
