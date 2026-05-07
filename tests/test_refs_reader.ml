open Fraud

let test_fold_example_references () =
  let path = "./fixtures/example-references.json" in
  let count = ref 0 in
  let fraud_count = ref 0 in
  let last_v0 = ref nan in
  Refs_reader.fold (fun () (vec, label) ->
    incr count;
    if label = `Fraud then incr fraud_count;
    last_v0 := vec.(0)
  ) () (Refs_reader.File path);
  Alcotest.(check bool) "non-empty" true (!count > 0);
  Alcotest.(check bool) "some frauds" true (!fraud_count > 0);
  Alcotest.(check bool) "fewer frauds than total" true (!fraud_count < !count);
  Alcotest.(check bool) "v0 in [0,1] or -1" true
    (let x = !last_v0 in (x >= 0.0 && x <= 1.0) || x = -1.0)

let () =
  Alcotest.run "refs_reader" [
    "fold", [
      Alcotest.test_case "fold over example-references" `Quick
        test_fold_example_references;
    ];
  ]
