(* Exactness tests for Fraud.Knn.

   Strategy: build the partition+KD index over a set of i16 reference points,
   then for many queries compare against a plain brute-force exact kNN over the
   SAME points.

   - exact mode  : the sorted k-best distances must equal brute force's k
                   smallest distances (true exact kNN), and the decision must
                   match (except at a genuine k/k+1 distance tie, which is
                   ambiguous and counted separately).
   - fast mode   : neighbours may differ (early-exit), but the *decision*
                   (approved = frauds < 3) must still match brute force. *)

let dim = Fraud.Knn.dim
let k = Fraud.Knn.k
let scale = Fraud.Knn.scale

let rng = Random.State.make [| 0x9E3779B9; 42; 1234 |]

(* mcc-bucket-ish discrete values the generator can emit (scaled). *)
let mcc_vals = [| 1500; 2000; 2500; 3000; 3500; 4500; 5000; 7500; 8000; 8500 |]

(* Generate one i16 reference/query point as an [int array] of length dim. *)
let gen_point ~clustered : int array =
  let v = Array.make dim 0 in
  let fraudy = clustered && Random.State.bool rng in
  let u lo hi = lo + Random.State.int rng (hi - lo + 1) in
  if clustered then begin
    if fraudy then begin
      v.(0) <- u 2000 scale;        (* amount high *)
      v.(1) <- u 5000 scale;        (* installments high *)
      v.(2) <- u 5000 scale;        (* amount vs avg high *)
      v.(7) <- u 2000 scale;        (* km from home high *)
      v.(8) <- u 4000 scale         (* tx_count high *)
    end else begin
      v.(0) <- u 0 500;
      v.(1) <- u 0 3000;
      v.(2) <- u 0 1000;
      v.(7) <- u 0 500;
      v.(8) <- u 0 3000
    end;
    v.(3) <- u 0 scale;
    v.(4) <- u 0 scale;
    v.(9) <- (if Random.State.bool rng then scale else 0);
    v.(10) <- (if Random.State.bool rng then scale else 0);
    v.(11) <- (if fraudy then scale else if Random.State.bool rng then scale else 0);
    v.(12) <- mcc_vals.(Random.State.int rng (Array.length mcc_vals));
    v.(13) <- u 0 scale;
    if Random.State.float rng 1.0 < 0.2 then begin
      v.(5) <- -scale; v.(6) <- -scale
    end else begin
      v.(5) <- u 0 scale; v.(6) <- u 0 scale
    end
  end
  else begin
    for d = 0 to dim - 1 do v.(d) <- Random.State.int rng (scale + 1) done;
    v.(9) <- (if Random.State.bool rng then scale else 0);
    v.(10) <- (if Random.State.bool rng then scale else 0);
    v.(11) <- (if Random.State.bool rng then scale else 0);
    v.(12) <- mcc_vals.(Random.State.int rng (Array.length mcc_vals));
    if Random.State.float rng 1.0 < 0.2 then begin
      v.(5) <- -scale; v.(6) <- -scale
    end
  end;
  v

let label_of (v : int array) : int =
  (* a label correlated with "fraudiness" plus noise, so buckets are mixed *)
  let score = (if v.(0) > 2000 then 1 else 0) + (if v.(8) > 3000 then 1 else 0)
              + (if v.(11) > 0 then 1 else 0) in
  if Random.State.float rng 1.0 < 0.1 then Random.State.int rng 2
  else if score >= 2 then 1 else 0

(* Flat reference store. *)
let make_refs n ~clustered =
  let store = Array.make (n * dim) 0 in
  let labels = Array.make n 0 in
  for i = 0 to n - 1 do
    let v = gen_point ~clustered in
    labels.(i) <- label_of v;
    Array.blit v 0 store (i * dim) dim
  done;
  (store, labels)

let dist (store : int array) i (q : int array) =
  let base = i * dim in
  let acc = ref 0 in
  for d = 0 to dim - 1 do
    let diff = store.(base + d) - q.(d) in
    acc := !acc + diff * diff
  done;
  !acc

(* Brute-force: returns (sorted k-best distances, frauds among k, kp1_dist).
   Tie rule for the count: take the k points of smallest distance, breaking
   ties toward LEGIT first then index — but we also report whether the k/k+1
   boundary is a distance tie so the caller can treat that as ambiguous. *)
let brute store labels n (q : int array) =
  let all = Array.init n (fun i -> (dist store i q, labels.(i), i)) in
  Array.sort
    (fun (da, la, ia) (db, lb, ib) ->
      if da <> db then compare da db
      else if la <> lb then compare la lb
      else compare ia ib)
    all;
  let kbest = Array.init k (fun i -> let (d, _, _) = all.(i) in d) in
  let frauds = ref 0 in
  for i = 0 to k - 1 do
    let (_, l, _) = all.(i) in
    frauds := !frauds + l
  done;
  let kth = let (d, _, _) = all.(k - 1) in d in
  let kp1 = let (d, _, _) = all.(k) in d in
  (kbest, !frauds, kth = kp1)

let build_index store labels n =
  Fraud.Knn.build ~n
    ~get:(fun p d -> store.(p * dim + d))
    ~label:(fun p -> labels.(p))

(* Pull the sorted k-best distances out of a fresh exact query by re-running
   with a scratch we can inspect. We expose them via a small replica: rerun
   brute on the index is not possible, so instead we verify exact-mode decision
   + that fast and exact agree, and rely on brute for ground truth. To check
   the actual neighbour distances we add an exact variant that returns them. *)

let run_case ~clustered ~n ~nq () =
  let store, labels = make_refs n ~clustered in
  let idx = build_index store labels n in
  let scratch = Fraud.Knn.create_scratch () in

  let queries =
    Array.init nq (fun i ->
      match i mod 3 with
      | 0 ->
        (* copy of a random ref: distance 0 -> early-exit fires *)
        let r = Random.State.int rng n in
        Array.init dim (fun d -> store.(r * dim + d))
      | 1 ->
        (* perturbed ref *)
        let r = Random.State.int rng n in
        Array.init dim (fun d ->
          let base = store.(r * dim + d) in
          if d = 5 || d = 6 then base
          else max 0 (min scale (base + Random.State.int rng 201 - 100)))
      | _ -> gen_point ~clustered)
  in

  let boundary_ties = ref 0 in
  let exact_decision_mismatch = ref 0 in
  let fast_decision_mismatch = ref 0 in
  let dist_mismatch = ref 0 in

  Array.iter
    (fun q ->
      let bkbest, bfrauds, btie = brute store labels n q in
      let ce = Fraud.Knn.fraud_count_with scratch idx q ~exact:true in
      (* The scratch now holds the exact k-best distances (sorted asc). *)
      let ekbest = Array.copy scratch.Fraud.Knn.best_d in
      let cf = Fraud.Knn.fraud_count_with scratch idx q ~exact:false in

      if ekbest <> bkbest then incr dist_mismatch;

      if btie then incr boundary_ties
      else begin
        if Fraud.Knn.approved_of_count ce <> Fraud.Knn.approved_of_count bfrauds then
          incr exact_decision_mismatch;
        if Fraud.Knn.approved_of_count cf <> Fraud.Knn.approved_of_count bfrauds then
          incr fast_decision_mismatch
      end)
    queries;

  Printf.printf
    "[%s n=%d nq=%d] dist_mismatch=%d exact_dec_mismatch=%d fast_dec_mismatch=%d boundary_ties=%d\n%!"
    (if clustered then "clustered" else "uniform")
    n nq !dist_mismatch !exact_decision_mismatch !fast_decision_mismatch !boundary_ties;

  Alcotest.(check int) "exact distances match brute force" 0 !dist_mismatch;
  Alcotest.(check int) "exact decision matches brute force" 0 !exact_decision_mismatch;
  Alcotest.(check int) "fast decision matches brute force" 0 !fast_decision_mismatch

let test_uniform () = run_case ~clustered:false ~n:4000 ~nq:4000 ()
let test_clustered () = run_case ~clustered:true ~n:6000 ~nq:6000 ()
let test_small () = run_case ~clustered:true ~n:200 ~nq:2000 ()

(* Quantization fidelity: float (round4) brute-force ordering must equal the
   i16 brute-force ordering, i.e. SCALE=10000 is decision-lossless. *)
let test_quantize_order () =
  let n = 800 in
  let round4 x = Float.round (x *. 10000.) /. 10000. in
  let fstore = Array.make (n * dim) 0.0 in
  for i = 0 to n * dim - 1 do fstore.(i) <- round4 (Random.State.float rng 1.0) done;
  (* sprinkle -1 sentinels in dims 5,6 *)
  for i = 0 to n - 1 do
    if Random.State.float rng 1.0 < 0.2 then begin
      fstore.(i * dim + 5) <- -1.0; fstore.(i * dim + 6) <- -1.0
    end
  done;
  let qstore = Array.map Fraud.Knn.quantize fstore in
  let fdist i (q : float array) =
    let base = i * dim in let acc = ref 0.0 in
    for d = 0 to dim - 1 do let df = fstore.(base + d) -. q.(d) in acc := !acc +. df *. df done; !acc
  in
  let qdist i (q : int array) =
    let base = i * dim in let acc = ref 0 in
    for d = 0 to dim - 1 do let df = qstore.(base + d) - q.(d) in acc := !acc + df * df done; !acc
  in
  let mismatches = ref 0 in
  for _ = 1 to 300 do
    let qi = Random.State.int rng n in
    let fq = Array.init dim (fun d -> fstore.(qi * dim + d)) in
    let iq = Array.init dim (fun d -> qstore.(qi * dim + d)) in
    let fsorted = Array.init n (fun i -> (fdist i fq, i)) in
    let isorted = Array.init n (fun i -> (qdist i iq, i)) in
    Array.sort (fun (a, ia) (b, ib) -> if a <> b then compare a b else compare ia ib) fsorted;
    Array.sort (fun (a, ia) (b, ib) -> if a <> b then compare a b else compare ia ib) isorted;
    (* compare the k nearest index sets *)
    for r = 0 to k - 1 do
      let (_, fi) = fsorted.(r) and (_, ii) = isorted.(r) in
      if fi <> ii then incr mismatches
    done
  done;
  Printf.printf "[quantize] knn-index ordering mismatches (float vs i16): %d\n%!" !mismatches;
  Alcotest.(check int) "i16 quantization preserves kNN ordering" 0 !mismatches

let () =
  Alcotest.run "knn"
    [ ("exactness",
       [ Alcotest.test_case "small" `Quick test_small;
         Alcotest.test_case "uniform" `Quick test_uniform;
         Alcotest.test_case "clustered" `Quick test_clustered ]);
      ("quantization",
       [ Alcotest.test_case "ordering" `Quick test_quantize_order ]) ]
