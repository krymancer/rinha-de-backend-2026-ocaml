(* Bench: brute-force 5-NN over 3M synthetic 14-dim float32 vectors.
   Goal: measure ns/query single thread before committing to brute force. *)

let n = 3_000_000
let d = 14
let k = 5

(* Flat float32 array, row-major: vec i lives at [i*d .. i*d+d-1].
   Bigarray Float32 → 4 bytes/elt → 3M*14*4 = 168 MB. *)
type vecs = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_vecs n d : vecs =
  let a = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * d) in
  let st = Random.State.make [| 42 |] in
  for i = 0 to n * d - 1 do
    Bigarray.Array1.unsafe_set a i (Random.State.float st 1.0)
  done;
  a

let make_labels n : Bytes.t =
  (* 1 = fraud, 0 = legit. ~5% fraud rate. *)
  let b = Bytes.create n in
  let st = Random.State.make [| 7 |] in
  for i = 0 to n - 1 do
    let v = if Random.State.float st 1.0 < 0.05 then '\001' else '\000' in
    Bytes.unsafe_set b i v
  done;
  b

(* Squared L2 distance, hot loop. Manually unrolled for d=14. *)
let[@inline] dist (vs : vecs) (q : vecs) (i : int) : float =
  let base = i * 14 in
  let d0 = Bigarray.Array1.unsafe_get vs base       -. Bigarray.Array1.unsafe_get q 0 in
  let d1 = Bigarray.Array1.unsafe_get vs (base + 1) -. Bigarray.Array1.unsafe_get q 1 in
  let d2 = Bigarray.Array1.unsafe_get vs (base + 2) -. Bigarray.Array1.unsafe_get q 2 in
  let d3 = Bigarray.Array1.unsafe_get vs (base + 3) -. Bigarray.Array1.unsafe_get q 3 in
  let d4 = Bigarray.Array1.unsafe_get vs (base + 4) -. Bigarray.Array1.unsafe_get q 4 in
  let d5 = Bigarray.Array1.unsafe_get vs (base + 5) -. Bigarray.Array1.unsafe_get q 5 in
  let d6 = Bigarray.Array1.unsafe_get vs (base + 6) -. Bigarray.Array1.unsafe_get q 6 in
  let d7 = Bigarray.Array1.unsafe_get vs (base + 7) -. Bigarray.Array1.unsafe_get q 7 in
  let d8 = Bigarray.Array1.unsafe_get vs (base + 8) -. Bigarray.Array1.unsafe_get q 8 in
  let d9 = Bigarray.Array1.unsafe_get vs (base + 9) -. Bigarray.Array1.unsafe_get q 9 in
  let d10 = Bigarray.Array1.unsafe_get vs (base + 10) -. Bigarray.Array1.unsafe_get q 10 in
  let d11 = Bigarray.Array1.unsafe_get vs (base + 11) -. Bigarray.Array1.unsafe_get q 11 in
  let d12 = Bigarray.Array1.unsafe_get vs (base + 12) -. Bigarray.Array1.unsafe_get q 12 in
  let d13 = Bigarray.Array1.unsafe_get vs (base + 13) -. Bigarray.Array1.unsafe_get q 13 in
  d0*.d0 +. d1*.d1 +. d2*.d2 +. d3*.d3 +. d4*.d4 +. d5*.d5 +. d6*.d6
  +. d7*.d7 +. d8*.d8 +. d9*.d9 +. d10*.d10 +. d11*.d11 +. d12*.d12 +. d13*.d13

(* 5-NN via top-k heap maintained as plain array. k=5 → linear scan cheaper than heap. *)
let knn5 (vs : vecs) (n : int) (q : vecs) (out_idx : int array) (out_d : float array) : unit =
  for j = 0 to k - 1 do out_d.(j) <- infinity; out_idx.(j) <- -1 done;
  let worst = ref infinity in
  let worst_pos = ref 0 in
  for i = 0 to n - 1 do
    let dd = dist vs q i in
    if dd < !worst then begin
      out_d.(!worst_pos) <- dd;
      out_idx.(!worst_pos) <- i;
      (* find new worst *)
      let w = ref out_d.(0) and wp = ref 0 in
      for j = 1 to k - 1 do
        if out_d.(j) > !w then begin w := out_d.(j); wp := j end
      done;
      worst := !w;
      worst_pos := !wp
    end
  done

let () =
  Printf.printf "building %d vecs of dim %d ...\n%!" n d;
  let t0 = Unix.gettimeofday () in
  let vs = make_vecs n d in
  let labels = make_labels n in
  Printf.printf "built in %.2fs (%d MB)\n%!" (Unix.gettimeofday () -. t0)
    (n * d * 4 / 1024 / 1024);

  (* Query buffer: one 14-dim vec. *)
  let q = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout d in
  let st = Random.State.make [| 99 |] in

  let out_idx = Array.make k 0 in
  let out_d = Array.make k 0.0 in
  let warmup = 3 in
  let trials = 20 in

  (* Warmup. *)
  for _ = 1 to warmup do
    for j = 0 to d - 1 do
      Bigarray.Array1.unsafe_set q j (Random.State.float st 1.0)
    done;
    knn5 vs n q out_idx out_d
  done;

  let times = Array.make trials 0.0 in
  for t = 0 to trials - 1 do
    for j = 0 to d - 1 do
      Bigarray.Array1.unsafe_set q j (Random.State.float st 1.0)
    done;
    let t0 = Unix.gettimeofday () in
    knn5 vs n q out_idx out_d;
    times.(t) <- (Unix.gettimeofday () -. t0) *. 1000.0
  done;

  Array.sort compare times;
  let med = times.(trials / 2) in
  let p99 = times.(trials - 1) in
  let mn = times.(0) in
  let avg = Array.fold_left (+.) 0.0 times /. float_of_int trials in

  (* Sanity: count fraud among top-5. *)
  let frauds = ref 0 in
  for j = 0 to k - 1 do
    if Bytes.unsafe_get labels out_idx.(j) = '\001' then incr frauds
  done;
  Printf.printf "knn5: min=%.2fms med=%.2fms avg=%.2fms p99=%.2fms (k=5, last query frauds=%d)\n"
    mn med avg p99 !frauds
