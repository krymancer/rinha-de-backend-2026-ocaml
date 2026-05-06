(* int8 quantize bench: same 3M*14 dataset but stored as int8.
   Distance: sum of (a_i - b_i)^2 in int. Max per-dim diff = 254, squared = 64516.
   14 dims → max sum = 903_224 → fits int easily.
   Mem: 3M * 14 = 42 MB (vs 168 MB float32). 4× smaller, fits L3 better. *)

let n = 3_000_000
let d = 14
let k = 5

type ivecs = (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_ivecs n d : ivecs =
  let a = Bigarray.Array1.create Bigarray.int8_signed Bigarray.c_layout (n * d) in
  let st = Random.State.make [| 42 |] in
  for i = 0 to n * d - 1 do
    (* uniform [0,1) → quantize to [-127,127]. Same RNG seed as float bench. *)
    let f = Random.State.float st 1.0 in
    let q = int_of_float (f *. 254.0) - 127 in
    Bigarray.Array1.unsafe_set a i q
  done;
  a

(* Squared L2 over int8 lanes. Manually unrolled d=14. *)
let[@inline] dist (vs : ivecs) (q : ivecs) (i : int) : int =
  let base = i * 14 in
  let d0 = Bigarray.Array1.unsafe_get vs base       - Bigarray.Array1.unsafe_get q 0 in
  let d1 = Bigarray.Array1.unsafe_get vs (base + 1) - Bigarray.Array1.unsafe_get q 1 in
  let d2 = Bigarray.Array1.unsafe_get vs (base + 2) - Bigarray.Array1.unsafe_get q 2 in
  let d3 = Bigarray.Array1.unsafe_get vs (base + 3) - Bigarray.Array1.unsafe_get q 3 in
  let d4 = Bigarray.Array1.unsafe_get vs (base + 4) - Bigarray.Array1.unsafe_get q 4 in
  let d5 = Bigarray.Array1.unsafe_get vs (base + 5) - Bigarray.Array1.unsafe_get q 5 in
  let d6 = Bigarray.Array1.unsafe_get vs (base + 6) - Bigarray.Array1.unsafe_get q 6 in
  let d7 = Bigarray.Array1.unsafe_get vs (base + 7) - Bigarray.Array1.unsafe_get q 7 in
  let d8 = Bigarray.Array1.unsafe_get vs (base + 8) - Bigarray.Array1.unsafe_get q 8 in
  let d9 = Bigarray.Array1.unsafe_get vs (base + 9) - Bigarray.Array1.unsafe_get q 9 in
  let d10 = Bigarray.Array1.unsafe_get vs (base + 10) - Bigarray.Array1.unsafe_get q 10 in
  let d11 = Bigarray.Array1.unsafe_get vs (base + 11) - Bigarray.Array1.unsafe_get q 11 in
  let d12 = Bigarray.Array1.unsafe_get vs (base + 12) - Bigarray.Array1.unsafe_get q 12 in
  let d13 = Bigarray.Array1.unsafe_get vs (base + 13) - Bigarray.Array1.unsafe_get q 13 in
  d0*d0 + d1*d1 + d2*d2 + d3*d3 + d4*d4 + d5*d5 + d6*d6
  + d7*d7 + d8*d8 + d9*d9 + d10*d10 + d11*d11 + d12*d12 + d13*d13

let knn5 (vs : ivecs) (n : int) (q : ivecs) (out_idx : int array) (out_d : int array) : unit =
  for j = 0 to k - 1 do out_d.(j) <- max_int; out_idx.(j) <- -1 done;
  let worst = ref max_int in
  let worst_pos = ref 0 in
  for i = 0 to n - 1 do
    let dd = dist vs q i in
    if dd < !worst then begin
      out_d.(!worst_pos) <- dd;
      out_idx.(!worst_pos) <- i;
      let w = ref out_d.(0) and wp = ref 0 in
      for j = 1 to k - 1 do
        if out_d.(j) > !w then begin w := out_d.(j); wp := j end
      done;
      worst := !w;
      worst_pos := !wp
    end
  done

let () =
  Printf.printf "building %d int8 vecs of dim %d ...\n%!" n d;
  let t0 = Unix.gettimeofday () in
  let vs = make_ivecs n d in
  Printf.printf "built in %.2fs (%d MB)\n%!" (Unix.gettimeofday () -. t0)
    (n * d / 1024 / 1024);

  let q = Bigarray.Array1.create Bigarray.int8_signed Bigarray.c_layout d in
  let st = Random.State.make [| 99 |] in

  let out_idx = Array.make k 0 in
  let out_d = Array.make k 0 in
  let warmup = 3 in
  let trials = 20 in

  for _ = 1 to warmup do
    for j = 0 to d - 1 do
      let f = Random.State.float st 1.0 in
      Bigarray.Array1.unsafe_set q j (int_of_float (f *. 254.0) - 127)
    done;
    knn5 vs n q out_idx out_d
  done;

  let times = Array.make trials 0.0 in
  for t = 0 to trials - 1 do
    for j = 0 to d - 1 do
      let f = Random.State.float st 1.0 in
      Bigarray.Array1.unsafe_set q j (int_of_float (f *. 254.0) - 127)
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
  Printf.printf "knn5 int8: min=%.2fms med=%.2fms avg=%.2fms p99=%.2fms\n"
    mn med avg p99
