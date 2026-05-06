(* IVF-flat over float32 3M*14 synthetic vecs.
   Build: pick C random vectors as centroids (skip k-means for bench; real impl will do it).
           Assign each vec to nearest centroid. Group by cell.
   Search: compute dist to all C centroids, take nprobe closest cells, brute-force their members.
   Bench p99 + recall vs ground-truth brute force. *)

let n = 3_000_000
let d = 14
let k = 5
let n_centroids = 1024
let nprobe_default = 8
let n_kmeans_iters = 5
let kmeans_sample = 200_000  (* train on subset for speed *)

type vecs = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_vecs n d : vecs =
  let a = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * d) in
  let st = Random.State.make [| 42 |] in
  for i = 0 to n * d - 1 do
    Bigarray.Array1.unsafe_set a i (Random.State.float st 1.0)
  done;
  a

(* Squared L2, unrolled d=14, vs[i] vs q. *)
let[@inline] dist_vq (vs : vecs) (q : vecs) (i : int) : float =
  let base = i * 14 in
  let s = ref 0.0 in
  for j = 0 to 13 do
    let x = Bigarray.Array1.unsafe_get vs (base + j) -. Bigarray.Array1.unsafe_get q j in
    s := !s +. x *. x
  done;
  !s

(* Squared L2, between two slabs vs1[i] and vs2[j]. *)
let[@inline] dist_vv (vs1 : vecs) (i : int) (vs2 : vecs) (j : int) : float =
  let b1 = i * 14 and b2 = j * 14 in
  let s = ref 0.0 in
  for o = 0 to 13 do
    let x = Bigarray.Array1.unsafe_get vs1 (b1 + o) -. Bigarray.Array1.unsafe_get vs2 (b2 + o) in
    s := !s +. x *. x
  done;
  !s

(* Brute force ground truth top-k indices. *)
let knn_brute (vs : vecs) (n : int) (q : vecs) : int array =
  let out_idx = Array.make k (-1) in
  let out_d = Array.make k infinity in
  let worst = ref infinity in
  let worst_pos = ref 0 in
  for i = 0 to n - 1 do
    let dd = dist_vq vs q i in
    if dd < !worst then begin
      out_d.(!worst_pos) <- dd;
      out_idx.(!worst_pos) <- i;
      let w = ref out_d.(0) and wp = ref 0 in
      for j = 1 to k - 1 do
        if out_d.(j) > !w then begin w := out_d.(j); wp := j end
      done;
      worst := !w; worst_pos := !wp
    end
  done;
  out_idx

(* k-means: pick init from sample, lloyd iters on sample. *)
let kmeans_train (vs : vecs) (n : int) (c : int) (iters : int) : vecs * int array =
  Printf.printf "kmeans: %d centroids, %d iters on %d-sample ...\n%!" c iters kmeans_sample;
  let centroids = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (c * d) in
  (* Init: random sample of vecs. *)
  let st = Random.State.make [| 1234 |] in
  for ci = 0 to c - 1 do
    let src = Random.State.int st n in
    for o = 0 to d - 1 do
      Bigarray.Array1.unsafe_set centroids (ci * d + o)
        (Bigarray.Array1.unsafe_get vs (src * d + o))
    done
  done;

  (* Sample indices. *)
  let sample = Array.init kmeans_sample (fun _ -> Random.State.int st n) in
  let assign = Array.make kmeans_sample 0 in
  let sums = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout (c * d) in
  let counts = Array.make c 0 in

  for it = 1 to iters do
    let t0 = Unix.gettimeofday () in
    (* Assign. *)
    for si = 0 to kmeans_sample - 1 do
      let vi = sample.(si) in
      let best = ref 0 and bd = ref infinity in
      for ci = 0 to c - 1 do
        let dd = dist_vv vs vi centroids ci in
        if dd < !bd then begin bd := dd; best := ci end
      done;
      assign.(si) <- !best
    done;
    (* Update. *)
    Bigarray.Array1.fill sums 0.0;
    Array.fill counts 0 c 0;
    for si = 0 to kmeans_sample - 1 do
      let vi = sample.(si) in
      let ci = assign.(si) in
      counts.(ci) <- counts.(ci) + 1;
      for o = 0 to d - 1 do
        let v = Bigarray.Array1.unsafe_get vs (vi * d + o) in
        Bigarray.Array1.unsafe_set sums (ci * d + o)
          (Bigarray.Array1.unsafe_get sums (ci * d + o) +. v)
      done
    done;
    for ci = 0 to c - 1 do
      if counts.(ci) > 0 then
        let inv = 1.0 /. float_of_int counts.(ci) in
        for o = 0 to d - 1 do
          Bigarray.Array1.unsafe_set centroids (ci * d + o)
            (Bigarray.Array1.unsafe_get sums (ci * d + o) *. inv)
        done
    done;
    Printf.printf "  iter %d/%d: %.2fs\n%!" it iters (Unix.gettimeofday () -. t0)
  done;
  centroids, assign  (* assign returned just for type, we re-assign all n below *)

(* Assign all n vecs to nearest centroid. *)
let assign_all (vs : vecs) (n : int) (centroids : vecs) (c : int) : int array =
  Printf.printf "assigning all %d vecs to %d cells ...\n%!" n c;
  let t0 = Unix.gettimeofday () in
  let a = Array.make n 0 in
  for i = 0 to n - 1 do
    let best = ref 0 and bd = ref infinity in
    for ci = 0 to c - 1 do
      let dd = dist_vv vs i centroids ci in
      if dd < !bd then begin bd := dd; best := ci end
    done;
    a.(i) <- !best
  done;
  Printf.printf "  done in %.2fs\n%!" (Unix.gettimeofday () -. t0);
  a

(* Build inverted lists: cell -> list of vec indices. *)
let build_lists (assign : int array) (n : int) (c : int) : int array array =
  let counts = Array.make c 0 in
  for i = 0 to n - 1 do counts.(assign.(i)) <- counts.(assign.(i)) + 1 done;
  let lists = Array.init c (fun ci -> Array.make counts.(ci) 0) in
  let cursors = Array.make c 0 in
  for i = 0 to n - 1 do
    let ci = assign.(i) in
    lists.(ci).(cursors.(ci)) <- i;
    cursors.(ci) <- cursors.(ci) + 1
  done;
  lists

(* IVF search: nprobe nearest cells, brute their members. *)
let knn_ivf (vs : vecs) (centroids : vecs) (c : int) (lists : int array array)
    (q : vecs) (nprobe : int) : int array =
  (* Step 1: nprobe nearest cells. *)
  let cell_d = Array.make c 0.0 in
  for ci = 0 to c - 1 do cell_d.(ci) <- dist_vq centroids q ci done;
  let cell_idx = Array.init c (fun i -> i) in
  Array.sort (fun a b -> compare cell_d.(a) cell_d.(b)) cell_idx;

  (* Step 2: brute over selected cells, top-k. *)
  let out_idx = Array.make k (-1) in
  let out_d = Array.make k infinity in
  let worst = ref infinity in
  let worst_pos = ref 0 in
  for p = 0 to nprobe - 1 do
    let ci = cell_idx.(p) in
    let lst = lists.(ci) in
    let m = Array.length lst in
    for j = 0 to m - 1 do
      let vi = lst.(j) in
      let dd = dist_vq vs q vi in
      if dd < !worst then begin
        out_d.(!worst_pos) <- dd;
        out_idx.(!worst_pos) <- vi;
        let w = ref out_d.(0) and wp = ref 0 in
        for jj = 1 to k - 1 do
          if out_d.(jj) > !w then begin w := out_d.(jj); wp := jj end
        done;
        worst := !w; worst_pos := !wp
      end
    done
  done;
  out_idx

let () =
  Printf.printf "building %d vecs of dim %d ...\n%!" n d;
  let t0 = Unix.gettimeofday () in
  let vs = make_vecs n d in
  Printf.printf "built in %.2fs\n%!" (Unix.gettimeofday () -. t0);

  let t0 = Unix.gettimeofday () in
  let centroids, _ = kmeans_train vs n n_centroids n_kmeans_iters in
  Printf.printf "kmeans done in %.2fs\n%!" (Unix.gettimeofday () -. t0);

  let assign = assign_all vs n centroids n_centroids in
  let lists = build_lists assign n n_centroids in
  let sizes = Array.map Array.length lists in
  Array.sort compare sizes;
  Printf.printf "cell sizes: min=%d p50=%d p99=%d max=%d\n%!"
    sizes.(0) sizes.(n_centroids/2)
    sizes.(n_centroids - n_centroids/100 - 1)
    sizes.(n_centroids - 1);

  (* Bench. *)
  let q = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout d in
  let st = Random.State.make [| 99 |] in

  (* Recall: ground truth from brute force, compare top-5. *)
  let recall_trials = 20 in
  let recall_n = ref 0 in
  let recall_total = ref 0 in
  for _ = 0 to recall_trials - 1 do
    for j = 0 to d - 1 do
      Bigarray.Array1.unsafe_set q j (Random.State.float st 1.0)
    done;
    let truth = knn_brute vs n q in
    let got = knn_ivf vs centroids n_centroids lists q nprobe_default in
    let truth_set = Array.fold_left (fun acc i -> i :: acc) [] truth in
    Array.iter (fun i ->
      if List.mem i truth_set then incr recall_n;
      incr recall_total
    ) got
  done;
  Printf.printf "recall@5 (nprobe=%d): %d/%d = %.3f\n%!"
    nprobe_default !recall_n !recall_total
    (float_of_int !recall_n /. float_of_int !recall_total);

  let warmup = 3 in
  let trials = 50 in
  for _ = 1 to warmup do
    for j = 0 to d - 1 do
      Bigarray.Array1.unsafe_set q j (Random.State.float st 1.0)
    done;
    let _ = knn_ivf vs centroids n_centroids lists q nprobe_default in ()
  done;

  List.iter (fun nprobe ->
    let times = Array.make trials 0.0 in
    for t = 0 to trials - 1 do
      for j = 0 to d - 1 do
        Bigarray.Array1.unsafe_set q j (Random.State.float st 1.0)
      done;
      let t0 = Unix.gettimeofday () in
      let _ = knn_ivf vs centroids n_centroids lists q nprobe in
      times.(t) <- (Unix.gettimeofday () -. t0) *. 1000.0
    done;
    Array.sort compare times;
    let med = times.(trials / 2) in
    let p99 = times.(trials * 99 / 100) in
    let mn = times.(0) in
    Printf.printf "ivf nprobe=%d: min=%.3fms med=%.3fms p99=%.3fms\n%!"
      nprobe mn med p99
  ) [1; 4; 8; 16; 32]
