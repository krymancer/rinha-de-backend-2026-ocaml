(* IVF-flat index for 14-dim float32 vectors.
   In-memory only for now; mmap save/load comes later. *)

let dim = 14
let k_neighbors = 5

type vecs = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  vecs : vecs;            (* n * dim *)
  n : int;
  labels : Bytes.t;       (* n bytes, 1 = fraud, 0 = legit *)
  centroids : vecs;       (* c * dim *)
  c : int;
  lists : int array array; (* c entries, each = vec indices in cell *)
}

(* Squared L2: vs[i] vs query q[0..13]. Manually unrolled. *)
let[@inline] dist_vq (vs : vecs) (i : int) (q : float array) : float =
  let base = i * 14 in
  let d0 = Bigarray.Array1.unsafe_get vs base       -. Array.unsafe_get q 0 in
  let d1 = Bigarray.Array1.unsafe_get vs (base + 1) -. Array.unsafe_get q 1 in
  let d2 = Bigarray.Array1.unsafe_get vs (base + 2) -. Array.unsafe_get q 2 in
  let d3 = Bigarray.Array1.unsafe_get vs (base + 3) -. Array.unsafe_get q 3 in
  let d4 = Bigarray.Array1.unsafe_get vs (base + 4) -. Array.unsafe_get q 4 in
  let d5 = Bigarray.Array1.unsafe_get vs (base + 5) -. Array.unsafe_get q 5 in
  let d6 = Bigarray.Array1.unsafe_get vs (base + 6) -. Array.unsafe_get q 6 in
  let d7 = Bigarray.Array1.unsafe_get vs (base + 7) -. Array.unsafe_get q 7 in
  let d8 = Bigarray.Array1.unsafe_get vs (base + 8) -. Array.unsafe_get q 8 in
  let d9 = Bigarray.Array1.unsafe_get vs (base + 9) -. Array.unsafe_get q 9 in
  let d10 = Bigarray.Array1.unsafe_get vs (base + 10) -. Array.unsafe_get q 10 in
  let d11 = Bigarray.Array1.unsafe_get vs (base + 11) -. Array.unsafe_get q 11 in
  let d12 = Bigarray.Array1.unsafe_get vs (base + 12) -. Array.unsafe_get q 12 in
  let d13 = Bigarray.Array1.unsafe_get vs (base + 13) -. Array.unsafe_get q 13 in
  d0*.d0 +. d1*.d1 +. d2*.d2 +. d3*.d3 +. d4*.d4 +. d5*.d5 +. d6*.d6
  +. d7*.d7 +. d8*.d8 +. d9*.d9 +. d10*.d10 +. d11*.d11 +. d12*.d12 +. d13*.d13

(* dist between two slabs (used for k-means). *)
let[@inline] dist_vv (vs1 : vecs) (i : int) (vs2 : vecs) (j : int) : float =
  let b1 = i * 14 and b2 = j * 14 in
  let s = ref 0.0 in
  for o = 0 to 13 do
    let x = Bigarray.Array1.unsafe_get vs1 (b1 + o) -. Bigarray.Array1.unsafe_get vs2 (b2 + o) in
    s := !s +. x *. x
  done;
  !s

(* Build IVF: train kmeans on sample of n_sample, assign all n. *)
let build ?(c = 1024) ?(iters = 5) ?(sample = 200_000) (vecs : vecs) (n : int) (labels : Bytes.t) : t =
  let centroids = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (c * dim) in
  let st = Random.State.make [| 1234 |] in

  for ci = 0 to c - 1 do
    let src = Random.State.int st n in
    for o = 0 to dim - 1 do
      Bigarray.Array1.unsafe_set centroids (ci * dim + o)
        (Bigarray.Array1.unsafe_get vecs (src * dim + o))
    done
  done;

  let sample = min sample n in
  let sample_ix = Array.init sample (fun _ -> Random.State.int st n) in
  let assign_s = Array.make sample 0 in
  let sums = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout (c * dim) in
  let counts = Array.make c 0 in

  for _it = 1 to iters do
    for si = 0 to sample - 1 do
      let vi = sample_ix.(si) in
      let best = ref 0 and bd = ref infinity in
      for ci = 0 to c - 1 do
        let dd = dist_vv vecs vi centroids ci in
        if dd < !bd then begin bd := dd; best := ci end
      done;
      assign_s.(si) <- !best
    done;
    Bigarray.Array1.fill sums 0.0;
    Array.fill counts 0 c 0;
    for si = 0 to sample - 1 do
      let vi = sample_ix.(si) in
      let ci = assign_s.(si) in
      counts.(ci) <- counts.(ci) + 1;
      for o = 0 to dim - 1 do
        let v = Bigarray.Array1.unsafe_get vecs (vi * dim + o) in
        Bigarray.Array1.unsafe_set sums (ci * dim + o)
          (Bigarray.Array1.unsafe_get sums (ci * dim + o) +. v)
      done
    done;
    for ci = 0 to c - 1 do
      if counts.(ci) > 0 then
        let inv = 1.0 /. float_of_int counts.(ci) in
        for o = 0 to dim - 1 do
          Bigarray.Array1.unsafe_set centroids (ci * dim + o)
            (Bigarray.Array1.unsafe_get sums (ci * dim + o) *. inv)
        done
    done
  done;

  let assign = Array.make n 0 in
  for i = 0 to n - 1 do
    let best = ref 0 and bd = ref infinity in
    for ci = 0 to c - 1 do
      let dd = dist_vv vecs i centroids ci in
      if dd < !bd then begin bd := dd; best := ci end
    done;
    assign.(i) <- !best
  done;

  let counts2 = Array.make c 0 in
  for i = 0 to n - 1 do counts2.(assign.(i)) <- counts2.(assign.(i)) + 1 done;
  let lists = Array.init c (fun ci -> Array.make counts2.(ci) 0) in
  let cursors = Array.make c 0 in
  for i = 0 to n - 1 do
    let ci = assign.(i) in
    lists.(ci).(cursors.(ci)) <- i;
    cursors.(ci) <- cursors.(ci) + 1
  done;
  { vecs; n; labels; centroids; c; lists }

(* Search: nprobe nearest cells, return fraud_score (0..1). *)
let fraud_score (idx : t) (q : float array) ~(nprobe : int) : float =
  let cell_d = Array.make idx.c 0.0 in
  for ci = 0 to idx.c - 1 do
    cell_d.(ci) <- dist_vq idx.centroids ci q
  done;
  let cell_idx = Array.init idx.c (fun i -> i) in
  Array.sort (fun a b -> compare cell_d.(a) cell_d.(b)) cell_idx;

  let out_idx = Array.make k_neighbors (-1) in
  let out_d = Array.make k_neighbors infinity in
  let worst = ref infinity in
  let worst_pos = ref 0 in
  let probe = min nprobe idx.c in
  for p = 0 to probe - 1 do
    let ci = cell_idx.(p) in
    let lst = idx.lists.(ci) in
    let m = Array.length lst in
    for j = 0 to m - 1 do
      let vi = lst.(j) in
      let dd = dist_vq idx.vecs vi q in
      if dd < !worst then begin
        out_d.(!worst_pos) <- dd;
        out_idx.(!worst_pos) <- vi;
        let w = ref out_d.(0) and wp = ref 0 in
        for jj = 1 to k_neighbors - 1 do
          if out_d.(jj) > !w then begin w := out_d.(jj); wp := jj end
        done;
        worst := !w; worst_pos := !wp
      end
    done
  done;

  let frauds = ref 0 in
  for j = 0 to k_neighbors - 1 do
    if out_idx.(j) >= 0 && Bytes.unsafe_get idx.labels out_idx.(j) = '\001' then
      incr frauds
  done;
  float_of_int !frauds /. float_of_int k_neighbors
