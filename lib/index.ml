(* IVF-flat index for 14-dim float32 vectors, cell-major layout. *)

let dim = 14
let k_neighbors = 5

type vecs = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type labels_ba = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type cell_offs = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  vecs : vecs;             (* n * dim, sorted cell-major *)
  n : int;
  labels : labels_ba;      (* n, sorted same as vecs *)
  centroids : vecs;        (* c * dim *)
  c : int;
  cell_offsets : cell_offs;(* len c+1 *)
}

(* Build a labels Array1 view from a Bytes buffer. *)
let bytes_to_ba (b : Bytes.t) : labels_ba =
  let n = Bytes.length b in
  let ba = Bigarray.Array1.create Bigarray.char Bigarray.c_layout n in
  for i = 0 to n - 1 do
    Bigarray.Array1.unsafe_set ba i (Bytes.unsafe_get b i)
  done;
  ba

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

let[@inline] dist_vv (vs1 : vecs) (i : int) (vs2 : vecs) (j : int) : float =
  let b1 = i * 14 and b2 = j * 14 in
  let s = ref 0.0 in
  for o = 0 to 13 do
    let x = Bigarray.Array1.unsafe_get vs1 (b1 + o)
            -. Bigarray.Array1.unsafe_get vs2 (b2 + o) in
    s := !s +. x *. x
  done;
  !s

let kmeans_train ~c ~iters ~sample ~st (vecs : vecs) (n : int) =
  let centroids = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (c * dim) in
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
  centroids

let assign_all ~c (vecs : vecs) (n : int) (centroids : vecs) =
  let assign = Array.make n 0 in
  for i = 0 to n - 1 do
    let best = ref 0 and bd = ref infinity in
    for ci = 0 to c - 1 do
      let dd = dist_vv vecs i centroids ci in
      if dd < !bd then begin bd := dd; best := ci end
    done;
    assign.(i) <- !best
  done;
  assign

let build ?(c = 1024) ?(iters = 5) ?(sample = 200_000)
    (vecs : vecs) (n : int) (labels_in : Bytes.t) : t =
  let st = Random.State.make [| 1234 |] in
  let centroids = kmeans_train ~c ~iters ~sample ~st vecs n in
  let assign    = assign_all ~c vecs n centroids in

  (* Build cell counts -> cell_offsets. *)
  let counts = Array.make c 0 in
  for i = 0 to n - 1 do counts.(assign.(i)) <- counts.(assign.(i)) + 1 done;
  let cell_offsets =
    Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout (c + 1) in
  let acc = ref 0 in
  for ci = 0 to c - 1 do
    Bigarray.Array1.set cell_offsets ci (Int64.of_int !acc);
    acc := !acc + counts.(ci)
  done;
  Bigarray.Array1.set cell_offsets c (Int64.of_int !acc);
  assert (!acc = n);

  (* Sort vecs+labels into cell-major layout. *)
  let sorted_vecs =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * dim) in
  let sorted_labels =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout n in
  let cursors = Array.make c 0 in
  (* cursors[ci] starts at cell_offsets[ci] *)
  for ci = 0 to c - 1 do
    cursors.(ci) <- Int64.to_int (Bigarray.Array1.get cell_offsets ci)
  done;
  for i = 0 to n - 1 do
    let ci = assign.(i) in
    let dst = cursors.(ci) in
    cursors.(ci) <- dst + 1;
    for o = 0 to dim - 1 do
      Bigarray.Array1.unsafe_set sorted_vecs (dst * dim + o)
        (Bigarray.Array1.unsafe_get vecs (i * dim + o))
    done;
    Bigarray.Array1.unsafe_set sorted_labels dst
      (Bytes.unsafe_get labels_in i)
  done;
  { vecs = sorted_vecs; n; labels = sorted_labels;
    centroids; c; cell_offsets }

let of_segments ~vecs ~n ~labels ~centroids ~c ~cell_offsets : t =
  { vecs; n; labels; centroids; c; cell_offsets }

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
    let lo = Int64.to_int (Bigarray.Array1.unsafe_get idx.cell_offsets ci) in
    let hi = Int64.to_int (Bigarray.Array1.unsafe_get idx.cell_offsets (ci + 1)) in
    for vi = lo to hi - 1 do
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
    if out_idx.(j) >= 0
       && Bigarray.Array1.unsafe_get idx.labels out_idx.(j) = '\001'
    then incr frauds
  done;
  float_of_int !frauds /. float_of_int k_neighbors
