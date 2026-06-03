(* Exact k-NN fraud detection over i16-quantized 14-dim vectors.

   Mirrors the challenge ground truth exactly:
     - k = 5, squared-Euclidean distance, threshold 0.6 (approved = frauds < 3).
     - Reference data is round4 (4 decimals). Quantizing each dim with
       SCALE = 10000 maps round4(x) to an exact integer, so the integer
       squared distance equals the float distance * 10^8 *exactly*. Neighbour
       ordering is therefore identical to the float brute-force used to label
       the test set -> detection is exact, not approximate.

   Speed comes from a two-level spatial index:
     - Partition the references into <=256 buckets by a key built from the
       discrete dimensions (presence of last_tx, is_online, card_present,
       unknown_merchant, mcc bucket, amount-vs-avg high, tx_count high).
     - Build a KD-tree per bucket (leaf size 128, split on the widest dim),
       with a bounding box per node.
     - Query: search the home bucket with branch-and-bound (bbox lower-bound
       pruning), then probe other buckets only while their bbox lower bound is
       below the current 5th-best distance. This is exact.
     - Optional early-exit: stop once the 5 neighbours are all within 0.14
       (the "confident" radius). Decision-exact on this dataset; off by default
       in [fraud_count ~exact:true]. *)

let dim = 14
let k = 5
let scale = 10000

(* (0.14 * scale)^2 — the "confident" squared radius used for early-exit. *)
let early_limit = 1400 * 1400

let leaf_size = 128

type i16ba = (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t
type intba = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_i16 n : i16ba =
  Bigarray.Array1.create Bigarray.int16_signed Bigarray.c_layout n

(* SIMD (SSE4.1) leaf scan: write exact i64 squared distances from the 16-wide
   query [q16] (dims 0..13, lanes 14,15 = 0) to points [start, start+len) into
   [out]. Bit-identical to the scalar path (asserted by tests/test_knn.ml). *)
external simd_leaf_dists : i16ba -> i16ba -> int -> int -> intba -> unit
  = "fraud_simd_leaf_dists" [@@noalloc]

type t = {
  n : int;
  vecs : i16ba;            (* n*dim, leaf-contiguous *)
  labels : Bytes.t;        (* n, leaf-contiguous: 1 = fraud, 0 = legit *)
  node_left : int array;   (* -1 => leaf *)
  node_right : int array;
  node_start : int array;  (* leaf: first point index into [vecs]/[labels] *)
  node_len : int array;
  node_min : int array;    (* node_count*dim *)
  node_max : int array;    (* node_count*dim *)
  roots : int array;       (* 256: partition key -> root node id, -1 if empty *)
}

(* --- quantization --- *)

let[@inline] quantize (x : float) : int =
  if x <= -1.0 then -scale
  else if x >= 1.0 then scale
  else int_of_float (Float.round (x *. float_of_int scale))

let quantize_vec (v : float array) : int array =
  Array.init dim (fun d -> quantize v.(d))

(* --- partition key --- *)

let[@inline] partition_key_of (get : int -> int) : int =
  let key = ref 0 in
  if get 5 >= 0 then key := !key lor 1;
  if get 9 > 0 then key := !key lor 2;
  if get 10 > 0 then key := !key lor 4;
  if get 11 > 0 then key := !key lor 8;
  let mr = get 12 in
  if mr <= 2047 then ()
  else if mr <= 4095 then key := !key lor (1 lsl 4)
  else if mr <= 6143 then key := !key lor (2 lsl 4)
  else key := !key lor (3 lsl 4);
  if get 2 > 4096 then key := !key lor (1 lsl 6);
  if get 8 > 2048 then key := !key lor (1 lsl 7);
  !key

(* Query-time partition key: same logic as [partition_key_of] but reads [q]
   directly. Passing a closure (fun d -> q.(d)) to partition_key_of allocates
   one closure per query on a non-flambda compiler — the only heap traffic left
   in the hot path — so inline it. *)
let[@inline] partition_key (q : int array) : int =
  let key = ref 0 in
  if Array.unsafe_get q 5 >= 0 then key := !key lor 1;
  if Array.unsafe_get q 9 > 0 then key := !key lor 2;
  if Array.unsafe_get q 10 > 0 then key := !key lor 4;
  if Array.unsafe_get q 11 > 0 then key := !key lor 8;
  let mr = Array.unsafe_get q 12 in
  if mr <= 2047 then ()
  else if mr <= 4095 then key := !key lor (1 lsl 4)
  else if mr <= 6143 then key := !key lor (2 lsl 4)
  else key := !key lor (3 lsl 4);
  if Array.unsafe_get q 2 > 4096 then key := !key lor (1 lsl 6);
  if Array.unsafe_get q 8 > 2048 then key := !key lor (1 lsl 7);
  !key

(* --- build --- *)

type build_node =
  | Leaf of { start : int; len : int; mn : int array; mx : int array }
  | Inner of { left : build_node; right : build_node; mn : int array; mx : int array }

(* Build the index from [n] source points read through [get p d] (i16-scaled
   value of dim [d] of point [p]) and [label p] (0/1). *)
let build ~n ~(get : int -> int -> int) ~(label : int -> int) : t =
  let buckets = Array.make 256 [] in
  for i = n - 1 downto 0 do
    let key = partition_key_of (fun d -> get i d) in
    buckets.(key) <- i :: buckets.(key)
  done;

  let perm = Array.make (max 1 n) 0 in
  let perm_pos = ref 0 in

  let bbox idxs =
    let mn = Array.make dim max_int and mx = Array.make dim min_int in
    Array.iter
      (fun i ->
        for d = 0 to dim - 1 do
          let x = get i d in
          if x < mn.(d) then mn.(d) <- x;
          if x > mx.(d) then mx.(d) <- x
        done)
      idxs;
    (mn, mx)
  in

  let rec build_tree (idxs : int array) : build_node =
    let mn, mx = bbox idxs in
    if Array.length idxs <= leaf_size then begin
      let start = !perm_pos in
      Array.iter (fun i -> perm.(!perm_pos) <- i; incr perm_pos) idxs;
      Leaf { start; len = Array.length idxs; mn; mx }
    end
    else begin
      let sd = ref 0 and bw = ref (-1) in
      for d = 0 to dim - 1 do
        let w = mx.(d) - mn.(d) in
        if w > !bw then begin bw := w; sd := d end
      done;
      let sd = !sd in
      Array.sort (fun a b -> compare (get a sd) (get b sd)) idxs;
      let mid = Array.length idxs / 2 in
      let left = build_tree (Array.sub idxs 0 mid) in
      let right = build_tree (Array.sub idxs mid (Array.length idxs - mid)) in
      Inner { left; right; mn; mx }
    end
  in

  let trees = Array.make 256 None in
  for key = 0 to 255 do
    match buckets.(key) with
    | [] -> ()
    | lst -> trees.(key) <- Some (build_tree (Array.of_list lst))
  done;

  (* Count nodes, then flatten into flat arrays with pre-order ids. *)
  let rec count = function
    | Leaf _ -> 1
    | Inner { left; right; _ } -> 1 + count left + count right
  in
  let node_count =
    Array.fold_left (fun acc t -> match t with None -> acc | Some bn -> acc + count bn) 0 trees
  in
  let node_count = max 1 node_count in
  let node_left = Array.make node_count (-1) in
  let node_right = Array.make node_count (-1) in
  let node_start = Array.make node_count 0 in
  let node_len = Array.make node_count 0 in
  let node_min = Array.make (node_count * dim) 0 in
  let node_max = Array.make (node_count * dim) 0 in
  let roots = Array.make 256 (-1) in

  let next = ref 0 in
  let set_bbox id mn mx =
    let base = id * dim in
    for d = 0 to dim - 1 do
      node_min.(base + d) <- mn.(d);
      node_max.(base + d) <- mx.(d)
    done
  in
  let rec assign = function
    | Leaf { start; len; mn; mx } ->
      let id = !next in
      incr next;
      node_left.(id) <- -1;
      node_right.(id) <- -1;
      node_start.(id) <- start;
      node_len.(id) <- len;
      set_bbox id mn mx;
      id
    | Inner { left; right; mn; mx } ->
      let id = !next in
      incr next;
      let l = assign left in
      let r = assign right in
      node_left.(id) <- l;
      node_right.(id) <- r;
      set_bbox id mn mx;
      id
  in
  for key = 0 to 255 do
    match trees.(key) with
    | None -> ()
    | Some bn -> roots.(key) <- assign bn
  done;

  (* Materialize leaf-contiguous vectors + labels. *)
  let vecs = make_i16 (max 1 (n * dim)) in
  let labels = Bytes.make (max 1 n) '\000' in
  for pos = 0 to n - 1 do
    let src = perm.(pos) in
    let base = pos * dim in
    for d = 0 to dim - 1 do
      Bigarray.Array1.unsafe_set vecs (base + d) (get src d)
    done;
    Bytes.unsafe_set labels pos (Char.chr (label src land 1))
  done;

  { n; vecs; labels; node_left; node_right; node_start; node_len;
    node_min; node_max; roots }

(* --- query --- *)

type scratch = {
  best_d : int array;
  best_l : int array;
  probe_lb : int array;    (* preallocated probe scratch: bucket lower bounds *)
  probe_root : int array;  (* preallocated probe scratch: bucket root node ids *)
  q16 : i16ba;             (* 16-wide i16 query for the SIMD kernel; lanes 14,15 stay 0 *)
  dists : intba;           (* leaf distance output buffer (size leaf_size) *)
}

let create_scratch () =
  let q16 = make_i16 16 in
  for i = 0 to 15 do Bigarray.Array1.unsafe_set q16 i 0 done;
  {
    best_d = Array.make k max_int;
    best_l = Array.make k 0;
    probe_lb = Array.make 256 0;
    probe_root = Array.make 256 0;
    q16;
    dists = Bigarray.Array1.create Bigarray.int Bigarray.c_layout leaf_size;
  }

let[@inline] reset_scratch s =
  for i = 0 to k - 1 do
    s.best_d.(i) <- max_int;
    s.best_l.(i) <- 0
  done

let[@inline] insert s d l =
  if d < s.best_d.(k - 1) then begin
    let pos = ref (k - 1) in
    while !pos > 0 && d < s.best_d.(!pos - 1) do
      s.best_d.(!pos) <- s.best_d.(!pos - 1);
      s.best_l.(!pos) <- s.best_l.(!pos - 1);
      decr pos
    done;
    s.best_d.(!pos) <- d;
    s.best_l.(!pos) <- l
  end

let[@inline] lower_bound t node (q : int array) : int =
  let base = node * dim in
  let acc = ref 0 in
  for d = 0 to dim - 1 do
    let lo = Array.unsafe_get t.node_min (base + d) in
    let hi = Array.unsafe_get t.node_max (base + d) in
    let qd = Array.unsafe_get q d in
    let g = if qd < lo then lo - qd else if qd > hi then qd - hi else 0 in
    acc := !acc + g * g
  done;
  !acc

(* Compute all leaf distances with one SIMD call, then fold into the top-k.
   The SIMD kernel has no per-point early-abort, but it is far cheaper than the
   14-dim scalar loop on the full scans that dominate the hard-query tail (where
   [worst] is large so early-abort rarely fired anyway). [insert] still keeps
   only [dd < worst], so the top-k result is exact. *)
let[@inline] scan_leaf t s start len =
  simd_leaf_dists t.vecs s.q16 start len s.dists;
  for i = 0 to len - 1 do
    let dd = Bigarray.Array1.unsafe_get s.dists i in
    if dd < Array.unsafe_get s.best_d (k - 1) then
      insert s dd (Char.code (Bytes.unsafe_get t.labels (start + i)))
  done

(* Branch-and-bound over a subtree rooted at [root] (whose lower bound is
   [root_bound]). Recursive; the tree depth is ~log2(bucket/leaf). *)
let rec search_node t s root root_bound (q : int array) =
  if root_bound < s.best_d.(k - 1) then begin
    let left = Array.unsafe_get t.node_left root in
    if left < 0 then
      scan_leaf t s (Array.unsafe_get t.node_start root) (Array.unsafe_get t.node_len root)
    else begin
      let right = Array.unsafe_get t.node_right root in
      let lb = lower_bound t left q in
      let rb = lower_bound t right q in
      let near, near_b, far, far_b =
        if lb <= rb then (left, lb, right, rb) else (right, rb, left, lb)
      in
      search_node t s near near_b q;
      if far_b < s.best_d.(k - 1) then search_node t s far far_b q
    end
  end

(* Count frauds among the k nearest neighbours of [q].
   [exact = true]  : full branch-and-bound, no early-exit -> exact kNN.
   [exact = false] : early-exit once the home bucket yields 5 neighbours within
                     the confident radius; probes other buckets otherwise. *)
let fraud_count_with s t (q : int array) ~exact : int =
  reset_scratch s;
  (* Stage the query for the SIMD leaf kernel; lanes 14,15 stay 0. q values are
     in [-10000, 10000], well within i16. *)
  for d = 0 to dim - 1 do
    Bigarray.Array1.unsafe_set s.q16 d (Array.unsafe_get q d)
  done;
  let key = partition_key q in
  let primary = Array.unsafe_get t.roots key in
  if primary >= 0 then search_node t s primary (lower_bound t primary q) q;

  let confident = (not exact) && s.best_d.(k - 1) <= early_limit in
  if not confident then begin
    (* Probe remaining buckets ordered by bbox lower bound, pruning. Collect
       survivors into preallocated parallel arrays (zero allocation — the old
       list+List.sort path churned the minor heap on exactly the tail queries
       that miss early-exit, spiking p99 via GC). *)
    let worst = Array.unsafe_get s.best_d (k - 1) in
    let cnt = ref 0 in
    for kk = 0 to 255 do
      if kk <> key then begin
        let r = Array.unsafe_get t.roots kk in
        if r >= 0 then begin
          let lb = lower_bound t r q in
          if lb < worst then begin
            let i = !cnt in
            Array.unsafe_set s.probe_lb i lb;
            Array.unsafe_set s.probe_root i r;
            incr cnt
          end
        end
      end
    done;
    (* Insertion sort [0,cnt) by probe_lb ascending — search nearer buckets
       first so [worst] tightens early and prunes the rest. cnt is small. *)
    let n = !cnt in
    for i = 1 to n - 1 do
      let lb = Array.unsafe_get s.probe_lb i in
      let r = Array.unsafe_get s.probe_root i in
      let j = ref (i - 1) in
      while !j >= 0 && Array.unsafe_get s.probe_lb !j > lb do
        Array.unsafe_set s.probe_lb (!j + 1) (Array.unsafe_get s.probe_lb !j);
        Array.unsafe_set s.probe_root (!j + 1) (Array.unsafe_get s.probe_root !j);
        decr j
      done;
      Array.unsafe_set s.probe_lb (!j + 1) lb;
      Array.unsafe_set s.probe_root (!j + 1) r
    done;
    for i = 0 to n - 1 do
      let lb = Array.unsafe_get s.probe_lb i in
      if lb < s.best_d.(k - 1) then
        search_node t s (Array.unsafe_get s.probe_root i) lb q
    done
  end;

  let frauds = ref 0 in
  for i = 0 to k - 1 do
    frauds := !frauds + s.best_l.(i)
  done;
  !frauds

let fraud_count t q ~exact =
  fraud_count_with (create_scratch ()) t q ~exact

let[@inline] approved_of_count c = c < 3

(* --- serialization (mmap-friendly binary) ---

   Layout (little-endian):
     0    magic "KDFRAUD1" (8)
     8    version u32 (=1)
     12   n u32
     16   dim u32
     20   node_count u32
     24   (zero pad to 64)
     64   roots[256] i32
     ...  node_left[nc] i32, node_right[nc] i32, node_start[nc] i32, node_len[nc] i32
     ...  node_min[nc*dim] i16, node_max[nc*dim] i16
     ...  labels[n] u8
     (pad to 4096)
     vpos vecs[n*dim] i16        <- page-aligned so the server can mmap it

   On load: everything except [vecs] is small and is read into native arrays;
   [vecs] (the 84 MB hot table at 3M) is mmap'd zero-copy. *)

let magic = "KDFRAUD1"

let vecs_offset ~n ~node_count =
  let pos = 64 + 256 * 4 + node_count * 4 * 4 + node_count * dim * 2 * 2 + n in
  (pos + 4095) / 4096 * 4096

let save t ~path =
  let oc = Out_channel.open_bin path in
  Fun.protect ~finally:(fun () -> Out_channel.close oc) (fun () ->
    let nc = Array.length t.node_left in
    let b4 = Bytes.create 4 and b2 = Bytes.create 2 in
    let w32 v = Bytes.set_int32_le b4 0 (Int32.of_int v); Out_channel.output_bytes oc b4 in
    Out_channel.output_string oc magic;
    w32 1; w32 t.n; w32 dim; w32 nc;
    Out_channel.output_bytes oc (Bytes.make (64 - (8 + 4 * 4)) '\000');
    for i = 0 to 255 do w32 t.roots.(i) done;
    let warr a = for i = 0 to nc - 1 do w32 a.(i) done in
    warr t.node_left; warr t.node_right; warr t.node_start; warr t.node_len;
    (* node bboxes: write in bulk via a Bytes buffer *)
    let wi16_arr (a : int array) =
      let len = Array.length a in
      let buf = Bytes.create (len * 2) in
      for i = 0 to len - 1 do Bytes.set_int16_le buf (i * 2) a.(i) done;
      Out_channel.output_bytes oc buf
    in
    wi16_arr t.node_min; wi16_arr t.node_max;
    Out_channel.output_bytes oc (Bytes.sub t.labels 0 t.n);
    let pos = 64 + 256 * 4 + nc * 4 * 4 + nc * dim * 2 * 2 + t.n in
    let vpos = vecs_offset ~n:t.n ~node_count:nc in
    Out_channel.output_bytes oc (Bytes.make (vpos - pos) '\000');
    (* vecs in bulk *)
    let total = t.n * dim in
    let chunk = 1 lsl 20 in
    let buf = Bytes.create (min total chunk * 2) in
    let i = ref 0 in
    while !i < total do
      let m = min chunk (total - !i) in
      for j = 0 to m - 1 do
        Bytes.set_int16_le buf (j * 2) (Bigarray.Array1.unsafe_get t.vecs (!i + j))
      done;
      Out_channel.output_bytes oc (if m * 2 = Bytes.length buf then buf else Bytes.sub buf 0 (m * 2));
      i := !i + m
    done;
    ignore b2)

let load ~path : t =
  let ic = In_channel.open_bin path in
  let read n = let b = Bytes.create n in really_input ic b 0 n; b in
  let hdr = read 64 in
  if Bytes.sub_string hdr 0 8 <> magic then failwith "Knn.load: bad magic";
  if Int32.to_int (Bytes.get_int32_le hdr 8) <> 1 then failwith "Knn.load: bad version";
  let n = Int32.to_int (Bytes.get_int32_le hdr 12) in
  let fdim = Int32.to_int (Bytes.get_int32_le hdr 16) in
  if fdim <> dim then failwith "Knn.load: dim mismatch";
  let nc = Int32.to_int (Bytes.get_int32_le hdr 20) in
  let roots_b = read (256 * 4) in
  let roots = Array.init 256 (fun i -> Int32.to_int (Bytes.get_int32_le roots_b (i * 4))) in
  let read_i32_arr () =
    let b = read (nc * 4) in
    Array.init nc (fun i -> Int32.to_int (Bytes.get_int32_le b (i * 4)))
  in
  let node_left = read_i32_arr () in
  let node_right = read_i32_arr () in
  let node_start = read_i32_arr () in
  let node_len = read_i32_arr () in
  let read_i16_arr () =
    let b = read (nc * dim * 2) in
    Array.init (nc * dim) (fun i -> Bytes.get_int16_le b (i * 2))
  in
  let node_min = read_i16_arr () in
  let node_max = read_i16_arr () in
  let labels = read n in
  In_channel.close ic;
  let vpos = vecs_offset ~n ~node_count:nc in
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  let vecs =
    Bigarray.array1_of_genarray
      (Unix.map_file fd ~pos:(Int64.of_int vpos)
         Bigarray.int16_signed Bigarray.c_layout false [| n * dim |])
  in
  Unix.close fd;  (* mapping survives independently of the fd *)
  (* Prefault: touch one element per 4 KiB page so the whole vecs table is
     resident before serving. The hot path then takes zero minor page faults
     under load (page faults are a pure tail event — the #1 C entry sidesteps
     them by keeping the table in malloc'd RAM; we get the same by faulting it
     in now). 84 MB / 4 KiB ~= 21k touches, trivial. *)
  let total = n * dim in
  let stride = 4096 / 2 in            (* 2 bytes per i16 -> one touch per page *)
  let sink = ref 0 and i = ref 0 in
  while !i < total do
    sink := !sink + Bigarray.Array1.unsafe_get vecs !i;
    i := !i + stride
  done;
  ignore (Sys.opaque_identity !sink);
  { n; vecs; labels; node_left; node_right; node_start; node_len;
    node_min; node_max; roots }
