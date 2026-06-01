(* End-to-end validation + latency bench for Fraud.Knn against the challenge's
   own data-generator output (exact brute-force ground truth).

   Usage: bench_knn <refs.json> <payloads.json> [--exact]

   refs.json     : array of {"vector":[..14..],"label":"fraud"|"legit"}
   payloads.json : {"entries":[{"request":{..},"expected_approved":bool,..}]}

   Reports: build time, detection errors (FP/FN) vs expected_approved, and the
   per-query latency distribution (p50/p99/p999/max) of the kNN itself. *)

open Fraud

let dim = Knn.dim

let exact = ref false
let args = ref []
let () =
  Arg.parse [ "--exact", Arg.Set exact, "use exact mode (no early-exit)" ]
    (fun a -> args := a :: !args) "bench_knn <refs.json> <payloads.json>"
let refs_path, pay_path =
  match List.rev !args with
  | [ r; p ] -> r, p
  | _ -> prerr_endline "need <refs.json> <payloads.json>"; exit 2

let now () = Unix.gettimeofday ()

(* ---- load + quantize refs ---- *)
let () = Printf.printf "[bench] reading refs from %s\n%!" refs_path
let t0 = now ()
let recs =
  let acc = ref [] in
  Refs_reader.fold (fun () (v, l) -> acc := (v, l) :: !acc) () (Refs_reader.File refs_path);
  Array.of_list (List.rev !acc)
let n = Array.length recs
let () = Printf.printf "[bench] %d refs read in %.2fs\n%!" n (now () -. t0)

let store = Array.make (n * dim) 0
let labels = Array.make n 0
let () =
  Array.iteri
    (fun i (v, l) ->
      for d = 0 to dim - 1 do store.(i * dim + d) <- Knn.quantize v.(d) done;
      labels.(i) <- (match l with `Fraud -> 1 | `Legit -> 0))
    recs

(* ---- build index ---- *)
let tb = now ()
let idx = Knn.build ~n ~get:(fun p d -> store.(p * dim + d)) ~label:(fun p -> labels.(p))
let () = Printf.printf "[bench] index built in %.2fs (mode=%s)\n%!"
    (now () -. tb) (if !exact then "exact" else "fast")

(* ---- load payloads ---- *)
let () = Printf.printf "[bench] reading payloads from %s\n%!" pay_path
let tp = now ()
let root = Yojson.Safe.from_file pay_path
let entries = match Detect.field root "entries" with `List xs -> Array.of_list xs | _ -> [||]
let nq = Array.length entries
let () = Printf.printf "[bench] %d payloads read in %.2fs\n%!" nq (now () -. tp)

(* Pre-vectorize + quantize all queries so the timed loop is pure kNN. *)
let queries = Array.make nq [||]
let expected = Array.make nq true
let () =
  Array.iteri
    (fun i e ->
      let req = Detect.field e "request" in
      let v = Detect.vectorize req in
      queries.(i) <- Knn.quantize_vec v;
      expected.(i) <- (match Detect.field e "expected_approved" with `Bool b -> b | _ -> true))
    entries

(* ---- run + time ---- *)
let scratch = Knn.create_scratch ()
let lat = Array.make nq 0.0
let fp = ref 0 and fn = ref 0 and tp_ = ref 0 and tn = ref 0
let () =
  (* warmup *)
  for i = 0 to min (nq - 1) 4095 do
    ignore (Knn.fraud_count_with scratch idx queries.(i) ~exact:!exact)
  done;
  for i = 0 to nq - 1 do
    let a = now () in
    let c = Knn.fraud_count_with scratch idx queries.(i) ~exact:!exact in
    let b = now () in
    lat.(i) <- (b -. a) *. 1e6;  (* microseconds *)
    let approved = Knn.approved_of_count c in
    let exp = expected.(i) in
    if approved && exp then incr tn
    else if (not approved) && (not exp) then incr tp_
    else if approved && (not exp) then incr fn   (* fraud approved = false negative *)
    else incr fp                                  (* legit denied = false positive *)
  done

let () =
  Array.sort compare lat;
  let pct p = lat.(min (nq - 1) (int_of_float (p *. float_of_int nq))) in
  let sum = Array.fold_left ( +. ) 0.0 lat in
  let errors = !fp + !fn in
  let e_weighted = !fp + 3 * !fn in
  Printf.printf "\n=== DETECTION (vs exact ground truth) ===\n";
  Printf.printf "  TP=%d TN=%d FP=%d FN=%d  errors=%d / %d  (%.3f%%)\n"
    !tp_ !tn !fp !fn errors nq (100. *. float_of_int errors /. float_of_int nq);
  Printf.printf "  weighted E = 1*FP+3*FN = %d\n" e_weighted;
  Printf.printf "\n=== kNN LATENCY (compute only, microseconds) ===\n";
  Printf.printf "  mean=%.2f  p50=%.2f  p90=%.2f  p99=%.2f  p99.9=%.2f  max=%.2f\n"
    (sum /. float_of_int nq) (pct 0.50) (pct 0.90) (pct 0.99) (pct 0.999) lat.(nq - 1);
  Printf.printf "  throughput ~= %.0f queries/sec/core\n" (float_of_int nq /. (sum /. 1e6))
