(* Read references JSON (gzipped via stdin pipe or plain file), build the exact
   partition+KD-tree index (Fraud.Knn) and write it to index.bin.

   Runs in the unconstrained build container, so the transient memory of the
   in-RAM build is fine; the runtime server only mmaps the result. *)

open Fraud

let in_path = ref "-"
let out_path = ref "index.bin"

let speclist = [
  "--in",  Arg.Set_string in_path,
    "input references.json path or '-' for stdin (default '-')";
  "--out", Arg.Set_string out_path, "output index.bin path (default index.bin)";
  (* accepted-but-ignored legacy IVF flags so existing invocations don't break *)
  "--c", Arg.Int ignore, "(ignored) legacy IVF cells";
  "--iters", Arg.Int ignore, "(ignored) legacy k-means iters";
  "--sample", Arg.Int ignore, "(ignored) legacy k-means sample";
  "--nprobe", Arg.Int ignore, "(ignored) legacy nprobe";
]

let now () = Unix.gettimeofday ()

let main () =
  Arg.parse speclist (fun _ -> ()) "build_index";
  let src : Refs_reader.source =
    if !in_path = "-" then Refs_reader.Stdin else Refs_reader.File !in_path
  in
  Printf.eprintf "[build_index] reading from %s\n%!"
    (if !in_path = "-" then "<stdin>" else !in_path);

  (* Read + quantize into a flat store. *)
  let t0 = now () in
  let recs = ref [] and count = ref 0 in
  Refs_reader.fold (fun () (v, l) ->
    recs := (v, l) :: !recs;
    incr count;
    if !count mod 500_000 = 0 then
      Printf.eprintf "[build_index] read %d in %.1fs\n%!" !count (now () -. t0)
  ) () src;
  let n = !count in
  let recs = Array.of_list (List.rev !recs) in
  Printf.eprintf "[build_index] %d refs read in %.1fs\n%!" n (now () -. t0);

  let dim = Knn.dim in
  let store = Array.make (max 1 (n * dim)) 0 in
  let labels = Array.make (max 1 n) 0 in
  Array.iteri (fun i (v, l) ->
    for d = 0 to dim - 1 do store.(i * dim + d) <- Knn.quantize v.(d) done;
    labels.(i) <- (match l with `Fraud -> 1 | `Legit -> 0)
  ) recs;

  let tb = now () in
  let idx = Knn.build ~n
    ~get:(fun p d -> Array.unsafe_get store (p * dim + d))
    ~label:(fun p -> Array.unsafe_get labels p) in
  Printf.eprintf "[build_index] KD index built in %.1fs (%d nodes)\n%!"
    (now () -. tb) (Array.length idx.Knn.node_left);

  Knn.save idx ~path:!out_path;
  let sz = (Unix.stat !out_path).Unix.st_size in
  Printf.eprintf "[build_index] wrote %s (%d MB)\n%!" !out_path (sz / (1024 * 1024))

let () = main ()
