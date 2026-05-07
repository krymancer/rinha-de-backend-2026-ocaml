(* Read references JSON (gzipped via stdin pipe or plain file),
   build IVF index, write index.bin. *)

open Fraud

let in_path = ref "-"        (* "-" means stdin *)
let out_path = ref "index.bin"
let cells = ref 1024
let iters = ref 5
let sample = ref 200_000
let nprobe_default = ref 8

let speclist = [
  "--in",  Arg.Set_string in_path,
    "input references.json path or '-' for stdin (default '-')";
  "--out", Arg.Set_string out_path, "output index.bin path (default index.bin)";
  "--c",   Arg.Set_int cells, "IVF cells (default 1024)";
  "--iters", Arg.Set_int iters, "k-means iters (default 5)";
  "--sample", Arg.Set_int sample, "k-means sample size (default 200_000)";
  "--nprobe", Arg.Set_int nprobe_default, "default nprobe written into header (default 8)";
]

(* For simplicity in v1, we use a list buffer of (float array * label).
   Memory peak ~360-460 MB for 3M records — acceptable in unconstrained
   build container. *)

let read_all_records src =
  let recs = ref [] in
  let count = ref 0 in
  let t0 = Unix.gettimeofday () in
  Refs_reader.fold (fun () (vec, label) ->
    recs := (vec, label) :: !recs;
    incr count;
    if !count mod 100_000 = 0 then
      Printf.eprintf "[build_index] read %d records in %.2fs\n%!"
        !count (Unix.gettimeofday () -. t0)
  ) () src;
  Printf.eprintf "[build_index] total records: %d in %.2fs\n%!"
    !count (Unix.gettimeofday () -. t0);
  List.rev !recs, !count

let to_bigarrays (recs : (float array * Refs_reader.label) list) (n : int) =
  let dim = Index.dim in
  let vecs = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * dim) in
  let labels = Bytes.create n in
  List.iteri (fun i (v, l) ->
    for o = 0 to dim - 1 do
      Bigarray.Array1.unsafe_set vecs (i * dim + o) v.(o)
    done;
    Bytes.unsafe_set labels i (match l with `Fraud -> '\001' | `Legit -> '\000')
  ) recs;
  vecs, labels

let main () =
  Arg.parse speclist (fun _ -> ()) "build_index";
  let src : Refs_reader.source =
    if !in_path = "-" then Refs_reader.Stdin else Refs_reader.File !in_path
  in
  Printf.eprintf "[build_index] reading from %s, writing to %s\n%!"
    (if !in_path = "-" then "<stdin>" else !in_path) !out_path;
  let recs, n = read_all_records src in
  let vecs, labels = to_bigarrays recs n in
  Printf.eprintf "[build_index] building IVF c=%d iters=%d sample=%d\n%!"
    !cells !iters !sample;
  let t0 = Unix.gettimeofday () in
  let idx = Index.build ~c:!cells ~iters:!iters ~sample:!sample
              vecs n labels in
  Printf.eprintf "[build_index] IVF built in %.2fs\n%!"
    (Unix.gettimeofday () -. t0);

  let header = Index_io.plan_layout
    ~n:idx.n ~c:idx.c ~dim:Index.dim ~nprobe_default:!nprobe_default in
  (* labels in idx are a Bigarray; rebuild a Bytes buffer for save *)
  let labels_b = Bytes.create idx.n in
  for i = 0 to idx.n - 1 do
    Bytes.unsafe_set labels_b i (Bigarray.Array1.unsafe_get idx.labels i)
  done;
  Printf.eprintf "[build_index] writing %s (%d MB)\n%!"
    !out_path (header.file_size / (1024 * 1024));
  Index_io.save ~path:!out_path ~header
    ~centroids:idx.centroids
    ~cell_offsets:idx.cell_offsets
    ~vecs:idx.vecs
    ~labels:labels_b;
  Printf.eprintf "[build_index] done\n%!"

let () = main ()
