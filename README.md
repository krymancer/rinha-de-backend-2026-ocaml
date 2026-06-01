# rinha-de-backend-2026-ocaml

OCaml entry for [Rinha de Backend 2026](https://github.com/zanfranceschi/rinha-de-backend-2026) — fraud detection via vector search over 3M reference vectors.

## Approach

The ground truth is **exact k-NN** (k=5, squared-Euclidean, threshold 0.6) over 3M
labeled 14-dim vectors, so the goal is exact detection at sub-millisecond p99.

- **Exact, lossless quantization.** Reference vectors are `round4` (4 decimals).
  Quantizing each dim with `SCALE = 10000` makes `round4(x)*10000` an exact
  integer, so integer squared distance equals the float distance × 10⁸ — the
  neighbour ordering is identical to the float ground truth. Detection is
  *exact*, not approximate (0 FP / 0 FN).
- **Partition + KD-tree.** Vectors are bucketed into ≤256 partitions by an 8-bit
  key over the discrete dimensions (last-tx presence, is_online, card_present,
  unknown_merchant, mcc bucket, amount-vs-avg, tx-count). Each bucket is a
  KD-tree (leaf 128, per-node bbox). Queries search the home bucket with
  branch-and-bound (bbox lower-bound pruning) and early-exit once 5 neighbours
  are within the confident radius, probing other buckets only when their bound
  beats the current 5th-best. Exact; microseconds per query.
- **Raw epoll server.** No framework: a single-threaded `epoll` loop (thin C
  stub — OCaml's `Unix.select` rebuilds fd_sets every call and caps throughput),
  fixed per-connection buffers, pipelined HTTP/1.1 parsing, pre-rendered
  responses, and a near-zero-allocation hot path. The mmap'd index keeps RSS at
  ~106 MB. This avoids the GC pauses and CFS-throttling tail latency of the
  previous `httpaf`+`lwt`+`nginx` stack.
- **Topology.** nginx (forward-only LB) + 2 API instances on a bridge network,
  within 1.0 CPU / 350 MB total, per the challenge architecture rules.

## Validated numbers (local, vs the challenge data-generator's exact ground truth)

| | 300k refs | 3M refs |
|---|---|---|
| detection errors (FP/FN) | 0 / 0 | 0 / 0 |
| kNN p99 (compute, scalar, no flambda) | 0.23 ms | 0.50 ms |
| server RSS (mmap index) | — | 106 MB |

Open-loop p99 under a 0.45-CPU cgroup cap, 3M index: **0.13–0.16 ms** at 2k–20k
RPS (no throttling). The grader peaks at ~900 RPS, so it runs comfortably in the
sub-millisecond regime.

## Layout

```
lib/
  detect.ml        payload → 14-dim vector (byte-level JSON parse)
  knn.ml           exact i16 partition+KD-tree index; build / save / load (mmap)
  epoll.ml         epoll bindings (over epoll_stubs.c)
  refs_reader.ml   streaming references.json parser
bin/
  build_index.ml   references.json → index.bin (KD format), offline
  bench_knn.ml     end-to-end validation + latency bench vs generator data
  server/main.ml   raw epoll HTTP server
```

(`lib/index.ml`, `index_io.ml` and the `bench*` tools are the earlier IVF
prototype, kept for reference.)

## Build & run

```sh
opam install --deps-only .   # dune yojson
dune build
dune test

# offline: build the index from the references file
gunzip -c references.json.gz | dune exec bin/build_index.exe -- --in - --out index.bin
dune exec bin/server/main.exe -- --index index.bin --port 9999

# docker (nginx LB + 2 api)
docker compose up --build
```

## License

MIT
