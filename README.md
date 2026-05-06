# rinha-de-backend-2026-ocaml

OCaml entry for [Rinha de Backend 2026](https://github.com/zanfranceschi/rinha-de-backend-2026) — fraud detection via vector search over 3M reference vectors.

Status: **work in progress**.

## Approach

- **Language**: OCaml 5, native compile, no SIMD intrinsics (yet).
- **Search**: IVF-flat with k-means clustering (1024 cells, nprobe=8 default). Pure OCaml, `Bigarray.float32` storage.
- **HTTP**: `httpaf` + `lwt`, two endpoints on `:9999` per spec — `GET /ready`, `POST /fraud-score`.
- **Data**: index pre-built offline; both API instances `mmap` the same `index.bin` for page sharing under the 350 MB total memory budget. *(planned)*

## Current numbers (Ryzen 5 5600, single thread, synthetic uniform 3M×14 fp32)

| approach | p99 latency |
|---|---|
| brute force | 28 ms |
| IVF nprobe=1 | 0.7 ms |
| IVF nprobe=4 | 1.9 ms |
| IVF nprobe=8 | 3.5 ms (recall@5 = 0.90 on worst-case uniform data) |

Test box (Mac Mini Late 2014) is roughly 2–3× slower; latency score expected in the 2000+ range.

## Layout

```
lib/
  detect.ml      payload → 14-dim float vector (normalize, ISO date, mcc_risk)
  index.ml       IVF-flat: build, search
bin/
  bench.ml       brute-force kNN bench
  bench_int8.ml  int8 quantize bench
  bench_ivf.ml   IVF kNN bench (recall + latency vs nprobe sweep)
  server/main.ml httpaf server
```

## Build & run

```sh
opam install --deps-only .
dune build
dune exec bin/server/main.exe -- --n 3000000   # synthetic data, will switch to real soon
```

## TODO

- [ ] Load real `references.json.gz` and serialize to binary `index.bin`
- [ ] `mmap` index in server
- [ ] `Dockerfile` + `docker-compose.yml` (nginx LB + 2× api)
- [ ] Verify p99 on test-box-class hardware
- [ ] Tune nprobe based on real-data recall

## License

MIT
