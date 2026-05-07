# Rinha 2026 — Real Data + mmap + Docker Design

**Date:** 2026-05-06
**Status:** Approved (brainstorm)
**Tracks README TODOs:** load real `references.json.gz`, `mmap` index in server, Dockerfile + compose.

## Goal

Replace synthetic startup with a pre-built, mmap'd `index.bin` and ship a rinha-compliant `docker-compose.yml` (nginx LB + 2× api, ≤1 CPU / 350 MB total).

## Decisions

| # | Decision | Rationale |
|---|---|---|
| 1 | fp32 index v1; int8 deferred to Phase 2 | fp32 already 3.5ms p99 dev box; ship correct first, quantize on real measurement |
| 2 | Bake `index.bin` into image at build time | `/ready` instant, deterministic cold start, latency is the score lever |
| 3 | Multi-stage Dockerfile produces index.bin and final runtime image in one build | Hermetic, reproducible for rinha submission |
| 4 | Memory split: nginx 32 / api 159 / api 159 = 350 MB | Relies on Docker overlay2 sharing the read-only image layer's `index.bin` across both api containers (same inode → single page cache footprint). If accounting double-counts in cgroup v2, fall back to int8 |

## Architecture

### Components

```
lib/
  index.ml           [modify]  add save_to_file, load_mmap; switch lists→flat layout
  index_io.ml        [new]     binary format, mmap helpers, header validation
bin/
  build_index.ml     [new]     references.json.gz → index.bin
  server/main.ml     [modify]  load via mmap, --index path arg
resources/                     [.gitignored — not committed]
  references.json.gz           50 MB, fetched from rinha repo
  mcc_risk.json                consistency-checked vs detect.ml
  normalization.json           consistency-checked vs detect.ml
docker/
  Dockerfile         [new]     multi-stage: build → index → runtime
  nginx.conf         [new]     round-robin api1/api2, no logic
docker-compose.yml   [new]
Makefile             [new]     fetch-data, build, index, docker, run
```

### Index file format (`index.bin`)

```
[ header — 4096 bytes, page-aligned ]
  u32  magic            = 0x49564631  ("IVF1")
  u32  version          = 1
  u64  n                                               (3_000_000)
  u32  c                                               (1024)
  u32  dim                                             (14)
  u32  nprobe_default                                  (8)
  u32  pad
  u64  centroids_off
  u64  cell_offsets_off
  u64  vecs_off                                        (page-aligned, 8192)
  u64  labels_off
  u64  file_size
  ... zero-pad to 4096

[ centroids ]      c * dim * 4   = 56 KB              fp32, c-major
[ cell_offsets ]   (c+1) * 8     = 8 KB               u64; offsets[i] = first vec idx of cell i; offsets[c] = n
[ pad to 4096 ]
[ vecs ]           n * dim * 4   = 168 MB             fp32, sorted by cell (contiguous within cell)
[ labels ]         n bytes       = 3 MB               1=fraud, 0=legit
```

Total ~171 MB. Single file, mmap'd `MAP_PRIVATE | PROT_READ`. Sequential scan within a cell is page-cache friendly.

### Build flow (offline, Docker stage 2)

```
references.json.gz
  │  gunzip stream
  ▼
JSON streaming parse → per record {vector:[14], label}
  │  copy into Bigarray.float32 (n*14)
  │  copy label byte (1=fraud, 0=legit) into Bytes (n)
  ▼
Index.build (kmeans c=1024, iters=5, sample=200_000)
  │  reorder vecs+labels into cell-sorted contiguous layout
  ▼
Index_io.save → index.bin
```

Indexer single-shot. Logs progress every 100k records. Target: <2 min on dev box.

### Runtime flow (server start)

```
main.ml startup:
  fd        = openfile "index.bin" RDONLY
  size      = file_size fd
  mmap region = MAP_PRIVATE PROT_READ
  validate header (magic, version, dims, file_size)
  bind Bigarray views over mmap'd region — no copy, no allocations
  /ready → 200 OK once mmap+validation done
  /fraud-score → vectorize → Index.search → JSON response
```

Header validated at startup; fail-loud on mismatch. Optional `madvise WILLNEED` on `vecs` segment to prefetch if cold-start latency matters.

## Rinha rule compliance

| Rule | Status |
|---|---|
| Port 9999, `GET /ready` (2xx), `POST /fraud-score` JSON | ✅ existing in `bin/server/main.ml` |
| 14-dim vector per DETECTION_RULES.md | ✅ `lib/detect.ml` |
| k=5 KNN, score = frauds/5 | ✅ `lib/index.ml:5,154-158` |
| `approved = score < 0.6` | ✅ `bin/server/main.ml:57` |
| MCC default 0.5 | ✅ `lib/detect.ml:23` |
| `-1` sentinel for null `last_transaction` | ✅ `lib/detect.ml:128-130` |
| LB + 2 API instances, round-robin | planned: nginx |
| LB applies no detection logic | planned: nginx pure proxy |
| docker-compose, public images, linux/amd64 | planned: Dockerfile `--platform=linux/amd64` |
| Sum ≤ 1.0 CPU / 350 MB | planned: 0.10+0.45+0.45 = 1.00, 32+159+159 = 350 |
| Bridge network only (no host/privileged) | planned: compose default |
| No reuse of test payloads as references | ✅ server uses only `references.json.gz` |

## docker-compose limits

```yaml
services:
  nginx:
    image: nginx:1.27-alpine
    ports: ["9999:9999"]
    volumes: ["./nginx.conf:/etc/nginx/nginx.conf:ro"]
    depends_on: [api1, api2]
    deploy:
      resources:
        limits: { cpus: "0.10", memory: "32M" }
  api1: &api
    image: rinha-fraud-ocaml:latest    # local dev tag; replaced with pushed Docker Hub tag for submission
    platform: linux/amd64
    expose: ["9999"]
    deploy:
      resources:
        limits: { cpus: "0.45", memory: "159M" }
  api2: *api
```

## nginx.conf (pure proxy)

```nginx
events { worker_connections 1024; }
http {
  upstream api {
    server api1:9999;
    server api2:9999;
    keepalive 32;
  }
  server {
    listen 9999;
    location / {
      proxy_pass http://api;
      proxy_http_version 1.1;
      proxy_set_header Connection "";
    }
  }
}
```

Default LB = round-robin per upstream module. No `if`, no rewrite, no logic.

## Error handling

- **Indexer**: fail loud on schema mismatch (missing field, wrong dim, n=0). Compare `normalization.json` constants vs `detect.ml` — fail build on drift.
- **Server startup**: validate header magic+version+dim+n+file_size. mmap fail → exit non-zero (rinha health check fails fast — better than serving garbage).
- **Per-request**: parse failure → 400 (existing `bin/server/main.ml:63-65`). Vector NaN/inf clamped in `detect.ml`. Unknown MCC → 0.5.
- **No silent fallbacks** — `vectorize` raises on missing fields, surfaces as 400.

## Testing strategy

**Unit (TDD):**
- `Index_io.save` then `load_mmap` round-trip on small synthetic (n=1000, c=8) — vecs/labels/centroids byte-equal.
- Header validation rejects bad magic, wrong version, wrong dim, truncated file.
- `build_index` on `resources/example-references.json` (32 KB excerpt) → expected n, expected centroid count.

**Integration:**
- Build `index.bin` from `example-references.json` (small).
- Boot server with that index, `curl /ready` → 200, `curl /fraud-score` with `example-payloads.json` records → valid `{approved, fraud_score}` shape, score in [0,1].

**End-to-end (post-Docker):**
- `docker compose up`, hit `localhost:9999/ready` → 200.
- Round-robin verify: 10 requests, both api1+api2 logs show traffic.
- Memory: `docker stats` after warmup, all containers under their limits.

**Out of scope (YAGNI):** dev-box load testing (Ryzen ≠ Mac Mini, not representative); recall measurement on real data (existing `bin/bench_ivf.ml` covers IVF quality).

## Phase 2 (deferred)

Triggered if Phase 1 measurement shows p99 > 5 ms on Mac-Mini-class hardware OR memory accounting blows the 350 MB limit:

- int8 quantization of `vecs` (4× smaller, leverages existing `bin/bench_int8.ml`).
- Per-cell residual encoding for tighter quantization.
- Re-tune `c` (cells) and `nprobe` defaults on real data.
