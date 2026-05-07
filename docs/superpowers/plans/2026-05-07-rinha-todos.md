# Rinha 2026 — Real Data + mmap + Docker + CI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the synthetic startup with a pre-built mmap'd `index.bin` and ship a rinha-compliant `docker-compose.yml` (nginx LB + 2× api, ≤1 CPU / 350 MB) plus a GitHub Actions workflow that publishes the image to `ghcr.io`.

**Architecture:** `lib/index.ml` is refactored to a flat cell-major layout so the in-memory and on-disk shapes match. A new `lib/index_io.ml` defines the binary format and mmap loader. A new `bin/build_index.ml` reads `references.json.gz` (from rinha repo, gunzip'd via stdin) into Bigarrays, runs `Index.build`, and writes `index.bin`. The server uses `Index_io.load_mmap` instead of `build_synth_index`. A multi-stage Dockerfile produces the runtime image with `index.bin` baked in; CI builds and pushes to `ghcr.io/<owner>/<repo>:latest`.

**Tech Stack:** OCaml 5, dune 3.0, Bigarray, `Unix.map_file` mmap, Yojson, httpaf+lwt, alcotest, gzip (external `gunzip`), nginx, Docker multi-stage, GitHub Actions, GHCR.

**Source spec:** `docs/superpowers/specs/2026-05-06-rinha-todos-design.md`

---

## File map

| Path | New / Modify | Responsibility |
|---|---|---|
| `lib/index.ml` | **Modify** | Refactor `t` to flat `cell_offsets`+sorted-vecs layout. `build` produces same shape on-disk uses. `fraud_score` reads via offsets. |
| `lib/index_io.ml` | **New** | Binary format constants, `header` record, `plan_layout`, `save`, `load_header`, `load_mmap`. |
| `lib/dune` | **Modify** | Add `bigarray` (vendored in stdlib for OCaml ≥ 4.07 but explicit is fine), keep existing libs. |
| `bin/build_index.ml` | **New** | Streaming JSON read → Bigarrays → `Index.build` → `Index_io.save`. CLI: `--in <path|-> --out <path> --c <int> --iters <int>`. |
| `bin/dune` | **Modify** | Register `build_index` executable. |
| `bin/server/main.ml` | **Modify** | Replace synth builder with `Index_io.load_mmap`. New CLI flag `--index <path>` (default `/app/index.bin`). |
| `tests/dune` | **New** | alcotest test runner. |
| `tests/test_index_io.ml` | **New** | Round-trip + header validation tests. |
| `tests/test_index_build.ml` | **New** | Build on `example-references.json` excerpt. |
| `tests/fixtures/example-references.json` | **New** | Fetched once from rinha repo (32 KB). |
| `tests/fixtures/example-payloads.json` | **New** | Fetched once from rinha repo (32 KB). |
| `Makefile` | **New** | `fetch-data`, `fetch-fixtures`, `build`, `test`, `index`, `docker-build`, `docker-up`. |
| `.gitignore` | **Modify** | Add `index.bin`, ensure resources/ stays ignored, allow `tests/fixtures/*.json`. |
| `Dockerfile` | **New** | Multi-stage: opam build → indexer run → minimal runtime image. |
| `nginx.conf` | **New** | Round-robin upstream, pure proxy. |
| `docker-compose.yml` | **New** | nginx + api1 + api2, limits sum to 1.0 CPU / 350 MB. |
| `.github/workflows/build-image.yml` | **New** | On push to `main`: build image, push to `ghcr.io/<owner>/<repo>:latest` and `:<sha>`. |

---

## Locked types (do not drift across tasks)

```ocaml
(* lib/index_io.ml *)
val magic : int32                            (* 0x49564631l = "IVF1" *)
val version : int32                          (* 1l *)
val header_size : int                        (* 4096 *)
val page_size : int                          (* 4096 *)

type header = {
  n : int;
  c : int;
  dim : int;                                 (* 14 *)
  nprobe_default : int;                      (* 8 *)
  centroids_off : int;
  cell_offsets_off : int;
  vecs_off : int;
  labels_off : int;
  file_size : int;
}

val plan_layout :
  n:int -> c:int -> dim:int -> nprobe_default:int -> header

val save :
  path:string ->
  header:header ->
  centroids:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  cell_offsets:(int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  vecs:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  labels:Bytes.t ->
  unit

val load_header : string -> header

type mmap_views = {
  fd : Unix.file_descr;
  centroids    : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  cell_offsets : (int64, Bigarray.int64_elt,  Bigarray.c_layout) Bigarray.Array1.t;
  vecs         : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  labels       : (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

val load_mmap : string -> header * mmap_views
```

```ocaml
(* lib/index.ml — refactored type *)
type vecs = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type labels_ba = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type cell_offs = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  vecs : vecs;            (* n * dim, sorted cell-major *)
  n : int;
  labels : labels_ba;     (* n entries, sorted same as vecs; '\001' fraud / '\000' legit *)
  centroids : vecs;       (* c * dim *)
  c : int;
  cell_offsets : cell_offs; (* length c + 1; entry i = first vec idx in cell i; entry c = n *)
}

(* Construct from raw input arrays (sorts internally) *)
val build :
  ?c:int -> ?iters:int -> ?sample:int ->
  vecs -> int -> Bytes.t -> t

(* Construct from already-flat segments (used by load_mmap path) *)
val of_segments :
  vecs:vecs -> n:int -> labels:labels_ba ->
  centroids:vecs -> c:int -> cell_offsets:cell_offs -> t

val fraud_score : t -> float array -> nprobe:int -> float
```

---

## Phase 0 — Prep

### Task 1: Fetch test fixtures and add Makefile

**Files:**
- Create: `Makefile`
- Create: `tests/fixtures/example-references.json` (downloaded)
- Create: `tests/fixtures/example-payloads.json` (downloaded)
- Modify: `.gitignore`

- [ ] **Step 1: Update `.gitignore` to track fixtures but ignore real data + index**

```diff
 _build/
 *.install
 *.swp
 .merlin
 .vscode/
 .DS_Store
-*.bin
+# index artifacts
+index.bin
+resources/index.bin
 references.json.gz
 references.json
+# do NOT ignore tests/fixtures/*.json — they are committed test data
```

Replace `.gitignore` with the above. Note: removing the broad `*.bin` is intentional; we name the artifact specifically.

- [ ] **Step 2: Create `Makefile`**

```make
.PHONY: build test fetch-fixtures fetch-data index docker-build docker-up clean

OWNER ?= $(shell git config --get remote.origin.url | sed -nE 's#.*[:/]([^/]+)/[^/]+$$#\1#p')
IMAGE ?= ghcr.io/$(OWNER)/rinha-de-backend-2026-ocaml

build:
	dune build

test: fetch-fixtures
	dune runtest

fetch-fixtures:
	@mkdir -p tests/fixtures
	@test -s tests/fixtures/example-references.json || \
		curl -fsSL -o tests/fixtures/example-references.json \
			https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/example-references.json
	@test -s tests/fixtures/example-payloads.json || \
		curl -fsSL -o tests/fixtures/example-payloads.json \
			https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/example-payloads.json

fetch-data:
	@mkdir -p resources
	@test -s resources/references.json.gz || \
		curl -fsSL -o resources/references.json.gz \
			https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/references.json.gz

index: build fetch-data
	gunzip -c resources/references.json.gz | _build/default/bin/build_index.exe --in - --out index.bin

docker-build:
	docker build -t $(IMAGE):latest --platform=linux/amd64 .

docker-up:
	docker compose up --build

clean:
	dune clean
	rm -f index.bin
```

- [ ] **Step 3: Run `make fetch-fixtures` and verify**

Run: `make fetch-fixtures && ls -la tests/fixtures/`
Expected: two files, `example-references.json` and `example-payloads.json`, sizes ~32 KB each.

- [ ] **Step 4: Commit**

```bash
git add Makefile .gitignore tests/fixtures/example-references.json tests/fixtures/example-payloads.json
git commit -m "chore: Makefile + rinha example fixtures"
```

---

### Task 2: Set up alcotest harness

**Files:**
- Create: `tests/dune`
- Create: `tests/test_smoke.ml`

- [ ] **Step 1: Create `tests/dune`**

```
(test
 (name test_smoke)
 (libraries fraud alcotest))
```

(We will add more test executables below; each gets its own `(test)` stanza.)

- [ ] **Step 2: Create the smallest possible failing test in `tests/test_smoke.ml`**

```ocaml
let test_alcotest_runs () =
  Alcotest.(check int) "trivial" 1 1

let () =
  Alcotest.run "smoke" [
    "smoke", [
      Alcotest.test_case "alcotest works" `Quick test_alcotest_runs;
    ];
  ]
```

- [ ] **Step 3: Run tests to verify the harness works**

Run: `dune runtest`
Expected: PASS, `1 test run` summary line.

- [ ] **Step 4: Commit**

```bash
git add tests/dune tests/test_smoke.ml
git commit -m "test: bootstrap alcotest harness"
```

---

## Phase 1 — Index file format + I/O (TDD)

### Task 3: Header layout + `plan_layout`

**Files:**
- Create: `lib/index_io.ml`
- Create: `tests/test_index_io.ml`
- Modify: `tests/dune`

- [ ] **Step 1: Add a failing test for `plan_layout`**

Append to `tests/dune`:

```
(test
 (name test_index_io)
 (libraries fraud alcotest))
```

(Replace the file so it now contains both `(test)` stanzas — alcotest dune blocks are not additive in a single block, so they must be separate `(test)` stanzas.)

Final `tests/dune`:

```
(tests
 (names test_smoke test_index_io)
 (libraries fraud alcotest))
```

Create `tests/test_index_io.ml`:

```ocaml
open Fraud

let test_plan_layout_basic () =
  let h = Index_io.plan_layout ~n:3_000_000 ~c:1024 ~dim:14 ~nprobe_default:8 in
  Alcotest.(check int) "n" 3_000_000 h.n;
  Alcotest.(check int) "c" 1024 h.c;
  Alcotest.(check int) "dim" 14 h.dim;
  Alcotest.(check int) "nprobe_default" 8 h.nprobe_default;
  Alcotest.(check int) "centroids_off is page-aligned" 0 (h.centroids_off mod 4096);
  Alcotest.(check int) "vecs_off is page-aligned" 0 (h.vecs_off mod 4096);
  Alcotest.(check bool) "centroids before cell_offsets"
    true (h.centroids_off < h.cell_offsets_off);
  Alcotest.(check bool) "cell_offsets before vecs"
    true (h.cell_offsets_off < h.vecs_off);
  Alcotest.(check bool) "vecs before labels"
    true (h.vecs_off < h.labels_off);
  Alcotest.(check int) "file_size matches labels segment end"
    (h.labels_off + h.n) h.file_size

let () =
  Alcotest.run "index_io" [
    "layout", [
      Alcotest.test_case "plan_layout 3M/1024/14" `Quick test_plan_layout_basic;
    ];
  ]
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `dune runtest`
Expected: FAIL — `Index_io` module not found (or `plan_layout` not defined).

- [ ] **Step 3: Implement `lib/index_io.ml` (header types + `plan_layout` only)**

```ocaml
(* Binary format and mmap helpers for index.bin. *)

let magic : int32 = 0x49564631l   (* "IVF1" little-endian *)
let version : int32 = 1l
let header_size = 4096
let page_size = 4096

type header = {
  n : int;
  c : int;
  dim : int;
  nprobe_default : int;
  centroids_off : int;
  cell_offsets_off : int;
  vecs_off : int;
  labels_off : int;
  file_size : int;
}

let[@inline] align_up x a = (x + a - 1) / a * a

let plan_layout ~n ~c ~dim ~nprobe_default =
  let centroids_off = header_size in
  let cell_offsets_off = centroids_off + c * dim * 4 in
  let vecs_off = align_up (cell_offsets_off + (c + 1) * 8) page_size in
  let labels_off = vecs_off + n * dim * 4 in
  let file_size = labels_off + n in
  { n; c; dim; nprobe_default;
    centroids_off; cell_offsets_off; vecs_off; labels_off; file_size }

(* placeholders, filled in subsequent tasks *)
let save ~path:_ ~header:_ ~centroids:_ ~cell_offsets:_ ~vecs:_ ~labels:_ =
  failwith "Index_io.save: not yet implemented"

let load_header _path : header = failwith "Index_io.load_header: not yet implemented"

type mmap_views = {
  fd : Unix.file_descr;
  centroids    : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  cell_offsets : (int64, Bigarray.int64_elt,  Bigarray.c_layout) Bigarray.Array1.t;
  vecs         : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  labels       : (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let load_mmap _path : header * mmap_views =
  failwith "Index_io.load_mmap: not yet implemented"
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `dune runtest`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add lib/index_io.ml tests/dune tests/test_index_io.ml
git commit -m "feat(index_io): header type + plan_layout"
```

---

### Task 4: `save` writes a valid header + segments

**Files:**
- Modify: `lib/index_io.ml`
- Modify: `tests/test_index_io.ml`

- [ ] **Step 1: Add a failing round-trip test**

Append to `tests/test_index_io.ml` (above the `()` runner; rewrite the runner to register both cases):

```ocaml
let with_tmp_file f =
  let path = Filename.temp_file "idx_" ".bin" in
  let r =
    try f path
    with e -> (try Sys.remove path with _ -> ()); raise e
  in
  (try Sys.remove path with _ -> ());
  r

let mk_centroids ~c ~dim =
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (c * dim) in
  for i = 0 to c * dim - 1 do
    Bigarray.Array1.set ba i (float_of_int i *. 0.001)
  done;
  ba

let mk_offsets ~c ~n =
  let ba = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout (c + 1) in
  let per = n / c in
  for i = 0 to c - 1 do
    Bigarray.Array1.set ba i (Int64.of_int (i * per))
  done;
  Bigarray.Array1.set ba c (Int64.of_int n);
  ba

let mk_vecs ~n ~dim =
  let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * dim) in
  for i = 0 to n * dim - 1 do
    Bigarray.Array1.set ba i (float_of_int i *. 0.0001)
  done;
  ba

let mk_labels ~n =
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set b i (if i mod 7 = 0 then '\001' else '\000')
  done;
  b

let test_save_writes_correct_size () =
  with_tmp_file (fun path ->
    let n = 1000 and c = 8 and dim = 14 in
    let header = Index_io.plan_layout ~n ~c ~dim ~nprobe_default:4 in
    let centroids    = mk_centroids ~c ~dim in
    let cell_offsets = mk_offsets ~c ~n in
    let vecs         = mk_vecs ~n ~dim in
    let labels       = mk_labels ~n in
    Index_io.save ~path ~header ~centroids ~cell_offsets ~vecs ~labels;
    let st = Unix.stat path in
    Alcotest.(check int) "file size matches header.file_size"
      header.file_size st.st_size)
```

Update the runner:

```ocaml
let () =
  Alcotest.run "index_io" [
    "layout", [
      Alcotest.test_case "plan_layout 3M/1024/14" `Quick test_plan_layout_basic;
    ];
    "save", [
      Alcotest.test_case "save writes correct size" `Quick test_save_writes_correct_size;
    ];
  ]
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `dune runtest`
Expected: FAIL with `Index_io.save: not yet implemented`.

- [ ] **Step 3: Implement `save`**

Replace the placeholder `save` in `lib/index_io.ml`:

```ocaml
let write_u32_le oc v =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 v;
  Out_channel.output_bytes oc b

let write_u64_le oc v =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int v);
  Out_channel.output_bytes oc b

let write_zeros oc n =
  let chunk = Bytes.make 4096 '\000' in
  let remaining = ref n in
  while !remaining >= 4096 do
    Out_channel.output_bytes oc chunk;
    remaining := !remaining - 4096
  done;
  if !remaining > 0 then
    Out_channel.output_bytes oc (Bytes.sub chunk 0 !remaining)

(* Write a Bigarray.float32 Array1 as raw little-endian bytes. *)
let write_f32_array oc (ba : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t) =
  let n = Bigarray.Array1.dim ba in
  let buf = Bytes.create 4 in
  for i = 0 to n - 1 do
    let bits = Int32.bits_of_float (Bigarray.Array1.unsafe_get ba i) in
    Bytes.set_int32_le buf 0 bits;
    Out_channel.output_bytes oc buf
  done

let write_i64_array oc (ba : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t) =
  let n = Bigarray.Array1.dim ba in
  let buf = Bytes.create 8 in
  for i = 0 to n - 1 do
    Bytes.set_int64_le buf 0 (Bigarray.Array1.unsafe_get ba i);
    Out_channel.output_bytes oc buf
  done

let save ~path ~header ~centroids ~cell_offsets ~vecs ~labels =
  let oc = Out_channel.open_bin path in
  let close () = Out_channel.close oc in
  Fun.protect ~finally:close (fun () ->
    (* Header *)
    write_u32_le oc magic;
    write_u32_le oc version;
    write_u64_le oc header.n;
    write_u32_le oc (Int32.of_int header.c);
    write_u32_le oc (Int32.of_int header.dim);
    write_u32_le oc (Int32.of_int header.nprobe_default);
    write_u32_le oc 0l;                              (* pad *)
    write_u64_le oc header.centroids_off;
    write_u64_le oc header.cell_offsets_off;
    write_u64_le oc header.vecs_off;
    write_u64_le oc header.labels_off;
    write_u64_le oc header.file_size;
    let written = 4 + 4 + 8 + 4 + 4 + 4 + 4 + 8 * 5 in
    write_zeros oc (header_size - written);

    (* Centroids — already at centroids_off = header_size *)
    write_f32_array oc centroids;
    let pos = header.centroids_off + header.c * header.dim * 4 in
    assert (pos = header.cell_offsets_off);

    (* Cell offsets *)
    write_i64_array oc cell_offsets;
    let pos = pos + (header.c + 1) * 8 in
    write_zeros oc (header.vecs_off - pos);

    (* Vecs *)
    write_f32_array oc vecs;
    let pos = header.vecs_off + header.n * header.dim * 4 in
    assert (pos = header.labels_off);

    (* Labels *)
    Out_channel.output_bytes oc (Bytes.sub labels 0 header.n))
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `dune runtest`
Expected: PASS, both layout + save cases.

- [ ] **Step 5: Commit**

```bash
git add lib/index_io.ml tests/test_index_io.ml
git commit -m "feat(index_io): save writes correctly sized index.bin"
```

---

### Task 5: `load_header` validates and reads the header

**Files:**
- Modify: `lib/index_io.ml`
- Modify: `tests/test_index_io.ml`

- [ ] **Step 1: Add a failing round-trip test for header**

Append to `tests/test_index_io.ml` (before the runner):

```ocaml
let test_save_load_header_roundtrip () =
  with_tmp_file (fun path ->
    let n = 1000 and c = 8 and dim = 14 in
    let header = Index_io.plan_layout ~n ~c ~dim ~nprobe_default:4 in
    Index_io.save ~path ~header
      ~centroids:(mk_centroids ~c ~dim)
      ~cell_offsets:(mk_offsets ~c ~n)
      ~vecs:(mk_vecs ~n ~dim)
      ~labels:(mk_labels ~n);
    let h2 = Index_io.load_header path in
    Alcotest.(check int) "n" header.n h2.n;
    Alcotest.(check int) "c" header.c h2.c;
    Alcotest.(check int) "dim" header.dim h2.dim;
    Alcotest.(check int) "nprobe_default" header.nprobe_default h2.nprobe_default;
    Alcotest.(check int) "centroids_off" header.centroids_off h2.centroids_off;
    Alcotest.(check int) "cell_offsets_off" header.cell_offsets_off h2.cell_offsets_off;
    Alcotest.(check int) "vecs_off" header.vecs_off h2.vecs_off;
    Alcotest.(check int) "labels_off" header.labels_off h2.labels_off;
    Alcotest.(check int) "file_size" header.file_size h2.file_size)

let test_load_header_bad_magic () =
  with_tmp_file (fun path ->
    let oc = Out_channel.open_bin path in
    Out_channel.output_string oc (String.make 4096 '\000');
    Out_channel.close oc;
    Alcotest.check_raises "bad magic raises"
      (Failure "Index_io.load_header: bad magic")
      (fun () -> ignore (Index_io.load_header path)))
```

Add cases to runner:

```ocaml
    "load_header", [
      Alcotest.test_case "save/load round-trip" `Quick test_save_load_header_roundtrip;
      Alcotest.test_case "rejects bad magic"     `Quick test_load_header_bad_magic;
    ];
```

- [ ] **Step 2: Run tests to verify failure**

Run: `dune runtest`
Expected: FAIL with `Index_io.load_header: not yet implemented`.

- [ ] **Step 3: Implement `load_header`**

Replace the placeholder `load_header` in `lib/index_io.ml`:

```ocaml
let read_exact ic n =
  let b = Bytes.create n in
  really_input ic b 0 n;
  b

let load_header path =
  let ic = In_channel.open_bin path in
  let close () = In_channel.close ic in
  Fun.protect ~finally:close (fun () ->
    let h = read_exact ic header_size in
    let m = Bytes.get_int32_le h 0 in
    if m <> magic then failwith "Index_io.load_header: bad magic";
    let v = Bytes.get_int32_le h 4 in
    if v <> version then failwith "Index_io.load_header: bad version";
    let n              = Int64.to_int (Bytes.get_int64_le h 8) in
    let c              = Int32.to_int (Bytes.get_int32_le h 16) in
    let dim            = Int32.to_int (Bytes.get_int32_le h 20) in
    let nprobe_default = Int32.to_int (Bytes.get_int32_le h 24) in
    (* skip pad u32 at offset 28 *)
    let centroids_off    = Int64.to_int (Bytes.get_int64_le h 32) in
    let cell_offsets_off = Int64.to_int (Bytes.get_int64_le h 40) in
    let vecs_off         = Int64.to_int (Bytes.get_int64_le h 48) in
    let labels_off       = Int64.to_int (Bytes.get_int64_le h 56) in
    let file_size        = Int64.to_int (Bytes.get_int64_le h 64) in
    { n; c; dim; nprobe_default;
      centroids_off; cell_offsets_off; vecs_off; labels_off; file_size })
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `dune runtest`
Expected: PASS, all cases.

- [ ] **Step 5: Commit**

```bash
git add lib/index_io.ml tests/test_index_io.ml
git commit -m "feat(index_io): load_header validates and round-trips"
```

---

### Task 6: `load_mmap` returns typed Bigarray views

**Files:**
- Modify: `lib/index_io.ml`
- Modify: `tests/test_index_io.ml`

- [ ] **Step 1: Add failing mmap round-trip test**

Append to `tests/test_index_io.ml`:

```ocaml
let test_load_mmap_roundtrip () =
  with_tmp_file (fun path ->
    let n = 1000 and c = 8 and dim = 14 in
    let header = Index_io.plan_layout ~n ~c ~dim ~nprobe_default:4 in
    let centroids = mk_centroids ~c ~dim in
    let offs      = mk_offsets ~c ~n in
    let vecs      = mk_vecs ~n ~dim in
    let lbls      = mk_labels ~n in
    Index_io.save ~path ~header ~centroids
      ~cell_offsets:offs ~vecs ~labels:lbls;
    let h2, views = Index_io.load_mmap path in
    Alcotest.(check int) "header n" n h2.n;
    (* Spot-check a few centroid values *)
    Alcotest.(check (float 1e-6)) "centroid[0]"
      (Bigarray.Array1.get centroids 0)
      (Bigarray.Array1.get views.centroids 0);
    Alcotest.(check (float 1e-6)) "centroid[c*dim-1]"
      (Bigarray.Array1.get centroids (c * dim - 1))
      (Bigarray.Array1.get views.centroids (c * dim - 1));
    (* Spot-check vecs *)
    Alcotest.(check (float 1e-6)) "vec[0]"
      (Bigarray.Array1.get vecs 0)
      (Bigarray.Array1.get views.vecs 0);
    Alcotest.(check (float 1e-6)) "vec[n*dim-1]"
      (Bigarray.Array1.get vecs (n * dim - 1))
      (Bigarray.Array1.get views.vecs (n * dim - 1));
    (* Cell offsets *)
    Alcotest.(check int64) "offs[0]"
      (Bigarray.Array1.get offs 0)
      (Bigarray.Array1.get views.cell_offsets 0);
    Alcotest.(check int64) "offs[c]"
      (Bigarray.Array1.get offs c)
      (Bigarray.Array1.get views.cell_offsets c);
    (* Labels *)
    Alcotest.(check char) "lbl[0]"
      (Bytes.get lbls 0)
      (Bigarray.Array1.get views.labels 0);
    Alcotest.(check char) "lbl[n-1]"
      (Bytes.get lbls (n - 1))
      (Bigarray.Array1.get views.labels (n - 1));
    Unix.close views.fd)
```

Add to runner:

```ocaml
    "load_mmap", [
      Alcotest.test_case "round-trip mmap views" `Quick test_load_mmap_roundtrip;
    ];
```

- [ ] **Step 2: Run tests to verify failure**

Run: `dune runtest`
Expected: FAIL `Index_io.load_mmap: not yet implemented`.

- [ ] **Step 3: Implement `load_mmap`**

Replace the placeholder `load_mmap` in `lib/index_io.ml`:

```ocaml
let load_mmap path =
  let header = load_header path in
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let map_f32 ~pos ~len =
    Unix.map_file fd ~pos:(Int64.of_int pos)
      Bigarray.float32 Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  let map_i64 ~pos ~len =
    Unix.map_file fd ~pos:(Int64.of_int pos)
      Bigarray.int64 Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  let map_chr ~pos ~len =
    Unix.map_file fd ~pos:(Int64.of_int pos)
      Bigarray.char Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  let centroids    = map_f32 ~pos:header.centroids_off    ~len:(header.c * header.dim) in
  let cell_offsets = map_i64 ~pos:header.cell_offsets_off ~len:(header.c + 1) in
  let vecs         = map_f32 ~pos:header.vecs_off         ~len:(header.n * header.dim) in
  let labels       = map_chr ~pos:header.labels_off       ~len:header.n in
  header, { fd; centroids; cell_offsets; vecs; labels }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `dune runtest`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add lib/index_io.ml tests/test_index_io.ml
git commit -m "feat(index_io): load_mmap returns typed views over file"
```

---

## Phase 2 — Refactor `Index.t` to flat layout

### Task 7: Refactor `Index.t` to use `cell_offsets` (sorted vecs)

**Files:**
- Modify: `lib/index.ml`
- Modify: `bin/bench_ivf.ml` (compile fix only — do not change behavior)
- Modify: `bin/server/main.ml` (compile fix in `build_synth_index`)

This is a non-trivial refactor. We change the in-memory representation so that `t.vecs` is sorted cell-major and `t.cell_offsets` is the bigarray of segment boundaries used to scan a cell. `Index.build` re-sorts internally.

- [ ] **Step 1: Add a failing test that uses the new shape**

Append to `tests/test_index_io.ml` (we will move it to a dedicated file later if it grows; for now, keep it close to the type changes):

```ocaml
let test_build_produces_sorted_layout () =
  let n = 256 and dim = 14 in
  let vs = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (n * dim) in
  let st = Random.State.make [| 99 |] in
  for i = 0 to n * dim - 1 do
    Bigarray.Array1.set vs i (Random.State.float st 1.0)
  done;
  let lbls = Bytes.make n '\000' in
  let idx = Index.build ~c:4 ~iters:3 ~sample:n vs n lbls in
  Alcotest.(check int) "n" n idx.n;
  Alcotest.(check int) "c" 4 idx.c;
  Alcotest.(check int) "cell_offsets length" 5 (Bigarray.Array1.dim idx.cell_offsets);
  Alcotest.(check int64) "first offset = 0" 0L (Bigarray.Array1.get idx.cell_offsets 0);
  Alcotest.(check int64) "last offset = n" (Int64.of_int n) (Bigarray.Array1.get idx.cell_offsets 4)
```

Add to runner under a new section:

```ocaml
    "index", [
      Alcotest.test_case "build produces sorted cell-major layout"
        `Quick test_build_produces_sorted_layout;
    ];
```

- [ ] **Step 2: Run tests to verify failure**

Run: `dune runtest`
Expected: FAIL — current `Index.t` has no `cell_offsets` field; current `build` returns the legacy `lists` shape.

- [ ] **Step 3: Replace `lib/index.ml` with the refactored version**

```ocaml
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

  (* Build cell counts → cell_offsets. *)
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
```

- [ ] **Step 4: Verify all benches still build (they are self-contained)**

`bin/bench.ml`, `bin/bench_ivf.ml`, `bin/bench_int8.ml` reimplement IVF inline and do not depend on `lib/index.ml`. They should build unchanged.

`bin/server/main.ml`'s `build_synth_index` calls `Fraud.Index.build vs !n_arg labels` — same signature, returns the new flat shape. Should compile unchanged in this task; we'll replace it entirely in Task 10.

Run: `dune build 2>&1 | head -40`
Expected: success. If a compile error mentions a removed field (`lists`), the engineer made an unintended edit — revert.

- [ ] **Step 5: Run all tests**

Run: `dune runtest`
Expected: PASS, including the new `build produces sorted cell-major layout` case.

- [ ] **Step 6: Commit**

```bash
git add lib/index.ml tests/test_index_io.ml
git commit -m "refactor(index): cell-major sorted layout w/ cell_offsets"
```

---

## Phase 3 — Offline indexer

### Task 8: Streaming JSON record reader

**Files:**
- Create: `lib/refs_reader.ml`
- Create: `tests/test_refs_reader.ml`
- Modify: `tests/dune`

The on-disk reference file is a single JSON array `[ {vector:[14],label:"fraud"|"legit"}, … ]`. We avoid `Yojson.Safe.from_channel` (which builds a 1.5+ GB AST) by streaming records one at a time.

We read raw bytes through `In_channel`, skip whitespace + array brackets + commas, and use Yojson on each record's substring (records are small).

- [ ] **Step 1: Update `tests/dune`**

```
(tests
 (names test_smoke test_index_io test_refs_reader)
 (libraries fraud alcotest))
```

- [ ] **Step 2: Add a failing test for `Refs_reader.fold`**

Create `tests/test_refs_reader.ml`:

```ocaml
open Fraud

let test_fold_example_references () =
  let path = "../tests/fixtures/example-references.json" in
  let count = ref 0 in
  let fraud_count = ref 0 in
  let last_v0 = ref nan in
  Refs_reader.fold (fun () (vec, label) ->
    incr count;
    if label = `Fraud then incr fraud_count;
    last_v0 := vec.(0)
  ) () (`File path);
  (* example-references.json contains a known number of records; assert >0
     and label mix is sensible. *)
  Alcotest.(check bool) "non-empty" true (!count > 0);
  Alcotest.(check bool) "some frauds" true (!fraud_count > 0);
  Alcotest.(check bool) "fewer frauds than total" true (!fraud_count < !count);
  Alcotest.(check bool) "v0 in [0,1] or -1" true
    (let x = !last_v0 in (x >= 0.0 && x <= 1.0) || x = -1.0)

let () =
  Alcotest.run "refs_reader" [
    "fold", [
      Alcotest.test_case "fold over example-references" `Quick
        test_fold_example_references;
    ];
  ]
```

Note: dune's test runtime cwd is the test dir; `../tests/fixtures/...` is correct because dune runs from `_build/default/tests/`. If a different path resolves, adjust by adding a `(deps (glob_files fixtures/*.json))` in `tests/dune`.

Update `tests/dune` to copy fixtures into the build dir:

```
(tests
 (names test_smoke test_index_io test_refs_reader)
 (libraries fraud alcotest)
 (deps (glob_files fixtures/*.json)))
```

And use `"./fixtures/example-references.json"` in the test.

- [ ] **Step 3: Run the test to verify it fails**

Run: `dune runtest`
Expected: FAIL — `Refs_reader` module not found.

- [ ] **Step 4: Implement `lib/refs_reader.ml`**

```ocaml
(* Stream-parse the rinha references.json format:
   [ {"vector":[f0,...,f13],"label":"fraud"|"legit"}, ... ]

   We do not build the full Yojson tree in memory.  Instead we scan raw
   bytes: skip whitespace / outer '[' / ',' / ']', then find one record
   substring delimited by matching '{' '}', and feed that to Yojson.Safe.
   Each record is small (<200 bytes), so per-record parse is cheap. *)

type label = [ `Fraud | `Legit ]

type source =
  | File of string
  | Channel of In_channel.t
  | Stdin

let source_to_chan = function
  | File path -> In_channel.open_text path
  | Channel ic -> ic
  | Stdin -> In_channel.stdin

(* The implementation uses a lookahead byte buffer pulled from the channel
   in chunks.  We expose just `fold`. *)

module Buf = struct
  type t = {
    ic : In_channel.t;
    mutable data : Bytes.t;
    mutable pos : int;
    mutable len : int;
    mutable eof : bool;
  }
  let make ic = { ic; data = Bytes.create 65536; pos = 0; len = 0; eof = false }
  let refill b =
    if not b.eof then begin
      let n = In_channel.input b.ic b.data 0 (Bytes.length b.data) in
      b.pos <- 0; b.len <- n;
      if n = 0 then b.eof <- true
    end
  let peek b =
    if b.pos >= b.len then refill b;
    if b.eof then None else Some (Bytes.unsafe_get b.data b.pos)
  let advance b = b.pos <- b.pos + 1
  let take b n =
    let out = Buffer.create n in
    let rec loop remaining =
      if remaining = 0 then ()
      else begin
        if b.pos >= b.len then refill b;
        if b.eof then failwith "Refs_reader: unexpected EOF"
        else begin
          let avail = b.len - b.pos in
          let take_n = min avail remaining in
          Buffer.add_subbytes out b.data b.pos take_n;
          b.pos <- b.pos + take_n;
          loop (remaining - take_n)
        end
      end
    in
    loop n;
    Buffer.contents out
end

let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let skip_ws_and b sep =
  let rec loop () =
    match Buf.peek b with
    | None -> ()
    | Some c when is_ws c || c = sep -> Buf.advance b; loop ()
    | Some _ -> ()
  in
  loop ()

(* Read one balanced JSON object beginning at peek='{'.  Strings handled
   so braces inside string literals don't confuse the depth counter. *)
let read_object b =
  (match Buf.peek b with
   | Some '{' -> ()
   | _ -> failwith "Refs_reader: expected '{'");
  let out = Buffer.create 200 in
  Buffer.add_char out '{';
  Buf.advance b;
  let depth = ref 1 in
  let in_str = ref false in
  let escape = ref false in
  while !depth > 0 do
    match Buf.peek b with
    | None -> failwith "Refs_reader: EOF inside record"
    | Some c ->
      Buffer.add_char out c;
      Buf.advance b;
      if !escape then escape := false
      else if !in_str then begin
        if c = '\\' then escape := true
        else if c = '"' then in_str := false
      end
      else begin
        if c = '"' then in_str := true
        else if c = '{' then incr depth
        else if c = '}' then decr depth
      end
  done;
  Buffer.contents out

let parse_record s : float array * label =
  let j = Yojson.Safe.from_string s in
  let vec_j = Detect.field j "vector" in
  let label_j = Detect.field j "label" in
  let label =
    match Detect.to_string label_j with
    | "fraud" -> `Fraud
    | "legit" -> `Legit
    | other -> failwith (Printf.sprintf "Refs_reader: unknown label %s" other)
  in
  let xs = Detect.to_list vec_j in
  if List.length xs <> 14 then
    failwith (Printf.sprintf "Refs_reader: expected 14 dims, got %d" (List.length xs));
  let arr = Array.make 14 0.0 in
  List.iteri (fun i v -> arr.(i) <- Detect.to_float v) xs;
  arr, label

let fold (f : 'a -> float array * label -> 'a) (acc0 : 'a) (src : source) : 'a =
  let ic = source_to_chan src in
  let b = Buf.make ic in
  let close () =
    match src with
    | File _ -> In_channel.close ic
    | Channel _ | Stdin -> ()
  in
  Fun.protect ~finally:close (fun () ->
    skip_ws_and b ' ';
    (match Buf.peek b with
     | Some '[' -> Buf.advance b
     | _ -> failwith "Refs_reader: expected '[' at start");
    let acc = ref acc0 in
    let rec loop () =
      skip_ws_and b ',';
      match Buf.peek b with
      | None -> failwith "Refs_reader: EOF before ']'"
      | Some ']' -> Buf.advance b
      | Some '{' ->
        let s = read_object b in
        let rec_ = parse_record s in
        acc := f !acc rec_;
        loop ()
      | Some c -> failwith (Printf.sprintf "Refs_reader: unexpected char %C" c)
    in
    loop ();
    !acc)
```

- [ ] **Step 5: Run tests**

Run: `dune runtest`
Expected: PASS. The example file should yield non-zero records and a sensible fraud mix.

- [ ] **Step 6: Commit**

```bash
git add lib/refs_reader.ml tests/dune tests/test_refs_reader.ml
git commit -m "feat(refs_reader): streaming JSON parser for references file"
```

---

### Task 9: `bin/build_index.ml` end-to-end

**Files:**
- Create: `bin/build_index.ml`
- Modify: `bin/dune`

- [ ] **Step 1: Register the executable**

Replace `bin/dune`:

```
(executables
 (names bench bench_int8 bench_ivf build_index)
 (libraries fraud unix yojson)
 (ocamlopt_flags (:standard -O3 -unsafe)))
```

- [ ] **Step 2: Create `bin/build_index.ml`**

```ocaml
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

(* Two-pass: first count, then load.  The references file is large enough
   we can't grow a Bigarray as we go.  Cheaper to count records by scanning
   once, then allocate exactly and fill on the second pass.

   For simplicity in v1, we use a chunked dynamic vector backed by a
   resizable plain array of (float array * label).  Memory peak is
   roughly 3M * (14*8 + overhead) bytes for the float arrays; ~360 MB.
   The build container is unconstrained, so this is acceptable. *)

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
```

- [ ] **Step 3: Build and verify it compiles**

Run: `dune build`
Expected: success.

- [ ] **Step 4: Run on the small example fixture**

Run:

```
gunzip -c </dev/null 2>/dev/null  # sanity (gunzip exists)
_build/default/bin/build_index.exe --in tests/fixtures/example-references.json --out /tmp/test_idx.bin --c 4 --iters 3 --sample 100
ls -la /tmp/test_idx.bin
```

Expected: `/tmp/test_idx.bin` exists; size matches `4*14*4 + 5*8 + N*14*4 + N` plus 4096+pad bytes (where N = record count in the fixture, ~50–200 records).

- [ ] **Step 5: Verify the index loads back via mmap**

Run:

```
cat > /tmp/load_check.ml <<'EOF'
let () =
  let h, _v = Fraud.Index_io.load_mmap "/tmp/test_idx.bin" in
  Printf.printf "OK n=%d c=%d dim=%d\n" h.n h.c h.dim
EOF
echo '(executable (name load_check) (libraries fraud))' > /tmp/load_check_dune
# (or run an alcotest case — see Step 6)
```

A cleaner approach: add an alcotest case in `tests/test_index_io.ml` that runs `build_index` end-to-end via `Sys.command`.  Skip the inline script; do Step 6 instead.

- [ ] **Step 6: Add an end-to-end alcotest**

Append to `tests/test_index_io.ml`:

```ocaml
let test_build_index_e2e () =
  let bin = "../bin/build_index.exe" in
  let in_  = "./fixtures/example-references.json" in
  let out_ = Filename.temp_file "idx_e2e_" ".bin" in
  let finally () = try Sys.remove out_ with _ -> () in
  Fun.protect ~finally (fun () ->
    let cmd = Printf.sprintf
      "%s --in %s --out %s --c 4 --iters 3 --sample 100 2>/dev/null"
      (Filename.quote bin) (Filename.quote in_) (Filename.quote out_) in
    let rc = Sys.command cmd in
    Alcotest.(check int) "build_index exit 0" 0 rc;
    let h, _v = Index_io.load_mmap out_ in
    Alcotest.(check int) "c" 4 h.c;
    Alcotest.(check int) "dim" 14 h.dim;
    Alcotest.(check bool) "n > 0" true (h.n > 0))
```

Add to runner:

```ocaml
    "build_index", [
      Alcotest.test_case "e2e: example-references → index.bin" `Quick
        test_build_index_e2e;
    ];
```

Update `tests/dune` so the test depends on the binary being built:

```
(tests
 (names test_smoke test_index_io test_refs_reader)
 (libraries fraud alcotest)
 (deps (glob_files fixtures/*.json)
       %{bin:build_index}))
```

If the `%{bin:build_index}` form does not resolve in your dune version, replace with:

```
 (deps (glob_files fixtures/*.json)
       (alias_rec ../bin/all)))
```

- [ ] **Step 7: Run `dune runtest`**

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add bin/dune bin/build_index.ml tests/test_index_io.ml tests/dune
git commit -m "feat(build_index): offline references.json → index.bin tool"
```

---

## Phase 4 — Server mmap integration

### Task 10: Replace synth builder with mmap loader

**Files:**
- Modify: `bin/server/main.ml`

- [ ] **Step 1: Replace `bin/server/main.ml` with mmap-based startup**

```ocaml
(* Fraud detection HTTP server — mmap'd index.bin. *)

open Lwt.Infix

let nprobe = 8
let port = ref 9999
let index_path = ref "/app/index.bin"

let load_index path : Fraud.Index.t =
  let t0 = Unix.gettimeofday () in
  let h, v = Fraud.Index_io.load_mmap path in
  let idx = Fraud.Index.of_segments
    ~vecs:v.vecs ~n:h.n ~labels:v.labels
    ~centroids:v.centroids ~c:h.c ~cell_offsets:v.cell_offsets in
  Printf.printf "[server] mmapped index n=%d c=%d in %.3fs from %s\n%!"
    h.n h.c (Unix.gettimeofday () -. t0) path;
  idx

let respond_string reqd ?(status = `OK) ?(content_type = "text/plain") body =
  let headers = Httpaf.Headers.of_list [
    "content-type", content_type;
    "content-length", string_of_int (String.length body);
  ] in
  let resp = Httpaf.Response.create ~headers status in
  Httpaf.Reqd.respond_with_string reqd resp body

let request_handler index _client_addr (reqd : Httpaf.Reqd.t) : unit =
  let req = Httpaf.Reqd.request reqd in
  match req.meth, req.target with
  | `GET, "/ready" ->
    respond_string reqd "ok"
  | `POST, "/fraud-score" ->
    let body_r = Httpaf.Reqd.request_body reqd in
    let buf = Buffer.create 1024 in
    let rec on_read bs ~off ~len =
      Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
      Httpaf.Body.schedule_read body_r ~on_read ~on_eof
    and on_eof () =
      try
        let json = Yojson.Safe.from_string (Buffer.contents buf) in
        let v = Fraud.Detect.vectorize json in
        let score = Fraud.Index.fraud_score index v ~nprobe in
        let approved = score < 0.6 in
        let body =
          Printf.sprintf "{\"approved\":%s,\"fraud_score\":%g}"
            (if approved then "true" else "false") score
        in
        respond_string reqd ~content_type:"application/json" body
      with e ->
        let msg = Printexc.to_string e in
        respond_string reqd ~status:`Bad_request ("error: " ^ msg)
    in
    Httpaf.Body.schedule_read body_r ~on_read ~on_eof
  | _ ->
    respond_string reqd ~status:`Not_found "not found"

let error_handler _client_addr ?request:_ _err start_response =
  let body = start_response Httpaf.Headers.empty in
  Httpaf.Body.write_string body "internal error";
  Httpaf.Body.close_writer body

let main () =
  let speclist = [
    "--port",  Arg.Set_int port,           "port (default 9999)";
    "--index", Arg.Set_string index_path,  "path to index.bin (default /app/index.bin)";
  ] in
  Arg.parse speclist (fun _ -> ()) "fraud-server";

  let index = load_index !index_path in

  let listen_addr = Unix.(ADDR_INET (inet_addr_any, !port)) in
  let connection_handler =
    Httpaf_lwt_unix.Server.create_connection_handler
      ~request_handler:(request_handler index)
      ~error_handler
  in
  Lwt_main.run begin
    Lwt_io.establish_server_with_client_socket listen_addr connection_handler
    >>= fun _server ->
    Printf.printf "[server] listening on :%d\n%!" !port;
    fst (Lwt.wait ())
  end

let () = main ()
```

- [ ] **Step 2: Build**

Run: `dune build`
Expected: success.

- [ ] **Step 3: End-to-end smoke from the shell**

Build a small index from the fixture, start the server pointing at it, hit it once, verify the response shape, kill server.

```
_build/default/bin/build_index.exe --in tests/fixtures/example-references.json --out /tmp/idx.bin --c 4 --iters 3 --sample 100

_build/default/bin/server/main.exe --port 19999 --index /tmp/idx.bin &
SVR=$!
sleep 0.5

curl -s http://127.0.0.1:19999/ready
echo

# pick first payload from example-payloads.json (it is a single record per array entry)
python3 -c 'import json,sys;print(json.dumps(json.load(open("tests/fixtures/example-payloads.json"))[0]))' \
  | curl -s -X POST -H 'content-type: application/json' \
      --data-binary @- http://127.0.0.1:19999/fraud-score
echo

kill $SVR
wait $SVR 2>/dev/null
```

Expected:
- `/ready` → `ok`
- `/fraud-score` → `{"approved":<true|false>,"fraud_score":<number 0..1>}` JSON.

- [ ] **Step 4: Commit**

```bash
git add bin/server/main.ml
git commit -m "feat(server): mmap index.bin instead of building synthetic data"
```

---

## Phase 5 — Docker

### Task 11: Multi-stage Dockerfile

**Files:**
- Create: `Dockerfile`
- Create: `.dockerignore`

- [ ] **Step 1: Create `.dockerignore`**

```
_build/
.git/
docs/
tests/fixtures/
*.swp
.merlin
.vscode/
.DS_Store
README.md
index.bin
```

(`tests/fixtures/` excluded — they are not needed in the image; the build stage downloads `references.json.gz` directly.)

- [ ] **Step 2: Create `Dockerfile`**

```dockerfile
# syntax=docker/dockerfile:1.7

# ---------- Stage 1: build OCaml binaries ----------
FROM ocaml/opam:debian-12-ocaml-5.1 AS build
WORKDIR /work

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp-dev libev-dev pkg-config curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*
USER opam

# Install OCaml deps explicitly (no .opam file in this project).
RUN opam update -y && \
    opam install -y dune yojson httpaf httpaf-lwt-unix lwt conf-libev

COPY --chown=opam:opam dune-project ./
COPY --chown=opam:opam lib lib
COPY --chown=opam:opam bin bin

RUN eval $(opam env) && dune build --release bin/build_index.exe bin/server/main.exe

# ---------- Stage 2: build index.bin ----------
FROM debian:12-slim AS index
WORKDIR /work
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates gzip && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /work/_build/default/bin/build_index.exe /usr/local/bin/build_index

RUN curl -fsSL -o /work/references.json.gz \
    https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/references.json.gz

RUN gunzip -c /work/references.json.gz \
    | build_index --in - --out /work/index.bin --c 1024 --iters 5 --sample 200000 \
 && rm /work/references.json.gz

# ---------- Stage 3: runtime ----------
FROM debian:12-slim AS runtime
WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends libgmp10 libev4 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /work/_build/default/bin/server/main.exe /app/server
COPY --from=index /work/index.bin /app/index.bin

EXPOSE 9999
ENTRYPOINT ["/app/server", "--index", "/app/index.bin", "--port", "9999"]
```

Notes for the engineer:
- The `opam install --deps-only --with-test` line covers the case where a `.opam` file is present. We don't have one yet; the fallback line installs the libraries explicitly. If the engineer adds an `.opam` file later, the first line is enough.
- Stage 2 is separate from stage 1 so we don't push the OCaml toolchain into the runtime image.

- [ ] **Step 3: Build the image locally**

Run:

```
docker build --platform=linux/amd64 -t rinha-fraud-ocaml:latest . 2>&1 | tail -40
```

Expected: success. The build will download ~50 MB references.json.gz and run the indexer (~1–2 min). Final image should be ~250 MB.

- [ ] **Step 4: Smoke-test the image**

```
docker run --rm -p 19999:9999 --platform=linux/amd64 rinha-fraud-ocaml:latest &
DOCKER_PID=$!
sleep 2

curl -s http://127.0.0.1:19999/ready
echo

python3 -c 'import json,sys;print(json.dumps(json.load(open("tests/fixtures/example-payloads.json"))[0]))' \
  | curl -s -X POST -H 'content-type: application/json' \
      --data-binary @- http://127.0.0.1:19999/fraud-score
echo

docker stop $(docker ps -q --filter ancestor=rinha-fraud-ocaml:latest) || true
```

Expected: `/ready` → `ok`, `/fraud-score` → JSON `{approved,fraud_score}`.

- [ ] **Step 5: Commit**

```bash
git add Dockerfile .dockerignore
git commit -m "feat(docker): multi-stage build w/ index.bin baked into image"
```

---

### Task 12: nginx config + docker-compose

**Files:**
- Create: `nginx.conf`
- Create: `docker-compose.yml`

- [ ] **Step 1: Create `nginx.conf`**

```nginx
worker_processes 1;
events {
  worker_connections 1024;
}
http {
  access_log off;
  error_log /dev/stderr warn;

  upstream api {
    server api1:9999;
    server api2:9999;
    keepalive 64;
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

- [ ] **Step 2: Create `docker-compose.yml`**

```yaml
services:
  api1: &api
    image: rinha-fraud-ocaml:latest    # local dev tag; CI swaps for ghcr.io tag
    platform: linux/amd64
    expose: ["9999"]
    deploy:
      resources:
        limits:
          cpus: "0.45"
          memory: "159M"

  api2: *api

  nginx:
    image: nginx:1.27-alpine
    platform: linux/amd64
    ports: ["9999:9999"]
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
    depends_on: [api1, api2]
    deploy:
      resources:
        limits:
          cpus: "0.10"
          memory: "32M"

networks:
  default:
    driver: bridge
```

- [ ] **Step 3: Bring it up**

Run:

```
docker compose up -d
sleep 5
curl -s http://127.0.0.1:9999/ready
```

Expected: `/ready` → `ok` (any 2xx).

- [ ] **Step 4: Verify round-robin with two requests**

```
for i in 1 2 3 4; do
  python3 -c 'import json,sys;print(json.dumps(json.load(open("tests/fixtures/example-payloads.json"))[0]))' \
    | curl -s -X POST -H 'content-type: application/json' --data-binary @- \
        http://127.0.0.1:9999/fraud-score
  echo
done

docker compose logs api1 api2 | tail -20
```

Expected: both `api1` and `api2` show request log lines (or at least `[server] listening`).

- [ ] **Step 5: Verify memory under limits**

Run: `docker stats --no-stream`
Expected: each container's `MEM USAGE` is under its limit (32 / 159 / 159 MB).

If the api containers exceed 159 MB, this is the int8 fallback trigger from the spec — note the observation in a follow-up issue and do **not** raise the limit (the rinha rule caps total at 350 MB).

- [ ] **Step 6: Tear down**

Run: `docker compose down`

- [ ] **Step 7: Commit**

```bash
git add nginx.conf docker-compose.yml
git commit -m "feat(deploy): nginx LB + docker-compose w/ rinha limits"
```

---

## Phase 6 — CI: push image to GHCR

### Task 13: GitHub Actions build-and-push workflow

**Files:**
- Create: `.github/workflows/build-image.yml`

- [ ] **Step 1: Create the workflow**

```yaml
name: Build and Push Image

on:
  push:
    branches: [main]
    tags: ["v*"]
  workflow_dispatch:

jobs:
  build-and-push:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write
    steps:
      - uses: actions/checkout@v4

      - uses: docker/setup-qemu-action@v3
      - uses: docker/setup-buildx-action@v3

      - name: Log in to GHCR
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Compute image tag
        id: tag
        run: |
          echo "image=ghcr.io/${{ github.repository }}" \
            | tr '[:upper:]' '[:lower:]' >> "$GITHUB_OUTPUT"

      - name: Build and push
        uses: docker/build-push-action@v6
        with:
          context: .
          platforms: linux/amd64
          push: true
          tags: |
            ${{ steps.tag.outputs.image }}:latest
            ${{ steps.tag.outputs.image }}:${{ github.sha }}
          cache-from: type=gha
          cache-to: type=gha,mode=max
```

- [ ] **Step 2: Update `docker-compose.yml` to use the GHCR tag by default**

Edit the `image:` line under `api1:` so the file is submission-ready. Replace:

```yaml
    image: rinha-fraud-ocaml:latest    # local dev tag; CI swaps for ghcr.io tag
```

with:

```yaml
    image: ghcr.io/${GITHUB_REPOSITORY:-junior-nascm/rinha-de-backend-2026-ocaml}:latest
```

(Local dev still works via `make docker-build` followed by retagging, or with a local override `docker-compose.override.yml` not committed.)

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/build-image.yml docker-compose.yml
git commit -m "ci: build and push image to ghcr.io on main"
```

- [ ] **Step 4: Verify locally that the workflow YAML is well-formed**

Run: `python3 -c 'import yaml,sys; yaml.safe_load(open(".github/workflows/build-image.yml"))' && echo OK`
Expected: `OK`.

- [ ] **Step 5: Push and observe the run**

Run: `git push origin main`

Then check: `gh run list --workflow=build-image.yml --limit 1`
Expected: a run starts on the latest commit. Wait for it to succeed (≈10–15 min first time, less on cache hits) with `gh run watch`.

After it finishes, verify the image is published:

```
gh api /user/packages/container/rinha-de-backend-2026-ocaml/versions --jq '.[0].metadata.container.tags'
```

Expected: list contains `latest` and the commit SHA.

---

## Phase 7 — End-to-end verification

### Task 14: Compose with the published GHCR image

**Files:** none — verification only.

- [ ] **Step 1: Pull and run from GHCR**

```
docker compose pull
docker compose up -d
sleep 8

curl -fsSL http://127.0.0.1:9999/ready

python3 -c 'import json,sys;print(json.dumps(json.load(open("tests/fixtures/example-payloads.json"))[0]))' \
  | curl -s -X POST -H 'content-type: application/json' --data-binary @- \
      http://127.0.0.1:9999/fraud-score
echo
```

Expected: `/ready` → `ok` (HTTP 2xx); `/fraud-score` → JSON with `approved` boolean and `fraud_score` ∈ [0,1].

- [ ] **Step 2: Verify resource ceilings**

Run: `docker stats --no-stream --format 'table {{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}'`
Expected: each row's MemUsage stays under its compose limit; sum ≤ 350 MB.

- [ ] **Step 3: Tear down**

Run: `docker compose down`

- [ ] **Step 4: Update README TODO list**

Edit `README.md` and remove the three completed bullets:

```diff
 ## TODO

-- [ ] Load real `references.json.gz` and serialize to binary `index.bin`
-- [ ] `mmap` index in server
-- [ ] `Dockerfile` + `docker-compose.yml` (nginx LB + 2× api)
+- [x] Load real `references.json.gz` and serialize to binary `index.bin`
+- [x] `mmap` index in server
+- [x] `Dockerfile` + `docker-compose.yml` (nginx LB + 2× api)
+- [ ] Phase 2: int8 quantization (gated on Mac-Mini-class p99 measurement)
```

- [ ] **Step 5: Commit**

```bash
git add README.md
git commit -m "docs: tick off real-data + mmap + docker TODOs"
```

---

## Self-review notes

**Spec coverage:** every spec section maps to at least one task — file format → Tasks 3-6, build flow → Tasks 7-9, runtime flow → Task 10, compose limits → Task 12, nginx → Task 12, error handling → covered in Task 5 (header validation) and Task 10 (mmap fail = exit nonzero by `Unix.openfile` raising), testing strategy → Tasks 2-9, GHCR addition → Task 13.

**Memory accounting risk** is documented in Task 12 Step 5: if a Phase-1 deployment exceeds 159 MB per api container, that triggers the Phase-2 (int8) path from the spec rather than raising limits.

**No placeholders in code blocks:** all OCaml/yaml/Dockerfile/nginx/Makefile contents are concrete and runnable.

**Type consistency:** `Index_io.header`, `Index_io.mmap_views`, `Index.t`, `Refs_reader.label` are defined once in the "Locked types" section and used consistently in tasks 3-10.
