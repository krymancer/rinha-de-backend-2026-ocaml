# syntax=docker/dockerfile:1.7

# ---------- Stage 1: build OCaml binaries ----------
FROM ocaml/opam:debian-12-ocaml-5.1 AS build
WORKDIR /work

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp-dev pkg-config curl ca-certificates gcc && \
    rm -rf /var/lib/apt/lists/*
USER opam

# Server is framework-free now (raw epoll); only dune + yojson needed.
# ocaml-option-flambda recompiles the switch's compiler with flambda, so the
# `-O3` / `[@inline]` in lib/dune actually fire (without it they are silent
# no-ops): ref->register in the kNN hot loops + cross-module inlining of the
# distance/scan code. ~1.5-2x on the scalar numeric path.
RUN opam update -y && opam install -y ocaml-option-flambda
RUN opam install -y dune yojson

COPY --chown=opam:opam dune-project ./
COPY --chown=opam:opam lib lib
COPY --chown=opam:opam bin bin

RUN eval $(opam env) && ocamlopt -config | grep -qx 'flambda: true' \
 && echo "flambda: ON" \
 && dune build --release bin/build_index.exe bin/server/main.exe

# ---------- Stage 2: build index.bin (exact KD index) ----------
FROM debian:12-slim AS index
WORKDIR /work
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates gzip && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /work/_build/default/bin/build_index.exe /usr/local/bin/build_index

RUN curl -fsSL -o /work/references.json.gz \
    https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/references.json.gz

RUN gunzip -c /work/references.json.gz \
    | build_index --in - --out /work/index.bin \
 && rm /work/references.json.gz

# ---------- Stage 3: runtime ----------
FROM debian:12-slim AS runtime
WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends libgmp10 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /work/_build/default/bin/server/main.exe /app/server
COPY --from=index /work/index.bin /app/index.bin

EXPOSE 9999
# Larger minor heap: the hot path is near-zero-alloc, so a big minor heap means
# essentially no GC during the run.
ENV OCAMLRUNPARAM=s=8M,o=200
ENV INDEX_PATH=/app/index.bin
ENV API_WARMUP_QUERIES=8192
ENTRYPOINT ["/app/server", "--index", "/app/index.bin", "--port", "9999"]
