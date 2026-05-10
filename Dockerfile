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
# Small minor heap + eager GC = shorter pauses under sustained load.
ENV OCAMLRUNPARAM=s=2M,o=120
ENTRYPOINT ["/app/server", "--index", "/app/index.bin", "--port", "9999"]
