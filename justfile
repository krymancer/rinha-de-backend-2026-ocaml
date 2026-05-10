owner := `git config --get remote.origin.url | sed -nE 's#.*[:/]([^/]+)/[^/]+$#\1#p'`
image := "ghcr.io/" + owner + "/rinha-de-backend-2026-ocaml"

default:
    @just --list

build:
    dune build

test: fetch-fixtures
    dune runtest

fetch-fixtures:
    mkdir -p tests/fixtures
    test -s tests/fixtures/example-references.json || \
        curl -fsSL -o tests/fixtures/example-references.json \
            https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/example-references.json
    test -s tests/fixtures/example-payloads.json || \
        curl -fsSL -o tests/fixtures/example-payloads.json \
            https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/example-payloads.json

fetch-data:
    mkdir -p resources
    test -s resources/references.json.gz || \
        curl -fsSL -o resources/references.json.gz \
            https://raw.githubusercontent.com/zanfranceschi/rinha-de-backend-2026/main/resources/references.json.gz

index: build fetch-data
    gunzip -c resources/references.json.gz | _build/default/bin/build_index.exe --in - --out index.bin

docker-build:
    docker build -t {{image}}:latest --platform=linux/amd64 .

docker-up:
    docker compose up --build

clean:
    dune clean
    rm -f index.bin
