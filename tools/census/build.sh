#!/usr/bin/env bash
# tools/census/build.sh — materialize the throwaway analysis database
# (census-as-data spec §3). Validates every mounted dataset (row count,
# FNV-1a64, header order) then builds .build/census.duckdb. Fails loudly at
# mount time; a dataset that can't mount aborts the whole build.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

for tool in duckdb python3; do
    command -v "$tool" >/dev/null 2>&1 || {
        echo "census: '$tool' not found — install with: brew install $tool" >&2
        exit 1
    }
done

MANIFEST="${CENSUS_MANIFEST:-tools/census/manifest.json}"

mkdir -p tools/census/.build
python3 tools/census/build_ddl.py "$MANIFEST" > tools/census/.build/ddl.sql
duckdb tools/census/.build/census.duckdb < tools/census/.build/ddl.sql > /dev/null
echo "census: built tools/census/.build/census.duckdb" >&2
