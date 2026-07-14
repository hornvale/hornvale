#!/usr/bin/env bash
# tools/census/check.sh — the harness's own gate (spec §3): mount-validate,
# smoke every mounted dataset, and fail on any golden-pin mismatch.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

bash tools/census/build.sh

db="tools/census/.build/census.duckdb"
echo "census-check: smoke (one count per mounted dataset)" >&2
duckdb "$db" -csv -c "SELECT study, count(*) FROM census_long GROUP BY study ORDER BY study" | sed 's/^/  /'
duckdb "$db" -csv -c "SELECT count(*) FROM timings" > /dev/null

echo "census-check: golden pins" >&2
pins="$(duckdb "$db" -csv -c ".read tools/census/queries/calibrate/golden-pins.sql")"
printf '%s\n' "$pins" | sed 's/^/  /'
if printf '%s\n' "$pins" | grep -q ",false$"; then
    echo "census-check: GOLDEN PIN MISMATCH — fixture and calibration.rs disagree" >&2
    exit 1
fi
echo "census-check: ok" >&2
