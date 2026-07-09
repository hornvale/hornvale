#!/usr/bin/env bash
# scripts/freeze-fixture.sh — capture a `hornvale` subcommand's stdout into a
# test fixture file, cleanly.
#
# The recurring bug this prevents: a fixture frozen with `cargo run ... > file`
# against an un-built binary (or with a stray `2>&1`) captures cargo's own
# progress lines ("Compiling…", "Finished…", "Running…") into the fixture,
# silently corrupting it. It bit two campaigns before it was caught by a
# byte-level regeneration diff. This script rebuilds first (so cargo has
# nothing to say on stdout at run time), captures stdout only, and then asserts
# the result carries no build noise — failing loudly if it somehow does.
#
# Usage:
#   scripts/freeze-fixture.sh <dest-file> -- <hornvale subcommand and args...>
#
# Examples:
#   scripts/freeze-fixture.sh cli/tests/fixtures/pre-x-seed-42-world.json -- \
#       new --seed 42 --out cli/tests/fixtures/pre-x-seed-42-world.json
#   scripts/freeze-fixture.sh cli/tests/fixtures/pre-x-seed-42-almanac.md -- \
#       almanac --world cli/tests/fixtures/pre-x-seed-42-world.json
#
# Note: subcommands that write their own output file (`new --out`) still need a
# dest here; for those the dest and the --out path are the same file and this
# script's stdout capture is a harmless empty overwrite — prefer using it for
# stdout-producing subcommands (almanac, phonology, concepts, streams, map).
set -euo pipefail

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" || $# -lt 3 ]]; then
    sed -n '2,28p' "$0"
    exit 2
fi

dest="$1"
shift
if [[ "$1" != "--" ]]; then
    echo "freeze-fixture: expected '--' before the subcommand, got '$1'" >&2
    exit 2
fi
shift
# Remaining args are the hornvale subcommand + its flags.

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

# Build first so run-time stdout is pure program output, never build progress.
# Build diagnostics go to the terminal (stderr), never into the fixture.
cargo build -q -p hornvale 1>&2

# Capture stdout ONLY. cargo's `-q` plus the prior build keeps this clean; we
# do not redirect stderr into the file (that was the original corruption).
cargo run -q -p hornvale -- "$@" > "$dest"

# Guard: fail loudly if any cargo/tool noise leaked into the fixture.
if grep -qE '^\s*(Compiling|Finished|Running|Building|warning:|error(\[|:))' "$dest"; then
    echo "freeze-fixture: BUILD NOISE leaked into $dest — refusing to keep a corrupt fixture:" >&2
    grep -nE '^\s*(Compiling|Finished|Running|Building|warning:|error(\[|:))' "$dest" >&2
    exit 1
fi

echo "freeze-fixture: wrote $dest ($(wc -l < "$dest" | tr -d ' ') lines), clean." >&2
