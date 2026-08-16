#!/usr/bin/env bash
# scripts/subfloor-run-chunked.sh — run the sub-floor nextest filterset in
# argv-safe chunks (discovered live while landing The Sluice's Task 0, on
# lefford — a second, unrelated shared-infrastructure bug found on the way
# to fixing the first one).
#
# WHY THIS EXISTS. Linux's execve() rejects any SINGLE argv/envp string
# longer than MAX_ARG_STRLEN (32 * PAGE_SIZE = 131072 bytes on every
# mainstream Linux kernel — a compile-time kernel constant, not a ulimit and
# not tunable). Verified directly on this box with a small harness: a
# 130000-byte argv string execve()s fine, a 131072-byte one fails with
# "Argument list too long" (E2BIG). The committed sub-floor roster
# (docs/timings/subfloor-roster.tsv, unmodified by this change and identical
# on origin/main) renders to a 186387-byte flat `test(=a) | test(=b) | ...`
# filterset at this writing — comfortably over that limit — so
# `cargo nextest run --workspace -E "$filter"` crashed with that exact error
# on EVERY Linux host, unconditionally, before a single test ran. Root
# CLAUDE.md places `gate-commit` on the Mac ("local, host-unguarded,
# seconds-scale"); macOS's ARG_MAX budget is spent on the TOTAL, with no
# equivalent per-string cap, which is presumably why this had never bitten
# before a session happened to commit a `.rs` change from lefford directly.
#
# THE FIX. Below the safe threshold, run the exact single `-E` invocation
# this recipe always ran — byte-for-byte the prior behaviour, unconditionally,
# on every host where the filter already fit (macOS today; Linux, for as
# long as the roster stays under the threshold). Above it, split the FLAT
# `a | b | c | ...` expression on its top-level ` | ` into argv-safe chunks
# and run nextest once per chunk, under `set -e` so the script stops at the
# FIRST FAILING CHUNK — the same fail-fast shape the single invocation
# always had, rather than running every chunk regardless and reporting a
# `--no-fail-fast`-style summary this recipe never asked for.
#
# THE SPLIT IS LOSSLESS BECAUSE THE ROSTER IS FLAT, verified against the
# committed roster at authoring time: it is exactly N `test(=name)` terms
# joined by ` | `, with no `&`, no `not(`, and no parenthesis nesting beyond
# the one call per term — `' | '.join(text.split(' | ')) == text` holds for
# the real, current roster. A future roster shape with real nesting (a
# `not(...)` or an `&`-joined term) would need a smarter splitter than a
# literal ` | ` split; this one does not attempt to parse the filterset DSL
# in general.
#
# THE FILTER ARRIVES AS A FILE PATH, NOT AS AN ARGV STRING — a 186387-byte
# filter handed to THIS SCRIPT as "$1" would already have hit the same
# E2BIG this script exists to route around, one process earlier (the
# `bash scripts/subfloor-run-chunked.sh "$filter"` call itself). The caller
# (`subfloor-run` in the Makefile) writes the roster straight to a temp file
# and hands us the (short) path instead.
set -euo pipefail

filter_file="${1:?usage: subfloor-run-chunked.sh <filterset-file>}"
filter="$(cat "$filter_file")"

# argv also carries "cargo", "nextest", "run", "--workspace", "-E", and the
# process's environment (~2 KB in a normal shell) — leave generous headroom
# under the hard 131072-byte MAX_ARG_STRLEN rather than hugging it.
SAFE_BYTES="${HV_SUBFLOOR_CHUNK_BYTES:-100000}"

if [ "${#filter}" -le "$SAFE_BYTES" ]; then
    exec cargo nextest run --workspace -E "$filter"
fi

echo "subfloor-run-chunked: filter is ${#filter} bytes, over the ${SAFE_BYTES}-byte safe threshold (Linux's MAX_ARG_STRLEN is 131072) — splitting into argv-safe chunks." >&2

# Split on the roster's own separator, one line per term (a plain newline
# split rather than IFS='|', which would also split inside any future term
# containing a literal '|' and would leave stray leading spaces from the
# " | " separator — neither is a concern for the flat `test(=name)` shape
# verified above, but the newline split is correct even if that changes).
mapfile -t terms < <(printf '%s' "$filter" | sed 's/ | /\n/g')

batch=""
chunk_num=0
run_batch() {
    chunk_num=$((chunk_num + 1))
    echo "subfloor-run-chunked: chunk $chunk_num (${#batch} bytes) …" >&2
    cargo nextest run --workspace -E "$batch"
}

for term in "${terms[@]}"; do
    candidate="${batch:+$batch | }$term"
    if [ -n "$batch" ] && [ "${#candidate}" -gt "$SAFE_BYTES" ]; then
        run_batch
        batch="$term"
    else
        batch="$candidate"
    fi
done
if [ -n "$batch" ]; then
    run_batch
fi
echo "subfloor-run-chunked: all $chunk_num chunk(s) passed." >&2
