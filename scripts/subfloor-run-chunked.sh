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
# on every host where the filter already fits under the threshold. Above it,
# split the FLAT `a | b | c | ...` expression on its top-level ` | ` into
# argv-safe chunks and run nextest once per chunk, under `set -e` so the
# script stops at the FIRST FAILING CHUNK — the same fail-fast shape the
# single invocation always had, rather than running every chunk regardless
# and reporting a `--no-fail-fast`-style summary this recipe never asked for.
#
# **CORRECTION (fix round 1): the short-circuit path is NOT "macOS today".**
# `docs/timings/subfloor-roster.tsv` is a single unkeyed path (`subfloor_path`
# in `windows/lab/src/timings.rs`) — a test's membership in the sub-floor
# tier does not vary by host, so the SAME 186387-byte filter is what every
# host renders, macOS included. At the current `SAFE_BYTES=100000`, every
# host takes the CHUNKED path today; the short-circuit exists for whenever
# the roster next shrinks below the threshold (or `HV_SUBFLOOR_CHUNK_BYTES`
# is raised on a host actually known not to need the margin), not because
# any host is on it now. An earlier draft of this comment asserted the
# opposite, which is exactly the kind of claim that goes unquestioned
# because it sounds like the reason the short-circuit exists at all — it was
# false, and code review (fix round 1) is what caught it, not testing.
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
#
# **PORTABILITY (fix round 1, both Critical): the splitter uses NO external
# tool and NO bash-4+ builtin.** The first version piped through
# `sed 's/ | /\n/g'` and read the result with `mapfile`. Both were wrong for
# this repo's own stated Mac/Linux split (root CLAUDE.md puts `gate-commit`
# on the Mac): GNU sed turns `\n` in a replacement into a newline, but
# BSD/macOS sed does not — it emits the two literal characters `\` `n`, so
# the "split" produces ONE run-on term that fails to parse as a filterset,
# and `gate-commit` goes red on every `.rs` commit on macOS specifically
# (this trades the Linux-only crash this script fixes for a Mac-only one on
# the same every-commit path). Separately, `mapfile` is bash 4+; macOS ships
# `/bin/bash` 3.2 (its last GPLv2 release), and `Makefile:subfloor-run` runs
# this script with plain `bash`, not a Homebrew 5.x one, so `mapfile` is
# `command not found` (exit 127 under `set -e`) on a stock Mac with no
# Homebrew bash on PATH. Both bugs were masked the same way: neither was
# exercised on the platform that would have shown it, because this was
# authored and tested only on lefford (Linux, a Homebrew-adjacent-but-
# irrelevant bash 5). The fix below uses only `${var//search/replace}`
# (bash's own string substitution, not delegated to any external `sed`/
# `tr`/`awk` — identical behaviour on every bash build) and a
# `while read` loop over a here-string (`<<<`, available since bash 2.05b),
# both well inside what bash 3.2 supports. No `mapfile`, no `sed`, no `awk`.
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

# Replace the roster's own separator with a real newline using bash's own
# parameter expansion (`${var//pattern/replacement}`) — no subprocess, so no
# GNU-vs-BSD divergence is possible. `$'\n'` is ANSI-C quoting, supported
# since bash 2.0.
term_lines="${filter// | /$'\n'}"

batch=""
chunk_num=0
run_batch() {
    chunk_num=$((chunk_num + 1))
    echo "subfloor-run-chunked: chunk $chunk_num (${#batch} bytes) …" >&2
    cargo nextest run --workspace -E "$batch"
}

# A `while read` loop over a here-string, not an array: `mapfile`/`readarray`
# are bash 4+ and unavailable on macOS's stock `/bin/bash` 3.2. `<<<` appends
# a trailing newline for us, so the final term is read like any other — no
# terms are dropped.
while IFS= read -r term; do
    [ -n "$term" ] || continue
    candidate="${batch:+$batch | }$term"
    if [ -n "$batch" ] && [ "${#candidate}" -gt "$SAFE_BYTES" ]; then
        run_batch
        batch="$term"
    else
        batch="$candidate"
    fi
done <<< "$term_lines"
if [ -n "$batch" ]; then
    run_batch
fi
echo "subfloor-run-chunked: all $chunk_num chunk(s) passed." >&2
