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
# irrelevant bash 5). That round's fix used only `${var//search/replace}`
# (bash's own string substitution) and a `while read` loop over a here-string
# (`<<<`, since bash 2.05b), both well inside what bash 3.2 supports.
#
# **PORTABILITY AND COST (fix round 2): `${var//search/replace}` WAS PORTABLE
# AND WAS ALSO UNUSABLE, and only one of those was checked.** Round 1 reasoned
# correctly about which constructs bash 3.2 *supports* and never measured what
# one of them *costs*. On macOS's stock bash 3.2, substituting over the
# roster's flat filterset is roughly O(n^3). Measured on `ambrose`, this very
# filter truncated: 5,000 B -> 0 s, 10,000 B -> 1 s, 20,000 B -> 8 s,
# 40,000 B -> 59 s, about 7.5x per doubling. It is ALSO locale-sensitive,
# because bash 3.2 pattern-matches multibyte-aware under a UTF-8 locale: at
# 40,000 bytes, `LC_ALL=C` takes 5 s and `LC_ALL=en_US.UTF-8` takes 62 s. At
# the real 229,189-byte size that is ~13 min under C and HOURS under UTF-8 —
# so `git commit` on a Mac hung in the pre-commit hook, in bash, with no
# compiler running and nothing for `ps` to show. It looked exactly like a
# wedged agent, which is how it cost an hour before anyone looked here.
#
# **`awk` IS SAFE HERE AND `sed` WAS NOT, and the difference is not a matter
# of taste.** Round 1 rejected `sed 's/ | /\n/g'` because `\n` in a
# REPLACEMENT is a GNU extension that BSD/macOS sed emits as two literal
# characters. `awk` has no such divergence: `"\n"` in `gsub`'s replacement is
# an ordinary string literal resolved by awk's own lexer, identical under
# POSIX awk, BSD awk (macOS ships `awk version 20200816`) and gawk. Verified
# rather than assumed, on macOS BSD awk: byte-for-byte identical output to
# the bash substitution over the same input (`cmp` clean, 19,432 bytes), and
# **0.030 s on the full 229,189-byte filter instead of hours** — and
# unchanged at 0.032 s under a UTF-8 locale, so the pathology is gone rather
# than merely narrowed.
#
# `mapfile` is still out (bash 4+; macOS ships `/bin/bash` 3.2, its last
# GPLv2 release, and `Makefile:subfloor-run` invokes plain `bash`). The
# `while read` loop over `<<<` below is unchanged and still 3.2-safe.
#
# THE STANDING LESSON, since this is now the second defect in this one file
# and the second bash-3.2 defect found in a single session (the other:
# `scripts/test-worktree-freshness.sh` does not even PARSE under 3.2, while
# passing shellcheck clean): "bash 3.2 supports this construct" is a
# different claim from "this construct is usable at our input sizes on bash
# 3.2", and only the first one is cheap to check. When a script here touches
# a string measured in hundreds of kilobytes, time it on the Mac before
# believing it.
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

# THE SPLIT HAPPENS ENTIRELY IN awk, AND THAT IS THE WHOLE POINT — no chunk
# text is ever held in a shell variable. See the PORTABILITY-AND-COST note in
# the header: bash 3.2 is not merely slow at this, it is superlinear at it,
# and the cost is in the ACCUMULATION as much as in the separator rewrite.
# Building `candidate="$batch | $term"` once per term and asking `${#candidate}`
# each time re-copies and re-measures a string that grows to SAFE_BYTES, ~3,200
# times; under a UTF-8 locale `${#var}` counts CHARACTERS, so each measurement
# walks the whole buffer multibyte-aware. Rewriting only the separator step in
# awk and leaving this loop in bash still took **8m24s** on `ambrose` with
# `LC_ALL=C` (and did not finish at all without it) — measured, not assumed,
# after that exact half-fix was tried.
#
# awk does the same arithmetic in one linear pass and writes each chunk
# straight to its own file, so bash only ever handles a handful of short
# paths. `LC_ALL=C`: the separator is pure ASCII, so a byte-wise match is both
# correct and immune to the multibyte cost above.
chunk_dir="$(mktemp -d)"
trap 'rm -rf "$chunk_dir"' EXIT

chunk_total="$(LC_ALL=C awk -v dir="$chunk_dir" -v safe="$SAFE_BYTES" '
{
    n = split($0, terms, / \| /)
    for (i = 1; i <= n; i++) {
        t = terms[i]
        if (t == "") continue
        tl = length(t)
        # Start a new chunk when appending " | " + this term would cross the
        # threshold — the same boundary the bash loop computed on `candidate`.
        if (len == 0 || len + 3 + tl > safe) {
            if (len > 0) close(f)
            chunk++
            f = sprintf("%s/chunk-%04d", dir, chunk)
            printf "%s", t > f
            len = tl
        } else {
            printf " | %s", t > f
            len += 3 + tl
        }
    }
}
END { if (chunk > 0) close(f); print chunk + 0 }
' "$filter_file")"

if [ "$chunk_total" -eq 0 ]; then
    echo "subfloor-run-chunked: refusing to run nothing — the splitter produced 0 chunks from a ${#filter}-byte filter." >&2
    exit 3
fi

# `chunk-%04d` zero-pads so the glob's lexicographic order IS numeric order.
chunk_num=0
for chunk_file in "$chunk_dir"/chunk-*; do
    chunk_num=$((chunk_num + 1))
    chunk_bytes="$(wc -c < "$chunk_file" | tr -d ' ')"
    echo "subfloor-run-chunked: chunk $chunk_num/$chunk_total ($chunk_bytes bytes) …" >&2
    cargo nextest run --workspace -E "$(cat "$chunk_file")"
done
echo "subfloor-run-chunked: all $chunk_num chunk(s) passed." >&2
