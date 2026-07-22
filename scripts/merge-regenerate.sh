#!/usr/bin/env bash
# scripts/merge-regenerate.sh %O %A %B %P — a git merge driver (PROC-12).
#
# Regenerates a fully-re-derived documentation artifact instead of
# textually merging it: neither side's committed text is authoritative,
# only a fresh run of the one command that produces this file is.
#
# Registered via `make install-hooks` (git config merge.hv-regenerate.
# driver); referenced per-path in .gitattributes. A clone that hasn't
# run that target sees no change in behavior — git falls back to its
# normal 3-way merge for any .gitattributes-named driver with no
# corresponding `git config` entry (gitattributes(5)).
#
# Exits nonzero on ANY failure (unrecognized path, or the generator
# command itself failing — most likely because a *different*, still-
# unresolved conflict elsewhere in the same merge means the crate
# doesn't currently build) and writes nothing to $A in that case. Git's
# actual behavior for a custom `merge=<driver>`-attributed path on a
# nonzero exit (verified directly, not assumed from prose): the path is
# left marked UNMERGED in the index (three conflict stages — base/ours/
# theirs), with the working-tree file at its pre-merge "ours" content —
# NOT textual <<<<<<< diff3 markers, which only appear for git's
# default, undriven conflict handling. `git status`/`git ls-files -u`
# show the real conflict either way; this file is never silently left
# with partial/broken driver output, and the merge as a whole still
# fails, requiring a human to resolve it normally. Never trust $A's
# on-disk content after a nonzero exit.
set -euo pipefail

ours="$2"
path="$4"

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT

case "$path" in
    docs/audits/type-audit-report.md)
        cargo run --manifest-path tools/type-audit/Cargo.toml -- report > "$tmp_out"
        ;;
    book/src/reference/concept-registry-generated.md)
        cargo run -q -p hornvale -- concepts > "$tmp_out"
        ;;
    book/src/reference/concept-manifest-generated.md)
        cargo run -q -p hornvale -- concepts --manifest > "$tmp_out"
        ;;
    book/src/reference/stream-manifest-generated.md)
        cargo run -q -p hornvale -- streams > "$tmp_out"
        ;;
    book/src/reference/phonology.md)
        cargo run -q -p hornvale -- phonology > "$tmp_out"
        ;;
    book/src/reference/proto-goblinoid-generated.md)
        cargo run -q -p hornvale -- proto > "$tmp_out"
        ;;
    *)
        echo "merge-regenerate: no dispatch entry for '$path'" >&2
        exit 1
        ;;
esac

cp "$tmp_out" "$ours"
