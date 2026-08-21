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

# Refuse unless this path is the ONLY conflicted one in the merge as a
# whole. When any other path is conflicted, the working tree is not the
# merge product (those paths sit at ours' content), so a regeneration
# here would silently bake ours-flavored output into an artifact the
# human then commits after resolving the rest, possibly by taking
# theirs. Exit nonzero and let git leave this path unmerged like any
# other conflict.
#
# The index CANNOT be the signal here: git performs the whole merge in
# memory and writes conflict stages to the index only AFTER every
# driver has run, so `git ls-files -u` is always empty at this moment
# (verified against real git behavior, not assumed from prose). The
# signal instead is `git merge-tree --write-tree`, which recomputes the
# merge in memory and lists the conflicted paths. Two cautions verified
# directly: it HONORS .gitattributes merge drivers (so ours is disabled
# below, or the probe would recurse into itself), and its first output
# line is the merged tree oid, with conflicted entries in `ls-files
# -u` format after it. Only real merges are checked (MERGE_HEAD); a
# lone cherry-pick/rebase conflict falls through to the legacy
# regenerate-always behavior.
gitdir="$(git rev-parse --git-dir)"
if [ -f "$gitdir/MERGE_HEAD" ]; then
    others="$(git -c merge.hv-regenerate.driver=/bin/false \
        merge-tree --write-tree HEAD MERGE_HEAD \
        | awk 'NR > 1 && NF >= 4 && $4 != p' p="$path")"
    if [ -n "$others" ]; then
        echo "merge-regenerate: other unresolved conflicts present; refusing to regenerate '$path' from a tree that is not the merge product" >&2
        exit 1
    fi
fi

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
