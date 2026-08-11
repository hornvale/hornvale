#!/usr/bin/env bash
# scripts/review-package.sh — Hornvale's review package: the superpowers
# `review-package` with a per-file size cap.
#
# WHY THIS EXISTS. The upstream script writes `git diff -U10 BASE..HEAD`
# whole. In this repo a single task routinely rewrites a committed artifact —
# `cli/tests/fixtures/world-seed-42.json` is 21,720 lines of epoch churn — and
# the reviewer then pays six figures of tokens to read a file that
# communicates nothing about the change. Measured: one task's diff was
# 2.77 MB, of which 99% was that one file; splitting it by hand produced a
# 77 KB code diff plus a 12 KB artifact sample and lost no review signal.
#
# The campaign brief has told implementers to "split code from artifacts" for
# a while. That instruction exists because the cost above already burned
# someone, which is the argument for mechanising it here rather than
# re-teaching it every campaign.
#
# WHAT IT DOES. Any single file whose diff exceeds the cap (default 50 KB) is
# emitted as its diff metadata + a bounded sample of the first hunks, with a
# pointer to the sibling `.full.diff` that always holds the complete,
# uncapped text. Nothing is lost — it is moved one Read call away, so the
# reviewer pays for it only when it has reason to.
#
# Usage: review-package.sh PLAN_FILE BASE HEAD [OUTFILE]
# Default OUTFILE: <repo-root>/.superpowers/sdd/<plan-basename>/review-<base7>..<head7>.diff
# Env: HV_REVIEW_FILE_CAP_BYTES (default 50000)
#      HV_REVIEW_FILE_SAMPLE_LINES (default 120)
set -euo pipefail

if [ $# -lt 3 ] || [ $# -gt 4 ]; then
  echo "usage: review-package.sh PLAN_FILE BASE HEAD [OUTFILE]" >&2
  exit 2
fi

plan=$1
base=$2
head=$3

[ -f "$plan" ] || { echo "no such plan file: $plan" >&2; exit 2; }
git rev-parse --verify --quiet "$base" >/dev/null || { echo "bad BASE: $base" >&2; exit 2; }
git rev-parse --verify --quiet "$head" >/dev/null || { echo "bad HEAD: $head" >&2; exit 2; }

cap=${HV_REVIEW_FILE_CAP_BYTES:-50000}
sample=${HV_REVIEW_FILE_SAMPLE_LINES:-120}

if [ $# -eq 4 ]; then
  out=$4
  mkdir -p "$(dirname "$out")"
else
  # Mirrors the plugin's `sdd-workspace`: one directory per plan, in the
  # working tree (Claude Code denies agent writes under .git/), self-ignored
  # so no plan's scratch can be committed. Replicated rather than called
  # because locating a version-numbered plugin cache from a plain script is
  # the brittle half of this; the layout is asserted in
  # scripts/test-review-package.sh.
  slug=$(basename "$plan" .md)
  if [ -z "$slug" ] || [ "$slug" = "." ] || [ "$slug" = ".." ]; then
    echo "cannot derive a workspace name from: $plan" >&2
    exit 2
  fi
  root=$(git rev-parse --show-toplevel)
  mkdir -p "$root/.superpowers/sdd/$slug"
  printf '*\n' > "$root/.superpowers/sdd/.gitignore"
  out="$root/.superpowers/sdd/$slug/review-$(git rev-parse --short "$base")..$(git rev-parse --short "$head").diff"
fi

full="${out%.diff}.full.diff"

raw=$(mktemp "${TMPDIR:-/tmp}/hv-review-raw.XXXXXX")
trap 'rm -f "$raw"' EXIT

git diff -U10 "${base}..${head}" > "$raw"

# Split on `diff --git` section boundaries and report the oversized ones. The
# path is read from the `+++ b/path` line (or `--- a/path` for a deletion)
# rather than parsed out of the `diff --git a/X b/X` header, whose two halves
# cannot be told apart when a path contains a space. Used only for this
# human-readable summary — the diff body itself is always emitted verbatim.
read_oversized() {
  LC_ALL=C awk -v cap="$cap" '
    function flush() {
      if (nlines == 0) return
      if (bytes > cap) printf "%s\t%d\t%d\n", (path == "" ? "(unknown path)" : path), bytes, nlines
    }
    /^diff --git / { flush(); nlines = 0; bytes = 0; path = "" }
    {
      nlines++; bytes += length($0) + 1
      if (path == "" && substr($0, 1, 6) == "+++ b/") path = substr($0, 7)
      if (path == "" && substr($0, 1, 6) == "--- a/") path = substr($0, 7)
    }
    END { flush() }
  ' "$raw"
}

oversized=$(read_oversized)

# The capped body. Everything before a section's first hunk header (the
# `diff --git`/`index`/`---`/`+++` lines) is always kept: that is the file's
# identity and mode, and it is what makes the sample legible.
emit_capped() {
  LC_ALL=C awk -v cap="$cap" -v sample="$sample" -v fullpath="$(basename "$full")" '
    function flush(   i, hunk, shown) {
      if (nlines == 0) return
      if (bytes <= cap) {
        for (i = 1; i <= nlines; i++) print buf[i]
        return
      }
      hunk = 0
      for (i = 1; i <= nlines; i++) if (substr(buf[i], 1, 2) == "@@") { hunk = i; break }
      if (hunk == 0) { for (i = 1; i <= nlines; i++) print buf[i]; return }
      for (i = 1; i < hunk; i++) print buf[i]
      printf "\n***** CAPPED: %d bytes / %d lines exceeds the %d-byte per-file cap.\n", bytes, nlines, cap
      printf "***** Showing the first %d lines of the diff body.\n", sample
      printf "***** The complete, uncapped diff for this file is in the sibling file: %s\n", fullpath
      printf "***** Open it ONLY if this file is material to the change under review.\n\n"
      shown = 0
      for (i = hunk; i <= nlines && shown < sample; i++) { print buf[i]; shown++ }
      printf "\n***** ...%d further lines omitted (see %s).\n\n", nlines - hunk + 1 - shown, fullpath
    }
    /^diff --git / { flush(); nlines = 0; bytes = 0 }
    { nlines++; bytes += length($0) + 1; buf[nlines] = $0 }
    END { flush() }
  ' "$raw"
}

common_header() {
  echo "# Review package: ${base}..${head}"
  echo
  echo "## Commits"
  git log --oneline "${base}..${head}"
  echo
  echo "## Files changed"
  git diff --stat "${base}..${head}"
  echo
}

# The full package is written unconditionally, so the pointer in a capped
# section is never dangling and a reviewer always has a complete artifact.
{
  common_header
  echo "## Diff (complete, uncapped)"
  cat "$raw"
} > "$full"

{
  common_header
  if [ -n "$oversized" ]; then
    echo "## Capped files"
    echo
    echo "These files exceed the ${cap}-byte per-file cap and appear below as"
    echo "diff metadata plus the first ${sample} lines. Their complete diffs are in:"
    echo "  $full"
    echo
    printf '%s\n' "$oversized" | while IFS=$'\t' read -r p b l; do
      printf '  %s — %s bytes, %s lines\n' "$p" "$b" "$l"
    done
    echo
  fi
  echo "## Diff"
  emit_capped
} > "$out"

commits=$(git rev-list --count "${base}..${head}")
out_bytes=$(wc -c < "$out" | tr -d ' ')
full_bytes=$(wc -c < "$full" | tr -d ' ')
capped_count=$([ -n "$oversized" ] && printf '%s\n' "$oversized" | wc -l | tr -d ' ' || echo 0)

echo "wrote ${out}: ${commits} commit(s), ${out_bytes} bytes, ${capped_count} file(s) capped"
echo "  full diff: ${full} (${full_bytes} bytes)"
