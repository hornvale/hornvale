#!/usr/bin/env bash
# scripts/test-review-package.sh — prove the review package's per-file cap
# SHRINKS the reviewer's diff without LOSING anything.
#
# Both halves are asserted, because either alone can pass for the wrong
# reason: a cap that dropped the file entirely would satisfy "smaller", and
# emitting the whole diff twice would satisfy "nothing lost". The
# discriminating assertion is that a marker planted at the tail of the big
# file is ABSENT from the capped package and PRESENT in the full sibling.
#
# Not part of `make gate` (it builds throwaway git repos and no Rust);
# `make shellcheck` lints it, and it runs on demand.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
script="$repo_root/scripts/review-package.sh"
work="$(mktemp -d "${TMPDIR:-/tmp}/hv-review-package-test.XXXXXX")"
trap 'rm -rf "$work"' EXIT

fails=0
pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1" >&2; fails=$((fails + 1)); }

# The repo and the packages live in SEPARATE directories. When they shared
# one, `git add -A` in the small-diff case swept the 102 KB package written
# by an earlier case into the commit under test, and the "small-only diff is
# not capped" assertion failed for a reason that had nothing to do with the
# script. Keep generated packages out of the tree being diffed.
repo="$work/repo"
outdir="$work/out"
mkdir -p "$repo" "$outdir"
cd "$repo"
git init -q .
git config user.email test@example.com
git config user.name test

mkdir -p docs/plans
cat > docs/plans/the-fixture.md <<'EOF'
# The Fixture
A plan file. Only its basename is read.
EOF

# --- BASE: a small code file and a small artifact ---
cat > code.rs <<'EOF'
pub fn original() -> u32 {
    1
}
EOF
printf '{"rows":[\n' > artifact.json
printf '  {"id":0,"v":0}\n' >> artifact.json
printf ']}\n' >> artifact.json
git add -A
git commit -qm base
base="$(git rev-parse HEAD)"

# --- HEAD: a small code edit, and an artifact rewritten past the cap ---
cat > code.rs <<'EOF'
pub fn original() -> u32 {
    UNIQUE_CODE_MARKER_KEEP_ME
}
EOF
{
  printf '{"rows":[\n'
  for i in $(seq 1 4000); do printf '  {"id":%d,"v":%d},\n' "$i" $((i * 7)); done
  printf '  {"tail":"UNIQUE_TAIL_MARKER_DEEP_IN_ARTIFACT"}\n'
  printf ']}\n'
} > artifact.json
git add -A
git commit -qm head
head="$(git rev-parse HEAD)"

# Guard the fixture itself: if the artifact were under the cap the whole test
# would pass vacuously. Assert the precondition before asserting the effect.
artifact_bytes="$(git diff "${base}..${head}" -- artifact.json | wc -c | tr -d ' ')"
if [ "$artifact_bytes" -le 50000 ]; then
  fail "fixture is too small to exercise the cap (${artifact_bytes} bytes) — test would be vacuous"
  exit 1
fi
pass "fixture artifact diff is ${artifact_bytes} bytes, above the 50000-byte cap"

out="$outdir/pkg.diff"
"$script" docs/plans/the-fixture.md "$base" "$head" "$out" > "$outdir/stdout.txt"
full="$outdir/pkg.full.diff"

[ -f "$full" ] || { fail "sibling full diff was not written to $full"; exit 1; }

out_bytes="$(wc -c < "$out" | tr -d ' ')"
full_bytes="$(wc -c < "$full" | tr -d ' ')"

# --- 1. It shrinks ---
if [ "$out_bytes" -lt $((full_bytes / 10)) ]; then
  pass "capped package is ${out_bytes} bytes vs ${full_bytes} full (<10%)"
else
  fail "capped package did not shrink: ${out_bytes} vs ${full_bytes} full"
fi

# --- 2. The code change survives in full ---
if grep -q UNIQUE_CODE_MARKER_KEEP_ME "$out"; then
  pass "the small code file's change is present in the capped package"
else
  fail "the small code file's change was lost from the capped package"
fi

# --- 3. The discriminating pair: the artifact's tail is moved, not shown ---
if grep -q UNIQUE_TAIL_MARKER_DEEP_IN_ARTIFACT "$out"; then
  fail "the capped package still carries the artifact's tail — nothing was capped"
else
  pass "the artifact's tail is absent from the capped package"
fi
if grep -q UNIQUE_TAIL_MARKER_DEEP_IN_ARTIFACT "$full"; then
  pass "the artifact's tail is present in the full sibling — nothing was lost"
else
  fail "the artifact's tail is missing from the full sibling — content was DESTROYED"
fi

# --- 4. The capped file is named, and points at the sibling ---
if grep -q "CAPPED:" "$out" && grep -q "artifact.json" "$out"; then
  pass "the capped section names the file and is marked CAPPED"
else
  fail "the capped section is missing its marker or filename"
fi
if grep -q "$(basename "$full")" "$out"; then
  pass "the capped section points at the sibling full diff by name"
else
  fail "the capped section does not name the sibling full diff"
fi

# --- 5. The full sibling is byte-identical to a plain `git diff -U10` ---
git diff -U10 "${base}..${head}" > "$outdir/expected.diff"
sed -n '/^## Diff (complete, uncapped)$/,$p' "$full" | tail -n +2 > "$outdir/actual.diff"
if cmp -s "$outdir/expected.diff" "$outdir/actual.diff"; then
  pass "the full sibling's diff body is byte-identical to git diff -U10"
else
  fail "the full sibling's diff body diverges from git diff -U10"
fi

# --- 6. A small-only diff is NOT capped (no false positives) ---
cat > code.rs <<'EOF'
pub fn original() -> u32 {
    2
}
EOF
git add -A
git commit -qm small
small_head="$(git rev-parse HEAD)"
"$script" docs/plans/the-fixture.md "$head" "$small_head" "$outdir/small.diff" > /dev/null
if grep -q "CAPPED:" "$outdir/small.diff" || grep -q "## Capped files" "$outdir/small.diff"; then
  fail "a small-only diff was capped — the cap fires when it should not"
else
  pass "a small-only diff is emitted whole"
fi

# --- 7. The default OUTFILE lands in the plan-scoped workspace ---
"$script" docs/plans/the-fixture.md "$base" "$head" > "$outdir/default.txt"
expected_dir="$repo/.superpowers/sdd/the-fixture"
if [ -d "$expected_dir" ] && grep -q "$expected_dir/review-" "$outdir/default.txt"; then
  pass "default OUTFILE lands in .superpowers/sdd/<plan-basename>/"
else
  fail "default OUTFILE did not land in $expected_dir"
fi
if [ "$(cat "$repo/.superpowers/sdd/.gitignore")" = "*" ]; then
  pass "the workspace self-ignores"
else
  fail "the workspace .gitignore is missing or wrong"
fi

echo
if [ "$fails" -eq 0 ]; then
  echo "test-review-package: PASS"
else
  echo "test-review-package: FAIL ($fails)" >&2
  exit 1
fi
