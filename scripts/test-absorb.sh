#!/usr/bin/env bash
# scripts/test-absorb.sh — what `make absorb` resolves, and what it refuses to.
#
# THE REFUSAL IS THE LOAD-BEARING TEST. A script that auto-resolves conflicts is
# only safe if it stops at the right ones: the value of resolving
# `docs/audits/type-audit-report.md` is small next to the cost of quietly
# resolving a source file or a census golden. So the "stops on a source
# conflict" and "stops on a census golden" cases are the ones that matter, and
# the happy path is the easy half.
#
# These drive the REAL script against a scratch repo, with the regeneration
# stubbed through HV_ABSORB_REGEN — a full `make rebaseline` needs the whole
# toolchain and cannot run in a temp directory. What is under test is which
# conflicts are resolved and which halt it, not whether the generator works.
set -uo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# `git -C` alone does NOT scope which repository git acts on: GIT_DIR is
# exported into hooks and outranks -C (the lesson tools/board/src/git.rs
# carries), so the location vars are scrubbed on every call.
g() { env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE -u GIT_COMMON_DIR git "$@"; }

# A scratch repo carrying the two files absorb.sh reads, plus a source file.
build_repo() {
    rm -rf "$tmp/r"; mkdir -p "$tmp/r/docs/audits" "$tmp/r/src" "$tmp/r/scripts"
    cp "$root/scripts/sluice-phases.sh" "$tmp/r/scripts/"
    cp "$root/scripts/absorb.sh"        "$tmp/r/scripts/"
    cat > "$tmp/r/docs/generated-paths.txt" <<'DECL'
# path	author
docs/audits/	artifacts
docs/audits/hand-written.md	none(a one-off, never regenerated)
book/census/	census
DECL
    mkdir -p "$tmp/r/book/census"
    printf 'count 1\n'  > "$tmp/r/docs/audits/report.md"
    printf 'by hand 1\n'> "$tmp/r/docs/audits/hand-written.md"
    printf 'rows 1\n'   > "$tmp/r/book/census/rows.csv"
    printf 'fn main(){}\n' > "$tmp/r/src/lib.rs"
    ( cd "$tmp/r" && g init -q -b main . && g config user.email a@a && g config user.name a \
        && g add -A && g commit -qm root ) >/dev/null 2>&1
}

# main and the branch both change $1, guaranteeing a conflict there.
diverge_on() {
    ( cd "$tmp/r" || exit 1
      g checkout -q -b campaign/x
      printf 'branch side\n' > "$1"; g add -A; g commit -qm branch
      g checkout -q main
      printf 'main side\n'   > "$1"; g add -A; g commit -qm main
      g checkout -q campaign/x
      # absorb.sh fetches origin; point origin at ourselves so origin/main exists.
      g remote add origin "$tmp/r" 2>/dev/null || true
      g fetch -q origin 2>/dev/null || true
    ) >/dev/null 2>&1
}

run_absorb() {
    ( cd "$tmp/r" && HV_ABSORB_REGEN="true" env -u GIT_DIR -u GIT_INDEX_FILE \
        bash scripts/absorb.sh 2>&1 )
}

# --- 1. the case that motivated this: an artifacts-authored conflict ---------
build_repo; diverge_on "docs/audits/report.md"
out="$(run_absorb)"; rc=$?
if [ "$rc" = "0" ] && printf '%s' "$out" | grep -q 'artifacts-authored'; then
    ok "an artifacts-authored conflict is resolved, not handed to a human"
else
    bad "artifacts conflict: rc=$rc, out=$(printf '%s' "$out" | tail -2 | tr '\n' ' ')"
fi
if [ -z "$( ( cd "$tmp/r" && g diff --name-only --diff-filter=U ) )" ]; then
    ok "no conflict is left in the tree afterwards"
else
    bad "a conflict survived the resolution"
fi

# --- 2. THE ONE THAT MATTERS: a source conflict must STOP it ----------------
build_repo; diverge_on "src/lib.rs"
out="$(run_absorb)"; rc=$?
if [ "$rc" = "4" ] && printf '%s' "$out" | grep -q 'needs you'; then
    ok "REFUSAL: a source conflict stops the script"
else
    bad "a source conflict did not stop it: rc=$rc"
fi
if printf '%s' "$out" | grep -q 'not a declared generated path'; then
    ok "REFUSAL names the offending path's author so the human knows why"
else
    bad "the refusal did not explain which path stopped it"
fi

# --- 3. a census golden must STOP it, though it IS declared -----------------
build_repo; diverge_on "book/census/rows.csv"
out="$(run_absorb)"; rc=$?
if [ "$rc" = "4" ] && printf '%s' "$out" | grep -q 'author=census'; then
    ok "REFUSAL: a census golden stops it — declared is not the same as artifacts"
else
    bad "a census golden was auto-resolved or mis-reported: rc=$rc"
fi

# --- 4. a hand-written file INSIDE a generated directory must STOP it -------
# The precedence case: docs/audits/ is `artifacts`, but this file overrides it
# with `none`. Getting this wrong silently overwrites hand-written prose.
build_repo; diverge_on "docs/audits/hand-written.md"
out="$(run_absorb)"; rc=$?
if [ "$rc" = "4" ] && printf '%s' "$out" | grep -q 'author=none'; then
    ok "REFUSAL: an overriding none() row beats its generated parent directory"
else
    bad "a hand-written file in a generated dir was auto-resolved: rc=$rc"
fi

# --- 5. refuses a dirty tree rather than merging over uncommitted work ------
build_repo; diverge_on "docs/audits/report.md"
printf 'uncommitted\n' >> "$tmp/r/src/lib.rs"
out="$(run_absorb)"; rc=$?
if [ "$rc" = "2" ] && printf '%s' "$out" | grep -q 'uncommitted'; then
    ok "REFUSAL: a dirty tree is refused before anything is merged"
else
    bad "a dirty tree was not refused: rc=$rc"
fi

# --- 6. refuses on main ------------------------------------------------------
build_repo
( cd "$tmp/r" && g checkout -q main && g remote add origin "$tmp/r" 2>/dev/null; true ) >/dev/null 2>&1
out="$(run_absorb)"; rc=$?
if [ "$rc" = "2" ] && printf '%s' "$out" | grep -q 'on main'; then
    ok "REFUSAL: running on main is refused"
else
    bad "running on main was not refused: rc=$rc"
fi

# --- 7. THE COMPOSITION TEST: mouth, chamber and absorb must AGREE ----------
# The individual layers being right is not the property that matters. If the
# mouth admits a conflict set the chamber then refuses, the candidate dies at
# the merge step having ALREADY TAKEN THE BOX — strictly worse than the bounce
# this change removes. The Turnstile shipped two guards that were each green and
# could not see each other; the defect was the disagreement, not either half.
#
# All three call sluice_is_regenerated_only, so this asserts the wiring: that
# each file really routes its decision through that one function rather than
# carrying a second copy of the rule.
for f in sluice-mouth.sh sluice-run.sh absorb.sh; do
    if grep -q 'sluice_is_regenerated_only' "$root/scripts/$f"; then
        ok "AGREEMENT: $f routes its decision through the shared classifier"
    else
        bad "$f does not call sluice_is_regenerated_only — a second copy of the rule will drift"
    fi
done
for f in sluice-mouth.sh sluice-run.sh absorb.sh; do
    if grep -q 'sluice-phases\.sh' "$root/scripts/$f"; then
        ok "AGREEMENT: $f sources the one implementation"
    else
        bad "$f calls the classifier without sourcing it"
    fi
done
# And the classifier itself must be DEFINED exactly once. Excludes test files:
# this file names the function in its own grep, and a scanner that matches its
# own pattern text reported "2 implementations" on the first run — the same trap
# that has bitten prose-about-a-command repeatedly in this repo.
impls="$(grep -rl '^sluice_is_regenerated_only() {' "$root/scripts/" 2>/dev/null | grep -vc '/test-')"
if [ "$impls" = "1" ]; then
    ok "AGREEMENT: exactly one implementation of the rule exists"
else
    bad "found $impls implementations of sluice_is_regenerated_only — one is the point"
fi

printf '\ntest-absorb: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
