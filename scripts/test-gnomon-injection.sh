#!/usr/bin/env bash
# scripts/test-gnomon-injection.sh — the authoring script's guards, driven
# without a build.
#
# WHY THIS TEST EXISTS. gnomon-injection.sh refused to run on ANY dirty tree
# outside its own fixture directory, and a census delivery's staged goldens are
# exactly that dirt — so a census that grew the registry could not re-author
# the arms its own gate compares against (The Warp, ledger #12; The Spillway
# spec §1, leg 2). The guard's stated job is narrower than its old predicate:
# it protects SOURCE it mutates and restores, and the manifest's `sha` claim
# about what was BUILT. book/ and docs/ are neither.
#
# DIRECTION THIS TEST ENFORCES: dirt under book/, docs/, clients/ and the
# fixture dir is allowed; dirt anywhere else still refuses. Both halves are
# asserted, because "book/ is allowed" alone would pass for a guard that had
# been deleted. clients/ joined the exclusion because the census's own
# artifact sweep regenerates `clients/game/core/tests/fixtures/`, and no
# `lab run` reads clients/ at all.
set -uo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
g() { env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE -u GIT_COMMON_DIR git -C "$tmp" "$@"; }

# A scratch repo carrying the script and the two files it sources. The script
# `cd`s to its own ../, so it runs against THIS repo, never the real one.
mkdir -p "$tmp/scripts" "$tmp/domains/terrain/src" "$tmp/book/src" "$tmp/docs" \
         "$tmp/clients/game/core/tests/fixtures" \
         "$tmp/windows/lab/tests/fixtures/injection/baseline-a"
cp "$root/scripts/gnomon-injection.sh" "$tmp/scripts/"
cp "$root/scripts/census-canonical-host.sh" "$tmp/scripts/"
cp "$root/scripts/census-canonical-host.txt" "$tmp/scripts/"
printf 'const X: f64 = 1.0;\n' > "$tmp/domains/terrain/src/strata.rs"
printf 'chapter\n' > "$tmp/book/src/x.md"
printf 'baseline\n' > "$tmp/docs/timings.md"
printf '{}\n' > "$tmp/clients/game/core/tests/fixtures/x.json"
printf 'readme\n' > "$tmp/windows/lab/tests/fixtures/injection/README.md"
printf '{}\n' > "$tmp/windows/lab/tests/fixtures/injection/baseline-a/schema.json"
g init -q 2>/dev/null
g config user.name t; g config user.email t@t
g add -A; g commit -qm base

canonical="$(tr '[:upper:]' '[:lower:]' < "$root/scripts/census-canonical-host.txt")"
here="$(hostname -s 2>/dev/null || hostname)"; here="$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')"

# Runs `check` with the host guard lifted (PILOT) unless we ARE the canonical
# box, so every tree-guard arm below asserts the TREE verdict and nothing else.
check() {
    if [ "$here" = "$canonical" ]; then
        (cd "$tmp" && bash scripts/gnomon-injection.sh check 2>&1)
    else
        (cd "$tmp" && HV_GNOMON_PILOT=1 bash scripts/gnomon-injection.sh check 2>&1)
    fi
}
reset_tree() { g reset -q --hard; g clean -qfd; }

# --- the host guard, exercised for real ------------------------------------
out="$(cd "$tmp" && bash scripts/gnomon-injection.sh check 2>&1)"; rc=$?
if [ "$here" = "$canonical" ]; then
    if [ "$rc" -eq 0 ]; then ok "on the canonical box, check passes a clean tree without PILOT"
    else bad "on the canonical box, check refused a clean tree: $out"; fi
else
    if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -q 'REFUSING to author the battery'; then
        ok "off the canonical box, check refuses and names the host"
    else bad "off-host check gave rc=$rc without the host refusal: $out"; fi
fi

# --- the tree guard --------------------------------------------------------
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "clean tree: check passes"; else bad "clean tree refused: $out"; fi

printf 'edited\n' >> "$tmp/book/src/x.md"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "a MODIFIED book/ file is allowed (the census's own output lives here)"
else bad "modified book/ refused — the delivery's goldens would deadlock again: $out"; fi

printf 'svg\n' > "$tmp/book/src/new.svg"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "an UNTRACKED book/ file is allowed (a census adds new golden files)"
else bad "untracked book/ refused: $out"; fi

printf 'row\n' >> "$tmp/docs/timings.md"
g add -A -- book docs
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "STAGED book/ and docs/ dirt is allowed (a delivery stages before it re-authors)"
else bad "staged book/docs dirt refused: $out"; fi
reset_tree

printf 'edited\n' >> "$tmp/clients/game/core/tests/fixtures/x.json"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "a MODIFIED clients/ file is allowed (the census's artifact sweep regenerates it)"
else bad "modified clients/ refused — the delivery's own sweep dirt would deadlock: $out"; fi
reset_tree

printf 'dirt\n' > "$tmp/windows/lab/tests/fixtures/injection/baseline-a/rows.csv"
out="$(check)"; rc=$?
if [ "$rc" -eq 0 ]; then ok "dirt inside the fixture directory is allowed (it is the script's own output)"
else bad "fixture-dir dirt refused: $out"; fi
reset_tree

# THE CONTROL: the guard still guards. Without these two, every arm above
# passes for a guard that has been deleted.
printf 'const X: f64 = 2.0;\n' > "$tmp/domains/terrain/src/strata.rs"
out="$(check)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -q 'domains/terrain/src/strata.rs'; then
    ok "CONTROL: a modified source file refuses and is named"
else bad "modified source did NOT refuse (rc=$rc): $out"; fi
reset_tree

printf 'x\n' > "$tmp/scripts/new.sh"; g add scripts/new.sh
out="$(check)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -q 'scripts/new.sh'; then
    ok "CONTROL: a staged new file outside book/docs refuses and is named"
else bad "staged new script did NOT refuse (rc=$rc): $out"; fi
reset_tree

out="$(cd "$tmp" && HV_GNOMON_PILOT=1 bash scripts/gnomon-injection.sh check extra 2>&1)"; rc=$?
if [ "$rc" -eq 2 ]; then ok "check with extra arguments is a usage error (rc=2)"
else bad "check with extra arguments gave rc=$rc: $out"; fi

# Anti-vacuity: `check` must never have built or authored anything.
if [ ! -f "$tmp/windows/lab/tests/fixtures/injection/manifest.json" ] && [ ! -d "$tmp/target" ]; then
    ok "check authored nothing and built nothing"
else bad "check left a manifest or a target/ behind — it is not a pre-flight"; fi

printf '\ntest-gnomon-injection: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
