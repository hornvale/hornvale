#!/usr/bin/env bash
# Tests for sluice-vet.sh's GATE MACHINERY section.
#
# WHAT IT MUST GET RIGHT, and the two halves pull in opposite directions:
#   - report a change the chamber WILL read (it weakens or repairs this run), and
#   - report a change the chamber will NOT read (it is inert, and silently so --
#     campaign/the-coherence lost two merge slots to exactly that on 2026-09-14).
# And it must stay quiet about ordinary Makefile edits, or every campaign learns
# to scroll past it.
#
# The probes are MINTED here rather than borrowed from live branches: a test
# anchored on somebody's campaign ref rots the moment that branch is deleted.
# Same reason scripts/test-sluice-vet.sh mints its zz-vet-* probes.
set -u
pass=0; fail=0
ok()  { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad() { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root" || exit 1
g() { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

base="$(g rev-parse origin/main 2>/dev/null)" || base=""
[ -n "$base" ] || { echo "test-sluice-vet-gate: SKIP — no origin/main"; exit 0; }
tmp="$(mktemp -d)"
cleanup() { for r in $refs; do g update-ref -d "$r" 2>/dev/null; done; rm -rf "$tmp"; }
refs=""; trap cleanup EXIT

# Mint a ref whose tree is origin/main's with ONE file replaced.
mint() {  # mint <name> <path-in-repo> <new-content-file>
    local name="$1" path="$2" src="$3" blob tree commit
    blob="$(g hash-object -w "$src")"
    GIT_INDEX_FILE="$tmp/idx" g read-tree "$base"
    GIT_INDEX_FILE="$tmp/idx" g update-index --cacheinfo "100644,$blob,$path"
    tree="$(GIT_INDEX_FILE="$tmp/idx" g write-tree)"
    commit="$(g commit-tree "$tree" -p "$base" -m "probe: $name

Sluice-Headline: probe")"
    g update-ref "refs/remotes/origin/campaign/zz-gate-$name" "$commit"
    refs="$refs refs/remotes/origin/campaign/zz-gate-$name"
    printf '%s' "$commit"
}
section() { bash scripts/sluice-vet.sh "campaign/zz-gate-$1" "$2" 2>&1 | sed -n '/GATE MACHINERY/,/^SHAPE/p'; }

echo "== a roster edit is reported as INERT (the chamber reads it from main)"
g show "$base:scripts/lane-sets.tsv" > "$tmp/roster"
printf '# probe line\n' >> "$tmp/roster"
c_roster="$(mint roster scripts/lane-sets.tsv "$tmp/roster")"
out="$(section roster "$c_roster")"
printf '%s' "$out" | grep -q "lane-sets.tsv" && ok "the roster edit is named" \
    || bad "a lane-sets.tsv edit was not reported at all"
printf '%s' "$out" | grep -q "INERT HERE" \
    && ok "it says the change is INERT for this run" \
    || bad "it did not say the roster edit is inert — this is the silence that cost two merge slots"

echo "== a change to a target a phase runs is reported as TAKING EFFECT"
# visual-check-run is reached via clients-check-run, which is what the roster
# names -- so this also exercises the one level of expansion.
g show "$base:Makefile" | sed 's/^visual-check-run:.*/&\n\t@echo probe/' > "$tmp/mk"
c_mk="$(mint makefile Makefile "$tmp/mk")"
out="$(section makefile "$c_mk")"
printf '%s' "$out" | grep -q "visual-check-run" \
    && ok "a target reached THROUGH a roster target is found (one-level expansion)" \
    || bad "the expansion missed visual-check-run — roster-named targets only is not enough"
printf '%s' "$out" | grep -q "TAKES EFFECT" \
    && ok "it says the change takes effect on this run" || bad "no TAKES EFFECT line"

echo "== an ordinary Makefile edit is NOT reported (no crying wolf)"
g show "$base:Makefile" | sed 's/^sweep-dry:.*/&\n\t@echo probe/' > "$tmp/mk2"
c_mk2="$(mint quiet Makefile "$tmp/mk2")"
out="$(section quiet "$c_mk2")"
if printf '%s' "$out" | grep -q "TAKES EFFECT"; then
    bad "a non-gate target (sweep-dry) was reported — every Makefile edit would scream and the section gets skipped"
else
    ok "CONTROL: a non-gate Makefile target is not reported"
fi

echo "== a candidate touching no gate machinery says so plainly"
g show "$base:docs/README.md" > "$tmp/doc" 2>/dev/null || echo x > "$tmp/doc"
printf '\n<!-- probe -->\n' >> "$tmp/doc"
c_doc="$(mint none docs/README.md "$tmp/doc")"
out="$(section none "$c_doc")"
printf '%s' "$out" | grep -q "none — no gate machinery" \
    && ok "it says 'none' rather than printing an empty header" \
    || bad "a clean candidate did not get a clear 'none'"

printf '\ntest-sluice-vet-gate: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
