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
# TWO HELPERS, AND CONFLATING THEM DESTROYED A MERGE PRODUCT.
#
# `g` strips GIT_DIR and GIT_INDEX_FILE because git exports them to hooks and
# they OUTRANK `git -C` -- scripts/test-sluice-vet.sh's header explains why, and
# that protection is right for every command that must not touch a caller's
# index.
#
# But `env -u GIT_INDEX_FILE` also strips a GIT_INDEX_FILE this script SETS.
# `GIT_INDEX_FILE="$tmp/idx" gi read-tree "$base"` therefore read origin/main's
# tree into the REAL index of whatever worktree the test ran in. Inside the
# chamber that was a merge product: the merge own new files then read as
# deleted, `git add -u` staged the deletion, and the chamber committed it as
# "regenerate after outboard". tooling/the-warden landed rc=0 having erased the
# five files it added, and main's lane-outboard.sh was left calling two scripts
# that no longer existed, so every subsequent outboard failed.
#
# So index-scoped commands use `gi`, which keeps the GIT_INDEX_FILE it is
# given. Everything else keeps `g`. They are never interchangeable.
g()  { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }
gi() { env -u GIT_DIR git "$@"; }

# THIS SUITE MUST NOT DISTURB THE INDEX OF THE WORKTREE IT RUNS IN.
#
# It ran inside a chamber merge product on 2026-09-14 and rewrote that index,
# because a GIT_INDEX_FILE set on the command line was stripped by a helper
# using `env -u GIT_INDEX_FILE`. The merge own new files then read as deleted,
# the chamber staged the deletion with `git add -u`, and tooling/the-warden
# LANDED rc=0 having erased the five files it added -- leaving main calling two
# scripts that no longer existed, so every subsequent outboard failed.
#
# Recording the index tree here and comparing it at the end turns that class of
# accident from silent into a named failure. It is checked against the same
# worktree this script runs in, whichever that is.
_index_before="$(g write-tree 2>/dev/null || echo unavailable)"

base="$(g rev-parse origin/main 2>/dev/null)" || base=""
[ -n "$base" ] || { echo "test-sluice-vet-gate: SKIP — no origin/main"; exit 0; }
tmp="$(mktemp -d)"
cleanup() { for r in $refs; do g update-ref -d "$r" 2>/dev/null; done; rm -rf "$tmp"; }
refs=""; trap cleanup EXIT

# Mint a ref whose tree is origin/main's with ONE file replaced.
mint() {  # mint <name> <path-in-repo> <new-content-file>
    local name="$1" path="$2" src="$3" blob tree commit
    blob="$(g hash-object -w "$src")"
    GIT_INDEX_FILE="$tmp/idx" gi read-tree "$base"
    GIT_INDEX_FILE="$tmp/idx" gi update-index --cacheinfo "100644,$blob,$path"
    tree="$(GIT_INDEX_FILE="$tmp/idx" gi write-tree)"
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
if printf '%s' "$out" | grep -q "lane-sets.tsv"; then
    ok "the roster edit is named"
else
    bad "a lane-sets.tsv edit was not reported at all"
fi
if printf '%s' "$out" | grep -q "INERT HERE"; then
    ok "it says the change is INERT for this run"
else
    bad "it did not say the roster edit is inert — this is the silence that cost two merge slots"
fi

echo "== a change to a target a phase runs is reported as TAKING EFFECT"
# visual-check-run is reached via clients-check-run, which is what the roster
# names -- so this also exercises the one level of expansion.
g show "$base:Makefile" | sed 's/^visual-check-run:.*/&\n\t@echo probe/' > "$tmp/mk"
c_mk="$(mint makefile Makefile "$tmp/mk")"
out="$(section makefile "$c_mk")"
if printf '%s' "$out" | grep -q "visual-check-run"; then
    ok "a target reached THROUGH a roster target is found (one-level expansion)"
else
    bad "the expansion missed visual-check-run — roster-named targets only is not enough"
fi
if printf '%s' "$out" | grep -q "TAKES EFFECT"; then
    ok "it says the change takes effect on this run"
else
    bad "no TAKES EFFECT line"
fi

echo "== a phase DRIVER edit is reported as TAKING EFFECT"
# The class this section was blind to until 2026-09-14. The roster is read from
# MAIN, but the SCRIPT a roster row names runs inside the chamber worktree,
# which holds the merge product. tooling/the-adjudicator added two suites to
# scripts/lane-outboard.sh and its own merge ran them --
#   == outboard: sluice vet blocks
#   == outboard: sluice vet census
# -- while this section reported "none — no gate machinery in this diff". A
# confident absence is the worst answer available here.
g show "$base:scripts/lane-outboard.sh" > "$tmp/driver"
printf '# probe line\n' >> "$tmp/driver"
c_driver="$(mint driver scripts/lane-outboard.sh "$tmp/driver")"
out="$(section driver "$c_driver")"
if printf '%s' "$out" | grep -q "lane-outboard.sh"; then
    ok "the driver edit is named"
else
    bad "a scripts/lane-outboard.sh edit was not reported at all — the silence this case exists for"
fi
if printf '%s' "$out" | grep -q "TAKES EFFECT"; then
    ok "it says the driver takes effect on this very run"
else
    bad "a phase driver was not reported as taking effect: $out"
fi
if printf '%s' "$out" | grep -q "INERT HERE"; then
    bad "a phase driver was misreported as INERT — it runs from the merge product"
else
    ok "it is not misfiled with the roster and the queue plumbing"
fi

echo "== a TEST SUITE a phase reaches is reported (the second level)"
# The case a name pattern could not see. scripts/lane-outboard.sh is named by
# the roster; the suites IT runs are not, and they execute from the merge
# product just as it does. A candidate weakening one of them is the "retreat"
# this section exists to surface, and tooling/the-attribution --- which changes
# four such suites --- was reported as touching only one INERT file.
g show "$base:scripts/test-sluice-drain.sh" > "$tmp/suite"
printf '# probe line\n' >> "$tmp/suite"
c_suite="$(mint suite scripts/test-sluice-drain.sh "$tmp/suite")"
out="$(section suite "$c_suite")"
if printf '%s' "$out" | grep -q "test-sluice-drain.sh"; then
    ok "a suite reached THROUGH a roster-named driver is found (two-level reachability)"
else
    bad "a suite lane-outboard.sh runs was not reported: $out"
fi
if printf '%s' "$out" | grep -q "TAKES EFFECT"; then
    ok "it says the suite takes effect on this very run"
else
    bad "the suite was not reported as taking effect"
fi

echo "== a non-phase script under scripts/ is NOT gate machinery (no crying wolf)"
# worktree-take.sh is a SUBJECT under test, not a judge: it is judged by
# test-worktree-take.sh. It is mentioned by census-run.sh, and `census` is not
# a chamber phase at all (sluice-run.sh refuses it outright), so an unscoped
# reachability scan reported this ordinary helper as gate machinery. Scoping to
# sluice-run.sh's own merge_phases is what keeps it out.
g show "$base:scripts/worktree-take.sh" > "$tmp/util"
printf '# probe line\n' >> "$tmp/util"
c_util="$(mint util scripts/worktree-take.sh "$tmp/util")"
out="$(section util "$c_util")"
if printf '%s' "$out" | grep -q "TAKES EFFECT"; then
    bad "an ordinary helper was reported as gate machinery: $out"
else
    ok "an ordinary scripts/ helper is not reported"
fi

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
if printf '%s' "$out" | grep -q "none — no gate machinery"; then
    ok "it says 'none' rather than printing an empty header"
else
    bad "a clean candidate did not get a clear 'none'"
fi

echo "== an absent but WELL-FORMED sha is refused, not vetted"
# THE CONFIDENT-SILENCE CASE. `git rev-parse <40-hex>` does not verify: it
# echoes an unknown sha back and exits 0. The vet's emptiness guard therefore
# never fired, and it printed a complete, plausible, entirely empty report --
# no decisions, no census, no surfaces, a clean Definition of Done -- for a
# commit that does not exist. Found by pasting a 12-character prefix padded out
# by hand, which is exactly how an operator produces one of these.
absent=de4f0c3b989a8bbd99b18f5c5e3eafaba5a8fcba
if g rev-parse --verify --quiet "$absent^{commit}" >/dev/null 2>&1; then
    bad "the fixture sha exists in this repository — pick another; this case is now vacuous"
else
    ok "CONTROL: the fixture sha is genuinely absent (so the case below can mean something)"
    set +e
    out="$(bash scripts/sluice-vet.sh campaign/zz-absent "$absent" 2>&1)"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ]; then
        ok "an absent sha is refused (rc=$rc), not vetted"
    else
        bad "the vet exited 0 on a sha that does not exist"
    fi
    if printf '%s' "$out" | grep -q "GATE MACHINERY"; then
        bad "it produced a report for a nonexistent commit — the confident silence itself"
    else
        ok "it produces no report at all, so there is nothing to act on"
    fi
    if printf '%s' "$out" | grep -qi "not a commit"; then
        ok "the refusal says what is wrong"
    else
        bad "the refusal does not say what is wrong: $out"
    fi
fi

echo "== this suite left the surrounding worktree's index alone"
_index_after="$(g write-tree 2>/dev/null || echo unavailable)"
if [ "$_index_before" = "unavailable" ] || [ "$_index_after" = "unavailable" ]; then
    ok "SKIP: could not read the index tree on this host"
elif [ "$_index_before" = "$_index_after" ]; then
    ok "the index tree is unchanged ($_index_before)"
else
    bad "THIS SUITE REWROTE THE INDEX of the worktree it ran in ($_index_before -> $_index_after). Inside the chamber that destroys the merge product: its new files read as deleted and 'git add -u' commits the deletion."
fi

printf '\ntest-sluice-vet-gate: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
