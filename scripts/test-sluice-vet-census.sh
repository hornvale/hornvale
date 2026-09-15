#!/usr/bin/env bash
# scripts/test-sluice-vet-census.sh — the vet's census-freshness rule.
#
# WHAT IT PINS. A census measures the world THROUGH the code at the moment it
# runs, so a golden committed before the branch's own last world-producing
# commit describes a world that no longer exists. That red arrives at merge, on
# the canonical box, after the serial claim has already been spent — the single
# most expensive shape of failure this queue produces.
#
# The vet's SURFACES section reports what a candidate TOUCHES. This rule
# reports ORDER, which is different information and is what actually decides
# the question: campaign/anchor-orbital-coherence and campaign/the-tidemark
# both changed world-producing code on 2026-09-14, and only the ordering
# separated the one whose goldens were current from the one whose goldens
# disagreed with anchor's on 101 columns.
#
# IT IS FILE-LEVEL BY DESIGN and the test pins that limitation explicitly, so a
# future reader does not mistake the check for a hunk-level one. A change under
# tests/ or benches/ is excluded (it cannot move a golden); a test module
# INSIDE a world-producing file still counts, and the report says so.
#
# GIT HERMETICITY: see the header of test-sluice-vet-blocks.sh. Same rules,
# same index self-check, same incident behind it.
set -u

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
g() { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

fails=0
ok()  { printf '  ok: %s\n' "$1"; }
bad() { printf '  FAIL: %s\n' "$1"; fails=$((fails + 1)); }

index_before="$(g -C "$root" write-tree 2>/dev/null || echo unavailable)"

scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
repo="$scratch/repo"
mkdir -p "$repo"

# A branch whose history the rule can read: main, then a census, then whatever
# the case under test needs after it.
build() {
    rm -rf "$repo"; mkdir -p "$repo"
    g -C "$repo" init -q -b main
    g -C "$repo" config user.email vet@test
    g -C "$repo" config user.name vet-test
    mkdir -p "$repo/kernel/src" "$repo/windows/worldgen/src" \
             "$repo/windows/worldgen/tests" "$repo/book/src/laboratory/generated/the-census"
    echo base > "$repo/kernel/src/lib.rs"
    g -C "$repo" add -A; g -C "$repo" commit -q -m base
    g -C "$repo" update-ref refs/remotes/origin/main HEAD
    g -C "$repo" checkout -q -b work
}
census_commit() {
    echo "rows,$1" > "$repo/book/src/laboratory/generated/the-census/rows.csv"
    g -C "$repo" add -A; g -C "$repo" commit -q -m "census $1"
}
# shellcheck disable=SC2016  # the body is deliberately unexpanded; $root is
# passed to the inner shell positionally.
report() ( cd "$repo" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c '
    HV_VET_LIB=1 . "$1/scripts/sluice-vet.sh"; census_freshness "$(git rev-parse HEAD)"' _ "$root" )

echo "== a census with nothing world-producing after it"
build; census_commit 1
out="$(report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"the goldens describe this tip"*) ok "calls the goldens current" ;; *) bad "did not call them current" ;; esac
case "$out" in *"AFTER the census"*) bad "flagged a branch with nothing after its census" ;; *) ok "flags nothing" ;; esac

echo "== a world-producing source changed AFTER the census"
build; census_commit 1
echo changed > "$repo/windows/worldgen/src/lib.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m "worldgen change"
out="$(report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"AFTER the census"*) ok "flags the stale census" ;; *) bad "missed a stale census" ;; esac
case "$out" in *"windows/worldgen/src/lib.rs"*) ok "names the file" ;; *) bad "did not name the file" ;; esac
case "$out" in *"the goldens describe this tip"*) bad "also called the goldens current" ;; *) ok "does not also bless it" ;; esac

echo "== world changed BEFORE the census — the ordering, not the mere presence"
# THE DISCRIMINATING CASE, and the suite did not have it at first. Every other
# fixture here censuses as its first commit, which makes "changed since the
# census" and "changed on this branch" the same set --- so a mutant anchoring
# the range at the merge base instead of the census commit passed the whole
# file. That mutant is the entire bug this rule exists to prevent: it would
# flag campaign/anchor-orbital-coherence, which censused correctly, as stale
# for the same reason it flags campaign/the-tidemark, which did not. This is
# anchor's real shape.
build
echo changed > "$repo/kernel/src/lib.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m "world change first"
census_commit 2
out="$(report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"the goldens describe this tip"*) ok "a pre-census world change does not make the census stale" ;; *) bad "flagged a correctly-sequenced census" ;; esac
case "$out" in *"AFTER the census"*) bad "reported a pre-census change as after it — the range is anchored wrong" ;; *) ok "anchors the range at the census, not the merge base" ;; esac

echo "== a TEST source after the census is not world-producing"
build; census_commit 1
echo t > "$repo/windows/worldgen/tests/suite.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m "test change"
out="$(report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"AFTER the census"*) bad "counted a tests/ change — this is the noise the filter exists to remove" ;; *) ok "excludes tests/" ;; esac
case "$out" in *"the goldens describe this tip"*) ok "still calls the goldens current" ;; *) bad "did not call them current" ;; esac

echo "== a branch shipping no census, but changing the world"
build
echo changed > "$repo/kernel/src/lib.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m "kernel change"
out="$(report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"ships no census"*) ok "says it ships no census" ;; *) bad "did not say so" ;; esac
case "$out" in *"census_sentinel"*) ok "warns a golden may move at merge" ;; *) bad "gave no warning" ;; esac

echo "== a branch shipping no census and touching no world source"
build
echo doc > "$repo/README.md"
g -C "$repo" add -A; g -C "$repo" commit -q -m docs
out="$(report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"ships no census"*) ok "says it ships no census" ;; *) bad "did not say so" ;; esac
case "$out" in *"census_sentinel"*) bad "warned about a prose-only candidate — cry-wolf" ;; *) ok "stays quiet" ;; esac

# --- the OTHER half: did MAIN move the world under the candidate? -----------
# A census measures the world THROUGH the code, so main's movement invalidates
# a candidate's goldens exactly as its own does. This was done by hand twice
# before being written down, and the second time I got it WRONG in the
# reassuring direction -- I told campaign/anchor-orbital-coherence that
# campaign/the-coherence "touches no world-producing code" without looking, and
# it had changed five terrain and worldgen sources.
# shellcheck disable=SC2016  # deliberately unexpanded; positional args.
main_side() ( cd "$repo" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c '
    HV_VET_LIB=1 . "$1/scripts/sluice-vet.sh"
    census_freshness_against_main "$(git merge-base HEAD origin/main)"' _ "$root" )

build_main_side() {
    build
    mkdir -p "$repo/domains/terrain/src" "$repo/windows/worldgen/tests" "$repo/scripts"
    echo base > "$repo/domains/terrain/src/lib.rs"
    g -C "$repo" add -A; g -C "$repo" commit -q -m sim-base
    g -C "$repo" update-ref refs/remotes/origin/main HEAD
    g -C "$repo" checkout -q -b cand
    echo branchwork > "$repo/README.md"
    g -C "$repo" add -A; g -C "$repo" commit -q -m branchwork
}
# Advance origin/main without moving the candidate.
advance_main() {  # advance_main <path> <content>
    g -C "$repo" checkout -q main
    mkdir -p "$repo/$(dirname "$1")"
    echo "$2" > "$repo/$1"
    g -C "$repo" add -A; g -C "$repo" commit -q -m "main moves $1"
    g -C "$repo" update-ref refs/remotes/origin/main HEAD
    g -C "$repo" checkout -q cand
}

echo "== MAIN moved a world-producing source since the branch diverged"
build_main_side
advance_main domains/terrain/src/lib.rs changed
out="$(main_side)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"MAIN moved 1 world-producing"*) ok "flags main's movement" ;; *) bad "missed main's movement: $out" ;; esac
case "$out" in *domains/terrain/src/lib.rs*) ok "names the file" ;; *) bad "did not name the file" ;; esac
case "$out" in *census_fixtures_match_a_probe_of_live_seeds*) ok "names the test that settles it" ;; *) bad "gave no way to settle it" ;; esac
case "$out" in *"moved no world-producing"*) bad "also said main moved nothing" ;; *) ok "does not also bless it" ;; esac

echo "== MAIN moved only scripts and docs (the-tidemark's real case)"
build_main_side
advance_main scripts/sluice-vet.sh tooling
out="$(main_side)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"moved no world-producing"*) ok "a tooling-only main does not alarm" ;; *) bad "cried wolf on scripts/: $out" ;; esac
case "$out" in *"MAIN moved"*) bad "flagged a scripts-only advance" ;; *) ok "flags nothing" ;; esac

echo "== MAIN moved only a TEST under a sim tree"
# The same exclusion census_freshness uses: a change under tests/ cannot move a
# golden, and counting it turns this into noise nobody reads.
build_main_side
advance_main windows/worldgen/tests/suite.rs testonly
out="$(main_side)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"moved no world-producing"*) ok "excludes tests/ on main's side too" ;; *) bad "counted a tests/ change: $out" ;; esac

# --- census_pins: goldens moved, were the pins even considered? -------------
# The rule that would have caught campaign/anchor-orbital-coherence's red on
# 2026-09-14 before the chamber spent 1033 s on it. The third case is the one
# that keeps it honest: the hook's own census_guard_files pattern includes a
# GOLDEN path, and counting that pattern here scores the exact failure as
# clean, so this suite pins that a golden is never mistaken for a pin.
# shellcheck disable=SC2016  # deliberately unexpanded; $root is positional.
pins_report() ( cd "$repo" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c '
    HV_VET_LIB=1 . "$1/scripts/sluice-vet.sh"; census_pins "$(git rev-parse HEAD)"' _ "$root" )

build_pins() {
    build
    mkdir -p "$repo/book/src/laboratory/generated/the-census" \
             "$repo/windows/lab/tests/suite" "$repo/tools/census/queries/calibrate"
}

echo "== goldens moved, NO pin source touched"
build_pins
echo "rows,2" > "$repo/book/src/laboratory/generated/the-census/rows.csv"
g -C "$repo" add -A; g -C "$repo" commit -q -m "census goldens"
out="$(pins_report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"NO calibration pin source"*) ok "flags the unpinned goldens" ;; *) bad "missed unpinned goldens" ;; esac
case "$out" in *considered*) bad "also called them considered" ;; *) ok "does not also bless it" ;; esac

echo "== goldens moved AND a pin source touched"
build_pins
echo "rows,2" > "$repo/book/src/laboratory/generated/the-census/rows.csv"
echo "pin" > "$repo/windows/lab/tests/suite/calibration.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m "census goldens and pins"
out="$(pins_report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *considered*) ok "reports the pins as considered" ;; *) bad "did not report them considered" ;; esac
case "$out" in *"NO calibration pin source"*) bad "flagged a re-pinned candidate — cry-wolf" ;; *) ok "does not flag it" ;; esac

echo "== a golden is not a pin (the hook-pattern trap)"
# book/.../rows.csv is in the hook's census_guard_files on purpose. If this
# rule counted that pattern, a candidate that moved ONLY goldens would score as
# having touched a pin, and anchor's red would have read clean.
build_pins
echo "rows,2" > "$repo/book/src/laboratory/generated/the-census/rows.csv"
echo "rows,2" > "$repo/book/src/laboratory/generated/census-of-the-meeting/rows.csv" 2>/dev/null ||
    { mkdir -p "$repo/book/src/laboratory/generated/census-of-the-meeting"; echo "rows,2" > "$repo/book/src/laboratory/generated/census-of-the-meeting/rows.csv"; }
g -C "$repo" add -A; g -C "$repo" commit -q -m "two golden files, no pins"
out="$(pins_report)"; printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"2 golden rows.csv moved and NO calibration pin"*) ok "two goldens, still zero pins" ;; *) bad "counted a golden as a pin" ;; esac

echo "== no goldens moved: silent even when a pin source changed"
build_pins
echo "pin" > "$repo/windows/lab/tests/suite/calibration.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m "pins only"
out="$(pins_report)"; printf '%s\n' "$out" | sed 's/^/    /'
if [ -z "$(printf '%s' "$out" | tr -d '[:space:]')" ]; then
    ok "says nothing when no golden moved"
else
    bad "spoke about pins on a candidate that moved no golden"
fi

index_after="$(g -C "$root" write-tree 2>/dev/null || echo unavailable)"
if [ "$index_before" != "$index_after" ]; then
    bad "THIS SUITE REWROTE THE INDEX of the worktree it ran in ($index_before -> $index_after)"
fi

if [ "$fails" -eq 0 ]; then echo "test-sluice-vet-census: PASS"; else echo "test-sluice-vet-census: $fails FAILED"; fi
exit $((fails > 0))
