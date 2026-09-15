#!/usr/bin/env bash
# scripts/test-sluice-vet-projection.sh — the vet's projected-merge rule.
#
# WHAT IT PINS. The mouth checks a candidate against TODAY's main, where it may
# merge cleanly and be correctly admitted. The chamber merges main AT DISPATCH
# TIME, by which point the rows ahead in the queue have landed. A candidate can
# therefore pass the mouth, take the strictly serial claim, and die at <merge>
# rc=10 having tested nothing — campaign/the-weft, 2026-09-04.
#
# Two live catches on the night this was written, both invisible to the mouth:
# campaign/the-tidemark against campaign/the-coherence (one line of
# cli/tests/suite/heavy_tier.rs, where both bump an array length 40 -> 41 and
# the merged answer is 42), and campaign/anchor-orbital-coherence against both
# of them (38 unresolvable paths, almost all census goldens).
#
# THE DIRECTION THAT MATTERS IS THE FALSE CLEAN. The first draft dropped any
# queued row that conflicted while being folded onto the projection — which
# silently removed the-tidemark, whose own conflict was almost entirely
# artifacts-authored bookkeeping the chamber regenerates, and then reported
# anchor as merging cleanly when a hand-run of the same simulation found 38
# conflicts. That case is pinned below.
#
# GIT HERMETICITY: see test-sluice-vet-blocks.sh. Same rules, same self-check.
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
export HV_SLUICE_DIR="$scratch/queue"
mkdir -p "$HV_SLUICE_DIR"

# A repo with main, plus branches that each edit the same line differently.
build() {
    rm -rf "$repo"; mkdir -p "$repo/docs" "$repo/src"
    g -C "$repo" init -q -b main
    g -C "$repo" config user.email vet@test
    g -C "$repo" config user.name vet-test
    printf '# path\tauthor\ndocs/report.md\tartifacts\n' > "$repo/docs/generated-paths.txt"
    printf 'const N = 40;\n' > "$repo/src/tier.rs"
    printf 'generated\n' > "$repo/docs/report.md"
    g -C "$repo" add -A; g -C "$repo" commit -q -m base
    g -C "$repo" update-ref refs/remotes/origin/main HEAD
    : > "$HV_SLUICE_DIR/queue.tsv"
}
# ts \t id \t branch \t sha \t state \t kind \t note
row() { printf '%s\treq-%s\t%s\t%s\t%s\tmerge\t\n' "$1" "$2" "$3" "$4" "$5" >> "$HV_SLUICE_DIR/queue.tsv"; }

# shellcheck disable=SC2016  # deliberately unexpanded; positional args.
project() ( cd "$repo" && env -u GIT_DIR -u GIT_INDEX_FILE bash -c '
    HV_VET_LIB=1 . "$1/scripts/sluice-vet.sh"; projected_merge "$2" "$3"' _ "$root" "$2" "$3" )

echo "== a candidate with nothing queued ahead"
build
g -C "$repo" checkout -q -b campaign/solo main
printf 'const N = 41;\n' > "$repo/src/tier.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m solo
SOLO="$(g -C "$repo" rev-parse HEAD)"
row 2026-01-01T00:00:00Z a campaign/solo "$SOLO" queued
out="$(project x "$SOLO" campaign/solo)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"nothing queued ahead"*) ok "says the mouth verdict is the whole story" ;; *) bad "did not say so: $out" ;; esac

echo "== a REAL conflict with the row ahead is reported"
build
g -C "$repo" checkout -q -b campaign/ahead main
printf 'const N = 41; // ahead\n' > "$repo/src/tier.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m ahead
AHEAD="$(g -C "$repo" rev-parse HEAD)"
g -C "$repo" checkout -q -b campaign/behind main
printf 'const N = 41; // behind\n' > "$repo/src/tier.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m behind
BEHIND="$(g -C "$repo" rev-parse HEAD)"
row 2026-01-01T00:00:00Z a campaign/ahead "$AHEAD" queued
row 2026-01-02T00:00:00Z b campaign/behind "$BEHIND" queued
out="$(project x "$BEHIND" campaign/behind)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"WILL NOT MERGE"*) ok "reports the projected failure" ;; *) bad "missed a projected merge failure" ;; esac
case "$out" in *src/tier.rs*) ok "names the conflicting path" ;; *) bad "did not name the path" ;; esac
case "$out" in *"folded on"*campaign/ahead*) ok "says which row it folded on" ;; *) bad "did not say what it projected against" ;; esac

echo "== an ARTIFACTS-ONLY conflict is not a merge failure"
# The chamber regenerates these on every run; reporting them as a blocker is
# the cry-wolf that got six real candidates bounced in one session.
build
g -C "$repo" checkout -q -b campaign/ahead2 main
printf 'ahead version\n' > "$repo/docs/report.md"
g -C "$repo" add -A; g -C "$repo" commit -q -m ahead2
AHEAD2="$(g -C "$repo" rev-parse HEAD)"
g -C "$repo" checkout -q -b campaign/behind2 main
printf 'behind version\n' > "$repo/docs/report.md"
g -C "$repo" add -A; g -C "$repo" commit -q -m behind2
BEHIND2="$(g -C "$repo" rev-parse HEAD)"
row 2026-01-01T00:00:00Z a campaign/ahead2 "$AHEAD2" queued
row 2026-01-02T00:00:00Z b campaign/behind2 "$BEHIND2" queued
out="$(project x "$BEHIND2" campaign/behind2)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"ALL artifacts-authored"*) ok "calls a regenerable collision what it is" ;; *) bad "did not: $out" ;; esac
case "$out" in *"WILL NOT MERGE"*) bad "reported a regenerable collision as a merge failure" ;; *) ok "does not call it a failure" ;; esac

echo "== a row that conflicts with the ACCUMULATED projection is still folded on"
# THE DIRECTION THAT MATTERS, and the first fixture for it could not fail.
#
# It used a single row ahead, which is a descendant of main and therefore
# merges CLEANLY with it — so the drop-on-conflict bug never triggered and a
# mutant restoring it passed. The real condition needs TWO rows ahead: the
# second conflicts not with main but with main-plus-the-first. That is exactly
# what happened in production (campaign/the-tidemark conflicted with
# main-plus-the-coherence, not with main), and dropping it reported
# campaign/anchor-orbital-coherence as clean when a hand-run found 38
# unresolvable conflicts.
#
# A and B collide on an ARTIFACTS path, so B's conflict is pure bookkeeping the
# chamber regenerates and B will still land. C collides with B on a REAL path.
# Drop B from the projection and C reads clean; keep it and C is correctly
# doomed.
build
g -C "$repo" checkout -q -b campaign/aa main
printf 'aa version\n' > "$repo/docs/report.md"
g -C "$repo" add -A; g -C "$repo" commit -q -m aa
AA="$(g -C "$repo" rev-parse HEAD)"
g -C "$repo" checkout -q -b campaign/bb main
printf 'bb version\n' > "$repo/docs/report.md"
printf 'const N = 41; // bb\n' > "$repo/src/tier.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m bb
BB="$(g -C "$repo" rev-parse HEAD)"
g -C "$repo" checkout -q -b campaign/cc main
printf 'const N = 41; // cc\n' > "$repo/src/tier.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m cc
CC="$(g -C "$repo" rev-parse HEAD)"
row 2026-01-01T00:00:00Z a campaign/aa "$AA" queued
row 2026-01-02T00:00:00Z b campaign/bb "$BB" queued
row 2026-01-03T00:00:00Z c campaign/cc "$CC" queued
# CONTROL: bb really does conflict with main-plus-aa, and not with main alone.
if g -C "$repo" merge-tree --write-tree "$AA" "$BB" >/dev/null 2>&1; then
    bad "CONTROL: campaign/bb does not conflict with the projection — this case is vacuous"
else
    ok "CONTROL: campaign/bb conflicts with main-plus-aa (so the drop-bug can fire)"
fi
if g -C "$repo" merge-tree --write-tree origin/main "$BB" >/dev/null 2>&1; then
    ok "CONTROL: campaign/bb merges cleanly with main alone (the real production shape)"
else
    bad "CONTROL: campaign/bb conflicts with main too — the fixture is not the shape it claims"
fi
out="$(project x "$CC" campaign/cc)"
printf '%s\n' "$out" | sed 's/^/    /'
case "$out" in *"folded on"*campaign/bb*) ok "folds on a row that conflicts with the projection" ;; *) bad "dropped it — the false-clean bug" ;; esac
case "$out" in *"WILL NOT MERGE"*) ok "still finds the candidate's own real conflict" ;; *) bad "FALSE CLEAN: reported a doomed candidate as mergeable" ;; esac

echo "== a candidate not in the queue projects nothing"
build
g -C "$repo" checkout -q -b campaign/unqueued main
printf 'const N = 41;\n' > "$repo/src/tier.rs"
g -C "$repo" add -A; g -C "$repo" commit -q -m unqueued
UNQ="$(g -C "$repo" rev-parse HEAD)"
out="$(project x "$UNQ" campaign/unqueued)"
if [ -z "$(printf '%s' "$out" | tr -d '[:space:]')" ]; then
    ok "says nothing about a sha with no queue row"
else
    bad "projected for an unqueued candidate: $out"
fi

index_after="$(g -C "$root" write-tree 2>/dev/null || echo unavailable)"
[ "$index_before" = "$index_after" ] || bad "THIS SUITE REWROTE THE INDEX ($index_before -> $index_after)"

if [ "$fails" -eq 0 ]; then echo "test-sluice-vet-projection: PASS"; else echo "test-sluice-vet-projection: $fails FAILED"; fi
exit $((fails > 0))
