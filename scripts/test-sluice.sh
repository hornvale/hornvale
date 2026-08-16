#!/usr/bin/env bash
# scripts/test-sluice.sh — property tests for the merge queue.
#
# Shaped after scripts/test-lane.sh, which pins flock's ORDERING rather than
# merely asserting a lock file exists. Same discipline here: each test pins a
# property the queue would be worthless without, and each must be shown to
# fail when the property is broken.
#
# `flock` is util-linux and is NOT shipped with macOS. sluice-queue.sh itself
# only ever runs on lefford (Linux), so it may assume flock unconditionally —
# but this test script must not hard-fail on a host without it, because a Mac
# session may run the tests. Same precedent as scripts/test-heavy-lock.sh: a
# skip is honest, a false green from a host that never ran the assertions is
# not.
#
# HERMETICITY: git exports GIT_DIR and GIT_INDEX_FILE to hooks, and they
# OUTRANK `git -C`. A temp directory is not isolation when the environment
# names the repository — tools/board learned this by re-initialising a
# developer's own checkout as bare. Every git invocation below runs under
# `env -u GIT_DIR -u GIT_INDEX_FILE`.
set -euo pipefail

if ! command -v flock >/dev/null 2>&1; then
    echo "test-sluice: SKIP — no flock on this host ($(uname -s));"
    echo "test-sluice:        sluice-queue.sh only ever runs on the canonical box."
    exit 0
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pass=0; fail=0
ok()   { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad()  { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
g()    { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
export HV_SLUICE_DIR="$tmp/state"

# A scratch repo with a main line and two campaign commits on one branch.
scratch="$tmp/repo"; mkdir -p "$scratch"; cd "$scratch"
g init -q -b main .
g config user.email t@t; g config user.name t
printf 'a\n' > f.txt; g add f.txt; g commit -qm root
g checkout -q -b campaign/x
printf 'b\n' >> f.txt; g commit -qam one; OLD="$(g rev-parse HEAD)"
printf 'c\n' >> f.txt; g commit -qam two; NEW="$(g rev-parse HEAD)"

echo "== queue: FIFO and ancestry coalescing"
cd "$scratch"
id1="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/x "$OLD")"
id2="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/x "$NEW")"

# The older request must be superseded, not silently dropped: the registry row
# TOOL-lane-supersession requires it be ledgered.
state_of() { bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' -v i="$1" '$2==i{print $5}'; }
if [ "$(state_of "$id1")" = "superseded" ]; then
    ok "an ancestor request is superseded when its descendant is enqueued"
else
    bad "expected id1 superseded, got '$(state_of "$id1")'"
fi
if [ "$(bash "$repo_root/scripts/sluice-queue.sh" next | cut -f2)" = "$id2" ]; then
    ok "next() returns the surviving request"
else
    bad "next() did not return id2"
fi

echo "== queue: a request on a DIFFERENT branch is NOT coalesced"
g checkout -q -b campaign/y main
printf 'z\n' >> f.txt; g commit -qam other; OTHER="$(g rev-parse HEAD)"
id3="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/y "$OTHER")"
if [ "$(state_of "$id2")" = "queued" ]; then
    ok "a different branch does not supersede a queued request"
else
    bad "id2 was wrongly superseded by a different branch"
fi
if [ "$(state_of "$id3")" = "queued" ]; then
    ok "the new branch's own request lands queued"
else
    bad "expected id3 queued, got '$(state_of "$id3")'"
fi

echo "== queue: a SAME-branch, non-ancestor sha (post-rebase) is NOT coalesced"
# This is the property the header comment's "COALESCING IS BY ANCESTRY, NOT
# BRANCH NAME" design decision actually exists for, and the section above
# does not exercise it: that section's OTHER is on a DIFFERENT branch, so its
# "ok" only ever proves the branch guard works — the ancestor guard is
# whichever, since both are false there simultaneously (OTHER is not a
# descendant of NEW regardless of branch). Isolate the ancestor guard with a
# real rebase-shaped scenario: SAME branch, a sha that is no longer an
# ancestor of its successor. `commit --amend` produces exactly that shape
# more cheaply than a full rebase — a sibling commit sharing campaign/z's
# parent, not a descendant of the pre-amend commit.
g checkout -q -b campaign/z main
printf 'p\n' >> f.txt; g commit -qam first; FIRST="$(g rev-parse HEAD)"
id4="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/z "$FIRST")"
g commit -q --amend -m "first (amended)"
AMENDED="$(g rev-parse HEAD)"
if g merge-base --is-ancestor "$FIRST" "$AMENDED"; then
    bad "test setup broken: amend produced a descendant, not a sibling — this test cannot isolate the ancestor guard"
else
    ok "test setup: the amended commit is not a descendant of the pre-amend sha"
fi
bash "$repo_root/scripts/sluice-queue.sh" add campaign/z "$AMENDED" >/dev/null
if [ "$(state_of "$id4")" = "queued" ]; then
    ok "a same-branch sha that is not an ancestor (post-rebase) does not supersede the old request"
else
    bad "id4 was wrongly superseded — branch-name-only matching would do this; ancestry must not"
fi

echo "== queue: a note cannot corrupt the queue file"
# The reviewer's finding: set-state's note was written into the TSV
# unsanitized. A physical newline in the note splits the row in two on the
# very next rewrite's `while read` loop (the second half re-parses as its own
# garbage row — when=<line2>, every other field empty), and that row then
# persists and compounds on every later rewrite since nothing ever deletes a
# row. A tab is less catastrophic (bash `read` folds an over-long tail into
# the LAST named variable) but still shifts what `list`/`next` print.
before_lines="$(bash "$repo_root/scripts/sluice-queue.sh" list | wc -l | tr -d ' ')"
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id4" queued "$(printf 'line1\nline2')" >/dev/null
after_lines="$(bash "$repo_root/scripts/sluice-queue.sh" list | wc -l | tr -d ' ')"
if [ "$before_lines" -eq "$after_lines" ]; then
    ok "a newline embedded in a note does not add a row to the queue ($before_lines rows, unchanged)"
else
    bad "row count changed ($before_lines -> $after_lines) — a newline in a note split into an extra row"
fi
malformed="$(bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' 'NF!=6{c++} END{print c+0}')"
if [ "$malformed" -eq 0 ]; then
    ok "every row still has exactly 6 tab-separated fields after a newline-bearing note"
else
    bad "$malformed row(s) do not have 6 fields — a note corrupted the TSV shape"
fi

# `id4`'s row lookup keys on field 2 (the id), which a corrupted field 6 can
# never shift — so this check stays valid even under the corrupted shape it
# is trying to catch. Deliberately NOT a substring check on the note field
# itself: awk's own `-F'\t'` splits a row with a raw tab embedded in field 6
# into 7 fields, so field 6 alone reads back as just "a" — the tab is real,
# but hidden from that read by the very corruption it caused. The field
# COUNT is the signal that cannot be fooled that way.
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id4" queued "$(printf 'a\tb')" >/dev/null
tab_row_fields="$(bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' -v i="$id4" '$2==i{print NF}')"
if [ "$tab_row_fields" -eq 6 ]; then
    ok "a tab embedded in a note does not split id4's row into extra fields"
else
    bad "id4's row has $tab_row_fields fields, not 6 — a tab in a note corrupted the TSV shape"
fi

echo "== queue: no argument of any subcommand can corrupt the row shape"
# The invariant that actually matters, stated once and checked after every
# attempt below: no input to ANY subcommand can produce a row whose field
# count is not 6. Attack every argument of every subcommand that ever
# writes — not just note/branch/sha, the three the reviews discussed by
# name; `id` and `state` too. `next` and `list` take no arguments, so they
# have no attack surface here.
assert_shape_intact() {
    local label="$1" before="$2"
    local after bad_rows
    after="$(bash "$repo_root/scripts/sluice-queue.sh" list | wc -l | tr -d ' ')"
    bad_rows="$(bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' 'NF!=6{c++} END{print c+0}')"
    if [ "$after" = "$before" ] && [ "$bad_rows" -eq 0 ]; then
        ok "$label: row count unchanged ($before), every row still 6 fields"
    else
        bad "$label: row count $before -> $after, $bad_rows malformed row(s)"
    fi
}
expect_reject() {
    local label="$1"; shift
    if bash "$repo_root/scripts/sluice-queue.sh" "$@" >/dev/null 2>&1; then
        bad "$label: expected rejection, but the command succeeded"
    else
        ok "$label: rejected (nonzero exit) as expected"
    fi
}
expect_accept() {
    local label="$1"; shift
    if bash "$repo_root/scripts/sluice-queue.sh" "$@" >/dev/null 2>&1; then
        ok "$label: accepted as expected"
    else
        bad "$label: expected acceptance, but the command failed"
    fi
}

rows_now="$(bash "$repo_root/scripts/sluice-queue.sh" list | wc -l | tr -d ' ')"

# add: branch alone
expect_reject "add: tab in branch"      add "$(printf 'br\tanch')" "$FIRST"
assert_shape_intact "add: tab in branch"      "$rows_now"
expect_reject "add: newline in branch"  add "$(printf 'br\nanch')" "$FIRST"
assert_shape_intact "add: newline in branch"  "$rows_now"

# add: sha alone
expect_reject "add: newline in sha"     add campaign/z "$(printf 'sha\nwithnewline')"
assert_shape_intact "add: newline in sha"     "$rows_now"
expect_reject "add: tab in sha"         add campaign/z "$(printf 'sha\twithtab')"
assert_shape_intact "add: tab in sha"         "$rows_now"
expect_reject "add: non-hex sha (clean, no whitespace)" add campaign/z "not-hex-but-otherwise-clean"
assert_shape_intact "add: non-hex sha"        "$rows_now"

# add: the re-reviewer's exact repro — both fields malicious at once
expect_reject "add: tab-branch AND newline-sha together (the re-reviewer's repro)" \
    add "$(printf 'br\tanch')" "$(printf 'sha\nwithnewline')"
assert_shape_intact "add: both branch and sha malicious" "$rows_now"

# set-state: id alone — a garbage id just matches nothing, so this is a
# no-op SUCCESS, not a rejection; there is no dedicated id validator, and
# this is the evidence that none is needed (id is only ever compared
# against, never freshly written into a new row).
expect_accept "set-state: tab-bearing id (no match, no-op)"     set-state "$(printf 'id\twith\ttabs')" queued
assert_shape_intact "set-state: tab-bearing id"     "$rows_now"
expect_accept "set-state: newline-bearing id (no match, no-op)" set-state "$(printf 'id\nwith\nnewlines')" queued
assert_shape_intact "set-state: newline-bearing id" "$rows_now"

# set-state: state alone — closed vocabulary, reject anything outside it
expect_reject "set-state: tab in state"          set-state "$id4" "$(printf 'run\tning')"
assert_shape_intact "set-state: tab in state"          "$rows_now"
expect_reject "set-state: unknown (clean) state" set-state "$id4" "not-a-real-state"
assert_shape_intact "set-state: unknown state"         "$rows_now"

# set-state: note alone — the one field that's STRIPPED, not rejected, so
# the command is expected to SUCCEED here; already covered in the section
# above by name, re-run once more as part of this same sweep for consistency.
expect_accept "set-state: newline+tab note (strip path)" \
    set-state "$id4" queued "$(printf 'more\nlines\tand\ttabs')"
assert_shape_intact "set-state: newline+tab note"   "$rows_now"

echo "== queue: a RUNNING request is never superseded"
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id2" running
# NOTE: the brief's version of this test had a stray, uncommitted write to
# f.txt on campaign/y (the current branch at this point) immediately before
# the checkout below — it collided with campaign/x's own copy of f.txt and
# aborted the checkout every run. Dropped; the append that matters is the one
# after switching to campaign/x, which produces NEWER as a true descendant of
# id2's sha.
g checkout -q campaign/x; printf 'd\n' >> f.txt; g commit -qam three
NEWER="$(g rev-parse HEAD)"
bash "$repo_root/scripts/sluice-queue.sh" add campaign/x "$NEWER" >/dev/null
if [ "$(state_of "$id2")" = "running" ]; then
    ok "a running request is not superseded by a newer descendant"
else
    bad "a running request was superseded — an authoring job would be orphaned"
fi

echo "== queue: an interrupted rewrite leaves no temp file behind"
# Moving the temp file INTO $HV_SLUICE_DIR fixed cross-filesystem atomicity
# but opened a narrower hole: a signal or failing command between `mktemp`
# and `mv` now leaks debris into the durable directory instead of an
# OS-reaped $TMPDIR. Prove the EXIT trap actually fires by killing a
# rewrite mid-flight: populate the queue with enough same-branch `queued`
# rows that `add`'s coalescing scan (one real `git merge-base` subprocess
# per candidate — measured ~5ms each on this box, so 3000 rows is a ~15s
# window) is still running when the signal lands, then confirm no
# `.queue.tmp.*` survives. Destructive to the queue file, so this section
# runs LAST.
kill_scratch="$tmp/kill-repo"; mkdir -p "$kill_scratch"; cd "$kill_scratch"
g init -q -b main .
g config user.email t@t; g config user.name t
printf 'a\n' > f.txt; g add f.txt; g commit -qm root
KSHA="$(g rev-parse HEAD)"
{
    n=0
    while [ "$n" -lt 3000 ]; do
        printf '2026-01-01T00:00:00Z\treq-slow-%d\tcampaign/slow\t%s\tqueued\t\n' "$n" "$KSHA"
        n=$((n+1))
    done
} > "$HV_SLUICE_DIR/queue.tsv"

bash "$repo_root/scripts/sluice-queue.sh" add campaign/slow "$KSHA" &
killpid=$!
sleep 0.3
kill -TERM "$killpid" 2>/dev/null || true
wait "$killpid" 2>/dev/null || true

leftover="$(find "$HV_SLUICE_DIR" -maxdepth 1 -name '.queue.tmp.*' 2>/dev/null | wc -l | tr -d ' ')"
if [ "$leftover" -eq 0 ]; then
    ok "an interrupted rewrite leaves no .queue.tmp.* file behind in the state dir"
else
    bad "$leftover leftover .queue.tmp.* file(s) found after a killed rewrite"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
