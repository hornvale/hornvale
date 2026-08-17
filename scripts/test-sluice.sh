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
# The request-path mutation tests below have to write their mutants INSIDE
# scripts/ (see their own comments for why $tmp cannot host them), so the
# EXIT trap sweeps those paths too — belt-and-suspenders alongside the
# `rm -f`/`update-ref -d` immediately after each test uses them, in case
# `set -e` aborts the script somewhere in between. The two `refs/remotes/
# sluice-test/*` refs are throwaway local refs the headline-refusal test
# creates with `git commit-tree` + `update-ref` (never a real remote, never
# pushed) so a real commit object exists to test the headline check
# against; swept here too for the same belt-and-suspenders reason.
trap 'rm -rf "$tmp"; \
      rm -f "$repo_root/scripts/.sluice-request-mutant-for-test.sh" \
            "$repo_root/scripts/.sluice-request-headline-mutant-for-test.sh"; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/wip 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/empty 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/good 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/trailer 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/nudge 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/stranded 2>/dev/null || true; \
      rm -f "$repo_root/scripts/.sluice-request-trailer-mutant-for-test.sh"' EXIT
export HV_SLUICE_DIR="$tmp/state"

# THE BOARD READ IS OFF FOR THE WHOLE FILE, and this is a hermeticity guard,
# not a convenience. Since Task 12, `sluice-request.sh` inherits
# `preflight-merge.sh`'s hold-off advisory: it runs `scripts/board-sync.sh`,
# which PUSHES `refs/hornvale/hosts/<host>` to this repository's real
# `origin`. The request-path sections below run the real script (and mutated
# copies of it) many times over, so leaving it on would mean a test suite
# that writes to the shared remote — the exact class of accident
# `scripts/CLAUDE.md`'s board incident records. Exported once, here, so a
# section added later inherits it without having to remember.
export HV_SLUICE_SKIP_BOARD=1

# AND THE FETCH IS OFF FOR THE WHOLE FILE, for the same reason and with a
# sharper lesson attached. `sluice-request.sh` refreshes the base before
# searching for the headline trailer, and this suite drives that script dozens
# of times: left on, every one is a network round trip, and against an https
# remote with no cached credential `git fetch` PROMPTS rather than failing —
# which in a non-interactive test run is an unbounded hang, not a slow test.
# Measured: 17.9 s to past 20 minutes with no output. Skipping only degrades
# the trailer search to the range this repo already has, which is exactly what
# every assertion here constructs deliberately anyway.
export HV_SLUICE_SKIP_FETCH=1

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

echo "== queue: an UNRESOLVABLE ancestor sha is stamped indeterminate, never silently ignored"
# THE DEFECT THIS PINS, observed live on 2026-08-16. `--is-ancestor` is
# three-valued (0 yes, 1 no, 128 cannot-resolve), and the original code read
# it under `if ...; then`, which buckets 128 with 1 — so "I do not have that
# object" was indistinguishable from "not an ancestor", and the `2>/dev/null`
# threw away the `fatal:` that said which. Since nothing in the request path
# fetched, the canonical box routinely lacked a just-pushed commit and
# silently declined to coalesce. campaign/the-rhumb queued THREE stage
# requests in one ancestry chain and none coalesced; the author confirmed
# exit 0 for each pair on their own machine.
#
# WHY THE FIVE COALESCING TESTS ABOVE COULD NOT CATCH IT: every one of them
# mints both commits locally with `git commit-tree`/`commit`, so both objects
# always resolve. The harness guaranteed the exact precondition production
# does not — the test environment supplied what the real path was missing.
# This test is the one that removes that guarantee.
g checkout -q -b campaign/absent main
printf 'q\n' >> f.txt; g commit -qam "real"; REAL="$(g rev-parse HEAD)"
# Well-formed, validates fine, and no such object exists in this repo — which
# is precisely the shape a not-yet-fetched push has from the box's point of
# view. `add` does not require the sha to exist, by design (the queue must
# stay durable), so this is reachable without faking anything.
GHOST=deadbeefdeadbeefdeadbeefdeadbeefdeadbeef
id_ghost="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/absent "$GHOST")"
bash "$repo_root/scripts/sluice-queue.sh" add campaign/absent "$REAL" >/dev/null
note_of() { bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' -v i="$1" '$2==i{print $7}'; }
case "$(note_of "$id_ghost")" in
    *"coalescing indeterminate"*)
        ok "an unanswerable ancestry question stamps the row instead of passing as a silent no" ;;
    "")
        bad "the row was left untouched with an empty note — this is the original defect: 128 read as 1" ;;
    *)
        bad "unexpected note on the indeterminate row: '$(note_of "$id_ghost")'" ;;
esac
if [ "$(state_of "$id_ghost")" = "queued" ]; then
    ok "an indeterminate row stays queued — unanswerable must not mean superseded either"
else
    bad "an indeterminate row was moved to '$(state_of "$id_ghost")'; it must stay queued for a human to judge"
fi

echo "== queue: MUTATION — the pre-fix two-valued read leaves the row silent, reddening the test above"
# Non-vacuity for the section above, in this file's established style: revert
# ONLY the three-way read to the original `if ...; then` form and confirm the
# indeterminate row comes back empty-noted. Without this, the test could pass
# for reasons unrelated to the exit-code discrimination.
mutant="$repo_root/scripts/.sluice-queue-anc-mutant-for-test.sh"
# shellcheck disable=SC2016  # $rsha/$sha must stay LITERAL in the mutant's
# source text — expanding them here would bake this test's values into the
# mutated script instead of reproducing the pre-fix code.
sed -e 's|^            anc_rc=0$|            anc_rc=0; if env -u GIT_DIR -u GIT_INDEX_FILE git merge-base --is-ancestor "$rsha" "$sha" >/dev/null 2>\&1; then anc_rc=0; else anc_rc=1; fi; true \\|' \
    "$repo_root/scripts/sluice-queue.sh" > "$mutant"
HV_SLUICE_DIR="$tmp/mutant-queue" bash "$mutant" add campaign/absent "$GHOST" >/dev/null 2>&1 || true
mut_ghost="$(HV_SLUICE_DIR="$tmp/mutant-queue" bash "$mutant" list 2>/dev/null | awk -F'\t' '$4=="'"$GHOST"'"{print $2}' | head -1)"
HV_SLUICE_DIR="$tmp/mutant-queue" bash "$mutant" add campaign/absent "$REAL" >/dev/null 2>&1 || true
mut_note="$(HV_SLUICE_DIR="$tmp/mutant-queue" bash "$mutant" list 2>/dev/null | awk -F'\t' -v i="$mut_ghost" '$2==i{print $7}')"
# THE MUTANT MUST ACTUALLY RUN. Without this guard the section below passes
# vacuously the moment the `sed` produces a script that does not parse: a
# dead mutant enqueues nothing, `mut_note` is empty for that reason, and an
# empty note is exactly what the assertion treats as success. Pin that the
# mutant reached the queue at all before reading anything off it.
if [ -n "$mut_ghost" ]; then
    ok "test setup: the mutant runs and enqueues (so an empty note below means the read, not a dead script)"
else
    bad "the mutant did not enqueue — the mutation assertion below would pass vacuously"
fi
if [ -n "$mut_ghost" ] && [ -z "$mut_note" ]; then
    ok "MUTATION CONFIRMED: the two-valued read leaves an unresolvable ancestor unstamped (the real one stamps it — see above)"
else
    bad "the mutant also stamped the row ('$mut_note') — the test above is not pinning the exit-code discrimination"
fi
rm -f "$mutant"

echo "== queue: a HELD request IS superseded by its descendant"
# The state test used to be `= "queued"`, which excluded `held` BY OMISSION
# rather than by decision. A held request is one the chamber reddened, so the
# author fixes it and resubmits — the descendant IS the replacement, and
# leaving the held row forever means every red accretes a permanent row
# nobody will ever act on. Found while superseding campaign/the-rhumb's held
# clients-red request by hand: even with the objects present and the fetch in
# place, coalescing still would not have touched it.
g checkout -q -b campaign/heldy main
printf 'h\n' >> f.txt; g commit -qam "held one"; HELD_OLD="$(g rev-parse HEAD)"
printf 'i\n' >> f.txt; g commit -qam "held two"; HELD_NEW="$(g rev-parse HEAD)"
id_held="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/heldy "$HELD_OLD")"
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id_held" held "clients rc=2"
bash "$repo_root/scripts/sluice-queue.sh" add campaign/heldy "$HELD_NEW" >/dev/null
if [ "$(state_of "$id_held")" = "superseded" ]; then
    ok "a held request is superseded by its descendant, so a fixed red does not leave a permanent row"
else
    bad "a held ancestor stayed '$(state_of "$id_held")' — held rows will accrete forever"
fi

echo "== queue: a RUNNING request is still never superseded (the held change must not widen this)"
# Guarding the blast radius of the change above: widening the state test from
# one value to two must not have widened it to three. A running request is an
# authoring job already inside the chamber; superseding it orphans a job
# mid-write. The suite already asserts this further down for its own reasons;
# asserted again HERE, adjacent to the change, so a future widening of the
# `case` is caught by a test that names why.
g checkout -q -b campaign/runny main
printf 'r\n' >> f.txt; g commit -qam "run one"; RUN_OLD="$(g rev-parse HEAD)"
printf 's\n' >> f.txt; g commit -qam "run two"; RUN_NEW="$(g rev-parse HEAD)"
id_run="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/runny "$RUN_OLD")"
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id_run" running
bash "$repo_root/scripts/sluice-queue.sh" add campaign/runny "$RUN_NEW" >/dev/null
if [ "$(state_of "$id_run")" = "running" ]; then
    ok "a running request is still not superseded after held became supersedable"
else
    bad "a running request became '$(state_of "$id_run")' — an authoring job would be orphaned mid-write"
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
malformed="$(bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' 'NF!=7{c++} END{print c+0}')"
if [ "$malformed" -eq 0 ]; then
    ok "every row still has exactly 7 tab-separated fields after a newline-bearing note"
else
    bad "$malformed row(s) do not have 7 fields — a note corrupted the TSV shape"
fi

# `id4`'s row lookup keys on field 2 (the id), which a corrupted field 7 can
# never shift — so this check stays valid even under the corrupted shape it
# is trying to catch. Deliberately NOT a substring check on the note field
# itself: awk's own `-F'\t'` splits a row with a raw tab embedded in field 7
# into 8 fields, so field 7 alone reads back as just "a" — the tab is real,
# but hidden from that read by the very corruption it caused. The field
# COUNT is the signal that cannot be fooled that way.
bash "$repo_root/scripts/sluice-queue.sh" set-state "$id4" queued "$(printf 'a\tb')" >/dev/null
tab_row_fields="$(bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' -v i="$id4" '$2==i{print NF}')"
if [ "$tab_row_fields" -eq 7 ]; then
    ok "a tab embedded in a note does not split id4's row into extra fields"
else
    bad "id4's row has $tab_row_fields fields, not 7 — a tab in a note corrupted the TSV shape"
fi

echo "== queue: no argument of any subcommand can corrupt the row shape"
# The invariant that actually matters, stated once and checked after every
# attempt below: no input to ANY subcommand can produce a row whose field
# count is not 7 (six through The Sluice Task 11; the `kind` column made it
# seven). Attack every argument of every subcommand that ever
# writes — not just note/branch/sha, the three the reviews discussed by
# name; `id` and `state` too. `next` and `list` take no arguments, so they
# have no attack surface here.
assert_shape_intact() {
    local label="$1" before="$2"
    local after bad_rows
    after="$(bash "$repo_root/scripts/sluice-queue.sh" list | wc -l | tr -d ' ')"
    bad_rows="$(bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' 'NF!=7{c++} END{print c+0}')"
    if [ "$after" = "$before" ] && [ "$bad_rows" -eq 0 ]; then
        ok "$label: row count unchanged ($before), every row still 7 fields"
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

echo "== queue: the kind column (Task 12 — gate-stage absorbed as a kind, not a second path)"
kind_of() { bash "$repo_root/scripts/sluice-queue.sh" list | awk -F'\t' -v i="$1" '$2==i{print $6}'; }

# 1. The default is `merge`, so every caller that predates the column — and
#    every row already on the canonical box's queue — keeps meaning what it
#    meant.
g checkout -q -b campaign/kind main
printf 'k1\n' > k.txt; g add k.txt; g commit -qm "kind one"; K1="$(g rev-parse HEAD)"
idk_default="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/kind "$K1")"
if [ "$(kind_of "$idk_default")" = "merge" ]; then
    ok "a request enqueued with no kind argument records kind=merge"
else
    bad "the default kind is '$(kind_of "$idk_default")', not merge"
fi

# 2. A third value is REFUSED, not silently stored. `kind` selects a code
#    path in the chamber (whether the run pushes), so a typo reaching the
#    queue would be a request nothing knows how to run.
if bash "$repo_root/scripts/sluice-queue.sh" add campaign/kind "$K1" rehearse >/dev/null 2>&1; then
    bad "an unknown kind was accepted — the vocabulary is not closed"
else
    ok "an unknown kind ('rehearse') is refused"
fi

# 3. COALESCING IS SCOPED TO THE KIND. This is the property the column would
#    be actively harmful without: a queued merge silently superseded by a
#    later stage gate on the same branch would drop the merge request
#    entirely, and the campaign would wait forever on a request the queue no
#    longer holds.
#
# EVERY REQUEST BELOW USES A DISTINCT SHA, and that is a requirement of the
# harness rather than a stylistic choice: `sluice-queue.sh`'s request id is
# `req-<sha12>-<second-resolution timestamp>`, so two adds of the SAME sha
# inside one second collide on the id, and `state_of` — which keys on the id —
# then reports two rows' states at once. Found the hard way here; the first
# draft reused $K1 and read back a two-line "superseded\nqueued".
printf 'k2\n' >> k.txt; g commit -qam "kind two"; K2="$(g rev-parse HEAD)"
printf 'k3\n' >> k.txt; g commit -qam "kind three"; K3="$(g rev-parse HEAD)"
printf 'k4\n' >> k.txt; g commit -qam "kind four"; K4="$(g rev-parse HEAD)"
idk_merge="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/kind "$K2" merge)"
idk_stage="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/kind "$K3" stage)"
if [ "$(state_of "$idk_merge")" = "queued" ]; then
    ok "a queued merge is NOT superseded by a later stage request on the same branch"
else
    bad "a stage request superseded a queued merge (state=$(state_of "$idk_merge")) — the merge would be lost"
fi
# …and the same-kind case still coalesces, or the scoping would have been
# achieved by breaking coalescing outright.
idk_stage2="$(bash "$repo_root/scripts/sluice-queue.sh" add campaign/kind "$K4" stage)"
if [ "$(state_of "$idk_stage")" = "superseded" ] && [ "$(kind_of "$idk_stage2")" = "stage" ]; then
    ok "a stage request IS still superseded by a later same-kind descendant"
else
    bad "same-kind coalescing broke: idk_stage=$(state_of "$idk_stage"), idk_stage2 kind=$(kind_of "$idk_stage2")"
fi

# 4. `reported` is a real state. A stage run has no `landed` to reach, and
#    reusing `landed` would assert main moved when it did not.
if bash "$repo_root/scripts/sluice-queue.sh" set-state "$idk_stage2" reported "stage gate green" >/dev/null 2>&1; then
    ok "set-state accepts 'reported', the stage kind's terminal state"
else
    bad "set-state refused 'reported' — a stage request has no way to terminate"
fi
if bash "$repo_root/scripts/sluice-queue.sh" set-state "$idk_stage2" finished >/dev/null 2>&1; then
    bad "set-state accepted 'finished' — the state vocabulary is no longer closed"
else
    ok "set-state still refuses a state outside the vocabulary"
fi
g checkout -q campaign/x

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

echo "== mouth: a conflicting candidate is refused WITHOUT taking the claim"
cd "$scratch"
g checkout -q main; printf 'MAIN\n' > f.txt; g commit -qam main-edit
g checkout -q -b campaign/conflict main~1
printf 'SIDE\n' > f.txt; g commit -qam side-edit
CONF="$(g rev-parse HEAD)"

# Point the claim at a path in our temp dir so we can assert it is untouched.
export HV_CENSUS_CLAIM_PATH="$tmp/claim"
export HV_CENSUS_LOCK="$tmp/lock"
export HV_SLUICE_ALLOW_UNPUSHED=1     # scratch repo has no remote
# Pin the base EXPLICITLY in every mouth test below. The scratch repo has no
# `origin`, so an unpinned HV_SLUICE_BASE defaults to `origin/main`, which
# does not resolve here — and sluice-mouth.sh now (correctly, see the
# controller's fix) exits 2 for an unresolvable base, not 1. Leaving
# HV_SLUICE_BASE unset would make the "exits 1" and "never took the claim"
# assertions below pass VACUOUSLY: they would still be true, but for the
# wrong reason (an infrastructure fault, not the conflict this test is
# actually about) — the exact defect the controller's review caught.
export HV_SLUICE_BASE=main
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/conflict "$CONF" >/dev/null 2>"$tmp/mouth.err"
rc=$?
set -e
if [ "$rc" -eq 1 ]; then
    ok "a conflicting candidate exits 1"
else
    bad "expected exit 1 for a conflict, got $rc"
fi
# THE PROPERTY THAT MATTERS: assert on the CLAIM, not on the message. A check
# that merely printed the right words while consuming the box would pass a
# message-based assertion.
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "the conflicting candidate never acquired the claim"
else
    bad "the mouth took the claim — the whole point is that it must not"
fi
if grep -q 'f.txt' "$tmp/mouth.err"; then
    ok "the conflicting path is reported"
else
    bad "no conflicting path in stderr"
fi

echo "== mouth: an already-merged candidate exits 3"
export HV_SLUICE_BASE=main
set +e
bash "$repo_root/scripts/sluice-mouth.sh" main "$(g rev-parse main)" >/dev/null 2>&1
rc=$?
set -e
if [ "$rc" -eq 3 ]; then ok "already-merged exits 3"; else bad "expected 3, got $rc"; fi

echo "== mouth: a base ref that does not resolve exits 2, not 1 — distinct from a conflict"
# This is the controller's design fix, exercised directly: `git merge-tree
# --write-tree` exits 1 both for a genuine conflict AND for a base ref that
# fails to resolve ("not something we can merge") — the two mean opposite
# things to the queue (bounce the campaign vs. an infrastructure fault that
# is not the campaign's problem), so sluice-mouth.sh resolves and verifies
# the base BEFORE calling merge-tree. Use the SAME conflicting candidate as
# the first section so a regression back to "exit 1 for everything" would
# still show as a false green there if this section did not exist.
unset HV_SLUICE_BASE
rm -f "$HV_CENSUS_CLAIM_PATH"
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/conflict "$CONF" >/dev/null 2>"$tmp/mouth-badbase.err"
rc=$?
set -e
if [ "$rc" -eq 2 ]; then
    ok "an unresolvable base ref (origin/main, no remote here) exits 2, not 1"
else
    bad "expected exit 2 for an unresolvable base ref, got $rc"
fi
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "an unresolvable-base candidate never acquired the claim either"
else
    bad "the mouth took the claim on an unresolvable base — must not"
fi
if grep -qi 'base ref' "$tmp/mouth-badbase.err"; then
    ok "the unresolvable-base reason is reported, distinct from a conflict message"
else
    bad "no base-ref reason found in stderr"
fi
export HV_SLUICE_BASE=main

echo "== mouth: an out-of-band landing on the base exits 4"
cd "$scratch"
g checkout -q main
OLD_MAIN="$(g rev-parse main)"
mkdir -p "$HV_SLUICE_DIR"
printf '%s' "$OLD_MAIN" > "$HV_SLUICE_DIR/last-pushed"
printf 'OOB\n' >> f.txt; g commit -qam out-of-band-on-main
export HV_SLUICE_BASE=main
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/y "$(g rev-parse campaign/y)" \
    >/dev/null 2>"$tmp/mouth-oob.err"
rc=$?
set -e
if [ "$rc" -eq 4 ]; then
    ok "an out-of-band landing on the base exits 4"
else
    bad "expected exit 4 for an out-of-band landing, got $rc"
fi
if grep -qi 'out-of-band' "$tmp/mouth-oob.err"; then
    ok "the out-of-band reason is reported"
else
    bad "no out-of-band reason found in stderr"
fi
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "an out-of-band-landing candidate never acquired the claim either"
else
    bad "the mouth took the claim on an out-of-band landing — must not"
fi

echo "== mouth: exit 4 (out-of-band) wins over exit 3 (already merged) when both apply"
# THE ORDERING DECISION, stated once here because the controller asked for it
# to be explicit and it is not obvious: OLD_MAIN is now an ancestor of the
# moved base (the out-of-band commit above builds on it), so the ANCESTOR
# CHECK ALONE would call OLD_MAIN "already merged" and return exit 3 —
# read by a caller as a routine no-op, nothing to do. But the base ALSO moved
# out-of-band since the queue last recorded pushing it, which means the
# queue's inductive guarantee (each merge builds on an already-proven main)
# is broken RIGHT NOW, independent of whether this specific candidate happens
# to already be included. Silently returning 3 would let that break hide
# behind an ordinary-looking "already merged" verdict — possibly masking some
# OTHER, still-unmerged candidate sitting behind the same base movement. So
# sluice-mouth.sh checks out-of-band FIRST and it wins: exit 4, not 3,
# whenever both conditions hold. (Reversing this priority is the mutation
# that would silently swallow an out-of-band landing under "nothing to do".)
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/collision "$OLD_MAIN" \
    >/dev/null 2>"$tmp/mouth-collision.err"
rc=$?
set -e
if [ "$rc" -eq 4 ]; then
    ok "exit 4 (out-of-band) wins over exit 3 (already merged) when both conditions hold"
else
    bad "expected exit 4 to win the 3-vs-4 collision, got $rc"
fi

# Clean up so later sections (which assume no out-of-band tracking) are unaffected.
rm -f "$HV_SLUICE_DIR/last-pushed"

echo "== mouth: a clean, admissible candidate exits 0 and prints ADMIT"
# Branch from the CURRENT (post-out-of-band-commit) tip of main and touch a
# brand-new file, so the merge against base=main is unambiguously clean —
# no shared history with f.txt's tangle of earlier conflict-test edits.
g checkout -q -b campaign/clean main
printf 'clean\n' > g.txt
g add g.txt
g commit -qam clean-commit
CLEAN="$(g rev-parse campaign/clean)"

export HV_SLUICE_BASE=main
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
admit_out="$(bash "$repo_root/scripts/sluice-mouth.sh" campaign/clean "$CLEAN" 2>"$tmp/mouth-admit.err")"
rc=$?
set -e
if [ "$rc" -eq 0 ]; then
    ok "a clean, admissible candidate exits 0"
else
    bad "expected exit 0 for a clean candidate, got $rc (stderr: $(cat "$tmp/mouth-admit.err"))"
fi
if printf '%s\n' "$admit_out" | grep -q "ADMIT campaign/clean $CLEAN"; then
    ok "the admit message names the branch and sha"
else
    bad "admit message missing or malformed: '$admit_out'"
fi
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "an admitted candidate never acquires the claim either — the mouth is read-only end to end"
else
    bad "the mouth took the claim on the admit path — must not"
fi

echo "== mouth: git calls are hermetic to a bogus GIT_DIR/GIT_INDEX_FILE"
# git EXPORTS GIT_DIR/GIT_INDEX_FILE to hooks and they OUTRANK cwd/`-C` — from
# a linked worktree (where all campaign work happens) they are absolute paths
# into a DIFFERENT repository (scripts/CLAUDE.md's board incident: an
# unscrubbed `git -C <tempdir>` re-initialised a developer's real checkout as
# bare). Point both at an unrelated decoy repo that does NOT contain $CLEAN,
# and confirm the mouth still evaluates against $scratch (via cwd), not the
# decoy — proven by still reaching the same exit-0 admit as the section above.
decoy="$tmp/decoy"; mkdir -p "$decoy"
( cd "$decoy" \
  && g init -q -b decoy-main . \
  && g config user.email t@t && g config user.name t \
  && printf 'x\n' > d.txt && g add d.txt && g commit -qm decoy-root )

cd "$scratch"
export GIT_DIR="$decoy/.git"
export GIT_INDEX_FILE="$decoy/.git/index"
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
hermetic_out="$(bash "$repo_root/scripts/sluice-mouth.sh" campaign/clean "$CLEAN" 2>"$tmp/mouth-hermetic.err")"
rc=$?
set -e
unset GIT_DIR GIT_INDEX_FILE

if [ "$rc" -eq 0 ]; then
    ok "a bogus GIT_DIR/GIT_INDEX_FILE does not redirect the mouth to a different repository"
else
    bad "expected exit 0 despite a bogus GIT_DIR, got $rc (stderr: $(cat "$tmp/mouth-hermetic.err"))"
fi
if printf '%s\n' "$hermetic_out" | grep -q "ADMIT campaign/clean $CLEAN"; then
    ok "the admit message is unaffected by the bogus GIT_DIR"
else
    bad "admit message missing/wrong under a bogus GIT_DIR: '$hermetic_out'"
fi

echo "== mouth: a 40-char ref with a non-hex character is rejected (not just the first char)"
# The brief's `case "$sha" in [0-9a-f]*)` only anchored the FIRST character,
# so a 40-char string beginning with a hex digit but containing a non-hex
# character elsewhere (a 'g', say) passed through. Task 2 settled that
# identifiers are validated strictly, not stripped (sluice-queue.sh's
# validate_sha); sluice-mouth.sh's SHA check is now the same shape.
BAD_SHA="0123456789abcdef0123456789abcdefg1234567"  # 40 chars, one 'g'
if [ "${#BAD_SHA}" -eq 40 ]; then
    ok "test setup: BAD_SHA is exactly 40 characters"
else
    bad "test setup broken: BAD_SHA is ${#BAD_SHA} characters, not 40"
fi
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/clean "$BAD_SHA" >/dev/null 2>"$tmp/mouth-badsha.err"
rc=$?
set -e
if [ "$rc" -eq 2 ]; then
    ok "a 40-char ref with an embedded non-hex character is rejected (exit 2)"
else
    bad "expected exit 2 for a 40-char non-hex ref, got $rc"
fi
if grep -qi 'hex' "$tmp/mouth-badsha.err"; then
    ok "the rejection reason names the hex requirement"
else
    bad "no hex-related reason found in stderr"
fi

echo "== chamber: setup =="
# The mouth tests above export HV_SLUICE_BASE=main and never unset it — fine
# for them (base_ref there is only ever resolved, never CHASED across a
# moving remote). Left set, every chamber test below would resolve its base
# against $chamber_repo's own local `main` branch, which new_topic_branch()
# always builds FROM and nothing ever advances — instead of `origin/main`,
# which Task 5's real push moves forward after every successful landing. That
# mismatch is invisible until a push actually happens: leaked into the
# chamber section it built every later scenario on the SAME stale base,
# so each one but the first was pushing a tree that no longer led to the
# (by-then-advanced) origin/main — a real non-fast-forward rejection, not a
# bug in the push guard. Unset it here so the chamber's own default
# (origin/main) governs, matching production.
unset HV_SLUICE_BASE
# scripts/sluice-run.sh is a full standalone entry point (its own worktree,
# its own claim, its own merge), not a library sourced with mocked
# variables the way test-lane.sh extracts lane-run.sh's tail. So it is run
# for real here, against a THROWAWAY scratch repo (HV_SLUICE_REPO_ROOT) that
# stands in for this one — never against $scratch above (the queue/mouth
# repo) or $root itself, so a chamber test can never fetch this repo's real
# origin/main or register a worktree in its real registry.
chamber_repo="$tmp/chamber-repo"
mkdir -p "$chamber_repo"
(
    cd "$chamber_repo"
    g init -q -b main .
    g config user.email t@t; g config user.name t
    printf 'root\n' > tracked.txt
    g add tracked.txt
    # A TRACKED docs/timings.md, matching production (docs/timings.md is a
    # committed file in the real repo). scripts/timed.sh appends a row to
    # this file on EVERY phase, regardless of what the phase itself does —
    # in a scratch repo where this path is untracked, `git clean -fd`
    # silently removes that row and the fix round 1 Critical 1 bug (a
    # non-authoring phase's tracked drift poisoning the next phase's tree)
    # is invisible. Production always has this file tracked, so the test
    # path must too.
    mkdir -p docs
    printf '# timings\n' > docs/timings.md
    g add docs/timings.md
    g commit -qm root
)
# A REAL bare repo as `origin` — Task 5 adds an actual `git push origin`, so
# the stand-in remote-tracking ref that sufficed for Tasks 2-4 (which never
# pushed) no longer does: a push needs somewhere real to land. This is a
# throwaway bare repo under $tmp, NEVER this repository's real origin — see
# the campaign's own push-safety rule (root of this task's brief).
chamber_origin="$tmp/chamber-origin.git"
git init -q --bare -b main "$chamber_origin"
g -C "$chamber_repo" remote add origin "$chamber_origin"
g -C "$chamber_repo" push -q origin main
g -C "$chamber_repo" fetch -q origin
echo "test-sluice: chamber origin is $(g -C "$chamber_repo" remote get-url origin) (must be under \$tmp, never the real repo)"

# The chamber sources scripts/census-canonical-host.sh and invokes
# scripts/timed.sh from `$repo_root/scripts/`, both resolved relative to
# HV_SLUICE_REPO_ROOT — so the scratch repo needs its own copies. This is
# the one place a chamber test depends on real repo content; both files are
# already read-only inputs to every other lane test in this repo.
mkdir -p "$chamber_repo/scripts"
cp "$repo_root/scripts/census-canonical-host.sh" "$chamber_repo/scripts/census-canonical-host.sh"
cp "$repo_root/scripts/timed.sh" "$chamber_repo/scripts/timed.sh"
# EVERY HELPER sluice-run.sh SOURCES MUST BE ON THIS LIST, and the list has no
# way to know that. Adding `sluice-headline.sh` without this line failed the
# whole chamber section at once — `set -e` plus a missing `.` source, so the
# run exited 1 before its first phase and four downstream assertions read as
# unrelated failures (out-of-order phases, an empty last-pushed). If you add a
# `. "$repo_root/scripts/…"` to sluice-run.sh, add its copy here.
cp "$repo_root/scripts/sluice-headline.sh" "$chamber_repo/scripts/sluice-headline.sh"

chamber_host_file="$tmp/chamber-host.txt"
printf '%s\n' "$(hostname -s)" > "$chamber_host_file"

# $1 = branch name, $2 = filename to add. Each topic branch touches its own
# new file so every merge in every scenario below is trivially conflict-free
# — conflict handling is sluice-mouth.sh's job (Task 3), not the chamber's;
# the chamber assumes admission already happened.
new_topic_branch() {
    g -C "$chamber_repo" checkout -q -b "$1" main
    printf 'x\n' > "$chamber_repo/$2"
    g -C "$chamber_repo" add "$2"
    g -C "$chamber_repo" commit -qm "topic: $1"
    g -C "$chamber_repo" rev-parse "$1"
}

poll_for_file() {
    local f="$1" tries="${2:-50}" i=0
    while [ "$i" -lt "$tries" ]; do
        [ -e "$f" ] && return 0
        sleep 0.1
        i=$((i+1))
    done
    return 1
}

# True iff pid $1 names a live process (kill -0 without actually signalling).
pid_alive() { kill -0 "$1" 2>/dev/null; }

poll_for_absence() {
    local f="$1" tries="${2:-50}" i=0
    while [ "$i" -lt "$tries" ]; do
        [ ! -e "$f" ] && return 0
        sleep 0.1
        i=$((i+1))
    done
    return 1
}

export HV_SLUICE_DIR="$tmp/state"; mkdir -p "$HV_SLUICE_DIR"
export HV_SLUICE_REPO_ROOT="$chamber_repo"
export HV_CANONICAL_HOST_FILE="$chamber_host_file"
export HV_CENSUS_LOCK="$tmp/chamber.lock"

echo "== chamber: phases run in the declared order, the tree is cleaned between phases, and the claim is the eight-field shape while held =="
lane_sets_1="$tmp/lane-sets-1.tsv"
TRACKLOG_1="$tmp/tracklog-1"; : > "$TRACKLOG_1"
CLEANMARKER_1="$tmp/cleanmarker-1"; : > "$CLEANMARKER_1"

cat > "$tmp/phase-a.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo A >> "$TRACKLOG_1"
echo residue > untracked-from-a.txt
sleep 1
SH
cat > "$tmp/phase-b.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo B >> "$TRACKLOG_1"
if [ -e untracked-from-a.txt ]; then
    echo DIRTY >> "$CLEANMARKER_1"
else
    echo CLEAN >> "$CLEANMARKER_1"
fi
SH
cat > "$tmp/phase-c.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo C >> "$TRACKLOG_1"
SH
chmod +x "$tmp/phase-a.sh" "$tmp/phase-b.sh" "$tmp/phase-c.sh"
{
    printf 'a\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-a.sh"
    printf 'b\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-b.sh"
    printf 'c\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-c.sh"
} > "$lane_sets_1"

sha1="$(new_topic_branch campaign/t1 topic1.txt)"
wt1="$tmp/wt1"

export HV_SLUICE_LANE_SETS="$lane_sets_1"
export HV_SLUICE_PHASES="a b c"
export HV_SLUICE_WORKTREE="$wt1"
export HV_CENSUS_CLAIM_PATH="$tmp/claim1"
export TRACKLOG_1 CLEANMARKER_1
rm -f "$HV_CENSUS_CLAIM_PATH"

bash "$repo_root/scripts/sluice-run.sh" campaign/t1 "$sha1" > "$tmp/run1.out" 2>&1 &
run1_pid=$!

if poll_for_file "$HV_CENSUS_CLAIM_PATH" 50; then
    ok "the claim file appears while the chamber holds the staff"
    missing=0
    for k in pid host user started goldens label ref cmdline; do
        grep -q "^${k}=" "$HV_CENSUS_CLAIM_PATH" || missing=$((missing+1))
    done
    if [ "$missing" -eq 0 ]; then
        ok "the claim carries all eight required keys while held (Task 0's shape)"
    else
        bad "the claim is missing $missing of the eight required keys"
    fi
else
    bad "the claim file never appeared — cannot verify its shape while held"
fi

if wait "$run1_pid"; then rc1=0; else rc1=$?; fi

if [ "$rc1" -eq 0 ]; then
    ok "the three-phase run exits 0"
else
    bad "expected rc 0, got $rc1 ($(cat "$tmp/run1.out"))"
fi
if [ "$(tr '\n' ' ' < "$TRACKLOG_1")" = "A B C " ]; then
    ok "phases ran in the declared order (A, B, C)"
else
    bad "phases ran out of order: $(tr '\n' ' ' < "$TRACKLOG_1")"
fi
if [ "$(cat "$CLEANMARKER_1")" = "CLEAN" ]; then
    ok "git clean -fd between phases removed phase a's untracked residue before phase b ran"
else
    bad "phase b saw phase a's untracked residue: marker=$(cat "$CLEANMARKER_1")"
fi
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "the claim is removed after a normal (rc=0) exit"
else
    bad "the claim survived a normal exit"
fi

# THE INTERFACE CONTRACT ITSELF (this task's brief): a green chamber run
# produces $HV_SLUICE_DIR/last-pushed containing the SHA it just pushed, and
# that SHA is genuinely what landed on the shared origin's main — not merely
# a file that happens to exist.
wt1_final_sha="$(g -C "$wt1" rev-parse HEAD)"
origin1_main_sha="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
last_pushed_val="$(cat "$HV_SLUICE_DIR/last-pushed" 2>/dev/null || true)"
if [ -f "$HV_SLUICE_DIR/last-pushed" ] && [ "$last_pushed_val" = "$wt1_final_sha" ]; then
    ok "last-pushed contains the exact SHA the run's own tree landed at ($wt1_final_sha)"
else
    bad "last-pushed is '$last_pushed_val', expected '$wt1_final_sha'"
fi
if [ "$origin1_main_sha" = "$wt1_final_sha" ]; then
    ok "the shared origin's main genuinely advanced to that same SHA"
else
    bad "origin main is '$origin1_main_sha', expected '$wt1_final_sha' — last-pushed does not reflect what actually landed"
fi

echo "== chamber: the merge subject is a valid census epoch label (merge(<campaign>): <headline>, not git's own auto-generated default) =="
# tools/census/history.sh:59 tags every committed census snapshot with an
# epoch label taken from `git log --follow --first-parent main -- <path>`'s
# %s (the commit SUBJECT). Under --no-ff the campaign's own commit sits off
# the first-parent line, so the MERGE commit's subject becomes that label —
# a junk or generic one silently degrades a committed artifact rather than
# merely reading badly. wt1_final_sha (above) IS that merge commit, produced
# by the real chamber run's own merge_msg TEMPLATE (sluice-run.sh) — this
# reads the actual commit the actual script wrote, never a hand-authored
# string, so a change to the template reddens this test (see the MUTATION
# below).
#
# NOT wt1_final_sha itself: every phase — authoring or not — commits its own
# tracked drift (docs/timings.md, via scripts/timed.sh, per the file's own
# "EVERY PHASE STARTS ON A CLEAN TREE" comment), so HEAD after a multi-phase
# run is the LAST such drift commit, not the merge. Verified against the
# actual chamber output (`git log --oneline --graph` on $wt1): merge(t1):
# ... sits three commits behind HEAD, under "regenerate after a/b/c". The
# merge is the run's only 2-parent commit, so walk back to it explicitly.
merge_sha1="$(g -C "$wt1" rev-list --merges -1 "$wt1_final_sha")"
wt1_subject="$(g -C "$wt1" log -1 --format=%s "$merge_sha1")"
if printf '%s' "$wt1_subject" | grep -qE '^merge\([a-z0-9-]+\): .+'; then
    ok "merge subject matches merge(<campaign>): <headline> ('$wt1_subject')"
else
    bad "merge subject '$wt1_subject' would be a useless census epoch label"
fi
# sluice-run.sh's `git merge` always takes a raw commit SHA as its candidate
# (new_topic_branch above returns `git rev-parse`, and the chamber's second
# positional argument is that SHA throughout this file, never a branch ref)
# — so git's OWN auto-generated subject for THIS call shape is
# "Merge commit '<sha>'", never "Merge branch ...". Confirmed empirically: a
# bare `git merge --no-ff --no-edit <sha>` with no -m produces exactly that
# in a scratch repo. The pattern below therefore matches every git-generated
# default (branch/commit/tag) rather than only the branch-name form a
# hand-authored `git merge campaign/y` would produce, so it stays correct
# for the call shape production actually makes.
if printf '%s' "$wt1_subject" | grep -qE '^Merge (branch|commit|tag) '; then
    bad "git's auto-generated default merge subject reached the first-parent line"
else
    ok "not git's auto-generated default merge subject"
fi

echo "== chamber: MUTATION — dropping the -m \"\$merge_msg\" argument from the merge call reddens both assertions above"
# Proves the two checks above watch the TEMPLATE, not a coincidence: strip
# the chamber's own -m argument — the exact mechanism that writes
# merge_msg's "merge(<campaign>): <headline>" subject — and confirm the
# resulting real merge commit (still produced by running sluice-run.sh for
# real, never hand-constructed) fails both assertions the way a genuine
# regression would. Falls to git's own default ("Merge commit '<sha>'"),
# which is exactly the shape the check above was written to catch.
# shellcheck disable=SC2016  # single-quoted on purpose: this is a literal grep pattern, not a shell expansion
mutant_merge_msg_line="$(grep -n '^if ! run_bg git merge --no-ff --no-edit -m "\$merge_msg" "\$sha"; then$' "$repo_root/scripts/sluice-run.sh" | cut -d: -f1)"
mutant3="$tmp/sluice-run-merge-msg-mutant.sh"
if [ -z "$mutant_merge_msg_line" ]; then
    bad "could not locate the merge invocation line in sluice-run.sh to mutate — it may have changed shape"
else
    {
        head -n "$((mutant_merge_msg_line - 1))" "$repo_root/scripts/sluice-run.sh"
        # shellcheck disable=SC2016  # single-quoted on purpose: this is the literal replacement source line, not a shell expansion here
        echo 'if ! run_bg git merge --no-ff --no-edit "$sha"; then'
        tail -n "+$((mutant_merge_msg_line + 1))" "$repo_root/scripts/sluice-run.sh"
    } > "$mutant3"
fi
chmod +x "$mutant3"

lane_sets_mut="$tmp/lane-sets-mut.tsv"
printf 'm\tcommit\tlocal\tno\ttrue\n' > "$lane_sets_mut"
shamut="$(new_topic_branch campaign/tmut topicmut.txt)"
wtmut="$tmp/wtmut"

export HV_SLUICE_LANE_SETS="$lane_sets_mut"
export HV_SLUICE_PHASES="m"
export HV_SLUICE_WORKTREE="$wtmut"
export HV_CENSUS_CLAIM_PATH="$tmp/claim-mut"
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$mutant3" campaign/tmut "$shamut" > "$tmp/runmut.out" 2>&1
rc_mut=$?
set -e

if [ "$rc_mut" -eq 0 ]; then
    # Same reasoning as merge_sha1 above: the "m" phase is a no-op ("true")
    # but still gets its own tracked-drift commit, so walk back to the
    # run's one 2-parent commit rather than reading HEAD.
    merge_sha_mut="$(g -C "$wtmut" rev-list --merges -1 HEAD)"
    mut_subject="$(g -C "$wtmut" log -1 --format=%s "$merge_sha_mut")"
    if printf '%s' "$mut_subject" | grep -qE '^merge\([a-z0-9-]+\): .+'; then
        bad "MUTATION DID NOT TAKE: mutant subject '$mut_subject' still matched the shape check"
    else
        ok "MUTATION CONFIRMED (shape check): mutant subject '$mut_subject' fails merge(<campaign>): <headline>"
    fi
    if printf '%s' "$mut_subject" | grep -qE '^Merge (branch|commit|tag) '; then
        ok "MUTATION CONFIRMED (default-subject check): mutant subject '$mut_subject' is git's own auto-generated default"
    else
        bad "MUTATION DID NOT TAKE: mutant subject '$mut_subject' did not fall back to git's default"
    fi
else
    bad "the mutant chamber run failed outright (rc=$rc_mut): $(cat "$tmp/runmut.out")"
fi
rm -f "$mutant3"

echo "== chamber: a non-authoring phase's TRACKED drift does not poison the next phase's tree (fix round 1, Critical 1) =="
# Reproduces the exact reported mechanism: scripts/timed.sh appends a row to
# the TRACKED docs/timings.md on every phase, regardless of what the phase
# itself does or whether the roster marks it `authors=yes`. `git clean -fd`
# only removes UNTRACKED files, so that tracked modification used to survive
# into the next phase — and a phase checking `git status --porcelain` is
# empty (exactly what seam-guard's own `tree_is_clean()` does) would see it
# and refuse. Phase "gate" here does nothing OF ITS OWN beyond `true` — the
# dirt comes entirely from scripts/timed.sh's own write, proving the bug is
# in the chamber's between-phase handling, not in anything a phase authors
# itself.
lane_sets_c1="$tmp/lane-sets-c1.tsv"
seamguard_marker="$tmp/seamguard-marker"; : > "$seamguard_marker"
cat > "$tmp/phase-gate.sh" <<'SH'
#!/usr/bin/env bash
true
SH
cat > "$tmp/phase-seamguard.sh" <<'SH'
#!/usr/bin/env bash
if [ -z "$(git status --porcelain)" ]; then
    echo CLEAN >> "$SEAMGUARD_MARKER"
    exit 0
else
    echo DIRTY >> "$SEAMGUARD_MARKER"
    exit 1
fi
SH
chmod +x "$tmp/phase-gate.sh" "$tmp/phase-seamguard.sh"
{
    printf 'gate\tstage\tlane\tno\tbash %s\n' "$tmp/phase-gate.sh"
    printf 'seamguard\tcampaign\tlane\tno\tbash %s\n' "$tmp/phase-seamguard.sh"
} > "$lane_sets_c1"

shac1="$(new_topic_branch campaign/c1 topicc1.txt)"
wtc1="$tmp/wtc1"

export HV_SLUICE_LANE_SETS="$lane_sets_c1"
export HV_SLUICE_PHASES="gate seamguard"
export HV_SLUICE_WORKTREE="$wtc1"
export HV_CENSUS_CLAIM_PATH="$tmp/claimc1"
export SEAMGUARD_MARKER="$seamguard_marker"
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-run.sh" campaign/c1 "$shac1" > "$tmp/runc1.out" 2>&1
rc_c1=$?
set -e

if [ "$rc_c1" -eq 0 ]; then
    ok "the gate-then-seamguard run exits 0 (the tree entering seamguard is clean)"
else
    bad "expected rc 0, got $rc_c1 ($(cat "$tmp/runc1.out"))"
fi
if [ "$(cat "$seamguard_marker")" = "CLEAN" ]; then
    ok "the non-authoring 'gate' phase's tracked drift (docs/timings.md) was committed before 'seamguard' ran, so it saw a clean tree"
else
    bad "seamguard saw a dirty tree: marker=$(cat "$seamguard_marker")"
fi
# Captured into a variable FIRST, not piped directly from `git log`: under
# `set -o pipefail`, `git log --oneline | grep -q pattern` can report the
# whole pipeline as failed even when grep finds its match, because `grep -q`
# exits as soon as it has a hit and `git` (writing further lines into a pipe
# grep has already closed) can exit non-zero from the resulting SIGPIPE —
# which pipefail then reports as the pipeline's own status. Found live: this
# exact assertion failed under `bash scripts/test-sluice.sh` while the same
# `git log | grep -q` succeeded when typed by hand (no `pipefail` in an
# interactive shell). Grepping a captured string sidesteps it entirely.
wtc1_log="$(g -C "$wtc1" log --oneline)"
if printf '%s\n' "$wtc1_log" | grep -q "regenerate after gate"; then
    ok "gate's own tracked drift reached a real commit rather than being silently discarded"
else
    bad "no 'regenerate after gate' commit found — gate's legitimate drift was discarded, not committed"
fi

echo "== chamber: the commit HOOK itself must not re-dirty the tree it just committed (fix round 2) =="
# Reproduces the exact reported mechanism: this repo's core.hooksPath is a
# REPOSITORY-level git config, inherited by every linked worktree including
# the chamber's own, so scripts/hooks/pre-commit fires on every commit this
# script makes. Its rust_relevant filter matches
# docs/audits/type-audit-report.md by name, and the `artifacts` phase's own
# commit stages exactly that file — so `make gate-commit` runs INSIDE the
# chamber's own commit, and gate-commit's own timed.sh-wrapped steps append a
# row to tracked docs/timings.md AFTER `git add` already ran, so `git commit`
# does not include it: real dirt, surviving `git clean -fd` into the next
# phase, same as Critical 1 but nested one level deeper. Reproduced here with
# a lightweight STAND-IN hook (never the real scripts/hooks/pre-commit, which
# would need a full Rust build to fire) that captures the identical
# mechanism: fires only on a matching staged path, and as a side effect
# dirties a SEPARATE tracked file — exactly what gate-commit's own timed.sh
# does to docs/timings.md.
# The hook file must be COMMITTED, not merely dropped into $chamber_repo's
# own working directory: `core.hooksPath` is a RELATIVE path here (matching
# production's own `scripts/hooks`), and git resolves a relative hooksPath
# against the WORKTREE that is actually committing, not against wherever the
# path happened to be created. $wth (the chamber's own worktree, created
# below) only ever contains what its branch's history checks out — an
# uncommitted `hooks-standin/` sitting only in $chamber_repo would never
# appear there, and the hook would silently never fire, which is exactly
# what happened on the first attempt at this test (caught by checking the
# chamber's OWN job log for the hook's marker line and finding it absent
# even with the fix reverted — a false green, not a working reproduction).
# Production's `scripts/hooks/pre-commit` avoids this only because it is
# already a normal tracked file, checked out fresh into every worktree.
g -C "$chamber_repo" checkout -q main
mkdir -p "$chamber_repo/hooks-standin"
cat > "$chamber_repo/hooks-standin/pre-commit" <<'HOOK'
#!/usr/bin/env bash
set -euo pipefail
if git diff --cached --name-only --diff-filter=ACMR | grep -q '^trigger\.txt$'; then
    echo "stand-in-hook: trigger matched -- appending to docs/timings.md (mimicking gate-commit's own timed.sh side effect)" >&2
    echo "| hook-row |" >> docs/timings.md
fi
exit 0
HOOK
chmod +x "$chamber_repo/hooks-standin/pre-commit"
g -C "$chamber_repo" add hooks-standin/pre-commit
g -C "$chamber_repo" commit -qm "add the stand-in hook"
g -C "$chamber_repo" config core.hooksPath hooks-standin

lane_sets_h="$tmp/lane-sets-h.tsv"
seamguard_h_marker="$tmp/seamguard-h-marker"; : > "$seamguard_h_marker"
cat > "$tmp/phase-h1.sh" <<'SH'
#!/usr/bin/env bash
# Stands in for the real `artifacts` phase regenerating
# docs/audits/type-audit-report.md: modifies an ALREADY-TRACKED file whose
# name matches the stand-in hook's own trigger.
echo v1 >> trigger.txt
SH
cat > "$tmp/phase-h2.sh" <<'SH'
#!/usr/bin/env bash
if [ -z "$(git status --porcelain)" ]; then
    echo CLEAN >> "$SEAMGUARD_H_MARKER"
    exit 0
else
    echo DIRTY >> "$SEAMGUARD_H_MARKER"
    exit 1
fi
SH
chmod +x "$tmp/phase-h1.sh" "$tmp/phase-h2.sh"
{
    printf 'h1\tstage\tlane\tyes\tbash %s\n' "$tmp/phase-h1.sh"
    printf 'h2\tcampaign\tlane\tno\tbash %s\n' "$tmp/phase-h2.sh"
} > "$lane_sets_h"

# trigger.txt must be TRACKED already (matching type-audit-report.md's real
# shape: an existing file being REGENERATED, not created for the first time).
( cd "$chamber_repo" && printf 'v0\n' > trigger.txt && g add trigger.txt && g commit -qm "add trigger.txt" )

shah="$(new_topic_branch campaign/h topich.txt)"
wth="$tmp/wth"

export HV_SLUICE_LANE_SETS="$lane_sets_h"
export HV_SLUICE_PHASES="h1 h2"
export HV_SLUICE_WORKTREE="$wth"
export HV_CENSUS_CLAIM_PATH="$tmp/claimh"
export SEAMGUARD_H_MARKER="$seamguard_h_marker"
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-run.sh" campaign/h "$shah" > "$tmp/runh.out" 2>&1
rc_h=$?
set -e

if [ "$rc_h" -eq 0 ]; then
    ok "the h1-then-h2 run exits 0 (the hook's own side effect did not poison h2's tree)"
else
    bad "expected rc 0, got $rc_h ($(cat "$tmp/runh.out"))"
fi
if [ "$(cat "$seamguard_h_marker")" = "CLEAN" ]; then
    ok "h2 saw a clean tree — the stand-in hook's own dirt (fired inside h1's commit) did not survive into h2"
else
    bad "h2 saw a dirty tree: marker=$(cat "$seamguard_h_marker") — the commit hook re-dirtied the tree it just committed"
fi
# Unset again so it cannot affect any later scenario's own commits — every
# remaining phase in this file happens not to touch trigger.txt, but there is
# no reason to leave a global config change armed past the test that needs it.
g -C "$chamber_repo" config --unset core.hooksPath

echo "== chamber: an authoring phase's drift is committed on the canonical host =="
lane_sets_2="$tmp/lane-sets-2.tsv"
cat > "$tmp/phase-d.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo authored >> tracked.txt
SH
chmod +x "$tmp/phase-d.sh"
printf 'd\tcommit\tlocal\tyes\tbash %s\n' "$tmp/phase-d.sh" > "$lane_sets_2"

sha2="$(new_topic_branch campaign/t2 topic2.txt)"
wt2="$tmp/wt2"

export HV_SLUICE_LANE_SETS="$lane_sets_2"
export HV_SLUICE_PHASES="d"
export HV_SLUICE_WORKTREE="$wt2"
export HV_CENSUS_CLAIM_PATH="$tmp/claim2"
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-run.sh" campaign/t2 "$sha2" > "$tmp/run2.out" 2>&1
rc2=$?
set -e

if [ "$rc2" -eq 0 ]; then
    ok "the authoring-phase run exits 0"
else
    bad "expected rc 0, got $rc2 ($(cat "$tmp/run2.out"))"
fi
subject2="$(g -C "$wt2" log -1 --format=%s)"
if [ "$subject2" = "chore(artifacts): regenerate after d" ]; then
    ok "an authoring phase's drift is committed with the expected subject"
else
    bad "unexpected HEAD subject after phase d: '$subject2'"
fi
if [ -z "$(g -C "$wt2" status --porcelain)" ]; then
    ok "the tree is clean after the authoring commit"
else
    bad "the tree is dirty after phase d's commit"
fi
# Captured first, not piped directly from `git show` — see the wtc1_log
# comment above for why: an external command piped straight into an
# early-exiting `grep -q` can fail the whole pipeline under `pipefail` via
# SIGPIPE even when grep finds its match.
tracked_txt_head="$(g -C "$wt2" show HEAD:tracked.txt)"
if printf '%s\n' "$tracked_txt_head" | grep -q authored; then
    ok "the authored content reached the committed file"
else
    bad "tracked.txt's committed content does not contain phase d's write"
fi

echo "== push: a nested untracked git repo the phase loop cannot clean is still caught =="
# THE ONE REACHABLE CASE for the surviving dirty-tree check (Important 2,
# fix round 1): a plain `git clean -fd` refuses to delete an untracked
# directory that is ITSELF a git repository (git requires force TWICE — `-f
# -f` / `--force --force` — to remove a nested repo, precisely so `git clean`
# never silently destroys unpushed work sitting in one). A phase that leaves
# one behind survives the phase loop's own cleanup and must still be caught
# before a push, or the chamber would land a candidate carrying stray,
# unaccounted-for content. This is deliberately the ONLY thing left in this
# check's reach after removing the redundant generated-paths diff (see
# sluice-run.sh's own comment on the deletion) — proving it still catches a
# real defect, not merely that it exists.
lane_sets_nested="$tmp/lane-sets-nested.tsv"
cat > "$tmp/phase-nested.sh" <<'SH'
#!/usr/bin/env bash
set -e
git init -q leftover-nested-repo
SH
chmod +x "$tmp/phase-nested.sh"
printf 'nested\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-nested.sh" > "$lane_sets_nested"

shanested="$(new_topic_branch campaign/tnested topicnested.txt)"
wtnested="$tmp/wtnested"

origin_main_before_nested="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"

export HV_SLUICE_LANE_SETS="$lane_sets_nested"
export HV_SLUICE_PHASES="nested"
export HV_SLUICE_WORKTREE="$wtnested"
export HV_CENSUS_CLAIM_PATH="$tmp/claimnested"
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-run.sh" campaign/tnested "$shanested" > "$tmp/runnested.out" 2>&1
rc_nested=$?
set -e

if [ "$rc_nested" -eq 12 ]; then
    ok "a phase leaving a nested untracked git repo behind exits 12 (dirty-tree), not 0"
else
    bad "expected rc 12, got $rc_nested ($(cat "$tmp/runnested.out"))"
fi
failed_col_nested="$(awk -F'\t' -v j="sluice-${shanested:0:12}" '$0 ~ j {print $9}' "$HV_SLUICE_DIR/jobs.tsv" | tail -1)"
# `<dirty-tree>`, not `dirty-tree` — fix round 2's collision-proofing (the
# coordinator's minor finding): the internal sentinel is bracketed so it can
# never collide with a literal set name from lane-sets.tsv.
if [ "$failed_col_nested" = "<dirty-tree>" ]; then
    ok "jobs.tsv records phase_failed=<dirty-tree> for the leftover nested repo"
else
    bad "jobs.tsv phase_failed column is '$failed_col_nested', expected '<dirty-tree>'"
fi
origin_main_after_nested="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
if [ "$origin_main_after_nested" = "$origin_main_before_nested" ]; then
    ok "origin's main did not move — the dirty tree was never pushed"
else
    bad "origin's main MOVED despite a dirty tree ($origin_main_before_nested -> $origin_main_after_nested)"
fi

echo "== chamber: the first failing phase stops the run before a later phase executes =="
lane_sets_3="$tmp/lane-sets-3.tsv"
TRACKLOG_3="$tmp/tracklog-3"; : > "$TRACKLOG_3"
cat > "$tmp/phase-e.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo E >> "$TRACKLOG_3"
SH
cat > "$tmp/phase-f.sh" <<'SH'
#!/usr/bin/env bash
echo F >> "$TRACKLOG_3"
exit 1
SH
cat > "$tmp/phase-g.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo G >> "$TRACKLOG_3"
SH
chmod +x "$tmp/phase-e.sh" "$tmp/phase-f.sh" "$tmp/phase-g.sh"
{
    printf 'e\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-e.sh"
    printf 'f\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-f.sh"
    printf 'g\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-g.sh"
} > "$lane_sets_3"

sha3="$(new_topic_branch campaign/t3 topic3.txt)"
wt3="$tmp/wt3"

export HV_SLUICE_LANE_SETS="$lane_sets_3"
export HV_SLUICE_PHASES="e f g"
export HV_SLUICE_WORKTREE="$wt3"
export HV_CENSUS_CLAIM_PATH="$tmp/claim3"
export TRACKLOG_3
rm -f "$HV_CENSUS_CLAIM_PATH"

set +e
bash "$repo_root/scripts/sluice-run.sh" campaign/t3 "$sha3" > "$tmp/run3.out" 2>&1
rc3=$?
set -e

if [ "$rc3" -eq 11 ]; then
    ok "a failing phase exits 11"
else
    bad "expected rc 11, got $rc3 ($(cat "$tmp/run3.out"))"
fi
seen3="$(tr '\n' ' ' < "$TRACKLOG_3")"
if [ "$seen3" = "E F " ]; then
    ok "the run stopped after the failing phase — the later phase never ran"
else
    bad "unexpected phase trace: $seen3"
fi
# jobs.tsv columns (fix round 1): when/job/branch/sha/why/rc/wall_s/waited_s/
# phase_failed/user_s/sys_s/cpu_ratio — phase_failed is column 9, not 8.
failed_col="$(awk -F'\t' -v j="sluice-${sha3:0:12}" '$0 ~ j {print $9}' "$HV_SLUICE_DIR/jobs.tsv" | tail -1)"
if [ "$failed_col" = "f" ]; then
    ok "jobs.tsv records phase f as the one that failed"
else
    bad "jobs.tsv phase_failed column is '$failed_col', expected 'f'"
fi
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "the claim is removed after a phase-failure exit too"
else
    bad "the claim survived a phase-failure exit"
fi

echo "== chamber: a killed chamber kills its children's process group before releasing the claim, and records why=TERM with a non-zero rc (fix round 1, Critical 2) =="
# Reproduces the exact reported mechanism: a real external killer only ever
# has the top-level pid (recorded in the claim), never a process group — so
# this sends a PLAIN `kill -TERM` to that one pid, exactly as an operator or
# a future abort tool would, and checks that the phase's own GRANDCHILD (the
# actual long-running work — the thing that used to reparent to init and
# keep the box busy, ledgered against scripts/lane-run.sh in
# .superpowers/sdd/followups.md) dies too, and that the claim is never
# observed gone while that grandchild is still alive.
lane_sets_4="$tmp/lane-sets-4.tsv"
grandchild_pid_file="$tmp/grandchild-pid"
rm -f "$grandchild_pid_file"
cat > "$tmp/phase-slow2.sh" <<'SH'
#!/usr/bin/env bash
( sleep 30 ) &
echo $! > "$GRANDCHILD_PID_FILE"
wait
SH
chmod +x "$tmp/phase-slow2.sh"
printf 'slow2\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-slow2.sh" > "$lane_sets_4"

sha4="$(new_topic_branch campaign/t4 topic4.txt)"
wt4="$tmp/wt4"

export HV_SLUICE_LANE_SETS="$lane_sets_4"
export HV_SLUICE_PHASES="slow2"
export HV_SLUICE_WORKTREE="$wt4"
export HV_CENSUS_CLAIM_PATH="$tmp/claim4"
export GRANDCHILD_PID_FILE="$grandchild_pid_file"
rm -f "$HV_CENSUS_CLAIM_PATH"

bash "$repo_root/scripts/sluice-run.sh" campaign/t4 "$sha4" > "$tmp/run4.out" 2>&1 &
run4_pid=$!

if poll_for_file "$HV_CENSUS_CLAIM_PATH" 50; then
    ok "the claim appears before the slow phase completes"
else
    bad "the claim never appeared — cannot test signal cleanup"
fi
chamber_pid="$(awk -F= '$1=="pid"{print $2}' "$HV_CENSUS_CLAIM_PATH")"
if [ -n "$chamber_pid" ] && pid_alive "$chamber_pid"; then
    ok "the claim's recorded pid ($chamber_pid) names a live process"
else
    bad "the claim's pid field is missing or not alive: '$chamber_pid'"
fi

if poll_for_file "$grandchild_pid_file" 50; then
    ok "the phase's own grandchild (the actual long-running work) has started"
else
    bad "the grandchild never started — cannot test child cleanup"
fi
grandchild_pid="$(cat "$grandchild_pid_file")"

# A PLAIN kill of the recorded top-level pid — not a process-group kill.
# Turning this into full-subtree cleanup is the chamber's own job
# (handle_signal + run_bg's process groups), which is what is under test.
kill -TERM "$chamber_pid" 2>/dev/null || true

# Poll BOTH conditions together, at the SAME instant, so a violation (the
# claim gone while the grandchild is still alive) cannot hide in the gap
# between two separate polls taken at different times.
resolved=0
violation=0
i=0
while [ "$i" -lt 100 ]; do
    if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
        resolved=1
        if pid_alive "$grandchild_pid"; then
            violation=1
        fi
        break
    fi
    sleep 0.03
    i=$((i+1))
done
if [ "$resolved" -eq 0 ]; then
    bad "the claim was never removed within 3s of the kill — cannot confirm ordering"
elif [ "$violation" -eq 0 ]; then
    ok "the claim was never observed gone while the grandchild was still alive"
else
    bad "observed the claim removed WHILE the grandchild process was still running — the box would read free while a child still holds it"
fi

if wait "$run4_pid" 2>/dev/null; then rc4=0; else rc4=$?; fi

if ! pid_alive "$grandchild_pid"; then
    ok "the grandchild process is gone after the kill — not orphaned"
else
    bad "the grandchild process ($grandchild_pid) is STILL RUNNING — orphaned"
fi
if [ ! -e "$HV_CENSUS_CLAIM_PATH" ]; then
    ok "the claim is removed after the chamber is killed with SIGTERM mid-phase"
else
    bad "the claim survived a SIGTERM kill mid-phase"
fi
if [ "$rc4" -ne 0 ]; then
    ok "a killed run's own process exits with a non-zero status ($rc4)"
else
    bad "a killed run exited 0 — indistinguishable from a full green run"
fi
# jobs.tsv columns (fix round 1): when/job/branch/sha/why/rc/wall_s/waited_s/
# phase_failed/user_s/sys_s/cpu_ratio.
why_col="$(awk -F'\t' -v j="sluice-${sha4:0:12}" '$0 ~ j {print $5}' "$HV_SLUICE_DIR/jobs.tsv" | tail -1)"
rc_col="$(awk -F'\t' -v j="sluice-${sha4:0:12}" '$0 ~ j {print $6}' "$HV_SLUICE_DIR/jobs.tsv" | tail -1)"
if [ "$why_col" = "TERM" ]; then
    ok "jobs.tsv records why=TERM for the killed run"
else
    bad "jobs.tsv why column is '$why_col', expected 'TERM'"
fi
if [ -n "$rc_col" ] && [ "$rc_col" -ne 0 ] 2>/dev/null; then
    ok "jobs.tsv records a non-zero rc ($rc_col) for the killed run — never indistinguishable from a green run"
else
    bad "jobs.tsv rc column is '$rc_col', expected non-zero"
fi

echo "== push: a chamber killed mid-phase never pushes — main does not move (this is the assertion that matters most in the whole campaign) =="
# Task 4's review found that \$? inside bash's own EXIT trap reads 0 when the
# shell dies from a signal, so a killed run's rc alone cannot be trusted to
# gate the push — the \`why\` guard (Step 3, sluice-run.sh) exists for exactly
# this. This cannot be proven from the outside (the guard is INSIDE the
# script that owns the push): construct a real chamber run, kill it with
# SIGTERM mid-phase — before it ever reaches the drift check or the push —
# and assert on the one fact that actually matters: the SHARED ORIGIN'S main
# never moved. jobs.tsv's why/rc columns are checked too, but only as
# corroborating evidence; a reviewer could otherwise object that a message
# alone was checked while the push silently happened anyway.
origin_main_before_kill="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"

lane_sets_push="$tmp/lane-sets-push.tsv"
sleeper_pid_file="$tmp/sleeper-pid"; rm -f "$sleeper_pid_file"
cat > "$tmp/phase-sleeper.sh" <<'SH'
#!/usr/bin/env bash
( sleep 30 ) &
echo $! > "$SLEEPER_PID_FILE"
wait
SH
chmod +x "$tmp/phase-sleeper.sh"
printf 'sleeper\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-sleeper.sh" > "$lane_sets_push"

shapush="$(new_topic_branch campaign/tpush topicpush.txt)"
wtpush="$tmp/wtpush"

export HV_SLUICE_LANE_SETS="$lane_sets_push"
export HV_SLUICE_PHASES="sleeper"
export HV_SLUICE_WORKTREE="$wtpush"
export HV_CENSUS_CLAIM_PATH="$tmp/claimpush"
export SLEEPER_PID_FILE="$sleeper_pid_file"
rm -f "$HV_CENSUS_CLAIM_PATH"

bash "$repo_root/scripts/sluice-run.sh" campaign/tpush "$shapush" > "$tmp/runpush.out" 2>&1 &
runpush_pid=$!

if poll_for_file "$HV_CENSUS_CLAIM_PATH" 50; then
    ok "the claim appears before the sleeper phase completes"
else
    bad "the claim never appeared — cannot test the push guard"
fi
if poll_for_file "$sleeper_pid_file" 50; then
    ok "the sleeper phase's own grandchild has started (so the kill lands mid-phase, well before the push)"
else
    bad "the sleeper grandchild never started — cannot test the push guard"
fi
chamberpush_pid="$(awk -F= '$1=="pid"{print $2}' "$HV_CENSUS_CLAIM_PATH")"

kill -TERM "$chamberpush_pid" 2>/dev/null || true
wait "$runpush_pid" 2>/dev/null || true

origin_main_after_kill="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
if [ "$origin_main_after_kill" = "$origin_main_before_kill" ]; then
    ok "the shared origin's main did NOT move after a mid-phase SIGTERM kill — the killed candidate was never pushed"
else
    bad "origin's main MOVED after a killed run ($origin_main_before_kill -> $origin_main_after_kill) — an untested tree reached main"
fi

why_col_push="$(awk -F'\t' -v j="sluice-${shapush:0:12}" '$0 ~ j {print $5}' "$HV_SLUICE_DIR/jobs.tsv" | tail -1)"
rc_col_push="$(awk -F'\t' -v j="sluice-${shapush:0:12}" '$0 ~ j {print $6}' "$HV_SLUICE_DIR/jobs.tsv" | tail -1)"
if [ "$why_col_push" = "TERM" ]; then
    ok "jobs.tsv records why=TERM for the killed candidate (corroborating evidence, not the property itself)"
else
    bad "jobs.tsv why column is '$why_col_push', expected 'TERM'"
fi
if [ -n "$rc_col_push" ] && [ "$rc_col_push" -ne 0 ] 2>/dev/null; then
    ok "jobs.tsv records a non-zero rc ($rc_col_push) for the killed candidate"
else
    bad "jobs.tsv rc column is '$rc_col_push', expected non-zero"
fi

echo "== chamber: a child that traps and swallows TERM is escalated to SIGKILL within a bounded deadline, and the lock is genuinely freed (fix round 2) =="
# Reproduces the exact reported mechanism: round 1's handle_signal sent
# TERM and then `wait`ed with no timeout and no escalation. Verified by the
# reviewer that a child which traps TERM makes that wait block forever, the
# claim is never removed, and fd 9's flock is never released — a
# permanently wedged lane with no force override. This phase's own child
# (and ITS OWN backgrounded grandchild) both explicitly ignore TERM, so only
# the bounded escalation to SIGKILL can ever end this run.
lane_sets_5="$tmp/lane-sets-5.tsv"
stubborn_pid_file="$tmp/stubborn-pid"; rm -f "$stubborn_pid_file"
cat > "$tmp/phase-stubborn.sh" <<'SH'
#!/usr/bin/env bash
trap '' TERM
( trap '' TERM; sleep 60 ) &
echo $! > "$STUBBORN_PID_FILE"
wait
SH
chmod +x "$tmp/phase-stubborn.sh"
printf 'stubborn\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-stubborn.sh" > "$lane_sets_5"

sha5="$(new_topic_branch campaign/t5 topic5.txt)"
wt5="$tmp/wt5"

export HV_SLUICE_LANE_SETS="$lane_sets_5"
export HV_SLUICE_PHASES="stubborn"
export HV_SLUICE_WORKTREE="$wt5"
export HV_CENSUS_CLAIM_PATH="$tmp/claim5"
export HV_SLUICE_KILL_TIMEOUT=2   # short for test speed; production default is 30s
export STUBBORN_PID_FILE="$stubborn_pid_file"
rm -f "$HV_CENSUS_CLAIM_PATH"

bash "$repo_root/scripts/sluice-run.sh" campaign/t5 "$sha5" > "$tmp/run5.out" 2>&1 &
run5_pid=$!

if poll_for_file "$HV_CENSUS_CLAIM_PATH" 50; then
    ok "the claim appears before the stubborn phase completes"
else
    bad "the claim never appeared — cannot test escalation"
fi
chamber5_pid="$(awk -F= '$1=="pid"{print $2}' "$HV_CENSUS_CLAIM_PATH")"
if poll_for_file "$stubborn_pid_file" 50; then
    ok "the stubborn (TERM-ignoring) grandchild has started"
else
    bad "the stubborn grandchild never started — cannot test escalation"
fi
stubborn_pid="$(cat "$stubborn_pid_file")"

kill_started="$(date +%s.%N)"
kill -TERM "$chamber5_pid" 2>/dev/null || true

if poll_for_absence "$HV_CENSUS_CLAIM_PATH" 150; then
    kill_ended="$(date +%s.%N)"
    ok "the claim is eventually removed — a TERM-ignoring child does not wedge the box forever"
else
    bad "the claim was never removed within 15s — the box appears wedged (the exact failure this fix closes)"
    kill_ended="$kill_started"
fi

elapsed="$(awk -v a="$kill_started" -v b="$kill_ended" 'BEGIN{printf "%.2f", b-a}')"
# THE TIMING EVIDENCE: bounded well below (proves it did not hang past the
# deadline) and clearly NOT near-instant (proves it genuinely waited rather
# than escalating immediately). The lower bound is loose on purpose:
# `handle_signal`'s deadline math uses bash's own $SECONDS, which has
# ONE-SECOND granularity, not sub-second — both when the deadline is set
# (`SECONDS + N`, off by up to 1s from the true elapsed time already) and
# when the loop's own exit condition is checked (`SECONDS -lt deadline`, so
# it can fire anywhere up to 1s early relative to a sub-second clock). Found
# live: a 2s nominal deadline measured 1.85s in one run and 1.24s in
# another, both genuine, neither a bug — a tight lower bound here would be
# testing $SECONDS' own rounding, not the escalation logic.
if awk -v e="$elapsed" -v t="2" 'BEGIN{exit !(e>=0.5 && e<=t+6)}'; then
    ok "the deadline worked: claim removed ${elapsed}s after SIGTERM (escalation deadline was 2s) — neither instant nor forever"
else
    bad "unexpected timing: claim removed after ${elapsed}s against a 2s escalation deadline"
fi

if ! pid_alive "$stubborn_pid"; then
    ok "the TERM-ignoring grandchild is gone — SIGKILL escalation reached it"
else
    bad "the TERM-ignoring grandchild ($stubborn_pid) is STILL RUNNING — escalation did not reach it"
fi

# THE LOCK, NOT JUST THE CLAIM FILE: a fresh acquirer on the SAME lock path
# must succeed quickly once this job is gone, proving fd 9 was actually
# closed and the flock released — not merely that the claim file (a
# separate, unlocked marker) was removed.
if flock -w 3 -E 99 "$HV_CENSUS_LOCK" -c true; then
    ok "the lock is genuinely freed after escalation — a fresh acquirer succeeds"
else
    bad "the lock is still held after the stubborn job is gone — fd 9 was never released"
fi
wait "$run5_pid" 2>/dev/null || true

echo "== chamber: a second signal arriving mid-escalation does not re-enter or restack the shutdown (fix round 2) =="
# Reproduces the reviewer's second observation: an earlier version of
# handle_signal re-entered when a second signal arrived while the first was
# still tearing a child down. Two TERMs sent 0.3s apart, both well inside
# the 3s escalation window below, so the second unambiguously lands while
# the first invocation is still in its escalation wait.
lane_sets_6="$tmp/lane-sets-6.tsv"
stubborn2_pid_file="$tmp/stubborn2-pid"; rm -f "$stubborn2_pid_file"
cat > "$tmp/phase-stubborn2.sh" <<'SH'
#!/usr/bin/env bash
trap '' TERM
( trap '' TERM; sleep 60 ) &
echo $! > "$STUBBORN2_PID_FILE"
wait
SH
chmod +x "$tmp/phase-stubborn2.sh"
printf 'stubborn2\tcommit\tlocal\tno\tbash %s\n' "$tmp/phase-stubborn2.sh" > "$lane_sets_6"

sha6="$(new_topic_branch campaign/t6 topic6.txt)"
wt6="$tmp/wt6"

export HV_SLUICE_LANE_SETS="$lane_sets_6"
export HV_SLUICE_PHASES="stubborn2"
export HV_SLUICE_WORKTREE="$wt6"
export HV_CENSUS_CLAIM_PATH="$tmp/claim6"
export HV_SLUICE_KILL_TIMEOUT=3
export STUBBORN2_PID_FILE="$stubborn2_pid_file"
rm -f "$HV_CENSUS_CLAIM_PATH"

bash "$repo_root/scripts/sluice-run.sh" campaign/t6 "$sha6" > "$tmp/run6.out" 2>&1 &
run6_pid=$!

if poll_for_file "$HV_CENSUS_CLAIM_PATH" 50; then
    ok "the claim appears before the second stubborn phase completes"
else
    bad "the claim never appeared — cannot test reentrancy"
fi
chamber6_pid="$(awk -F= '$1=="pid"{print $2}' "$HV_CENSUS_CLAIM_PATH")"
# The claim appears right after the lock is acquired — well BEFORE the merge
# and the phase itself run. Sending the first kill here (without waiting for
# the phase's own grandchild to exist) would land it during the MERGE
# instead, which dies instantly to a plain TERM — no escalation, nothing to
# send a second signal INTO, and the "first" test's own investigation found
# exactly this: wall_s=0 and no "=== phase stubborn2 ===" line in the log.
# Waiting for the grandchild's pid file the same way the escalation test
# does guarantees both kills land inside the actual phase.
if poll_for_file "$stubborn2_pid_file" 50; then
    ok "the second stubborn (TERM-ignoring) grandchild has started"
else
    bad "the second stubborn grandchild never started — cannot test reentrancy"
fi

kill -TERM "$chamber6_pid" 2>/dev/null || true
sleep 0.3
kill -TERM "$chamber6_pid" 2>/dev/null || true

if poll_for_absence "$HV_CENSUS_CLAIM_PATH" 150; then
    ok "the run still completes cleanly after a second signal mid-handling"
else
    bad "the claim was never removed — a second signal broke the shutdown"
fi
wait "$run6_pid" 2>/dev/null || true

job6_log="$(find "$HV_SLUICE_DIR" -maxdepth 1 -name "sluice-${sha6:0:12}-*.log" 2>/dev/null | head -1)"
if [ -n "$job6_log" ] && grep -q "ignoring the repeat" "$job6_log"; then
    ok "the second signal was recognised and ignored, not re-entered (the log confirms it)"
else
    bad "no 'ignoring the repeat' message found in the job log — cannot confirm the second signal did not re-enter"
fi
# `grep -c` ALREADY prints "0" (and exits 1) on no match, so `|| echo 0`
# would double the output on that path (caught live: produced "0\n0",
# which then failed the `-eq` test below with a shell syntax error rather
# than a clean assertion failure). `|| true` adds nothing on top of what
# grep already printed.
if [ -n "$job6_log" ]; then
    caught_count="$(grep -c "caught SIG" "$job6_log" || true)"
else
    caught_count=0
fi
if [ "$caught_count" -eq 1 ]; then
    ok "exactly one shutdown sequence ran (one 'caught SIG' line) — the second signal did not restack a second escalation"
else
    bad "expected exactly one 'caught SIG' line, found $caught_count — the second signal may have restacked the shutdown"
fi

echo "== push: the recorded last-pushed SHA is what the mouth compares against"
cd "$scratch"
mkdir -p "$HV_SLUICE_DIR"
g checkout -q main
g rev-parse main > "$HV_SLUICE_DIR/last-pushed"
# Move main out from under the recorded value — simulating an out-of-band land.
printf 'oob\n' >> f.txt; g commit -qam out-of-band
export HV_SLUICE_BASE=main
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/x "$NEW" >/dev/null 2>"$tmp/oob.err"
rc=$?
set -e
if [ "$rc" -eq 4 ]; then
    ok "an out-of-band landing on the base is detected (exit 4)"
else
    bad "expected exit 4 for an out-of-band landing, got $rc"
fi
if grep -q 'OUT-OF-BAND' "$tmp/oob.err"; then
    ok "the out-of-band message names the condition"
else
    bad "no OUT-OF-BAND message"
fi
rm -f "$HV_SLUICE_DIR/last-pushed"; unset HV_SLUICE_BASE

echo "== chamber: a kind=stage run gates the merge product and NEVER pushes (Task 12) =="
# THE ONE PROPERTY THAT MAKES A STAGE GATE SAFE TO ABSORB INTO THE MERGE
# QUEUE. Everything else about the two kinds is identical by construction
# (same claim, same worktree, same merge, same phase loop), so the only
# thing worth pinning is the difference: a stage run must reach rc=0 with
# `main` exactly where it started. Asserting the *absence* of a push is the
# assertion that can go vacuously green, so this reads origin's own ref
# before and after and compares them, rather than trusting the log text.
cd "$chamber_repo"
lane_sets_stage="$tmp/lane-sets-stage.tsv"
TRACKLOG_STAGE="$tmp/tracklog-stage"; : > "$TRACKLOG_STAGE"
cat > "$tmp/phase-stage.sh" <<'SH'
#!/usr/bin/env bash
set -e
echo S >> "$TRACKLOG_STAGE"
SH
chmod +x "$tmp/phase-stage.sh"
printf 's\tstage\tlane\tno\tbash %s\n' "$tmp/phase-stage.sh" > "$lane_sets_stage"

sha_stage="$(new_topic_branch campaign/tstage topic-stage.txt)"
wt_stage="$tmp/wt-stage"
origin_main_before_stage="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
last_pushed_before_stage="$(cat "$HV_SLUICE_DIR/last-pushed" 2>/dev/null || true)"

HV_SLUICE_LANE_SETS="$lane_sets_stage" \
HV_SLUICE_PHASES="s" \
HV_SLUICE_WORKTREE="$wt_stage" \
HV_CENSUS_CLAIM_PATH="$tmp/claim-stage" \
TRACKLOG_STAGE="$TRACKLOG_STAGE" \
    bash "$repo_root/scripts/sluice-run.sh" campaign/tstage "$sha_stage" stage \
    > "$tmp/run-stage.out" 2>&1 && rc_stage=0 || rc_stage=$?

origin_main_after_stage="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
last_pushed_after_stage="$(cat "$HV_SLUICE_DIR/last-pushed" 2>/dev/null || true)"

if [ "$rc_stage" -eq 0 ]; then
    ok "a green stage run exits 0"
else
    bad "expected rc 0 from a green stage run, got $rc_stage ($(cat "$tmp/wt-stage.log" 2>/dev/null; tail -30 "$HV_SLUICE_DIR"/*.log 2>/dev/null))"
fi
if [ "$(tr -d '\n' < "$TRACKLOG_STAGE")" = "S" ]; then
    ok "the stage run really ran its phase (so the no-push assertion below is not vacuous)"
else
    bad "the stage run's phase never executed: '$(tr -d '\n' < "$TRACKLOG_STAGE")'"
fi
if [ "$origin_main_after_stage" = "$origin_main_before_stage" ]; then
    ok "origin's main is untouched by a green stage run ($origin_main_before_stage)"
else
    bad "a stage run PUSHED: origin main moved $origin_main_before_stage -> $origin_main_after_stage"
fi
if [ "$last_pushed_after_stage" = "$last_pushed_before_stage" ]; then
    ok "last-pushed is untouched by a stage run"
else
    bad "a stage run rewrote last-pushed: '$last_pushed_before_stage' -> '$last_pushed_after_stage'"
fi
if grep -q 'STAGE REPORT' "$HV_SLUICE_DIR"/sluice-"${sha_stage:0:12}"-*.log 2>/dev/null; then
    ok "the stage run's log carries the STAGE REPORT verdict line"
else
    bad "no STAGE REPORT line in the stage run's log"
fi

echo "== chamber: MUTATION — deleting the kind gate makes the stage run push, reddening the assertion above =="
# The assertion above is an ABSENCE, which is exactly the shape that passes
# for the wrong reason. Prove it can fail: take the real script, delete the
# four lines that turn `kind=stage` into an early `exit 0`, and confirm the
# same scenario now moves origin's main.
#
# The range is anchored on the block's own opening COMMENT, not on `if [
# "$kind" = "stage" ]` — that condition appears twice in the file (the phase
# selection near the top uses it too), and deleting the first occurrence
# would leave `$phases` unset under `set -u`, producing a mutant that fails
# for a reason unrelated to pushing. The guard below checks BOTH directions:
# the gate is gone AND the phase selection survived.
mutant_run="$tmp/sluice-run-nokindgate.sh"
sed '/^# THE ONE BRANCH THAT MAKES A STAGE GATE A KIND/,/^fi$/d' \
    "$repo_root/scripts/sluice-run.sh" > "$mutant_run"
if grep -q 'STAGE REPORT' "$mutant_run" || ! grep -q 'merge_phases=' "$mutant_run"; then
    bad "the mutation did not cleanly remove just the kind gate — the demonstration below proves nothing"
else
    sha_stage_m="$(new_topic_branch campaign/tstagem topic-stage-m.txt)"
    origin_main_before_m="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
    HV_SLUICE_LANE_SETS="$lane_sets_stage" \
    HV_SLUICE_PHASES="s" \
    HV_SLUICE_WORKTREE="$tmp/wt-stage-m" \
    HV_CENSUS_CLAIM_PATH="$tmp/claim-stage-m" \
    TRACKLOG_STAGE="$TRACKLOG_STAGE" \
        bash "$mutant_run" campaign/tstagem "$sha_stage_m" stage \
        > "$tmp/run-stage-m.out" 2>&1 || true
    origin_main_after_m="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main | cut -f1)"
    if [ "$origin_main_after_m" != "$origin_main_before_m" ]; then
        ok "the mutant DOES push on kind=stage — the real script's no-push assertion is live, not vacuous"
    else
        bad "the mutant did not push either; the no-push assertion above may be passing for an unrelated reason"
    fi
fi
cd "$scratch"

echo "== request: REF shape validation (no ssh involved)"
if bash "$repo_root/scripts/sluice-request.sh" campaign/x short 2>"$tmp/req.err"; then
    bad "a short REF was accepted"
else
    ok "a short REF is refused (exit nonzero)"
fi
if grep -q '40-char SHA' "$tmp/req.err"; then
    ok "the short-REF refusal names the 40-char requirement"
else
    bad "no 40-char message on a short REF"
fi

echo "== request: an unpushed (but well-formed) REF is refused before ssh ever runs"
# A syntactically valid 40-hex string that is not reachable from any remote
# branch of THIS repo. sluice-request.sh's own repo_root is derived from its
# OWN location (scripts/sluice-request.sh's BASH_SOURCE), not from cwd or any
# scratch repo this suite builds elsewhere — same shape as
# scripts/lane-dispatch.sh's own `git -C "$repo_root" branch -r --contains`,
# so this check runs against the REAL repository test-sluice.sh itself lives
# in, not the throwaway $scratch/$chamber_repo trees used above.
unpushed_ref="deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
if bash "$repo_root/scripts/sluice-request.sh" campaign/x "$unpushed_ref" 2>"$tmp/req2.err"; then
    bad "an unpushed REF was accepted"
else
    ok "an unpushed (well-formed) REF is refused (exit nonzero)"
fi
if grep -q 'not on any remote branch' "$tmp/req2.err"; then
    ok "the unpushed-REF refusal names the reason"
else
    bad "no 'not on any remote branch' message on an unpushed REF"
fi

echo "== request: durable-before-nudge — a failed remote enqueue prints no success line"
# A fake `ssh` on PATH, standing in for the real canonical-box connection.
# FAKE_SSH_RESULT picks whether the "remote" sluice-queue.sh add succeeded.
# Real, already-pushed refs (this repo's own origin/main and
# origin/campaign/the-sluice tips) so the REF-is-pushed check above this one
# in sluice-request.sh passes and execution actually reaches the ssh call.
mkdir -p "$tmp/bin"
cat > "$tmp/bin/ssh" <<'FAKESSH'
#!/usr/bin/env bash
# Stand-in for a real ssh call in the request-path tests: ignores its host
# and remote-command arguments (this suite is not exercising THEIR content,
# only how sluice-request.sh reacts to the exit code and stdout/stderr ssh
# would return) and answers according to $FAKE_SSH_RESULT.
case "${FAKE_SSH_RESULT:-ok}" in
    ok)   echo "req-deadbeef0000-20260101T000000Z" ;;
    fail) echo "sluice-queue-remote: add refused" >&2; exit 1 ;;
esac
FAKESSH
chmod +x "$tmp/bin/ssh"

# A pushed ref THAT CARRIES A HEADLINE TRAILER. These tests are about the
# enqueue path — how sluice-request.sh reacts to ssh's exit code — and they
# must reach the ssh call to test anything. Since the headline check now
# refuses a kind=merge submission with no `Sluice-Headline:` trailer, a bare
# real ref (origin/campaign/the-sluice, which predates the convention) is
# turned away before the ssh call and every assertion below it reads as an
# enqueue failure. Mint a commit on top of that ref with the trailer, and
# publish it under refs/remotes/ so the is-it-pushed check passes too. Its
# parent is an ancestor of `main`, so the `main..sha` range the check runs
# over is exactly this one commit.
pushed_base="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" rev-parse origin/campaign/the-sluice)"
pushed_ref="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree \
    "$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" rev-parse "$pushed_base^{tree}")" \
    -p "$pushed_base" -m "chore: a commit for the enqueue-path tests

Sluice-Headline: a headline so these tests reach the enqueue step")"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test/nudge "$pushed_ref"

if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=fail \
    bash "$repo_root/scripts/sluice-request.sh" campaign/x "$pushed_ref" \
    >"$tmp/req3.out" 2>"$tmp/req3.err"; then
    bad "sluice-request.sh exited 0 despite a failed remote enqueue"
else
    ok "sluice-request.sh exits nonzero when the remote enqueue fails"
fi
if grep -q "read it back" "$tmp/req3.out"; then
    bad "the nudge-worthy 'read it back' line was printed despite the enqueue failing — a caller watching stdout would wrongly nudge an operator about a request that was never queued"
else
    ok "no 'read it back' line on a failed enqueue — nothing here would cue a caller to nudge an operator about an unqueued request"
fi
if grep -q 'remote enqueue FAILED' "$tmp/req3.err"; then
    ok "the failure is reported explicitly on stderr"
else
    bad "no explicit failure message on a failed enqueue"
fi

if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=ok \
    bash "$repo_root/scripts/sluice-request.sh" campaign/x "$pushed_ref" \
    >"$tmp/req4.out" 2>"$tmp/req4.err"; then
    ok "sluice-request.sh exits 0 when the remote enqueue succeeds"
else
    bad "sluice-request.sh exited nonzero despite a successful (mocked) remote enqueue"
fi
if grep -q 'req-deadbeef0000-20260101T000000Z' "$tmp/req4.out" && grep -q "read it back" "$tmp/req4.out"; then
    ok "a successful enqueue prints the id and the nudge-worthy 'read it back' line — exactly the signal a submit workflow should gate its operator-nudge step on"
else
    bad "a successful enqueue did not print the expected id/read-it-back lines"
fi

echo "== request: MUTATION — printing success unconditionally would flip the failed-enqueue test red"
# Demonstrates the durable-before-nudge test above is non-vacuous: a
# sluice-request.sh that echoes the ssh output regardless of its exit code
# (the exact bug lane-dispatch.sh's own header names — "a dispatch that did
# not start must not report success") makes the FAILURE case above wrongly
# look like a nudge-worthy success.
#
# The mutant is written INSIDE scripts/, not under $tmp: sluice-request.sh
# derives its own repo_root from its own BASH_SOURCE location (dirname/..),
# so a copy living outside the repo cannot find
# scripts/census-canonical-host.txt or `git -C` its way to a real repo at
# all — this was caught live (the first cut of this mutation landed the
# mutant in $tmp and failed for that reason, not because the mutation didn't
# take). Removed unconditionally on the way out, success or failure.
mutant="$repo_root/scripts/.sluice-request-mutant-for-test.sh"
# Line-number surgery, not a text-pattern substitution: the four-line
# `if ! req=...; then …; exit 1; fi` guard becomes one unconditional
# assignment that swallows ssh's exit code entirely (`|| true`) — the exact
# "reported success regardless of whether the remote side actually ran"
# shape lane-dispatch.sh's own header warns about. Located by matching the
# guard's own first and last lines so a future edit to sluice-request.sh
# that moves this block does not silently mutate the wrong lines.
# shellcheck disable=SC2016  # single-quoted on purpose: this is a literal grep pattern, not a shell expansion
guard_start="$(grep -n '^if ! req="\$(ssh' "$repo_root/scripts/sluice-request.sh" | cut -d: -f1)"
# The FIRST `^fi$' AFTER guard_start — sluice-request.sh's earlier
# unpushed-ref check closes its own `if` with a bare `fi` too, so an
# unconditional `head -1` over the whole file would find THAT one instead.
guard_end="$(awk -v s="$guard_start" 'NR>s && /^fi$/{print NR; exit}' "$repo_root/scripts/sluice-request.sh")"
if [ -z "$guard_start" ] || [ -z "$guard_end" ]; then
    bad "could not locate the ssh-guard block in sluice-request.sh to mutate — sluice-request.sh may have changed shape"
else
    {
        head -n "$((guard_start - 1))" "$repo_root/scripts/sluice-request.sh"
        # shellcheck disable=SC2016  # single-quoted on purpose: this is the literal mutant source line, not a shell expansion here
        echo 'req="$(ssh "$host" "$remote_cmd" 2>/dev/null || true)"'
        tail -n "+$((guard_end + 1))" "$repo_root/scripts/sluice-request.sh"
    } > "$mutant"
fi
chmod +x "$mutant"
if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=fail bash "$mutant" campaign/x "$pushed_ref" \
    >"$tmp/req5.out" 2>"$tmp/req5.err"; then
    if grep -q "read it back" "$tmp/req5.out"; then
        ok "MUTATION CONFIRMED: the unguarded mutant prints the nudge-worthy line even on a failed enqueue (the real script does not — see above)"
    else
        bad "the mutant did not reproduce the expected false-success shape"
    fi
else
    bad "the mutant unexpectedly still exited nonzero — mutation did not take"
fi
rm -f "$mutant"

echo "== request: a 40-char ref with an embedded non-hex character is rejected (not just the first char)"
# Fix round 1, Important 1. `case "$ref" in [0-9a-f]*)` (the brief's original,
# modelled on the older lane-dispatch.sh shape) is a GLOB: it matches
# anything STARTING with one hex digit, so a 40-char string with a bad 2nd
# character sailed through. sluice-mouth.sh already hit and fixed this exact
# bug in Task 3 of this same plan (see its own "a 40-char ref with a
# non-hex character is rejected" section above) — this pins the same fix in
# sluice-request.sh.
BAD_SHA="deadbeefdeadbeefdeadbeefdeadbeefdeadbeez"
if [ "${#BAD_SHA}" -eq 40 ]; then
    ok "test setup: BAD_SHA is exactly 40 characters"
else
    bad "test setup: BAD_SHA is not 40 characters (${#BAD_SHA})"
fi
if bash "$repo_root/scripts/sluice-request.sh" campaign/x "$BAD_SHA" 2>"$tmp/badsha.err"; then
    bad "a 40-char ref with an embedded non-hex character was accepted"
else
    ok "a 40-char ref with an embedded non-hex character is rejected (exit nonzero)"
fi
if grep -q 'hex only' "$tmp/badsha.err"; then
    ok "the rejection reason names the hex requirement"
else
    bad "no hex-only message on a non-hex 40-char ref"
fi

echo "== request: a headline-less or junk-headline submission is refused BY THE SCRIPT, not merely advised in a skill"
# Fix round 1, Important 2. sluice-request.sh now derives the SAME subject
# sluice-run.sh's own default will actually use
# (`git log -1 --format=%s "$sha"`) and refuses to enqueue when it looks
# like a placeholder. Real commit objects, not strings the test merely
# hands the script: `git commit-tree` mints them without touching this
# repository's working tree or any real branch, and a throwaway
# refs/remotes/ ref (never a real remote, never pushed anywhere) satisfies
# the earlier "is this on a remote branch" check so execution actually
# reaches the headline check under test. Both refs are removed
# unconditionally below and the sweep is duplicated in the suite's own EXIT
# trap in case something aborts in between.
headline_test_tree="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" rev-parse "HEAD^{tree}")"
wip_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD -m "wip")"
# `git commit-tree -m ""` genuinely succeeds (verified: git only refuses an
# empty message from the interactive `git commit` editor path, not from
# commit-tree), so the truly headline-LESS case — not merely a junk one — is
# a real commit object here too, not a string the test merely hands the
# script.
empty_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD -m "")"
good_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD -m "feat(sluice): a real headline for testing")"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test/wip "$wip_sha"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test/empty "$empty_sha"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test/good "$good_sha"

# HV_SLUICE_BASE IS PINNED TO HEAD FOR EVERY INVOCATION IN THIS SECTION, and
# that is hermeticity rather than convenience. The trailer is searched over
# `$HV_SLUICE_BASE..$sha`, and these commits are minted with `-p HEAD`, so
# pinning the base to HEAD makes the range exactly the one minted commit
# wherever this suite runs. Without it the range is `origin/main..$sha`, and
# inside a chamber phase HEAD is the MERGE PRODUCT — so the range would
# include the candidate's own commits, which under this very convention carry
# a `Sluice-Headline:` trailer. A junk-subject commit would then inherit
# somebody else's headline and be accepted, and the refusal test would go
# green for a reason that has nothing to do with what it is testing.
req() { HV_SLUICE_BASE=HEAD bash "$repo_root/scripts/sluice-request.sh" "$@"; }

if req campaign/x "$wip_sha" 2>"$tmp/wip.err"; then
    bad "a submission whose commit subject is 'wip' (junk), with no trailer, was accepted"
else
    ok "a submission whose commit subject is 'wip' (junk), with no trailer, is refused"
fi
if grep -q 'Sluice-Headline' "$tmp/wip.err"; then
    ok "the refusal names the trailer to add (and points at the census-epoch-label consequence)"
else
    bad "the refusal does not name Sluice-Headline, so it does not tell the author what to do"
fi

if req campaign/x "$empty_sha" 2>"$tmp/empty.err"; then
    bad "a submission with a genuinely EMPTY commit subject and no trailer was accepted"
else
    ok "a submission with a genuinely empty commit subject and no trailer is refused"
fi

# THE CONTRACT CHANGE, PINNED. `good_sha` carries a perfectly respectable
# subject — `feat(sluice): a real headline for testing` — and no trailer. The
# OLD rule accepted exactly this, and that is precisely how a tidy-up commit
# became a permanent label: the junk check knows placeholder WORDS, and an
# ordinary subject is not a placeholder word. Under the new rule a subject,
# however good, is not an authored headline.
if req campaign/x "$good_sha" >/dev/null 2>"$tmp/goodsubj.err"; then
    bad "a real-looking SUBJECT with no trailer was accepted — this is the old rule, and the failure it caused"
else
    ok "a real-looking subject with no trailer is refused: a subject is not an authored headline"
fi

# The accepting case: an unremarkable subject, with the trailer that matters.
# This is the displacement failure in miniature — the commit's own subject is
# the kind of thing that used to become a permanent label, and the trailer is
# what actually gets used.
trailer_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD \
    -m "chore: tidy up after the real work

Sluice-Headline: the thing that actually landed")"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test/trailer "$trailer_sha"
if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=ok \
    req campaign/x "$trailer_sha" >"$tmp/trailer.out" 2>"$tmp/trailer.err"; then
    ok "a submission carrying a Sluice-Headline: trailer is accepted, whatever its subject says"
else
    bad "a submission with a valid trailer was wrongly refused: $(cat "$tmp/trailer.err")"
fi

# THE PLACEMENT TRAP, PINNED IN BOTH DIRECTIONS. git's trailer parser reads
# only the message's LAST block, so a `Sluice-Headline:` stranded above a
# blank line and another trailer is silently invisible — an unparsed trailer
# and an absent one are indistinguishable. The commit that introduced this
# whole convention was written that way and its own headline did not resolve.
# Since the mechanism cannot be made forgiving without abandoning git's own
# semantics, the trap is instead made LOUD: refused, with the reason named.
stranded_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD \
    -m "chore: a commit whose trailer is in the wrong block

Sluice-Headline: this line is stranded above a blank line

Claude-Session: https://example.invalid/session")"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test/stranded "$stranded_sha"
if req campaign/x "$stranded_sha" 2>"$tmp/stranded.err"; then
    bad "a Sluice-Headline stranded outside the final block was ACCEPTED — it would silently fall back at merge time"
else
    ok "a Sluice-Headline outside the message's final block is refused, not silently ignored"
fi
if grep -q 'LAST block' "$tmp/stranded.err"; then
    ok "the refusal explains the placement rule, so the author is not left guessing why a trailer they wrote did nothing"
else
    bad "the refusal does not mention block placement — the trap stays silent in practice"
fi

echo "== request: MUTATION — the pre-fix tip-subject rule accepts the commit the real one refuses"
# Non-vacuity for the contract change above. Revert ONLY the headline
# derivation to the old `git log -1 --format=%s` and confirm `good_sha` — a
# fine subject with no trailer — sails through, which is the behaviour that
# put a doubled label and three malformed ones on main.
hmut="$repo_root/scripts/.sluice-request-trailer-mutant-for-test.sh"
# `#` as the sed delimiter, NOT `|`: the replacement text contains `||`, and
# a `|`-delimited s/// ends at the first one — "unknown option to `s'", which
# is how this was caught. The replacement must stay single-quoted so `$ref`
# and `$repo_root` reach the mutant as literal shell, not this script's values.
# shellcheck disable=SC2016
sed -e 's#^    headline="\$(sluice_headline_of .*)"$#    headline="$(git -C "$repo_root" log -1 --format=%s "$ref" 2>/dev/null || true)"#' \
    "$repo_root/scripts/sluice-request.sh" > "$hmut"
if grep -q 'log -1 --format=%s' "$hmut"; then
    ok "test setup: the mutant carries the old tip-subject derivation (so an accept below means the rule, not a dead script)"
else
    bad "the mutation sed did not apply — the assertion below would pass vacuously"
fi
if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=ok HV_SLUICE_BASE=HEAD \
    bash "$hmut" campaign/x "$good_sha" >/dev/null 2>&1; then
    ok "MUTATION CONFIRMED: the old tip-subject rule accepts a trailer-less commit (the real one refuses it — see above)"
else
    bad "the mutant also refused — the test above is not pinning the trailer requirement"
fi
rm -f "$hmut"

echo "== request: a kind=stage submission with the SAME junk headline is ACCEPTED (Task 12)"
# The exemption's own test, and it is the reason the refusal above needed one
# too. `wip_sha` is the identical commit object refused a few lines up: the
# only thing that changed is the kind. A stage gate's merge commit is
# discarded with the chamber's worktree, so no subject it carries can become
# the census epoch label the refusal exists to protect — and refusing a
# mid-campaign `wip` commit that a plan-stage boundary legitimately sits on
# would block the one thing a stage gate is for.
if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=ok bash "$repo_root/scripts/sluice-request.sh" \
    campaign/x "$wip_sha" stage >"$tmp/wipstage.out" 2>"$tmp/wipstage.err"; then
    ok "the same 'wip'-subject commit IS accepted as a kind=stage request"
else
    bad "a kind=stage request was refused for its headline: $(cat "$tmp/wipstage.err")"
fi
if grep -q 'kind=stage' "$tmp/wipstage.out"; then
    ok "the accepted stage request reports kind=stage (so the acceptance above is not a merge in disguise)"
else
    bad "the stage request's success line does not report kind=stage: $(cat "$tmp/wipstage.out")"
fi

echo "== request: MUTATION — deleting the headline check would flip the wip-refusal test red"
mutant2="$repo_root/scripts/.sluice-request-headline-mutant-for-test.sh"
# The range is anchored on the block's opening COMMENT and its closing `fi`.
# It used to be anchored on `^headline="$(git -C` through the next `^esac$`,
# which stopped matching the moment Task 12 wrapped the whole check in
# `if [ "$kind" = "merge" ]` and indented it — and the failure mode was not a
# `bad` verdict but an abort, because the `chmod` below ran unconditionally
# on a file the `else` branch had never written. Both are fixed: the anchor
# tracks the block's real shape, and every step after it is inside the
# `else`.
# shellcheck disable=SC2016  # single-quoted on purpose: a literal sed address, not a shell expansion
if sed '/^# A `stage` REQUEST IS EXEMPT/,/^fi$/d' \
        "$repo_root/scripts/sluice-request.sh" > "$mutant2" \
   && ! grep -q 'not a real headline' "$mutant2" \
   && grep -q 'sluice-queue.sh add' "$mutant2"; then
    chmod +x "$mutant2"
    if PATH="$tmp/bin:$PATH" FAKE_SSH_RESULT=ok bash "$mutant2" campaign/x "$wip_sha" \
        >"$tmp/wipmutant.out" 2>"$tmp/wipmutant.err"; then
        ok "MUTATION CONFIRMED: without the headline check, a 'wip'-subject submission is accepted (the real script refuses it — see above)"
    else
        bad "the headline-check mutant unexpectedly still refused — mutation did not take: $(cat "$tmp/wipmutant.err")"
    fi
else
    bad "could not cleanly excise the headline-check block from sluice-request.sh — it may have changed shape, and this mutation is now proving nothing"
fi
rm -f "$mutant2"

env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/wip 2>/dev/null || true
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/empty 2>/dev/null || true
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test/good 2>/dev/null || true

echo "== make sluice-status / sluice-log: MUST reach the canonical box over ssh, never read local state directly"
# Fix round 1, Critical. The first cut of these two Makefile targets read
# \$HV_SLUICE_DIR (or its default) LOCALLY — but sluice-request.sh enqueues
# onto the CANONICAL box over ssh, and sluice-run.sh runs there too, so from
# any other machine the old targets silently reported an empty queue / "no
# such job" regardless of the real state. Proven here by CONTENT, not merely
# "ssh was invoked": a fake `ssh` on PATH runs the Makefile's own remote
# command string, but against a DELIBERATELY DIFFERENT \$HV_SLUICE_DIR than
# this test process's own environment carries — standing in for a real
# remote host's own separate \$HOME. A target that (bug) reads its local
# environment directly shows the LOCAL sentinel; a target that (fix) only
# ever expands \$HV_SLUICE_DIR inside the single-quoted ssh payload shows the
# REMOTE one, because the plain local value never reaches that payload at
# all — make's own \$$ escaping keeps it literal until the far shell (here,
# faked) expands it.
mkdir -p "$tmp/bin" "$tmp/local-state" "$tmp/remote-state"
cat > "$tmp/bin/ssh" <<'FAKESSH'
#!/usr/bin/env bash
# args: $1=host $2=remote command string, exactly as the Makefile's
# sluice-status/sluice-log targets (and lane-status/lane-log before them)
# build it. Runs it against $FAKE_REMOTE_STATE_DIR instead of whatever this
# process's own environment carries, standing in for a real remote host's
# own, separate $HOME.
shift
env HV_SLUICE_DIR="$FAKE_REMOTE_STATE_DIR" bash -c "$1"
FAKESSH
chmod +x "$tmp/bin/ssh"

printf '2026-01-01T00:00:00Z\treq-local\tcampaign/x\tdeadbeef\tqueued\tLOCAL-SENTINEL-SHOULD-NOT-APPEAR\n' \
    > "$tmp/local-state/queue.tsv"
printf '2026-01-01T00:00:00Z\treq-remote\tcampaign/x\tdeadbeef\tqueued\tREMOTE-SENTINEL-EXPECTED\n' \
    > "$tmp/remote-state/queue.tsv"

status_out="$(cd "$repo_root" && PATH="$tmp/bin:$PATH" HV_SLUICE_DIR="$tmp/local-state" \
    FAKE_REMOTE_STATE_DIR="$tmp/remote-state" make --no-print-directory sluice-status 2>&1)"
if printf '%s' "$status_out" | grep -q 'REMOTE-SENTINEL-EXPECTED'; then
    ok "sluice-status shows the canonical box's own queue (reached over ssh)"
else
    bad "sluice-status did not show the remote sentinel — got: $status_out"
fi
if printf '%s' "$status_out" | grep -q 'LOCAL-SENTINEL-SHOULD-NOT-APPEAR'; then
    bad "sluice-status leaked the CALLER's own local state instead of the canonical box's"
else
    ok "sluice-status did not read the caller's own local queue file"
fi

echo "LOCAL LOG CONTENT — SHOULD NOT APPEAR" > "$tmp/local-state/some-job.log"
echo "REMOTE LOG CONTENT — EXPECTED" > "$tmp/remote-state/some-job.log"
log_out="$(cd "$repo_root" && PATH="$tmp/bin:$PATH" HV_SLUICE_DIR="$tmp/local-state" \
    FAKE_REMOTE_STATE_DIR="$tmp/remote-state" make --no-print-directory sluice-log JOB=some-job 2>&1)"
if printf '%s' "$log_out" | grep -q 'REMOTE LOG CONTENT'; then
    ok "sluice-log shows the canonical box's own job log (reached over ssh)"
else
    bad "sluice-log did not show the remote sentinel — got: $log_out"
fi
if printf '%s' "$log_out" | grep -q 'LOCAL LOG CONTENT'; then
    bad "sluice-log leaked the CALLER's own local log file instead of the canonical box's"
else
    ok "sluice-log did not read the caller's own local log file"
fi

echo "== make sluice-status: MUTATION — a local-only (pre-fix) target would show the local sentinel, reddening the test above"
# Same "reproduce the exact deleted bug" discipline as the sluice-request.sh
# mutations above: a standalone one-target Makefile carrying the ORIGINAL
# (buggy) recipe, run with the SAME environment the fixed-target test above
# used. If this shows the local sentinel where the real target does not,
# the assertions above are proven non-vacuous.
mutant_mk="$tmp/sluice-status-mutant.mk"
cat > "$mutant_mk" <<'MUTANTMK'
sluice-status:
	@bash scripts/sluice-queue.sh list | column -t -s "$$(printf '\t')" || true
MUTANTMK
mutant_status_out="$(cd "$repo_root" && HV_SLUICE_DIR="$tmp/local-state" \
    make --no-print-directory -f "$mutant_mk" sluice-status 2>&1)"
if printf '%s' "$mutant_status_out" | grep -q 'LOCAL-SENTINEL-SHOULD-NOT-APPEAR'; then
    ok "MUTATION CONFIRMED: the pre-fix (local-only) sluice-status recipe leaks the caller's own local queue (the real target does not — see above)"
else
    bad "the pre-fix sluice-status mutant unexpectedly did not show the local sentinel — mutation did not take"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
