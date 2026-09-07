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

# BUILD tools/sluice's release binary ONCE, HERE — after the flock skip guard
# above (so a host with no flock, which exits before this line, never pays
# for a build it will never use), and before the very first test that
# forwards to it (the FIFO/ancestry-coalescing block a little below). This
# script's own claim/set-state/list commands go through
# scripts/sluice-queue.sh's forwarding block, which deliberately never builds
# the binary itself (that is the fix for an earlier Critical: a shim that
# silently rebuilds on every call). In a warm worktree this went unnoticed —
# `tools/sluice/target/release/sluice` was already on disk from earlier work
# — but a fresh chamber worktree has no such binary, and every forwarded-verb
# test between here and the chamber setup below failed with a
# build-instruction error disguised as three ancestry-coalescing assertion
# failures (rc=11 at the sluice chamber's `outboard` phase, 2026-09-05).
# Building unconditionally here means every test in this file, not just the
# chamber section, exercises the binary the production path actually uses.
#
# `cd` into $repo_root for the build itself (in a subshell, so this file's
# own cwd is untouched): rustup's toolchain override is resolved from the
# process's CWD, not from `--manifest-path`, so a build invoked from
# elsewhere can silently pick a different (older, non-overridden) default
# toolchain and fail to parse an edition2024 manifest. Bit this campaign
# twice already; see the matching comment at the chamber's HV_SLUICE_BIN
# export below, which does NOT rebuild — it only re-points the binary path
# for scratch repos holding a copy of sluice-queue.sh with no tools/sluice
# sibling beside them.
#
# A build failure must fail loudly HERE, not 1100 lines later as confusing
# assertion failures about ancestry — `set -euo pipefail` (above) already
# makes that so: this line is not itself guarded, so a nonzero cargo exit
# aborts the whole script.
( cd "$repo_root" && cargo build --quiet --release --manifest-path tools/sluice/Cargo.toml >&2 )

pass=0; fail=0
ok()   { printf '  ok: %s\n' "$1"; pass=$((pass+1)); }
bad()  { printf '  FAIL: %s\n' "$1"; fail=$((fail+1)); }
g()    { env -u GIT_DIR -u GIT_INDEX_FILE git "$@"; }

# THE REAL REPO'S HEAD, RECORDED BEFORE ANY TEST RUNS. The last assertion in
# this file proves it did not move. On 2026-09-04 a test in this suite pointed
# HV_SLUICE_REPO_ROOT at the real repo; sluice-run.sh then detached and reset
# that checkout, and because it happened inside the chamber it silently threw
# away a merge product — the merge reported rc=0 and LANDED while main received
# only the pre-merge tree. Nothing here noticed, because every assertion was
# about the queue and none was about the blast radius.
# HV_TEST_FAKE_START_HEAD is a seam for proving this guard is LIVE. Verifying
# it the direct way — re-introducing the override and watching HEAD move —
# means running a chamber against this very checkout, which is the incident
# itself; that is not a thing to do to a live worktree to test a test. The
# seam proves the COMPARISON fires. That an override actually moves HEAD is
# established by the 2026-09-04 incident, not by re-staging it.
real_head_at_start="${HV_TEST_FAKE_START_HEAD:-$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)" rev-parse HEAD 2>/dev/null || echo unknown)}"

tmp="$(mktemp -d)"
# The request-path mutation tests below have to write their mutants INSIDE
# scripts/ (see their own comments for why $tmp cannot host them), so the
# EXIT trap sweeps those paths too — belt-and-suspenders alongside the
# `rm -f`/`update-ref -d` immediately after each test uses them, in case
# `set -e` aborts the script somewhere in between. The `refs/remotes/
# sluice-test-$$/*` refs are throwaway local refs the headline-refusal test
# creates with `git commit-tree` + `update-ref` (never a real remote, never
# pushed) so a real commit object exists to test the headline check against;
# swept here too for the same belt-and-suspenders reason.
#
# THE `$$` IS LOAD-BEARING AND WAS ADDED AFTER THIS SUITE REDDENED A CAMPAIGN.
# The refs must live in the REAL repository, because `git branch -r --contains`
# is what sluice-request.sh checks and only the real ref store answers it. But
# `refs/remotes/*` is SHARED ACROSS LINKED WORKTREES, and the chamber's
# worktree (/home/nathan/Projects/hornvale-sluice-wt) is a linked worktree of
# this repository rather than a clone. So two concurrent runs of this file used
# ONE ref, and whichever finished first deleted it: the survivor's next
# sluice-request call reported "<sha> is not on any remote branch — push first"
# and five assertions failed on a tree with nothing wrong with it.
#
# That happened on 2026-09-05. campaign/the-lot went red at `outboard` rc=11
# after 420 s — 241 passed, 5 failed — because the OPERATOR was running this
# same file in another worktree while the chamber ran it, and three of those
# local runs' EXIT traps fired inside the chamber's window. The campaign was
# billed a serial-box slot for a collision in the gate, and the first reading
# of it was "flaky test", which it is not: it is deterministic given overlap.
# Naming the refs per-process removes the shared object entirely.
trap 'rm -rf "$tmp"; \
      rm -f "$repo_root/scripts/.sluice-request-mutant-for-test.sh" \
            "$repo_root/scripts/.sluice-request-headline-mutant-for-test.sh"; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/wip 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/empty 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/good 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/trailer 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/nudge 2>/dev/null || true; \
      env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/stranded 2>/dev/null || true; \
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

# AND THE SERVER-SIDE HEADLINE REFUSAL IS OFF FOR THE WHOLE FILE, because
# nearly every `add` below enqueues a kind=merge row in a scratch repo whose
# commits carry no trailers — the suite is exercising FIFO, coalescing, states
# and the mouth, not headlines. Left on, all of it would be refused.
#
# A knob that disables enforcement is only safe if nothing in production can
# set it, so that is asserted rather than assumed: the section
# "request: the headline knob is not reachable from the production path"
# below greps sluice-request.sh's remote command for it. The sections that
# test the check itself unset this deliberately, per invocation.
export HV_SLUICE_SKIP_HEADLINE=1

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

echo "== queue: the SERVER-SIDE headline refusal — the one that actually enforces"
# `sluice-request.sh` checks the same thing on the SUBMITTER's machine, from
# the submitter's checkout, so a campaign whose scripts/ predates the trailer
# rule silently gets the old subject-based check. That happened on the rule's
# first outside submission. `add` runs on the canonical box, which every
# submission passes through whatever the caller is running, so the enforcing
# copy lives there. These call it with the suite-wide skip UNSET.
# RUNS FROM $repo_root, in a subshell, and that is not incidental.
# `sluice-queue.sh` resolves the candidate against its CWD — the same way its
# coalescing `git merge-base` already does — because in production
# `sluice-request.sh` ssh's `cd <repo> && scripts/sluice-queue.sh add`, so cwd
# IS the repository. This suite `cd`s into the scratch repo early and stays
# there, so calling it from here would ask a repo that has never heard of
# these commits, get "unresolvable", and take the indeterminate branch — which
# ACCEPTS. Every refusal assertion below would then fail for a reason that has
# nothing to do with the check. Found exactly that way.
qadd() { ( cd "$repo_root" && env -u HV_SLUICE_SKIP_HEADLINE HV_SLUICE_BASE=HEAD \
    bash "$repo_root/scripts/sluice-queue.sh" "$@" ); }
hl_tree="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" rev-parse 'HEAD^{tree}')"
hl_none="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$hl_tree" -p HEAD -m "chore: a perfectly ordinary subject with no trailer")"
hl_good="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$hl_tree" -p HEAD -m "chore: tidy up

Sluice-Headline: the thing that actually landed")"

hq="$tmp/hl-queue"
if HV_SLUICE_DIR="$hq" qadd add campaign/x "$hl_none" merge >/dev/null 2>"$tmp/hl1.err"; then
    bad "a kind=merge add with no trailer was ACCEPTED by the server — the client-side check is bypassable, so this is the enforcing one"
else
    ok "a kind=merge add with no trailer is refused by sluice-queue.sh itself"
fi
if [ "$(HV_SLUICE_DIR="$hq" bash "$repo_root/scripts/sluice-queue.sh" list | wc -l)" = "0" ]; then
    ok "a refused add enqueues NOTHING — the queue is not left holding a row it rejected"
else
    bad "a refused add still wrote a row"
fi
if grep -q 'Sluice-Headline' "$tmp/hl1.err"; then
    ok "the server refusal names the trailer and the placement rule"
else
    bad "the server refusal does not name Sluice-Headline"
fi

if HV_SLUICE_DIR="$tmp/hl-ok" qadd add campaign/x "$hl_good" merge >/dev/null 2>"$tmp/hl2.err"; then
    ok "a kind=merge add carrying the trailer is accepted"
else
    bad "a valid trailer was refused by the server: $(cat "$tmp/hl2.err")"
fi
# ANNOUNCING WHICH RULE RAN is what makes a stale client visible: the caller
# reads this over ssh regardless of what its own scripts/ contains.
if grep -q 'headline OK (trailer rule)' "$tmp/hl2.err"; then
    ok "the server announces which rule it applied, so a stale caller can see the trailer rule ran"
else
    bad "the server does not announce the rule — a stale client cannot tell which check it met"
fi

if HV_SLUICE_DIR="$tmp/hl-stage" qadd add campaign/x "$hl_none" stage >/dev/null 2>&1; then
    ok "a kind=stage add with no trailer is still accepted — the exemption survives on the server too"
else
    bad "a stage add was refused for a missing headline; a stage merge commit is discarded and can never be permanent"
fi

# UNANSWERABLE MUST NOT REFUSE. Durability outranks the check: losing a real
# request because the box cannot resolve an object is worse than an unlabelled
# merge. Same three-way discipline as coalescing, and the row is stamped so an
# operator sees which ones went unchecked.
if ( cd "$repo_root" && env -u HV_SLUICE_SKIP_HEADLINE HV_SLUICE_BASE=no-such-ref-here \
    HV_SLUICE_DIR="$tmp/hl-ind" bash "$repo_root/scripts/sluice-queue.sh" \
    add campaign/x "$hl_none" merge >/dev/null 2>&1 ); then
    ok "an UNANSWERABLE headline check accepts rather than refusing — durability outranks the check"
else
    bad "an unresolvable base refused a real request; the queue must not lose a request over a missing object"
fi
case "$(HV_SLUICE_DIR="$tmp/hl-ind" bash "$repo_root/scripts/sluice-queue.sh" list | cut -f7)" in
    *"headline indeterminate"*) ok "the unchecked row is stamped, so it is visible in sluice-status rather than silently unchecked" ;;
    *) bad "an unchecked row carries no stamp — indistinguishable from a checked one" ;;
esac

echo "== queue: MUTATION — without the server check, the trailer-less add is accepted"
qmut="$repo_root/scripts/.sluice-queue-headline-mutant-for-test.sh"
# shellcheck disable=SC2016  # `$kind` and `$HV_SLUICE_SKIP_HEADLINE` must stay
# LITERAL — this is matching the mutant's source text, not evaluating it.
sed -e 's#^    if \[ "\$kind" = "merge" \] && \[ "\${HV_SLUICE_SKIP_HEADLINE:-}" != "1" \]; then#    if false; then#' \
    "$repo_root/scripts/sluice-queue.sh" > "$qmut"
if grep -q 'if false; then' "$qmut"; then
    ok "test setup: the mutant has the server check disabled (so an accept below means the check, not a dead script)"
else
    bad "the mutation sed did not apply — the assertion below would pass vacuously"
fi
if env -u HV_SLUICE_SKIP_HEADLINE HV_SLUICE_BASE=HEAD HV_SLUICE_DIR="$tmp/hl-mut" \
    bash "$qmut" add campaign/x "$hl_none" merge >/dev/null 2>&1; then
    ok "MUTATION CONFIRMED: without the check the trailer-less add is accepted (the real one refuses it — see above)"
else
    bad "the mutant also refused — the tests above are not pinning the server-side check"
fi
rm -f "$qmut"

echo "== request: the headline knob is not reachable from the production path"
# The knob above is only safe if production cannot set it. sluice-request.sh
# builds a FIXED remote command string; if that string ever mentions the knob,
# every real submission would skip the enforcing check and this suite would
# still be green, because the suite sets it for itself.
if grep -q 'HV_SLUICE_SKIP_HEADLINE' "$repo_root/scripts/sluice-request.sh"; then
    bad "sluice-request.sh mentions HV_SLUICE_SKIP_HEADLINE — production submissions could skip the enforcing check"
else
    ok "sluice-request.sh never sets HV_SLUICE_SKIP_HEADLINE, so the test knob cannot leak into production"
fi

echo "== phases: a prose-only candidate skips seam-guard, clients and heavy"
# campaign/the-illumination's docs-recovery merge paid the full ladder for three
# files: 3561 s, of which 3124 s (88%) went to phases that cannot observe a
# prose change. The rule that fixes that must be right in BOTH directions, and
# the dangerous direction is the false positive — skipping heavy for something
# that is not prose. Every negative case below is one of those.
# shellcheck source=scripts/sluice-phases.sh
. "$repo_root/scripts/sluice-phases.sh"

if sluice_is_prose_only "book/src/frontier/idea-registry.md
docs/retrospectives/the-illumination.md
docs/timings.md"; then
    ok "the real docs-recovery path set classifies as prose-only"
else
    bad "the docs-recovery path set was NOT classified prose-only, so the saving never applies"
fi

# `.claude/skills/**` joined the allowlist 2026-09-01. It earns its place by the
# same argument the rest do — no phase AUTHORS anything under it (checked:
# regenerate-artifacts.sh, gate-full-heavy.sh and the clients targets mention it
# zero times) and NO workspace test reads it — so no skipped phase could observe
# such a change. campaign/the-overture paid ~1130 s of extra box time for a
# two-file skill fix before it was added.
if sluice_is_prose_only ".claude/skills/closing-a-campaign/SKILL.md
docs/timings.md"; then
    ok "a skill fix plus its timings row classifies as prose-only"
else
    bad "a .claude/skills change is not prose-only — the allowlist entry is missing or wrong"
fi

# THE NEGATIVE THAT DEFINES THE ENTRY'S EDGE. It is `.claude/skills/**`, not
# `.claude/**`: settings and hook configuration govern how a session behaves and
# are not obviously unobservable to a phase. If this ever passes, someone
# widened the glob and took the argument with it.
if sluice_is_prose_only ".claude/settings.json"; then
    bad ".claude/settings.json classified prose-only — the glob was widened past its argument"
else
    ok ".claude/settings.json is NOT prose-only (the entry is skills-only, deliberately)"
fi

# Fails toward running: one non-prose path anywhere in the range is enough.
if sluice_is_prose_only ".claude/skills/x/SKILL.md
kernel/src/lib.rs"; then
    bad "a skill plus a .rs file classified prose-only — the rule stopped failing toward running"
else
    ok "a skill plus one .rs file takes the full ladder"
fi

for bad_case in "kernel/src/lib.rs" "Cargo.toml" "clients/game/core/tests/fixtures/x.json" \
                "book/src/gallery/atlas.js" "book/src/laboratory/generated/the-sounding/rows.csv" \
                "book/src/reference/concept-registry-generated.md"; do
    if sluice_is_prose_only "docs/a-real-doc.md
$bad_case"; then
        bad "'$bad_case' alongside prose classified as PROSE-ONLY — heavy would be skipped for a change it must see"
    else
        ok "'$bad_case' forces the full ladder even when the rest of the range is prose"
    fi
done

# FAILS TOWARD RUNNING MORE. An empty list means the diff could not be read;
# a classifier that cannot see the change must not be why a phase is skipped.
if sluice_is_prose_only ""; then
    bad "an EMPTY path list classified as prose-only — a failed diff would skip heavy"
else
    ok "an empty path list is not prose-only, so a failed diff keeps every phase"
fi

case "$(sluice_drop_expensive_phases 'artifacts outboard gate seam-guard clients heavy')" in
    "artifacts outboard gate") ok "dropping leaves exactly artifacts, outboard and gate, in order" ;;
    *) bad "unexpected phase list after dropping: '$(sluice_drop_expensive_phases 'artifacts outboard gate seam-guard clients heavy')'" ;;
esac

echo "== ack: adjudicating an out-of-band landing"
# The mouth returns 4 and says a human must decide; until scripts/sluice-ack.sh
# existed there was no way to RECORD that decision, so the operator hand-wrote
# last-pushed on 2026-08-19 and the act survived only in a queue note.
ack_dir="$tmp/ackdir"; mkdir -p "$ack_dir"
ack() { HV_SLUICE_DIR="$ack_dir" bash "$repo_root/scripts/sluice-ack.sh" "$@"; }
if ack >/dev/null 2>&1; then
    bad "sluice-ack accepted an empty reason — an unexplained adjudication is the broken guarantee wearing a hat"
else
    ok "sluice-ack refuses without a reason"
fi
if ack "no baseline yet" >/dev/null 2>&1; then
    bad "sluice-ack adjudicated with no baseline recorded — it would invent one"
else
    ok "sluice-ack refuses when no baseline exists"
fi
g -C "$repo_root" rev-parse origin/main > "$ack_dir/last-pushed" 2>/dev/null || echo dummy > "$ack_dir/last-pushed"
before_ack="$(md5sum "$ack_dir/last-pushed" | cut -d' ' -f1)"
if ack "nothing moved" >/dev/null 2>&1; then
    bad "sluice-ack 'adjudicated' when the baseline already matched — a no-op reported as success is how state drifts"
else
    ok "sluice-ack refuses when there is nothing to adjudicate"
fi
if [ "$before_ack" = "$(md5sum "$ack_dir/last-pushed" | cut -d' ' -f1)" ]; then
    ok "the refused adjudication left the baseline byte-identical"
else
    bad "the refused adjudication rewrote the baseline"
fi

echo "== phases: what the chamber ACTUALLY runs (decisions 0148, 0426)"
# THE CASE ABOVE TESTS THE FUNCTION AGAINST A LITERAL, WHICH IS NOT THE SAME AS
# TESTING THE CHAMBER. Nothing asserted what sluice-run.sh actually sets, so the
# phase list could change without a single test noticing — the exact
# reports-on-a-set-it-does-not-own shape this suite keeps finding elsewhere.
# Read from the script rather than restated here, so this cannot drift from it.
merge_list="$(sed -n 's/^merge_phases="\(.*\)"$/\1/p' "$repo_root/scripts/sluice-run.sh")"
stage_list="$(sed -n 's/^stage_phases="\(.*\)"$/\1/p' "$repo_root/scripts/sluice-run.sh")"
if [ "$merge_list" = "artifacts outboard gate clients heavy" ]; then
    ok "a merge runs exactly: $merge_list"
else
    bad "merge_phases is '$merge_list' — decision 0426 fixed it at 'artifacts outboard gate clients heavy'"
fi
# `heavy` LAST is load-bearing, not cosmetic: it is the most expensive phase,
# so running it before a cheap phase that would have gone red wastes the one
# serial box. Asserted separately from the equality above so a reorder that
# still contains the right five names cannot pass as "the list is right".
case "$merge_list" in
    *heavy) ok "heavy runs last" ;;
    *)      bad "heavy is not the last phase of a merge — see sluice-run.sh's own ordering comment" ;;
esac
# seam-guard stays off (0148, undisturbed by 0426); heavy came BACK on (0426).
case "$merge_list" in
    *seam-guard*) bad "seam-guard is back in the merge list; 0148 took it off and 0426 deliberately did not put it back" ;;
    *heavy*)      ok "heavy runs on a merge (decision 0426) and seam-guard does not" ;;
    *)            bad "heavy is missing from the merge list; decision 0426 put it back after The Governor cut the tier 3.52x" ;;
esac
# THE TWO LISTS DIVERGE BY EXACTLY `heavy`, AND THAT IS ASSERTED RATHER THAN
# ALLOWED. 0148 made them identical; 0426 put `heavy` back on the merge list
# ONLY, restoring the pre-0148 arrangement (at 3163ceb2c^ the stage list was
# already `artifacts outboard gate clients` — `heavy` has never been a
# stage-gate phase). The reason is specific: heavy's
# `census_fixtures_match_a_probe_of_live_seeds` compares a live probe against
# committed census fixtures that are refreshed once per campaign at pre-merge
# close, so on a stage gate it would red predictably and benignly for the
# whole middle of any world-touching campaign.
# Checked as "stage plus heavy equals merge" rather than as two literals, so a
# future change to the shared four is not required to touch this line twice.
if [ "$stage_list heavy" = "$merge_list" ]; then
    ok "a stage gate runs the merge's phases minus heavy: $stage_list"
elif [ "$merge_list" = "$stage_list" ]; then
    bad "merge and stage are identical ('$merge_list') — 0426 puts heavy on the merge list ONLY; if a stage gate should run it, the census-fixture red argued in sluice-run.sh must be answered first"
else
    bad "merge ('$merge_list') and stage ('$stage_list') differ by something other than a trailing 'heavy'; if that is deliberate, update this test and say why"
fi

echo "== phases: MUTATION — an allowlist without its exclusions would skip heavy for a generated artifact"
# Non-vacuity: widen the allowlist to bare `book/*` — the obvious, wrong
# version of this rule — and confirm a heavy-authored artifact then passes as
# prose. If it does not, the negative cases above are not pinning the
# exclusions.
mut_prose_only() {
    local changed="$1" pth
    [ -n "$changed" ] || return 1
    while IFS= read -r pth; do
        [ -n "$pth" ] || continue
        case "$pth" in docs/*|book/*) ;; *) return 1 ;; esac
    done <<MEOF
$changed
MEOF
    return 0
}
if mut_prose_only "book/src/laboratory/generated/the-sounding/rows.csv"; then
    ok "MUTATION CONFIRMED: a bare book/* allowlist accepts a heavy-authored artifact (the real one refuses it)"
else
    bad "the mutant also refused — the exclusion cases above are not pinning anything"
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

# set-state: id alone. THE INJECTION ARGUMENT HERE IS UNCHANGED AND STILL
# RIGHT: there is no dedicated id validator and none is needed, because an id
# is only ever COMPARED against, never freshly written into a new row — which
# is why a tab- or newline-bearing id cannot deform the queue, and why
# assert_shape_intact is the assertion that matters on these two lines.
#
# WHAT CHANGED IS THE EXIT STATUS, AND ONLY BECAUSE THE OLD ONE COST SOMETHING
# (2026-08-19). These previously expected ACCEPTANCE — "a garbage id just
# matches nothing, so this is a no-op SUCCESS". True of an ATTACKER's id, whose
# author does not care whether it worked. False of an OPERATOR's id: a mistyped
# id reported success for work it had not done, the finished run's row stayed
# `running`, and coalescing then refused to supersede it (running rows never
# are), so a resubmission queued behind a ghost. The injection reasoning did not
# consider operator error, and did not need to; both conclusions hold at once —
# no validator, AND a no-match refuses.
expect_reject "set-state: tab-bearing id (no match, refused)"     set-state "$(printf 'id\twith\ttabs')" queued
assert_shape_intact "set-state: tab-bearing id"     "$rows_now"
expect_reject "set-state: newline-bearing id (no match, refused)" set-state "$(printf 'id\nwith\nnewlines')" queued
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

# 5. AN UNKNOWN ID IS A REFUSAL, NOT A SILENT SUCCESS. This is the defect that
#    manufactures a GHOST: set-state rewrote every row unchanged and exited 0
#    when the id matched nothing, so a mistyped id reported success for work it
#    had not done. The row stayed `running` after its chamber exited, and
#    coalescing then refused to supersede it — running rows never are — so the
#    campaign's resubmission queued BEHIND a job that had already finished.
#    Observed live 2026-08-19: `…T173353Z` typed for `…T163353Z`, one digit.
before_ghost="$(md5sum "$HV_SLUICE_DIR/queue.tsv" | cut -d' ' -f1)"
if bash "$repo_root/scripts/sluice-queue.sh" set-state "req-no-such-id" held "x" >/dev/null 2>&1; then
    bad "set-state ACCEPTED an unknown id — a typo silently leaves a ghost row running"
else
    ok "set-state refuses an id that matches no row"
fi
after_ghost="$(md5sum "$HV_SLUICE_DIR/queue.tsv" | cut -d' ' -f1)"
if [ "$before_ghost" = "$after_ghost" ]; then
    ok "the refused set-state left the queue byte-identical — nothing half-applied"
else
    bad "the refused set-state MUTATED the queue"
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

# The kill above was interrupted BEFORE the `mv`, so $HV_SLUICE_DIR/queue.tsv
# still holds the 3000 synthetic `queued` rows this section wrote to force a
# slow coalescing scan — already "destructive to the queue file" per the
# comment above, so no test before this point could have depended on their
# survival. It used to be genuinely harmless debris: nothing downstream ever
# READ the queue file's rows again, only its own tmp-file hygiene. That
# stopped being true the moment sluice-mouth.sh grew a cross-candidate
# overlap check (it now runs `sluice-queue.sh list` on every admit, and
# sluice-run.sh asks the mouth before every chamber phase) — a leftover
# 3000-row queue turns every later mouth call in this file into 3000
# `git cat-file`/`merge-tree` subprocesses against a $KSHA these later
# sections' repos cannot resolve, slow enough to blow the chamber tests'
# `poll_for_file` timeouts on the claim file. Reset it to empty so later
# sections see the small, real queue they always assumed.
: > "$HV_SLUICE_DIR/queue.tsv"

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

# A SECOND scratch bare repo standing in for the tangled mirror. Same rule as
# origin above: under $tmp, never a real remote. HV_SLUICE_MIRROR_REMOTE names
# it so production's default ("tangled") is never contacted by a test.
chamber_mirror="$tmp/chamber-mirror.git"
git init -q --bare -b main "$chamber_mirror"
g -C "$chamber_repo" remote add mirror-under-test "$chamber_mirror"
g -C "$chamber_repo" push -q mirror-under-test main
export HV_SLUICE_MIRROR_REMOTE=mirror-under-test

# The chamber sources scripts/census-canonical-host.sh and invokes
# scripts/timed.sh from `$repo_root/scripts/`, both resolved relative to
# HV_SLUICE_REPO_ROOT — so the scratch repo needs its own copies. This is
# the one place a chamber test depends on real repo content; both files are
# already read-only inputs to every other lane test in this repo.
mkdir -p "$chamber_repo/scripts"
cp "$repo_root/scripts/census-canonical-host.sh" "$chamber_repo/scripts/census-canonical-host.sh"
cp "$repo_root/scripts/timed.sh" "$chamber_repo/scripts/timed.sh"
cp "$repo_root/scripts/sluice-queue.sh" "$chamber_repo/scripts/sluice-queue.sh"
# EVERY HELPER sluice-run.sh SOURCES MUST BE ON THIS LIST, and the list has no
# way to know that. Adding `sluice-headline.sh` without this line failed the
# whole chamber section at once — `set -e` plus a missing `.` source, so the
# run exited 1 before its first phase and four downstream assertions read as
# unrelated failures (out-of-order phases, an empty last-pushed). If you add a
# `. "$repo_root/scripts/…"` to sluice-run.sh, add its copy here.
cp "$repo_root/scripts/sluice-headline.sh" "$chamber_repo/scripts/sluice-headline.sh"
cp "$repo_root/scripts/sluice-phases.sh" "$chamber_repo/scripts/sluice-phases.sh"

chamber_host_file="$tmp/chamber-host.txt"
printf '%s\n' "$(hostname -s)" > "$chamber_host_file"

# $1 = branch name, $2 = filename to add. Each topic branch touches its own
# new file so every merge in every scenario below is trivially conflict-free.
# THIS COMMENT USED TO SAY conflict handling is sluice-mouth.sh's job "not the
# chamber's; the chamber assumes admission already happened". That stopped
# being true when sluice-run.sh grew its pre-lock mouth check: the chamber now
# ASKS the mouth before taking the box. The scenarios below are still
# conflict-free, and they still exercise the chamber rather than admission —
# but they no longer do so because the chamber is blind to conflicts. The
# mouth-check section at the end of this file covers that seam directly.
#
# Note also that sluice-run.sh EXECUTES the mouth rather than sourcing it, and
# resolves it as a sibling of its own file (BASH_SOURCE) rather than under
# $repo_root — so it does NOT need a copy in the "every helper must be on this
# list" block above. The first cut resolved it under $repo_root, found nothing
# in this scratch repo, and refused every chamber run in this file.
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
# HV_SLUICE_BIN is the test seam scripts/sluice-queue.sh's forwarding block
# reads (test seam ONLY — nothing in production sets it). The binary itself
# was already built once, near the top of this file (right after the flock
# skip guard) — that early build serves every test before this point,
# exercising the DEFAULT sibling-path resolution the production path
# actually uses. This second export does NOT rebuild anything; it only
# re-points the resolved path for $chamber_repo below, which gets a COPY of
# sluice-queue.sh with no tools/sluice sibling beside it (see the two
# `cp .../sluice-queue.sh` sites in this file) and so has no manifest of its
# own to build claim/set-state/list against. Both cp sites are covered
# because both are invoked (via HV_SLUICE_REPO_ROOT) only after this point.
export HV_SLUICE_BIN="$repo_root/tools/sluice/target/release/sluice"

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

# THE MIRROR. Nathan added a second remote and asked main be pushed there too.
# Two properties matter and they pull in opposite directions: it must actually
# mirror, and it must NEVER be able to fail a merge that already landed.
# ANTI-VACUITY FIRST. The scratch mirror is seeded with main at setup, so a
# bare "does the mirror have this SHA" check can pass without the mirror code
# ever running — it would pass with the feature deleted. Require the run to
# SAY it mirrored before believing the ref.
# shellcheck disable=SC2012  # these names are generated by sluice-run.sh as
# $job_id.log, i.e. req-<40-hex>-<UTC stamp>.log — no spaces or globs possible,
# and `ls -t` is the concise way to take the newest.
run1_log="$(ls -t "$HV_SLUICE_DIR"/sluice-*.log 2>/dev/null | head -1)"
if grep -q "mirrored .* to " "${run1_log:-/dev/null}" 2>/dev/null; then
    ok "mirror: the run reports mirroring, so the assertion below is not vacuous"
    mirror1_main_sha="$(g -C "$chamber_repo" ls-remote "$chamber_mirror" refs/heads/main 2>/dev/null | cut -f1)"
    if [ "$mirror1_main_sha" = "$wt1_final_sha" ]; then
        ok "mirror: the second remote's main advanced to the same SHA origin got"
    else
        bad "mirror: remote main is '$mirror1_main_sha', expected '$wt1_final_sha'"
    fi
else
    bad "mirror: run1 never reported a mirror push, so this chamber run does not reach the push step and cannot test the mirror at all"
fi

echo "== chamber: the phase-selection block RUNS when HV_SLUICE_PHASES is unset"
# THE TEST THAT WOULD HAVE CAUGHT THE REGRESSION, and its absence is the whole
# lesson. The prose-only skip is guarded by `[ -z "${HV_SLUICE_PHASES:-}" ]`,
# and EVERY chamber case in this file exports HV_SLUICE_PHASES to keep phases
# cheap — so not one of them ever entered the block. The helper's own unit
# tests were green, `bash -n` parsed, shellcheck was clean, and the chamber
# still died on its first real merge with
#
#   scripts/sluice-run.sh: line 358: base_sha: unbound variable
#
# because the block read a variable assigned ~90 lines below it, and `set -u`
# is fatal. A unit-tested helper wired in wrong is indistinguishable from an
# untested one at the point where it matters.
#
# So: drive the real script with HV_SLUICE_PHASES UNSET and assert it gets
# PAST the decision. It will fail later — a scratch repo has no `make
# gate-suite-run` — and that is fine; the assertion is about the block, and
# specifically that no unbound variable kills the run before any phase starts.
# sluice-run.sh writes its own run log to $HV_SLUICE_DIR/<job>.log rather than
# stdout — verified: a real chamber invocation left its caller's redirect file
# at 0 bytes while the run log had the whole run. So point HV_SLUICE_DIR at a
# scratch dir and read the log the script actually writes.
unset_dir="$tmp/unsetphases-state"; mkdir -p "$unset_dir"
( unset HV_SLUICE_PHASES
  HV_SLUICE_DIR="$unset_dir" timeout 90 bash "$repo_root/scripts/sluice-run.sh" \
    campaign/x "$(g -C "$chamber_repo" rev-parse HEAD)" stage >/dev/null 2>&1 ) || true
cat "$unset_dir"/*.log > "$tmp/unsetphases.out" 2>/dev/null || true
# NON-VACUITY FIRST. This assertion's own first draft sat ABOVE the chamber
# setup, so $chamber_repo was unbound, the subshell died, `|| true` swallowed
# it, and grep found nothing in an EMPTY file — which the check below reads as
# success. The test for an unbound-variable bug was itself vacuous because of
# an unbound variable. Assert the run produced output before believing what is
# not in it.
if [ ! -s "$tmp/unsetphases.out" ]; then
    bad "the chamber produced NO output with HV_SLUICE_PHASES unset — this assertion would pass vacuously"
elif grep -q 'unbound variable' "$tmp/unsetphases.out"; then
    bad "the chamber died on an unbound variable with HV_SLUICE_PHASES unset: $(grep 'unbound variable' "$tmp/unsetphases.out" | head -1)"
else
    ok "the phase-selection block runs to completion with HV_SLUICE_PHASES unset (no unbound variable)"
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

# THE MIRROR MUST NOT BE ABLE TO FAIL A MERGE THAT LANDED. Point it at a
# remote that cannot exist for THIS run only. By the time the mirror push
# runs, main has already moved on origin and last-pushed is written; a
# non-zero exit here would report a landed merge as a failure and send
# somebody hunting a candidate that was fine. Same class as the vanished
# tempfile that turned a green `artifacts` phase into rc=11 on 2026-09-06.
mirror_before_break="$HV_SLUICE_MIRROR_REMOTE"
g -C "$chamber_repo" remote add broken-mirror "$tmp/no-such-remote.git" 2>/dev/null || true
export HV_SLUICE_MIRROR_REMOTE=broken-mirror

set +e
bash "$repo_root/scripts/sluice-run.sh" campaign/t2 "$sha2" > "$tmp/run2.out" 2>&1
rc2=$?
set -e
export HV_SLUICE_MIRROR_REMOTE="$mirror_before_break"


if [ "$rc2" -eq 0 ]; then
    ok "the authoring-phase run exits 0"
else
    bad "expected rc 0, got $rc2 ($(cat "$tmp/run2.out"))"
fi

# Placed AFTER the rc2 diagnostic on purpose: an earlier draft asserted on
# $wt2 before that check, so a failed run died on a missing directory instead
# of reporting why it failed. A test's own failure path must not outrank the
# diagnostic it was added next to.
origin2_after="$(g -C "$chamber_repo" ls-remote "$chamber_origin" refs/heads/main 2>/dev/null | cut -f1)"
wt2_final="$(g -C "$wt2" rev-parse HEAD 2>/dev/null || echo "")"
# shellcheck disable=SC2012  # see the note at run1_log above.
run2_log="$(ls -t "$HV_SLUICE_DIR"/sluice-*.log 2>/dev/null | head -1)"
if grep -q "could not mirror" "${run2_log:-/dev/null}" 2>/dev/null; then
    if [ -n "$wt2_final" ] && [ "$origin2_after" = "$wt2_final" ]; then
        ok "mirror: THE PROPERTY — an unreachable mirror did not stop main landing on origin"
    else
        bad "mirror: origin main '$origin2_after' vs worktree '$wt2_final' — the broken mirror blocked the real push"
    fi
else
    bad "mirror: run2 never attempted a mirror push, so the unreachable-mirror property is untested here"
fi
if grep -q "MAIN IS LANDED ON origin" "${run2_log:-/dev/null}" 2>/dev/null; then
    ok "mirror: the warning states main IS landed, so it cannot read as a failed merge"
else
    bad "mirror: no reassurance in the warning: $(grep -i mirror "${run2_log:-/dev/null}" 2>/dev/null | head -2)"
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
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test-$$/nudge "$pushed_ref"

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
# `git commit-tree -m ""` genuinely succeeds, so the truly headline-LESS case
# — not merely a junk one — is a real commit object here too, not a string the
# test merely hands the script.
#
# `</dev/null` IS LOAD-BEARING AND ITS ABSENCE MADE THIS SUITE HANG. An
# earlier note here recorded this as "verified: git only refuses an empty
# message from the interactive `git commit` editor path, not from
# commit-tree". That verified the wrong property. Git does not REFUSE the
# empty message — it treats `-m ""` as no message supplied and falls back to
# reading one from STDIN, so with stdin inherited this call blocks forever.
# Measured on this exact command: `</dev/null` exits 0 instantly, stdin
# inherited exits 124 under `timeout 10`.
#
# It is stdin-dependent, which is why it presented as flaky rather than
# broken: the same code passed in 18.92 s under one invocation and hung past
# 900 s under another, with the only difference being whether stdin happened
# to be at EOF. A hang has no output, so it reads as "the suite is slow now"
# and gets blamed on whatever was most recently added.
empty_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD -m "" </dev/null)"
good_sha="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" commit-tree "$headline_test_tree" -p HEAD -m "feat(sluice): a real headline for testing")"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test-$$/wip "$wip_sha"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test-$$/empty "$empty_sha"
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test-$$/good "$good_sha"

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
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test-$$/trailer "$trailer_sha"
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
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref refs/remotes/sluice-test-$$/stranded "$stranded_sha"
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

env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/wip 2>/dev/null || true
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/empty 2>/dev/null || true
env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" update-ref -d refs/remotes/sluice-test-$$/good 2>/dev/null || true

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

echo "== chamber: the mouth is asked BEFORE the box is taken =="
# The defect this pins, observed 2026-08-23: a candidate that could not merge
# still took the staff, built a worktree, failed at the <merge> step and
# exited 10. The mouth could have said so for free, and nothing asked it —
# sluice-run.sh did not reference sluice-mouth.sh at all.
#
# A CONFLICTING candidate needs both sides to touch the SAME path, which
# new_topic_branch() deliberately never does; build one by hand.
# Earlier chamber scenarios have already advanced origin/main by pushing, so
# the local `main` here is behind it. Re-point at the remote before building a
# conflict on top, or the fixture's own push is rejected non-fast-forward and
# every assertion below reports on a fixture that was never created.
# Earlier phases leave docs/timings.md dirty (scripts/timed.sh appends a row on
# every phase), which blocks a checkout. This is a throwaway fixture repo under
# $tmp — never this repository — so resetting it is safe and is not the
# restore-from-a-copy case.
# NOT `clean -fd`: the helper scripts this chamber needs (census-canonical-host.sh,
# timed.sh, sluice-headline.sh, sluice-phases.sh) are copied in UNTRACKED, so a
# clean deletes them and every later scenario dies on a missing source file.
# reset --hard alone handles the tracked drift, which is all there is.
g -C "$chamber_repo" reset -q --hard
g -C "$chamber_repo" fetch -q origin
g -C "$chamber_repo" checkout -q -B main origin/main
g -C "$chamber_repo" checkout -q -B campaign/tconf main
printf 'branch side\n' > "$chamber_repo/tracked.txt"
g -C "$chamber_repo" commit -qam "conflict: branch side"
conf_sha="$(g -C "$chamber_repo" rev-parse campaign/tconf)"
g -C "$chamber_repo" push -q origin campaign/tconf
g -C "$chamber_repo" checkout -q main
printf 'main side\n' > "$chamber_repo/tracked.txt"
g -C "$chamber_repo" commit -qam "conflict: main side"
g -C "$chamber_repo" push -q origin main
g -C "$chamber_repo" fetch -q origin

# Our own fixture push moved origin/main, so `last-pushed` now disagrees with
# it and the mouth reports an OUT-OF-BAND LANDING (exit 4) — correctly, and
# BEFORE it ever reaches the conflict check, since 4 is tested ahead of 1.
# Re-point it so the verdict under test here is the conflict, not the drift.
g -C "$chamber_repo" rev-parse origin/main > "$HV_SLUICE_DIR/last-pushed"

# sluice-run.sh does `exec >>"$run_log" 2>&1` (line ~301) before any of the
# code under test, so NOTHING it prints reaches the file we redirect into.
# Greps against that file are vacuously true — which is exactly how the
# HV_SLUICE_SKIP_MOUTH assertion below first "passed" while reading an empty
# file. Read the run log instead.
# Each sub-test below gets its OWN state dir, so exactly one run log exists in
# it and there is nothing to disambiguate. `ls -t` over a shared dir is not
# good enough: several of these runs share a SHA and land in the same second,
# so the newest-first pick is a coin flip — which is how the SKIP_MOUTH
# assertion read the PREVIOUS run's log and reported a failure that was really
# an ambiguous selection.
only_run_log() {
    local f
    for f in "$1"/sluice-*.log; do
        [ -e "$f" ] && { printf '%s\n' "$f"; return 0; }
    done
    return 1
}

export HV_SLUICE_PHASES="a b c"
export HV_SLUICE_WORKTREE="$tmp/wt-mouth"
export HV_CENSUS_CLAIM_PATH="$tmp/claim-mouth"
rm -f "$HV_CENSUS_CLAIM_PATH"
rm -rf "$HV_SLUICE_WORKTREE"

mrc=0
mouth1_dir="$tmp/state-mouth1"; mkdir -p "$mouth1_dir"
g -C "$chamber_repo" rev-parse origin/main > "$mouth1_dir/last-pushed"
HV_SLUICE_DIR="$mouth1_dir" \
  bash "$repo_root/scripts/sluice-run.sh" campaign/tconf "$conf_sha" > "$tmp/mouth1.out" 2>&1 || mrc=$?
mlog="$(only_run_log "$mouth1_dir")"
if [ "$mrc" -eq 21 ]; then
    ok "a conflicting candidate exits 21 (mouth verdict 1), not 10 (the <merge> step)"
else
    bad "expected rc 21 from the pre-lock mouth check, got $mrc (log ${mlog:-none})"
fi
# THE HEADLINE CLAIM: the box was never taken.
#
# NOT "the claim file does not exist afterwards" — that assertion is VACUOUS.
# The chamber removes its claim on exit (this suite asserts exactly that a few
# sections up: "the claim is removed after a normal (rc=0) exit"), so the file
# is absent after every run whether the box was taken or not, and the check
# would pass even with the whole refusal deleted. Read the run log for the
# moment of acquisition instead — that line is written once, when the staff is
# actually held, and nothing erases it.
if [ -z "$mlog" ]; then
    bad "no run log for the refused candidate — cannot tell whether the box was taken"
elif grep -q 'holds the staff' "$mlog"; then
    bad "the run log says it HELD THE STAFF — the chamber took the box before refusing, which is the whole defect"
elif grep -q 'queued for the staff' "$mlog"; then
    bad "the run log says it QUEUED for the staff — it reached the flock wait it was supposed to skip"
else
    ok "the run log shows the staff was never queued for or held — the box was never taken"
fi

# MUTATION: neutralise the refusal and confirm these assertions redden. Without
# this, a test asserting rc=21 could be satisfied by any early exit at all.
mut="$tmp/sluice-run.mut.sh"
# shellcheck disable=SC2016  # the single quotes are the point: this must match
# the LITERAL text `exit $((20 + mouth_rc))` in the script under test, not the
# value of that arithmetic expression.
sed 's|exit \$((20 + mouth_rc))|: ;|' "$repo_root/scripts/sluice-run.sh" > "$mut"
if ! cmp -s "$mut" "$repo_root/scripts/sluice-run.sh"; then
    rm -f "$HV_CENSUS_CLAIM_PATH"; rm -rf "$HV_SLUICE_WORKTREE"
    mutrc=0
    mut_dir="$tmp/state-mut"; mkdir -p "$mut_dir"
    g -C "$chamber_repo" rev-parse origin/main > "$mut_dir/last-pushed"
    HV_SLUICE_DIR="$mut_dir" bash "$mut" campaign/tconf "$conf_sha" > "$tmp/mouth1mut.out" 2>&1 || mutrc=$?
    mutlog="$(only_run_log "$mut_dir")"
    if [ "$mutrc" -eq 21 ]; then
        bad "MUTATION DID NOT TAKE: the mutant still exited 21 with the refusal removed"
    elif [ -n "$mutlog" ] && grep -q 'holds the staff' "$mutlog"; then
        ok "MUTATION CONFIRMED: with the refusal removed the mutant HELD THE STAFF and exited $mutrc — so the assertion above is testing the refusal, not some unrelated early exit"
    else
        bad "MUTATION INCONCLUSIVE: the mutant exited $mutrc but its log never shows it holding the staff — the 'box was never taken' assertion above is not proven non-vacuous"
    fi
else
    bad "MUTATION NOT APPLIED: the 'exit \$((20 + mouth_rc))' line was not found — this test proves nothing"
fi

# A NON-VERDICT MUST NOT REFUSE. The mouth's exit 2 means "I could not
# evaluate this", which is a statement about the environment, not the
# candidate. Turning that into a refusal would invent a new way to fail closed
# — and did: the first cut resolved the mouth under $repo_root, got 127 here,
# and refused every chamber run in this file.
rm -f "$HV_CENSUS_CLAIM_PATH"; rm -rf "$HV_SLUICE_WORKTREE"
nrc=0
mouth2_dir="$tmp/state-mouth2"; mkdir -p "$mouth2_dir"
HV_SLUICE_BASE=no-such-ref-at-all HV_SLUICE_DIR="$mouth2_dir" \
  bash "$repo_root/scripts/sluice-run.sh" campaign/tconf "$conf_sha" > "$tmp/mouth2.out" 2>&1 || nrc=$?
nlog="$(only_run_log "$mouth2_dir")"
if [ "$nrc" -eq 22 ]; then
    bad "an unevaluable base exited 22 — a non-verdict was treated as a refusal"
elif [ -n "$nlog" ] && grep -q 'PROCEEDING to the box' "$nlog"; then
    ok "an unevaluable base warns and PROCEEDS rather than refusing (rc=$nrc)"
else
    bad "expected a 'PROCEEDING to the box' warning in the run log for an unevaluable base; got rc=$nrc, log=${nlog:-none}"
fi

# The override exists and actually skips the check.
rm -f "$HV_CENSUS_CLAIM_PATH"; rm -rf "$HV_SLUICE_WORKTREE"
src=0
mouth3_dir="$tmp/state-mouth3"; mkdir -p "$mouth3_dir"
HV_SLUICE_SKIP_MOUTH=1 HV_SLUICE_DIR="$mouth3_dir" \
  bash "$repo_root/scripts/sluice-run.sh" campaign/tconf "$conf_sha" > "$tmp/mouth3.out" 2>&1 || src=$?
slog="$(only_run_log "$mouth3_dir")"
if [ "$src" -eq 21 ]; then
    bad "HV_SLUICE_SKIP_MOUTH=1 did not skip the mouth — still exited 21"
elif [ -z "$slog" ]; then
    bad "HV_SLUICE_SKIP_MOUTH=1: no run log found — cannot tell whether the mouth was consulted"
elif grep -qE 'sluice-mouth:|REFUSED BEFORE THE BOX' "$slog"; then
    bad "HV_SLUICE_SKIP_MOUTH=1 still consulted the mouth (run log names it)"
else
    ok "HV_SLUICE_SKIP_MOUTH=1 skips the check entirely — the run log shows no mouth verdict (rc=$src)"
fi
unset HV_SLUICE_PHASES HV_SLUICE_WORKTREE

echo "== census: kind=census is queueable, and delivers on a branch =="
# A census cannot be a chamber phase (census-run.sh takes the shared claim
# itself and rm -f's the claim file on exit), so scripts/sluice-census.sh runs
# it instead. These tests stub census-run.sh entirely: the point under test is
# the DELIVERY contract — refuse a bad ref, commit only the census's own
# output, push a branch, never push main — not whether a census computes.

if bash "$repo_root/scripts/sluice-queue.sh" add c/x "$(printf '0%.0s' $(seq 1 40))" census >/dev/null 2>&1; then
    ok "the queue accepts kind=census"
else
    bad "the queue rejected kind=census — validate_kind was not widened"
fi
if bash "$repo_root/scripts/sluice-queue.sh" add c/x "$(printf '0%.0s' $(seq 1 40))" nonsense >/dev/null 2>&1; then
    bad "the queue accepted a nonsense kind — widening validate_kind removed its teeth"
else
    ok "an unknown kind is still rejected (the widening did not open the gate)"
fi

cen="$tmp/cen"; mkdir -p "$cen/scripts"
# sluice-census.sh resolves sluice-queue.sh under HV_SLUICE_REPO_ROOT (=$cen
# here), same as sluice-run.sh does for $chamber_repo above — so this scratch
# repo needs its own copy for the SAME reason. Before fix round 2 this went
# unnoticed because a missing script there just made `claim` fail, and the
# `*)` arm proceeded unbookkept regardless; now that arm REFUSES (Critical
# F1), so the copy is load-bearing for every case below that expects rc=0.
# HV_SLUICE_BIN (exported once, above, and never unset) lets this copy reach
# the real checkout's binary without needing its own tools/sluice sibling.
cp "$repo_root/scripts/sluice-queue.sh" "$cen/scripts/sluice-queue.sh"
cen_origin="$tmp/cen-origin.git"; git init -q --bare -b main "$cen_origin"
(
    cd "$cen"
    g init -q -b main .
    g config user.email c@c; g config user.name c
    mkdir -p book/src/laboratory/generated/the-census
    printf 'seed,value\n42,1\n' > book/src/laboratory/generated/the-census/rows.csv
    # census_golden_count (sluice-census.sh) and injection_arms_stale both read
    # docs/generated-paths.txt off the WORKTREE, falling back to HV_SLUICE_REPO_ROOT
    # (=$cen here) when the worktree copy is absent — and this fixture never
    # declared either, which sluice_path_author (sluice-phases.sh) tolerates by
    # returning empty rather than failing, so census_golden_count silently read 0
    # for every "moves" run this file has ever driven. Nothing before The Spillway
    # asserted the COUNT, only the branch's eventual content, so the gap was mute
    # until the arms' own trigger (`n_goldens -gt 0`) started depending on it.
    # Mirrors the real file's two rows for this path (exact-row-beats-directory).
    mkdir -p docs
    cat > docs/generated-paths.txt <<'DECL'
# path	author
book/src/laboratory/generated/the-census/schema.json	artifacts
book/src/laboratory/generated/the-census/	census
DECL
    # docs/timings.md must be TRACKED here or this fixture cannot reproduce
    # production at all. The run's own ledger row is what the general `add -u`
    # sweeps, and its absence from this fixture is exactly why the production
    # null went unexercised while these tests appeared to cover it: with no
    # row, "nothing moved" means an EMPTY index, which is a different branch
    # of sluice-census.sh than the one real censuses actually take.
    printf 'baseline\n' > docs/timings.md
    # The Spillway: the delivery compares the census's column set with the
    # Gnomon arms' and re-authors the arms when the world moved or the sets
    # differ. Both files in serde's real pretty-print shape (the study's own
    # "name" at indent 4, nested under "study" at indent 2; column names at
    # exactly indent 6), matching, so the null arms below stay null. This
    # fixture used to put the study's own "name" at indent 2 with no "study"
    # wrapper — a shape the real files never have, found at the campaign's
    # close alongside the ^ {4,} extractor bug this shape could never have
    # caught.
    mkdir -p windows/lab/tests/fixtures/injection/baseline-a
    printf '{\n  "columns": [\n    {\n      "name": "seed"\n    },\n    {\n      "name": "value"\n    }\n  ],\n  "study": {\n    "description": "synthetic",\n    "name": "the-census"\n  }\n}\n' \
        > book/src/laboratory/generated/the-census/schema.json
    printf '{\n  "columns": [\n    {\n      "name": "seed"\n    },\n    {\n      "name": "value"\n    }\n  ],\n  "study": {\n    "description": "synthetic",\n    "name": "gnomon-injection"\n  }\n}\n' \
        > windows/lab/tests/fixtures/injection/baseline-a/schema.json
    printf 'seed,value\n0,1\n' > windows/lab/tests/fixtures/injection/baseline-a/rows.csv
    g add -A; g commit -qm root
    g remote add origin "$cen_origin"; g push -q origin main
)
cen_ref="$(g -C "$cen" rev-parse HEAD)"
cen_wt="$tmp/cen-wt"; cp -r "$cen" "$cen_wt"

# THE STUB AUTHORING SCRIPT lives in the WORKTREE copy, because the delivery
# runs the ref's own gnomon-injection.sh (its ARMS literals must match the
# source at that ref), never the queue's. `check` answers per the mode; a run
# records that it happened, records whether the box lock was HELD around it
# (flock -n on a fresh descriptor fails while the delivery holds the lock —
# the positive control for spec §3.2 step 3), and rewrites one arm file.
# Markers go under $tmp, outside the worktree, so `add -u` cannot sweep them.
cp "$repo_root/scripts/timed.sh" "$cen/scripts/timed.sh"
write_gnomon() {  # $1 = ok|refuse|fail
    cat > "$cen_wt/scripts/gnomon-injection.sh" <<STUB
#!/usr/bin/env bash
if [ "\${1:-}" = "check" ]; then
    [ "$1" = "refuse" ] && { echo "gnomon-injection: REFUSING to run with a dirty tree (stub)" >&2; exit 1; }
    exit 0
fi
echo authored >> "$tmp/gnomon-ran"
if flock -n "\${HV_CENSUS_LOCK:?}" -c true 2>/dev/null; then echo free > "$tmp/gnomon-lock"; else echo held > "$tmp/gnomon-lock"; fi
[ "$1" = "fail" ] && { echo "stub authoring exploded" >&2; exit 1; }
printf 'seed,value\n0,9\n' > "$cen_wt/windows/lab/tests/fixtures/injection/baseline-a/rows.csv"
exit 0
STUB
    chmod +x "$cen_wt/scripts/gnomon-injection.sh"
}
write_gnomon ok

# The stub: `worktree` prints the path; a real invocation does what the mode says.
write_stub() {  # $1 = moves|still|fails
    cat > "$cen/scripts/census-run.sh" <<STUB
#!/usr/bin/env bash
[ "\${1:-}" = "worktree" ] && { echo "$cen_wt"; exit 0; }
case "$1" in
  moves) printf 'seed,value\n42,2\n' > "$cen_wt/book/src/laboratory/generated/the-census/rows.csv" ;;
  still) : ;;
  # The PRODUCTION-SHAPED null: no golden moves, but the run records its own
  # cost, exactly as timed.sh does inside the real census worktree.
  timings) printf 'run 879s\n' >> "$cen_wt/docs/timings.md" ;;
  fails) echo "stub census exploded" >&2; exit 9 ;;
esac
exit 0
STUB
    chmod +x "$cen/scripts/census-run.sh"
}

run_census() {  # $1 = ref ; echoes rc
    HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" HV_CENSUS_LOCK="$tmp/census.lock" \
        bash "$repo_root/scripts/sluice-census.sh" "$1" >/dev/null 2>&1
    echo $?
}

# The job id sluice-census.sh derives is only second-resolution
# (census-<ref12>-<stamp-to-the-second>), so two runs of the same ref inside
# one second share a log file (`exec >>` appends). A grep over the whole file
# after such a run can match bytes a NEIGHBOURING run wrote, not this one's
# own output. newest_census_log finds the current newest log path; pairing it
# with a byte offset taken BEFORE a run lets a caller grep only what that run
# itself appended.
newest_census_log() {
    local f log=""
    for f in "$tmp/cen-state"/census-*.log; do [ -e "$f" ] && log="$f"; done
    printf '%s' "$log"
}

write_stub moves
rc_bad=$(run_census "deadbeef")
if [ "$rc_bad" = "2" ]; then
    ok "a short/non-hex ref is refused with rc=2, before any work"
else
    bad "expected rc=2 for a malformed ref, got $rc_bad"
fi

main_before="$(g -C "$cen_origin" rev-parse main)"
rc_moves=$(run_census "$cen_ref")
if [ "$rc_moves" = "0" ]; then ok "a census whose goldens moved exits 0"; else bad "expected rc=0, got $rc_moves"; fi
delivered="$(g -C "$cen_origin" for-each-ref --format='%(refname:short)' 'refs/heads/census/*' | head -1)"
if [ -n "$delivered" ]; then
    ok "the goldens were delivered on a branch ($delivered)"
else
    bad "no census/* branch was pushed — the delivery half did nothing"
fi
if [ "$(g -C "$cen_origin" rev-parse main)" = "$main_before" ]; then
    ok "MAIN WAS NOT PUSHED — census goldens land through the chamber, not around it"
else
    bad "main MOVED — a census pushed straight to main, which is the one thing this must never do"
fi
if [ -n "$delivered" ] && g -C "$cen_origin" show "$delivered:book/src/laboratory/generated/the-census/rows.csv" 2>/dev/null | grep -q '42,2'; then
    ok "the delivered branch carries the regenerated golden, not the old one"
else
    bad "the delivered branch does not carry the new golden"
fi

# --- THE ARMS RODE ALONG (The Spillway, spec §3.2) --------------------------
# The world moved, so the delivery re-authored the arms, under the lock, and
# committed them with the goldens.
if [ -f "$tmp/gnomon-ran" ]; then ok "a moving census re-authored the Gnomon arms"
else bad "a moving census did NOT run gnomon-injection.sh"; fi
if [ "$(cat "$tmp/gnomon-lock" 2>/dev/null)" = "held" ]; then
    ok "the box lock was HELD while the arms were authored (flock -n failed inside the stub)"
else
    bad "the arms were authored with the lock FREE (marker: $(cat "$tmp/gnomon-lock" 2>/dev/null || echo none))"
fi
if [ -n "$delivered" ] && g -C "$cen_origin" show "$delivered:windows/lab/tests/fixtures/injection/baseline-a/rows.csv" 2>/dev/null | grep -q '0,9'; then
    ok "the delivered branch carries the RE-AUTHORED arm beside the goldens"
else bad "the delivered branch does not carry the re-authored arm"; fi
if [ -n "$delivered" ] && g -C "$cen_origin" log -1 --format=%B "$delivered" | grep -q 'Gnomon arms re-authored at'; then
    ok "the commit message names the arms"
else bad "the commit message does not name the arms"; fi
if [ -n "$delivered" ] && g -C "$cen_origin" show "$delivered:docs/timings.md" 2>/dev/null | grep -q '| gnomon-injection |'; then
    ok "the re-authoring's cost landed as a gnomon-injection row in docs/timings.md"
else bad "no gnomon-injection row in the delivered timings ledger — the cost went unrecorded"; fi
rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"

# MUTATION: a run that moves nothing must not manufacture a branch.
before_n="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
write_stub still
rc_still=$(run_census "$cen_ref")
after_n="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
# "no new branch" alone is VACUOUS here, and a mutation proved it: deleting the
# guard entirely still pushes nothing, because `git commit` refuses an empty
# commit. The test then passes for a reason that has nothing to do with the
# behaviour it claims to check. Assert the guard's OWN output, which only the
# guard can produce.
still_log=""
for _f in "$tmp/cen-state"/census-*.log; do [ -e "$_f" ] && still_log="$_f"; done
if [ "$rc_still" = "0" ] && [ "$before_n" = "$after_n" ] \
   && [ -n "$still_log" ] && grep -q 'NO GOLDENS MOVED' "$still_log"; then
    ok "a census that moves nothing reports NO GOLDENS MOVED, exits 0, and pushes no branch"
else
    bad "unmoved census: rc=$rc_still, branches $before_n -> $after_n, log=${still_log:-none} \
(expected rc=0, no new branch, and the NO GOLDENS MOVED line)"
fi

# THE PRODUCTION-SHAPED NULL, which nothing exercised until 2026-09-02 and
# which is the case every real census actually takes. Above, `still` stages
# NOTHING, so it lands on sluice-census.sh's empty-index branch. A real run
# always stages its own docs/timings.md row, so it lands on the branch below --
# and that branch's guard was UNREACHABLE, because the predicate asked "is
# anything staged?" when the question was "did anything move BESIDES the row?".
# Two censuses on 2026-09-02 delivered a branch whose entire content was
# `docs/timings.md | 1 +` while announcing that goldens had moved.
#
# The delivery is deliberately KEPT on a null: dropping it would destroy the
# run's cost measurement, reopening for census the bug The Governor's Task 7
# closed for heavy. So this asserts the REPORT is honest, not that the push is
# skipped.
write_stub timings
rc_t=$(run_census "$cen_ref")
t_log=""
for _f in "$tmp/cen-state"/census-*.log; do [ -e "$_f" ] && t_log="$_f"; done
if [ "$rc_t" = "0" ] && [ -n "$t_log" ] && grep -q 'NO GOLDENS MOVED' "$t_log"; then
    ok "a census that moves only the timings row reports NO GOLDENS MOVED"
else
    bad "timings-only census: rc=$rc_t, log=${t_log:-none} (expected rc=0 and NO GOLDENS MOVED)"
fi
# Asserts the guard's OWN output, not a branch COUNT, and the difference is not
# stylistic. The branch name is census/<ref12>-<stamp-to-the-second>, and this
# harness runs `moves` and `timings` against the SAME ref inside the same
# second, so both compose an identical name and the second push fast-forwards
# the first instead of creating a branch. A count assertion reads 1 -> 1 and
# calls a successful delivery a dropped row. (That collision is real in
# production too, if two censuses of one ref ever land in the same second --
# rare, and not this test's business.)
if [ -n "$t_log" ] && grep -q 'DELIVERED on' "$t_log"; then
    ok "the timings row is still DELIVERED on a branch -- the cost measurement survives"
else
    bad "timings-only census did not deliver; the run's cost measurement was dropped"
fi
# The anti-vacuity half: the branch must carry the row and NOT a golden, or both
# assertions above could pass on a branch that had actually moved goldens.
t_branch="$(g -C "$cen_origin" for-each-ref --format='%(refname:short)' 'refs/heads/census/*' | tail -1)"
t_files="$(g -C "$cen_origin" show --name-only --format= "$t_branch" 2>/dev/null | grep -c .)"
if [ "$t_files" = "1" ] && g -C "$cen_origin" show --name-only --format= "$t_branch" 2>/dev/null | grep -q '^docs/timings\.md$'; then
    ok "the delivered branch carries the timings row ALONE -- no golden rode along"
else
    bad "timings-only branch carried $t_files path(s), expected exactly docs/timings.md"
fi

# --- ARMS: the null does not re-author; a stale column set does -------------
if [ ! -f "$tmp/gnomon-ran" ] && grep -q 'Gnomon arms unchanged' "$t_log"; then
    ok "a census that moved nothing, with matching arms, left the arms alone and said so"
else bad "null census: ran=$([ -f "$tmp/gnomon-ran" ] && echo yes || echo no), log lacks 'Gnomon arms unchanged'"; fi

# The column trigger alone: the census moves nothing, but an arm was authored
# against a registry the census has since outgrown. Committed in the worktree
# so the tree is clean when the delivery starts, exactly as a real ref is.
pre_c="$(g -C "$cen_wt" rev-parse HEAD)"
printf '{\n  "columns": [\n    {\n      "name": "seed"\n    }\n  ],\n  "study": {\n    "description": "synthetic",\n    "name": "gnomon-injection"\n  }\n}\n' \
    > "$cen_wt/windows/lab/tests/fixtures/injection/baseline-a/schema.json"
g -C "$cen_wt" add -A; g -C "$cen_wt" -c user.name=c -c user.email=c@c commit -qm "stale arm"
rm -f "$tmp/gnomon-ran"
rc_c=$(run_census "$cen_ref")
c_log=""
for _f in "$tmp/cen-state"/census-*.log; do [ -e "$_f" ] && c_log="$_f"; done
if [ "$rc_c" = "0" ] && [ -f "$tmp/gnomon-ran" ] && grep -q 'arms are stale' "$c_log" && grep -q '^sluice-census:   baseline-a: 1 column' "$c_log"; then
    ok "a null census with a STALE arm re-authors, and the log names the arm and the count"
else bad "stale-arm census: rc=$rc_c ran=$([ -f "$tmp/gnomon-ran" ] && echo yes || echo no) log=$c_log"; fi
g -C "$cen_wt" reset -q --hard "$pre_c"
rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"

# --- ARMS: a failed authoring is a refusal, not a delivery --------------------
# After the first `moves` delivery, the worktree golden already reads 42,2, so
# a second `moves` stub moves nothing (n_goldens=0) and, with matching
# columns, the delivery would skip the arms entirely. Write it back to 42,1
# and commit, so `moves` moves it again and the arms path is actually entered.
printf 'seed,value\n42,1\n' > "$cen_wt/book/src/laboratory/generated/the-census/rows.csv"
g -C "$cen_wt" add -A; g -C "$cen_wt" -c user.name=c -c user.email=c@c commit -qm "golden back to 1"
write_stub moves; write_gnomon fail
before_af="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
pre_af_log="$(newest_census_log)"
off_af="$(wc -c < "$pre_af_log" 2>/dev/null || echo 0)"
rc_af=$(run_census "$cen_ref")
after_af="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
af_log="$(newest_census_log)"
# A run that opened a NEW log (the second rolled over) started at byte 0 of
# it; only a run that reused the SAME path inherits the pre-run offset.
[ "$af_log" = "$pre_af_log" ] || off_af=0
if [ "$rc_af" = "4" ] && [ "$before_af" = "$after_af" ] && [ -n "$af_log" ] \
   && tail -c "+$((off_af + 1))" "$af_log" | grep -q 'ARMS NOT RE-AUTHORED' \
   && tail -c "+$((off_af + 1))" "$af_log" | grep -q 're-authoring the Gnomon injection arms' \
   && [ -n "$(g -C "$cen_wt" diff --cached --name-only)" ]; then
    ok "a failed re-authoring exits 4, pushes nothing, names itself, and leaves the goldens staged for recovery"
else bad "failed authoring: rc=$rc_af branches $before_af -> $after_af staged=$(g -C "$cen_wt" diff --cached --name-only | wc -l) log=$af_log"; fi
g -C "$cen_wt" reset -q --hard; rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"

# --- ARMS: a refused pre-flight never authors and never takes the lock -------
# Follows (d)'s reset --hard, whose HEAD carries the golden-back-to-1 commit
# (42,1), so `moves` (still baked from write_stub above) moves it again here too.
write_gnomon refuse
before_ar="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
pre_ar_log="$(newest_census_log)"
off_ar="$(wc -c < "$pre_ar_log" 2>/dev/null || echo 0)"
rc_ar=$(run_census "$cen_ref")
after_ar="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
ar_log="$(newest_census_log)"
[ "$ar_log" = "$pre_ar_log" ] || off_ar=0
if [ "$rc_ar" = "4" ] && [ "$before_ar" = "$after_ar" ] && [ -n "$ar_log" ] \
   && tail -c "+$((off_ar + 1))" "$ar_log" | grep -q 'refused its pre-flight' \
   && tail -c "+$((off_ar + 1))" "$ar_log" | grep -q 're-authoring the Gnomon injection arms' \
   && [ ! -f "$tmp/gnomon-ran" ]; then
    ok "a refused check exits 4, pushes nothing, and authoring was never entered"
else bad "refused check: rc=$rc_ar branches $before_ar -> $after_ar ran=$([ -f "$tmp/gnomon-ran" ] && echo yes || echo no) log=$ar_log"; fi
g -C "$cen_wt" reset -q --hard; rm -f "$tmp/gnomon-ran" "$tmp/gnomon-lock"
write_gnomon ok


write_stub fails
# Capture the baseline HERE rather than reusing $after_n from an earlier block.
# It was reused, and inserting a test between the two silently broke this one:
# the new test legitimately pushed a branch, so a stale baseline reported a
# delivery this run never made. A test whose baseline is set by a distant,
# unrelated block is fragile to insertion in a way that reads as a real failure.
before_fail="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
rc_fail=$(run_census "$cen_ref")
after_fail="$(g -C "$cen_origin" for-each-ref 'refs/heads/census/*' | wc -l)"
if [ "$rc_fail" != "0" ] && [ "$after_fail" = "$before_fail" ]; then
    ok "a failed census propagates non-zero and delivers nothing"
else
    bad "failed census: rc=$rc_fail, branches $before_fail -> $after_fail (expected non-zero and no branch)"
fi

echo "== census: a request id must name a CLAIMED row, and a row of kind=census =="
# THE SAME CRITICAL AS sluice-run.sh's, on the more expensive path, plus the
# kind check sluice-run.sh had and this script did not (fix round 3, Important
# 2): `sluice-census.sh req-<a stage request>` spent ~15 minutes of the box
# answering a question nobody asked, and then wrote `reported` on the row as
# though the stage gate had run.
cenq="$tmp/cen-state/queue.tsv"; mkdir -p "$tmp/cen-state"
cen_row() { printf '2026-09-05T00:00:00Z\treq-%s\tc/x\t%s\t%s\t%s\t\n' "$1" "$2" "$3" "$4"; }
cen_state_of() { awk -F'\t' -v i="req-$1" '$2==i{print $5}' "$cenq"; }
write_stub still

: > "$cenq"
cen_row cq "$cen_ref" queued census >> "$cenq"
set +e
cq_out="$(HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" \
          timeout 30 bash "$repo_root/scripts/sluice-census.sh" req-cq 2>&1)"
cq_rc=$?
set -e
if [ "$cq_rc" = "16" ] && printf '%s' "$cq_out" | grep -q "NOT claimed" \
   && [ "$(cen_state_of cq)" = "queued" ]; then
    ok "a census request id whose row is still queued is REFUSED rc=16"
else
    bad "req-cq gave rc=$cq_rc state=$(cen_state_of cq) out='$cq_out' — an unclaimed census can still be started twice"
fi

: > "$cenq"
cen_row ck "$cen_ref" running stage >> "$cenq"
set +e
ck_out="$(HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" \
          timeout 30 bash "$repo_root/scripts/sluice-census.sh" req-ck 2>&1)"
ck_rc=$?
set -e
if [ "$ck_rc" = "2" ] && printf '%s' "$ck_out" | grep -q "kind='stage'"; then
    ok "a census entry point refuses a row of kind=stage, naming the kind it found"
else
    bad "req-ck (kind=stage) gave rc=$ck_rc out='$ck_out' — a stage request can still start a ~15-minute census"
fi

# THE POSITIVE DIRECTION, or the two refusals above are satisfied by a script
# that refuses everything. A claimed row of the right kind must go through and
# reach the stubbed census.
: > "$cenq"
cen_row cg "$cen_ref" running census >> "$cenq"
set +e
cg_out="$(HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" \
          timeout 60 bash "$repo_root/scripts/sluice-census.sh" req-cg 2>&1)"
cg_rc=$?
set -e
if [ "$cg_rc" = "0" ] && printf '%s' "$cg_out" | grep -q "kind=census"; then
    ok "a CLAIMED row of kind=census resolves and runs (the guards refuse, they do not block)"
else
    bad "req-cg gave rc=$cg_rc out='$cg_out' — a legitimate queued census can no longer be dispatched by id"
fi
rm -f "$cenq"

echo "== request path: every kind the usage string offers is a kind it ACCEPTS =="
# THE DEFECT THIS PINS, and it is the reason this test is shaped as a LOOP over
# the advertised kinds rather than three hand-written cases. `kind=census`
# shipped with sluice-request.sh's usage strings widened and its validating
# `case` left alone, so `make sluice-census` printed
# "unknown kind 'census' (merge|stage)" — a feature whose documentation and
# whose entry point both worked and whose validation did not. It survived a
# green four-phase merge because the census tests exercised
# `sluice-queue.sh add census`, the half that HAD been widened, and never the
# request path a caller takes.
#
# So the assertion is not "census is accepted". It is "the usage string and the
# validation agree", derived from the script itself, which cannot drift the way
# a hand-listed set can: add a fourth kind to the usage text and forget the
# case, and this reddens without anyone editing this file.
req="$repo_root/scripts/sluice-request.sh"
advertised="$(grep -om1 '\[merge[a-z|]*\]' "$req" | tr -d '[]' | tr '|' ' ')"
if [ -z "$advertised" ]; then
    bad "could not read the advertised kinds out of sluice-request.sh's usage string"
else
    ok "usage string advertises: $advertised"
    rejected=""
    for k in $advertised; do
        # A bogus REF: we want the KIND check's verdict, and it runs before any
        # network or host work, so an invalid sha is enough to stop it there.
        out="$(bash "$req" some/branch not-a-sha "$k" 2>&1 || true)"
        case "$out" in
            *"unknown kind"*) rejected="$rejected $k" ;;
        esac
    done
    if [ -n "$rejected" ]; then
        bad "sluice-request.sh ADVERTISES these kinds and REFUSES them:$rejected"
    else
        ok "every advertised kind survives the validating case (no usage/validation drift)"
    fi
    out="$(bash "$req" some/branch not-a-sha definitely-not-a-kind 2>&1 || true)"
    case "$out" in
        *"unknown kind"*) ok "a bogus kind is still rejected (the widening kept its teeth)" ;;
        *) bad "a bogus kind was NOT rejected — the kind check has stopped checking" ;;
    esac
fi


# ---------------------------------------------------------------------------
# `claim` — the atomic select-and-mark that replaces `next` + `set-state`.
#
# These run against their OWN HV_SLUICE_DIR with rows written directly, rather
# than through `add`: `claim` touches no git objects, so building real commits
# to exercise it would test `add`'s ancestry logic a fourth time and tell us
# nothing about the transaction under test.
# ---------------------------------------------------------------------------
echo "== queue: claim is an atomic select-and-mark"
cdir="$tmp/claimstate"; mkdir -p "$cdir"
CQ="$cdir/queue.tsv"
row() { printf '%s\treq-%s\t%s\t%s\t%s\t%s\t\n' "2026-09-04T00:00:00Z" "$1" "$2" "$3" "$4" "$5"; }
qq() { HV_SLUICE_DIR="$cdir" bash "$repo_root/scripts/sluice-queue.sh" "$@"; }
cstate_of() { awk -F'\t' -v i="req-$1" '$2==i{print $5}' "$CQ"; }

# T1 — nothing queued: empty output, exit 0, same contract `next` had.
: > "$CQ"
row aaa campaign/a aaaaaaaaaaaa landed merge >> "$CQ"
out="$(qq claim 2>/dev/null)"; rc=$?
if [ -z "$out" ] && [ "$rc" = "0" ]; then
    ok "claim on a drained queue prints nothing and exits 0 (keeps next's contract)"
else
    bad "claim on a drained queue printed '$out' rc=$rc — dispatchers test for empty output"
fi

# T2 — the transaction: after ONE call the row is already running. This is the
# property `next` could not provide, and the whole reason this subcommand
# exists.
: > "$CQ"
row bbb campaign/b bbbbbbbbbbbb queued merge >> "$CQ"
out="$(qq claim "taken by the test")"
if [ -n "$out" ] && [ "$(cstate_of bbb)" = "running" ]; then
    ok "claim marks the row running in the same call that selects it"
else
    bad "after claim the row was '$(cstate_of bbb)' — the TOCTOU window is still open"
fi

# T3 — the executor's form: claim a specific ref, not merely the head of the queue.
: > "$CQ"
row ccc campaign/c cccccccccccc queued merge >> "$CQ"
row ddd campaign/d dddddddddddd queued merge >> "$CQ"
out="$(qq claim --sha dddddddddddd "by sha")"
if [ "$(cstate_of ddd)" = "running" ] && [ "$(cstate_of ccc)" = "queued" ]; then
    ok "claim --sha takes the named row and leaves the rest queued"
else
    bad "claim --sha took ccc=$(cstate_of ccc) ddd=$(cstate_of ddd) — an executor would claim the wrong job"
fi

# T4 — THE DUPLICATE-EXECUTION GUARD. A second claimant on a row somebody
# already holds must refuse, and must not touch the row. This is the exact
# failure of 2026-09-04: a hand-run drain launched a merge that a direct
# sluice-run.sh was already executing, because the row still read `queued`.
: > "$CQ"
row eee campaign/e eeeeeeeeeeee running merge >> "$CQ"
set +e; err="$(qq claim --sha eeeeeeeeeeee 2>&1 >/dev/null)"; rc=$?; set -e
if [ "$rc" = "4" ] && [ "$(cstate_of eee)" = "running" ] \
   && printf '%s' "$err" | grep -q "NOT queued"; then
    ok "claim --sha on an already-held row refuses rc=4, says why, and changes nothing"
else
    bad "claim --sha on a held row gave rc=$rc state=$(cstate_of eee) err='$err' — duplicate execution is still reachable"
fi

# T5 — a ref with no row at all is a DIFFERENT answer from a ref somebody
# holds. An executor run ad hoc (no queue row) is legitimate; an executor
# racing another run is not. One exit code for each, or the caller cannot
# tell them apart.
: > "$CQ"
row fff campaign/f ffffffffffff queued merge >> "$CQ"
set +e; qq claim --sha 999999999999 >/dev/null 2>&1; rc=$?; set -e
if [ "$rc" = "5" ]; then
    ok "claim --sha for an absent ref exits 5, distinct from the rc=4 already-held"
else
    bad "claim --sha for an absent ref exited $rc — ad hoc runs cannot be told from races"
fi

# T6 — ATOMICITY UNDER CONTENTION, the property the whole design turns on.
# Twelve simultaneous claimants, ONE queued row: exactly one may win. With
# `next` + a separate `set-state` this is precisely what failed.
: > "$CQ"
row ggg campaign/g gggggggggggg queued merge >> "$CQ"
winners="$tmp/claim-winners"; : > "$winners"
for i in $(seq 1 12); do
    ( out="$(qq claim "racer-$i" 2>/dev/null)"; [ -n "$out" ] && echo "$i" >> "$winners" ) &
done
wait
nwin="$(grep -c . "$winners" 2>/dev/null || echo 0)"
if [ "$nwin" = "1" ] && [ "$(cstate_of ggg)" = "running" ]; then
    ok "12 simultaneous claimants, 1 queued row -> exactly 1 winner (select+mark is atomic)"
else
    bad "$nwin of 12 claimants won the same row — the queue would dispatch it $nwin times"
fi


# T7 — THE EXECUTOR ITSELF REFUSES A ROW SOMEBODY HOLDS. T4 pins the queue's
# answer; this pins that sluice-run.sh actually ASKS. The 2026-09-04 duplicate
# happened because the executor never consulted the queue at all, so a correct
# queue would not have stopped it. Runs the real script — the claim check sits
# above the box lock and the worktree, so the refusal is cheap and touches
# nothing.
# RE-ESTABLISH THE SCRATCH REPO'S COPY OF sluice-queue.sh, AND ASSERT IT.
# The copy made at setup is UNTRACKED in $chamber_repo, and between there and
# here this suite's own chamber tests run a real chamber against that same
# scratch repo — and sluice-run.sh does `git clean -fd` between phases
# (scripts/sluice-run.sh:649), which deletes exactly this file. That made T7/T8
# ORDER-DEPENDENT: they passed in one merge (219/0) and failed in the very next
# candidate's (217/2), blocking an innocent campaign at `outboard`. A flaky test
# in `outboard` blocks every merge, which is worse than the defect T7/T8 guard.
#
# The assertion is the load-bearing half. Without it a missing script makes
# sluice-run.sh take its "could not reach the queue" arm, proceed into real git
# work and die 128 — which reads like a broken guard instead of a missing file,
# and cost an hour of misattribution the first time.
cp "$repo_root/scripts/sluice-queue.sh" "$chamber_repo/scripts/sluice-queue.sh"
if [ -s "$chamber_repo/scripts/sluice-queue.sh" ]; then
    ok "the scratch chamber repo carries sluice-queue.sh (T7/T8 below can mean something)"
else
    bad "the scratch chamber repo has NO sluice-queue.sh — T7/T8 below would report a fake rc=128"
fi

echo "== sluice-run: refuses a ref another run already holds"
: > "$CQ"
row hhh campaign/h hhhhhhhhhhhh running merge >> "$CQ"
set +e
# NOTE THE ABSENCE OF AN HV_SLUICE_REPO_ROOT OVERRIDE, AND DO NOT ADD ONE.
# It is exported to the scratch $chamber_repo above and must stay there.
# Pointing it at the real repo runs this script against a LIVE checkout, and
# since the harness also exports HV_CENSUS_LOCK to a temp lock, the nested run
# takes a FREE lock and proceeds to detach and reset that checkout. Done inside
# the chamber on 2026-09-04 it discarded a merge product mid-run: the merge
# reported rc=0 and LANDED while main received only the pre-merge tree's
# artifact regens. The blast-radius guard at the end of this file exists to
# catch a recurrence.
runout="$(HV_SLUICE_DIR="$cdir" bash "$repo_root/scripts/sluice-run.sh" \
    campaign/h hhhhhhhhhhhh merge 2>&1)"
runrc=$?
set -e
if [ "$runrc" = "9" ] && printf '%s' "$runout" | grep -q "REFUSING"; then
    ok "sluice-run refuses rc=9 when the row is already held (the duplicate is unreachable)"
else
    bad "sluice-run gave rc=$runrc on a held row — a second run of one job is still reachable"
fi

# T8 — and it does NOT refuse a ref with no row, because an ad hoc run is a
# legitimate operator escape hatch. A fix that closed the hatch would have been
# the wrong fix; this pins that it stayed open. Asserted on the MESSAGE rather
# than on a full run, which would take the box.
: > "$CQ"
row iii campaign/i iiiiiiiiiiii queued merge >> "$CQ"
set +e
adhoc="$(HV_SLUICE_DIR="$cdir" timeout 20 bash "$repo_root/scripts/sluice-run.sh" \
    campaign/zzz 999999999999 merge 2>&1)"
set -e
if printf '%s' "$adhoc" | grep -q "AD HOC"; then
    ok "a ref with no queue row runs ad hoc and says so (the escape hatch stayed open)"
else
    bad "an unqueued ref did not report AD HOC — the operator escape hatch may have closed"
fi

echo "== sluice-run: a request id supplies branch, sha and kind from the row"
# THE ROW IS `running`, AND THAT IS NOW LOAD-BEARING RATHER THAN INCIDENTAL.
# This test used to seed the row `queued` and pass, which is precisely the
# Critical of fix round 3: resolving an id skipped the interlock on the
# ASSUMPTION that the caller held the row, and this test asserted the
# resolution while asserting nothing about the claim. `running` is what a
# dispatcher's `claim` writes, so it is the only state a legitimate id-form
# caller can present. The negative direction is the very next test.
: > "$CQ"
row jjj campaign/j jjjjjjjjjjjj running stage >> "$CQ"
set +e
idout="$(HV_SLUICE_DIR="$cdir" timeout 20 bash "$repo_root/scripts/sluice-run.sh" req-jjj 2>&1)"
set -e
if printf '%s' "$idout" | grep -q "kind=stage"; then
    ok "a request id on a CLAIMED row is resolved to its branch, sha and kind"
else
    bad "sluice-run did not resolve req-jjj to kind=stage — an operator-typed kind can still disagree with the row"
fi

# THE CRITICAL OF FIX ROUND 3. A `req-*` id whose row still reads `queued` was
# run anyway, with the interlock skipped: `sluice-drain.sh` could then legally
# `claim` that row (it IS queued) and dispatch the same job a second time —
# the 2026-09-04 duplicate by a different road — coalescing could supersede it
# mid-write, and `release_row` (gated on `claimed_here`) wrote no terminal
# state, leaving a permanent ghost. Reproduced by the reviewer: the row read
# `queued` before AND after a run that had already gone into real work.
# BOTH DIRECTIONS ARE ASSERTED, here and immediately above, because a guard
# with only its refusal witnessed is satisfied by a script that refuses
# everything.
: > "$CQ"
row kkk campaign/k kkkkkkkkkkkk queued stage >> "$CQ"
set +e
qidout="$(HV_SLUICE_DIR="$cdir" timeout 20 bash "$repo_root/scripts/sluice-run.sh" req-kkk 2>&1)"
qidrc=$?
set -e
if [ "$qidrc" = "16" ] \
   && printf '%s' "$qidout" | grep -q "NOT claimed" \
   && printf '%s' "$qidout" | grep -q "req-kkk" \
   && printf '%s' "$qidout" | grep -q "state='queued'" \
   && [ "$(cstate_of kkk)" = "queued" ]; then
    ok "a request id whose row is still queued is REFUSED rc=16, naming the id and its state"
else
    bad "req-kkk on a queued row gave rc=$qidrc state=$(cstate_of kkk) out='$qidout' — an unclaimed row still runs, so a dispatcher can launch it a second time"
fi

# THIS IS PROC-stage-request-should-read-its-own-queue-row's negative
# control: an id with no matching row must be refused, not treated as an
# ad hoc branch name (which is what the positional form does for an unknown
# ref — see the AD HOC test above). Reading `req-` as "go find a row" only
# for a KNOWN id would be a silent bug in the other direction.
set +e
noidout="$(HV_SLUICE_DIR="$cdir" timeout 20 bash "$repo_root/scripts/sluice-run.sh" req-nosuchrow 2>&1)"
noidrc=$?
set -e
if [ "$noidrc" = "2" ] && printf '%s' "$noidout" | grep -q "no queue row with id"; then
    ok "a request id with no matching row is refused, not run as an ad hoc branch name"
else
    bad "req-nosuchrow gave rc=$noidrc, out=$noidout — an unresolvable id should refuse, not run ad hoc"
fi

# ---------------------------------------------------------------------------
# THE REFUSAL PATHS THAT HAD NO WITNESS AT ALL (fix round 3, Important 3).
#
# `grep` across every scripts/test-*.sh for `exit 3`, `rc=13`, `claim failed`
# and `REFUSING — could not reach` returned ZERO hits. Those four paths ARE the
# remedy for fix round 2's Critical: the shim stopped building its binary on
# demand, so a missing binary now REFUSES (exit 3) instead of aborting mid-
# script, and each of its three callers had to learn to tell "the queue refused"
# from "the queue could not be asked". A build failure read as "queue drained"
# or as "running unbookkept" is the whole failure mode, and none of the code
# that prevents it was exercised by anything.
#
# ONE INDUCED CONDITION drives all four: point HV_SLUICE_BIN (the shim's own
# documented test seam) at a path that does not exist. That is exactly the
# production shape — a fresh worktree where nobody ran `make prewarm`.
echo "== the missing-binary refusals: every caller must fail CLOSED"
nobin="$tmp/no-such-sluice-binary"
rm -f "$nobin"

set +e
shimout="$(HV_SLUICE_BIN="$nobin" bash "$repo_root/scripts/sluice-queue.sh" list 2>&1)"
shimrc=$?
set -e
if [ "$shimrc" = "3" ] && printf '%s' "$shimout" | grep -q "no built binary"; then
    ok "the shim refuses exit 3 with no binary, and names what to build"
else
    bad "sluice-queue.sh list with a missing binary gave rc=$shimrc out='$shimout' — callers cannot tell 'unreachable' from 'refused'"
fi

# sluice-run.sh, POSITIONAL form: the claim call's non-{0,4,5} arm.
: > "$CQ"
row mmm campaign/m mmmmmmmmmmmm queued merge >> "$CQ"
set +e
r13a="$(HV_SLUICE_DIR="$cdir" HV_SLUICE_BIN="$nobin" timeout 20 \
        bash "$repo_root/scripts/sluice-run.sh" campaign/m mmmmmmmmmmmm merge 2>&1)"
r13a_rc=$?
set -e
if [ "$r13a_rc" = "13" ] && printf '%s' "$r13a" | grep -q "could not reach the queue"; then
    ok "sluice-run (positional) refuses rc=13 when the queue cannot be asked"
else
    bad "sluice-run gave rc=$r13a_rc out='$r13a' — an unreachable queue must not become an unbookkept run"
fi

# sluice-run.sh, ID form: the `list` pipeline's rc, which used to be swallowed.
# Under `set -e` this died at rc=3 — outside this script's exit vocabulary, and
# recorded by the drain as `CHAMBER RED rc=3 — attribution pending`, blaming the
# candidate for a toolchain fault.
set +e
r13b="$(HV_SLUICE_DIR="$cdir" HV_SLUICE_BIN="$nobin" timeout 20 \
        bash "$repo_root/scripts/sluice-run.sh" req-mmm 2>&1)"
r13b_rc=$?
set -e
if [ "$r13b_rc" = "13" ] && printf '%s' "$r13b" | grep -q "could not reach the queue"; then
    ok "sluice-run (id form) refuses rc=13, not the shim's own rc=3, when the queue is unreachable"
else
    bad "sluice-run req-mmm gave rc=$r13b_rc out='$r13b' — expected 13; rc=3 is not in this script's vocabulary and reads as a red candidate"
fi

# sluice-census.sh, both forms. A census is the most expensive thing on the
# box, so an unbookkept one is the most expensive form of this defect.
set +e
c13a="$(HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" HV_SLUICE_BIN="$nobin" \
        timeout 20 bash "$repo_root/scripts/sluice-census.sh" "$cen_ref" 2>&1)"
c13a_rc=$?
set -e
if [ "$c13a_rc" = "13" ] && printf '%s' "$c13a" | grep -q "could not reach the queue"; then
    ok "sluice-census (positional) refuses rc=13 when the queue cannot be asked"
else
    bad "sluice-census gave rc=$c13a_rc out='$c13a' — expected 13"
fi

# AND THE MIS-ATTRIBUTION THIS FIXES (Important 5). sluice-census.sh runs under
# `set -uo pipefail` with no `-e`, so the `list | awk` pipeline's rc 3 fell on
# the floor, `_row` came back empty, and it reported "no queue row with id
# 'req-…'" — a FALSE claim about a row that exists, at rc=2, which reads as the
# operator's typo. The assertion is on the ABSENCE of that sentence as much as
# on the code.
set +e
c13b="$(HV_SLUICE_REPO_ROOT="$cen" HV_SLUICE_DIR="$tmp/cen-state" HV_SLUICE_BIN="$nobin" \
        timeout 20 bash "$repo_root/scripts/sluice-census.sh" req-anything 2>&1)"
c13b_rc=$?
set -e
if [ "$c13b_rc" = "13" ] && printf '%s' "$c13b" | grep -q "could not reach the queue" \
   && ! printf '%s' "$c13b" | grep -q "no queue row with id"; then
    ok "sluice-census (id form) says the QUEUE is unreachable, not that the row does not exist"
else
    bad "sluice-census req-anything gave rc=$c13b_rc out='$c13b' — an unreachable queue must not be reported as a missing row"
fi

# sluice-drain.sh: the loop must NOT report "queue drained" off an empty stdout
# it never earned. This is the arm that turned a build failure into a green-
# looking "queue drained after 0 run(s)".
set +e
d1="$(HV_SLUICE_DIR="$cdir" HV_SLUICE_BIN="$nobin" timeout 60 \
      bash "$repo_root/scripts/sluice-drain.sh" 1 2>&1)"
d1_rc=$?
set -e
# `grep -q "queue drained"` is NOT the right negative and cost this test one
# red run: the refusal's own wording is `NOT reporting 'queue drained'`, so the
# bare phrase appears in the healthy output. The DRAINED REPORT is
# `queue drained after N run(s)`, and that exact form is what must be absent.
if [ "$d1_rc" != "0" ] && printf '%s' "$d1" | grep -q "claim failed" \
   && ! printf '%s' "$d1" | grep -q "queue drained after"; then
    ok "the drain refuses non-zero and does NOT say 'queue drained' when claim could not run"
else
    bad "drain gave rc=$d1_rc out='$d1' — a broken queue reported as a drained one is the fix-round-2 Critical, back"
fi

# ---------------------------------------------------------------------------
# CROSS-CANDIDATE OVERLAP ADVISORY. sluice-mouth.sh's own admit/refuse verdict
# only ever compares a candidate against $base — it has no opinion about the
# OTHER rows sitting in the queue at the same moment. That blind spot let
# campaign/the-weft queue behind campaign/the-housemark (both touching
# windows/vessel), get admitted (the mouth only checked main), take the box,
# and die rc=10 at <merge> having tested nothing. sluice_report_queue_overlaps
# closes that gap — as an ADVISORY only, never a refusal (a colliding
# candidate may never land; campaign/the-zenith went red on its own the same
# night and never did).
#
# A fresh scratch repo AND a fresh HV_SLUICE_DIR, kept off the giant shared
# queue.tsv this file has been accumulating in $tmp/state since line ~103 —
# an isolated fixture is easier to reason about than one filtering out a
# hundred unrelated rows from earlier sections.
# ---------------------------------------------------------------------------
echo "== mouth: cross-candidate overlap advisory =="
cross="$tmp/cross-repo"; mkdir -p "$cross"; cd "$cross"
g init -q -b main .
g config user.email x@x; g config user.name x
mkdir -p docs src docs/audits
cat > docs/generated-paths.txt <<'DECL'
# path	author
docs/audits/report.md	artifacts
DECL
printf 'root\n' > root.txt
g add -A; g commit -qm root

cross_dir="$tmp/cross-state"; mkdir -p "$cross_dir"
export HV_SLUICE_DIR="$cross_dir"
export HV_SLUICE_BASE=main
export HV_SLUICE_ALLOW_UNPUSHED=1

# --- 1. a genuine SOURCE collision between two live queue rows -------------
# Both branches ADD the same new file from main's tip with different content:
# an add/add conflict between them, while each merges main CLEANLY on its own
# (main has neither), so the mouth's own base-only verdict would ADMIT.
g checkout -q -b campaign/candidate main
mkdir -p src
printf 'candidate version\n' > src/shared.rs
g add -A; g commit -qm candidate-adds-shared
CAND1="$(g rev-parse campaign/candidate)"

# git prunes a directory left empty by the checkout above (main has no
# src/shared.rs), so it must be recreated before writing into it again.
g checkout -q -b campaign/other main
mkdir -p src
printf 'other version\n' > src/shared.rs
g add -A; g commit -qm other-adds-shared
OTHER1="$(g rev-parse campaign/other)"
g checkout -q campaign/candidate

bash "$repo_root/scripts/sluice-queue.sh" add campaign/other "$OTHER1" merge >/dev/null

set +e
cross1_out="$(bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate "$CAND1" 2>"$tmp/cross1.err")"
cross1_rc=$?
set -e
if [ "$cross1_rc" -eq 0 ]; then
    ok "cross-candidate: the candidate still ADMITs (exit 0) despite a queue-row collision"
else
    bad "cross-candidate: expected exit 0 despite the collision, got $cross1_rc"
fi
if printf '%s\n' "$cross1_out" | grep -q "ADMIT campaign/candidate $CAND1"; then
    ok "cross-candidate: the ADMIT message itself is unaffected by the advisory"
else
    bad "cross-candidate: ADMIT message missing/wrong: '$cross1_out'"
fi
if grep -q "OVERLAP — campaign/other (queued)" "$tmp/cross1.err" \
   && grep -q "overlap: *src/shared.rs" "$tmp/cross1.err"; then
    ok "cross-candidate: the advisory names the colliding branch, its state, and the conflicting path"
else
    bad "cross-candidate: no OVERLAP advisory found: $(cat "$tmp/cross1.err")"
fi
if grep -qi "this is ADVISORY" "$tmp/cross1.err"; then
    ok "cross-candidate: the advisory labels itself advisory, not a verdict"
else
    bad "cross-candidate: missing the ADVISORY disclaimer: $(cat "$tmp/cross1.err")"
fi

# --- 2. THE ANTI-NOISE CASE: a collision ONLY in a declared-artifacts path --
# Same shape as (1), but both branches touch only docs/audits/report.md,
# declared `artifacts` in docs/generated-paths.txt above. No advisory at all:
# the chamber resolves this kind of conflict by regeneration on every run,
# and six real candidates were bounced over exactly this file in one session
# before sluice_is_regenerated_only existed.
g checkout -q -b campaign/candidate2 main
mkdir -p docs/audits
printf 'candidate report v2\n' > docs/audits/report.md
g add -A; g commit -qm candidate2-touches-report
CAND2="$(g rev-parse campaign/candidate2)"

g checkout -q -b campaign/other2 main
mkdir -p docs/audits
printf 'other report v2\n' > docs/audits/report.md
g add -A; g commit -qm other2-touches-report
OTHER2="$(g rev-parse campaign/other2)"
g checkout -q campaign/candidate2

bash "$repo_root/scripts/sluice-queue.sh" add campaign/other2 "$OTHER2" merge >/dev/null

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate2 "$CAND2" >/dev/null 2>"$tmp/cross2.err"
cross2_rc=$?
set -e
if [ "$cross2_rc" -eq 0 ]; then
    ok "cross-candidate anti-noise: still ADMITs"
else
    bad "cross-candidate anti-noise: expected exit 0, got $cross2_rc"
fi
if ! grep -q "OVERLAP" "$tmp/cross2.err"; then
    ok "cross-candidate anti-noise: an artifacts-only collision prints NO advisory"
else
    bad "cross-candidate anti-noise: OVERLAP fired on a regenerated-only collision: $(cat "$tmp/cross2.err")"
fi

# --- 2b. THE MIXED COLLISION: artifacts paths must not be listed ------------
# Case 2 above collides ONLY in artifacts paths and is silenced. That is the
# easy half, and it is the half that was already right. The advisory judged
# the set AS A WHOLE — `sluice_is_regenerated_only` returns 0 only if EVERY
# member qualifies — so a collision in one hand-authored file plus two
# generated ones printed all three under the heading "source path(s)".
#
# Observed on its first production firing, 2026-09-05: campaign/the-lot vs
# campaign/the-warp listed docs/audits/type-audit-report.md beside one real
# file. The advisory must name the real path, count only it, and say the rest
# exist without listing them.
# The declaration must live on BOTH refs, so it goes onto main before either
# branch exists. This mirrors production exactly: a directory row saying
# `artifacts`, and a more specific FILE row overriding it to none() — which is
# how docs/audits/campaign-reconciliation.tsv is declared.
g checkout -q main
mkdir -p docs/audits
cat > docs/generated-paths.txt <<'DECL2B'
# path	author
docs/audits/report.md	artifacts
docs/audits/	artifacts
docs/audits/handmade2b.tsv	none(hand-authored by each campaign; never regenerated)
DECL2B
printf 'base hand 2b\n' > docs/audits/handmade2b.tsv
g add -A; g commit -qm main-declares-handmade2b

g checkout -q -b campaign/candidate2b main
mkdir -p docs/audits src
printf 'candidate report 2b\n' > docs/audits/report.md
printf 'candidate hand 2b\n' > docs/audits/handmade2b.tsv
printf 'candidate src 2b\n' > src/mixed.rs
g add -A; g commit -qm candidate2b-touches-all-three
CAND2B="$(g rev-parse campaign/candidate2b)"

g checkout -q -b campaign/other2b main
mkdir -p docs/audits src
printf 'other report 2b\n' > docs/audits/report.md
printf 'other hand 2b\n' > docs/audits/handmade2b.tsv
printf 'other src 2b\n' > src/mixed.rs
g add -A; g commit -qm other2b-touches-all-three
OTHER2B="$(g rev-parse campaign/other2b)"
g checkout -q campaign/candidate2b

bash "$repo_root/scripts/sluice-queue.sh" add campaign/other2b "$OTHER2B" merge >/dev/null

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate2b "$CAND2B" >/dev/null 2>"$tmp/cross2b.err"
cross2b_rc=$?
set -e
if [ "$cross2b_rc" -eq 0 ]; then
    ok "mixed collision: still ADMITs (the advisory never changes the exit code)"
else
    bad "mixed collision: expected exit 0, got $cross2b_rc"
fi
if grep -q "overlap: *src/mixed.rs" "$tmp/cross2b.err"; then
    ok "mixed collision: the genuine source path IS listed"
else
    bad "mixed collision: the real path was not reported: $(cat "$tmp/cross2b.err")"
fi
if ! grep -q "overlap: *docs/audits/report.md" "$tmp/cross2b.err"; then
    ok "mixed collision: THE REGRESSION — an artifacts-authored path is NOT listed as a source overlap"
else
    bad "mixed collision: docs/audits/report.md was listed as a source path: $(cat "$tmp/cross2b.err")"
fi
if grep -q "overlap: *docs/audits/handmade2b.tsv" "$tmp/cross2b.err"; then
    ok "mixed collision: a file declared hand-authored by BOTH refs is listed"
else
    bad "mixed collision: the hand-authored file was suppressed as noise: $(cat "$tmp/cross2b.err")"
fi
if grep -qE "OVERLAP — campaign/other2b \(queued\): 2 source path" "$tmp/cross2b.err"; then
    ok "mixed collision: the COUNT is of real paths only, not of the whole collision"
else
    bad "mixed collision: wrong count in the heading: $(grep OVERLAP "$tmp/cross2b.err")"
fi
if grep -q "1 artifacts-authored path(s) also collide" "$tmp/cross2b.err"; then
    ok "mixed collision: the suppressed paths are ACKNOWLEDGED by count, not silently dropped"
else
    bad "mixed collision: no note that artifacts paths were omitted: $(cat "$tmp/cross2b.err")"
fi

# --- 2c. THE ASYMMETRIC DECLARATION -----------------------------------------
# Everything above declares the same thing on both refs, so it pins the RULE
# and not its INPUTS — the precise blind spot that let the mouth and the
# chamber disagree earlier today while sharing one function and passing an
# agreement test. Here the two refs disagree: the candidate overrides the
# directory row to none(), the queued row inherits `artifacts`. "Both must
# agree before it counts as noise" means this path stays listed.
g checkout -q main
printf 'base asym\n' > docs/audits/asym.md
g add -A; g commit -qm main-adds-asym

# THE DIRECTION MATTERS AND THE FIRST DRAFT GOT IT WRONG. Putting the none()
# override on the CANDIDATE does not discriminate: both the real rule and a
# candidate-only rule refuse to call it noise, so the test passed against a
# mutant that consulted one ref. The override therefore lives on the QUEUED
# ROW, where the candidate reads `artifacts` and only the other side objects.
g checkout -q -b campaign/candidate2c main
printf 'candidate asym\n' > docs/audits/asym.md
g add -A; g commit -qm candidate2c-touches-asym-inheriting-artifacts
CAND2C="$(g rev-parse campaign/candidate2c)"

g checkout -q -b campaign/other2c main
cat > docs/generated-paths.txt <<'DECL2C'
# path	author
docs/audits/report.md	artifacts
docs/audits/	artifacts
docs/audits/handmade2b.tsv	none(hand-authored by each campaign; never regenerated)
docs/audits/asym.md	none(this campaign says a human writes it)
DECL2C
printf 'other asym\n' > docs/audits/asym.md
g add -A; g commit -qm other2c-declares-asym-hand-authored
OTHER2C="$(g rev-parse campaign/other2c)"
g checkout -q campaign/candidate2c

bash "$repo_root/scripts/sluice-queue.sh" add campaign/other2c "$OTHER2C" merge >/dev/null

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate2c "$CAND2C" >/dev/null 2>"$tmp/cross2c.err"
cross2c_rc=$?
set -e
if [ "$cross2c_rc" -eq 0 ]; then
    ok "asymmetric declaration: still ADMITs"
else
    bad "asymmetric declaration: expected exit 0, got $cross2c_rc"
fi
if grep -q "overlap: *docs/audits/asym.md" "$tmp/cross2c.err"; then
    ok "asymmetric declaration: the QUEUED ROW calling it hand-authored keeps it listed, though the candidate reads artifacts"
else
    bad "asymmetric declaration: suppressed on the strength of ONE ref's declaration — the refs were not both consulted: $(cat "$tmp/cross2c.err")"
fi

# --- 3. no collision at all: total silence ----------------------------------
g checkout -q -b campaign/candidate3 main
mkdir -p src
printf 'candidate three\n' > src/only_candidate.rs
g add -A; g commit -qm candidate3-file
CAND3="$(g rev-parse campaign/candidate3)"

g checkout -q -b campaign/other3 main
mkdir -p src
printf 'other three\n' > src/only_other.rs
g add -A; g commit -qm other3-file
OTHER3="$(g rev-parse campaign/other3)"
g checkout -q campaign/candidate3

bash "$repo_root/scripts/sluice-queue.sh" add campaign/other3 "$OTHER3" merge >/dev/null

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate3 "$CAND3" >/dev/null 2>"$tmp/cross3.err"
cross3_rc=$?
set -e
if [ "$cross3_rc" -eq 0 ] && [ ! -s "$tmp/cross3.err" ]; then
    ok "cross-candidate: a non-colliding queue row produces no advisory output at all — silence is the common case"
else
    bad "cross-candidate: expected exit 0 and empty stderr, got rc=$cross3_rc stderr=$(cat "$tmp/cross3.err")"
fi

# --- 4. an unresolvable sha in a queue row: a printed note, not a false
#        "no conflict", and the check still completes for the other rows ---
BOGUS_SHA="deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
g checkout -q -b campaign/candidate4 main
mkdir -p src
printf 'candidate four\n' > src/shared4.rs
g add -A; g commit -qm candidate4-file
CAND4="$(g rev-parse campaign/candidate4)"

g checkout -q -b campaign/other4 main
mkdir -p src
printf 'other four\n' > src/shared4.rs
g add -A; g commit -qm other4-file
OTHER4="$(g rev-parse campaign/other4)"
g checkout -q campaign/candidate4

bash "$repo_root/scripts/sluice-queue.sh" add campaign/ghost "$BOGUS_SHA" merge >/dev/null
bash "$repo_root/scripts/sluice-queue.sh" add campaign/other4 "$OTHER4" merge >/dev/null

set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate4 "$CAND4" >/dev/null 2>"$tmp/cross4.err"
cross4_rc=$?
set -e
if [ "$cross4_rc" -eq 0 ]; then
    ok "cross-candidate: an unresolvable queue row does not abort the check (still exits 0)"
else
    bad "cross-candidate: expected exit 0, got $cross4_rc"
fi
if grep -qi "cannot resolve" "$tmp/cross4.err" && grep -q "campaign/ghost" "$tmp/cross4.err"; then
    ok "cross-candidate: the unresolvable row is named in a printed note, never silently read as 'no conflict'"
else
    bad "cross-candidate: no 'cannot resolve' note for the bogus row: $(cat "$tmp/cross4.err")"
fi
if grep -q "OVERLAP — campaign/other4" "$tmp/cross4.err"; then
    ok "cross-candidate: the check still completes for the resolvable row after skipping the bogus one"
else
    bad "cross-candidate: the resolvable colliding row was not reported: $(cat "$tmp/cross4.err")"
fi

# --- 5. THE EXIT-CODE INVARIANT, isolated as its own assertion -------------
# This is the constraint most likely to be broken by a later edit: the
# advisory must NEVER change the exit code. Scenario 1 above already collides
# and already exits 0; restate it as its own named check so a regression
# here fails on a line that says exactly what broke.
if [ "$cross1_rc" -eq 0 ]; then
    ok "cross-candidate: THE INVARIANT — an admissible candidate with a live overlap still exits 0, unconditionally"
else
    bad "cross-candidate: THE INVARIANT BROKE — overlap advisory changed the exit code to $cross1_rc"
fi

# --- 6. THE STALE-DECLARATION CASE -----------------------------------------
# The mouth used to classify a conflict against docs/generated-paths.txt as it
# sits in the WORKING TREE. The chamber classifies against the merge product.
# Those are the same file only when the checkout happens to be current, and on
# 2026-09-05 it was not: this box's main checkout sat five commits behind
# origin/main, its copy lacked the row declaring
# docs/audits/campaign-reconciliation.tsv hand-authored, the classifier fell
# back to the docs/audits/ DIRECTORY row and said `artifacts`, and the mouth
# printed ADMIT. The chamber refused the same sha three seconds later.
#
# WHY THE EXISTING AGREEMENT CASE COULD NOT CATCH IT. Cases 1-5 above, and the
# shared sluice_is_regenerated_only they exercise, vary the CONFLICT and hold
# the DECLARATION fixed — one file, read by both callers. The divergence was
# never in the logic; it was in which copy of the declaration each side read.
# So this case varies exactly that: main and the candidate agree, and the
# working tree disagrees with both.
g checkout -q main
mkdir -p docs/audits
cat > docs/generated-paths.txt <<'DECL'
# path	author
docs/audits/report.md	artifacts
docs/audits/	artifacts
docs/audits/handmade.tsv	none(hand-authored by each campaign at close; never regenerated)
DECL
printf 'base row\n' > docs/audits/handmade.tsv
g add -A; g commit -qm main-declares-handmade-hand-authored

g checkout -q -b campaign/candidate6 main
printf 'candidate row\n' > docs/audits/handmade.tsv
g add -A; g commit -qm candidate6-appends-its-row

g checkout -q main
printf 'base row v2\n' > docs/audits/handmade.tsv
g add -A; g commit -qm main-appends-another-row
g checkout -q campaign/candidate6
CAND6="$(g rev-parse campaign/candidate6)"

# Both refs declare the file hand-authored, so the honest verdict is REFUSE.
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate6 "$CAND6" >/dev/null 2>"$tmp/cross6.err"
cross6_rc=$?
set -e
if [ "$cross6_rc" -eq 1 ]; then
    ok "stale-declaration: a conflict in a file BOTH refs declare hand-authored is refused"
else
    bad "stale-declaration: expected exit 1, got $cross6_rc: $(cat "$tmp/cross6.err")"
fi

# Now stale ONLY the working tree, exactly as a behind-by-N checkout does:
# drop the file row so the directory row `artifacts` is what a working-tree
# reader finds. The refs are untouched, so the verdict must not move.
grep -v '^docs/audits/handmade.tsv	' docs/generated-paths.txt > "$tmp/decl.staled"
cp "$tmp/decl.staled" docs/generated-paths.txt
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate6 "$CAND6" >/dev/null 2>"$tmp/cross6b.err"
cross6b_rc=$?
set -e
if [ "$cross6b_rc" -eq 1 ]; then
    ok "stale-declaration: THE REGRESSION — a working tree missing the file row does not flip the verdict to ADMIT"
else
    bad "stale-declaration: a staled working tree flipped the verdict to $cross6b_rc — the mouth is reading the checkout again, not the refs: $(cat "$tmp/cross6b.err")"
fi
g checkout -q -- docs/generated-paths.txt

# THE ANTI-VACUITY CONTROL. Everything above asserts a refusal; a mouth that
# refused unconditionally would pass both. A conflict in a genuinely
# artifacts-authored file must still ADMIT, with the staled tree restored.
g checkout -q main
printf 'base report\n' > docs/audits/report.md
g add -A; g commit -qm main-touches-report
g checkout -q -b campaign/candidate7 main~1
printf 'candidate report\n' > docs/audits/report.md
g add -A; g commit -qm candidate7-touches-report
CAND7="$(g rev-parse campaign/candidate7)"
set +e
bash "$repo_root/scripts/sluice-mouth.sh" campaign/candidate7 "$CAND7" >/dev/null 2>"$tmp/cross7.err"
cross7_rc=$?
set -e
if [ "$cross7_rc" -eq 0 ]; then
    ok "stale-declaration anti-vacuity: an artifacts-authored conflict still ADMITs — the refusals above are discriminating"
else
    bad "stale-declaration anti-vacuity: expected exit 0, got $cross7_rc: $(cat "$tmp/cross7.err")"
fi

unset HV_SLUICE_ALLOW_UNPUSHED
export HV_SLUICE_DIR="$tmp/state"
export HV_SLUICE_BASE=main

# ---------------------------------------------------------------------------
# THE BLAST-RADIUS GUARD. Every other assertion in this file is about what the
# queue DID; this one is about what the suite MUST NOT TOUCH. Last, so it sees
# the whole run.
# ---------------------------------------------------------------------------
echo "== suite: the real repository was never touched"
real_head_at_end="$(env -u GIT_DIR -u GIT_INDEX_FILE git -C "$repo_root" rev-parse HEAD 2>/dev/null || echo unknown)"
if [ "$real_head_at_start" = "unknown" ] || [ "$real_head_at_end" = "unknown" ]; then
    bad "could not read the real repo's HEAD — the blast-radius guard did not run, which is not the same as passing"
elif [ "$real_head_at_start" = "$real_head_at_end" ]; then
    ok "the real repo's HEAD is unmoved ($real_head_at_start) — no test escaped its scratch"
else
    bad "THE SUITE MOVED THE REAL REPO: $real_head_at_start -> $real_head_at_end. A test is pointing HV_SLUICE_REPO_ROOT (or a git -C) at the live checkout; inside the chamber that discards the merge product and the run still reports green."
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
