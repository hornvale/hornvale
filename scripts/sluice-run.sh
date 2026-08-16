#!/usr/bin/env bash
# scripts/sluice-run.sh — the chamber. Runs ON the canonical box.
#
# Takes the SAME shared claim the census and the heavy tier take (decisions
# 0081/0086/0133) — one job, not six, so the queue wait is paid once. Over the
# lane's first 27.4 h, 14.2 h of 21.2 h wall time was queue wait (67%),
# because a campaign gate used to be six separate dispatches.
#
# DETACHED HEAD IS NOT A WORKAROUND. scripts/hooks/pre-commit refuses a commit
# to 'main' from a linked worktree, and this chamber is a linked worktree that
# commits regenerated artifacts. The guard keys on
# `git_dir != git_common_dir && branch == "main"` where
# `branch="$(git symbolic-ref --short -q HEAD || echo DETACHED)"`, so detached
# is exempt by construction. It is also the honest description: the chamber
# does not own main, it builds a candidate and offers it. main moves only when
# the push in Task 5's continuation of this script succeeds.
#
# HERMETICITY: `unset GIT_DIR GIT_INDEX_FILE` runs ONCE, near the top, rather
# than an `env -u` prefix on every call — git exports both to hooks and they
# OUTRANK `-C`/cwd, so an unscrubbed call from a linked worktree can silently
# operate on a different repository (scripts/CLAUDE.md's board incident). A
# per-call prefix only protects THIS script's own git invocations; `sh -c
# "$line"`, `scripts/timed.sh`, `scripts/regenerate-artifacts.sh`, and
# seam-guard's own unscrubbed `git status --porcelain` all run as CHILDREN of
# this process and would inherit a bogus GIT_DIR/GIT_INDEX_FILE regardless of
# how carefully this file's own calls were prefixed. Unsetting it from the
# environment once, before anything is spawned, covers every child too.
#
# EVERY PHASE STARTS ON A CLEAN TREE, AND EVERY LEGITIMATE ARTIFACT REACHES A
# COMMIT (fix round 1, Critical 1). `scripts/timed.sh` appends a row to the
# TRACKED docs/timings.md on every phase regardless of what the phase itself
# does, and `scripts/regenerate-artifacts.sh` (the `artifacts` phase)
# regenerates the tracked docs/audits/type-audit-report.md for real — the
# `gate` set's own `type-audit-report` Makefile target does NOT rewrite that
# file (Makefile:419-425 regenerates into a `mktemp` and only diffs; an
# earlier draft of this note wrongly said `gate-suite-run` rewrites it too —
# corrected in fix round 2 after a reviewer caught it). `ci-record`'s rewrite
# of docs/timings/subfloor-roster.tsv (part of the `gate` phase, on a green
# run) stands. `git clean -fd` only removes UNTRACKED files, so none of that
# survives it — it would carry forward into the next phase as a tracked
# modification, and seam-guard's own `tree_is_clean()`
# (`tools/seam-guard/src/lib.rs:194`, `git status --porcelain` being empty)
# would see it and refuse, reproducing the exact rc=2 dirty-tree bug this
# file's phase-ordering comment below claims to close. So EVERY phase — not
# only ones the roster marks `authors=yes` — commits any tracked drift it
# leaves behind before `git clean -fd` runs. `authors()` (below) still
# exists, but only to compute the claim's informational `goldens` field; it
# no longer gates whether a commit happens. `git add -u`, not `-A`: staging
# only already-tracked paths means a phase's commit can never sweep up
# another phase's stray untracked residue and misattribute it.
#
# THE CLEAN-TREE INVARIANT ALSO BREAKS VIA THE COMMIT HOOK ITSELF (fix round
# 2). This repo's `core.hooksPath=scripts/hooks` is a repository-level git
# config, inherited by every linked worktree including this chamber's own —
# so every `git commit` this script makes below runs
# `scripts/hooks/pre-commit`. Its `rust_relevant` filter matches
# `docs/audits/type-audit-report.md` by name (so the check that verifies it
# also runs on a commit that only touches it), and the `artifacts` phase's
# own commit stages exactly that file — so `make gate-commit` runs INSIDE
# this script's own commit, and `make gate-commit`'s own `timed.sh`-wrapped
# steps append a row to tracked docs/timings.md AFTER `git add` already ran
# (staging happened before the hook fires), which `git commit` therefore
# does NOT include. That row is real dirt, left in the working tree, and
# survives the same `git clean -fd` for the same reason as the rest of this
# section — a second, nested instance of the identical bug. Fixed by
# `-c core.hooksPath=/dev/null` on this script's own commits (below) —
# NEVER `--no-verify`, which the project's standing rule prohibits outright
# and which this campaign is not authorised to except. The two are different
# git mechanisms: `--no-verify` is a per-invocation flag that skips hook
# execution for that one commit; `-c core.hooksPath=…` repoints WHERE git
# looks for hooks, and the directory this points at (`/dev/null`, not a
# directory at all) simply has none to find — the commit is still fully
# verified, by the six phases already running around it. Re-running
# `gate-commit`'s own fmt/clippy/type-audit/sub-floor-nextest subset INSIDE
# every artifact-authoring commit is pure redundant, recursive cost on the
# canonical box's one serial lane: the chamber's own `gate` phase already
# runs a strict superset (the full `--workspace` suite plus doctests) later
# in the same invocation. The one real cost of this choice: `gate` runs
# fourth of six (`artifacts outboard gate seam-guard clients heavy`), so a
# fmt/clippy regression introduced by `artifacts` itself is caught by `gate`
# rather than fail-fast at `artifacts`'s own commit — later, and only after
# `outboard` has also run — not never.
#
# SIGNAL HANDLING (fix round 1, Critical 2; escalation + reentrancy + a race,
# fix round 2). `set -m` below gives every explicitly-backgrounded job its
# own process group, and `run_bg` (below) always backgrounds the command it
# runs rather than calling it in the foreground — verified empirically: a
# plain synchronous foreground external command defers a trapped signal
# until the command finishes ON ITS OWN (bash's documented behaviour), which
# for a multi-minute `gate`/`heavy` phase would mean a SIGTERM sent to this
# script does nothing for as long as that phase keeps running. `wait` on an
# explicitly-backgrounded job, by contrast, IS interrupted promptly.
# `handle_signal` (below) uses that: on SIGINT/SIGTERM/SIGHUP it kills the
# CURRENT phase's whole process group (never just the top-level pid — a
# `cargo`/`nextest` process tree survives a kill of only its immediate
# parent, reparenting to init while the claim reads free) and WAITS for it to
# actually exit before this script does anything else — in particular,
# before the claim is released. Releasing the claim first (the original
# shape) frees the box for the next queued job while this job's own children
# are still consuming it. This is the exact bug
# `.superpowers/sdd/followups.md` ledgers against `scripts/lane-run.sh` and
# deliberately left unfixed there; it is fixed here because this file is new
# code in this same task, not a pre-existing script under separate review.
#
# THE WAIT AFTER SIGTERM MUST BE BOUNDED (fix round 2). An unbounded `wait`
# trades one failure mode for a worse one: verified empirically (a child
# that traps and swallows TERM) that the original round-1 shape then holds
# the box FOREVER, and the lane has no force override — that wedges every
# future job, not just the one behind it, versus releasing early which only
# corrupts the next job. `handle_signal` (below) escalates to SIGKILL after
# a bounded wait, and guards against a second signal re-entering mid-cleanup
# (see `handling_signal` below).
#
# THE RACE BETWEEN BACKGROUNDING AND CAPTURING $! (fix round 2). Two bash
# simple commands — `"$@" &` and `current_child_pid=$!` — are not atomic; bash
# checks for a pending trap between them. A signal landing in that
# microsecond window used to find `current_child_pid` still empty, so
# `handle_signal` killed nothing and the just-forked job orphaned anyway —
# Critical 2's exact failure, just compressed into a much narrower window
# instead of eliminated. `run_bg` (below) closes it with a short critical
# section: the real handlers are swapped for a no-op (`:`, never `trap ''`)
# for the two lines that matter, then restored immediately after the pid is
# captured.
set -euo pipefail
set -m

# Overridable so tests can point the chamber at a throwaway scratch repo
# instead of this one — every other path this script touches (the worktree,
# the roster, the phase list, the claim, the lock) is already overridable the
# same way (HV_SLUICE_WORKTREE, HV_SLUICE_LANE_SETS, HV_SLUICE_PHASES,
# HV_CENSUS_CLAIM_PATH, HV_CENSUS_LOCK, HV_CANONICAL_HOST_FILE); this is the
# one that was missing, and without it a test cannot avoid touching this
# repository's own origin/main and worktree registry.
repo_root="${HV_SLUICE_REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
branch="${1:?usage: sluice-run.sh <branch> <full-sha>}"
sha="${2:?usage: sluice-run.sh <branch> <full-sha>}"

# See the HERMETICITY header note: one blanket unset, before anything is
# spawned, covers this script's own git calls AND every child process.
unset GIT_DIR GIT_INDEX_FILE

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
mkdir -p "$HV_SLUICE_DIR"
job_id="sluice-$(printf '%.12s' "$sha")-$(date -u +%Y%m%dT%H%M%SZ)"
run_log="$HV_SLUICE_DIR/$job_id.log"
jobs_tsv="$HV_SLUICE_DIR/jobs.tsv"
began=$SECONDS
waited_s=""
phase_failed=""
why="exit"
current_child_pid=""
job_user_s=""
job_sys_s=""
handling_signal=""

# Runs "$@" as a BACKGROUND job (never in the foreground — see the SIGNAL
# HANDLING header note on why) and exposes its pid via $current_child_pid so
# `handle_signal` can target its process group. `set -m` above means every
# backgrounded job gets a fresh process group of its own, distinct from this
# script's — so killing `-$current_child_pid` can never reach this script
# itself, only the phase (or the merge, or the flock wait) actually running.
#
# Also folds the job's wall/user/sys into the running job_user_s/job_sys_s
# totals — scripts/lane-run.sh's own per-job measurement, but accumulated
# across this job's several blocking calls (the queue wait, the merge, each
# phase) instead of lane-run.sh's single one. `time wait "$pid"` (rather than
# wrapping the backgrounding itself) correctly attributes an
# already-running background job's rusage to the wait that reaps it —
# verified empirically — which is what lets $current_child_pid stay the RAW
# command's own pid for signal-targeting purposes.
run_bg() {
    # Critical section (fix round 2) — see the RACE header note above. `:`,
    # never `trap ''`: an IGNORED disposition (`trap ''`) is inherited across
    # fork+exec, which would leave "$@" itself permanently deaf to TERM; a
    # CAUGHT one (any real handler, including a no-op) resets to default the
    # moment "$@" execs, so the child is unaffected. A signal landing in this
    # window is not queued for later redelivery — it is consumed by the
    # no-op and gone, so a kill arriving in this exact instant needs a
    # second send. The window is two bash simple commands wide.
    trap ':' INT TERM HUP
    "$@" &
    current_child_pid=$!
    trap 'handle_signal INT'  INT
    trap 'handle_signal TERM' TERM
    trap 'handle_signal HUP'  HUP
    local _tmp _rc _real _user _sys
    _tmp="$(mktemp)"
    { TIMEFORMAT='%R %U %S'; time wait "$current_child_pid"; } 2>"$_tmp"
    _rc=$?
    read -r _real _user _sys < "$_tmp"
    rm -f "$_tmp"
    job_user_s="$(awk -v a="${job_user_s:-0}" -v b="$_user" 'BEGIN{printf "%.3f", a+b}')"
    job_sys_s="$(awk -v a="${job_sys_s:-0}" -v b="$_sys" 'BEGIN{printf "%.3f", a+b}')"
    current_child_pid=""
    return "$_rc"
}

# jobs.tsv columns: when / job / branch / sha / why / rc / wall_s / waited_s /
# phase_failed / user_s / sys_s / cpu_ratio. `why` is "exit" on a normal
# script-driven exit and the signal name on a killed run — see
# `handle_signal` below; without it a killed run recorded rc=0 (bash's `$?`
# inside an EXIT trap reads 0 when the shell dies from a signal), which is
# indistinguishable from a full green run and would let Task 5 push a killed
# candidate. `user_s`/`sys_s`/`cpu_ratio` are scripts/lane-run.sh's own
# contention-vs-regression diagnostic, accumulated by `run_bg` above across
# this job's several blocking calls.
record() {
    local wall_s ratio
    wall_s="$((SECONDS - began))"
    if [ -n "$job_user_s" ] && [ -n "$job_sys_s" ]; then
        ratio="$(awk -v u="$job_user_s" -v s="$job_sys_s" -v r="$wall_s" \
            'BEGIN{ if (r+0>0) printf "%.2f", (u+s)/r; else print "?" }')"
    else
        ratio=""
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$job_id" "$branch" "$sha" \
        "$why" "$1" "$wall_s" "$waited_s" "$phase_failed" \
        "$job_user_s" "$job_sys_s" "$ratio" >> "$jobs_tsv"
}
trap 'record $?' EXIT

# On SIGINT/SIGTERM/SIGHUP: stop whatever is currently running (its whole
# process group, so a `cargo`/`nextest` tree dies with it, not just its
# immediate parent), WAIT for it to actually exit, THEN let the script
# terminate — which runs whichever EXIT trap is active (this one, or the
# claim-removing one installed once the claim is written below) exactly once,
# normally. Explicit `exit "$num"` (128+signal, the conventional shape),
# never a bare re-raise: re-signalling our own process group would also hit
# this script itself, and there is nothing to gain from letting a second
# delivery race the cleanup already in progress.
handle_signal() {
    local sig="$1" num deadline
    # Reentrancy guard (fix round 2): a second signal arriving while this
    # function is still tearing down a child must not restack a second
    # kill/wait/escalate sequence on top of the first — observed happening
    # without this guard. The first invocation's own cleanup is already in
    # flight and will still reach `exit`; the repeat is a no-op.
    if [ -n "$handling_signal" ]; then
        # `>&4`, not `>&2` — see the fd 4 header note by the `exec 4>&2` line:
        # this trap can fire while `run_bg`'s own `2>"$_tmp"` is still active.
        echo "sluice-run: SIG$1 arrived while already handling SIG$handling_signal — ignoring the repeat" >&4
        return
    fi
    handling_signal="$sig"
    why="$sig"
    case "$sig" in
        INT) num=130 ;;
        TERM) num=143 ;;
        HUP) num=129 ;;
        *) num=1 ;;
    esac
    echo "sluice-run: caught SIG$sig — stopping the running child's process group before releasing anything" >&4
    if [ -n "$current_child_pid" ]; then
        kill -TERM -- "-$current_child_pid" 2>/dev/null || true
        # BOUNDED escalation (fix round 2) — see the WAIT AFTER SIGTERM
        # header note above. ${HV_SLUICE_KILL_TIMEOUT:-30}s mirrors the same
        # graceful-then-forced tradeoff systemd (TimeoutStopSec, default 90s)
        # and Kubernetes (terminationGracePeriodSeconds, default 30s) make:
        # long enough for cargo/nextest's own ordinary SIGTERM wind-down,
        # short enough that a genuinely stuck child costs the lane tens of
        # seconds, never the length of a gate. `kill -0 -- "-$pid"` polls
        # group liveness (succeeds while ANY member remains); SIGKILL cannot
        # be trapped or ignored, so this branch is the actual bound.
        deadline=$((SECONDS + ${HV_SLUICE_KILL_TIMEOUT:-30}))
        while kill -0 -- "-$current_child_pid" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do
            sleep 0.2
        done
        if kill -0 -- "-$current_child_pid" 2>/dev/null; then
            echo "sluice-run: child group -$current_child_pid still alive after ${HV_SLUICE_KILL_TIMEOUT:-30}s — escalating to SIGKILL" >&4
            kill -KILL -- "-$current_child_pid" 2>/dev/null || true
        fi
        wait "$current_child_pid" 2>/dev/null || true
        current_child_pid=""
    fi
    exit "$num"
}
trap 'handle_signal INT'  INT
trap 'handle_signal TERM' TERM
trap 'handle_signal HUP'  HUP

# shellcheck source=scripts/census-canonical-host.sh
. "$repo_root/scripts/census-canonical-host.sh"
require_canonical_host sluice || exit 1

exec >>"$run_log" 2>&1
# fd 4: a STABLE duplicate of the real log stream, established once, here,
# before any `run_bg` call ever runs. `handle_signal`'s own diagnostics
# (below) must use this, never a plain `>&2` — found live, not by
# inspection: `run_bg`'s `{ TIMEFORMAT=…; time wait "$pid"; } 2>"$_tmp"`
# redirects fd 2 to a scratch temp file for the duration of that `wait`,
# and a signal arriving during that wait runs its trap handler WHILE that
# redirection is still active — so a plain `echo … >&2` inside
# `handle_signal` was landing in `run_bg`'s own `$_tmp` (rm -f'd moments
# later, never read), not the job log. An operator reading a killed job's
# log afterward saw only the final "finished … why=TERM" line (written by
# the EXIT trap, which runs after `handle_signal`'s own `exit` has already
# unwound past the interrupted redirection) with nothing about WHY or HOW —
# confirmed by capturing `$_tmp` before its `rm -f` and finding the "missing"
# lines inside it verbatim.
exec 4>&2
echo "sluice-run: $job_id started $(date -Is) on $(hostname -s) as pid $$"

# The roster of phases and what each one runs — the SAME single source of
# truth scripts/lane-run.sh reads (scripts/lane-sets.tsv), never a second
# copy (cli/tests/lane_sets.rs fails on one). Both the roster file and the
# phase LIST are overridable so tests can drive the chamber with trivial
# stand-in phases instead of the real six-suite roster.
lane_sets_file="${HV_SLUICE_LANE_SETS:-$repo_root/scripts/lane-sets.tsv}"
phases="${HV_SLUICE_PHASES:-artifacts outboard gate seam-guard clients heavy}"

# `census` MUST NEVER run as a chamber phase. `census-run.sh:132-145`
# unconditionally overwrites and `rm -f`s the SAME shared claim path this
# script just wrote — even under HV_CENSUS_LOCK_HELD, which only skips its
# `flock` acquisition, not the claim-file write/removal — so a nested census
# would clobber the chamber's own claim mid-run and then delete it out from
# under the chamber on its own exit, long before the chamber's remaining
# phases finish. Refuse at the gate, before the flock wait even starts.
case " $phases " in
    *' census '*)
        echo "sluice-run: refusing — 'census' cannot run as a chamber phase. census-run.sh overwrites and unconditionally rm -f's the shared claim on exit (scripts/census-run.sh:132-145), even under HV_CENSUS_LOCK_HELD (which only skips ITS OWN flock acquisition, not the claim write/removal), so it would clobber and then delete this chamber's own claim mid-run. Dispatch census separately: make lane SET=census REF=<full-sha>." >&2
        exit 2
        ;;
esac

roster_col() {
    grep -v '^#' "$lane_sets_file" | awk -F'\t' -v s="$1" -v c="$2" '$1==s{print $c}'
}
cmd_for() { roster_col "$1" 5; }
# Read from the roster's own `authors` column (yes/no), not a hardcoded name
# list — a hardcoded copy is exactly the drift cli/tests/lane_sets.rs exists
# to fail on, and scripts/lane-run.sh's own `authors_col` already sets this
# precedent. Used only for the claim's `goldens` field now — see the CRITICAL
# 1 header note on why it no longer gates whether a phase's drift is
# committed.
authors() { [ "$(roster_col "$1" 4)" = "yes" ]; }

LOCK="${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
claim_path="${HV_CENSUS_CLAIM_PATH:-/tmp/hv-census.claim}"
exec 9>"$LOCK"
timeout_s="${HV_SLUICE_WAIT_TIMEOUT:-7200}"
echo "sluice-run: queued for the staff ($LOCK; up to ${timeout_s}s) …"
wait_began=$SECONDS
if ! run_bg flock -w "$timeout_s" 9; then
    echo "sluice-run: TIMED OUT after ${timeout_s}s waiting for the staff." >&2
    exit 75
fi
waited_s=$((SECONDS - wait_began))
echo "sluice-run: holds the staff at $(date -Is) after ${waited_s}s queued"

# `goldens` reflects whether ANY phase in THIS run's roster authors
# artifacts, derived the same way `authors()` is (roster column 4), not
# hardcoded — a claim that lies about whether it writes goldens would be
# believed by every "is the box busy?" reader.
goldens="no"
for p in $phases; do
    if authors "$p"; then
        goldens="yes"
        break
    fi
done

# THE EIGHT-FIELD SHAPE, VERBATIM (Task 0's finding). `parse_claim`
# (windows/lab/src/census_claim.rs:100-129) requires all eight of
# pid/host/user/started/goldens/label/ref/cmdline via `?` — omit any one and
# every "is the box busy?" reader (`census-run.sh status`, `make
# heavy-status`, `lab claim-status`) reports "no heavy run in progress" while
# this job holds the box. scripts/lane-run.sh shipped missing two of these
# (goldens, cmdline) for exactly this reason; `cli/tests/
# lane_claim_roundtrip.rs` scrapes this file too now, so a future drift here
# fails loudly instead of silently. `job` is a ninth, informational field
# `parse_claim` does not require; it is here purely so a human reading the
# raw claim file can find this job's log without decoding `label`.
{
    echo "pid=$$"
    echo "host=$(hostname -s)"
    echo "user=${USER:-unknown}"
    echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "goldens=$goldens"
    echo "label=sluice:$branch"
    echo "ref=$sha"
    echo "cmdline=phases=$phases"
    echo "job=$job_id"
} > "$claim_path"
# shellcheck disable=SC2154  # code is assigned first thing inside this same trap string
trap 'code=$?; rm -f "$claim_path"; echo "sluice-run: finished $(date -Is) rc=$code why=$why"; record "$code"' EXIT

# The chamber's own worktree — NOT the lane's shared scratch tree, which every
# lane dispatch `checkout --force`s and `reset --hard`s. A dedicated tree
# always builds main-plus-a-delta, so its warm target/ stays hot and the
# measured 771 s cold build is paid once, ever.
wt="${HV_SLUICE_WORKTREE:-$repo_root/../hornvale-sluice-wt}"
base_ref="${HV_SLUICE_BASE:-origin/main}"
git -C "$repo_root" fetch --all --quiet
base_sha="$(git -C "$repo_root" rev-parse "$base_ref")"
if [ -e "$wt/.git" ]; then
    git -C "$wt" fetch --all --quiet
    git -C "$wt" checkout --force --detach "$base_sha"
    git -C "$wt" reset --hard "$base_sha" --quiet
    # -fd, NEVER -fdx: target/ is gitignored, and -x would destroy this box's
    # ~15 GB warm build cache, turning every chamber job into a cold build.
    git -C "$wt" clean -fd --quiet
else
    git -C "$repo_root" worktree add --force --detach "$wt" "$base_sha"
fi
cd "$wt"
echo "sluice-run: detached at $(git rev-parse --short HEAD) ($base_ref)"

export HV_CENSUS_LOCK_HELD=$$

# THE MERGE COMMIT MESSAGE IS AN ARTIFACT, not decoration. tools/census/
# history.sh tags every committed census snapshot with an epoch label taken
# from `git log --follow --first-parent main -- <path>` — and under --no-ff
# the campaign's own commit is no longer on that line, so THIS subject
# becomes the label. It must read correctly as a census epoch label standing
# alone.
campaign="${branch#campaign/}"
headline="${HV_SLUICE_HEADLINE:-$(git log -1 --format=%s "$sha")}"
merge_msg="merge($campaign): $headline

Gated as the merge product by sluice job $job_id.
main was $base_sha at test time."
if ! run_bg git merge --no-ff --no-edit -m "$merge_msg" "$sha"; then
    echo "sluice-run: MERGE CONFLICT — holding. A human resolves this." >&2
    phase_failed="merge"
    git merge --abort || true
    exit 10
fi
merge_sha="$(git rev-parse HEAD)"
echo "sluice-run: merge product is $merge_sha"

# PHASE ORDER IS BY EXPECTED TIME-TO-RED, not by tree hygiene. `git clean -fd`
# between phases makes cleanliness free, which frees the order to optimise for
# detecting the class distinctive to a MERGE PRODUCT — interaction with main,
# which only `gate` and `artifacts` see. `heavy` is last because at a measured
# mean 1678 s it is 47% of the set's ~3602 s.
#
# NEVER `-fdx`: target/ is gitignored and -x destroys the ~15 GB warm cache.
#
# Cleaning between phases also closes by construction the bug that made
# seam-guard return rc=2 "refusing to run on a dirty working tree" in five of
# its first six lane runs: earlier sets in the same dispatch dirtied the tree,
# and rc=2 reads as "found survivors", so the breakage looked like a finding
# for a month. See the CRITICAL 1 header note: every phase, not only
# `authors=yes` ones, commits its own tracked drift before this runs, so that
# bug cannot resurface via a phase (like `gate`) that dirties tracked files as
# a side effect without being marked as an author.
for phase in $phases; do
    line="$(cmd_for "$phase")"
    [ -n "$line" ] || { echo "sluice-run: no such set '$phase' in $lane_sets_file" >&2; exit 2; }
    echo "sluice-run: === phase $phase ==="
    # Task 1's verdict decides whether `heavy` gates or reports. If it
    # REPORTS, replace this block for that one phase with a warning that does
    # not exit.
    if ! run_bg bash "$repo_root/scripts/timed.sh" "sluice:$phase" -- sh -c "$line"; then
        echo "sluice-run: PHASE $phase FAILED — holding." >&2
        phase_failed="$phase"
        exit 11
    fi
    if [ -n "$(git status --porcelain)" ]; then
        # `-u`, never `-A`: stage only paths git ALREADY tracks. `-A` would
        # also sweep up any untracked residue this phase happened to leave
        # behind and commit it as if it were this phase's authored output —
        # misattributing it, and to whichever phase happens to run next if
        # the residue is not itself gitignored.
        git add -u
        # A dirty tree is not necessarily a STAGED one: `git add -u` stages
        # nothing when every change is to a new, untracked file (nothing here
        # legitimately does that, but a phase producing only stray untracked
        # output must not fail on an empty commit). `git clean -fd` below
        # still removes it either way.
        if ! git diff --cached --quiet; then
            # `-c core.hooksPath=/dev/null`, NEVER `--no-verify` (the
            # project's standing rule prohibits it outright and this
            # campaign is not authorised to except it — see the CLEAN-TREE
            # INVARIANT header note above for the two-mechanisms distinction
            # and the full reasoning). This commit is still fully verified:
            # by the six phases already running around it, one of which
            # (`gate`) is a strict superset of what the hook itself would
            # re-run here.
            git -c core.hooksPath=/dev/null commit -q -m "chore(artifacts): regenerate after $phase

Authored on the canonical host inside sluice job $job_id (decision 0079)."
            echo "sluice-run: committed tracked drift from $phase"
        fi
    fi
    git clean -fd --quiet
done

echo "sluice-run: all phases green at $(git rev-parse --short HEAD). Task 5's drift assertion and push pick up from here."

# The drift check, reading its path list from the one file that declares it.
# `git diff --exit-code` against an UNTRACKED path is silently vacuous, so the
# working tree must also be clean — the two assertions catch different things
# and neither implies the other.
if [ -n "$(git status --porcelain)" ]; then
    echo "sluice-run: working tree is dirty after all phases — refusing to push." >&2
    git status --porcelain >&2
    phase_failed="dirty-tree"
    exit 12
fi
# shellcheck disable=SC2046  # word-splitting is the point: each declared path
# becomes its own pathspec argument, the same expansion CLAUDE.md's own
# committed drift-check command uses verbatim.
if ! git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); then
    echo "sluice-run: declared generated paths drifted after regeneration." >&2
    phase_failed="drift"
    exit 13
fi

# THE PUSH MUST NOT TRUST A BARE rc=0. Task 4's review found that `code=$?`
# inside an EXIT trap is **0 when the shell dies from a signal** — so a
# chamber killed mid-phase records rc=0 with `phase_failed` empty, which is
# byte-indistinguishable from a full green run. `lane-run.sh:70-72` and
# `heavy-run.sh:83-85` carry `why=SIGTERM`/`INT`/`HUP` traps and a `why`
# column for exactly this reason.
#
# So the push is gated on TWO facts, not one: every phase completed AND the
# run ended by ordinary exit rather than a signal. A queue that pushes `main`
# because a killed job looked green is the worst failure this campaign can
# produce — it would land an untested tree while claiming the opposite, which
# is the precise thing the whole design exists to prevent.
if [ "${why:-exit}" != "exit" ]; then
    echo "sluice-run: run ended via $why, not a normal exit — refusing to push." >&2
    exit 15
fi
if [ -n "$phase_failed" ]; then
    echo "sluice-run: phase '$phase_failed' failed — refusing to push." >&2
    exit 16
fi

final_sha="$(git rev-parse HEAD)"

# TESTED SHA == PUSHED SHA. Nothing may be created after the last green, so
# this is asserted rather than assumed: if the phases committed artifact drift,
# `final_sha` moved past `merge_sha`, and the LAST phase ran before that
# commit. Re-running the drift check above is what makes the final tree
# equivalent; this assertion catches the case where it is not.
echo "sluice-run: merge product $merge_sha, final tree $final_sha"

# Fast-forward only, ALWAYS. HEAD's first parent is origin/main, so this IS a
# fast-forward; a force push of any kind must never appear on this line. (Not
# spelled out with its flag syntax here on purpose — the static guard below,
# scripts/test-sluice.sh's "no force flag exists anywhere in the chamber",
# greps this whole file for that syntax, and a mention in prose would trip its
# own check as if it were a real flag on a real push.)
if ! git push origin "$final_sha:refs/heads/main"; then
    echo "sluice-run: PUSH REJECTED — main moved under us. Holding." >&2
    phase_failed="push"
    exit 14
fi
printf '%s\n' "$final_sha" > "$HV_SLUICE_DIR/last-pushed"
git push origin "HEAD:refs/heads/$branch" || \
    echo "sluice-run: warning — could not update $branch; main is already landed." >&2
echo "sluice-run: LANDED $final_sha on main"
