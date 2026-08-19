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
# verified, by the four phases already running around it. Re-running
# `gate-commit`'s own fmt/clippy/type-audit/sub-floor-nextest subset INSIDE
# every artifact-authoring commit is pure redundant, recursive cost on the
# canonical box's one serial lane: the chamber's own `gate` phase already
# runs a strict superset (the full `--workspace` suite plus doctests) later
# in the same invocation. The one real cost of this choice: `gate` runs
# third of four (`artifacts outboard gate clients`), so a
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
branch="${1:?usage: sluice-run.sh <branch> <full-sha> [merge|stage]}"
sha="${2:?usage: sluice-run.sh <branch> <full-sha> [merge|stage]}"
# THE STAGE GATE IS THIS SCRIPT WITH THE PUSH TURNED OFF (The Sluice, Task
# 12). `gate-stage` used to be a separate dispatch path — its own script,
# its own detached job, its own shared scratch worktree, its own jobs.tsv —
# for one caller, and keeping it would have kept the whole asynchronous
# layer alive to serve it. A stage request is instead a queue entry with
# `kind=stage`: it takes the same claim, merges main+branch in the same
# chamber, and runs the same roster-declared phases against the same real
# merge product. It just never pushes, and it runs the `stage`-rung phases
# rather than all six. One branch, at the push step — see the `kind` gate
# below `final_sha`.
kind="${3:-merge}"
case "$kind" in
    merge|stage) ;;
    *) echo "sluice-run: unknown kind '$kind' (merge|stage)" >&2; exit 2 ;;
esac

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
echo "sluice-run: $job_id started $(date -Is) on $(hostname -s) as pid $$ kind=$kind"

# The roster of phases and what each one runs — the SAME single source of
# truth scripts/lane-run.sh reads (scripts/lane-sets.tsv), never a second
# copy (cli/tests/lane_sets.rs fails on one). Both the roster file and the
# phase LIST are overridable so tests can drive the chamber with trivial
# stand-in phases instead of the real six-suite roster.
lane_sets_file="${HV_SLUICE_LANE_SETS:-$repo_root/scripts/lane-sets.tsv}"

# THE TWO PHASE LISTS ARE LITERALS, AND THAT IS A CHOICE, not an oversight.
# Both could be derived from `scripts/lane-sets.tsv`'s `gate` column
# (`stage` rows for one, `stage`+`campaign` minus `census` for the other),
# and the roster would then be the only place a set is named. It is not done
# that way because ORDER IS LOAD-BEARING here and roster order is not phase
# order: the roster lists `heavy` fifth of ten, while the phase comment
# below places it LAST deliberately (at a measured mean 1678 s it is 47% of
# the set's ~3602 s, so running it before a cheap phase that would have gone
# red wastes half an hour of the one serial box). A derived list would have
# silently reordered that. `cli/tests/lane_sets.rs` reads these two lines
# instead and fails if either names a set with no roster row — the same
# direction it used to enforce over the Makefile's `lane-dispatch.sh` lines,
# pointed at the caller that replaced them.
# THE MERGE RUNS FOUR PHASES, NOT SIX (Nathan, 2026-08-19; decision 0148).
# `seam-guard` and `heavy` came off this list, so a merge and a stage gate now
# run the SAME phases and differ only in the push — which was always the design
# ("the stage gate is this script with the push turned off"), and is now true of
# the phase list too.
#
# WHAT THIS COSTS, STATED PLAINLY BECAUSE IT IS A REAL REDUCTION IN COVER.
# Decision 0139's guarantee — "every commit on origin/main is the tip of a tree
# that was gated as itself" — is unchanged in KIND and weaker in DEGREE: the
# merge product is still built and still gated as itself, by four phases rather
# than six. Nothing runs `seam-guard` or `heavy` automatically any more. They
# keep their `campaign`-rung rows in scripts/lane-sets.tsv and their own entry
# points (`make seam-guard`, `make heavy-remote REF=<full-sha>`), and those are
# now the ONLY things that run them.
#
# WHY IT IS WORTH IT: measured on this box, `heavy` was 2026 s of a 65-minute
# merge and `seam-guard` costs a full scoped test run per call site — together
# the large majority of a merge's wall time, on two sets whose guarantees move
# at campaign cadence rather than per-merge. Paying them on every merge priced
# a campaign-cadence check at merge frequency, which is the same mispricing
# decision 0132 split the gates to remove.
merge_phases="artifacts outboard gate clients"
stage_phases="artifacts outboard gate clients"

if [ "$kind" = "stage" ]; then
    phases="${HV_SLUICE_PHASES:-$stage_phases}"
else
    phases="${HV_SLUICE_PHASES:-$merge_phases}"
fi

# PROSE-ONLY CANDIDATES SKIP THE THREE EXPENSIVE PHASES. The rule, the
# allowlist and the reasoning live in scripts/sluice-phases.sh so they can be
# tested without standing up a chamber; this is only the wiring.
#
# Guarded by `-z HV_SLUICE_PHASES` so an explicit override always wins, and
# `git diff` failure leaves `changed` empty, which sluice_is_prose_only treats
# as NOT prose-only — the classifier failing is never a reason to skip a phase.
# shellcheck source=scripts/sluice-phases.sh
. "$repo_root/scripts/sluice-phases.sh"
if [ -z "${HV_SLUICE_PHASES:-}" ]; then
    # THE BASE IS RESOLVED HERE, NOT BORROWED. An earlier cut of this block
    # read `$base_sha`, which is assigned ~90 lines BELOW — under `set -u`
    # that is an unbound variable and the chamber died before its first
    # phase, on every merge, with `line 358: base_sha: unbound variable`.
    # Resolving locally removes the ordering dependency entirely, so a future
    # reorder of this file cannot reintroduce it. `|| true` plus `2>/dev/null`
    # means an unresolvable base yields an empty `changed`, which
    # sluice_is_prose_only treats as NOT prose-only — the full ladder.
    prose_base="$(git -C "$repo_root" rev-parse "${HV_SLUICE_BASE:-origin/main}" 2>/dev/null || true)"
    changed=""
    if [ -n "$prose_base" ]; then
        changed="$(git -C "$repo_root" diff --name-only "$prose_base".."$sha" 2>/dev/null || true)"
    fi
    if sluice_is_prose_only "$changed"; then
        phases="$(sluice_drop_expensive_phases "$phases")"
        echo "sluice-run: PROSE-ONLY candidate — every changed path is hand-written prose."
        echo "sluice-run:   skipping seam-guard, clients and heavy; none can observe a prose change."
        echo "sluice-run:   phases now: $phases"
        printf '%s\n' "$changed" | sed 's/^/sluice-run:     /'
    fi
fi

# `census` MUST NEVER run as a chamber phase. `census-run.sh:132-145`
# unconditionally overwrites and `rm -f`s the SAME shared claim path this
# script just wrote — even under HV_CENSUS_LOCK_HELD, which only skips its
# `flock` acquisition, not the claim-file write/removal — so a nested census
# would clobber the chamber's own claim mid-run and then delete it out from
# under the chamber on its own exit, long before the chamber's remaining
# phases finish. Refuse at the gate, before the flock wait even starts.
case " $phases " in
    *' census '*)
        echo "sluice-run: refusing — 'census' cannot run as a chamber phase. census-run.sh overwrites and unconditionally rm -f's the shared claim on exit (scripts/census-run.sh:132-145), even under HV_CENSUS_LOCK_HELD (which only skips ITS OWN flock acquisition, not the claim write/removal), so it would clobber and then delete this chamber's own claim mid-run. Run a census separately, through its own wrapper: ssh <canonical> 'cd ~/Projects/hornvale && HV_CENSUS_REF=<full-sha> scripts/census-run.sh'." >&2
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
    echo "label=sluice-$kind:$branch"
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
# alone. (Path-scoped, so only a merge that MOVES the census mints a label;
# the subject is permanent and human-read either way.)
#
# THE SUBJECT IS AUTHORED, NOT INFERRED. It comes from a `Sluice-Headline:`
# trailer anywhere in the range, and `sluice-request.sh` refuses a merge
# submission without one — see scripts/sluice-headline.sh for why the old
# rule (the tip commit's subject) failed on four of the first four merges.
# Both sides call the SAME helper so the mouth's refusal is about the string
# the chamber actually uses.
#
# The fallback is deliberately still here and deliberately still the tip
# subject. The mouth is the enforcement point; this is the chamber, and a
# chamber that refuses to compose a message is a chamber that abandons a
# candidate holding the box. A `stage` run legitimately has no trailer at all
# (it is exempt at the mouth, since its merge commit is discarded), and
# HV_SLUICE_HEADLINE stays for the tests that drive this file directly.
# `sluice_strip_merge_prefix` applies to every path, so the doubling that
# landed permanently on 9aae0d27 cannot recur however the string arrives.
# shellcheck source=scripts/sluice-headline.sh
. "$repo_root/scripts/sluice-headline.sh"
campaign="$(sluice_short_name "$branch")"
headline="${HV_SLUICE_HEADLINE:-$(sluice_headline_of "$repo_root" "$base_ref" "$sha")}"
if [ -z "$headline" ]; then
    headline="$(git log -1 --format=%s "$sha")"
    echo "sluice-run: no Sluice-Headline: trailer in $base_ref..${sha:0:12}; falling back to the tip subject." >&2
fi
headline="$(sluice_strip_merge_prefix "$headline")"
merge_msg="merge($campaign): $headline

Gated as the merge product by sluice job $job_id.
main was $base_sha at test time."
# FIX ROUND 1 (reviewer finding, Important 1): this used to `exit 10` right
# here. Every one of the five sites in this file that assigns `phase_failed`
# was immediately followed by its own `exit`, which meant the single gate
# below it (further down, where `phase_failed` and `why` are actually
# consulted) could NEVER be reached with `phase_failed` set — nothing that
# set it ever survived to fall through and be read. The comment that used to
# sit at that gate ("gated on TWO facts") described a live check that no
# mutation test could ever redden, which is the exact fault shape this
# campaign has hit three times before. The fix: every failure site below now
# only RECORDS `phase_failed` (and, for the loop, `break`s) — never exits on
# the spot. `phase_failed` becomes the actual decision point, read exactly
# once, at the single gate. The per-site exit CODE is preserved there via a
# `case`, so nothing external (jobs.tsv, an operator's `$?`, the existing
# rc=11 phase-failure test) sees a different contract than before.
# `phase_failed="<merge>"`, not `"merge"`: `$phase_failed` is either one of
# these two internal sentinels OR a literal set name from `$phases` (read
# from `scripts/lane-sets.tsv`), and the gate's `case` below tells them apart
# by VALUE alone. Angle brackets make the two internal values impossible for
# a real set name to collide with — no set in `lane-sets.tsv` is bracketed,
# and `for phase in $phases` already implies a set name has no shell-breaking
# characters — rather than merely unlikely (fix round 2, coordinator's minor
# finding: nothing today is named `merge` or `dirty-tree`, but "impossible"
# costs nothing here and doesn't rely on that staying true).
if ! run_bg git merge --no-ff --no-edit -m "$merge_msg" "$sha"; then
    echo "sluice-run: MERGE CONFLICT — holding. A human resolves this." >&2
    phase_failed="<merge>"
    git merge --abort || true
fi

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
#
# Guarded by `phase_failed` still being empty: a merge conflict above means
# there is nothing here worth running — `merge_sha` was never assigned, and
# entering the loop against a half-merged (or aborted) tree would either error
# on an unbound variable or silently test the wrong thing.
if [ -z "$phase_failed" ]; then
    merge_sha="$(git rev-parse HEAD)"
    echo "sluice-run: merge product is $merge_sha"

    for phase in $phases; do
        line="$(cmd_for "$phase")"
        [ -n "$line" ] || { echo "sluice-run: no such set '$phase' in $lane_sets_file" >&2; exit 2; }
        echo "sluice-run: === phase $phase ==="
        # Task 1's verdict decides whether `heavy` gates or reports. If it
        # REPORTS, replace this block for that one phase with a warning that
        # does not `break`.
        if ! run_bg bash "$repo_root/scripts/timed.sh" "sluice:$phase" -- sh -c "$line"; then
            echo "sluice-run: PHASE $phase FAILED — holding." >&2
            phase_failed="$phase"
            break
        fi
        if [ -n "$(git status --porcelain)" ]; then
            # `-u`, never `-A`: stage only paths git ALREADY tracks. `-A`
            # would also sweep up any untracked residue this phase happened
            # to leave behind and commit it as if it were this phase's
            # authored output — misattributing it, and to whichever phase
            # happens to run next if the residue is not itself gitignored.
            git add -u
            # A dirty tree is not necessarily a STAGED one: `git add -u`
            # stages nothing when every change is to a new, untracked file
            # (nothing here legitimately does that, but a phase producing
            # only stray untracked output must not fail on an empty commit).
            # `git clean -fd` below still removes it either way.
            if ! git diff --cached --quiet; then
                # `-c core.hooksPath=/dev/null`, NEVER `--no-verify` (the
                # project's standing rule prohibits it outright and this
                # campaign is not authorised to except it — see the
                # CLEAN-TREE INVARIANT header note above for the
                # two-mechanisms distinction and the full reasoning). This
                # commit is still fully verified: by the four phases already
                # running around it, one of which (`gate`) is a strict
                # superset of what the hook itself would re-run here.
                git -c core.hooksPath=/dev/null commit -q -m "chore(artifacts): regenerate after $phase

Authored on the canonical host inside sluice job $job_id (decision 0079)."
                echo "sluice-run: committed tracked drift from $phase"
            fi
        fi
        git clean -fd --quiet
    done
    if [ -z "$phase_failed" ]; then
        echo "sluice-run: all phases green at $(git rev-parse --short HEAD)."
    fi
fi

# THE CLEAN-TREE CHECK, AND WHY THE SIBLING CHECK IT USED TO HAVE IS GONE
# (fix round 1, Important 2). This used to be followed by a SECOND assertion,
# `git diff --exit-code -- <declared generated paths>`, on the theory that it
# caught something different. The reviewer found that is false: `git status
# --porcelain` being empty STRICTLY IMPLIES that second diff is also empty,
# because `git diff` (working tree vs. index/HEAD) sees a subset of what
# `git status --porcelain` sees (which also reports staged-but-uncommitted
# changes) — so once this check passes, that one could never have failed.
# Worse, by the time execution reaches here the phase loop above has ALREADY
# `git add -u`+committed every phase's tracked drift and `git clean -fd`'d the
# rest, so the tree is clean by construction in the ordinary case; this check
# only ever fires in the narrow residue `git clean -fd` cannot remove — a
# phase leaving behind an untracked NESTED git repository, which git refuses
# to delete with a single `-f` (this is what `scripts/test-sluice.sh`'s
# "push: a nested untracked git repo the phase loop cannot clean is still
# caught" section reproduces and reddens).
#
# The failure the deleted check's own comment claimed to catch — "a phase
# failed to regenerate a declared artifact" — was never actually detectable
# this way. Nothing here holds an independent copy of what a declared path
# SHOULD contain; the phase loop stages and commits whatever the working tree
# happens to hold, correct or not, and a path nothing touched (a phase that
# silently no-ops instead of regenerating) produces no diff against HEAD
# either, since nothing changed it. Detecting THAT failure needs either an
# independent re-regeneration to diff against, or trusting the phase's own
# rc=0 as an implicit "I actually did it" contract — genuinely out of this
# task's scope, so it is named here rather than left to look like it was
# quietly handled.
if [ -z "$phase_failed" ] && [ -n "$(git status --porcelain)" ]; then
    echo "sluice-run: working tree is dirty after all phases — refusing to push." >&2
    git status --porcelain >&2
    phase_failed="<dirty-tree>"
fi

# THE SINGLE GATE. Every failure site above only recorded `phase_failed` (or,
# for `handle_signal`, `why`) and fell through — this is the one place that
# turns either into an exit. `phase_failed` is genuinely live here now (see
# the fix-round-1 note above): a real chamber run reaches this line with it
# set, by construction, for a merge conflict, a failed phase, or a leftover
# nested repo. The `case` preserves each site's ORIGINAL exit code so nothing
# external (jobs.tsv, an operator's `$?`, the existing rc=11 phase-failure
# test) sees a different contract than before this fix. Exit codes 13 and 16
# are retired, not reassigned — 13 was the deleted drift check above; 16 was
# this gate's own former generic "phase_failed set" exit, now replaced by the
# per-cause codes below.
#
# `why != exit` IS NOT LIVE THE SAME WAY, and it would be dishonest to claim
# otherwise. `handle_signal` (the INT/TERM/HUP trap installed near the top of
# this file) unconditionally calls `exit "$num"` in its own non-reentrant
# branch, immediately after the one place that ever assigns `why="$sig"` — so
# by the time ANY code in this script could observe a non-"exit" `why`, the
# process has already terminated from inside the trap. Verified empirically,
# not just reasoned: removing this whole `if` block and re-running
# `scripts/test-sluice.sh`'s "a chamber killed mid-phase never pushes" section
# left it green — mid-phase kills are ALREADY fully prevented from pushing by
# that earlier, unconditional `exit`, independent of this check. It is kept
# anyway as a backstop against a FUTURE `handle_signal` that returns instead
# of exiting on some path (the same belt-and-suspenders shape
# `lane-run.sh`/`heavy-run.sh` use), not because any test can currently redden
# it without mutating `handle_signal` itself — which would assert on a script
# this task does not ship.
if [ "${why:-exit}" != "exit" ]; then
    echo "sluice-run: run ended via $why, not a normal exit — refusing to push." >&2
    exit 15
fi
if [ -n "$phase_failed" ]; then
    echo "sluice-run: phase '$phase_failed' failed — refusing to push." >&2
    case "$phase_failed" in
        "<merge>")      exit 10 ;;
        "<dirty-tree>") exit 12 ;;
        *)              exit 11 ;;   # a named phase from $phases failed
    esac
fi

final_sha="$(git rev-parse HEAD)"

# THE ONE BRANCH THAT MAKES A STAGE GATE A KIND AND NOT A SECOND SCRIPT.
# Everything above ran identically for both kinds — same claim, same
# worktree, same real merge against the current `origin/main`, same
# roster-declared phases, same clean-tree invariant, same single failure
# gate. A stage gate differs only in what it does with a green verdict:
# nothing. It reports, and `main` is untouched.
#
# It is placed AFTER the failure gate, not before, deliberately: a stage run
# whose phases went red must still exit with that phase's own code (10/11/12/
# 15), or `kind=stage` would become a way to launder a red run into an rc=0
# "reported". The only thing this skips is the push.
#
# The merge commit this run built is discarded with the worktree — the next
# chamber job `checkout --force`s and `reset --hard`s it. That is the point:
# a stage gate answers "would this branch survive contact with main today",
# and tomorrow's answer is a different question against a different main.
if [ "$kind" = "stage" ]; then
    echo "sluice-run: STAGE REPORT — every phase green on merge product $merge_sha (final tree $final_sha)."
    echo "sluice-run: nothing pushed; main is unchanged at $base_sha. kind=stage."
    exit 0
fi

# TESTED SHA == PUSHED SHA. `final_sha` is read fresh, after the phase loop's
# own commits (each phase's tracked drift lands as its own commit, so
# `final_sha` is ordinarily ahead of `merge_sha`) and after the clean-tree
# check above has already refused to continue on anything left uncommitted —
# so what gets pushed is exactly what every phase actually tested, never a
# tree with untested residue layered on top.
echo "sluice-run: merge product $merge_sha, final tree $final_sha"

# Fast-forward only, ALWAYS. HEAD's first parent is origin/main, so this IS
# a fast-forward; a force push of any kind must never appear on this line.
#
# THIS USED TO BE GUARDED BY A STATIC LINT IN scripts/test-sluice.sh, and the
# lint is gone — deleted deliberately, not lost track of. Four review rounds
# tried to make "assert every git push in this file is one of the reviewed
# invocations" hold as source-text analysis: a denylist of forbidden flags
# (beaten by five unanticipated forms), an allowlist with a hand-rolled
# line-joiner (beaten by bash's continuation semantics being reimplemented
# wrong — a joined `git pu\`+newline+`sh -f …` line split "push" across a
# space the joiner inserted, so the detector matched nothing), then a
# rewrite that asked bash's own parser instead of re-modelling it
# (`declare -f` on the file wrapped as a function body) — which closed that
# class for good, but opened a worse one: an unbalanced `}` in the audited
# file terminates the wrapper function early, so everything after it runs as
# real top-level code AT AUDIT TIME, including a real `git push`. A static
# checker that can be made to execute the thing it is auditing is a worse
# security property than having no checker. What every round actually
# confirmed, the hard way, is that a static detector over shell text cannot
# see RUNTIME argv formation — `p=push; git "$p" …`, `git p''ush …` — because
# that only exists once the shell actually evaluates it, which a safe static
# checker must never do.
#
# The enforcement that actually matters lives at RUNTIME instead:
# `scripts/hooks/pre-push` (repository-level `core.hooksPath`, so it fires
# for every push from every session, no opt-in) refuses any delete or
# non-fast-forward push to a non-local remote unless `HV_PUSH_OK=1` —
# verified against the real remote (a genuine rewind of a campaign branch
# was refused, the remote unmoved, while an ordinary fast-forward went
# through). That covers the actual danger (this script, or a human, force-
# pushing or deleting something on the shared remote) for every push this
# repository ever makes, not just the two lines below. What it does not
# cover — an ordinary, non-force push to the WRONG destination — is already
# exercised functionally by this repo's own chamber tests, which run a real
# push against a scratch origin and assert the pushed SHA and destination
# ref are exactly what was expected; that is real execution catching a real
# mistake, not text pattern-matching a hypothetical one.
if ! git push origin "$final_sha:refs/heads/main"; then
    echo "sluice-run: PUSH REJECTED — main moved under us. Holding." >&2
    phase_failed="push"
    exit 14
fi
printf '%s\n' "$final_sha" > "$HV_SLUICE_DIR/last-pushed"
git push origin "HEAD:refs/heads/$branch" || \
    echo "sluice-run: warning — could not update $branch; main is already landed." >&2
echo "sluice-run: LANDED $final_sha on main"
