#!/usr/bin/env bash
# Drain the merge queue in FIFO order, one job at a time, kind read from the row.
#
# WHAT THIS IS. The queue scripts (sluice-queue.sh, sluice-mouth.sh,
# sluice-run.sh, sluice-census.sh) are each committed and tested. The thing that
# ORCHESTRATES them — pop the next row, gate it, dispatch on kind, set the
# terminal state — lived in an operator's session scratchpad for weeks, ungated
# and untested, while doing real gating work. It was the direct cause of two
# defects in one night (2026-08-27), so it lives here now.
#
# IT NEVER DECIDES WHETHER A JOB *SHOULD* RUN. Vetting is the operator's job:
# a redundant census, a decision minted outside its block, a schema bump that
# breaks a consumer — none of that is visible here and none of it should be.
# This script decides only ORDER and MECHANISM.
#
#   bash scripts/sluice-drain.sh [max_jobs]     # default 5
#
# Set HV_DRAIN_LIB=1 to source it for its functions without draining (tests).

set -u

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# EXPORTED, not merely assigned. Every script this one invokes
# (sluice-queue.sh, sluice-run.sh, sluice-census.sh) resolves the same default
# independently, so an unexported value agrees with them only by coincidence --
# and a test or operator who redirects it would move this loop's view of the
# queue without moving its children's. Exporting the resolved value is a no-op
# in the default case and the difference between working and silently
# half-redirected in every other.
export HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"

# --- the two decision rules, extracted so they can be tested ----------------

# Does the mouth's verdict apply to this kind of job?
#
# A CENSUS NEVER MERGES MAIN, so a merge-conflict verdict cannot speak to
# whether it may run: census-run.sh checks out the ref detached and measures the
# world there, and main is neither touched nor consulted. Gating a census on a
# merge conflict refuses a valid, standing-authorised run for a reason that does
# not apply to it — which is exactly what happened to campaign/the-sources at
# 83fcd168908e, refused over two aggregate conflicts in files a census does not
# read. The conflict is still real and still blocks the DELIVERY branch's
# eventual merge; that is the campaign's problem at merge time, not a reason to
# withhold the measurement now.
# ANNOUNCE A TERMINAL STATE ON THE BOARD, ADDRESSED TO THE BRANCH.
#
# The queue read the board on the way IN and wrote nothing on the way OUT.
# sluice-request.sh syncs and reads it for the hold-off advisory (line ~201);
# every terminal state below was written to the queue ROW and nowhere else. So
# a campaign that submitted had no way to learn its outcome except polling
# `make sluice-status` or being told by whoever was operating the queue — which
# is what actually happened, by hand, forty-odd times on 2026-09-07.
#
# THE BRANCH IS THE ADDRESS. The queue row carries no submitter identity — no
# host, no session, no user; `host=` in sluice-request.sh's output is the
# CANONICAL box, not the caller's — so there is no session to push to even in
# principle. What every row does carry is the branch, and `board post <kind>
# <by>` takes exactly that. A campaign reading the board sees its own name.
#
# IT IS PULL, NOT PUSH, AND THAT IS THE HONEST LIMIT. The board is read at
# SessionStart, and its sync is asynchronous, so the render a session sees is
# one sync behind (root CLAUDE.md). A campaign learns its outcome at its next
# session start, or immediately with `make board-sync && make board`. That is
# strictly better than nothing, which is what exists today, and it removes the
# operator from the routine path without pretending to be a notification.
#
# BEST-EFFORT, NEVER FATAL. By the time this runs the job is over and its
# terminal state is already recorded in the row, which is the authoritative
# place. A board that is unreachable, or a checkout with no built binary, must
# not turn a landed merge into a drain failure — the same rule scripts/timed.sh
# learned when a vanished tempfile reddened a green phase. Deliberately never
# compiles the binary, matching board-render.sh's precedent.
announce() {
    local state="$1" note="$2" cand
    [ "${HV_SLUICE_SKIP_BOARD:-}" = "1" ] && return 0
    for cand in tools/board/target/release/board tools/board/target/debug/board; do
        if [ -x "$repo_root/$cand" ]; then
            (cd "$repo_root" && "./$cand" post notice "$BR" \
                "note=sluice: $BR $state — $note" >/dev/null 2>&1) \
                || echo "sluice-drain: warning — could not announce $state on the board (the row is authoritative)" >&2
            return 0
        fi
    done
}

mouth_applies_to() {
    [ "${1:-}" != "census" ]
}

# Which script actually runs this kind of job?
#
# `census` is NOT a chamber job. census-run.sh takes the shared claim ITSELF and
# `rm -f`s the claim file on exit, so nesting it inside a job already holding the
# claim would clobber and then delete that job's own claim mid-run — which is why
# sluice-run.sh refuses `census` as a phase by design. The queue row for a census
# exists for ORDERING, not dispatch.
dispatch_for() {
    case "${1:-}" in
        census) echo "scripts/sluice-census.sh" ;;
        *)      echo "scripts/sluice-run.sh" ;;
    esac
}

# --- the drainer registry --------------------------------------------------
#
# WHAT THIS IS FOR. `flock` on /tmp/hv-census.lock serializes the WORK; nothing
# serialized the LOOPS that feed it. On 2026-09-13 two sessions each left a
# drain loop running against the same queue. The mutex did its job perfectly --
# one holder, one waiter, no row run twice, the shared worktree untouched -- and
# the queue was still incoherent, because the two loops had different POLICIES:
# one deliberately refused to auto-drain merges and the other drained three. A
# mutex orders work and has no opinion about which work should exist.
#
# WHY A LOCK AND NOT A PIDFILE. A pidfile lies in both directions: it outlives a
# SIGKILLed drainer, and a drainer killed before its cleanup leaves a file
# asserting life. An flock cannot -- the kernel releases it when the holder
# dies, so "is it held" and "is it alive" are the same question. This repository
# already learned that in windows/lab/src/census_claim.rs; a pidfile here would
# be a second, worse derivation of a solved problem. The registration FILE below
# exists only to put a human-readable label on a lock; the LOCK is the truth,
# and a reader who finds a file whose lock is free has found a corpse, which
# `sluice-status` reports as STALE rather than believing.
#
# WHY PER KIND, AND NOT ONE LOCK PER BOX. "One drainer per box" is the wrong
# invariant, and the session that found this bug is the counterexample: a
# merge-only dispatcher running beside a stage/census loop was CORRECT and
# useful. The right invariant is "no two loops claiming the same kinds", which
# one lock per kind expresses exactly -- disjoint policies coexist, overlapping
# ones refuse -- with no policy strings to canonicalise or compare.

# The closed set of queue kinds. scripts/sluice-queue.sh's validate_kind is the
# authority; this mirrors it so a policy can be checked without sourcing it, and
# `the_kind_set_matches_the_queues` in the test suite fails if the two drift.
DRAIN_KNOWN_KINDS="merge stage census"

# Fixed file descriptors, one per kind. bash 3.2 (what macOS ships, and what
# scripts/check-bash32.sh holds this file to) has no `exec {fd}>` form, so a
# dynamic fd is not available. The kind set is closed, so three constants cost
# nothing and read more plainly than the allocation would.
lock_fd_for() {
    if [ "${1:-}" = merge ];  then echo 21
    elif [ "${1:-}" = stage ];  then echo 22
    elif [ "${1:-}" = census ]; then echo 23
    else echo ""
    fi
}

# Does this policy claim this kind? Policies are comma-separated kind lists.
policy_admits() {
    local policy="${1:-}" kind="${2:-}" k
    [ -n "$kind" ] || return 1
    local IFS=,
    for k in $policy; do
        if [ "$k" = "$kind" ]; then return 0; fi
    done
    return 1
}

# Every kind named by a policy must be one the queue actually has. A typo like
# `--kinds=merges` would otherwise produce a loop that locks nothing, claims
# nothing, and looks healthy -- the silent-no-op shape this whole change exists
# to stop.
policy_is_valid() {
    local policy="${1:-}" k known ok
    [ -n "$policy" ] || return 1
    # SPLIT WITH `tr`, NOT `local IFS=,`. The obvious version sets IFS to a
    # comma to split the policy -- and that IFS is still in force for the INNER
    # loop over $DRAIN_KNOWN_KINDS, which is space-separated. It then iterates
    # once over the single word "merge stage census", matches nothing, and
    # refuses EVERY policy including the real ones. Its four "refuses a bad
    # policy" assertions all passed while it did so; only the positive control
    # ("real policies validate") caught it.
    for k in $(printf '%s' "$policy" | tr ',' ' '); do
        ok=0
        for known in $DRAIN_KNOWN_KINDS; do
            if [ "$k" = "$known" ]; then ok=1; fi
        done
        if [ "$ok" = 0 ]; then return 1; fi
    done
    return 0
}

# Who holds the lock for this kind, as a human-readable line? Read from the
# registration file, which is a label and may be stale -- callers must have
# established that the lock is actually held before quoting this.
registration_line() {
    local kind="${1:-}" f="$HV_SLUICE_DIR/drainers/${1:-}"
    if [ -r "$f" ]; then
        tr '\n' ' ' < "$f"
    else
        printf '%s' "(no registration file; the holder did not write one)"
    fi
}

# Take the lock for every kind in the policy, or take none and refuse.
#
# ALL-OR-NOTHING ON PURPOSE. A partial acquisition would leave this process
# holding some kinds while another loop holds the rest, which is precisely the
# split-policy state the locks exist to prevent -- and it would do it while
# reporting a refusal, so nobody would go looking.
acquire_policy_locks() {
    local policy="$1" mode="$2" k fd taken=""
    mkdir -p "$HV_SLUICE_DIR/drainers" || return 2
    local IFS=,
    for k in $policy; do
        fd="$(lock_fd_for "$k")"
        if [ -z "$fd" ]; then
            echo "sluice-drain: no lock fd for kind '$k' -- refusing rather than draining unprotected" >&2
            release_policy_locks "$taken"
            return 2
        fi
        eval "exec $fd>\"$HV_SLUICE_DIR/drainer-$k.lock\"" || { release_policy_locks "$taken"; return 2; }
        if ! flock -n "$fd"; then
            echo "sluice-drain: REFUSING -- another drainer already claims kind '$k'." >&2
            echo "sluice-drain:   holder: $(registration_line "$k")" >&2
            echo "sluice-drain:   This is not a lock you should force. Either wait for that loop," >&2
            echo "sluice-drain:   narrow this one with --kinds= to the kinds it does not claim, or" >&2
            echo "sluice-drain:   stop it deliberately. See 'make sluice-status' for every drainer." >&2
            eval "exec $fd>&-"
            release_policy_locks "$taken"
            return 3
        fi
        taken="$taken${taken:+,}$k"
        {
            echo "pid=$$"
            echo "host=$(hostname)"
            echo "kinds=$policy"
            echo "mode=$mode"
            echo "started=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
        } > "$HV_SLUICE_DIR/drainers/$k"
    done
    DRAIN_HELD_KINDS="$taken"
    return 0
}

release_policy_locks() {
    local held="${1:-}" k fd
    [ -n "$held" ] || return 0
    local IFS=,
    for k in $held; do
        fd="$(lock_fd_for "$k")"
        rm -f "$HV_SLUICE_DIR/drainers/$k"
        if [ -n "$fd" ]; then eval "exec $fd>&-" 2>/dev/null || true; fi
    done
}

# --- the drain -------------------------------------------------------------

run_one() {
    local TS ID BR SHA STATE KIND REST
    # TS and STATE are positional placeholders: the queue TSV's column order is
    # the contract, so they are read to reach the columns after them, not used.
    # shellcheck disable=SC2034
    IFS=$'\t' read -r TS ID BR SHA STATE KIND REST <<<"$1"
    [ -n "${BR:-}" ] || return 1

    local BEFORE MRC MOUTHOUT
    BEFORE="$(git -C "$repo_root" rev-parse --short origin/main)"
    MRC=0

    # THE MOUTH IS A GATE, NOT A LOG LINE. Consume its exit code: campaign/the-deed
    # (2026-08-23) had its conflicts PRINTED by a pre-launch mouth check and was
    # launched anyway, because the check and the launch sat in one non-branching
    # script. Refusing here costs nothing and never takes the box.
    #
    # NOT `if ! cmd; then rc=$?` — inside the body of an `if !`, `$?` is the
    # NEGATED status, always 0. That warning is written into sluice-run.sh and the
    # bug was then committed here anyway, which is how campaign/the-granary's
    # conflict was recorded as "rc=0" in its queue row.
    if mouth_applies_to "$KIND"; then
        MOUTHOUT="$(cd "$repo_root" && bash scripts/sluice-mouth.sh "$BR" "$SHA" 2>&1)" || MRC=$?
    else
        MOUTHOUT="sluice-drain: kind=$KIND — mouth check skipped (it does not merge main)"
    fi

    if [ "$MRC" -ne 0 ]; then
        # The row is `running` by now (claim precedes the mouth), so this
        # `held` is what RELEASES it. It was already the right call when the
        # row arrived here `queued`; it is load-bearing now.
        printf '%s\n' "$MOUTHOUT"
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" held \
            "REFUSED AT THE MOUTH rc=$MRC, box never taken, main unchanged at ${BEFORE}. $(printf '%s' "$MOUTHOUT" | tr '\n' ' ' | cut -c1-300)")
        announce "REFUSED AT THE MOUTH" "rc=$MRC, box never taken, main unchanged at ${BEFORE}. Absorb main and resubmit; the sha changes, so it is a new request. $(printf '%s' "$MOUTHOUT" | tr '\n' ' ' | cut -c1-400)"
        echo "=== rc=$MRC MOUTH REFUSAL — box never taken, main ${BEFORE}..${BEFORE} ==="
        return 0
    fi

    echo "launching $BR kind=$KIND sha=${SHA:0:12} main=$BEFORE"
    # The row is ALREADY `running`: `claim` above marked it in the same locked
    # pass that selected it. The old separate `set-state running` here is what
    # left the TOCTOU window open — between `next` and this line the row read
    # `queued`, and the mouth check below used to run inside that window.

    local START RC ELAPSED AFTER LOG runner
    START=$SECONDS
    runner="$(dispatch_for "$KIND")"
    # BOTH RUNNERS NOW TAKE THE REQUEST ID, NOT branch/sha/kind. The drain
    # already claimed this row above (in the same locked pass that selected
    # it), so the runner resolves branch/sha/kind straight from the row it
    # was told about rather than trusting whatever this loop passed
    # positionally — which is what let a hand-typed `merge` land a
    # kind=stage request on main once already. This also removes the need to
    # export a claim-id environment variable: there is nothing ambient for a
    # nested sluice-run.sh (e.g. one invoked from inside scripts/test-sluice.sh,
    # itself run as an `outboard` phase) to inherit and mistake for its own
    # claim.
    (cd "$repo_root" && bash "$runner" "$ID") >/dev/null 2>&1
    RC=$?
    ELAPSED=$((SECONDS - START))

    git -C "$repo_root" fetch --quiet origin 2>/dev/null
    AFTER="$(git -C "$repo_root" rev-parse --short origin/main)"
    LOG=$(find "$HV_SLUICE_DIR/" -maxdepth 1 \
              -name "*${SHA:0:12}*.log" -printf '%T@ %p\n' 2>/dev/null \
          | sort -rn | head -1 | cut -d' ' -f2-)

    # ALWAYS SET A TERMINAL STATE. A row left `running` becomes a ghost, because
    # coalescing never supersedes a running row — the next submission from that
    # branch queues behind a job that already finished.
    if [ "$RC" = "0" ] && [ "$KIND" = "stage" ]; then
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" reported \
            "all stage phases rc=0 in ${ELAPSED}s; main unchanged at ${AFTER}.")
        announce "STAGE GREEN" "all stage phases rc=0 in ${ELAPSED}s; main unchanged at ${AFTER}. Nothing pushed."
    elif [ "$RC" = "0" ] && [ "$KIND" = "census" ]; then
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" reported \
            "census rc=0 in ${ELAPSED}s; main unchanged at ${AFTER}. $(census_note "$LOG")")
        announce "CENSUS DONE" "rc=0 in ${ELAPSED}s; main unchanged at ${AFTER}. $(census_note "$LOG")"
    elif [ "$RC" = "0" ]; then
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" landed \
            "all merge phases rc=0 in ${ELAPSED}s; main ${BEFORE}..${AFTER}.")
        announce "LANDED" "all merge phases rc=0 in ${ELAPSED}s; main ${BEFORE}..${AFTER}."
    else
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" held \
            "CHAMBER RED rc=$RC after ${ELAPSED}s; main ${BEFORE}..${AFTER}. Log: ${LOG} — attribution pending.")
        announce "CHAMBER RED" "rc=$RC after ${ELAPSED}s; main ${BEFORE}..${AFTER}. Log: ${LOG}. Attribution pending — read the log before assuming it is your code."
    fi

    echo "=== rc=$RC elapsed=${ELAPSED}s kind=$KIND main ${BEFORE}..${AFTER} ==="
    grep -a "timed.sh: 'sluice:" "$LOG" 2>/dev/null || echo "(no phase timings)"
    if [ "$BEFORE" != "$AFTER" ]; then
        echo "=== what this run pushed ==="
        git -C "$repo_root" log --oneline "${BEFORE}..${AFTER}" 2>/dev/null | sed 's/^/  /'
    fi
    return 0
}

# The queue row's census verdict, READ FROM THE RUN rather than asserted.
#
# This row said "Goldens delivered on a census/* branch" for every rc=0 census,
# unconditionally. That is the same defect scripts/sluice-census.sh carried until
# 4a2fc72d7 -- and fixing the run log left this copy of the claim standing, on the
# surface operators actually read (`make sluice-status`). campaign/the-pawl's
# census on 2026-09-02 printed "NO GOLDENS MOVED" in its log and "Goldens
# delivered" in its row, in the same minute.
#
# FAILS TOWARD "MOVED", deliberately. A missing or unreadable log yields the
# goldens-moved wording, because the two errors are not symmetric: claiming
# movement that did not happen costs a reader one `git diff --stat`, while
# claiming a null that did not happen invites them to skip a delivery branch
# they needed to merge.
census_note() {
    local log="${1:-}"
    if [ -n "$log" ] && grep -q 'NO GOLDENS MOVED' "$log" 2>/dev/null; then
        printf '%s' "NO GOLDENS MOVED — the census agrees with the ref; the census/* branch carries the run's timings row only, so there is nothing to merge for goldens' sake."
    else
        printf '%s' "Goldens moved and are delivered on a census/* branch — see the run log for the branch name and the make sluice line."
    fi
}

# Claim and run exactly one row. Exit status is the LOOP's instruction:
#   0  a row was claimed and run (its own rc is recorded in the queue row)
#   1  the queue holds nothing claimable right now
#   2  claim itself failed -- cannot tell drained from broken, so stop
drain_once() {
    local label="${1:-1}" row rc claim_out
    # STDOUT AND STDERR ARE CAPTURED TOGETHER, AND THE RC IS CHECKED
    # BEFORE the output is trusted as a row (fix round 2, Critical F1).
    # `claim` now REFUSES (exit 3) rather than building on demand when
    # its binary is missing, and this loop used to discard stderr
    # (`2>/dev/null`) and treat any empty stdout -- including empty
    # stdout from a rc=3 refusal -- as "drained". That read a build
    # failure as "queue drained after 0 run(s)": a wrong answer, with
    # the one line that would have explained it thrown away. On success
    # `claim` writes nothing to stderr, so folding the streams together
    # costs the success path nothing.
    claim_out="$(cd "$repo_root" && bash scripts/sluice-queue.sh claim "launched by the drain loop" 2>&1)"
    rc=$?
    if [ "$rc" -ne 0 ]; then
        echo "sluice-drain: claim failed (rc=$rc) -- cannot tell drained from broken, so NOT reporting 'queue drained'." >&2
        printf '%s\n' "$claim_out" >&2
        return 2
    fi
    row="$claim_out"
    if [ -z "${row//[[:space:]]/}" ]; then
        return 1
    fi
    echo "=== [$label] $(printf '%s' "$row" | cut -f3) kind=$(printf '%s' "$row" | cut -f6) sha=$(printf '%s' "$row" | cut -f4 | cut -c1-12) ==="
    run_one "$row"
}

# The kind of the row at the head of the queue, or empty if nothing is queued.
head_queued_kind() {
    (cd "$repo_root" && bash scripts/sluice-queue.sh list 2>/dev/null) \
        | awk -F'\t' '$5=="queued"{print $6; exit}'
}

# Is the canonical box already working? A drainer that claims a row while the
# box is busy marks that row `running` and then blocks on the flock, so
# `make sluice-status` shows a job as running for as long as the wait lasts.
# That is not wrong, but it is unreadable, and a human reading the queue during
# a long census would conclude two jobs were running at once. Waiting to claim
# costs nothing -- the row is not going anywhere -- and keeps the queue's own
# report honest, which is this change's whole subject.
box_is_busy() {
    (cd "$repo_root" && bash scripts/census-run.sh status 2>&1) | grep -q "running:"
}

# Watch the queue and drain the kinds this policy claims, until stopped.
#
# THE LOOP LIVES HERE, AND THAT IS THE POINT. Until now it did not: operators
# wrote `while true; do sluice-drain.sh 1; sleep; done` into a session
# scratchpad, which is exactly where the DISPATCH logic lived before it caused
# two defects in one night and was moved into this file (see the header). The
# watching half stayed outside and reproduced the identical failure: untested
# code doing real gating work, invisible to every instrument, outliving the
# session that started it because an orphan reparents to pid 1.
#
# It also cannot work anywhere else. A lock taken INSIDE a one-shot drain is
# released when that drain exits, so two scratchpad loops calling this script
# would take turns and never see each other. A registration that does not span
# the loop's own sleeps is not a registration.
# Has this process been orphaned — its parent gone, reparented to init?
#
# THIS IS THE ORIGINAL COMPLAINT, NOT A NEW ONE. The drainer that started all
# this outlived the session that launched it: the session ended, init adopted
# the loop, and it kept draining for hours with nothing able to see it. The
# locks above make such a loop VISIBLE; this makes it STOP. Both are wanted —
# visibility is what a reader needs, and exiting is what the box needs.
#
# A deliberately daemonised drainer (nohup, systemd, a detached runner) has
# ppid 1 legitimately and must not be killed by this, so it is opt-out. The
# DEFAULT is to exit, because the documented way to run this is in the
# foreground, and the failure this guards is far more common than the
# daemon case.
is_orphaned() {
    [ "$(ps -o ppid= -p $$ 2>/dev/null | tr -d ' ')" = "1" ]
}

watch_loop() {
    local policy="$1" max="$2" idle="$3" i=0 kind rc
    echo "sluice-drain: watching kinds=$policy (max-jobs=${max:-unlimited}, idle=${idle}s), pid $$"
    while :; do
        if [ "$max" -gt 0 ] && [ "$i" -ge "$max" ]; then
            echo "sluice-drain: reached max-jobs=$max -- stopping"
            return 0
        fi
        if [ "${DRAIN_ALLOW_ORPHAN:-0}" != 1 ] && is_orphaned; then
            echo "sluice-drain: my parent is gone and I have been reparented to init." >&2
            echo "sluice-drain:   Exiting rather than draining on as an orphan nobody can see they" >&2
            echo "sluice-drain:   are competing with. Pass --allow-orphan if you meant to daemonise." >&2
            return 0
        fi
        kind="$(head_queued_kind)"
        if [ -z "$kind" ]; then
            sleep "$idle"; continue
        fi
        if ! policy_admits "$policy" "$kind"; then
            # Somebody else's kind is at the head. Do NOT reach past it: the
            # queue is FIFO and jumping the line is exactly the incoherence
            # this change exists to prevent.
            sleep "$idle"; continue
        fi
        if box_is_busy; then
            sleep "$idle"; continue
        fi
        i=$((i + 1))
        drain_once "$i"; rc=$?
        if [ "$rc" = 2 ]; then
            echo "sluice-drain: stopping -- claim is broken, not drained" >&2
            return 1
        fi
        if [ "$rc" = 1 ]; then
            i=$((i - 1))   # nothing was claimed; do not spend a job slot on it
            sleep "$idle"
        fi
    done
}

usage() {
    cat >&2 <<'USAGE'
usage:
  sluice-drain.sh [--kinds=<csv>] [N]        drain up to N rows, then exit (default 5)
  sluice-drain.sh watch [--kinds=<csv>]      drain continuously until stopped
                        [--max-jobs=N] [--idle=SECS]

  --allow-orphan
            keep running after the parent process exits. Off by default: an
            orphaned drainer outliving its session is the failure this whole
            mechanism exists to stop.

  --kinds   which queue kinds this drainer claims (merge,stage,census).
            Defaults to all three. A lock is taken per kind for the life of the
            run, so two drainers with overlapping kinds cannot both start, and
            two with disjoint kinds can.
USAGE
}

main() {
    local mode=one_shot policy="merge,stage,census" max="" idle=45 arg
    for arg in "$@"; do
        case "$arg" in
            watch)        mode=watch ;;
            --kinds=*)    policy="${arg#--kinds=}" ;;
            --max-jobs=*) max="${arg#--max-jobs=}" ;;
            --idle=*)     idle="${arg#--idle=}" ;;
            --allow-orphan) DRAIN_ALLOW_ORPHAN=1 ;;
            -h|--help)    usage; return 0 ;;
            *[!0-9]*)     echo "sluice-drain: unrecognised argument '$arg'" >&2; usage; return 2 ;;
            *)            max="$arg" ;;
        esac
    done
    if [ "$mode" = one_shot ]; then max="${max:-5}"; else max="${max:-0}"; fi

    if ! policy_is_valid "$policy"; then
        echo "sluice-drain: --kinds='$policy' names something outside the queue's kinds ($DRAIN_KNOWN_KINDS)." >&2
        echo "sluice-drain:   Refusing: a policy that matches no kind would loop forever draining nothing." >&2
        return 2
    fi

    DRAIN_HELD_KINDS=""
    acquire_policy_locks "$policy" "$mode" || return $?
    # shellcheck disable=SC2064  # expand DRAIN_HELD_KINDS now, not at trap time
    trap "release_policy_locks '$DRAIN_HELD_KINDS'" EXIT INT TERM

    local i rc
    if [ "$mode" = watch ]; then
        watch_loop "$policy" "$max" "$idle"
    else
        for ((i = 1; i <= max; i++)); do
            drain_once "$i"; rc=$?
            if [ "$rc" = 2 ]; then return 1; fi
            if [ "$rc" = 1 ]; then
                echo "queue drained after $((i - 1)) run(s)"
                break
            fi
        done
    fi
    git -C "$repo_root" fetch -q origin 2>/dev/null
    echo "main: $(git -C "$repo_root" rev-parse --short origin/main)"
}

if [ -z "${HV_DRAIN_LIB:-}" ]; then
    main "$@"
fi
