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
        printf '%s\n' "$MOUTHOUT"
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" held \
            "REFUSED AT THE MOUTH rc=$MRC, box never taken, main unchanged at ${BEFORE}. $(printf '%s' "$MOUTHOUT" | tr '\n' ' ' | cut -c1-300)")
        echo "=== rc=$MRC MOUTH REFUSAL — box never taken, main ${BEFORE}..${BEFORE} ==="
        return 0
    fi

    echo "launching $BR kind=$KIND sha=${SHA:0:12} main=$BEFORE"
    (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" running \
        "launched by operator; kind=$KIND read from the row") >/dev/null 2>&1

    local START RC ELAPSED AFTER LOG runner
    START=$SECONDS
    runner="$(dispatch_for "$KIND")"
    if [ "$KIND" = "census" ]; then
        (cd "$repo_root" && bash "$runner" "$SHA") >/dev/null 2>&1
    else
        (cd "$repo_root" && bash "$runner" "$BR" "$SHA" "$KIND") >/dev/null 2>&1
    fi
    RC=$?
    ELAPSED=$((SECONDS - START))

    git -C "$repo_root" fetch --quiet origin 2>/dev/null
    AFTER="$(git -C "$repo_root" rev-parse --short origin/main)"
    LOG=$(find "$HOME/.local/state/hornvale/sluice/" -maxdepth 1 \
              -name "*${SHA:0:12}*.log" -printf '%T@ %p\n' 2>/dev/null \
          | sort -rn | head -1 | cut -d' ' -f2-)

    # ALWAYS SET A TERMINAL STATE. A row left `running` becomes a ghost, because
    # coalescing never supersedes a running row — the next submission from that
    # branch queues behind a job that already finished.
    if [ "$RC" = "0" ] && [ "$KIND" = "stage" ]; then
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" reported \
            "all four stage phases rc=0 in ${ELAPSED}s; main unchanged at ${AFTER}.")
    elif [ "$RC" = "0" ] && [ "$KIND" = "census" ]; then
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" reported \
            "census rc=0 in ${ELAPSED}s; main unchanged at ${AFTER}. Goldens delivered on a census/* branch — see the run log for the branch name and the make sluice line.")
    elif [ "$RC" = "0" ]; then
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" landed \
            "four phases rc=0 in ${ELAPSED}s; main ${BEFORE}..${AFTER}.")
    else
        (cd "$repo_root" && bash scripts/sluice-queue.sh set-state "$ID" held \
            "CHAMBER RED rc=$RC after ${ELAPSED}s; main ${BEFORE}..${AFTER}. Log: ${LOG} — attribution pending.")
    fi

    echo "=== rc=$RC elapsed=${ELAPSED}s kind=$KIND main ${BEFORE}..${AFTER} ==="
    grep -a "timed.sh: 'sluice:" "$LOG" 2>/dev/null || echo "(no phase timings)"
    if [ "$BEFORE" != "$AFTER" ]; then
        echo "=== what this run pushed ==="
        git -C "$repo_root" log --oneline "${BEFORE}..${AFTER}" 2>/dev/null | sed 's/^/  /'
    fi
    return 0
}

main() {
    local max="${1:-5}" i row
    for ((i = 1; i <= max; i++)); do
        row="$(cd "$repo_root" && bash scripts/sluice-queue.sh next 2>/dev/null)"
        if [ -z "${row//[[:space:]]/}" ]; then
            echo "queue drained after $((i - 1)) run(s)"
            break
        fi
        echo "=== [$i] $(printf '%s' "$row" | cut -f3) kind=$(printf '%s' "$row" | cut -f6) sha=$(printf '%s' "$row" | cut -f4 | cut -c1-12) ==="
        run_one "$row" || break
    done
    git -C "$repo_root" fetch -q origin 2>/dev/null
    echo "main: $(git -C "$repo_root" rev-parse --short origin/main)"
}

if [ -z "${HV_DRAIN_LIB:-}" ]; then
    main "$@"
fi
