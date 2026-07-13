#!/usr/bin/env bash
# scripts/timed.sh — the timing ledger (TOOL-suite-timing-ledger, first step).
#
# The suite instruments the world but never watched its own wall time: the
# 209s branches sweep, and later a whole suite creeping 65s -> 43.5 min, lived
# unnoticed until they hurt. This records wall + CPU time for expensive runs
# (full-fixture regens, censuses, full gates) to docs/timings.md, so creep is
# visible BEFORE it forces a scramble. Committed milestone log; times are
# machine/load-specific (see the host/cores/cpu_ratio columns), so it is NOT
# drift-checked and never gates the build — it is a record you read.
#
# cpu_ratio = (user+sys)/wall — roughly the parallelism achieved. It separates
# the two ways a run gets slower: more WORK (user climbs) vs more CONTENTION
# (wall climbs, cpu_ratio falls).
#
# Usage:
#   scripts/timed.sh <label> -- <command...>   # run it, append a row, pass exit code
#   scripts/timed.sh report [label]            # print the ledger (optionally one label)
set -uo pipefail

ROOT="$(git rev-parse --show-toplevel)"
LEDGER="$ROOT/docs/timings.md"

cores() { getconf _NPROCESSORS_ONLN 2>/dev/null || echo '?'; }

cmd_report() {
    local filter="${1:-}"
    [ -f "$LEDGER" ] || { echo "no ledger yet at $LEDGER" >&2; return 0; }
    if [ -z "$filter" ]; then cat "$LEDGER"; return 0; fi
    echo "== timings for label '$filter' =="
    # Table rows are: | when | label | wall | user | sys | ratio | ... |
    # Columns after -F'|' (leading '|' makes $1 empty): $2 when, $3 label,
    # $4 wall, $5 user, $6 sys, $7 ratio, $8 commit, $9 branch, $10 host,
    # $11 cores. Median via insertion sort — BSD awk (macOS) has no asort.
    awk -F'|' -v L="$filter" '
        function trim(s){ gsub(/^[ \t]+|[ \t]+$/, "", s); return s }
        trim($3)==L {
            n++; walls[n]=trim($4)+0; last=trim($4)+0
            printf "  %s  wall=%ss  cpu_ratio=%s  %s@%s\n", trim($2), trim($4), trim($7), trim($10), trim($8)
        }
        END{
            if(n==0){ print "  (no runs recorded)"; exit }
            for(i=2;i<=n;i++){ key=walls[i]; j=i-1; while(j>=1 && walls[j]>key){ walls[j+1]=walls[j]; j-- }; walls[j+1]=key }
            med = (n%2)? walls[(n+1)/2] : (walls[int(n/2)]+walls[int(n/2)+1])/2
            printf "  -- %d runs; latest wall=%ss, median=%ss", n, last, med
            if(med>0 && last > 1.5*med) printf "  <== LATEST IS >1.5x MEDIAN (investigate)"
            printf "\n"
        }' "$LEDGER"
}

cmd_run() {
    local label="$1"; shift
    [ "${1:-}" = "--" ] || { echo "usage: timed.sh <label> -- <command...>" >&2; return 2; }
    shift
    [ "$#" -gt 0 ] || { echo "timed.sh: no command given" >&2; return 2; }

    # Time the command with bash's builtin `time` (portable across macOS BSD /
    # Linux GNU). fd 8/9 carry the command's own stdout/stderr straight
    # through; only `time`'s report (TIMEFORMAT) lands in $tmp.
    local tmp rc real user sys ratio
    tmp="$(mktemp)"
    { TIMEFORMAT='%R %U %S'; time "$@" 1>&8 2>&9; } 8>&1 9>&2 2>"$tmp"
    rc=$?
    read -r real user sys < "$tmp"; rm -f "$tmp"

    ratio="$(awk -v u="$user" -v s="$sys" -v r="$real" 'BEGIN{ if(r+0>0) printf "%.2f",(u+s)/r; else print "?" }')"
    [ -f "$LEDGER" ] || init_ledger
    printf '| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$label" "$real" "$user" "$sys" "$ratio" \
        "$(git rev-parse --short HEAD 2>/dev/null || echo '-')" \
        "$(git branch --show-current 2>/dev/null || echo '-')" \
        "$(hostname -s 2>/dev/null || echo '-')" "$(cores)" \
        >> "$LEDGER"
    echo "timed.sh: '$label' wall=${real}s user=${user}s sys=${sys}s cpu_ratio=${ratio} (recorded) rc=$rc" >&2
    return "$rc"
}

init_ledger() {
    mkdir -p "$(dirname "$LEDGER")"
    cat > "$LEDGER" <<'MD'
# Timing ledger

Append-only record of expensive runs — full-fixture regens, censuses, full
gates — so runtime creep is visible *before* it forces a scramble (the suite
instruments the world but never watched its own wall time). One row per
deliberate milestone run, written by [`scripts/timed.sh`](../scripts/timed.sh)
(`make timings` to view). Times are machine- and load-specific — read
`host`/`cores`/`cpu_ratio`, not the raw seconds, across different machines.
`cpu_ratio = (user+sys)/wall` ≈ parallelism achieved: it separates *more work*
(user climbs) from *more contention* (wall climbs, ratio falls). This file is
NOT drift-checked and never gates the build; it is a record you read. The
build-failing tolerance-band version is a later step (`TOOL-suite-timing-ledger`).

| when (UTC) | label | wall_s | user_s | sys_s | cpu_ratio | commit | branch | host | cores |
|---|---|---|---|---|---|---|---|---|---|
MD
}

case "${1:-}" in
    report) shift; cmd_report "$@";;
    ''|-h|--help) echo "usage: timed.sh <label> -- <command...>   |   timed.sh report [label]" >&2;;
    *) cmd_run "$@";;
esac
