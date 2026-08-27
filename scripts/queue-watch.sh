#!/usr/bin/env bash
# queue-watch.sh — emit one line per merge-queue event worth acting on.
#
# WHAT IT IS FOR. Somebody has to notice that a request arrived. Before this,
# that somebody was Nathan, typing "btw something queued" — a human polling a
# TSV on behalf of a machine. This script is the poller; the operator session
# arms it with the Monitor tool, and each line it prints becomes a notification.
#
#     Monitor({command: "bash scripts/queue-watch.sh",
#              description: "sluice queue", persistent: true})
#
# WHAT IT DELIBERATELY DOES NOT DO. It does not drain, vet, merge, or decide
# anything. Notification and judgment are different jobs, and only the first
# one is safe to automate: the queue exists so that a human-supervised operator
# gates what reaches main (decision 0139). A script that auto-merged would be
# the thing the queue was built to prevent, wearing the queue's own name.
#
# IT IS SILENT ON `running` ON PURPOSE. That transition is caused by the
# operator, so echoing it back is noise, and a noisy watcher is not merely
# annoying — Monitor auto-stops one, which converts spam into BLINDNESS.
# The same reasoning drives the burst cap below.
#
# SILENCE MUST NOT BE ABLE TO MEAN "HEALTHY". It emits on the red terminal
# state as well as the green ones, and announces its own blindness if the queue
# becomes unreadable. A watcher that only reported good news would sit mute
# through a stalled queue and look exactly like a quiet one.
#
# STATE LIVES IN ~/.local/state/hornvale/sluice/, NOT A SESSION SCRATCHPAD.
# Sessions are disposable and their scratch directories die with them; this
# file has to outlive the session that armed it, or every new session either
# replays all of history as events or starts blind. Override with QW_STATE.
set -u
cd /home/nathan/Projects/hornvale || exit 1
STATE="${QW_STATE:-$HOME/.local/state/hornvale/sluice/queue-watch.state}"
# The 6th field classifies WHY a row is held, because `held` means two
# unrelated things: a chamber job that went red (needs attribution — wake the
# operator) and a request the operator declined on purpose (needs nothing; they
# just did it). Conflating them makes the watcher cry wolf at its own actions,
# and an alarm that fires on your own deliberate moves trains you to ignore it
# — the failure CLAUDE.md describes for a gate that reddens on day one and
# stays red. This fired for real within minutes of arming: a deliberate
# "redundant census, not run" hold was reported as needing attribution.
# Unknown hold reasons fall through to the LOUD branch on purpose: a mouth
# refusal is a hold nobody chose, so fail toward waking someone.
snap() { bash scripts/sluice-queue.sh list 2>/dev/null | awk -F'\t' 'NF>=6 {h = (index($7,"HELD BY OPERATOR")==1) ? "op" : "chamber"; print $2"|"$5"|"$3"|"$6"|"substr($4,1,12)"|"h}'; }
box_busy() { bash scripts/census-run.sh status >/dev/null 2>&1; }

[ -f "$STATE" ] || snap > "$STATE"
fails=0; last_drainable=""
while true; do
    cur="$(snap)"
    if [ -z "$cur" ]; then
        fails=$((fails+1))
        [ "$fails" = "${QW_BLIND_AFTER:-3}" ] && echo "QUEUE WATCHER BLIND: sluice-queue.sh list returned nothing ${QW_BLIND_AFTER:-3} polls running"
        [ -n "${TEST_ONCE:-}" ] && break
        sleep 60; continue
    fi
    fails=0
    tmp_c="$(mktemp)"; tmp_p="$(mktemp)"
    printf '%s\n' "$cur" | sort > "$tmp_c"
    sort "$STATE" > "$tmp_p" 2>/dev/null || true
    # A lost or stale state file would otherwise replay the entire history as
    # events; Monitor auto-stops a noisy watcher, so a flood does not just spam,
    # it BLINDS me. Collapse anything bigger than a plausible real burst.
    n_new="$(comm -13 "$tmp_p" "$tmp_c" | grep -c . || true)"
    if [ "$n_new" -gt 8 ]; then
        echo "QUEUE RESYNC: $n_new rows differ from my last snapshot — state file was stale or lost, not $n_new real events. Re-read the queue directly."
        printf '%s
' "$cur" > "$STATE"
        [ -n "${TEST_ONCE:-}" ] && break
        sleep 45; continue
    fi
    while IFS='|' read -r id st br kind sha why; do
        [ -n "${id:-}" ] || continue
        case "$st" in
            queued)   echo "QUEUE ARRIVED: $br $sha kind=$kind — needs vetting" ;;
            held)     [ "${why:-chamber}" = "op" ] || \
                          echo "QUEUE RED: $br $sha kind=$kind is HELD — needs attribution" ;;
            landed)   echo "QUEUE LANDED: $br $sha" ;;
            reported) echo "QUEUE REPORTED: $br $sha kind=$kind" ;;
        esac
    done < <(comm -13 "$tmp_p" "$tmp_c")
    rm -f "$tmp_c" "$tmp_p"
    printf '%s\n' "$cur" > "$STATE"

    waiting="$(printf '%s\n' "$cur" | awk -F'|' '$2=="queued"{print $1}' | sort | tr '\n' ' ')"
    if [ -n "$waiting" ] && ! box_busy; then
        if [ "$waiting" != "$last_drainable" ]; then
            echo "QUEUE DRAINABLE: box is free, $(printf '%s' "$waiting" | wc -w | tr -d ' ') request(s) waiting"
            last_drainable="$waiting"
        fi
    else
        last_drainable=""
    fi
    [ -n "${TEST_ONCE:-}" ] && break
    sleep 45
done
