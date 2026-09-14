#!/usr/bin/env bash
# scripts/sluice-drainers.sh — who is draining the merge queue, and is it alive?
#
# WHY THIS EXISTS. `make sluice-status` shows queue ROWS. Until The Watchman
# nothing showed the LOOPS that consume them, so two operators could each leave
# a drainer running and neither instrument would say so — which happened on
# 2026-09-13, with one loop deliberately refusing to auto-drain merges and the
# other draining three. The `flock` on the box claim serializes the work and has
# no opinion about which loops should exist.
#
# THE LOCK IS THE TRUTH; THE FILE IS A LABEL. A drainer holds one flock per
# queue kind it claims, for the life of its loop, and writes a small
# registration file beside it. The file can outlive its writer (SIGKILL runs no
# trap); the lock cannot, because the kernel releases it when the holder dies.
# So this report never believes a file: it tests the lock, and calls a label
# whose lock is free exactly what it is.
#
# DIRECTION THIS CHECK ENFORCES: it reports what is registered and whether it is
# live. It does NOT prove a live drainer is healthy, is making progress, or has
# the policy you wanted — only that something holds that kind and who said it
# was.
set -u

HV_SLUICE_DIR="${HV_SLUICE_DIR:-$HOME/.local/state/hornvale/sluice}"
KINDS="${HV_DRAIN_KINDS:-merge stage census}"

live=0
stale=0
for k in $KINDS; do
    lk="$HV_SLUICE_DIR/drainer-$k.lock"
    rf="$HV_SLUICE_DIR/drainers/$k"
    # Nothing has ever claimed this kind on this box.
    if [ ! -e "$lk" ] && [ ! -e "$rf" ]; then continue; fi
    held=yes
    if ( flock -n 24 ) 24>"$lk" 2>/dev/null; then held=no; fi
    if [ "$held" = yes ]; then
        live=$((live + 1))
        if [ -r "$rf" ]; then
            printf 'drainer  %-7s LIVE   %s\n' "$k" "$(tr '\n' ' ' < "$rf")"
        else
            printf 'drainer  %-7s LIVE   (held, but the holder wrote no registration file)\n' "$k"
        fi
    elif [ -e "$rf" ]; then
        stale=$((stale + 1))
        printf 'drainer  %-7s STALE  %s\n' "$k" "$(tr '\n' ' ' < "$rf" 2>/dev/null)"
        printf 'drainer  %-7s        ^ the lock is FREE, so that drainer is gone. This kind is unclaimed.\n' ""
    fi
done

if [ "$live" -eq 0 ] && [ "$stale" -eq 0 ]; then
    echo "drainers: none registered — every queue kind is unclaimed"
elif [ "$live" -eq 0 ]; then
    echo "drainers: none live ($stale stale label(s) above) — every queue kind is unclaimed"
fi
