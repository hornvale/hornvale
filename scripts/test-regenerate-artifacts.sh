#!/usr/bin/env bash
# scripts/test-regenerate-artifacts.sh — guard the independent census trailer.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/scripts/regenerate-artifacts.sh"

awk '
  /^[[:space:]]*(spawn )?run -p hornvale -- lab domesday[[:space:]]*$/ {
    domesday = NR
    domesday_spawned = ($0 ~ /^[[:space:]]*spawn /)
  }
  /^[[:space:]]*(spawn )?run -p hornvale -- lab anomalies[[:space:]]*$/ {
    anomalies = NR
    anomalies_spawned = ($0 ~ /^[[:space:]]*spawn /)
  }
  /^[[:space:]]*reap[[:space:]]*$/ {
    if (domesday && anomalies && NR > anomalies && !reaped) {
      trailer_reap = NR
      reaped = 1
    }
  }
  END {
    if (!domesday || !anomalies) {
      print "FAIL: census trailer commands are missing" > "/dev/stderr"
      exit 1
    }
    if (!domesday_spawned || !anomalies_spawned) {
      print "FAIL: Domesday and anomalies must both be spawned" > "/dev/stderr"
      exit 1
    }
    if (!trailer_reap) {
      print "FAIL: census trailer has no reap after both jobs" > "/dev/stderr"
      exit 1
    }
    print "ok: Domesday and anomalies share a reaped parallel trailer"
  }
' "$script"
