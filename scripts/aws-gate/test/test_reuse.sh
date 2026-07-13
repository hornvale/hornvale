#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export HVG_STUB_LOG; HVG_STUB_LOG="$(mktemp)"
# shellcheck source=/dev/null
source "$here/stub.sh"
# A running box for this worktree already exists -> reuse, no run-instances.
stub_response() { case "$*" in *"describe-instances"*) echo "i-existing";; esac; }
# shellcheck source=/dev/null
source "$here/../lib.sh"; # shellcheck source=/dev/null
source "$here/../gate-remote.sh"  # must define find_or_launch without executing main
id="$(find_or_launch)"
[ "$id" = "i-existing" ] || { echo "FAIL: should reuse running box"; exit 1; }
grep -q "run-instances" "$HVG_STUB_LOG" && { echo "FAIL: must not launch when one exists"; exit 1; }
echo "test_reuse: OK"
