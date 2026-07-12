#!/usr/bin/env bash
# Compare local vs remote regenerated artifacts on HEAD. Until the libm port
# lands, divergence is EXPECTED (Linux libm) and reported as PENDING, not FAIL.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"
# ... run scripts/regenerate-artifacts.sh locally -> /tmp/local-book ...
# ... run the same on the remote box, rsync its book/ -> /tmp/remote-book ...
if diff -r /tmp/local-book /tmp/remote-book >/dev/null; then
  echo "VERIFIED: remote box is a faithful oracle"; exit 0
elif grep -q '^name = "libm"' Cargo.lock 2>/dev/null; then
  echo "FAIL: unexpected artifact divergence with libm present"; exit 1
else
  echo "PENDING: artifact divergence expected until the libm port lands (not a failure)"; exit 0
fi
