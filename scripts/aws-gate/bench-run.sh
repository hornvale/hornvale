#!/usr/bin/env bash
# Runs ON the bench box. Times the full suite with per-line epoch timestamps so
# per-test-binary durations can be recovered. Pass/fail is irrelevant here — we
# measure whether it COMPLETES and how long each binary takes. --no-fail-fast so
# a failing artifact/determinism test (expected on Linux libm) doesn't stop the run.
set -uo pipefail
cd "$HOME/work" || exit 1
# shellcheck source=/dev/null
source "$HOME/.cargo/env"
echo "SUITE_START $(date +%s)"
cargo test --workspace --no-fail-fast 2>&1 \
  | while IFS= read -r l; do printf '%s %s\n' "$(date +%s)" "$l"; done
echo "SUITE_EXIT ${PIPESTATUS[0]} $(date +%s)"
