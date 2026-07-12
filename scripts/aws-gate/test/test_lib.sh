#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export HVG_STUB_LOG; HVG_STUB_LOG="$(mktemp)"
# shellcheck source=/dev/null
source "$here/stub.sh"
# shellcheck source=/dev/null
source "$here/../lib.sh"

fail() { echo "FAIL: $1" >&2; exit 1; }

# aws_admin uses default creds (no --profile); aws_runner pins the profile.
aws_admin ec2 describe-instances >/dev/null
grep -q -- "--profile" "$HVG_STUB_LOG" && fail "aws_admin must NOT pin a profile"
: > "$HVG_STUB_LOG"
aws_runner ec2 describe-instances >/dev/null
grep -q -- "--profile $HVG_PROFILE" "$HVG_STUB_LOG" || fail "aws_runner must pin $HVG_PROFILE"
grep -q -- "--region $HVG_REGION" "$HVG_STUB_LOG" || fail "aws_runner must pin region"

# manifest round-trips through a temp file.
export HVG_MANIFEST; HVG_MANIFEST="$(mktemp)"; echo '{}' > "$HVG_MANIFEST"
manifest_set launch_template lt-abc123
[ "$(manifest_get launch_template)" = "lt-abc123" ] || fail "manifest round-trip"

echo "test_lib: OK"
