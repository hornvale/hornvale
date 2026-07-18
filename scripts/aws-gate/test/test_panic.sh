#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export HVG_STUB_LOG; HVG_STUB_LOG="$(mktemp)"
# shellcheck source=/dev/null
source "$here/stub.sh"
stub_response() { case "$*" in *"list-access-keys"*) echo '{"AccessKeyMetadata":[{"AccessKeyId":"AKIA"}]}';;
                                *"describe-instances"*) echo "i-1 i-2";; esac; }
# shellcheck source=/dev/null
source "$here/../lib.sh"; # shellcheck source=/dev/null
source "$here/../panic.sh"
panic_all
# The access key must be deactivated BEFORE any terminate call.
key_line=$(grep -n "update-access-key.*Inactive" "$HVG_STUB_LOG" | head -1 | cut -d: -f1)
term_line=$(grep -n "terminate-instances" "$HVG_STUB_LOG" | head -1 | cut -d: -f1)
if ! { [ -n "$key_line" ] && [ "$key_line" -lt "${term_line:-99999}" ]; }; then
  echo "FAIL: deactivate key before terminate"; exit 1
fi
echo "test_panic: OK"
