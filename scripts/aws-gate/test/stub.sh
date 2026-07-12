#!/usr/bin/env bash
# Records every `aws` invocation to $HVG_STUB_LOG. If the sourcing test defines
# a `stub_response` function, each call is dispatched to it for canned output
# (it receives the same args as `aws`). Sourced by tests; makes no real calls.
aws() { echo "aws $*" >> "$HVG_STUB_LOG"; if declare -F stub_response >/dev/null; then stub_response "$@"; fi; }
