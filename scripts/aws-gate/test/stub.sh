#!/usr/bin/env bash
# Records every `aws` invocation to $HVG_STUB_LOG and echoes canned output
# from $HVG_STUB_OUT (a function name the test defines). Sourced by tests.
aws() { echo "aws $*" >> "$HVG_STUB_LOG"; if declare -F stub_response >/dev/null; then stub_response "$@"; fi; }
