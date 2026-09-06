#!/usr/bin/env bash
# Explicit Charter diagnostic; never a routine gate or a census runner.
set -euo pipefail
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
exec python3 "$script_dir/charter_measure.py" "$@"
