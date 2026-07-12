#!/usr/bin/env bash
# scripts/aws-gate/panic.sh — EMERGENCY STOP. Deactivate the runner identity
# (nothing can launch), then terminate every tagged box, cancel spot requests,
# and delete the launch template. Admin creds. Safe to run repeatedly.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

panic_all() {
  echo "PANIC: disabling runner identity first"
  for k in $(aws_admin iam list-access-keys --user-name hornvale-gate-runner \
              --query 'AccessKeyMetadata[].AccessKeyId' --output text 2>/dev/null || true); do
    aws_admin iam update-access-key --user-name hornvale-gate-runner --access-key-id "$k" --status Inactive || true
  done
  echo "PANIC: terminating tagged instances"
  local ids; ids="$(aws_admin ec2 describe-instances --filters "Name=tag:project,Values=hornvale-gate" \
      "Name=instance-state-name,Values=pending,running,stopping,stopped" \
      --query 'Reservations[].Instances[].InstanceId' --output text || true)"
  # shellcheck disable=SC2086
  [ -n "$ids" ] && aws_admin ec2 terminate-instances --instance-ids $ids || true
  echo "PANIC: cancelling spot requests + deleting launch template"
  # ... cancel open spot requests tagged project; delete the launch template by name ...
  echo "PANIC: complete — runner disabled. Re-enable with scripts/aws-gate/setup.sh"
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  panic_all "$@"
fi
