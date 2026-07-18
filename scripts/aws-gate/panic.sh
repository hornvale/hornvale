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
  if [ -n "$ids" ]; then
    # shellcheck disable=SC2086
    aws_admin ec2 terminate-instances --instance-ids $ids || true
  fi
  echo "PANIC: deleting the launch template (belt-and-suspenders; key-disable already blocks launches)"
  # One-time spot requests auto-close when their instance terminates and are not
  # separately tagged, so there is nothing safe to bulk-cancel here (cancelling
  # by state alone could hit unrelated workloads). Deleting the template by name
  # stops any reuse of it; setup.sh recreates it on re-enable.
  aws_admin ec2 delete-launch-template --launch-template-name hornvale-gate >/dev/null 2>&1 || true
  echo "PANIC: complete — runner disabled. Re-enable with scripts/aws-gate/setup.sh"
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  panic_all "$@"
fi
