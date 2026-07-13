#!/usr/bin/env bash
# scripts/aws-gate/gate-remote.sh — run the CI Rust gate on this worktree's
# remote spot box. Reuses a running box, else launches one; rsyncs the worktree,
# runs the gate over SSH streaming output, syncs regenerated artifacts back.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

find_or_launch() {
  local wt; wt="$(worktree_name)"
  local id
  id="$(aws_runner ec2 describe-instances \
      --filters "Name=tag:project,Values=hornvale-gate" "Name=tag:worktree,Values=$wt" \
                "Name=instance-state-name,Values=pending,running" \
      --query 'Reservations[0].Instances[0].InstanceId' --output text 2>/dev/null || true)"
  if [ -n "$id" ] && [ "$id" != "None" ]; then echo "$id"; return; fi
  # Concurrency guard: refuse if MAX_COUNT boxes already run.
  local n; n="$(aws_runner ec2 describe-instances --filters "Name=tag:project,Values=hornvale-gate" \
      "Name=instance-state-name,Values=pending,running" --query 'length(Reservations[].Instances[])' --output text)"
  [ "${n:-0}" -lt "$HVG_MAX_COUNT" ] || { echo "MAX_COUNT ($HVG_MAX_COUNT) reached; refusing to launch" >&2; return 1; }
  aws_runner ec2 run-instances --launch-template "LaunchTemplateId=$(manifest_get launch_template)" \
      --instance-market-options 'MarketType=spot' \
      --tag-specifications "ResourceType=instance,Tags=[{Key=project,Value=hornvale-gate},{Key=worktree,Value=$wt}]" \
      --query 'Instances[0].InstanceId' --output text
}

main() {
  local id ip
  id="$(find_or_launch)" || exit 1
  aws_runner ec2 wait instance-running --instance-ids "$id"
  ip="$(aws_runner ec2 describe-instances --instance-ids "$id" --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)"
  local ssh="ssh -i $HOME/.hornvale-gate/id -o StrictHostKeyChecking=accept-new ubuntu@$ip"
  $ssh 'touch /run/hvg-heartbeat'                       # refresh idle timer
  rsync -az --delete --exclude target/ --filter=':- .gitignore' -e "ssh -i $HOME/.hornvale-gate/id -o StrictHostKeyChecking=accept-new" \
      "$(git rev-parse --show-toplevel)/" "ubuntu@$ip:work/"
  # Keep the heartbeat fresh during the gate: a cold build can exceed the 15-min
  # idle timeout, and a single touch-at-start would let the box self-terminate
  # mid-run. Background loop, reaped after the gate returns.
  # shellcheck disable=SC2086
  ( while sleep 300; do $ssh 'touch /run/hvg-heartbeat' 2>/dev/null || break; done ) &
  local hb_pid=$!
  # shellcheck disable=SC2086
  local rc=0
  $ssh 'cd work && make gate && scripts/regenerate-artifacts.sh && git -c core.fileMode=false diff --exit-code -- book/' || rc=$?
  kill "$hb_pid" 2>/dev/null || true
  # Sync any regenerated artifacts back so legitimate changes can be committed locally.
  rsync -az -e "ssh -i $HOME/.hornvale-gate/id -o StrictHostKeyChecking=accept-new" "ubuntu@$ip:work/book/" "$(git rev-parse --show-toplevel)/book/"
  [ $rc -eq 0 ] && echo "gate-remote: PASS" || echo "gate-remote: FAIL (rc=$rc)"
  return $rc
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  main "$@"
fi
