#!/usr/bin/env bash
# One-off BASELINE benchmark: run the CURRENT suite (no vendored libm; platform
# glibc) on an x64 Linux spot box and measure whether it COMPLETES in reasonable
# time. Determinism/artifact tests WILL fail on Linux (glibc libm != Apple's) —
# that is expected and ignored; we measure wall-time + per-binary timing only.
#
# Admin creds (arbitrary instance type / x64 AMI, not the c7g-pinned runner).
# ALWAYS terminates the box on exit. The box is tagged project=hornvale-gate, so
# the 5-min circuit breaker reaps it at MAX_AGE (2h) if this script ever dies.
#
# Usage: bench-baseline.sh [instance-type] [timeout-secs]
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

TYPE="${1:-c7a.4xlarge}"       # AMD x64, 16 vCPU
TIMEOUT_SECS="${2:-2700}"      # 45 min
ARCH_PARAM="amd64"             # this baseline is x64
REPO_ROOT="$(git -C "$here" rev-parse --show-toplevel)"
LOG="/tmp/hvg-bench-${TYPE}.log"

sg="$(manifest_get security_group)"
[ -n "$sg" ] || { echo "no security_group in manifest — run setup.sh first" >&2; exit 1; }
ami="$(aws_admin ssm get-parameter \
  --name "/aws/service/canonical/ubuntu/server/24.04/stable/current/${ARCH_PARAM}/hvm/ebs-gp3/ami-id" \
  --query Parameter.Value --output text)"
echo "== $TYPE / ami $ami / timeout ${TIMEOUT_SECS}s"

ud="$(mktemp)"
cat > "$ud" <<'EOS'
#!/usr/bin/env bash
export DEBIAN_FRONTEND=noninteractive
apt-get update -y && apt-get install -y build-essential pkg-config git rsync curl
sudo -u ubuntu bash -lc 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
touch /run/hvg-ready
EOS
ud_b64="$(base64 < "$ud" | tr -d '\n')"

# 60 GB gp3 root — the default (~8 GB) fills during the cold workspace build and
# `ld` dies with SIGBUS. (Ubuntu Canonical AMIs root at /dev/sda1.)
iid="$(aws_admin ec2 run-instances --image-id "$ami" --instance-type "$TYPE" \
  --instance-market-options 'MarketType=spot' \
  --security-group-ids "$sg" --key-name hornvale-gate \
  --metadata-options 'HttpTokens=optional' --user-data "$ud_b64" \
  --block-device-mappings '[{"DeviceName":"/dev/sda1","Ebs":{"VolumeSize":60,"VolumeType":"gp3","DeleteOnTermination":true}}]' \
  --tag-specifications "ResourceType=instance,Tags=[{Key=project,Value=hornvale-gate},{Key=role,Value=bench},{Key=Name,Value=hvg-bench-$TYPE}]" \
  --query 'Instances[0].InstanceId' --output text)"
echo "== instance $iid"
trap 'echo "== terminating $iid"; aws_admin ec2 terminate-instances --instance-ids "$iid" >/dev/null 2>&1 || true' EXIT

aws_admin ec2 wait instance-running --instance-ids "$iid"
ip="$(aws_admin ec2 describe-instances --instance-ids "$iid" --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)"
echo "== ip $ip; waiting for toolchain (userdata)..."
SSH=(ssh -i "$HOME/.hornvale-gate/id" -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10)
ready=0
for _ in $(seq 1 80); do
  if "${SSH[@]}" "ubuntu@$ip" 'test -f /run/hvg-ready' 2>/dev/null; then ready=1; break; fi
  sleep 15
done
[ "$ready" = 1 ] || { echo "== box never became ready" >&2; exit 1; }
echo "== ready; rsync worktree"
rsync -az --delete --exclude target/ --exclude .git/ --filter=':- .gitignore' \
  -e "ssh -i $HOME/.hornvale-gate/id -o StrictHostKeyChecking=accept-new" \
  "$REPO_ROOT/" "ubuntu@$ip:work/"
scp -i "$HOME/.hornvale-gate/id" -o StrictHostKeyChecking=accept-new \
  "$here/bench-run.sh" "ubuntu@$ip:bench-run.sh"

echo "== running suite (this cold-builds first, then tests; timeout ${TIMEOUT_SECS}s)"
t0="$(date +%s)"
"${SSH[@]}" "ubuntu@$ip" "timeout ${TIMEOUT_SECS} bash bench-run.sh" > "$LOG" 2>&1 && rc=0 || rc=$?
t1="$(date +%s)"

echo "== RESULT ($TYPE) =="
echo "  wall seconds (incl. cold build): $((t1 - t0))"
if [ "$rc" = 124 ]; then
  echo "  OUTCOME: TIMED OUT at ${TIMEOUT_SECS}s — suite did NOT complete"
else
  echo "  OUTCOME: run finished (ssh rc=$rc); see SUITE_EXIT line for cargo's own code"
fi
echo "  --- last 5 log lines ---"; tail -5 "$LOG"
echo "  --- per-binary durations (from timestamps) ---"
awk '/Running / {split($0,a," "); ts=a[1]; if (prev) printf "    %5ds  %s\n", ts-prev, prevname; prev=ts; sub(/^[0-9]+ /,""); prevname=$0} END {if (prev) printf "    (last binary or hang)  %s\n", prevname}' "$LOG" | head -40
echo "== full log: $LOG"
