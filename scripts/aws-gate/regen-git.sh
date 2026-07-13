#!/usr/bin/env bash
# scripts/aws-gate/regen-git.sh — run an expensive command (default: the full
# artifact regeneration) on a spot box and return the result as a git commit,
# over SSH. No GitHub, no token on the box: the branch is pushed to a bare repo
# ON the box over the SSH we already have, the box commits ON the real branch
# and pushes back, and we fetch that commit locally for review. The artifacts
# come home as a durable, reviewable commit instead of a fragile rsync — and
# nothing is auto-merged: you review the fetched diff, then `git merge
# --ff-only FETCH_HEAD` yourself.
#
# Usage:
#   regen-git.sh <worktree> [-- <remote-cmd...>]     # default cmd: regenerate-artifacts.sh
#   regen-git.sh <worktree> --type c7a.4xlarge [-- ...]
#
# The remote-cmd is space-joined onto the box, so wrap anything with quotes,
# pipes, or redirects in a committed script and invoke that (the default,
# `bash scripts/regenerate-artifacts.sh`, is the intended shape).
#
# ALWAYS terminates the box on exit. The commit objects are fetched locally
# BEFORE teardown, so the box dying can't lose them.
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

TYPE="c7a.16xlarge"          # 64 vCPU — sized for the 1000-world census regen
ROOT_GB=80
SSH_KEY="$HOME/.hornvale-gate/id"

WT=""
REMOTE_CMD=(env HV_CENSUS=1 bash scripts/regenerate-artifacts.sh)
while [ "$#" -gt 0 ]; do
    case "$1" in
        --type) TYPE="$2"; shift 2;;
        --) shift; REMOTE_CMD=("$@"); break;;
        -*) echo "unknown flag: $1" >&2; exit 2;;
        *) if [ -z "$WT" ]; then WT="$1"; shift; else echo "unexpected arg: $1" >&2; exit 2; fi;;
    esac
done
[ -n "$WT" ] || { echo "usage: regen-git.sh <worktree> [--type T] [-- <cmd...>]" >&2; exit 2; }
WT="$(cd "$WT" && git rev-parse --show-toplevel)" || { echo "not a git worktree: $WT" >&2; exit 2; }
BRANCH="$(git -C "$WT" branch --show-current)"
[ -n "$BRANCH" ] || { echo "worktree is in detached HEAD; check out a branch first" >&2; exit 2; }
GIT_SSH="ssh -i $SSH_KEY -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10"
SSH=(ssh -i "$SSH_KEY" -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10)

echo "== regen-git: worktree $WT (branch $BRANCH), cmd: ${REMOTE_CMD[*]}"

sg="$(manifest_get security_group)"
[ -n "$sg" ] || { echo "no security_group in manifest — run setup.sh first" >&2; exit 1; }
ami="$(aws_admin ssm get-parameter --name "/aws/service/canonical/ubuntu/server/24.04/stable/current/amd64/hvm/ebs-gp3/ami-id" --query Parameter.Value --output text)"

ud="$(mktemp)"
cat > "$ud" <<'EOS'
#!/usr/bin/env bash
export DEBIAN_FRONTEND=noninteractive
apt-get update -y && apt-get install -y build-essential pkg-config git rsync curl
sudo -u ubuntu bash -lc 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
sudo -u ubuntu git config --global user.email regen@hornvale
sudo -u ubuntu git config --global user.name  hornvale-regen
sudo -u ubuntu git init --quiet --bare /home/ubuntu/hv.git
touch /run/hvg-ready
EOS
ud_b64="$(base64 < "$ud" | tr -d '\n')"

iid="$(aws_admin ec2 run-instances --image-id "$ami" --instance-type "$TYPE" \
  --instance-market-options 'MarketType=spot' \
  --security-group-ids "$sg" --key-name hornvale-gate \
  --metadata-options 'HttpTokens=optional' --user-data "$ud_b64" \
  --block-device-mappings "[{\"DeviceName\":\"/dev/sda1\",\"Ebs\":{\"VolumeSize\":${ROOT_GB},\"VolumeType\":\"gp3\",\"DeleteOnTermination\":true}}]" \
  --tag-specifications "ResourceType=instance,Tags=[{Key=project,Value=hornvale-gate},{Key=role,Value=diag},{Key=Name,Value=hvg-regen-git}]" \
  --query 'Instances[0].InstanceId' --output text)"
echo "== instance $iid ($TYPE)"
trap 'echo "== terminating $iid"; aws_admin ec2 terminate-instances --instance-ids "$iid" >/dev/null 2>&1 || true' EXIT

aws_admin ec2 wait instance-running --instance-ids "$iid"
ip="$(aws_admin ec2 describe-instances --instance-ids "$iid" --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)"
echo "== ip $ip; waiting for toolchain + bare repo..."
ready=0
for _ in $(seq 1 80); do
  if "${SSH[@]}" "ubuntu@$ip" 'test -f /run/hvg-ready' 2>/dev/null; then ready=1; break; fi
  sleep 15
done
[ "$ready" = 1 ] || { echo "== box never became ready" >&2; exit 1; }

echo "== push $BRANCH -> box:hv.git (branch history over SSH)"
GIT_SSH_COMMAND="$GIT_SSH" git -C "$WT" push --force "ubuntu@$ip:hv.git" "$BRANCH:$BRANCH"

# On the box: clone the pushed branch (real history), run the command, and
# commit the result ON the branch — or report no drift if the tree is clean.
cat > /tmp/regen-git-run.sh <<JOB
set -uo pipefail
source "\$HOME/.cargo/env"
rm -rf ~/work && git clone --quiet ~/hv.git ~/work && cd ~/work
git checkout --quiet "$BRANCH"
echo "REGEN_START \$(date +%s) on \$(nproc) cores"
${REMOTE_CMD[*]}
echo "REGEN_END \$(date +%s)"
git add -A
if git diff --cached --quiet; then
  echo "REGEN_RESULT no-drift"
else
  git commit --quiet -m "regen: artifacts regenerated on \$(uname -m)/\$(nproc)-core Linux

Produced by scripts/aws-gate/regen-git.sh (git-over-SSH). Review before merge.

Claude-Session: https://claude.ai/code/session_017rThA5iKFQqFwrAiE1ZHGB"
  git push --quiet origin "$BRANCH"
  echo "REGEN_RESULT committed \$(git rev-parse HEAD)"
  git -c core.fileMode=false diff --stat HEAD~1 HEAD
fi
JOB
scp -i "$SSH_KEY" -o StrictHostKeyChecking=accept-new /tmp/regen-git-run.sh "ubuntu@$ip:regen-git-run.sh"

echo "== running on box: ${REMOTE_CMD[*]}"
"${SSH[@]}" "ubuntu@$ip" "timeout 5400 bash regen-git-run.sh" 2>&1 | tee /tmp/hv-regen-git.log
result="$(grep '^REGEN_RESULT' /tmp/hv-regen-git.log | tail -1)"

if [[ "$result" == *no-drift* ]]; then
  echo "== NO DRIFT — the branch already carries the canonical artifacts; nothing to fetch."
  exit 0
fi

echo "== fetch the regen commit back (objects land locally, before teardown)"
GIT_SSH_COMMAND="$GIT_SSH" git -C "$WT" fetch "ubuntu@$ip:hv.git" "$BRANCH"
echo ""
echo "===== REVIEW: git diff HEAD..FETCH_HEAD (in $WT) ====="
git -C "$WT" -c core.fileMode=false diff --stat HEAD FETCH_HEAD
echo ""
echo "== FETCH_HEAD = $(git -C "$WT" rev-parse --short FETCH_HEAD). To land after review:"
echo "     git -C $WT merge --ff-only FETCH_HEAD"
