#!/usr/bin/env bash
# bench-box.sh — a warm, interactively-drivable diagnostic EC2 box.
#
# Decouples "keep a box alive" from "run a thing" so an iterative investigation
# (run subset -> flamegraph -> tweak -> re-run) costs seconds/iteration, not a
# 4-minute launch each time. Commands are base64-piped to the box, so there is
# zero SSH quoting to fight. Long jobs run detached with a tailable log, so we
# are never blind mid-run.
#
# Verbs:
#   up <instance-type> [name]   launch+setup(toolchain,perf,FlameGraph)+rsync worktree
#   sync [name]                 re-rsync the local worktree to the box
#   run  [name] <cmd...>        run <cmd> in ~/work, stream output (short things)
#   bg   [name] <cmd...>        run <cmd> detached -> ~/job.log; returns immediately
#   tail [name] [n]             tail ~/job.log (default 40 lines)
#   flame[name] <cmd...>        perf-record <cmd>, build a flamegraph, pull the .svg
#   pull [name] <remote> <local>  rsync a path off the box
#   list                        show boxes + live state
#   down [name|--all]           terminate
#
# Diag boxes are tagged project=hornvale-gate (role=diag), so the circuit
# breaker/panic/teardown all cover them; the 2h MAX_AGE is the backstop if you
# forget `down`. For a single run longer than 2h, raise HVG_MAX_AGE_SECS.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

STATE_DIR="$HOME/.hornvale-gate/boxes"; mkdir -p "$STATE_DIR"
SSH_OPTS=(-i "$HOME/.hornvale-gate/id" -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10)

_arch_of() { case "${1%%.*}" in *[0-9]g*) echo arm64;; *) echo amd64;; esac; }
_current() { cat "$STATE_DIR/.current" 2>/dev/null || true; }
_resolve() { # echo the box name to operate on ($1 if a known box, else current)
  if [ -n "${1:-}" ] && [ -f "$STATE_DIR/$1" ]; then echo "$1"; else _current; fi
}
_ip_of() { grep '^ip=' "$STATE_DIR/$1" | cut -d= -f2; }
_iid_of() { grep '^iid=' "$STATE_DIR/$1" | cut -d= -f2; }
# Pipe a remote bash script with no quoting worries.
_remote() { # NAME SCRIPT...
  local name="$1"; shift
  local b64; b64="$(printf '%s' "$*" | base64 | tr -d '\n')"
  # shellcheck disable=SC2029
  ssh "${SSH_OPTS[@]}" "ubuntu@$(_ip_of "$name")" "echo $b64 | base64 -d | bash"
}

cmd_up() {
  local type="$1" name="${2:-$1}" arch ami
  arch="$(_arch_of "$type")"
  ami="$(aws_admin ssm get-parameter --name "/aws/service/canonical/ubuntu/server/24.04/stable/current/$arch/hvm/ebs-gp3/ami-id" --query Parameter.Value --output text)"
  local sg; sg="$(manifest_get security_group)"
  [ -n "$sg" ] || { echo "run setup.sh first (no security_group)" >&2; return 1; }
  echo "== up '$name': $type ($arch, $ami)"
  local ud b64
  ud="$(mktemp)"
  cat > "$ud" <<'EOS'
#!/usr/bin/env bash
export DEBIAN_FRONTEND=noninteractive
apt-get update -y
apt-get install -y build-essential pkg-config git rsync curl linux-tools-common linux-tools-aws
sudo -u ubuntu bash -lc 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
git clone --depth 1 https://github.com/brendangregg/FlameGraph /opt/FlameGraph || true
echo 'kernel.perf_event_paranoid=-1' > /etc/sysctl.d/99-perf.conf
echo 'kernel.kptr_restrict=0' >> /etc/sysctl.d/99-perf.conf
sysctl --system >/dev/null 2>&1 || true
touch /run/hvg-ready
EOS
  b64="$(base64 < "$ud" | tr -d '\n')"
  local iid
  iid="$(aws_admin ec2 run-instances --image-id "$ami" --instance-type "$type" \
    --instance-market-options 'MarketType=spot' \
    --security-group-ids "$sg" --key-name hornvale-gate \
    --metadata-options 'HttpTokens=optional' --user-data "$b64" \
    --block-device-mappings '[{"DeviceName":"/dev/sda1","Ebs":{"VolumeSize":60,"VolumeType":"gp3","DeleteOnTermination":true}}]' \
    --tag-specifications "ResourceType=instance,Tags=[{Key=project,Value=hornvale-gate},{Key=role,Value=diag},{Key=Name,Value=hvg-diag-$name}]" \
    --query 'Instances[0].InstanceId' --output text)"
  aws_admin ec2 wait instance-running --instance-ids "$iid"
  local ip; ip="$(aws_admin ec2 describe-instances --instance-ids "$iid" --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)"
  printf 'iid=%s\nip=%s\ntype=%s\n' "$iid" "$ip" "$type" > "$STATE_DIR/$name"
  echo "$name" > "$STATE_DIR/.current"
  echo "== $iid @ $ip; waiting for toolchain..."
  local ready=0
  for _ in $(seq 1 80); do
    if ssh "${SSH_OPTS[@]}" "ubuntu@$ip" 'test -f /run/hvg-ready' 2>/dev/null; then ready=1; break; fi
    sleep 15
  done
  [ "$ready" = 1 ] || { echo "== never became ready" >&2; return 1; }
  cmd_sync "$name"
  echo "== box '$name' READY. Try: $0 run $name -- cargo test -p hornvale-worldgen --no-run"
}

cmd_sync() {
  local name; name="$(_resolve "${1:-}")"
  local root; root="$(git -C "$here" rev-parse --show-toplevel)"
  echo "== rsync worktree -> '$name'"
  rsync -az --delete --exclude target/ --exclude .git/ --filter=':- .gitignore' \
    -e "ssh ${SSH_OPTS[*]}" "$root/" "ubuntu@$(_ip_of "$name"):work/"
}

cmd_run() {
  local name; name="$(_resolve "${1:-}")"; if [ "${1:-}" = "$name" ]; then shift; fi; if [ "${1:-}" = "--" ]; then shift; fi
  _remote "$name" "cd \"\$HOME/work\" && source \"\$HOME/.cargo/env\" && $*"
}

cmd_bg() {
  local name; name="$(_resolve "${1:-}")"; if [ "${1:-}" = "$name" ]; then shift; fi; if [ "${1:-}" = "--" ]; then shift; fi
  _remote "$name" "cat > /tmp/job.sh <<'JOB'
cd \"\$HOME/work\" && source \"\$HOME/.cargo/env\"
$*
JOB
nohup bash /tmp/job.sh > \$HOME/job.log 2>&1 & echo \"started pid \$! -> ~/job.log\""
}

cmd_tail() {
  local name; name="$(_resolve "${1:-}")"; if [ "${1:-}" = "$name" ]; then shift; fi
  _remote "$name" "tail -n ${1:-40} \$HOME/job.log 2>/dev/null || echo '(no job.log yet)'"
}

cmd_flame() {
  local name; name="$(_resolve "${1:-}")"; if [ "${1:-}" = "$name" ]; then shift; fi; if [ "${1:-}" = "--" ]; then shift; fi
  echo "== perf-recording on '$name' (this runs your command under perf)"
  _remote "$name" "cd \"\$HOME/work\" && source \"\$HOME/.cargo/env\"
perf record -F 99 -g -o /tmp/perf.data -- bash -c '$*'
perf script -i /tmp/perf.data | /opt/FlameGraph/stackcollapse-perf.pl | /opt/FlameGraph/flamegraph.pl > /tmp/flame.svg
echo flame-bytes: \$(wc -c < /tmp/flame.svg)"
  local out; out="./flame-${name}-$(date +%s).svg"
  rsync -az -e "ssh ${SSH_OPTS[*]}" "ubuntu@$(_ip_of "$name"):/tmp/flame.svg" "$out"
  echo "== flamegraph -> $out"
}

cmd_pull() {
  local name; name="$(_resolve "${1:-}")"; if [ "${1:-}" = "$name" ]; then shift; fi
  rsync -az -e "ssh ${SSH_OPTS[*]}" "ubuntu@$(_ip_of "$name"):$1" "$2"
  echo "== pulled $1 -> $2"
}

cmd_list() {
  local f name
  for f in "$STATE_DIR"/*; do
    [ -f "$f" ] || continue; name="$(basename "$f")"; [ "$name" = ".current" ] && continue
    local iid; iid="$(_iid_of "$name")"
    local state; state="$(aws_admin ec2 describe-instances --instance-ids "$iid" --query 'Reservations[0].Instances[0].State.Name' --output text 2>/dev/null || echo gone)"
    printf '  %-16s %s %s %s%s\n' "$name" "$iid" "$(_ip_of "$name")" "$state" "$([ "$name" = "$(_current)" ] && echo ' *')"
  done
}

cmd_down() {
  local name
  if [ "${1:-}" = "--all" ]; then
    for f in "$STATE_DIR"/*; do
      [ -f "$f" ] || continue; name="$(basename "$f")"; [ "$name" = ".current" ] && continue
      aws_admin ec2 terminate-instances --instance-ids "$(_iid_of "$name")" >/dev/null 2>&1 || true
      rm -f "$f"; echo "== down $name"
    done
    rm -f "$STATE_DIR/.current"; return
  fi
  name="$(_resolve "${1:-}")"
  aws_admin ec2 terminate-instances --instance-ids "$(_iid_of "$name")" >/dev/null 2>&1 || true
  rm -f "$STATE_DIR/$name"; if [ "$(_current)" = "$name" ]; then rm -f "$STATE_DIR/.current"; fi
  echo "== down $name"
}

case "${1:-}" in
  up)    shift; cmd_up "$@";;
  sync)  shift; cmd_sync "$@";;
  run)   shift; cmd_run "$@";;
  bg)    shift; cmd_bg "$@";;
  tail)  shift; cmd_tail "$@";;
  flame) shift; cmd_flame "$@";;
  pull)  shift; cmd_pull "$@";;
  list)  shift; cmd_list "$@";;
  down)  shift; cmd_down "$@";;
  *) echo "usage: $0 {up <type> [name]|sync|run <cmd>|bg <cmd>|tail [n]|flame <cmd>|pull <remote> <local>|list|down [name|--all]}" >&2; exit 1;;
esac
