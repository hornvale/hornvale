#!/usr/bin/env bash
# Box bootstrap. Installs rustup (auto-selects the repo-pinned toolchain when a
# worktree is synced), sccache pointed at the shared S3 bucket, and a 15-min
# idle self-terminate timer. HVG_SCCACHE_BUCKET is templated in by setup.sh.
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
# cron runs the idle self-terminate; unzip unpacks the AWS CLI v2 installer.
# Ubuntu 24.04 (noble) dropped the `awscli` apt package ("no installation
# candidate"), which killed this whole script before rustup — install the
# official AWS CLI v2 bundle instead (arch-matched: aarch64 / x86_64).
apt-get update -y && apt-get install -y build-essential pkg-config git rsync curl unzip cron
systemctl enable --now cron || true
curl -sSL "https://awscli.amazonaws.com/awscli-exe-linux-$(uname -m).zip" -o /tmp/awscliv2.zip
unzip -q /tmp/awscliv2.zip -d /tmp
/tmp/aws/install
sudo -u ubuntu bash -lc 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
# cargo-nextest is the gate's test runner (decision 0040); prebuilt binary,
# arch-matched — `cargo install` would spend minutes compiling it per cold box.
nx=linux; [ "$(uname -m)" = aarch64 ] && nx=linux-arm
sudo -u ubuntu bash -c "curl -LsSf https://get.nexte.st/latest/$nx | tar zxf - -C /home/ubuntu/.cargo/bin"
# sccache (shared S3 cache) is a DEFERRED optimization: until it is wired,
# RUSTC_WRAPPER is left unset and cargo builds normally (correct, just cold).
# TODO(sccache): install the sccache binary (NOT sccache-dist) and set
# RUSTC_WRAPPER=sccache + SCCACHE_BUCKET in /etc/environment.
# Idle self-terminate: every minute, if the heartbeat is older than HVG idle,
# terminate. aws lives in /usr/local/bin, which cron's default PATH lacks.
cat >/usr/local/bin/hvg-idle-check <<'EOS'
#!/usr/bin/env bash
hb=/run/hvg-heartbeat; idle_min=15
[ -f "$hb" ] || { touch "$hb"; chown ubuntu:ubuntu "$hb"; }
if [ "$(( ($(date +%s) - $(stat -c %Y "$hb")) / 60 ))" -ge "$idle_min" ]; then
  # IMDSv2 token (works whether or not the instance enforces it).
  tok=$(curl -sX PUT "http://169.254.169.254/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 60")
  iid=$(curl -s -H "X-aws-ec2-metadata-token: $tok" http://169.254.169.254/latest/meta-data/instance-id)
  /usr/local/bin/aws ec2 terminate-instances --region us-east-1 --instance-ids "$iid"
fi
EOS
chmod +x /usr/local/bin/hvg-idle-check
echo '* * * * * root /usr/local/bin/hvg-idle-check' >/etc/cron.d/hvg-idle
