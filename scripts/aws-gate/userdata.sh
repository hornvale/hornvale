#!/usr/bin/env bash
# Box bootstrap. Installs rustup (auto-selects the repo-pinned toolchain when a
# worktree is synced), sccache pointed at the shared S3 bucket, and a 15-min
# idle self-terminate timer. HVG_SCCACHE_BUCKET is templated in by setup.sh.
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
apt-get update -y && apt-get install -y build-essential pkg-config git rsync curl
sudo -u ubuntu bash -lc 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
# sccache (prebuilt aarch64 musl release), shared cache in S3.
curl -sSL -o /tmp/sccache.tgz https://github.com/mozilla/sccache/releases/latest/download/sccache-dist-aarch64-unknown-linux-musl.tar.gz || true
# ... install sccache to /usr/local/bin, set RUSTC_WRAPPER + SCCACHE_BUCKET in /etc/environment ...
# Idle self-terminate: every minute, if the heartbeat is older than HVG idle, terminate.
cat >/usr/local/bin/hvg-idle-check <<'EOS'
#!/usr/bin/env bash
hb=/run/hvg-heartbeat; idle_min=15
[ -f "$hb" ] || touch "$hb"
if [ "$(( ($(date +%s) - $(stat -c %Y "$hb")) / 60 ))" -ge "$idle_min" ]; then
  iid=$(curl -s http://169.254.169.254/latest/meta-data/instance-id)
  aws ec2 terminate-instances --region us-east-1 --instance-ids "$iid"
fi
EOS
chmod +x /usr/local/bin/hvg-idle-check
echo '* * * * * root /usr/local/bin/hvg-idle-check' >/etc/cron.d/hvg-idle
