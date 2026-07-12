# Remote Gate on AWS Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A `make gate-remote` wrapper that runs the authoritative CI Rust gate on an ephemeral AWS spot instance per worktree, with least-privilege isolation and layered kill switches, so parallel sessions stop contending for local cores.

**Architecture:** Plain AWS CLI shell scripts under `scripts/aws-gate/`, tagged `project=hornvale-gate` so teardown/panic enumerate-by-tag (complete by construction). A dedicated `hornvale-gate-runner` IAM identity launches boxes; setup/teardown/panic use the developer's admin creds. A 5-min circuit-breaker Lambda + a 15-min idle self-terminate + a $25/day budget action + a manual panic script bound worst-case spend.

**Tech Stack:** bash (`set -euo pipefail`), AWS CLI v2, Python 3 (Lambda, stdlib only), `shellcheck`, `rsync`, `ssh`. No CloudFormation/Terraform/Ansible. No new Rust deps.

## Global Constraints

- **Region us-east-1; instance `c7g.4xlarge` spot; ARM64 (Graviton).** These are fixed by the spec; the runner IAM policy enforces them as conditions.
- **Everything tagged `project=hornvale-gate`.** Teardown and panic enumerate by this tag.
- **Two credential contexts:** `aws_admin` (developer's default creds — setup, teardown, panic) and `aws_runner` (`--profile hornvale-gate` — launch/terminate from the wrapper). The runner cannot disable itself; only admin can.
- **Idle timeout 15 min; MAX_COUNT 10; MAX_AGE 2h; budget alert $10/day; budget hard action $25/day.** All as named shell constants in one place (`lib.sh`).
- **Toolchain pins to 1.96.1** via `rust-toolchain.toml`; the box installs `rustup`, which auto-selects it. Never hard-code the version in userdata.
- **The panic path must not depend on any provisioning state or IaC tool** — direct AWS API calls only.
- **Every shell script is `shellcheck`-clean** and starts `#!/usr/bin/env bash` + `set -euo pipefail`, matching `scripts/gate-fast.sh`.
- **Billable steps are confirmation-gated:** no script that creates AWS resources or launches an instance runs without explicit developer go-ahead. Building/committing scripts is free; only Task 8 spends money.
- **Determinism go-live is separate:** the tool is built and mergeable now; the artifact-drift half is trusted only once the `libm` port makes Linux byte-identical (verified by `gate-remote-verify`).

---

### Task 1: `lib.sh` foundations + stubbed test harness

**Files:**
- Create: `scripts/aws-gate/lib.sh`
- Create: `scripts/aws-gate/test/test_lib.sh`
- Create: `scripts/aws-gate/test/stub.sh`

**Interfaces:**
- Produces: constants (`HVG_REGION`, `HVG_INSTANCE_TYPE`, `HVG_PROJECT_TAG`, `HVG_PROFILE`, `HVG_MAX_COUNT`, `HVG_MAX_AGE_SECS`, `HVG_IDLE_MIN`, `HVG_BUDGET_ALERT`, `HVG_BUDGET_HARD`, `HVG_MANIFEST`); functions `aws_admin`, `aws_runner`, `worktree_name`, `manifest_get KEY`, `manifest_set KEY VALUE`, `tagged_arns` (query Resource Groups Tagging API for the project tag).

- [ ] **Step 1: Write the failing test**

`scripts/aws-gate/test/stub.sh` — a reusable AWS stub that records calls:
```bash
#!/usr/bin/env bash
# Records every `aws` invocation to $HVG_STUB_LOG and echoes canned output
# from $HVG_STUB_OUT (a function name the test defines). Sourced by tests.
aws() { echo "aws $*" >> "$HVG_STUB_LOG"; if declare -F stub_response >/dev/null; then stub_response "$@"; fi; }
```

`scripts/aws-gate/test/test_lib.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export HVG_STUB_LOG; HVG_STUB_LOG="$(mktemp)"
# shellcheck source=/dev/null
source "$here/stub.sh"
# shellcheck source=/dev/null
source "$here/../lib.sh"

fail() { echo "FAIL: $1" >&2; exit 1; }

# aws_admin uses default creds (no --profile); aws_runner pins the profile.
aws_admin ec2 describe-instances >/dev/null
grep -q -- "--profile" "$HVG_STUB_LOG" && fail "aws_admin must NOT pin a profile"
: > "$HVG_STUB_LOG"
aws_runner ec2 describe-instances >/dev/null
grep -q -- "--profile $HVG_PROFILE" "$HVG_STUB_LOG" || fail "aws_runner must pin $HVG_PROFILE"
grep -q -- "--region $HVG_REGION" "$HVG_STUB_LOG" || fail "aws_runner must pin region"

# manifest round-trips through a temp file.
export HVG_MANIFEST; HVG_MANIFEST="$(mktemp)"; echo '{}' > "$HVG_MANIFEST"
manifest_set launch_template lt-abc123
[ "$(manifest_get launch_template)" = "lt-abc123" ] || fail "manifest round-trip"

echo "test_lib: OK"
```

- [ ] **Step 2: Run it, verify it fails**

Run: `bash scripts/aws-gate/test/test_lib.sh`
Expected: FAIL — `lib.sh` does not exist / functions undefined.

- [ ] **Step 3: Write `lib.sh`**

```bash
#!/usr/bin/env bash
# scripts/aws-gate/lib.sh — shared constants and helpers for the remote gate.
# Sourced by setup/teardown/gate-remote/panic; never executed directly.

HVG_REGION="${HVG_REGION:-us-east-1}"
HVG_INSTANCE_TYPE="${HVG_INSTANCE_TYPE:-c7g.4xlarge}"
HVG_PROJECT_TAG="${HVG_PROJECT_TAG:-project=hornvale-gate}"
HVG_PROFILE="${HVG_PROFILE:-hornvale-gate}"
HVG_MAX_COUNT="${HVG_MAX_COUNT:-10}"
HVG_MAX_AGE_SECS="${HVG_MAX_AGE_SECS:-7200}"   # 2h
HVG_IDLE_MIN="${HVG_IDLE_MIN:-15}"
HVG_BUDGET_ALERT="${HVG_BUDGET_ALERT:-10}"
HVG_BUDGET_HARD="${HVG_BUDGET_HARD:-25}"
HVG_MANIFEST="${HVG_MANIFEST:-$HOME/.hornvale-gate/manifest.json}"

# Admin context: the developer's default credentials (setup/teardown/panic).
aws_admin() { aws --region "$HVG_REGION" "$@"; }
# Runner context: the boxed-in launcher identity (gate-remote launch/terminate).
aws_runner() { aws --profile "$HVG_PROFILE" --region "$HVG_REGION" "$@"; }

worktree_name() { basename "$(git rev-parse --show-toplevel)"; }

manifest_get() { # KEY -> value ("" if absent); pure text, no jq dependency assumption
    python3 -c 'import json,sys;print(json.load(open(sys.argv[1])).get(sys.argv[2],""))' "$HVG_MANIFEST" "$1"
}
manifest_set() { # KEY VALUE
    mkdir -p "$(dirname "$HVG_MANIFEST")"
    [ -f "$HVG_MANIFEST" ] || echo '{}' > "$HVG_MANIFEST"
    python3 -c 'import json,sys;p=sys.argv[1];d=json.load(open(p));d[sys.argv[2]]=sys.argv[3];json.dump(d,open(p,"w"),indent=2,sort_keys=True)' "$HVG_MANIFEST" "$1" "$2"
}

tagged_arns() { # -> ARNs carrying the project tag, one per line
    local key="${HVG_PROJECT_TAG%%=*}" val="${HVG_PROJECT_TAG#*=}"
    aws_admin resourcegroupstaggingapi get-resources \
        --tag-filters "Key=$key,Values=$val" \
        --query 'ResourceTagMappingList[].ResourceARN' --output text | tr '\t' '\n'
}
```

- [ ] **Step 4: Run it, verify it passes**

Run: `bash scripts/aws-gate/test/test_lib.sh`
Expected: `test_lib: OK`. Then `shellcheck scripts/aws-gate/lib.sh scripts/aws-gate/test/*.sh` — clean.

- [ ] **Step 5: Commit**

```bash
git add scripts/aws-gate/lib.sh scripts/aws-gate/test/
git commit -m "feat(aws-gate): lib.sh constants + admin/runner creds + manifest + stub test harness"
```

---

### Task 2: The `hornvale-gate-runner` IAM identity + policy

**Files:**
- Create: `scripts/aws-gate/policy/runner-policy.json`
- Create: `scripts/aws-gate/setup-iam.sh` (sourced by `setup.sh`; kept separate so it is testable and reviewable alone)
- Create: `scripts/aws-gate/test/test_policy.sh`

**Interfaces:**
- Consumes: `lib.sh`.
- Produces: function `setup_iam` (idempotent create-if-absent of the user, policy, access key → local profile); the policy JSON.

- [ ] **Step 1: Write the failing test** (the policy must actually box the runner in)

`scripts/aws-gate/test/test_policy.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
p="$here/../policy/runner-policy.json"
python3 - "$p" <<'PY'
import json,sys
d=json.load(open(sys.argv[1]))
txt=json.dumps(d)
run=[s for s in d["Statement"] if "ec2:RunInstances" in (s.get("Action") if isinstance(s.get("Action"),list) else [s.get("Action")])]
assert run, "must grant RunInstances"
cond=json.dumps(run[0].get("Condition",{}))
assert "c7g.4xlarge" in cond, "RunInstances must be conditioned on the instance type"
assert "spot" in cond.lower(), "RunInstances must require spot"
assert "us-east-1" in txt, "must pin the region"
assert "project" in txt and "hornvale-gate" in txt, "must require the project tag"
assert '"iam:*"' not in txt and '"*"' not in [s.get("Action") for s in d["Statement"]], "no wildcard admin"
print("policy OK")
PY
```

- [ ] **Step 2: Run it, verify it fails**

Run: `bash scripts/aws-gate/test/test_policy.sh`
Expected: FAIL — policy file missing.

- [ ] **Step 3: Write the policy + `setup-iam.sh`**

`scripts/aws-gate/policy/runner-policy.json` (condition-scoped; verify exact condition keys live in Task 8):
```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "LaunchOnlyBoxedSpot",
      "Effect": "Allow",
      "Action": "ec2:RunInstances",
      "Resource": "*",
      "Condition": {
        "StringEquals": { "ec2:InstanceType": "c7g.4xlarge", "aws:RequestedRegion": "us-east-1", "ec2:InstanceMarketType": "spot" },
        "StringLike": { "aws:RequestTag/project": "hornvale-gate" }
      }
    },
    {
      "Sid": "TerminateOwnTaggedBoxes",
      "Effect": "Allow",
      "Action": "ec2:TerminateInstances",
      "Resource": "*",
      "Condition": { "StringEquals": { "aws:ResourceTag/project": "hornvale-gate", "aws:RequestedRegion": "us-east-1" } }
    },
    {
      "Sid": "DescribeAndCache",
      "Effect": "Allow",
      "Action": ["ec2:DescribeInstances", "ec2:DescribeSpotInstanceRequests", "ec2:DescribeLaunchTemplates"],
      "Resource": "*",
      "Condition": { "StringEquals": { "aws:RequestedRegion": "us-east-1" } }
    },
    {
      "Sid": "PassOnlyTheBoxRole",
      "Effect": "Allow",
      "Action": "iam:PassRole",
      "Resource": "arn:aws:iam::*:role/hornvale-gate-runner-box",
      "Condition": { "StringEquals": { "iam:PassedToService": "ec2.amazonaws.com" } }
    },
    {
      "Sid": "SccacheBucketOnly",
      "Effect": "Allow",
      "Action": ["s3:GetObject", "s3:PutObject", "s3:ListBucket"],
      "Resource": ["arn:aws:s3:::hornvale-gate-sccache-*", "arn:aws:s3:::hornvale-gate-sccache-*/*"]
    }
  ]
}
```

`scripts/aws-gate/setup-iam.sh`:
```bash
#!/usr/bin/env bash
# Idempotent: create the runner user, attach the boxed policy, ensure an access
# key exists and is written to the local `hornvale-gate` profile. Admin creds.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

setup_iam() {
  local user="hornvale-gate-runner"
  aws_admin iam get-user --user-name "$user" >/dev/null 2>&1 || aws_admin iam create-user --user-name "$user" --tags "Key=project,Value=hornvale-gate"
  aws_admin iam put-user-policy --user-name "$user" --policy-name hornvale-gate-runner --policy-document "file://$here/policy/runner-policy.json"
  # Create a key only if the profile has none configured yet.
  if ! aws --profile "$HVG_PROFILE" sts get-caller-identity >/dev/null 2>&1; then
    local out; out="$(aws_admin iam create-access-key --user-name "$user" --output json)"
    local id secret; id="$(echo "$out" | python3 -c 'import json,sys;print(json.load(sys.stdin)["AccessKey"]["AccessKeyId"])')"
    secret="$(echo "$out" | python3 -c 'import json,sys;print(json.load(sys.stdin)["AccessKey"]["SecretAccessKey"])')"
    aws configure set aws_access_key_id "$id" --profile "$HVG_PROFILE"
    aws configure set aws_secret_access_key "$secret" --profile "$HVG_PROFILE"
    aws configure set region "$HVG_REGION" --profile "$HVG_PROFILE"
    chmod 600 "$HOME/.aws/credentials" 2>/dev/null || true
    manifest_set runner_key_id "$id"
  fi
  manifest_set runner_user "$user"
}
```

- [ ] **Step 4: Run it, verify it passes** — `bash scripts/aws-gate/test/test_policy.sh` → `policy OK`; `shellcheck` the new scripts → clean. (No live AWS call yet.)

- [ ] **Step 5: Commit**

```bash
git add scripts/aws-gate/policy scripts/aws-gate/setup-iam.sh scripts/aws-gate/test/test_policy.sh
git commit -m "feat(aws-gate): boxed-in runner IAM identity + policy (condition-scoped) + policy test"
```

---

### Task 3: `setup.sh` — network, keypair, launch template, sccache bucket

**Files:**
- Create: `scripts/aws-gate/setup.sh`
- Create: `scripts/aws-gate/userdata.sh` (box bootstrap: rustup + idle timer + sccache)

**Interfaces:**
- Consumes: `lib.sh`, `setup-iam.sh`.
- Produces: `~/.hornvale-gate/manifest.json` keys `sccache_bucket`, `security_group`, `keypair`, `launch_template`, `subnet`, `az`; the launch template referencing `userdata.sh` (base64).

- [ ] **Step 1: Write `userdata.sh`** (runs on the box at first boot)

```bash
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
```
> The heartbeat file `/run/hvg-heartbeat` is `touch`ed by `gate-remote.sh` at the start of each run (over SSH). Exact sccache install lines and the AMI's cron availability are verified live in Task 8.

- [ ] **Step 2: Write `setup.sh`** (idempotent create-if-absent; supports `--dry-run`)

```bash
#!/usr/bin/env bash
# scripts/aws-gate/setup.sh — provision the remote-gate infrastructure.
# Idempotent (create-if-absent, keyed by name/tag). BILLABLE on first real run.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"
# shellcheck source=/dev/null
source "$here/setup-iam.sh"
DRY_RUN=0; [ "${1:-}" = "--dry-run" ] && DRY_RUN=1
run() { if [ "$DRY_RUN" = 1 ]; then echo "DRY: $*"; else "$@"; fi; }

section() { printf '\n== %s\n' "$1"; }

section "IAM runner identity";        [ "$DRY_RUN" = 1 ] && echo "DRY: setup_iam" || setup_iam
section "sccache S3 bucket"
bucket="hornvale-gate-sccache-$(aws_admin sts get-caller-identity --query Account --output text)"
aws_admin s3api head-bucket --bucket "$bucket" 2>/dev/null || run aws_admin s3api create-bucket --bucket "$bucket" --region "$HVG_REGION"
run aws_admin s3api put-bucket-tagging --bucket "$bucket" --tagging 'TagSet=[{Key=project,Value=hornvale-gate}]'
manifest_set sccache_bucket "$bucket"
section "AMI (Ubuntu 24.04 ARM64 via SSM public parameter)"
ami="$(aws_admin ssm get-parameter --name /aws/service/canonical/ubuntu/server/24.04/stable/current/arm64/hvm/ebs-gp3/ami-id --query Parameter.Value --output text)"
manifest_set ami "$ami"
section "Security group (SSH from this IP only)"
myip="$(curl -s https://checkip.amazonaws.com)"
# ... create SG if absent, authorize tcp/22 from ${myip}/32, tag project ...
section "Keypair"
# ... create keypair if absent, write private key to ~/.hornvale-gate/id (600), tag ...
section "Box instance profile (hornvale-gate-runner-box)"
# The BOX's own role — distinct from the runner user and the Lambda role. The
# runner's scoped iam:PassRole (Task 2) targets exactly this role name, so it
# MUST exist or every launch is denied. Create if absent:
#   - role `hornvale-gate-runner-box`, trust policy = ec2.amazonaws.com;
#   - inline policy: ec2:TerminateInstances on instances tagged project=hornvale-gate
#     (the 15-min idle self-terminate), + s3 Get/Put/List on the sccache bucket
#     (RUSTC_WRAPPER=sccache);
#   - instance profile `hornvale-gate-runner-box`, role added; tag project.
section "Launch template"
# ... register/refresh launch template: spot, $HVG_INSTANCE_TYPE, $ami, SG, keypair,
#     IamInstanceProfile=hornvale-gate-runner-box, base64(userdata.sh with bucket templated),
#     TagSpecifications project=hornvale-gate ...
echo "setup complete (dry-run=$DRY_RUN)"
```
> The `...` blocks are filled with concrete `aws` calls during Task 8's live iteration; each is a `create-if-absent` guarded by a `describe`. Keep every created resource tagged `project=hornvale-gate`. The box instance-profile role is the target of the runner's scoped `iam:PassRole` — creating it here closes the gap the Task 2 review surfaced.

- [ ] **Step 3: Verify structure without spending** — `bash scripts/aws-gate/setup.sh --dry-run` prints the intended actions (admin `sts`/`ssm`/`checkip` reads are free and fine); `shellcheck scripts/aws-gate/setup.sh scripts/aws-gate/userdata.sh` clean.

- [ ] **Step 4: Commit**

```bash
git add scripts/aws-gate/setup.sh scripts/aws-gate/userdata.sh
git commit -m "feat(aws-gate): setup.sh — sccache bucket, AMI, SG, keypair, launch template (dry-run + idempotent)"
```

---

### Task 4: The circuit breaker (Lambda) + its pure decision function

**Files:**
- Create: `scripts/aws-gate/circuit_breaker.py`
- Create: `scripts/aws-gate/test/test_circuit_breaker.py`
- Modify: `scripts/aws-gate/setup.sh` (deploy the Lambda + role + 5-min EventBridge rule; create the $10 alert and $25 action budgets)

**Interfaces:**
- Produces: `instances_to_kill(instances, now_ts, max_age_secs, max_count) -> (ids, breach)` — pure; and `handler(event, context)` that describes tagged instances, applies it, and disables the runner on breach.

- [ ] **Step 1: Write the failing test** (pure decision logic — no boto3)

`scripts/aws-gate/test/test_circuit_breaker.py`:
```python
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from circuit_breaker import instances_to_kill

def inst(id, age): return {"InstanceId": id, "age": age}

def test_kills_over_age():
    ids, breach = instances_to_kill([inst("a", 8000), inst("b", 10)], now_ts=0, max_age_secs=7200, max_count=10)
    assert ids == ["a"] and not breach

def test_count_breach_kills_all_and_flags():
    boxes = [inst(str(i), 10) for i in range(12)]
    ids, breach = instances_to_kill(boxes, now_ts=0, max_age_secs=7200, max_count=10)
    assert breach and set(ids) == {str(i) for i in range(12)}

if __name__ == "__main__":
    test_kills_over_age(); test_count_breach_kills_all_and_flags(); print("circuit_breaker: OK")
```

- [ ] **Step 2: Run it, verify it fails** — `python3 scripts/aws-gate/test/test_circuit_breaker.py` → ImportError/AssertionError.

- [ ] **Step 3: Write `circuit_breaker.py`**

```python
"""Circuit breaker for the remote gate. Runs on a 5-min schedule.

Terminates any hornvale-gate box older than MAX_AGE or, if more than MAX_COUNT
run at once, terminates all and disables the runner identity. Stdlib + boto3
(present in the Lambda runtime) only.
"""
import os, time

def instances_to_kill(instances, now_ts, max_age_secs, max_count):
    breach = len(instances) > max_count
    if breach:
        return [i["InstanceId"] for i in instances], True
    return [i["InstanceId"] for i in instances if i["age"] >= max_age_secs], False

def handler(event, context):
    import boto3
    region = os.environ.get("HVG_REGION", "us-east-1")
    max_age = int(os.environ.get("HVG_MAX_AGE_SECS", "7200"))
    max_count = int(os.environ.get("HVG_MAX_COUNT", "10"))
    ec2 = boto3.client("ec2", region_name=region)
    now = int(time.time())
    resv = ec2.describe_instances(
        Filters=[{"Name": "tag:project", "Values": ["hornvale-gate"]},
                 {"Name": "instance-state-name", "Values": ["pending", "running"]}])
    boxes = [{"InstanceId": i["InstanceId"], "age": now - int(i["LaunchTime"].timestamp())}
             for r in resv["Reservations"] for i in r["Instances"]]
    ids, breach = instances_to_kill(boxes, now, max_age, max_count)
    if ids:
        ec2.terminate_instances(InstanceIds=ids)
    if breach:
        iam = boto3.client("iam")
        for k in iam.list_access_keys(UserName="hornvale-gate-runner")["AccessKeyMetadata"]:
            iam.update_access_key(UserName="hornvale-gate-runner", AccessKeyId=k["AccessKeyId"], Status="Inactive")
    return {"terminated": ids, "breach": breach}
```

- [ ] **Step 4: Run it, verify it passes** — `python3 scripts/aws-gate/test/test_circuit_breaker.py` → `circuit_breaker: OK`.

- [ ] **Step 5: Add deploy + budgets to `setup.sh`** (dry-run-guarded, tagged) — a `section "Circuit breaker"` that zips `circuit_breaker.py`, creates/updates the Lambda + execution role (terminate tagged instances + iam:UpdateAccessKey on the runner + logs) + a 5-min EventBridge schedule with invoke permission; and a `section "Budgets"` that creates a $10 NOTIFY budget (email) and a $25 budget with an IAM-deny **budget action** targeting the runner. Store arns in the manifest. Exact `aws lambda`/`aws budgets` calls verified live in Task 8.

- [ ] **Step 6: Commit**

```bash
git add scripts/aws-gate/circuit_breaker.py scripts/aws-gate/test/test_circuit_breaker.py scripts/aws-gate/setup.sh
git commit -m "feat(aws-gate): circuit-breaker Lambda (pure decision fn + handler) + budgets in setup"
```

---

### Task 5: `gate-remote.sh` — the wrapper

**Files:**
- Create: `scripts/aws-gate/gate-remote.sh`
- Create: `scripts/aws-gate/test/test_reuse.sh`

**Interfaces:**
- Consumes: `lib.sh`, the manifest.
- Produces: function `find_or_launch` (reuse a running tagged box for this worktree, else launch one) returning the instance id; the end-to-end run.

- [ ] **Step 1: Write the failing test** (reuse-or-launch decision, stubbed AWS)

`scripts/aws-gate/test/test_reuse.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export HVG_STUB_LOG; HVG_STUB_LOG="$(mktemp)"
# shellcheck source=/dev/null
source "$here/stub.sh"
# A running box for this worktree already exists -> reuse, no run-instances.
stub_response() { case "$*" in *"describe-instances"*) echo "i-existing";; esac; }
# shellcheck source=/dev/null
source "$here/../lib.sh"; # shellcheck source=/dev/null
source "$here/../gate-remote.sh"  # must define find_or_launch without executing main
id="$(find_or_launch)"
[ "$id" = "i-existing" ] || { echo "FAIL: should reuse running box"; exit 1; }
grep -q "run-instances" "$HVG_STUB_LOG" && { echo "FAIL: must not launch when one exists"; exit 1; }
echo "test_reuse: OK"
```
> `gate-remote.sh` must guard its `main` behind `[ "${BASH_SOURCE[0]}" = "$0" ]` so sourcing it in tests defines functions without running.

- [ ] **Step 2: Run it, verify it fails** — `bash scripts/aws-gate/test/test_reuse.sh` → fails (function undefined).

- [ ] **Step 3: Write `gate-remote.sh`**

```bash
#!/usr/bin/env bash
# scripts/aws-gate/gate-remote.sh — run the CI Rust gate on this worktree's
# remote spot box. Reuses a running box, else launches one; rsyncs the worktree,
# runs the gate over SSH streaming output, syncs regenerated artifacts back.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
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
  # shellcheck disable=SC2086
  local rc=0
  $ssh 'cd work && make gate && scripts/regenerate-artifacts.sh && git -c core.fileMode=false diff --exit-code -- book/' || rc=$?
  # Sync any regenerated artifacts back so legitimate changes can be committed locally.
  rsync -az -e "ssh -i $HOME/.hornvale-gate/id -o StrictHostKeyChecking=accept-new" "ubuntu@$ip:work/book/" "$(git rev-parse --show-toplevel)/book/"
  [ $rc -eq 0 ] && echo "gate-remote: PASS" || echo "gate-remote: FAIL (rc=$rc)"
  return $rc
}

[ "${BASH_SOURCE[0]}" = "$0" ] && main "$@"
```
> The exact remote gate command (whether `make gate` + the drift check, or a dedicated remote target) is finalized against the real box in Task 8; the artifact-drift half is knowingly divergent until `libm` lands (Task 8 / verify).

- [ ] **Step 4: Run it, verify it passes** — `bash scripts/aws-gate/test/test_reuse.sh` → `test_reuse: OK`; `shellcheck` clean.

- [ ] **Step 5: Commit**

```bash
git add scripts/aws-gate/gate-remote.sh scripts/aws-gate/test/test_reuse.sh
git commit -m "feat(aws-gate): gate-remote.sh wrapper (reuse-or-launch + rsync + remote gate + sync-back)"
```

---

### Task 6: `teardown.sh` + `panic.sh` (tag-enumerated) + Makefile + shellcheck target

**Files:**
- Create: `scripts/aws-gate/teardown.sh`, `scripts/aws-gate/panic.sh`
- Create: `scripts/aws-gate/test/test_panic.sh`
- Modify: `Makefile` (targets `gate-remote`, `gate-panic`, `gate-remote-setup`, `gate-remote-teardown`, `shellcheck`)

**Interfaces:**
- Consumes: `lib.sh`, `tagged_arns`.
- Produces: `panic.sh` (deactivate runner key → terminate tagged boxes → cancel spot requests → delete launch template), `teardown.sh` (full tag-enumerated removal).

- [ ] **Step 1: Write the failing test** (panic deactivates the key FIRST, then terminates)

`scripts/aws-gate/test/test_panic.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export HVG_STUB_LOG; HVG_STUB_LOG="$(mktemp)"
# shellcheck source=/dev/null
source "$here/stub.sh"
stub_response() { case "$*" in *"list-access-keys"*) echo '{"AccessKeyMetadata":[{"AccessKeyId":"AKIA"}]}';;
                                *"describe-instances"*) echo "i-1 i-2";; esac; }
# shellcheck source=/dev/null
source "$here/../lib.sh"; # shellcheck source=/dev/null
source "$here/../panic.sh"
panic_all
# The access key must be deactivated BEFORE any terminate call.
key_line=$(grep -n "update-access-key.*Inactive" "$HVG_STUB_LOG" | head -1 | cut -d: -f1)
term_line=$(grep -n "terminate-instances" "$HVG_STUB_LOG" | head -1 | cut -d: -f1)
[ -n "$key_line" ] && [ "$key_line" -lt "${term_line:-99999}" ] || { echo "FAIL: deactivate key before terminate"; exit 1; }
echo "test_panic: OK"
```

- [ ] **Step 2: Run it, verify it fails** — function undefined.

- [ ] **Step 3: Write `panic.sh` and `teardown.sh`**

`scripts/aws-gate/panic.sh` (the tier-0 switch — direct API, no dependence on manifest completeness):
```bash
#!/usr/bin/env bash
# scripts/aws-gate/panic.sh — EMERGENCY STOP. Deactivate the runner identity
# (nothing can launch), then terminate every tagged box, cancel spot requests,
# and delete the launch template. Admin creds. Safe to run repeatedly.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
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
  [ -n "$ids" ] && aws_admin ec2 terminate-instances --instance-ids $ids || true   # shellcheck disable=SC2086
  echo "PANIC: cancelling spot requests + deleting launch template"
  # ... cancel open spot requests tagged project; delete the launch template by name ...
  echo "PANIC: complete — runner disabled. Re-enable with scripts/aws-gate/setup.sh"
}

[ "${BASH_SOURCE[0]}" = "$0" ] && panic_all "$@"
```

`scripts/aws-gate/teardown.sh` (graceful full removal, tag-enumerated):
```bash
#!/usr/bin/env bash
# scripts/aws-gate/teardown.sh — remove ALL hornvale-gate resources. Enumerates
# by the project tag (complete by construction), plus well-known-name backstops
# for IAM/budgets the tagging API covers unevenly. Admin creds.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"
"$here/panic.sh"                                  # stop the bleeding first
echo "teardown: tagged resources:"; tagged_arns
# ... for each ARN, dispatch to the right delete by service; then delete the
#     runner user/keys/policy, the two budgets, the Lambda + role + rule, the
#     SG, the keypair, and the sccache bucket by their known names ...
echo "teardown: complete"
```

- [ ] **Step 4: Run it, verify it passes** — `bash scripts/aws-gate/test/test_panic.sh` → `test_panic: OK`; `shellcheck scripts/aws-gate/*.sh` clean.

- [ ] **Step 5: Wire the Makefile**

```makefile
gate-remote: ## Run the CI gate on this worktree's AWS spot box
	@scripts/aws-gate/gate-remote.sh
gate-panic: ## EMERGENCY: disable the runner and kill all gate resources
	@scripts/aws-gate/panic.sh
gate-remote-setup: ## Provision remote-gate infra (BILLABLE; confirmation-gated)
	@scripts/aws-gate/setup.sh
gate-remote-teardown: ## Remove all remote-gate infra
	@scripts/aws-gate/teardown.sh
shellcheck: ## Lint all shell scripts
	@shellcheck scripts/*.sh scripts/aws-gate/*.sh scripts/aws-gate/test/*.sh scripts/hooks/*
```

- [ ] **Step 6: Commit**

```bash
git add scripts/aws-gate/teardown.sh scripts/aws-gate/panic.sh scripts/aws-gate/test/test_panic.sh Makefile
git commit -m "feat(aws-gate): tag-enumerated teardown + tier-0 panic (key-first) + Makefile targets"
```

---

### Task 7: `gate-remote-verify` + docs

**Files:**
- Create: `scripts/aws-gate/gate-remote-verify.sh`
- Create: `scripts/aws-gate/README.md`
- Modify: `Makefile` (`gate-remote-verify` target)

- [ ] **Step 1: Write `gate-remote-verify.sh`** — regenerate artifacts locally and remotely on the current `HEAD`, `diff` the two artifact trees, and report:
  - identical → `VERIFIED: remote box is a faithful oracle`;
  - differ AND `libm` not yet in the dep tree (`grep -q '^libm' Cargo.lock` is false) → `PENDING: divergence expected until the libm port lands (not a failure)` exit 0;
  - differ AND `libm` present → `FAIL: unexpected divergence` exit 1.

```bash
#!/usr/bin/env bash
# Compare local vs remote regenerated artifacts on HEAD. Until the libm port
# lands, divergence is EXPECTED (Linux libm) and reported as PENDING, not FAIL.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"
# ... run scripts/regenerate-artifacts.sh locally -> /tmp/local-book ...
# ... run the same on the remote box, rsync its book/ -> /tmp/remote-book ...
if diff -r /tmp/local-book /tmp/remote-book >/dev/null; then
  echo "VERIFIED: remote box is a faithful oracle"; exit 0
elif grep -q '^name = "libm"' Cargo.lock 2>/dev/null; then
  echo "FAIL: unexpected artifact divergence with libm present"; exit 1
else
  echo "PENDING: artifact divergence expected until the libm port lands (not a failure)"; exit 0
fi
```

- [ ] **Step 2: Write `README.md`** — one page: what it is, `make gate-remote-setup` (billable), daily use (`make gate-remote`), the kill switches (`make gate-panic`, the circuit breaker, budgets), the cost model and worst-case bound, the libm go-live note, and `make gate-remote-teardown`.

- [ ] **Step 3: shellcheck + commit**

```bash
shellcheck scripts/aws-gate/gate-remote-verify.sh
git add scripts/aws-gate/gate-remote-verify.sh scripts/aws-gate/README.md Makefile
git commit -m "feat(aws-gate): gate-remote-verify (libm-aware) + README"
```

---

### Task 8: Live bring-up (BILLABLE — human-in-the-loop acceptance)

**This task spends money and MUST be run interactively with the developer. Do not automate it or run any step without explicit go-ahead.** It is where the `...` blocks and every AWS-CLI specific are finalized against the real API.

- [ ] **Step 1: Confirm go-ahead.** Ask the developer to confirm before the first billable call. Confirm the account (`aws sts get-caller-identity` → `665449637458`).
- [ ] **Step 2: `make gate-remote-setup`.** Fill in each `...` block live until setup completes cleanly; re-run to prove idempotency (second run creates nothing). Verify the runner profile works: `aws --profile hornvale-gate sts get-caller-identity`.
- [ ] **Step 3: First `make gate-remote`.** From a worktree; confirm a spot box launches, the gate runs and streams, and the exit code mirrors. Time a warm second run.
- [ ] **Step 4: Exercise every kill switch.** Confirm the idle timer terminates a box after 15 min; invoke the circuit-breaker Lambda manually with a synthetic over-age/over-count input and confirm it terminates + (on breach) disables the runner, then re-enable via setup; run `make gate-panic` and confirm the runner key goes Inactive and boxes die; confirm the $10 budget alert email arrives and the $25 action is wired.
- [ ] **Step 5: `gate-remote-verify`.** Expect `PENDING` today (pre-libm). Record it; this is the go-live tripwire for when the port lands.
- [ ] **Step 6: `make gate-remote-teardown`.** Confirm `tagged_arns` returns empty afterward and the AWS console shows no residual gate resources. Re-run `setup` to leave the tool ready for daily use (or leave torn down — developer's call).
- [ ] **Step 7: Commit** any live-hardened script changes: `git commit -am "fix(aws-gate): finalize AWS CLI specifics against the live account"`.

---

## Notes for the executor

- **Money is real in Task 8 only.** Tasks 1–7 are pure authoring: shellcheck + the stubbed/pure tests are the whole gate, and none of them call AWS for real (the `--dry-run`/read-only `sts`/`ssm` calls in Task 3 are free). Never run `setup.sh` (non-dry) or `gate-remote.sh` outside Task 8's confirmation.
- **The panic path is sacred:** `panic.sh` must never `source` anything that could fail to load (keep it dependent only on `lib.sh` + direct API), and it must deactivate the key *before* terminating. The ordering test in Task 6 guards this.
- **Everything carries `project=hornvale-gate`.** If a resource can't be tagged at create time, tag it immediately after — teardown/panic completeness depends on it.
- **libm is the go-live gate, not a build blocker.** Ship Tasks 1–8; `gate-remote-verify` flips from PENDING to VERIFIED the day the port merges, with no code change here.
