# Remote Gate on AWS — design

**Date:** 2026-07-12
**Status:** Approved (brainstorming session)
**Scope:** A `make gate-remote` wrapper that runs the authoritative CI Rust gate on an
ephemeral AWS spot instance instead of the developer's Mac, so parallel campaign sessions
stop contending for local cores. Includes least-privilege isolation, a manual panic
switch, an automated circuit breaker, an idle self-terminate, and a hard budget action.
Dev tooling only — it does not touch the simulation or its determinism contracts.

---

## 1. Motivation

Parallel campaign sessions each run `make gate` locally, and on a 10-core Mac that drives
the load average into the 90s — work becomes effectively single-threaded. The gate is
CPU-heavy but embarrassingly offloadable: it is a pure function of the worktree contents.
Moving it to per-session cloud compute removes the contention entirely at ~$1–2.5/day.

**Prerequisite for trust, not for building.** The gate's artifact-drift check requires
byte-identical output across machines. Today Linux libm ≠ Apple libm, so a Linux box
produces different bytes and the drift check fails. A separate, in-progress `libm`-crate
port makes transcendentals platform-independent; once it lands, an AWS Linux run is
byte-identical to the Mac. This tool is **built now** (platform-agnostic plumbing) and
**trusted for determinism-sensitive artifacts** the day the port merges, gated by the
verification in §8. Until then it is usable for the compile/test/lint majority of the gate
and knowingly divergent on the artifact check.

## 2. Non-goals

- Not GitHub Actions CI (that stays as-is for push/PR; this is the local-iteration loop).
- Not the two Deno client jobs (Atlas/Orrery) — lightweight, not the contention source.
- Not a persistent shared build server (that would relocate the contention to one box).
- Not Terraform/CDK — plain AWS CLI, consistent with the repo's no-new-tools ethos.

## 3. Topology & lifecycle

- **One spot instance per worktree**, `c7g.4xlarge` (16-vCPU Graviton3), us-east-1,
  tagged `hornvale-gate` + `worktree=<name>`. Base image: latest Canonical Ubuntu 24.04
  LTS ARM64.
- `make gate-remote` from a worktree: if that worktree's tagged box is `running`, reuse
  it; else launch from the launch template. Reuse keeps `target/` warm on the box's root
  EBS, so runs within a burst are incremental (~30–60 s).
- **Idle self-terminate: 15 min.** A systemd timer on the box terminates the instance
  after 15 min with no gate activity; each run refreshes a heartbeat file. On spot, "stop"
  is not available, so idle → terminate.
- **Cold-start speed via `sccache` → S3.** Every box reads/writes one shared S3
  compilation cache, so a fresh box (first run after a reap) rarely pays a full cold
  build. (A persistent per-worktree EBS cache volume, for true cross-reap incremental, is
  a documented future optimization if cold starts still bite.)

## 4. Sync & execution

- The wrapper `rsync`s the worktree to the box over SSH, excluding `target/` and honoring
  `.gitignore`, so it gates **uncommitted** changes exactly like local `make gate`.
- It runs the **authoritative CI Rust gate** (mirroring `.github/workflows/ci.yml`'s Rust
  job) over SSH, streaming stdout live:
  1. `cargo test --workspace`
  2. `cargo fmt --check`
  3. `cargo clippy --workspace --all-targets -- -D warnings`
  4. The "Artifacts are current" regenerate-and-`git diff` determinism check.
- On completion it prints `PASS`/`FAIL`, mirrors the gate's exit code as its own, and
  `rsync`s any regenerated artifacts **back** to the worktree so legitimately-changed
  artifacts can be reviewed and committed locally.
- The Mac only orchestrates and displays; no gate CPU runs locally.

## 5. Isolation — the throwaway runner identity

The wrapper authenticates as a dedicated least-privilege IAM user **`hornvale-gate-runner`**
(its own access key, used via an `hornvale-gate` AWS profile), never the developer's admin
credentials. Its policy is boxed with IAM conditions:

- `ec2:RunInstances` only for `InstanceType = c7g.4xlarge`, spot only, in `us-east-1`,
  requiring the `hornvale-gate` tag; `ec2:TerminateInstances`/`StopInstances` scoped to
  that tag; `s3:*Object` scoped to the sccache bucket; nothing else.

Two payoffs: a buggy wrapper **cannot** launch a large instance or a fleet outside these
bounds, and every kill switch (§6) works by disabling this one identity — instant and
total.

## 6. Kill switches — defense in depth by latency tier

AWS has no instantaneous hard spend cap (billing data lags hours), so protection is
layered by reaction time. All automated tiers are asleep-safe.

| Tier | Mechanism | Reaction | Automated |
|---|---|---|---|
| 0 · manual panic | `make gate-panic` (`scripts/aws-gate/panic.sh`): terminate all tagged boxes, cancel spot requests, delete the launch template, **deactivate the runner's access key** → every future launch fails `AccessDenied` | instant | you pull it |
| 1 · circuit breaker | Lambda on a 5-min EventBridge tick: terminate any gate box older than `MAX_AGE` or beyond `MAX_COUNT`; if the count cap is breached, auto-disable the runner | ~5 min | yes |
| 2 · idle timer | 15-min per-box self-terminate (§3) | ~15 min | yes |
| 3 · hard budget action | AWS Budgets **action** at `$25/day`: attach an explicit deny-all to the runner. `$10/day` is a separate email alert. | hours (billing lag) | yes (backstop) |

**Defaults:** `MAX_COUNT = 10`, `MAX_AGE = 2h`, hard budget `$25/day`, alert `$10/day` —
all adjustable. Tier 0 is the "shitting-blood" switch; tier 1 is the asleep protection
that the billing-lagged budget cannot provide.

**Worst-case bound.** With these caps, a total runaway is at most ~10 boxes for ≤2h each
before tier 1 reaps them; a relaunch-loop that slips the count cap is caught by the $25/day
budget action. So even if every fast guard failed, the ceiling is **~$25 before the runner
is disabled** — and realistically tier 1 stops it within ~5 minutes.

**Cost of the safety net.** The circuit-breaker Lambda runs ~288×/day, each a sub-second
`describe`+maybe-`terminate` — within the Lambda free tier (~$0/mo). EventBridge and
Budgets are free.

## 7. Provisioning — a tag-scoped reconciler in shell

Plain AWS CLI, no CloudFormation/Terraform/Ansible. The pattern is a **tag reconciler**:
every resource is created carrying `project=hornvale-gate`, and destruction enumerates by
that tag rather than by a hand-maintained list — so teardown is complete **by
construction**, including anything a future `setup.sh` adds and a hand-written teardown
would have forgotten. This is the safety property full IaC would provide, without the
tooling or the stack-operation latency.

- **`scripts/aws-gate/setup.sh`** — idempotent create-if-absent (keyed by tag/name), so it
  is safe to re-run and doubles as the converge/update path. It creates: the sccache S3
  bucket; the `hornvale-gate-runner` IAM user + condition-scoped policy + access key
  (written to the local `hornvale-gate` profile, mode 600); the circuit-breaker Lambda +
  execution role + 5-min EventBridge rule; the security group (SSH from the caller's
  current public IP only); an SSH keypair; the launch template (spot, Ubuntu 24.04 ARM64,
  instance profile, userdata that installs `rustup` — auto-selecting pinned 1.96.1 from
  `rust-toolchain.toml` — plus the idle timer and sccache); and the `$10` alert + `$25`
  action budgets. It writes a **resource manifest** (`~/.hornvale-gate/manifest.json`:
  launch-template id, sg id, bucket, subnet/AZ, runner profile, Lambda arn) that the
  wrapper reads instead of rediscovering by tag on every run.
- **`scripts/aws-gate/teardown.sh`** — enumerates every `project=hornvale-gate` resource
  via the Resource Groups Tagging API and deletes it (IAM/budgets, which the tagging API
  covers unevenly, are removed by their well-known names as a backstop). Prints each
  resource destroyed.
- Both scripts print every resource they touch; both are idempotent.

**Confirmation gate:** building the scripts is free; the first `setup.sh` run and the first
instance launch create billable resources. Neither runs without explicit developer
go-ahead.

## 8. The determinism go-live gate

`make gate-remote-verify` runs the full gate locally and remotely on the same commit and
asserts the **regenerated artifacts are byte-identical**. This is the acceptance test that
the remote box is a faithful oracle for the determinism-sensitive artifact check.

Until the `libm` port lands it is expected to **differ** (Linux libm) — the command reports
this as a known-pending state, not a hard failure, and documents it. The day the port
merges, a green `gate-remote-verify` flips the tool to fully trusted. The compile/test/lint
tiers of the gate are unaffected by libm and are trustworthy immediately.

## 9. Repo layout

```
scripts/aws-gate/
  setup.sh            # one-time provisioning (billable; confirmation-gated)
  teardown.sh         # remove everything setup.sh created
  gate-remote.sh      # the wrapper: launch/reuse, rsync, run gate, stream, sync back
  panic.sh            # tier-0 kill switch
  circuit-breaker.py  # tier-1 Lambda handler (source; deployed by setup.sh)
  lib.sh              # shared: profile, tags, region, instance discovery
Makefile targets:
  gate-remote         # run the gate for the current worktree on AWS
  gate-remote-verify  # local-vs-remote byte-identity acceptance test
  gate-panic          # -> scripts/aws-gate/panic.sh
```

All shell is `shellcheck`-clean (repo standard). The Lambda is Python (AWS's zero-dep
runtime); it introduces no Rust workspace dependency.

## 10. Sequencing

Built now, in parallel with the `libm` port; only §8's go-live verification depends on it.
Rough stages: (1) provisioning + teardown + the runner identity; (2) the wrapper
(launch/reuse/rsync/run/stream/sync-back); (3) the safety net (panic, circuit breaker,
budgets); (4) the verification command + docs. Each stage is independently testable
against the developer's real account (with the confirmation gate on the first billable
step).
