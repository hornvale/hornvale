# Remote gate on AWS

Runs the authoritative CI Rust gate (`cargo test --workspace`, `cargo fmt --check`,
`cargo clippy --workspace --all-targets -- -D warnings`, and the "Artifacts are current"
regenerate-and-diff check) on an ephemeral AWS spot instance instead of the developer's
Mac, so parallel campaign sessions stop contending for local cores. Dev tooling only — it
does not touch the simulation or its determinism contracts. Full design:
`docs/superpowers/specs/2026-07-12-remote-gate-aws-design.md`.

One spot instance per worktree (`c7g.4xlarge`, 16-vCPU Graviton3, us-east-1), tagged
`project=hornvale-gate` + `worktree=<name>`. The wrapper authenticates as a dedicated
least-privilege IAM user (`hornvale-gate-runner`), never the developer's admin
credentials — its policy can only launch that one instance type, spot only, tagged, in
that one region.

## One-time setup (BILLABLE)

```
make gate-remote-setup
```

Provisions everything: the sccache S3 bucket, the runner IAM identity + boxed policy +
access key, the security group (SSH from your current IP only), the SSH keypair, the
launch template, the circuit-breaker Lambda + EventBridge rule, and the `$10`/`$25`
budgets (below). Idempotent — safe to re-run; it converges rather than duplicating.
Nothing before this command spends anything; this command starts the meter (Lambda,
EventBridge, and Budgets are themselves free — see "Cost model").

## Daily use

```
make gate-remote
```

Reuses this worktree's running box if there is one, else launches from the launch
template. `rsync`s the worktree over SSH (excluding `target/`, honoring `.gitignore`, so
it gates uncommitted changes exactly like local `make gate`), runs the gate, streams
output live, prints `PASS`/`FAIL` mirroring the gate's exit code, and syncs any
regenerated artifacts back so legitimate changes can be reviewed and committed locally.
Reusing a box keeps `target/` warm, so runs within a burst are incremental (~30-60s).

## Kill switches — defense in depth by latency tier

AWS has no instantaneous hard spend cap (billing data lags hours), so protection is
layered by reaction time. All automated tiers are asleep-safe.

| Tier | Mechanism | Reaction |
|---|---|---|
| 0 · manual panic | `make gate-panic`: terminate every tagged box, cancel spot requests, delete the launch template, deactivate the runner's access key — every future launch fails `AccessDenied` | instant |
| 1 · circuit breaker | Lambda on a 5-min EventBridge tick: terminate any gate box older than `MAX_AGE` (2h) or, if more than `MAX_COUNT` (10) boxes are running at once, terminate all of them and auto-disable the runner | ~5 min |
| 2 · idle timer | 15-min per-box self-terminate — a heartbeat file is refreshed on each gate run; a box with no activity for 15 min terminates itself (spot instances can't "stop", only terminate) | ~15 min |
| 3 · hard budget action | AWS Budgets action at `$25`: attaches an explicit deny-all to the runner identity. `$10` is a separate email-only alert. | hours (billing lag) |

`make gate-panic` is the "shitting-blood" switch — pull it any time, from any state, and
it's safe to run repeatedly. `MAX_COUNT`, `MAX_AGE_SECS`, `HVG_BUDGET_ALERT`, and
`HVG_BUDGET_HARD` are constants in `scripts/aws-gate/lib.sh` (overridable via env vars of
the same name), not hardcoded per-script.

## Cost model and worst-case bound

Normal cost is ~$1-2.50/day for active use — one `c7g.4xlarge` spot box per active
worktree, alive only while a gate is running or within its 15-min idle window.

**Worst case:** with the tier-1 and tier-2 defaults, a total runaway is at most ~10 boxes
for ≤2h each before the circuit breaker reaps them; a relaunch loop that somehow slips
the count cap is caught by the `$25` budget action. So even if every faster guard failed,
the ceiling is **~$25 before the runner identity is disabled** — and realistically tier 1
stops it within ~5 minutes. The safety net itself is effectively free: the
circuit-breaker Lambda runs ~288x/day, each invocation a sub-second describe-and-maybe-
terminate, well within the Lambda free tier; EventBridge and Budgets cost nothing.

## The libm go-live gate

```
make gate-remote-verify
```

Runs the full gate locally and remotely on the same commit and diffs the regenerated
artifact trees — the acceptance test for whether the remote box is a faithful oracle for
the determinism-sensitive artifact check (`docs/superpowers/specs/2026-07-12-remote-gate-aws-design.md`
§8). Reports one of three outcomes:

- **`VERIFIED`** — the artifact trees are byte-identical; the remote box's determinism
  check is fully trustworthy.
- **`PENDING`** (exit 0, not a failure) — the trees differ, but `Cargo.lock` doesn't yet
  carry a `libm` dependency. Today Linux libm ≠ Apple libm, so a Linux box produces
  different transcendental bytes than the Mac; this divergence is *expected* until the
  in-progress `libm`-crate port lands and makes transcendentals platform-independent.
- **`FAIL`** (exit 1) — the trees differ *and* `libm` is present in `Cargo.lock`. Once the
  port has landed, divergence is no longer explainable by platform libm and is a real
  bug.

The day the `libm` port merges, a green `gate-remote-verify` flips this tool to fully
trusted for the artifact-drift tier of the gate. The compile/test/lint tiers are
unaffected by libm and are trustworthy immediately, before that day.

## Tearing down

```
make gate-remote-teardown
```

Runs `gate-panic` first (stop the bleeding), then enumerates every `project=hornvale-gate`
resource via the Resource Groups Tagging API and deletes it, plus the handful of
IAM/budget resources the tagging API covers unevenly (removed by well-known name as a
backstop). Prints every resource it touches. Complete by construction — anything a future
`setup.sh` adds and a hand-written teardown would have forgotten is still tagged, so it's
still found.
