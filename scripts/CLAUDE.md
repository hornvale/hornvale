# CLAUDE.md — working in `scripts/`

These scripts are the connective tissue of the gate ladder, artifact
regeneration, and the AWS remote gate. Read the root `CLAUDE.md` "Commands"
section for the `make` targets that call them.

## The single source of truth for artifacts

`regenerate-artifacts.sh` regenerates **every** committed generated artifact
(three seed-42 almanacs, the elevation map, registry/manifest dumps, lab
studies, the type-audit report) — and CI + `make rebaseline` both call it, so
local and CI regeneration cannot silently diverge (that's the point). Key
knobs:

- **`SKIP_CENSUS=1`** — skip the census `lab run`s. CI sets this and uses a
  fast seed probe (`ci-census-probe.sh`) instead; everyday local regen also
  skips them so the gate stays fast.
- **`HV_CENSUS=1`** — regenerate the censuses. Since The Local Census the
  full ~2000-world census is a ~7-min LOCAL run (was ~1–2 h), so this is the
  sanctioned pre-merge refresh — `HV_CENSUS=1 bash regenerate-artifacts.sh`,
  once per campaign at the close, keeping the fixtures current with main
  (decision 0063, superseding 0046's AWS-only mandate). `make regen-remote`
  (the AWS box) is abandoned — this box is the single canonical platform
  (decision 0063; AWS differs on ~0.1% of discrete-count metrics).
- **`census-run.sh`** — run a census on THIS box under a `flock`, so triggers
  from either dev machine (over SSH) queue one-at-a-time instead of contending.
  `scripts/census-run.sh` regenerates the canonical goldens; `HV_CENSUS_REF=<ref>
  scripts/census-run.sh` runs a pushed branch in a scratch worktree. Only this
  box authors goldens (the canonical-machine constraint, decision 0063).
- After regen, the drift check is `git diff` over
  `book/src/gallery book/src/reference book/src/laboratory docs/audits` — note
  **`docs/audits/`** is in the list (the type-audit report drifts on
  pub-boundary changes; a common miss).

## The gate ladder

- `gate-fast.sh` — scopes fmt/clippy/test to changed crates (iteration only).
- `gate-full-heavy.sh` — the cost-tagged `heavy:` `#[ignore]`d tier that the
  commit gate defers (see `cli/tests/heavy_tier.rs`).
- `preflight-merge.sh` — GO/NO-GO before integrating a campaign branch;
  peeks at main's checkout and warns if another session is mid-landing.
- `doctor.sh` — the repo self-map (`make doctor`); good orientation for a
  fresh session.

## `aws-gate/` — billable, admin-credentialed, handle with care

The remote gate provisions real EC2 spot infrastructure. `gate-remote.sh`
runs the CI gate on a spot box; `gate-remote-verify.sh` is the local-vs-remote
**byte-identity acceptance test** (the ratification gate for any
determinism-config change — e.g. Proposed decision 0061). `panic.sh` is the
emergency stop: it **deactivates the runner identity first** (so nothing can
launch), then terminates tagged instances — that order is asserted by
`test/test_panic.sh` and must not be reversed. Setup/teardown are
confirmation-gated in the Makefile.

## Shell conventions

- Every script must pass `make shellcheck` (all of `scripts/**`). Prefer
  explicit `if`/`then` over `A && B || C` (SC2015) — `C` also runs when `B`
  fails.
- Scripts run under `set -euo pipefail` where they can; `|| true` is used
  deliberately where a step is best-effort (e.g. `panic.sh`, safe to run
  repeatedly).
- The git hooks in `hooks/` run `make quick` pre-commit (`make install-hooks`
  points git at them).
