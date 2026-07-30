# CLAUDE.md — working in `scripts/`

These scripts are the connective tissue of the gate ladder, artifact
regeneration, and census runs (plus `aws-gate/`, kept only as history). Read
the root `CLAUDE.md` "Commands" section for the `make` targets that call
them.

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
  sanctioned pre-merge refresh — run it via **`census-run.sh`**, not
  `regenerate-artifacts.sh` directly, once per campaign at the close, keeping
  the fixtures current with main. Since decision 0081 all three entry points
  serialize (one heavy writer per box, bounded 45-min wait, `census-run.sh
  status` to ask); the wrapper additionally ledgers the run in
  `docs/timings.md`
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
  Takes no claim: short jobs never queue behind long ones (decision 0081).
- `gate-full-heavy.sh` — the cost-tagged `heavy:` `#[ignore]`d tier that the
  commit gate defers (see `cli/tests/heavy_tier.rs`). **Takes the shared box
  claim** (decision 0086) — here, at the seam, rather than only in the wrapper,
  because a wrapper cannot guard a direct `make gate-full`. Where there is no
  `flock` (macOS ships none) it proceeds unserialised with a note rather than
  failing.
- **`heavy-run.sh`** — run the heavy tier on THIS box under the shared claim,
  the same way `census-run.sh` runs a census. `HV_HEAVY_REF=<sha>` runs a
  pushed ref in a scratch worktree; `status` asks who holds the box and is
  legal from any machine — but it reads the claim in the **local** `/tmp`, so
  from the Mac it always says "no". Use **`make heavy-status`** to ask the
  canonical box instead; that is almost always the question you mean. Carries the canonical-host guard, because the tier
  **authors committed artifacts**: `the-history` (`cli/tests/history_battery.rs`),
  `the-sounding` (`windows/chronicle/tests/sounding_sweep.rs`), and
  `occupancy.csv` (`windows/worldgen/tests/occupancy_readout.rs`) — plus
  `census_fixtures_match_a_probe_of_live_seeds`, which compares a live probe
  against lefford-authored fixtures. Review and commit those artifacts **on
  the canonical box**. Dispatch from the Mac with `make heavy-remote REF=<sha>`.
- `test-heavy-lock.sh` — proves the claim EXCLUDES (second acquirer refused
  while held; a normal exit and a `-9` both release), not merely that a lock
  file exists. Skips where there is no `flock`.
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
  points git at them). The hook also carries the **golden-pins.sql tripwire
  guard**: staging any of `windows/lab/tests/{calibration,
  branches_family_calibration,gathering_calibration}.rs` or
  `tools/census/queries/calibrate/golden-pins.sql` runs `make census-check`
  (~2.5 min) before the commit lands. That SQL file deliberately duplicates
  every calibration pin as an independent check against the committed census
  fixture, and the duplication went stale twice (2026-07-13, 2026-07-20)
  because nothing forced it back into sync — this closes the gap by
  construction rather than by memory.
