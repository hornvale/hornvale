# The Region — retrospective

Process lessons, not product. One page.

## What worked

- **Autopilot's hard stops held.** G3 (spec) and G6 (merge) were the only
  gates that waited on Nathan; the fidelity call surfaced as the one
  mid-brainstorm carve-out and forked the design correctly. The G4 plan
  self-review caught a real bug in my own plan (a commutation test written
  with `assert_eq!` where floating-point commutation is only tight-approximate)
  before any subagent ran — evidence the self-review step earns its place.
- **Producer-sourced golden discipline carried over from The Isotherm** without
  re-teaching. The regional evaluator-equivalence golden was generated from
  `temperature_grid_region` (a throwaway example), committed to orrery testdata
  with provenance, and the client reconstructs — never a client golden
  admiring itself.
- **The interpolation guard test** (a continuous layer must differ from
  nearest-cell somewhere) came out of a task review, not the plan. It guards
  the campaign's whole reason for existing against a silent regression to
  nearest-cell. Reviews finding the test the plan lacked is the loop working.

## What to carry forward

- **In a worktree, Write/Edit with worktree paths, not main-checkout paths.**
  The spec and an idea-registry edit landed in the *main* checkout first
  (absolute main paths issued while the shell cwd was the worktree), and had to
  be relocated and the main tree reverted. The Read tool anchoring a file to
  its main-checkout path silently misdirects later writes. Cost: ~15 minutes
  and a near-miss on polluting main for parallel sessions.

- **Run the full gate at baseline to characterize inherited reds first.** I
  trusted a memory note that main was green; it was stale — Sculpting (and
  later the True Name/KindId campaign) had merged, leaving a census-fixture
  schema lag (`rows.csv` missing Sculpting metric columns) that makes
  `hornvale-lab` calibration tests red on main *independent of any campaign*.
  It surfaced only at the final gate, forcing a mid-close investigation to
  prove the red was inherited (confirmed by running the tests on unmerged main).
  A baseline gate up front would have named it on day one.

- **A catalog version bump re-pins every downstream golden, including a prior
  campaign's.** Cutting `world-wasm-v3` shifted The Isotherm's *global*
  temperature golden by ~0.82 °C — Sculpting's terrain had moved the world
  since v2, and temperature rides elevation. This is correct (a release
  snapshots current main), but budget for it: a catalog-bump campaign owns
  re-pinning goldens it did not author.

- **Stage-boundary absorption was skipped.** The campaign ran fast enough to be
  effectively single-session, so its first meeting with main was at close —
  where main had moved 8+ commits (the True Name/KindId ECS campaign). The
  absorption was clean (one generated-file conflict in the type-audit report,
  resolved by regeneration), but the CLAUDE.md cadence (absorb at every
  plan-stage boundary) would have surfaced the census lag and the KindId
  re-keying earlier and in smaller pieces.

## Deferred at close (Nathan's calls)

- **Census/calibration regen deferred** ("no regen yet"). The merged main
  therefore ships with the inherited census lag red; the AWS regen (which also
  clears it for every session) is the standing next step. This is *not* a
  Region missed re-pin — verified inherited.
- **The Menagerie fauna-panic fix** stays standalone (census-path; a parallel
  session's `d88cb6d` already carries a related `capacity-by-abs-latitude`
  fauna-filter fix).
- **The full CDLOD renderer** (orrery#2 proper) is its own follow-on campaign,
  now unblocked; this campaign shipped the contract + a single-patch proof.
