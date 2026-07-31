# Retrospective — The Weir

Process lessons only; the product story is the chronicle.

## What worked

- **The compiler as migration checklist.** Deleting the thirteen readouts
  first and letting `cargo check --workspace` enumerate every straggler
  beat any grep-driven plan: the plan's own caller counts were wrong twice
  (cli/dictionary.rs's "13 sites" were pattern noise; `derive_wild_npcs`
  was a real caller the plan missed) and the compiler was wrong never.
- **The review loop caught a Critical in a "docs and attributes only"
  task.** The crate-level `#[allow(clippy::disallowed_methods)]` silently
  disabling the libm bans is the sharpest catch of either perf campaign —
  a lint-semantics fact (`one lint, one switch per scope`) that no test
  could see, found because the reviewer refused to assume and built a
  throwaway crate to check. Corollary: there is no such thing as a
  low-risk task category; risk lives in semantics, not diffs.
- **Red-run discipline on the synthetic arm.** Green-then-flip-then-red,
  with preconditions asserted before the gate assertion, is what makes a
  hand-built-world test evidence instead of theater (the
  passing-by-the-wrong-path lesson applied prospectively for once).
- **Bounded re-find under pressure.** The plan's smoke constant (seed 56)
  drifted before merge; the implementer ran the ten-seed scan 0093
  prescribes instead of restoring the sweep, and the drift became the
  mechanism's first live demonstration. Write the re-find rule INTO the
  epoch-sensitive constant's comment — it worked.

## What to change

- **Check lint scoping semantics before shipping an allow.** An
  `#[allow(lint_name)]` suppresses the whole lint family in scope, not the
  entry you meant. When two policies share one lint (`disallowed-methods`
  now carries libm + derivation), every blanket allow trades away both.
  House rule going forward: production allows are function-scoped;
  crate/module blankets only in test scope, and the trade recorded in the
  owning decision.
- **A verification task's battery list should be chosen by what the change
  can break, not by what the campaign has been running.** Task 4's brief
  reused the campaign's habitual batteries and omitted `session_snapshot`
  — the actual byte pin for the path being restructured. The reviewer ran
  it; the brief should have. Name the *pinning* test for the touched
  surface explicitly in every verification step.
- **Plan-time caller counts are hypotheses, not facts.** State them as
  "working estimate, verify live" (this plan did) and expect both false
  positives and misses; budget fix-round time for prose the mechanical
  migration leaves stale — three of this campaign's five fix rounds were
  doc-truthfulness, and two of those were introduced BY mechanical
  migration ("_of → _from" substitution mangling contrast sentences).
- **"Zero X" claims attract counterexamples.** "Zero builds" shipped over
  a measured 1.5 s inert sculpt. The correction cost a round. Quantified
  claims in test docs get measured before they get written, same as spec
  claims.

## Deviations recorded

- `exposure_from` promoted to pub (Task 2 — the deletion removed the only
  public exposure entry; minimal consequence).
- `lexicon_of_in` deleted beyond the ratified list (ledger #6 — uncalled
  sculpting wrapper the lint would have redded within two tasks).
- `derive_wild_npcs` takes pre-fit concentrations, not `&DemographyReport`
  (avoids a new vessel→demography dep edge).
- Layering page did not drift at rebaseline (plan over-predicted; it moves
  only on dep-graph changes).
- Census schema doc-strings fixed via the sanctioned `backfill-schema`
  mechanism, not a live census (doc prose only; `"backfilled": true`
  markers will vanish at the next real census regen — expected).
- 0093/spec seed constant corrected 56→57 pre-merge (record not yet
  ratified on main; the drift itself is recorded in the decision).

## Follow-ups (promoted from the worktree register before teardown)

- **The remaining half of the weir:** ~16 worldgen `(world)`-shaped lens
  readouts (`seas_lines`, `firmament_lines`, `sky_report`, `world_name_in`,
  `vestige_png`, kin) survive behind sanctioned allows on cold CLI/book
  paths. Same treatment as the chorus family when their cost matters or
  their allow-boilerplate normalizes.
- A* / room-addressing precompute (~32 % of the health battery — the
  largest remaining sim cost; own campaign, own evidence).
- Scene-window threading (the Casement line owns the client half).
- The broader build-volume audit under 0093's criterion (which other tests
  over-build; the soc1 sweep was the surgical instance).
- `kernel::tick` clone: measured ~2 % — deferred WITH the measurement so
  it isn't re-suspected from the code smell.
- `threading_equivalence` is a purity pin post-deletion; a committed
  golden snapshot would restore teeth if wanted.
- The `_from` suffix is now vestigial (no `_of` twins remain for the
  deleted family) — a quiescent-window rename sweep is parked in the
  campaign ledger.
- `demography_report_with_beta_from`'s beta sweep mirrors (not calls) the
  demography pipeline — a five-witnesses drift risk worth a cross-pin.
