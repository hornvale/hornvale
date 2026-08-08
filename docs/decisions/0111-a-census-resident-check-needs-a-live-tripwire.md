# 0111. A census-resident check requires a live tripwire in the gate

**Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan · **Relates:**
[0097](0097-assert-the-robust-half-measure-the-fragile-half.md),
[0032](0032-calibration-loads-the-census-fixture.md),
[0110](0110-the-census-is-the-suites-shared-world-building-pass.md)

In the context of moving three world-consistency checks onto the committed
census fixture — and of decision 0097 §4's warning, already realized once:
*The Siding* found the census stale for 139 commits while every gate ran
green, because nothing paired the fixture's generator with a verifier — we
decided that **a census-resident check is not safe to ship until a live
tripwire exists in the gate that rebuilds a small fixed seed set and compares
it against the committed fixture, at a budget of ≤ 15 seconds, built at the
shallowest depth any moved check needs.**

## The mechanism

`windows/lab/tests/tripwire.rs`'s
`the_committed_census_agrees_with_a_live_rebuild_of_the_tripwire_seeds`
rebuilds three fixed seeds (`TRIPWIRE_SEEDS = [0, 1, 2]` — the lowest three,
deliberately unremarkable rather than hand-picked to pass) live, on every
gate run, and calls the **same extractor functions** the metrics use —
reached through `hornvale_lab::registry()`, never reimplemented, because a
reimplementation could drift from the thing it guards. Any disagreement
between the live rebuild and the corresponding cell in the committed
`rows.csv` is a hard failure naming the metric, the seed, and both values.

**The budget is not the couple of seconds a cheap probe suggests.** At the
measured 3.90 s/world for a Full build (debug, the spec's §3.2 profile),
three Full-depth seeds cost ~11.7 s — close to cost-neutral against the
~16.5 s the three retired hunts had cost combined, and the tripwire's first
real run (against the regenerated fixture, `d36be41b`) measured 11.437 s,
matching the prediction. The depth rule — build to the *shallowest* rung any
guarded check needs, never `Full` unless one of them requires it — is what
keeps a future tranche of cheaper checks from paying the Full-build tax
this tranche's three checks (`hydro-variant-coverage` at Terrain,
`toponymic-core-size`/`toponymic-roots-won`/`crisis-fires` at Full) already
need regardless.

## Mutation evidence, required before any check moves

Nathan's condition at G1 (ledger #6): a guard against silent staleness that
is itself silently broken is strictly worse than no guard, because it
converts an honest gap into a false assurance. Before Task 8 moved the first
check, Task 3 proved the tripwire fires: editing exactly one cell in the
committed fixture (seed 0's `crisis-fires`, `true` → `false`) turned the
test red, naming the metric, the seed, and both values verbatim; restoring
the cell returned it to green. The same probe also confirmed a known
residual gap rather than leaving it hypothetical: when a tripwire seed's
`refusal` cell alone goes stale (a world that used to refuse now builds, or
the reverse), the loop's `continue` on `row.refusal.is_some()` skips that
seed's comparison entirely, ~4 s faster and silently. That gap is
recorded in the module's own "What it does not do" section, not fixed — a
forward note for whoever generalizes this mechanism next.

## Consequences

- **This is 0097 §4's generator-paired-with-verifier rule, given a
  mechanism it did not previously have.** 0097 named the risk; this decision
  is what makes the pairing check something that runs, in the gate, on every
  commit, rather than a discipline someone has to remember.
- **Three seeds bound staleness; they do not eliminate it.** A drift that
  moves only seeds outside `TRIPWIRE_SEEDS` still waits for the next census
  regen to surface. The full proof remains `calibration.rs`'s `#[ignore]`d
  `census_fixture_matches_live_run`. This decision accepts a bounded gap in
  exchange for gate residency at ≤ 15 s; the alternative (probing all ~200
  metrics) is exactly what exiled `fixture_staleness.rs` to the heavy tier
  in the first place.
- **The refusal-skip gap is a named follow-up, not a fixed defect.** A
  generalizing campaign should decide whether a stale refusal status needs
  its own comparison arm before extending `GUARDED` much further.
- **The ≤ 15 s budget is a design constraint on future tranches, not just a
  measurement of this one.** A tranche of Terrain-rung-only checks costs
  ~1.5 s for three seeds; a tranche that needs Full depth inherits this
  tranche's ~11.7 s regardless of how many checks it adds, because the cost
  is the build, not the check count.

## See also

`docs/superpowers/specs/2026-08-07-the-assay-design.md` §4.2; the Task 3
report (green/mutation/restore/refusal-probe evidence, verbatim panic text
in `tripwire.rs`'s module doc).
