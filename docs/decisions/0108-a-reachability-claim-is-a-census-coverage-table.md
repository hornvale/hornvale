# 0108. A reachability claim is a census coverage table

**Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan · **Relates:**
[0093](0093-seed-hunting-is-not-a-test-mechanism.md),
[0097](0097-assert-the-robust-half-measure-the-fragile-half.md)

In the context of retiring `domains/terrain/tests/hydro_witness.rs` — a test
whose own module doc already stated the failure it existed to prevent
(`Hydro::Spring` and `Hydro::Aquifer` were unreachable on every seed for the
whole life of the lithology model, and every hand-built unit test touching
them passed regardless, because a unit test over a constructed input
certifies the function, not that anything real ever calls it with those
values) — we decided that **a ∀-variant-∃-seed reachability claim is a census
question, answered as a per-variant coverage table over the committed
fixture**, extending decision 0093's two-instrument split (census / synthetic)
to a third.

## Why neither existing instrument fits

0093 split seed-sweeping tests into two boxes: a *census* question (measure a
frequency, pin the distribution) or a *synthetic* question (hand-build a world
already in the state a behavior needs, test the behavior directly, zero
builds). `hydro_witness` is neither. It is not measuring a rate — "how often
does `Spring` appear" is not the claim; the claim is "`Spring` appears at
all," a claim a rate can only express as a boundary case (`> 0`). And it
cannot be synthesized: the property under test is *reachability of the
derivation itself* — whether the real lithology pipeline, run on real input,
can ever emit a given `Hydro` variant. A hand-built world with a committed
`Hydro::Spring` fact would test the *consumer* of that fact, not whether the
producer can produce it; the whole point is that hand-built inputs were
exactly how this class of bug hid for the model's entire life.

A reachability claim's natural shape is `∀v ∈ V. ∃s. P(v, w_s)` — for every
variant, some seed exhibits it. Read against a fixed small seed set, that is
what `hydro_witness` swept 8 seeds to answer, breaking early once every
variant was found. Read against the census's 1,000 seeds, the same claim
becomes a coverage table: for each variant, the count and share of worlds
that exhibit it. A coverage table subsumes the boolean claim (`count > 0`)
and reports strictly more — which the 8-seed sweep never could, because a
sweep that stops at first success cannot say whether the thing it found was
common or vanishingly rare.

## The decision

A test whose only use of a seed sweep is to establish `∃s. P(v, w_s)` for
each `v` in a small enumerable set does not sweep seeds itself. It registers
a `Categorical`/coverage-shaped metric against the shared census build, and
its gate-side assertion loads the committed fixture and asserts each
variant's row count is nonzero, naming the absent variant on failure. The
metric derives its checklist from the type itself (`Hydro::ALL`), so a new
variant self-enrolls without a corresponding code change to the test.

## Consequences

- **`hydro-variant-coverage`'s own result is the honest anticlimax this
  record exists to report.** On the regenerated 1,000-world census, all five
  `Hydro` variants appear on all 1,000 worlds — one combination across the
  whole population, zero variance. The 8-seed sweep it replaces was passing
  on its first world every time. A coverage table earns its keep here not by
  finding a gap but by being able to *say* there wasn't one, at a sample size
  the retired test could never afford.
- **The gate cost drops to the fixture-load floor.** 24 census-resident
  calibration checks against 1,000 worlds already cost less than the three
  retired hunts cost against a handful of worlds each (spec §3.1) — this
  extends that comparison to a fourth check family rather than repeating a
  fresh measurement.
- **This does not license treating every ∀∃ claim as a coverage table
  without checking feasibility first** — see decision 0112: a reachability
  claim that cannot be read off committed facts, or whose predicate needs a
  build the census does not already perform, still needs its own
  instrument.

## See also

`docs/superpowers/specs/2026-08-07-the-assay-design.md` §3.4, §5;
`windows/lab/tests/calibration.rs`'s
`every_hydro_variant_is_reachable_somewhere_in_the_census` (the gate-side
assertion, replacing the deleted `domains/terrain/tests/hydro_witness.rs`);
`windows/lab/src/metrics.rs`'s `hydro-variant-coverage` extractor.
