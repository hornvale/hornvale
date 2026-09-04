# 0686. A kind's prevalence never normalises against its siblings

**Status:** Accepted (2026-09-03) · **Campaign:** The Weft · **Decider:**
Nathan · **Relates:** [0669](0669-a-sites-tier-is-placed-or-derived.md),
[0009](0009-models-author-dice-roll.md)

In the context of building the derived-feature tier (four kinds — spring,
overhang, thicket, erratic — each occurring at facet resolution from a
per-kind prevalence recipe), and facing the temptation to make per-kind
prevalences sum to 1 so "how much of the world is derived-feature" reads as
one clean number, we decided that **no mechanism normalises a kind's
prevalence against its siblings — each kind's abundance is independently
dialable to 1.0**, accepting that the four kinds' occurrence sets may
overlap and that no single number caps how much of the world can carry a
derived feature.

## Context

A simplex constraint (prevalences summing to 1, the way a categorical draw's
outcome weights must) is not a tuning choice — it is a **structural cap on
enterable density**. Raising one kind's abundance would necessarily lower
another's, which is exactly what campaign #1's rider 3 forbids: no mechanism
may impose a ceiling on how much of the world is enterable. The Prospect
already measured the ceiling a *placed* point process imposes (1 site per
9,830 facets, arithmetic on the icosahedral grid); a normalised derived
tier would import the same shape of cap through a different door.

## Decision

Each `WeftKind`'s abundance is its own free parameter, tunable "individually
and severally" (spec §5.2's phrase). Nothing sums the four kinds' recipes to
1, and nothing constrains one kind's eligible-facet share as a function of
another's. A facet may carry more than one kind's eligibility at once; which
kind actually occurs there is resolved by the per-kind prevalence draw, not
by a shared allocation.

## Consequence

**Measured, not merely permitted.** Seed 42's four kinds reach a **combined
0.256285** of land-eligible facets — spring **0.035924** (403), overhang
**0.075147** (843), thicket **0.135229** (1,517), and erratic **0.038153**
(428) by their own per-kind occurrence tests — while each kind's own abundance
constant remained set independently, with no accounting against the others.
Raising any one kind's abundance toward 1.0 lowers nothing else; the four are
additive in eligibility, never partitioned. An earlier draft of this paragraph
put H3's mutual-information readings in those four slots; Task 13's close sweep
corrected the cross-metric substitution against the pinned readout table.

**A future fifth kind inherits the same freedom.** Spec §5.7's "a kind is
three things and nothing else" — a component bundle, a prevalence recipe,
the three scalars — is an *additive* append precisely because there is no
shared budget a new kind must draw from. Any future normalisation proposal
should read this record first: it would reopen the cap #1 rider 3 already
closed.

## See also

`windows/worldgen/src/weft/kinds.rs` (per-kind abundance constants);
`windows/lab/tests/suite/weft_density.rs` (the measured per-kind and union
figures); spec §5.2 ("Nothing normalises across kinds… a simplex constraint
is exactly the structural cap #1 rider 3 forbids"); [The Weft
chronicle](../../book/src/chronicle/the-weft.md).
