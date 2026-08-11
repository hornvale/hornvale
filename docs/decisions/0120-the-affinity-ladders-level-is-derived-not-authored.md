# 0120. The affinity ladder's level is derived, not authored

**Status:** Accepted (2026-08-10, G6) · **Decider:** Nathan · **Relates to:**
[0009](0009-models-author-dice-roll.md),
[0016](0016-studies-preregister-hypotheses.md),
[0106](0106-a-constants-justification-must-match-its-kind.md),
[0107](0107-habitability-is-a-relation-not-a-constant.md)

In the context of a biome-affinity row whose fallback level was an authored
`0.25`, facing the discovery that the number had never been derived from
anything and was emptying the world, we decided that **a row states a *shape*
and never a *level*** — the level is the kind's own
`sovereignty_floor(mass, potency)`, and the ladder's rungs are preferences in
`[0, 1]` mapped through it — accepting that this relevels every row in the
registry at once, including rows a merged campaign authored and no later
campaign edited.

## The rule

```
  factor(biome) = floor + (1 − floor) · preference(biome)
  floor         = hornvale_kernel::sovereignty_floor(mass, potency)
```

The ladder's four steps are **preferences**, not factors: stronghold `1.00`,
near `0.70`, marginal `0.45`, elsewhere `0.00`. Two consequences follow by
construction rather than by convention — a row's `default` **is** its floor
exactly (the `preference == 0.0` case, which is why the elsewhere rung is never
listed), and a stronghold is exactly `1.00` for every kind however heavy, so the
rungs stay comparable across the registry.

`BiomeAffinity::from_preferences` is the only constructor
`biome_affinity_registry` uses, so the mapping cannot be bypassed by hand, and
`radiation_affinity.rs::every_row_is_the_ladder_mapped_through_the_kinds_sovereignty_floor`
asserts it for every row.

**Context.** The affinity row's fourth step was `0.25`, and its provenance is
that it appeared in The Range's implementation plan **only inside test-fixture
code written as illustration**. It is in that campaign's spec nowhere. The
Range's implementer adopted the example as an authored constant; The Radiation
adopted that constant as house style for six more kinds. Decision 0106 asks
every constant to declare what kind of truth it is; this one had been given the
`gauge` classification informally — *"a uniform affinity is a placement no-op;
the level is gauge"* — and that classification was **half true**. The level is
gauge for how a single kind *ranks* cells, since a scale-free ranking cannot be
reordered by a constant. It is load-bearing for the very next consumer
downstream, because the same factor multiplies the capacity that becomes a
settlement's population, and the history bake's volume is a function of
population.

**Why this quantity and not another.** This is the model's own algebra rather
than a new one. `ConditionResponse::eval` is
`floor + (1 − floor) · devotion · bump` — the same `sovereignty_floor`, in the
same position, mapping a preference in `[0, 1]` into `[floor, 1]`. And
`sovereignty_floor` is the model's single existing statement of how much
environmental unsuitability a creature's mass and potency buy it off. A biome
affinity asks that same question one level coarser, over classes rather than
along an axis, so it takes the same answer instead of an unrelated second one. A
second answer would be a second model.

Rejected on evidence, and recorded so they are not re-proposed: raising the
level to `0.50` (one undesigned number for another, with a floor-shaped
justification attached); grading it off the marine forage supply field (a
productivity gradient, ocean-only, and for one kind a literal squaring of a
curve its resource axis already reads); deriving it from elevation devotion
(circular). Replacing the fourth step *alone* was also rejected, and this is the
argument for remapping the whole ladder rather than only its base: gnoll's floor
is `0.495` and the woolly mammoth's `0.692`, both above the old `0.45` marginal
step, so a biome declared *marginal* would have scored below an unlisted one and
declaring a preference would have been a penalty for holding it.

**Not a double count.** Every occupant of this registry has
`elevation.devotion < sovereignty_floor`, which is the admission test's own
inequality, so `tolerance_liebig`'s minimum is the unfloored elevation term at
every cell and the floor computed *inside* `per_species_suitability` never
reaches the product. Read that narrowly: the floor computed *here*, to set a
row's level, is applied **outside** the minimum and is not discarded at all.
Collapsing the two produced a general rule — *mass does not reach this path's
output* — that is refuted by mutation, and it reached this campaign's spec
before it was caught.

## Cost accepted

**Deriving the level relevels the entire registry, including rows nobody
edited.** This is the cost, and it is not hypothetical: it retroactively broke a
merged campaign's published finding. The Range measured gnoll falling 20 → 2
settlements while its arid share rose only 0.000 → 0.500, and built a caveat on
it — *a downward-only mask suppresses rather than relocates, nine settlements
removed for every one that moved*. Under this rule the same row gives gnoll
**13 → 40** at an arid share of **0.825**. The old fourth step had been
suppressing the world at The Range too; one settling people in nine could not
move a census, so nothing saw it. That campaign's chronicle and its Confidence
Gradient bet are corrected as part of The Radiation's close.

**What it cost to find.** Six rows at the authored level took seed 42's history
from 552 occupation records to 193, settlements 192 → 100, subordinations
232 → 41, standing tribute 83 → 17 — four deliberate fidelity floors breached at
once, and the dose non-linear in the number of rows. Under this rule the same
six rows give 704, *above* the 552 measured with no elf rows at all. The
implementer stopped rather than lowering the floors, which is why the number was
derived instead of nudged.

**This makes the level principled; it does not split the job.** One number still
serves two consumers — a ranking that does not care about its magnitude, and a
productivity that does. That is registered as an open successor
(`BIO-affinity-level-is-two-quantities`), and the same class of surprise can
recur wherever a justification is true of one consumer of a quantity and silent
about the rest.

**See also.** [The Radiation](../../book/src/chronicle/the-radiation.md);
[the retrospective](../retrospectives/the-radiation.md);
[The Range](../../book/src/chronicle/the-range.md);
the spec's §3.2 erratum
(`docs/superpowers/specs/2026-08-09-the-radiation-design.md`);
`domains/species/src/lib.rs::biome_affinity_registry`.
