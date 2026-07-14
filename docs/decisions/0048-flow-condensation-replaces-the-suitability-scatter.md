# 0048. Flow-condensation replaces the suitability scatter

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of population becoming a physically-grounded, conserved
field (the-gathering, MAP-7's field half) rather than a free variable drawn
per site, we decided that **flow-condensation is the sole settlement-
placement mechanism**, retiring the suitability-scatter formula and its
greedy spacing pass entirely, rather than keeping both as coexisting tiers a
world could choose between.

**Context.** The retired mechanism scored every habitable cell with a
hand-weighted suitability sum (freshwater, coast, temperance, minus
hostility), ranked and greedily spaced the winners, and drew each site's
population from its own suitability score — a static equilibrium with no
conserved quantity and no field a later relaxation could evolve. The
replacement condenses settlements as attractors of an up-gradient population
flow over a carrying-capacity field `K` (`domains/terrain/src/drainage.rs`'s
proven shape, comparator flipped): every settlement's population is the
field integrated over its catchment, so `Σ population == Σ K` holds by
construction at the field level, not by tuning. The two mechanisms are not
complementary fidelities the way, say, `ConstantSun` and a generated star
system are (tier-0/tier-1 coexisting by design) — the suitability scatter
was never a fidelity a world would meaningfully choose to keep, and running
both would mean maintaining two placement mechanisms with no shared
downstream reader. One mechanism, retired outright, was judged cheaper than
an opt-in tier no world had reason to select.

**Consequence.** The blast radius was accepted and paid in the landing
commit: every generated almanac, the settlement map, the dictionary's
biome-gap rows, and settlement-touching golden fixtures were regenerated
(seed-42 count 998 → 182 at the calibrated threshold). The `--min-suitability`
scenario pin (`SETTLEMENT_PIN`) is retired with the mechanism it configured;
saves carrying a `settlement-pin` fact from before this change still load
(the fact is ignored, as unknown pins already are — no save-format break).
A documented, accepted regression rides along: the interim per-species
condensation loosens the old cross-tag spacing rule (two species may now
co-occupy geography), because full multi-species competitive exclusion is
MAP-22's job, not this campaign's — see decision 0047's crate split and the
MAP-22 coexistence-stack spec.

**See also.** Spec: `docs/superpowers/specs/2026-07-13-population-field-design.md`
§3 (flow-accumulation & condensation), §5 (the conservation invariant); the
MAP-22 coexistence-stack spec
(`docs/superpowers/specs/2026-07-13-the-coexistence-stack-design.md`), which
restores cross-species exclusion properly; decision 0047 (the `demography`
crate this mechanism lives in).
