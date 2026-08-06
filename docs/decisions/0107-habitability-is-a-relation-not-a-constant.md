# 0107. Habitability is a relation, not a constant

**Status:** Accepted (2026-08-05) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0097](0097-assert-the-robust-half-measure-the-fragile-half.md),
[0103](0103-suitability-and-headcount-are-distinct-types.md),
[0106](0106-a-constants-justification-must-match-its-kind.md)

In the context of discovering that Hornvale carried **three mutually
inconsistent oracles for the word "habitable"**, we decided that **habitability
is not a property of ground but a relation between a people, a cell, and an
era — and that its single expression is capacity.** A cell is habitable *for
someone*, or not at all.

## The three oracles, and why one had to win

```
  oracle                       rule                                    read by
  ---------------------------  --------------------------------------  ------------
  era.habitable (bake_eras)    land above era sea level AND mean       Bake::factor
                               temp >= FREEZE_C (-10 C)
  caps > 0      (vacant_for)   this species' capacity is non-zero      siting, refuge
  glacial_maximum_habitable    a full climate rebuild via              refugia
                               hornvale_climate::is_habitable
```

The first two disagreed over roughly **half of all land**, because they were
computed from two different climates. The third chose the refugia the bake then
routed migrants toward, by a rule neither of the others used. A world could
therefore route a people onto ground that a second oracle called dead and a
third called a refuge.

The decisive asymmetry is that only one of the three can express *for whom*.
`era.habitable` is a `CellMap<bool>` — it has no species argument and structurally
cannot acquire one without becoming a capacity field. Capacity already carries
both indices. So the collapse had exactly one admissible direction.

## What follows

**Cold ground is not gated, it is poor.** A glacial maximum squeezes a world
instead of switching it off. This is what The Fallow wanted a soil stock to
supply and could not reach, and it is why stages 6 and 7 of The Tilth both
failed: they were arguments about how a *binary gate* should behave, and the
gate was the wrong object. A floored axis under `min()` can never bind, so
whichever axis is bare decides everything — the same defect on two axes, landed,
measured, and reverted in `511d1fa9`.

**Eviction becomes a pressure outcome, not a gate outcome.** `step_community`'s
`eff == 0.0` branch fires far more rarely, because an exact zero is rare in a
continuous field. A people that cannot feed itself now tries to leave before it
dies, and starves only where there is nowhere to go.

**A mask that no longer decides anything must not be left looking as if it
does.** `Bake::factor` now gates on ice alone, and `era.ice` is identically
empty on every production path, so the mask is inert. Two bake fixtures had
built their "unusable ground" out of `EraClimate.habitable` and silently lost
the property they were constructed to exercise — a vassal with nowhere to go
found somewhere, and a roller that should have widened to the third ring stopped
in the first. Both passed nothing for as long as they were green. **A fixture
that wants dead ground must now say so per-people, in capacity.**

**Ocean exclusion must ride supply, not the mask.** It currently rides
`elev >= sea_level` inside the era mask; a terrestrial uptake vector has no
supply at sea, which is where the exclusion belongs. This is the most likely
site of a silent regression and carries its own guard (`era_substrate.rs`).

## The cost this buys, stated honestly

Collapsing to one oracle **compresses the between-world variance in
habitability**. Measured at the two endpoints of this arc:

- **Seed 1234** was a dead world for the whole campaign — 0 survivors — and now
  carries 36 alive, 70 sites, and 16-deep occupation columns, with
  recolonisation across centuries 13–19.
- **Seed 42**, the flagship, fell from **209 settlements to 122**, and its chief
  settlements lost 30–50% of their populations (bugbear 88 → 67, goblin 82 → 41,
  human 77 → 36).

That is the same mechanism read from both ends: a gate has no gradient to
degrade along, so it produces all-or-nothing worlds; a continuous squeeze
produces middling ones. **Dead worlds live and rich worlds thin.** Whether that
compression is desirable is a separate, open question about `V_max` and the
response curves — it is not a defect in the collapse, and this record does not
settle it.

## What this record does not decide

- **Whether seed 42's new thinness is correct.** The 209 → 122 fall is measured,
  not judged.
- **The two-tier gate/modifier split** (spec §3.3). `tolerance_tiered` exists and
  nothing calls it; it is the successor to the flat `min()`, landed in shadow.
- **Fundamental versus realized niche.** `per_species_capacity` answers "could
  this species live here *alone*"; the bake uses it to answer "does this species
  live here". Ecology has kept those apart since Hutchinson (1957); Hornvale has
  one field and one word. Competition exists downstream in `coexist::pack` and
  never feeds back.
- **Within-cell temperature distribution** (spec §3.4). Capacity reads the cell
  *mean*, and by Jensen's inequality `R(mean(T)) != mean(R(T))` for a nonlinear
  response — which overestimates near the optimum and underestimates in the
  tails, and *the tails are where refugia live*. Unstarted and first-class.

## See also

The Tense spec §§2.1, 3.2 (`docs/superpowers/specs/2026-08-05-the-tense-design.md`);
The Tilth spec (`2026-08-04-the-tilth-design.md`), whose stages 6 and 7 this
record explains the failure of; decision 0106, whose `hornvale-choice` cell is
where `K_m` and `V_max` sit.
