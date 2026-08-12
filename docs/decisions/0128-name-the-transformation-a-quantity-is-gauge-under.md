# 0128. Name the transformation a quantity is gauge under

**Status:** Accepted (2026-08-12) · **Decider:** Nathan · **Relates to:**
[0106](0106-a-constants-justification-must-match-its-kind.md),
[0120](0120-the-affinity-ladders-level-is-derived-not-authored.md),
[0016](0016-studies-preregister-hypotheses.md)

In the context of a derived quantity being defended as *gauge* — free to move
because some consumer cannot see it — facing the measurement that the biome
affinity ladder's level is gauge under a **uniform rescale** and load-bearing
under a **level change**, we decided that **a gauge claim must name the
transformation it holds under and the consumers it was checked against, or it
is not a claim at all**, accepting that this makes "X is gauge" a longer
sentence in every constant's doc block and retires the short form entirely.

## The rule

A gauge claim has three parts, and a claim missing any one of them is
inadmissible as a justification for leaving a quantity underived:

1. **The transformation.** Gauge under *what* — a uniform rescale of a whole
   row, an additive offset, a reparameterisation? "Gauge" alone names an
   equivalence class without saying which one.
2. **The consumers checked.** Which readers of the quantity were measured to be
   invariant, and by what. A consumer not named is a consumer not checked.
3. **The consumers not checked.** Named as such. The failure mode this record
   exists to prevent is not a false statement; it is a **true and incomplete**
   one, and an incomplete statement is more durable than a false one because
   nothing contradicts it.

## What forced it

The affinity ladder's level was defended for two campaigns with the sentence
*"a uniform affinity is a placement no-op; the level is gauge"*. Decision 0120
already found that half true — the same factor multiplies the capacity that
becomes a settlement's population, so the level is gauge for how a kind *ranks*
cells and load-bearing for the very next consumer downstream — and let the
ranking exemption stand.

**The surviving exemption is false too, and The Muster measured it.** A change
to the level is not a uniform rescale.
`BiomeAffinity::from_preferences` maps each preference to
`floor + (1 − floor) · p`, which holds a stronghold at exactly `1.00` while
pulling every lower rung down: it changes the ladder's **contrast**, not its
scale. The factor then multiplies the capacity field per cell, keyed on that
cell's biome, so it reweights biome against every other condition in the
limiting product, and cells reorder.

Measured on seed 42, the seven authored rows moved from their shipped level with
every authored shape held fixed:

- **all seven row-carrying kinds have their own cell ranking changed**, and
  `gnoll`'s argmax — the cell `best_home` would pick — moves from cell 30312 to
  cell 2276, with 5 of its top 50 cells surviving;
- **all eleven row-less kinds are bit-identical**, which is the control: for a
  kind carrying no row the factor is `1.0` at every level, and there the level
  genuinely is gauge.

So two different claims had been wearing one sentence:

| claim | verdict |
|---|---|
| a **uniform rescale** of a whole row cannot reorder that kind's own ranking | **true** — a scale-free ranking is not reordered by a constant |
| changing the **level** preserves within-kind ranking | **false** — the constructor is not a uniform rescale |

The first is what the previous campaign's chapter says and it remains correct.
The second is what everyone read it as. For a kind carrying a shaped row the
level is load-bearing in all four of its consumers — within-kind cell ranking,
`per_species_capacity`, the packer's per-kind share, and the packer's
cell-capacity sum — and gauge in none of them.

**How it survived.** The short form names no transformation, so there is nothing
for a measurement to disagree with. It was inherited, restated, cited, and
carried across two campaigns and a ratified decision without ever being
expressible as a testable statement. The sentence that finally asserted it in
its strongest form was written into this campaign's *own* planning text as the
thing to record — a controller re-deriving the error while documenting it.

## Cost accepted

**Every gauge claim in the tree is now a longer sentence, and some are owed a
measurement they never had.** This record does not retroactively invalidate
them; it makes the short form inadmissible going forward and requires the three
parts wherever a gauge claim is used to justify leaving a quantity underived.
The affinity level's own claim is rewritten in full beside its derivation
(`domains/species/src/lib.rs::biome_affinity_registry`).

**A claim of this shape is not falsifiable by the commit gate.** No test can see
an unstated transformation, and the guard for this particular quantity was
itself blind — it held zero affinity rows and could not have reddened on any
affinity change whatsoever. What found it was a preregistered sweep and a
mandatory positive control on the repaired guard, which is expensive relative
to a lint and is the only thing that worked.

## What this generalises

The same shape recurs wherever a justification is true of one consumer of a
quantity and silent about the rest, which 0120 already named as an open class.
This record narrows the general warning into a checkable form: **the question to
ask a gauge claim is not "is this true?" but "under which transformation, and
which consumers were measured?"** A claim that cannot answer the second question
has not been checked; it has been repeated.

**See also.** [The Muster](../../book/src/chronicle/the-muster.md);
[the retrospective](../retrospectives/the-muster.md);
[The Radiation](../../book/src/chronicle/the-radiation.md);
`domains/species/src/lib.rs::biome_affinity_registry` (the four consumers, with
the evidence for each); `windows/worldgen/tests/beta_calibration_freeze.rs`
(the repaired guard, its positive control, and the arms that do not fire).
