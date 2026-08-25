# 0259. Conceptual deficiency is derived from authored psychology, never authored per people

**Status:** Accepted (2026-08-25) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0021](0021-no-alignment-axis.md),
[0256](0256-a-hosts-testimony-is-fallible-by-construction.md),
[0260](0260-a-seed-invariant-derivation-is-a-window.md) (where the measurement
lives);
[The Confidant](../../book/src/chronicle/the-confidant.md)

In the context of needing every culture to differ in which feelings it has
words for, we decided that **which felt states a people can name is derived
from `MindVector` — attributes authored for other reasons, before this
question existed — and is never authored per people**, accepting a mapping
whose ceiling nobody chose in exchange for a distribution nobody chose either.

## Context

The tempting implementation is a table: for each of the fifteen peoples, list
the feelings it lacks a word for. It is one file, it is legible, and it is
exactly what makes the instrument worthless. If we author the deficiency
distribution and then measure the deficiency distribution, the measurement
recovers the constant we typed in. This project already names that failure —
auditing the generator — and the campaign's own spec forbids any study whose
conclusion follows from the authored mechanism.

Deriving it forces the deficiency to be **emergent with respect to the
question being asked**. `MindVector` carries three `[0, 1]` scalars with a
meaningful midpoint, and the felt-state pack carries three valence-opposed
pairs. One scalar governs one pair by which side of the midpoint a species
falls: a species that meets a blockage by standing gets `frustrated`, one that
flees gets `lost`; a slow deliberator gets `content`, a fast one gets `eager`; a
generational planner gets `helpless`, an immediate opportunist gets
`searching`. Nobody chose the resulting distribution, because nobody was
thinking about feeling-words when those three numbers were authored.

## Consequences

- **The distribution is genuinely non-degenerate and was checked as such**, by
  forcing the psyche lookup to `None` and watching it collapse. Goblin holds
  zero of six (authored exactly at the midpoint on all three axes — a real
  reading, not an omission); snow-elf one; hobgoblin and human two; the other
  eleven three. Per-concept rates run 13.3% to 73.3%.
- **The mapping's ceiling is 50%, and it is an artifact we must keep declaring.**
  Awarding at most one pole per pair means no people can ever hold more than
  three of six, and eleven of fifteen sit exactly at that ceiling — so the mode
  *is* the bound. A reader meeting "no culture names more than half its own
  feelings" would take it for a finding about impoverished minds. It is a
  property of a three-scalar mapping, real tongues do have words for both
  frustration and hopelessness, and the published artifact says so in its own
  header rather than leaving the correction to prose elsewhere.
- **Findings are sought one step downstream.** The mechanism makes deficiency
  visible; it validates nothing by itself. Whether conflation predicts
  coordination failure between creatures, or how deficiency correlates with
  anything the mapping does not mention, are admissible questions. "Does absence
  of a word predict a misreport" is not — that is the mechanism restated.
- **Reporting the distribution, not a summary, is part of the decision.** A
  degenerate mapping is then visible as a degenerate distribution rather than
  hidden inside a mean.
- **What we give up:** we cannot hand-place a specific expressive gap in a
  specific people for narrative reasons. Any such authorship would have to
  enter through the attributes, where it is subject to everything else those
  attributes already drive.

## See also

Spec §3.4 and §5.1; the derivation lives in `windows/worldgen`'s exposure rule;
the published distribution is `docs/audits/the-confidant-report.md`.
