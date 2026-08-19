# 0156. A typology bundle is authored, not derived

**Status:** Accepted (2026-08-19) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md) (studies are data),
[0009](0009-models-author-dice-roll.md) (models author, dice roll)

In the context of giving each family its own **rules** for building words — a
`Typology` of morphology, onset law, coda law, harmony and orthography — we
decided that **a bundle is a named authored row, never a point in the
five-field product space**, because the product is 4 × 3 × 4 × 2 × 3 = **288**
combinations of which only a handful are coherent languages, and admitting the
cross-product would present 284 untried paths to the next campaign as though
they were supported.

## Context

The Burr introduced `family_typology()`: `sonorant-open` (the elves),
`templatic` (the dwarves), `isolating-tonal` (the dragons), and `concatenative`
(the control, and every unfamilied kind). Each is hand-authored, the same
discipline `family_proto()` already used and decision 0011 applies to studies.
A generic "pick a value per field" constructor would have been shorter to write
and impossible to read: most of the 288 cells are not languages, and nothing in
the type would have said which.

## Consequences

- Adding a family is authoring one row, reviewed as prose, not opening a
  combinatorial surface. `cli/tests` and the in-module anti-vacuity tests pin
  that only the authored rows exist.
- The module doc carries the 288 count explicitly so a reader knows the
  unexercised paths are a deliberate omission, not an oversight — a
  miscount of that product was itself a finding this campaign corrected.
- The cost is that a genuinely new combination requires a new authored row and
  its own review, never a config edit. Accepted: that review is the point.
