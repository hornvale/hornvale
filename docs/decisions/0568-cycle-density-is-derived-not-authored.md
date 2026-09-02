# 0568. Cycle density is derived from rock and workmanship, never authored

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) ·
**Relates:** [0016](0016-studies-preregister-hypotheses.md) (the ordering below
was frozen before the code existed),
[0009](0009-models-author-dice-roll.md) (rules are authored; instances are
drawn), [0566](0566-a-place-is-a-graph-before-it-is-a-map.md) ·
[The Crosscut](../../book/src/chronicle/the-crosscut.md)

In the context of choosing how many cycles a level of the underworld should
carry, facing the easy answer of a tuning constant labelled "fun", we decided
that **the target derives from what the place is — the rock it formed in and
whether anyone worked it — and that the one authored number in the grammar is
Dormans' legibility clip `[1, 5]`, named as his** — accepting a preregistered
ordering that the readout could have falsified, and accepting that the readout
we then took measured the budget function more than it measured the world.

## Context

The base comes from the rock, because the rock is what made the routes: karst
is a maze because water dissolves many ways through it, a lava tube is a single
conduit, a fracture system sits between. Lava tube 1, fracture 2, karst 3. The
increment comes from workmanship, because a worked place must ventilate — a
mine drives a crosscut so that intake and return air can circulate, and a
dead-end drift is the unventilated one. A drow tier or a made chamber adds one.
Neither number is a knob; each is a claim about the place, and each is wrong in
a way somebody could argue with.

The clip is Dormans': "two to five cycles give each level a distinct and
recognizable shape … adding more cycles just seems to clutter". It is authored,
it is cited, and it is the only authored constant in the grammar. The lower
bound is 1 rather than 2 because a level must always have at least one loop of
its own — the capability invariant of decision 0566 — while the upper bound is
his.

**The ordering was frozen before the code, and it held.** Prediction: panel
medians strictly ordered lava tube < fracture < karst, and drow tier > wild
cave within each kind carrying both. A tie or an inversion anywhere would have
been FALSIFIED. Measured over seeds 42, 7 and 1234, every cave-bearing vertex
of each (874, 1681 and 1266 descents):

```text
                 WildCave   DrowTier
  LavaTube          1          2
  Fracture          2          3
  Karst             3          4
```

Identical on all three seeds. **PASSED**, on both halves.

## Consequence

- **The measurement is weaker than the verdict word suggests, and this record
  says so rather than banking the pass.** The medians equal the budget function
  exactly, at every cell of that table, on every seed — because the grammar
  reaches its target on essentially every level. So the readout confirms the
  budget is *derived and reached*; it does not independently witness that the
  world varies the way the rule says. The derivation is real; the measurement
  of it collapsed onto the parameter it was meant to test. A future readout
  wanting more would have to vary something the budget does not read.
- The `[1, 5]` clip stays a **guard**, asserted rather than predicted: every
  level's realm count is in range, checked, not hoped for.
- The `Made` origin is in the rule and absent from the readout: no reachable
  world carries one until The Plat writes chamber overrides, so only the
  character half of the workmanship term could be measured here.
- When residents exist, revisit whether density should also read population —
  a city breathes more than a lair — rather than treating rock and workmanship
  as the complete set of terms.

## See also

- [The Crosscut design](../superpowers/specs/2026-09-01-the-crosscut-design.md)
  §3.2 step 5, §4.2.
- [The circuits of seeds 42, 7 and 1234](../audits/underworld-circuit-seed-panel.md)
  — the committed witness page these numbers are read from.
- [The Crosscut chronicle](../../book/src/chronicle/the-crosscut.md).
