# 0966. The underworld budget is symmetric and its allocation is not

**Status:** Accepted (2026-09-11) · **Decider:** Nathan · **Campaign:** The Ceiling

## Context

The Winze's amendment C.3 asked what bounds a spreading horror, and argued the
bound must be **symmetric** with the bound on a spreading ecology:

> Flip the polarity of "a spreading horror" and the positive analogue is a
> spreading *ecology* — the lush underworld the same row asks for. Both eat the
> same rock. One budget with two kinds of consumer is a mechanism; a
> special-case cap on monsters is a knob wearing a mechanism's clothes.

It then named its own cost: a world whose rock feeds a balrog far is the same
world whose chemotrophic ecology is rich, so **"barren and deadly" and "lush
and safe" both become unreachable world-types** — and Nathan's constraint for
this line of work is the opposite ("not every world may become *DOOM*", the
Carpenter Apocalypse-Trilogy shape: three world-ending scenarios that differ
from each other).

The underworld-larder metaplan carried the question into rung 3 and recorded
its status exactly: "**Nathan has not ruled.**" It stayed unruled for
eighteen days, and it blocked rung 3, which blocks rung 4.

## The rule

**The budget is symmetric. The allocation is not.**

One budget with two kinds of consumer — C.3's mechanism claim is adopted
unchanged. What C.3 did not separate, and this decision does: **which consumer
the budget feeds is decided by the mix of energy sources present, not by how
much energy arrives.**

C.3's cost follows from an assumption it never states — that allocation tracks
magnitude. Severing the two restores both world-types on a second axis:

```text
                 |  ecology wins allocation  |  horror wins allocation
  ---------------+---------------------------+------------------------
  high budget    |  lush and safe            |  lush and deadly
  low budget     |  barren and safe          |  barren and deadly
```

All four quadrants are reachable, and none is authored — both coordinates are
derived from the same seven-source field.

## Why this is available now and was not on 2026-08-24

C.3 predates The Sources, whose measurements bear on it directly and point in
**opposite directions for the two halves** — which is what makes the split
legitimate rather than a convenient reading:

- **Magnitude does not separate worlds.** `subterranean_energy`'s
  between-worlds `separation` is `0.145249` against a preregistered `>= 0.25`
  (falsified, n=12), and no rung's within-world p10-p90 width clears one full
  `ENERGY` band. Twelve worlds do not separate as distinct kinds of world by
  the energy scalar.
- **Composition does vary.** The dominant-source histogram occupies **all
  seven** sources, the largest taking 38.4%, and "more than one source
  dominates at every rung" (S3). Re-run on this campaign's base
  `26003913d`, pooled over seeds 42/7/1234, 19,105 chambers:
  `[720, 7338, 3025, 5013, 2639, 57, 313]` in `EnergySource::ALL` order.

The metaplan states the same conclusion as rung 3's inherited diagnosis —
"variety survives in *composition* — which mechanism dominates, not how much
arrives" — and this decision is that diagnosis promoted from a design warning
into the allocation rule itself.

## What is NOT decided here, and must not be read into it

**Whether composition separates the world-types is UNMEASURED.** That
composition *varies* (measured) is not that it *separates worlds into
distinguishable kinds* (not measured). The Sources measured the between-worlds
statistic on the **magnitude** scalar only; no between-worlds statistic exists
for composition at all.

This decision fixes the **shape** of the answer. The Ceiling's Stage 1
preregisters and measures whether the shape is inhabitable, and **a null sends
this decision back to C.3's binary** — at which point it is superseded with
evidence rather than re-argued. Recording the rule before the measurement is
deliberate and is the reverse of retuning to rescue a prediction (decision
0016): the prediction is frozen here, in advance, with its falsifier named.

A second open question rides with it: `subterranean_energy` is the **mean of
seven** terms, which compresses by construction, so The Sources'
magnitude-compression finding has two candidate causes — the rock (its stated
one) and the combination rule — that nothing has separated. If the combination
rule is a major cause, composition is more available than the metaplan's
inherited diagnosis implies.

## Consequences

- **A future campaign must not restore a special-case cap on a horror's
  range.** That is the specific act C.3 argued against and this record
  ratifies: one budget, two consumers, no monster knob.
- **Nor may it re-couple allocation to magnitude** without reopening this
  decision. "Richer rock ⇒ the horror ranges further" is precisely the
  coupling severed here.
- **Both coordinates must be derived, never authored.** A per-world constant
  on either axis reintroduces C.3's knob on the other axis.
- **This decision is falsifiable and expires by measurement, not by age.** If
  The Ceiling's Stage 1 reports that composition does not separate the
  world-types, this record is superseded by one recording that result and
  choosing between C.3's original two. Cite the measurement, not this record,
  for any claim that the quadrants are reachable.
- **The Tidemark is unaffected.** It holds the marine half of rung 4 and its
  spec §7 states the C.3 question is untouched by it, which remains true under
  this ruling.
