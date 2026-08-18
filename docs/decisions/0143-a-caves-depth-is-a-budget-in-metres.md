# 0143. A cave's depth is a budget in metres, not a stratigraphic band

**Status:** Accepted (2026-08-18) · **Decider:** Nathan · **Relates:**
[0142](0142-the-underworld-carries-two-ladders.md),
[0137](0137-the-craton-clamp-is-a-budget-not-a-limit.md)

In the context of The Hollow having given a cave's *kind* its own fields while
leaving its deepening step reading the same `proneness` scalar the presence gate
reads, and The Underworld's first task having measured that the resulting depth
coordinate is effectively two-valued, we decided that **a cave carries a depth
reach in metres, derived independently of the stratigraphic band boundaries and
without reading the presence proneness**, because existence and depth are
different questions that had been asked of one number, and they want opposite
calibrations.

This resolves the registry row that had been open since The Hollow, whose own
**Where** cell said it was waiting for a consumer to supply the evidence for or
against the split. The evidence is the first table below.

## The weld, and what it cost

`Fracture` must peak above ~0.5 proneness or the deepest band never occurs, yet
any smooth field peaking that high plateaus over the 9.4% of land on a plate
contact and floods `Fracture`'s share. One field cannot be tuned for both.

Downstream, the depth coordinate inherited the stratigraphic ladder's shape.
Measured over seeds 42 / 7 / 1234, the five habitation classes held:

| | [0,2) K | [2,10) | [10,25) | [25,50) | [50,∞) |
|---|---|---|---|---|---|
| seed 42 | 655 | 6 | 4 | 1 | 208 |
| seed 7 | 998 | 24 | 16 | 4 | 639 |
| seed 1234 | 786 | 25 | 9 | 2 | 444 |

Two occupied classes out of five, with 1–25 caves in the middle three, and the
bottom class matching the `Roots` band count essentially one-for-one.

## What the budget is, and the three constraints on it

`cave_depth_reach_m(kind, buffer, column)` is a **pure derivation** — void
closure under lithostatic load, with rock strength interpolated across ISRM
field grades. It adds no draw and takes no stream label, so it cannot perturb
stream consumption order.

1. **It must not read the presence proneness.** That shared scalar *is* the
   weld; the signature takes none, so the weld cannot re-form by accident.
2. **It prefers a pure function of fields terrain already owns** — cave kind,
   lithology, relief — in the manner of `geothermal_gradient`. A draw would be
   admissible only if a pure derivation proved degenerate, and it did not.
3. **Earth anchors are a sanity ceiling, never a derivation.** Lava tubes form
   inside a flow and are shallow; karst follows dissolution to ~1–2.2 km
   (Veryovkina, 2212 m); fracture voids close under load within a few km.

Post-split, all five classes are occupied on every seed at a modal share of
42–44%, against a preregistered 70% ceiling that the pre-split world failed at
75%. The spread came from the world, not from a moved threshold.

## Two consequences that are not obvious

**`deepest_band` survives and inverts its dependency.** It is now *derived from*
the budget by comparing it against the column, so the archive keeps answering
"which bands does this void penetrate" while no longer being the depth
coordinate. Both `Cave` constructors derive the band from the budget through one
definition, so a disagreeing pair cannot be constructed; `Cave` is
`#[non_exhaustive]`, which forbids struct-literal construction outside the
defining crate while leaving every field read untouched.

**The budget has atoms, and a downstream consumer must expect them.** The
derivation clamps — lava tubes at 200 m, the general ceiling at 3 km, and a
paleokarst arm — so reach is a row of spikes rather than a spread: seed 42's
single fattest reach value holds 23.1% of its caves. Anything binning a quantity
derived from reach must place its edges in measured valleys and say so. The
habitation ladder's `Deeps` edge was moved for exactly this reason after a 1 K
re-bin showed the original edge sitting on a bin holding 39.9% of one seed.
