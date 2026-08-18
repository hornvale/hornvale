# 0142. The underworld carries two ladders, and neither derives the other

**Status:** Accepted (2026-08-18) · **Decider:** Nathan · **Relates:**
[0105](0105-water-keeps-bands-rock-becomes-a-graph.md),
[0143](0143-a-caves-depth-is-a-budget-in-metres.md)

In the context of The Delvers having withdrawn two subterranean dwarf kinds
because nothing could tell them apart by depth, and The Underworld's first task
having measured that the stratigraphic band ladder cannot answer a habitation
question at all, we decided that **the underworld carries two independent
ladders — `BandKind` for stratigraphy and `DelveRung` for habitation — and that
neither is derived from the other**, because the two ladders answer different
questions and their boundaries fall in different places for different reasons.

## The two questions

`BandKind` spaces its rungs by **rock-unit boundary**: `Cover` at the soil
depth, `Basement` at soil plus sediment, `Roots` at half the crustal thickness.
It answers *what is this rock and how old is it*, and it keeps that job intact —
`Era`, `RockClass`, `unconformity` — unmodified by this decision.

`DelveRung` spaces its rungs by **temperature offset above the cell's surface
datum**, so a rung's depth in metres varies by cell with the geothermal
gradient: the same rung sits roughly twice as deep under an ancient craton
(15 K/km) as under young thin crust (30 K/km). It answers *how deep am I and
what is it like here*.

`ChamberAddr.band` indexes the habitation ladder (`chamber/v2`). A `Chamber`
reports both its delve rung and the `BandKind` it sits in, and reading either
off the other is forbidden.

## Why the split is necessary and not merely tidy

A ladder whose rungs are rock-unit boundaries is **two-valued in practice**.
Measured over seeds 42 / 7 / 1234: soil plus sediment is ≈0 m almost everywhere
and the `Roots` top is ≈half the Moho depth, so the deepest band's top depth
takes essentially two values, and any monotone function of a two-valued input is
two-valued. Two of five habitation classes were occupied; the deepest class
matched the `Roots` band count one-for-one on every seed.

Re-spacing the rungs cannot repair that, and the campaign proved it by trying:
ΔT = gradient × depth, and the measured gradient spans **1.27×** (p10 21.795 →
p90 27.780 K/km) against a depth spread of ~10⁴×, so ΔT is depth rescaled by a
near-constant and inherits its shape exactly. **The second ladder is only worth
having because its input is independent** — which is decision 0143.

## What this costs

- **An epoch.** `chamber/v1` → `chamber/v2`, because the address space's meaning
  changes rather than merely its values. Every seeded golden and byte-identity
  fixture rebaselines, and one census refresh is paid.
- **Two ladders to keep consistent.** A cave's `deepest_band` is now *derived
  from* its depth budget by comparing that budget against the column, so the
  archive keeps answering "which bands does this void penetrate" correctly
  while no longer being the depth coordinate. That derivation is by construction
  in `Cave::new` and `Cave::from_reach`, not by convention — see
  [0143](0143-a-caves-depth-is-a-budget-in-metres.md) for the third
  constructor, `Cave::from_parts_unchecked`, which exists to build a
  disagreeing pair for exactly one test and which the *generator* never
  reaches.
- **Any future reader must ask which ladder it wants.** A rung name is not a
  rock unit and a rock unit is not a depth. The names were chosen to be
  unconfusable (`Undercroft`, `Shallows`, `Deeps`, `Underdeep`, `Sunless`
  against `Regolith`, `Cover`, `Basement`, `Roots`) for exactly this reason.

## What is authored, and says so

The habitable ceiling — the ΔT beyond which the campaign declares a chamber
uninhabitable, and therefore where the bottom rung begins — is an **authored
fidelity constant** at 50 K, frozen in the spec before any fit. A later re-bin
established it is the least well-placed edge in the table (1.5% of seed 1234
lies within **±0.5 K** of it, and seed 7 carries occupied 1 K bins at both 48
and 49 K) and it was **not moved**, because moving a frozen
authored value after unblinding is a retune. That cost is accepted as the price
of having authored it rather than leaving it as a silence.
