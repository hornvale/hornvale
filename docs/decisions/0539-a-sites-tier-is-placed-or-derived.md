# 0539. A site's tier is `placed` or `derived`, and standing is a separate axis

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (ideonomy, 2 passes,
1 overturn) · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0038](0038-identity-computes-on-the-canonical-grid.md),
[0102](0102-one-per-cell-was-an-index-artifact.md)

In the context of The Prospect making sites enterable, and of the measured
finding that 40,962 level-6 vertices cannot populate 402,653,184 walk facets —
so a per-facet surface must be derived from noise interacting with macro
features rather than placed from vertices — we decided that **a `Site` carries
a `Tier` of `Placed` or `Derived`, and that whether a site participates in the
ledger is a SEPARATE, MUTABLE axis that this decision deliberately does not
name**, accepting that `Tier::Derived` is modelled and unused until a later
campaign builds the surface tier.

## Context

The campaign measured a ceiling nobody had computed:

```
  level-6 vertices     40,962
  walk facets         402,653,184   (6 x 4^13)
  max placed per facet 1 in 9,830  = 0.0102%
```

So the two kinds of world feature are not a matter of taste:

| | **Placed** | **Derived** |
| --- | --- | --- |
| generated from | a level-6 vertex, by a seeded draw | noise at facet resolution |
| count | bounded, ~10^2-10^4 planet-wide | unbounded, ~10^6+ |
| deterministic by | **record** — facts in the ledger, replayable | **derivation** — a pure function of (seed, position) |
| costs | a stream label, a save-format contract, storage | **nothing** |
| may shape world history | yes | no |

Nathan's framing: a placed delve is a Dwarven Kingdom that can be invaded
through a dimensional gate, whose mind flayers then emerge and devastate the
countryside — because its fate is a fact other systems read. A derived delve may
be every bit as large, as complex, as devastated, and leaves no mark on the
world's evolution.

## Decision

`Site` carries `Tier::{Placed, Derived}`. Everything The Prospect builds is
`Placed`; `Derived` is modelled now for the same reason `Extent::Region` is
(decision 0538) — so the surface tier is a fill-in rather than a migration of
every consumer.

## Why not name the criterion instead — the overturn

The first proposal was to name the *criterion* (participation in the record)
rather than the *mechanism*: `chronicled` versus `uncharted`. An ideonomy
inversion pass refuted it, and the refutation is the useful part.

**Promotion makes participation MUTABLE.** A derived site that a player enters
joins the record. So `chronicled`/`uncharted` names a **state**, not a **kind**,
while the generation tier is immutable. The proposal conflated two orthogonal
axes and proposed naming one with the other's vocabulary.

Implication-mining then found a third state the two-name scheme would have
hidden: **a promoted derived site is in the ledger but never shaped the world's
past.** It can shape the future and not the history. That is a real semantic
distinction, and it only appears once the axes are separated.

Symmetry-hunting found the asymmetry is free rather than enforced: the ledger is
append-only, so standing is **monotone** — a kingdom cannot be un-recorded. The
one-directionality costs nothing to guarantee.

## Why `placed`/`derived` specifically

The project already owns both words for exactly this distinction. `hornvale
locale --strange` prints *"103 **placed** exotic sites"*; `derived` appears in
377 source files as this codebase's word for computed-not-stored. The pair adds
no new vocabulary.

Two alternatives were disqualified on collision rather than taste:

- **`chronicled`** — `windows/chronicle` is the derived-history engine, 58
  files. Two chronicle-ish concepts in one system reads fine for a month.
- **`attested`** — partly spoken for by The Attestation campaign.

`uncharted` is genuinely free (0 occurrences) and is kept in reserve for the
standing axis.

## Consequences

**The standing axis is deliberately unnamed.** The Prospect does not implement
promotion, and naming an axis before building it burns a good word and invites
the mutable-state confusion the inversion just found. When promotion ships it
gets its own decision and its own record, as a **state field** rather than a
kind. Candidates then: `attested`, `recorded`, `reckoned`, `witnessed`,
`uncharted`.

**`Derived` costs no determinism contract.** A derived feature is a pure
function of `(seed, position)` — no stream label, no epoch, no bytes stored, and
stable across sessions. That is what makes "unbounded" affordable, and it is why
the surface tier can be built without touching any save-format contract.

**This is "coarse constrains fine" applied to a layer that does not exist yet.**
The Constitution already ratifies provider tiers coexisting, with higher
fidelity refining and never contradicting lower. Nothing here is new
architecture; it names the two tiers a surface layer will need and records why
the naming argument that seemed obvious was wrong.
