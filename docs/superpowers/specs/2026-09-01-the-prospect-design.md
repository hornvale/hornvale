# The Prospect — design

**Decision block:** 0536–0545 · **Drafted:** 2026-09-01 ·
**Decider:** Nathan (autopilot) · **Campaign:** `campaign/the-prospect`

A *prospect* is both the view of what lies ahead and a place worth
investigating. This campaign makes the walk band say what is here, lets you
walk into it, and widens what counts as a place from "somebody built it" to
"there is something here."

## 1. The problem, measured

Standing at seed 42's flagship, `look` says:

```
Tropical seasonal forest — buttressed canopy, sun-warmed, damp, on a rise —
in the lands of Doaba.
No direction here is closed; the nearest ground lies N, NE, E, SE, S, SW, W, NW.
```

`enter` works here. Nothing says so. And what you enter is *"A small room,
holding a doorway and a screen"* — anonymous. Doaba is named as a **region**
you are in, never as a **place** you could walk into.

Three tiles east — 3.4 km — the answer is
`Nothing here is built; there is nothing to enter.`

So the world has one door within walking distance of the start, it is
invisible, and the room behind it has no identity.

**The raw material for many more doors already exists and is explicitly
stranded.** `cli/src/main.rs`'s own doc on `--strange`:

> they are a rare minority of land by design, so the tier was generated but
> **unreachable**. This is the verification surface.

103 exotic sites on seed 42, each with a vertex, a biome and a reason it is
strange ("under a fungal canopy", "grown with mineral crystal, with biota found
nowhere else"), reachable only through a debugging flag.

## 2. What exists, surveyed

| thing | where | shape |
| --- | --- | --- |
| `Terrain::is_built(&Facet) -> bool` | `windows/vessel/src/brief.rs:53` | **already facet-resolution**, gates `structure_at` and all chamber generation |
| `cave_proneness(&MaterialBuffer, f64) -> f64` | `domains/terrain/src/lithology.rs:452` | **continuous, positional** — a pure function of two fields, no roster |
| `LocaleContext::strange_site_rows()` | `windows/locale` | 103 vertex-placed sites, unreachable in play |
| `strangeness` | per-room scalar in `[0,1]` | already in every locale's regime line |
| `Attach::{Hub, Beside(Ec), Within(Ntpp)}` | `windows/vessel/src/interior/pattern.rs:21` | an **RCC-8 pattern language**, scale-free |
| settlement roster | `clients/game/bin/src/plate.rs:106` | the **only** thing the map draws |

Two of these are better news than expected. `is_built` already takes a
`Facet`, so the widened predicate does not inherit the 120 km resolution defect
that `CLIM-water-label-resolution-vs-walk-band` records. And `cave_proneness`
is positional, so caves need no roster and no seeded draw to place.

## 3. `built` is the wrong name, and the rename carries meaning

`brief.rs:53` defines it as *"Whether a **structure stands here**."* That is
"made by hands", which is false for a cave (dissolved by water) and false for a
fungal canopy (grown). Widening `built` would put a lie in the predicate every
enterable place hangs off.

**Decision 0536 (proposed): the gate becomes a `Site`, and `built` becomes a
property of one kind of site.**

```rust
/// Something at a facet with an interior worth entering.
pub struct Site {
    pub kind: SiteKind,
    pub name: Option<Name>,
    pub extent: Extent,          // see §7
}

pub enum SiteKind { Settlement, Cave, Exotic }
```

`site_at(&Facet) -> Option<Site>` replaces `is_built` as the gate.
`Terrain::is_built` survives, unchanged, as what makes a `Settlement` a
settlement. The word "site" is already the project's own: `plate.rs` says "an
undiscovered site", the CLI says "placed exotic sites".

## 4. Surfacing, in prose

A locale with a site gains a clause naming it. Not an inventory — **one or two
salient things**, because the world is deliberately not completable and a facet
owes the player something that catches the eye rather than a manifest.

```
Tropical seasonal forest — buttressed canopy, sun-warmed, damp, on a rise —
in the lands of Doaba. You can enter the bugbear village of Doaba.
```

```
Limestone scarp — dry, wind-scoured, on a rise — in the lands of Doaba.
A cave mouth opens back into the hill.
```

Rules:

1. A facet with no site says **nothing**. Silence is honest and it makes the
   density gap visible rather than papered over. Expect most facets to be
   silent after this campaign; that is the input to the follow-on study.
2. At most **two** sites are named, ranked by salience (§6). The rest are
   enterable but unannounced — the fiction is a world that does not inventory
   itself.
3. The clause names the site's **kind and name**, never its contents.

## 5. Surfacing, on the map

`plate.rs`'s drawing path takes a settlement roster and nothing else, so caves
and exotic sites are invisible at every zoom. It gains a **site** roster,
of which settlements are one kind.

Existing behaviour that must survive: the discovery gate (an undiscovered site
is not drawn at all), and the by-rank major/minor split for settlements.
Non-settlement sites draw at a single weight in this campaign; ranking them
against population is out of scope.

## 6. Widening: caves and exotic sites become sites

**Caves.** Derived, no draw. A facet holds a cave where `cave_proneness`
clears a threshold and the local relief admits a mouth. The threshold is
**calibrated, not guessed** — preregistered in §9.

**Exotic sites.** The 103 already exist; they become `SiteKind::Exotic` and
gain an interior. Their strangeness descriptor is already authored ("under a
fungal canopy") and becomes the prose.

**Salience** ranks what gets named when a facet holds more than one:
settlement > exotic > cave, with `strangeness` breaking ties. This is a
presentation rule, not world-state.

## 7. Re-siting at walk resolution, and extent

Exotic sites are placed at level-6 vertices — the same ~120 km mesh that
produced the all-river defect. Reading "is there a site near me" off that mesh
would reproduce it exactly: every facet for tens of kilometres would claim the
same cave.

**Decision 0537 (proposed): a site is re-sited to a specific facet by a seeded
draw from its originating vertex.**

**CORRECTED 2026-09-01, after Task 5 measured it: this is NOT an epoch.** This
section originally read "a new stream label and therefore an epoch — every
world's site placement moves." That is wrong on the project's own rule, which
`domains/CLAUDE.md:34` states plainly: *"New label = safe; changed/reused label
= an epoch."* A new label consumes no draws from any existing stream, so nothing
that exists today re-derives — and site placement is new behaviour, so there is
no prior placement to move. Task 5 confirmed it: both pin-isolation suites stayed
green (astronomy 20/20, terrain 20/20) and `make rebaseline-goldens` was a no-op.

The campaign therefore costs no epoch at all. Everything below about a site
having a real address rather than being "somewhere within 120 km" stands
unchanged; only the price was overstated.

**Decision 0538 (proposed): `Site` carries an `Extent`, and this campaign only
ever emits `Extent::Point`.**

```rust
pub enum Extent {
    Point,                   // one facet — all this campaign emits
    Region { /* reserved */ },
}
```

Nathan's observation is that exotic sites are not uniform in scale — a
wasteland or a cursed land is miles across with components inside it. Modelling
extent now costs one enum and one match arm; **not** modelling it makes
multi-facet sites a migration of every consumer later. Since §7 mints an epoch
anyway, carving the shape out now is free.

## 8. Non-goals, stated so the boundary is legible

- **No landscape pattern grammar.** `Attach::{Hub, Beside, Within}` is RCC-8
  and scale-free, and lifting it from chamber interiors to landscapes is
  genuinely the right long-term model — but it deserves its own spec, because
  the interesting question is whether landscape patterns need relations a room
  never needs (`Surrounds`, `Overlooks`, `Downstream-of`). Filing that question
  is in scope; answering it is not.
- **No sub-facet terrain band.** 262,000 rooms of derived grass is oatmeal at
  higher resolution. Sites are discrete and sampled, not a continuous fine map.
- **No content authoring.** This campaign makes doors visible and openable. It
  does not decide what is behind them beyond what the chamber generator already
  produces.
- **No fix for `CLIM-water-label-resolution-vs-walk-band`.** Related and
  separately filed.

## 9. Preregistered measurements (decision 0016)

Frozen before the code that would move them.

**H1 — surfacing does not change what is enterable.** After §4 and §5, the set
of facets where `enter` succeeds is **identical** to before, for a fixed seed.
Surfacing is presentation. *Falsified if any facet changes enterability.*

**H2 — the cave threshold yields a rate in a preregistered band.** Over 2,000
land facets on 5 seeds, the fraction holding a cave is **1%–8%**. Below 1% the
widening buys nothing; above 8% caves stop being remarkable. *A result outside
the band is a finding, and the threshold is re-derived once, in the open.*

**H3 — the density baseline, which is the campaign's real output.** Over the
same sample, the fraction of land facets holding **at least one** site of any
kind. **No predicted value** — this is the number the follow-on campaign needs
and nobody has it today. Recording it is the success criterion, whatever it is.

## 10. Risks

| risk | mitigation |
| --- | --- |
| The epoch moves every world | Deliberate and Nathan-approved; worlds regenerate from seed. Byte-identity tests will move and must be re-baselined **as a stated act**, not quietly. |
| `site_at` is called per-facet per-render and could be hot | Measure before optimising. `is_built` is already on that path, so the shape is not new. |
| The rename touches many call sites | It is mechanical and the compiler finds them all — `is_built` survives, so no behaviour is silently reinterpreted. |
| H3 comes back at 2% and the vision looks far off | That is the point of measuring it. A low number aims the next campaign at generation rather than rendering. |

## 11. Success criteria

1. At seed 42's flagship, `look` names Doaba as a place you can enter.
2. Entering it says Doaba, not "a small room."
3. A cave and an exotic site are each reachable in play, named in prose, and
   drawn on the map.
4. H1 holds; H2 and H3 are measured and recorded whatever they say.
5. `main` stays green, and the epoch's artifact churn is declared, not absorbed.
