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
| `cave_proneness(&MaterialBuffer, f64) -> f64` | `domains/terrain/src/lithology.rs:452` | a pure function — but its INPUTS are Vertex-bound (`provider.rs:319`, `:371`), so proneness exists only at 110-132 km. See §6's correction. |
| `LocaleContext::strange_site_rows()` | `windows/locale` | 103 vertex-placed sites, unreachable in play |
| `strangeness` | per-room scalar in `[0,1]` | already in every locale's regime line |
| `Attach::{Hub, Beside(Ec), Within(Ntpp)}` | `windows/vessel/src/interior/pattern.rs:21` | an **RCC-8 pattern language**, scale-free |
| settlement roster | `clients/game/bin/src/plate.rs:106` | the **only** thing the map draws |

`is_built` already takes a `Facet`, so the widened predicate does not inherit
the 120 km resolution defect that `CLIM-water-label-resolution-vs-walk-band`
records — though its real implementation turns out to be a membership test over
a precomputed set of settlement-territory facet ids (`liveness.rs:745`), not a
field read.

**The second claim in this paragraph was wrong and is corrected in §6.** It read
"`cave_proneness` is positional, so caves need no roster and no seeded draw to
place." That is true of the FUNCTION and false of its DATA.

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
2. **CORRECTED 2026-09-02: exactly one, not two.** This read "at most **two**
   sites are named, ranked by salience (§6)", and two was never built.
   `Brief.site` is an `Option<Site>` — the type carries one — resolved by
   `max_by_key(Site::salience)`, and `site_clause` renders one sentence. Naming
   two would need a `Vec` and a conjunction in the prose, neither of which
   exists.

   The number was wrong and the *rationale* was right, which is why this is a
   correction rather than a defect: "the fiction is a world that does not
   inventory itself" argues for one more strongly than for two. The rest are
   enterable but unannounced.
3. The clause names the site's **kind and name**, never its contents.

## 5. Surfacing, on the map

`plate.rs`'s drawing path takes a settlement roster and nothing else, so caves
and exotic sites are invisible at every zoom. It gains a **site** roster,
of which settlements are one kind.

**CORRECTED 2026-09-02 — Nathan reversed the discovery gate for placed
sites.** This read: "Existing behaviour that must survive: the discovery gate
(an undiscovered site is not drawn at all)". It no longer must survive, and
saying it does is the opposite of the shipped rule — see decision 0540.

A placed site's GLYPH is drawn whether or not it is discovered; its PROPER NAME
is still withheld until discovery. Nathan's framing: *"show the kind glyph and
withhold the name. We can say it's a cave, a village, etc, just don't give its
name."* The by-rank major/minor split for settlements does survive, unchanged.
Non-settlement sites draw at a single weight in this campaign; ranking them
against population is out of scope.

## 6. Widening: caves and exotic sites become sites

**Caves. CORRECTED 2026-09-01 — they need the placement draw after all.** This
section read "Derived, no draw", reasoning from `cave_proneness` being a pure
function. Its inputs are Vertex-bound (`material_at`, `cave_proneness_at`), so
proneness exists only at 110-132 km spacing, and thresholding the nearest vertex
would make every facet for tens of kilometres a cave — reproducing
`CLIM-water-label-resolution-vs-walk-band` exactly.

So a cave is PLACED, through §7's mechanism, with a distinct reason so a cave
and an exotic site at one vertex do not collocate.

**CORRECTED AGAIN 2026-09-02: there is no threshold, because there was already a
cave model and I did not look for it.** This paragraph said "the threshold on
proneness still decides WHETHER a vertex warrants a cave, and is calibrated, not
guessed." Both halves are void. `GeneratedTerrain::cave_at`
(`domains/terrain/src/provider.rs:413`) already answers whether a cave exists,
and answers it far better than a proneness threshold: it refuses ocean, runs
`cave_process` over material, drainage, **crust age** and **plate-boundary
distance**, weights by a tectonic belt term, and gates the result against a
**noise field** so caves cluster coherently.

Measured: `cave_at` yields 874/1647/1681/1116/2440 caves on seeds
42/13/7/1/100. The invented threshold yielded 1564/970/1638/1355/2772 — it
disagreed in BOTH directions, which is a better argument for deleting it than
any comparison of magnitudes. The correction is a net −72 lines, with
`features.rs` down 129 by pure deletion.

**A cave is still PLACED**, because `cave_at` answers per-VERTEX and a vertex
spans 110-132 km. Existence comes from the model; the address comes from §7.

The error is worth keeping: I checked that the function was pure and inferred
its answer was available anywhere, without checking where its inputs live.

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
multi-facet sites a migration of every consumer later.

The original justification here was "since §7 mints an epoch anyway, carving the
shape out now is free." §7 mints no epoch, so that argument is withdrawn — but
the conclusion stands on its own and is cheaper than it looked: one enum and one
match arm against a migration of every consumer, with no determinism cost at
all.

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
