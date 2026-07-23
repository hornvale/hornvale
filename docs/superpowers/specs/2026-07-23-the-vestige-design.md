# The Vestige — Design

**Date:** 2026-07-23
**Status:** Approved (brainstorming session)
**Campaign:** The Vestige (slug-named per decision 0026) — campaign 3 of the
subsurface arc (MAP-10). *Working name; blessing requested at G3.*
**Provenance:** The Deep gave the world a column + deep-time archive; The Lode
gave it caves and ore. The Vestige gives the underworld its **history made
visible** — the sealed vaults, abandoned delvings, buried ruins, and ancient
gate-scars a wanderer finds: *something was sealed, delved, or breached here
long ago.* It is the next rung up the autonomy ladder (inert features →
emplaced-but-inert residue with latent hazard), and deliberately the **derived
historical-residue** slice of the event-features rung: residue is read
*backward* from the narrated past — no live mutation, no metaphysics
activation. Four ideonomy passes (2026-07-23) hardened it; their load-bearing
pulls are folded in throughout.

---

## 1. Goal

Give the underworld its **residue**: for a place, what past occupation left
behind — a sealed ward, an exhausted mine, a drowned sump, a buried undercity,
a dormant gate-scar — each carrying *who made it, when, in what state it has
decayed to, and what it now threatens.* The Lovecraftian "the dwarves delved
too deep, sealed it, and were forgotten" becomes a **derived reading of the
narrated past**, not a forward simulation.

Deliberately scoped:
- **Derived historical, not live.** Residue is a pure function of the seed via
  the narrated deep-time past + the settlement-abandonment history; nothing
  mutates during active sim. Provenance is **`derived-historical`** (a narrated
  event computed from the seed, exactly the rift-and-fit / archive discipline),
  not a live-ledgered event. **No new committed facts, no epoch** — the arc's
  standing discipline; `lens_purity` stays green.
- **No metaphysics.** A gate-scar is a *physical/historical wound* with a dread;
  *what came through* (demons/aboleths) and the thaumic energy stay behind the
  metaphysics gate (UNI-2) for their own campaign. Residue ships the door and
  its story, not the entity.

## 2. Architecture: the arc's first *integration* layer

The brainstorm's central structural finding (ideonomy pass 3, graph
hidden-hubs): **this campaign adds almost no new physics — it is a *join*** of
systems already shipped. A residue feature is a node whose every edge is an
existing layer:

- **made-by / declines-into** → the **settlement-abandonment history** (The
  Living Community: occupation records, ghost towns — who delved, when, and
  whether they were abandoned);
- **sits-in @ depth/era** → **The Deep's** column + deep-time archive;
- **clusters-at** → **The Lode's** ore/cave provinces (the *reasons* a people
  delved);
- **emits dread into** → **The Dread's** hazard field (PSY-11), which the
  danger drive already reads;
- **carries a warning that decays into myth** → **MEM / LANG-36**;
- **stacks-atop older residue** → the **palimpsest** (MAP-30).

So residue is **derived by joining** those layers — which is precisely why it
is cleanly derived-historical. **Placement:** a worldgen-derived layer (the
composition root, where the history + terrain + Lode features already meet); it
*reads* the ledger's occupation facts and terrain fields and **commits
nothing**.

## 3. The residue feature model

A residue feature is multi-faceted (ideonomy pass 3, decomposability):
- **kind** — from the taxonomy (§4).
- **maker** — a past settlement (people-made) or the deep itself (pre-human).
- **era** — its place on one of two clocks (pass 2, rate): people-residue on
  the **historical clock** (the settlement history's eras), pre-human residue
  on the **deep-time clock** (The Deep's archive). Older = deeper = more
  forgotten.
- **seal-state** — `maintained / lapsing / breached`, derived from whether its
  keeper-settlement is extant / declining / a long-abandoned ghost town.
- **valence** — **venerated ↔ forgotten** (ideonomy pass 3, negation + UNI-10):
  a residue whose keeper-culture *endured* is remembered and venerated (a holy
  tomb, a living ward); one whose keeper was *lost* decays to forgotten and
  dreaded (a breached vault, a cursed mine, UNI-6's haunting). This is the
  **same axis as seal-state** — `maintained` reads venerated, `breached` reads
  forgotten — and it rescues the layer from monotone horror: the underworld
  holds the holy *and* the accursed.
- **hazard-kind + dread level** (pass 4, cross-domain): **structural**
  (cave-in) · **toxic-gas** (firedamp) · **pestilent** (a sealed plague-pit) ·
  **flooded** (a drowned sump) · **numinous** (the gate-scar's thinness) ·
  **cursed** (cultural). Feeds The Dread's hazard field.
- **size** — point (a sealed door) → building (a vault) → **city (an undercity
  / necropolis — the Moria scale)** → cosmic (a gate-scar's elsewhere).
- **source** — `accident` (delved too deep → disturbed) / `design` (a
  deliberate ward or tomb) / `emergence` (nature sealed its own).

## 4. The taxonomy (the maker → purpose tree)

```
subsurface residue
├─ geological / pre-human  (deep-time clock, deepest, terrain/deep-time-derived)
│   gate-scar (dormant dimensional wound) · cataclysm-scar · natural collapse
│   · NATURAL seal  (the deep closed its own chamber — a lava-capped void, a drowned sump)
└─ people-made  (historical clock, shallower, settlement-history-derived)
    ├─ extractive ── abandoned delving / ghost-mine · tailings-spoil
    ├─ habitative ── buried ruin · undercity / necropolis
    └─ custodial ─── sealed vault · ward · deep tomb   (+ the rare MAINTAINED, still-warded seal)
```

Two empty-branch finds carried in (pass 2): a **natural seal** (no maker — the
earth itself contained something) and the rare **maintained seal** (a living
settlement still warding it — the not-yet-forgotten, the venerated pole).

## 5. The occupation-and-decay cycle + the palimpsest stack

Residue features are **frozen phases of one site life-cycle** (ideonomy pass 1,
cycle) — the organizing structure, not a flat list:

```
 Virgin → Delved → Worked → Disturbed → Sealed/Warded → Abandoned → Forgotten → (RE-DELVED atop the residue)
                              ("delved too deep")        keepers→ghost town        a new, ignorant people — the engine
```

Each residue *type* is that cycle stopped at a phase; the **seal-state is where
its keeper sits in the abandonment sub-loop**; and the closure — *re-delving
the forgotten* — is the Lovecraftian engine (a new mine cuts into an old sealed
thing whose warning is long gone). A site is therefore a **per-cell palimpsest
stack** (MAP-30, pass 1 modularity): dated layers — a modern ruin atop an
ancient vault atop a primordial gate-scar — The Deep's column model applied to
event-residue.

## 6. The three-rate decay and the forgotten warning

The mechanism, from the abstraction-lift to the **nuclear-waste long-term-
warning problem** (ideonomy pass 2): a residue is *a persistent disturbance
whose containment decays faster than it, and whose warning decays faster than
the containment.*

```
  the RESIDUE itself ....... slow / never   (the sealed thing persists)
  the CONTAINMENT .......... medium         (the ward weakens; the keeper lapses)
  the WARNING legibility ... fast           (inscription → taboo → ghost story → noise)
```

So a sealed feature carries a **warning whose legibility decays with age**, and
the culture that finds it **reads it through the epistemic filter (LANG-36) and
gets it wrong** — the forgotten interdiction, the Lovecraftian core, mechanised
as the memory economy (MEM) applied to a physical marker. **Age drives the
seal-state:** recent seals maintained/venerated, ancient seals
breached/forgotten — depth ≡ age ≡ forgottenness, the arc's spine.

## 7. Derivation

Residue **clusters** where the shipped layers imply it should (pass 4,
distribution): **old settlement sites** (the history's ghost towns) × **rich
ore/cave provinces** (The Lode's reasons-to-delve) × **the deep numinous zones**
(The Deep's Underneath, where gate-scars sit). A deterministic hash-noise point
process (The Lode's mechanism, a new `derive` label — zero sequential draws)
places sites within those clusters; the settlement-history read supplies
maker/era/seal-state; the two clocks supply age. Nothing is drawn sequentially,
nothing committed.

## 8. Determinism and the no-epoch guarantee

- Residue is a **pure function of the seed** via the narrated history + terrain;
  it **reads** the ledger's occupation facts (read-only) and **commits none**.
  World identity is untouched → **not an epoch**; `lens_purity` stays green.
- One new **hash-noise** `derive` label (site placement) — additive, zero
  sequential draws (The Lode's `FEATURES` precedent).
- **Narrated, never simulated forward** — the decay/seal-state is read backward
  from the history's abandonment record, not integrated (the Lorenz discipline).
- New derived observations (a residue map, an almanac section, census columns,
  a dread-field contribution) are drift-checked; census refreshed out-of-band.

## 9. Deliverables and consumer surface

The **atlas of five perspectives** (ideonomy pass 4) is the consumer surface —
the same residue read five ways:
1. **The residue query** on the worldgen surface: `residue_at(cell)` →
   the palimpsest stack of dated, typed, seal-stated facets.
2. **The Dread coupling** (physical/hazard): each residue facet **exposes** a
   hazard-kind × dread readout on the residue query. Feeding that as a static
   *source* into The Dread's hazard field (PSY-11) is the natural coupling —
   done in this campaign **only if it stays a derived, static contribution**
   (no active-sim entanglement); otherwise the readout is the hook and the
   wiring is a follow-on. *(Flagged for G3: wire-vs-expose.)*
3. **Almanac "The Vestige" section**: notable sealed wards, great abandoned
   delvings, undercities, and — the venerated pole — revered deep-tombs; the
   forgotten-vs-venerated split; prominent gate-scars.
4. **Lens**: a residue map (categorical by kind/valence; the clusters visible).
5. **Census metrics**: residue-site density, breached-vs-maintained (forgotten
   vs venerated) fraction, dominant hazard-kind, mean warning-legibility.
6. **Book (DoD)**: chronicle + freshness sweep; **MAP-10 re-scored** (campaign 3
   of the arc); a retrospective. Register the residue/valence concepts;
   **no new committed facts.**

*(economic/mythic/social/epistemic perspectives are the atlas's other pages —
salvage-motive-to-re-delve, the warning-as-myth, territorial claim, and the
finder's mis-read — surfaced where cheap, e.g. an almanac line, and otherwise
noted as hooks for the culture/economy consumers.)*

## 10. Non-goals (deliberate scope fence, with hooks)

- **Live event-mutation** — containment failing *during* active sim, a gate
  opening in real time, active delving. The next rung; residue's seal-state is
  the derived snapshot, the hook.
- **Metaphysics activation** — the *entity* behind a seal / *what floods through*
  a gate (demons, aboleths), and the thaumic energy. Gated on UNI-2; the
  `numinous` hazard-kind + the gate-scar are the reserved hooks.
- **The fine undercity graph** — depth-as-coordinate, traversable passages, the
  lightless venue vocabulary. MAP-10's deferred deep tier; the `city`-size
  undercity is the coarse hook.
- **Objects / relics / loot** — a *buried artifact* (a portable object) vs a
  *place*; the salvage economy. The economic atlas-page is the hook.
- **Surface residue** — surface ruins, monuments, battlefields (this is the
  subsurface arc); the tell spans the seam and is noted.
- **The full memory economy (MEM-1..7)** — the warning-legibility decay uses a
  coarse age-and-abandonment proxy, not the full surplus×will upkeep model; MEM
  is the refinement.

## 11. Ideonomy provenance

Four passes, no overturn — the core held (derived historical residue; the
occupation-decay cycle; the three-rate decay; the palimpsest stack). **Pass 1**
(dimension-id + substitution → cycle): the **occupation-and-decay cycle** as the
organizing structure (features = frozen phases; *re-delving the forgotten* = the
engine), residue as a **palimpsest stack** (modularity), **age drives
seal-state** (age), and the **hazard side-effect that outlives purpose**
(side-effect — the seal inverting protection→lure). **Pass 2** (tree +
abstraction-lift → procedure): the **nuclear-waste-warning** lift and its
**three-rate decay** (residue > containment > warning), the **maker→purpose
tree** with the **natural-seal** and **maintained-seal** finds, and the **two
clocks** (historical vs deep-time). **Pass 3** (organon + negation → graph): the
**integration-layer reframe** (residue joins four shipped systems) and the
**venerated↔forgotten valence** (UNI-10) — the two biggest finds — plus the
**size tiers** (incl. city-scale undercity). **Pass 4** (cross-domain +
combination → atlas): the **five-perspective atlas** (the consumer surface),
the **hazard kinds** (structural/toxic/pestilent/flooded/numinous/cursed, from
brownfield + plague-pit + firedamp), and **source-typing**
(accident/design/emergence). Considered and declined: live mutation (next
rung), the entity behind the seal (metaphysics gate), objects/relics (loot
layer), surface residue (subsurface arc), the full MEM upkeep model (refinement).
