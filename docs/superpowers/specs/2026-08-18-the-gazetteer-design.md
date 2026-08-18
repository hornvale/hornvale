# The Gazetteer — design

**Status:** DRAFT — awaiting G3 review. **Campaign:** The Gazetteer.
**Branch:** `campaign/the-gazetteer`.

The world learns the names of its own landscape. Rivers, landmasses, seas
and salt lakes become individuated objects with stable identities, per-culture
names, extents and a total ordering by magnitude — so that a later campaign can
draw a map whose *names* are the fogged layer and whose terrain is the given.

---

## 0. Provenance, and the null this campaign must not inherit

**The Watershed (2026-07-29) designed items 2 and 3 of this campaign in full,
measured its own prediction, and shipped a null instead.** Its §3 Item 2
(individuate landmasses) and Item 3 (individuate rivers by their mouths) are
reused here substantially unchanged, including their identity schemes, their
size floors and their API shapes. That work was good and it was reviewed; this
campaign is its continuation, not its repetition.

**What was falsified was the justification, not the design.** The Watershed
sought landscape names as a cure for *settlement-name collisions*: it predicted
below 15% and measured a floor of **44.8%** across an eight-seed,
1837-settlement battery, before writing the code. The reason is structural and
is not going to change — a settlement's landscape is shared by construction, so
the discriminator's effective cardinality is a tenth of its nominal
cardinality.

**That null does not bind this campaign, and the distinction is the single
most important sentence in this document.** This campaign's payoff is a
toponymic *knowledge* layer: a name is a thing a character can learn, lack, or
be told, and the eventual map fogs names rather than terrain. Name-collision
rate is not this campaign's criterion, is not among its preregistered
hypotheses (§8), and would not be improved by satisfying them. Decision 0024
already ratified that uniqueness is a reference-time property and that no
future work fixes the collision rate by adding entropy; nothing here reopens
that.

This paragraph exists because the failure mode is documented and recurrent: a
reader who greps "landmass naming" finds a preregistered prediction, a
falsification, and a shipped null, and the cheapest available inference is that
the area is closed. It is closed *for that criterion only*.

---

## 1. What this campaign produces

- **`domains/terrain`: a feature index.** Four classes of individuated region —
  landmass, sea, salt lake, river — each with a draw-free stable identity, an
  extent, an anchor cell and an integer magnitude. Computed once per terrain
  and held on `GeneratedTerrain`, the discipline `ChannelNetwork` already uses.
- **`windows/worldgen`: per-culture names for them**, through the existing
  `NameKind::Landform` leg, keyed on feature identity and species — so one
  river has as many names as there are peoples with a word for it, and none of
  them is *the* name.
- **A total, deterministic ordering by magnitude within each class**, which is
  the placement channel a later map's label gate will read.
- **A committed gallery artifact** (`book/src/gallery/gazetteer-seed-42.md`)
  listing seed 42's named landscape per culture, drift-checked like every other.
- **`explain` and the almanac naming places** — the payoff that lands without
  any map existing.

### What it does not produce

No map. No fog. No client work. Those are the two follow-on campaigns (§9).

---

## 2. The gap, read from the code

The identification machinery is almost entirely present and is discarded at
the last step.

- **`terrain::shape::land_component_sizes`** (`domains/terrain/src/shape.rs:156`)
  already walks exactly the connected components this campaign needs — a BFS
  over `Geosphere::neighbors` gated on `elevation >= sea_level` — and then
  returns `Vec<usize>`, throwing away every component's cell set and any
  identity. The gap is precisely that return type.
- **`drainage::downhill_targets`** (`domains/terrain/src/drainage.rs:24`) gives
  every land cell a downhill pointer, which *is* a flow forest. A river is a
  maximal subtree of it; nothing reads it that way.
- **`WaterKind`** (`domains/terrain/src/water.rs:11`) classifies cells as
  `Ocean` / `SaltBasin` / `River` / `DryLand`. A sea and a salt lake are
  connected components of the first two. No code takes those components.
- **`NameKind::Landform`** (`domains/language/src/naming.rs:110`) already names
  a landform per culture, keyed on identity rather than cell — "a landform has
  ONE identity and MANY names". Its **only caller is `volcano_name`**
  (`windows/worldgen/src/volcano.rs:192`).

And the shape of the absence, stated exactly: `"the Great Delta"` on the
gallery's elevation page is a hardcoded English string literal at
`windows/worldgen/src/lib.rs:3941`. The world cannot say what a river is called
because nothing makes a river a thing that could have a name.

---

## 3. The model — a feature is an individuated region

A **feature** carries five things, and every one is derived from already
committed terrain with **zero stream draws**:

```
identity   a stable FeatureId, canonical and integer
class      Landmass | Sea | SaltLake | River
extent     the cell set the feature occupies
anchor     the single cell a label is drawn at
magnitude  the integer scalar that ranks it within its class
```

### 3.1 Identity is the lowest cell id, and its fragility is declared

| class | component | identity | magnitude |
|---|---|---|---|
| Landmass | land cells under `Geosphere::neighbors` | lowest `CellId` in it | cell count |
| Sea | `WaterKind::Ocean` cells | lowest `CellId` in it | cell count |
| SaltLake | `WaterKind::SaltBasin` cells | lowest `CellId` in it | cell count |
| River | maximal subtree of the flow forest | its **terminal** cell (sea mouth or interior sink) | catchment size |

Lowest-cell-id is canonical, integer, order-independent and requires no
tie-break. It is also **not stable under anything that moves a coastline**: a
sea-level change can renumber every landmass, and their names move with them.

That is acceptable and must be stated rather than discovered, for one reason
that makes it safe: **names are derived, never committed.** A world's landscape
names are a pure function of `(seed, feature identity, species)`, re-derived on
read exactly as `volcano_name` is. So a terrain change that renumbers features
is already an epoch by other means, and no saved world carries a name that
could contradict its own terrain. A *session's* knowledge of a name (the
follow-on fog campaign) is keyed on `FeatureId` and therefore inherits this
property; that campaign must declare it, and this paragraph is the notice.

### 3.2 The naming tier is a floor, and unnamed is a legitimate answer

Only components at or above a floor are named; the rest stay anonymous. A
settlement on an unnamed rock simply has no landmass concept, exactly as a
settlement away from water has no hydrology concept. The Watershed's measured
floors are the starting point, **not the shipped values** — §7 F1 settles them
against the current tree.

```
LANDMASS_MIN_CELLS = 20      -> 14 named landmasses (Watershed, seed 42)
river catchment    >= 24     -> 115 named rivers    (Watershed, seed 42)
```

### 3.3 The salt must be injective over (class, identity) — a collision by construction

`volcano_name` salts on a bare cell id (`u64::from(volcano.source.0)`). If each
class salted on its own bare cell id, then **a landmass whose lowest cell equals
a river's terminal cell receives the identical name from the same people**, and
so would a sea and a salt lake sharing a boundary cell id. This is a collision
by construction, not by chance: it is guaranteed for any world where the ids
coincide, and it is silent — two features simply have the same name, which is a
thing real toponymy does, so nothing looks wrong.

**Requirement:** the salt passed to `NameKind::Landform` is injective over
`(class, identity)`. The implementation is the implementer's to choose after
reading the draw site; what this spec fixes is the property and the test that
holds it.

**Requirement:** the chosen salt must leave `volcano_name`'s existing draw
unmoved, or volcano names in every committed world change. Whether volcanoes
join the same injective scheme or keep their present salt is an implementation
call with one binding constraint: **a moved volcano name is a STOP** (§7 F3's
decision table).

### 3.4 Ordering is total, deterministic, and is the placement channel

Within a class, features order by magnitude descending, ties broken by
identity ascending — integer-only, so cross-platform byte-identical, and total
by construction.

This ordering is the **placement channel**, and it is `box_rank`'s job one
rung up: `surrounds_ascii::box_rank` already decides which of two cells
contesting one character box keeps it, on salience with a document-order
tie-break. A map's label gate is the same question at planetary scale. Note
what this deliberately is *not*: under decision 0142 a rendering channel
carries one measurement axis, and placement is not among the three (nominal /
ordinal / epistemic). Ordering a feature set is not a claim about the world;
it is a claim about what fits on the page.

**This campaign ships the magnitude and the ordering. It does not ship a
salience banding**, because a label-density calibration with no map to
calibrate against would be a number invented to look decided. The follow-on
map campaign sets the bands against a rendered picture.

---

## 4. Naming, and what it must not move

Names go through the existing leg, unchanged:

```
language/<species>/name/landform
```

**No new stream label. No epoch.** Decision 0083 declares a label per
*algorithm*; The Watershed's own §7 flag 3 already applied it here and found
`river/name/v1` and `landmass/name/v1` to be **phantom labels — same algorithm,
different subject**. Decision 0084 adds that an epoch is declared only when a
derivation *moved*, and none does: `NameKind::Landform` draws off its own seed
path precisely so that adding landform naming reseeds nothing that exists.

`domains/language/src/naming.rs:2318` already carries the property that pins
this — a landform draw must move no existing kind's draw for any
`(seed, species, salt)`. That test is the positive control for §8 H3 and must
be extended to the new callers rather than duplicated.

---

## 5. Refusals

- **No new clustering algorithm.** The scope line is exact: every class that
  falls out of a graph traversal over already-committed fields ships; every
  class needing a new clustering pass waits. Mountain ranges, bays, capes,
  straits, peninsulas and biome regions are all on the far side of that line
  (§9). Volcanoes are already individuated and already named — this campaign
  reuses them and mints nothing for them.
- **No borrowing between cultures.** The Watershed decided borrowing ships
  (its §7 flag 2), selecting the Steeped people with the most settlements on
  the river. It "buys realism, not the criterion" — and it is a *fog* concern:
  which people's name you learn is the follow-on campaign's question. Deferred
  with its decision intact.
- **No commitment of names to the ledger.** Names are derived on read (§3.1).
- **No renaming of `"the Great Delta"`.** The hardcoded literal at
  `lib.rs:3941` is a *notable*, a different surface from a toponym. Replacing
  it is a visible-prose change to a committed artifact and belongs to whichever
  campaign owns that page.

---

## 6. Layering

`domains/terrain` gains the feature index and depends on nothing new — it
already owns `Geosphere`, `WaterKind` and `drainage`. Naming lives in
`windows/worldgen`, the composition root, because it joins terrain (the
feature) to language (the phonology), and a `domains/terrain` →
`domains/language` edge would be a sibling dependency the architecture test
forbids. This is the same placement `volcano_name` already occupies, and the
same reason LOC-toponymy's vocabulary had to leave `locale`.

---

## 7. What is unverified, and how each is settled

**F1 — the seed-42 counts are contradictory in committed prose.** The
chronicle says **123** named rivers; the Watershed spec says **115** at the
naming tier and **14** landmasses. At most one river figure is right and
neither is evidence. Both predate a year of terrain work, and The Grain is
moving water banding *right now*. **Settled by:** Task 1 measures landmass,
sea, salt-lake and river counts on the current tree and reports them. Every
floor in §3.2 is chosen against Task 1's output, not against this table.

**F2 — does refactoring `land_component_sizes` move its output?** It sorts
sizes descending and is read by the `landmass-count` and `continent-count`
census metrics. **Settled by:** the refactor keeps the existing function as a
thin caller of the new component-yielding one, and its output is asserted
byte-identical before and after — a positive control separating "the components
are right" from "the metric is undisturbed". If the two cannot be reconciled,
that is a finding, not a workaround.

**F3 — does naming move any committed artifact?** This is the additivity
claim and the one that can fail loudly. **Settled by:** `make rebaseline`
followed by the drift check, read against this decision table rather than
against a prediction:

| what moved | reading | action |
|---|---|---|
| `docs/audits/type-audit-report.md` only | expected — the campaign adds `pub` items | regenerate and commit in the **same** commit |
| `docs/digest/` | expected — a decision record lands | same commit |
| `book/src/gallery/gazetteer-seed-42.md` | expected — it is this campaign's new artifact | `git add` it in the commit that introduces it (a drift check against an untracked path is silently vacuous) |
| any **other** `book/src/gallery/` page | **STOP** — a rendered world moved, so naming consumed a draw | do not rebaseline; diagnose |
| any volcano name anywhere | **STOP** — §3.3's salt constraint was violated | do not rebaseline; diagnose |
| any census CSV | **STOP** — epoch event | escalate |
| `book/src/reference/*-generated.md` | expected **only if** the campaign registers a concept or predicate | if it did not, STOP |

**F4 — does The Grain collide?** The Grain holds a `hold-off` on
`windows/scene/` and `windows/locale/` and is changing `LocaleFields.water`
value semantics. This campaign touches neither directory. But its salt-lake
class reads `WaterKind::SaltBasin`, and The Grain's re-banding moves how a room
inherits water. **Settled by:** post a `notice` naming `domains/terrain/` and
coordinate before Task 3 (the salt-lake class) lands; if the classification is
in motion, salt lakes drop to §9 and the campaign ships three classes.

**F5 — cost.** A BFS over a level-5 geosphere (~10k cells) per class, once per
terrain build. Expected trivial against the ~1.2 s a `LocaleContext` build
already costs. **Settled by:** Task 1 reports it; a build-time regression
above 5% is a finding that sends the index behind a lazy seam.

---

## 8. Preregistered measurement

Frozen before the code that would move it (decision 0016). Note what does and
does not enforce this: no study JSON carries a hypothesis field, so this
section *is* the freeze, and nothing mechanical compares a result to it.

**H1 — the landscape is individuated at a useful granularity.** Every class
yields a total deterministic ordering, and the named-feature count at seed 42
falls inside a band set from Task 1's measurement before any floor is chosen.
*Falsified if* a class yields one feature covering nearly everything (the floor
is too low to discriminate) or hundreds of near-identical ones (too high to
name).

**H2 — one landform, many names.** For a feature named by two peoples with
different phonologies, the two names differ. *Falsified if* they coincide at a
rate indistinguishable from one people naming it twice — which would mean the
species salt is not reaching the draw.

**H3 — naming is additive.** No committed artifact moves except this
campaign's own new one and the two generated reports (§7 F3's table).
*Falsified by* any STOP row.

**H2 is the one at real risk**, and it is the one worth stating a null for in
advance: if per-culture divergence turns out to be weak, the finding is that
landform names are effectively universal, which is a fact about the phonology
draw and a legitimate campaign result. Do not retune a constant to rescue it
after unblinding.

---

## 9. Out of scope, carried forward rather than dropped

Each wants an idea-registry row.

- **The map** (`RENDER-fogged-world-map-rung`, campaign 2). Mercator —
  chosen because a constant-bearing course draws as a **straight line** on it,
  which is exactly the dead-reckoned rhumb model The Rhumb shipped;
  equirectangular would bend the courses the sim computes. Scrollable and
  zoomable viewport, terrain unfogged, labels gated by §3.4's ordering.
  Mercator's polar divergence needs a declared latitude clamp, which belongs
  in the caption under decision 0142's lost-axis discipline.
- **The fog** (campaign 3). Names learned by travel and by hearsay, per
  culture; concealed sites (the 103 exotic sites, caves) revealed on
  discovery. This is where `RENDER-fogged-world-map-rung` is actually
  satisfied.
- **Ranges, bays, capes, straits, peninsulas, biome regions.** Everything
  needing a clustering pass rather than a graph traversal (§5).
- **Borrowing** — The Watershed's §7 flag 2, decided but deferred (§5).
- **The registry row's cost claim is wrong and must be corrected.**
  `RENDER-fogged-world-map-rung` says "an ASCII equirectangular renderer does
  not exist, and that is the whole cost of the rung". Five already exist —
  `elevation_ascii`, `paleo_ascii`, `settlement_ascii`, plus climate and
  demography. It also says the view carries "the same three epistemic states
  the local chart does", which is unbuildable: within the local chart's
  radius every cell is at least `sensed`, whereas a planet needs a fourth
  state covering nearly all of its surface. Correct the row in this campaign.

---

## 10. Decision to promote

> **A landscape feature is individuated by graph traversal over committed
> terrain, and its name is derived, never committed.** Identity is canonical
> and integer (the lowest cell id of its component, or a river's terminal
> cell), so it is draw-free and needs no tie-break; a name is a pure function
> of `(seed, identity, species)` re-derived on read, so a terrain change that
> renumbers features can never leave a saved world carrying a name its own
> terrain contradicts. One landform has one identity and as many names as
> there are peoples with a word for it.

---

## 11. Task outline

**Stage 1 — measure, then individuate**
1. **Probe.** Report current seed-42 counts for all four classes across a
   range of floors, and the per-build cost. Settles F1 and F5. Every floor in
   §3.2 is chosen against this output.
2. Landmass and sea classes; `land_component_sizes` refactored to a thin
   caller with its output asserted byte-identical (F2's positive control).
3. Salt-lake class — **gated on F4's coordination with The Grain.**
4. River class off the flow forest; catchment as magnitude.

**Stage 2 — name and expose**
5. The injective salt (§3.3) and the naming callers; extend
   `naming.rs:2318`'s property to the new callers.
6. H2's per-culture divergence measurement.
7. `explain` and the almanac name places; the gazetteer gallery artifact.
8. H3's rebaseline against §7 F3's decision table.

**Stage 3 — close**
9. Chronicle entry, freshness sweep, retrospective, registry rows and the
   `RENDER-fogged-world-map-rung` correction (§9).
