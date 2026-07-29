# The Watershed — design

**Campaign:** The Watershed
**Date:** 2026-07-29
**Status:** DRAFT — awaiting G3 review.

The world names its own landscape. Rivers and landmasses become individuated
things with names, settlements are named after them, and the naming epoch
The Shibboleth opened closes with a name that tells two places apart.

## 1. The problem

### 1.0 Prior art, which this spec must not relitigate

**Decision 0024 (ratified, 2026-07-09) says uniqueness is a reference-time
property.** Committed names may collide; any surface that would be ambiguous
disambiguates at render time from the entities' own site facts, "exactly as
Earth accepts its forty-one Springfields." It states plainly that **no future
work fixes the collision rate by adding entropy.**

This campaign is consistent with that decision and does not reopen it, on two
grounds the decision itself supplies:

- 0024 forecloses *drawn* entropy — stuffing more dice into the string. The
  Watershed adds none; every discriminator here is a derived fact about the
  world.
- 0024 explicitly **anticipates and endorses this route**: "collision
  pressure is expected to fall as a natural by-product of world density:
  every future substrate that adds per-place facts … widens the descriptor
  space without touching the naming engine (LANG-9)." Naming the landscape is
  that substrate.

What this spec *does* retire is the framing that a low-collision number is
itself the goal. It is not, per 0024. The goal is a world whose places are
named after things that exist; falling collisions are the by-product 0024
predicted. §5's criterion is therefore a **prediction**, not a requirement.

**LANG-9 already recorded the limit I re-derived at cost.** Its row reads:
"colliding names agree on their descriptors BY CONSTRUCTION — the gloss IS
the site-descriptor set — so no fact fed INTO naming separates a collision;
only facts outside it can." Four attempts to add descriptor types (§1 below)
each plateaued, exactly as that sentence predicts. Landmasses and rivers are
*particulars*, not descriptors, which is why they fall outside the limit.

**The Wearing shipped a large part of this while The Shibboleth was parked.**
It retired the drawn stem and registered nineteen exposure-gated toponymic
concepts (`hill river valley island ford marsh spring coast lake high low
great little new old north south over under`), plus per-culture name shapes
and a nucleus template set. Measured on main today, 8 seeds / 1842
settlements:

```
                        colliding   mean chars   seed-42 glosses
main (The Wearing)         65.1%        9.5           104
the-shibboleth (parked)    56.5%        9.1           134
```

The parked branch is the better number despite carrying fewer concepts,
because crops and predecessor peoples are *orthogonal* axes while The
Wearing's nineteen are all site descriptors subject to LANG-9's limit. But
the two campaigns overlap heavily and **`river` and `coast` are duplicate
registrations**. §8 states the reconciliation.

### 1.1 The Shibboleth's own measurements

The Shibboleth removed the random stem from settlement names, trading
uniqueness for meaning. `Vngoashshngaoshshngoogootao` became `Gootao`, and a
name became a translatable description of its site — biome, variant,
hydrology, relief, predecessor people, staple. That was the right trade and
it is measured: 8 seeds, 1842 settlements, mean name length 27 → 9.1
characters.

It did not reach the low-collision criterion. **56.5% of settlements share a
name with another.** At seed 42, nine settlements are called `Ka`, on four
different continents.

Four separate attempts to close the gap by adding concepts each landed within
a few points of the last:

```
site facts (river/creek/coast/basin/upland/lowland)   64.0%
+ predecessor peoples                                 63.5%
+ staples (barley/wheat/rice/millet/tuber/vine)       56.5%
```

### 1.2 Why adding concepts plateaus

Every concept in the naming vocabulary is a **type** — `coast`, `rice`,
`upland`. A taxonomy has tens of members, and types are exactly what
neighbouring places share. 329 settlements at seed 42 draw on ~60 types and
realize only **134 distinct glosses**; the criterion needs ~296.

This is LANG-9's recorded limit, rediscovered. The missing ingredient is
**particulars** — proper nouns, whose cardinality
equals the number of things rather than the size of a vocabulary. Real
toponymy names the landscape first and settlements after it: Newcastle-upon-
**Tyne**, where *Tyne* is older than the language using it.

### 1.3 The second cause, which is cheaper

Collisions are not distributed evenly. Measured at seed 42:

```
gloss concepts   settlements   colliding
    1                90          90.0%
    2               108          59.3%
    3               105          38.1%
    4                22          40.9%
    5                 4           0.0%
```

**Name arity dominates.** The 90 single-concept names collide at 90%.
`naming.rs` draws `take = range_u32(1, 2)`, so roughly half of all names use
one concept even where more are available.

Note also that 329 settlements with 134 glosses produce **191 distinct
names**: different species render the same gloss differently, so cross-
lexicon divergence is already contributing and needs no work.

## 2. Non-goals

- **Naming oceans.** Seed 42 has one connected world ocean (29886 cells)
  plus five puddles of 1–4 cells. Connected components are the wrong
  individuation for sea basins — Earth is the same, and names basins by
  convention and enclosure, not topology. Zero discriminating power. A later
  campaign may name basins geometrically; this one does not.
- **Naming mountain ranges, forests, deserts.** Same mechanism, real value
  for prose, no additional discriminating power beyond landmass for the
  settlements that exist. Deferred, registered as an idea-registry row.
- **Individual person names.** "Shaman's Creek" needs a named shaman. Real,
  and the richest remaining vein, but it is a different campaign — it needs
  people to be entities with names before places can be named after them.
- **Modelling sediment as a new field.** Lithology already ships (§3.3); a
  colour field added solely to justify a name would be decoration.
- **Guaranteeing uniqueness.** This campaign approaches the criterion
  statistically. Relational naming (`Upper Gootao`, `Gootao-by-Bababo`) is
  the only mechanism that *guarantees* it and is deliberately held back —
  see §7.

## 3. Design

Four items. Item 1 is independent and lands first; items 2–4 build on each
other.

### Item 1 — floor the name arity at two concepts

`domains/language/src/naming.rs` currently draws:

```rust
let take = if candidates.len() == 1 { 1 } else { stream.range_u32(1, 2) as usize }
    .min(candidates.len());
```

Change the draw to `range_u32(2, 3)`, still clamped by `.min(candidates.len())`
so a site with one exposed concept still yields a one-concept name. This is
one line and no new machinery.

**Stream discipline:** the draw must consume the same number of values as
today, or every downstream name changes for the wrong reason. `range_u32`
consumes one value regardless of bounds, so consumption order is unchanged.

Expected: the 90 single-concept settlements move to two concepts where their
site offers one, taking that cohort from 90% toward the measured 59% band.

### Item 2 — individuate landmasses

A **landmass** is a connected component of non-ocean cells under
`Geosphere::neighbors`, identified by the **lowest cell id it contains**.
Draw-free, deterministic, and stable under anything that does not move the
coastline.

Measured at seed 42: 30 components, of which **10 have ≥100 cells** (1994,
1976, 1842, 1277, 907, 874, 831, 703, 356, 104), 4 more have ≥20, and 10 are
single-cell rocks.

Only components at or above a size floor are named; the floor is
`LANDMASS_MIN_CELLS = 20`, giving 14 named landmasses at seed 42 and leaving
rocks anonymous. A settlement on an unnamed rock simply has no landmass
concept, exactly as a settlement away from water has no hydrology concept.

New API in `domains/terrain`, beside `sitefact`:

```rust
pub struct LandmassId(pub u32);              // the lowest cell id in the component
pub fn landmass_at(terrain: &GeneratedTerrain, cell: CellId) -> Option<LandmassId>;
pub fn landmasses(terrain: &GeneratedTerrain) -> Vec<(LandmassId, usize)>;  // id, size
```

Computed once per terrain and cached on the provider — the same discipline
The Cistern established for scene derivation, and for the same reason.

### Item 3 — individuate rivers by their mouths

`downhill_targets` in `domains/terrain/src/drainage.rs` already gives every
land cell a downhill pointer, which is a **flow forest**. A river is a
maximal subtree of that forest, and its identity is its **terminal cell** —
the ocean cell it empties into, or the interior minimum it dies in.

```rust
pub struct RiverId(pub u32);                 // the terminal (mouth or sink) cell id
pub fn river_at(terrain: &GeneratedTerrain, cell: CellId) -> Option<RiverId>;
pub fn catchment_of(terrain: &GeneratedTerrain, river: RiverId) -> &[CellId];
```

`river_at` returns `None` for a cell whose drainage is below
`sitefact::CREEK_DRAINAGE` — the constant already shipped — so the tiers the
site facts already draw are the tiers the names use:

```
drainage >= 4    (CREEK_DRAINAGE)   619 sea-mouths at seed 42
drainage >= 24   (RIVER_DRAINAGE)    65 sea-mouths
drainage >= 100                       4 sea-mouths
catchment >= 24 cells                115 rivers      <- the naming tier
```

**The naming tier is catchment size, not mouth drainage**: 115 rivers at
seed 42. 66 of them are endorheic — they die inland without reaching the sea,
which is a distinct and nameable class.

Also computed once per terrain and cached.

### Item 4 — name rivers from their catchment lithology

A river's sediment **is its upstream rock**. `MaterialBuffer`
(`domains/terrain/src/lithology.rs`) already carries six continuous axes per
cell, on ocean floor as well as land: `silica` (mafic→felsic), `grain`
(fine→coarse), `induration` (soft→hard), `carbonate`, `metamorphic_grade`,
`porosity`. A river's load is the catchment mean of these.

Measured across the 115 rivers at seed 42:

```
              min     max    mean     sd
silica       0.258   0.719   0.517   0.095
carbonate    0.050   0.574   0.151   0.128
induration   0.434   0.817   0.529   0.084
grain        0.404   0.896   0.637   0.120
metamorph    0.000   0.856   0.130   0.197
```

**Naive banding fails, and the spec must say so.** Three bands on three axes
gives 7 occupied classes with **81 of 115 rivers in a single one**. Averaging
is a low-pass filter and most catchments are unremarkable — this is the
type-vocabulary trap of §1.1 reappearing one level up.

**The fix is to name from the outlier, not the average.** Rank every river
against every other river on each axis, and name it for the axis on which it
is most extreme:

```
axis         extreme   gloss             physical reading
-----------  --------  ----------------  ---------------------------------
carbonate    high      chalk-river       pale, hard water, karst springs
silica       low       dark-river        basaltic catchment
silica       high      bright-river      granitic sand
induration   low       silt-river        braided, shifting banks
induration   high      gorge-river       incised, rock-walled
grain        fine      turbid-river      loess-laden  (the Huang He case)
grain        coarse    gravel-river      cobbled bed
metamorph    high      slate-river       grey, gorge-cut
```

Percentile ranking **guarantees spread by construction**: every axis has a
top and bottom decile no matter how the world came out, so a geologically
flat world still yields distinguishable rivers and a varied one yields vivid
ones. It also makes a river's name a claim about *this world*, which is the
right semantics — a "dark river" is dark for here.

Ties break on axis declaration order, then on `RiverId`, deterministically.

A river's gloss also carries its **terminus**: `sea-` or `inland-` prefix,
from the endorheic flag already computed. That is free and doubles the
class count.

Landmass glosses come from the same shape at a different grain: size rank
(`great-`/`lesser-`), latitude band, and whether it is the largest component
its settlements know.

#### How a name reaches a second people

There is **no canonical word — there is a canonical gloss.** This is already
how Hornvale names everything: `glossed_name` returns `(roman, ipa, gloss)`,
the gloss is the meaning, and each tongue mints its own word from its own
lexicon. `chalk-river` is the fact; goblin and kobold say different words for
it. That is Huáng Hé / Yellow River exactly.

Two peoples meeting one river resolve by **exposure depth**, which the
exposure classifier already computes:

- **Steeped** (the people lives on the river): it glosses the river itself,
  from its own vantage. Two peoples may reach different glosses for one
  river — Rio Grande / Río Bravo, big versus fierce.
- **KnowsOf** (the people knows the river but does not live on it): it
  **borrows** the neighbour's word and nativizes it through the cascade
  machinery `domains/language` already runs for cognates. Danube / Donau /
  Duna, and Newcastle-upon-**Tyne**.

A borrowed word is a **particular**, untranslated, so it carries full
discriminating power into every language that adopts it. This is the
mechanism by which a type-based gloss system acquires proper nouns.

Borrowing is the one genuinely new piece of language machinery in this
campaign and is flagged at G3.

## 4. Determinism and epoch

- **Individuation is draw-free.** Landmass ids and river ids are derived from
  fields that already exist. No new stream, no new consumption, no pin
  interaction.
- **Naming is not.** River and landmass names are minted per language, which
  needs one new stream label each: `river/name/v1`, `landmass/name/v1`.
- **New concepts append to `EPOCH_COHORTS`** as one cohort, per The
  Accession's discipline.
- **This lands inside the epoch The Shibboleth already opened.** Adding
  concepts changes the lexicon universe, which re-mints every word in every
  tongue — the parked branch already moved 56 of ~300 roots. Landing The
  Watershed separately would pay that cost, and the full re-pin of goldens,
  book prose and pinned literals, a **second** time. It must be one epoch.
- **Caching must be byte-identical**, verified by `cmp` on a seed-42 world
  before and after, per the discipline `every-scene-call-rebuilds-the-planet`
  and The Cistern established.

## 5. Verification

Preregistered, per decision 0016. The hypothesis is fixed before the code.

**Primary claim.** Over the 8-seed, 1842-settlement battery already used for
The Shibboleth's measurements, the share of settlements sharing a name with
another falls from **56.5%** to **below 15%**.

Derivation of that figure, at seed 42:

```
distinct glosses today                    134
+ landmass                                187
+ landmass + river-mouth                  266
```

266 of ~296 needed, before Item 1 contributes anything. Item 1 independently
moves 90 settlements off a 90% collision rate. The prediction that the two
compose to under 15% is the falsifiable part.

**Secondary claims.**

1. Mean name length stays **under 14 characters** (today: 9.1; the stem era:
   up to 27). A name that distinguishes by being long has not solved the
   problem.
2. Every river gloss is **truthful**: a `chalk-river` is in the top decile of
   catchment carbonate for its world. Asserted directly, in the manner of
   `a_settlement_name_gloss_is_truthful_to_its_own_site_facts`.
3. **No river name is used for two rivers on the same landmass.** Global
   uniqueness is not claimed; local uniqueness is, and is what a reader
   notices.
4. Landmass and river individuation are **byte-identical under caching**.

**Falsification is a finding.** If percentile naming still collapses — if
most rivers cluster near the median on every axis simultaneously — that is
the headline, and it says the lithology field is smoother than it looks.

## 6. Expected result

Settlement names that read as places: `Ka` on the chalk river of the great
northern landmass becomes distinguishable from the eight other `Ka`s without
becoming unpronounceable. The book gains a gazetteer page. And the world
acquires a layer it has never had — a landscape that is *named*, which is the
precondition for lore, for directions, for a character saying where they are
from.

## 7. Flagged for review (G3)

1. **Epoch scope.** This lands inside The Shibboleth's parked epoch rather
   than as its own. That means the parked branch grows before it merges, and
   the merge decision covers both campaigns at once. The alternative — merge
   The Shibboleth at 56.5%, then pay a second full re-pin — is worse, but it
   is your call, and it is the reason the branch is still parked.
2. **Borrowing is new language machinery.** Loanword nativization reuses the
   cascade, but "which people's word does a borrower borrow" is a new rule
   with a determinism contract. It is the piece most likely to need its own
   task and its own tests.
3. **Two new stream labels** (`river/name/v1`, `landmass/name/v1`) — a
   save-format contract, permanent, renameable only by epoch suffix.
4. **The naming tier is a threshold I chose.** `catchment >= 24 cells` gives
   115 rivers at seed 42 and `LANDMASS_MIN_CELLS = 20` gives 14 landmasses.
   Both are defensible and neither is derived from anything; a different
   world may want different floors.
5. **Oceans are a non-goal** despite being the thing you named first. §2 says
   why: one connected ocean, no discriminating power. If you want sea basins
   named for prose reasons regardless, that is a legitimate override and I
   would put it in this campaign rather than a later one.
6. **The Shibboleth vs The Wearing (§8).** The parked branch duplicates work
   main already shipped, and duplicate-registers two concepts. The
   reconciliation in §8 discards roughly half of a branch that took four
   commits and a full gate to make green. That is the right call and it is
   still a loss; you should see it stated rather than buried.
7. **The criterion is now a prediction, not a requirement** (§1.0). If you
   want it to be a requirement, that means superseding decision 0024, which
   is a separate and larger decision than this campaign.

## 8. Reconciling The Shibboleth with The Wearing

The two campaigns solved the same problem in parallel, and main won the race.
The parked branch splits cleanly into a redundant half and an orthogonal one.

**Discard (superseded by The Wearing's nineteen concepts):**

- the six `sitefact` concepts — `river`, `creek`, `coast`, `basin`, `upland`,
  `lowland`. `river` and `coast` are duplicate registrations outright;
  `upland`/`lowland` are The Wearing's `high`/`low`; `basin` is covered by
  `valley`/`marsh`.
- the stem removal itself, already on main.

**Keep and rebase onto main (orthogonal, and the reason the parked branch
scores better):**

- **Sonority sequencing** — a phonotactic rule, not a concept. It reorders
  onset clusters by rising sonority (`Nsaav` → `Smaav`, `Ngshaap` →
  `Sngaap`), touching every word in every tongue. Independent of anything The
  Wearing did, and it composes with the nucleus template set rather than
  competing with it.
- **Staples** — six climate-derived concepts on a temperature × moisture
  band, exposure-gated on subsistence. Orthogonal to every site descriptor:
  what grows here does not follow from where here is.
- **Predecessor peoples** — history-derived, reading the occupation
  stratigraphy. Thin today at five peoples, and the vein that scales with the
  bestiary.
- **Four defect fixes the parked branch's gate found**, all of which apply to
  main unchanged: scene features deduped by name; a stale `GRIEVANCE_NPC`
  whose test passes vacuously; a gloss-truthfulness test that predated the
  facts it names; six terrain concepts declaring `Lexicalization::Expected`
  with no pack to realize them.

The rebase is not mechanical — both branches touch `naming.rs`, the concept
registry, and `EPOCH_COHORTS` — but the kept half is small and the conflicts
are concentrated. **Estimate: one task, not one campaign.**
