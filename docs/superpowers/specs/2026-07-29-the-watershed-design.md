# The Watershed — design

**Campaign:** The Watershed
**Date:** 2026-07-29
**Status:** CLOSED, PARTLY FALSIFIED (2026-07-31). This spec never passed G3,
and the campaign shipped without it passing — deliberately. Measuring §5 before
building it falsified the arithmetic, and three further sections were found
anchored to code that does not exist on main. What shipped is Item 0 (sonority),
Item 5 (predecessor peoples), and an instrument repair; Items 1–4 are specified,
measured, and NOT built. Read
[the chronicle](../../../book/src/chronicle/the-watershed.md) first; this
document is kept as written for the record, with the corrections listed
immediately below, and is NOT a plan for the successor campaign.

**Corrections, none of them applied to the body:**

- **§5's primary claim is false.** Items 2–4 cannot reach below 15% collisions.
  The measured floor, assuming perfectly injective naming on the full
  `(gloss, landmass, river)` tuple, is **44.8%** over an 8-seed battery. The
  binding constraint is settlement CLUSTERING, not descriptor vocabulary:
  seed 42's 14 named landmasses and 123 named rivers are occupied by only
  **5** and **13**.
- **§5's multipliers are wrong.** ×1.40 / ×1.99 were simulated over a corpus
  from a branch that never merged. Measured on main at seed 42: **×1.29 /
  ×1.63**. The stated *reason* for the risk was also wrong — river identity is
  ~84% orthogonal to the gloss, as hoped.
- **§1.3's arity cliff is a single-seed artifact.** Visible at seed 42, absent
  over 8 seeds, where each added concept buys a flat ~20pp. Item 1's direction
  survives; its "the payoff is at three" threshold does not.
- **§3 Items 2–3 anchor on `sitefact`**, a module that exists only on the
  unmerged `the-shibboleth` branch — the same half §8 deliberately discarded.
  Main's equivalents are `is_river_cell`, `is_ford_cell`,
  `carve::WATERFALL_MIN_DRAINAGE`, `ISLAND_CELL_CAP`, `landmass_size_capped`.
- **§4's epoch premise is void** (no parked branch to land inside), and no
  `ROOT_EPOCH` bump was owed: per `784c2cb6`, the label documents a change to
  the *assignment algorithm*, and sonority changes the phonology that algorithm
  draws from.
- **§7 flag 3 is withdrawn, not decided.** `river/name/v1` and
  `landmass/name/v1` are phantom labels under decision 0083 — same algorithm,
  different subject. Borrowing owes a label instead; it is a different
  derivation.
- **§7 flag 2 is decided:** borrowing ships, selecting the Steeped people with
  the most settlements on the river. It buys realism, not the criterion.

Original header follows.

**Status:** DRAFT — awaiting G3 review. **The reconciliation of §8 is DONE
and merged** (`ccf370bc`, 2026-07-29): staples and three defect fixes landed
on main, taking collisions 65.1% → 59.8% as a purely additive epoch. Sonority
moved out of the reconciliation and into this campaign as Item 0, with a
known seam to close. Predecessor peoples likewise, as Item 5.

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

Collisions are not distributed evenly. Measured on **current main** at seed
42, after the §8 reconciliation:

```
gloss concepts   settlements   colliding
    1               114          88.6%
    2               102          74.5%
    3                65          30.8%
    4                33          21.2%
    5                14          14.3%
    6                 1           0.0%
```

**Name arity dominates, and the cliff is between two and three.** Going from
one concept to two buys little (88.6% → 74.5%); going from two to three more
than halves it (74.5% → 30.8%). 216 of 329 settlements — two thirds — sit on
the wrong side of that cliff.

This table was re-measured against main specifically because the parked
branch's version of it put the cliff between one and two, and Item 1 was
originally specified from that. It is the same measurement on a different
codebase, and it changes what Item 1 should do.

Note also that 329 settlements with 129 glosses produce **175 distinct
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

Six items. Item 0 is independent and carries a known defect to close. Item 1
is independent. Items 2–4 build on each other. Item 5 is optional and last.

### Item 0 — finish sonority sequencing, seam rule included

Onset and coda templates are drawn by picking manners independently, so
`[Nasal, Nasal]` is a legal onset (names opening `ngng-`) and reverse-sonority
clusters no language uses are equally legal. Ordering each template by
sonority — rising toward the nucleus in an onset, falling away in a coda,
equal-sonority neighbours collapsed — fixes it, and **sorting rather than
rejecting keeps the draw count identical**, so the constraint costs no
entropy. That half is written and measured (`Nsaav` → `Smaav`, `Ngshaap` →
`Sngaap`; `Ngngoqjqobqotdo` → `Ngaqqjokqotdo`).

**The half that is NOT written, and why this is Item 0 rather than merged
already.** Two independent obstacles, both discovered empirically:

1. **The syllable seam.** SSP inside a template cannot see a coda `sh`
   meeting an onset `sh` across a syllable boundary, which is precisely how
   `Vngoashshngaoshshngoogootao` was built. Fixing it needs the rule in
   BOTH `conforms` and `repair_phonotactics`, which encode well-formedness
   independently. A first attempt taught only `conforms` and left repair
   emitting output `conforms` rejects — two disagreeing notions of
   well-formed, worse than one permissive one, and it was reverted.
   **Repair's DP must carry the incoming sonority**, making `best[i]` a table
   indexed by position AND incoming sonority.
2. **Wear-then-repair.** Applying the finished half to main failed
   `glossed_names_audibly_contain_their_words_under_a_saturated_corpus`:
   `worn_compound` runs `wear_under` and then `assemble`/repair, so a name's
   surface can be an intermediate that equals neither the citation form nor
   the wear-only reflex the property compares against (`Faaffa`, against
   `faaffaa` and `faffa`). The property is defending something real —
   audibility of the morpheme a gloss names — so it must not be weakened to
   admit the intermediate. Either repair preserves audibility by
   construction, or the property learns the actual emitted reflex. **Decide
   which before writing either.**

**Known collateral.** `wear_is_keyed_to_frequency_not_to_the_compound_slot`
carries a narrow seed-searched fixture that has been re-swept three times.
Sonority breaks it again — and widens it: 0..600 held exactly one qualifying
seed before, and five afterwards (164, 305, 325, 370, 521), because
SSP-ordered templates are markedly more repairable. Re-sweep with the
`#[ignore]`d `sweep_wear_fixture_seed`, added for this purpose.

This item re-mints every word in every tongue. It is the reason this campaign
is an epoch at all — items 1–4 are additive.

### Item 1 — raise the name arity toward three concepts

Per §1.3 the payoff is at three, not two. Raise the draw's floor so a name
takes three site concepts wherever the site exposes three, clamped by
`.min(candidates.len())` so a thin site still yields a short name.

**Stream discipline:** the draw must consume the same number of values as
today, or every downstream name changes for the wrong reason. `range_u32`
consumes one value regardless of its bounds, so widening the bounds is
consumption-identical. Verify this rather than assume it — the exact draw
site is The Wearing's, not the one the parked branch modified.

**Interaction to respect.** The Wearing draws a per-culture `NameShape`, so
arity is partly a cultural property — some peoples prefer the bare simplex,
others the specific-plus-generic compound. A blanket floor would flatten that
distinction, which is a real loss. Prefer raising each shape's own arity
band, so a simplex-preferring culture still reads as simplex-preferring, just
less collision-prone. **If that cannot be done cleanly, say so and take the
blanket floor knowingly** — do not quietly erase the culture signal.

Expected: the 216 settlements at one or two concepts move up where their site
allows, toward the measured 30.8% band. This is the single cheapest item in
the campaign and should be measured ALONE before items 2–4 land, so its share
is known rather than assumed.

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

### Item 5 — predecessor peoples (optional)

The deepest occupation layer at a settlement's cell, where it belongs to
another species: a goblin steading raised on a gnoll ruin is named for the
gnolls, and `hornvale history --site` already reads out the stratigraphy
behind the name. Measured at 0.5 percentage points over five peoples, so it
is here for MEANING, not for the criterion — and it is the vein that scales
with the bestiary, where "a griffin killed a goblin here" is high-cardinality
by construction.

It needs one new public API: a static kind-concept lookup in
`domains/species`, because `settlement_site_concepts` returns
`Vec<&'static str>` and the kind concepts (`gnoll-kind`, …) exist today only
as literals inside a registration loop. That is a small addition and a real
design decision, which is why it is an item rather than a line.

Land it last. If items 0–4 hit the §5 prediction without it, it can be judged
on flavour alone, which is the honest basis for it.

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

**Primary claim.** Over the 8-seed, 1842-settlement battery, the share of
settlements sharing a name with another falls from the post-reconciliation
baseline of **59.8%** to **below 15%**.

Derivation, at seed 42. The landmass and river numbers below were measured by
simulating the discriminator over the parked branch's 134-gloss corpus; the
baseline has since moved to 129, so treat the ratios rather than the
absolutes as the load-bearing part:

```
distinct glosses (measured on the branch)   134
+ landmass                                  187      (x1.40)
+ landmass + river-mouth                    266      (x1.99)
```

Applied to main's 129 glosses that is ~257 of the ~296 needed, before Item 1
contributes anything — and Item 1 has 216 settlements sitting below the
arity cliff. **The prediction that the two compose to under 15% is the
falsifiable part**, and the ×1.99 is the number most likely to be wrong,
because it assumes river identity is uncorrelated with the site facts a name
already carries. It is not perfectly uncorrelated: a river cell is a `river`
or `ford` cell, which the gloss may already name.

**Re-measure the landmass and river multipliers against main before relying
on them.** They are the only figures in this spec not taken from the codebase
they will run on.

**Secondary claims.**

1. Mean name length stays **under 14 characters** (today: 9.2; the stem era:
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
6. **The Shibboleth vs The Wearing (§8) — resolved, merged as `ccf370bc`.**
   Roughly half of a branch that took four commits and a full gate to make
   green was discarded as superseded. That was the right call and it is still
   a loss; it is stated here rather than buried. The half that landed took
   collisions 65.1% → 59.8% additively.
8. **Item 1 changed after re-measuring on main.** The parked branch put the
   arity cliff between one and two concepts; main puts it between two and
   three. The spec originally specified the wrong floor, from a measurement
   taken on the wrong codebase. Every other number in §1 has now been
   re-taken against main; the landmass and river multipliers in §5 have NOT,
   and are flagged there.
7. **The criterion is now a prediction, not a requirement** (§1.0). If you
   want it to be a requirement, that means superseding decision 0024, which
   is a separate and larger decision than this campaign.

## 8. The reconciliation — DONE, merged as `ccf370bc`

The two campaigns solved the same problem in parallel, and main won the race.
The parked branch split cleanly into a redundant half and an orthogonal one.
This section is now a record of what happened, not a plan.

**Result: 65.1% → 59.8% collisions, 104 → 129 seed-42 glosses, and the
lexicon golden gained 30 lines while changing ZERO words** — a purely
additive epoch, which is strictly better than the parked branch's own
outcome (it re-minted 56 roots across every tongue for the same gain).
Dropping sonority is what bought that.

**Discard (superseded by The Wearing's nineteen concepts):**

- the six `sitefact` concepts — `river`, `creek`, `coast`, `basin`, `upland`,
  `lowland`. `river` and `coast` are duplicate registrations outright;
  `upland`/`lowland` are The Wearing's `high`/`low`; `basin` is covered by
  `valley`/`marsh`.
- the stem removal itself, already on main.

**Landed (orthogonal, and the reason the parked branch scored better):**

- **Staples** — six climate-derived concepts on a temperature × moisture
  band, exposure-gated on subsistence. Orthogonal to every site descriptor:
  what grows here does not follow from where here is.
- **Three defect fixes**, two of which were live on main: `windows/scene`
  deduped features BY NAME (latent — it passed only while the flagship's own
  name happened to be unique); `GRIEVANCE_NPC`, stale on its third rename,
  whose negative assertions pass vacuously for a non-existent NPC, now guarded
  by a fixture-existence assertion that caught this very campaign's rename on
  the next gate run; and an exact duplicate of the variant exposure block left
  by The Wearing's merge.

**Deferred out of the reconciliation, into this campaign:**

- **Sonority sequencing → Item 0.** Applying it to main surfaced, within two
  test runs, exactly the seam its own parked commit named. See Item 0.
- **Predecessor peoples → Item 5.** It needs a public static kind-concept
  lookup in `domains/species` that does not exist. See Item 5.
