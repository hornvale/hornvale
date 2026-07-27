# The Vacancy — Design

**Status:** Draft for G3 review (2026-07-26) · **Author:** Claude (campaign-
autopilot) · **Decider:** Nathan · **Registry:** BIO-37 (the roster-generation
half — this campaign is its first instantiation), MAP-11 (the water half of the
habitat-medium row), BIO-25/26/27, BIO-39, BIO-41 · **Ledger:**
`.superpowers/sdd/decision-ledger.md`

> The roster has sixteen kinds, and every one of them lives on land. The world
> model declares far more states than sixteen terrestrial kinds can inhabit —
> ten marine biomes with no occupants, enum variants no creature carries,
> consumer branches no data reaches, and one metabolic class whose documented
> physics the code does not implement. This campaign fills the productive
> vacancies, records the ones it deliberately leaves empty, and installs the
> instrument that makes a vacancy visible instead of silent.

## 1. What this delivers

1. **The coverage instrument** — a committed, expected-table test naming every
   declared state of the species model and the rung it occupies, plus a
   measured biome-occupancy readout. Built and pinned against the frozen
   sixteen before any new kind exists.
2. **The non-void roster test** — every roster kind must achieve carrying
   capacity above the viability floor on at least one cell of at least one
   world. Catches the BIO-39 failure class (an authored niche no world can
   satisfy) at authoring time instead of months later by hand.
3. **The marine supply axis** — one new resource axis (id 5) and a supply field
   defined on ocean cells, opening the sea to the habitat model. Pinned
   **byte-identical for the existing sixteen**.
3b. **A real `ANIMAL_PREY` supply field**, replacing the hard-coded zero that
   has kept four shipped kinds out of every world ever generated (§3.5). Added
   after the instrument found them, not before.
4. **~12–14 biosphere-only fauna**, terrestrial and marine, filling named dark
   cells: three unoccupied land climate regions, five or six marine biomes,
   four dark trait combinations, and one amphibious kind.
5. **One new Settled people** — hot-arid, `StatusBasis::Generosity`, the fifth
   people and the first witness of that status basis.
6. **The `Autotroph` doc correction**, with the model fix captured as its own
   registry row (BIO-42).

The exit condition is preregistered in §9 and stated in terms of rung
promotions, not creature count.

## 2. The frame: coverage, not creatures

Lifted of its domain vocabulary, "add more species" is *a declared state space
with uninhabited states* — the shape that recurs as uninhabited types in a type
system, unreachable branches in a test suite, unproductive nonterminals in a
grammar. Every one of those domains knows the same hazard: **an uninhabited
declared state is indistinguishable from a wrong one.** Nobody can tell whether
`Ametabolic`'s `None` life-history is handled correctly until something carries
it.

So kinds are selected by which state they promote. BIO-37 already fixed this as
project precedent — "new roster kinds are chosen by filling productive cells,
not by copying the bestiary" — and this campaign is the first instantiation of
that row's roster-generation half.

A declared state moves up a ladder:

```
  DECLARED  --a kind carries it-->        WITNESSED
  WITNESSED --a consumer branches on a real value--> EXERCISED
  EXERCISED --a metric or census reports it-->       MEASURED
  MEASURED  --a test fails if it breaks-->           PINNED

  forbidden:    DECLARED -> MEASURED  (no witness, nothing to measure)
  pathological: DECLARED -> PINNED    (a test over a branch no data reaches;
                                       green forever, asserts nothing)
```

That last transition is one the project has been bitten by before, and it is
why §4's instrument is a table of *intended* rungs rather than an assertion
that every variant is witnessed.

Three distinct failure modes hide under "declared but not really there," and
they take different treatments:

```
+---------------------------+---------------------------+-------------------+
| failure mode              | instance here             | treatment         |
+---------------------------+---------------------------+-------------------+
| honest seam (declared,    | none left - Crepuscular   | leave alone       |
|   doc says unused)        |  was one before The Vigil |                   |
| STALE CLAIM (doc != code) | Autotroph (S3.3)          | correct the doc;  |
|                           |                           | model fix -> its  |
|                           |                           | own campaign      |
| UNREACHABLE (code != data)| minded Gregarious;        | give it a witness |
|                           | StatusBasis::Generosity;  |                   |
|                           | the 10 marine biomes      |                   |
| ADMISSIBLE BUT VOID       | any kind whose authored    | REFUSE LOUDLY    |
|   (loads, passes every    | niche no world satisfies  | (S4.2), never     |
|    check, exists nowhere) | - the BIO-39 class        | witness           |
+---------------------------+---------------------------+-------------------+
```

## 3. The coverage ledger — where the sixteen actually sit

Read directly from the registries in `domains/species/src/lib.rs`,
`domains/climate/src/biome.rs`, `kernel/src/ecology.rs`, and
`windows/worldgen/src/components.rs`; the biome column is read off authored
optima and is **re-derived by measurement in stage 1** before any kind is
selected against it.

### 3.1 Trait-axis states

```
+-----------------------------------+-----------+---------------------------+
| declared state                    | rung      | witnesses                 |
+-----------------------------------+-----------+---------------------------+
| StatusBasis::Generosity           | DECLARED  | none                      |
| minded Gregarious (society <=>    | DECLARED  | none - the filter at      |
|   minded AND social, dec. 0068)   |           |   components.rs:303 has   |
|                                   |           |   no data that reaches it |
| MetabolicClass::Autotroph         | WITNESSED | treant, twig-blight - but |
|                                   | not exer. |   computed as Endotherm   |
|                                   |           |   (see 3.3)               |
| MetabolicClass::Ametabolic        | WITNESSED | xorn only; sole carrier   |
|                                   |           |   of the None branch      |
| ActivityCycle::Crepuscular        | WITNESSED | white-dragon only         |
| SocialForm::Sessile               | WITNESSED | 2, both family "plant"    |
| Gregarious x ANIMAL_PREY          | DECLARED  | none - all three herders  |
|                                   |           |   are pure PLANT_FORAGE   |
| Sessile x DETRITUS                | DECLARED  | none - both Sessile kinds |
|                                   |           |   are PHOTOSYNTHATE       |
| DETRITUS (any)                    | WITNESSED | otyugh only               |
| a multi-axis (amphibious) uptake  | DECLARED  | none - every kind's niche |
|   vector                          |           |   is one medium           |
+-----------------------------------+-----------+---------------------------+
```

Witnessed and needing no work: both `Sociality` variants,
`StatusBasis::{Rank, Knowledge}`, `ActivityCycle::{Diurnal, Nocturnal}`,
`MetabolicClass::{Endotherm, Ectotherm}`, all four `SocialForm` variants,
`MINERAL`, `PHOTOSYNTHATE`, `PLANT_FORAGE`, and `Settled × Ectotherm` (kobold).

**`ANIMAL_PREY` is the exception, and this table said otherwise until the
instrument ran.** It is witnessed in four kinds' *uptake vectors* and has no
*supply* at all — a distinction this section originally missed, because reading
the registries shows who eats prey and says nothing about whether prey exists.
See §3.5.

### 3.2 Climate-region states

Twenty-two biomes are declared: twelve land, ten marine.

**Land**, read off the authored optima of the sixteen:

- **Hot-arid** — one witness, and it is a dragon (red, 20 °C / 0.10). No
  mundane kind, no people. The largest single land gap.
- **Savanna and tropical-seasonal** (hot, mid-moisture) — held only by the
  goblin generalist's wide skirt; no kind is *centred* there.
- **Boreal / taiga** (cold, moist) — thin: kobold 6 °C, rust-monster 8 °C,
  giant-elk 10 °C, none centred on the cold-moist cell.
- Covered and left alone: hot-wet (bugbear 21/0.82, otyugh 23/0.83, black
  dragon), cold and polar (mammoth −25, white dragon −20, giant-goat −5),
  alpine (kobold 3000 m, giant-goat 3800 m).

**Marine** — all ten unoccupied. This is the model's single largest vacancy,
and §3.4 is why it is now in scope.

### 3.3 The stale claim: `Autotroph`

`MetabolicClass::Autotroph`'s doc (`lib.rs:815`) says a phototroph's basal rate
is surface/area-limited, "so the §4 universal exponent does NOT apply —
activating this class is its own modelling decision. Unused seam." The code
disagrees: `allometry.rs:47` assigns Autotroph `B0_ENDOTHERM` under
`P_METABOLIC = 0.75`, and `pace_multiplier` returns `1.0`. Treant and
twig-blight are therefore computed as endotherms of the same mass, and nothing
tests otherwise. The doc was true when written and was not revisited when The
Menagerie witnessed the class.

**This campaign corrects the doc and does not touch the allometry.** Changing a
scaling exponent and expanding the roster in one campaign destroys attribution
for both — The Menagerie's one-variable-per-stage discipline. The model fix is
BIO-42 and gets its own campaign, which can measure against a roster this one
leaves frozen. This is a fidelity-relevant deferral and is flagged for G3
review rather than settled here.

### 3.4 The sea is open, and was designed to be

Aquatic kinds were initially scoped out of this campaign on the reasoning that
The Tumult's land mask zeroes all five resource-supply axes on submerged cells,
so an aquatic kind would have `K = 0` everywhere and be a ghost. **That
reasoning was wrong**, and three separate docs say so:

- The land mask itself (`lib.rs:713`): "An aquatic kind arrives by authoring a
  marine supply axis and a supply field defined on water, **not by an
  exemption** from a global rule; its uptake vector would simply weight an axis
  these fields do not touch."
- The K assembly (`lib.rs:858`): "Nothing in this assembly special-cases water;
  an aquatic kind authored onto a future marine supply axis would get a
  non-zero K at sea from this same product, **unchanged**."
- The axis basis (`kernel/src/ecology.rs:85`): "The basis is **open** — later
  campaigns may register further axes with higher ids." `MINERAL`'s own doc
  reserves "the aquatic mineral axis … for a later basis extension."

The Tumult put the mask on *supply* rather than on assembled K precisely so
this day would need no exemption. The blocker was self-imposed.

**The design.** One new axis, `MARINE_FORAGE` (id 5), standing for marine
primary production and the prey web it supports at this fidelity. Its supply
field is defined on ocean cells from inputs climate already computes — the
marine biome class (`Upwelling` is documented as the high-productivity case;
`CoralReef` and `KelpForest` are the shallow productive classes), sea-surface
temperature, and depth through the euphotic zone. One amplitude constant is the
single calibration knob, following The Demesne's precedent of exactly one knob
per supply axis, re-fit once.

Trophic differentiation *within* the sea (plankton distinct from marine prey,
mirroring the land's `PHOTOSYNTHATE`/`PLANT_FORAGE`/`ANIMAL_PREY` split) is a
later refinement, captured as a registry row rather than built here.

**Why this is cheap and provable.** Axis ids are explicit `u16`s, and the
existing kinds' `ResourceVector`s are sparse — none of them mentions id 5. Both
the Pianka overlap and the supply dot product therefore gain only zero terms,
so **every existing kind's K is bit-for-bit unchanged**. That is a byte-identity
test, not an expectation, and it is stage 2's exit gate. Adding a *new* id is
the sanctioned extension; the hazard the kernel warns about is changing or
reusing an existing one, which this does not do.

**Corrected after the stage-2 review.** This spec originally justified
"append, never insert" by float non-associativity. That rationale is **wrong for
a zero-weight axis**: `x + 0.0 == x` exactly at every position, so a mid-slice
insert — even a prepend — leaves every sum bit-identical, which a mutation test
confirmed. The real hazard is **positional tie-breaking**: a niche's dominant
axis anchors at `v1_basis()[0]` and only a *strictly* greater weight displaces
the leader, so basis position decides every tie, including the total tie of the
zero vector. Prepending an axis would silently change what a zero-weight niche
resolves to and flip its off-chain trophic classification. The rule is
unchanged; the reason it exists is not what this spec first said, and the
distinction matters because an author checking the stated rationale would find
nothing wrong and ship the reorder.

**The amphibious kind is the proof.** A kind weighting both terrestrial and
marine axes needs no special case whatsoever — the sparse vector and the
saturating sum handle it. If an amphibious kind requires any new branch, the
axis-extension design is wrong, and stage 3 will say so.

### 3.5 The four kinds that never existed

**Found by the instrument on its first run**, which is the strongest validation
this campaign's premise could have had, and the most awkward.

`ANIMAL_PREY` supply is hard-coded to `0.0` in the K assembly
(`lib.rs:910` — its own doc calls it "Stage 2's placeholder zero"). The three
chromatic dragons and the owlbear are authored as pure-`ANIMAL_PREY` obligate
predators. Zero supply, saturated, multiplied by any condition response, is
zero: **their K is exactly zero on every cell of every seed.** They are in the
registry, they satisfy every referential-integrity check, and they are absent
from every world that has ever been generated. The Vigil gave those dragons
minds, perception, and a frozen Draconic tongue; nothing has ever carried it.

This is precisely §2's *admissible but void* category — the one this spec says
must be **refused, not witnessed** — and it was already true on `main` before
this campaign began. Task 3's readout returned 12 of 16 kinds, and that is how
it was found.

**The fix ships here** (Nathan, 2026-07-26): a real `ANIMAL_PREY` supply field,
defined as a **scale of the forage field by a trophic-transfer efficiency**
(Lindeman's ~10% rule). This is exactly parallel to the shipped
`forage_supply_field`, which is `FORAGE_FRACTION = 0.5` of `base_carrying` — one
constant, physically anchored, non-circular (it reads primary production, never
predator or prey populations), and land-masked transitively because forage
already is. Marine predators are unaffected; they eat `MARINE_FORAGE`.

Leaving it out was considered and rejected: §5.1's `Gregarious × ANIMAL_PREY`
predators would have been authored as two *more* ghosts, satisfying exit
criterion 5 on paper with kinds that do not exist — a campaign about filling
vacancies contradicting itself to claim one.

The consequence for the plan is deliberate. §4.2's non-void test ships with an
explicit allowlist of the four known-void kinds naming this blocker, and the
task that lands the prey field **deletes the allowlist**. The deletion is the
proof the fix worked.

## 4. The instrument

### 4.1 The coverage table

An expected-table test in `domains/species`, following the shape already
established by `every_kind_has_the_authored_social_form`: each declared state
paired with its **intended** rung and its witnesses. Editing the roster forces
a deliberate edit here, which is the point — the table states intent and rots
loudly instead of silently.

It deliberately does **not** assert "every declared variant has a witness."
Deliberately-empty cells are legitimate (BIO-37: "empty cells are
creature-design predictions"), and an assertion forbidding them would either be
false or would force junk kinds into the roster to satisfy it.

### 4.2 The non-void roster test

For every kind in the biosphere registry, over a fixed small seed set: the kind
achieves K above the viability floor on at least one cell of at least one
world. Hard assertion.

This is the campaign's cheapest and longest-lived deliverable. The failure it
catches is not hypothetical: BIO-39 records that kobold's elevation optimum sat
at or above the highest land on most seeds, so its documented "exclusive
highland stronghold" was unoccupiable and its fit ran ~25× below every other
people *everywhere* — shipped, and found by hand in a later campaign. Every
kind stages 3 and 4 author is validated by this test on arrival, marine kinds
included; for them it is also the check that the new supply field actually
reaches the sea.

Per-kind occupancy figures are **reported** in the coverage readout, not
asserted. A pinned occupancy number would freeze whatever it happens to be,
including a bug — the hazard already recorded about drift checks pinning output
against *change* rather than against being *wrong*.

## 5. The roster additions

Numbers (`mass`, and `potency` as CR/30 per decision 0064) are sourced from the
5E Monster Manual **at authoring time and verified against it** — this spec
names cells and candidates, not invented magnitudes. Psychology, niche, and
condition-response are authored ecologically from body and habitat; 5E
behavioural and moral canon is not imported (decision 0021, no alignment axis).
Where a productive cell has no 5E-attested occupant, the cell is left dark and
recorded rather than filled with an invention.

**Three provenances, and one of them is perishable.** The roster's numbers come
from three sources with different authorities and — the part that matters —
very different rates of decay:

| provenance | governs | decays when |
|---|---|---|
| 5E canon | `mass`, `potency` (CR/30) | never — an external fixed corpus |
| real-Earth science | the scaling exponents and calibration anchors (Kleiber's ¾, the 40 kg/60 yr anchor) | never |
| **measured from this world** | **every `ConditionNiche` optimum** | **every time terrain or climate moves under it** |

Authority runs in that order where they conflict: reality governs derived
quantities, this world's measurements govern placement, and 5E governs only the
input magnitude. Real-animal masses are deliberately *not* mixed in for the
mundane beasts — a roster where "lion" came from biology and "owlbear" from the
Monster Manual would have incoherent provenance for no gain in a world that is
not Earth.

The third row is why stage 1's occupancy readout is a **committed artifact and
not a scratch measurement**: every new kind's condition optima are authored
against it, on named percentiles, exactly as The Tumult re-authored the existing
optima against its measured settleable-land table. That table is what let the
kobold's misplacement be diagnosed at all, and its absence is why the
misplacement shipped in the first place. Committing the readout hands the next
re-datum campaign the instrument this one had to build from scratch.

### 5.1 Terrestrial fauna

| cell to fill | candidate | why this cell |
|---|---|---|
| hot-arid, mundane | giant scorpion / ankheg | the largest land gap; also a third `Ectotherm` |
| savanna, `Gregarious × ANIMAL_PREY` | giant hyena / lion | the dark combination — every current herder is a pure forager |
| boreal, `Gregarious × ANIMAL_PREY` | dire wolf | the cold-moist gap; the same combination in a second climate |
| hot-arid or savanna herbivore | rhinoceros / giant boar | gives the new predators a prey base in the same region |
| tropical, `Ectotherm` apex | giant constrictor snake | hot-wet is covered by peoples and a dragon, not by an ectotherm |
| `DETRITUS`, second witness | carrion crawler | detritus has exactly one witness (otyugh) |
| `Sessile × DETRITUS` | shrieker / violet fungus | a genuinely new cell, not a third plant — a decomposer that cannot move |
| ~~`Crepuscular`, mundane witness~~ | ~~giant badger~~ | **withdrawn at plan-review — not authorable as fauna** (see below) |

**A cell this spec listed without checking the lattice.** `ActivityCycle` is a
field of `PerceptionVector`, and `perception_registry` is keyed to minded
*speaking* kinds only — The Vigil enforces `speech ⊆ perception ⊆ mind`, so a
kind added to perception without speech fails `check_integrity` at load. A
mundane beast therefore **cannot carry an `ActivityCycle` at all**, and "a
mundane `Crepuscular` witness" is unauthorable without widening the perception
registry, which would break that lattice for one cell's sake. The cell is
withdrawn; `Crepuscular`'s single-witness state stays recorded in the coverage
table naming *this* blocker rather than the absence of a candidate. The fifth
people may incidentally supply the second witness depending on its authored
insolation optimum (§5.3).

### 5.2 Marine and amphibious fauna

| cell to fill | candidate | why this cell |
|---|---|---|
| `CoralReef` | reef shark / giant crab | the shallow productive class |
| `KelpForest` | giant octopus | cold shallow; a high-cognition solitary invertebrate |
| `Epipelagic`, `Gregarious × ANIMAL_PREY` | killer whale | the dark combination a third time, now at sea, at apex mass |
| `Bathypelagic` / `Abyssal` | giant squid | deep, dark, solitary — and the furthest any kind sits from the light |
| **amphibious** (land + marine uptake) | giant crocodile | the multi-medium uptake vector — §3.4's proof case |

Left dark, with reasons, rather than filled: `HydrothermalVent` and
`HadalTrench` (a vent community is **chemotrophic**, which is a fifth
`MetabolicClass` — widening that registry makes every reader the blast radius,
The Eremite's lesson, and it is not needed for any other cell); `SeaIce` and
`Upwelling` (candidates exist but add no new state beyond the four above);
`Mesopelagic` (spanned by the bathypelagic kind's skirt).

**A clade costs more than a kind.** `components.rs` requires a `family_proto`
entry in `domains/language` for any family label carried by ≥2 kinds. Two fungi
therefore mean a new `fungus` proto articulation vector — so even
"biosphere-only" fauna can reach the language crate. Each stage chooses per
clade whether a shared family earns its proto or the kinds stay singleton
families; the choice is explicit, not discovered by a failing integrity check.

### 5.3 The fifth people

**Hot-arid, `StatusBasis::Generosity`.** One kind, four dark cells: the
`Generosity` status basis, the hot-arid climate gap, a fifth language family,
and the first people authored under the coverage instrument.

The pairing is ecologically ordinary rather than a stretch: where forage is
scarce and high-variance, sharing a windfall is the status currency, and
provision-based standing is the well-attested arid-land pattern. It is derived
from the environment, not imported from lore.

**The kind is the gnoll** (Nathan, G3, 2026-07-26 — authoring the world's
creatures is a taste call, per The Menagerie's spec §2). Arid/savanna, mundane
CR so `potency` is 0, and a family distinct from every existing one. Its 5E
canon is scavenger-predator, which the sim reads as body and diet only —
decision 0021 means none of its lore morality comes with it.

Discarded: arid lizardfolk (`Ectotherm` is already witnessed by kobold, so it
opened no new cell) and thri-kreen.

**Gnoll follows kobold's shape, not the goblinoids'.** Kobold is the roster's
existing singleton-family people: `family_of` maps it to its own name, and it
carries no `family_proto` entry, because `components.rs` requires a proto only
for a label held by ≥2 kinds (verified — `family_proto` holds exactly
`goblinoid`, `draconic`, `plant`). Gnoll is therefore authored as a singleton
family with its own articulation and lexicon and **no proto**, which is the
cheaper of the two peopled shapes and the one with a working precedent.

## 6. Deliberately left dark

Recorded so each vacancy is a decision rather than an oversight; each becomes a
`DECLARED` row in the coverage table naming its blocker.

- **An aquatic *people*.** Not blocked by condensation — `condense_stack` takes
  `geo`, the density field, masses and a threshold, with **no terrain argument
  and no ocean test**, so it would condense settlements at sea unchanged. The
  blocker is everything *downstream* of a settlement: freshwater proximity (The
  Confluence's river-distance term), the locale window, and the walk all assume
  land, and none of them has been audited for water. That audit is MAP-11's
  campaign, and it is the natural successor to this one.
- **Minded `Gregarious` — the nomadic band.** Decision 0068's whole reason for
  existing, shipped with zero instances. Deferred at Nathan's budget call: it
  risks discovering that settlement-free peoples are unsupported downstream,
  which would swallow the campaign.
- **A second `Ametabolic` witness.** Constructs are manufactured, not born —
  BIO-13's question, not a biosphere roster's.
- **Chemotrophy**, and with it the vent and hadal biomes (§5.2).
- **`Autotroph`'s real physics** — BIO-42 (§3.3).

## 7. Determinism and blast radius

Species are authored, `hornvale_species::stream_labels()` is empty, and the new
axis is a constant — this campaign **draws nothing and adds no stream label**,
so it is not an epoch. Two things do change:

**A kernel change (stage 2).** Adding a resource axis touches
`kernel/src/ecology.rs`, and a kernel change ripples through every domain with
byte-identity regressions that appear only in the artifact drift check and the
censuses, never in a unit test. Stage 2 therefore runs the full gate, and its
exit gate is the byte-identity pin described in §3.4 — not merely "tests pass."

**Committed genesis behaviour (stages 3–4).** New kinds compete in the
coexistence stack, so settlement outcomes, populations, and every downstream
readout move. The Menagerie's retro is explicit that this blast radius is
under-predicted by the plan — it expected census fixtures and drifted twelve
always-run tests across cli/lab/scene/worldgen. Budget a reconciliation pass
over the **whole** test surface at each drifting stage, with the implementer
instructed to STOP-and-report on non-mechanical drift rather than blind-update.

Enumerated reach, by partition (artifacts *and* consumers, both):

- **Consumers:** `kernel::ecology` (stage 2), the coexistence packer and
  habitat K (`domains/demography`), settlement genesis, phonology /
  proto-language / the dictionary and chorus (stage 4, plus any clade needing a
  proto), religion, the repl, the Lab's metrics/schema/runner/charts, and
  `windows/worldgen`'s integrity checks.
- **Artifacts:** the three seed-42 almanacs, the elevation map, the `concepts`
  and `streams` dumps, every lab study under `book/src/laboratory/generated/`,
  and the census goldens.
- **Not in reach, verified:** the new `alchemy` domain.
  `windows/worldgen/src/alchemy.rs`'s `substrate_of_traits` ignores its
  `BiosphereTraits` argument entirely and has no call site outside its own
  module, so the roster does not reach it yet.
- **`EntityId` minting** is alphabetical, so new kinds renumber instances.
  Benign (worlds re-derive) and already recorded as such by The Menagerie.

Golden handling splits deliberately: the cheap always-run goldens re-pin **in
the commit that drifts them**, never deferred to the close; only the expensive
census regen waits for stage 5. That satisfies both the re-baseline rule and
the one-regen-per-campaign rule, which would otherwise conflict.

## 8. Staging — one variable per stage

| stage | content | drift |
|---|---|---|
| **1** | The instrument (§4) against the frozen sixteen, plus the measured biome-occupancy readout stages 3–4 select against. The `Autotroph` doc correction. | none — no behaviour change |
| **2** | The `MARINE_FORAGE` axis and its supply field (§3.4). No new kind. | **none, and that is the exit gate** — byte-identical for all sixteen |
| **2b** | The real `ANIMAL_PREY` supply field (§3.5). No new kind. | **genesis — four existing kinds materialize.** Deliberately its own stage so that drift is attributable to the prey field alone, never confused with the new roster |
| **3** | The fauna (§5.1, §5.2), terrestrial and marine, including the amphibious proof case. | genesis; re-pin always-run goldens in-commit |
| **4** | The fifth people (§5.3), now including the language/chorus/religion surface. | genesis + language artifacts; re-pin in-commit |
| **5** | Close: absorb `the-tithe` **first**, then one census regen on `lefford` (`HV_CENSUS=1`), book sweep, retrospective, registry rows. | census goldens |

Stages 1 and 2 being drift-free is what makes 3 and 4 attributable: neither the
instrument nor the marine mechanism can be accused of having been shaped to fit
the new roster, because both are provably no-ops on the old one.

## 9. Exit criteria (preregistered, before any measurement)

Frozen here, before stage 1's measurement runs:

1. The coverage table exists, is committed, and every row's rung is justified by
   a witness list or a named blocker.
2. The non-void roster test exists and passes on **every** kind — the sixteen
   and everything stages 3 and 4 add. It ships in stage 1 with an explicit
   allowlist of the four kinds §3.5 found void, and **stage 2b deletes the
   allowlist**; the deletion is the criterion, because a passing test with an
   allowlist proves nothing.
3. Stage 2 is byte-identical: every committed artifact and every existing kind's
   K is unchanged by the axis addition.
4. `StatusBasis::Generosity` and `DETRITUS`-beyond-otyugh reach **PINNED**.
5. `Gregarious × ANIMAL_PREY`, `Sessile × DETRITUS`, and the multi-axis
   amphibious uptake vector reach at least **WITNESSED**, with the consumer
   branch demonstrated to be taken.
6. Hot-arid, savanna, and boreal each gain at least one kind *centred* there,
   and at least four of the ten marine biomes gain an occupant — measured on the
   stage-1 readout, re-run after stage 4.
7. Peoples diversity does not collapse: the fifth people does not reduce the
   number of peoples holding settlements below four on the canonical seed.

Criteria 6 and 7 are the falsifiable ones. 7 is the likeliest to fail — a fifth
competitor in the settlement stack is exactly the pressure The Sundering's
preregistered gate caught as depopulation. If either fails, the honest move is
The Menagerie's: ship the structural deliverables, leave the payoff as a
preregistered `#[ignore]`d test, and name the prerequisite.

## 10. Non-goals

- **No aquatic or amphibious *people*** (§6) — fauna only at sea this campaign.
- No trophic split within the marine axis (§3.4), no chemotroph metabolic class
  (§5.2), no widening of `SocialForm` (BIO-37's grid names *pair* and *colonial*
  topologies the enum lacks; widening a registry makes every reader the blast
  radius, and no cell above needs it).
- No allometry change (§3.3). No air-column habitat. No generative species
  sampling (BIO-28/29 stay banked; this roster is authored).
- No behavioural or lair content (BIO-27 owns it, game-layer).

## 11. Risks

- **Marine amplitude calibration becomes a rabbit hole.** Mitigated by
  restricting it to exactly one constant, The Demesne's precedent. If one knob
  cannot produce marine K comparable to land, that is a finding to report, not a
  thing to keep tuning.
- **The fifth people collapses peoples diversity** — criterion 7. Mitigated by
  staging it last, so stages 1–3 are shippable if 4 must be reframed.
- **A census regen races `the-tithe`.** It re-pins
  `book/src/laboratory/generated/the-history/rows.csv`; our regen rewrites every
  census golden. Absorb it before the regen and re-run the full gate on the
  merged result — a clean textual merge has hidden a semantic collision here
  before. `the-threshold` and `the-tumult` touch no roster file (verified: empty
  diff over `domains/species`, `domains/demography`,
  `windows/worldgen/src/components.rs`).
- **A new kind is authored void** — the BIO-39 class. The one risk this campaign
  structurally removes rather than mitigates (§4.2).
- **The stage-3 blast radius exceeds the plan**, as it did in The Menagerie.
  Mitigated by budgeting a full-test-surface reconciliation per drifting stage
  rather than a fixture update.

## 12. Decisions promoted from the ledger

- The deliverable is coverage of the declared state space; the roster is the
  means (#1).
- Budget: fauna breadth plus one people — Nathan's call (#2).
- 5E supplies magnitudes only; psychology and niche are authored ecologically
  (#3, decisions 0064 and 0021).
- Fictional support is treated by failure mode: stale claims get the doc,
  unreachable branches get a witness, admissible-void states get a refusal (#4).
- The coverage table is a committed deliverable, not a spec artifact (#7).
- The nomadic band is deferred, not dropped (#8).
- **The sea is in scope** — the land mask was never the blocker, and the axis
  basis was built open. Aquatic *fauna* in, aquatic *people* out (#9, Nathan's
  call).
