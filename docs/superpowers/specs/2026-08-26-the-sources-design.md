# The Sources — an energy field over the rock

Rung 2 of the Underworld Larder
(`docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`).
Rung 1 (The Gossan) made chemotrophy *expressible*. This rung makes it
*witnessed*, by deriving the thing a chemotroph eats.

## 1. What occasioned it

The metaplan's §4 gives rung 2 three jobs: derive the six lithology-keyed
energy sources, move `TrophicMode::Chemotrophic` from `Declared` to
`Witnessed`, and unblock hydrothermal vents as free evidence that the
mechanism is not underworld-special-cased. Its §7 adds a fourth, and calls
it a precondition on rung 3: **measure whether lithology-derived energy
varies BETWEEN worlds**, not merely within one.

## 2. Keystone: energy is a U in depth, and nothing today can see the U

`domains/climate/src/underworld.rs` already states the shape of the answer,
in the module doc of the authored community corpus:

> `ENERGY` — ... Underground the supply is **detrital import** near the
> surface and **chemolithotrophy** off the geothermal gradient at depth, so
> the axis *inverts* with depth rather than falling with it (spec §4.4).
> That inversion is the corpus's central claim and is measured, not
> asserted, by `tests/underworld.rs::energy_is_not_monotone_in_depth`.

That test asserts the **shape**, not merely non-monotonicity: the mid-ladder
`Deeps` trough must sit strictly below both the shallow half and the deep
half. So the project has already committed, in shipped and tested code, to a
U-shaped energy profile with its trough at `Deeps`.

**A U cannot be seen at one sample per column.** `subterranean_substrate_field`
evaluates once per vertex, at the cave's `depth_reach_m` — the bottom of the
U. Every rung in a column shares that one reading.
`MAP-per-rung-substrate` carries this as a known, unowned deficiency.

This is why per-rung resolution is not a refinement of this campaign but its
enabling condition: **at one sample per column, the campaign's central claim
is not expressible.** That settles §4.4 below.

## 3. What already ships

Verified against the tree at `7576eca00`, 2026-08-26. This section exists
because the campaign turned out to be far less construction and far more
connection than the metaplan assumed, and a later reader should be able to
check that claim rather than take it.

### 3.1 The ruler exists, and something already authors values on it

`hornvale_kernel::ENERGY` is environment-axis id **1**, `AxisValence::Scalar`,
values in `[0,1]` (`kernel/src/ecology.rs:348`). It is not new and this
campaign does not add it.

`domains/climate/src/underworld.rs` authors values on it for 22 underworld
communities, on five levels whose doc comments name the mechanism:

```
E_INERT    0.00  no import from above and no exploitable chemical gradient
E_LEAN     0.25  seepage-borne traces only
E_FED      0.50  a working base -- a stream's organic load, or a modest chemical one
E_RICH     0.75  a strong base: direct detrital delivery, or SULPHIDE OXIDATION AT DEPTH
E_TEEMING  1.00  a whole channel's load at one point, or FULL CHEMOLITHOTROPHY ON A HOT GRADIENT
```

Two of the six sources this rung derives are already named, by hand, in the
top two levels of a shipped corpus. **That corpus is this campaign's
validation target and not its output** — the corpus says what a *named
community* has; this rung says what *a place* has.

### 3.2 The function shape exists, and this rung is the missing third sibling

```
axis         pure fn                                  per-vertex field                   state
-----------  ---------------------------------------  ---------------------------------  -------
WATER        chamber_moisture(depth, wt, porosity)    (inside subterranean_substrate)    SHIPPED
SUBSTRATE    subterranean_substrate(...)              subterranean_substrate_field(...)  SHIPPED
ENERGY       --                                       --                                 ABSENT
```

`chamber_moisture` is the template in every respect that matters: a pure
scalar of physical inputs, `[0,1]`-valued, **already parameterised by
`depth_m`**, with a calibration-free `_at_reach` sibling so the sweep that
chose its constant is reproducible from the tree. This rung copies that
posture rather than inventing one.

### 3.3 Every input ships per-vertex

- `GeneratedTerrain::material_at(v) -> MaterialBuffer` — `silica`, `grain`,
  `induration`, `carbonate`, `metamorphic_grade`, `porosity`
  (`domains/terrain/src/lithology.rs:84`).
- `GeneratedTerrain::geothermal_gradient_at(v) -> GeothermalGradient`
  (`domains/terrain/src/provider.rs:549`), whose module doc already calls
  itself "the deep's energy base".
- `hornvale_terrain::delve` — `rungs()`, `delta_t_range_of(rung)`,
  `rung_at_depth(depth_m, gradient)`, `HABITABLE_CEILING_K = 50.0`.

The registry row's "geothermal (ships)" is `geothermal_gradient_at`. Nothing
in §3.3 is authored by this campaign.

### 3.4 The bands are ΔT classes, not depths

`Band` is six-valued (`Surface`, `Undercroft`, `Shallows`, `Deeps`,
`Underdeep`, `Nadir`); five name rock. A band is a **ΔT interval above the
surface datum**, so the depth of a band varies by vertex with the geothermal
gradient. Per-rung evaluation is therefore ΔT-first, depth-second, which is
the coordinate the ladder already uses.

## 4. Design

### 4.1 The six sources rest on five independent inputs, and this is stated up front

`BIO-subterranean-energy-sources` names six candidates keyed on lithology.
Mapped onto the shipped axes they are **not six independent inputs**:

```
source                keyed on                       independent input
--------------------  -----------------------------  --------------------------
serpentinization      ultramafic  -> LOW silica      \  silica, two bands
iron reduction        mafic       -> LOW-MID silica  /
radiolysis            granite     -> HIGH silica     -  silica, third band
sulphide oxidation    metamorphic_grade              -  metamorphic_grade
methanogenesis        carbonate + porosity           -  carbonate x porosity
geothermal            (ships)                        -  geothermal gradient
```

Three of the six read **one axis at different bands**. That is a real
reduction and the spec states it rather than letting an implementer discover
it: six *named* sources, four *independent* lithological inputs (silica,
metamorphic_grade, carbonate, porosity), plus the gradient. `induration` is
deliberately unused — the metaplan §3.2 measured
`induration x metamorphic_grade` at **0.9818**, so admitting both would be
double-counting one signal under two names. `grain` is unused because no
source names it.

**The named sources survive in the code as named terms** — each its own
function with a doc comment naming the rock and the reaction, in the
derivation-comment discipline `underworld.rs`'s corpus already uses — and
are summed to one value on the `ENERGY` ruler. The registry row's claim that
"the DIFFERENCES between sources motivate ecology, trade, exploration and
mining" is preserved by also returning **which source dominates**, so a later
rung can ask what kind of place this is and not only how rich it is. This is
`MARINE_FORAGE`'s precedent applied deliberately: one axis, one calibration
knob, with the distinction retrievable rather than discarded.

### 4.2 Water gates chemistry

Serpentinization and methanogenesis are water-rock reactions; sulphide
oxidation needs a fluid path. Energy at a rung therefore reads **that rung's
moisture**, which per-rung resolution supplies. This is the first place the
per-rung decision pays for itself inside this campaign rather than in rung 3.

### 4.3 Depth: reproducing the U

Each source carries its own depth behaviour, and the U of §2 is their sum,
not an imposed shape:

- **detrital import** — falls with depth from the surface (this is the
  shallow arm, and it is the one term that is not chemotrophic);
- **serpentinization, radiolysis, iron reduction** — rock properties,
  approximately depth-flat;
- **sulphide oxidation** — needs oxidant from above meeting sulphide from
  below, so it peaks at intermediate depth;
- **geothermal / chemolithotrophy** — rises with ΔT (this is the deep arm).

**The U is a prediction of this design, not an input to it.** No term is
shaped to produce a trough. Whether the sum troughs at `Deeps` — where the
authored corpus puts it — is measured, and a failure is a finding recorded
with its number, never a retune (decision 0016; the `energy_is_not_monotone_in_depth`
doc states the same rule for the corpus).

### 4.4 Per-rung resolution, across all three occupied axes

**Nathan's ruling, 2026-08-26**, taken with the cost stated. `ENERGY`,
`WATER` and `SUBSTRATE` all resolve per rung; `MAP-per-rung-substrate` is
discharged.

Evaluation point: the rung's **ΔT midpoint**, not its top. This is the
registry row's own prescription ("the top makes rank 0 degenerate") and is
inherited rather than re-derived. `Band::Nadir` has an open top
(`delta_t_range_of` returns `(50.0, None)`), so it has no midpoint and is
evaluated at the cave's own `depth_reach_m` instead.

**That gives the campaign a free positive control.** Today every rung reads
`depth_reach_m`; after this change `Nadir` still does. So the deepest rung's
substrate and moisture must be **byte-identical before and after**, while
shallower rungs move. A change that moves `Nadir` is wrong, and a change that
moves nothing else is vacuous. Both directions are asserted.

**The consumer question this opens, and its answer.** The two production call
sites (`per_species_suitability` at `windows/worldgen/src/lib.rs:1500` and
`per_species_capacity_at` at `:1797`) consume a `VertexMap<Substrate>` — one
value per vertex, no rung. Per-rung resolution forces a choice about which
rung a species' suitability reads. The answer is the **best rung**: a
subterranean species is credited with `max` over the column's rungs of the
suitability computed *at that rung* — the max of the whole per-rung
suitability score, not the rung with the most energy and not a per-axis max
(which could assemble a chimeric place that exists at no rung). This is what
`delve_seating::seat_at` already assumes when it picks a rung per candidate. Aggregating instead (a mean over rungs) would let a column of five
hostile rungs and one excellent one score below a uniformly mediocre column,
which inverts the seating logic that reads the result.

### 4.5 `CHEMOSYNTHATE` — a new resource axis

`PHOTOSYNTHATE` (id 0) is documented as "ambient solar (**or magical**)
energy fixing". A chemotroph fixes neither. This rung registers

```
CHEMOSYNTHATE   id 6   ResourceKind::Field
```

`Field` rather than `Stock`, matching `PHOTOSYNTHATE`: it is ambient primary
production, not standing biomass. **The id is a save-format contract** and
the basis is append-only for positional reasons the kernel's own doc
explains; id 6 is the next free id and nothing is renumbered.

Riding `PHOTOSYNTHATE` instead was considered and rejected: a world where the
sun goes out must not keep feeding its chemotrophs, and one axis cannot say
that. `MARINE_FORAGE`'s doc records the cost of the opposite choice —
conflating axes to save a calibration knob, with the split deferred to a
named registry row — and this rung declines to buy that debt for a
distinction the whole program exists to draw.

### 4.6 Chemotrophy becomes witnessed

One kind gains `TrophicMode::Chemotrophic`. This is the visible moment, and
it forces the two tests The Gossan left as a handoff:

- `chemotrophic_is_declared_and_unwitnessed` — its assertion must change. Its
  own failure message says so.
- `sanctioned_thermal_keys_are_pairwise_distinct` — **replaced, not
  deleted**, with a direct per-kind pin on `trophic_mode`. Its message is
  explicit that deletion loses a guard the property was standing in for, and
  The Gossan's review *demonstrated by mutation* that every other guard stays
  green when the thermal key is duplicated.

**The kind is not rung 4's tenant.** Rung 4 is "something that eats the
budget and spreads"; this rung authors something that merely exists where the
rock is rich. Choice of kind is an implementation decision constrained here
only by that: it must not spread, and it must not require naming
(`thaumic` stays 0.0, nothing named — metaplan §4).

### 4.7 The vent unblock

`marine_forage_supply_field` gives `Biome::HydrothermalVent` a productivity
of `0.02` and its comment says why: "Chemotrophic in reality; not modellable
as forage yet (BIO-chemotrophy)." That refusal is now false and the comment
is its own repeal condition. A vent's productivity derives from the same
mechanism as a chamber's, which is the point — metaplan §4 calls it "free
evidence that the mechanism is not underworld-special-cased."

## 5. Task 0 — the harvest

`main` contradicts itself today, and the contradiction is this program's own:

- the metaplan (line 39) cites
  `windows/worldgen/tests/suite/winze_scale_probe.rs` for "chambers 3.264x"
  and calls `BIO-underworld-has-no-energy`'s size clause falsified;
- that file **does not exist on main**;
- `book/src/frontier/idea-registry.md:622` still reads "0.50-0.80x".

Nothing mechanical objects: a path or registry ID named in `docs/` prose
resolves against nothing. So this rung opens by harvesting the measurement
stratum of `campaign/the-winze` (unmerged; 25 ahead, 403 behind):

- **three registry rows** — `BIO-subterranean-energy-sources` (which
  metaplan §4 says this rung must author if The Winze does not land it),
  `MAP-delving-hazard`, and the corrected `BIO-underworld-has-no-energy`;
- **five probes** — `winze_scale_probe`, `winze_energy_probe`,
  `ore_separation_probe`, `ore_viability_probe`,
  `off_lithology_decorrelation_probe`. Each is a single `#[ignore]`d heavy
  test, so the commit gate does not pay for them. Precedent for landing
  measurement with no production code is `underworld_lithology_probe.rs`,
  already on main.

**The probes are re-run against current main, not merely moved.** Their
numbers were taken 403 commits ago, before The Glasshouse's temperature epoch
merged. Whether they moved is unmeasured, and a moved number is a finding
recorded with its value — not a reason to discard the probe and not a reason
to keep the old figure. The metaplan's §3.1 and §3.2 are updated to whatever
the re-run says.

The Winze's **implementation** stratum (its plan tasks 2-8) is out of scope
and is parked, not resumed: written pre-Stope and pre-Drift, with a Ruling 4
that moves every world's history. A future campaign re-premises it.

## 6. Preregistration — does energy vary BETWEEN worlds?

Frozen here, before the derivation exists, per decision 0016 and because
metaplan §7 makes it a precondition on rung 3.

**Hypothesis.** Lithology-derived subterranean energy varies systematically
between worlds, not merely within one.

**Why it is in doubt.** The Winze's own Task 1 assumed ore prospectivity
varied usefully across the map and measured **75% of all land inside a band
0.0067 wide** — a near-constant field everyone assumed had structure. The
same instrument family is in use here, and `mineral_supply_field` reads that
very quantity.

**Criterion, frozen now, as a formula.** Let `E_s` be the multiset of
per-rung subterranean energy values over all cave-bearing vertices of seed
`s`, and `m_s = median(E_s)`. Over the frozen seed set
`S = {1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001}` (n = 12,
fixed here so it cannot be tuned later). Its provenance, stated exactly
because a plausible-sounding one is worse than none: `underworld_lithology_probe`
and `winze_scale_probe` both use the SAME trio `[42, 7, 1234]` — not two
different sets — and `lithology.rs`'s own calibration surveys used
`[1, 7, 42, 99]`. `S` is the union of those two (`{1, 7, 42, 99, 1234}`)
extended to twelve with round arbitrary values, so that the three seeds every
existing underworld measurement was taken on remain directly comparable:

```
    separation = IQR({ m_s : s in S })  /  median({ IQR(E_s) : s in S })
```

— the between-world spread of world medians, over the typical within-world
spread. **The prediction is `separation >= 0.25`.**

Below that, worlds differ by less than a quarter of the variation an
individual world already contains, and rung 3's "not every world is *DOOM*"
cannot rest on this quantity — which is a **finding rung 3 must inherit**,
not a failure of this rung.

IQR on both sides deliberately: it is the statistic the ore-prospectivity
null was expressed in ("75% of all land inside a band 0.0067 wide"), so a
null here is directly comparable to the precedent that motivates the check.
**A blind zone this leaves, named rather than discovered:** IQR of medians is
insensitive to a single outlier world, so a seed set in which eleven worlds
agree and one is extraordinary reads as a null. If the criterion fails, the
per-seed medians are reported individually before the verdict is drawn.

**Reported either way, with its number, in the chronicle.** A falsified
prediction is a finding; the count of post-unblinding changes is zero by
construction because this paragraph is committed before the derivation is.

## 6a. Nathan's ruling on the depth profile (2026-08-26, post-measurement)

Recorded here because it changes what rung 3 inherits, and because it was
made **after** Task 5's measurement — so its status as a ruling rather than a
rescue has to be legible.

Task 5 measured the derived per-rung profile and found it **monotone**:
medians `0.169 / 0.201 / 0.265 / 0.281 / 0.281` from `Undercroft` to `Nadir`.
§4.3's prediction that it would trough at `Deeps` is **falsified and stays
falsified**; no source constant was changed and the committed test now
asserts what was measured.

**Nathan's ruling: "Nothing should be dead by default, but it's good to have
a significant amount of variation."** So the trough was never a design goal —
it was a prediction inherited from `underworld.rs`'s hand-authored corpus,
and this spec's §2 treated that corpus as a target to reproduce. That
framing was a choice of mine and it is the part that does not survive.

Three consequences:

1. **The monotone result serves the program's actual goal.** The Underworld
   Larder exists because capacity was computed from insolation, so a people
   800 m down was fed by sunlight. A field that rises with depth means the
   deep underworld feeds itself, on rock, with no reference to the star. That
   is the thing the arc was for.
2. **The corpus and the field now disagree about the middle depths**, and
   `energy_is_not_monotone_in_depth` still pins the corpus's version. That
   disagreement is real and is NOT resolved here. A live possibility is that
   it is not a contradiction at all: the corpus is 22 **named exemplar
   communities**, the field describes **every chamber**, and "a flooded sump
   gallery is poor" can be true while "the median chamber at that depth is
   poor" is false. Comparing a median-over-all-chambers against a corpus of
   recognizable named places may be a category error in this spec's §2.
   Rung 3 should preregister that question properly rather than inherit this
   spec's framing of it.
3. **The success axis moves from SHAPE to SPREAD.** What the world needs is
   dusty halls *and* fungal forests at the same depth — collapsed ecologies,
   blighted ground, realms of the undead. Everything measured to date is a
   median, which cannot distinguish a world of identical chambers from a
   world of wildly varied ones. Task 6 gains criteria S1/S2/S3, frozen before
   the code that would move them.

**What energy structurally cannot supply, with its existing home**, so rung 3
does not try to build it here: undead populations
(`BIO-undead-palimpsest` — "carrying capacity is a function of history, not
productivity"; `K = supply × PROD(condition)` cannot bind a population that
consumes no supply); blight and ruin (`BIO-anthropogenic-community`'s
origin-by-tenure grid, one cell of six populated); and collapsed ecology,
which is a **disturbance history** rather than an energy level — the kernel's
`DISTURBANCE` axis is declared, sits in the environment basis, and is
occupied by nothing at all.

## 7. Blast radius

Measured, not reasoned, and incomplete by design — the gaps are named:

- **Production call sites of `subterranean_substrate_field`: exactly two**
  (`windows/worldgen/src/lib.rs:1500`, `:1797`).
- **Kinds with `HabitatRealm::Subterranean`: three** — `rust-monster`,
  `xorn`, `drow` (`domains/species/src/lib.rs:2461/2465/2480`).
- **Not a save-format epoch on present evidence.** The derivation is pure,
  takes no stream draws, and changes no seed label or stream consumption
  order. It moves values for a given seed, which the commit-to-commit
  determinism standard permits and `make rebaseline` absorbs.
- **The census is the open question.** Its named capacity columns are
  `distinguishable-capacity-{bugbear,goblin,kobold}`, none of them
  subterranean — which is **not sufficient** to conclude the census does not
  move, and must not be reported as if it were. A task measures it with
  `make lab-diff STUDY=the-census`. A census refresh is an autopilot
  carve-out requiring Nathan's explicit authorization, so if the number is
  non-zero the campaign stops and asks rather than regenerating.
- **The field is weakly pinned and a mutation control is mandatory.**
  `underworld_conditions_probe.rs:129` records that neutralising the depth
  this field derives left "all 614 worldgen tests green". A green suite is
  therefore not evidence that a change here was seen.

## 8. Deliberately not in this rung

- **No carrying-capacity ceiling.** That is rung 3, and the metaplan holds an
  open design question for it (amendment C.3's symmetric budget) that Nathan
  has not ruled on. This rung produces the field; it does not decide what the
  field supports.
- **No tenant.** Rung 4.
- **No unification with `windows/locale`'s `EnergySource`.** Metaplan §5
  defers it until this rung knows what it needs; a `{ Sunlit,
  Chemosynthetic, Geothermal }` enum one layer over with one consumer is a
  guess at a shared vocabulary, not evidence of one.
- **No fix to `BIO-autotroph-physics`.** Metaplan §5: bundling destroys its
  attribution.
- **No flip of `BIO-chemotrophy` to `shipped`.** Its promise spans rungs 2
  and 3 and its status is Nathan's call.
- **No resumption of The Winze's implementation stratum.** §5.

## 9. Flagged for G3

1. **`CHEMOSYNTHATE` as `ResourceAxis` id 6 — a save-format contract.**
   Append-only, next free id, nothing renumbered. Leads this section because
   an axis id is permanent once a world carries a `ResourceVector` keyed on
   it.
2. **Per-rung resolution moves shipped worlds** (§4.4, §7). Ruled by Nathan
   with the cost stated; the census half of that cost is not yet measured and
   is a carve-out.
3. **The between-worlds criterion (§6) may falsify**, and rung 3's central
   promise rests on it. A null here is the most valuable thing this rung
   could produce and is budgeted for as a real outcome.
4. **Task 0 lands another campaign's commits.** Three registry rows and five
   probes from `campaign/the-winze`, re-run rather than trusted.

## 10. Definition of Done

Beyond the standard close (`closing-a-campaign`):

- The metaplan's §3.1/§3.2 figures updated to the re-run's actual numbers,
  and its §7 marked measured with the §6 result.
- `book/src/frontier/idea-registry.md`: `MAP-per-rung-substrate` discharged;
  `BIO-underworld-has-no-energy` carrying the corrected size clause;
  `BIO-subterranean-energy-sources` landed and moved off `raw`.
  `BIO-chemotrophy` untouched.
- The Confidence Gradient (`book/src/open-questions.md`) re-scored if this
  rung moves one of its bets (decision 0030).
- A chronicle entry and a retrospective, both before the merge submission.
