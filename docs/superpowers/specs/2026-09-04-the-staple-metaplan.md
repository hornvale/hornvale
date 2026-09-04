# The Staple — a metaplan: how a dwelling becomes a city

*A staple is two things, and the metaplan needs both. It is the commodity a
place lives on, and it is the staple town — the place the law says goods must
be brought to, which exists only because a hinterland exists to bring them.
Nothing is a staple town by itself. The name is the argument: a city is not a
big settlement, it is the apex of a network, and this document is about the
network.*

**Status.** A metaplan. It ships no mechanism and governs several campaigns.
It carries one committed artifact — the Task 0 probe of §1, which is the
evidence every claim below rests on.

**Provenance.** Opened 2026-09-04 as **The Precincts**, the Rose Window's
rung 2 ("a settlement stops being one undifferentiated blob and gains parts").
The owner's brief asked that the metaplan's worked example be checked against
the code early rather than assumed. It was, and it failed — in both
directions — so the campaign became this instead. `The Precincts` is not
retired; it is **rung R3 below**, and it now has a document explaining what
must be true before it has a subject.

---

## 1. The measurement that refounded the campaign

`windows/worldgen/tests/suite/brief_axis_probe.rs`, run at `f20fdbecb` over
seeds 42 / 7 / 13 / 100 / 1234. Population: **4,002 occupations — 1,006 alive,
2,996 ended.** It is an `#[ignore]`d readout, committed rather than left in
scratch so the numbers below can be re-taken instead of decaying into a claim
with a date.

Rose Window §1b.5 states rung 2's target as: *"a `Trade` + `Seat` +
`Classical` + high-population coastal site draws docks, warehouse row, market,
curia, temple precinct, uptown villas, tenements, walls and gates, an
extramural suburb"*, contrasted against *"a `Backwater` `Agrarian`
`Neolithic` site [that] draws three patterns from the same inventory and is a
hamlet"*. Every axis it names is a `Brief` field. Measured:

```
  axis              measured over 4,002 occupations                verdict
  ---------------   -------------------------------------------   --------------
  notability        Common 4002 / 4002  (alive AND ended)          CONSTANT
  function          Agrarian 3951, Mine 51; Trade/Cult/Fort 0      NEAR-CONSTANT
  tech              alive Classical 1006/1006; ended all four      TEMPORAL ONLY
  peak_population   max ever 86, against a 150 hamlet ceiling      NEVER CLEARS
  people            12-15 distinct alive per world, well spread    VARIES
  coastal           19.2% - 45.5% of alive occupations             VARIES
  strata            0 - 23 ended occupations under a living one    VARIES
```

`Trade & Seat & Classical` is **0 on every seed**. So is `Backwater` +
`Neolithic`. **Both poles of the contrast are unreachable**, so the example
cannot discriminate between them — not because the vocabulary is wrong but
because the index has no values.

### 1.1 Why — three different causes, not one

- **`function` and `notability` are hardcoded literals**, at the one site a
  community is founded: `windows/worldgen/src/history_bake.rs:2682`
  (`function: Function::Agrarian`) and `:2686` (`notability:
  Notability::Common`). Neither is derived from anything. `Bake::open`'s own
  comment says so: *"opens every community `Agrarian` — the engine's default
  and, before this campaign, its only reachable value."*
- **`tech` is a world clock, not a place axis.** It alone is derived, by
  `tech_for(year + tech_offset)`. So it separates *eras* and never two
  contemporaneous places, and exactly one era is ever observable from a
  standing world. The ended population spans all four horizons; the living
  population is 100% `Classical` on every seed. A predicate of the form
  "tech >= Bronze" can therefore never distinguish two living settlements.
- **`peak_population` is bounded by one vertex's carrying capacity — and a
  catchment mechanism the world already has is discarded to make it so.**
  `Bake::eff_capacity` (`history_bake.rs:1738`) is
  `caps_now()[pidx].at(vertex) * factor(era, vertex)` — a single-vertex read —
  and population grows logistically toward it. The scale is
  `SETTLERS_PER_CAPACITY = 100.0` (`windows/worldgen/src/lib.rs:756`),
  documented as "settlers a maximal-suitability vertex supports" and tuned so
  seed 42's live settlement count lands in a walkable band. **It is a
  save-format constant** — "changing it re-places every world".

  **The half that matters most was found during the close's book sweep, not
  during the design.** Settlement GENESIS does not read a single vertex: it
  reads a catchment. `domains/demography/src/flow.rs` is *"the terrain
  `drainage` algorithm with the gradient flipped — people climb the K-gradient
  as water descends elevation"*, and `condense.rs` reads settlements off that
  field as attractors whose **catchment** population clears a threshold, with
  `Sigma population == Sigma K` holding exactly because settlements *partition*
  the budget rather than each sampling a local value
  (`book/src/domains/settlement.md`). So the world already models population as
  a watershed. **The deep-history bake never sees it**: `history_bake.rs`
  contains no read of `flow`, `condense`, `Condensation` or any accumulation,
  and `caps_by_era` hands it a per-vertex `CapacityMap`. The catchment that
  decides where a settlement *is* is discarded the moment history starts
  deciding how big it *gets*, and the community regrows logistically against
  its own vertex's K alone. That is why a genesis catchment averaging ~22
  people yields a bake peak of 86 and never a city.

### 1.2 Corroboration nobody had to go looking for

- **`domains/history/src/flesh.rs:601 structures_of` is already a
  settlement-scale composition, and six of its eight `Structure` variants are
  unreachable.** `Market` needs `Trade`, `Shrine`/`Temple` need `Cult`, `Wall`
  needs `Fort` — all three functions occur zero times in 4,002 records — and
  `Longhouse` needs `peak_population >= 200` against a measured maximum of 86.
  Every settlement in every world is one to three `Hut`s plus a `Granary`
  (or a `Mineshaft`, 1.3% of the time). Consumed by
  `windows/almanac/src/history.rs:798`.
- **Dead branches downstream.** `flesh.rs:538` tests `Notability::Seat`;
  `:466` and `:543` test `Function::Fort` and `Function::Cult`. And in the
  vessel, `pattern::role_for`'s index-2 arms select `Role::Hall` on `Seat` and
  `Role::Shrine` on `Cult` — so **two of the seven authored chamber roles are
  unreachable in the shipped path.** (Derived from the measurement plus the
  match arms; not separately measured, and worth measuring before either role
  is relied on.)
- **The idea registry already said it, in as many words.**
  `SOC-species-scale-mechanism`: *"today five peoples share one climate-only
  capacity field and a global GROWTH_RATE, so scale varies in magnitude, never
  kind — why every settlement is a hamlet."* The consequence for the district
  rung had not been drawn.

### 1.3 A registry correction this campaign owes

`SOC-dense-settlement` (ratified 0102) says at most one alive community per
geosphere cell, *"`Bake::vacant_habitable` enforces it"*. **That function no
longer exists**, and the change was properly ratified: **decision 0145**
(2026-08-18, relating 0102 and 0143) re-keyed the index on `(CellId, DelveRung)`
because "the exclusion rule the world actually wants is one community per
*place*, and a cell is not a place once the world has a vertical coordinate".
The enforcer is now `Bake::vacant_for` (`history_bake.rs:1751`), keying
`node_index` by **`(vertex, rung)`**, not by cell — so a drow hall and a
human town already coexist in one cell, by design (The Underworld, spec §4.6).
What remains forbidden is two communities at the *same rung*. Three doc
comments still cite the vanished name, one of them (`cli/src/systems.rs:856`)
quoting the registry row back at itself. The row's *intent* stands; its
mechanism claim is stale, and the work it implies is smaller than it reads.
**The decision log did its job here and the registry row did not** — 0145
landed and the row never absorbed it. That is the failure mode worth naming:
nothing mechanical propagates a ratified amendment to the live index.

---

## 2. The wheel

The seven rungs do not form one ladder. Sorting them by **characteristic
rate** splits them cleanly in two, and the split predicts the epoch column
exactly:

```
  rung                                rate           epoch   arc
  ---------------------------------   ------------   -----   ---------
  R1  a dwelling belongs to a people  derivation     no      READING
  R2  a building's shape has a reason derivation     no      READING
  D1  a settlement has worked land    years          YES     DYNAMICS
  D2  more than one thing flows       seasonal (*)   YES     DYNAMICS
  D3  the flow returns downhill       generations    YES     DYNAMICS
  D4  places specialize               generations    YES     DYNAMICS
  D5  a city                          centuries      YES     DYNAMICS
  D6  decay, and refounding on ruin   centuries      YES     DYNAMICS
  R3  districts   (was The Precincts) derivation     no      READING
```

(*) D2 is the only rung with a native rate already in the code: the bake
already steps `PHASES_PER_YEAR = 12`.

**The READING arc** is pure functions over a finished world. Nothing commits,
so nothing epochs — the same property that let The Plat's reading of a descent
plan move not one byte of any plan (decision 0646), and the same property
decision 0069 gives the vessel by keeping `Interior` unserialized. These rungs
are cheap, immediately visible, and independently orderable.

**The DYNAMICS arc** gives the bake mechanism. Every rung moves world
identity, so every one costs an epoch, a census re-baseline, and the
conversion of history-adjacent study pins from values to invariants. They
**compound**: D2 changes populations, so D1's calibration must be re-taken.
That compounding is the metaplan's true cost and it is larger than any single
rung's.

### 2.1 It is a partial order, not a chain

Drawing this as a ladder would smuggle in sequencing that is not real — the
mistake Rose Window §1a.6 caught when it turned "templates *or* a solver" into
rungs. The honest dependencies:

```
  READING   R1 --> R2 ------------------------------------> R3
                                                             ^
  DYNAMICS  D1 --> D2 --> D3 --> D4 --> D5 --> D6 -----------+
                    |                    ^
                    +--------------------+
                    D5 needs D2's flows directly, not only through D4
```

- **R1 depends on nothing.** It can start today.
- **R3 needs the dynamics arc for its SUBJECT, not for its MACHINERY.** The
  district composer can be built and proven at R1's cost against the
  settlements that exist, and gets richer when cities arrive. This is the
  sequencing result worth having: the reading arc never has to wait.
- **D1 does not need D2.** Land can be worked before anything is exchanged.

### 2.2 Why a wheel, and the half that was missing

Negating "the sequence ends at a city" gives "it ends at a ruin" — and the
measured world lives there. **2,996 ended occupations against 1,006 alive,
with 0–23 ruins stacked under each living one.** A sequence that only builds
upward describes the minority case. Closed as a cycle:

```
    founding --> growth --> surplus --> subordination --> apex
       ^                                                   |
       |                                                   v
    refounding <-- ruin <-- abandonment <-- decline <-------+
```

D6 is that bottom arc, and the stratigraphy the probe measured is its
observable: a place is usually built on the wreck of an earlier one. The Plat
already established the vocabulary for reading it — tenancy as a *tense*
rather than a flag (decision 0649) — over the underworld. D6 is that argument
on the surface.

---

## 3. Hornvale is already a stock-and-flow model, with one commodity, and the commodity is people

This is the metaplan's central mechanical finding, and it reframes the whole
dynamics arc from "build a supply network" to "un-conflate and complete the
one that exists".

`Community` (`history_bake.rs:1168+`) carries **two stocks**:

- **`population: f64`** — inflow is logistic growth against `eff_capacity`;
  outflows are pressure mortality, remittance, and flight.
- **`stores: f64`** — *"Accumulated wealth — tribute, stores, the granary.
  Feeds raiding strength but is **NEVER eaten**: it does not enter the
  pressure term... Lost with the community when it closes."*

and **one flow** between them (`history_bake.rs:3122-3137`):

```rust
let remittance = rel.assessment.min((harvest + bleed) * (1.0 - conceal));
...
self.communities[sub].population   -= remittance;
self.communities[rel.patron].stores += remittance;
```

Read the units. **A tribute payment converts a subordinate's *people* into a
patron's *wealth*, at one to one, and wealth converts back to nothing.** The
supporting machinery is real and calibrated — `conceal` is a modelled
information asymmetry ("the dominant taxes what it can SEE — the land, never
the granary"), `FLIGHT_BURDEN` is the threshold at which a vassal runs,
`ASSESS_RATE = 0.025` is deliberately set at `GROWTH_RATE/8` so the demand
binds over the middle of the capacity curve rather than decoratively, and a
seasonal harvest curve keyed on the site's latitude and biome resolves harvest
banking against winter draw-down.

So the river of people runs. It runs into a **sink**: a reservoir with one
inflow and no outflow but annihilation.

Three consequences organize the dynamics arc:

1. **The commodity must be split before anything else is added.** People and
   food are the same number today. D2's first job is not "add fuel and stone",
   it is to make `population` and a subsistence stock distinct quantities with
   their own units — after which adding a third is a pattern rather than a
   redesign.
2. **The return flow is where the three dead functions come from.**
   Re-instantiated in metabolism — capillaries to veins to heart — the form
   predicts flow in both directions; a heart that only takes is a tumour. What
   flows back down a tribute relation is protection, goods, and legitimacy,
   and those are precisely `Fort`, `Trade` and `Cult`. **A function is not a
   label read off a field at founding; it is the downhill half of a relation.**
   That supersedes this campaign's own first recommendation, which had
   `function` derived from a catchment (ledger #3).
3. **Climate already reaches capacity; it does not yet reach a city.**
   `caps_by_era` is indexed by era and `eff_capacity` multiplies by
   `factor(era, vertex)`, so a cooling era already lowers capacity, raises
   pressure and kills communities — the chain from climate to collapse is
   built. What is missing is the *middle*: a city's capacity is its own
   vertex's, so it cannot be starved by its hinterland failing. D1 is that
   link, and it is the same link the return flow needs. **One mechanism, two
   payoffs** — which is the argument for D1 preceding D2 rather than the
   reverse.

### 3.1 Prices, and the reason R3 is last rather than first

A city's population is a *throughput*, not a stock: people arrive, are born,
die and leave, and the standing number is an equilibrium of flows. Once more
than one thing flows and places specialize, **scarcity is measurable**, and a
price is a derived scarcity signal — not authored, not a constant, and
therefore constitutional under "models author, dice roll" (decision 0009) in
the same way carrying capacity is.

The payoff lands on R3. §1b.5's own vocabulary — *"uptown villas, tenements"*
— **is a rent gradient**, and a rent gradient over a city's lattice is
derivable rather than authored. That is the difference between a district
vocabulary that must be *hoped* to discriminate and one that has a reason, and
it is the same move The Plat made when it replaced "the heart is the hub"
(which named most of the floor) with the graph median under the plan's own
metric.

This is the furthest-out claim in this document and it is flagged as such: it
depends on D2 and D4 both landing, and nothing here measures it.

---

## 4. The rungs

Each entry states what becomes true, what it inherits, and — for a dynamics
rung — the probe that must precede it.

### READING arc

**R1 — a dwelling belongs to its people.**
`Brief::people` is written at exactly one site (`windows/vessel/src/brief.rs:296`)
and **read at zero sites in the whole vessel**; the module doc already said
"`tech` and `people` are carried and not read at all". So a bugbear warren and
a human cottage are byte-identical given the same seed. This rung wires
`people` into the chamber-band pattern vocabulary. It is the axis Rose Window
§1a.6's culture argument actually names — *"a people's houses look alike, and
that is what makes them a people's houses"* — and it has 12–15 values per
world already. **No epoch at the chamber band** (decision 0069); reading it at
the LOCALE band would feed `warmth_at` and a creature's thermal drive, which
is committed history, and that is exactly what `Pattern::at_locale` gates.
Inherits The Blocking's composer. **Depends on nothing.**

**R2 — a building's shape has a reason.**
`structure_at` draws chamber count and links from the locale seed alone —
*"The brief is a GATE and never a parameter of the draw"*
(`windows/vessel/src/structure.rs`) — so a cave and a village get identically
distributed structures, and `links` is a path graph in depth order. Three
source comments already name this campaign's predecessor as the one that must
revisit that (`structure.rs:43`, `:104`, `session.rs:7073`); `Session::further_in`
depends on the path-graph reading. Inherits R1's vocabulary, without which it
has nothing to say.

**R3 — districts.** *(the original The Precincts.)*
A settlement gains parts. Machinery: the band-agnostic `Pattern`/`Attach`/
`compose`/`permits` composer, plus `windows/worldgen/src/plat.rs`, whose own
doc records that it reads no seed, vertex, terrain or ledger *"which is what
lets The Precincts lift it over a district graph"*. Buildable and provable at
R1's cost; **its subject arrives with D5, and its best mechanism with §3.1.**
Note that `built_rooms` (`windows/vessel/src/liveness.rs:8719`) gives a
settlement **exactly one walk-band facet** (1.126 km side) and names the gap
itself: *"widening it to a settlement's outskirts or worked fields is a real
question... a later campaign's to ask."* That widening is D1, not R3.

### DYNAMICS arc

**D1 — a settlement has worked land.** **Restated after §1.1's close-time
finding: this rung is not "add a catchment", it is "make the two halves of the
model agree about what feeds a settlement".** The catchment exists
(`domains/demography/src/flow.rs`, `condense.rs`) and genesis uses it; the bake
discards it. Reconciling them is a smaller and better-founded change than
inventing a mechanism, and it inherits `flow`'s determinism properties for free
— it draws nothing and is "integer-and-comparison only". `domains/topology/src/route.rs:166
least_cost_from` remains available where a TRAVEL-cost catchment is wanted
rather than a K-gradient one, and choosing between them is part of the rung.
**Probe first, and it can falsify the rung: if catchment accumulation is
spatially flat, every catchment sums alike and this is a uniform rescale in
disguise, which is not a pathology and not a city either.** The genesis
figures are the place to start — a mean catchment of ~22 with 182 settlements
on seed 42 is a distribution somebody can already read the spread off.

**D2 — more than one thing flows, and some of it by exchange.** Split people
from subsistence (§3, consequence 1); add a voluntary exchange beside the
coercive one. **Probe: does adding exchange destabilize a demography
calibrated over a 200-seed census?** The existing coercion model is tuned to
a coupling (`ASSESS_RATE` at `GROWTH_RATE/8`) that a second channel may break.

**D3 — the flow returns downhill.** `stores` gains an outflow. Protection,
goods and legitimacy move down a relation; `Fort`, `Trade` and `Cult` become
derivable as the downhill half. **Probe: what share of relations produce a
non-`Agrarian` function, and is the distribution a gradient or a cliff?**

**D4 — places specialize.** A place stops growing its own food because
someone else's reaches it. Depends on D2; this is the rung that makes
`function` a consequence rather than a label, upholding §1b.4's binding rule
that the brief is a coordinate and never a catalogue entry.

**D5 — a city.** The apex where flows converge, with `notability` derived and
comparative — a place whose catchment dominates its neighbours' *is* a seat.
Requires revisiting `SETTLERS_PER_CAPACITY` and, per §1.3, the *same-rung*
half of `SOC-dense-settlement` only.

**D6 — decay and refounding.** The bottom arc of §2.2's wheel, and the
majority case. Inherits The Plat's tense vocabulary (decision 0649).

---

## 5. Standing rules

1. **No dynamics rung starts without a preregistered Task 0 probe that could
   falsify it.** Decision 0016 already requires the freeze; this states where
   the burden falls hardest. The unpredictable rungs are exactly the expensive
   ones, and this campaign's own probe is the exemplar — it falsified §1b.5
   before a line of design was written.
2. **A reading rung never commits; a dynamics rung always epochs.** If a
   proposed reading rung turns out to commit, it has been mis-classified —
   re-place it before building it.
3. **Each dynamics rung invalidates its predecessors' calibration.** Budget
   the re-measurement as a line item in the rung's own plan, not as a
   surprise at its close.
4. **A function, a notability, or a tier is derived or it is absent.** Never a
   label assigned by rank or by table. §1b.4 binds and this metaplan does not
   relax it.
5. **A number in this document is a claim with a date.** Re-run
   `brief_axis_probe` rather than quoting §1; the cost figures in the Rose
   Window metaplan went stale exactly this way.

---

## 6. What this metaplan does NOT decide

- Which rung is built next, beyond observing that R1 depends on nothing and
  costs no epoch.
- The catchment's shape — radius, discount, whether catchments partition
  (a watershed divide, Christaller's lattice) or overlap. D1's probe informs
  it; nothing here chooses.
- Any price mechanism. §3.1 argues the payoff and explicitly measures nothing.
- Whether `SETTLERS_PER_CAPACITY` is raised, replaced, or left alone.
- The chamber-band pattern inventory, the connectivity rule, whether a
  district is a `Role` or a new type, and how extent is represented. Those are
  R3's spec, and R3 is downstream.
- The `people` axis's own vocabulary: fifteen peoples do not imply fifteen
  authored inventories, and the compression is R1's design problem.

## 7. Consequences for the record

- Rose Window **§1b.5's worked example is superseded** — falsified in both
  directions by §1. Its *machinery* claim ("two bands, one composer, one
  validator") is untouched and still correct.
- Rose Window **§1b.11 is narrowed**: it registered the city case as a
  separate history campaign *and* wrote a worked example that depends on it,
  which is the contradiction this metaplan resolves.
- **`SOC-dense-settlement` needs its mechanism sentence corrected** (§1.3).
- `CLIENT-district-patterns` should point here: the district rung is unbuilt
  because it has no subject, not because the composer is missing.
- The Circuit metaplan's unpaid non-goal — *"The Plat records what it learns
  for it"* — is discharged by §2, §2.2 and §4's R3.
