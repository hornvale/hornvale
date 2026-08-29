# The Winze — design

*A winze is a shaft driven downward from inside a working. It is the part of a
mine that goes looking.*

**Status:** spec, awaiting G3.
**Autopilot:** engaged. Ledger at `.superpowers/sdd/decision-ledger.md`.
**Sibling:** supersedes the mechanism of **The Planes**, whose transit-realm
scope returns to the Chorography's campaign-3 slot unbuilt. That spec lives on
`campaign/the-planes` and is deliberately not linked here — it may never merge,
and §1 below states its falsification chain inline so this document stands
alone.

---

## 1. What occasioned it

The Planes proposed three mechanisms and measured two of them dead. The
sequence is the reason this spec exists and is worth stating plainly:

1. **A wound is a cell property gated on ancient crust.** Measured: 0 / 0 / 36
   scar cells and **zero** occupations over one, across seeds 42 / 7 / 1234.
   Falsified.
2. **A wound is the deepest terminus of a fraction of caves, selected by
   rank.** Better — it removed an absolute threshold on a quantity whose
   distribution nobody had measured. But still geometry: it placed wounds *by
   depth*.
3. **Nathan, 2026-08-19:** depth neither guarantees nor precludes one. *"It's
   just that if the cave reaches one, it's not likely to go further."* The
   causality is inverted. A wound does not appear because a place is deep; a
   place is deep **because a wound stopped the digging there.**

**"A fraction of the extreme minima" is therefore a survivorship effect, not a
placement rule** — and the thing that *finds* a wound is a culture delving, not
a cave existing. A natural void discovers nothing.

That reframing is cheaper than either design it replaces: no order statistic,
no fraction constant, no clamped-distribution problem. But it needs a
substrate that does not exist, and finding that out is what makes this its own
campaign rather than a third attempt at the last one.

## 2. Keystone

> **A delving stops where it finds something. What it found is legible only
> to the people it killed, and only for as long as anyone remembers.**

## 3. The findings this rests on

All verified in the tree, not inferred.

### 3.1 Every occupation in every world is `Agrarian`

`windows/worldgen/src/history_bake.rs:2260` contains the **only**
`Function::` assignment in production code:

```rust
function: Function::Agrarian,
```

`Function::{Mine, Trade, Cult, Fort}` are constructed nowhere outside test
fixtures. There are no mines in any world. There never have been.

### 3.2 Four of five vestige kinds cannot occur

`vestige_from_occupation` maps function to kind:

```
Function::Mine        -> AbandonedDelving   UNREACHABLE (no mine exists)
Function::Fort|Cult   -> SealedVault        UNREACHABLE (neither exists)
everything else       -> BuriedRuin         the only live kind
pre-human scar        -> GateScar           measured 0/0/36 cells
the deep sealing itself -> NaturalSeal      same gate as GateScar
```

The committed seed-42 gallery page carries a **five-entry legend** for this
taxonomy. One entry can occur.

### 3.3 An occupation has no depth

`Community.rung: DelveRung` exists build-local in `history_bake.rs:942`.
`OccupationRecord` carries no depth at all, so *"how deep did this delving
get"* is unsaved and unaskable from the ledger.

### 3.4 `dread` has no consumer

Read only by `render.rs` (a colour) and one census metric. Nothing in
placement, seating, or history consults it. There is no avoidance mechanism in
the world, in any form.

### 3.5 The knowledge machinery is half-built, and the built half is the right half

```rust
warning_legibility = exp(-(now - end) / WARNING_HALF_LIFE_DAYS)
dread              = base + 0.4 * (1.0 - warning_legibility)
```

**Dread rises as legibility decays.** The world already encodes "the older it
is, the less you know what it is, and the more you fear it." Nothing reads
either number. `KnownChannel` is a *player-session* overlay, not a
culture-level one, so knowledge *between* cultures is not modelled.

### 3.6 The derivations the design needs all ship

- `prospectivity(...)` — exported from `hornvale-terrain`.
- `seat_at(...)` — which rung of a cell's column a people would settle at
  (The Underworld).
- `Cave { kind, deepest_band, depth_reach_m }` — a terminus in metres, with
  `deepest_band == band_at_depth(column, depth_reach_m)` enforced by
  construction.

## 4. The design

### 4.1 A function is derived, not drawn

A settlement founded on rich ore is a `Mine`. `prospectivity_at(cell)` is
shipped and exported, and `Bake::open` already has `site`, `people`, `year`
and `population` in scope.

**CORRECTED 2026-08-19, at plan time.** An earlier draft of this paragraph
said "this is one line becoming a derivation". It is not. **`Bake` has no
terrain** — it imports the `DelveRung` *type* and nothing else from
`hornvale-terrain`, so prospectivity is not reachable from `open` at all. The
change is a precomputed field threaded in, following the shape `caps_by_era`
already sets (`&'a [Vec<CapacityMap>]`, built at the composition root and
borrowed by `Bake`): build a `CellMap<f64>` of `prospectivity_at` once and
hand it to `bake_history`. Precedented and not architecturally hard, but a
signature change rather than a literal swap — and the cost belongs in the
plan, not hidden in a sentence.

It unblocks `AbandonedDelving` on its own.

**The other functions are NOT in scope.** `Trade`, `Cult` and `Fort` each want
their own derivation (a route, a shrine-worthy feature, a defensible seat) and
each is its own argument. This campaign derives exactly one and says so.

### 4.2 A delving has a depth, and the depth is committed

A mine's working depth derives from `seat_at`, deepened over its tenure. It
goes on `OccupationRecord`, because "how deep did they get" is exactly the
question the rest of the design asks and it cannot be re-derived from the seed
once the occupation has ended.

### 4.3 The hazard is per-increment, and it is the whole mechanism

Each increment of delving carries a small probability of breaching. **A
delving that breaches stops** — and ends.

Nothing selects on depth. The survivorship shape falls out: a breached delving
sits at its own maximum by construction, and delvings that end for ordinary
reasons are shallower. "A fraction of the extreme minima" is an **output**.

### 4.4 The consequence is on the finders, not on the future

Nathan's ruling, 2026-08-19. A breach **ends the delving culture** — flees or
is destroyed. It does **not** place a permanent penalty on the ground.

**Future people may move back in.** This is explicit and it is the reason
avoidance-as-field-penalty is refused (§7): a cell that is forever unsettleable
is a scar on the map, not a memory in a culture.

### 4.5 What a later culture knows is transmitted, decays, and can be wrong

Three states, and the model already has the numbers for all three:

```
RECENT      the warning is legible. A later people can read what happened.
DECAYED     warning_legibility -> 0, dread -> high. They know SOMETHING is
            wrong and not what. (MEM-2's floating gap: living memory reaches
            ~3 generations.)
WARDED      SealState::Maintained reads SAFE --- and may be wrong. A ward
            that is being kept is indistinguishable from a place that was
            never dangerous.
```

That last line is the design's best property and it is free: **the model can
be mistaken in the direction that kills people**, without anyone authoring a
deception. It is also decision 0003's source-blindness arriving structurally —
a later culture receives an appearance (dread), never a source (what is
behind the wall).

**Whether a later founding acts on that knowledge is NOT in this campaign**
(§7). The campaign produces the knowledge; consuming it is the next one.

### 4.6 Nothing is named

A breach records that a delving ended by breaching. It does not record what
came through, because nothing knows. `thaumic` stays 0.0; UNI-2 stays
unratified.

## 5. Preregistration

Frozen before the code (decision 0016). Branch tables, not predictions.

### 5.1 Do mines exist, and how many? (before the hazard is written)

```
mines per world, across seeds [42, 7, 1234], once function derives:

  0 on a majority of seeds -> the prospectivity derivation is wrong. STOP.
                              Do NOT widen the ore threshold to manufacture
                              mines; report and let the controller choose.
  1-5 per world            -> viable but thin. Pool across the panel;
                              single-seed claims banned for the campaign.
  6-40 per world           -> proceed.
  >40% of all occupations  -> TOO MANY. A world that is mostly mines has the
                              same defect as one with none. Report first.
```

### 5.2 Does the hazard produce the survivorship shape?

The claim is that breached delvings sit at their own maximum depth **without
being selected for depth**. Measure the depth distribution of breached versus
ordinarily-ended delvings.

```
breached and ordinary distributions are indistinguishable
    -> the survivorship claim is FALSE and the mechanism is decoration.
       Report it as the headline. Do NOT tune the hazard to separate them.
breached are deeper, with overlap
    -> the claim holds. Report the separation and the overlap honestly.
```

### 5.3 Determinism

- The hazard **draws**, so it perturbs `history_bake`'s stream and every
  world's history changes. This is deliberate, is an epoch-scale artifact
  move, and must be re-pinned in the commit that causes it.
- `OccupationRecord` gains a field — a save-format change, additive, never a
  rename.

## 6. Save-format and epoch consequences

**Additive; not a rename; every world's history moves.**

- A new field on `OccupationRecord`, a new `CauseOfEnd` variant, a new
  predicate, and a new draw in the bake. The draw is the big one: stream
  consumption order is a contract, and a new draw inside the epoch loop moves
  every seed's history.
- Keystone fixtures refreeze at merge, from main's tip.
- For an enum widening, **the compiler is the enumeration** — no plan will
  carry a match-site list.

## 7. Non-goals

- **No avoidance-as-placement-penalty.** Nathan's ruling. Future people may
  resettle; the ground is not cursed, the culture is dead.
- **No cursed biome.** `HazardKind::Cursed` is a vestige classification; there
  is no biome, and adding one is a climate-layer argument this campaign does
  not make.
- **No culture-level knowledge channel.** The campaign produces legibility and
  dread with honest semantics; building the transmission layer (MEM-1..5) is
  its own campaign.
- **No `Trade`/`Cult`/`Fort` derivation.** One function, derived, argued for.
- **No metaphysics.** Nothing named; `thaumic` stays 0.0.
- **No transit realms.** The Planes' original scope returns to the
  Chorography's c3 slot, unbuilt.

## 8. Task shape

```
1  MEASURE: does prospectivity separate? would mines exist, and how many?
2  Function derives from ore --- mines exist; AbandonedDelving becomes reachable
3  A delving has a committed depth
4  The hazard, and the CauseOfEnd it produces
5  MEASURE: the survivorship shape (5.2) --- the null is publishable
6  Legibility and dread get honest semantics at a breached delving
7  Narration: almanac + census
8  Book, chronicle, retrospective, decision record
```

## 9. Provenance

Nathan's reframing of 2026-08-19, in conversation, after The Planes' second
mechanism was scoped. Three `ideonomy-plain` passes across the two campaigns;
the one that produced this shape was dimension-identification +
organon-construction over a matrix, on the axes *visibility*, *source*, and
*hierarchicalness* — which surfaced that `source` walks rock → history →
survivors → observer exactly once per state, and that `visibility` splits into
"can you read what it is" (`warning_legibility`, shipped) and "can you tell
it is something" (the physical seal, unmodelled).

Every mechanism in §4 is Nathan's. The contribution of this document is
checking each against the tree and finding that three of the four things it
needs are declared and unproduced.

---

# AMENDMENT, 2026-08-19: a mine is FOUNDED for ore, not reclassified onto it

§4.1 said a settlement founded on rich ore *is* a `Mine`. **Measured false,
and the way it is false is the campaign's most useful finding.** The section
stands above; this replaces it.

## B.1 What Task 1 measured

```
prospectivity = 0.6*setting + 0.3*unrest + 0.1*metamorphic_grade
                setting = 0.1 off a plate boundary, 0.4-0.7 on one

seed 42  land:      min 0.0600  p50 0.0602  p75 0.0667  p90 0.1442  max 0.6771
seed 7   land:      min 0.0600  p50 0.0623  p75 0.1191  p90 0.2113  max 0.7398
seed 7   occupied:  min 0.0600  p50 0.0607  p75 0.0653  p90 0.1213  max 0.5679

occupations in land's top prospectivity decile: 0.48% / 2.75% / 11.81%
  --- at or BELOW the ~10% chance alone would produce, never above
```

Two facts, and the second is the important one:

1. **Prospectivity is a near-constant floor with a thin tail.** Off a plate
   boundary `setting = 0.1`, so most land sits at `0.6 x 0.1 = 0.06`; on seed
   42, 75% of land lies in a band 0.0067 wide. There is almost nothing to
   separate on.
2. **Settlements are UNDER-represented in high-ore ground**, not merely
   indifferent to it — and that is the model being *correct*. Ore is high on
   plate boundaries and unrest; farmland is not. Agrarian siting puts
   settlements where ore isn't, by construction.

## B.2 The general error, and it cost three mechanisms

**A derivation cannot reclassify a population that was placed by a different
objective.** Every occupation in Hornvale is sited by capacity-maximising
agrarian logic. Relabelling some of them as mines cannot work, because that
logic has already put them where ore is not — correctly, and it would be a
worse model if it hadn't.

This is the same error as The Planes' first two mechanisms, which tried to
find wounds among cells and caves the world had placed for unrelated reasons.
Three deaths, one cause: **reclassification, where creation was needed.**

## B.3 A mine is a daughter founded on a different objective

`Bake::grow` already spawns daughter settlements and already scores candidate
sites:

```rust
let dest = traversable_neighbors(self.cur(), site)
    .filter(|&n| self.vacant_for(era, n, dpidx))
    .max_by(|a, b| {
        let sa = caps_now()[dpidx].at(*a) * river_factor(river_prox.get(*a));
        ...  // tie-break: lowest CellId, total_cmp
    });
self.open(people, dest, year, DAUGHTER_POP, Founding::From(parent), ...)
```

It already carries `Founding::From(parent)`. **A mining camp is a daughter
founded on ore instead of on fertility, from a parent that supplies it** —
which is what a mining camp is.

So the change is **a second scoring objective inside one existing site
choice**, not a new founding path:

```
TODAY      score = capacity * river_factor          (one objective)
PROPOSED   an expansion is occasionally a WORKING, scoring
           score = f(prospectivity)                 (the other objective)
           and opening with function = Mine
```

**Genesis is untouched.** That is deliberate and it bounds the blast radius:
genesis is the most byte-identity-sensitive path in the bake, and this design
never enters it.

## B.4 Can anything live where the ore is? Measured: yes

The obvious objection to B.3 is that a settlement founded on ore starves,
since ore tracks unrest and plate boundaries. Task 1b measured it before this
section was written:

```
fraction of top-prospectivity-decile land cells viable for at least one people
                  VIABLE_MIN=2.0     SURVIVE_K=5.0
  worst seed          71.49%            56.51%     (seed 1234)
  range               71-97%            56-97%
```

Per-decile median capacity **does** decline as prospectivity rises on 2 of 3
seeds — the anti-correlation is real and visible — but typical capacity runs
in the 10s-30s against a floor of 2-5, so it rarely flips a cell unviable.
Mutation control: forcing the capacity closure to `0.0` drives the fraction
to 0%, so the non-zero result is not a probe that cannot fail.

**Mines need not be satellite-only.** The design is writable.

## B.5 The viability floor is `SURVIVE_K`, not `VIABLE_MIN`

An earlier brief of mine named `VIABLE_MIN = 2.0` as "the bake's own floor".
**Wrong, and worth recording because a spec that inherits it would compare
the wrong quantities.** Every use of `VIABLE_MIN` in `history_bake.rs`
compares it to a *population* (`if pop < VIABLE_MIN`), never to a capacity.

The floor a genesis founding's own starvation arithmetic uses is
`SURVIVE_K = GENESIS_POP / COLLAPSE_PRESSURE = 10.0 / 2.0 = 5.0`, derived
independently by `niche_breadth_probe.rs`. **Any spec needing a single
viability floor uses `SURVIVE_K`.**

## B.6 What is NOT decided here

- **How often an expansion is a working.** A rate, chosen from measured data
  the way every other constant in this campaign must be — not frozen in prose.
- **The ore scoring function.** Given B.1's near-constant floor, a naive
  `max_by(prospectivity)` will tie across most of the map. The scoring must be
  shown to discriminate before it is adopted, and that is a measurement.
- Everything in §4.2-§4.6 stands: depth, the per-increment hazard, the
  survivorship shape, decaying and fallible knowledge, and naming nothing.

## B.7 Save-format consequence, restated

This changes **placement**, so every world's history moves — a larger blast
radius than the reclassification design had, and the reason B.3 keeps genesis
out of scope. Re-pin in the commit that causes the move; treat an *absence*
of drift as a stop.

---

# AMENDMENT, 2026-08-24: what came through MAY persist and travel — the campaign permits it and models none of it

§4.4 was read, by the session resuming this campaign, as a guarantee that a
breach's consequence stays with the finders. **It never said that.** Grepped
before writing this: the document above contains no occurrence of *spread*,
*contagion*, *propagate*, or *containment*. §4.4 refuses exactly one thing — a
**permanent penalty on the ground** — and §7 names the same refusal as
"avoidance-as-placement-penalty… a cell that is forever unsettleable is a scar
on the map."

Nathan's clarification, 2026-08-24, in conversation: the ruling was that the
harm **could** stay with the finders, not that it necessarily does. Evil that
spreads through several underworld systems is wanted, not refused. This
amendment records the permission, states what bounds it, and defers all of it.

## C.1 What occasioned it: the premise moved while the campaign was parked

Ledger #21 parked this campaign behind The Stope. The Stope merged; The Drift
merged 2026-08-24. Re-measured against the substrate that now exists
(`windows/worldgen/tests/suite/winze_scale_probe.rs`, seeds 42/7/1234,
pooled, per land cell):

```
                       pre-Drift            post-Drift
  chambers        1.648x / 2.361x           3.264x     falsified
  reachable                 0.172x          3.264x     falsified
  runs                      0.253x          0.502x     survives
```

The surface half did not move (11283/19332/11684 land cells, byte-identical),
so every ratio moved because its numerator did. `chamber_exists`'s existence
coin (`EXISTENCE_DENSITY = 0.5`) was deleted, which doubled every count; the
`entrance` axis was deleted, which merged two of the four readings; and
reachability went **7.28% -> 100%**, which moved the third across the line.

`BIO-underworld-has-no-energy`'s **size** clause is falsified: the sole
surviving reading is RUNS, and a run is a container of places rather than a
place. Its **energy** clause is untouched, and C.3 is why that is the half
that matters. Junctions additionally link systems laterally — 4,165 links,
largest network 119 systems at one band, on seed 42.

## C.2 The position the spec never enumerated

What comes through a breach can sit in one of three places, and §4 considered
two of them:

```
  ABSTRACT     a recorded event with no referent          <- what §4.4/§4.6 ship
  MECHANICAL   a permanent penalty on the ground          <- what §7 REFUSES
  ALIVE        a tenant that eats, spreads, and can be
               killed, fled, or bargained with            <- never considered
```

A mind flayer colony, an aboleth, a balrog are the third position. They are
**not** the second: a live tenant is a thing a later people can meet, and a
placement penalty is a number on a cell that nobody can meet. §7's refusal
survives this amendment unchanged, and so does §4.4's sentence as written.

**The connectivity is what makes the third position interesting rather than
decorative.** Pre-Drift, 92.7% of the lattice was unreachable, so a tenant
would have been sealed in a pocket by the geometry — alive in name and
abstract in effect. At 100% intra-system reachability plus lateral junctions,
range is a real quantity for the first time.

## C.3 What bounds it, and why it must be DERIVED

Nathan's constraint, same conversation: not every world may become *DOOM*, the
same way not every world may become *The Walking Dead*. Carpenter's Apocalypse
Trilogy is the shape wanted — three world-ending scenarios that differ from
each other, rather than one that arrives in every world at the same hour.

**A tuned ceiling cannot deliver that and a derived one can.** A constant
capping how far a horror ranges is a knob whose value is the same in every
world by construction; varying it per world is authoring, not simulating.
What varies per world already is the **rock**.

The bounding quantity is therefore the underworld's own productive base —
`BIO-subterranean-energy-sources`' "an Underworld as lush as the Overworld
needs its own productive base" — and the campaign that builds it inherits two
results from this one:

- **How much space it must feed:** 3.264x the surface, all of it reachable.
- **That its inputs are real:** ledger #22 established the six lithology axes
  carry genuine independent signal (the earlier "everything correlates"
  reading was an instrument artifact — the vectors had been sorted
  independently before correlating, which by the rearrangement inequality
  computes the maximum correlation over any pairing rather than the
  correlation of the data). `winze_energy_probe`'s M4 assertions now pin that
  corrected verdict.

**The bound should be symmetric, and this is the amendment's one design
claim.** Flip the polarity of "a spreading horror" and the positive analogue
is a spreading *ecology* — the lush underworld the same row asks for. Both eat
the same rock. One budget with two kinds of consumer is a mechanism; a
special-case cap on monsters is a knob wearing a mechanism's clothes. A world
whose rock feeds a balrog far is the same world whose chemotrophic ecology is
rich, and that coupling is content rather than a cost.

## C.4 What this amendment changes, and what it does not

**Changes — one sentence of permission, no machinery:**

> A breach's consequence **may** persist beyond the delving that caused it,
> and may travel the lattice it was released into. This campaign records that
> a delving ended by breaching and models neither the tenant nor its range.
> §4.6's "nothing is named" is what defers it: nothing knows what came
> through, so nothing can yet say how far it went.

**Does not change:**

- §4.1–4.3. The function derivation, the committed depth, and the
  per-increment hazard are untouched by connectivity — nothing in them
  selects on depth or on reachability.
- §4.4's own sentence. It refuses a placement penalty and continues to.
- §4.5. Knowledge still transmits, decays, and can be wrong. The amendment
  sharpens one case rather than altering it: 57% of cave systems are
  multi-entrance (1.87 apertures per system, pooled), so a maintained ward
  reading safe is now **provably** insufficient rather than merely fallible.
- §4.6 and §7. Nothing is named; `thaumic` stays 0.0; no metaphysics.
- §6. **No save-format consequence.** The permission adds no field, no
  variant, and no draw — recording that a delving ended by breaching is
  already §6's enumeration. A tenant with a range would add all three, which
  is a second reason it belongs to its own campaign and its own epoch.

## C.5 Provenance

Two `ideonomy-plain` passes, 2026-08-24. The first
(dimension-identification + combination over a matrix; cardinality,
reversibility, materiality) established that the consequential substrate move
is **connectivity, not scale**, and that the materiality axis — a chamber is a
place, a run is a coordinate — is what the size clause actually failed on.

The second (substitution + organon-construction over a graph; intentionality,
polarity, animacy) produced C.2's three-position ladder and C.3's symmetry
claim, and found the graph's shape: the delving/depth/finders/knowledge
cluster touches the energy cluster at **exactly one node, the breach**. Two
ideas under one name, joined at a single point — which is why building this
campaign without the energy model is the natural cut rather than a
compromise.

---

# AMENDMENT, 2026-08-29: a working searches outward for its ore

**Nathan's ruling, 2026-08-29, after Task 2 landed.** §B.3's design stands in
every other respect; this changes one thing — the set of sites a working may be
founded on — and freezes a preregistration for the re-measure before the code
is written.

## E.1 What Task 2 measured, and what bound it

```
                        seed 42    seed 7   seed 1234
  mines                       1        13           2
  occupations              1240       661         898
  mine share              0.08%     1.97%       0.22%

  daughter throws           466       292          23
  throws with ANY vacant
    neighbour at/above
    ORE_CUT = 0.24            4        23           4
```

**Neither the rate nor the cut is the binding constraint.** The rate is the
site's own prospectivity and the cut sits on a measured plateau; what binds is
that `Bake::grow` considers only the parent's **direct traversable
neighbours**, and ore is rarely one hop from farmland. That is Task 1's
finding — settlements are under-represented in high-ore ground because
agrarian siting correctly puts them where ore is not — arriving one hop out
and biting the design that was built to answer it.

On seed 42 that is one mining camp in 1,240 settlements. `AbandonedDelving`,
the vestige kind this campaign exists to make reachable, is reachable in
principle and absent in practice.

## E.2 The change

A working is founded by a **ring scan outward** from the parent, not by a scan
of the parent's direct neighbours.

**The argument is from the objective, not from the count.** One hop is the
right radius for the agrarian daughter path: a farm village spreads to the next
field, and the next field is next. A working is founded *because of where the
ore is*, so its search radius must be set by the thing it is searching for. The
two objectives were sharing a radius because they were sharing a code path, and
that is the kind of coupling §B.3 was already unpicking when it split the
scoring — this finishes the split.

**There is precedent in the same file and it is not a stretch.**
`Bake::best_home` (`history_bake.rs:1674`) already resolves a destination by
`self.nearest_ring(from, |ring| …)` for relocation. A working is closer in kind
to a relocation than to a daughter: in both, a people moves to reach something
specific rather than spilling into adjacent room.

## E.3 This is a POST-UNBLINDING change to the mechanism, and the record says so

Decided after seeing 1 / 13 / 2. That must be stated plainly rather than
smoothed over, and two things about it are worth being precise about, because
the difference between them is the whole of this project's preregistration
discipline:

**It is not a retune to rescue a falsified prediction.** §5.1's branch table
was *satisfied*: no seed returned zero, so the STOP row never fired, and the
result landed in the sanctioned `1-5 per world` row — "viable but thin. Pool
across the panel; single-seed claims banned for the campaign." The
preregistration anticipated this outcome and permitted the campaign to
continue. Nothing here is being rescued.

**It is a design change on an axis the preregistration never spoke to.** §5.1
asks "do mines exist, and how many" as a question about whether the derivation
*works*. It does. The question Nathan answered is a different one — whether a
feature a player meets once in 1,240 settlements is *in the world* in any
meaningful sense — and no frozen criterion addressed it, because none was
written. A campaign is allowed to discover that it measured the wrong thing;
what it is not allowed to do is quietly move a number it did measure.

**What would have made this illegitimate**, recorded so a later reader can
check we did not do it: lowering `ORE_CUT` to manufacture mines. §5.1's STOP
row forbids it by name, and Task 2 measured why — at the barren floor you get
42 / 19 / 6 mines, of which 23 of seed 42's 42 stand on prospectivity 0.0600,
which is to say on no ore at all. The cut is untouched by this amendment.

## E.4 Preregistration for the re-measure — frozen before the code

Decision 0016. Computed against the resolver, not beside it: the numbers below
are branch tables over what the existing `ore_siting_probe` and `mines_exist`
tests already report, not predictions from a second implementation.

### E.4.1 Mine population

§5.1's four-row table is **unchanged and still governs** — the acceptance
range for "does the derivation work" does not move because the search radius
did:

```
  0 on a majority of seeds -> the derivation is wrong. STOP. Do NOT widen the
                              ore threshold to manufacture mines.
  1-5 per world            -> viable but thin. Pool across the panel;
                              single-seed claims banned for the campaign.
  6-40 per world           -> proceed.
  >40% of all occupations  -> TOO MANY. Report first.
```

Added for this amendment, because a ring scan can overshoot in a way a
one-hop scan could not:

```
  mines exceed the count of daughter throws that HAVE a workable-ore
  candidate in range
      -> impossible; a founding was minted somewhere other than the scan.
         STOP.

  the ring scan reaches so far that a working's parent is not plausibly its
  supplier (state the radius distribution; §B.3 calls a mining camp "a
  daughter founded on ore FROM A PARENT THAT SUPPLIES IT")
      -> report the distribution before proceeding. A supply relationship
         that spans half a continent is a different design, not this one.
```

### E.4.2 The Task 5 panel — the rule, fixed now, before the hazard exists

Task 5 compares the depth distribution of breached against ordinarily-ended
delvings. Its power is set by how many delvings exist, which this amendment
changes, so the panel rule is frozen here rather than chosen once the answer
is visible:

```
  pooled mines across [42, 7, 1234] >= 60
      -> keep the panel at [42, 7, 1234]. Task 5 pools over it.
  fewer than 60
      -> extend the panel by CONSECUTIVE seeds 0, 1, 2, ... (skipping any
         already in it) until pooled mines >= 60 or the panel reaches 12
         seeds, whichever comes first. Report the panel actually used and
         the count it reached.
```

**Why 60, and why a rule rather than a number chosen later.** The comparison
needs enough in the *smaller* group to be a distribution rather than a handful:
at a breach fraction anywhere near a third, 60 delvings gives roughly 20
breached against 40 ordinary, which is the neighbourhood where an overlap
statement means something. The figure is a floor on sample size, not a target
for the mechanism, and the seeds are taken in a fixed order so the panel cannot
be chosen for its answer.

**Task 4 chooses the hazard rate without reference to any of this.** The rate
is a modelling choice about how dangerous digging is; if it were picked to land
the breached count somewhere convenient, this rule would be laundering a
tuned constant through a sample-size argument. Task 4's report states its rate
and its justification before Task 5 runs.

## E.5 What does not change

- **The cut.** `ORE_CUT = 0.24`, read off `prospectivity`'s own definition
  (`0.6 × 0.4` is where "on a plate boundary" begins) and corroborated by the
  plateau measurement. Untouched.
- **The rate.** The site's own prospectivity, per the field's own doc.
  Untouched.
- **The stream leg.** `settlement/working/v1`, keyed on the parent's
  `(vertex, band, year)`. A ring scan changes which vertex is chosen, not how
  the draw is derived, so no epoch bump and no label change.
- **Genesis.** Still untouched, and still the reason the blast radius is
  bounded.
- **§4.2–4.6, §7, and amendment C.** Depth, the per-increment hazard, the
  survivorship claim, fallible knowledge, naming nothing, and the permission
  that a breach's consequence may travel — all unchanged.
