# The Tense — capacity is written in the eternal present

**Campaign:** the-tense · **Branches off:** `campaign/the-tilth` (it needs that
branch's `per_species_capacity`, which does not exist on `main`) · **Supersedes:**
The Tilth stages 6 and 7, which are patches on the layer below this one

## 1. The thesis

```
  K(s, c)       = Vmax . S(supply(s,c)) . min_a R_a(s, field_a(c))
  eff(s, c, e)  = K(s, c) . [ mask(e, c) ]
```

The era `e` appears **only inside that bracket**. Capacity is a pure function of
*present-day* climate, so the bake plays two thousand years of deep history on
today's map, and the only thing an ice age can do to a cell is switch it off.

Everything this arc has fought over follows from that one fact:

- **The Fallow** needed capacity to vary over time and had to invent a soil stock
  to get it, because the field itself could not move.
- **Stage 6** and **stage 7** were an argument about how a *binary gate* should
  behave — floored elevation, then unfloored temperature — because the gate is the
  only era-varying quantity in the model. Both fixed their target and created a
  new defect, which is the signature of patching the wrong layer.
- **The capacity field and the era mask contradict each other over ~50% of land**
  because they are computed from two different climates.
- **Seed 1234 dies permanently** rather than degrading, because a gate has no
  gradient to degrade along.

The repair is to give capacity a tense:

```
  K(s, c, e)    = Vmax . S(supply(s,c,e)) . min_a R_a(s, field_a(c,e))
```

Then the mask is unnecessary. Cold ground is not *gated*, it is *poor*. A glacial
maximum squeezes a world instead of switching it off, and a species is excluded
by arithmetic rather than by decree.

## 2. What the ideonomy pass found

Beyond the seven defects already measured, three passes (substitution+negation →
dictionary; abstraction-lift+combination → graph; dimension-identification+
tree-finding → notation) surfaced these. Each is independently checkable.

### 2.1 Three incompatible definitions of "habitable"

```
  oracle                        rule                                       used by
  ----------------------------  -----------------------------------------  ------------
  era.habitable  (bake_eras)    land above era sea level AND mean temp     Bake::factor
                                >= FREEZE_C (-10C)
  caps > 0       (vacant_for)   per-species capacity is non-zero           siting, refuge
  glacial_maximum_habitable     full climate rebuild via                   refugia
                                hornvale_climate::is_habitable
```

The first two disagree over roughly half the land. The third computes the
refugia the bake then routes migrants toward, by a rule neither of the others
uses. §3 collapses all three to one.

### 2.2 The fundamental niche is being used as the realized niche

`per_species_capacity` answers "could this species live here **alone**". The bake
uses it to answer "does this species live here". Ecology has kept those apart
since Hutchinson (1957); the codebase has one word and one field. Competition
exists downstream in `coexist::pack` but never feeds back into the capacity the
bake reasons over.

### 2.3 Cells are ~12,000 km² and carry one temperature

40,962 cells over an Earth-sized globe is about 12,450 km² each — Connecticut,
or Northern Ireland. Every such cell is assigned a single temperature, so there
are no warm valleys, no south-facing slopes, no sheltered coasts. **Real refugia
are sub-grid, and this grid has no sub.** That is a structural reason a glacial
maximum wipes a planet rather than thinning it, independent of every other defect
here.

### 2.4 Response curves are symmetric; thermal biology is not

`ConditionResponse` is a Gaussian, symmetric about its optimum. Real thermal
performance curves are left-skewed: a gradual rise to the optimum and a sharp
crash past it, because protein denaturation has no mirror image on the cold side.
A species 20 °C above its optimum should be in far worse shape than one 20 °C
below, and today they are identical.

### 2.5 The constraint structure is flat, and that is why stages 6 and 7 failed

Four axes as peers under one `min()`. But two different *kinds* of constraint are
being expressed through one operator:

- **Viability gates** — lethal limits. Few, steep, and rightly able to reach zero.
- **Performance modifiers** — preferences. Many, shallow, and rightly floored by
  sovereignty.

Mixing them under a single minimum is unstable by construction: a floored axis can
never bind, so whichever axis is unfloored becomes the sole determinant wherever
it dips below the others' floor. That is the *same* bug stage 6 found in elevation
and stage 7 reproduced in temperature. Neither arrangement of floors can fix it,
because the defect is the flatness, not the floors.

### 2.6 The coupling is one-way

Place scores species; species never alter place. Niche construction (`BIO-32`) is
out of scope here, but it is worth recording that the arrow only points one way,
because §3's design does not change that and a later campaign will want to.

## 3. Design

### 3.1 Era-varying substrate — the whole change, in one line of type

`substrate_field(geo, terrain, climate, ..)` gains an era parameter, or takes an
era-adjusted climate. Per-cell temperature, moisture and insolation become
functions of `(cell, era)`; elevation stays fixed but *height above sea level*
moves with the era's eustatic sea level, which the era series already carries.

`per_species_capacity` then takes an era and returns that era's field. Nothing
about its arithmetic changes.

### 3.2 Delete `Bake::factor` and the habitability mask

With §3.1, `eff_capacity(era, cell, people)` is simply `K(species, cell, era)`.
The `-10 °C` snowline stops being a gate on the dynamics and becomes what it
always should have been: a *diagnostic*, reportable in the almanac, binding on
nothing.

Consequences to accept deliberately:

- `step_community`'s `eff == 0.0` climate-eviction branch fires far more rarely,
  because an exact zero is rare in a continuous field. Eviction becomes a
  *pressure* outcome — the land got poor, the population could not be fed — which
  is the mechanism The Fallow wanted and could not reach.
- The three habitability oracles of §2.1 collapse to one: capacity.
- Ocean exclusion must survive the change. It currently rides the era mask's
  `elev >= sea_level`; after this it must ride supply (a terrestrial uptake vector
  has no supply at sea), which is where §2.1's comment already says it belongs.
  **This is the most likely place for a silent regression and gets its own test.**

### 3.3 Two-tier constraints

Split `ConditionNiche`'s axes into **gates** and **modifiers**:

```
  tolerance(s, c, e) = gate(s, c, e) * modifier(s, c, e)

  gate     = product over lethal-limit axes, each UNFLOORED, each able to reach 0
  modifier = min over preference axes, each FLOORED by sovereignty_floor
```

Temperature is a gate. Moisture is probably a gate (desiccation is lethal).
Insolation and elevation are modifiers. The floors then do exactly what BIO-26
claims for them — a well-defended species is never fully excluded *by a
preference* — without ever making anything immune to a lethal limit.

This supersedes stages 6 and 7. Both should be reverted as part of this campaign
rather than carried, because both are arrangements of a distinction this section
removes.

### 3.4 Explicitly out of scope

- **Sub-grid heterogeneity** (§2.3). The right fix is a within-cell temperature
  *distribution* rather than a mean, so a fraction of each cell stays habitable
  through a glacial maximum. Large, and it interacts with the locale/room scale.
  Recorded, deferred.
- **Asymmetric response curves** (§2.4). A one-field change to
  `ConditionResponse` (a skew term), but it re-authors every niche and should not
  ride a physics change.
- **Fundamental vs realized niche** (§2.2), **niche construction** (§2.6), and
  the **subterranean stratum** (`MAP-10`'s deferred ecology rung, which is what
  would let a snowball world keep an Underdark population).

## 4. The cost, measured

This is the campaign's central risk and the reason it is not obviously right.
Measured on seed 42, release build, `windows/worldgen/tests/capacity_cost_probe.rs`:

```
  substrate_field                  151.5 ms
  per_species_capacity             169.5 ms   (6 species; substrate is 89% of it)
  x25 eras (CLIMATE_ERAS)         4237.7 ms   = 4.24 s per world

  memory, one era, 6 species         2.0 MB
  memory, one era, 300 species      98.3 MB
  memory, 25 eras resident, 6 sp    49.2 MB
```

**Read those as CORE-seconds, not wall-clock.** An earlier draft of this section
multiplied 4.24 s by 1000 worlds and concluded "+70 minutes", which was wrong
twice: the census is embarrassingly parallel, and the measurement is off-host.

- **Parallelism is real.** `windows/lab/src/runner.rs:210` spawns
  `available_parallelism()` threads over contiguous seed ranges, so a 1000-world
  study saturates the box. On lefford's 40 cores, 4240 core-seconds is on the
  order of **~2 minutes** of added wall-clock, not 70.
- **The measurement is on the wrong machine.** 169.5 ms was timed on the Mac
  (aarch64). Censuses run on lefford, a 40-core R720 whose 2012-era cores are
  slower per-thread by an amount **this spec has not measured**. The honest
  statement is a ratio, not a second-count, until the probe is re-run there —
  which is why H4 is stated as a ratio.

Census scale is checked rather than assumed: `studies/the-census.study.json` is
`seeds: {from: 0, count: 1000}` × 1 pin set = **1000 worlds**, and its
`metrics: "all"` resolves through `required_depth` to `BuildDepth::Full`, so every
census world does run the bake. (`CLAUDE.md`'s "~2000-world census" presumably
aggregates the other `studies/census-of-*.json`; use the per-study count.)

**So time is probably not the binding constraint — memory is, and only at scale.**
The runner holds a world per thread. At today's 6 species that is 2.0 MB of
capacity field per worker and nothing to discuss. At the several-hundred-settling-
species target this arc exists to reach, one era is **98.3 MB per worker**, and
40 workers is **~3.9 GB of capacity fields alone**. That wall is already there,
independent of this campaign; giving capacity an era axis only makes it matter
sooner, and it is the reason §4's mitigation 2 (stream one era, never hold the
series) is a requirement rather than an optimisation.

Three mitigations, in the order they should be tried:

1. **Hoist the era-invariant work.** Substrate is 89% of the cost, and only its
   temperature and moisture components actually move with the era. Mineral supply,
   elevation and the terrestrial/marine masks are era-invariant and are currently
   rebuilt every call. This alone may be most of the 25×.
2. **Compute per era-change, not per epoch.** The bake's `era_index_for` is
   monotone, so each era is entered exactly once: at most 25 rebuilds per world,
   streamed one at a time, never resident together. §4's memory row for
   all-eras-resident is then irrelevant — which matters enormously at 300 species,
   where one era alone is 98 MB.
3. **Decouple capacity's era resolution from the mask's.** Nothing requires 25
   capacity fields; the deep-time signal is smooth, and 5 with interpolation may
   be indistinguishable. Measure before assuming 25 is needed.

**If none of the three brings it under budget, this campaign should not ship**,
and the fallback is The Fallow's stock — a cheap scalar per cell carrying the
land's memory — as an approximation to a time-varying field.

Given the parallelism above, mitigation 1 (hoisting the era-invariant 89%) is
likely sufficient on its own for *time*. Mitigation 2 is required regardless, for
memory at species scale.

## 5. Preregistration (decision 0016)

**H1 — the oracles agree.** After §3.2 there is exactly one habitability rule in
the bake's path. Verified structurally (no `era.habitable` reference survives in
`history_bake.rs`), not statistically.

**H2 — worlds degrade instead of switching off.** Seed 1234, which today bakes 27
occupations and **zero** survivors, ends with a non-zero surviving population, and
its die-off is spread across more than one century rather than concentrated in
century 1.

**H3 — cold still excludes.** No settling species calls more than **10%** of land
below its own lethal gate survivable. This is stage 7's H5 restated against a
per-species limit instead of the global −10 °C snowline, which is the reason H5
failed: −10 °C is not any particular species' physiological limit.

**H4 — the cost is paid.** Two readings, because one machine cannot answer both:

- *Per world*: a full seed-42 build is no more than **1.5×** its pre-campaign
  cost, measured with `capacity_cost_probe.rs` on the same box before and after.
- *Per census*: `bash scripts/census-run.sh` on **lefford** grows by no more than
  **1.5×** wall-clock, measured against the `docs/timings.md` ledger. This is the
  reading that decides the campaign, and it can only be taken on the canonical
  host — a Mac measurement cannot stand in for it.

Stated as ratios because the absolute numbers are machine-dependent and this
spec's own figures were taken on the wrong machine.

**H5 — the null.** If §3.1 lands and the binding-axis and capacity distributions
are materially unchanged from stage 6.1's, then era-variance is not the missing
signal and the defect is in the authored niches after all — report it, revert, and
the deferred re-authoring becomes the campaign.

## 6. Risks

- **Ocean exclusion is currently load-bearing on the mask** (§3.2). If it does not
  survive on supply alone, settlements appear at sea and the failure may be quiet.
- **This is a genesis epoch on top of an unmerged genesis epoch.** The Tilth's
  per-species rewire (`64db5432`) is already red with 22 un-accepted goldens. That
  branch should be re-measured after this campaign, and rebaselined once, not
  twice.
- **`CAPACITY_V_MAX` is stale now and will be staler.** It is deliberately not
  re-derived until the physics stops moving; every measurement taken between then
  and now is on an ungauged scale, and that must be stated wherever they are
  quoted.
- **The cost mitigations are unproven.** §4's three levers are reasoned, not
  measured. Task 1 of the plan is to measure lever 1, because if substrate hoisting
  does not deliver, the rest of the campaign is not worth planning.
