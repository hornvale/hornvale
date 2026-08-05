# The Tense — capacity is written in the eternal present

**Campaign:** the-tense · **Runs on `campaign/the-tilth` itself**, as further
stages, not on a branch of its own · **Supersedes:** The Tilth stages 6 and 7,
which are patches on the layer below this one and were reverted in `511d1fa9`

*Why not its own branch:* it needs `per_species_capacity`, which exists only on
`campaign/the-tilth`, so a separate branch would fork from an unmerged branch and
inherit a merge-order dependency. That is exactly the hazard that killed the
abandoned "The Sovereign" framing earlier the same day — a campaign branched off
`main` on the belief its subject matter was there, caught only by an unresolved
import at the pre-commit hook. One branch also means **one golden rebaseline**
for the parked per-species rewire (`64db5432`) and for this work together, which
§6 wants anyway.

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
tree-finding → notation) surfaced these. Each is independently checkable, and
§2.3's consequences were worked out further in review — see §3.4, which is where
that thread ended up.

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

### 3.4 A within-cell temperature distribution (promoted to first-class)

Capacity reads the cell **mean**. By Jensen's inequality that is wrong whenever
the response is nonlinear, which a Gaussian emphatically is:

```
  R(mean(T))  !=  mean(R(T))
```

Evaluating at the mean overestimates near the optimum (where R is concave) and
underestimates in the tails — **and the tails are where refugia live.** This is a
defect at *any* grid resolution; it is not fixed by subdividing, only shrunk.
Integrating removes it outright:

```
  K(s, c, e) = Vmax . S(supply) . E_{T ~ N(mu(c,e), sigma(c))} [ R(s, T) ]
```

a few-point quadrature per cell per species.

**Do not subdivide the globe instead.** The ladder was measured:

```
 lvl      cells   km2/cell   across km    300sp MB/era
   6     40,962      12452         126           98.3
   7    163,842       3113          63          393.2
   8    655,362        778          31         1572.9
  10 10,485,762         49           8        25165.8
```

Cost is 4x per level and multiplies with §3.1's 25 eras, while real glacial
refugia are *kilometres* — sheltered valleys, south-facing slopes, coastal
pockets. Level 8 is still 31 km across; nothing under level 10 resolves a valley,
and level 10 is 25 GB per era per worker at species scale. Subdivision buys the
whole cost and none of the refugia. The architecture also already carries a fine
scale for places (`walk_depth = globe_level + 6`, ~1.7 km); the defect is that the
coarse layer *claims homogeneity*, not that it is coarse.

#### Where sigma comes from — and where it does NOT

Two tempting sources were checked and both fail:

- **The room-scale `MicroField` cannot supply it.** `windows/locale/src/micro.rs`
  derives `relief`/`aspect`/`wetness`/`openness` from `room_seed` alone — a pure
  address hash with no dependence on its parent cell. Its statistics are therefore
  *identical in every cell of every world*, so aggregating children yields a
  constant, and a constant sigma produces no differential refugia. The room field
  is decoration for prose, not a decomposition of the cell field; the two scales
  are not a multiresolution hierarchy.
- **The fBm relief has essentially no sub-cell content, by design.**
  `RELIEF_FREQUENCY = 8.0` with 4 octaves and lacunarity 2 gives octave
  wavelengths of 7.3 / 3.7 / 1.8 / 0.92 cells at L6. Only the last is near
  sub-cell, and at gain 0.5 it carries 12.5% of `RELIEF_AMPLITUDE_M = 240 m` —
  about 30 m, or **0.2 °C** at a 6.5 °C/km lapse rate. That truncation is
  deliberate: the constant's own comment records rejecting 48.0 *because* its
  dominant octave was sub-Nyquist "jitter the sea-level percentile averages away".

So sigma must be **derived**, not sampled. The construction, using only committed
state and adding no seeded draw (hence not epoch-triggering):

```
  sigma_subcell(c) = k . spread( elevation over c's neighbours ) . relief_scale(c)
  sigma_T(c)       = lapse_rate . sigma_subcell(c)
```

- Neighbourhood elevation spread is one `O(cells x 6)` pass over the existing
  field. It licenses the estimate by **self-similarity**: for fBm-like terrain the
  variance just below the grid is a fixed ratio of the variance just above it.
- `relief_scale(induration, boundary_hops)` already exists in
  `domains/terrain/src/elevation.rs` — hard rock and plate-boundary belts roughen,
  soft rock lies smooth. This is what makes sigma *terrain-dependent*: the Alps
  hold refugia through a glacial maximum, the plains do not.
- `k` is the one authored constant, and it is a fractal-dimension ratio rather
  than a fitted knob. It declares its kind under decision 0104.

**Deferred within this stage:** `MicroField.aspect` is shaded-to-sunlit slope
aspect — the single most important real microclimate-refugium mechanism — and it
exists already, as noise that never touches temperature. Wiring it in as a
systematic warm fraction rather than a symmetric spread is the natural successor,
and belongs with the locale scale rather than here.

### 3.5 Explicitly out of scope

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

**Neither time nor memory is binding at level 6 — an earlier draft of this
paragraph called memory "a wall" and that was wrong.** The runner holds a world
per thread; the fleet and the arithmetic:

```
  host      RAM     workers     300 species, one era resident
  --------  ------  -------     ------------------------------
  lefford   384 GB       40      3.9 GB    (1% of the box)
  MBP        64 GB      ~10      1.0 GB
  ambrose    39 GB       12      1.2 GB
```

3.9 GB on a 384 GB machine is not a constraint, even at the several-hundred-
species target this arc exists to reach. Memory only bites if the globe is
subdivided (§3.4 declines to), and then it bites the *development* boxes well
before lefford — level 8 at 300 species is 18.9 GB on ambrose, half the machine.

Mitigation 2 below (stream one era, never hold the series) therefore stands on
tidiness and on headroom against future growth, **not** on necessity. Stating it
as necessary was an overstatement built on an unchecked multiplication, the same
error as the "+70 minutes" above and in the same paragraph.

Three mitigations, in the order they should be tried:

1. **Hoist the era-invariant work.** Substrate is 89% of the cost, and only its
   temperature and moisture components actually move with the era. Mineral supply,
   elevation and the terrestrial/marine masks are era-invariant and are currently
   rebuilt every call. This alone may be most of the 25×.
2. **Compute per era-change, not per epoch.** The bake's `era_index_for` is
   monotone, so each era is entered exactly once: at most 25 rebuilds per world,
   streamed one at a time, never resident together. This keeps the resident set at
   one era rather than 25 — cheap insurance, not a rescue, per the corrected
   memory arithmetic above.
3. **Decouple capacity's era resolution from the mask's.** Nothing requires 25
   capacity fields; the deep-time signal is smooth, and 5 with interpolation may
   be indistinguishable. Measure before assuming 25 is needed.

**If none of the three brings it under budget, this campaign should not ship**,
and the fallback is The Fallow's stock — a cheap scalar per cell carrying the
land's memory — as an approximation to a time-varying field.

Given the parallelism above, mitigation 1 (hoisting the era-invariant 89%) is
likely sufficient on its own, and is the plan's first task: if it does not
deliver, the rest of the campaign is not worth planning.

## 5. Preregistration (decision 0016)

**H1 — the oracles agree.** After §3.2 there is exactly one habitability rule in
the bake's path. Verified structurally (no `era.habitable` reference survives in
`history_bake.rs`), not statistically.

**H2 — worlds degrade instead of switching off.** Seed 1234, which today bakes 27
occupations and **zero** survivors, ends with a non-zero surviving population, and
its die-off is spread across more than one century rather than concentrated in
century 1.

**H3 — cold still excludes.** No settling species calls more than **10%** of land
below its own lethal gate survivable. This restates **The Tilth stage 7's H5**
(not this spec's, above) against a per-species limit instead of the global
−10 °C snowline, which is the reason that hypothesis failed: −10 °C is not any particular species' physiological limit.

**H4 — the cost is paid.** Two readings, because one machine cannot answer both:

- *Per world*: a full seed-42 build is no more than **1.5×** its pre-campaign
  cost, measured with `capacity_cost_probe.rs` on the same box before and after.
- *Per census*: `bash scripts/census-run.sh` on **lefford** grows by no more than
  **1.5×** wall-clock, measured against the `docs/timings.md` ledger. This is the
  reading that decides the campaign, and it can only be taken on the canonical
  host — a Mac measurement cannot stand in for it.

Stated as ratios because the absolute numbers are machine-dependent and this
spec's own figures were taken on the wrong machine.

**H5 — refugia are terrain-dependent.** After §3.4, the fraction of a cell that
stays habitable through the glacial maximum **correlates with that cell's
neighbourhood elevation spread** (Spearman rho > 0.5 over land, seed 42). A
uniform sigma would score ~0 and would mean the construction has fallen back to a
constant, which is the exact failure mode that rules out the room-scale
`MicroField` as a source.

**H6 — seed 1234 is the acceptance case.** With §3.1 and §3.4 both in, seed 1234
retains a non-zero surviving population *and* its die-off spans more than one
century. It is the only probe seed that currently ends empty, it does so for the
reason this campaign targets, and it needs no new instrument — the existing
`history_shape_probe.rs` prints both quantities. (This subsumes H2, which is kept
because it is the weaker, era-axis-only form.)

**H7 — the null.** If §3.1 lands and the binding-axis and capacity distributions
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
