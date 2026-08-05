# The Tilth — productivity is a property, tolerance is a relation

**Status:** Draft for review (2026-08-04) · **Campaign:** the-tilth ·
**Supersedes:** the step ordering in
[The Keeping §8](2026-08-04-the-keeping-design.md#8-redirect-after-task-0),
overturned twice by measurement · **Builds on:** decisions
[0100](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0100-fact-phenomenon-myth.md),
[0103](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0103-suitability-and-headcount-are-distinct-types.md),
and The Keeping's step B (`CarryingInput.is_land`)

## 1. The thesis

Nathan, during The Keeping: **"habitability is a relation between a species and a
location, not a property of the location alone."** Step B applied that once, at
the flag level — `habitable` (three conjuncts) became `is_land` (one property).
This campaign applies it to the arithmetic underneath, where the same error is
made three more times.

## 2. What is wrong, measured

### 2.1 `carrying_capacity` is not the model it cites

The module doc claims *"a Miami-model net-primary-productivity proxy (Lieth)."*
Lieth's published model is

```
NPP_temp   = 3000 / (1 + exp(1.315 - 0.119 T))     g/m2/yr
NPP_precip = 3000 * (1 - exp(-0.000664 P))         P in mm/yr
NPP        = min(NPP_temp, NPP_precip)
```

Only the Liebig minimum survives:

```
   T(C)   Miami NPP   Miami %of40C   Hornvale temp_response
    -20         73           2.5%                 0.00
    -10        227           7.8%                 0.00
      0        635          21.8%                 0.00
      2        762          26.2%                 0.00
     22       2359          81.1%                 1.00
     30       2715          93.4%                 0.60
     40       2907         100.0%                 0.10
```

- **Miami has no temperature optimum.** It is monotone and saturating; above
  22 °C the two models move in **opposite directions**.
- **Miami never reaches zero.** Hornvale is *exactly* zero below 2 °C, so the
  hard gate that closes the frozen wastes is a **departure from the cited model**,
  not a consequence of it.
- **A tent is a tolerance curve, not a productivity curve** — and tolerance
  curves already exist per-species as `ConditionNiche.temperature`. So the base
  field carries a **species-blind 22 °C optimum that no species chose**.

### 2.2 The calibration could not have detected it

`capacity-by-abs-latitude`'s own doc: *"the polar mean floored at POLE_FLOOR (1%
of …) … rather than a division blowup."* The polar mean is *exactly zero* often
enough to need a floor, so the headline (~20.96 against a preregistered floor of
3) is largely `tropical / (0.01 × baseline)` — **guaranteed by the hard zero it
was meant to validate.**

### 2.3 Per-species conditions combine by product, not by Liebig

The base field takes `min(temp, moisture)` — Liebig's law of the minimum,
correctly. `per_species_suitability` then multiplies **four** condition responses:

```
saturated * temperature.eval(..) * moisture.eval(..) * insolation.eval(..) * elevation.eval(..)
```

For four plausible factors `[0.81, 0.60, 0.50, 0.40]`:

```
  product (today) : 0.0972
  minimum (Liebig): 0.4000     -> 4.1x compression from using the product
```

So one model uses Liebig and the layer above it does not, and the inconsistency
is most of the 1–2 orders of magnitude that made every newly-opened cell
unsurvivable in step B. Ecologically, Liebig is the standard: the *binding*
factor limits, and a species mildly suboptimal on four axes is not penalised four
times over.

### 2.4 Consequence, measured: the monopoly

Task 0 measured that `hobgoblin` and `kobold` take essentially every best-fit
cell while `goblin`, `bugbear` and `gnoll` win **zero on every seed** — despite
well-separated authored optima (The Keeping §2.1). The lift explains it: **the
base field is shaped like a temperate generalist**, so every species is scored
against an incumbent nobody authored, and whoever most resembles that incumbent
wins everywhere. This is an identification failure — a supply model with a demand
model's parameter embedded in it.

And the cold is not a roster problem. Evaluating the curve rather than reading the
optimum:

```
kobold temperature suitability:  -20C 0.407   -10C 0.596   -5C 0.715
                                   0C 0.813     6C 0.862   22C 0.596
```

**Kobold prefers −5 °C to +22 °C.** The roster already has a cold-adapted people;
it simply cannot eat there.

## 3. The target architecture

Five stages, of which three already exist and are correct:

```
1  climate -> NPP                    Lieth, species-blind, monotone   <- WRONG
2  NPP -> supply axes                forage/prey/detritus cascade     <- ok
3  supply x niche -> per-species     axis_supply                      <- ok
4  tolerance                          ConditionNiche                  <- ok, but duplicated at 1
5  supply x tolerance -> capacity     headcount units                 <- MISSING
```

### 3.1 Stage 1 — adopt Lieth

`temp_response`'s tent is **deleted**. Its replacement, normalised to the model's
own asymptote so `BASE = 1.0` keeps meaning "one unit of productivity":

```rust
/// Lieth's Miami temperature term, normalised to its 3000 g/m2/yr asymptote.
/// Monotone increasing and saturating: there is NO optimum, and it never
/// reaches zero. Both properties are the point (spec §2.1).
fn npp_temperature(t_c: f64) -> f64 {
    1.0 / (1.0 + math::exp(1.315 - 0.119 * t_c))
}
```

The moisture term becomes the saturating form too, treating `moisture ∈ [0,1]` as
a normalised precipitation proxy with the scale stated rather than implied. The
Liebig minimum between the two is **kept** — it is the one part that was already
faithful.

**The aridity double-count goes.** `carrying_inputs_of` folds
`aridity = ((0.2 - moisture) * 5).clamp(0,1)` into `hostility`, so moisture is
counted twice — once as the Liebig limiter and again as a hostility penalty,
~20× total on semi-arid ground. With the saturating moisture term doing the work,
`hostility` keeps only `terrain.unrest_at`, which is what it was for.

### 3.2 Stage 4 — tolerance lives in exactly one place

Nothing to build: `ConditionNiche` already holds per-species temperature,
moisture, insolation and elevation responses, floored by
`sovereignty_floor(mass, potency)`. Stage 1's deletion is what stops the base
field duplicating it. **This is the whole of "1 and 2" — they are two stages of
one pipeline, not two competing options.**

### 3.3 Stage 5 — conditions combine by Liebig, and capacity has units

Two changes to `per_species_suitability`, which is renamed once more to
`per_species_capacity` because it will now return one:

- **Liebig, not product**: the four condition responses combine by `min`, matching
  the base field and §2.3.
- **Michaelis–Menten with a dimensional ceiling**: today's
  `supply / (1.0 + supply)` saturates at a dimensionless `1.0`, which is what
  destroyed the units (decision 0103). Replace it with `V_max · S / (K_m + S)`,
  where `V_max` is in headcount and `K_m` is a stated half-saturation supply. That
  keeps the bounded, stable behaviour the saturation was there for **and** returns
  a `CapacityMap`.

Note this refines the shorthand "desaturate" from The Keeping §8: the saturation
is not the error — saturating at a *dimensionless* 1.0 is.

## 4. Preregistration (decision 0016)

Frozen before any of §3 is written. Instrument: `keeping_probe.rs`, extended;
baseline: Task 0 and step B's recorded numbers. Probe seeds 42, 7, 999999,
16244526067196353746, 1234.

**H1 — THE HEADLINE: the monopoly breaks.** Today `goblin`, `bugbear` and `gnoll`
win **zero** best-fit cells on **every** seed. Prediction: **all six settling
species win non-zero best-fit territory on at least three of five seeds**, with no
species authored or edited. If the base field's implicit incumbent is what
collapsed the roster, removing it must undo the collapse.

**H2 — the cold is inhabited.** At least one settlement below 0 °C mean
temperature on at least three seeds, and kobold over-represented there relative
to its global share. Predicted with **no new species**, which is the surprising
half.

**H3 — marginal ground becomes usable.** Cells clearing the daughter-founding bar
in newly-opened ground rise above **zero**. Step B's baseline, measured on all
five probe seeds after `is_land` landed:

```
seed        NEW ground opened      survivable   expandable   expansion ratio
             (K>0, gate-excluded)   (eff>1.0)   (eff>11.43)   before -> after
42               70  ( 0.63%)            0           0        0.72x -> 0.74x
7              3126  (16.41%)           78           0        0.91x -> 1.27x
999999         2040  (12.72%)           20           0        0.95x -> 1.44x
16244...       2697  (22.77%)           61           0        0.94x -> 1.43x
1234             97  ( 0.84%)            1           0        0.47x -> 0.49x
```

The `expandable` column is **zero on every seed** — that is the number this
campaign has to move, and it is why step B alone changed nothing that matters.

**H4 — the null, stated in advance.** If the monopoly persists after §3, the base
field was *not* the cause and the collapse belongs to the contest layer — the
might-ordered draft and competitive exclusion of MAP-22's coexistence stack. That
would be a genuine finding and redirects to MAP-22 rather than to authoring. It
must not be rescued by retuning `V_max`, `K_m`, or any niche after unblinding.

## 5. Blast radius

Larger than step B, which moved seed 42 by one fact.

- **World identity moves substantially** on every seed: goldens, the three seed-42
  almanacs, the settlement map, `world-seed-42.json`, scene fixtures, the
  dictionary, non-census studies.
- **A census on lefford is required** — a carve-out needing explicit
  authorization. The censuses are *already* stale from step B, so this campaign
  should carry a single regeneration rather than two.
- **`capacity-by-abs-latitude` must be re-specified, not just re-pinned.** §2.2
  shows the current reading is an artefact of the polar zero it floors. With
  productivity non-zero at the poles the ratio becomes a real measurement for the
  first time, and its preregistered floor of 3 needs re-deriving rather than
  re-pinning — this is decision 0097's "convert the fragile half to a census-
  measured rate" applied to the metric that mattered most.
- Expect re-pins across `demesne`, `history_emit`, `lens_purity`, lab
  `metrics.rs`, and the `beta`/`approach-ease` calibrations. Each must carry a
  **direction argument**, per the practice step B established.

## 5a. The three constants, derived (2026-08-04)

§6 names fitted constants as this campaign's central risk, since its thesis is
that the model already carries too much unexplained arithmetic. All three are
therefore **measured or externally anchored, and shown** —
`windows/worldgen/tests/tilth_probe.rs` is the instrument, run over the five
probe seeds.

```
  K_m    = 0.08036     median axis_supply over land            (n = 183,078)
  V_max  = 176.0       SOLVED, not read off:
                         target 68.87   median capacity on good ground today
                                        (n = 3,056, top decile)
                       / MM frac 0.6874 at good-ground supply p90 = 0.17668
                       / tolerance 0.5692  median min-of-conditions (Liebig) for
                                        the BEST-FIT settler on good ground
                                        (p10 0.3485, p90 0.6576)
  precipitation        NO NEW CONSTANT. `climate.precip_at(cell)` already exists:
                       precip_mm_yr(m) = 2000.0 * m^1.5, Earth-ranged, provenance
                       "a documented approximation (spec §5 model card)"
```

Three things worth stating about this derivation:

- **`V_max` had to be solved *through* the Michaelis-Menten fraction**, not read
  off. An earlier version of the probe printed the target (68.87) as though it
  were `V_max`; that would have under-scaled every world by ~2.6x. The factors
  are measured, the arithmetic is stated, and the result is checkable.
- **The calibration is anchored on the case the model already gets right** - good
  ground, top decile - and the marginal cases then fall wherever the model puts
  them. That is what makes H1-H3 falsifiable rather than fitted.
- **`P_FULL` is RETRACTED — there was never a constant to author.** Nathan asked
  whether the scale should be per-world, and checking found `precip_at` already
  in `domains/climate`: `precip_mm_yr(m) = 2000.0 * m^1.5`, Earth-ranged, with its
  provenance already cited to the spec's model card. My `P_FULL` would have been a
  **second, inconsistent conversion of the same quantity** — precisely the
  duplication this campaign exists to remove — and it was *linear*, where the real
  function's `1.5` exponent exists to stop mid-range moisture reading as tropical.
  At median land moisture 0.3707 mine said 741 mm/yr; `precip_mm_yr` says **451**,
  which matches Earth's *median* land far better than my mean-anchored figure.
  **Stage 1 consumes `precip_at`.**

  On the per-world question the answer is **no, and it already isn't**: `moisture`
  is a physical budget (upwind evaporation, orographic rainout, continental
  drying) clamped to `[0,1]`, never normalised per world. So a dry world genuinely
  reads dry and `precip_mm_yr` returns low mm/yr — an Athas stays an Athas.
  Per-world normalisation would be the *bug*: it would rescale every world's
  driest ground to average and erase exactly the thing that makes such a world
  distinctive.

The measured tolerance figure also upgrades §2.3 from illustration to data: the
median min-of-conditions on good ground is **0.5692**, so the product form's
compression can be quoted against real distributions rather than the
plausible-looking factors §2.3 currently uses. Recorded as a followup.

## 5b. Provenance audit — three states, and the worst one is not "missing"

Nathan asked whether the provenance of the math is clearly explained in the code.
Audited: the discipline **exists and is uneven**, in three distinct states.

**Sourced and honest.** `sovereignty_floor`: *"AUTHORED biological prior (not
census-calibrated)."* `carrying_capacity`'s constants: *"CALIBRATED
(the-gathering, 2026-07-13): measured against the 200-seed census … frozen as
measured, not as a placeholder."* `precip_mm_yr`: *"a documented approximation
(spec §5 model card)."* These are auditable.

**Described but unsourced** — the entire moisture budget:

```rust
const EVAP: f64 = 0.5;         /// Precipitable water added per upwind step over ocean
const OROG_K: f64 = 0.07;      /// Orographic rainout coefficient
const CONVECTIVE: f64 = 0.005; /// Convective rainout per overland step in a rising band
const DECAY: f64 = 0.006;      /// Fractional decay of precipitable water per overland step
```

Every one says *what it is*. Not one says **where the number came from** — no
measurement, no citation, no "authored".

**Falsely sourced.** `carrying_capacity` cites Lieth's Miami model and does not
implement it (§2.1). **This is worse than being unsourced**, because a citation
*stops the reader checking*, and it is why the defect survived four campaigns
while its calibration ran green. The lesson generalises past this campaign: an
unsourced constant is a gap a reader can see, and a wrongly-sourced formula is a
gap that actively defends itself.

**What this campaign owes:** stage 1's Lieth adoption states its coefficients
*with* the citation they actually satisfy, and the retracted `P_FULL` is replaced
by a call to the function that already carries provenance. Repairing the moisture
budget's four unsourced constants is out of scope and recorded as a followup — it
is a different campaign, and probably the one §7 q2's fertility term belongs to.

## 5c. What does not transfer to a non-Earth world

Nathan asked whether other properties drift on an Athas, a Mon Cala, a Pandora.

| world | representable? | what happens |
|---|---|---|
| **Athas**-like (hot, arid) | **yes, and improving** | exactly what step B's `is_land` and stage 1's monotone temperature term unlock |
| **Mon Cala**-like (ocean) | geometrically yes (`--ocean-fraction`) | **uninhabitable**: every settler is terrestrial and `MARINE_FORAGE` has no settling consumer (The Keeping's F10) |
| **Pandora**-like (low-g, dense air) | **no** | Hornvale defines no planetary radius, hence no surface gravity, so there is no gravity-dependent biology to drift |

The deepest non-transferable assumption is not a single constant: **adopting Lieth
imports Earth's photosynthetic chemistry as a universal.** Its coefficients are
fitted to Earth's biosphere — Earth's CO₂, water-carbon life, a G-type spectrum —
and Hornvale *generates star classes*, so a red-dwarf world receives different
insolation while keeping Earth's photosynthetic response curve. The same holds for
`FORAGE_FRACTION`/`PREY_FRACTION` (~10% trophic transfer is an Earth observation)
and for `snow_fraction` being centred on 0 °C (assumes water).

This is a limit to **state, not to fix**. Adopting Lieth makes the Earth
assumption explicit and citable where the tent made it invisible, which is an
improvement even though it does not remove it. A world whose biosphere is not
water-carbon around a Sun-like star is outside this model's scope, and saying so
is more honest than a tent that quietly assumed the same thing.

## 5d. Stage 1 + 4 measured (2026-08-05) — two findings, one a self-correction

Implemented and measured before stage 5, per the ledgered decision to keep the two
increments separately attributable. **Not yet committed**: see the note at the end.

### The support exploded, and the count FELL

```
  cells with K>0 for >=1 settler   was 3038/8130/3944/5231/2140
                                   now 11010/17636/16033/11209  (~all land)
  seed 42                          232 settlements -> 157;  14,562 facts -> 10,369
```

Lieth's "never reaches zero" did exactly what it promised — essentially all land now
carries non-zero capacity. But **settlements went down**, which is the opposite of
the campaign's direction, and the reason is that Lieth's water term is *stricter*
than what it replaced:

```
  at median land moisture 0.3707  ->  precip_mm_yr = 2000 * m^1.5 = 451 mm/yr
    old water term (RAW moisture)     0.371
    new water term (Lieth precip)     0.259     = 70% of the old
  temperature, temperate band:  15C tent 0.650 / Lieth 0.615
                                22C tent 1.000 / Lieth 0.786
                                30C tent 0.600 / Lieth 0.905
```

Raw moisture in `[0,1]` was silently a **much more generous** water term than
Lieth's saturating function on a real mm/yr total. So good ground got worse while
extreme ground became barely viable, and the net is fewer settlements. This is not
a defect in stage 1 — it is the model being honest for the first time — but it means
**`V_max` and `K_m` must be RE-DERIVED on the new physics before stage 5**, because
they were measured against the old capacity distribution. The staged sequencing is
what caught that; an all-at-once landing would have used stale constants.

### §2.4's mechanism was WRONG, and this measurement proves it

H1 did **not** move: `goblin`, `bugbear` and `gnoll` still win **zero** best-fit
cells on every seed. Only `human` gained (0/0/154/36 → 34/734/748/168).

§2.4 claimed the base field, being *"shaped like a temperate generalist,"* scored
every species against an implicit incumbent and so amplified whoever most resembled
it. **That is algebraically impossible.** Best-fit is

```
  argmax_sp  eff(c, sp)  =  argmax_sp  [ capacity(c) x K_sp(c) ]  =  argmax_sp  K_sp(c)
```

because `capacity(c)` is **species-blind and therefore cancels out of the argmax**.
The base field cannot influence who wins a cell, no matter what shape it has. The
monopoly is decided entirely inside `K_sp` — that is, by `axis_supply` and the
condition combination — so **only stage 5 can break it.**

This is a genuine falsification of the spec's stated mechanism, and it sharpens H1
rather than weakening it: if the monopoly breaks at stage 5, the cause is
unambiguously the **product-versus-Liebig** change, because that is the only
remaining term that treats species differently. The staged split bought exactly the
attribution it was designed to buy — and it also bought this correction, which an
all-at-once landing would have hidden behind a working result.

### Not committed yet

Stage 1 + 4 moves world identity substantially (232 → 157 settlements on seed 42),
so committing it demands a full rebaseline and re-pin pass — which stage 5 would
then immediately redo, since it changes the same numbers again. The measurement is
recorded here instead, and the two stages will land together with **one**
rebaseline. That is a deliberate deviation from the ledgered "land 1+4, then land
5" sequencing: the *attribution* the split existed to protect is secured above,
analytically and empirically, so the remaining value of separate commits is
bookkeeping, not evidence.

## 5e. H1 tested (2026-08-05) — partially confirmed, with a precise residual

Because the species-blind capacity cancels from `argmax` (§5d), best-fit territory
depends *only* on how the per-species term combines. So H1 is testable as a **pure
measurement** — both rules computed over the same cells, no production change, exact
attribution. Pooled over the five probe seeds:

```
  species         PRODUCT   LIEBIG min
  kobold            24880        29134
  goblin                0  ->      458
  hobgoblin         40160        34652
  bugbear               0  ->      927
  gnoll                 0  ->        0      <- STILL excluded
  human              1818         1687
  species winning ANY territory:  3/6  ->  5/6
```

**The combination rule was the cause for two of the three exclusions.** Swapping
the product for Liebig's minimum moves `goblin` and `bugbear` off zero, and flattens
the distribution (hobgoblin −14%, kobold +17%). That confirms §2.3's diagnosis and
justifies stage 5 on measured grounds rather than on tidiness.

**H1's stated threshold — all six on at least three of five seeds — is NOT met.**
`gnoll` wins nothing, and its cause is different in kind:

```
  moisture 0.12 (gnoll's optimum) -> precip  83 mm/yr -> Lieth water term 0.054
  moisture 0.37 (median land)     -> precip 450 mm/yr -> Lieth water term 0.258
  gnoll moisture tolerance:  0.874 at its optimum, 0.538 at median land
```

Gnoll is *perfectly tolerant* of the ground it is authored for and that ground has
**almost no productivity** — and gnoll eats `ANIMAL_PREY 0.65 + PLANT_FORAGE 0.35`,
both pure functions of `base_carrying`. So the arid specialist starves in the desert
it was designed for: on wet ground its tolerance excludes it, and on dry ground
there is nothing to eat. **This is a trophic exclusion, not a field or a
combination-rule exclusion**, and it is The Keeping's trophic finding arriving for a
specific species under the corrected model.

So the campaign splits its own hypothesis cleanly: **stage 5 fixes what the
combination rule broke, and cannot fix what the food web excludes.** Gnoll needs a
non-photosynthate resource niche — the roster work of §8 step D — and no amount of
work on productivity or tolerance will seat it. Recording that as the campaign's
boundary is more useful than a partial pass or a rescued threshold, and per decision
0016 the threshold is **not** moved to make H1 read green.

## 6. Risks

- **`V_max` and `K_m` are two new authored constants** in a campaign whose thesis
  is that the model has too much unexplained arithmetic. They must be *derived* —
  `V_max` from the existing `SETTLERS_PER_CAPACITY` frame so today's good ground
  keeps roughly today's capacity, `K_m` from the measured `axis_supply`
  distribution — and the derivation written down, not fitted to make H1 pass.
- **Liebig-by-minimum is not differentiable and has a kink**, where the product is
  smooth. If a downstream consumer depends on smoothness, `min` will show up as
  banding. Measure before assuming.
- **Non-zero polar productivity may populate the ice.** `Bake::factor` still
  zeroes glaciated cells, so ice is handled; but *unglaciated* polar land becomes
  livable, which is intended and should be checked for plausibility rather than
  merely counted.
- **H1 may overshoot**: removing the incumbent could over-fragment territory into
  many tiny single-species holdings. The probe should report a distribution, not
  just "did each species win something".

## 7. Open questions

1. **Is the moisture→precipitation scale worth authoring, or should moisture stay
   linear?** Lieth's precipitation term needs mm/yr; Hornvale has a normalised
   `[0,1]` moisture. Recommendation: apply the saturating form with an explicit,
   documented scale, because a linear moisture term is the same category of
   unexplained arithmetic this campaign exists to remove.
2. **Should `soil_of` feed productivity?** It computes soil orders that
   `carrying_capacity` ignores, and real NPP is frequently nutrient-limited.
   Faithful-to-Miami says no (Miami is climate-only). Recommendation: out of scope
   here, recorded as the honest slot for a fertility term — and it is also where
   the broken `DETRITUS → MINERAL → PHOTOSYNTHATE` loop would close.
3. **Does `capacity-by-abs-latitude`'s floor of 3 survive re-derivation?** It was
   preregistered against a model with polar zeros. Recommendation: re-derive it
   from Lieth directly, because the model **predicts the ratio from theory**:

   ```
   Lieth temperature term, tropical / polar
     25 C vs -10 C  ->  11.13
     25 C vs -20 C  ->  34.66
   ```

   Two useful consequences. The preregistered floor of 3 is far too loose — theory
   says ~11 at minimum for a real tropical/polar contrast, so a world reading 5
   would pass a floor of 3 while being badly wrong. And the **observed ~20.96 sits
   inside the theory band**, which is a mild vindication of the current number even
   though §2.2 shows it was computed against a floored zero: the reading is
   plausible, its derivation was not. That gives the successor a principled anchor
   instead of a census-fitted one.

   **BUT the ratio is world-dependent, and the metric is not (Nathan, 2026-08-04:
   "is that calculation going to be accurate for all worlds regardless of
   obliquity, tidal lock, etc?").** Measured over the committed 1000-world census:

   ```
                 n     median   mean     capacity-by-abs-latitude
     spinning   952     23.98   21.99
     LOCKED      48      0.55    0.64    <- no latitudinal gradient AT ALL
     pinned all-world mean 20.9646   spinning-only 21.9892   locked shift -1.0246
   ```

   **Lieth itself is universal** — it is a *pointwise* temperature→NPP relation,
   valid at a cell regardless of *why* that cell is that temperature. What does not
   transfer is the **latitudinal summary statistic**. On a tidally locked world the
   thermal structure is organised around the substellar point
   (`domains/climate/src/substellar.rs`; `circulation.rs`: *"organized around the
   substellar point instead"*), so `|latitude| < 30` vs `> 60` cuts across the
   physics and samples hot and cold longitudes alike. Those 48 worlds report ~0.55
   — and the metric's own doc claims it is *"comfortably clear of the trivial
   'poles support as much as the tropics' failure mode."* **They are sitting in that
   failure mode and they are inside the pinned mean.**

   Obliquity, by contrast, is **not** a contamination source here and can be bounded:
   Hornvale draws 0–35° (median 18.1, n=1000, max 35.0), never past the ~54° where
   poles out-insolate the equator, so the gradient never inverts.

   So the metric must become **coordinate-aware or scoped**: either `Absent` on
   locked worlds (honest and cheap) or re-expressed in substellar angle when locked
   (better science, more work). Per decision 0097 the durable framing is that
   *"productivity follows the thermal gradient"* is the invariant, while
   *"|latitude| is the thermal coordinate"* is a world-dependent assumption that a
   spinning-world metric silently makes.
