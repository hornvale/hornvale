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
in newly-opened ground rise above **zero** — step B's measured value on every
seed.

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
