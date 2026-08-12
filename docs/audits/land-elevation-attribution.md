# Land-elevation attribution

**Status:** committed finding. **Task:** The Glasshouse, Stage A Task 4.
**Measured:** 2026-08-12, this Mac, 12 worlds at the canonical level 6,
default pins, against
`docs/superpowers/specs/2026-08-12-the-glasshouse-design.md` §3.3 and
decisions [0053](../decisions/0053-ocean-fraction-is-a-target-under-supply-limited-crust.md) /
[0106](../decisions/0106-a-constants-justification-must-match-its-kind.md).
**Probe:** `domains/terrain/src/land_elevation_attribution.rs`.

Hornvale's worlds run ~19 K colder than Earth, and one of the three measured
causes is hypsometry: **54.5% of land stands above 2000 m** against Earth's
~11%, with **mean land elevation 2266.87 m** (median over the 1000-seed census
range, Task 3) against Earth's ~840 m. At the lapse rate the sim actually
applies — `LAPSE_C_PER_M = 6.5 / 1000`
(`domains/climate/src/temperature.rs:16`), an environmental rate, *not* the dry
adiabatic 9.8 K/km — that is a near-constant **−14.7 K**, and it is what drives
the biome/soil uniformity, since `classify_land` evaluates elevation-gated
specials before the Whittaker lookup. Every temperature figure in this
document uses that 6.5 K/km; §3.5 explains why the rate has to be named rather
than assumed.

Fixing it needed a target. Two candidates were ruled out analytically before
this measurement:

- **`ISOSTASY_REF_KM = 30.0` is a gauge.** `derive_sea_level` takes the
  ocean-fraction *percentile* of the same elevation distribution, so a uniform
  offset applied to every elevation shifts sea level by exactly as much and
  changes nothing observable. Its value is unobservable; "calibrating" it is
  meaningless.
- **`ISOSTASY_M_PER_KM = 180.0` is physics.** Airy isostasy over the
  crust/mantle density contrast gives ~150 m/km. It is not a tuning knob.

This document reports which of the remaining additive terms carries the
height, and names what Stage B should target.

---

## 1. How the terms combine — a correction to the plan

`assemble_elevation` (`domains/terrain/src/elevation.rs`) closes with exactly
five additive terms, as the plan stated:

```rust
base + boundary_term + hotspot_term + relief_term + CELL_EPSILON_M * f64::from(cell.0)
```

**But those five sum to `elevation_pre`, the pre-carve surface**, and the
hypsometry metric measures neither that surface nor a raw elevation. It
measures `elevation − sea_level` on the *final* surface, and `globe::generate`
closes with the documented identity `elevation == elevation_pre +
carve_delta_m` (the carve plus the sea-trim, composed at generate level). So
the decomposition of what the metric actually sees has **seven components, not
five**:

| # | component | site | note |
|---|---|---|---|
| 1 | `base` | `isostatic_m(*crust.get(cell))` | `180 · (crust_km − 30)`; driven entirely by the crust-thickness field |
| 2 | `boundary` | `boundary_profile_m(…) × profile_scale(…)` | signed — `Uplift` positive, `Trough` negative |
| 3 | `hotspot` | `dome_m` summed over `trail_seamounts` | sum of positive domes |
| 4 | `relief` | `RELIEF_AMPLITUDE_M · relief_scale(…) · (fbm − 0.5) · 2` | zero-mean by construction |
| 5 | `epsilon` | `CELL_EPSILON_M · cell.0` | ~0.04 m across a level-6 globe; a tie-breaker, not physics |
| 6 | `carve delta` | `TectonicGlobe::carve_delta_m` | incision, repose, deposition, wedge/delta/atoll, sea-trim |
| 7 | `−sea level` | `TectonicGlobe::sea_level` | the datum the height is measured *above* |

Omitting 6 and 7 would not merely have lost detail. Component 7 turns out to
carry more of the *mean* than any assembled term does (§2), so a five-term
account would have misattributed the entire finding.

### 1.1 Which terms are reachable how, and why nothing was zeroed

The plan's original method — neutralise a term, regenerate, diff the land mean
— is unusable here, for two independent reasons, either one fatal:

1. **`relief_term` cannot be zeroed through its input.**
   `relief_scale(induration, hops) = (0.25 + 0.75·induration) · belt` with
   `belt ≥ 1.0`. At `induration = 0.0` it still returns `0.25 · belt`: a hard
   floor no argument reaches past.
2. **Zeroing any term moves sea level, which changes the land set.**
   `derive_sea_level` is the ocean-fraction percentile of the elevation
   distribution. Remove a term → the distribution shifts → sea level shifts →
   *which cells are land* shifts. The before/after comparison would be taken
   over two different cell sets and confounded by construction.

So the probe reads the real terms instead. Every *input* is a public field on
`TectonicGlobe` (`plate_of`, `crust`, `plates`, `boundary`,
`boundary_distance`, `induration`, `trail_seamounts`, `elevation`,
`sea_level`, `carve_delta_m`), but every per-term *helper* is crate-internal:
`isostatic_m` is `pub(crate)`, `relief_scale` and `dome_m` are private,
`SphereFbm` is `pub(crate)`. An out-of-crate test would have had to
reimplement the arithmetic and would have drifted from it silently. So the
per-cell body of `assemble_elevation` was extracted into a crate-internal
`ElevationTerms` struct that `assemble_elevation` itself sums, and the probe
lives in the crate and reads that struct. **No term's arithmetic is
duplicated.** The one input the globe does not retain is the continental mask,
re-derived by the definitional comparison `globe::generate` uses
(`crust >= CONTINENTAL_THRESHOLD_KM`) — and guarded, not trusted, by §1.2.

Byte-identity was the binding constraint on the extraction, since float
addition is not associative: `ElevationTerms::total()` sums
`base + boundary + hotspot + relief + epsilon` in exactly the original
left-associative order, with no intermediate regrouping. Evidence in §4.

### 1.2 Conservation

Before any statistic is computed, the probe asserts on **every land cell of
every seed** — 183,399 cells — that the components re-add to the elevation the
pipeline actually produced:

```rust
let reconstructed = t.total() + carve;
assert!((reconstructed - elevation).abs() < 1e-9, …);
```

This replaces the usual "assert the mutation took" check and is stronger: if
the decomposition were describing a different world, every share below would
be void. It passes. (The tolerance is not slack: `generate` composes the
elevation as `(pre + carve) + trim` while the retained delta is
`carve + trim`, so the two can differ by a rounding of the last bit — about
1e-13 m at these magnitudes, a thousandfold inside 1e-9.)

---

## 2. The measurement

```
cargo test -p hornvale-terrain --lib the_land_elevation_terms_attribute_their_variance -- --nocapture
```

```
=== land-elevation attribution: 12 worlds, level 6 ===

POOLED over every land cell of every seed  (n = 183399 land cells)
component            mean (m)       sd (m)     Var/VarY     Cov/VarY
base (isostasy)       -769.21      1484.19       0.9918       0.9140
boundary                77.82       337.91       0.0514       0.0338
hotspot                 70.62       354.18       0.0565       0.0014
relief                   0.80        37.83       0.0006       0.0002
epsilon                  0.02         0.01       0.0000      -0.0000
carve delta            -87.14       193.01       0.0168       0.0218
-sea level            2964.30       392.71       0.0694       0.0287
TOTAL (e - sea)       2257.21      1490.34            -       1.0000
  mean(e - sea) recomputed directly: 2257.21 m

WITHIN-WORLD Cov/VarY per seed (sea level is constant, so its share is 0)
seed     mean (m)     land base (isostasy)     boundary      hotspot       relief      epsilon  carve delta   -sea level
0         2434.85    17594       0.9388       0.0587      -0.0171       0.0000      -0.0000       0.0195      -0.0000
1         2359.21    14395       0.9742       0.0053      -0.0052       0.0016       0.0000       0.0241       0.0000
2         2020.08    12245       0.9249       0.0641      -0.0080       0.0022      -0.0000       0.0168       0.0000
3         2564.51    20542       0.9501       0.0127       0.0203      -0.0021      -0.0000       0.0190      -0.0000
4         2225.18    16597       0.9676       0.0218      -0.0096      -0.0010       0.0000       0.0213      -0.0000
5         1862.58    10805       0.9535       0.0324      -0.0187      -0.0011       0.0000       0.0338       0.0000
6         2183.33    15425       0.9468       0.0237      -0.0044       0.0023       0.0000       0.0315      -0.0000
7         2370.29    19046       0.9202       0.0370       0.0188      -0.0012       0.0000       0.0251       0.0000
8         2085.49    16710       0.9375       0.0312      -0.0036       0.0050      -0.0000       0.0299       0.0000
9         2295.24    16065       0.9209       0.0654      -0.0063       0.0009      -0.0000       0.0190       0.0000
10        2273.28    12419       0.9485       0.0048       0.0254       0.0011      -0.0000       0.0201       0.0000
11        2070.07    11556       0.9070       0.0786      -0.0007      -0.0016      -0.0000       0.0167       0.0000
MEAN                             0.9408       0.0363      -0.0008       0.0005      -0.0000       0.0231       0.0000

DERIVED READING (the same means, in crust-thickness terms)
  mean land crust thickness                 25.73 km
  isostatic base at that thickness        -769.21 m
  mean sea level                         -2912.96 m
  crust thickness at sea level              13.82 km
  the isostatic shelf break (crust = 20 km)   -1800.00 m
  => sea level sits 1112.96 m BELOW the shelf break, i.e. 6.18 km of crust below it

WHY SEA LEVEL LANDS THERE (sphere fractions, mean over the sweep)
  analytic continental supply (majors, the rescale's budget)   0.2592
  cells actually at or above the continental threshold         0.2724
  land the sea-level percentile granted                        0.3731
  =>   0.1007 of the sphere is land standing on SUB-threshold crust
  supply / land quota                                          0.6948  (the shelf-break fallback fires below SUPPLY_SHORTFALL_FACTOR = 0.5)
```

### 2.1 Reading the two variance columns

Naive shares (`Var(cᵢ)/Var(y)`) do not sum to 1, because the components are
correlated. The decomposition that *is* exact is the covariance one: since
`y = Σᵢ cᵢ`, `Var(y) = Σᵢ Cov(cᵢ, y)`, so `Cov(cᵢ, y)/Var(y)` sums to exactly
1 (asserted in the probe) and may legitimately be negative for a component
that opposes the total. The `Cov/VarY` column is therefore the attribution;
the `Var/VarY` column only says how much a component varies on its own.

### 2.2 Cross-check against the instrument

The probe's pooled mean of `elevation − sea_level` is **2257.21 m** against
Task 3's **2266.87 m** median / 2234.85 m mean over 1000 census worlds — 0.4%
from the median, and inside the 1474–2746 m per-world range. The two are
independent readings: Task 3's went through `windows/lab`'s
`mean-land-elevation-m` metric over `GeneratedTerrain`; this one calls
`hornvale_terrain::generate` directly. `worldgen` passes `world.seed` through
to that function unchanged, so these are literally the census's own first
twelve worlds. The agreement means the decomposition is describing the same
population the campaign's headline number came from.

---

## 3. The finding

### 3.1 The dominant term is `base` — the isostatic base over crust thickness

**`base` explains 91.4% of the pooled variance of `elevation − sea_level` over
land, and 94.1% within-world** (range 90.7–97.4% across the twelve seeds — the
attribution is not seed-dependent). Every other component is a rounding error
beside it: `boundary` 3.4%, `carve delta` 2.2%, `−sea level` 2.9% (pooled
only; zero within a world by construction), `hotspot` 0.1%, `relief` 0.02%,
`epsilon` nil.

Two of these were predictable and are confirmed rather than discovered.
`relief` is zero-mean by construction (`(fbm − 0.5) · 2`) and measures a mean
of **0.80 m** over 183,399 land cells, so it was never a candidate for the
elevated mean — and at sd 37.83 m it is not even a serious contributor to the
variance. `epsilon` is a tie-breaker at 0.02 m. What is *not* predictable is
how small `hotspot` turns out to be: it is a sum of strictly positive domes,
so it does raise the mean (+70.62 m), but at `Cov/VarY = 0.0014` it explains
essentially none of the spread, and within-world its share is slightly
**negative** on 8 of 12 seeds — the domes sit where the surface is otherwise
low.

### 3.2 But the *mean* is a story about where the coastline is cut, not about uplift

The variance answer alone would be misleading, and this is the part of the
finding that reframes the campaign's problem: **land does not stand 2257 m up
because anything lifted it.**

Stating that gauge-invariantly matters, because this document's own opening
rules `ISOSTASY_REF_KM` out as an unobservable gauge. Any sentence of the form "mean
`base` is −769 m, *below the datum*" or "sea level is at −2913 m" is a
statement about that arbitrary zero and would read differently if the constant
were 25 or 35 — so those framings are not used here. Only **differences**
survive a change of gauge, and the finding is entirely a difference:

| gauge-free quantity | value |
|---|---|
| crust thickness where the coastline is cut | 13.82 km |
| mean crust thickness over land | 25.73 km |
| **land stands above its own coastline by** | **11.91 km of crust × 180 m/km = 2144 m** |
| crust at the isostatic shelf break (`CONTINENTAL_THRESHOLD_KM`) | 20 km |
| **the coastline is cut below the shelf break by** | **6.18 km of crust = 1113 m** |

The mechanism is therefore not uplift: it is that **the coastline is cut far
down the crustal ramp**, and everything above the cut is then high by
arithmetic.

**The mean closes exactly.** For completeness, the pooled per-component means
add up to the measured total — but only with the *cell-weighted* sea-level
component (`+2964.30`, the mean of `−sea` over land cells), not the
*per-world unweighted* mean sea level (`−2912.96`, which is what the DERIVED
READING block prints and what the gauge-free table above uses). Mixing the two
is what an earlier draft of this section did, and it left the arithmetic ~51 m
short:

```
  base       boundary   hotspot   relief  epsilon    carve    -sea(weighted)   total
  -769.21   +  77.82  +  70.62  + 0.80  + 0.02   -  87.14  +  2964.30      = 2257.21  ✓
```

Two labels are worth stating exactly, since both are easy to slip:
**−619.95 m** is the mean of the five *assembled* terms (`elevation_pre` over
land); **−707.09 m** is that plus the carve delta — the mean **final
elevation** over land. Neither is "the assembled terms".

The same total reached the gauge-free way, as a cross-check on the table above:
**2143.75 m** of crust-thickness difference (the table's 2144 m, unrounded),
plus **+62.12 m** from the four decoration and carve terms together, plus
**+51.34 m** from the difference between the cell-weighted and unweighted sea
levels, = **2257.21 m** — exactly the measured total, to the last printed
digit. Both routes agree, which is the point: the 2257 m is a difference, not a
position relative to a gauge.

### 3.3 Why sea level lands there — measured, not inferred

The last block of output closes the causal chain:

- the sea-level percentile grants **0.3731** of the sphere as land, which is
  just the drawn land quota (`ocean_fraction` draws 0.50–0.75, so
  `1 − target` averages 0.375). The percentile is doing its job exactly.
- but only **0.2724** of the sphere has crust at or above the continental
  threshold.
- so **0.1007 of the sphere — about 27% of all land — stands on sub-threshold
  crust**, on the taper between the abyssal floor and the shelf break. To find
  that much extra area the percentile must descend past the shelf break, and
  it descends 1113 m.

Hornvale therefore has **no continental platform near sea level**. The crust
field is a continuous ramp from `OCEANIC_KM = 7` to craton peaks of 33–45 km,
the coastline is cut arbitrarily part-way down that ramp, and the continental
interior then sits on the full 25.73 km average — 2257 m up. Earth's mean land
elevation is 840 m not because its continents are thin but because its
continental platform is a broad flat surface within a few hundred metres of
base level, with a steep shelf-slope drop to the abyss; the coastline lands
*on the shoulder*, not part-way down the ramp.

### 3.4 The mechanism already has a guard, and it does not fire

`effective_ocean_target` (decision 0053) exists precisely to stop a world
filling its land quota below the shelf break: when continental supply falls
short of the quota it re-places sea level at the shelf break instead. Its
trigger is `supply >= SUPPLY_SHORTFALL_FACTOR × land_quota` with
`SUPPLY_SHORTFALL_FACTOR = 0.5`.

Measured supply/quota over this sweep is **0.6948** — a 31% shortfall, which
sails past a trigger that demands a 50% one. This is consistent with, not a
contradiction of, `SUPPLY_SHORTFALL_FACTOR`'s own documentation: it records
that default draws bottom out at supply/quota ≈ 0.554 over the frozen
1000-seed census and that 0.5 was chosen to bisect the empty gap between that
population and the ≲ 0.18 of a lone clamped craton, expressly so that
**default worlds provably keep the exact-percentile path byte-identical**.
That was the right call for the bimodality question 0053 was answering. Its
cost, unmeasured until now, is that **every default world's coastline is cut
~1.1 km below its own shelf break** — which is the *mechanism* behind the
campaign's −14.7 K, though not, as §3.5 shows, a −14.7 K refund.

### 3.5 The 1113 m is not a budget — read this before quoting it

**1113 m is the depth of the cut, not the elevation a fix recovers.** It is
the most quotable number in this document and it will be misread as a
recoverable amount unless the following is read with it.

Raising sea level to the shelf break does not merely subtract 1113 m from
every land cell's height. **It also shrinks the land set**: land would become
the cells whose crust clears the threshold — 0.2724 of the sphere instead of
0.3731 — and the 0.1007 that drops out is precisely the *lowest* band of the
old land (crust 13.82–20 km, heights 0–1113 m above the old coastline).
Removing a band that lies entirely below the mean **raises the mean of what
remains**, so the reduction is strictly less than 1113 m. That inequality is
rigorous; the size of the gap is not, and this probe does not measure it,
because it never computes the conditional mean crust over the cells that would
survive.

An area-weighted estimate from the numbers this probe *does* have: if the
dropped band averages ~16.5 km of crust, the retained set averages
(0.3731 × 25.73 − 0.1007 × 16.5) / 0.2724 ≈ **29.1 km**, standing
180 × (29.1 − 20) ≈ **1646 m** above the new coastline against 2144 m above
the old — a recovery of about **500 m**. Sweeping the band average over
16.0–17.0 km moves that only to 465–530 m, so the estimate is not sensitive to
the assumption. An independent review estimate put it nearer **650 m**. Both
are far from 1113 m, which is the load-bearing point.

In temperature, using the rate the sim itself applies —
`LAPSE_C_PER_M = 6.5 / 1000` (`domains/climate/src/temperature.rs:16`), the
same rate that turns 2266.87 m into the campaign's −14.7 K:

| elevation recovered | ΔT at 6.5 K/km |
|---|---|
| 1113 m (the cut depth — **not** achievable) | 7.2 K |
| ~650 m (review estimate) | 4.2 K |
| ~500 m (this document's estimate) | 3.3 K |

So the realistic range for route 2 alone is roughly **450–700 m, or 3–4.5 K**
— useful, and roughly a fifth to a third of the campaign's 14.7 K, but not a
solution to it on its own. (A paired "~650 m / ~6 K" figure appears in review;
6 K at 650 m implies a dry-adiabatic ~9.8 K/km rather than the 6.5 K/km the
climate code actually applies, so the K column above is the one to plan
against.) **Stage B must measure the conditional mean over the retained set
before quoting any budget at all** — it is one extra accumulator in this
probe.

---

## 4. Byte-identity

The extraction is a pure refactor of `assemble_elevation`'s per-cell body:
same five terms, same left-associative summation order, no intermediate
regrouping. Verified two ways.

`cargo nextest run -p hornvale-terrain` — 227 tests, all passing, including
`tectonic_properties`' pin-isolation and determinism batteries and
`carve_properties`' mass-balance books:

```
     Summary [   5.618s] 227 tests run: 227 passed, 5 skipped
```

The decisive check is the committed-artifact drift check, because a byte change
can pass every test and still move the elevation map. After
`SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` (plus the post-abort
steps run by hand — see the note below):

```
$ git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ \
      docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
DRIFT CHECK EXIT: 0
```

Clean. That covers `elevation-seed-42.png`, `biome-seed-42*.png`,
`column-seed-42.png`, the three seed-42 almanacs, and the live 50-seed
`the-chorus` study rows — all regenerated (timestamps confirm) and all
byte-identical. `cli/tests/exit_criterion::almanac_is_byte_deterministic_and_seed_sensitive`
also passes.

**Note on the script's exit code.** `regenerate-artifacts.sh` aborts at its
census-schema re-derivation step with `rows.csv header does not match study
'the-census' schema` — the committed census rows lack `insolation-rel`,
`zone-position` and `mean-land-elevation-m`, the three metrics Tasks 2–3 added.
That is the campaign's **known deliberate red**, awaiting the pre-merge census
refresh on the canonical box; it is not caused by this change and would abort
identically at `HEAD~1`. The steps after the abort (domesday, type-audit
report, seam-guard roster, trope reports, digest) were run by hand so the
drift check above covers the whole list.

---

## 5. What Stage B should target

**Target the crust-thickness field's hypsometric profile, and where sea level
cuts it — not any of the four additive decoration terms.**

Concretely, the plan's four candidates resolve as follows:

| candidate | verdict |
|---|---|
| `base` / the crust-thickness field | **THE TARGET.** 91.4% pooled / 94.1% within-world variance share |
| `boundary_term` amplitudes | not the target: 3.4% of variance, +78 m of mean |
| `hotspot_term` amplitudes | not the target: 0.1% of variance, +71 m of mean |
| `relief_term` amplitude | not the target: 0.02% of variance, +0.8 m of mean |

And since both isostasy constants are ruled out at the top of this document —
one a gauge, one physics — "the crust-thickness field" means one of the three
things that decide where the coastline cuts the ramp.

**Route 1 — the shape of the ramp.** `crust.rs`'s craton profile:
`PEAK_MIN_KM = 33` / `PEAK_MAX_KM = 45` against `OCEANIC_KM = 7` and
`CONTINENTAL_THRESHOLD_KM = 20`, and the taper between them. A crust field with
a broad flat platform near the elevation of zero isostatic head, and a steep
shelf-slope drop, would put mean land near Earth's 840 m *without* touching a
single decoration term.

**Route 2 — where the percentile is allowed to land.**
`SUPPLY_SHORTFALL_FACTOR = 0.5` in `elevation.rs` is the constant that
currently permits a 31% shortfall to be filled below the shelf break. It is a
`hornvale-gauge`/`hornvale-choice` constant with a documented measured basis
(decision 0053), so moving it is a deliberate re-decision with an epoch's worth
of consequences — every default world's coastline moves — not a tuning nudge.
Worth ~450–700 m (3–4.5 K) on its own, per §3.5 — **not** the 1113 m the cut
depth suggests.

**Route 3 — the craton rescale misses its own budget by ~37%, and nobody knows
why.** This is the route this document nearly lost, and it may be the cheapest
of the three, because unlike the other two it is a candidate *defect* rather
than a re-decision: if the rescale delivered what it aims at, the coastline
would already sit at or above the shelf break and the hypsometry would already
be roughly Earth-like. The pipeline's stated intent is not the problem;
something downstream of the intent defeats it.

The apples-to-apples comparison is between the budget the rescale targets and
the supply it achieves, both in the same analytic units:

| quantity | value | what it is |
|---|---|---|
| `budget` (`crust.rs:615`) | **≈ 0.41** | `(1 − ocean_target) · (1 + margin)`, margin 0.05–0.15; at the mean drawn target 0.625 that is 0.375 × 1.10 |
| realised `continental_supply` | **0.2592** | the same `craton_continental_steradians`, summed over the *post-clamp* radii |
| **the miss** | **≈ 37%** | |

The mechanism is visible in four lines (`crust.rs:634–641`): the rescale solves
`scale` so the summed continental steradians hit `budget × 4π`, and then

```rust
c.radius_rad = (c.radius_rad * scale).min(0.6);
```

**clamps every radius at 0.6 rad**, discarding whatever area the clamp cuts —
while `continental_supply` sums the same function over those clamped radii. A
review estimate attributes roughly half the miss to that clamp; the remainder
is **unexplained** and worth an hour before either other route is chosen. The
second candidate is that `craton_continental_steradians` does not deduct cap
overlaps (its own doc calls itself an upper estimate), which would make the
realised area smaller still.

For the same reason, **do not read the 0.2592-vs-0.2724 pair as evidence that
the rescale is working.** An earlier draft of this finding glossed it that way
and the gloss is wrong: the two numbers are not comparable. 0.2592 is
majors-only, analytic, and overlap-blind; 0.2724 is grid-realised and includes
the threshold-clearing area contributed by microcontinents and terranes, which
`continental_supply` never counts. They land close by coincidence of two
opposing errors, and their closeness says nothing about the budget the rescale
was aiming at.

Which route Stage B takes is a design decision, not a measurement one, and this
document deliberately does not make it — but **route 3 should be investigated
first**, because its answer changes what routes 1 and 2 are even for. What the
measurement settles is that the lever is on the *crust-and-coastline* side of
the pipeline and that the boundary, hotspot and relief terms are not worth
touching for hypsometry: together they contribute 3.5% of the variance and
+149 m of the 2257 m.

A caution for whichever route is taken: **all three move sea level on every
world**, so all three are byte-identity epochs for the whole terrain pipeline —
new coastlines, new biomes, new censuses — not local edits.

---

## 6. Probe cost and placement

**In the commit gate, not the heavy tier.** Twelve level-6 terrain globes cost
**roughly 3–6 s at ordinary load**, and the figure is **load-dependent rather
than a single number** — which is why a range is recorded here instead of one value:

| measurement | this probe | whole `hornvale-terrain` suite | conditions |
|---|---|---|---|
| `cargo test`, single test | 3.26–3.98 s | — | moderate load |
| `cargo nextest run -p hornvale-terrain` | 4.755 s | 5.618 s | moderate load |
| `cargo nextest run -p hornvale-terrain` | 5.702 s | 6.675 s | moderate load, later in the session |
| independent review re-run | 3.80 s | 5.406 s | loadavg ~16 |
| `cargo nextest run -p hornvale-terrain` | 14.792 s | 16.156 s | loadavg ~28, other sessions gating |

The last row is contention, not the probe: the whole suite scales by the same
~2.7×. Every figure is well inside the plan's ~30 s threshold, so the probe
carries no `#[ignore]` and the conservation assert of §1.2 runs on every gate
rather than only under `make gate-full`. The probe's own doc comment records
the same range, so the two cannot drift apart.

Worth recording: had it needed deferring, the plan's suggested reason string
`heavy: 50-world elevation attribution probe` would have gone **red**.
`cli/tests/heavy_tier.rs` holds every `heavy:` reason to one verbatim
canonical string, and an untokenised reason has to be added to that file's
`EXPECTED_UNTOKENISED` roster as a review decision.

The probe also required a `claim: readout(…)` tag: `cli/tests/claim_shape.rs`
(decision 0093) fails any test that iterates seeds without declaring its
quantifier, and it caught this one immediately.

**New constants:** none. The probe introduces no constant of any kind, so
decision 0106's provenance requirement does not apply. `LEVEL` and
`SEED_COUNT` are sample-size parameters of a test, and `LEVEL` is
`crate::GLOBE_LEVEL` rather than a fresh literal.
