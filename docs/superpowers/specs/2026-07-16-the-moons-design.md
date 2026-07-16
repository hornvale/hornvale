# The Moons — Design

**Date:** 2026-07-16
**Status:** **PARKED — split, and blocked on The Reckoning.** This spec was written before the campaign was decomposed. It is campaign **B** of two; do not execute it until **The Reckoning** (campaign A — stellar/planetary age + moon formation mechanism) has merged. See §0.
**Tickets:** [hornvale#4](https://github.com/hornvale/hornvale/issues/4) (moon surface data); consumer proof in hornvale/orrery; unblocks orrery#4 (stand on the world *and its moons*).
**Parent contracts:** `2026-07-16-the-isotherm-design.md` (the parameterized-quantity + normative-evaluator pattern this extends from time into space), `2026-07-16-the-region-design.md` (cross-repo contract discipline, producer-sourced goldens), decision 0022 (the sim emits data, clients render), decision 0033 (quantization at the emit boundary), decision 0055 (the wasm catalog boundary).
**Roadmap:** The Isotherm §2 item 3. The Region shipped; this is next.

---

## 0. Why this spec is parked, and what changes when it wakes

Nathan's ruling at G3 (2026-07-16): **draw ages, and draw formation mechanism
too.** That reaches much further than moon faces. A moon's formation mechanism
predicts its **inclination** — and inclination is already drawn (0–10°, i.e.
every moon in Hornvale today is implicitly *regular*), and Eclipse Seasons
dates real eclipses off inclination and node. Constraining inclination from
mechanism therefore moves every seeded world's eclipses: an **epoch**. Worse,
`windows/lab/src/metrics.rs` runs a 100-year dated-eclipse scan for its cadence
metrics, so the epoch moves **census rows** and needs an AWS census
regeneration — an explicit-authorization carve-out.

So the arc split (Nathan's call):

- **Campaign A — The Reckoning** (the epoch, astronomy only): draw the star's
  age bounded by the main-sequence lifetime; derive the planet's age; draw each
  moon's **formation mechanism** and let it drive age, density, mass, and
  inclination. Re-pins the eclipse batteries; needs the census regen.
- **Campaign B — The Moons** (this spec, additive, no epoch): the
  `scene/moon/v1` contract, the crater/mare population, the evaluator, and the
  orrery consumer proof.

**What The Reckoning retires from this spec when B wakes** — revise §§4–5 then,
not now:

1. **§5's assumed lunar bulk density dies.** Density becomes a *derivation* from
   formation mechanism: a giant-impact moon is iron-poor (~3.3 g cm⁻³ — it is
   re-accreted mantle debris with no core, which is exactly why Luna is 3.34
   against Earth's 5.51); a captured body may be icy (~1.5–2.0). `radius_km`
   then follows from real density rather than an assumption. This is the single
   biggest honesty upgrade available to B.
2. **§5's "crater density is drawn because there is no age" admission narrows —
   but does not vanish**, and the reason is physically interesting.
   **Crater density saturates**: impact flux collapsed after the first ~700 Myr,
   and the lunar highlands are *saturated* — so heavily cratered that new
   impacts destroy old ones and the count stops rising. Past ~1 Gyr, body age
   barely changes a face. What actually varies is **surface age**, and *maria
   reset the clock* (lunar maria flooded 3.1–3.9 Gyr ago, erasing everything
   beneath — which is precisely why they are smooth and dark against the
   battered bright highlands). So B's model becomes: **highlands saturate**
   (fixed density, age-independent); **maria carry density ∝ (age − flood
   time)**. That contrast *is* the Moon's face, and it arrives for real reasons.
3. **§5's `maria_fraction` invented constant weakens**: flood timing and extent
   both follow from how long the body stayed hot, which follows from mass — so
   the rule gains a physical spine instead of a calibration to Luna.
4. **Captured moons become possible for the first time**, which B's §4 must
   handle: an irregular body is small, high-inclination, possibly retrograde,
   and compositionally alien. An icy moon has no maria at all.

Everything below this section predates the split and is preserved as written.
Read §§3–4 (the contract shape and the derive-vs-generate rule) as still
binding — those were ratified and are what B is. Read §5's model card as
**superseded in the ways listed above**.

## 1. Problem

`scene/system/v1` gives each moon orbital elements only — `sidereal_days`,
`distance_mm`, `size_rel`, `phase_offset`. The orrery draws them as flat grey
spheres. They have no faces.

The generator knows more than it says. Per moon it carries **mass** (drawn,
0.05–2.5 lunar masses), distance, period (Kepler III), angular diameter,
tidal strength, inclination, and node longitude. Mass is load-bearing
physics — it fixes radius, surface gravity, and whether the body ever melted.

## 2. Goal

`scene/moon/v1`: per moon, the **derived physical scalars** plus a **drawn
crater and mare population**, with a **normatively documented profile
evaluator** that reconstructs elevation and surface class at any resolution.
Producer-sourced contract tests on both sides of the repo boundary; the orrery
consumes it and the moons get faces.

## 3. Why a population and an evaluator, not a texture and not a lattice

hv#4 proposes two tiers. Both are rejected, for reasons worth stating once:

**A client-side procedural texture** (the ticket's "cheapest" tier — ship a
few parameters, let the client build the surface "seeded from the world seed +
moon index") **violates decision 0022.** The line, which this spec makes
explicit because it will recur:

> A client may **derive** presentation from shipped quantities through a
> documented evaluator. A client may never **generate** world content.

The Lens's ice overlay and wind arrows are derivations — pure functions of
shipped temperature and band count. A procedural crater field derives from
nothing; it invents *where the craters are*, which is a fact about the world,
authored in TypeScript, on which two clients need not agree. The sim owns the
world.

**A fixed per-moon lattice** is honest but hits two walls. The kernel has only
2D noise (`value_noise_2d`, `fbm_2d`), which tears at the antimeridian and
degenerates at the poles; astronomy cannot borrow terrain's geosphere, because
a domain depends on the kernel and nothing else. A lattice therefore needs new
3D noise in the kernel — **new hash constants, a save-format-contract
change** — to solve a problem this campaign does not actually have. And it
bakes in a resolution floor that the sequel immediately hits: you cannot stand
on a 64×32 lattice.

**A moon's surface does not want noise. It wants craters, and craters are
objects.** A moon's face *is* its impact population — real crater
size-frequency distributions are power laws, and surface roughness at every
scale simply *is* craters, all the way down. Elevation is the sum of crater
profiles by great-circle distance: seam-free by construction, pole-safe, no
new kernel constants, and physically the honest model rather than a texture
imitating one.

This is **The Isotherm's pattern in space instead of time.** That campaign
shipped parameterized quantities (`t_mean_c`, `t_swing_c`,
`season_period_days`) plus one normative evaluator a client reconstructs
across the year, pinned by a producer-sourced contract test on both sides. A
crater population plus a profile evaluator is the same move, and it inherits
the same property that made it good: **resolution-freedom.** The client
evaluates at whatever density its camera needs.

## 4. The contract — `scene/moon/v1`

One document per moon, addressed by moon index (generation order, matching
`scene/system/v1`'s `moons` array). Every number quantized at the emit
boundary (8 significant digits, decision 0033).

### 4.1 Document-level scalars

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | `"scene/moon/v1"` |
| `moon_index` | integer | Position in `scene/system/v1`'s `moons`, generation order. |
| `radius_km` | number | **Derived** from mass at an assumed bulk density (§5). |
| `surface_gravity_ms2` | number | **Derived**: `GM/r²`. |
| `maria_fraction` | number | **Derived** from mass (§5): the fraction of the surface flooded by basalt. Exactly `0` on bodies below the differentiation threshold. |
| `albedo` | number | **Derived** from `maria_fraction` (§5). |
| `craters` | array of object | The drawn impact population (§4.2). |
| `maria` | array of object | The drawn mare basins (§4.2). |
| `surface_class_legend` | array of string | Stable append-only order; `surface_class` in §4.3 indexes it. v1: `["highland", "mare", "crater-floor", "ejecta"]`. |

### 4.2 The populations

Each crater:

| Field | Type | Meaning |
|---|---|---|
| `lat`, `lon` | number | Centre, degrees. Uniform on the sphere (equal-area). |
| `radius_km` | number | Drawn from the size-frequency power law (§5). |
| `depth_km` | number | **Derived** from `radius_km` by the depth–diameter relation (§5). |

Each mare:

| Field | Type | Meaning |
|---|---|---|
| `lat`, `lon` | number | Centre, degrees. |
| `radius_km` | number | Basin radius. |
| `depth_km` | number | Flood depth below the highland datum. |

Both arrays are ordered by draw order and are part of the contract: the same
seed yields the same populations in the same order.

### 4.3 The evaluator (normative — documented verbatim in the book, pinned on both sides)

Elevation, in km relative to the mean radius, at a point `p` on the sphere:

```
elevation(p) = Σ_mare  mare_profile(θ(p, m) · R / m.radius_km) · (−m.depth_km)
             + Σ_crater crater_profile(θ(p, c) · R / c.radius_km) · c.depth_km
```

where `θ(a, b)` is the **great-circle angular separation** in radians, `R` is
`radius_km`, and the profiles are pure functions of the normalized radial
distance `x` (distance from the centre in units of the feature's radius):

```
crater_profile(x) =            // returns a signed multiple of depth_km
    x²  − 1                    for x < 1        (parabolic bowl; −1 at centre, 0 at rim)
    RIM · (1 − (x − 1)/0.3)    for 1 ≤ x < 1.3  (raised rim, decaying to 0)
    0                          for x ≥ 1.3

mare_profile(x) =
    1                          for x < 1        (flat flooded floor)
    0                          for x ≥ 1
```

with `RIM = 0.15` (the rim rises to 15% of the crater's depth). Both profiles
are C⁰ and vanish outside their support, so a point's elevation depends only
on the features whose support contains it.

`surface_class(p)`, in legend order of precedence:

```
ejecta        if 1 ≤ x < 1.3 for the SMALLEST-radius crater whose support contains p
crater-floor  if x < 1 for any crater
mare          if x < 1 for any mare
highland      otherwise
```

Ties break by draw order (earlier wins), which is what makes the class
deterministic where features overlap.

**Great-circle separation** is computed with the haversine form, for
conditioning at small angles:

```
θ = 2 · asin( sqrt( sin²(Δlat/2) + cos(lat₁)·cos(lat₂)·sin²(Δlon/2) ) )
```

The producer states this formula; the client uses it verbatim. A
producer-sourced golden (§7) pins agreement rather than a JS reconstruction —
the Isotherm's hard-won lesson.

## 5. The model card (what is physics, what is dice, what is admitted)

**Derived (real formulas):**
- **Radius** from mass at an assumed bulk density: `r = (3M / 4πρ)^(1/3)`.
- **Surface gravity**: `g = GM/r²`.
- **Depth–diameter**: craters follow the observed simple-crater relation
  `depth ≈ 0.2 · diameter`, i.e. `depth_km = 0.4 · radius_km`.
- **Albedo** from composition: `albedo = f·0.07 + (1 − f)·0.13` for
  `f = maria_fraction` — basalt maria against anorthosite highlands.

**Drawn (from the seed, per moon, from its own stream):**
- The crater count and each crater's centre and radius, from the
  size-frequency power law (below).
- Each mare's centre, radius, and flood depth.

**Approximated (declared) — this section is the honest part:**
- **The bulk density is assumed lunar** (3.344 g cm⁻³). The generator draws
  mass, not composition, so density is an assumption, not a derivation. Every
  moon is therefore implicitly rocky; there are no icy bodies in v1.
- **`maria_fraction` from mass is a rule, not a simulation.** Radiogenic heat
  scales with volume (∝ M) and is lost through the surface (∝ M^{2/3}), so
  retained heat scales as M^{1/3} — a body must exceed a threshold to melt and
  flood at all. v1 uses
  `maria_fraction = clamp(0, 0.35, K · (M^{1/3} − M_min^{1/3}))`, calibrated so
  a 1.0-lunar-mass body lands near Luna's observed ~16%. This is a
  defensible monotone rule with the right physical *shape*; it is not a
  thermal history.
- **Crater density is drawn, not integrated.** Crater density is impact flux ×
  age, and **the model has no age** — not for the star, not for the system,
  not for the moon (verified: nothing in `domains/astronomy` carries one).
  Inventing an age to derive density from would be invented precision
  masquerading as physics. So the crater count is drawn from its own stream
  and the contract says so plainly. **This is the single biggest admission in
  the campaign, and it is a feature: the surface is honest about being dice.**
- **The size-frequency power law** (`N(>D) ∝ D^{−2}`) has the right form and an
  uncalibrated constant. No secondary cratering, no crater relaxation, no
  degradation with age (no age), no tidal-heating volcanism, no ray systems.
- **No far-side/near-side asymmetry.** Luna's maria are famously nearside-heavy
  because of crustal thickness asymmetry from tidal locking. v1 places maria
  uniformly. Tidal locking is *knowable* here (`tide_rel` exists), so this is a
  deliberate v1 simplification with an obvious promotion path, not an oversight.

## 6. Determinism and streams

**New stream label**, declared in `domains/astronomy/src/streams.rs` and
published through `stream_labels()` into the generated manifest:
`MOON_SURFACE = "moon-surface"`.

**No epoch, and no existing world changes.** The precedent is exact and has
been run twice: `inclination_deg` (SKY-6) and `node_longitude_deg` (Eclipse
Seasons) were both added as new per-moon quantities drawn **after admission
from their own streams**, additively, without an epoch. Because the surface
draws come from a *new* label, no existing draw moves and every existing
world's moons keep their orbits, periods, and eclipses byte-for-byte. The
pin-isolation tests (`domains/astronomy/tests/genesis_properties.rs`) are the
guard: a pinned path must consume the same draws as the unpinned one.

Generation lives in `domains/astronomy` (crate `hornvale-astronomy`), where
`Moon` lives — **not** in terrain, and it borrows nothing from terrain: a
domain depends on `hornvale-kernel` and nothing else. It needs no kernel
change at all, which is the point of §3: great-circle distance and closed-form
profiles need no noise field.

## 7. Testing

- **Producer property tests** (`domains/astronomy/tests/`): the same seed
  yields the same populations in the same order; crater radii follow the drawn
  power law; `maria_fraction` is 0 below the threshold and monotone in mass;
  every crater centre is a valid lat/lon; radius/gravity match closed form.
- **Evaluator restatement test (producer-side):** the §4.3 evaluator,
  implemented once in Rust as the ground truth, is asserted to reproduce the
  generator's own elevation field at sampled points. If the profile shape ever
  changes, this fails and the change is recognized as a **contract** change,
  not a refactor. (The Isotherm's `temperature_at` restatement, exactly.)
- **Pin isolation:** a `--moons` pinned world consumes the same draws as the
  unpinned path (the existing battery, extended).
- **Cross-repo golden:** a **producer-sourced** CSV of
  `(moon_index, lat, lon) → elevation, surface_class` triples, generated in
  hornvale and committed to the orrery's testdata, pinning the client
  evaluator against Rust. **Producer-sourced, never a JS reconstruction** —
  this is The Isotherm's explicit retrospective lesson.
- **Seam/edge cases:** a point antipodal to a crater; a crater centred on a
  pole; a crater spanning the antimeridian; two overlapping craters (class
  precedence); a moon with zero maria.

## 8. Non-goals

- **The orrery's full moon rendering** — this campaign ships the contract, the
  producer, the wasm export, and **one focused consumer proof** (parse +
  evaluate + render one moon). The polished moon view rides with orrery#4. The
  Isotherm/Region precedent: producer-heavy campaign, one consumer proof,
  broad client feature deferred.
- **Standing on a moon** (orrery#4). This unblocks it; it is not it.
- **Icy moons / composition variety.** Density is assumed lunar (§5). A
  composition axis is a later campaign and a new drawn quantity.
- **Crater degradation, secondaries, rays, relaxation, tidal volcanism.** All
  need an age or a thermal history the model does not have.
- **Near-side/far-side maria asymmetry.** §5; deliberate v1 simplification.
- **Moon surfaces in `scene/tiles/v1`.** Rejected: forcing a moon through the
  world's schema means faking ocean/biome/moisture/plate or making them
  optional, weakening the contract for every existing consumer.

## 9. Flagged for G3

1. **Save-format contract (leads the section, per the carve-out rules):** one
   new stream label, `moon-surface`. **No epoch; no existing world changes** —
   the draws come from a new label after admission, exactly as SKY-6 and
   Eclipse Seasons did. Confirming this reading is the main ask.
2. **The `maria_fraction` rule is the campaign's one invented constant.** Its
   *shape* is physical (retained heat ∝ M^{1/3}); its constant `K` is
   calibrated to hit Luna's ~16% at 1.0 lunar masses and is otherwise
   uncalibrated. It is declared as approximated. If you would rather ship
   `maria_fraction = 0` for v1 and defer flooding entirely, that is a smaller
   and even more honest campaign — say so.
3. **Crater density is drawn because the model has no age.** Flagged not
   because it is contentious but because it is the biggest admission, and the
   book should carry it plainly rather than let a reader assume the craters
   were counted from an impact history.
4. **The derive-vs-generate line** (§3) is stated here for the first time as a
   general rule. It reads as a decision-log candidate, not just a spec
   paragraph — worth your call on whether it should be ratified as one.
5. **No census exposure, no AWS spend, no calibration regen.** Additive
   generation behind a new stream; existing worlds byte-identical.
