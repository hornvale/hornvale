# The Mire — weather's memory in the ground

**Campaign:** The Mire · Weather Consequence program, C1 (places half)
**Branch:** `the-mire`
**Date:** 2026-08-03
**Status:** spec, awaiting G3 review

---

## 1. Motivation

Hornvale's weather is fully realized and entirely inert.

`domains/climate/src/weather.rs` ships a real synoptic generator —
`WeatherState`, `CloudType`, `storm_propensity`, `weather_phase`. *The
Firmament* made it observable. *The Occlusion* made it occlude the sky.
*The Elements* made it felt (`CLIM-felt-weather`, salience = deviation from a
temperate baseline). And then nothing consumes it: `grep -rn
'rain\|storm\|wind\|cloud\|precip' windows/vessel/src/*.rs` returns zero hits.
The walk layer has never heard of weather.

The available next move *looks* like "rain reduces movement speed," which is
the least interesting place weather could live in a sim-first world — an
option that varies on one axis is a ladder wearing a menu
(`MAP-two-axis-options`), and `UNI-32` forbids manufacturing tension at the
avatar in any case.

The claim this campaign makes instead:

> **Weather is a rate. The dramatic quantity is its time-integral on a
> substrate with a decay constant.**

Consequence outlives the storm. The rain stops and the ford is still gone.
That single operator, at six different time constants, is what the whole
wishlist behind this program factors through — mud (ground, τ≈days), a wet
dog (a body, τ≈hours), wildfire risk (fuel, τ≈weeks), the harvest (soil,
τ≈a season), the closed pass (snowpack, τ≈months), the crushed hull (sea ice,
τ≈years).

The Mire proves the operator on **places**, at two time constants three
orders of magnitude apart. A sibling campaign takes **bodies**.

---

## 2. Scope

### In scope (C1, "option A")

1. A **substrate abstraction** — one extension-shaped trait, not two
   hand-rolled fields. The wider set (soil moisture, fuel dryness, sea ice)
   lands immediately behind this campaign and must not require editing it.
2. Two substrates:
   - **Surface wetness** (τ ≈ days) — the mire proper.
   - **Snowpack** (τ ≈ months) — the threshold case.
3. A **daily precipitation quantity** (see §4.1 — this does not exist today).
4. **Freeze state** as a *modifier*, not a substrate (see §4.4).
5. One **reader**: weather-gated edge conductance in `domains/topology`'s
   connection graph, filling `MAP-61`'s explicitly-deferred *"conductance
   gating in time by climate (seasonal lanes)"*.
6. A **preregistered study** (§6) measuring what that gating does to world
   topology across seasons and latitudes.

### Explicitly deferred

| Deferred | To |
|---|---|
| Bodies (wetness/chill carried on an entity) | sibling campaign |
| Soil moisture, fuel dryness, sea ice | "option B", immediately following |
| Tracks / footprints as findable facts | travels with bodies (who left it) |
| Behavioural gating (people off the streets) | C2, *The Gate* |
| Weather in speech and lore | C3, *The Remark* |
| Wind as transport (scent, sail) | C4, *The Bearing* |
| Discrete hazards (lightning, flood, wildfire) | C5, *The Blow* |
| Harvest, famine, migration | C6, *The Reckoning* |

### Non-goals

- No stat modifiers on the possessed agent. Friction lands on the world's
  topology, never on the avatar's numbers.
- No stored/mutable world state. No save-format change. No epoch.
- No new crate, no new external dependency.
- Prose describing wet or frozen ground is *welcome if nearly free*, but it
  is not the campaign's measurement (see §6's note on latency).

---

## 3. The operator

For a substrate `S` at cell `c` and day `d`:

```
S(c, d) = S(c, d-1) + source(c, d) - sink(c, d, S(c, d-1))
```

A **forward recurrence**, not a convolution.

### 3.1 Why not a convolution

An earlier framing had `S(c,D) = Σ_k w(c, D-k)·exp(-k/τ)` — a linear
decaying convolution, evaluable at any day without touching the days before
it. That framing is **wrong**, and the reason is load-bearing.

The decay rate is not constant. Snowpack does not decay at τ≈90 d: below
freezing it does not decay *at all*, and above freezing it ablates fast. The
rate is a function of the substrate's own state and of the temperature field.
A state-dependent rate is not a linear operator, so it cannot be evaluated as
a weighted sum over past weather.

Cost is unchanged — O(K) either way; the convolution's advantage was never
speed. What changes is that the **initial condition** becomes the real
problem, because a threshold substrate has *unbounded* memory in a cold
climate.

### 3.2 Spin-up to an annual fixed point

The forcing is seasonally periodic (orbital season × the drifting
`weather_phase`). So: iterate the recurrence from `S = 0` for successive
simulated years until the year-over-year trajectory converges within a
tolerance, then retain that converged annual trajectory for the cell.

- Surface wetness converges in **one** year.
- Snowpack converges in a **few**.
- Permanent ice **never converges** — it grows without bound. That
  non-convergence is not a bug: it *is* a glacier, and the iteration cap is
  the campaign's honest statement of "this cell accumulates indefinitely."

This is consonant with `CLIM-operators`' relaxation spine,
`CLIM-ice-albedo`'s explicit "relax to a fixed point", and
`CLIM-cryosphere`'s accumulation-minus-ablation balance.

**Determinism:** the whole thing remains a pure function of the world seed.
Weather is already pure in `(seed, lon, lat, day)`; the recurrence, the
spin-up, and the convergence test add no draws and no stored state. No
ledger commit, no serialization boundary, therefore **no quantization
concern and no epoch**. Reproducibility follows from the existing weather
determinism rather than from anything new.

### 3.3 The trait

```rust
/// A material that integrates weather over time and decays.
pub trait Substrate {
    /// What this substrate accumulates on day `d` at this cell.
    fn source(&self, ctx: &DayContext) -> f64;

    /// What it loses on day `d`, given how much of it is present.
    /// The `present` argument is what makes the sink state-dependent —
    /// the reason this is a recurrence and not a convolution.
    fn sink(&self, ctx: &DayContext, present: f64) -> f64;

    /// Iteration cap for spin-up. `None` = converges within a year.
    fn spin_up_years(&self) -> u32;
}
```

`DayContext` carries the per-cell, per-day environmental reads the
substrates need: daily precipitation (§4.1) split by phase, mean temperature,
and freeze state. Adding a substrate in "option B" means adding one impl —
no edits to the existing two. That is the constitutional
"adding a domain never edits an existing one" rule taken one level down,
which is exactly `CLIM-operators`' own argument.

---

## 4. Components

### 4.1 Daily precipitation — the missing quantity

**This does not exist today and is the campaign's largest new surface.**
Verified:

- `weather.rs` yields a **categorical** `WeatherState`
  (`Clear`/`Fair`/`Overcast`/`Rain`/`Storm`) — a rung, not millimetres.
- `precipitation.rs` yields `precip_mm_yr(moisture) -> Precipitation` — an
  **annual climatology**, not a daily rate.
- `weather_phase`'s own doc comment states: *"Deterministic in `(seed, lon,
  lat, day)`; nothing integrates."*

So C1 must mint a daily rate. The design constraint that keeps this honest:

> **The daily rate must sum, over a year, to the existing annual
> climatology.** Coarse constrains fine.

Approach: distribute `precip_mm_yr` across the year in proportion to a
per-day weight derived from the day's `WeatherState` intensity (zero for
`Clear`/`Fair`, rising through `Overcast`/`Rain`/`Storm`), normalized so the
annual sum is preserved per cell. Phase split into rain vs snow via the
already-shipped `snow_fraction(mean_temp_c)`.

This makes the new quantity a *refinement* of a shipped one rather than a
competing second source of truth — the "higher fidelity refines, never
contradicts, lower" rule. **It also gives the campaign a free, sharp
invariant test:** annual sum of daily precip must equal `precip_mm_yr` to
within tolerance, per cell, for every seed in the study population.

### 4.2 Surface wetness (τ ≈ days)

- **Source:** the day's liquid precipitation.
- **Sink:** evaporative drying, scaled by temperature and by the day's
  cloud fraction (already shipped as `cloud_fraction`); **suppressed below
  freezing** — frozen ground does not dry.
- **Saturates:** yes. Past a field capacity, additional rain does not
  increase wetness; it becomes runoff. A ceiling, not an unbounded integral.
- **Yields:** a `[0,1]` *receptivity* — how soft, how boggy, how print-taking
  the ground is.

### 4.3 Snowpack (τ ≈ months)

- **Source:** the day's solid precipitation.
- **Sink:** ablation, **zero below freezing** and rising with temperature
  above it. This is the state-dependent sink that forces §3.1.
- **Saturates:** no — it accumulates without an intrinsic ceiling, which is
  what makes the glacier case real.
- **Yields:** a depth.

### 4.4 Freeze state — a modifier, not a substrate

Freeze state has no integral. It is a threshold read straight off the
temperature field. It is **not** a seventh member of the family; it is the
thing that makes the family nonlinear — it gates snowpack's ablation,
suppresses surface drying, and (later, nearly free) turns a wet surface into
the ice that crunches underfoot.

Modelling it as a peer substrate would have been the obvious mistake; the
grid in the ledger (`#10`) is what caught it.

### 4.5 The reader — weather-gated conductance

`domains/topology/src/graph.rs:38` already carries
`Edge { to, kind, conductance: f64 }`, and `reachable_regions(min_conductance)`
already thresholds on it. The connection graph is derived on demand and
**never committed** (per `MAP-61`'s shipped slice), and *The Sundering*
already established the precedent of a time-varying graph.

So: a conductance modifier as a function of the substrate state at the edge's
endpoints, at a given day.

- Deep mud lowers `LandRoute` and `Adjacency` conductance (the stuck wagon).
- Deep snowpack lowers it further (the closed pass).
- A *frozen* mire **raises** it back — hard ground travels well. This
  asymmetry is deliberate and is the campaign's clearest demonstration that
  the modifier is not a monotone penalty dressed up as physics.
- `WaterRoute` is left untouched in C1; sea ice is option B's business.

**Latency note.** Substrate state is *latent* — mud is invisible until
something touches it. A field with no reader is unobservable, and
unobservable work is how tests that assert nothing ship green
(*The Sounding*'s headline was lost exactly here). This reader is therefore
in-scope, not optional.

---

## 5. Architecture and layering

| Piece | Home | Why |
|---|---|---|
| `Substrate` trait, wetness, snowpack, daily precip | `domains/climate` | Verified precedent: `domains/climate/Cargo.toml` depends on `hornvale-kernel` and nothing else, and `moisture.rs` already performs orographic work on terrain data received as `CellMap<ReferenceElevation>` parameters. Climate reading terrain via kernel types is established; no sibling dependency is created. |
| Conductance modifier | `domains/topology` or the composition root | Topology is kernel-only today. If the modifier needs climate types, it is composed in `windows/worldgen` and topology stays kernel-pure — decided at plan time against the actual type surface. |
| Composition | `windows/worldgen` | The composition root, where domains meet. |

No new crate. No new external dependency (the allowlist stays
`serde`/`serde_json`/`libm`). No `HashMap`/`HashSet`. No wall-clock time.

---

## 6. Preregistered measurement

Frozen **before** the code that would move it (decision 0016). Nothing
mechanical enforces this; the freeze lives here.

**Population.** 200 seeds, default pins, the standard icosphere mesh, **land
cells only**, evaluated at 12 days evenly spaced across one converged annual
trajectory. (Stating mesh + filter + seed count explicitly, per the
measure-the-population-you-apply-to rule; one world is an anecdote.)

**H1 — the swing exists, and is not absurd.**
The passable fraction of the connection graph at the default
`min_conductance`, computed per day and reduced to `(max − min)` across the
12 sample days, has a **median across seeds between 5% and 60%**.

- Below 5%: weather-gated conductance does not move world topology, and the
  drama of weather is local rather than systemic. A real, shippable finding.
- Above 60%: a world whose graph half-disconnects every winter is a bug, not
  a season. The ceiling is asserted deliberately — a floor without a ceiling
  is how an absurd-high value passes unnoticed.

**H2 — the axis is differentiated (the load-bearing one).**
The swing increases monotonically with `|latitude|` band. If the swing were
uniform across latitude, the gate would be adding *noise*, not *seasons* —
H1 could pass on pure noise, so H2 is what checks the model actually
differentiates the named axis.

**H3 — the invariant.**
Annual sum of daily precipitation equals `precip_mm_yr` per cell, within
tolerance, for every seed in the population. (§4.1.)

**Falsification is a finding.** If H2 fails, the honest headline is that the
substrate integral does not produce latitude-structured seasonality, and the
campaign reports that rather than retuning a constant to rescue it. Any
post-unblinding retune is stated in the chronicle.

---

## 7. Risks

| Risk | Assessment |
|---|---|
| **Daily precip is a new source of truth that could contradict the shipped annual climatology** | Mitigated by construction (§4.1: normalized to preserve the annual sum) and pinned by H3. This is the largest new surface and leads the G3 flagged list. |
| **Spin-up cost** | The gate budget is already ~15 min (*The Timekeeper*). Snowpack spin-up is per-cell over a few simulated years; if it lands in a hot path this is a real timing regression. Plan must budget it and the `ci` alarm will catch drift. |
| **Glacier non-convergence read as a hang** | Handled by an explicit `spin_up_years` cap per substrate; non-convergence is a *result*, recorded, not an error. |
| **The conductance modifier is a monotone penalty in disguise** | The frozen-mire-raises-conductance asymmetry (§4.5) is the specific guard, and it is testable. |
| **Trait shape wrong for option B** | The three option-B substrates were checked against the trait in the ledger's grid (`#10`); soil moisture needs a sink that relaxes to a *climatological equilibrium* rather than to zero, which `sink(ctx, present)` expresses. Sea ice additionally wants albedo feedback, which this trait does **not** express — flagged now as a known option-B extension, not a surprise. |

---

## 8. Definition of Done

- Two substrates implementing one trait; option-B substrates addable without
  editing them.
- Daily precipitation minted, with H3's annual-sum invariant test.
- Weather-gated conductance in the connection graph, with the frozen-ground
  asymmetry tested.
- The preregistered study run, with H1/H2/H3 reported **including nulls**.
- `make gate` green; artifacts regenerated and drift-checked
  (`make rebaseline` + the `git diff --exit-code` sweep over
  `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`,
  `docs/audits/`).
- Book: a chronicle entry (`book/src/chronicle/the-mire.md`), a freshness
  sweep, and a re-score of any Confidence Gradient bet this moves.
- A retrospective in `docs/retrospectives/the-mire.md`.
- Registry rows captured for the six intermediaries, the τ-ladder, and the
  deferred halves.

---

## 9. Decisions promoted from the ledger

See `.superpowers/sdd/decision-ledger.md` for the full entries.

- **The integral is a forward recurrence, not a convolution** (`#9`) — the
  state-dependent sink is definitional, not an implementation detail.
- **Freeze state is a modifier, not a substrate** (`#10`).
- **Fuel dryness is the inverse integral of wetness** (`#10`) — option B is
  cheaper than "six substrates" sounds, and `CLIM-hazards`' wildfire arm is
  nearer than its C5 slot implies.
- **C1 must ship a reader** (`#11`) — latent state with no consumer is
  unmeasurable.
- **Climate takes terrain as kernel-typed parameters** (`#8`) — precedent,
  not a new pattern.
