# Drawn weather — felt sky, realistic clouds (The Firmament) — design

**Date:** 2026-07-19 · **Status:** awaiting G3 review
**Program:** Weather Program campaign 4 (after The Turning's diurnal, The Gyre's currents, The Rains' precipitation).
**Epoch:** NO — a level-0 additive derived read; byte-identical; no census regen.

## 0. Program context and the level it climbs

The Weather Program: diurnal (shipped) → wind/ocean-current vector fields
(shipped) → precipitation epoch (shipped) → **drawn weather (this spec)**,
under the Lorenz guard-rail — fields and drawn/sampled events, never a
forward-integrated storm engine (chaotic, unresumable from a quantized save).

Every prior campaign gave the smooth analytic climate another *mean*. This
one gives it **texture in time**: the fast, stochastic weather over the slow
climatology — the "models author, dice roll" pattern applied to the sky. It
is deliberately **level 0** (The Gyre's coupling discipline): a pure derived
read that feeds observation only. It changes nothing a biome reads, writes no
ledger fact, and reorders no draw, so it is byte-identical and not an epoch.
The two heavier couplings an ideonomy pass surfaced — the storminess
climatology that would grow **weather-gods** (a religion cascade → epoch), and
the disturbance feedback that would move **biomes** (the level-2 tier) — are
deliberately cleaved off (§7).

## 1. The weather-state model (`CLIM-weather`)

A place's sky is a small **state machine** — `Clear · Fair · Overcast · Rain
· Storm`, with a high `Cirrus` overlay that composes independently. The
states are authored; which state a cell is in on a day is a **sampled read,
never a stepped integration**:

```
weather_at(fields, cell, day) = classify(
    propensity(climate-fields at cell),      // the slow prior
    noise_phase(cell.position, day, seed)    // the fast stochastic pick
)
```

- **Propensity** is a per-cell tendency drawn from the fields already
  computed: moisture, the rising/subsiding circulation band (uplift),
  temperature and its instability, ocean proximity. A wet, rising, warm cell
  is storm-prone; a dry, subsiding, cool one is clear-prone. This is the
  climatological *bias* of the noise, not a per-day value.
- **The noise phase** is an Fbm-style field over `position × day` (the
  kernel's derive-once noise pattern, evaluated at the day coordinate), smooth
  in time. Smoothness is load-bearing: because the phase moves continuously
  from day to day, the sampled state slides between *adjacent* states
  (`Overcast→Rain`), so the machine's forbidden transitions (`Clear↛Storm`, a
  sky "builds"; `Storm↛Clear`, a storm "clears") **emerge from the sampling**
  rather than being enforced by a rule.

Pure function of `(cell, day, seed)`; nothing integrates; no per-day memory is
stored. The Lorenz guard-rail holds by construction. A locked world (no band
structure) still has a defined propensity from its substellar fields; it
carries weather, biased toward its substellar/terminator geometry.

## 2. Realistic clouds — the state's own face (`CLIM-clouds`, presentation)

`CloudType { None, Cumulus, Stratus, Nimbostratus, Cumulonimbus, Cirrus }` is
a **pure projection of the weather state** — cumulonimbus *is* Storm, stratus
*is* Overcast, nimbostratus *is* Rain, cumulus *is* Fair, and Cirrus is the
high, thin overlay (a storm precursor). "Realistic clouds" is exactly this
typing plus the client rendering it: there is **no separate cloud engine** to
build. This subsumes the *presentation* side of the diagnostic
`cloud_fraction` field The Rains shipped — that field remains as the
climatological cloud *amount* (a mean); weather adds the instantaneous *type*.

## 3. Felt weather at a place and day (the payoff)

The primary deliverable, sim-side and deterministic:

- **Possession / REPL** query `weather_at(here, now)` and narrate the sky —
  *"Overcast, with rain building from the west" · "A storm breaks overhead" ·
  "Clear, a few fair-weather clouds."* Weather you experience where you stand,
  on the day you stand there.
- **The almanac** reports the sky at its sample sites on its reference day —
  the same driest-interior / open-ocean sites the rains line already uses,
  gaining a weather sentence.
- **Observation only.** No ledger fact is written; nothing persists. This is
  what keeps the campaign level 0.

## 4. The client — realistic clouds on the Living Globe

- The scene emits the per-tile weather **propensity** and the day's
  **cloud-type** snapshot as additive `scene/tiles/v1` fields.
- The orrery upgrades its clouds overlay from a uniform white fraction to
  **typed clouds** — cumulus stipple, stratus sheets, towering cumulonimbus
  over storms, high cirrus wisps — evolving as the day-clock scrubs.
- **Determinism boundary:** the client animates typed clouds from the emitted
  propensity as **determinism-waived eyecandy** (the Living-Globe rule,
  decisions 0022/0023, exactly like the current clouds/currents advection), so
  no noise port to TypeScript and **no golden pin** are needed. The sim's
  `weather_at` stays the single authority for the felt/narrated payoff; the
  client's animation is the plausible, non-authoritative picture of it. A
  higher-fidelity option (client re-derives the exact per-day state, golden-
  pinned) is explicitly deferred — the eyecandy path delivers "realistic
  clouds" without it.

## 5. Determinism and the level

**Level 0, no epoch, byte-identical:**

- Weather is a pure derived read (climate fields + an additive noise field
  over space×time). It **consumes no existing draws and reorders no labels**
  (the noise is evaluated positionally, the Fbm derive-once pattern; if a new
  seed-derivation label is introduced it is drawn independently and appended,
  perturbing no existing stream), writes **no ledger fact**, and changes
  **nothing a biome reads**.
- Therefore every committed **world, ledger, and census metric is
  byte-identical**. This is **not** an epoch and needs **no census regen**.
- Two classes of committed artifact gain **additive** content and re-pin (the
  same discipline as The Rains' precip fields — an additive change, not an
  epoch): the `scene/tiles/v1` byte-pin (new weather propensity + cloud-type
  fields), and the seed-42 almanac gallery renders (a new weather sentence at
  the sample sites). Neither touches the world, the ledger, or the census.
- New **world-wasm release (v11)** for the added scene fields; orrery re-pin
  at G6. No golden (client parses + animates, recomputes nothing).

## 6. Testing

Producer property tests (mutation-verified; mechanism-isolating, not value-
measuring):

- **Propensity tracks the fields** — storm-prone where wet + rising + warm,
  clear-prone where dry + subsiding; flip the propensity sign → fails.
- **Temporal smoothness** — sampling a cell across consecutive days rarely
  jumps between non-adjacent states (the forbidden-transition structure
  emerges); a discontinuous noise phase → fails.
- **Cloud type is the state's projection** — each state maps to its one cloud
  type; the mapping is total.
- **Determinism** — same `(seed, cell, day)` → same state; byte-identical
  rebuild; a locked world carries defined weather.

Client: `parseTiles` gains the weather fields; the typed-cloud overlay builds
and animates; **visual verification (The Lens rule)** — typed clouds gather
over the storm-prone belts, cirrus reads high and thin, and a place's sky
*builds and clears* believably as the day-clock scrubs.

## 7. Non-goals (the fence — each deferred with a home)

- **Storm-as-object + cyclone tracks** — a named cyclone with a parametric
  path (`position(day)` = genesis + a precomputed, quantized steering
  integral, sampled — memory without a stepper) advected across the globe,
  the Living-Globe spectacle. A **sequel** (the field ships first, the object
  later — The Gyre's shape). Captured under `CLIM-weather` / a storm-track row.
- **Weather-gods** — a persistent **storminess climatology** (a smooth
  per-region field: warm-SST × latitude-window for cyclones, front-frequency
  for temperate storms) feeding the phenomena stream so religion grows
  weather-deities. This is a **sequel epoch** (beliefs re-derive → census
  regen) — `CLIM-weather`'s connected half. The ephemeral per-day sampler here
  deliberately writes nothing so this stays cleanly separable.
- **Disturbance → biome feedback** — fire/flood-maintained biomes
  (`CLIM-hazards` → succession) is the **level-2 feedback tier**.
- **CLIM-hazards extremes** committed as dated omen-grade facts (drought,
  flood, wildfire) — parked.
- **CLIM-air-masses** — explicit air-mass/front *objects* with modeled
  boundaries; here the frontal character is implicit in the propensity + noise,
  not a modeled object.
- **Precipitation phase** (`CLIM-precip-phase`, sleet/hail) — stays parked.

## 8. Flagged for G3

1. **No epoch, no census (the good news):** this is a level-0 additive derived
   read — byte-identical worlds/almanacs/census, no regen. Confirming this
   reading is the main ask; the heaviest carve-out is only the v11 release.
2. **world-wasm-v11 release + orrery re-pin + pushes** — the G6 carve-outs
   (externally visible), same shape as v10.
3. **Client determinism boundary:** typed clouds animate as determinism-waived
   eyecandy from the emitted propensity (no golden), sim `weather_at`
   authoritative — flag if you want the exact-reconstruction/golden path
   instead.
4. **Fidelity:** the propensity weighting (which field combination makes a cell
   storm- vs clear-prone), the state thresholds, and the noise time-scale (how
   many days a weather system persists) — visual/plausibility-tuned; flag any
   target.

## 9. Decisions (promoted from the campaign ledger)

- **One campaign, clouds via weather** (Nathan): cloud type is the weather
  state's own face, not a separate model — the state machine unifies them.
- **Primary payoff: felt weather at a place/day** (Nathan): the sim narrates
  the sky at your location and day (REPL/almanac/possession).
- **Level 0, no epoch** (Nathan): ship the sampled state field as a pure
  observation read; the weather-gods religion coupling (an epoch) is a sequel.
- **Defer storm-as-object / tracks** (Nathan): the synoptic-state field ships
  first; cyclone objects with parametric tracks are a sequel (field-first, the
  Gyre's shape).
- **Sampled state Field, never stepped** (G1, ideonomy: substitution/negation/
  state-machine, 1 pass, enriched not overturned): the Lorenz guard-rail as an
  axis swap (stepped → sampled); the state machine's forbidden transitions
  emerge from time-smooth noise.
- **Client eyecandy from propensity, no golden** (G2): weather rendering is
  determinism-waived (0022/0023); the sim `weather_at` is the authority.
