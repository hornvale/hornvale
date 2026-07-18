# Diurnal Temperature — design

**Date:** 2026-07-18 · **Status:** awaiting G3 review
**Program:** the first campaign of the **Weather Program** (below).
**Sibling:** The Wandering Sun (seasonal/locked libration on `temperature_at`);
this is its intra-day twin.

## 0. The Weather Program (context — this spec builds only the first campaign)

Nathan's ask — "diurnal temperatures, wind/current vector fields, working
toward precipitation and storm/meaningful weather" — is a multi-campaign
**program**, and it maps onto an already-drawn frontier roadmap. The
governing constraint is constitutional: the sim core **never
forward-integrates a chaotic system** (the Lorenz guard-rail). "Weather"
here means **deterministic fields + seeded, drawn disturbance events**, not a
dynamical storm engine; the client (orrery) advects particles over those
fields for the *felt* motion, non-deterministically, never re-entering world
identity (decisions 0022/0023 — "keyframes from the sim, life from the
client").

Proposed sequence (each its own spec → plan → merge):

1. **Diurnal temperature** — MAP-24's diurnal harmonic (**this spec**).
2. **Wind + ocean-current vector fields** — `CLIM-ocean-currents` (gyres from
   the band winds × Coriolis) + a time-aware wind field; carries the
   `CLIM-operators` enabling refactor (the kernel `Field` trait — see §7).
3. **Precipitation** — `CLIM-precip-units` (moisture→mm/yr, typed) +
   `CLIM-moisture-transport` + `CLIM-clouds`.
4. **Weather / storms** — MAP-24 storm-scale + the drawn-disturbance pattern
   (BIO-36): storminess as a field, individual storms **drawn and narrated**,
   never integrated.

The client payoff across the program is the Living Globe (MAP-23) advecting
these fields.

## 1. Goal

Give the climate a **day/night temperature cycle**: a third term on
`temperature_at` beside the mean and the seasonal anomaly, so a spinning
world's deserts bake by day and freeze by night while its oceans barely move,
and the orrery's temperature lens **pulses** as the terminator sweeps. Pure
byte-identical read-layer — no epoch, no new draws.

## 2. The model

```
T(cell, day) = mean(cell) + seasonal_anomaly(cell, day) + diurnal_anomaly(cell, day)
```

**Load-bearing invariant:** `diurnal_anomaly` is **zero-mean over one
rotation**. Averaged across a day it cancels, so `mean_temperature` — the
value the census reads and biomes classify on — is unchanged. This is what
keeps worlds and census byte-identical.

```
diurnal_anomaly(cell, day) = A_climate(cell) x D(cell, day)
```

- **`D(cell, day)`** — the normalized diurnal waveform in ~[-1, +1]: the
  **zero-mean first harmonic of the day's clamped solar elevation**,
  phase-shifted by a thermal lag so its peak falls in local **afternoon** and
  its trough near dawn. Its harmonic amplitude scales with the day's
  **daytime span**, so it flattens to ≈0 under polar day / polar night and
  swings wider as a longer rotation day gives more time to heat and cool. The
  solar elevation is computed **climate-locally** from `(latitude, seasonal
  declination(year_phase), hour_angle(day_fraction))` — the same parameters
  the seasonal/locked terms already use — so **no cross-domain dependency on
  astronomy** is introduced (constitutional layering holds); all trig routes
  through `kernel::math`.
- **`A_climate(cell)`** — the per-cell **diurnal amplitude** (half the diurnal
  temperature range), a stable, season-independent, identity-bearing scalar
  ≥ 0, precomputed once per world (like the mean field) and shipped in the
  scene. Drivers, accuracy-first:

  ```
  driver          big swing       small swing       source
  --------------  --------------  ----------------  -------------------------
  dryness         desert          humid             moisture_field (1 - m)
  continentality  deep interior   maritime / ocean  land vs sea (elev/sea_level)
  elevation       thin air, high  sea level         ReferenceElevation
  ```

  `A_climate = BASE_DTR/2 x dryness x continentality x (1 + elevation_gain)`,
  the constants calibrated so the range reads Earth-like: hot desert ~20-30°C
  peak-to-peak, humid tropics ~8°C, open ocean ~1-2°C.

**Regime:** the diurnal term applies to **spinning worlds only**. Locked
worlds (no rotation, no day/night) return exactly zero diurnal anomaly — The
Wandering Sun already owns their seasonal libration. `RotationRegime::Locked`
is untouched.

**Deferred fidelity (each with a home, Nathan's call to pull forward):**
diurnal *asymmetry* (fast morning warm-up / slow evening cool-down → a higher
harmonic); *cloud* damping (`CLIM-clouds`, the precipitation campaign);
exact DTR calibration targets.

## 3. Producer contract (sim + scene)

**`domains/climate/src/diurnal.rs`** (new — the "A2 seam", shaped so a future
`Field` impl wraps it without rework, see §7):
- `diurnal_amplitude(moisture: f64, continentality: f64, elevation_above_sea: ReferenceElevation) -> f64`
  — the per-cell `A_climate` (°C).
- `diurnal_waveform(latitude_deg: f64, obliquity_deg: f64, year_phase: f64, day_fraction: f64) -> f64`
  — the normalized `D` in ~[-1,+1], zero-mean over the rotation,
  afternoon-lagged.
- `diurnal_anomaly(amplitude: f64, latitude_deg, obliquity_deg, year_phase, day_fraction) -> TempAnomaly`
  — their product.

**Provider (`domains/climate/src/provider.rs`)** precomputes a
`diurnal_amplitude: CellMap<f64>` once per world (from the moisture, geosphere,
and elevation it already holds), mirroring how `mean` is precomputed.

**`temperature_at` (`domains/climate/src/temperature.rs`)** gains a
`diurnal_amp: &CellMap<f64>` parameter and adds `diurnal_anomaly(...)` in its
**`Spinning` branch only**, sitting beside the seasonal term (preserving the
seasonal↔diurnal symmetry: latitude-over-the-year vs longitude-over-the-day).

**`scene/tiles/v1`** gains one additive per-cell field **`t_diurnal_amp_c`**
(the precomputed `A_climate`, quantized at emit — decision 0033). No new wasm
export: it flows through the existing `hw_scene_tiles`; **world-wasm-v8**
carries the richer tiles scene. The client *reads* the amplitude and
*re-derives* `D` (§4 golden).

**Almanac:** a small **diurnal-range line** per sample site (peak-to-peak
swing °C, hottest/coldest local hour) — a text-side check independent of the
client.

## 4. The orrery consumer (`the-diurnal-cycle` branch)

- `parseTiles` (`src/sim/scene.ts`) adds `t_diurnal_amp_c` (strict, quantized,
  house style).
- The **temperature lens** adds `t_diurnal_amp_c × D(tile, clock-time-of-day)`
  to the `mean + seasonal` it already renders. `D` is computed client-side
  from the solar geometry the orrery already reconstructs (The Wandering Sun's
  `systemSeasonalContext`/`lockedClimate` machinery) evaluated at the clock's
  **fractional day** (the HUD already carries time-of-day, e.g. "Day 103 ·
  16:48"). At spinning rates the terminator sweeps and the colors pulse; at
  the fast seasonal-hold rates the diurnal term holds at the current
  time-of-day, so "watch a year" is unaffected.
- A small **"watch a day"** toggle mirroring "watch a year" — hold the season,
  ensure a spinning rate — so the diurnal cycle is discoverable, not just
  emergent.
- **Cross-repo golden:** a producer-sourced golden pins the client's `D`
  reconstruction at **tile centers × several times-of-day** (The Wandering
  Sun's eval-point lesson: same formula *and* same point; and per
  parse-vs-recompute, a recomputed value earns a committed golden).
- **Re-pin:** `wasm:release` → **world-wasm-v8** at G6.

## 5. Determinism and safety

- **Byte-identical** worlds, ledger, census (the daily mean is unchanged —
  the zero-mean invariant, §2). No epoch, no new stream draws, no reordered
  labels.
- `scene/tiles/v1` output bytes change (the new field), so its **byte-pin
  fixture re-pins in the same commit** — the established additive-within-v1
  pattern (The Isotherm added `t_mean`/`t_swing`; The Wandering Sun the
  `locked` marker; neither bumped to v2). Re-pin in the drifting commit, never
  deferred to close.
- Quantize at emit only; all transcendentals via `kernel::math` (0041).
- The **Lorenz guard-rail is not in play**: this is pure closed-form, no
  integrator.

## 6. Testing

Producer property tests (mutation-verified — each must fail if the diurnal
term is stubbed to zero):
- **Zero-mean keystone** — `diurnal_anomaly` integrated over one rotation ≈ 0.
- **Byte-identity** — seed-42 world/ledger/census byte-identical vs.
  pre-campaign; keystone fixture refrozen from main's tip at merge.
- **Spinning-only** — locked worlds return exactly zero.
- **Amplitude monotonicity** — drier > humid, interior > maritime, high > low
  elevation; open ocean ≈ flat.
- **Phase** — the day's temperature argmax lands in local afternoon (after
  solar noon), the min near dawn.
- **Edge geometry** — poles at solstice swing ≈ 0; a longer rotation day
  yields a larger range.
- **Determinism** — byte-identical rebuild; trig via `kernel::math`.

Scene & cross-repo: golden + schema pin for `t_diurnal_amp_c`; the
`scene/tiles/v1` byte-pin re-pinned; the producer-sourced `D`-reconstruction
golden at tile centers × times-of-day.

Client: `parseTiles` field test; the diurnal-waveform math unit-tested; the
reconstruction golden-pinned; **visual verification (The Lens rule)** — render
seed 42, run the day, screenshot the terminator sweep, confirm day-warm /
night-cool / desert-big-swing / ocean-flat (open the PNGs).

## 7. Why not the kernel `Field` trait now (the A2 decision)

MAP-24 frames time-varying climate as the kernel `Field` trait finally using
its long-dormant `WorldTime` argument. An ideonomy pass
(organon-construction + tree-finding; scope/naturalness/animacy axes) found
the A-vs-B frame ("extend `temperature_at`" vs "do the `Field` trait now") was
a **scope confusion**: the diurnal *model* is settled and shared by both; only
the *housing* of the harmonic differs, and housing is a synthetic, reversible
choice that should follow consumers. The diurnal term's natural sibling is the
**seasonal term**, already housed as a free-fn branch in `temperature_at`;
lifting diurnal alone into the trait would split two symmetric harmonics
across two idioms. So: **A2** — extend `temperature_at`, but extract a clean
`diurnal` module (§3) whose signatures a `Field` impl wraps later. The trait
refactor is **re-placed** to the winds/currents campaign as `CLIM-operators`,
where it pays across multiple fields and migrates seasonal + diurnal
**together**.

## 8. Non-goals (the fence — deferred, each with a home)

- Not precipitation, winds, currents, or storms (later program campaigns).
- Not diurnal *asymmetry* or *cloud* damping (`CLIM-clouds`).
- Not coupling diurnal into biomes / frost / habitability / the walk layer —
  a read-layer only this campaign.
- Not the `Field`-trait refactor (`CLIM-operators`, winds campaign).
- Not locked worlds.

## 9. Flagged for G3

1. **Save-format (leads):** an additive per-cell field `t_diurnal_amp_c` on
   `scene/tiles/v1` + a `diurnal_amp` parameter on `temperature_at`. **No
   epoch** — worlds/ledger/census byte-identical (zero-mean invariant); the
   scene byte-pin **re-pins within v1** (additive precedent). Confirming this
   reading is the main ask.
2. **world-wasm-v8 release + orrery re-pin + origin pushes** — G6 carve-outs.
3. **Fidelity:** the accuracy-first amplitude model (dryness × continentality
   × elevation) and the deferrals (asymmetry, clouds, DTR calibration
   targets) — fidelity cuts are yours; flag any you want pulled forward.
4. **Program shape:** the four-campaign sequence (§0) — confirm the framing
   and that diurnal-first is right.

## 10. Decisions (promoted from the campaign ledger)

- **Framing** — weather is deterministic **fields + drawn events**, not a
  forward-integrated storm engine (Lorenz guard-rail; BIO-36; 0022/0023).
  Nathan confirmed.
- **First campaign** = diurnal temperature (MAP-24 diurnal harmonic). Nathan
  chose.
- **Reach** = sim + scene + orrery "watch a day" (two-repo, world-wasm-v8).
  Nathan chose.
- **A2 housing** — extend `temperature_at` with a clean `diurnal` seam module;
  defer the `Field` trait to `CLIM-operators` (winds campaign). Ideonomy:
  organon-construction + tree-finding, 1 pass, reframed A/B → A2 (scope
  confusion; seasonal↔diurnal symmetry), 0 overturn of "start with A". Nathan
  confirmed.
- **Zero-mean diurnal term** — preserves the daily/annual mean → byte-identical
  census, no epoch. (Precedent: The Wandering Sun's containment.)
