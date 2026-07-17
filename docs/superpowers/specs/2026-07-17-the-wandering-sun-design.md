# The Wandering Sun — design

**Date:** 2026-07-17 · **Status:** awaiting G3 review
**Origin:** seed 8 diagnosis (locked world, 21.79° obliquity: ice sits ~22°
off the night-region center at solstice).

## 1. Goal

Make the climate's seasonal geometry follow the true orbital season on
**both** world types, so the ice, the hot spot, and the day/night
terminator move as one — and give the orrery a way to *watch* that cycle
over a year. Two parts, producer + client (The Real Sky shape):

- **A. Obliquity libration** (climate producer; determinism-affecting).
- **B. "Watch a year" at the globe** (orrery client; cheap).

Success: on seed 8 (locked) the ice cap tracks the sub-solar point across
the year instead of holding at the equatorial antistellar; on a spinning
world the seasonal ice aligns with the sun instead of leading it by
`year_phase_offset`; and at the globe, 10 d/s and ~1 mo/s run the year
forward with the planet held still, ice and temperature visibly evolving.

## 2. The bug, stated once

The illumination follows the true orbital phase
`year_phase(t) = frac(day / year + year_phase_offset)` —
`Calendar::solar_declination` (astronomy) and the client's `subsolarPoint`
both use it. The **temperature** does not:

- **Locked worlds** — the substellar point is the hard constant
  `SUBSTELLAR = [1,0,0]` (equator; `climate/substellar.rs:10`), and
  `temperature_at` returns a static `base` (`temperature.rs:111`).
  Obliquity is ignored entirely, so the hot spot and ice never move, while
  the true sub-solar point librates ±obliquity in latitude over the orbit.
  Measured on seed 8: ice cap center of mass +2° latitude; night-region
  center swings to ∓21.8° at solstice → ~22–24° offset.
- **Spinning worlds** — the seasonal term phases on
  `frac(day / year_length)` (`temperature.rs:117`), dropping
  `year_phase_offset`, so the temperature season leads/lags the sun by that
  offset (seed 42: 0.209 turns). Invisible there only because seed 42's
  obliquity is 0.96°.

Both are the same defect: the seasonal temperature must be phased on the
same `year_phase` the illumination uses. The illumination is correct; the
temperature is not.

## 3. Part A — the producer fix

### 3.1 Spinning worlds (the one-liner)

Phase the seasonal term on `year_phase`, not bare `frac(day/year_length)`:
`temperature.rs:117` uses the calendar's `year_phase(day)` (with
`year_phase_offset`). The per-cell `t_mean`/`t_swing` coefficients
(mean + hemisphere-signed amplitude) are **phase-free and do not change** —
so **`scene/tiles/v1` is byte-identical for spinning worlds**; only
`temperature_at(day)` sampled values move. The client's `temperatureAt`
(orrery `climate.ts`) gains the same `year_phase_offset` in its phase
(removing the comment that documents the omission) — Part B's repo, but
conceptually this fix.

### 3.2 Locked worlds (the substantive change): a librating substellar

The seasonal excursion is driven by a **day-dependent substellar
direction** at latitude `obliquity · sin(year_phase(day))`, longitude 0 —
the exact expression `subsolarPoint` uses, so temperature and illumination
share one truth. Decomposition that contains the blast radius:

- **Annual-mean fields stay on the fixed equatorial substellar.** The
  libration's latitude averages to 0 over the year, so the *annual-mean*
  substellar is still the equator. `mean_temperature`, **moisture**, and
  **insolation** — every other consumer of the shared `substellar_cosine`
  — keep using `SUBSTELLAR = [1,0,0]` unchanged. Only the temperature's
  **seasonal deviation** uses the moving substellar. This confines A to the
  temperature seasonal term; moisture and insolation do not move.
- Locked-world temperature becomes **day-dependent** (today it is static).

### 3.3 Plumbing

`ClimateInputs` (`climate/provider.rs`) gains `year_phase_offset`, threaded
from the astronomy forcing through the `worldgen` composition root
(additive input field; the value already exists on `forcing`).

### 3.4 The scene-contract representation — **the G3 decision**

The client renders ice/temperature from `scene/tiles/v1`'s `t_mean_c`/
`t_swing_c` via its own `temperatureAt` reconstruction (Isotherm pattern:
client reconstructs, a producer-sourced golden pins it). That contract is
**mean + one hemisphere-signed harmonic** — built for latitude-based
(spinning) seasons. It **cannot faithfully carry the locked libration**:
an equatorial cell on the substellar path heats up *twice a year* (the
substellar crosses the equator at `year_phase` 0 and 0.5) — a **semiannual**
signal a single first harmonic structurally misses.

**Recommendation (leads G3):** for locked worlds, the client reconstructs
the moving-substellar temperature directly — the same Isotherm pattern the
existing `temperatureAt` already is, just a locked branch. The client has
everything it needs: each equirect cell's lat/lon → 3-D position, the
world's `obliquity_deg`/`year_phase_offset` (from `scene/system/v1`), and
`season_period_days`. It computes `substellar_dir(day)` and the substellar
cosine per cell, reproducing the producer's locked temperature mapping,
pinned against a **producer-sourced golden** (the `climate-triples` /
`region-temperature` pattern, extended to a locked seed). `scene/tiles/v1`
gains a compact **additive** marker for locked worlds (e.g. a `locked`
flag; obliquity/offset already ride in `scene/system/v1`) so the client
picks the locked branch. `t_mean_c`/`t_swing_c` stay as-is for spinning
worlds; for locked worlds `t_swing_c` is superseded by the reconstruction
(kept for schema stability, or documented as unused on locked worlds — a
G3 sub-decision).

**Alternatives considered:** (a) fit a *second* harmonic into the contract
(a new `t_swing2_c`/`t_phase_c` layer) — captures the semiannual but bloats
the contract for every world and still approximates; (b) producer emits
temperature **keyframes** at N reference days and the client interpolates
(the `living-globe` keyframes-from-sim pattern) — faithful and
logic-free-on-client, but a heavier document. Both are fallbacks if the
locked reconstruction proves unfaithful.

## 4. Part B — "Watch a year" at the globe (client)

- **Lift the globe speed cap.** `speedPolicy.ts` caps the globe at
  `maxMult: 86400` (1 day/s); raise it so 10 d/s and ~1 mo/s are offered
  (the HUD re-enables those buttons at the globe rung).
- **Freeze the diurnal spin at the fast rates.** In the globe view's
  `update(day)`, a "seasonal" mode (engaged when the active rate exceeds the
  old cap) holds the mesh rotation (`spinGroup.rotation.z`) and the
  sub-solar **longitude** fixed while the seasonal terms — sub-solar
  **latitude** and `temperatureAt(tiles, i, day)` — keep advancing with
  `day`. The planet holds a face; the ice line breathes; the sub-solar
  point drifts north–south. Slow rates (1 hr/s, 1 day/s) are unchanged
  (spin visible, watch a day). The ladder becomes *slow = watch a day, fast
  = watch a year*.
- **Locked worlds** don't spin (longitude fixed already), so there the cap
  lift alone suffices; the libration from Part A is what animates.
- Whatever lens is active does the work: **natural** = ice advance/recede;
  **temperature** = the field redistributing. A caption notes the hold
  ("holding the daily spin — watching the year").
- Pure client; no new producer dependency beyond A making locked worlds
  worth watching.

## 5. Determinism and safety

- **No ledger epoch.** Temperature is re-derived from the seed, never
  stored; no seed-derivation label, draw, or order changes. Pins on the
  determinism contracts are untouched.
- **Rebaseline, scoped:** spinning-world `scene/tiles/v1` is **byte-
  identical** (§3.1); locked-world `scene/tiles/v1` changes (a marker plus
  the now-meaningful seasonal), and `temperature_at(day)` sampled values
  change on every world with obliquity ≠ 0. Committed artifacts that sample
  temperature at a day drift: the three seed-42 almanacs (seed 42 spins,
  obliquity 0.96° — tiny but nonzero), the orrery climate goldens, and any
  **census** climate metric.
- **Census regen (your carve-out, leads G3 alongside §3.4):** the canonical
  census measures climate; if any census metric samples `temperature_at` at
  a day (vs annual mean/extremes), it drifts and wants an AWS regen. If it
  samples only annual statistics (mean, coldest = mean − |swing|), it is
  **unaffected** — to be confirmed before the regen decision.

## 6. Testing

- Producer: a property battery that the locked-world seasonal temperature's
  substellar direction equals `subsolarPoint`'s at the same day (temperature
  tracks illumination); the seed-8 solstice offset closes to ≈0; spinning
  worlds' seasonal peak now aligns with `solar_declination`'s extreme; the
  `terminator_acceptance` lab test extended for the moving substellar.
- Cross-repo goldens: a **producer-sourced** locked-world temperature golden
  (new seed) pinning the client's locked reconstruction, beside the existing
  spinning goldens.
- Client: `temperatureAt` phase includes `year_phase_offset` (unit test on a
  nonzero-offset fixture); the globe seasonal-mode freeze holds
  `spinGroup.rotation.z` while `day` advances (unit test); the fast rates
  are enabled at the globe (HUD test). Visual verification (The Lens rule):
  render seed 8 at 1 mo/s and confirm the ice tracks the terminator across
  the year — the seed-8 before/after this campaign exists to close.

## 7. Non-goals

- **Longitude libration** (the analemma figure-8 from orbital eccentricity)
  — latitude-only; a later refinement.
- **Moisture/insolation seasonality** — they stay annual-mean on the fixed
  equatorial substellar (§3.2). A day-dependent hydrology is its own body of
  work.
- **The flat climatology map pane** — the globe-freeze was chosen over it
  (brainstorm); captured as a follow-up (ORRERY-climatology-map).
- **A cryosphere** — ice stays a client derivation from temperature
  (decision 0022), unchanged.

## 8. Flagged for G3

1. **Save-format / determinism (leads):** §3.4 — the locked-world scene
   representation (recommended: client reconstructs the moving substellar,
   pinned by a producer-sourced golden; `scene/tiles/v1` gains an additive
   `locked` marker). No epoch; spinning worlds byte-identical. Confirming the
   representation choice is the main ask.
2. **Census regen (your carve-out):** §5 — whether any census metric samples
   `temperature_at(day)` and therefore drifts; if so, an AWS regen is needed.
   To be measured before deciding; nothing runs without your OK.
3. **The spinning sibling fold-in:** §3.1 — including the `year_phase_offset`
   phase fix for spinning worlds in this campaign (vs a separate one). It is a
   one-liner producer-side + a client one-liner, and it makes ice track the
   sun on both world types.

## 9. Decisions (promoted from the campaign ledger)

- One campaign, producer + client, A + B (B is the instrument that shows A).
- Latitude-only libration; moisture/insolation stay on the mean equatorial
  substellar (blast-radius containment).
- Globe-freeze over a flat map pane for B (Nathan's G-choice); the fast rates
  themselves engage the freeze (no separate toggle).
