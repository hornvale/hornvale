# Campaign 19: Firm Ground II — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-09-year-3-metaplan-design.md` (§4 binds this campaign; Constitution §2 governs)
**Provenance:** The opening campaign of Year 3 (the world has a past), and its
Campaign 0 in the metaplan's sense — the substrate the deep-time spine reads,
delivered and re-baselined once before anything is built on it. The sequence
number 19 follows Campaign 18 (The Meeting); if the parallel rendering thread
claims 19 first, this renumbers at chronicle time — the chronicle is the
authoritative order (decision 0017). Downstream: Campaign 20 (Deep Time,
`domains/paleoclimate`) consumes the time-varying orbital elements this ships;
the Year-4 epistemic layer consumes the placed observer.

---

## 1. Goal

The world's sky is drawn once at genesis and frozen for all `WorldTime`: the
anchor carries a single fixed obliquity, orbits are perfectly circular, and the
almanac is observed from nowhere in particular — position-blind, at
`places[0]`, on genesis day 0. Paleoclimate (Campaign 20) has almost nothing to
read, and the epistemic layer has no vantage point to be honest and wrong from.

Firm Ground II retires both deficits at once, because both change almanac
output and a single re-baseline should absorb the churn:

1. **The sky acquires a past** — the Milankovitch triad (obliquity,
   eccentricity, precession) becomes a function of `WorldTime`, oscillating
   slowly over deep time, with obliquity's wobble coupled to moon presence.
2. **The observer acquires a place** — `ObserverContext` gains a real position
   on the globe; daylight varies with latitude; the visible sky is culled to
   the observer's day/night hemisphere.

It builds no consumer of either — paleoclimate reads the forcing in Campaign
20, the epistemic almanac reads the vantage in Year 4. This campaign lays the
two seams and proves them, and ships the forcing **pin** the Year-3 exit
criterion varies.

## 2. Design principles

1. **Determinism is the sharp constraint, and the migration guard is precise.**
   Every new drawn quantity is a permanent labeled stream (ADR 0006). Because
   labeled streams derive independently from the seed (`seed.derive(label)`),
   adding new forcing labels **does not perturb any existing draw** — star
   mass, orbit, obliquity, moons all consume exactly what they consume today.
   The *drift* terms are anchored so that every pre-existing element evaluated
   at `t = 0` equals its genesis draw (obliquity's wobble is invisible at the
   present, visible only in deep time). What genuinely changes the present — a
   new eccentricity, latitude-dependent daylight, a placed observer — is what
   the single re-baseline exists to absorb.
2. **Coarse constrains fine, and the scope lines are bright.** The forcing
   *drifts* the elements the domain already holds; it adds no new bodies. The
   observer sees a *hemisphere*, not an alt-azimuth sky. The deferred list (§11)
   is long on purpose — this is firm ground, not a spherical-astronomy engine.
3. **The pin expresses the null control.** The Year-3 experiment varies orbital
   forcing and controls it with a zero-forcing world; the forcing pin therefore
   must reach zero (circular orbit, no obliquity drift) as a legal setting.
4. **Kernel seam, cause-blind consumers.** The observer's position extends
   `ObserverContext` (a kernel type), not `Phenomenon` — position is the
   observer's, not the body's. No producing domain is edited to read it;
   consumers stay blind to why a body is or isn't visible.

## 3. Half A — The moving sky (SKY-1/2/4/21)

**The forcing model.** Each element of the Milankovitch triad becomes a slow
oscillation over `WorldTime` (`day: f64`, absolute standard days):

- **Obliquity** — `ε(t) = ε₀ + A_ε·(sin(2π·t/P_ε + φ_ε) − sin(φ_ε))`, so
  `ε(0) = ε₀` exactly (the anchor point), where `ε₀` is today's drawn genesis
  obliquity. `P_ε` fixed near the real ~41 kyr (expressed in std days); `A_ε`
  and `φ_ε` drawn on labeled streams. **Moon coupling (SKY-21):** `A_ε` is
  bounded small when a large stabilizing moon is present and large when the
  world is moonless — the largest moon's stabilizing measure (from the
  `tide_rel`/mass the domain already computes) gates the amplitude, turning one
  moon draw into two coupled consequences.
- **Eccentricity** — `e(t) = e₀ + A_e·sin(2π·t/P_e + φ_e)`, genuinely new
  (orbits are circular today). `e₀` drawn small (Earth-like scale), `P_e` near
  ~100 kyr, `A_e`/`φ_e` drawn. Eccentricity is a **tilt-independent seasonal
  driver** (SKY-2): a zero-obliquity world now has apsidal seasons and therefore
  a year.
- **Precession** — `p(t) = φ_p + 2π·t/P_p`, `P_p` near ~21 kyr, `φ_p` drawn.
  Precession sets the phase alignment between axial tilt and perihelion,
  modulating the seasonal-contrast strength — the coupling that makes the
  glacial record vary rather than merely cycle.

**Per-body phase offsets (SKY-4).** Genesis day 0 is currently an unintended
grand alignment (every moon new, year and day beginning together), because
`moon_phase`/`year_phase`/`local_day` are all `(t/period).fract()`. Each body
gets a drawn phase offset so day 0 is an ordinary day.

**Where it lives.** `Anchor`'s scalar `obliquity: Degrees` becomes an
`OrbitalForcing` description (mean + amplitude + phase per element, plus the
moon-coupling input) exposing `obliquity_at(t)`, `eccentricity_at(t)`,
`precession_at(t)` — pure functions of the system and `t`. `Calendar`
(`calendar.rs`) consumes them: `daylight_fraction` reads `obliquity_at(t)` and
the apsidal term from `eccentricity_at(t)`; `season_phase` reflects both
drivers; the phase offsets thread into `moon_phase`/`year_phase`/`local_day`.

**Facts (SKY-15 partial).** The forcing parameters (means, amplitudes, phases,
and the moon-coupling verdict) are committed as facts so `why` recounts them —
value-kind enforced per ADR 0010.

## 4. Half B — The placed observer (SEQ-4/5, SKY-8)

**The kernel seam.** `ObserverContext` gains a geographic position — reusing
the kernel's existing `GeoCoord { latitude, longitude }` (already returned by
`Geosphere::coord`), not a new type. The struct's own contract (`adding a field
here must not break existing sources`) is honored: the field is additive, and
the existing `ObserverContext::at(place, time)` gains a position-taking sibling
(or a documented default) so tier-0 sources and their tests keep compiling
unchanged.

**Resolution and the default vantage.** `windows/worldgen` resolves the
observer's `place` entity to its cell on the shared `Geosphere` and reads the
cell's `coord` (a `GeoCoord`, latitude/longitude). The **default almanac observes from the
flagship cell** — so the almanac finally reports the sky of a real place. This
introduces a deliberate new dataflow, stated plainly: the almanac's sky now
depends on where the flagship landed (its latitude and longitude). It stays
deterministic because placement is deterministic.

**Latitude daylight (SKY-8).** `daylight_fraction` becomes a function of
`(t, latitude)`. With solar declination `δ(t) = ε(t)·sin(2π·season_phase(t))`
and latitude `φ`, the daylight fraction follows the standard sunrise
equation `cos H₀ = −tan φ · tan δ` (clamped to the polar day/night limits):
12h-flat at the equator, running to 24h/0h at the poles. The old planet-wide
single number is gone.

**Coarse horizon / day-night (the SEQ-5 defect).** Day versus night at the
observer is set by the observer's **longitude and the rotation phase**, not the
global `is_daylight` clock. The night sky (moons, neighbor stars) is delivered
only when it is night *at the observer*; the sun only when it is day there. On a
tidally locked world the observer's longitude relative to the substellar point
fixes perpetual day or night, so the fixed sun no longer reaches the night side
and the night-side observer sees the stars the day side cannot — the concrete
bug SEQ-5 names, fixed. This is **hemisphere-coarse** visibility: a body is up
or down with the observer's day/night, not placed at an altitude and azimuth.

## 5. The forcing pin and the exit-criterion dependency

The Year-3 exit criterion (metaplan §2) is *two worlds differing only in a
deep-time pin (orbital forcing)*, with a null control of *zero orbital forcing*.
So this campaign ships the pin that experiment varies:

- **A forcing pin** setting the oscillation amplitudes (and eccentricity mean),
  with **zero as a legal setting** — `A_ε = A_e = 0`, `e₀ = 0` — recovering the
  circular-orbit, fixed-obliquity behavior for the null control. A non-zero
  setting gives a world a deep-time climate history to diverge on.
- **Pin isolation (save-format contract).** The pinned path consumes the same
  draws as the unpinned path, asserted by test in the pattern of
  `normal_pin_matches_the_unpinned_draw_for_spinning_worlds`
  (`anchor.rs`) — a pin must never shift stream consumption.

The exact pin surface (one `--forcing <profile>` knob versus separate
`--eccentricity`/`--obliquity-drift` knobs) is an implementation choice for the
plan; the binding requirement is that zero forcing is expressible and
pin-isolated.

## 6. Composition root and dataflow

`windows/worldgen` is the only place the seams meet: it resolves the flagship
cell to a `GeoPosition`, builds the `ObserverContext` the almanac observes
through, and — unchanged in spirit — reconstructs the sky provider from the
ledger (`sky_of`). No domain-to-domain edge is added; astronomy still imports
nothing, climate still reads scalar stellar inputs. The one new fact of life is
that the observation context now carries a place, so the almanac window renders
the sky of the flagship rather than of an omniscient nowhere.

## 7. Save format, determinism, and the re-baseline

- **New stream labels**, declared as constants in `streams.rs` and published
  via `stream_labels()` into the generated manifest: the forcing means,
  amplitudes, and phases (`astronomy/forcing/…`) and the per-body phase offsets
  (`astronomy/phase/…`). Never renamed; deliberate regeneration would use an
  epoch suffix.
- **Existing draws are untouched** — new labels derive independent sub-streams,
  so star mass, orbit, obliquity `ε₀`, moons, and neighbors consume exactly
  today's draws. This is what makes the drift-anchored `t = 0` identity hold by
  construction for the pre-existing elements.
- **Re-baseline exactly once.** All four censuses re-run, the drift study
  regenerates, the calibration rows re-assert, and every committed artifact
  (the three seed-42 almanacs, the elevation map, the reference dumps, the lab
  studies) refreshes once. Every later Year-3 study inherits these baselines.

## 8. The Lab and testing

- **Property battery** (`genesis_properties.rs` extended): exact formula checks
  on `obliquity_at(t)`/`eccentricity_at(t)`/`precession_at(t)`; the drift-anchor
  identity (`ε(0) == ε₀` byte-for-byte); latitude-daylight endpoints (equatorial
  flat at ~0.5, polar reaching 0/1 across the year); and observer culling (a
  locked world's sun is absent from the night-side vantage).
- **A new calibration for the family:** the obliquity oscillation amplitude is
  bounded iff a large stabilizing moon is present — a moonless world's obliquity
  range over deep time is strictly wider than a mooned world's, asserted
  row-by-row over the drift study (the sixth calibration, after the five
  Year-1/Year-2 rows).
- **Pin isolation** for the forcing pin, as in §5.

## 9. The book (opens the campaign)

Per book-driven development, the campaign opens by writing the astronomy domain
chapter to **tier 2**: the moving sky (the Milankovitch triad as functions of
`WorldTime`, moon-coupled obliquity, apsidal seasons) and the placed observer
(latitude daylight, the hemisphere-culled sky). The astronomy model card is
extended with the new rows — each element's mean/amplitude/phase declared drawn,
the drift laws declared approximated, the moon coupling declared derived, the
observer position declared derived-from-placement. The chapter states the bright
scope lines so a reader knows the sky drifts but gains no new bodies, and the
observer sees a hemisphere but not an altitude.

## 10. Success criteria

- `obliquity_at(t)`, `eccentricity_at(t)`, `precession_at(t)` evaluable and
  exactly formula-checked; the drift-anchor identity holds byte-for-byte at
  `t = 0` for the pre-existing elements.
- A zero-obliquity world has a year (apsidal seasons from eccentricity).
- The moon-coupling calibration passes over the drift study.
- Genesis day 0 is an ordinary day (no grand alignment).
- `ObserverContext` carries a `GeoCoord`; the default almanac observes from
  the flagship; daylight varies with latitude; a locked world's night-side
  vantage does not see the sun.
- The forcing pin reaches zero (the null control) and is pin-isolated.
- The re-baseline is clean: a second full regeneration leaves the committed
  artifacts byte-identical.
- The full gate passes: `cargo test --workspace && cargo fmt --check &&
  cargo clippy --workspace --all-targets -- -D warnings`, plus the artifact
  drift check.
- The astronomy chapter reads at tier 2 with the extended model card.

## 11. Explicitly deferred

Decisions, not omissions: per-body altitude/azimuth and twilight (SKY-7);
per-neighbor celestial coordinates and constellations (SKY-12); wandering
planets, comets, and meteors (SKY-9/10/18); the synodic-vs-sidereal moon-phase
fix (SKY-20); stellar variability and the M-dwarf/A-star range (SKY-13); binary
hosts (SKY-16); sky colour from spectral class (SKY-17); retrograde rotation
(SKY-22); the tier-cross paired comparison that `GeneratedSky` refines
`ConstantSun` (SKY-23, TOOL-5); and — the whole point of the year — the
paleoclimate *consumption* of this forcing (Campaign 20) and the epistemic
almanac's consumption of the vantage (Year 4). The forcing is produced here; it
is read elsewhere.
