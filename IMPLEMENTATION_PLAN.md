# The Terminator Wake (Slumber Tier-1 solar) — plan

The reserved `is_awake` body-swap: read the REAL sun (latitude × season × diurnal
phase) instead of the fractional-day window. Spec: the-slumber §1 (Tier-1) +
PSY-9. Substrate ready: `Calendar::solar_altitude_at(StdDays, latitude)` (None =
locked), `RoomAddr::coord().latitude`; calendar injected at vessel/lab via
`worldgen::sky_of(world).calendar()`.

## Stage 1: The solar Terrain hook + `is_awake` body-swap
**Goal:** `Terrain::solar_altitude(room, day) -> Option<f64>` (None = no day/night
cycle / locked); `LocaleTerrain` holds a calendar and implements it; planted +
synthetic terrains give a simple synthetic sun. `is_awake(activity, Option<f64>)`
reads altitude: diurnal = up, nocturnal = down, crepuscular = near-horizon;
None → always awake (fatigue-only). `next_awake_day` scans solar.
**Success:** a creature's wake state follows the real sun; locked → fatigue-only;
lib unit tests for solar is_awake per cycle + latitude/season.
**Status:** Not Started

## Stage 2: Wire + inject the calendar
**Goal:** `LocaleTerrain::new` takes the calendar (from `worldgen::sky_of`);
session + health construct it so; affect_of/tick read `terrain.solar_altitude`.
**Success:** the live rhythm follows the real sun; vessel + health green.
**Status:** Not Started

## Stage 3: Fallout, gallery rebaseline, close
**Goal:** behavior/harness re-pins; possession galleries regenerated (solar
sleep); null-control holds; gate; DoD (chronicle freshness, PSY-9 flip).
**Status:** Not Started
