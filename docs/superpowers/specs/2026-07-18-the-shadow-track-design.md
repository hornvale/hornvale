# The Shadow Track — design

**Date:** 2026-07-18 · **Status:** awaiting G3 review
**Sequel to:** The Real Sky (the designated `scene/eclipses/v1` follow-on).

## 1. Goal

Eclipse Seasons made a world's eclipses dated events with real ground
tracks and a recurrence ladder; none of it has a client contract, so the
Orrery — which scrubs a year of sky time and draws the globe — cannot show
a single one. This campaign ships `scene/eclipses/v1` (producer + wasm +
golden + reference page) and an Orrery consumer that marks the year's real
eclipses on the day scrubber and sweeps each solar eclipse's shadow band
across the globe.

Success: on a mooned seed, the day scrubber carries a tick for every dated
eclipse in the shown year (solar/lunar, total/annular distinguished), and
scrubbing to a solar eclipse draws its ground-track band on the globe where
the sim says it was seen.

## 2. The producer contract — `scene/eclipses/v1`

A **parameterized** scene, like `scene/tiles-region/v1`: a windowed query
rather than a static snapshot, because eclipses are a temporal series.

```
EclipsesScene {
  schema: "scene/eclipses/v1",
  seed: u64,
  from_day: f64,          // the queried window, echoed back
  until_day: f64,
  events: [EclipseElem, …] // day-ascending (producer order)
}
EclipseElem {
  day: f64,               // absolute standard days (WorldTime)
  moon_index: usize,      // distance-sorted index into the system's moons
  body: String,           // "solar" | "lunar"
  kind: String,           // "total" | "annular"
  track: GroundTrackElem | null   // solar only; null for lunar
}
GroundTrackElem {
  center_lat_deg: f64,    // band center latitude at mid-event
  half_width_deg: f64,    // half-width of the full-omen band
  start_lon_deg: f64,     // sub-solar longitude at crossing start [-180,180)
  end_lon_deg: f64,       // sub-solar longitude at crossing end
  duration_days: f64      // crossing duration
}
```

- **Lunar events carry no track** (`null`): the producer's `ground_track`
  returns `None` for a lunar event — the anchor's shadow is one shadow over
  the whole night side, not a swept band. Lunar events are clock-marks-only.
- Pure read: `neighbors_scene`-style build over
  `hornvale_astronomy::eclipse_events(system, calendar, from, until)` and,
  per solar event, `ground_track(system, calendar, event)` — both pure
  computations over the already-derived star system and calendar. Consumes
  no `Stream` draws.
- Errors when the world has no generated sky (the tier-0 constant sun has no
  moons and no eclipses), mirroring `moons_scene`/`neighbors_scene`.

## 3. Producer work (hornvale)

- `windows/scene`: `ECLIPSES_SCHEMA`, `EclipseElem`, `GroundTrackElem`,
  `EclipsesScene`, `eclipses_scene(world, from: f64, until: f64)`,
  `eclipses_json` — mirroring `neighbors_scene`, reaching the system via
  `sky_of(world).system()` and the calendar via
  `hornvale_astronomy::calendar_of(system)`. Every f64 quantizes at emit
  (decision 0033); `body`/`kind` are identifier-text strings.
- CLI: `hornvale scene eclipses --world W --from D --until D` beside
  `scene tiles-region` (which is the parameterized-CLI precedent).
- wasm: `hw_scene_eclipses(from: f64, until: f64) -> i32`, mirroring
  `hw_scene_tiles_region`'s parameterized shape (f64 args cross the wasm ABI).
- Tests: a golden-determinism + schema pin over a mooned seed and a fixed
  window (seed 42 has two moons and dated eclipses); a range/consistency
  battery (events day-ascending and within `[from, until]`; solar events
  carry a track, lunar events null; `center_lat` in [-90,90], lon in
  [-180,180)); byte-identity across rebuild.
- Reference page: `book/src/reference/scene-eclipses-v1.md` (the
  scene-neighbors-v1 pattern), documenting the parameterized window, the
  event fields, the solar-only track, and the ground-track latitude
  approximation (cite the producer model card).
- Cross-repo golden: a **producer-sourced** seed-42 `scene/eclipses/v1`
  JSON at a fixed window, committed to the orrery testdata (The Isotherm
  rule).

## 4. The Orrery consumer (branch `the-shadow-track`)

- Parser `parseEclipses` in `src/sim/scene.ts` (strict, camelCase, range-
  validated — house style), catalog `sceneEclipses(fromDay, untilDay)`
  (parameterized, mirroring `sceneTilesRegion`), wired through the wasm
  loader; the client requests the window its scrubber shows (the displayed
  world year: `[0, season_period_days]` by default, re-queried if the clock
  advances the year).
- **Scrubber marks:** an overlay on the day scrubber (`src/ui/hud.ts`'s
  `hud-scrubber`) placing a tick per event at `day / scrubberMax`, shaped/
  colored by `body` (solar vs lunar) and `kind` (total vs annular). Clicking
  a mark opens an `InfoCard` inspector (date, body, kind, which moon). Pure
  geometry over the parsed events — unit-tested without the DOM where the
  position math lives.
- **Globe ground band:** when the clock's `day` is within a solar event's
  crossing window (`[day, day + duration_days]`, or a small display margin
  around `day`), draw its band on the globe — the latitude strip
  `center_lat ± half_width` spanning `start_lon → end_lon`, as a
  semi-transparent overlay on the globe mesh. **Static band in v1** (shown
  while the clock is at the eclipse); the animated W→E sweep is a non-goal
  (§7). Reuses the globe's existing lat/lon→mesh mapping.
- Re-pin: `wasm:release` moves to **world-wasm-v7** at G6 (release + re-pin
  are Nathan-gated carve-outs).

## 5. Determinism and safety

- **No epoch, no new draws, no ledger change:** every export is a pure read
  of already-derived astronomy (`eclipse_events`/`ground_track` are
  closed-form over the system + calendar). Worlds, almanacs, and censuses
  are byte-identical. The only new bytes are the new `scene/eclipses/v1`
  documents and their goldens.
- No stream labels added or reordered; quantization at emit only (0033).
- Additive: a brand-new schema; no existing scene document changes.

## 6. Testing summary

Producer: the golden + determinism + range battery above; the CLI covered
by the artifact drift check; architecture test covers the new export's
layering (windows/scene → domains/astronomy). Consumer: parser fixtures
(producer-sourced), scrubber-mark position math (unit), the globe-band
geometry (unit — the lat/lon strip for a known track), the
clock-within-event predicate. Visual verification (The Lens rule): render a
mooned seed, scrub to a solar eclipse, confirm the band lands where the
almanac's ground track says — a screenshot joins the G6 evidence.

## 7. Non-goals

- **The recurrence ladder** (saros / eclipse year / parade rate) — the
  producer computes it (`best_cycle`, `series_returns`,
  `parade_days_per_year`); surfacing it is captured as
  `ORRERY-eclipse-rhythm`, a later campaign.
- **The animated shadow sweep** (the band moving W→E as the clock advances
  through the eclipse) — v1 shows the static band; captured as
  `ORRERY-shadow-sweep`.
- **Lunar-eclipse shading on the moon/system view** (the moon visibly
  darkening) — captured as `ORRERY-lunar-shading`; v1 marks lunar events on
  the clock only.
- **The full 3-D umbral cone** (`ORRERY-shadow-play`) — this campaign is its
  lighter cousin (the ground band, not the shadow geometry).
- **Per-observer sight tiers** (`EclipseSight`) — the band already partitions
  the globe honestly; a per-latitude sight readout is deferred.

## 8. Flagged for G3

1. **Save-format (leads):** a new parameterized scene schema
   `scene/eclipses/v1` + `hw_scene_eclipses(from, until)`. No epoch, no
   draw-order change, no ledger/census impact — worlds byte-identical. The
   `scene/tiles-region/v1` parameterized precedent is the direct model.
   Confirming this reading is the main ask.
2. **world-wasm-v7 release + orrery re-pin + origin pushes** — G6 carve-outs;
   nothing external happens without your OK.
3. **The client's query window** — the client requests eclipses for the
   displayed world year (`[0, season_period_days]`), re-querying if the clock
   advances to another year. Alternative (a fixed multi-year span baked into
   one document) was rejected: it bloats the document and the scrubber only
   shows one year at a time.

## 9. Decisions (promoted from the campaign ledger)

- Parameterized query (from/until) over a fixed-span document — the
  tiles-region precedent; the scrubber shows one year at a time.
- Static globe band in v1; animated sweep deferred (the event is hours long
  against a year scrubber, so the static band is what a scrub reveals).
- Lunar events are clock-marks-only (the producer gives them no ground
  track by construction).
- Scope = clock marks + globe ground tracks (Nathan's G-choice); the
  recurrence ladder is deferred.
