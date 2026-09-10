# Opening capability matrix — 2026-09

This is an execution audit, not a proposed simulation change. A row is
`existing` only when its live producer has an authoritative output and a
compatible, exercised visual grammar. `needs_observation_surface` means that
the simulation and a renderer already exist, but the observation exporter
cannot yet bind that source into `observation/frame/v1`. `needs_renderer`
means that the semantic source exists but no compatible Atlas grammar was
found. A domain type, an internal debug representation, or an unexercised
symbol is not a witness.

## Witness environment

All commands below used a freshly built seed-42 world at a temporary path:

```text
cargo run -p hornvale -- new --seed 42 --out $TMP/world.json
world of seed 42 written to $TMP/world.json (20135 facts; village: Doaba)
```

Each listed `scene` command was run twice against that same world and its two
stdout files compared with `cmp`: `neighbors`, `system`, `moons`, `tiles
--width 16`, and `tiles-region --face 0 --level 0 --ix 0 --iy 0 --samples 2`
were all byte-identical. The five direct raster commands (`star-chart`,
`map --field elevation`, `biome-map`, `paleo-map`, and `settlement-map`) also
repeated byte-identically on this Mac. That is a same-host witness only: the
CLI deliberately labels those raster bytes platform-local in
[`cli/src/main.rs`](../../cli/src/main.rs) (`PLATFORM_LOCAL_RENDER_NOTE`).

The current Atlas scene parser accepted the recorded `scene/tiles/v1` output:

```text
Atlas accepted scene/tiles/v1 16x8; fields=elevation,biome,plate,features
```

The current observation exporter was separately tested with a valid manifest
whose source command was `scene neighbors`; it refused before creating a
packet:

```text
error: source_commands: export currently requires exactly
'cargo run -p hornvale -- underworld --seed 42'
```

That boundary is implemented by `underworld_source` and `export_frames` in
[`cli/src/observations.rs`](../../cli/src/observations.rs). Atlas observation
frames accept only `observation/frame/v1` with textual `spatial.readout`
([`clients/atlas/src/observation.ts`](../../clients/atlas/src/observation.ts));
the ordinary Atlas map separately accepts `scene/tiles/v1`
([`clients/atlas/src/scene.ts`](../../clients/atlas/src/scene.ts)).

## Matrix

| Candidate object | Scale / primary axis / unit | Live producer and observed packet or output | Renderer grammar and client path | Witness and state | Precise gap |
|---|---|---|---|---|---|
| Notable stellar neighborhood | astronomical neighborhood / apparent brightness and sky position / stars | `cargo run -p hornvale -- scene neighbors --world $TMP/world.json` emitted `scene/neighbors/v1`, seed 42, 5 `neighbors`; `star-chart --world … --out $TMP/star-chart.png` emitted the planisphere PNG and Markdown star list. | Spatial star-chart raster exists in the CLI (`domains/astronomy/src/render.rs`, invoked by `cmd_star_chart`); Atlas has no `scene/neighbors/v1` parser or star-chart view. | Repeated JSON bytes matched; raster SHA-256 was `02982ce2…cde5`. **needs_observation_surface** | The phenomena and direct chart are real, but `observations export` accepts only the underworld command and cannot attach the neighbors packet/readout and provenance to an observation frame. A phone/laptop star renderer remains follow-on work after that packet boundary. |
| Primary system | system / orbital arrangement / bodies and orbital elements | `cargo run -p hornvale -- scene system --world $TMP/world.json` emitted `scene/system/v1`, seed 42, with moon/orbit data. | Required grammar is a spatial orrery. No Atlas parser/view imports `scene/system/v1`; `clients/atlas/src/scene.ts` rejects any schema other than tiles v1. | Repeated JSON bytes matched; SHA-256 `dd95bd26…5459`. **needs_renderer** | Authoritative semantic packet exists, but there is no compatible client visual grammar. The generic observation exporter additionally cannot yet select this command, but renderer absence is the first rendering boundary. |
| Moons and surface descriptors | planetary system / orbit, phase, and surface descriptors / moons | `cargo run -p hornvale -- scene moons --world $TMP/world.json` emitted `scene/moons/v1`, seed 42, 2 moons. | Required grammar is a moon strip/orbital detail view. Atlas contains no `scene/moons/v1` parser or view. | Repeated JSON bytes matched; SHA-256 `11d6cd46…1108`. **needs_renderer** | The astronomy producer is present; no compatible client grammar exists. Observation export also has the shared underworld-only source restriction. |
| Planetary surface | world / relief, ocean, biome, plate, and settlement arrangement / map tiles | `cargo run -p hornvale -- scene tiles --world $TMP/world.json --width 16` emitted `scene/tiles/v1`: 16×8, 128 elevation and biome entries, 307 features. | Spatial map. `clients/atlas/src/scene.ts` parsed the actual output; `clients/atlas/src/main.ts` draws elevation, biome, plate, and features. | Repeated JSON bytes matched; SHA-256 `5e8d0635…0fc7`; live Atlas parse succeeded. **existing** | No simulation or map-renderer gap. To make it an episode, add a non-underworld observation source adapter/provenance binding; that is packaging work, not a missing authoritative or visual capability. |
| Geographic regional tile | region / local relief, biome, water, and drainage / sampled tile nodes | `cargo run -p hornvale -- scene tiles-region --world $TMP/world.json --face 0 --level 0 --ix 0 --iy 0 --samples 2` emitted `scene/tiles-region/v1`: 3×3 = 9 nodes. | Required grammar is a zoomable regional tile. Atlas only parses `scene/tiles/v1`, not `scene/tiles-region/v1`. | Repeated JSON bytes matched; SHA-256 `b5a70c52…c0fc7`. **needs_renderer** | The bounded regional semantic packet is authoritative, but no compatible Atlas regional-tile parser/rendering path exists. |
| Terrain elevation field | field / elevation / meters | The `scene tiles` command above emitted `elevation_m`; `cargo run -p hornvale -- map --world $TMP/world.json --field elevation --out $TMP/map-elevation.png` emitted a rendered elevation map. | Spatial field map. Atlas parses and palettes `elevation_m` (`clients/atlas/src/scene.ts`, `palette.ts`); the direct PNG is a second renderer. | Scene bytes matched; direct raster SHA-256 `7045653c…bd4e`. **existing** | No simulation or field-renderer gap. A future observation episode needs the same non-underworld source adapter as the planetary-surface row. |
| Climate field | field / annual mean temperature or precipitation / °C or mm/yr | `scene tiles` emitted `t_mean_c` and `precip_mm_yr` arrays in the live `scene/tiles/v1` document; `biome-map` is a separate rendered classification witness, not a temperature/precipitation field renderer. | A continuous climate-field grammar is required. Atlas's `TilesScene` parser and palettes retain/render elevation, biome, and plate, but not `t_mean_c` or `precip_mm_yr`. | Authoritative values were present in the repeated tiles output. **needs_renderer** | This is not a climate-simulation gap: the scene packet already emits units. Add a client field selection/palette/legend that preserves the producer's units; then add the observation-source adapter. |
| Habitat biome classification | habitat / biome category and spatial distribution / biome tiles | `scene tiles` emitted `biome` indexes and `biome_legend`; `cargo run -p hornvale -- biome-map --world $TMP/world.json --out $TMP/biome-map.png` emitted the CLI biome map. | Spatial categorical map. The actual tiles output parsed in Atlas and its existing `biome` layer uses the producer legend. | Scene bytes matched; direct raster SHA-256 `651a468e…304c`. **existing** | No habitat simulation or categorical-map renderer gap. A candidate episode still needs a non-underworld observation adapter to create an `observation/frame/v1` witness. |

## Boundaries deliberately not promoted

- `paleo-map` is a realized deep-time raster (`2d726538…13ec` in the repeated
  same-host check), but it is outside this opening slice and no Atlas
  paleoclimate grammar was witnessed.
- `settlement-map` is a realized raster (`b24b595b…751d`), but settlement is
  later in the public scale spine and is not relabeled as habitat.
- Existing underworld manifests remain the internal production-path pilot.
  They are not evidence that an astronomical-to-habitat cell can be exported:
  the exporter names that command as its sole accepted source.

## Source and path checks

- CLI producer dispatch and stdout schemas: [`cli/src/main.rs`](../../cli/src/main.rs).
- Semantic scene contracts: [`windows/scene/src/lib.rs`](../../windows/scene/src/lib.rs)
  and [`windows/scene/src/region.rs`](../../windows/scene/src/region.rs).
- Terrain, climate, and astronomy render implementations: `domains/terrain/src/render.rs`,
  `domains/climate/src/render.rs`, and `domains/astronomy/src/render.rs`.
- The only observation packet writer names `frame-NNN.json` under the caller's
  `--out` directory in [`cli/src/observations.rs`](../../cli/src/observations.rs);
  no audited opening command silently writes a repository artifact.
