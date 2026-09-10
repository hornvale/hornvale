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

Before Task 7, the observation exporter was separately tested with a valid
manifest whose source command was `scene neighbors`; it refused before
creating a packet:

```text
error: source_commands: export currently requires exactly
'cargo run -p hornvale -- underworld --seed 42'
```

Task 7 admitted exactly the corresponding `scene neighbors` command for
`HV-009`. Its adapter builds only to `BuildDepth::Astronomy`, serializes the
complete existing `scene/neighbors/v1` document into `spatial.readout`, and
binds `fnv1a64:35d7bb6804371e60` to those bytes. Two 900-packet exports had
identical files and the same complete-sequence SHA-256:
`8e45bc6d6fbd9a04607b387d72d82557fb46d404520f10a2b86b03e362ad1c6b`.
Atlas observation frames accept `observation/frame/v1`; when the supplied
source is `hornvale scene/neighbors/v1 stdout`, the preview plots the supplied
RA, declination, brightness, and magnitude fields at 390×844 and 1440×900.
The ordinary Atlas map separately accepts `scene/tiles/v1`
([`clients/atlas/src/scene.ts`](../../clients/atlas/src/scene.ts)).

## Matrix

| Candidate object | Scale / primary axis / unit | Live producer and observed packet or output | Renderer grammar and client path | Witness and state | Precise gap |
|---|---|---|---|---|---|
| Notable stellar neighborhood | astronomical neighborhood / apparent brightness and sky position / stars | `cargo run -p hornvale -- scene neighbors --world $TMP/world.json` emitted `scene/neighbors/v1`, seed 42, 5 `neighbors`; `star-chart --world … --out $TMP/star-chart.png` emitted the planisphere PNG and Markdown star list. `HV-009` admits the committed-fixture form of the neighbors command and preserves the complete scene JSON in its frame packet. | The CLI planisphere remains available. Atlas's observation preview plots the supplied 148 field stars and 5 notable neighbors by RA/declination; notable-star radius uses supplied relative brightness and field-star radius uses supplied magnitude class. | Repeated source JSON bytes matched; raster SHA-256 was `02982ce2…cde5`. `HV-009` frame-000 SHA-256 is `bfd65269d4503b9268ac2ef8d05c8f8fc7f6644612a29e5e1256b07bfa680ac7`; two complete 900-frame exports shared sequence SHA-256 `8e45bc6d6fbd9a04607b387d72d82557fb46d404520f10a2b86b03e362ad1c6b`. **existing** | This is the minimum frame-preview contract, not a general-purpose Atlas `scene/neighbors/v1` scene parser or interactive planisphere. Those remain follow-on work; the client derives no data beyond placing the supplied scene fields. |
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
