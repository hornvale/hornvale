# The Scene Protocol, Cartographic Pole — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-09-rendering-strategy-design.md` (Ring 2; roadmap item 3; decision 0022 governs the seam). Registry: RENDER-1.
**Provenance:** The rendering strategy defined scene descriptions as the boundary artifact between the std-only workspace and external clients: deterministic, machine-readable, semantic-only JSON over the query surface, with schemas as save-format-class contracts. This spec designs the first scene kind — the cartographic tile lattice — and the window that emits it. The situated pole (observer at a place and time) is deliberately out of scope; nothing here may foreclose it.

---

## 1. Goal

A new window, `windows/scene` (crate `hornvale-scene`), emitting `scene/tiles/v1`: a tile-lattice JSON description of a world's surface — elevation, ocean, biome, plate, unrest per tile, settlements as point features — surfaced as `hornvale scene tiles`, with a committed, drift-checked example scene that doubles as the future in-book web viewer's (RENDER-2) test data.

## 2. The schema — `scene/tiles/v1`

One top-level JSON object, compact (single-line) serialization:

```json
{
  "schema": "scene/tiles/v1",
  "seed": 42,
  "width": 256,
  "height": 128,
  "sea_level_m": 123.4,
  "elevation_m": [ /* f64 × width·height */ ],
  "ocean":       [ /* bool × width·height */ ],
  "biome":       [ /* u16 legend index × width·height */ ],
  "biome_legend": [ "ice", "tundra", …, "abyssal" ],
  "plate":       [ /* u32 × width·height */ ],
  "unrest":      [ /* f64 × width·height */ ],
  "features": [
    { "name": "…", "kind": "settlement", "latitude": 12.3, "longitude": -45.6 },
    { "name": "…", "kind": "flagship",  "latitude": 7.8,  "longitude": 90.1 }
  ]
}
```

**Grid convention.** Row-major, top row first: latitude 90° → −90° down, longitude −180° → 180° across, sampled at pixel centers — byte-for-byte the raster renders' convention. `height` is always `width / 2` (equirectangular).

**Semantic-only.** Raw meters, catalog names, ids — no colors, no glyphs, no projection hints, and nothing that reveals which system produced a value. Clients own all presentation.

**The legend.** `biome_legend` is the **full biome catalog in declaration order** (all 22 `Biome` variants via a new `Biome::catalog()` accessor, names from the existing `Biome::name()`). Indexes are therefore stable across worlds and versions — clients may cache palettes keyed by index. Appending a biome variant later appends a legend entry and stays `v1`.

**Features.** Settlements from the ledger (the `places` + `LATITUDE`/`LONGITUDE`-fact pattern the settlement map command uses); a place missing either coordinate fact is skipped (the settlement-map precedent). The flagship **appears exactly once**, with `kind: "flagship"` — it is excluded from the plain `"settlement"` entries, not duplicated. Order: non-flagship places in `places` order, flagship last (deterministic).

**Contract class (save-format).** Same seed + same width → byte-identical JSON, forever. Determinism rests on: `serde` struct field order (fixes key order), `serde_json`'s ryu float text (shortest round-trip, platform-independent), and `Vec` layer order. The schema string is the epoch handle: **additive** changes (new fields, appended legend entries) stay `v1`; any change to the meaning, order, or type of an existing field mints `scene/tiles/v2` — emitted alongside, never replacing, exactly like seed-label epochs. A golden test pins the v1 bytes for a small fixed world.

## 3. The window — `windows/scene`

- Crate `hornvale-scene`, depending on `hornvale-worldgen` (worlds are built only through the composition root), `hornvale-kernel`, and the terrain/climate/settlement domain crates for types. The architecture test's window rule (kernel + domains + windows) already admits this.
- Public API:
  - `tiles_scene(world: &World, width: u32) -> Result<TilesScene, SceneError>` — builds the typed scene. Width must be even and in `16..=1024`; violations return `SceneError` with the reason (loud, like `GenesisError`).
  - `TilesScene` — public struct mirroring §2, `#[derive(Serialize)]`, every field documented.
  - `scene_json(scene: &TilesScene) -> String` — compact `serde_json` serialization.
- Sampling: for each pixel center, nearest geosphere cell via the kernel's `NearestCellIndex` (§4); per-cell values from the public provider surface (`elevation_at`, `is_ocean`, `plate_of`, `unrest_at`, `biome_map`). One index build per scene; terrain and climate share the same geosphere level so one index serves both lookups.
- Constant-sun worlds are fully supported — the cartographic pole never touches the sky. No `--day`/time parameter exists in v1; time arrives with the situated pole.

## 4. Kernel improvement — `NearestCellIndex`

The latitude-band nearest-cell index currently exists twice as a private struct (`domains/terrain/src/render.rs`, `domains/climate/src/render.rs`). The scene window is the third consumer, so the index is promoted to `kernel/src/geosphere.rs` as public `NearestCellIndex` (`new(&Geosphere)`, `nearest(&Geosphere, latitude, longitude) -> CellId`, 30 bands of 6°, ascending-cell-order construction — the existing implementation, moved not rewritten). Both domain copies are deleted and their render modules switch to the kernel type; the existing band-index-vs-brute-force equivalence test relocates to the kernel. No behavior change anywhere — a pure dedup, verified by the untouched committed raster artifacts.

## 5. CLI, artifact, CI

- `hornvale scene tiles [--world <PATH>] [--width <N>]` — JSON to stdout. Unknown scene kinds error loudly naming the known kinds (`tiles`); width violations report the bounds. Usage text gains the command.
- Committed example: `book/src/gallery/scene-tiles-seed-42.json`, default width 256, generated by the CI sky world (`/tmp/hv-ci-sky.json`). It rides the existing `git diff --exit-code book/src/gallery/` drift check via a new line in CI's "Artifacts are current" list. No SUMMARY entry — it is data, not a chapter; mdbook ships it as a static asset at a stable URL, which is precisely what RENDER-2's viewer will fetch.
- The book's reference section gains a short **`scene/tiles/v1` schema page** (hand-written, like the concept-registry chapter's prose): field-by-field meaning, grid convention, legend stability, and the additive-vs-epoch rule — the contract's public face.

## 6. Testing

- **Byte-determinism:** two `scene_json(tiles_scene(world, w))` calls → identical strings; and across two separately built worlds from the same seed.
- **Golden:** a small width (e.g. 16×8) scene for a fixed seed asserted against exact committed-in-test JSON — the schema regression tripwire; changing it knowingly is the epoch decision point.
- **Validation:** odd width, width < 16, width > 1024 → `SceneError` naming the bound; unknown kind at the CLI → loud error.
- **Features:** generated world's scene contains the flagship with `kind: "flagship"` and every place that has coordinates.
- **Kernel:** the relocated brute-force equivalence test for `NearestCellIndex`; terrain/climate render tests keep passing unmodified (dedup proof).
- **Docs/CI:** docs_consistency green; the full CI artifact list regenerates byte-identically.

## 7. Consequences for existing documents

- Registry: RENDER-1 flips `spec'd → shipped` on merge, Where → this spec.
- Chronicle 20 + `docs/retrospectives/campaign-20.md` (decisions 0013/0020).
- The rendering strategy's roadmap item 3: satisfied on merge; item 4 (the web viewer) becomes buildable, feeding on the committed scene JSON.

## 8. Non-goals

- **No situated pole** — no observer, no time axis, no phenomena serialization; only the contracts here must not foreclose it (they don't: a future `scene/sky/v1` or `scene/locale/v1` is a sibling kind, not a change to `tiles`).
- **No presentation** — no palettes, no suggested styling, no projection metadata beyond the grid convention.
- **No compression** — compact JSON only; if size ever matters, gzip at rest, never bespoke encodings in the schema.
- **No client** — RENDER-2 stays vision; this spec only sets its table.
- **No new tile layers beyond §2** — moisture, temperature, currents etc. are additive later; v1 ships what the providers expose publicly today.
