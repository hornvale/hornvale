# The Freshwater — Design

**Campaign:** The Freshwater (working name) — surface the inland water the sim
already computes so the client can draw it.
**Registry:** new MAP row (id computed at CLOSE, after absorbing main — the
Cartographer's hard-won lesson).
**Repos:** hornvale (`windows/scene` producer field + fixtures + a **wasm
release**) → Orrery (client rendering). **Autopilot** engaged (G3/G6 hard stops).

---

## 1. Goal

The sim knows its inland water per cell — `WaterKind = Ocean/SaltBasin/River/
DryLand` from flow-accumulation drainage, plus waterfall sites (`domains/terrain`
`water.rs`, exposed by the provider's `water_kind_at`/`drainage_at`/
`waterfalls()`). But the scene the client renders emits only `ocean`. This
campaign plumbs freshwater out to the scene and draws it: **the full river
network, lakes, and waterfalls on the flat map, and only the largest rivers +
biggest lakes on the clean globe overview** (Nathan's scope call).

## 2. Keystone & posture (from the ideonomy pass)

- **The client renders emitted water, never derives it.** Water is *derived* by
  the sim (authoritative), not invented client-side — a client that re-derived
  rivers from elevation would be non-authoritative and non-deterministic. The
  producer field is the whole point. (`autonomy` axis.)
- **Water is static per world** — a genesis-time terrain feature, not
  time-varying like weather. Emit once with the tiles/region, cached like biome/
  elevation; no animation. (`rate` axis.)
- **Connectivity is implicit in river-cell adjacency.** The scene emits *per-cell*
  `WaterKind`/`drainage`, not a flow graph; adjacent river cells read as a
  continuous line. Vector rivers from an emitted flow-graph is a deferred
  refinement (§9).

## 3. What is emitted (producer — `windows/scene`)

Append to **both** `scene/tiles/v1` (`TilesScene`, the globe) and the region
scene (`scene/tiles-region/v1`, what the map reads), **additively per the schema-
stability contract** (no epoch — these schemas already append fields):

- `water: Vec<u8>` — the tile/node's `WaterKind` as a stable index, plus
  `water_legend: Vec<String>` (the 4 stable names, self-describing — the same
  index+legend idiom as `biome`/`biome_legend`). Populated from
  `terrain.water_kind_at(cell)`. Integer → byte-exact, no quantization.
- `drainage: Vec<f64>` — flow-accumulation drainage per tile (0 on ocean/dry),
  from `terrain.drainage_at(cell)`. Gives river magnitude (a great river reads
  wider than a creek). Quantized at emit like every other scene float.
- `waterfalls: Vec<WaterfallPoint>` — the waterfall sites as lat/lon points
  (the `features` point-list idiom), from `terrain.waterfalls()`. On the region
  scene, only the waterfalls whose cell falls inside the region tile.

Serialize `WaterKind` by its stable lowercase-hyphenated name into
`water_legend` (reuse `windows/locale`'s `water_kind_name`/`serialize_water_kind`
convention — lift it to a shared spot if cleanest). Type-audit verdict tags on
every new field. Field ORDER is appended (contract).

## 4. Scope (Nathan) — map full, globe major

- **Map (full water):** every river cell, every salt-lake, every waterfall.
- **Globe (major water only):** high-drainage rivers (`drainage >=` a threshold)
  + large salt-basin lakes (connected-component size `>=` a threshold), so the
  overview shows the *massive* water features (the original vision's
  "longest rivers, biggest lakes") without cluttering the clean planet.

## 5. Client rendering (Orrery)

### 5.1 Scene TS types
Extend `TilesScene` and `RegionScene` (`src/sim/scene.ts`) with `water`,
`water_legend`/`waterLegend`, `drainage`, `waterfalls`. Parse/validate as the
existing fields are.

### 5.2 The map (full water) — pixel cells
The map base is a per-node `NearestFilter` pixel texture (`mapTexture.ts`
`regionPixelRGBA`). Fold water into that colour resolution: a **River** node →
a flowing-blue pixel (lighter/brighter than deep ocean, keyed to `drainage` for
a subtle width/intensity), a **SaltBasin** node → a distinct still-lake tone.
Ocean stays the depth-toned blue. This makes rivers/lakes first-class pixels of
the map, adjacency giving continuity. Waterfalls → icon sprites via the existing
`mapSymbols` icon tier (a new waterfall glyph in `sprites.ts`).

### 5.3 The globe (major water) — thresholded overlay
On the globe's cube-sphere surface, tint the *major*-water cells into the base
vertex colour (river cells above the drainage threshold → a thin blue; large
salt-lakes → a lake tint), or a lightweight line/patch overlay if tinting reads
too chunky (visual-pass decision). Thresholds keep it sparse; the clean overview
survives.

## 6. Determinism & schema

- **Additive append, no epoch.** Both schemas already carry the stability
  contract; new fields are appended, existing consumers unaffected.
- **Byte-identity:** `water`/`water_legend` are integers/strings (exact);
  `drainage`/waterfall lat-lon are floats, quantized at the emit boundary by the
  existing `quantize` path. Same seed+pins → byte-identical scenes.
- **Fixtures & goldens:** regen the committed scene fixtures / almanac / any
  drift-checked scene artifacts (`make rebaseline` + the CI artifact list).
- **Stream/consumption order:** water is read from the already-built terrain
  provider — no new seed draw, no stream-order change (a pure read of the
  existing globe). Confirm the provider is threaded, not re-sculpted (the
  Single-Sculpt idiom).

## 7. The wasm release (carve-out)

The Orrery gets water only via a **world-wasm release** built from the new
`windows/scene` — externally visible, Nathan authorizes the release + the orrery
re-pin. It follows the established release path (the same one The Real Sky et al.
used). Staged last on the producer side, first on the client side.

## 8. Staging

1. **Producer field.** Append `water`/`water_legend`/`drainage`/`waterfalls` to
   `TilesScene` + `RegionScene`; populate from the provider; type-audit tags;
   unit tests (a known seed's river/lake/waterfall counts are non-trivial and
   deterministic); regen fixtures; scene-schema reference pages. Deliverable: the
   scene carries water, drift-checked, gate green. **No client yet.**
2. **Wasm release.** Build + release the world-wasm; re-pin the orrery to it
   (carve-out — Nathan authorizes). Deliverable: the orrery's fetched scenes
   carry water.
3. **Map full-water render.** TS scene types + `mapTexture` water colouring +
   waterfall icons. Controller visual pass on the flat map.
4. **Globe major-water overlay.** Thresholded river/lake tint on the sphere.
   Controller visual pass; confirm the overview stays clean.

## 9. Non-goals / deferred

- **Vector rivers from an emitted flow-graph** — smooth polyline rivers need the
  downstream-connectivity graph emitted; v1 draws cells (adjacency-continuous).
- **Flowing-water animation** — water is static; a client cosmetic later.
- **Lake bathymetry / river depth** — only the classification + drainage
  magnitude, not a depth field.
- **Naming rivers/lakes** — the sim doesn't name them yet; a language-domain
  followup.
- **Re-sculpting for water** — none; a pure read of the existing provider.

## 10. Risks

1. **Scene-fixture drift is broad** — a new scene field regenerates every
   committed scene artifact; the drift-check will flag them all. Regen in the
   producer commit, re-gate fully (the Residue lesson: a latent leak goes live
   when an artifact regenerates).
2. **stream_labels()/manifest** — if any new seed-derivation label is added
   (none expected — pure read), it must reach the generated stream manifest
   (the Few-and-Many recurring gap). Bake a manifest-regen step into the plan.
3. **Globe clutter** — the major-water thresholds are a visual-pass tuning; too
   low re-clutters the overview we just fought for.
4. **Wasm release sequencing** — the client stages (3,4) block on the release
   (2); don't build client rendering against un-released scene fields.

## 11. DoD / registry

- New MAP registry row (id at CLOSE), shipped, pointing at the chronicle + this
  spec.
- Chronicle + retrospective; scene-schema reference page freshness; the terrain
  water chapter freshness sweep (does a chapter describe the scene's water?).
- Confidence Gradient: check the scene-protocol / rendering bets.
- Followups: vector rivers, animation, river/lake naming.
