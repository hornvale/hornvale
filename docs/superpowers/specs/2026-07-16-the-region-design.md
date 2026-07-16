# The Region — Design

> **Status: SHIPPED** (2026-07-16). `scene/tiles-region/v1` producer, CLI,
> `world-wasm-v3` export, reference page + golden, and the orrery consumer
> (parser, extended evaluator, single-patch registration/seam proof) all
> merged. Chronicle: [`the-region`](../../../book/src/chronicle/the-region.md);
> retrospective: [`the-region`](../../retrospectives/the-region.md). The full
> CDLOD renderer (orrery#2 proper) is a deferred follow-on. Census/calibration
> regen deferred by Nathan at close.

The map learned to answer *when* (The Isotherm). Now it must answer *closer*.

`scene/tiles/v1` resamples a whole world onto one global equirectangular
lattice. A client that zooms in eventually exhausts it — at the maximum width
(1024×512 ≈ 39 km/tile) the lattice already samples the substrate finer than
the substrate's own resolution, so there is nothing more to ask for globally.
The orrery's `cubeSphere.ts` already carries a full cube-sphere quadtree
addressing scheme (`face/level/ix/iy`, `maxLevel` ~1.5 m spacing) built to let
the camera descend past that point — but it has no producer to feed it. This
campaign builds that producer: a **regional** tile query that returns the same
per-tile layers for one quadtree tile's footprint, at higher on-tile sample
density, as a new cross-repo contract alongside `scene/tiles/v1`.

The honest heart of it: the world's substrate is a per-cell `CellMap` over a
geosphere at the canonical globe level (level 6 — 40,962 cells, ~110 km). That
is the physical resolution floor. A regional query cannot invent sub-110 km
*physics*. What it can do is (a) address the sphere as a streamable quadtree
instead of one monolithic global array, and (b) **interpolate** the continuous
layers between cell samples so a zoomed-in mesh reads as a smooth surface
rather than 110 km facets. The contract states both plainly: interpolation is
cosmetic smoothing, not new fidelity, and the cell spacing is the floor.

## 1. Goal

Ship `scene/tiles-region/v1`: a producer query, a `world-wasm-v3` export, a
normatively documented cross-repo contract, and the producer-sourced contract
tests that pin it — with a minimal orrery consumer proving the data path
end-to-end. Unblock orrery#2's LOD terrain renderer.

## 2. Roadmap context (the ticket decomposition, non-binding beyond campaign 1)

hv#3 (the regional producer + contract) and orrery#2 (the CDLOD terrain
renderer) are the two tickets. The Isotherm precedent — a producer-heavy
campaign plus one *focused* consumer proof, with the broad client feature
deferred — applies here:

- **Campaign 1 (this spec):** the `scene/tiles-region/v1` contract, the
  hornvale producer (`windows/scene`), the `hw_scene_tiles_region` export +
  `world-wasm-v3`, the producer/consumer contract tests, and an orrery
  consumer that *proves* the contract: strict typed parsing of a real regional
  document, evaluator reuse, and a single-patch render smoke that confirms a
  regional tile mesh seams with the globe. This fully delivers hv#3 and
  unblocks orrery#2.
- **orrery#2 proper (its own follow-on client campaign):** camera-distance
  select/split/merge, per-tile geometry build/cache/dispose, skirts, and
  markers/picking through the LOD tree. Substantial three.js work; deferred so
  campaign 1 stays a clean cross-repo contract, exactly as The Isotherm kept
  its consumer to the ice overlay.

This decomposition is a scope call flagged for G3 (§10); Nathan can fold the
full renderer into campaign 1 if he wants it in one arc.

## 3. The contract — `scene/tiles-region/v1` (a new schema)

A regional tile is a cube-sphere quadtree sub-quad, not the global
equirectangular lattice `scene/tiles/v1` describes. Its grid formula, its
addressing, and its per-node (not per-global-tile) layers are a different
document *shape*, so it is a **new schema alongside** v1 — never a v1 field
addition and never a re-meaning of v1 (decision 0055; scene-tiles-v1
Stability §). It **reuses** v1's per-layer semantics table by reference rather
than redefining the layers.

### 3.1 The address and the node grid (normative projection)

An address is `(face, level, ix, iy, samples)`:

- `face` ∈ `0..6` — one of six cube faces.
- `level` ∈ `0..=24` — quadtree depth; the face is `2^level × 2^level` tiles.
- `ix`, `iy` ∈ `0..2^level` — the tile's column/row on the face.
- `samples` = N ∈ `1..=256` — quads per tile edge, yielding an **(N+1)×(N+1)**
  node grid. Matches the orrery's `TILE_QUADS`.

The node grid is row-major, **row (iy-direction / `b`) outer, column
(ix-direction / `a`) inner**: node index `i = row·(N+1) + col`,
`row, col ∈ 0..=N`. This is exactly the orrery's `tileGrid` order.

The **normative projection** (identical arithmetic in the Rust producer, this
page, and the orrery's `cubeSphere.ts`; the "three homes" discipline from The
Isotherm's evaluator):

```
FACES[6] = { n, u, v } basis triples:
  0: n=( 1, 0,0) u=(0, 1,0) v=(0,0,1)
  1: n=(-1, 0,0) u=(0,-1,0) v=(0,0,1)
  2: n=( 0, 1,0) u=(-1,0,0) v=(0,0,1)
  3: n=( 0,-1,0) u=( 1,0,0) v=(0,0,1)
  4: n=( 0, 0,1) u=( 1,0,0) v=(0,1,0)
  5: n=( 0,0,-1) u=(-1,0,0) v=(0,1,0)

param(index, offset, level) = -1 + 2·(index + offset) / 2^level
a = param(ix, col/N, level)          # ∈ [-1, 1]
b = param(iy, row/N, level)
faceUnit(face, a, b) = normalize( n + a·u + b·v )   # the node's unit vector s
```

The node's geographic coordinate is derived (`latitude = asin(sᵤ), longitude
= atan2(s_y, s_x)`, degrees) — this is the orrery's `unitLatLon` and matches
hornvale's `geosphere::coord`. **Sampling uses the unit vector `s` directly**
(`NearestCellIndex::nearest_to_position`), not the lat/lon round-trip, so the
producer's cross-platform byte-identity holds through the libm-routed
transcendentals (decision 0041) with no redundant `asin`/`atan2`.

The document does **not** carry per-node lat/lon or positions: the client
reconstructs node geometry from the address via this same projection. (Additive
room — a `positions` array could append later; ledger #4's rationale.)

### 3.2 Sampling: nearest-cell discrete + barycentric continuous (the fidelity posture)

For each node's unit vector `s`, the producer finds the nearest geosphere cell
and its local neighborhood, then:

| Layer | Sampling | Why |
|---|---|---|
| `ocean`, `biome`, `plate` | **nearest-cell** | categorical — you cannot interpolate a category |
| `elevation_m`, `unrest`, `t_mean_c`, `t_swing_c`, `moisture` | **barycentric** over the local cell triangle | continuous — smooth C0 surface across the tile |

Barycentric interpolation is **producer-internal**: the producer ships the
already-interpolated arrays; the client never reimplements it (contrast the
temperature *evaluator* in §3.4, which the client does reconstruct). Weights
come from cell-center unit positions (cached, full-precision, libm-routed) and
the sample position — arithmetic, deterministic, quantized at the emit boundary
(decision 0033). The recommended algorithm (nearest cell + its geosphere
neighbors → the containing spherical triangle → planar barycentric in the
tangent plane) is a producer detail in §4; the **result** is the contract,
pinned byte-for-byte by a golden. Same address + same world → byte-identical
document.

### 3.3 The per-node and document-level layers (reusing scene-tiles-v1 semantics)

The document is one JSON object, field order fixed (contract):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always `"scene/tiles-region/v1"`. |
| `seed` | integer | The world's seed (u64; BigInt-aware parsing above 2^53). |
| `face`,`level`,`ix`,`iy` | integer | The tile address (§3.1). |
| `samples` | integer | N; the node grid is `(N+1)²`, row-major (§3.1). |
| `sea_level_m` | number | Sea level, meters — document-level, same scale as `elevation_m`. |
| `season_period_days` | number | The seasonal sinusoid's period (§3.4); document-level. Self-contained exactly as in v1. |
| `circulation_bands` | integer, **absent when tidally locked** | Circulation cells per hemisphere; document-level (same convention/absence rule as v1). |
| `biome_legend` | array of string | The biome catalog, in v1's stable append-only order — `biome` indexes into it. |
| `elevation_m` | array of number, `(N+1)²` | **barycentric** (§3.2). |
| `ocean` | array of boolean, `(N+1)²` | **nearest-cell** — the categorical coastline truth. |
| `biome` | array of integer, `(N+1)²` | **nearest-cell**, index into `biome_legend`. |
| `plate` | array of integer, `(N+1)²` | **nearest-cell**. |
| `unrest` | array of number, `(N+1)²` | **barycentric**, [0,1]. |
| `t_mean_c` | array of number, `(N+1)²` | **barycentric** annual-mean °C. |
| `t_swing_c` | array of number, `(N+1)²` | **barycentric** hemisphere-signed seasonal half-swing °C. |
| `moisture` | array of number, `(N+1)²` | **barycentric** moisture index, [0,1]. |

Every per-layer *meaning* (units, sign conventions, the biome legend, moisture
being a dimensionless index not mm/yr) is **the v1 table** — the reference page
(§5) links to `scene-tiles-v1.md` rather than restating it. `features` are
omitted from this schema (ledger #4).

### 3.4 Temperature over the year (normative; interpolation commutes with the evaluator)

The client reconstructs a node's temperature at any day with the **same
evaluator as v1**, over the regional arrays — the ticket's "extend, don't
fork":

```
t(node i, day) = t_mean_c[i] + t_swing_c[i] · sin(τ · frac(day / season_period_days))
```

This is exact under interpolation because the seasonal period is **global**:
`temperature_at(cell, day) = mean(cell) + swing(cell)·sin(θ)` with the same
scalar `θ = τ·frac(day/period)` at every cell, so barycentric-interpolating the
*actual temperature* over a triangle equals evaluating the evaluator on the
*interpolated* mean and swing. **Interpolation and the seasonal evaluator
commute.** This property is what makes the producer's ground-truth
(`temperature_grid_region`, §7) equal the reconstruction, and it is stated
normatively so no consumer thinks the interpolated layers need a different
evaluator. The phase gotcha carries over verbatim: it is `frac(day/period)`,
**never** offset by `scene/system/v1`'s `year_phase_offset`.

Ice remains a client derivation from `t_mean_c`/`t_swing_c` (v1's guidance),
now smooth across the tile because the two layers are interpolated.

### 3.5 Honesty caveats (stated in the schema page, not buried)

- **The resolution floor is ~110 km.** Below the geosphere cell spacing the
  query returns no new physical detail — interpolation only smooths between the
  same cell samples. A tile deeper than the cell spacing is honest but carries
  no information the coarser tile lacked. The page says this outright.
- **Interpolated elevation can disagree with the nearest-cell coastline.**
  `ocean` is the categorical truth; `elevation_m` is smoothed relief that may
  cross `sea_level_m` independently near a coast. A client wanting a crisp
  shoreline reads `ocean`; one wanting smooth bathymetry reads `elevation_m`.
  Documented, not silently reconciled.

## 4. Producer implementation (hornvale)

- **`kernel` / `domains`**: no physics change, no new streams, no draw-order
  change. Sampling reuses `NearestCellIndex::nearest_to_position` and
  `Geosphere::position`/neighbors already public. If a barycentric helper over
  a cell triangle is worth sharing, it lands in the kernel beside
  `NearestCellIndex` (pure geometry, deterministic); otherwise it stays private
  to `windows/scene`.
- **`windows/scene`**: a new `RegionScene` struct + `tiles_region_scene(world,
  face, level, ix, iy, samples) -> Result<RegionScene, SceneError>`, mirroring
  `tiles_scene`'s shape (validation → build layers → quantized serde). Discrete
  layers via `nearest_to_position`; continuous via the barycentric helper.
  `SceneError` grows the address-validation variants (bad face, level out of
  range, ix/iy out of range for the level, samples out of range) with loud
  messages in the existing manner. Extend `temperature_grid` →
  `temperature_grid_region(world, face, level, ix, iy, samples, day)` returning
  full-precision ground-truth per-node temperatures for the contract test
  (§7) — the interpolated `temperature_at` per node.
- **`clients/world-wasm`**: new export `hw_scene_tiles_region(face: u32, level:
  u32, ix: u32, iy: u32, samples: u32) -> i32`, mirroring `hw_scene_tiles`
  (0 ok; 2 scene error with envelope; -3 no world). A **`world-wasm-v3`**
  release is cut at close (Nathan authorizes the tag push; decision 0055).
- **CLI**: `hornvale scene tiles-region --face --level --ix --iy --samples
  [--world]`, mirroring `hornvale scene tiles`; prints one document to stdout.
  Needed to regenerate the committed golden example.

## 5. Documentation (the contract's public face)

- **`book/src/reference/scene-tiles-region-v1.md` — new page**: the address +
  node grid (§3.1, with the normative projection), the sampling table (§3.2),
  the field table (§3.3) linking `scene-tiles-v1.md` for per-layer meaning, the
  temperature evaluator + commutation property (§3.4), the honesty caveats
  (§3.5), and a Stability section holding it to the same save-format discipline
  as v1. A committed seed-42 example at a fixed address, byte-checked by CI.
- **`book/src/reference/scene-tiles-v1.md`**: a one-line pointer to the
  regional page under a "Going closer" note; no v1 content changes.
- Book `SUMMARY.md` entry; `docs_consistency` covers the new links.

## 6. Consumer implementation (orrery)

- **Typed parsing** (`src/sim/scene.ts` or a sibling): a `RegionScene` type
  with strict validation (array lengths = `(N+1)²`, finiteness, `biome` indices
  in-legend, `circulation_bands ≥ 1` when present). Strict because the client
  is pinned to a `world-wasm-v3` release that guarantees the shape.
- **Evaluators** (`src/sim/climate.ts`): the existing `temperatureAt` /
  `coldestC` **extended to accept regional arrays**, not forked — one evaluator,
  both scene shapes (§3.4). `cubeSphere.ts`'s projection is already the
  normative one; the parser and a small adapter feed regional layers to it.
- **The consumer proof (this campaign's tracer):** load one regional tile via
  `hw_scene_tiles_region`, build its `(N+1)²` patch mesh from the address, and
  render it registered on the globe — a single patch, seam-verified against the
  global surface, not the full select/split/merge tree. Proves the whole data
  path (address → layers → mesh → correct placement) in production posture.
  The camera-driven LOD machinery is orrery#2's remaining scope.
- **Re-pin**: `CATALOG_VERSION` → `world-wasm-v3`; vendored
  `public/hornvale_world.wasm` refreshed in the same commit as the strict
  parser (never strict-parse against the old binary).

## 7. Contract tests (the campaign's spine)

**Producer (hornvale):**
- **Determinism**: same address + same world → byte-identical document; a
  rebuilt world reproduces it (the `scene_is_byte_deterministic` pattern).
- **Grid & sizing**: all per-node arrays length `(N+1)²`; address validation is
  loud on bad face/level/ix/iy/samples; node order matches the projection.
- **Sampling correctness**: discrete layers at a node equal the nearest cell's
  value (`nearest_to_position`); at a node coincident with a cell center the
  barycentric layers equal that cell's value exactly (interpolation is exact at
  the vertices).
- **Commutation (the §3.4 property)**: `interp(temperature_at)` (form A) equals
  `interp(mean) + interp(swing)·sin(τ·frac(day/period))` (form B) over all nodes
  × sampled days. The two forms are equal in *real* arithmetic (the period is
  global, so `θ` factors out of the weighted sum) but differ by float rounding
  because the weighted sums reduce in a different order — so the test asserts
  **tight-approximate** equality (relative ~1e-9), not bit-exact. (Only the
  nearest-cell degenerate case is bit-exact, as in The Isotherm.) A client reads
  the *quantized* layers and reproduces it to ~8 significant digits; the page
  states these bounds so no consumer mistakes one precision for another.
- **Locked / zero-obliquity**: `t_swing_c` all-zero, `circulation_bands`
  absent, evaluator degenerates to the mean.
- **Golden**: a committed `scene-tiles-region-seed-42-*.json` at a fixed
  address, byte-checked in CI (added to the artifact-freshness list). This is
  the producer-sourced golden — the campaign's spine, per The Isotherm's core
  lesson (the golden must be the producer's own output, never a client
  reconstruction of itself).

**Consumer (orrery):**
- **The binary is the fixture**: vitest instantiates the vendored wasm, calls
  `hw_new(42)` + `hw_scene_tiles_region(...)`, and asserts the strict parser
  accepts the real document. No committed JSON copy (a copy only drifts).
- **Evaluator equivalence**: producer-pinned golden triples `(node, day) → t`
  (regenerated by a documented command, committed in orrery testdata with
  provenance) assert the TS evaluator reproduces the Rust values to
  quantization precision — over *regional interpolated* layers, proving the
  extended evaluator and the commutation property cross the repo boundary.
- **Seam smoke**: the single-patch render test asserts the patch's shared-edge
  nodes agree with the global surface within tolerance; existing Playwright
  smoke stays green.

## 8. Consequences for existing artifacts

Purely additive: a new schema, a new export, a new CLI subcommand, a new
reference page, one new golden example. **No `scene/tiles/v1` change** (no v1
golden/gallery rebaseline). **No census regen** (no worldgen change; decision
0046 posture). **No epoch, no new streams, no new concepts.** `world-wasm`
goes v2 → v3 (additive export; the catalog-release workflow, decision 0055).

## 9. Non-goals

- **The full CDLOD renderer** (orrery#2 proper): select/split/merge,
  geometry cache/dispose, skirts, picking/markers through the tree — the
  immediate follow-on client campaign, unblocked by this one (§2).
- **Genuine sub-110 km fidelity**: not invented here. The render-lens noise
  path (MAP-49) is the only honest source and is a separate campaign.
- **Per-region features / settlements** (ledger #4): layer-only v1; additive
  later.
- **Raising the globe level** or a regional re-mesh: that would be a
  determinism epoch, not a tier — out of scope.
- **Non-temperature interpolated evaluators** on the client: the client only
  reconstructs temperature; every other layer is consumed as shipped.

## 10. Flagged for G3 (autopilot carve-out summary)

1. **Fidelity call (Nathan-decided, ledger #1):** interpolate continuous
   layers, keep discrete nearest-cell; the contract states the ~110 km cell
   floor and the coast-disagreement caveat plainly (§3.5). Confirming this is
   the posture the spec builds on.
2. **Determinism-contract (ledger #5):** the producer owns the normative
   cube-sphere projection; the client consumes layer arrays **positionally and
   must not reproject to look up values** (the likeliest client bug, the
   `year_phase_offset` analogue). Barycentric interpolation is producer-internal
   and golden-pinned; the commutation property (§3.4) keeps the evaluator exact.
3. **New schema, not a v1 change (ledger #2):** `scene/tiles-region/v1`
   alongside v1; save-format-class discipline; one new golden example.
4. **Scope decomposition (§2):** campaign 1 = contract + producer + world-wasm-v3
   + contract tests + a single-patch orrery proof; the full CDLOD renderer is
   orrery#2's own campaign. Nathan can fold it in if he wants one arc.
5. **External actions at close (G6):** `world-wasm-v3` tag push + release;
   orrery re-pin; ticket comments/closes (hv#3, orrery#2 status).
6. **Independent loose end (ledger #6):** the Menagerie CI red is handled as a
   separate tiny fix, not part of this campaign.

## 11. Definition of done

Both repos' gates green (hornvale `make gate`; orrery vitest + the render
smoke + Playwright); the §7 contract tests in place on both sides; the
`scene-tiles-region-v1.md` page written and the v1 page cross-linked; the
golden example committed and CI-drift-checked; `world-wasm-v3` released and
orrery re-pinned (Nathan-authorized at G6); chronicle entry + retrospective +
book freshness sweep (including any Confidence-Gradient re-score) per the
standard campaign close.
