# The Bearing — design

**Working name** (blessed at G6). A determinism-safe performance campaign: give
`NearestCellIndex` a 2-D (latitude × longitude) bucket grid so resolving a
direction/position to its nearest cell scans a small neighborhood instead of
three full latitude rings. Third geosphere-perf campaign of the session (after
The Lookup, shipped; The Commons, parked at G3).

## Goal

`NearestCellIndex::nearest` — the direction→cell lookup — is the dominant cost
of the entire **observation surface**: a samply profile on current `origin/main`
put it at **78% of the `map` (elevation) render**, 36% of `scene tiles`, 19% of
`biome-map`, and 10.5% of the census. It buckets cells into 30 latitude bands
and scans the query's band ± 1 — but each band holds a full longitude ring
(~1,365 cells at level 6), so a query touches ~4,100 cells, and a 1024×512 map
does ~2.1 billion dot products. Sub-bucket each band by longitude and scan only
a longitude window around the query; return the **bit-identical** cell.

## Background — the measurement

Prototyped in `spike-nearest-index`: a 2-D grid (30 lat bands × 60 lon buckets),
scanning band ± 1 and a longitude window of half-width `ceil(24° / cos(lat) /
6°)` buckets, saturating to the full ring near the poles.

- **`map` (elevation): 1.806 s → 0.760 s (2.4×)**; `scene tiles` 0.54 → 0.42 s;
  `biome-map` 0.45 → 0.38 s.
- **Byte-identical:** an equality test (`a1_grid_matches_the_full_band_scan_
  over_a_dense_sweep`) confirms the grid returns the exact cell the full band
  scan returns over a 16k-point equirectangular sweep at levels 3–6, plus every
  cell center resolving to itself. (That equality test is the real proof; the
  seed-42 world sha is also unchanged, corroborating but weak — genesis may not
  exercise `nearest` heavily, so the artifact/census drift checks are the
  load-bearing end-to-end gate.)
- `nearest` fell from 78% → 45% of the map render (the latitude axis is still
  three full bands — headroom, see followups).

## The change (`kernel/src/geosphere.rs`)

- `NearestCellIndex` stores `grid: Vec<Vec<CellId>>` indexed by `band *
  LON_BUCKETS + lon_bucket`, cells ascending `CellId` within each bucket.
- A shared `scan_at` walks band ± 1 × a longitude window widened by 1/cos(lat)
  (full-ring fallback when the window would exceed the sphere). `nearest`
  (lat/lon) and `nearest_to_position` (a unit vector; derives lat via `asin z`,
  lon via `atan2`, cos(lat) via `sqrt(1 - z²)` — no extra transcendental for
  cos) both call it. The `dot3` compare and the `d > best_dot` strict tie-break
  are unchanged.

Public signatures (`nearest`, `nearest_to_position`) are unchanged; every
consumer is untouched.

## Byte-safety — the campaign's spine

**A1's contract is exact reproduction, not "the true nearest": it must return
the same cell the band scan did, for every query, at every level the index is
built at.** That reframes the determinism risk precisely:

- **RISK, leads G3 — coverage is empirical, not proven, and untested below
  level 3.** The longitude window must always contain every cell the full band
  scan would have found. It's a generous bound (24°/cos(lat), full-ring near
  poles) validated by the dense sweep — but only at **levels 3–6**. At coarse
  levels the mesh's covering radius grows (level 2 ≈ 15°), so the window could
  under-cover and silently return a different cell than the band scan — a
  catastrophic-silent kernel bug. **Mitigations the plan MUST carry:** (1) the
  equality test runs at **every level the index is used at** (enumerate the
  callers — renders, room, scene, climate provider, census — levels 2–6); (2) a
  coarse-level / low-cell-count **safety fallback** to the full-ring scan, so
  windowing applies only where the test proves it exact; (3) the artifact-drift
  and census-drift checks as the final gate — the committed PNGs and census must
  not move a byte.
- Not an epoch: no draw, no seed label, no serialized-byte change
  (`NearestCellIndex` is a derived index, never serialized).

## Scope

**In:** the 2-D grid for `NearestCellIndex`; the equality test extended to all
used levels; the coarse-level safety fallback; the tuning constants
(`LON_BUCKETS`, `NEAR_COVER_DEGREES`) documented as coverage-load-bearing.

**Out (followups):** B1 (precomputed pixel→cell render table — stacks on A1,
the next render win); tighter latitude windowing (nearest is still 45% of map);
A2/A3 (mesh-hierarchy descent / warm-start walk — byte-fragile); A4 (HEALPix/S2
— frontier rework); profiling `possess`.

## Non-goals

No new dependency, no physics/format/draw change, no approximate lookup (the
result stays bit-exact). Independent of The Commons (different file, different
mechanism); they compose but land separately.

## Decisions (promoted from the ledger)

- **A1 (2-D bucket grid), returning the bit-identical cell** — over A2/A3
  (byte-fragile), A4 (rework), and approximation (forbidden). Chosen via the
  session's rich ideonomy exploration (ledger #1).
- **Coverage is validated by an all-levels equality test + a coarse-level
  full-ring fallback**, not left empirical (the G3 risk resolution).
- **Not an epoch**; artifact-drift + census-drift are the merge gates.

## Definition of Done

The 2-D grid landed; the equality test green at every used level; the coarse-
level fallback in place; full `make gate` + artifact-drift (`map`/`biome-map`/
`paleo`/`settlement` PNGs unmoved) + census-drift on the merged tree; chronicle
+ retrospective; followups (B1, latitude windowing, A2/A3/A4, `possess`)
promoted.
