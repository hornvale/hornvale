# The Bearing — implementation plan

> **For agentic workers:** the 2-D grid exists as a validated prototype in this
> worktree (`kernel/src/geosphere.rs` + the `a1_grid_...` test). Execution
> **formalizes** it and — the material addition over the spike — makes coverage
> **provable** (window sized from the mesh's covering radius) rather than a
> hand-picked constant, and extends the equality test to every level the index
> is used at. Do NOT re-derive the scan from scratch (byte-diverging risk); the
> prototype's `dot3` compare + `d > best_dot` tie-break are the reference.

**Goal:** `NearestCellIndex` gets a 2-D (lat band × lon bucket) grid returning
the bit-identical cell the full band scan returned, ~2.4× on the map render.

**Architecture:** window the longitude scan to the query's neighborhood, sized
so it provably contains every cell the band scan would find; full-ring near the
poles / at coarse levels.

## Global Constraints (verbatim from the spec)

- **Bit-identical to the band scan, at every level the index is built at.** Not
  an epoch (the index is a derived structure, never serialized). No physics/
  draw/label/format change.
- Merge gates: `lens_purity`, the equality test at all used levels, full `make
  gate`, **artifact drift** (`map`/`biome-map`/`paleo`/`settlement` PNGs must not
  move a byte) and **census drift** (1000 seeds, Nathan's out-of-band regen).
- No `HashMap`/`HashSet`; `#![warn(missing_docs)]`; type-audit tags; `cargo fmt`.

---

### Task 1: 2-D grid with a provable coverage bound

**Files:** `kernel/src/geosphere.rs` (`NearestCellIndex`).

**Interfaces:** `new`, `nearest`, `nearest_to_position` signatures unchanged;
result bit-identical to the band scan.

- [ ] **Step 1 — coverage bound at construction.** In `new`, compute the mesh's
  covering radius as `max` over cells of the angular distance to the farthest
  adjacent neighbor (`geo.neighbors(c)`, `acos(dot3(pos,posn))`), stored as a
  degrees bound `cover_deg` on the index. This provably exceeds the distance
  from any query to its nearest cell, so a longitude window of half-width
  `ceil(cover_deg / cos(lat) / LON_DEGREES)` cannot exclude the band scan's
  winner. (Replaces the prototype's hand-picked `NEAR_COVER_DEGREES` constant.)
- [ ] **Step 2 — grid + windowed `scan_at`** (already prototyped): `grid:
  Vec<Vec<CellId>>` by `band * LON_BUCKETS + lon`, ascending `CellId`; scan
  band ± 1 × the lon window; **full-ring fallback** when `2k+1 >= LON_BUCKETS`
  (poles/coarse). `nearest` (lat/lon) and `nearest_to_position` (asin/atan2,
  `cos(lat)=sqrt(1-z²)`) share it. `dot3` + `d > best_dot` unchanged.
- [ ] **Step 3 — verify** `cargo test -p hornvale-kernel` (incl. Task 2's test).
- [ ] **Step 4 — commit:** `perf(kernel): 2-D lat/lon index for NearestCellIndex (The Bearing)`.

### Task 2: The all-levels equality test

**Files:** `kernel/src/geosphere.rs` `#[cfg(test)]`.

- [ ] **Step 1 — enumerate used levels.** Grep `NearestCellIndex::new` callers
  (renders, room, scene, climate provider, census); confirm the level range.
  Document it in the test.
- [ ] **Step 2 — extend the equality test** (`a1_grid_matches_the_full_band_
  scan_over_a_dense_sweep`) to run at **every used level (2–6)**, dense
  equirectangular sweep + every-cell-center-resolves-to-itself, asserting
  `index.nearest == band_scan` (the reference reimplemented in-test). Add an
  assertion that the computed `cover_deg` bound is `>=` the observed max
  query→nearest distance over the sweep (the coverage is sufficient, not just
  lucky).
- [ ] **Step 3 — run:** expect green at all levels. If level 2 diverges, the
  bound/fallback is wrong — fix Task 1, not the test.
- [ ] **Step 4 — commit:** `test(kernel): The Bearing — equality vs band scan at every used level`.

### Task 3: End-to-end drift verification

- [ ] **Step 1:** `lens_purity` + the terrain/climate/scene render tests green.
- [ ] **Step 2:** `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` then
  `git diff book/src/gallery` — the committed PNGs (`elevation`, `biome`,
  `paleo`, `settlement`, `column`, `sediment`, `features`, `vestige`) must be
  **byte-unmoved**. This is the real end-to-end proof the index reproduces the
  band scan on the actual render grids.
- [ ] **Step 3 — commit** only if a regen refreshed a drift-clean artifact
  (expected: no diff, no commit).

### Task 4: Close

**Files:** `book/src/chronicle/the-bearing.md` (+ SUMMARY);
`docs/retrospectives/the-bearing.md`.

- [ ] Chronicle (book altitude: the observation surface was the lurking cost;
  the 2-D window; provable coverage; ~2.4× on the map, byte-identical).
- [ ] Retrospective (the stale-profile trap that hid this until we profiled
  renders; empirical→provable coverage; the equality-test-as-contract; the
  spike-then-formalize mode). Promote followups (B1, latitude windowing,
  A2/A3/A4, `possess`).
- [ ] Absorb origin/main; full `make gate` + artifact-drift on the merged tree;
  census drift is Nathan's out-of-band carve-out. G6 hard-stop package.

## Self-Review

**Spec coverage:** the 2-D grid → T1; the G3 coverage risk → T1 (provable
bound) + T2 (all-levels test) + T3 (artifact drift); DoD → T4. Complete.

**Placeholder scan:** code steps point at the prototype deliberately (re-typing
a byte-critical scan from prose is the re-derivation risk this plan forbids);
each test/verify step names its command and expected result.

**Type consistency:** `grid`/`scan_at`/`lat_band`/`lon_bucket`/`cover_deg`,
`nearest`/`nearest_to_position` — consistent with the spec and prototype.
