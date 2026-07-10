# The Measured Coast — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Campaign:** 25 (confirm numbering at merge — Deep Time, Campaign 24, is in progress on a parallel branch)
**Provenance:** The continental-rendering brainstorm (2026-07-09) diagnosed the "guitar-pick" continent shapes as the signature of the generator's abstraction: a first-order Voronoi partition with one continental flag per plate produces near-convex regions at a single scale. The ratified remedy is a three-campaign roadmap — this campaign (measure + lens, no terrain epoch), then **Crust** (terrain epoch v2: crust as a per-cell field, cratons, margins, hypsometry), then **Sculpting** (terrain epoch v3: modulated relief, belt anatomy, erosion, hotspot trails). This spec covers only the first. Its premise: measurement before surgery — the shape improvements of B and C must be judged against preregistered metrics, not vibes.

---

## 1. Goal

Two independent deliverables, with **zero save-format impact** — world JSON, almanacs, every drawn stream, and every `Stream` consumption order stay byte-identical:

1. **Shape metrics and the Census of Coasts** — lab metrics that quantify continental shape (shoreline development, hypsometric bimodality, continent structure, plate-size concentration), plus a committed, drift-checked census over them. This is the "before" photograph.
2. **Render-time coastline refinement** — the elevation map's pixel pass interpolates between cell centers and adds bounded, seeded coastal displacement, turning 220 km staircase coasts into pixel-scale coastlines without changing any world. Coarse-constrains-fine, applied literally at the lens.

## 2. Shape metrics (Part 1)

Four metric families appended to the lab registry (`windows/lab/src/metrics.rs`), each computed from the derived `TectonicGlobe` (never from serialized state):

- **Shoreline development index** — `D = L / (2 √(π A))`, coastline length over the circumference of the circle with the land's area (the limnology standard, honest at cell scale where a fractal dimension is not). `L` sums the great-circle lengths of cell edges whose two cells straddle sea level; `A` sums land-cell areas. The discrete estimators (cell area, edge length on the geodesic grid) are plan detail, but must be deterministic, documented in the metric's doc comment, and consistent across metrics. A compact single-plate continent scores near 1; a fjorded coast scores several times that.
- **Hypsometric bimodality** — Ashman's `D = |μ_land − μ_ocean| / √((σ²_land + σ²_ocean)/2)` between the land and ocean elevation populations, plus a **shelf fraction**: the fraction of cells within a fixed band of sea level (band width a named constant, Earth-informed, on the order of ±200 m). Earth's hypsometry is strongly bimodal with a populated shelf; the current generator is expected to score bimodal but shelf-poor.
- **Continent structure** — connected-component count over land cells (deterministic BFS in cell-id order) and the largest component's share of land area.
- **Plate-size concentration** — the Gini coefficient over plate cell counts. Earth is heavy-tailed (~7 giants plus microplates); the current near-uniform Voronoi should score low, and this metric is the direct witness for Crust's plate-size work.

**The census.** Following the house pattern (big author-time census + small CI drift variant):

- `studies/census-of-coasts.study.json` — the new metrics over 10,000 seeds, default pins; published to `book/src/laboratory/` like the existing censuses.
- Drift coverage of the new metrics needs no new study: the three CI-run
  studies (`census-lands-drift`, `census-of-the-meeting`, `census-of-skies`)
  select `"metrics": "all"`, so the new columns are drift-checked from the
  commit that adds them. (Amended during planning: the originally proposed
  `census-coasts-drift.study.json` would have double-run identical builds.)

Studies are data, metrics are code (decision 0011). The census tables become the preregistered baseline that Campaigns B and C must move — the target directions (shoreline index up, shelf fraction up, plate Gini up, largest-continent share more dispersed) are recorded in the book's laboratory page prose alongside the baseline numbers.

## 3. Render-time coastline refinement (Part 2)

**Today** (`domains/terrain/src/render.rs`): each pixel of the equirectangular map takes the elevation of its single nearest cell via `NearestCellIndex`, so a coastline is a chain of 220 km cell silhouettes.

**The refinement**, per pixel:

1. **Interpolate** elevation over the nearest cell centers (inverse-distance-squared over the three nearest; exact estimator is plan detail, deterministic and seam-free at cell boundaries and poles).
2. **Displace near the coast**: `e(px) = interp + A(interp) · n(p̂)`, where the amplitude envelope `A` peaks where `interp` is at sea level and decays with `|interp − sea_level|` (Gaussian envelope; peak amplitude and width are named constants with peak ≤ the envelope width, chosen in the plan so displacement can never flip a cell-interior pixel). The noise `n` is the kernel's stateless seeded value-noise/fBm evaluated on the unit-sphere position (e.g. averaged over the three orthogonal coordinate-plane slices to avoid seams and pole artifacts) — **no `Stream` draws are ever consumed**.
3. `MAP_WIDTH` rises 256 → 1024 (PNG 1024×512) so the detail is visible. The 72×24 ASCII REPL map is unchanged.

**Seeding.** The noise seed derives from the world seed under a new label in the terrain `streams` module, published through `stream_labels()` like every label. The label is a save-format constant from birth; because it feeds only hash-noise (never a `Stream`), no consumption-order contract is touched.

**Contract posture.** No epoch — the world is untouched; the lens improved. The refinement is the coarse-constrains-fine doctrine at pixel scale: the cell's elevation is the prior, the pixel value stays within the (bounded) envelope of it, and a land/ocean flip is only possible where the interpolated elevation is already within the envelope of sea level — the coastal ring.

**Blast radius (enumerated, regenerated in one commit):** the committed gallery elevation map (`book/src/gallery/`), the generated stream manifest (`book/src/reference/`, one new label row), and any render test pinning image bytes or `MAP_WIDTH`. CI's drift check confirms nothing else moved — in particular the three seed-42 almanacs and `world.json` must be byte-identical.

## 4. Tests

- **Metric correctness on synthetic globes** — a hemisphere-cap landmass must score near the shoreline index's floor; a deliberately crenellated mask must score strictly higher; Ashman's D and shelf fraction pinned on hand-built elevation maps; component count/share on multi-blob masks.
- **Metric determinism** — same seed → identical metric values across two independent world builds (house property-test style).
- **Refinement respects the prior** — property test over seeds: every pixel's elevation within the envelope bound of its interpolated prior; every land/ocean flip occurs only where `|interp − sea_level|` is inside the envelope; pixels far from coasts byte-match plain interpolation.
- **Save-format byte-identity** — world JSON and almanac output byte-identical before/after the render change (asserted in-test, not just by CI drift).
- **Census drift** — covered by the existing `all`-metric CI studies from the metrics commit onward.

## 5. Definition of Done

- The full gate (`cargo test --workspace`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`).
- Census of Coasts published to the book's laboratory section with baseline numbers and target directions; gallery map regenerated at 1024×512; stream-manifest reference regenerated.
- Chronicle entry; freshness sweep of stale chapters (the map appears in several); campaign retrospective in `docs/retrospectives/` (decision 0020).
- `IMPLEMENTATION_PLAN.md` removed when all stages complete.

## 6. Non-goals and coexistence

- **No generator change of any kind** — no crust field, no plate-shape change, no `GLOBE_LEVEL` bump, nothing that alters a drawn value. Those are Campaigns B (Crust) and C (Sculpting), each a deliberate terrain epoch.
- **No `windows/worldgen` edits.** Part 1 appends to the lab registry; Part 2 is terrain-domain-local. This keeps the campaign disjoint from the in-progress Deep Time branch (Campaign 24); the merge-time check is numbering plus the CI drift list, per the parallel-campaign discipline.
- **Rift-and-fit fake history** stays on the idea registry, held for judgment against this campaign's metrics after Sculpting.
