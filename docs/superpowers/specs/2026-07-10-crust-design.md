# Crust — Design

**Date:** 2026-07-10
**Status:** Approved (brainstorming session)
**Campaign:** Crust (slug-named per decision 0026; The Branches is in flight in parallel)
**Provenance:** Second leg of the ratified terrain-overhaul roadmap (Measured Coast → **Crust** → Sculpting). The Measured Coast (Campaign 25) preregistered the baseline this campaign is judged against (Study 010). Design decisions ratified in brainstorming (2026-07-10): canonical level-6 grid **plus** a `--globe-level` fidelity pin; conservative epoch (plate skeleton streams survive); parallel with The Branches, Crust merges second. Three ideonomy pulls hardened the design; their findings are folded in throughout and called out where load-bearing.

---

## 1. Goal

**Terrain epoch v2.** Continental crust becomes a per-position field decoupled from plates; continents grow from drawn cratons; plate sizes go heavy-tailed and plate edges go irregular; elevation becomes isostatic over crust thickness, producing a true shelf and passive/active margin asymmetry; the canonical grid rises to level 6 with a `--globe-level` pin. Same seed, deliberately different world — the guitar pick dies here. Success is measured, not vibed: the after-census must land inside preregistered acceptance bands (§7).

## 2. The crust field

New module `domains/terrain/src/crust.rs`.

**Cratons are drawn objects.** New stream `cratons` (one label, documented draw order): a continental-area budget draw, a craton count (drawn range scaled to the budget), then per-craton center position (two draws, area-preserving), radius, and age, sequentially. Craton count is pinnable (§7).

**Crust is a pure function of position.** `crust_thickness(p)` and `crust_age(p)` for any unit vector `p`: each craton contributes a lobed kernel — radial taper from its center, modulated angularly by stateless fBm keyed to (seed, craton id) so outlines are lobed and irregular, never circular; the field takes the `max` over cratons (thickness) and the age of the winning craton (age), with a thin oceanic floor elsewhere. No per-cell draws anywhere: any grid at any level samples the same underlying world, and coarse-constrains-fine is exact rather than approximate. The lobing kernel is its own unit with its own tests.

**Continental = thickness above threshold.** A named constant `CONTINENTAL_THRESHOLD` splits crust into continental and oceanic. `Plate.continental` and the `plate-kind` stream retire — superseded, label never reused. (Grep confirms no consumer of `Plate.continental` outside the terrain crate.)

**Fields are kernel Fields.** `crust_thickness`/`crust_age` implement the kernel's `Field<T>` trait (`kernel/src/field.rs`). Terrain declares the `Position` interpretation: `x` = longitude degrees, `y` = latitude degrees, converted internally to the unit sphere; crust ignores `WorldTime` (static at this tier). This makes the canonical-grid/views contract (§6) constitutional machinery, not a terrain convention.

**Typed quantities:** thickness crosses the API as a `CrustKm` newtype (validating constructor, named conversions); craton age stays a bare `f64` ratio in [0, 1] (`type-audit: bare-ok(ratio)`); every new `pub` primitive carries its type-audit verdict tag (the check is default-deny in CI).

**Banked:** crust thickness is the vertical budget the underworld (MAP-10) and ore extraction (TECH-3) will someday read. Nothing consumes it beyond elevation in this campaign.

## 3. Plates v1.5 (conservative epoch)

Kept byte-for-byte: `plate-count`, `plate-seeds`, `plate-motion`, `maturity`, `hotspots`, `ocean-fraction`, `coast-render` — seed 42 keeps its recognizable plate skeleton, making the before/after a controlled experiment (same skeletons, new flesh).

- **Heavy-tailed sizes:** new stream `plate-weights` — one weight per plate via a power-law transform of uniform draws — feeding weighted Voronoi assignment (`assign_plates` maximizes `dot − w·penalty` or equivalent; exact form is plan detail, deterministic with the existing tie-break). A few giants, a fringe of microplates; the plate-size Gini metric is the witness.
- **Irregular edges:** the assignment metric gains a stateless fBm perturbation (hash-noise keyed under a new `plate-edge` label; no draws) so plate boundaries wander instead of tracing great circles.
- Boundary classification, `velocity_at`, and the maturity machinery survive unchanged over the new assignment.

## 4. Isostasy, margins, and sea level

Elevation assembly v2 (`elevation.rs`, same file, new formula — the epoch):

```
e(cell) = isostatic(crust_thickness(cell))        // Airy: thick floats high
        + boundary term (kept machinery, re-tuned amplitude constants)
        + hotspot term (kept stream)
        + strict-ordering epsilon (kept)
```

The craton kernel's radial taper crosses `CONTINENTAL_THRESHOLD` gradually, so isostasy produces a **shelf band** at every continental edge for free — passive margins are the taper; active margins are the taper plus the existing convergent-boundary terms (trench, coastal range). The hypsometric curve becomes genuinely bimodal-with-shelf.

**Sea level** keeps its exact-percentile mechanism and the `ocean-fraction` label unchanged. The epicontinental-sea bias from the roadmap is **accomplished structurally and needs no mechanism**: once a shelf plateau exists in the hypsometry, a wide range of drawn ocean fractions places sea level on it — shallow inland seas are the default outcome, not a special case. (YAGNI: the previously imagined draw-bias transform is dropped.)

**Elevation stays canonical-grid-first.** The boundary term depends on graph distance (BFS) — mesh-bound, not pointwise. Crust is a Field; elevation is a `CellMap` on the canonical grid containing both field-derived and graph-derived terms. The map lens continues to interpolate the grid (Campaign 25 machinery, unchanged).

## 5. Canonical grid and the level pin

**The contract (to be ratified as a decision, slug `identity-computes-on-the-canonical-grid`):** *a world's identity-bearing derivations — sea level, drainage, endorheic basins, land components, settlement placement, paleoclimate strata — compute once on the world's canonical grid; everything pointwise is a Field that observation may sample at any resolution.* Campaign 25's coastline lens was the first instance; crust makes it constitutional.

- `GLOBE_LEVEL` default rises 5 → 6 (10,242 → 40,962 cells, ~110 km).
- New pin `--globe-level` (legal 4–7): the canonical level joins world identity — same seed at different levels is a different world, by declaration. The pin consumes no draws (structural, like the composition of the pin set itself); crust fields make cross-level worlds *consistent* (shared geometry) without being *identical* (sea level, drainage, and facts differ). Worldgen's one-shot static `Geosphere` becomes a per-level cache (`BTreeMap<u32, Geosphere>` behind a lock, deterministic).
- Icosphere levels nest (level-N vertices are a subset of level-N+1 vertices): the field-consistency test asserts `crust_thickness` byte-agrees at shared vertices across levels.

## 6. Pins

`TerrainPins` gains two members; one changes meaning; all get metering.

- **`--globe-level <4..=7>`** — canonical grid level; default 6 (§5).
- **`--continents <N>`** — pins the craton count (legal 1–16; the drawn range is plan-tuned within it); the count draw is consumed either way (pin isolation).
- **`--supercontinent`** — semantics move from clustering continental *plates* to clustering *cratons*: `Some(true)` re-centers drawn craton positions around the first-drawn craton (same draws consumed, positions re-derived — the Measured-Coast-era clustering algorithm transplanted to cratons); `Some(false)` re-affirms scattered. Interacts legally with `--continents`.
- **Pin metering** (ideonomy pull 2): genesis notes record, for every pinned value, the drawn value it overrode — `"pinned ocean-fraction 0.80 (seed draws 0.64)"`. Notes only; no ledger facts; `scout` and the Lab read them for free.

## 7. Measurement: two-stage, bands not directions

**Two-stage methodology.** The shoreline-development estimator is resolution-dependent, so a single before/after across both a level bump and a generator change would be confounded. The campaign therefore lands in two measured stages on one branch:

1. **Stage L (level bump only):** `GLOBE_LEVEL = 6`, generator v1 otherwise. Re-run census-of-coasts → the **interim baseline** (v1@L6), quoted in prose.
2. **Stage C (crust):** the full v2 generator. Re-run census-of-coasts → the **after** photograph (v2@L6), judged against the bands below.

**Acceptance bands** (preregistered here, before any generator code lands; judged on v2@L6 medians; undershoot and overshoot both fail — ideonomy pull 3: every classic procgen failure is an overshoot of a "good" direction):

```
metric                    band              rationale
------------------------  ----------------  ----------------------------------------
shoreline-development     1.3x - 3.0x of    estimator is resolution-bound; band is
                          v1@L6 interim     relative to the same-grid interim
hypsometric-bimodality    2.0 - 8.0         retain Earth-like separation; above 8
                                            means two delta functions, no shelf
shelf-fraction            0.08 - 0.22       Earth's true shelf+coastal-plain share;
                                            SUPERSEDES Study 010's "up" direction
                                            (see note)
continent-count           3 - 12            Earth: 6-8 major landmasses
largest-continent-share   0.25 - 0.65       Afro-Eurasia is ~0.57 of Earth's land
plate-size-gini           0.45 - 0.75       ~7 giants + microplate fringe
```

**Supersession note (recorded openly, per the preregistration discipline):** Study 010 preregistered "shelf fraction up" from a baseline median of 0.274. That baseline number reflects decay-slope coasts — many cells *near* sea level without shelf structure — not a shelf. A true shelf is a *mode near sea level inside an otherwise bimodal distribution*; Earth's within-±200 m share is roughly 0.10–0.15 of the surface. The band above supersedes the direction, and the Census of Coasts II page must state this supersession and its reasoning explicitly — changing a preregistered target is permitted only in the open.

**Publication mechanics** (ideonomy pull 3): re-running the census overwrites `book/src/laboratory/generated/census-of-coasts/` — the drift-checked artifacts always show the *current* epoch. Each epoch's photograph survives in hand-written prose: Study 010 holds the v1@L5 baseline; a new **Census of Coasts II** page (next study number free at merge) records the v1@L6 interim, the v2@L6 after, the band verdicts, and the supersession note. Crust's "after" is Sculpting's "before".

**Census hygiene riding the epoch:** `census-of-skies` switches from `"metrics": "all"` to the named sky metrics (its terrain columns were always redundant with `census-lands-drift`); with the 10,000-seed sky census no longer building terrain-heavy metrics at L6, CI's lab step stays in minutes. (Lazy per-domain `WorldView` builds are explicitly deferred — YAGNI until profiling says otherwise.)

## 8. Deep Time coupling (owned, not discovered)

Paleoclimate consumes `elevation` and `sea_level` through the composition root. With a real shelf, eustatic swings (±120 m full-ice) flip large shelf areas — ice-age land bridges and drowned plains are the intended payoff of this coupling, arriving with this campaign. Consequences the spec owns: the paleo gallery artifacts (`paleo-seed-42.md` and images) regenerate with dramatically different strata; fossil-shoreline facts change; any paleoclimate test pinned to current strata values re-baselines in the epoch commit (the golden-pin discipline: re-pin in the drifting commit). No paleoclimate *code* changes.

## 9. Epoch mechanics

**Stream ledger of the epoch:**

```
kept      terrain root, plate-count, plate-seeds, plate-motion,
          maturity, hotspots, ocean-fraction, coast-render
retired   plate-kind        (superseded by the crust field; label
                             never reused, per the epoch discipline)
new       plate-weights     (per-plate weight draws)
new       plate-edge        (hash-noise only; no draws — manifest-
                             documented like coast-render)
new       cratons           (budget, count, then per-craton
                             position/radius/age, sequential)
```

**Fixture refreezes, all in the epoch commit(s), never deferred:** `cli/tests/fixtures/world-seed-42.json` (the lens-purity guard — its module doc says exactly this: deliberate epochs regenerate it in the same commit, chronicle records why), the three seed-42 almanacs, elevation/biome/settlement/star-chart galleries where world-dependent, the paleo gallery, the stream manifest, `pre-branches-*` fixtures if The Branches has merged by then (merge-second reconciliation). Stage L and Stage C each regenerate the full drifting set in their own commit.

**Type audit:** every new `pub` primitive field carries a verdict tag; `CrustKm` joins the newtype family; the committed audit report regenerates.

**The Branches:** Crust merges second (ratified). At merge: renumber if 28 is taken, refreeze fixtures over the merged base, reconcile `windows/worldgen` (both campaigns touch it), and re-run both campaigns' identity tests.

## 10. Tests

- **Pin isolation** extended to the new pins: `--continents`, `--supercontinent` (craton semantics), `--globe-level` — pinned and unpinned paths consume identical draws per stream (house pattern from `tectonic_properties.rs`).
- **Field purity and cross-level consistency:** `crust_thickness`/`crust_age` are pure (`Field` contract) and byte-agree at vertices shared between nested levels.
- **Lobing kernel unit tests:** radial monotonicity of the envelope, angular modulation bounded, no circular outlines (angular variance above a floor).
- **Isostasy:** monotone in thickness; shelf-band existence (a synthetic single-craton world has a nonempty elevation band between abyssal and platform modes).
- **Heavy tail:** Gini over drawn plate weights exceeds a floor across a seed sweep.
- **Genesis property batteries** (`genesis_properties.rs` / `tectonic_properties.rs`) extended, not replaced; all existing invariants that survive the epoch (velocity tangency, plate assignment totality, sea-level percentile exactness, drainage determinism) still hold at level 6.
- **Census bands:** an author-time check (not CI) that the v2@L6 medians sit inside §7's bands, recorded in Census II; CI drift-checks the regenerated artifacts as always.

## 11. Definition of Done

- The full gate; type-audit check + regenerated report; both census stages run and published; Census of Coasts II page with band verdicts and the supersession note; chronicle entry; freshness sweep (terrain.md, laboratory overview, any chapter stating cell counts, plate-flag prose, paleo chapters); decision doc `identity-computes-on-the-canonical-grid`; retrospective; idea-registry row for adaptive/local mesh refinement (`raw`, rejected-for-now rationale); fixture refreezes per §9; merge-second reconciliation with The Branches.

## 12. Non-goals

- **Terranes, microcontinents, fBm relief, belt anatomy, erosion, hotspot trails, discrete arcs** — Sculpting (terrain epoch v3), per the roadmap.
- **Rift-and-fit fake history** — MAP-21, judged after Sculpting.
- **Adaptive/local mesh refinement** — registry row only; breaks uniform-mesh assumptions for no gain the lens doesn't already provide.
- **Full plate simulation over geologic time** — rejected in the roadmap brainstorm; fights "the seed is the world's identity".
- **Lazy per-domain WorldView builds** — deferred until profiling demands it; the census-of-skies metric-selection fix covers the known waste.
