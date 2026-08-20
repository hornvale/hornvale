//! Underworld level generation: a chamber's own shape (The Adit).
//!
//! A chamber is a bucket, not a place (spec keystone) — `ChamberAddr`
//! addresses which of up to `BRANCHES_PER_SYSTEM` interchangeable habitats
//! exists; nothing here changes that. This module builds a second,
//! independent layer: a real room/corridor level for a rung of one cave
//! system. `FRAME`-tier under decision 0069, same as `crate::lattice`:
//! derived fresh from a `Seed` on every call, nothing serialized.

use std::collections::BTreeMap;

use hornvale_kernel::Seed;

use crate::lattice::{Cell, Rect};

mod carve;
mod region;

pub use carve::Algorithm;

/// A cell's role within a generated underworld level.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LevelCellKind {
    /// Standing room.
    Floor,
    /// Impassable rock.
    Wall,
    /// Standing room below the chamber's own water table (spec §4.4).
    Flooded,
    /// A connection down toward the next rung (spec §4.6).
    StairsDown,
    /// A connection up toward the rung above (spec §4.6).
    StairsUp,
}

/// A generated underworld level: one rung of one cave system, under one
/// surface cell. Never serialized — re-derive it from the same inputs
/// rather than storing it (decision 0069).
/// type-audit: bare-ok(count: dof)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Level {
    /// The level's bounds.
    pub extent: Rect,
    /// Every cell of `extent`, with its kind. Total: every cell of the
    /// extent appears exactly once.
    pub cells: BTreeMap<Cell, LevelCellKind>,
    /// How many independent seeded choices generation made. Reported, not
    /// recomputed, mirroring `lattice::Lattice::dof`.
    pub dof: u32,
    /// Every leaf's chosen style, in generation order — Task 6 threads the
    /// realized worked-fraction from one rung's level into the next's bias.
    pub leaf_styles: Vec<LeafStyle>,
}

/// Level extent before content is carved into it. Task 1 is unscaled by
/// rung; later tasks may widen this — see `generate_level_extent`.
const BASE_LEVEL_W: i32 = 40;
/// See `BASE_LEVEL_W`.
const BASE_LEVEL_H: i32 = 24;

/// The extent a level gets, scaled by how deep its rung sits (deeper rungs
/// get more room) — spec §4.3's "`DelveRung` modulates size", read out of
/// the rung's position in `hornvale_terrain::rungs()` rather than a
/// worldgen-internal rank (that function is not confirmed `pub` across the
/// crate boundary; this one is).
pub fn generate_level_extent(rung: hornvale_terrain::DelveRung) -> Rect {
    let rank = hornvale_terrain::rungs()
        .iter()
        .position(|r| *r == rung)
        .unwrap_or(0) as i32;
    Rect {
        x: 0,
        y: 0,
        w: BASE_LEVEL_W + 4 * rank,
        h: BASE_LEVEL_H + 2 * rank,
    }
}

/// Which content generator and worked/natural reading a leaf gets (spec
/// §4.3). The worked-vs-natural draw is independent of, but biased by, the
/// chamber's `ChamberOrigin` — decoupled on purpose, so this module never
/// writes back to a field `is_sump`'s dryness rule already depends on
/// (spec §4.4).
/// type-audit: bare-ok(flag: worked)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LeafStyle {
    /// The content generator this leaf uses.
    pub algorithm: carve::Algorithm,
    /// Whether this leaf reads as worked stone rather than natural void.
    pub worked: bool,
}

/// The worked-chance bias used when there is no previous rung to inherit
/// from (Task 6's `generate_descent` overrides this for every rung after
/// the first).
/// type-audit: bare-ok(ratio)
pub const NEUTRAL_WORKED_BIAS: f64 = 0.5;

fn choose_leaf_style(
    cave_kind: hornvale_terrain::CaveKind,
    origin: hornvale_worldgen::chamber::ChamberOrigin,
    inherited_worked_bias: f64,
    stream: &mut hornvale_kernel::Stream,
    dof: &mut u32,
) -> LeafStyle {
    use hornvale_worldgen::chamber::ChamberOrigin;
    let base_chance = match origin {
        ChamberOrigin::Made => 0.85,
        ChamberOrigin::Found => 0.10,
    };
    let worked_chance = (0.5 * base_chance + 0.5 * inherited_worked_bias).clamp(0.0, 1.0);
    let roll = stream.next_f64();
    *dof += 1;
    let worked = roll < worked_chance;
    let algorithm = if worked {
        carve::Algorithm::RoomsAndCorridors
    } else {
        match cave_kind {
            hornvale_terrain::CaveKind::Karst => carve::Algorithm::CellularCave,
            hornvale_terrain::CaveKind::LavaTube => carve::Algorithm::Tunneler,
            hornvale_terrain::CaveKind::Fracture => carve::Algorithm::AngularRooms,
        }
    };
    LeafStyle { algorithm, worked }
}

/// Generate a level, choosing each leaf's style from `cave_kind`/`origin`
/// with `inherited_worked_bias` folded in (Task 6's continuation draw).
///
/// **Derives one stream per algorithm family, once, before the leaf loop**
/// — not per leaf. Two leaves can share an algorithm (composite levels are
/// the point), so `carve`'s own doc explains why a shared, advancing
/// stream is required rather than a fresh derive per call: the same
/// `derive once, thread &mut Stream through every draw` shape
/// `lattice::allocate`/`lattice::grow` already use, and this module's own
/// `region::build_region` already follows correctly.
/// type-audit: bare-ok(ratio: inherited_worked_bias)
pub fn generate_level_with_origin(
    extent: Rect,
    cave_kind: hornvale_terrain::CaveKind,
    origin: hornvale_worldgen::chamber::ChamberOrigin,
    inherited_worked_bias: f64,
    seed: Seed,
) -> Level {
    let (tree, mut dof) = region::build_region(extent, seed);
    let mut cells = BTreeMap::new();
    for x in extent.x..(extent.x + extent.w) {
        for y in extent.y..(extent.y + extent.h) {
            cells.insert(Cell(x, y), LevelCellKind::Wall);
        }
    }
    let mut style_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_STYLE).stream();
    let mut cellular_stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_CELLULAR)
        .stream();
    let mut tunneler_stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_TUNNELER)
        .stream();
    let mut rooms_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_ROOMS).stream();
    let mut leaf_styles = Vec::new();
    for rect in region::leaves(&tree) {
        let style = choose_leaf_style(
            cave_kind,
            origin,
            inherited_worked_bias,
            &mut style_stream,
            &mut dof,
        );
        let stream = match style.algorithm {
            carve::Algorithm::CellularCave => &mut cellular_stream,
            carve::Algorithm::Tunneler => &mut tunneler_stream,
            carve::Algorithm::AngularRooms | carve::Algorithm::RoomsAndCorridors => {
                &mut rooms_stream
            }
        };
        dof += carve::carve(style.algorithm, rect, stream, &mut cells);
        leaf_styles.push(style);
    }
    connect_split_boundaries(&tree, &mut cells);
    Level {
        extent,
        cells,
        dof,
        leaf_styles,
    }
}

/// Every `Floor`/`Flooded` cell within `region`'s own leaf rects (not the
/// whole level) — the candidate endpoints a connector can anchor to.
fn walkable_cells_in(region: &region::Region, cells: &BTreeMap<Cell, LevelCellKind>) -> Vec<Cell> {
    let mut out = Vec::new();
    for rect in region::leaves(region) {
        for x in rect.x..(rect.x + rect.w) {
            for y in rect.y..(rect.y + rect.h) {
                let cell = Cell(x, y);
                if matches!(
                    cells.get(&cell),
                    Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
                ) {
                    out.push(cell);
                }
            }
        }
    }
    out
}

/// The closest pair of cells (Manhattan distance) between two sets —
/// O(len(a) * len(b)), fine for a one-time generation step at level-sized
/// cell counts (a handful of splits, at most `MAX_COMPOSITE_DEPTH` deep).
fn nearest_pair(a: &[Cell], b: &[Cell]) -> Option<(Cell, Cell)> {
    let mut best: Option<(Cell, Cell, i32)> = None;
    for &pa in a {
        for &pb in b {
            let dist = (pa.0 - pb.0).abs() + (pa.1 - pb.1).abs();
            if best.is_none_or(|(_, _, d)| dist < d) {
                best = Some((pa, pb, dist));
            }
        }
    }
    best.map(|(pa, pb, _)| (pa, pb))
}

/// Carve a straight L-shaped `Floor` corridor between two arbitrary
/// cells — the same shape `carve::connect_centers` already uses for
/// within-leaf room connections, generalized to take endpoints directly
/// rather than deriving them from room rects.
fn connect_cells(a: Cell, b: Cell, cells: &mut BTreeMap<Cell, LevelCellKind>) {
    for x in a.0.min(b.0)..=a.0.max(b.0) {
        cells.insert(Cell(x, a.1), LevelCellKind::Floor);
    }
    for y in a.1.min(b.1)..=a.1.max(b.1) {
        cells.insert(Cell(b.0, y), LevelCellKind::Floor);
    }
}

/// Post-order walk of the partition tree: connect each `Split`'s two
/// children to each other, after first recursing into both — so by the
/// time a split connects its own two sides, each side is already fully
/// connected internally (by induction), and joining any one point from
/// each side joins the whole subtrees. This is the fix for the gap named
/// in this task's own header: `region::cut` leaves a permanent wall gap
/// between siblings, and nothing else in this module ever carves through
/// it.
fn connect_split_boundaries(region: &region::Region, cells: &mut BTreeMap<Cell, LevelCellKind>) {
    if let region::Region::Split(a, b) = region {
        connect_split_boundaries(a, cells);
        connect_split_boundaries(b, cells);
        let a_cells = walkable_cells_in(a, cells);
        let b_cells = walkable_cells_in(b, cells);
        if let Some((pa, pb)) = nearest_pair(&a_cells, &b_cells) {
            connect_cells(pa, pb, cells);
        }
    }
}

/// Generate a level over `extent`: build the partition tree, then carve each
/// leaf's interior with one content generator over a rock background.
///
/// **Delegates to `generate_level_with_origin`** with a fixed default
/// kind/origin — this simple entry point's own callers are the only thing
/// that default matters for; Task 8's integration tests exercise real
/// `Chamber` values through `generate_level_with_origin` directly.
pub fn generate_level(extent: Rect, seed: Seed) -> Level {
    generate_level_with_origin(
        extent,
        hornvale_terrain::CaveKind::Karst,
        hornvale_worldgen::chamber::ChamberOrigin::Found,
        NEUTRAL_WORKED_BIAS,
        seed,
    )
}

/// As `generate_level_with_origin`, additionally carving a flooded basin
/// when `is_sump` says this chamber is phreatic. Reuses
/// `hornvale_worldgen::chamber::is_sump` directly — a chamber's depth is a
/// single scalar (spec §4.4), so this answers a per-chamber question, not
/// a per-cell one, and the basin is the first leaf in generation order
/// rather than a further seeded choice.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(diagnostic-value: water_table_m), bare-ok(ratio: inherited_worked_bias)
pub fn generate_level_with_water(
    extent: Rect,
    cave_kind: hornvale_terrain::CaveKind,
    origin: hornvale_worldgen::chamber::ChamberOrigin,
    depth_m: f64,
    water_table_m: f64,
    inherited_worked_bias: f64,
    seed: Seed,
) -> Level {
    let mut level =
        generate_level_with_origin(extent, cave_kind, origin, inherited_worked_bias, seed);
    if hornvale_worldgen::chamber::is_sump(origin, depth_m, water_table_m)
        && let Some(basin) = region_first_leaf_rect(extent, seed)
    {
        for x in basin.x..(basin.x + basin.w) {
            for y in basin.y..(basin.y + basin.h) {
                let cell = Cell(x, y);
                if level.cells.get(&cell) == Some(&LevelCellKind::Floor) {
                    level.cells.insert(cell, LevelCellKind::Flooded);
                }
            }
        }
    }
    level
}

/// The same partition tree `generate_level_with_origin` already built,
/// re-derived (not stored) so the flooding pass can find "the first leaf"
/// without threading the tree itself through every function above it —
/// `FRAME`-tier re-derivation is exactly what decision 0069 calls for.
fn region_first_leaf_rect(extent: Rect, seed: Seed) -> Option<Rect> {
    let (tree, _dof) = region::build_region(extent, seed);
    region::leaves(&tree).into_iter().next()
}

/// The realized worked-fraction of a level's leaves — Task 6's own
/// continuation signal.
fn realized_worked_fraction(level: &Level) -> f64 {
    if level.leaf_styles.is_empty() {
        return NEUTRAL_WORKED_BIAS;
    }
    let worked = level.leaf_styles.iter().filter(|s| s.worked).count();
    worked as f64 / level.leaf_styles.len() as f64
}

/// Place a stairs-down cell in the first leaf and, if `has_up`, a
/// stairs-up cell in the last leaf — deterministic picks off the same
/// partition tree `generate_level_with_water` already built, matching
/// `region_first_leaf_rect`'s re-derivation pattern.
///
/// **Inlines `build_region`/`leaves()` rather than calling
/// `region_first_leaf_rect`**, deliberately: that helper only returns the
/// *first* leaf, and this function also needs the *last* one for
/// `StairsUp`, so reusing it would mean a second `build_region` call over
/// the same extent/seed for no benefit — this way the tree is built once.
///
/// **Order matters for the single-leaf case.** When the partition tree has
/// exactly one leaf, `first == last`, and both stairs are searched for in
/// the same rect. The down-stairs write below happens first and mutates
/// `level.cells` in place, so the up-stairs search below it reads that
/// mutation live — `first_walkable_cell` no longer matches the just-placed
/// `StairsDown` cell against `Floor`/`Flooded`, so it lands on a *different*
/// walkable cell (or, correctly, none, if the leaf had only one). Do not
/// refactor `first_walkable_cell` to read a cached/snapshotted cell state:
/// that would let the up-stairs silently overwrite the down-stairs (or vice
/// versa) on any single-leaf rung whose leaf has exactly one walkable cell.
/// Pinned by `stairs_down_and_stairs_up_never_share_a_cell` below.
fn place_connections(level: &mut Level, extent: Rect, has_up: bool, seed: Seed) {
    let (tree, _dof) = region::build_region(extent, seed);
    let leaf_rects = region::leaves(&tree);
    if let Some(&first) = leaf_rects.first()
        && let Some(cell) = first_walkable_cell(level, first)
    {
        level.cells.insert(cell, LevelCellKind::StairsDown);
    }
    if has_up
        && let Some(&last) = leaf_rects.last()
        && let Some(cell) = first_walkable_cell(level, last)
    {
        level.cells.insert(cell, LevelCellKind::StairsUp);
    }
}

/// The first `Floor`/`Flooded` cell found in `rect`, in column-major order
/// within the rectangle (x outer, y inner) — `place_connections`' own
/// search for somewhere standable to put a stairs cell.
fn first_walkable_cell(level: &Level, rect: Rect) -> Option<Cell> {
    for x in rect.x..(rect.x + rect.w) {
        for y in rect.y..(rect.y + rect.h) {
            let cell = Cell(x, y);
            if matches!(
                level.cells.get(&cell),
                Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
            ) {
                return Some(cell);
            }
        }
    }
    None
}

/// Generate every rung of one descent under one entrance (spec §4.5).
/// `CaveKind` is fixed for the whole descent (a cave system has one kind —
/// see the spec's §4.5 correction); `origins`/`depths_m` vary per rung,
/// parallel to `rungs`. Each rung's realized worked-fraction becomes the
/// next rung's inherited bias, so a whole descent can read as uniformly
/// natural, uniformly worked, or genuinely transitioning — not
/// independently re-rolled at every rung.
///
/// **Draws a fresh per-rung `Seed` from `UNDERWORLD_LEVEL_DESCENT`, once
/// per rung, rather than passing the same top-level `seed` to every rung's
/// `generate_level_with_water` call.** Every stream a level's own
/// generation derives (`UNDERWORLD_LEVEL_PARTITION`, `_STYLE`, `_CELLULAR`,
/// `_TUNNELER`, `_ROOMS`) is derived fresh from whatever `Seed` it's given
/// — so two rungs handed the identical `seed` would restart their own
/// generation from identical stream state and produce correlated,
/// near-duplicate shapes, the same defect class `carve`'s per-leaf fix
/// (this file, Task 3/4) exists to prevent, one level up. A drawn `u64`
/// re-wrapped as `Seed(..)` is fully reproducible (still a pure function
/// of the original `seed`) without needing `generate_level_with_origin`'s
/// whole call chain restructured to thread a persistent stream across
/// rungs the way it already does across leaves within one level.
/// type-audit: bare-ok(diagnostic-value: depths_m), bare-ok(diagnostic-value: water_table_m)
pub fn generate_descent(
    rungs: &[hornvale_terrain::DelveRung],
    cave_kind: hornvale_terrain::CaveKind,
    origins: &[hornvale_worldgen::chamber::ChamberOrigin],
    depths_m: &[f64],
    water_table_m: f64,
    seed: Seed,
) -> Vec<Level> {
    assert_eq!(rungs.len(), origins.len(), "one origin per rung");
    assert_eq!(rungs.len(), depths_m.len(), "one depth per rung");
    let mut descent_stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_DESCENT)
        .stream();
    let mut bias = NEUTRAL_WORKED_BIAS;
    let mut levels = Vec::with_capacity(rungs.len());
    for (i, &rung) in rungs.iter().enumerate() {
        let extent = generate_level_extent(rung);
        let rung_seed = Seed(descent_stream.next_u64());
        let mut level = generate_level_with_water(
            extent,
            cave_kind,
            origins[i],
            depths_m[i],
            water_table_m,
            bias,
            rung_seed,
        );
        place_connections(&mut level, extent, i > 0, rung_seed);
        bias = realized_worked_fraction(&level);
        levels.push(level);
    }
    levels
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generation_is_deterministic() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 20,
            h: 12,
        };
        let a = generate_level(extent, hornvale_kernel::Seed(42));
        let b = generate_level(extent, hornvale_kernel::Seed(42));
        assert_eq!(a, b, "same seed must produce byte-identical levels");
    }

    #[test]
    fn every_cell_of_the_extent_has_a_kind() {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 20,
            h: 12,
        };
        let level = generate_level(extent, hornvale_kernel::Seed(1));
        for x in extent.x..(extent.x + extent.w) {
            for y in extent.y..(extent.y + extent.h) {
                assert!(
                    level.cells.contains_key(&Cell(x, y)),
                    "cell ({x}, {y}) missing from a total level"
                );
            }
        }
        assert_eq!(
            level.cells.len(),
            (extent.w * extent.h) as usize,
            "no cell outside the extent"
        );
    }

    /// claim: rate(seed: 0..200) — the WORKED-FRACTION across a 200-seed
    /// sweep is compared between `Made` and `Found` origins, a statistical
    /// mean-property claim, not a per-seed-without-exception invariant.
    #[test]
    fn made_chambers_lean_worked_found_chambers_lean_natural() {
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let mut made_worked = 0;
        let mut found_worked = 0;
        const TRIALS: u64 = 200;
        for s in 0..TRIALS {
            let extent = Rect {
                x: 0,
                y: 0,
                w: 40,
                h: 24,
            };
            let made = generate_level_with_origin(
                extent,
                CaveKind::Karst,
                ChamberOrigin::Made,
                NEUTRAL_WORKED_BIAS,
                Seed(s),
            );
            let found = generate_level_with_origin(
                extent,
                CaveKind::Karst,
                ChamberOrigin::Found,
                NEUTRAL_WORKED_BIAS,
                Seed(s),
            );
            if made.leaf_styles.iter().any(|s| s.worked) {
                made_worked += 1;
            }
            if found.leaf_styles.iter().any(|s| s.worked) {
                found_worked += 1;
            }
        }
        assert!(
            made_worked > found_worked,
            "made ({made_worked}/{TRIALS}) should read worked more often than found ({found_worked}/{TRIALS})"
        );
    }

    /// claim: rate(seed: single) — not a sweep at all; this asserts a
    /// design invariant (that `ChamberOrigin` is `Copy` and untouched by
    /// the call) via a single representative seed, so the seed-shaped
    /// binding in the caller's own detection is incidental rather than a
    /// statistical claim over a range.
    #[test]
    fn chamber_origin_is_never_mutated_by_geometry() {
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let extent = Rect {
            x: 0,
            y: 0,
            w: 40,
            h: 24,
        };
        let origin = ChamberOrigin::Found;
        let _ = generate_level_with_origin(
            extent,
            CaveKind::Fracture,
            origin,
            NEUTRAL_WORKED_BIAS,
            Seed(4),
        );
        // `origin` is Copy and untouched by the call above — this test
        // exists to assert the DESIGN, not the runtime: the function takes
        // `origin` by value and returns nothing that could feed back into
        // it, so ChamberOrigin's own ecology contract (is_sump's dryness
        // rule, spec §4.3) cannot be perturbed by this module. Compiling is
        // the proof.
        assert_eq!(origin, ChamberOrigin::Found);
    }

    #[test]
    fn a_sump_gets_a_flooded_region_a_made_chamber_never_does() {
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;

        let extent = Rect {
            x: 0,
            y: 0,
            w: 40,
            h: 24,
        };
        // depth_m > water_table_m => phreatic (is_phreatic's own contract).
        let sump = generate_level_with_water(
            extent,
            CaveKind::Karst,
            ChamberOrigin::Found,
            100.0,
            10.0,
            NEUTRAL_WORKED_BIAS,
            Seed(2),
        );
        assert!(
            sump.cells.values().any(|k| *k == LevelCellKind::Flooded),
            "a phreatic Found chamber must carve a flooded region"
        );

        let made = generate_level_with_water(
            extent,
            CaveKind::Karst,
            ChamberOrigin::Made,
            100.0,
            10.0,
            NEUTRAL_WORKED_BIAS,
            Seed(2),
        );
        assert!(
            made.cells.values().all(|k| *k != LevelCellKind::Flooded),
            "a Made chamber is drained regardless of the water table (is_sump's own rule)"
        );

        let dry = generate_level_with_water(
            extent,
            CaveKind::Karst,
            ChamberOrigin::Found,
            5.0,
            10.0,
            NEUTRAL_WORKED_BIAS,
            Seed(2),
        );
        assert!(
            dry.cells.values().all(|k| *k != LevelCellKind::Flooded),
            "a vadose Found chamber (above the water table) stays dry"
        );
    }

    /// claim: rate(seed: 0..100) — the deepest rung's mean worked-fraction
    /// across a 100-seed sweep is compared against the neutral baseline, a
    /// statistical mean-property claim (compounding inertia), not a
    /// per-seed-without-exception invariant.
    #[test]
    fn worked_fraction_has_inertia_across_rungs() {
        use hornvale_terrain::{CaveKind, DelveRung};
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [DelveRung::Undercroft, DelveRung::Shallows, DelveRung::Deeps];
        // All three rungs Made: with inertia, later rungs' worked fraction
        // should not regress toward NEUTRAL_WORKED_BIAS as hard as an
        // independent re-roll would — measured as "the deepest rung's worked
        // fraction, averaged over many seeds, exceeds the neutral baseline".
        const TRIALS: u64 = 100;
        let mut deepest_worked_total = 0.0;
        for s in 0..TRIALS {
            let origins = [
                ChamberOrigin::Made,
                ChamberOrigin::Made,
                ChamberOrigin::Made,
            ];
            let depths_m = [20.0, 60.0, 120.0];
            let levels =
                generate_descent(&rungs, CaveKind::Karst, &origins, &depths_m, 500.0, Seed(s));
            let deepest = levels.last().expect("three rungs requested");
            let worked = deepest.leaf_styles.iter().filter(|s| s.worked).count() as f64;
            let total = deepest.leaf_styles.len().max(1) as f64;
            deepest_worked_total += worked / total;
        }
        let mean_worked_fraction = deepest_worked_total / TRIALS as f64;
        assert!(
            mean_worked_fraction > NEUTRAL_WORKED_BIAS,
            "three Made rungs should compound toward worked, not float at neutral: got {mean_worked_fraction}"
        );
    }

    /// claim: rate(seed: single) — asserts a level-count invariant at one
    /// representative seed, not a statistical claim over a range.
    #[test]
    fn generate_descent_produces_one_level_per_rung() {
        use hornvale_terrain::{CaveKind, DelveRung};
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [DelveRung::Undercroft, DelveRung::Shallows];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 60.0];
        let levels = generate_descent(
            &rungs,
            CaveKind::LavaTube,
            &origins,
            &depths_m,
            500.0,
            Seed(1),
        );
        assert_eq!(levels.len(), 2);
    }

    /// claim: invariant(seed: single) — asserts a connectivity invariant
    /// (every level down-connected, every level but the shallowest also
    /// up-connected) at one representative seed.
    #[test]
    fn every_level_but_the_first_has_stairs_up_every_level_has_stairs_down() {
        use hornvale_terrain::{CaveKind, DelveRung};
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [DelveRung::Undercroft, DelveRung::Shallows, DelveRung::Deeps];
        let origins = [ChamberOrigin::Found; 3];
        let depths_m = [20.0, 60.0, 120.0];
        let levels = generate_descent(
            &rungs,
            CaveKind::Fracture,
            &origins,
            &depths_m,
            500.0,
            Seed(6),
        );

        for (i, level) in levels.iter().enumerate() {
            let has_down = level
                .cells
                .values()
                .any(|k| *k == LevelCellKind::StairsDown);
            let has_up = level.cells.values().any(|k| *k == LevelCellKind::StairsUp);
            assert!(has_down, "level {i} is missing its stairs down");
            if i == 0 {
                assert!(
                    !has_up,
                    "the shallowest level must not have stairs up (it leads to Surface, not a generated level)"
                );
            } else {
                assert!(has_up, "level {i} is missing its stairs up");
            }
        }
    }

    /// claim: invariant(seed: 0..200) — down-stairs and up-stairs never
    /// share a cell, on any level of any seed in the sweep, including the
    /// common case (`region::split_probability(0) == 0.35`, so a
    /// no-split single-leaf partition is the majority outcome at depth 0)
    /// where the partition tree has exactly one leaf and both stairs are
    /// searched for in the very same rect. A prior review traced
    /// `place_connections` and confirmed this holds today because
    /// `first_walkable_cell` reads `level.cells`' LIVE state — the
    /// down-stairs write happens first and mutates the map, so the
    /// up-stairs search no longer matches that cell against
    /// `Floor`/`Flooded` — but nothing pinned it: reordering the two writes,
    /// or refactoring the walkable-cell search to read a cached/snapshotted
    /// state instead of live `level.cells`, could silently make one
    /// placement overwrite the other with no test catching it.
    ///
    /// Asserts THREE things, deliberately, not just non-collision: (1)
    /// every level has *exactly one* `StairsDown` cell — guaranteed by
    /// `carve::tests::every_algorithm_produces_at_least_one_floor_cell`'s
    /// own contract (every leaf gets >= 1 `Floor` cell, and the first leaf
    /// always exists), so this must never be zero; (2) every level but the
    /// first has *exactly one* `StairsUp` cell (the shallowest level, which
    /// has none, is checked separately against zero-or-one so the loop stays
    /// correct there too) — a `<=` bound here would silently accept a level
    /// missing its up-stairs entirely, the reversed-write-order twin of the
    /// down-stairs-overwritten defect assertion (1) exists to catch; (3)
    /// when both are present, they differ. (1) is the one that actually
    /// catches a collision that
    /// silently overwrites the down-stairs — a bare non-collision check
    /// (`assert_ne!` only, guarded by non-empty loops) passes vacuously
    /// when a collision empties one side's `Vec` via `BTreeMap` overwrite,
    /// which is exactly what happened when this was verified against a
    /// deliberately reintroduced cached-snapshot mutation of
    /// `place_connections` (both writes read a pre-mutation clone instead of
    /// live `level.cells`): the mutation compiled, every level still got a
    /// `StairsUp` cell, but `level 2`'s `StairsDown` cell vanished (silently
    /// overwritten by the `StairsUp` write to the same cell) — caught by
    /// assertion (1) here, and separately by the pre-existing
    /// `every_level_but_the_first_has_stairs_up_every_level_has_stairs_down`
    /// test at `Seed(6)`, which is not guaranteed to hit the single-leaf
    /// path on every future edit the way this sweep is. Also asserts the
    /// sweep actually exercises the single-leaf case (a positive control),
    /// so it cannot pass vacuously if the extent or seed range ever changes
    /// to avoid that path.
    #[test]
    fn stairs_down_and_stairs_up_never_share_a_cell() {
        use hornvale_terrain::{CaveKind, DelveRung};
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [DelveRung::Undercroft, DelveRung::Shallows];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 60.0];
        const TRIALS: u64 = 200;
        let mut single_leaf_levels_probed = 0;
        for s in 0..TRIALS {
            let levels = generate_descent(
                &rungs,
                CaveKind::Fracture,
                &origins,
                &depths_m,
                500.0,
                Seed(s),
            );
            for (i, level) in levels.iter().enumerate() {
                let down_cells: Vec<Cell> = level
                    .cells
                    .iter()
                    .filter(|(_, k)| **k == LevelCellKind::StairsDown)
                    .map(|(c, _)| *c)
                    .collect();
                let up_cells: Vec<Cell> = level
                    .cells
                    .iter()
                    .filter(|(_, k)| **k == LevelCellKind::StairsUp)
                    .map(|(c, _)| *c)
                    .collect();
                assert_eq!(
                    down_cells.len(),
                    1,
                    "seed {s} level {i}: expected exactly one StairsDown cell, found {down_cells:?}"
                );
                if i > 0 {
                    assert_eq!(
                        up_cells.len(),
                        1,
                        "seed {s} level {i}: expected exactly one StairsUp cell, found {up_cells:?}"
                    );
                } else {
                    assert!(
                        up_cells.len() <= 1,
                        "seed {s} level {i}: expected at most one StairsUp cell, found {up_cells:?}"
                    );
                }
                for down in &down_cells {
                    for up in &up_cells {
                        assert_ne!(
                            down, up,
                            "seed {s} level {i}: down-stairs and up-stairs share cell {down:?}"
                        );
                    }
                }
                if level.leaf_styles.len() == 1 && i > 0 {
                    single_leaf_levels_probed += 1;
                }
            }
        }
        assert!(
            single_leaf_levels_probed > 0,
            "this sweep never exercised the single-leaf (first-leaf-equals-last-leaf) \
             case both stairs must share a rect in — widen the seed range or rung \
             count so the test is not vacuous"
        );
    }

    /// claim: rate(seed: single) — asserts a determinism-vs-correlation
    /// invariant at one representative seed, not a statistical claim over a
    /// range.
    #[test]
    fn two_rungs_with_the_same_origin_and_kind_still_differ() {
        // The rung-level twin of carve.rs's "two leaves sharing a stream must
        // differ" regression. Repeats the SAME rung twice — an artificial but
        // precise probe: identical extent, origin, kind and depth for both
        // entries, so the only thing that could make the two produced levels
        // differ is which seed each one draws. Using two DIFFERENT rungs here
        // would be a weaker test: different rungs get different extents
        // (`generate_level_extent`), so their `cells` maps would have
        // different key sets and always compare unequal regardless of whether
        // the seed-reuse defect this test guards against is present.
        use hornvale_terrain::{CaveKind, DelveRung};
        use hornvale_worldgen::chamber::ChamberOrigin;

        let rungs = [DelveRung::Undercroft, DelveRung::Undercroft];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 20.0];
        let levels = generate_descent(
            &rungs,
            CaveKind::Karst,
            &origins,
            &depths_m,
            500.0,
            Seed(13),
        );
        assert_ne!(
            levels[0].cells, levels[1].cells,
            "two rungs must not produce the same cells even with matching origin/kind/depth"
        );
    }

    #[test]
    /// claim: invariant(seed: 0..20) — every generated level's walkable
    /// cells form exactly one connected component, for every seed in the
    /// range, swept across every `CaveKind` x `ChamberOrigin` combination.
    /// This is the property Task 2's original design silently failed to
    /// guarantee for any level with more than one leaf.
    ///
    /// **Swept over all 6 `(CaveKind, ChamberOrigin)` combinations**, not
    /// just `(Fracture, Found)` — a prior version of this sweep hardcoded
    /// that one pair, and `choose_leaf_style`'s `Found`-origin base chance
    /// (0.10) blended against the neutral 0.5 bias put every leaf on
    /// `AngularRooms`/`RoomsAndCorridors`, so `CellularCave` and `Tunneler`
    /// were never exercised here at all — exactly the blind spot that let
    /// Task 10's `CellularCave`-disconnected-caverns defect go uncaught
    /// until an unrelated integration test happened to hit it. Also asserts
    /// a positive control (`composite_levels_probed > 0`): the
    /// cross-leaf-connectivity property this sweep guards (Task 9) is only
    /// meaningfully tested if at least some generated levels actually have
    /// more than one leaf, mirroring the `single_leaf_levels_probed`
    /// positive control in `stairs_down_and_stairs_up_never_share_a_cell`
    /// above.
    fn every_walkable_cell_is_reachable_from_every_other() {
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;
        use std::collections::{BTreeSet, VecDeque};

        let mut composite_levels_probed = 0;
        for cave_kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            for origin in [ChamberOrigin::Found, ChamberOrigin::Made] {
                for seed_value in 0..20u64 {
                    let extent = Rect {
                        x: 0,
                        y: 0,
                        w: 40,
                        h: 24,
                    };
                    let level = generate_level_with_origin(
                        extent,
                        cave_kind,
                        origin,
                        NEUTRAL_WORKED_BIAS,
                        Seed(seed_value),
                    );
                    if level.leaf_styles.len() > 1 {
                        composite_levels_probed += 1;
                    }
                    let walkable: BTreeSet<Cell> = level
                        .cells
                        .iter()
                        .filter(|(_, k)| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded))
                        .map(|(&c, _)| c)
                        .collect();
                    let Some(&start) = walkable.iter().next() else {
                        continue; // a degenerate all-wall level has nothing to check
                    };
                    let mut seen = BTreeSet::new();
                    let mut queue = VecDeque::new();
                    seen.insert(start);
                    queue.push_back(start);
                    while let Some(Cell(x, y)) = queue.pop_front() {
                        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                            let next = Cell(x + dx, y + dy);
                            if walkable.contains(&next) && seen.insert(next) {
                                queue.push_back(next);
                            }
                        }
                    }
                    assert_eq!(
                        seen.len(),
                        walkable.len(),
                        "{cave_kind:?}/{origin:?} seed {seed_value}: {} of {} walkable cells unreachable from {start:?}",
                        walkable.len() - seen.len(),
                        walkable.len()
                    );
                }
            }
        }
        assert!(
            composite_levels_probed > 0,
            "this sweep never generated a level with more than one leaf — the \
             cross-leaf connectivity property (Task 9) this test guards would \
             pass vacuously"
        );
    }
}
