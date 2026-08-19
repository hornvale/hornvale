//! Underworld level generation: a chamber's own shape (The Adit).
//!
//! A chamber is a bucket, not a place (spec keystone) — `ChamberAddr`
//! addresses which of up to `SLOTS_PER_BAND` interchangeable habitats
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
    Level {
        extent,
        cells,
        dof,
        leaf_styles,
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
}
