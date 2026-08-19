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

/// Generate a level over `extent`. **Task 1 only**: fills everything as a
/// single bordered floor. Task 2 replaces this body with the real
/// partition-tree scaffold; the signature and name are the ones every
/// later task builds on.
pub fn generate_level(extent: Rect, seed: Seed) -> Level {
    let mut stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_PLACEHOLDER)
        .stream();
    let mut dof: u32 = 0;
    // One draw, discarded, so the label is genuinely exercised rather than
    // dead — Task 2 replaces this with real consumption.
    let _ = stream.next_u64();
    dof += 1;

    let mut cells = BTreeMap::new();
    for x in extent.x..(extent.x + extent.w) {
        for y in extent.y..(extent.y + extent.h) {
            let border = x == extent.x
                || y == extent.y
                || x == extent.x + extent.w - 1
                || y == extent.y + extent.h - 1;
            let kind = if border {
                LevelCellKind::Wall
            } else {
                LevelCellKind::Floor
            };
            cells.insert(Cell(x, y), kind);
        }
    }
    Level { extent, cells, dof }
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
}
