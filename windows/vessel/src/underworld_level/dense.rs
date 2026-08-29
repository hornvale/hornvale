//! Dense storage for a level's cells (Task 1 of The Gallery: a pure
//! performance refactor, byte-identical by construction).
//!
//! `Level.cells`'s own doc says it is TOTAL over the level's extent — every
//! cell of the extent has an entry, always. A `BTreeMap<Cell, _>` paid an
//! O(log N) tree traversal on every read and write of that total index (The
//! Lookup measured a dense-keyed `BTreeMap` at ~22% of genesis self-time; a
//! `Vec` dropped it to ~2% — see `kernel/CLAUDE.md`). `CellGrid` replaces the
//! map with a flat `Vec`, indexed directly rather than searched.
//!
//! **Iteration order matches `BTreeMap<Cell, _>`'s exactly, by construction,
//! not by sorting.** `Cell(pub i32, pub i32)`'s derived `Ord` compares field
//! 0 (`x`) before field 1 (`y`), so a `BTreeMap<Cell, _>` iterates in
//! ascending `(x, y)` order. The backing store here is indexed
//! `(x - extent.x) * extent.h + (y - extent.y)`, so walking it front to back
//! visits every `y` for a fixed `x` before advancing `x` — ascending `(x,
//! y)`, for free.

use crate::lattice::{Cell, Rect};
use crate::underworld_level::LevelCellKind;

/// A level's cells, densely stored: exactly one [`LevelCellKind`] per cell
/// of `extent`, indexed directly rather than looked up in a tree map.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CellGrid {
    /// The grid's bounds. Every cell within this rectangle has an entry;
    /// nothing outside it does.
    extent: Rect,
    /// One entry per cell of `extent`, laid out in `(x - extent.x) *
    /// extent.h + (y - extent.y)` order — ascending `(x, y)`, matching
    /// `BTreeMap<Cell, _>`'s own order exactly (see the module doc).
    cells: Vec<LevelCellKind>,
}

impl CellGrid {
    /// A new grid over `extent`, every cell initialized to `fill`.
    pub fn new(extent: Rect, fill: LevelCellKind) -> CellGrid {
        let len = (extent.w.max(0) as usize).saturating_mul(extent.h.max(0) as usize);
        CellGrid {
            extent,
            cells: vec![fill; len],
        }
    }

    /// `c`'s index into the backing store, or `None` if `c` lies outside
    /// `extent`.
    fn index_of(&self, c: Cell) -> Option<usize> {
        if !self.extent.contains(c) {
            return None;
        }
        let dx = (c.0 - self.extent.x) as usize;
        let dy = (c.1 - self.extent.y) as usize;
        Some(dx * self.extent.h as usize + dy)
    }

    /// `c`'s kind. `None` if and only if `c` lies outside the extent.
    pub fn get(&self, c: Cell) -> Option<LevelCellKind> {
        self.index_of(c).map(|i| self.cells[i])
    }

    /// Set `c`'s kind. A no-op if `c` lies outside the extent.
    pub fn set(&mut self, c: Cell, k: LevelCellKind) {
        if let Some(i) = self.index_of(c) {
            self.cells[i] = k;
        }
    }

    /// Every cell of the extent with its kind, in ascending `(x, y)` order —
    /// exactly the order a `BTreeMap<Cell, _>` over the same cells would
    /// give (see the module doc).
    pub fn iter(&self) -> impl Iterator<Item = (Cell, LevelCellKind)> + '_ {
        let h = self.extent.h.max(0);
        let x0 = self.extent.x;
        let y0 = self.extent.y;
        self.cells.iter().enumerate().map(move |(i, &k)| {
            let i = i as i32;
            let dx = if h == 0 { 0 } else { i / h };
            let dy = if h == 0 { 0 } else { i % h };
            (Cell(x0 + dx, y0 + dy), k)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_dense_grid_agrees_with_a_btreemap_on_every_cell_and_on_order() {
        let extent = Rect {
            x: -3,
            y: 7,
            w: 11,
            h: 5,
        };
        let mut grid = CellGrid::new(extent, LevelCellKind::Wall);
        let mut map: std::collections::BTreeMap<Cell, LevelCellKind> =
            std::collections::BTreeMap::new();
        for x in extent.x..(extent.x + extent.w) {
            for y in extent.y..(extent.y + extent.h) {
                map.insert(Cell(x, y), LevelCellKind::Wall);
            }
        }
        let writes = [
            (0, 0, LevelCellKind::Floor),
            (10, 4, LevelCellKind::Flooded),
            (5, 2, LevelCellKind::StairsUp),
            (1, 3, LevelCellKind::StairsDown),
        ];
        for (dx, dy, k) in writes {
            let c = Cell(extent.x + dx, extent.y + dy);
            grid.set(c, k);
            map.insert(c, k);
        }
        let from_grid: Vec<(Cell, LevelCellKind)> = grid.iter().collect();
        let from_map: Vec<(Cell, LevelCellKind)> = map.iter().map(|(&c, &k)| (c, k)).collect();
        assert_eq!(
            from_grid, from_map,
            "dense iteration must match BTreeMap order exactly"
        );
        assert_eq!(
            grid.get(Cell(extent.x - 1, extent.y)),
            None,
            "outside the extent is None"
        );
        assert_eq!(
            grid.get(Cell(extent.x, extent.y)),
            Some(LevelCellKind::Floor)
        );
    }
}
