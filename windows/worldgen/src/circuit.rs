//! The descent plan (The Crosscut, The Circuit campaign 1): **a place is a
//! graph before it is a map, and the graph is series-parallel.**
//!
//! A [`DescentPlan`] is grown for the five-rung descent `Underground::enter`
//! (`windows/vessel/src/underground.rs`) walks under one vertex, before any
//! level is carved. Nodes are grid regions per level, edges are same-floor
//! passages or stairways whose two ends share one coordinate, and every
//! cycle is a [`Realm`] recorded with its two paths and Dormans' length
//! class. Growth uses exactly two operations — series (`extend`) and
//! parallel (`cycle`) — so planarity, reachability and one-innermost-realm
//! hold by construction (spec §3.4).
//!
//! FRAME-tier (decision 0069): derived from `(seed, vertex)` on every call,
//! never serialized. Density is DERIVED from rock and workmanship
//! (`cycle_budget`); the `[1, 5]` clip is the one authored constant and is
//! Dormans' (spec §3.2 step 5).

use hornvale_kernel::Band;

/// A region's span in cells, either axis — the Adit's `MIN_REGION_SPAN`,
/// now the grid pitch. The grid is `w / REGION_SPAN` by `h / REGION_SPAN`.
/// type-audit: bare-ok(count: REGION_SPAN)
pub const REGION_SPAN: i32 = 8;
/// Dormans' clutter ceiling ("two to five cycles give each level a distinct
/// and recognizable shape"). The one authored constant in the grammar.
/// type-audit: bare-ok(count: MAX_CYCLES_PER_LEVEL)
pub const MAX_CYCLES_PER_LEVEL: u8 = 5;
/// The floor of the same clip: every level gets at least one loop.
/// type-audit: bare-ok(count: MIN_CYCLES_PER_LEVEL)
pub const MIN_CYCLES_PER_LEVEL: u8 = 1;

/// Level width before rank scaling (moved verbatim from
/// `windows/vessel/src/underworld_level/mod.rs`'s `BASE_LEVEL_W`).
const BASE_LEVEL_W: i32 = 40;
/// See `BASE_LEVEL_W`.
const BASE_LEVEL_H: i32 = 24;

/// The extent a level of `rung` gets, `(w, h)` in cells: deeper rungs get
/// more room. The formula `generate_level_extent` in `windows/vessel` used
/// to own; it now delegates here so the plan and the realizer agree.
/// type-audit: bare-ok(count: return)
pub fn level_extent_wh(rung: Band) -> (i32, i32) {
    let rank = hornvale_terrain::rungs()
        .iter()
        .position(|r| *r == rung)
        .unwrap_or(0) as i32;
    (BASE_LEVEL_W + 4 * rank, BASE_LEVEL_H + 2 * rank)
}

/// How many region columns and rows a level of a rung has.
/// type-audit: bare-ok(count: cols), bare-ok(count: rows)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GridDims {
    /// Region columns.
    pub cols: u8,
    /// Region rows.
    pub rows: u8,
}

/// The region grid of a rung: a DERIVED shape of the extent, never an
/// authored count (spec §3.1).
pub fn grid_dims(rung: Band) -> GridDims {
    let (w, h) = level_extent_wh(rung);
    GridDims {
        cols: (w / REGION_SPAN) as u8,
        rows: (h / REGION_SPAN) as u8,
    }
}

/// One region's position in a level's grid.
/// type-audit: bare-ok(index: col), bare-ok(index: row)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GridCell {
    /// Column, `0..cols`.
    pub col: u8,
    /// Row, `0..rows`.
    pub row: u8,
}

/// A region's carveable rectangle in level cells. `worldgen` cannot name
/// `vessel`'s `Rect`, so this is the plan's own; the realizer converts.
/// type-audit: bare-ok(count: x), bare-ok(count: y), bare-ok(count: w), bare-ok(count: h)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegionRect {
    /// Left edge, in cells.
    pub x: i32,
    /// Top edge, in cells.
    pub y: i32,
    /// Width in cells.
    pub w: i32,
    /// Height in cells.
    pub h: i32,
}

impl RegionRect {
    /// The overlap of two rectangles, or `None` if they do not overlap.
    pub fn intersect(&self, other: &RegionRect) -> Option<RegionRect> {
        let x0 = self.x.max(other.x);
        let y0 = self.y.max(other.y);
        let x1 = (self.x + self.w).min(other.x + other.w);
        let y1 = (self.y + self.h).min(other.y + other.h);
        (x1 > x0 && y1 > y0).then_some(RegionRect {
            x: x0,
            y: y0,
            w: x1 - x0,
            h: y1 - y0,
        })
    }
}

/// The rectangle a grid cell owns, minus its right and bottom cell, which
/// stay wall — the same one-cell dividing wall `region::cut` left between
/// siblings (spec §3.3). The extent's last column and row are therefore
/// always wall too.
pub fn region_rect(rung: Band, cell: GridCell) -> RegionRect {
    let (w, h) = level_extent_wh(rung);
    let d = grid_dims(rung);
    let x0 = cell.col as i32 * w / d.cols as i32;
    let x1 = (cell.col as i32 + 1) * w / d.cols as i32;
    let y0 = cell.row as i32 * h / d.rows as i32;
    let y1 = (cell.row as i32 + 1) * h / d.rows as i32;
    RegionRect {
        x: x0,
        y: y0,
        w: x1 - x0 - 1,
        h: y1 - y0 - 1,
    }
}

/// Index of a [`Node`] in [`DescentPlan::nodes`].
/// type-audit: bare-ok(index: NodeId)
pub type NodeId = usize;
/// Index of a [`Realm`] in [`DescentPlan::realms`].
/// type-audit: bare-ok(index: RealmId)
pub type RealmId = usize;

/// One region of one level.
/// type-audit: bare-ok(index: level), bare-ok(count: depth)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Node {
    /// Which level (index into [`DescentPlan::rungs`]).
    pub level: u8,
    /// Which grid cell of that level.
    pub cell: GridCell,
    /// Hops from the entrance along a shortest path — the gradient The Plat
    /// reads as intimacy and The Brattice as danger.
    pub depth: u16,
    /// The innermost cycle holding this node; `None` for a spine node on no
    /// cycle.
    pub realm: Option<RealmId>,
}

/// How two nodes connect.
/// type-audit: bare-ok(count: Stair.x), bare-ok(count: Stair.y)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EdgeKind {
    /// A same-level passage between grid-adjacent regions.
    Passage,
    /// A stairway between the same grid cell on adjacent levels; both ends
    /// stand on cell `(x, y)` of their level (spec §3.3).
    Stair {
        /// Shared column coordinate.
        x: i32,
        /// Shared row coordinate.
        y: i32,
    },
}

/// One connection. For a `Stair`, `a` is the upper node and `b` the lower.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Edge {
    /// One end.
    pub a: NodeId,
    /// The other end.
    pub b: NodeId,
    /// Passage or stairway.
    pub kind: EdgeKind,
}

/// Dormans' four cycle classes by relative path length (Fig. 9.8).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LengthClass {
    /// Both paths long and within one of each other.
    LongLong,
    /// The existing segment is the long way round.
    LongShort,
    /// The new path is the long way round.
    ShortLong,
    /// Both short and within one of each other.
    ShortShort,
}

/// A cycle: two node-disjoint paths between the same two nodes.
/// type-audit: bare-ok(index: anchor_level)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Realm {
    /// The realm whose path this one was cut from, if nested.
    pub parent: Option<RealmId>,
    /// The level of `path_a`; the level whose budget this realm counts to.
    pub anchor_level: u8,
    /// The pre-existing segment, endpoints included.
    pub path_a: Vec<NodeId>,
    /// The new path, endpoints included (shared with `path_a`).
    pub path_b: Vec<NodeId>,
    /// Dormans' class from the two lengths.
    pub class: LengthClass,
}

/// The plan for one descent: a series-parallel graph over grid regions.
/// type-audit: bare-ok(count: dof)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DescentPlan {
    /// The rungs, shallowest first; `level ℓ` is `rungs[ℓ]`.
    pub rungs: Vec<Band>,
    /// Every region.
    pub nodes: Vec<Node>,
    /// Every connection.
    pub edges: Vec<Edge>,
    /// Where the descent is entered (level 0, west edge).
    pub entrance: NodeId,
    /// The deepest level's terminus, carrying today's dangling stairs down.
    pub terminus: NodeId,
    /// Every cycle, in creation order (a nested realm follows its parent).
    pub realms: Vec<Realm>,
    /// Draws made while growing, counted at each draw.
    pub dof: u32,
}

impl DescentPlan {
    /// Every node on `level`, ascending id.
    /// type-audit: bare-ok(index: level)
    pub fn nodes_on(&self, level: usize) -> Vec<NodeId> {
        (0..self.nodes.len())
            .filter(|&i| self.nodes[i].level as usize == level)
            .collect()
    }
    /// Every passage on `level`.
    /// type-audit: bare-ok(index: level)
    pub fn passages_on(&self, level: usize) -> Vec<(NodeId, NodeId)> {
        self.edges
            .iter()
            .filter(|e| e.kind == EdgeKind::Passage && self.nodes[e.a].level as usize == level)
            .map(|e| (e.a, e.b))
            .collect()
    }
    /// Every stairway whose UPPER end is on `level`: `(upper, lower, x, y)`.
    /// type-audit: bare-ok(index: level), bare-ok(count: return)
    pub fn stairs_from(&self, level: usize) -> Vec<(NodeId, NodeId, i32, i32)> {
        self.edges
            .iter()
            .filter_map(|e| match e.kind {
                EdgeKind::Stair { x, y } if self.nodes[e.a].level as usize == level => {
                    Some((e.a, e.b, x, y))
                }
                _ => None,
            })
            .collect()
    }
    /// Every stairway whose LOWER end is on `level`: `(upper, lower, x, y)`.
    /// type-audit: bare-ok(index: level), bare-ok(count: return)
    pub fn stairs_into(&self, level: usize) -> Vec<(NodeId, NodeId, i32, i32)> {
        self.edges
            .iter()
            .filter_map(|e| match e.kind {
                EdgeKind::Stair { x, y } if self.nodes[e.b].level as usize == level => {
                    Some((e.a, e.b, x, y))
                }
                _ => None,
            })
            .collect()
    }
    /// The carveable rectangle of `node`'s region.
    pub fn region_of(&self, node: NodeId) -> RegionRect {
        let n = self.nodes[node];
        region_rect(self.rungs[n.level as usize], n.cell)
    }
    /// Every node one edge away from `node`, ascending id.
    pub fn neighbours(&self, node: NodeId) -> Vec<NodeId> {
        let mut out: Vec<NodeId> = self
            .edges
            .iter()
            .filter_map(|e| {
                if e.a == node {
                    Some(e.b)
                } else if e.b == node {
                    Some(e.a)
                } else {
                    None
                }
            })
            .collect();
        out.sort_unstable();
        out.dedup();
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Band;

    /// claim: invariant(rung: every habitation rung) — the rank-zero grid is
    /// asserted directly; every other rung is swept by the two tests below.
    #[test]
    fn the_rank_zero_grid_is_five_by_three() {
        let d = grid_dims(Band::Undercroft);
        assert_eq!((d.cols, d.rows), (5, 3));
    }

    /// claim: invariant(rung: every habitation rung, grid cell: every column
    /// and row of that rung's grid) — region rects must tile the level
    /// extent with a one-cell wall on every side and between every pair of
    /// cells, for EVERY rung and EVERY cell, not merely a spot-checked one.
    #[test]
    fn region_rects_tile_the_extent_with_one_cell_walls() {
        for &rung in hornvale_terrain::rungs()
            .iter()
            .filter(|r| **r != Band::Surface)
        {
            let (w, h) = level_extent_wh(rung);
            let d = grid_dims(rung);
            let mut claimed = std::collections::BTreeSet::new();
            for col in 0..d.cols {
                for row in 0..d.rows {
                    let r = region_rect(rung, GridCell { col, row });
                    assert!(
                        r.w >= 1 && r.h >= 1,
                        "{rung:?} ({col},{row}) degenerate {r:?}"
                    );
                    assert!(
                        r.x >= 0 && r.y >= 0 && r.x + r.w < w && r.y + r.h < h,
                        "{rung:?} ({col},{row}) leaves no wall: {r:?} in {w}x{h}"
                    );
                    for x in r.x..r.x + r.w {
                        for y in r.y..r.y + r.h {
                            assert!(
                                claimed.insert((x, y)),
                                "{rung:?} cell ({x},{y}) claimed twice"
                            );
                        }
                    }
                }
            }
        }
    }

    /// claim: invariant(rung: every adjacent rung pair, grid cell: every
    /// shared column and row of both rungs' grids) — spec §3.3: a stairway's
    /// two ends share a coordinate, so the region above and the region below
    /// must overlap for EVERY grid cell of EVERY adjacent rung pair —
    /// asserted, not assumed.
    #[test]
    fn every_grid_cell_overlaps_its_twin_one_rung_down() {
        let rungs: Vec<Band> = hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect();
        for pair in rungs.windows(2) {
            let (upper, lower) = (pair[0], pair[1]);
            let d = grid_dims(upper);
            let dl = grid_dims(lower);
            for col in 0..d.cols.min(dl.cols) {
                for row in 0..d.rows.min(dl.rows) {
                    let a = region_rect(upper, GridCell { col, row });
                    let b = region_rect(lower, GridCell { col, row });
                    assert!(
                        a.intersect(&b).is_some(),
                        "{upper:?}/{lower:?} ({col},{row}): {a:?} vs {b:?}"
                    );
                }
            }
        }
    }
}
