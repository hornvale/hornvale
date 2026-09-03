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

use crate::character::Character;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Band, Seed, Stream, Vertex};
use hornvale_terrain::CaveKind;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// A region's span in cells, either axis — the Adit's `MIN_REGION_SPAN`,
/// now the grid pitch. The grid is `w / REGION_SPAN` by `h / REGION_SPAN`.
/// type-audit: bare-ok(count: REGION_SPAN)
/// plumb: pending(wave-1)
pub const REGION_SPAN: i32 = 8;
/// Dormans' clutter ceiling ("two to five cycles give each level a distinct
/// and recognizable shape"). The one authored constant in the grammar.
/// type-audit: bare-ok(count: MAX_CYCLES_PER_LEVEL)
/// plumb: pending(wave-1)
pub const MAX_CYCLES_PER_LEVEL: u8 = 5;
/// The floor of the same clip: every level gets at least one loop.
/// type-audit: bare-ok(count: MIN_CYCLES_PER_LEVEL)
/// plumb: pending(wave-1)
pub const MIN_CYCLES_PER_LEVEL: u8 = 1;

/// Level width before rank scaling. Since The Crosscut, Task 3,
/// `windows/vessel/src/underworld_level/mod.rs`'s `generate_level_extent`
/// DELEGATES to [`level_extent_wh`] rather than carrying its own copy of
/// this constant, so there is exactly one authored value left to agree
/// with.
/// plumb: pending(wave-1)
const BASE_LEVEL_W: i32 = 40;
/// See `BASE_LEVEL_W`.
/// plumb: pending(wave-1)
const BASE_LEVEL_H: i32 = 24;

/// The extent a level of `rung` gets, `(w, h)` in cells: deeper rungs get
/// more room. Since The Crosscut, Task 3, this is the ONE place the
/// formula lives — `windows/vessel/src/underworld_level/mod.rs`'s
/// `generate_level_extent` delegates to this function rather than carrying
/// a duplicate copy. `the_rank_zero_grid_is_five_by_three` pins the result.
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
    /// The key this node holds, for the gate on that edge; `None` almost
    /// everywhere.
    pub key: Option<crate::brattice::KeyFor>,
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
    /// A requirement on this edge's ways, if a pattern placed one (spec §3.1).
    pub gate: Option<crate::brattice::Gate>,
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
/// type-audit: bare-ok(count: dof), bare-ok(count: extensions), bare-ok(count: fallback_realms), bare-ok(count: failed_draws), bare-ok(count: skipped_patterns)
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
    /// How many `extend` (series) moves succeeded. Each cost exactly two
    /// draws (the op selector and the edge index), so [`DescentPlan::dof`]
    /// can be recounted exactly rather than merely bounded (spec §4.5).
    pub extensions: u32,
    /// How many realms were closed by `plan_descent`'s deterministic,
    /// DRAW-FREE fallback pass rather than by a drawn `cycle` move. They
    /// carry no `4 ·` term in the §4.5 recount precisely because they spend
    /// nothing.
    pub fallback_realms: u32,
    /// Draws spent by attempts that added neither a realm nor an
    /// extension — the one term §4.5's floor could not predict, counted at
    /// the failure site rather than reconstructed from the derivation tree
    /// (a failed attempt leaves nothing in the tree to walk).
    pub failed_draws: u32,
    /// Per realm, in `realms` order: which pattern was drawn and whether it
    /// was applied or why it was skipped (spec §3.1).
    pub patterns: Vec<crate::brattice::Outcome>,
    /// Realms that ended with no pattern stamped: those whose drawn row was
    /// refused for a [`crate::brattice::Skip`] reason, AND those for which no
    /// row was admissible at all (the draw is still made and discarded, so
    /// the draw count stays data-independent). One per realm, so
    /// `patterns.len() - skipped_patterns` is the number of realms carrying a
    /// stamped pattern.
    pub skipped_patterns: u32,
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
    /// The index of the edge joining `a` and `b` in either order, if any.
    /// type-audit: bare-ok(index: return)
    pub fn edge_index(&self, a: NodeId, b: NodeId) -> Option<usize> {
        self.edges
            .iter()
            .position(|e| (e.a == a && e.b == b) || (e.a == b && e.b == a))
    }
    /// The gate on the edge joining `a` and `b`, with the edge's index.
    /// type-audit: bare-ok(index: return)
    pub fn gate_between(&self, a: NodeId, b: NodeId) -> Option<(usize, &crate::brattice::Gate)> {
        let ix = self.edge_index(a, b)?;
        self.edges[ix].gate.as_ref().map(|g| (ix, g))
    }
}

/// Target cycles per level: DERIVED from the rock (karst dissolves many
/// routes, a lava tube is one conduit, a fracture system sits between) and
/// from workmanship (a worked place must ventilate, so it loops), clipped
/// to Dormans' `[MIN_CYCLES_PER_LEVEL, MAX_CYCLES_PER_LEVEL]`. `ChamberOrigin::Made`
/// joins the `worked` term when The Plat gives it a production writer.
/// type-audit: bare-ok(count: return)
pub fn cycle_budget(kind: CaveKind, character: Character) -> u8 {
    let base: u8 = match kind {
        CaveKind::LavaTube => 1,
        CaveKind::Fracture => 2,
        CaveKind::Karst => 3,
    };
    let worked: u8 = match character {
        Character::DrowTier => 1,
        Character::WildCave | Character::FungalGardens => 0,
    };
    (base + worked).clamp(MIN_CYCLES_PER_LEVEL, MAX_CYCLES_PER_LEVEL)
}

/// Dormans' class from two path lengths (edge counts). "Long" is RELATIVE:
/// a path is the long way round when it is strictly longer than the other
/// plus one. Within one of each other, both count long at three or more
/// edges and short below. Frozen here (spec §3.2 step 6) and exported.
/// type-audit: bare-ok(count: len_a), bare-ok(count: len_b)
pub fn length_class(len_a: usize, len_b: usize) -> LengthClass {
    if len_a > len_b + 1 {
        LengthClass::LongShort
    } else if len_b > len_a + 1 {
        LengthClass::ShortLong
    } else if len_a >= 3 && len_b >= 3 {
        LengthClass::LongLong
    } else {
        LengthClass::ShortShort
    }
}

/// The decimal key every plan leg is further derived by: the vertex.
fn vertex_key(vertex: Vertex) -> String {
    format!("{}", vertex.0)
}

fn leg(seed: Seed, label: StreamLabel<'static>, vertex: Vertex) -> Stream {
    seed.derive(label)
        .derive(StreamLabel::dynamic(&vertex_key(vertex)))
        .stream()
}

/// Draw an index in `0..n` from `stream`, counting the draw. `n >= 1`.
fn draw_index(stream: &mut Stream, n: usize, dof: &mut u32) -> usize {
    *dof += 1;
    (stream.next_u64() % n as u64) as usize
}

/// The growing plan and the per-level occupancy the grammar needs.
struct Builder {
    plan: DescentPlan,
    dims: Vec<GridDims>,
    /// Which grid cells each level has already spent.
    used: Vec<BTreeSet<GridCell>>,
    /// `(level, cell) -> node`.
    index: BTreeMap<(u8, GridCell), NodeId>,
}

impl Builder {
    /// Does `level` still have room for a loop of its own — some passage
    /// `(a, b)` on it whose endpoints a free detour can join
    /// (`free_path(level, ca, cb, 1)`)? The capability invariant (Task 2
    /// review, second pass): a level with no anchored realm always has at
    /// least one feasible same-floor cycle. Every operation that spends
    /// `level`'s cells while it still has no realm of its own checks this —
    /// via [`Builder::would_still_cycle`] — BEFORE committing the spend, so
    /// the deterministic fallback in `plan_descent` succeeds by
    /// construction rather than by chance.
    fn level_can_cycle(&self, level: u8) -> bool {
        self.plan
            .passages_on(level as usize)
            .into_iter()
            .any(|(a, b)| {
                let (ca, cb) = (self.plan.nodes[a].cell, self.plan.nodes[b].cell);
                self.free_path(level, ca, cb, 1).is_some()
            })
    }

    /// Evaluate [`Builder::level_can_cycle`] on `level` as the SERIES move
    /// `try_extend` is about to make would leave it — the Crosscut's
    /// deferred minor, taken by The Brattice (Task 1, step 10). The plain
    /// [`Builder::would_still_cycle`] answers against the PRE-extend passage
    /// set, which is wrong in both directions: it keeps `u`–`v`, the one
    /// passage the move removes, and it misses the chain that replaces it.
    /// So this evaluates the post-extend set exactly — `interior`'s grid
    /// squares spent, `u`–`v` gone, and `u`–`interior`–`v` in its place.
    /// Everything this function handles is a lattice square, an AREA whose
    /// rectangle `region_rect` gives — never a mesh vertex.
    fn would_still_cycle_after_extend(
        &mut self,
        level: u8,
        u: NodeId,
        v: NodeId,
        interior: &[GridCell], // lexicon: a lattice square, an area
    ) -> bool {
        for &c in interior {
            self.used[level as usize].insert(c);
        }
        // lexicon: `Node.cell` is a grid square — an area, not a mesh vertex.
        let (cu, cv) = (self.plan.nodes[u].cell, self.plan.nodes[v].cell); // lexicon: area
        let mut pairs: Vec<(GridCell, GridCell)> = self // lexicon: area
            .plan
            .passages_on(level as usize)
            .into_iter()
            .filter(|&(a, b)| !((a == u && b == v) || (a == v && b == u)))
            .map(|(a, b)| (self.plan.nodes[a].cell, self.plan.nodes[b].cell)) // lexicon: area
            .collect();
        let mut chain = vec![cu];
        chain.extend_from_slice(interior);
        chain.push(cv);
        for w in chain.windows(2) {
            pairs.push((w[0], w[1]));
        }
        let can = pairs
            .iter()
            .any(|&(ca, cb)| self.free_path(level, ca, cb, 1).is_some());
        for &c in interior {
            self.used[level as usize].remove(&c);
        }
        can
    }

    /// Evaluate [`Builder::level_can_cycle`] on `level` AS IF `cells` were
    /// already spent there, without actually spending them: tentatively
    /// mark `cells` used, check, then unmark. `cells` must currently be
    /// free (the caller's own candidate spend), so unmarking exactly
    /// restores the prior state.
    fn would_still_cycle(&mut self, level: u8, cells: &[GridCell]) -> bool {
        for &c in cells {
            self.used[level as usize].insert(c);
        }
        let can = self.level_can_cycle(level);
        for &c in cells {
            self.used[level as usize].remove(&c);
        }
        can
    }

    fn add_node(&mut self, level: u8, cell: GridCell) -> NodeId {
        let id = self.plan.nodes.len();
        self.plan.nodes.push(Node {
            level,
            cell,
            depth: 0,
            realm: None,
            key: None,
        });
        self.used[level as usize].insert(cell);
        self.index.insert((level, cell), id);
        id
    }

    fn add_passage(&mut self, a: NodeId, b: NodeId) {
        let (a, b) = (a.min(b), a.max(b));
        self.plan.edges.push(Edge {
            a,
            b,
            kind: EdgeKind::Passage,
            gate: None,
        });
    }

    fn remove_passage(&mut self, a: NodeId, b: NodeId) {
        let (a, b) = (a.min(b), a.max(b));
        self.plan
            .edges
            .retain(|e| !(e.kind == EdgeKind::Passage && e.a == a && e.b == b));
    }

    /// Grid-adjacent cells of `cell` on a level of `dims`, N, E, S, W.
    fn grid_neighbours(dims: GridDims, cell: GridCell) -> Vec<GridCell> {
        let mut out = Vec::with_capacity(4);
        if cell.row > 0 {
            out.push(GridCell {
                col: cell.col,
                row: cell.row - 1,
            });
        }
        if cell.col + 1 < dims.cols {
            out.push(GridCell {
                col: cell.col + 1,
                row: cell.row,
            });
        }
        if cell.row + 1 < dims.rows {
            out.push(GridCell {
                col: cell.col,
                row: cell.row + 1,
            });
        }
        if cell.col > 0 {
            out.push(GridCell {
                col: cell.col - 1,
                row: cell.row,
            });
        }
        out
    }

    /// The INTERIOR cells of a shortest path from `from` to `to` on `level`
    /// through cells not yet used, by breadth-first search with a fixed
    /// neighbour order — deterministic, no draw. `from` and `to` may be used
    /// (they are the endpoints); every interior cell must be free. `None`
    /// when no such path exists. A path with no interior (adjacent
    /// endpoints) is refused when `min_interior` is 1 or more.
    fn free_path(
        &self,
        level: u8,
        from: GridCell,
        to: GridCell,
        min_interior: usize,
    ) -> Option<Vec<GridCell>> {
        let dims = self.dims[level as usize];
        let used = &self.used[level as usize];
        let mut prev: BTreeMap<GridCell, GridCell> = BTreeMap::new();
        let mut q = VecDeque::new();
        for n in Self::grid_neighbours(dims, from) {
            if n == to {
                if min_interior == 0 {
                    return Some(Vec::new());
                }
                continue;
            }
            if !used.contains(&n) && !prev.contains_key(&n) {
                prev.insert(n, from);
                q.push_back(n);
            }
        }
        while let Some(c) = q.pop_front() {
            for n in Self::grid_neighbours(dims, c) {
                if n == to {
                    let mut path = vec![c];
                    let mut cur = c;
                    while let Some(&p) = prev.get(&cur) {
                        if p == from {
                            break;
                        }
                        path.push(p);
                        cur = p;
                    }
                    path.reverse();
                    if path.len() >= min_interior {
                        return Some(path);
                    }
                    continue;
                }
                if !used.contains(&n) && !prev.contains_key(&n) {
                    prev.insert(n, c);
                    q.push_back(n);
                }
            }
        }
        None
    }

    /// A stairway's shared coordinate: **ONE draw** into the overlap of the
    /// two regions (asserted non-empty by
    /// `every_grid_cell_overlaps_its_twin_one_rung_down`) MINUS every
    /// coordinate a stairway already touching either endpoint holds.
    ///
    /// **One draw, not two, since the final review (spec §3.3/§4.5
    /// amendment).** Drawing `x` and `y` independently cannot express the
    /// exclusion, and without it two stairways could coincide: a node on a
    /// middle rung is the upper end of one stairway and the lower end of
    /// another, the two coordinates were drawn independently from
    /// overlapping intersections, and on ~9% of five-rung descents they
    /// landed on the same cell — whereupon the realizer wrote a `StairsUp`
    /// over the `StairsDown` it had just written and left an orphan
    /// `StairsUp` on the floor below. `dof` therefore counts one draw per
    /// stairway, not two.
    ///
    /// The candidate list is never empty: the smallest intersection any
    /// adjacent rung pair admits is several cells across, and a node carries
    /// at most one stairway up and one down, so at most two coordinates are
    /// ever excluded.
    fn stair_coordinate(
        &self,
        upper: NodeId,
        lower: NodeId,
        stair: &mut Stream,
        dof: &mut u32,
    ) -> (i32, i32) {
        let overlap = self
            .plan
            .region_of(upper)
            .intersect(&self.plan.region_of(lower))
            .expect("adjacent rungs' twin regions overlap (Task 1 test)");
        let taken: BTreeSet<(i32, i32)> = self
            .plan
            .edges
            .iter()
            .filter_map(|e| match e.kind {
                EdgeKind::Stair { x, y }
                    if e.a == upper || e.b == upper || e.a == lower || e.b == lower =>
                {
                    Some((x, y))
                }
                _ => None,
            })
            .collect();
        let candidates: Vec<(i32, i32)> = (overlap.x..overlap.x + overlap.w)
            .flat_map(|x| (overlap.y..overlap.y + overlap.h).map(move |y| (x, y)))
            .filter(|c| !taken.contains(c))
            .collect();
        assert!(
            !candidates.is_empty(),
            "a stairway always has a free coordinate: nodes {upper}/{lower} overlap in {overlap:?} and hold only {taken:?}"
        );
        let i = draw_index(stair, candidates.len(), dof);
        candidates[i]
    }

    fn add_stair(&mut self, upper: NodeId, lower: NodeId, stair: &mut Stream, dof: &mut u32) {
        let (x, y) = self.stair_coordinate(upper, lower, stair, dof);
        self.plan.edges.push(Edge {
            a: upper,
            b: lower,
            kind: EdgeKind::Stair { x, y },
            gate: None,
        });
    }

    /// Lay a passage chain through `interior` from `from` to `to`, creating
    /// nodes for the interior cells. Returns the node path, endpoints included.
    fn lay_path(
        &mut self,
        level: u8,
        from: NodeId,
        interior: &[GridCell],
        to: NodeId,
    ) -> Vec<NodeId> {
        let mut path = vec![from];
        let mut prev = from;
        for &c in interior {
            let n = self.add_node(level, c);
            self.add_passage(prev, n);
            path.push(n);
            prev = n;
        }
        self.add_passage(prev, to);
        path.push(to);
        path
    }

    fn degree(&self, n: NodeId) -> usize {
        self.plan.neighbours(n).len()
    }

    /// From `v`, walk away from `u` through degree-two nodes along passages
    /// for up to `hops`, returning the segment `[u, v, ...]`.
    fn segment(&self, u: NodeId, v: NodeId, hops: usize) -> Vec<NodeId> {
        let mut seg = vec![u, v];
        for _ in 0..hops {
            let last = *seg.last().unwrap();
            let before = seg[seg.len() - 2];
            if self.degree(last) != 2 {
                break;
            }
            let next = self
                .plan
                .neighbours(last)
                .into_iter()
                .find(|&n| n != before);
            match next {
                Some(n)
                    if self.plan.edges.iter().any(|e| {
                        e.kind == EdgeKind::Passage
                            && ((e.a == last && e.b == n) || (e.a == n && e.b == last))
                    }) =>
                {
                    seg.push(n)
                }
                _ => break,
            }
        }
        seg
    }

    /// The innermost realm containing both `u` and `v`, if any.
    fn realm_containing(&self, u: NodeId, v: NodeId) -> Option<RealmId> {
        self.plan
            .realms
            .iter()
            .enumerate()
            .rev()
            .find(|(_, r)| {
                let m = |n| r.path_a.contains(&n) || r.path_b.contains(&n);
                m(u) && m(v)
            })
            .map(|(i, _)| i)
    }
}

/// Grow the plan for one descent (spec §3.2). `rungs` is the walked list,
/// shallowest first (`hornvale_terrain::rungs()` minus `Surface`).
pub fn plan_descent(
    seed: Seed,
    vertex: Vertex,
    rungs: &[Band],
    kind: CaveKind,
    character: Character,
) -> DescentPlan {
    assert!(!rungs.is_empty(), "a descent has at least one rung");
    let mut spine = leg(seed, crate::streams::UNDERWORLD_PLAN_SPINE, vertex);
    let mut cycle = leg(seed, crate::streams::UNDERWORLD_PLAN_CYCLE, vertex);
    let mut extend = leg(seed, crate::streams::UNDERWORLD_PLAN_EXTEND, vertex);
    let mut stair = leg(seed, crate::streams::UNDERWORLD_PLAN_STAIR, vertex);
    let mut dof = 0u32;
    let dims: Vec<GridDims> = rungs.iter().map(|&r| grid_dims(r)).collect();
    let mut b = Builder {
        plan: DescentPlan {
            rungs: rungs.to_vec(),
            nodes: Vec::new(),
            edges: Vec::new(),
            entrance: 0,
            terminus: 0,
            realms: Vec::new(),
            dof: 0,
            extensions: 0,
            fallback_realms: 0,
            failed_draws: 0,
            patterns: Vec::new(),
            skipped_patterns: 0,
        },
        used: vec![BTreeSet::new(); rungs.len()],
        index: BTreeMap::new(),
        dims,
    };

    // 1. The spine.
    let row0 = draw_index(&mut spine, b.dims[0].rows as usize, &mut dof) as u8;
    let mut arrival = b.add_node(0, GridCell { col: 0, row: row0 });
    b.plan.entrance = arrival;
    for level in 0..rungs.len() as u8 {
        let d = b.dims[level as usize];
        let all: Vec<GridCell> = (0..d.cols)
            .flat_map(|col| (0..d.rows).map(move |row| GridCell { col, row }))
            .filter(|c| !b.used[level as usize].contains(c))
            .collect();
        let target_cell = all[draw_index(&mut spine, all.len(), &mut dof)];
        let interior = b
            .free_path(level, b.plan.nodes[arrival].cell, target_cell, 0)
            .expect("an otherwise empty level always has a free path");
        let target = b.add_node(level, target_cell);
        b.lay_path(level, arrival, &interior, target);
        if (level as usize) + 1 < rungs.len() {
            let below = b.add_node(level + 1, target_cell);
            b.add_stair(target, below, &mut stair, &mut dof);
            arrival = below;
        } else {
            b.plan.terminus = target;
        }
    }

    // 2. Cycles and extensions, level by level, to a derived budget.
    let budget = cycle_budget(kind, character) as usize;
    for level in 0..rungs.len() as u8 {
        let mut attempts = 0;
        while anchored_realms(&b.plan, level as usize) < budget && attempts < 80 {
            attempts += 1;
            let passages = b.plan.passages_on(level as usize);
            if passages.is_empty() {
                break;
            }
            // §4.5's exact recount: an attempt that lands spends a fixed
            // number of draws (four for a cycle, two for an extension), so
            // only the FAILURES need counting, and they are counted here,
            // at the site, from `dof` itself — never inferred later.
            let before = dof;
            let op = draw_index(&mut cycle, 10, &mut dof);
            let landed = if op < 7 {
                let (u, v) = passages[draw_index(&mut cycle, passages.len(), &mut dof)];
                try_cycle(&mut b, level, u, v, &mut cycle, &mut stair, &mut dof)
            } else {
                let (u, v) = passages[draw_index(&mut extend, passages.len(), &mut dof)];
                let ok = try_extend(&mut b, level, u, v);
                if ok {
                    b.plan.extensions += 1;
                }
                ok
            };
            if !landed {
                b.plan.failed_draws += dof - before;
            }
        }
        // Deterministic fallback (Task 2 review, Critical): the random
        // budget loop above is best-effort — a cross-floor cycle from a
        // shallower level can leave this one starved of free cells before
        // it gets a turn. If the level still holds no loop at all, walk its
        // passages in order and attempt one draw-free same-floor cycle at
        // the first one that admits it, so `MIN_CYCLES_PER_LEVEL` holds by
        // construction rather than by chance. Spends no stream draw, so
        // `dof` is unaffected.
        if anchored_realms(&b.plan, level as usize) == 0 {
            let passages = b.plan.passages_on(level as usize);
            for (u, v) in passages {
                let path_a = b.segment(u, v, 0);
                if try_same_floor_cycle(&mut b, level, u, v, path_a) {
                    b.plan.fallback_realms += 1;
                    break;
                }
            }
        }
        debug_assert!(
            anchored_realms(&b.plan, level as usize) >= 1,
            "the capability invariant guarantees the fallback a cycle"
        );
    }

    // 3. Attributes.
    assign_realms(&mut b.plan);
    assign_depth(&mut b.plan);
    // 4. Classes, from the REALIZED paths (The Brattice, Ruling A).
    recompute_classes(&mut b.plan);
    // 5. Gates (The Brattice): one pattern draw per realm, after growth so
    // `extend` can no longer orphan an edge attribute.
    let mut pattern_leg = leg(seed, crate::streams::UNDERWORLD_GATE_PATTERN, vertex);
    crate::brattice::stamp(&mut b.plan, kind, character, &mut pattern_leg, &mut dof);
    b.plan.dof = dof;
    b.plan
}

/// One drawn `cycle` attempt. Returns whether a realm was added — the
/// caller charges a failure's draws to [`DescentPlan::failed_draws`]
/// (spec §4.5).
fn try_cycle(
    b: &mut Builder,
    level: u8,
    u: NodeId,
    v: NodeId,
    cycle: &mut Stream,
    stair: &mut Stream,
    dof: &mut u32,
) -> bool {
    let hops = draw_index(cycle, 3, dof);
    let path_a = b.segment(u, v, hops);
    let end = *path_a.last().unwrap();
    // Minor (Task 2 review): a degenerate segment can close back onto `u`
    // (a short walk through degree-two nodes that loops back to its own
    // start); that would give `cu == ce` and a duplicated edge, so refuse
    // it outright rather than let either branch below act on it.
    if end == u {
        return false;
    }
    let (cu, ce) = (b.plan.nodes[u].cell, b.plan.nodes[end].cell);
    let cross = draw_index(cycle, 100, dof) < 35
        && (level as usize) + 1 < b.plan.rungs.len()
        && cu != ce
        && !b.used[level as usize + 1].contains(&cu)
        && !b.used[level as usize + 1].contains(&ce);
    if cross && let Some(interior) = b.free_path(level + 1, cu, ce, 0) {
        // Capability invariant (Task 2 review, second pass): if level
        // `ℓ + 1` still has no realm of its own, check — AS IF this
        // landing's cells (`cu`, `ce`, `interior`) were already spent —
        // that it would still have a feasible same-floor cycle of its own.
        // Refuse the cross-floor attachment and fall through to the
        // same-floor branch below (do not `return`) if it would not. This
        // replaces the earlier count-based `CROSS_FLOOR_RESERVE`: a count of
        // free cells says nothing about whether they are reachable from any
        // of the level's own passages, and a count-based reserve left 9 of
        // 72,000 swept plans starved regardless of its value.
        let mut spend: Vec<GridCell> = interior.clone();
        spend.push(cu);
        spend.push(ce);
        let ok = anchored_realms(&b.plan, level as usize + 1) > 0
            || b.would_still_cycle(level + 1, &spend);
        if ok {
            let parent = b.realm_containing(u, end);
            let lu = b.add_node(level + 1, cu);
            let le = b.add_node(level + 1, ce);
            b.add_stair(u, lu, stair, dof);
            let mut path_b = vec![u];
            path_b.extend(b.lay_path(level + 1, lu, &interior, le));
            b.add_stair(end, le, stair, dof);
            path_b.push(end);
            let class = length_class(path_a.len() - 1, path_b.len() - 1);
            b.plan.realms.push(Realm {
                parent,
                anchor_level: level,
                path_a,
                path_b,
                class,
            });
            return true;
        }
        // The invariant refuses: fall through to the same-floor branch
        // below rather than spending level `ℓ + 1`'s last feasible cycle.
    }
    try_same_floor_cycle(b, level, u, end, path_a)
}

/// The same-floor half of a cycle attachment: given the pre-existing
/// segment `path_a` from `u` to `end` (endpoints included), lay a
/// node-disjoint detour through free cells on the same level and record the
/// realm. Touches no stream — shared by `try_cycle`'s own same-floor branch
/// (drawn) and the deterministic, draw-free fallback in `plan_descent`
/// (Task 2 review, Critical). Returns whether a realm was added.
fn try_same_floor_cycle(
    b: &mut Builder,
    level: u8,
    u: NodeId,
    end: NodeId,
    path_a: Vec<NodeId>,
) -> bool {
    let (cu, ce) = (b.plan.nodes[u].cell, b.plan.nodes[end].cell);
    let Some(interior) = b.free_path(level, cu, ce, 1) else {
        return false;
    };
    let parent = b.realm_containing(u, end);
    let path_b = b.lay_path(level, u, &interior, end);
    let class = length_class(path_a.len() - 1, path_b.len() - 1);
    b.plan.realms.push(Realm {
        parent,
        anchor_level: level,
        path_a,
        path_b,
        class,
    });
    true
}

/// One drawn `extend` attempt. Returns whether the series move landed —
/// the caller counts a success in [`DescentPlan::extensions`] and charges a
/// failure's draws to [`DescentPlan::failed_draws`] (spec §4.5).
fn try_extend(b: &mut Builder, level: u8, u: NodeId, v: NodeId) -> bool {
    let (cu, cv) = (b.plan.nodes[u].cell, b.plan.nodes[v].cell);
    let Some(interior) = b.free_path(level, cu, cv, 1) else {
        return false;
    };
    // Capability invariant (Task 2 review, second pass): if this level
    // still has no realm of its own, refuse to spend the detour's cells if
    // doing so would leave it with no feasible same-floor cycle at all.
    if anchored_realms(&b.plan, level as usize) == 0
        && !b.would_still_cycle_after_extend(level, u, v, &interior)
    {
        return false;
    }
    // Every realm path that ran through u-v now runs through the detour.
    b.remove_passage(u, v);
    let path = b.lay_path(level, u, &interior, v);
    for r in &mut b.plan.realms {
        let mut touched = 0;
        for p in [&mut r.path_a, &mut r.path_b] {
            if let Some(i) = p
                .windows(2)
                .position(|w| (w[0] == u && w[1] == v) || (w[0] == v && w[1] == u))
            {
                touched += 1;
                let forward = p[i] == u;
                let mut mids: Vec<NodeId> = path[1..path.len() - 1].to_vec();
                if !forward {
                    mids.reverse();
                }
                p.splice(i + 1..i + 1, mids);
            }
        }
        debug_assert!(
            touched <= 1,
            "extend: both paths of one realm carried the same edge (impossible for node-disjoint interiors)"
        );
    }
    true
}

/// Re-derive every realm's [`LengthClass`] from its REALIZED paths.
///
/// `Realm.class` used to be whatever [`length_class`] returned when the realm
/// was CREATED, and `try_extend` splices interior chains into existing realm
/// paths afterwards — so the exported class described a graph that no longer
/// existed. Measured over 4,412 realms: `path_a` reaches **16** edges against
/// a creation-time ceiling of **3**, which made the class `LongShort`
/// (`len_a > len_b + 1`, i.e. `len_a >= 4`) structurally UNREACHABLE. Nothing
/// read `class` until The Brattice, so the staleness was latent; The Brattice
/// reads it to select a cycle pattern and four inventory rows were dead data
/// as a result (Ruling A). [`length_class`]'s rule itself is unchanged, and
/// none of the four §4 readouts reads `class`, so this moves no readout.
fn recompute_classes(plan: &mut DescentPlan) {
    for r in &mut plan.realms {
        r.class = length_class(r.path_a.len() - 1, r.path_b.len() - 1);
    }
}

/// `Node.realm` = the LAST realm (creation order) whose paths hold the node:
/// a nested realm is created after its parent, so last is innermost.
fn assign_realms(plan: &mut DescentPlan) {
    for n in &mut plan.nodes {
        n.realm = None;
    }
    for (rid, r) in plan.realms.iter().enumerate() {
        for &n in r.path_a.iter().chain(r.path_b.iter()) {
            plan.nodes[n].realm = Some(rid);
        }
    }
}

/// Breadth-first hops from the entrance.
fn assign_depth(plan: &mut DescentPlan) {
    let mut dist: BTreeMap<NodeId, u16> = BTreeMap::new();
    let mut q = VecDeque::from([plan.entrance]);
    dist.insert(plan.entrance, 0);
    while let Some(n) = q.pop_front() {
        let d = dist[&n];
        for m in plan.neighbours(n) {
            if let std::collections::btree_map::Entry::Vacant(e) = dist.entry(m) {
                e.insert(d + 1);
                q.push_back(m);
            }
        }
    }
    for (i, n) in plan.nodes.iter_mut().enumerate() {
        n.depth = dist.get(&i).copied().unwrap_or(u16::MAX);
    }
}

/// Realms whose `path_a` sits on `level` — the level's own density (spec §4.2).
/// type-audit: bare-ok(index: level), bare-ok(count: return)
pub fn anchored_realms(plan: &DescentPlan, level: usize) -> usize {
    plan.realms
        .iter()
        .filter(|r| r.anchor_level as usize == level)
        .count()
}

/// Spec §4.1: the share of non-entrance regions reachable from the entrance
/// that stay reachable under the removal of ANY single edge — two
/// edge-disjoint routes home (Menger). Regions unreachable from the entrance
/// are excluded from both numerator and denominator.
/// type-audit: bare-ok(ratio: return)
pub fn loop_share(plan: &DescentPlan) -> f64 {
    fn reach(plan: &DescentPlan, skip: Option<usize>) -> BTreeSet<NodeId> {
        let mut seen = BTreeSet::from([plan.entrance]);
        let mut q = VecDeque::from([plan.entrance]);
        while let Some(n) = q.pop_front() {
            for (i, e) in plan.edges.iter().enumerate() {
                if Some(i) == skip {
                    continue;
                }
                let m = if e.a == n {
                    e.b
                } else if e.b == n {
                    e.a
                } else {
                    continue;
                };
                if seen.insert(m) {
                    q.push_back(m);
                }
            }
        }
        seen
    }
    let base = reach(plan, None);
    let candidates: Vec<NodeId> = base
        .iter()
        .copied()
        .filter(|&n| n != plan.entrance)
        .collect();
    if candidates.is_empty() {
        return 0.0;
    }
    let mut robust: BTreeSet<NodeId> = candidates.iter().copied().collect();
    for i in 0..plan.edges.len() {
        let r = reach(plan, Some(i));
        robust.retain(|n| r.contains(n));
    }
    robust.len() as f64 / candidates.len() as f64
}

/// Spec §4.3: does any realm's `path_b` touch a level other than its anchor?
/// type-audit: bare-ok(flag: return)
pub fn has_cross_floor_realm(plan: &DescentPlan) -> bool {
    plan.realms.iter().any(|r| {
        r.path_b
            .iter()
            .any(|&n| plan.nodes[n].level != r.anchor_level)
    })
}

/// Spec §4.4: among nodes on at least one realm, the share on two or more.
/// `None` when no node is on any realm.
/// type-audit: bare-ok(ratio: return)
pub fn semilattice_overlap(plan: &DescentPlan) -> Option<f64> {
    let mut count: BTreeMap<NodeId, usize> = BTreeMap::new();
    for r in &plan.realms {
        let members: BTreeSet<NodeId> = r.path_a.iter().chain(r.path_b.iter()).copied().collect();
        for n in members {
            *count.entry(n).or_insert(0) += 1;
        }
    }
    if count.is_empty() {
        return None;
    }
    let on_two = count.values().filter(|&&c| c >= 2).count();
    Some(on_two as f64 / count.len() as f64)
}

/// Report-only companion to [`loop_share`] (spec §4.1, ledger amendment): the
/// share of non-entrance nodes lying on at least one realm — i.e. carrying
/// `Some` in [`Node::realm`]. Disclosed after Task 2 showed the entrance
/// doorway is a bridge on ~40% of seeds, so the reader can tell "the entrance
/// is a bridge" from "the levels have no loops". Carries no verdict word and
/// is never gated.
/// type-audit: bare-ok(ratio: return)
pub fn cycle_membership_share(plan: &DescentPlan) -> f64 {
    let candidates: Vec<&Node> = plan
        .nodes
        .iter()
        .enumerate()
        .filter(|&(i, _)| i != plan.entrance)
        .map(|(_, n)| n)
        .collect();
    if candidates.is_empty() {
        return 0.0;
    }
    let on_realm = candidates.iter().filter(|n| n.realm.is_some()).count();
    on_realm as f64 / candidates.len() as f64
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

    fn habitation_rungs() -> Vec<Band> {
        hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect()
    }
    fn plan(seed: u64, vertex: u32) -> DescentPlan {
        plan_descent(
            Seed(seed),
            Vertex(vertex),
            &habitation_rungs(),
            CaveKind::Karst,
            Character::WildCave,
        )
    }

    /// claim: invariant(seed: 0..100) — Spec §3.4 (1): every edge joins
    /// grid-adjacent cells on one level or the same cell on adjacent
    /// levels — planar and embedded by construction.
    #[test]
    fn every_edge_is_grid_adjacent_or_a_vertical_stair() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            for e in &p.edges {
                let (a, b) = (p.nodes[e.a], p.nodes[e.b]);
                match e.kind {
                    EdgeKind::Passage => {
                        assert_eq!(a.level, b.level, "seed {s}: passage crosses levels");
                        let dc = (a.cell.col as i32 - b.cell.col as i32).abs();
                        let dr = (a.cell.row as i32 - b.cell.row as i32).abs();
                        assert_eq!(dc + dr, 1, "seed {s}: passage {e:?} not grid-adjacent");
                    }
                    EdgeKind::Stair { x, y } => {
                        assert_eq!(
                            a.level + 1,
                            b.level,
                            "seed {s}: stair {e:?} not one rung down"
                        );
                        assert_eq!(a.cell, b.cell, "seed {s}: stair {e:?} changes grid cell");
                        let ra = p.region_of(e.a);
                        let rb = p.region_of(e.b);
                        for r in [ra, rb] {
                            assert!(
                                x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h,
                                "seed {s}: stair coordinate ({x},{y}) outside {r:?}"
                            );
                        }
                    }
                }
            }
        }
    }

    /// claim: invariant(seed: 0..100) — Spec §3.4 (2): every node reachable
    /// from the entrance.
    #[test]
    fn every_node_is_reachable_from_the_entrance() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            let mut seen = BTreeSet::new();
            let mut q = VecDeque::from([p.entrance]);
            seen.insert(p.entrance);
            while let Some(n) = q.pop_front() {
                for m in p.neighbours(n) {
                    if seen.insert(m) {
                        q.push_back(m);
                    }
                }
            }
            assert_eq!(seen.len(), p.nodes.len(), "seed {s}: unreachable regions");
        }
    }

    /// claim: invariant(kind: [LavaTube, Fracture, Karst], character:
    /// [WildCave, FungalGardens, DrowTier], vertex: [1, 7, 42, 1000], seed:
    /// 0..400) — Spec §3.4 (3) and §3.2 step 5: every realm adds exactly one
    /// to the Kirchhoff mesh count, and every level sits inside the clip,
    /// swept across every kind/character/vertex combination and 400 seeds
    /// each (Task 2 review: the narrower `plan(s, 7)`-only, 0..100 sweep
    /// missed a starved level that first appears at seed 242).
    #[test]
    fn realms_are_the_mesh_count_and_every_level_is_inside_the_clip() {
        let kinds = [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst];
        let characters = [
            Character::WildCave,
            Character::FungalGardens,
            Character::DrowTier,
        ];
        let vertices = [1u32, 7, 42, 1000];
        let rungs = habitation_rungs();
        for &kind in &kinds {
            for &character in &characters {
                for &vertex in &vertices {
                    for s in 0..400u64 {
                        let p = plan_descent(Seed(s), Vertex(vertex), &rungs, kind, character);
                        let cyclomatic = p.edges.len() as i64 - p.nodes.len() as i64 + 1;
                        assert_eq!(
                            cyclomatic,
                            p.realms.len() as i64,
                            "kind {kind:?} character {character:?} vertex {vertex} seed {s}: E-V+1 != realms"
                        );
                        for level in 0..p.rungs.len() {
                            let n = anchored_realms(&p, level);
                            assert!(
                                n >= MIN_CYCLES_PER_LEVEL as usize
                                    && n <= MAX_CYCLES_PER_LEVEL as usize,
                                "kind {kind:?} character {character:?} vertex {vertex} seed {s} level {level}: {n} realms"
                            );
                        }
                    }
                }
            }
        }
    }

    /// claim: invariant(kind: [LavaTube, Fracture, Karst], character:
    /// [WildCave, FungalGardens, DrowTier], vertex: [1, 7, 42, 1000], seed:
    /// 0..400) — Spec §3.3's execution amendment (final review): a node on a
    /// middle rung can be the UPPER end of one stairway and the LOWER end of
    /// another, and two coordinates drawn independently from overlapping
    /// intersections coincided on ~9% of five-rung descents, whereupon the
    /// realizer's `stairs_into` loop overwrote the `StairsDown` the
    /// `stairs_from` loop had written and left an orphan `StairsUp` below.
    /// So: no two `Stair` edges may share a `(level, x, y)` at EITHER end,
    /// over the real habitation ladder rather than a two-rung stub.
    #[test]
    fn no_two_stairways_share_a_coordinate_at_either_end() {
        let kinds = [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst];
        let characters = [
            Character::WildCave,
            Character::FungalGardens,
            Character::DrowTier,
        ];
        let vertices = [1u32, 7, 42, 1000];
        let rungs = habitation_rungs();
        for &kind in &kinds {
            for &character in &characters {
                for &vertex in &vertices {
                    for s in 0..400u64 {
                        let p = plan_descent(Seed(s), Vertex(vertex), &rungs, kind, character);
                        let mut held: BTreeMap<(u8, i32, i32), usize> = BTreeMap::new();
                        for (i, e) in p.edges.iter().enumerate() {
                            let EdgeKind::Stair { x, y } = e.kind else {
                                continue;
                            };
                            for end in [e.a, e.b] {
                                let key = (p.nodes[end].level, x, y);
                                if let Some(other) = held.insert(key, i) {
                                    panic!(
                                        "kind {kind:?} character {character:?} vertex {vertex} \
                                         seed {s}: stairways {other} and {i} both stand on \
                                         level {} ({x},{y})",
                                        p.nodes[end].level
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// claim: invariant(seed: 0..100) — every node on a cycle names the
    /// innermost realm containing it; every nested realm's parent contains
    /// both its endpoints.
    #[test]
    fn realm_ownership_is_innermost_and_parents_contain_their_children() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            for (rid, r) in p.realms.iter().enumerate() {
                if let Some(parent) = r.parent {
                    assert!(parent < rid, "seed {s}: parent after child");
                    let pr = &p.realms[parent];
                    let members: BTreeSet<NodeId> =
                        pr.path_a.iter().chain(pr.path_b.iter()).copied().collect();
                    assert!(
                        members.contains(&r.path_a[0])
                            && members.contains(r.path_a.last().unwrap()),
                        "seed {s}: realm {rid}'s endpoints not in parent {parent}"
                    );
                }
                assert_eq!(
                    r.path_a.first(),
                    r.path_b.first(),
                    "seed {s}: paths start apart"
                );
                assert_eq!(
                    r.path_a.last(),
                    r.path_b.last(),
                    "seed {s}: paths end apart"
                );
                let inner_a: BTreeSet<_> = r.path_a[1..r.path_a.len() - 1].iter().collect();
                let inner_b: BTreeSet<_> = r.path_b[1..r.path_b.len() - 1].iter().collect();
                assert!(
                    inner_a.is_disjoint(&inner_b),
                    "seed {s}: realm {rid} paths share an interior node"
                );
            }
            for (nid, n) in p.nodes.iter().enumerate() {
                let holding: Vec<RealmId> = p
                    .realms
                    .iter()
                    .enumerate()
                    .filter(|(_, r)| r.path_a.contains(&nid) || r.path_b.contains(&nid))
                    .map(|(i, _)| i)
                    .collect();
                assert_eq!(
                    n.realm,
                    holding.last().copied(),
                    "seed {s}: node {nid} realm"
                );
            }
        }
    }

    #[test]
    fn the_plan_is_deterministic_and_reads_the_vertex() {
        assert_eq!(plan(42, 7), plan(42, 7));
        assert_ne!(
            plan(42, 7),
            plan(42, 8),
            "two caves in one world must not share a plan"
        );
        assert_ne!(plan(42, 7), plan(43, 7));
    }

    /// The §4.5 recount, as amended at the final review. The G3 draft asked
    /// for an independent walk of the decomposition tree; that walk cannot
    /// exist, because a failed `cycle` or `extend` attempt spends its draws
    /// and leaves nothing in the tree. So the count is taken at the FAILURE
    /// SITE instead ([`DescentPlan::failed_draws`]) and the identity below
    /// is exact rather than a bound:
    ///
    /// ```text
    ///   dof = 1                      the entrance row
    ///       + levels                 one spine stair cell per level
    ///       + stairs                 ONE coordinate per stairway (§3.3)
    ///       + 4 · drawn realms       op + edge + hops + cross
    ///       + 2 · extensions         op + edge
    ///       + failed_draws           counted where they are spent
    ///       + realms                 ONE pattern draw per realm (The
    ///                                Brattice, spec §3.8)
    /// ```
    ///
    /// `drawn realms` excludes the deterministic fallback pass's realms,
    /// which are draw-free by construction. The loose `floor <= dof <=
    /// ceiling` bracket §4.5 states is asserted too, so the published
    /// bounds stay honest, and `dof == floor` is asserted outright on a plan
    /// where nothing failed — which is what makes the floor an exact count
    /// and not merely a lower bound nothing ever touches.
    ///
    /// claim: invariant(kind: [LavaTube, Fracture, Karst], vertex: [1, 7,
    /// 42, 1000], seed: 0..400)
    #[test]
    fn dof_counts_every_draw() {
        let rungs = habitation_rungs();
        let mut saw_a_flawless_plan = false;
        let mut saw_a_fallback_realm = false;
        for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
            for vertex in [1u32, 7, 42, 1000] {
                for s in 0..400u64 {
                    let p =
                        plan_descent(Seed(s), Vertex(vertex), &rungs, kind, Character::WildCave);
                    assert!(
                        p.dof > 0,
                        "{kind:?} vertex {vertex} seed {s}: no draw counted"
                    );
                    let levels = p.rungs.len() as u32;
                    let stairs = p
                        .edges
                        .iter()
                        .filter(|e| matches!(e.kind, EdgeKind::Stair { .. }))
                        .count() as u32;
                    // `Edge.a` is the upper node on every `Stair` (final review,
                    // Minor #4): a chute's `toward_a` direction is UP, so any
                    // read of a chute's climb direction is silently backwards
                    // if this ever stops holding. Swept broader here (3 kinds x
                    // 4 vertices x 400 seeds) than the narrower geometry test.
                    for e in &p.edges {
                        if matches!(e.kind, EdgeKind::Stair { .. }) {
                            assert_eq!(
                                p.nodes[e.a].level + 1,
                                p.nodes[e.b].level,
                                "{kind:?} vertex {vertex} seed {s}: stair {e:?} has Edge.a below Edge.b"
                            );
                        }
                    }
                    let drawn_realms = p.realms.len() as u32 - p.fallback_realms;
                    let floor = 1
                        + levels
                        + stairs
                        + 4 * drawn_realms
                        + 2 * p.extensions
                        + p.realms.len() as u32;
                    let ceiling = floor + 4 * 80 * levels;
                    assert_eq!(
                        p.dof,
                        floor + p.failed_draws,
                        "{kind:?} vertex {vertex} seed {s}: dof is not floor {floor} plus the {} draws its failed attempts spent",
                        p.failed_draws
                    );
                    assert!(
                        floor <= p.dof && p.dof <= ceiling,
                        "{kind:?} vertex {vertex} seed {s}: dof {} outside [{floor}, {ceiling}]",
                        p.dof
                    );
                    if p.failed_draws == 0 {
                        assert_eq!(
                            p.dof, floor,
                            "{kind:?} vertex {vertex} seed {s}: a plan with no failed attempt must spend exactly its floor"
                        );
                        saw_a_flawless_plan = true;
                    }
                    if p.fallback_realms > 0 {
                        saw_a_fallback_realm = true;
                    }
                }
            }
        }
        assert!(
            saw_a_flawless_plan,
            "no swept plan grew without a failed attempt — the `dof == floor` equality above was never actually checked"
        );
        assert!(
            saw_a_fallback_realm,
            "no swept plan used the draw-free fallback — the `- fallback_realms` term above was never actually exercised"
        );
    }

    #[test]
    fn budget_is_derived_from_rock_and_workmanship() {
        assert!(
            cycle_budget(CaveKind::LavaTube, Character::WildCave)
                < cycle_budget(CaveKind::Fracture, Character::WildCave)
        );
        assert!(
            cycle_budget(CaveKind::Fracture, Character::WildCave)
                < cycle_budget(CaveKind::Karst, Character::WildCave)
        );
        assert!(
            cycle_budget(CaveKind::Karst, Character::DrowTier)
                > cycle_budget(CaveKind::Karst, Character::WildCave)
        );
        assert!(cycle_budget(CaveKind::Karst, Character::DrowTier) <= MAX_CYCLES_PER_LEVEL);
        assert!(cycle_budget(CaveKind::LavaTube, Character::WildCave) >= MIN_CYCLES_PER_LEVEL);
    }

    #[test]
    fn length_class_follows_the_frozen_rule() {
        assert_eq!(length_class(1, 3), LengthClass::ShortLong);
        assert_eq!(length_class(3, 1), LengthClass::LongShort);
        assert_eq!(length_class(3, 4), LengthClass::LongLong);
        assert_eq!(length_class(1, 2), LengthClass::ShortShort);
        assert_eq!(length_class(2, 2), LengthClass::ShortShort);
    }

    /// claim: invariant(seed: 0..200) — every realm's `class` is the class of
    /// its REALIZED paths, not of the paths it was created with. `try_extend`
    /// splices chains into existing realm paths after the realm is recorded,
    /// so a class frozen at creation goes stale; `LongShort` was unreachable
    /// under the frozen reading (a creation-time `path_a` is at most 3 edges
    /// and `path_b` at least 2, and `LongShort` needs `len_a >= 4`), which is
    /// asserted here as an actual sighting rather than trusted — an equality
    /// alone cannot tell this pass apart from one that never runs.
    #[test]
    fn realm_class_is_recomputed_from_the_realized_paths() {
        let rungs = habitation_rungs();
        let mut saw_long_short = false;
        for s in 0..200u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                let p = plan_descent(Seed(s), Vertex(2), &rungs, kind, Character::DrowTier);
                for (i, r) in p.realms.iter().enumerate() {
                    let want = length_class(r.path_a.len() - 1, r.path_b.len() - 1);
                    assert_eq!(
                        r.class,
                        want,
                        "{kind:?} seed {s} realm {i}: class {:?} is stale against realized ({}, {})",
                        r.class,
                        r.path_a.len() - 1,
                        r.path_b.len() - 1
                    );
                    if r.class == LengthClass::LongShort {
                        saw_long_short = true;
                    }
                }
            }
        }
        assert!(
            saw_long_short,
            "no realm in the sweep is LongShort — the recompute changed nothing and four inventory rows stay dead"
        );
    }

    /// claim: rate(seed: 0..100) — Spec §4.3's move must be REACHABLE for the
    /// readout to mean anything: somewhere in 100 seeds a realm spans two
    /// floors.
    #[test]
    fn some_seed_produces_a_cross_floor_realm() {
        assert!((0..100u64).any(|s| has_cross_floor_realm(&plan(s, 7))));
    }

    /// A bare 3-node, 2-edge single-level path: no cycle, so no node's
    /// `realm` is ever `Some`. Shared by the loop-share and
    /// cycle-membership tests below — factored out (rather than duplicated)
    /// so the lexicon guard's per-file token ceiling
    /// (`docs/audits/lexicon-inventory.tsv`) does not grow every time a new
    /// test wants the same fixture; growing it requires a rebaseline a human
    /// agrees with, not a second copy of an existing literal.
    fn bare_three_node_path() -> DescentPlan {
        DescentPlan {
            rungs: vec![Band::Undercroft],
            nodes: vec![
                Node {
                    level: 0,
                    cell: GridCell { col: 0, row: 0 },
                    depth: 0,
                    realm: None,
                    key: None,
                },
                Node {
                    level: 0,
                    cell: GridCell { col: 1, row: 0 },
                    depth: 1,
                    realm: None,
                    key: None,
                },
                Node {
                    level: 0,
                    cell: GridCell { col: 2, row: 0 },
                    depth: 2,
                    realm: None,
                    key: None,
                },
            ],
            edges: vec![
                Edge {
                    a: 0,
                    b: 1,
                    kind: EdgeKind::Passage,
                    gate: None,
                },
                Edge {
                    a: 1,
                    b: 2,
                    kind: EdgeKind::Passage,
                    gate: None,
                },
            ],
            entrance: 0,
            terminus: 2,
            realms: vec![],
            dof: 0,
            extensions: 0,
            fallback_realms: 0,
            failed_draws: 0,
            patterns: vec![],
            skipped_patterns: 0,
        }
    }

    #[test]
    fn a_tree_has_zero_loop_share_and_a_cycle_has_full() {
        assert_eq!(loop_share(&bare_three_node_path()), 0.0);
        // Whether the entrance itself lands on a cycle (degree >= 2) is a
        // real per-seed coin flip — measured at ~60% positive across 200
        // (seed, vertex=1) draws — not a bug: a cave has exactly one
        // physical doorway, so `loop_share` is *correctly* zero whenever
        // that doorway's sole passage is a cut edge, however cyclic the
        // rest of the level is. `plan(1, 1)` (the brief's literal pairing)
        // lands on the negative side of that coin flip under the
        // controller-corrected op-then-edge draw order (correction 2);
        // `plan(6, 1)` is a verified-positive substitute that exercises the
        // same code path.
        assert!(loop_share(&plan(6, 1)) > 0.0);
    }

    /// claim: invariant(seed: 0..50) — [`cycle_membership_share`] is always
    /// a well-formed share, the frozen hand-built 3-node path (no realm
    /// touches any node) gives exactly 0.0, and — checked against an
    /// independent re-derivation over `plan.nodes` rather than merely a
    /// `[0,1]` bound — every real plan's share matches "count non-entrance
    /// nodes with `realm.is_some()`, divide by non-entrance node count"
    /// exactly. A bound alone cannot tell this function apart from one that
    /// always returns 0.0 (0.0 satisfies `(0.0..=1.0).contains`); the
    /// re-derivation can, and at least one swept plan has `realms.len() >=
    /// 1` and a strictly positive share, so the equality is not vacuously
    /// checked against an all-zero sweep either.
    #[test]
    fn cycle_membership_share_is_a_ratio_and_zero_on_a_bare_path() {
        assert_eq!(cycle_membership_share(&bare_three_node_path()), 0.0);
        let mut saw_nonzero = false;
        for s in 0..50u64 {
            let p = plan(s, 3);
            let share = cycle_membership_share(&p);
            assert!(
                (0.0..=1.0).contains(&share),
                "seed {s}: share {share} out of [0,1]"
            );
            let non_entrance: Vec<&Node> = p
                .nodes
                .iter()
                .enumerate()
                .filter(|&(i, _)| i != p.entrance)
                .map(|(_, n)| n)
                .collect();
            let expected = if non_entrance.is_empty() {
                0.0
            } else {
                let on_realm = non_entrance.iter().filter(|n| n.realm.is_some()).count();
                on_realm as f64 / non_entrance.len() as f64
            };
            assert_eq!(
                share, expected,
                "seed {s}: cycle_membership_share disagrees with the hand-derived count"
            );
            if !p.realms.is_empty() && share > 0.0 {
                saw_nonzero = true;
            }
        }
        assert!(
            saw_nonzero,
            "no swept plan had realms and a positive share — the equality check above would be vacuous"
        );
    }
}
