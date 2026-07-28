//! Region growing: the organic embedding, for places nobody built.
//!
//! A cave is not a partition of a rectangle. Each chamber tunnels a short
//! passage out of the previous one, then all of them claim cells outward until
//! the extent is exhausted — so regions are contiguous blobs rather than rects,
//! and the boundary between two of them is wherever they met.
//!
//! Chambers strung along a passage rather than scattered across the extent, and
//! that is a fidelity requirement before it is an aesthetic one: the anchor graph
//! asserts a chain, so consecutive chambers must SHARE A BOUNDARY, and the only
//! way to know they do is to start them adjacent.
//!
//! Returned in the same `Lattice` shape as `allocate`, with one difference the
//! caller must respect: `regions` holds each blob's BOUNDING rect, which is a
//! SUMMARY of a blob rather than the blob, so two grown regions' rects may
//! overlap and `Rect::contains` is necessary-but-not-sufficient for membership.
//! The authoritative assignment is `Lattice::owner`, which this function
//! publishes rather than discards — `classify::region_of` reads that map, because
//! scanning overlapping bounding rects answers a different question.

use super::{Cell, Lattice, Rect};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// Grow `structure`'s chambers into `extent` as contiguous blobs.
pub fn grow(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT_GROWN).stream();

    // Seed cells, strung along a PASSAGE rather than scattered into bands.
    //
    // The band version was where §7 rule 1 broke, and the reason is worth stating,
    // because it is the difference between an embedder and a generator. Spreading
    // seeds across nominal x-bands orders the chambers along one axis only, and at
    // the extents `extent_for` derives the plan is as tall as it is wide (16x16 at
    // four chambers) — so a y-jitter of up to 15 rows swamps an x-separation of 4
    // columns, and blob 2 can end up ABOVE blob 1 rather than beside it. The chain
    // the anchor graph asserts is then unembeddable: at seed 67 chambers 1 and 2
    // shared no boundary at all, so link (1,2) could not be realized however the
    // walls were derived. A method that leaves that to chance is deciding the
    // graph's relations for itself, which is the one thing an embedder may not do.
    //
    // So chamber 0 gets a drawn cell, and every later chamber TUNNELS out of the
    // previous one: a direction, then a run. Its first cell neighbours a cell
    // chamber `i - 1` already owns and ownership is never reassigned, so
    // consecutive chambers touch BY CONSTRUCTION and rule 1's first direction is a
    // property of the method rather than a hope about the flood. This reads as a
    // cave too — chambers strung along a passage is what a cave system IS, where
    // scattered blobs were a Voronoi diagram wearing a cave's name.
    //
    // Exactly two draws per chamber either way, so rule 7's budget is unchanged
    // and a collision cannot move the stream position (the rule `structure_at`
    // follows on its own collisions).
    let mut frontier: Vec<VecDeque<Cell>> = Vec::with_capacity(n);
    let mut owner: BTreeMap<Cell, usize> = BTreeMap::new();
    // Counted at the draws themselves. `free_from` and the direction rotation
    // deliberately consume none, so this stays two per chamber however the
    // collisions fall — the property rule 7 reads when it compares against `2 * n`.
    let mut dof: u32 = 0;
    for i in 0..n {
        let first = stream.next_u64();
        let second = stream.next_u64();
        dof += 2;
        let mut claimed: VecDeque<Cell> = VecDeque::new();
        if i == 0 {
            let drawn = Cell(
                extent.x + (first % extent.w.max(1) as u64) as i32,
                extent.y + (second % extent.h.max(1) as u64) as i32,
            );
            // Unreachable for the first chamber, which claims nothing before it.
            // Kept because the alternative is worse than dead code: a chamber
            // whose seed cell was already claimed would start with a frontier cell
            // it does not own and could end up owning NOTHING, which `bounding`
            // would then paper over with a 1x1 rect.
            let c = free_from(drawn, &owner, extent);
            owner.insert(c, i);
            claimed.push_back(c);
        } else {
            // Tunnel out of chamber `i - 1`. The anchor may be any of its cells
            // with a free neighbour — `frontier[i - 1]` holds them in claim order,
            // so taking the last keeps the passage moving away from where the
            // previous chamber started rather than doubling back over it.
            let anchor = frontier[i - 1]
                .iter()
                .rev()
                .copied()
                .find(|c| free_neighbour(*c, &owner, extent).is_some());
            // `first` picks the direction, and the rotation makes the choice
            // total without a second draw. `second` is the run: a passage as
            // long as a chamber's nominal side at most, which is what spaces the
            // chambers out now that they are no longer assigned bands.
            let run = 1 + (second % super::CHAMBER_SIDE as u64) as i32;
            let mut at = anchor;
            let mut heading: Option<(i32, i32)> = None;
            for _ in 0..run {
                let Some(from) = at else { break };
                let step = match heading {
                    // Keep tunnelling the same way while the way is open; the
                    // direction is drawn once, not once per cell.
                    Some(d) if is_free(Cell(from.0 + d.0, from.1 + d.1), &owner, extent) => Some(d),
                    _ => turn_from(from, first, &owner, extent),
                };
                let Some(d) = step else { break };
                heading = Some(d);
                let next = Cell(from.0 + d.0, from.1 + d.1);
                owner.insert(next, i);
                claimed.push_back(next);
                at = Some(next);
            }
            if claimed.is_empty() {
                // Every cell of chamber `i - 1` is walled in by other chambers, so
                // no passage can leave it. Unreachable while `n <= MAX_CHAMBERS`
                // and the extent has room — 4 chambers in 256 cells cannot enclose
                // one another — and left as a fallback rather than an `expect`
                // because rule 1 failing loudly on an unrealized link is a better
                // report than a panic inside a derivation.
                let c = free_from(Cell(extent.x, extent.y), &owner, extent);
                owner.insert(c, i);
                claimed.push_back(c);
            }
        }
        frontier.push(claimed);
    }

    // Round-robin flood, so no chamber starves. BTreeMap keeps the order total.
    //
    // FIFO, and that is the whole difference between a flood and a snake. Popping
    // the frontier's END makes this depth-first: a blob crawls one long tendril
    // instead of spreading, and a tendril can slither PAST a neighbour and wrap
    // around behind it. At seed 6 that put chamber 1 to the left of chamber 0,
    // which left chambers 1 and 2 not touching at all — so link (1,2) could not
    // be realized and §7 rule 1 failed on a lattice the grower called finished.
    // Taking the oldest cell instead grows each blob outward at an even radius, so
    // blobs seeded in ordered x-bands stay in that order and consecutive chambers
    // meet. (VecDeque, not a Vec used as a queue: iteration order is positional,
    // so determinism is unaffected — the precedent is `scene/src/surrounds.rs`.)
    //
    // The loop drains the frontiers rather than watching for a pass that claimed
    // nothing: a chamber can pop a cell whose four neighbours are all owned and
    // make no progress while still holding frontier cells that would, so
    // stopping on "no progress this pass" can leave cells unclaimed — and an
    // unclaimed cell inside the extent is a hole that belongs to no chamber.
    // Termination is by exhaustion instead: every cell is pushed at most once
    // (it is claimed as it is pushed) and each iteration pops one.
    while frontier.iter().any(|f| !f.is_empty()) {
        for (i, mine) in frontier.iter_mut().enumerate() {
            let Some(from) = mine.pop_front() else {
                continue;
            };
            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let next = Cell(from.0 + dx, from.1 + dy);
                if !extent.contains(next) || owner.contains_key(&next) {
                    continue;
                }
                owner.insert(next, i);
                mine.push_back(next);
            }
        }
    }

    let regions = (0..n)
        .map(|i| bounding(&owner, i, extent))
        .collect::<Vec<_>>();
    // As in `allocate`: the threshold is a PAIR, so the wall derivation exempts
    // the one crossing this doorway serves and not every side of its cell.
    let mut thresholds: BTreeSet<(Cell, Cell)> = BTreeSet::new();
    let mut doorways: Vec<(usize, usize, Cell)> = Vec::with_capacity(structure.links.len());
    for &(a, b) in &structure.links {
        let (near, far) = meeting_pair(&owner, a, b, extent);
        if let Some(far) = far {
            thresholds.insert((near.min(far), near.max(far)));
        }
        doorways.push((a, b, near));
    }
    let walls = super::walls_around(&owner, &thresholds);
    Lattice {
        extent,
        regions,
        walls,
        doorways,
        owner,
        dof,
    }
}

/// The four steps a passage may take, in a fixed order so a rotation over them
/// is total and reproducible.
const HEADINGS: [(i32, i32); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

/// Is `cell` inside `extent` and unclaimed?
/// type-audit: bare-ok(flag: return)
fn is_free(cell: Cell, owner: &BTreeMap<Cell, usize>, extent: Rect) -> bool {
    extent.contains(cell) && !owner.contains_key(&cell)
}

/// Any free neighbour of `cell`, in `HEADINGS` order.
fn free_neighbour(cell: Cell, owner: &BTreeMap<Cell, usize>, extent: Rect) -> Option<Cell> {
    HEADINGS
        .iter()
        .map(|(dx, dy)| Cell(cell.0 + dx, cell.1 + dy))
        .find(|c| is_free(*c, owner, extent))
}

/// The heading a passage takes out of `from`: the first open one in `HEADINGS`
/// rotated to start at `draw`. Consumes no draw of its own, so a blocked
/// direction cannot move the stream position.
fn turn_from(
    from: Cell,
    draw: u64,
    owner: &BTreeMap<Cell, usize>,
    extent: Rect,
) -> Option<(i32, i32)> {
    let start = (draw % HEADINGS.len() as u64) as usize;
    (0..HEADINGS.len())
        .map(|k| HEADINGS[(start + k) % HEADINGS.len()])
        .find(|(dx, dy)| is_free(Cell(from.0 + dx, from.1 + dy), owner, extent))
}

/// The first unclaimed cell at or after `from` in row-major order within
/// `extent`, wrapping once. Consumes no draw, so a collision cannot shift the
/// stream. Falls back to `from` only if the extent is entirely claimed, which
/// cannot happen while there are fewer chambers than cells.
fn free_from(from: Cell, owner: &BTreeMap<Cell, usize>, extent: Rect) -> Cell {
    let area = extent.area();
    let start = (from.1 - extent.y) * extent.w + (from.0 - extent.x);
    for step in 0..area {
        let at = (start + step) % area;
        let c = Cell(extent.x + at % extent.w, extent.y + at / extent.w);
        if !owner.contains_key(&c) {
            return c;
        }
    }
    from
}

/// The bounding rect of chamber `i`'s claimed cells.
fn bounding(owner: &BTreeMap<Cell, usize>, i: usize, extent: Rect) -> Rect {
    let mut r: Option<(i32, i32, i32, i32)> = None;
    for (c, &o) in owner {
        if o != i {
            continue;
        }
        r = Some(match r {
            None => (c.0, c.1, c.0, c.1),
            Some((x0, y0, x1, y1)) => (x0.min(c.0), y0.min(c.1), x1.max(c.0), y1.max(c.1)),
        });
    }
    match r {
        Some((x0, y0, x1, y1)) => Rect {
            x: x0,
            y: y0,
            w: x1 - x0 + 1,
            h: y1 - y0 + 1,
        },
        // Unreachable while every chamber holds its seed cell, which `free_from`
        // guarantees. Kept honest rather than removed: a 1x1 at the origin is
        // what a chamber owning nothing would look like, and §7 rule 1 is what
        // should catch that, not this arm hiding it.
        None => Rect {
            x: extent.x,
            y: extent.y,
            w: 1,
            h: 1,
        },
    }
}

/// A cell owned by `a` and the cell across the boundary owned by `b`: the lowest
/// such cell in `BTreeMap` order, so the choice is total and seed-free.
///
/// The second element is `None` when the two blobs never touch, and that case is
/// deliberately not papered over. The doorway still lands on a cell `a` owns, so
/// it is in the wrong place rather than in nobody's region — but no threshold is
/// exempted, so nothing opens and §7 rule 1 fails loudly on a link the grower
/// could not realize. The passage seeding above is what makes it unreachable;
/// this arm is what would report a regression in that.
fn meeting_pair(
    owner: &BTreeMap<Cell, usize>,
    a: usize,
    b: usize,
    extent: Rect,
) -> (Cell, Option<Cell>) {
    let mut mine: Option<Cell> = None;
    for (c, &o) in owner {
        if o != a {
            continue;
        }
        if mine.is_none() {
            mine = Some(*c);
        }
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let neighbour = Cell(c.0 + dx, c.1 + dy);
            if owner.get(&neighbour) == Some(&b) {
                return (*c, Some(neighbour));
            }
        }
    }
    (mine.unwrap_or(Cell(extent.x, extent.y)), None)
}
