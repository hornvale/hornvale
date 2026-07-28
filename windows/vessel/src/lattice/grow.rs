//! Region growing: the organic embedding, for places nobody built.
//!
//! A cave is not a partition of a rectangle. Chambers get seed cells spread
//! across the extent, then claim cells outward in `links` order until the
//! extent is exhausted — so regions are contiguous blobs rather than rects,
//! and the boundary between two of them is wherever they met.
//!
//! Returned in the same `Lattice` shape as `allocate`, with one difference the
//! caller must respect: `regions` holds each blob's BOUNDING rect, which is a
//! SUMMARY of a blob rather than the blob, so two grown regions' rects may
//! overlap and `Rect::contains` is necessary-but-not-sufficient for membership.
//! A `Lattice` carries no per-cell ownership, so the authoritative assignment
//! does not survive this function: `walls` is what a mover must respect, and
//! `classify::region_of` resolves an overlap by first-match.

use super::{Cell, Lattice, Rect};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::{BTreeMap, BTreeSet};

/// Grow `structure`'s chambers into `extent` as contiguous blobs.
pub fn grow(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT_GROWN).stream();

    // Seed cells: deterministic spread, jittered inside each nominal band.
    // Exactly two draws per chamber, always, so a collision cannot move the
    // stream position (the rule `structure_at` follows on its own collisions).
    let mut frontier: Vec<Vec<Cell>> = Vec::with_capacity(n);
    let mut owner: BTreeMap<Cell, usize> = BTreeMap::new();
    for i in 0..n {
        let band = extent.w / n as i32;
        let bx = extent.x + band * i as i32 + (stream.next_u64() % band.max(1) as u64) as i32;
        let by = extent.y + (stream.next_u64() % extent.h.max(1) as u64) as i32;
        let drawn = Cell(
            bx.min(extent.x + extent.w - 1),
            by.min(extent.y + extent.h - 1),
        );
        // The bands do not overlap at the extents `extent_for` derives, so this
        // scan is unreachable today. It is here because the alternative is worse
        // than dead code: a chamber whose seed cell was already claimed would
        // start with a frontier cell it does not own and could end up owning
        // NOTHING, which `bounding` would then paper over with a 1x1 rect.
        let c = free_from(drawn, &owner, extent);
        owner.insert(c, i);
        frontier.push(vec![c]);
    }

    // Round-robin flood, so no chamber starves. BTreeMap keeps the order total.
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
            let Some(from) = mine.pop() else {
                continue;
            };
            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let next = Cell(from.0 + dx, from.1 + dy);
                if !extent.contains(next) || owner.contains_key(&next) {
                    continue;
                }
                owner.insert(next, i);
                mine.push(next);
            }
        }
    }

    let regions = (0..n)
        .map(|i| bounding(&owner, i, extent))
        .collect::<Vec<_>>();
    let doorways = structure
        .links
        .iter()
        .map(|&(a, b)| (a, b, meeting_cell(&owner, a, b, extent)))
        .collect::<Vec<_>>();
    let walls = grown_walls(&owner, &doorways);
    Lattice {
        extent,
        regions,
        walls,
        doorways,
    }
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

/// A cell owned by `a` that touches a cell owned by `b`; the lowest such cell
/// in `BTreeMap` order, so the choice is total and seed-free.
///
/// If the two blobs never touch — which the checker's rule 1 must be free to
/// fail on — this still returns a cell `a` owns, so the doorway is at worst in
/// the wrong place rather than in nobody's region.
fn meeting_cell(owner: &BTreeMap<Cell, usize>, a: usize, b: usize, extent: Rect) -> Cell {
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
                return *c;
            }
        }
    }
    mine.unwrap_or(Cell(extent.x, extent.y))
}

/// Boundaries between differently-owned neighbours, minus doorway cells.
fn grown_walls(
    owner: &BTreeMap<Cell, usize>,
    doorways: &[(usize, usize, Cell)],
) -> BTreeSet<(Cell, Cell)> {
    let door_cells: BTreeSet<Cell> = doorways.iter().map(|&(_, _, c)| c).collect();
    let mut walls = BTreeSet::new();
    for (c, &o) in owner {
        let here = *c;
        for (dx, dy) in [(1, 0), (0, 1)] {
            let there = Cell(here.0 + dx, here.1 + dy);
            if let Some(&other) = owner.get(&there)
                && other != o
                && !door_cells.contains(&here)
                && !door_cells.contains(&there)
            {
                walls.insert((here.min(there), here.max(there)));
            }
        }
    }
    walls
}
