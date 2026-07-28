//! Rectilinear allocation: BSP run INVERSELY.
//!
//! Wolverson's chapter 4 splits a rectangle to *invent* rooms. This splits one
//! to *allocate* space among chambers that already exist, in `links` order, so
//! consecutive chambers share the edge their doorway sits on and adjacency is
//! realized by construction rather than checked afterwards.
//!
//! `structure_at` builds a PATH graph, so the recursion is a chain: split off
//! the first chamber, recurse on the remainder. Integer arithmetic only — no
//! float enters world identity.

use super::{Cell, Lattice, Rect};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::BTreeSet;

/// The smallest a chamber may be on either axis. Below 2 there is no interior
/// cell to stand in once walls take the boundary.
/// type-audit: bare-ok(count)
pub const MIN_CHAMBER_SPAN: i32 = 2;

/// Embed `structure` in `extent`.
///
/// Splits along the longer axis at each step, at a position the seed chooses
/// within the band that leaves both sides at least `MIN_CHAMBER_SPAN` — that
/// band IS the residual degree of freedom, and the seed fills exactly it.
pub fn allocate(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed
        .derive(crate::streams::ROOM_LAYOUT_RECTILINEAR)
        .stream();
    let mut regions: Vec<Rect> = Vec::with_capacity(n);
    let mut remaining = extent;

    for i in 0..n {
        if i + 1 == n {
            regions.push(remaining);
            break;
        }
        // Give this chamber a fair share of what is left, then let the seed
        // move the cut inside the legal band.
        let parts = (n - i) as i32;
        let (mine, rest) = split(remaining, parts, &mut stream);
        regions.push(mine);
        remaining = rest;
    }

    let doorways = structure
        .links
        .iter()
        .map(|&(a, b)| {
            let cell = shared_edge_cell(regions[a], regions[b]);
            (a, b, cell)
        })
        .collect::<Vec<_>>();

    let walls = walls_between(&regions, &doorways);
    Lattice {
        extent,
        regions,
        walls,
        doorways,
    }
}

/// Split `r` into a first part sized about `1/parts` of it and the remainder,
/// cutting the longer axis. The cut position is drawn from the band that keeps
/// both sides at `MIN_CHAMBER_SPAN` or more.
fn split(r: Rect, parts: i32, stream: &mut hornvale_kernel::Stream) -> (Rect, Rect) {
    let horizontal = r.w >= r.h;
    let span = if horizontal { r.w } else { r.h };
    let ideal = (span / parts).max(MIN_CHAMBER_SPAN);
    let lo = MIN_CHAMBER_SPAN;
    let hi = span - MIN_CHAMBER_SPAN;
    // Jitter the ideal cut inside [lo, hi]; a degenerate band collapses to lo.
    let cut = if hi <= lo {
        lo
    } else {
        let width = (hi - lo + 1) as u64;
        let jitter = (stream.next_u64() % width) as i32;
        // Bias toward `ideal` by averaging it with the jittered position, so
        // shares stay roughly fair while the seed still moves the wall.
        (((ideal + (lo + jitter)) / 2).max(lo)).min(hi)
    };
    if horizontal {
        (
            Rect {
                x: r.x,
                y: r.y,
                w: cut,
                h: r.h,
            },
            Rect {
                x: r.x + cut,
                y: r.y,
                w: r.w - cut,
                h: r.h,
            },
        )
    } else {
        (
            Rect {
                x: r.x,
                y: r.y,
                w: r.w,
                h: cut,
            },
            Rect {
                x: r.x,
                y: r.y + cut,
                w: r.w,
                h: r.h - cut,
            },
        )
    }
}

/// A cell on the shared boundary of two abutting regions, chosen at the
/// midpoint of their overlap so a doorway is never in a corner.
fn shared_edge_cell(a: Rect, b: Rect) -> Cell {
    if a.x + a.w == b.x || b.x + b.w == a.x {
        let x = if a.x + a.w == b.x {
            a.x + a.w - 1
        } else {
            b.x + b.w - 1
        };
        let y0 = a.y.max(b.y);
        let y1 = (a.y + a.h).min(b.y + b.h);
        Cell(x, y0 + (y1 - y0) / 2)
    } else {
        let y = if a.y + a.h == b.y {
            a.y + a.h - 1
        } else {
            b.y + b.h - 1
        };
        let x0 = a.x.max(b.x);
        let x1 = (a.x + a.w).min(b.x + b.w);
        Cell(x0 + (x1 - x0) / 2, y)
    }
}

/// Every cell pair that straddles a region boundary, minus the doorway cells.
/// This is the wall set, and it is derived rather than drawn: a wall exists
/// exactly where two regions meet and no doorway was cut.
fn walls_between(regions: &[Rect], doorways: &[(usize, usize, Cell)]) -> BTreeSet<(Cell, Cell)> {
    let door_cells: BTreeSet<Cell> = doorways.iter().map(|&(_, _, c)| c).collect();
    let mut walls = BTreeSet::new();
    let region_of = |cell: Cell| regions.iter().position(|r| r.contains(cell));
    for r in regions {
        for cx in r.x..(r.x + r.w) {
            for cy in r.y..(r.y + r.h) {
                let here = Cell(cx, cy);
                for (dx, dy) in [(1, 0), (0, 1)] {
                    let there = Cell(cx + dx, cy + dy);
                    if region_of(here) != region_of(there)
                        && region_of(there).is_some()
                        && !door_cells.contains(&here)
                        && !door_cells.contains(&there)
                    {
                        walls.insert((here.min(there), here.max(there)));
                    }
                }
            }
        }
    }
    walls
}
