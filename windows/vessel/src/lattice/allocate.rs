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
use std::collections::{BTreeMap, BTreeSet};

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
    // Counted where the draw happens, never inferred from the loop shape: the
    // field exists to catch a future edit that spends a draw somewhere new, and a
    // number derived from `n` could not.
    let mut dof: u32 = 0;

    for i in 0..n {
        if i + 1 == n {
            regions.push(remaining);
            break;
        }
        // Give this chamber a fair share of what is left, then let the seed
        // move the cut inside the legal band.
        let parts = (n - i) as i32;
        let (mine, rest) = split(remaining, parts, &mut stream, &mut dof);
        regions.push(mine);
        remaining = rest;
    }

    // The rects partition `extent`, so per-cell ownership is exact here rather
    // than a summary — but it is still published, because `classify` must read
    // one authoritative map for both methods and not two.
    let mut owner: BTreeMap<Cell, usize> = BTreeMap::new();
    for (i, r) in regions.iter().enumerate() {
        for cx in r.x..(r.x + r.w) {
            for cy in r.y..(r.y + r.h) {
                owner.insert(Cell(cx, cy), i);
            }
        }
    }

    // The threshold PAIR, not just the cell: the wall derivation must exempt the
    // one boundary crossing this doorway serves, and nothing else at that cell.
    let mut thresholds: BTreeSet<(Cell, Cell)> = BTreeSet::new();
    let mut doorways: Vec<(usize, usize, Cell)> = Vec::with_capacity(structure.links.len());
    for &(a, b) in &structure.links {
        let (near, far) = shared_edge_pair(regions[a], regions[b]);
        thresholds.insert((near.min(far), near.max(far)));
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

/// Split `r` into a first part sized about `1/parts` of it and the remainder,
/// cutting the longer axis. The cut position is drawn from the band that keeps
/// both sides at `MIN_CHAMBER_SPAN` or more.
///
/// `dof` is incremented at the draw itself, not once per call: a band too narrow
/// to jitter consumes nothing, and rule 7 must see that.
fn split(r: Rect, parts: i32, stream: &mut hornvale_kernel::Stream, dof: &mut u32) -> (Rect, Rect) {
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
        let drawn = stream.next_u64();
        *dof += 1;
        let jitter = (drawn % width) as i32;
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

/// The pair of cells a doorway between two abutting regions joins: the cell on
/// the near side of the shared boundary and the one directly across it, at the
/// midpoint of the overlap so a doorway is never in a corner.
///
/// The PAIR rather than the cell, because the wall derivation must know which
/// boundary this doorway opens. Returned so the second cell is always the first
/// plus `(1, 0)` or `(0, 1)`, which is what makes the pair normalizable the same
/// way `walls` is.
fn shared_edge_pair(a: Rect, b: Rect) -> (Cell, Cell) {
    if a.x + a.w == b.x || b.x + b.w == a.x {
        let x = if a.x + a.w == b.x {
            a.x + a.w - 1
        } else {
            b.x + b.w - 1
        };
        let y0 = a.y.max(b.y);
        let y1 = (a.y + a.h).min(b.y + b.h);
        let y = y0 + (y1 - y0) / 2;
        (Cell(x, y), Cell(x + 1, y))
    } else {
        let y = if a.y + a.h == b.y {
            a.y + a.h - 1
        } else {
            b.y + b.h - 1
        };
        let x0 = a.x.max(b.x);
        let x1 = (a.x + a.w).min(b.x + b.w);
        let x = x0 + (x1 - x0) / 2;
        (Cell(x, y), Cell(x, y + 1))
    }
}
