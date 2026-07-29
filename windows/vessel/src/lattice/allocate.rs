//! Rectilinear allocation: BSP run INVERSELY.
//!
//! Wolverson's chapter 4 splits a rectangle to *invent* rooms. This splits one
//! to *allocate* space among chambers that already exist, in `links` order, so
//! consecutive chambers share the wall their doorway sits in and adjacency is
//! realized by construction rather than checked afterwards.
//!
//! `structure_at` builds a PATH graph, so the recursion is a chain: split off
//! the first chamber, recurse on the remainder. Integer arithmetic only — no
//! float enters world identity.
//!
//! # What Task 4b changed here
//!
//! A wall is a cell, so a split CONSUMES one. Splitting a span `L` into two
//! interiors `a` and `b` now means `a + 1 + b == L`, and the chain runs over the
//! extent's INTERIOR — `extent.inset(1)` — because the exterior shell is fabric
//! too. Every cell the chain does not hand to a chamber is `Wall`, which is why
//! there is no second pass deriving walls from the geometry: the walls are the
//! LEFTOVER, and a leftover cannot disagree with the thing it is left over from.

use super::{Cell, CellKind, Lattice, Rect};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::BTreeMap;

/// The smallest a chamber's INTERIOR may be on either axis. Below 2 there is no
/// standing room worth the name once the fabric around it is counted.
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
    let mut interiors: Vec<Rect> = Vec::with_capacity(n);
    // The shell is fabric, so the chain never sees it. Inset here rather than at
    // each split: one place to be wrong, and §7 rule 3(i) reads the result back.
    let mut remaining = extent.inset(1);
    // Counted where the draw happens, never inferred from the loop shape: the
    // field exists to catch a future edit that spends a draw somewhere new, and a
    // number derived from `n` could not.
    let mut dof: u32 = 0;

    for i in 0..n {
        if i + 1 == n {
            interiors.push(remaining);
            break;
        }
        // Give this chamber a fair share of what is left, then let the seed
        // move the cut inside the legal band.
        let parts = (n - i) as i32;
        let (mine, rest) = split(remaining, parts, &mut stream, &mut dof);
        interiors.push(mine);
        remaining = rest;
    }

    // Fabric first, standing room second. Everything is `Wall` until a chamber's
    // interior claims it, so the leftovers are exactly the exterior shell and the
    // split lines — nothing derives a wall from the geometry a second time, which
    // is where Task 3 found two defects.
    let mut cells: BTreeMap<Cell, CellKind> = BTreeMap::new();
    for cx in extent.x..(extent.x + extent.w) {
        for cy in extent.y..(extent.y + extent.h) {
            cells.insert(Cell(cx, cy), CellKind::Wall);
        }
    }
    for (i, r) in interiors.iter().enumerate() {
        for cx in r.x..(r.x + r.w) {
            for cy in r.y..(r.y + r.h) {
                cells.insert(Cell(cx, cy), CellKind::Floor(i));
            }
        }
    }

    // One wall cell per link becomes a threshold. `carve` refuses to open
    // anything that is not fabric, so a doorway can never eat a chamber's floor.
    let mut doorways: Vec<(usize, usize, Cell)> = Vec::with_capacity(structure.links.len());
    for &(a, b) in &structure.links {
        let cell = carve(&mut cells, interiors[a], interiors[b], a, b);
        doorways.push((a, b, cell));
    }

    Lattice {
        extent,
        cells,
        doorways,
        dof,
    }
}

/// Split `r` into a first interior sized about `1/parts` of it, ONE wall line,
/// and the remainder — so `mine + 1 + rest == r` along the axis cut. The cut
/// position is drawn from the band that keeps both interiors at
/// `MIN_CHAMBER_SPAN` or more.
///
/// `dof` is incremented at the draw itself, not once per call: a band too narrow
/// to jitter consumes nothing, and rule 7 must see that.
fn split(r: Rect, parts: i32, stream: &mut hornvale_kernel::Stream, dof: &mut u32) -> (Rect, Rect) {
    let horizontal = r.w >= r.h;
    let span = if horizontal { r.w } else { r.h };
    let ideal = (span / parts).max(MIN_CHAMBER_SPAN);
    let lo = MIN_CHAMBER_SPAN;
    // One cell of the span is spent on the wall line, so the far bound is one
    // tighter than it was under the boundary model. Getting this wrong produces a
    // chamber of width 1 rather than a compile error, which is what
    // `no_chamber_is_degenerate` is for.
    let hi = span - 1 - MIN_CHAMBER_SPAN;
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
                x: r.x + cut + 1,
                y: r.y,
                w: r.w - cut - 1,
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
                y: r.y + cut + 1,
                w: r.w,
                h: r.h - cut - 1,
            },
        )
    }
}

/// Open one cell of the wall line between two interiors into a
/// `Threshold(a, b)`, and return it.
///
/// At the MIDPOINT of the overlap, so a doorway is never in a corner — under the
/// boundary model that was only aesthetics, and now it is load-bearing: a
/// corner-adjacent threshold can touch a third chamber's floor, which §7 rule 1
/// reads as an invented relation.
///
/// Refuses to open anything that is not already fabric. Two interiors the chain
/// did not place one wall apart is unreachable for a path graph, and the honest
/// failure is a doorway that opens nothing — §7 rule 1 fails loudly on the
/// unrealized link — rather than a doorway that eats a chamber's floor and makes
/// the two chambers adjacent for the wrong reason.
fn carve(cells: &mut BTreeMap<Cell, CellKind>, a: Rect, b: Rect, ia: usize, ib: usize) -> Cell {
    let candidate = if a.x + a.w + 1 == b.x || b.x + b.w + 1 == a.x {
        let x = if a.x + a.w + 1 == b.x {
            a.x + a.w
        } else {
            b.x + b.w
        };
        let y0 = a.y.max(b.y);
        let y1 = (a.y + a.h).min(b.y + b.h);
        Cell(x, y0 + (y1 - y0) / 2)
    } else {
        let y = if a.y + a.h + 1 == b.y {
            a.y + a.h
        } else {
            b.y + b.h
        };
        let x0 = a.x.max(b.x);
        let x1 = (a.x + a.w).min(b.x + b.w);
        Cell(x0 + (x1 - x0) / 2, y)
    };
    if cells.get(&candidate) == Some(&CellKind::Wall) {
        cells.insert(candidate, CellKind::Threshold(ia, ib));
    }
    candidate
}
