//! Rectilinear allocation: BSP run INVERSELY.
//!
//! Wolverson's chapter 4 splits a rectangle to *invent* rooms. This splits one
//! to *allocate* space among chambers that already exist, so LINKED chambers
//! share the wall their doorway sits in and adjacency is realized by
//! construction rather than checked afterwards.
//!
//! `structure.links` is a rooted TREE (`structure.rs` invariant 2), so the
//! recursion is over the tree: a node takes a slice of its region and hands the
//! remainder to its children, one strip each, every strip cut ACROSS the node's
//! own cut so that it touches the node's slice. A CHAIN is the `k = 1` case of
//! that recursion — one child, no strip cut, the remainder passed straight down
//! — which is why a path graph still gets the cuts it always did, in the same
//! order, from the same draws. Integer arithmetic only — no float enters world
//! identity.
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
/// plumb: universal(a fixed geometry bound on a chamber's interior size)
pub const MIN_CHAMBER_SPAN: i32 = 2;

/// Embed `structure` in `extent`.
///
/// A node's own slice is cut along the longer axis of the region it is given;
/// its children's strips are cut across that line. Every cut sits at a position
/// the seed chooses within the band that leaves both sides at least
/// `MIN_CHAMBER_SPAN` — that band IS the residual degree of freedom, and the seed
/// fills exactly it, once per edge of the tree.
pub fn allocate(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed
        .derive(crate::streams::ROOM_LAYOUT_RECTILINEAR)
        .stream();
    // Placed by the recursion rather than pushed in index order, because a
    // child's slice is decided when its PARENT is placed, not when its index
    // comes up. `None` until placed, so a chamber the tree never reaches is a
    // loud panic below rather than a silently missing rect.
    let mut interiors: Vec<Option<Rect>> = vec![None; n];
    // Counted where the draw happens, never inferred from the loop shape: the
    // field exists to catch a future edit that spends a draw somewhere new, and a
    // number derived from `n` could not.
    let mut dof: u32 = 0;
    // The shell is fabric, so the recursion never sees it. Inset here rather than
    // at each split: one place to be wrong, and §7 rule 3(i) reads the result back.
    place(
        structure,
        0,
        extent.inset(1),
        &mut interiors,
        &mut stream,
        &mut dof,
    );
    let interiors: Vec<Rect> = interiors
        .into_iter()
        .enumerate()
        .map(|(i, r)| {
            r.unwrap_or_else(|| panic!("chamber {i} was never placed: the tree is not rooted at 0"))
        })
        .collect();

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

/// Give `node` a slice of `region` and hand the remainder to its children, one
/// strip each, every strip sharing a wall with the node's slice.
///
/// The node's cut runs along the region's LONGER axis (the axis [`split`] picks),
/// so a chain reproduces the old chain recursion cut for cut and draw for draw.
/// The children's strips are cut PERPENDICULAR to that line, so each one keeps
/// the whole of the remainder's edge that faces the node — that is what makes
/// every specified link one wall apart BY CONSTRUCTION, which [`carve`] requires
/// and §7 rule 1 reads back.
///
/// Both branches of [`split_axis`] keep `r.x` and `r.y`, so a child's own slice
/// still touches that facing edge however deep the recursion goes: a child takes
/// the near half of its strip, never the far one.
///
/// One cut per EDGE of the tree, and no more — a node with `c` children spends
/// one cut for its own slice plus `c - 1` strip cuts, which sums over the tree to
/// `n - 1`. That is [`super::classify::freedom_of_a_tree`], and §7 rule 7 holds
/// the allocator to it exactly.
/// type-audit: bare-ok(index: node)
fn place(
    structure: &Structure,
    node: usize,
    region: Rect,
    interiors: &mut [Option<Rect>],
    stream: &mut hornvale_kernel::Stream,
    dof: &mut u32,
) {
    let children = structure.children(node);
    if children.is_empty() {
        interiors[node] = Some(region);
        return;
    }
    // The node's fair share is one part in however many chambers its subtree
    // holds — the same "how many rooms must still fit in here" question the chain
    // asked as `n - i`, which for a chain is exactly this number.
    let parts = structure.subtree_size(node) as i32;
    // Read from the same helper `split` reads, never re-derived here: two copies
    // of the axis rule could disagree, and the strips would then run ALONG the
    // node's cut instead of across it — every one of them touching the node's
    // slice only by accident, which is the property rule 1 rests on.
    let node_cut_horizontal = longer_axis_is_horizontal(region);
    let (mine, rest) = split(region, parts, stream, dof);
    interiors[node] = Some(mine);
    let mut remaining = rest;
    // How many chambers are still waiting for a share of `remaining`. Counted
    // down as each strip is cut off, so `left / share` stays an honest "how many
    // equal shares of what is left does this subtree deserve".
    let mut left = children
        .iter()
        .map(|&c| structure.subtree_size(c) as i32)
        .sum::<i32>();
    for (k, &child) in children.iter().enumerate() {
        if k + 1 == children.len() {
            // The last child takes what is left whole, exactly as the chain's
            // last chamber did. No cut, so no draw — which is what keeps the
            // budget at one per edge.
            place(structure, child, remaining, interiors, stream, dof);
            break;
        }
        let share = structure.subtree_size(child) as i32;
        debug_assert!(
            left / share >= 1,
            "a child's subtree cannot be larger than what is left to divide"
        );
        // Strips run across the node's cut: cut the OTHER axis.
        let (strip, more) = split_axis(remaining, !node_cut_horizontal, left / share, stream, dof);
        left -= share;
        place(structure, child, strip, interiors, stream, dof);
        remaining = more;
    }
}

/// The longer-axis cut every node takes for its own slice: `r` into a first
/// interior sized about `1/parts` of it, ONE wall line, and the remainder — so
/// `mine + 1 + rest == r` along the axis cut. The cut position is drawn from the
/// band that keeps both interiors at `MIN_CHAMBER_SPAN` or more.
fn split(r: Rect, parts: i32, stream: &mut hornvale_kernel::Stream, dof: &mut u32) -> (Rect, Rect) {
    split_axis(r, longer_axis_is_horizontal(r), parts, stream, dof)
}

/// Which axis a node's own cut runs along: the WIDTH when `r` is at least as wide
/// as it is tall, ties to the width. The one statement of the rule, so [`place`]
/// can cut its children's strips across it without restating it.
/// type-audit: bare-ok(flag: return)
fn longer_axis_is_horizontal(r: Rect) -> bool {
    r.w >= r.h
}

/// [`split`] with the axis chosen by the caller rather than by the longer side:
/// `horizontal` cuts the WIDTH (a vertical wall line), as `split` does when
/// `r.w >= r.h`.
///
/// One cut routine, two callers, so a strip and a node's own slice cannot drift
/// apart in how they spend the band or the draw.
///
/// `dof` is incremented at the draw itself, not once per call: a band too narrow
/// to jitter consumes nothing, and rule 7 must see that.
/// type-audit: bare-ok(flag: horizontal), bare-ok(count: parts)
fn split_axis(
    r: Rect,
    horizontal: bool,
    parts: i32,
    stream: &mut hornvale_kernel::Stream,
    dof: &mut u32,
) -> (Rect, Rect) {
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
