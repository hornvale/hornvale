//! Region growing: the organic embedding, for places nobody built.
//!
//! A cave is not a partition of a rectangle. Each chamber tunnels a short
//! passage out of the previous one, then all of them claim cells outward until
//! the interior is exhausted — so chambers are contiguous blobs rather than
//! rects, and the fabric between two of them is wherever they stopped.
//!
//! Chambers strung along a passage rather than scattered across the extent, and
//! that is a fidelity requirement before it is an aesthetic one: the anchor graph
//! asserts a chain, so consecutive chambers must be SEPARATED BY EXACTLY ONE
//! WALL CELL, and the only way to know they are is to start them that way.
//!
//! # Claim with a separation rule, and never take a cell back
//!
//! This is Task 4b's central move, and it is deliberately not "grow, then carve
//! walls between the blobs". A cell is claimable by chamber `i` only if **no
//! neighbour of it is owned by a different chamber**; whatever is left unclaimed
//! when the flood exhausts itself becomes `Wall`.
//!
//! Three properties fall out by construction rather than by luck, and each one is
//! a rule the checker would otherwise have to hope for:
//!
//! - **§7 rule 2.** Two `Floor` cells of different chambers are never adjacent,
//!   because the separation rule refuses the claim that would make them so.
//! - **§7 rule 8.** Every claimed cell is claimed FROM an adjacent cell of the
//!   same chamber, and nothing is ever removed, so a blob is connected. A
//!   grow-then-carve design would take cells back, and carving a wall through a
//!   concave blob can strand its far half — a sealed pocket of floor, which is
//!   the failure mode walls-as-cells introduces.
//! - **A threshold always has somewhere to go.** Chamber `i` starts TWO cells
//!   from chamber `i - 1`, so exactly one cell sits between them — and that cell
//!   is RESERVED as the threshold there and then, before the flood runs.
//!
//! The reservation is the second thing Task 4b had to get right, and §7 rule 1 is
//! what taught it. Choosing the threshold AFTER the flood, by looking for a wall
//! cell with a floor of each chamber beside it, fails two ways at four chambers:
//! the only such cell can be one where THREE blobs meet, so opening it joins two
//! chambers the graph does not link (an invented relation); and if the search is
//! tightened to demand exactly two, there may be no such cell at all, so the link
//! is dropped and the chamber behind it is a sealed pocket. Both were observed.
//! Reserving up front removes the search: the cell is set aside while its
//! surroundings are still empty, and the separation rule treats a reserved cell as
//! belonging to BOTH its chambers, so no third blob can come to touch it.
//!
//! The authoritative assignment is [`Lattice::cells`], which is total over the
//! extent. This function publishes no bounding rects: Task 4b deleted `regions`
//! because a grown blob's bounding box overlaps its neighbours', and a
//! rect-scanning `region_of` agreed with the truth for exactly one of the two
//! methods (ledger #17).

use super::{Cell, CellKind, HEADINGS, Lattice, Rect, neighbours};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::{BTreeMap, VecDeque};

/// Grow `structure`'s chambers into `extent` as contiguous blobs.
pub fn grow(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT_GROWN).stream();
    // The exterior shell is fabric, so nothing grows into it. That is what makes
    // §7 rule 3(i) — the plan is ENCLOSED — hold for this method without a second
    // pass drawing a border afterwards.
    let interior = extent.inset(1);

    // Seed cells, strung along a PASSAGE rather than scattered into bands.
    //
    // The band version was where §7 rule 1 broke in Task 2, and the reason is
    // worth keeping, because it is the difference between an embedder and a
    // generator. Spreading seeds across nominal x-bands orders the chambers along
    // one axis only, and at the extents `extent_for` derives the plan is about as
    // tall as it is wide — so a y-jitter of many rows swamps an x-separation of
    // four columns, and blob 2 can end up ABOVE blob 1 rather than beside it. The
    // chain the anchor graph asserts is then unembeddable: at seed 67 chambers 1
    // and 2 shared no boundary at all, so link (1,2) could not be realized however
    // the walls were derived. A method that leaves that to chance is deciding the
    // graph's relations for itself, which is the one thing an embedder may not do.
    //
    // So chamber 0 gets a drawn cell, and every later chamber TUNNELS out of the
    // previous one: a direction, then a run. Task 4b moved its first cell from one
    // step out to TWO, so a wall cell sits between the blobs from the start.
    //
    // Exactly two draws per chamber either way, so rule 7's budget is unchanged
    // and a blocked direction cannot move the stream position (the rule
    // `structure_at` follows on its own collisions).
    let mut frontier: Vec<VecDeque<Cell>> = Vec::with_capacity(n);
    let mut owner: BTreeMap<Cell, usize> = BTreeMap::new();
    // Cells set aside as thresholds while the tunnel is still being dug, keyed by
    // the pair of chambers they will join. Held apart from `owner` because a
    // threshold belongs to TWO chambers and an owner map can only hold one — the
    // same reason `CellKind::serves` is a predicate rather than an accessor.
    let mut reserved: BTreeMap<Cell, (usize, usize)> = BTreeMap::new();
    // Counted at the draws themselves. The direction rotation and the fallback
    // scan deliberately consume none, so this stays two per chamber however the
    // collisions fall — the property rule 7 reads when it compares against `2 * n`.
    let mut dof: u32 = 0;
    for i in 0..n {
        let first = stream.next_u64();
        let second = stream.next_u64();
        dof += 2;
        let mut claimed: VecDeque<Cell> = VecDeque::new();
        if i == 0 {
            let drawn = Cell(
                interior.x + (first % interior.w.max(1) as u64) as i32,
                interior.y + (second % interior.h.max(1) as u64) as i32,
            );
            owner.insert(drawn, 0);
            claimed.push_back(drawn);
        } else {
            // Tunnel out of chamber `i - 1`, leaving one cell of fabric behind.
            // The anchor may be any of its cells from which a two-cell step lands
            // somewhere this chamber may claim — `frontier[i - 1]` holds them in
            // claim order, so taking the last keeps the passage moving away from
            // where the previous chamber started rather than doubling back over it.
            //
            // `first` picks the direction and the rotation makes the choice total
            // without a second draw.
            let launch = frontier[i - 1].iter().rev().copied().find_map(|a| {
                rotated(first).into_iter().find_map(|d| {
                    // The SKIPPED cell becomes the threshold, so it must be
                    // reservable; the landing cell must be claimable under the
                    // separation rule.
                    let skip = Cell(a.0 + d.0, a.1 + d.1);
                    let land = Cell(a.0 + 2 * d.0, a.1 + 2 * d.1);
                    (reservable(skip, &owner, &reserved, interior, i - 1, i)
                        && claimable(land, &owner, &reserved, interior, i))
                    .then_some((skip, land, d))
                })
            });
            match launch {
                Some((door, start, d)) => {
                    reserved.insert(door, (i - 1, i));
                    owner.insert(start, i);
                    claimed.push_back(start);
                    // `second` is the run: a passage as long as a chamber's
                    // nominal side at most, which is what spaces the chambers out
                    // now that they are no longer assigned bands. The run STOPS at
                    // the first cell the separation rule refuses rather than
                    // skipping it, because a skipped cell would break the blob in
                    // two and rule 8 is exactly what that violates.
                    let run = 1 + (second % super::CHAMBER_SIDE as u64) as i32;
                    let mut at = start;
                    for _ in 1..run {
                        let next = Cell(at.0 + d.0, at.1 + d.1);
                        if !claimable(next, &owner, &reserved, interior, i) {
                            break;
                        }
                        owner.insert(next, i);
                        claimed.push_back(next);
                        at = next;
                    }
                }
                None => {
                    // No two-cell step out of chamber `i - 1` lands anywhere this
                    // chamber may hold with a reservable cell between. Unreachable
                    // while `n <= MAX_CHAMBERS` and the interior has room — 4
                    // chambers in 17x17 cells cannot box one another in — and
                    // handled by a seed-free scan rather than an `expect`, because
                    // rule 1 failing loudly on an unrealized link is a better
                    // report than a panic inside a derivation. No threshold is
                    // reserved on this path, so the link really is unrealized and
                    // the checker says which one.
                    if let Some(c) = first_claimable(&owner, &reserved, interior, i) {
                        owner.insert(c, i);
                        claimed.push_back(c);
                    }
                }
            }
        }
        frontier.push(claimed);
    }

    // Round-robin flood, so no chamber starves. BTreeMap keeps the order total.
    //
    // FIFO, and that is the whole difference between a flood and a snake. Popping
    // the frontier's END makes this depth-first: a blob crawls one long tendril
    // instead of spreading, and a tendril can slither PAST a neighbour and wrap
    // around behind it. At seed 6 in Task 2 that put chamber 1 to the left of
    // chamber 0, which left chambers 1 and 2 not touching at all. Taking the
    // oldest cell instead grows each blob outward at an even radius, so blobs
    // seeded along a passage stay in that order and consecutive chambers stay one
    // wall apart. (VecDeque, not a Vec used as a queue: iteration order is
    // positional, so determinism is unaffected — the precedent is
    // `scene/src/surrounds.rs`.)
    //
    // The loop drains the frontiers rather than watching for a pass that claimed
    // nothing: a chamber can pop a cell all of whose neighbours are refused and
    // make no progress while still holding frontier cells that would. Termination
    // is by exhaustion instead — every cell is pushed at most once (it is claimed
    // as it is pushed) and each iteration pops one.
    //
    // Unlike Task 2's flood, this one does NOT expect to claim every interior
    // cell. What the separation rule refuses is what the fabric is made of.
    while frontier.iter().any(|f| !f.is_empty()) {
        for (i, mine) in frontier.iter_mut().enumerate() {
            let Some(from) = mine.pop_front() else {
                continue;
            };
            for next in neighbours(from) {
                if !claimable(next, &owner, &reserved, interior, i) {
                    continue;
                }
                owner.insert(next, i);
                mine.push_back(next);
            }
        }
    }

    // Total over the extent, in one pass: claimed is floor, reserved is threshold,
    // everything else — the exterior shell and whatever the separation rule refused
    // — is fabric.
    let mut cells: BTreeMap<Cell, CellKind> = BTreeMap::new();
    for cx in extent.x..(extent.x + extent.w) {
        for cy in extent.y..(extent.y + extent.h) {
            let c = Cell(cx, cy);
            let kind = match (owner.get(&c), reserved.get(&c)) {
                // A reserved cell is never claimed — `is_free` excludes both maps
                // — so these arms do not overlap in practice. Reservation wins if
                // they ever do: losing a threshold the tunnel promised strands a
                // chamber, where gaining one floor cell costs nothing.
                (_, Some(&(a, b))) => CellKind::Threshold(a, b),
                (Some(&i), None) => CellKind::Floor(i),
                (None, None) => CellKind::Wall,
            };
            cells.insert(c, kind);
        }
    }

    // Read back off the reservations rather than searched for: the tunnel already
    // decided where each doorway is, and a second search over the finished
    // geometry is exactly the two-passes-one-geometry shape Task 3 found defects
    // in twice.
    //
    // A link the tunnel did not reserve — anything but the chain `structure_at`
    // guarantees (`structure.rs` invariant 2), or a chamber the launch search
    // could not place — gets a doorway at the interior's origin, which is floor
    // rather than a threshold. Deliberately not papered over: nothing opens, and
    // §7 rules 1 and 3 both fail loudly on the link the grower could not realize.
    let mut doorways: Vec<(usize, usize, Cell)> = Vec::with_capacity(structure.links.len());
    for &(a, b) in &structure.links {
        let key = (a.min(b), a.max(b));
        let cell = reserved
            .iter()
            .find(|&(_, &(x, y))| (x.min(y), x.max(y)) == key)
            .map(|(c, _)| *c)
            .unwrap_or(Cell(interior.x, interior.y));
        doorways.push((a, b, cell));
    }

    Lattice {
        extent,
        cells,
        doorways,
        dof,
    }
}

/// Is `cell` inside `interior` and spoken for by nobody?
///
/// Reserved counts as spoken for: a threshold is not a cell a blob may swallow.
/// type-audit: bare-ok(flag: return)
fn is_free(
    cell: Cell,
    owner: &BTreeMap<Cell, usize>,
    reserved: &BTreeMap<Cell, (usize, usize)>,
    interior: Rect,
) -> bool {
    interior.contains(cell) && !owner.contains_key(&cell) && !reserved.contains_key(&cell)
}

/// May chamber `i` claim `cell`? **The separation rule.**
///
/// Free, inside the interior, and with no neighbour spoken for by any OTHER
/// chamber — a neighbour owned by `i` itself is not merely allowed but expected,
/// since that is how a blob grows, and a reserved neighbour is allowed exactly
/// when `i` is one of the two chambers it joins.
///
/// That last clause is what keeps a reserved threshold's floor neighbours down to
/// the two chambers it names. Without it a third blob could grow up beside the
/// reserved cell, and opening it would then join three chambers where the graph
/// links two — which §7 rule 1 reports as an invented relation, and did.
/// type-audit: bare-ok(index: i), bare-ok(flag: return)
fn claimable(
    cell: Cell,
    owner: &BTreeMap<Cell, usize>,
    reserved: &BTreeMap<Cell, (usize, usize)>,
    interior: Rect,
    i: usize,
) -> bool {
    is_free(cell, owner, reserved, interior)
        && neighbours(cell).iter().all(|n| {
            owner.get(n).is_none_or(|&o| o == i)
                && reserved.get(n).is_none_or(|&(a, b)| a == i || b == i)
        })
}

/// May `cell` be set aside as the threshold joining chambers `a` and `b`?
///
/// Free, inside the interior — so a doorway is never punched through the exterior
/// shell, which §7 rule 3(i) checks — with every already-owned neighbour belonging
/// to `a` or `b`, and with no reserved neighbour at all.
///
/// The no-reserved-neighbour clause forbids two thresholds side by side. Two
/// adjacent thresholds form a run a mover crosses in two steps, so the run joins
/// every chamber either cell touches; `classify::realized_links` reads thresholds
/// as connected runs for exactly that reason, and this is the construction side of
/// the same fact.
/// type-audit: bare-ok(index: a), bare-ok(index: b), bare-ok(flag: return)
fn reservable(
    cell: Cell,
    owner: &BTreeMap<Cell, usize>,
    reserved: &BTreeMap<Cell, (usize, usize)>,
    interior: Rect,
    a: usize,
    b: usize,
) -> bool {
    is_free(cell, owner, reserved, interior)
        && neighbours(cell)
            .iter()
            .all(|n| owner.get(n).is_none_or(|&o| o == a || o == b) && !reserved.contains_key(n))
}

/// [`HEADINGS`] rotated to start at `draw`.
///
/// All four in a reproducible order, so a direction is CHOSEN from one draw while
/// the choice stays total: a blocked direction falls through to the next rather
/// than costing a second draw, which is what keeps rule 7's budget at two per
/// chamber however the collisions fall.
fn rotated(draw: u64) -> [(i32, i32); 4] {
    let start = (draw % HEADINGS.len() as u64) as usize;
    [0, 1, 2, 3].map(|k| HEADINGS[(start + k) % HEADINGS.len()])
}

/// The first cell in row-major order that chamber `i` may claim. Consumes no
/// draw, so a collision cannot shift the stream.
/// type-audit: bare-ok(index: i)
fn first_claimable(
    owner: &BTreeMap<Cell, usize>,
    reserved: &BTreeMap<Cell, (usize, usize)>,
    interior: Rect,
    i: usize,
) -> Option<Cell> {
    (interior.y..(interior.y + interior.h))
        .flat_map(|cy| (interior.x..(interior.x + interior.w)).map(move |cx| Cell(cx, cy)))
        .find(|c| claimable(*c, owner, reserved, interior, i))
}
