//! `anchor_cells` — the JOIN: a chamber's anchors placed at lattice cells.
//!
//! Hornvale's fine layer is two layers that had never met. The **relational**
//! one is [`crate::interior::Interior`] — anchors and the RCC-8 relations
//! between them, where creatures actually stand
//! ([`crate::liveness::Occupancy`] holds `(RoomAddr, AnchorId)`). The
//! **metric** one is [`Lattice`] — cells of one grid, which is what gets
//! drawn. This module is the map from the first to the second.
//!
//! # This is an EMBEDDING, and the interior is authoritative
//!
//! In the graph-drawing sense: the anchor graph is the thing, and a placement
//! is one drawing of it. Anchors are **discovered** — derived from the brief,
//! seeded, the same for every reader of that world. Cell geometry is
//! **invented**, and [`Lattice::dof`] already counts the invention ("how many
//! independent choices the embedder made — one per stream draw it consumed"),
//! because inventing beyond what the graph leaves free "is the one thing an
//! embedder may not do". The same budget binds here, and [`place`] reports its
//! own count rather than having one recomputed for it.
//!
//! So an outcome must never read a `Cell` where it could read a relation. If
//! two anchors are adjacent, that is a fact about the room; which cell each
//! landed on is a fact about this drawing of it.
//!
//! # Nothing derived from a placement may be committed (decision 0069)
//!
//! A placement is `FRAME`-tier, like the lattice it lands in: derived on
//! entry, discarded on exit, never serialized and never a fact's object. That
//! is what lets the placement **change between versions without corrupting any
//! world** — and it is exactly why nothing derived from it may be committed. A
//! fact keyed on a cell, or a belief that accumulated because a creature was
//! *drawn* somewhere, would point into a layer that is free to regenerate
//! differently forever, and the world would silently rot the day this scan is
//! retuned. Sight may decide what a client is SHOWN; it may not decide what an
//! agent comes to BELIEVE.

use crate::interior::{AnchorId, Interior};
use crate::lattice::{Cell, CellKind, Lattice, kind_of, neighbours};
use crate::streams::ROOM_LAYOUT_ANCHORS;
use hornvale_kernel::Seed;
use std::collections::{BTreeMap, BTreeSet};

/// Place every anchor of `chamber`'s interior at a cell of `lattice`.
///
/// See [`place`] for the algorithm and the degrees of freedom it spends; this
/// is that function with the choice count dropped, which is the form every
/// caller wants.
/// type-audit: bare-ok(index: chamber)
pub fn anchor_cells(
    interior: &Interior,
    lattice: &Lattice,
    chamber: usize,
    seed: Seed,
) -> BTreeMap<AnchorId, Cell> {
    place(interior, lattice, chamber, seed).0
}

/// Whether `placement` is a **faithful** drawing of `interior` (spec §2.2).
///
/// Three conditions, and the third is §2.2's own:
///
/// 1. every placed cell is inside the extent and PASSABLE — an anchor in the
///    building's fabric is not a place a creature can stand;
/// 2. no two anchors share a cell — without this the checker would accept the
///    maximally degenerate drawing, where every anchor collapses onto one cell
///    and every "path between" two of them is the empty path;
/// 3. anchors adjacent in the interior are placed at cells with a passable
///    path between them **that crosses no third anchor's cell**.
///
/// # Over `walkable_neighbors`, not `neighbors`
///
/// The spec's §2.2 says "adjacent in `Interior.adjacency`", which is
/// [`Interior::neighbors`]. This checks the strictly WIDER relation
/// [`Interior::walkable_neighbors`] — adjacency (`Ec`) plus containment
/// (`Ntpp`) in either direction — so it satisfies §2.2 and asks for more.
/// Two reasons:
///
/// - **It is the interior layer's single definition of one walkable hop.**
///   `route_within`'s A\* successors, `Occupancy::walk`'s adjacency check and
///   `Interior::is_connected` all call it rather than re-deriving their own.
///   Faithfulness over the narrower relation would let a hearth CONTAINED by a
///   hall be drawn across the room behind a third anchor: a step the relational
///   layer calls legal, and that the drawn plan shows no way to take.
/// - **Non-vacuity.** `pattern::compose` attaches with `Attach::Within` as
///   readily as `Attach::Beside`, and a contained anchor gets NO adjacency
///   edge (it is "already linked by containment"). A composition whose every
///   edge is containment would make a `neighbors`-only check true by having
///   nothing to check.
///
/// # Partial placements
///
/// A pair is checked only when BOTH its anchors are placed, so this is
/// meaningful mid-scan on a partial map — which is what [`place`] accepts a
/// candidate against. On a complete placement (what `anchor_cells` returns)
/// every pair is checked.
/// type-audit: bare-ok(flag: return)
pub fn is_faithful(
    interior: &Interior,
    lattice: &Lattice,
    placement: &BTreeMap<AnchorId, Cell>,
) -> bool {
    let occupied: BTreeSet<Cell> = placement.values().copied().collect();
    if occupied.len() != placement.len() {
        return false;
    }
    for cell in &occupied {
        if !kind_of(lattice, *cell).is_some_and(|k| k.passable()) {
            return false;
        }
    }
    for (&a, &from) in placement {
        for b in interior.walkable_neighbors(a) {
            // Both relations are symmetric, so each unordered pair is checked
            // once, from its lower anchor.
            if b <= a {
                continue;
            }
            let Some(&to) = placement.get(&b) else {
                continue;
            };
            if !reaches(lattice, from, to, &occupied) {
                return false;
            }
        }
    }
    true
}

/// The placement, and how many independent choices making it consumed.
///
/// **A seeded placement scan, not a constraint solver.** The chamber's own
/// floor is walked in ROW-MAJOR order (`y` outer, `x` inner — the order the
/// plan is drawn in, not `BTreeMap<Cell, _>`'s column-major key order), and
/// anchors are assigned in [`Interior::ids`] order. Each anchor takes the
/// first cell — sweeping forward, cyclically, from where its draw starts —
/// that keeps the PARTIAL placement faithful. The whole partial map is
/// re-checked, not only the pairs involving the new anchor, because a later
/// anchor dropped onto a cell some earlier pair's only path ran through would
/// otherwise cut it unseen.
///
/// **The sweep is LAZY, and that is a cost decision with a measurement behind
/// it.** Testing every free cell and then drawing among the admissible ones —
/// the first thing this function did — costs one [`is_faithful`] call per
/// (anchor × free cell): **0.3–2.7 ms per call in release**, measured at the
/// four chamber counts, against The Panes' whole-`snapshot()`-plus-JSON budget
/// of 1.249 ms. Testing candidates only until one passes costs one call per
/// anchor in the common case and is the same function of the same inputs.
///
/// # The degrees of freedom it spends
///
/// One draw per anchor that has more than one free cell, and none otherwise —
/// so the count returned is at most the anchor count, and is the number of
/// anchors whose position the graph genuinely left open. The anchor graph
/// constrains an anchor's position only through faithfulness; where more than
/// one cell is available, choosing where to begin looking is a choice the
/// graph left free, and taking it is not invention. Where one cell is left,
/// there is nothing to choose and no draw is taken.
///
/// # The filter has never yet been observed to BIND
///
/// Measured, not assumed (2026-08-06, Task 2): removing the faithfulness test
/// from this sweep entirely — take the drawn cell, unfiltered — leaves
/// `every_placement_is_faithful` GREEN across all four chamber counts × 64
/// seeds. A chamber's floor is an open region, the interiors the generator
/// composes hold at most five anchors, and no five distinct floor cells cut
/// such a region. So today the property holds because of the FLOOR's topology,
/// not because of this filter, and the property test would pass on an embedder
/// that did not check at all.
///
/// The filter stays, and the reason is stated rather than assumed: it is what
/// makes the guarantee hold BY CONSTRUCTION when any of those three facts
/// moves — a richer interior (The Blocking already added five anchor kinds), a
/// grown lattice's non-convex blob, or the kind-aware placement below. What a
/// reader must NOT do is read that green test as evidence this scan is doing
/// the work; it is not, yet.
///
/// # When no cell is admissible: a STATED relaxation
///
/// If no cell in the sweep keeps the placement faithful, the anchor takes the
/// cell its draw landed on, and the returned placement therefore FAILS
/// [`is_faithful`]. The relaxation is stated rather than silent — the checker
/// is how it is reported, and `every_placement_is_faithful` is what asserts it
/// does not happen for any structure the generator produces. If a chamber has
/// fewer floor cells than the interior has anchors, the surplus anchors are
/// left UNPLACED (absent from the map) rather than stacked, since stacking
/// would put two creatures in one cell for a reason no rule would report.
/// type-audit: bare-ok(index: chamber), bare-ok(count: return)
fn place(
    interior: &Interior,
    lattice: &Lattice,
    chamber: usize,
    seed: Seed,
) -> (BTreeMap<AnchorId, Cell>, u32) {
    let field = floor_of(lattice, chamber);
    let mut placed: BTreeMap<AnchorId, Cell> = BTreeMap::new();
    let mut used: BTreeSet<Cell> = BTreeSet::new();
    let mut choices = 0u32;
    if field.is_empty() {
        return (placed, choices);
    }
    let mut stream = seed.derive(ROOM_LAYOUT_ANCHORS).stream();
    for id in interior.ids() {
        let free: Vec<Cell> = field
            .iter()
            .copied()
            .filter(|c| !used.contains(c))
            .collect();
        if free.is_empty() {
            // Fewer floor cells than anchors: leave the surplus unplaced.
            continue;
        }
        let start = if free.len() > 1 {
            choices += 1;
            (stream.next_u64() % free.len() as u64) as usize
        } else {
            0
        };
        // The sweep starts where the draw says and takes the first cell that
        // keeps the placement faithful. The fallback is the drawn cell itself:
        // the stated relaxation, which `is_faithful` then reports.
        let mut chosen = free[start];
        for k in 0..free.len() {
            let cell = free[(start + k) % free.len()];
            placed.insert(id, cell);
            let ok = is_faithful(interior, lattice, &placed);
            placed.remove(&id);
            if ok {
                chosen = cell;
                break;
            }
        }
        placed.insert(id, chosen);
        used.insert(chosen);
    }
    (placed, choices)
}

/// `chamber`'s own floor cells, in row-major order.
///
/// FLOOR only, though [`CellKind::serves`] would also admit the chamber's
/// thresholds. Same reason `standing_cell` refuses one: a doorway serves two
/// chambers, and a mark drawn on the `+` hides an opening the plan promises is
/// walkable, so the drawn count of doorways would disagree with
/// [`Lattice::doorways`] for a reason no rule reports.
///
/// (A kind-aware placement — the `Alcove` at a wall cell its own doc describes,
/// the `Threshold` anchor at the `Threshold` cell realizing the same link — is
/// the obvious refinement and is NOT v1: a wall cell is impassable, so an
/// anchor at one cannot satisfy faithfulness as §2.2 states it, and widening
/// that is a spec change rather than an implementation one.)
/// type-audit: bare-ok(index: chamber)
fn floor_of(lattice: &Lattice, chamber: usize) -> Vec<Cell> {
    let mut out = Vec::new();
    for y in lattice.extent.y..lattice.extent.y + lattice.extent.h {
        for x in lattice.extent.x..lattice.extent.x + lattice.extent.w {
            let cell = Cell(x, y);
            if kind_of(lattice, cell) == Some(CellKind::Floor(chamber)) {
                out.push(cell);
            }
        }
    }
    out
}

/// Is there a passable path from `from` to `to` that steps on no cell of
/// `blocked` except its own endpoints?
///
/// A flood over the four [`crate::lattice::HEADINGS`] steps — the same
/// orthogonal-only notion of a step the rest of the lattice uses, so a path
/// this reports is a path a mover can actually walk. Depth-first, because the
/// question is WHETHER rather than how far: `reachable_from` floods the same
/// way for the same reason.
/// type-audit: bare-ok(flag: return)
fn reaches(lattice: &Lattice, from: Cell, to: Cell, blocked: &BTreeSet<Cell>) -> bool {
    if from == to {
        return true;
    }
    if !kind_of(lattice, from).is_some_and(|k| k.passable()) {
        return false;
    }
    let mut seen: BTreeSet<Cell> = [from].into_iter().collect();
    let mut frontier = vec![from];
    while let Some(at) = frontier.pop() {
        for n in neighbours(at) {
            if n == to {
                return kind_of(lattice, n).is_some_and(|k| k.passable());
            }
            if blocked.contains(&n) || !kind_of(lattice, n).is_some_and(|k| k.passable()) {
                continue;
            }
            if seen.insert(n) {
                frontier.push(n);
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::lattice::{embed_with, extent_for};
    use crate::liveness::Terrain;
    use crate::structure::{Structure, structure_at};
    use hornvale_kernel::{RoomAddr, WorldTime};

    const WALK: u32 = 12;

    /// A `Terrain` whose built-set is keyed at the WALK band, exactly as
    /// `LocaleTerrain` is — the shape `interior::derive`'s own tests use,
    /// because a raw chamber address must read as unbuilt.
    struct WalkKeyedTerrain {
        built_walk_ids: BTreeSet<u64>,
    }
    impl Terrain for WalkKeyedTerrain {
        fn elevation(&self, _r: &RoomAddr) -> f64 {
            0.0
        }
        fn is_fresh_water(&self, _r: &RoomAddr) -> bool {
            false
        }
        fn temperature(&self, _r: &RoomAddr, _d: WorldTime) -> f64 {
            -20.0
        }
        fn is_built(&self, r: &RoomAddr) -> bool {
            r.pack()
                .ok()
                .is_some_and(|id| self.built_walk_ids.contains(&id.0))
        }
    }

    fn brief() -> Brief {
        // built + cold, which is what `WalkKeyedTerrain` reports at a built
        // locale — `chamber_interior_of` debug-asserts the two agree.
        Brief::from_parts(None, None, None, None, 0, true, true)
    }

    /// The `n`th walk-band locale, `n` written out as base-4 path digits.
    fn locale_number(n: u64) -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| ((n >> (2 * i)) & 0b11) as u8).collect(),
        }
    }

    /// A REAL structure of exactly `chamber_count` chambers, found by scanning
    /// locales rather than hand-built: `structure_at` draws the count, so the
    /// honest way to get a four-chamber structure is to go and find a locale
    /// that has one.
    fn structure_of(chamber_count: usize, seed: Seed) -> (RoomAddr, Structure) {
        for n in 0u64..4096 {
            let locale = locale_number(n);
            let s = structure_at(&locale, &brief(), seed, WALK).expect("built");
            if s.chambers.len() == chamber_count {
                return (locale, s);
            }
        }
        panic!("no locale in 4096 draws a {chamber_count}-chamber structure at {seed:?}");
    }

    /// A chamber's real interior, its structure's real lattice, and which
    /// chamber it is — all through the derivations the session itself calls
    /// (`structure_at`, `embed_with`, `chamber_interior_of`), never built by
    /// hand. The chamber index varies with the seed so a sweep covers every
    /// role rather than one.
    fn fixture(chamber_count: usize, seed: Seed) -> (Interior, Lattice, usize) {
        let (locale, structure) = structure_of(chamber_count, seed);
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [locale.pack().expect("a walk-band locale packs").0]
                .into_iter()
                .collect(),
        };
        // Keyed to the LOCALE's own seed, exactly as `Session::lattice_of` does.
        let lattice = embed_with(
            &structure,
            &brief(),
            extent_for(&structure),
            locale.seed(seed),
        );
        let at = (seed.0 as usize) % chamber_count;
        let interior = crate::interior::chamber_interior_of(
            &structure.chambers[at],
            &terrain,
            WALK,
            &brief(),
            at,
        );
        (interior, lattice, at)
    }

    /// Faithfulness, as §2.2 of the spec defines it: anchors adjacent in the
    /// interior are placed at cells with a passable path between them that
    /// crosses no third anchor's cell.
    ///
    /// A property over every structure the generator produces, not one
    /// example — an embedding can be faithful for a two-anchor room and
    /// scatter a six-anchor one, and only the sweep sees that.
    ///
    /// **Read `place`'s "the filter has never yet been observed to bind"
    /// before trusting this green.** Measured: this test also passes with the
    /// scan's faithfulness test removed. It is a real property of the result
    /// and it is checked here, but on today's corpus it is a claim about the
    /// FLOOR's topology, not about the scan.
    #[test]
    fn every_placement_is_faithful() {
        for n in 1..=crate::structure::MAX_CHAMBERS {
            for seed in 0u64..64 {
                let (interior, lattice, chamber) = fixture(n, Seed(seed));
                let placed = anchor_cells(&interior, &lattice, chamber, Seed(seed));
                assert!(
                    is_faithful(&interior, &lattice, &placed),
                    "n={n} seed={seed}: adjacent anchors were placed without a \
                     passable path between them"
                );
            }
        }
    }

    #[test]
    fn every_anchor_is_placed_exactly_once() {
        let (interior, lattice, chamber) = fixture(2, Seed(7));
        let placed = anchor_cells(&interior, &lattice, chamber, Seed(7));
        assert_eq!(
            placed.len(),
            interior.ids().len(),
            "not every anchor got a cell"
        );
        let mut cells: Vec<Cell> = placed.values().copied().collect();
        cells.sort();
        cells.dedup();
        assert_eq!(cells.len(), placed.len(), "two anchors share one cell");
    }

    #[test]
    fn every_placed_cell_serves_this_chamber() {
        let (interior, lattice, chamber) = fixture(3, Seed(11));
        let placed = anchor_cells(&interior, &lattice, chamber, Seed(11));
        for (id, cell) in &placed {
            let kind = lattice.cells.get(cell).expect("placed inside the extent");
            assert!(
                kind.serves(chamber),
                "anchor {id:?} was placed at {cell:?}, which does not serve chamber {chamber}"
            );
        }
    }

    #[test]
    fn the_placement_is_deterministic() {
        let (interior, lattice, chamber) = fixture(2, Seed(3));
        let a = anchor_cells(&interior, &lattice, chamber, Seed(3));
        let b = anchor_cells(&interior, &lattice, chamber, Seed(3));
        assert_eq!(a, b, "same inputs, same placement");
    }

    /// The negative control on `is_faithful`. A checker that returns `true`
    /// for everything would make `every_placement_is_faithful` vacuous, and
    /// that is exactly how a green suite hides a broken embedding.
    #[test]
    fn is_faithful_rejects_a_scattered_placement() {
        let (interior, lattice, chamber) = fixture(2, Seed(5));
        let mut scattered = anchor_cells(&interior, &lattice, chamber, Seed(5));
        assert!(
            scattered.len() > 1,
            "this control needs at least two anchors to have a pair to break"
        );
        // Checked rather than assumed: the corner of the extent is the exterior
        // shell, so it must be impassable for the move below to mean anything.
        let corner = Cell(lattice.extent.x, lattice.extent.y);
        assert_eq!(
            kind_of(&lattice, corner),
            Some(CellKind::Wall),
            "the extent's corner is not the building's fabric"
        );
        // Move one anchor into the fabric, where nothing can path to it.
        if let Some((_, cell)) = scattered.iter_mut().next() {
            *cell = corner;
        }
        assert!(
            !is_faithful(&interior, &lattice, &scattered),
            "is_faithful accepted an anchor placed in the building's fabric"
        );
    }

    #[test]
    fn the_seed_is_read_at_all() {
        // Task 5's negative control perturbs the placement with a different
        // seed and asserts what is DRAWN moves while what is KNOWN does not.
        // If the seed were ignored, that control would be vacuous — so the
        // property it depends on is pinned here, where it belongs.
        let (interior, lattice, chamber) = fixture(4, Seed(1));
        let placements: Vec<BTreeMap<AnchorId, Cell>> = (0..16u64)
            .map(|s| anchor_cells(&interior, &lattice, chamber, Seed(s)))
            .collect();
        assert!(
            placements.iter().any(|p| *p != placements[0]),
            "sixteen seeds placed the anchors identically — the seed is ignored"
        );
    }

    #[test]
    fn the_embedder_invents_no_more_than_the_graph_leaves_free() {
        // §7 rule 7's discipline, applied to this embedder: `Lattice::dof`
        // counts one per draw consumed, and the anchor graph leaves each
        // anchor's own position free and nothing else. So the ceiling is one
        // choice per anchor, and a scan that took two would be inventing a
        // freedom the graph never had.
        for n in 1..=crate::structure::MAX_CHAMBERS {
            for seed in 0u64..16 {
                let (interior, lattice, chamber) = fixture(n, Seed(seed));
                let (placed, dof) = place(&interior, &lattice, chamber, Seed(seed));
                assert!(
                    dof as usize <= interior.ids().len(),
                    "n={n} seed={seed}: the scan spent {dof} choices on {} anchors",
                    interior.ids().len()
                );
                assert_eq!(placed.len(), interior.ids().len(), "n={n} seed={seed}");
            }
        }
    }
}
