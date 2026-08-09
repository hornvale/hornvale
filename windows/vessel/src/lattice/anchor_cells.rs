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
/// four chamber counts, where its median alone exceeded The Panes'
/// whole-`snapshot()`-plus-JSON figure of 1.249 ms. Testing candidates only
/// until one passes costs one call per anchor in the common case and is the
/// same function of the same inputs.
///
/// What it costs now, over the 4 × 64 rectilinear sweep, stated at three
/// points because a median alone would set a ceiling wrong: **42 µs median
/// (3.4% of 1.249 ms), 410 µs p99 (33%), 437 µs max (35%)**. The tail is where
/// this lands, not the median — the worst cases are the small chambers with
/// five anchors, where the sweep rejects most of what it tries.
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
/// # Where the filter BINDS, and where it is slack
///
/// Measured, not assumed (2026-08-06, Task 2), by removing the filter from
/// this sweep entirely — take the drawn cell, unfiltered — and re-running both
/// corpora of 4 chamber counts × 64 seeds:
///
/// | corpus | unfaithful, filtered | unfaithful, unfiltered |
/// |---|---|---|
/// | rectilinear (`allocate`) | 0 | 0 |
/// | grown (`grow`) | 5 | 10 |
///
/// **On the grown corpus the filter halves the failures, so it binds today.**
/// A grown chamber is a non-convex blob: the sweep finds one-cell-wide
/// corridors (`n=4 seed=22` is three cells in a row; `seed=34` is its vertical
/// twin) where a single anchor IS a cut vertex, and it finds large blobs with
/// a pinch in them (`n=3 seed=0`, 79 floor cells) that fail just as readily.
/// `the_grown_corpus_is_where_the_filter_binds` is the test, and it asserts a
/// measured CEILING rather than universality, because five of those blobs
/// cannot be embedded faithfully at all.
///
/// **On the rectilinear corpus it is slack**, and that is a fact about
/// rectangles: a chamber `allocate` produces is convex, so no five distinct
/// floor cells cut it and the property holds however the anchors land. Since
/// `structure_at` returns `None` unless `brief.built`, production reaches only
/// this corpus today — the grown one is the hostile geometry held in reserve,
/// not a live path.
///
/// # When no cell is admissible: a STATED relaxation, and it FIRES
///
/// If no cell in the sweep keeps the placement faithful, the anchor takes the
/// cell its draw landed on, and the returned placement therefore FAILS
/// [`is_faithful`]. The relaxation is stated rather than silent, and the
/// checker is how it is reported: a caller that needs the guarantee must ASK,
/// because the return type cannot refuse.
///
/// It is not hypothetical. It fires on **5 of 256** grown fixtures and on none
/// of the rectilinear ones. What it guarantees when it fires is what the grown
/// test asserts rather than what this sentence promises: anchors stay distinct,
/// stay inside the chamber, and never outnumber the cells holding them.
///
/// If a chamber has fewer floor cells than the interior has anchors — **3 of
/// 256 grown fixtures**, two of them two-cell chambers — the surplus anchors
/// are left UNPLACED (absent from the map) rather than stacked, since stacking
/// would put two creatures in one cell for a reason no rule would report. A
/// caller must therefore treat a missing `AnchorId` as possible, not as a bug.
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

    /// The brief that selects the GROWN embedding, passed to `embed_with` and
    /// to NOTHING else.
    ///
    /// A structure exists only where `brief.built` (`structure_at` returns
    /// `None` otherwise), so this brief cannot derive a structure or an
    /// interior — and `chamber_interior_of` would debug-assert against it,
    /// since the terrain here reports built. It selects a METHOD, which is
    /// exactly how `lattice/mod.rs`, `render.rs` and `classify.rs` already use
    /// it: the grown lattice is the hostile geometry, reachable as a fixture
    /// and not reachable in production.
    fn wild() -> Brief {
        Brief::from_parts(None, None, None, None, 0, false, true)
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
        fixture_embedded_with(chamber_count, seed, &brief())
    }

    /// [`fixture`], with the embedding METHOD chosen by `method` — the built
    /// brief for the rectilinear allocation production always takes, the wild
    /// one for the grown blob it never does.
    fn fixture_embedded_with(
        chamber_count: usize,
        seed: Seed,
        method: &Brief,
    ) -> (Interior, Lattice, usize) {
        let (locale, structure) = structure_of(chamber_count, seed);
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [locale.pack().expect("a walk-band locale packs").0]
                .into_iter()
                .collect(),
        };
        // Keyed to the LOCALE's own seed, exactly as `Session::lattice_of` does.
        let lattice = embed_with(
            &structure,
            method,
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
    /// **This corpus alone does not test the scan**, and the companion test
    /// does. Measured: this test also passes with the scan's faithfulness
    /// filter removed, because an `allocate` chamber is a rectangle and no
    /// five distinct floor cells cut a rectangle. It is a real property of the
    /// result and it is checked here — it is the corpus production actually
    /// reaches — but the claim that the SCAN earns it lives in
    /// [`the_grown_corpus_is_where_the_filter_binds`], where removing the
    /// filter doubles the failures.
    /// claim: invariant(forall-seed) — over 1..=MAX_CHAMBERS x 0..64
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

    /// How many of the 256 grown fixtures the scan cannot place faithfully.
    ///
    /// A measured ceiling, not a target: the number of chambers whose geometry
    /// defeats a placement scan, which is what the stated relaxation exists
    /// for. Lowering it is an improvement and should be re-pinned here;
    /// RAISING it means the scan got worse, and that is what this number is
    /// here to make impossible to do quietly. Removing the scan's filter
    /// entirely doubles it, which is the measurement that shows the filter is
    /// load-bearing.
    /// type-audit: bare-ok(count)
    const GROWN_RELAXATIONS: usize = 5;

    /// The same sweep as [`every_placement_is_faithful`], against the GROWN
    /// embedding — and this is the one where the scan's filter does work.
    ///
    /// **Why a second corpus exists at all.** A rectilinear chamber is a
    /// rectangle, so no five distinct floor cells can cut it and the property
    /// holds however the anchors land. A grown chamber is a non-convex blob:
    /// the sweep finds one-cell-wide corridors (`n=4 seed=22` is three cells
    /// in a row, `seed=34` its vertical twin), where a single anchor IS a cut
    /// vertex. That is the geometry the filter was written for, and without a
    /// corpus containing it the campaign's keystone property was a claim about
    /// rectangles.
    ///
    /// **Unreachable in production, and that is why it is a fixture.**
    /// `structure_at` returns `None` unless `brief.built` (`structure.rs`),
    /// and `brief_of` truncates to the walk band before asking `is_built`, so
    /// `Session::lattice_of` always dispatches to `allocate`. This is the
    /// hostile case held in reserve against the day a wild place gets
    /// chambers — not a live defect.
    ///
    /// **What it asserts is a CEILING, not universality**, because the honest
    /// answer is that some blobs cannot be embedded faithfully at all: five of
    /// 256 fall back to the stated relaxation. Asserting zero here would be
    /// asserting something false.
    /// claim: rate(forall-seed, unfaithful.len() <= GROWN_RELAXATIONS, measured
    /// ceiling) — with a non-vacuity guard (surplus not empty), over 256 cases
    #[test]
    fn the_grown_corpus_is_where_the_filter_binds() {
        let mut unfaithful: Vec<(usize, u64, usize, usize, usize)> = Vec::new();
        let mut surplus: Vec<(usize, u64, usize, usize, usize)> = Vec::new();
        for n in 1..=crate::structure::MAX_CHAMBERS {
            for seed in 0u64..64 {
                let (interior, lattice, chamber) = fixture_embedded_with(n, Seed(seed), &wild());
                let placed = anchor_cells(&interior, &lattice, chamber, Seed(seed));
                let floor = floor_of(&lattice, chamber).len();
                let anchors = interior.ids().len();
                if !is_faithful(&interior, &lattice, &placed) {
                    unfaithful.push((n, seed, chamber, floor, anchors));
                }
                if placed.len() < anchors {
                    surplus.push((n, seed, chamber, floor, anchors));
                }
                // Whatever the relaxation does, it does these three things:
                // it never stacks two anchors, never leaves the chamber, and
                // never places more anchors than there are cells to hold.
                let distinct: BTreeSet<Cell> = placed.values().copied().collect();
                assert_eq!(
                    distinct.len(),
                    placed.len(),
                    "n={n} seed={seed}: the relaxation stacked two anchors"
                );
                assert!(
                    placed.len() <= floor,
                    "n={n} seed={seed}: {} anchors placed in {floor} cells",
                    placed.len()
                );
                for (id, cell) in &placed {
                    assert!(
                        kind_of(&lattice, *cell).is_some_and(|k| k.serves(chamber)),
                        "n={n} seed={seed}: {id:?} left chamber {chamber}"
                    );
                }
            }
        }
        eprintln!(
            "GROWN: cases=256 unfaithful={} surplus-unplaced={}\n  unfaithful (n, seed, chamber, floor, anchors): {unfaithful:?}\n  surplus: {surplus:?}",
            unfaithful.len(),
            surplus.len()
        );
        assert!(
            !surplus.is_empty(),
            "no grown chamber holds fewer cells than its interior holds anchors, so \
             the surplus-unplaced branch is untested here"
        );
        assert!(
            unfaithful.len() <= GROWN_RELAXATIONS,
            "{} of 256 grown placements are unfaithful, over the measured ceiling of \
             {GROWN_RELAXATIONS}: {unfaithful:?}",
            unfaithful.len()
        );
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

    /// claim: reachability(seed: 0..16) — non-degeneracy: the seed is not
    /// ignored
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

    /// claim: invariant(forall-seed) — over 1..=MAX_CHAMBERS x 0..16
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
