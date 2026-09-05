//! Read the realized graph back off a solved lattice, and check it against the
//! graph that was specified.
//!
//! This is the direction that makes the embedder falsifiable. `allocate` claims
//! to realize `structure.links` by construction; this module does not believe it.
//! Reading adjacency back off the geometry — rather than trusting the code path
//! that wrote it — is what turns Amendment 2 §1b.8's seven rules from prose into
//! assertions.
//!
//! **And an eighth, earned by Task 4b's model change.** Under the boundary model
//! connectivity was guaranteed: regions tiled the extent and doorways linked them,
//! so there was nowhere for a mover to be stranded. Walls as CELLS can seal a
//! pocket of floor, so rule 8 asserts that every floor cell is reachable from the
//! threshold through passable cells only. Amendment 2 listed seven rules; the
//! eighth is not a bonus, it is the cost of the new model paid out loud.
//!
//! Every rule here asks [`CellKind::passable`], never `== CellKind::Wall`. A rule
//! written against the variant breaks the day `Rubble` arrives.
//!
//! # Why the structural rules stay 4-connected while movement is 8-connected
//!
//! **Named and decided, not left to fall out** (The Pavement, Task 6). Every
//! flood in this module reads `neighbours(..)[..4]`, while `Session::step` and
//! `Underground::peek` now walk all eight bearings with the corner rule applied.
//! The two are deliberately different, because they answer different questions:
//!
//! - **These rules describe what the GENERATOR built.** `grow`'s flood spreads
//!   orthogonally, its separation rule (`claimable`) is over orthogonal
//!   adjacency, and its tunnels are orthogonal by argument rather than by
//!   accident (see `grow::rotated`'s doc: a one-cell-wide diagonal corridor is
//!   unwalkable under the very corner rule that makes movement 8-connected). A
//!   structural invariant stated in a connectivity the construction does not use
//!   is not checking the construction.
//! - **The mover is not the generator.** Section 3.1 makes a WALK 8-connected;
//!   it does not make a doorway diagonal or a chamber boundary diagonal.
//!
//! The two places the difference could bite, and what happens at each:
//!
//! 1. **`realized_links` reporting a link the graph never specified, if this
//!    flood were widened.** Widening was the alternative and it is the one that
//!    would break rule 1, not keeping it narrow. This flood walks a run of
//!    `Threshold` cells and collects the `Floor` chambers beside it, and
//!    `grow::reservable` constrains only a reserved cell's FOUR ORTHOGONAL
//!    neighbours (`neighbours(cell)[..4]`) to belong to the two chambers the
//!    doorway names — a threshold's DIAGONAL neighbours are unconstrained and may
//!    be owned by a third chamber. An 8-connected flood would therefore collect
//!    that third chamber and report `(A, C)` as realized, which the anchor graph
//!    never specified: rule 1 would fail on correctly generated lattices, and the
//!    fix would be a generator change with its own epoch.
//!
//!    **What this is NOT, and the correction is worth keeping.** The first
//!    version of this note argued the other direction — that a walker crossing
//!    diagonally between two chambers would mint an undoored connection rule 1
//!    exists to catch, and that `Session::step`'s
//!    `INDOOR_UNDOORED_ROOM_REFUSAL` therefore protects this invariant. **That
//!    was false.** Whenever the corner rule opens such a diagonal, the open
//!    flank is provably the `Threshold` joining exactly those two chambers, so
//!    the link is already in the graph and already reported — rule 1 could never
//!    have fired. See that constant's own doc for the proof and for the three
//!    reasons the refusal is right anyway. The refusal and this restriction are
//!    still one decision; they are simply not the same argument.
//! 2. **`reachable_from` under-reporting** (rule 8's sealed-pocket detection).
//!    It does, and the direction is safe: a 4-connected flood reaches no more
//!    than a walker can, so the rule is conservative. It may reject a layout a
//!    diagonal step would have rescued; it can never certify a pocket as
//!    reachable when it is not.
//!
//! Neither reading is free of cost, and the cost of this one is stated rather
//! than hidden: rule 8 is stricter than the verb.
//!
//! **Read this note, not a summary of it.** Both restriction sites below point
//! here by name rather than restating the argument, because it was restated once
//! already and the copy was the half that went wrong.

use super::{Cell, CellKind, Lattice, Rect, neighbours};
use std::collections::BTreeSet;

/// What occupies `cell`. `None` means **outside the extent**, and nothing else.
///
/// [`Lattice::cells`] is total, so there is no second reading of `None` to guess
/// between — which was the whole point of making wall-ness a positive fact rather
/// than an absence. Replaces Task 1's `region_of`, whose answer for a doorway cell
/// had to be one chamber when the truth is two.
pub fn kind_of(lattice: &Lattice, cell: Cell) -> Option<CellKind> {
    lattice.cells.get(&cell).copied()
}

/// The bounding rect of `chamber`'s FLOOR cells, or `None` if it holds none.
///
/// Floor only, deliberately. Including the thresholds a chamber shares would make
/// two chambers' bounds overlap for a reason that is not a defect, which is
/// exactly the ambiguity that made Task 1's `regions` field a trap: a rect you
/// cannot test membership against is a summary masquerading as an authority. This
/// is a summary and says so — for membership, ask [`kind_of`].
/// type-audit: bare-ok(index: chamber)
pub fn bounds_of(lattice: &Lattice, chamber: usize) -> Option<Rect> {
    let mut span: Option<(i32, i32, i32, i32)> = None;
    for (c, k) in &lattice.cells {
        if *k != CellKind::Floor(chamber) {
            continue;
        }
        span = Some(match span {
            None => (c.0, c.1, c.0, c.1),
            Some((x0, y0, x1, y1)) => (x0.min(c.0), y0.min(c.1), x1.max(c.0), y1.max(c.1)),
        });
    }
    span.map(|(x0, y0, x1, y1)| Rect {
        x: x0,
        y: y0,
        w: x1 - x0 + 1,
        h: y1 - y0 + 1,
    })
}

/// Where a mover STANDS on arriving in `chamber` with no cell of its own yet:
/// the middle of the room if the middle is the room's own floor, else the first
/// floor cell it owns. `None` only if it owns no floor at all.
///
/// The middle first because that is what a reader of the drawn plan expects of
/// "you came in and stopped" — but **the fallback is the whole point, not a
/// defensive flourish**. `bounds_of` is a bounding rect over floor cells, and a
/// GROWN chamber's blob is not convex: the centre of its bounding box is
/// routinely a wall, or another chamber's floor. Taking the centre on trust would
/// put the possession inside the fabric of every organically grown building, and
/// the mark would be drawn on a cell it cannot occupy —
/// `the_standing_cell_is_always_the_chambers_own_floor` is what catches it, and
/// `the_middle_of_a_grown_chamber_is_not_always_its_floor` is what proves the
/// fallback fires rather than being dead code.
///
/// A `Threshold` is deliberately NOT a candidate even though it is passable and
/// serves the chamber: the mark would be drawn over the `+` and the plan would
/// stop showing a doorway it promises is walkable.
///
/// Deterministic and seed-free either way: the fallback is `BTreeMap` order, so
/// re-entering a room lands in the same cell and the plan is byte-identical by
/// construction (decision 0069).
/// type-audit: bare-ok(index: chamber)
pub fn standing_cell(lattice: &Lattice, chamber: usize) -> Option<Cell> {
    let own_floor = |c: Cell| kind_of(lattice, c) == Some(CellKind::Floor(chamber));
    if let Some(b) = bounds_of(lattice, chamber) {
        let middle = Cell(b.x + b.w / 2, b.y + b.h / 2);
        if own_floor(middle) {
            return Some(middle);
        }
    }
    lattice.cells.keys().copied().find(|c| own_floor(*c))
}

/// The cell of the doorway realizing the link between `a` and `b`, either way
/// round, or `None` if the two are not linked.
///
/// Read off [`Lattice::doorways`] rather than searched for among the `Threshold`
/// cells: the doorway list is what the embedder REPORTS carving, and §7 rule 3
/// already checks it against the kind map in both directions, so a second,
/// independent search here would be a second opinion where the campaign has spent
/// three tasks removing them.
/// type-audit: bare-ok(index: a), bare-ok(index: b)
pub fn doorway_between(lattice: &Lattice, a: usize, b: usize) -> Option<Cell> {
    lattice
        .doorways
        .iter()
        .find(|&&(p, q, _)| (p, q) == (a, b) || (p, q) == (b, a))
        .map(|&(_, _, cell)| cell)
}

/// The cell a mover lands on having crossed `through` INTO `chamber`: the first
/// of the threshold's orthogonal neighbours that is `chamber`'s own floor.
///
/// **A step through a doorway ends beside it, not in it.** Two reasons, and the
/// second is the load-bearing one: physically, walking through a door puts you in
/// the room rather than in the doorframe; and a mark drawn ON the threshold hides
/// the `+`, so a player standing in a doorway would read a plan with one fewer
/// doorway than the building has. That makes the drawn count disagree with
/// `Lattice::doorways` for a reason no rule would report.
///
/// `None` if the threshold touches no floor of `chamber` — which §7 rule 1 already
/// reports as the dropped link it is, so the caller may treat it as a failure
/// rather than as a case to paper over.
/// type-audit: bare-ok(index: chamber)
pub fn cell_beyond(lattice: &Lattice, through: Cell, chamber: usize) -> Option<Cell> {
    // First four only: a mover lands beside the threshold ORTHOGONALLY, never
    // diagonally, so the search stays over `neighbours`'s first four entries.
    neighbours(through)
        .into_iter()
        .take(4)
        .find(|c| kind_of(lattice, *c) == Some(CellKind::Floor(chamber)))
}

/// Every ORTHOGONALLY adjacent pair of PASSABLE cells inside the extent.
///
/// **It said "the complete set of steps a mover may take", and that is no longer
/// true** (The Pavement): a mover takes eight bearings, corner rule permitting.
/// What this enumerates is the 4-connected passable graph of the lattice as
/// BUILT, which is what the rules over it want — see the module note "Why the
/// structural rules stay 4-connected while movement is 8-connected". A caller
/// wanting what a mover may do must ask
/// [`crate::lattice::diagonal_is_blocked`] as well.
///
/// `(1, 0)` and `(0, 1)` only, so each unordered pair is visited exactly once from
/// its lower cell.
pub fn openings(lattice: &Lattice) -> Vec<(Cell, Cell)> {
    let mut out = Vec::new();
    let e = lattice.extent;
    for cx in e.x..(e.x + e.w) {
        for cy in e.y..(e.y + e.h) {
            let here = Cell(cx, cy);
            if !kind_of(lattice, here).is_some_and(|k| k.passable()) {
                continue;
            }
            for (dx, dy) in [(1, 0), (0, 1)] {
                let there = Cell(cx + dx, cy + dy);
                if kind_of(lattice, there).is_some_and(|k| k.passable()) {
                    out.push((here, there));
                }
            }
        }
    }
    out
}

/// Every unordered pair of chambers a mover can actually get between — the
/// adjacency read off the geometry rather than taken from `links`.
///
/// Two sources, and both are ways through:
///
/// 1. **Two `Floor` cells of different chambers touching.** §7 rule 2 forbids it,
///    and it is reported here as well so rule 1 cannot come up green on a lattice
///    where two chambers bleed into each other.
/// 2. **A connected RUN of `Threshold` cells**, which joins every chamber whose
///    floor touches it. A run rather than a single cell because two carved cells
///    could end up side by side and a mover would cross both in two steps; and
///    derived from the run's FLOOR NEIGHBOURS rather than from its `Threshold(a,b)`
///    label, so a threshold that touches a third chamber's floor is reported as
///    the invented relation it is, and a threshold labelled `(a,b)` that reaches
///    only `a` realizes nothing and is caught as the dropped link it is.
///
/// type-audit: bare-ok(index: return)
pub fn realized_links(lattice: &Lattice) -> BTreeSet<(usize, usize)> {
    let mut out = BTreeSet::new();
    for (p, q) in openings(lattice) {
        if let (Some(CellKind::Floor(i)), Some(CellKind::Floor(j))) =
            (kind_of(lattice, p), kind_of(lattice, q))
            && i != j
        {
            out.insert((i.min(j), i.max(j)));
        }
    }
    let mut seen: BTreeSet<Cell> = BTreeSet::new();
    for (&c, k) in &lattice.cells {
        if !matches!(k, CellKind::Threshold(_, _)) || seen.contains(&c) {
            continue;
        }
        let mut queue = vec![c];
        seen.insert(c);
        let mut touching: BTreeSet<usize> = BTreeSet::new();
        while let Some(at) = queue.pop() {
            // First four, AND IT STAYS FOUR NOW THAT MOVEMENT IS EIGHT — a
            // decision, not an oversight. Read the module-level note "Why the
            // structural rules stay 4-connected while movement is 8-connected"
            // for it; the one-line version is that `reservable` constrains only a
            // threshold's ORTHOGONAL neighbours, so an 8-connected flood here
            // would collect a third chamber sitting diagonally off the doorway
            // and report a link the anchor graph never specified. That is rule 1
            // failing on a correctly generated lattice, which is the opposite of
            // what an earlier draft of this comment claimed.
            for n in neighbours(at).into_iter().take(4) {
                match kind_of(lattice, n) {
                    Some(CellKind::Floor(i)) => {
                        touching.insert(i);
                    }
                    Some(CellKind::Threshold(_, _)) if seen.insert(n) => queue.push(n),
                    _ => {}
                }
            }
        }
        for a in &touching {
            for b in &touching {
                if a < b {
                    out.insert((*a, *b));
                }
            }
        }
    }
    out
}

/// Every passable cell a mover can reach from `chamber`, stepping only between
/// passable cells.
///
/// **From ONE cell, not from every cell the chamber serves**, and that is what
/// makes §7 rule 8 able to fail. Flooding from all of a chamber's cells would make
/// a sealed pocket its own starting point, so the rule would report every pocket
/// as reachable and never discriminate — a check that cannot fail. The start is
/// the lowest passable cell serving `chamber` in `BTreeMap` order, so it is total
/// and seed-free.
/// type-audit: bare-ok(index: chamber)
pub fn reachable_from(lattice: &Lattice, chamber: usize) -> BTreeSet<Cell> {
    let mut out = BTreeSet::new();
    let Some(start) = lattice
        .cells
        .iter()
        .find(|(_, k)| k.passable() && k.serves(chamber))
        .map(|(c, _)| *c)
    else {
        return out;
    };
    let mut queue = vec![start];
    out.insert(start);
    while let Some(at) = queue.pop() {
        // First four, deliberately, though a mover walks eight — see the
        // module-level note "Why the structural rules stay 4-connected while
        // movement is 8-connected". The direction of the inequality is what
        // makes it safe: a 4-connected flood reaches a SUBSET of what a walker
        // can, so rule 8 is STRICTER than movement requires. It can reject a
        // layout a diagonal would have rescued; it can never pass a pocket a
        // walker cannot reach, which is the failure that would ship.
        for n in neighbours(at).into_iter().take(4) {
            if kind_of(lattice, n).is_some_and(|k| k.passable()) && out.insert(n) {
                queue.push(n);
            }
        }
    }
    out
}

/// `n - 1` cut positions for `n` chambers, for ANY rooted tree — a tree on `n`
/// nodes has `n - 1` edges and the allocator spends exactly one cut per edge
/// (Task 4).
///
/// This is the number rule 7 compares `Lattice::dof` against. It is written as a
/// function of the chamber count alone because that is all the graph determines —
/// if a future method needs more freedom than this, the honest move is to widen
/// this function and say why, never to stop comparing.
/// type-audit: bare-ok(count: chambers), bare-ok(count: return)
pub fn freedom_of_a_tree(chambers: usize) -> u32 {
    chambers.saturating_sub(1) as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::housemark::{AuthorityMark, Housemark, ThresholdPosture};
    use crate::lattice::{allocate, embed_with, extent_for, grow};
    use crate::site::{Site, SiteKind};
    use crate::structure::{Role, Structure, structure_at};
    use hornvale_history::record::{Function, Notability};
    use hornvale_kernel::{Facet, Seed};

    const WALK: u32 = 13;
    /// Widened from Task 3's 24 in Task 4b. Rules 1, 2 and 8 are claims that the
    /// GROWER's construction makes true — a separation rule and a reservation, argued
    /// rather than searched for — and an argument is worth more seeds than a search:
    /// the failure this task hit at four chambers appeared at one seed in twelve, so
    /// two dozen was uncomfortably close to the resolution of the check.
    const SEEDS: std::ops::Range<u64> = 0..192;

    fn locale(n: u64) -> Facet {
        Facet {
            face: 3,
            path: (0..WALK).map(|i| ((i as u64 + n) % 4) as u8).collect(),
        }
    }

    /// A built site with no business at all: the grammar's floor, two chambers,
    /// `T{ H }`. Also the METHOD selector for the rectilinear arm of the tests
    /// that hand-build their own trees ([`h4_every_tree_embeds_under_all_eight_rules_with_exact_freedom`]),
    /// where only `built` is read.
    fn built() -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            true,
            true,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// A living, warm, communal, plain-postured agrarian dwelling — the BUSH
    /// shape, four chambers, `T{ H, W, S }`: a fork of three at the door.
    fn bush() -> Brief {
        Brief::from_parts(
            Some(Function::Agrarian),
            None,
            Some(Notability::Common),
            None,
            Some(Housemark {
                authority: AuthorityMark::Common,
                threshold: ThresholdPosture::Plain,
            }),
            0,
            true,
            false,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// The same dwelling, cold: the DEEP shape, `T{ H{ W, S } }` — four
    /// chambers with the fork one step in rather than at the door.
    fn deep() -> Brief {
        let mut b = bush();
        b.cold = true;
        b
    }

    /// A waypoint: `Trade`'s business IS keeping goods, so three chambers,
    /// `T{ H, S }`.
    fn trade() -> Brief {
        let mut b = bush();
        b.function = Some(Function::Trade);
        b
    }

    /// A CAVE: a site nobody built. Both the shape source for the grown arm —
    /// the wild draw still gives a chain of 1..=MAX_CHAMBERS (spec §3.5) — and
    /// the method selector `embed_with` reads to send it to `grow`.
    ///
    /// It carries a SITE now, and must: `structure_at` gates on `brief.site`
    /// (decision 0666), so the old site-less method selector derives no
    /// structure at all and could only ever be passed to `embed_with`. Making
    /// it a real cave brief is what lets the grown arm's structures come from
    /// the same derivation production's caves take, rather than being borrowed
    /// from a settlement.
    fn wild() -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            false,
            true,
            Some(Site::placed(SiteKind::Cave, None)),
            None,
        )
    }

    /// Which method produced a lattice. Carried through the corpus because rule 7
    /// budgets the two methods differently — a cut is one choice, a seed cell is
    /// two — and a test that cannot tell them apart can only check the looser
    /// bound, which means not checking the tighter one at all.
    #[derive(Clone, Copy, Debug)]
    enum Method {
        Rectilinear,
        Grown,
    }

    /// Every (structure, lattice, method) triple the rules are checked over.
    ///
    /// **Varied by BRIEF on the rectilinear arm, by SEED on the grown one, and
    /// the asymmetry is the campaign's whole point** (The Cruck, Task 3). A
    /// built structure's chamber count and shape are the grammar's, so
    /// scanning seeds for a four-chamber settlement finds nothing a brief did
    /// not already decide — the four shape briefs below are how this corpus
    /// reaches four chambers, and how it reaches a FORK at all. A wild
    /// structure's count is still drawn, so the grown arm varies by seed
    /// exactly as it always did and its chains are byte-for-byte The Lintel's.
    fn corpus() -> Vec<(crate::structure::Structure, crate::lattice::Lattice, Method)> {
        let mut out = Vec::new();
        for s in SEEDS {
            for shape in [built(), trade(), bush(), deep()] {
                let st = structure_at(&locale(s), &shape, Seed(s), WALK).expect("a built site");
                let e = extent_for(&st);
                out.push((
                    st.clone(),
                    embed_with(&st, &shape, e, Seed(s)),
                    Method::Rectilinear,
                ));
            }
            let st = structure_at(&locale(s), &wild(), Seed(s), WALK).expect("a cave is a site");
            let e = extent_for(&st);
            out.push((
                st.clone(),
                embed_with(&st, &wild(), e, Seed(s)),
                Method::Grown,
            ));
        }
        assert!(
            out.iter().map(|(s, _, _)| s.chambers.len()).max().unwrap()
                == crate::structure::MAX_CHAMBERS,
            "the corpus never reaches MAX_CHAMBERS, so the rules are unchecked at \
             the count most likely to break them"
        );
        assert!(
            out.iter().any(|(s, _, _)| s.children(0).len() >= 2),
            "the corpus never reaches a FORK, so every rule below is a claim \
             about chains — which is exactly what it was before The Cruck"
        );
        out
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag.
    #[test]
    fn rule_1_the_realized_graph_is_exactly_the_specified_one() {
        // BOTH directions, and the second is the half that makes this an embedder
        // check rather than a completeness check. A missing adjacency is a dropped
        // relation; an EXTRA one is invented information, which is the single
        // thing an embedder may not add (Amendment 1 §1a.7). Rule 1 as the spec
        // words it names only the first, so this asserts the stronger property and
        // says so.
        for (s, l, m) in corpus() {
            let realized = realized_links(&l);
            let specified: BTreeSet<(usize, usize)> =
                s.links.iter().map(|&(a, b)| (a.min(b), a.max(b))).collect();
            for pair in &specified {
                assert!(
                    realized.contains(pair),
                    "{m:?}: link {pair:?} is specified but not realized — the \
                     embedder dropped an adjacency the anchor graph asserts"
                );
            }
            for pair in &realized {
                assert!(
                    specified.contains(pair),
                    "{m:?}: chambers {pair:?} can be walked between in the lattice \
                     but are not linked in the graph — the embedder invented a \
                     relation"
                );
            }
            // And the LABELS, third: a `Threshold(a, b)` naming a pair the graph
            // does not link is invented information even when the geometry happens
            // not to realize it.
            for (c, k) in &l.cells {
                if let CellKind::Threshold(a, b) = *k {
                    assert!(
                        specified.contains(&(a.min(b), a.max(b))),
                        "{m:?}: the cell {c:?} is a threshold between chambers \
                         {a} and {b}, which the graph does not link"
                    );
                }
            }
        }
    }

    /// §7 rule 2, THE WALL LAW, as one callable statement.
    ///
    /// Extracted from `rule_2_two_chambers_floors_are_never_adjacent` so that H4
    /// and the fork witness can assert it too, and stated exactly once so the
    /// three callers cannot drift apart. `corpus()` builds its structures with
    /// `structure_at`, which produces chains only, so before this extraction
    /// rules 2, 3 and 4 had NEVER SEEN A FORK under either method — and rule 1
    /// does not subsume rule 2: two LINKED chambers whose floors touch with no
    /// fabric between them realize exactly the specified pair, so rule 1 stays
    /// green while the plan is a lie.
    ///
    /// `s` is carried for the failure message only. On a fork the shape is what
    /// a reader needs first, and a bare method name does not carry it.
    fn check_rule_2(l: &Lattice, s: &Structure, m: &str) {
        // The wall law, in the form walls-as-cells gives it. Task 1 had to phrase
        // it over a separately-derived set of cell pairs — "every wall pair is a
        // non-adjacency" — which made it a claim about the derivation. This is a
        // claim about the world: if you can step from one chamber's floor straight
        // onto another's, there is no fabric between them and the plan is a lie
        // whatever the wall set says.
        for (p, q) in openings(l) {
            if let (Some(CellKind::Floor(i)), Some(CellKind::Floor(j))) =
                (kind_of(l, p), kind_of(l, q))
            {
                assert_eq!(
                    i, j,
                    "{m} links {:?}: {p:?} is chamber {i}'s floor and {q:?} is \
                     chamber {j}'s, and they are adjacent — two rooms with no \
                     wall between them are one room",
                    s.links
                );
            }
        }
    }

    /// §7 rule 3 — all three clauses, as one callable statement.
    ///
    /// (i) `cells` is TOTAL over the extent; (ii) the outer ring is entirely
    /// `Wall`; (iii) thresholds and doorways name each other, both ways. Same
    /// extraction reason as [`check_rule_2`].
    fn check_rule_3(l: &Lattice, s: &Structure, m: &str) {
        let e = l.extent;
        assert_eq!(
            l.cells.len() as i32,
            e.area(),
            "{m} links {:?}: the kind map holds {} cells for a {}-cell extent, so \
             `kind_of` returning None no longer means only 'outside'",
            s.links,
            l.cells.len(),
            e.area()
        );
        for cx in e.x..(e.x + e.w) {
            for cy in e.y..(e.y + e.h) {
                let c = Cell(cx, cy);
                let k = kind_of(l, c)
                    .unwrap_or_else(|| panic!("{m}: no kind for {c:?} inside the extent"));
                let on_ring = cx == e.x || cy == e.y || cx == e.x + e.w - 1 || cy == e.y + e.h - 1;
                if on_ring {
                    assert_eq!(
                        k,
                        CellKind::Wall,
                        "{m} links {:?}: the outer ring is {k:?} at {c:?} — a plan \
                         open to the outside is not a building",
                        s.links
                    );
                }
            }
        }
        let declared: BTreeSet<Cell> = l.doorways.iter().map(|&(_, _, c)| c).collect();
        for (c, k) in &l.cells {
            if matches!(k, CellKind::Threshold(_, _)) {
                assert!(
                    declared.contains(c),
                    "{m} links {:?}: {c:?} is a threshold no doorway declares — an \
                     undeclared way through is a hole in the plan",
                    s.links
                );
            }
        }
        for &(a, b, c) in &l.doorways {
            assert!(
                matches!(kind_of(l, c), Some(CellKind::Threshold(_, _))),
                "{m} links {:?}: the doorway ({a},{b}) is declared at {c:?}, which \
                 is {:?} rather than a threshold",
                s.links,
                kind_of(l, c)
            );
        }
    }

    /// §7 rule 4 — one doorway per link, no more and no fewer. Same extraction
    /// reason as [`check_rule_2`].
    fn check_rule_4(l: &Lattice, s: &Structure, m: &str) {
        // The doorway is ONE CELL now, not a pair of half-boundaries, so reading
        // it from either side must give one answer. Asserted as uniqueness per
        // unordered pair: two entries for one pair is exactly how two chambers
        // come to disagree about which cell is the door.
        let mut seen: BTreeSet<(usize, usize)> = BTreeSet::new();
        for &(a, b, _) in &l.doorways {
            let key = (a.min(b), a.max(b));
            assert!(
                seen.insert(key),
                "{m} links {:?}: chambers {a} and {b} have two doorways between \
                 them, so the two sides can disagree about which cell is the door",
                s.links
            );
        }
        assert_eq!(
            seen.len(),
            s.links.len(),
            "{m} links {:?}: one doorway per link, no more and no fewer",
            s.links
        );
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — a forall-corpus-entry
    /// invariant, like its rule_1/rule_4/rule_7/rule_8 siblings: `corpus()`
    /// loops `for s in SEEDS` and builds 384 seed-derived structure/lattice
    /// pairs (both methods) per call. The tag arrived with the fix-round-1
    /// extraction, which bound the structure as `s` where the body previously
    /// discarded it as `_` — the shape was always this; only the binding the
    /// seed-loop detector reads is new.
    #[test]
    fn rule_2_two_chambers_floors_are_never_adjacent() {
        for (s, l, m) in corpus() {
            check_rule_2(&l, &s, &format!("{m:?}"));
        }
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — a forall-corpus-entry
    /// invariant, like its rule_1/rule_4/rule_7/rule_8 siblings: `corpus()`
    /// loops `for s in SEEDS` and builds 384 seed-derived structure/lattice
    /// pairs (both methods) per call. The tag arrived with the fix-round-1
    /// extraction, which bound the structure as `s` where the body previously
    /// discarded it as `_` — the shape was always this; only the binding the
    /// seed-loop detector reads is new.
    #[test]
    fn rule_3_the_plan_is_enclosed_and_every_threshold_is_declared() {
        // **No longer tautological**, which is the reification's clearest single
        // gain. Task 1's closure rule was the contrapositive of `walls_around`'s
        // own exemption condition read back off the same ownership map, so it
        // checked the wall derivation's self-consistency and nothing else. Under
        // this model it has three pieces of independent content, and an embedder
        // could fail any of them:
        //
        //   (i)   `cells` is TOTAL over the extent — the claim the type makes;
        //   (ii)  the outer ring is entirely `Wall` — the plan is ENCLOSED;
        //   (iii) thresholds and doorways name each other, both ways.
        for (s, l, m) in corpus() {
            check_rule_3(&l, &s, &format!("{m:?}"));
        }
    }

    #[test]
    fn rule_3_actually_fails_on_an_unenclosed_lattice() {
        // The NEGATIVE CONTROL, kept from Task 4 and re-aimed at the claim that
        // now carries the content. Punch one hole in the exterior wall and one
        // undeclared threshold into the fabric, and confirm each half of rule 3
        // notices. A rule that has never failed is not yet a check.
        let (_, l, _) = corpus()
            .into_iter()
            .find(|(s, _, _)| s.chambers.len() > 1)
            .expect("the corpus reaches MAX_CHAMBERS, so a multi-chamber plan is in it");
        let e = l.extent;

        let mut holed = l.clone();
        let hole = Cell(e.x, e.y + e.h / 2);
        assert_eq!(kind_of(&holed, hole), Some(CellKind::Wall), "{hole:?}");
        holed.cells.insert(hole, CellKind::Floor(0));
        let unenclosed = (e.x..(e.x + e.w)).flat_map(|cx| {
            (e.y..(e.y + e.h)).filter_map(move |cy| {
                let c = Cell(cx, cy);
                let on_ring = cx == e.x || cy == e.y || cx == e.x + e.w - 1 || cy == e.y + e.h - 1;
                on_ring.then_some(c)
            })
        });
        assert!(
            unenclosed
                .clone()
                .any(|c| kind_of(&holed, c) != Some(CellKind::Wall)),
            "opening {hole:?} in the exterior wall did not make the ring \
             non-Wall, so rule 3(ii) cannot detect an unenclosed plan either"
        );

        let mut leaky = l.clone();
        let victim = *leaky
            .cells
            .iter()
            .find(|(c, k)| **k == CellKind::Wall && e.inset(1).contains(**c))
            .expect("a multi-chamber plan has interior fabric")
            .0;
        leaky.cells.insert(victim, CellKind::Threshold(0, 1));
        let declared: BTreeSet<Cell> = leaky.doorways.iter().map(|&(_, _, c)| c).collect();
        assert!(
            !declared.contains(&victim),
            "carving {victim:?} into an undeclared threshold left it declared \
             anyway, so rule 3(iii) cannot detect an undeclared way through"
        );
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, _) in corpus()` this test's body
    /// runs (Fix round 3 correction — this one binds the method as `_`,
    /// not `m` like its 7 siblings) binds `s` to one of those
    /// ALREADY-SEEDED structures, not to something seedless. This is a
    /// forall-corpus-entry invariant, not a false-positive flag.
    #[test]
    fn rule_4_two_chambers_cannot_disagree_about_a_doorway() {
        for (s, l, m) in corpus() {
            check_rule_4(&l, &s, &format!("{m:?}"));
        }
    }

    #[test]
    fn rule_6_the_solve_carries_no_state() {
        // Same inputs, solved from scratch, in an order that would expose a
        // carried cache: A, then B, then A again.
        let st = structure_at(&locale(1), &bush(), Seed(1), WALK).expect("built");
        let e = extent_for(&st);
        let a1 = allocate(&st, e, Seed(1));
        let _b = allocate(&st, e, Seed(2));
        let a2 = allocate(&st, e, Seed(1));
        assert_eq!(a1, a2, "an intervening solve changed a later one's result");
        let g1 = grow(&st, e, Seed(1));
        let _g2 = grow(&st, e, Seed(2));
        assert_eq!(g1, grow(&st, e, Seed(1)));
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag. (Also reports the per-method/chamber-count DOF
    /// spent via `eprintln!`, but the enforced claim is the exact-budget
    /// `assert_eq!` on every corpus entry — invariant, not readout.)
    #[test]
    fn rule_7_the_embedder_spends_only_the_freedom_the_graph_leaves() {
        // Reported per method and chamber count, because Task 4b reworked BOTH
        // embedders and rule 7 is the check that says whether the rework changed
        // how many draws are spent. If these numbers move, that is a finding to
        // report rather than a budget to widen.
        let mut spent: BTreeSet<(&'static str, usize, u32)> = BTreeSet::new();
        for (s, l, m) in corpus() {
            // Each method's budget is stated in ITS OWN terms, up front. A budget
            // computed from the result -- widened whenever it is exceeded -- is not
            // a check, and that is the shape this test must not take.
            let n = s.chambers.len();
            let budget = match m {
                // One cut per interior boundary; the seed moves the cut and
                // nothing else. A cut now consumes a cell for its wall line, but
                // it is still ONE choice.
                Method::Rectilinear => freedom_of_a_tree(n),
                // A seed cell is a POINT, so two draws per chamber, not one.
                Method::Grown => 2 * n as u32,
            };
            spent.insert((
                match m {
                    Method::Rectilinear => "rectilinear",
                    Method::Grown => "grown",
                },
                n,
                l.dof,
            ));
            assert_eq!(
                l.dof, budget,
                "{m:?} with {n} chambers spent {} choices against a budget of \
                 {budget}. Over budget means the embedder is INVENTING, which is \
                 the one thing it may not do. Under budget is also a finding: the \
                 residual freedom the graph leaves is going unused, so either the \
                 budget is wrong or the method is more rigid than it claims.",
                l.dof
            );
        }
        eprintln!("rule 7, (method, chambers, dof spent): {spent:?}");
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag.
    #[test]
    fn rule_8_every_floor_cell_is_reachable_from_the_threshold() {
        // **The rule Task 4b's model earns.** Under the boundary model this was
        // guaranteed: regions tiled the extent and doorways linked them, so there
        // was nowhere to be stranded. Walls as cells can seal a pocket of floor,
        // and the grower is where it would happen — which is why the grower claims
        // with a separation rule and never takes a cell back, so the property holds
        // by argument rather than by luck. This test is what makes the argument
        // falsifiable.
        //
        // `structure.threshold == chambers[0]` always (`structure.rs`'s stated
        // invariant 1), so chamber 0 is the entry.
        for (s, l, m) in corpus() {
            let reached = reachable_from(&l, 0);
            for (c, k) in &l.cells {
                if !k.passable() {
                    continue;
                }
                assert!(
                    reached.contains(c),
                    "{m:?} with {} chambers: {c:?} is {k:?} and cannot be reached \
                     from the threshold chamber through passable cells — a sealed \
                     pocket of floor is a room the player can see on the plan and \
                     never enter",
                    s.chambers.len()
                );
            }
        }
    }

    #[test]
    fn rule_8_actually_fails_on_a_sealed_pocket() {
        // The negative control for the new rule, and the one this task most needed:
        // rule 8 is green on every derived lattice by construction, so a green rule
        // 8 is no evidence the condition can discriminate at all.
        //
        // Seal the LAST floor cell of chamber 0 rather than the first: the flood
        // starts at the lowest passable cell serving the chamber, so sealing THAT
        // one would make the pocket its own starting point and prove nothing.
        let (_, mut l, _) = corpus()
            .into_iter()
            .find(|(s, _, _)| s.chambers.len() > 1)
            .expect("the corpus reaches MAX_CHAMBERS");
        let mine: Vec<Cell> = l
            .cells
            .iter()
            .filter(|(_, k)| **k == CellKind::Floor(0))
            .map(|(c, _)| *c)
            .collect();
        assert!(
            mine.len() > 1,
            "chamber 0 holds one cell, so its last cell is also the flood's start \
             and this control would prove nothing"
        );
        let pocket = *mine.last().expect("chamber 0 holds floor");
        // First four (orthogonal) only: `reachable_from` floods only those
        // (see its own note), so walling all eight of `neighbours` here would
        // over-seal relative to what the flood actually reads — a weaker
        // control than intended, since the assertion below would still pass
        // even if the four orthogonal walls alone were not enough to seal the
        // pocket. Restricting to `[..4]` makes this a control on the exact
        // cells rule 8's own reachability check walks.
        for n in neighbours(pocket)[..4].iter() {
            if kind_of(&l, *n).is_some_and(|k| k.passable()) {
                l.cells.insert(*n, CellKind::Wall);
            }
        }
        assert!(
            !reachable_from(&l, 0).contains(&pocket),
            "walling every passable neighbour of {pocket:?} left it reachable, so \
             rule 8 cannot detect a sealed pocket either — the rule is inert"
        );
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag.
    #[test]
    fn a_chambers_bounds_are_its_floor_and_nothing_else() {
        // `bounds_of` replaces the deleted `regions` field, so its contract is
        // asserted rather than assumed: every floor cell of the chamber falls
        // inside, and — the half that matters — the rect is a SUMMARY, so no cell
        // outside the chamber's floor is claimed to be inside it.
        for (s, l, m) in corpus() {
            for i in 0..s.chambers.len() {
                let b = bounds_of(&l, i)
                    .unwrap_or_else(|| panic!("{m:?}: chamber {i} holds no floor at all"));
                for (c, k) in &l.cells {
                    if *k == CellKind::Floor(i) {
                        assert!(
                            b.contains(*c),
                            "{m:?}: {c:?} is chamber {i}'s floor and outside {b:?}"
                        );
                    }
                }
            }
        }
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag.
    #[test]
    fn the_standing_cell_is_always_the_chambers_own_floor() {
        // Where the possession is put when it walks in. Two claims: it exists for
        // every chamber (a chamber with nowhere to stand is a chamber `enter` must
        // refuse), and it is that chamber's OWN floor — never fabric, never the
        // neighbour's room, and never a threshold, which is passable and serves the
        // chamber but would put the drawn mark over a drawn doorway.
        for (s, l, m) in corpus() {
            for i in 0..s.chambers.len() {
                let c = standing_cell(&l, i)
                    .unwrap_or_else(|| panic!("{m:?}: chamber {i} offers nowhere to stand"));
                assert_eq!(
                    kind_of(&l, c),
                    Some(CellKind::Floor(i)),
                    "{m:?}: chamber {i} would be stood in at {c:?}, which is not its floor"
                );
            }
        }
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag. Carries an embedded non-vacuity guard
    /// (`middles_that_miss > 0`) over the same corpus sweep, riding on
    /// builds the invariant already pays for.
    #[test]
    fn the_middle_of_a_grown_chamber_is_not_always_its_floor() {
        // The negative control on `standing_cell`'s fallback, and the reason the
        // fallback exists at all rather than being defensive habit. A grown blob is
        // not convex, so the centre of its bounding rect is routinely fabric or the
        // neighbour's room; taking it on trust would stand the possession inside a
        // wall. If this ever counts zero, the fallback has become dead code and the
        // simpler `standing_cell` would be the honest one.
        let mut middles_that_miss = 0;
        for (s, l, m) in corpus() {
            for i in 0..s.chambers.len() {
                let Some(b) = bounds_of(&l, i) else { continue };
                let middle = Cell(b.x + b.w / 2, b.y + b.h / 2);
                if kind_of(&l, middle) != Some(CellKind::Floor(i)) {
                    assert!(
                        matches!(m, Method::Grown),
                        "a RECTILINEAR chamber's floor is a rect, so its middle is \
                         its own floor; chamber {i} at {middle:?} is not"
                    );
                    middles_that_miss += 1;
                }
            }
        }
        assert!(
            middles_that_miss > 0,
            "no grown chamber's bounding-box centre missed its floor across the \
             whole corpus, so `standing_cell`'s fallback is never exercised"
        );
    }

    /// claim: invariant(seed: corpus SEEDS 0..192) — Fix round 2 correction:
    /// the round-1 tag on this test claimed `seed: none` and called the
    /// corpus seedless. Both were false — `corpus()` (`classify.rs:301`,
    /// not 298 as an earlier round of this correction said — 298 is the
    /// first line of `corpus()`'s own doc comment, not the `fn` line)
    /// loops `for s in SEEDS` (`SEEDS: Range<u64> = 0..192`, line 271) and
    /// builds 384 seed-derived structures/lattices (both embedder methods)
    /// per call; the outer `for (s, l, m) in corpus()` this test's body
    /// runs binds `s` to one of those ALREADY-SEEDED structures, not to
    /// something seedless. This is a forall-corpus-entry invariant, not a
    /// false-positive flag.
    #[test]
    fn a_step_through_a_doorway_lands_beside_it_in_the_chamber_entered() {
        // The cell a threshold crossing ends on, for BOTH chambers it joins: the
        // doorway's own neighbour floor, so the step ends in the room rather than
        // in the doorframe — which is also what keeps the drawn `+` visible.
        for (s, l, m) in corpus() {
            for &(a, b, through) in &l.doorways {
                for chamber in [a, b] {
                    let landed = cell_beyond(&l, through, chamber).unwrap_or_else(|| {
                        panic!(
                            "{m:?}: the doorway at {through:?} joins ({a},{b}) and has no \
                             floor of chamber {chamber} beside it"
                        )
                    });
                    assert_eq!(
                        kind_of(&l, landed),
                        Some(CellKind::Floor(chamber)),
                        "{m:?}: crossing {through:?} into chamber {chamber} lands on {landed:?}"
                    );
                    assert_ne!(landed, through, "a crossing must not end in the doorway");
                }
            }
            assert_eq!(l.doorways.len(), s.links.len());
        }
    }

    /// Every rooted labelled tree on `1..=MAX_CHAMBERS` nodes with `parent <
    /// child` — the shape [`Structure`]'s invariant 2 admits, enumerated rather
    /// than drawn. Parent pointers by mixed radix: `parents[i] ∈ 0..i` for
    /// `i >= 1`, so the count is `1 + 1 + 2 + 6 = 10` on `1..=4` nodes.
    ///
    /// Enumerated because a corpus of DRAWN structures can only exercise the
    /// shapes the current `structure_at` happens to produce. Task 3 has not run
    /// yet, so today it produces chains only — and an embedder that can embed a
    /// chain is exactly what this campaign found insufficient.
    fn every_tree() -> Vec<Structure> {
        let mut out = Vec::new();
        for n in 1..=crate::structure::MAX_CHAMBERS {
            let combos: usize = (1..n).product::<usize>().max(1);
            for code in 0..combos {
                let mut links = Vec::new();
                let mut rest = code;
                for i in 1..n {
                    let p = rest % i;
                    rest /= i;
                    links.push((p, i));
                }
                let chambers: Vec<Facet> = (0..n).map(|i| locale(i as u64)).collect();
                let roles = (0..n)
                    .map(|i| {
                        if i == 0 {
                            Role::Threshold
                        } else {
                            [Role::Hearthroom, Role::Store, Role::Loomroom][i - 1]
                        }
                    })
                    .collect();
                out.push(Structure {
                    threshold: chambers[0].clone(),
                    chambers,
                    links,
                    roles,
                });
            }
        }
        out
    }

    /// Is this tree a PATH — no node with two children?
    ///
    /// The shape `structure_at` produces today and, after The Cruck, the shape a
    /// WILD site still produces: built sites run the grammar and allocate, wild
    /// sites draw a chain and grow (spec §3.5). So this predicate is what divides
    /// H4's two arms, and it is a property of the tree rather than a list of
    /// tree indices, which a widened `MAX_CHAMBERS` would silently invalidate.
    fn is_a_chain(s: &crate::structure::Structure) -> bool {
        (0..s.chambers.len()).all(|i| s.children(i).len() <= 1)
    }

    /// claim: invariant(forall-tree, seed: 0..256) — H4: every reachable tree
    /// embeds faithfully under the RECTILINEAR method, and every chain under the
    /// grown one.
    ///
    /// **The grown arm is narrower than the preregistered H4, deliberately**
    /// (ledger #15, spec §7's H4 amendment of 2026-09-05). The grower realizes a
    /// fork on 2,536 of 2,560 (tree, seed) pairs and drops one link on 24; every
    /// structural remedy measured also moves GROWN bytes for chains, which spec
    /// §6 marks STOP. The 24 are pinned by tree and seed in
    /// [`the_grower_drops_a_link_on_exactly_these_fork_seeds`] rather than
    /// quietly excluded here — this test says what holds, that one says exactly
    /// what does not.
    #[test]
    fn h4_every_tree_embeds_under_all_eight_rules_with_exact_freedom() {
        let trees = every_tree();
        assert_eq!(
            trees.len(),
            10,
            "the enumeration must stay exhaustive over 1..=MAX_CHAMBERS — a \
             generator that silently shrinks turns this invariant into a \
             narrower one with the same name"
        );
        assert_eq!(
            trees.iter().filter(|s| is_a_chain(s)).count(),
            4,
            "the grown arm runs over chains only, so a `is_a_chain` that stopped \
             recognising them would leave that arm asserting nothing while this \
             test stayed green"
        );
        for s in trees {
            let n = s.chambers.len();
            let chain = is_a_chain(&s);
            for seed in 0u64..256 {
                let e = extent_for(&s);
                let rect = embed_with(&s, &built(), e, Seed(seed));
                // Built only for a chain. The grown arm does not run on a fork
                // (see this test's doc), so embedding one there would be 1,536
                // lattices constructed and dropped unexamined.
                let grown = chain.then(|| embed_with(&s, &wild(), e, Seed(seed)));
                let mut arms = vec![(&rect, "rectilinear", freedom_of_a_tree(n))];
                if let Some(g) = &grown {
                    arms.push((g, "grown", 2 * n as u32));
                }
                for (l, m, budget) in arms {
                    let specified: BTreeSet<(usize, usize)> =
                        s.links.iter().map(|&(a, b)| (a.min(b), a.max(b))).collect();
                    assert_eq!(
                        realized_links(l),
                        specified,
                        "{m} seed {seed} links {:?}: rule 1",
                        s.links
                    );
                    // Rules 2, 3 and 4 through the shared helpers, so H4 asserts
                    // all EIGHT rather than the four it named. They are what a
                    // FORK most needs and what `corpus()` cannot reach: rule 1
                    // passes on two linked chambers whose floors touch with no
                    // fabric between them, and only rule 2 catches that.
                    let where_ = format!("{m} seed {seed}");
                    check_rule_2(l, &s, &where_);
                    check_rule_3(l, &s, &where_);
                    check_rule_4(l, &s, &where_);
                    assert_eq!(
                        l.dof, budget,
                        "{m} seed {seed} links {:?}: rule 7 must be EXACT",
                        s.links
                    );
                    for i in 0..n {
                        let b = bounds_of(l, i).unwrap_or_else(|| {
                            panic!("{m} seed {seed}: chamber {i} owns no floor")
                        });
                        if m == "rectilinear" {
                            assert!(
                                b.w >= crate::lattice::allocate::MIN_CHAMBER_SPAN
                                    && b.h >= crate::lattice::allocate::MIN_CHAMBER_SPAN,
                                "{m} seed {seed} links {:?}: chamber {i} is {b:?}",
                                s.links
                            );
                        }
                    }
                    let floors = l.cells.iter().filter(|(_, k)| k.passable()).count();
                    assert_eq!(
                        reachable_from(l, 0).len(),
                        floors,
                        "{m} seed {seed} links {:?}: rule 8",
                        s.links
                    );
                }
            }
        }
    }

    /// claim: invariant(forall-fork-tree, seed: 0..256) — a PINNED KNOWN LIMIT,
    /// not a passing rule: the exact set of (tree, seed) pairs on which the
    /// GROWER fails to realize a specified link.
    ///
    /// **Why this exists rather than a narrowed H4 and silence.** Ledger #15 and
    /// spec §7's H4 amendment (2026-09-05) accept that the grower drops one link
    /// on 24 of 2,560 fork (tree, seed) pairs, because every structural remedy
    /// measured also moves GROWN bytes for chains — a cave transcript moving is
    /// a STOP row in spec §6 — and because production never routes a fork to
    /// `grow` at all: wild sites draw chains (§3.5) and built sites `allocate`
    /// (`embed_with` dispatches on `built`). Accepting a limit is not the same
    /// as forgetting it, so the failures are written down to the pair.
    ///
    /// **It reddens in EITHER direction**, which is the whole point and the
    /// reason this is an equality rather than a count or a bound. A count
    /// ratchet has slack and a violation sits green inside it; an equality does
    /// not. If someone fixes the grower, this test fails and says so — a fix
    /// observed rather than inferred. If a change makes the grower drop a link
    /// somewhere new, it fails too.
    ///
    /// **The three rules fail TOGETHER on exactly these pairs**, and that
    /// coincidence is asserted rather than assumed. One dropped link is read by
    /// rule 1 (the link is unrealized), by rule 3 (the doorway falls back to the
    /// interior's origin, which is floor rather than a threshold — `grow`'s own
    /// doorway read-back comment says it will), and by rule 8 (the chamber
    /// behind the missing doorway is a sealed pocket). Asserting the three sets
    /// are EQUAL is what says the fork failures corrupt nothing else: rule 2 and
    /// the `dof` budget are asserted over ALL 2,560 pairs below, unconditionally.
    #[test]
    fn the_grower_drops_a_link_on_exactly_these_fork_seeds() {
        let pinned: BTreeSet<(Vec<(usize, usize)>, u64)> = [
            (
                vec![(0, 1), (0, 2), (0, 3)],
                vec![
                    5, 26, 46, 62, 65, 70, 94, 121, 137, 167, 180, 203, 211, 235, 249,
                ],
            ),
            (
                vec![(0, 1), (1, 2), (1, 3)],
                vec![34, 58, 60, 90, 110, 125, 184, 202, 218],
            ),
        ]
        .into_iter()
        .flat_map(|(links, seeds)| seeds.into_iter().map(move |s| (links.clone(), s)))
        .collect();
        assert_eq!(
            pinned.len(),
            24,
            "the pinned set is 24 pairs, one per failure"
        );

        let mut dropped: BTreeSet<(Vec<(usize, usize)>, u64)> = BTreeSet::new();
        let mut undeclared_doorway: BTreeSet<(Vec<(usize, usize)>, u64)> = BTreeSet::new();
        let mut sealed: BTreeSet<(Vec<(usize, usize)>, u64)> = BTreeSet::new();
        let mut forks = 0;
        for s in every_tree() {
            if is_a_chain(&s) {
                continue;
            }
            forks += 1;
            let n = s.chambers.len();
            let specified: BTreeSet<(usize, usize)> =
                s.links.iter().map(|&(a, b)| (a.min(b), a.max(b))).collect();
            for seed in 0u64..256 {
                let e = extent_for(&s);
                let l = embed_with(&s, &wild(), e, Seed(seed));
                let here = (s.links.clone(), seed);
                if realized_links(&l) != specified {
                    dropped.insert(here.clone());
                }
                // Rule 3's doorway half only: a declared doorway that is not a
                // `Threshold`. The whole of rule 3 is stated once, in
                // `rule_3_the_plan_is_enclosed_and_every_threshold_is_declared`;
                // this is the single clause an unrealized link trips, named
                // rather than restated.
                if l.doorways
                    .iter()
                    .any(|&(_, _, c)| !matches!(kind_of(&l, c), Some(CellKind::Threshold(_, _))))
                {
                    undeclared_doorway.insert(here.clone());
                }
                let floors = l.cells.iter().filter(|(_, k)| k.passable()).count();
                if reachable_from(&l, 0).len() != floors {
                    sealed.insert(here.clone());
                }
                // Unconditional over every fork pair: a dropped link must not
                // cost the wall law, the doorway count, or the freedom budget as
                // well. Rule 3 is NOT here — it is one of the three the dropped
                // link trips, pinned by the equality below. The same helpers H4
                // calls, so a fork is judged by the same statement a chain is.
                check_rule_2(&l, &s, &format!("grown seed {seed}"));
                check_rule_4(&l, &s, &format!("grown seed {seed}"));
                assert_eq!(
                    l.dof,
                    2 * n as u32,
                    "grown seed {seed} links {:?}: rule 7 must be EXACT on a \
                     fork too — the tunnel spends two draws per chamber whether \
                     or not it finds somewhere to put the doorway",
                    s.links
                );
            }
        }
        assert_eq!(
            forks, 6,
            "six of the ten trees fork; a sweep that found fewer would pin a \
             smaller set and still read as green"
        );
        assert_eq!(
            dropped, pinned,
            "the grower's unrealized-link set MOVED. If it shrank, the limit \
             ledger #15 records has been narrowed or fixed — say so and repin. \
             If it grew, something regressed."
        );
        assert_eq!(
            undeclared_doorway, dropped,
            "rule 3's doorway clause and rule 1 no longer fail on the same \
             pairs, so a fork is failing rule 3 for some reason OTHER than the \
             dropped link this test accepts"
        );
        assert_eq!(
            sealed, dropped,
            "rule 8 and rule 1 no longer fail on the same pairs, so a fork is \
             sealing a pocket for some reason OTHER than the dropped link this \
             test accepts"
        );
    }
}
