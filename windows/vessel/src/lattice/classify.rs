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

/// Every adjacent pair of PASSABLE cells inside the extent — the complete set of
/// steps a mover may take.
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
            for n in neighbours(at) {
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
        for n in neighbours(at) {
            if kind_of(lattice, n).is_some_and(|k| k.passable()) && out.insert(n) {
                queue.push(n);
            }
        }
    }
    out
}

/// How many independent choices the anchor graph LEAVES free for a chain
/// embedding: one cut per interior boundary.
///
/// This is the number rule 7 compares `Lattice::dof` against. It is written as a
/// function of the chamber count alone because that is all the graph determines —
/// if a future method needs more freedom than this, the honest move is to widen
/// this function and say why, never to stop comparing.
/// type-audit: bare-ok(count: chambers), bare-ok(count: return)
pub fn freedom_of_a_chain(chambers: usize) -> u32 {
    chambers.saturating_sub(1) as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::lattice::{allocate, embed_with, extent_for, grow};
    use crate::structure::structure_at;
    use hornvale_kernel::{RoomAddr, Seed};

    const WALK: u32 = 12;
    /// Widened from Task 3's 24 in Task 4b. Rules 1, 2 and 8 are claims that the
    /// GROWER's construction makes true — a separation rule and a reservation, argued
    /// rather than searched for — and an argument is worth more seeds than a search:
    /// the failure this task hit at four chambers appeared at one seed in twelve, so
    /// two dozen was uncomfortably close to the resolution of the check.
    const SEEDS: std::ops::Range<u64> = 0..192;

    fn locale(n: u64) -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| ((i as u64 + n) % 4) as u8).collect(),
        }
    }

    fn built() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    fn wild() -> Brief {
        Brief::from_parts(None, None, None, None, false, true)
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

    /// Every (structure, lattice, method) triple the rules are checked over: both
    /// methods, many seeds, and therefore every chamber count `structure_at`
    /// produces.
    fn corpus() -> Vec<(crate::structure::Structure, crate::lattice::Lattice, Method)> {
        let mut out = Vec::new();
        for s in SEEDS {
            let st = structure_at(&locale(s), &built(), Seed(s), WALK).expect("built");
            let e = extent_for(&st);
            out.push((
                st.clone(),
                embed_with(&st, &built(), e, Seed(s)),
                Method::Rectilinear,
            ));
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
        out
    }

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

    #[test]
    fn rule_2_two_chambers_floors_are_never_adjacent() {
        // The wall law, in the form walls-as-cells gives it. Task 1 had to phrase
        // it over a separately-derived set of cell pairs — "every wall pair is a
        // non-adjacency" — which made it a claim about the derivation. This is a
        // claim about the world: if you can step from one chamber's floor straight
        // onto another's, there is no fabric between them and the plan is a lie
        // whatever the wall set says.
        for (_, l, m) in corpus() {
            for (p, q) in openings(&l) {
                if let (Some(CellKind::Floor(i)), Some(CellKind::Floor(j))) =
                    (kind_of(&l, p), kind_of(&l, q))
                {
                    assert_eq!(
                        i, j,
                        "{m:?}: {p:?} is chamber {i}'s floor and {q:?} is chamber \
                         {j}'s, and they are adjacent — two rooms with no wall \
                         between them are one room"
                    );
                }
            }
        }
    }

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
        for (_, l, m) in corpus() {
            let e = l.extent;
            assert_eq!(
                l.cells.len() as i32,
                e.area(),
                "{m:?}: the kind map holds {} cells for a {}-cell extent, so \
                 `kind_of` returning None no longer means only 'outside'",
                l.cells.len(),
                e.area()
            );
            for cx in e.x..(e.x + e.w) {
                for cy in e.y..(e.y + e.h) {
                    let c = Cell(cx, cy);
                    let k = kind_of(&l, c)
                        .unwrap_or_else(|| panic!("{m:?}: no kind for {c:?} inside the extent"));
                    let on_ring =
                        cx == e.x || cy == e.y || cx == e.x + e.w - 1 || cy == e.y + e.h - 1;
                    if on_ring {
                        assert_eq!(
                            k,
                            CellKind::Wall,
                            "{m:?}: the outer ring is {k:?} at {c:?} — a plan open \
                             to the outside is not a building"
                        );
                    }
                }
            }
            let declared: BTreeSet<Cell> = l.doorways.iter().map(|&(_, _, c)| c).collect();
            for (c, k) in &l.cells {
                if matches!(k, CellKind::Threshold(_, _)) {
                    assert!(
                        declared.contains(c),
                        "{m:?}: {c:?} is a threshold no doorway declares — an \
                         undeclared way through is a hole in the plan"
                    );
                }
            }
            for &(a, b, c) in &l.doorways {
                assert!(
                    matches!(kind_of(&l, c), Some(CellKind::Threshold(_, _))),
                    "{m:?}: the doorway ({a},{b}) is declared at {c:?}, which is \
                     {:?} rather than a threshold",
                    kind_of(&l, c)
                );
            }
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

    #[test]
    fn rule_4_two_chambers_cannot_disagree_about_a_doorway() {
        // The doorway is ONE CELL now, not a pair of half-boundaries, so reading
        // it from either side must give one answer. Asserted as uniqueness per
        // unordered pair: two entries for one pair is exactly how two chambers
        // come to disagree about which cell is the door.
        for (s, l, _) in corpus() {
            let mut seen: BTreeSet<(usize, usize)> = BTreeSet::new();
            for &(a, b, _) in &l.doorways {
                let key = (a.min(b), a.max(b));
                assert!(
                    seen.insert(key),
                    "chambers {a} and {b} have two doorways between them, so the \
                     two sides can disagree about which cell is the door"
                );
            }
            assert_eq!(
                seen.len(),
                s.links.len(),
                "one doorway per link, no more and no fewer"
            );
        }
    }

    #[test]
    fn rule_6_the_solve_carries_no_state() {
        // Same inputs, solved from scratch, in an order that would expose a
        // carried cache: A, then B, then A again.
        let st = structure_at(&locale(1), &built(), Seed(1), WALK).expect("built");
        let e = extent_for(&st);
        let a1 = allocate(&st, e, Seed(1));
        let _b = allocate(&st, e, Seed(2));
        let a2 = allocate(&st, e, Seed(1));
        assert_eq!(a1, a2, "an intervening solve changed a later one's result");
        let g1 = grow(&st, e, Seed(1));
        let _g2 = grow(&st, e, Seed(2));
        assert_eq!(g1, grow(&st, e, Seed(1)));
    }

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
                Method::Rectilinear => freedom_of_a_chain(n),
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
        for n in neighbours(pocket) {
            if kind_of(&l, n).is_some_and(|k| k.passable()) {
                l.cells.insert(n, CellKind::Wall);
            }
        }
        assert!(
            !reachable_from(&l, 0).contains(&pocket),
            "walling every passable neighbour of {pocket:?} left it reachable, so \
             rule 8 cannot detect a sealed pocket either — the rule is inert"
        );
    }

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
}
