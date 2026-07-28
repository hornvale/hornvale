//! Read the realized graph back off a solved lattice, and check it against the
//! graph that was specified.
//!
//! This is the direction that makes the embedder falsifiable. `allocate` claims
//! to realize `structure.links` by construction; this module does not believe it.
//! Reading adjacency back off the geometry — rather than trusting the code path
//! that wrote it — is what turns Amendment 2 §1b.8's seven rules from prose into
//! assertions.

use super::{Cell, Lattice};
use std::collections::BTreeSet;

/// Which chamber owns `cell`. `None` outside the extent.
///
/// Reads `Lattice::owner`, never `regions`. Scanning `regions` would be wrong for a
/// grown lattice, whose rects are overlapping bounding boxes — a scan answers
/// "which bounding box does this fall in first", which is a different question and
/// happens to agree only for rectilinear lattices. That near-agreement is exactly
/// what would have made the bug survive review.
/// type-audit: bare-ok(index: return)
pub fn region_of(lattice: &Lattice, cell: Cell) -> Option<usize> {
    lattice.owner.get(&cell).copied()
}

/// Every unordered pair of chambers whose regions actually touch across a cell
/// boundary that is NOT walled — the adjacency a mover can use, read off the
/// geometry rather than taken from `links`.
/// type-audit: bare-ok(index: return)
pub fn realized_links(lattice: &Lattice) -> BTreeSet<(usize, usize)> {
    let mut out = BTreeSet::new();
    for (a, b) in openings(lattice) {
        if let (Some(ra), Some(rb)) = (region_of(lattice, a), region_of(lattice, b))
            && ra != rb
        {
            out.insert((ra.min(rb), ra.max(rb)));
        }
    }
    out
}

/// Every adjacent cell pair inside the extent that no wall separates — the
/// complete set of ways a mover may pass between two cells.
pub fn openings(lattice: &Lattice) -> Vec<(Cell, Cell)> {
    let mut out = Vec::new();
    let e = lattice.extent;
    for cx in e.x..(e.x + e.w) {
        for cy in e.y..(e.y + e.h) {
            let here = Cell(cx, cy);
            for (dx, dy) in [(1, 0), (0, 1)] {
                let there = Cell(cx + dx, cy + dy);
                if !e.contains(there) {
                    continue;
                }
                let pair = (here.min(there), here.max(there));
                if !lattice.walls.contains(&pair) {
                    out.push(pair);
                }
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
    const SEEDS: std::ops::Range<u64> = 0..24;

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
                    "{m:?}: {pair:?} are adjacent in the lattice but not linked in \
                     the graph — the embedder invented a relation"
                );
            }
        }
    }

    #[test]
    fn rule_2_every_wall_is_a_non_adjacency() {
        for (_, l, _) in corpus() {
            for &(a, b) in &l.walls {
                let (ra, rb) = (region_of(&l, a), region_of(&l, b));
                assert!(
                    ra != rb || ra.is_none(),
                    "a wall stands between {a:?} and {b:?}, which are both in \
                     region {ra:?} — a decorative wall is a lie about the world"
                );
            }
        }
    }

    /// Closure: every way between two REGIONS must be a declared doorway. An
    /// undeclared gap is a hole in the plan the render would draw as floor and
    /// the mover would walk through without a door.
    ///
    /// **Tautological over a DERIVED lattice, and stated rather than hidden.**
    /// `walls_around` walls every cross-owner pair except the declared
    /// thresholds, so "unwalled and cross-region implies a door cell" is the
    /// contrapositive of its own exemption condition read back off the same
    /// ownership map. Over today's two methods this checks the wall
    /// derivation's SELF-CONSISTENCY, not closure independently.
    ///
    /// What would make it load-bearing is a `walls` set written by anything
    /// other than `walls_around`: the spec §3.2 radial and branching methods,
    /// or a hand-authored fixture. Until one exists, the rule is a real check
    /// waiting for a second writer — which is a different thing from a check
    /// that passed, and [`rule_3_actually_fails_on_an_unclosed_lattice`] is
    /// what keeps the difference honest by proving the condition CAN fail.
    #[test]
    fn rule_3_no_opening_is_unaccounted_for() {
        for (_, l, m) in corpus() {
            let doors: BTreeSet<Cell> = l.doorways.iter().map(|&(_, _, c)| c).collect();
            for (a, b) in openings(&l) {
                let (ra, rb) = (region_of(&l, a), region_of(&l, b));
                if ra == rb {
                    continue;
                }
                assert!(
                    doors.contains(&a) || doors.contains(&b),
                    "{m:?}: cells {a:?} and {b:?} lie in different regions with no \
                     wall and no doorway between them"
                );
            }
        }
    }

    #[test]
    fn rule_3_actually_fails_on_an_unclosed_lattice() {
        // The NEGATIVE CONTROL for the rule above. Rule 3 passes on every
        // derived lattice by construction, so a green rule 3 is no evidence the
        // condition can discriminate at all — and this task is the first to
        // build a PICTURE on the wall set, so the wall set's closure check had
        // better be able to fail. Take a real lattice, delete one wall, and
        // assert the closure condition catches it. Same posture as deleting a
        // type-audit tag to prove the tool reads the file.
        let (s, mut l, _) = corpus()
            .into_iter()
            .find(|(s, _, _)| s.chambers.len() > 1)
            .expect("the corpus reaches MAX_CHAMBERS, so a multi-region plan is in it");
        assert!(
            !l.walls.is_empty(),
            "a {}-chamber plan must have walls for this control to remove one",
            s.chambers.len()
        );
        let victim = *l
            .walls
            .iter()
            .next()
            .expect("a multi-region plan has walls");
        l.walls.remove(&victim);
        let doors: BTreeSet<Cell> = l.doorways.iter().map(|&(_, _, c)| c).collect();
        let leak = openings(&l).into_iter().find(|&(a, b)| {
            region_of(&l, a) != region_of(&l, b) && !doors.contains(&a) && !doors.contains(&b)
        });
        assert!(
            leak.is_some(),
            "removing the wall between {victim:?} did not produce an unaccounted \
             opening, so rule 3 cannot detect one either — the rule is not merely \
             tautological over derived lattices, it is inert"
        );
    }

    #[test]
    fn rule_4_two_chambers_cannot_disagree_about_a_doorway() {
        // The doorway derives from the shared EDGE, so reading it from either
        // side must give one answer. Asserted as uniqueness per unordered pair:
        // two entries for one pair is exactly how two chambers come to disagree.
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
        for (s, l, m) in corpus() {
            // Each method's budget is stated in ITS OWN terms, up front. A budget
            // computed from the result -- widened whenever it is exceeded -- is not
            // a check, and that is the shape this test must not take.
            let n = s.chambers.len();
            let budget = match m {
                // One cut per interior boundary; the seed moves the cut and
                // nothing else.
                Method::Rectilinear => freedom_of_a_chain(n),
                // A seed cell is a POINT, so two draws per chamber, not one.
                Method::Grown => 2 * n as u32,
            };
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
    }
}
