//! ASCII, and the legend that makes it checkable.
//!
//! The legend is the deliverable as much as the picture is. §6's parity contract
//! is tested by walking what the render CLAIMS to depict and demanding that
//! `examine` accept each of it — so a render that draws a thing without naming it
//! in the legend is a render the parity test cannot check, which is how The
//! Lintel's `look`-named-but-`examine`-denied jar shipped. The converse is
//! guarded here rather than trusted: the legend is read back off the finished
//! picture, so it cannot name a glyph the picture never draws.
//!
//! # 1:1, because a wall is a cell
//!
//! Task 4 had to draw this picture at `(2w+1) x (2h+1)`. A wall was a
//! NON-ADJACENCY then — a property of the boundary between two cells, and a 1:1
//! grid has nowhere to put one — so odd positions were cells, even positions were
//! the boundaries between them, and every glyph had a coordinate mapping standing
//! between it and the lattice.
//!
//! Task 4b reified the wall, and this module is where the saving is collected. One
//! glyph per cell, read straight off [`Lattice::cells`]: `Floor` → `.`, `Wall` →
//! `#`, `Threshold` → `+`. There is no arithmetic between a picture position and a
//! cell, so there is no off-by-one class to get wrong — which matters most for
//! Task 5, which was about to inherit it in order to mark where the possession
//! stands.
//!
//! Nothing is inferred a second time. Task 3 found two defects in exactly that
//! shape — two passes deciding independently what a boundary was — so the picture
//! asks the kind map and nothing else. Notably the exterior wall is no longer a
//! special case drawn from the extent: it is `Wall` cells like any other, which is
//! also what makes §7 rule 3(i) a check the render cannot quietly satisfy on its
//! own.

use super::{Cell, CellKind, Lattice};

/// The glyph for a cell a mover may stand in.
/// type-audit: bare-ok(render-internal)
pub const FLOOR: char = '.';
/// The glyph for the building's fabric: a cell a mover may not enter.
/// type-audit: bare-ok(render-internal)
pub const WALL: char = '#';
/// The glyph for a threshold: the cell a declared doorway opens between two
/// chambers.
/// type-audit: bare-ok(render-internal)
pub const DOORWAY: char = '+';

/// What the legend calls [`FLOOR`].
/// type-audit: bare-ok(prose)
pub const FLOOR_NOUN: &str = "the floor";
/// What the legend calls [`WALL`].
/// type-audit: bare-ok(prose)
pub const WALL_NOUN: &str = "a wall";
/// What the legend calls [`DOORWAY`]. The SAME words
/// `chamber_prose::noun(AnchorKind::Threshold)` uses, asserted by
/// `the_plans_word_for_a_doorway_is_the_chambers_word` — two words for one thing
/// is the drift §6 exists to prevent, and a player who reads `a doorway` off the
/// plan will type exactly that.
/// type-audit: bare-ok(prose)
pub const DOORWAY_NOUN: &str = "a doorway";

/// A drawn plan: the picture, and what each glyph means.
/// type-audit: bare-ok(prose: picture), bare-ok(identifier-text: legend)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Plan {
    /// The picture, one line per grid row, each row newline-terminated.
    pub picture: String,
    /// Glyph and its meaning, in drawing order. What the parity test walks.
    pub legend: Vec<(char, &'static str)>,
}

/// Draw `lattice`.
///
/// Takes the lattice alone. The plan's signature offered `structure` and `at` for
/// Task 6's region names, with an explicit licence to drop them rather than leave
/// an unexplained `let _ =` — dropped, for two reasons worth recording. Nothing
/// in v1's picture varies with the chamber stood in, because there is no CELL
/// position to mark yet: Task 5 is what gives the possession one, and that is the
/// task where a "you are here" mark stops being a guess. And the caption naming
/// which chamber you stand in belongs to the session, which already owns the
/// `[chamber id, day]` block this plan's header mirrors.
pub fn render(lattice: &Lattice) -> Plan {
    let e = lattice.extent;
    let mut picture = String::with_capacity(((e.w + 1) * e.h) as usize);
    for cy in e.y..(e.y + e.h) {
        for cx in e.x..(e.x + e.w) {
            picture.push(glyph(lattice.cells.get(&Cell(cx, cy))));
        }
        picture.push('\n');
    }
    // Read back off the finished picture, so the legend cannot name a glyph the
    // render never drew. A doorway is the case that matters: `doorways` always
    // holds one entry per link, but a grown lattice whose two blobs never met
    // carves no threshold, and the legend must not promise a `+` that is not
    // there (§7 rule 1 is what reports the underlying failure; this only refuses
    // to paper over it).
    let legend = [
        (FLOOR, FLOOR_NOUN),
        (WALL, WALL_NOUN),
        (DOORWAY, DOORWAY_NOUN),
    ]
    .into_iter()
    .filter(|&(g, _)| picture.contains(g))
    .collect();
    Plan { picture, legend }
}

/// The glyph for one cell's kind.
///
/// `None` is unreachable — [`Lattice::cells`] is total over the extent and §7 rule
/// 3 checks it — and drawn as fabric rather than as a distinct glyph, because a
/// cell the map does not hold is not a cell a mover may stand in. Matched on the
/// variant here rather than through `passable()` on purpose: this is the one place
/// whose whole job is to distinguish the kinds, so a `Rubble` arriving must appear
/// as an unhandled arm and force a decision about how it draws.
fn glyph(kind: Option<&CellKind>) -> char {
    match kind {
        Some(CellKind::Floor(_)) => FLOOR,
        Some(CellKind::Threshold(_, _)) => DOORWAY,
        Some(CellKind::Wall) | None => WALL,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::lattice::{embed_with, extent_for};
    use crate::structure::structure_at;
    use hornvale_kernel::{RoomAddr, Seed};

    const WALK: u32 = 12;
    const SEEDS: std::ops::Range<u64> = 0..48;

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

    /// Every (structure, lattice) pair the render is checked over: both methods,
    /// many seeds, so every chamber count `structure_at` produces.
    fn corpus() -> Vec<(crate::structure::Structure, Lattice)> {
        let mut out = Vec::new();
        for s in SEEDS {
            let st = structure_at(&locale(s), &built(), Seed(s), WALK).expect("built");
            let e = extent_for(&st);
            out.push((st.clone(), embed_with(&st, &built(), e, Seed(s))));
            out.push((st.clone(), embed_with(&st, &wild(), e, Seed(s))));
        }
        assert_eq!(
            out.iter().map(|(s, _)| s.chambers.len()).max().unwrap(),
            crate::structure::MAX_CHAMBERS,
            "the corpus never reaches MAX_CHAMBERS, so the render is unchecked at \
             the count most likely to break it"
        );
        out
    }

    fn rows(plan: &Plan) -> Vec<Vec<char>> {
        plan.picture.lines().map(|r| r.chars().collect()).collect()
    }

    #[test]
    fn the_picture_encodes_the_kind_map_exactly() {
        // Read back off the drawn characters rather than taken from the code that
        // drew them, and it is the whole picture now rather than the odd/even
        // subset Task 4 could check — the exterior wall included, which used to be
        // the one glyph the readback had to take on trust because the render drew
        // it from the extent rather than from the lattice.
        for (_, l) in corpus() {
            let p = render(&l);
            let grid = rows(&p);
            let e = l.extent;
            assert_eq!(grid.len(), e.h as usize, "row count");
            for r in &grid {
                assert_eq!(r.len(), e.w as usize, "row width");
            }
            for cy in e.y..(e.y + e.h) {
                for cx in e.x..(e.x + e.w) {
                    let g = grid[(cy - e.y) as usize][(cx - e.x) as usize];
                    let k = l
                        .cells
                        .get(&Cell(cx, cy))
                        .copied()
                        .unwrap_or_else(|| panic!("no kind at ({cx},{cy})"));
                    let want = match k {
                        CellKind::Floor(_) => FLOOR,
                        CellKind::Wall => WALL,
                        CellKind::Threshold(_, _) => DOORWAY,
                    };
                    assert_eq!(
                        g, want,
                        "({cx},{cy}) draws {g:?} for a cell the lattice says is \
                         {k:?}, which draws {want:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn the_legend_names_only_glyphs_the_picture_draws() {
        for (_, l) in corpus() {
            let p = render(&l);
            for (g, noun) in &p.legend {
                assert!(
                    p.picture.contains(*g),
                    "the legend names {g:?} ({noun}) but the picture never draws it"
                );
            }
            // And the other direction: every glyph drawn is named, or the parity
            // test walks a legend that omits the thing a player asks about.
            for g in p.picture.chars().filter(|c| *c != '\n') {
                assert!(
                    p.legend.iter().any(|&(l, _)| l == g),
                    "the picture draws {g:?} and the legend does not name it"
                );
            }
        }
    }

    #[test]
    fn a_doorway_is_drawn_once_per_declared_link() {
        for (s, l) in corpus() {
            let p = render(&l);
            assert_eq!(
                p.picture.matches(DOORWAY).count(),
                s.links.len(),
                "{} links but {} doorways drawn — a drawn destination no link \
                 asserts is invented, and a link with none drawn is dropped",
                s.links.len(),
                p.picture.matches(DOORWAY).count()
            );
        }
    }

    #[test]
    fn the_picture_has_an_unbroken_border_of_wall() {
        // §7 rule 3(i) as a reader sees it. The rule is asserted on the lattice in
        // `classify`; this asserts it on the PICTURE, because "the plan reads as a
        // building" is a claim about the drawn thing and a reader checks the border
        // before anything else.
        for (_, l) in corpus() {
            let p = render(&l);
            let grid = rows(&p);
            let last = grid.len() - 1;
            for (y, row) in grid.iter().enumerate() {
                let edge = y == 0 || y == last;
                for (x, g) in row.iter().enumerate() {
                    if edge || x == 0 || x == row.len() - 1 {
                        assert_eq!(*g, WALL, "the border is {g:?} at ({x},{y}):\n{}", p.picture);
                    }
                }
            }
        }
    }

    #[test]
    fn a_single_chamber_plan_is_one_room_with_no_doorway() {
        // The degenerate count, which the corpus reaches but never isolates: one
        // chamber has no links, so a `+` drawn here would be pure invention and
        // the legend must not promise one.
        let s = crate::structure::Structure {
            threshold: locale(0),
            chambers: vec![locale(0)],
            links: Vec::new(),
        };
        let l = embed_with(&s, &built(), extent_for(&s), Seed(3));
        let p = render(&l);
        assert!(!p.picture.contains(DOORWAY), "{}", p.picture);
        assert!(!p.legend.iter().any(|&(g, _)| g == DOORWAY));
        assert!(p.picture.contains(FLOOR) && p.picture.contains(WALL));
    }

    #[test]
    fn the_render_is_pure() {
        for (_, l) in corpus() {
            assert_eq!(render(&l), render(&l));
        }
    }

    #[test]
    fn the_plans_word_for_a_doorway_is_the_chambers_word() {
        // Two wordings for one thing is the drift §6 exists to prevent: a player
        // reads `a doorway` off the plan and types exactly that, and `examine`
        // resolves it against the chamber's anchors.
        assert_eq!(
            Some(DOORWAY_NOUN),
            crate::chamber_prose::noun(crate::interior::AnchorKind::Threshold)
        );
    }

    #[test]
    fn the_widest_plan_fits_a_terminal() {
        // A floor plan is read in a transcript, so the ceiling on CHAMBER_SIDE is
        // a rendering fact. Task 1 asserted it on the EXTENT as a proxy, guessing
        // the render would be 1:1 plus a border; Task 4 found the render was
        // doubled and re-founded the claim on the picture. Task 4b makes the
        // original guess true — 1:1, border included — and the claim stays here,
        // on the drawn thing, because that is where it belongs whatever the model.
        //
        // WIDTH is the hard bound: past 80 columns a transcript wraps and a plan
        // stops being legible at all. Height is a different kind of constraint —
        // a tall plan scrolls, which costs a reader nothing — so it is bounded
        // only loosely, as a ceiling a real regression would blow through.
        for n in 1..=crate::structure::MAX_CHAMBERS {
            let chambers: Vec<RoomAddr> = (0..n).map(|i| locale(i as u64)).collect();
            let s = crate::structure::Structure {
                threshold: chambers[0].clone(),
                links: (1..n).map(|i| (i - 1, i)).collect(),
                chambers,
            };
            let p = render(&embed_with(&s, &built(), extent_for(&s), Seed(1)));
            let grid = rows(&p);
            let width = grid[0].len();
            assert!(
                width <= 80,
                "{n} chambers draw a {width}-column plan, which wraps in an \
                 80-column transcript and stops being a plan"
            );
            assert!(
                grid.len() <= 40,
                "{n} chambers draw a {}-row plan",
                grid.len()
            );
        }
    }
}
