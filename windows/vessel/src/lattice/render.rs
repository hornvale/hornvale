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
//! Walls are drawn from `Lattice::walls`, never inferred from region boundaries a
//! second time. A wall drawn by independent arithmetic is exactly how a picture
//! comes to disagree with the world it depicts (§7 rule 2) — and Task 3 found
//! exactly that defect twice, in two passes deciding independently what a boundary
//! was, so this is not a hypothetical.
//!
//! Which chamber a cell belongs to comes from `Lattice::owner`, NEVER from
//! `regions`: a grown lattice's rects are overlapping bounding boxes, so a rect
//! scan answers a different question and happens to agree only for rectilinear
//! plans (ledger #17).
//!
//! # A wall lives BETWEEN cells, so the picture is doubled
//!
//! The plan's sketched render drew one glyph per cell and called a cell a wall
//! when every way out of it was walled. **That glyph never fires.**
//! `MIN_CHAMBER_SPAN` is 2, so every region is at least 2x2 and every cell of it
//! has a same-region orthogonal neighbour that no wall separates — so no cell is
//! ever walled in on all four sides, and a 1:1 picture of this lattice contains
//! no `#` at all. Measured, not reasoned: the plan's own
//! `map_indoors_draws_a_floor_plan` fails on `plan.contains('#')`.
//!
//! The cause is structural rather than a slip. Every cell of this lattice is
//! FLOOR — `owner` assigns all of them to a chamber — and a wall is a
//! NON-ADJACENCY between two cells, which is a property of the boundary and not
//! of either cell. A 1:1 grid has nowhere to draw it. So the picture is
//! `(2w+1) x (2h+1)`: odd positions are cells, even positions are the boundaries
//! between them, and every `#` stands exactly where a non-adjacency does.
//!
//! What the picture depicts is PASSABILITY, taken off the wall set: `.` where a
//! mover may pass or stand, `#` where it may not, `+` where the passage it may
//! take crosses from one chamber into another. The extent's rim is drawn `#` from
//! the extent rather than from `walls` — the lattice is the whole structure, so
//! there is no cell outside it and `walls_around` records no pair for a
//! neighbour that has no owner. That is the one `#` in this picture not read off
//! the wall set, and it is called out here because a reader is entitled to know
//! which is which.

use super::{Cell, Lattice};

/// The glyph for a cell a mover may stand in, or a boundary it may cross.
/// type-audit: bare-ok(render-internal)
pub const FLOOR: char = '.';
/// The glyph for a non-adjacency: the boundary a mover may not cross.
/// type-audit: bare-ok(render-internal)
pub const WALL: char = '#';
/// The glyph for a threshold: the one boundary between two chambers that a
/// declared doorway opens.
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
    /// The picture, one line per doubled-grid row, each row newline-terminated.
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
    let (last_x, last_y) = (2 * e.w, 2 * e.h);
    let mut picture = String::with_capacity(((last_x + 2) * (last_y + 1)) as usize);
    for py in 0..=last_y {
        for px in 0..=last_x {
            picture.push(glyph(lattice, px, py));
        }
        picture.push('\n');
    }
    // Read back off the finished picture, so the legend cannot name a glyph the
    // render never drew. A doorway is the case that matters: `doorways` always
    // holds one entry per link, but a grown lattice whose two blobs never met
    // exempts no threshold, and the legend must not promise a `+` that is not
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

/// The glyph at one doubled-grid position.
///
/// `px` odd is a cell column and `px` even is the boundary between two of them;
/// likewise `py`. So an odd/odd position is a cell, an even/odd or odd/even
/// position is one wall segment, and an even/even position is the junction where
/// up to four segments meet.
fn glyph(lattice: &Lattice, px: i32, py: i32) -> char {
    let e = lattice.extent;
    if px == 0 || py == 0 || px == 2 * e.w || py == 2 * e.h {
        return WALL;
    }
    match (px % 2, py % 2) {
        // A cell. Every cell of a lattice belongs to a chamber, so every cell is
        // floor; what a mover may do is decided at the boundaries.
        (1, 1) => FLOOR,
        // A vertical segment, between the cells left and right of it.
        (0, 1) => {
            let cy = e.y + (py - 1) / 2;
            between(lattice, Cell(e.x + px / 2 - 1, cy), Cell(e.x + px / 2, cy))
        }
        // A horizontal segment, between the cells above and below it.
        (1, 0) => {
            let cx = e.x + (px - 1) / 2;
            between(lattice, Cell(cx, e.y + py / 2 - 1), Cell(cx, e.y + py / 2))
        }
        // A junction: wall exactly when one of the segments meeting here is, so
        // a wall's line stays unbroken and an open junction stays open. Read off
        // the segments themselves rather than off the geometry a second time —
        // one derivation, which is the rule the whole module follows. Terminates
        // at depth one: a junction's four neighbours are never junctions.
        _ => {
            if [(1, 0), (-1, 0), (0, 1), (0, -1)]
                .iter()
                .any(|&(dx, dy)| glyph(lattice, px + dx, py + dy) == WALL)
            {
                WALL
            } else {
                FLOOR
            }
        }
    }
}

/// The glyph for the boundary between two adjacent cells.
///
/// The wall set is the authority on passability, in both directions: walled means
/// no passage, and unwalled means the mover may cross — which is exactly what
/// `classify::openings` reads. A crossing between two DIFFERENT chambers is a
/// threshold, and by §7 rule 3 the only unwalled cross-chamber boundaries are the
/// declared doorways, so `+` marks a doorway without this function needing to
/// consult `doorways` and risk a second opinion about where one is.
fn between(lattice: &Lattice, a: Cell, b: Cell) -> char {
    if lattice.walls.contains(&(a.min(b), a.max(b))) {
        return WALL;
    }
    match (lattice.owner.get(&a), lattice.owner.get(&b)) {
        (Some(x), Some(y)) if x != y => DOORWAY,
        // Same chamber, or a cell no chamber owns. An unowned cell inside the
        // extent is a hole `grow` claims cannot happen and §7 rules 1-3 are what
        // would report — but nothing walls it, so a mover could cross, and
        // drawing floor is what is true about the crossing rather than what is
        // comfortable about the derivation.
        _ => FLOOR,
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
    const SEEDS: std::ops::Range<u64> = 0..16;

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
    fn the_picture_encodes_the_wall_set_exactly() {
        // Rule 2 for the PICTURE, read back off the drawn characters rather than
        // taken from the code that drew them: every `#` between two cells of the
        // extent is a wall the lattice holds, every `+` is an unwalled crossing
        // between two chambers, and nothing else is drawn where a wall stands.
        for (_, l) in corpus() {
            let p = render(&l);
            let grid = rows(&p);
            let (last_x, last_y) = (2 * l.extent.w, 2 * l.extent.h);
            assert_eq!(grid.len(), (last_y + 1) as usize, "row count");
            for r in &grid {
                assert_eq!(r.len(), (last_x + 1) as usize, "row width");
            }
            for py in 0..=last_y {
                for px in 0..=last_x {
                    let g = grid[py as usize][px as usize];
                    if px == 0 || py == 0 || px == last_x || py == last_y {
                        assert_eq!(g, WALL, "the extent's rim is a wall at ({px},{py})");
                        continue;
                    }
                    let pair = match (px % 2, py % 2) {
                        (1, 1) => {
                            assert_eq!(g, FLOOR, "a cell is floor at ({px},{py})");
                            continue;
                        }
                        (0, 1) => {
                            let cy = l.extent.y + (py - 1) / 2;
                            Some((
                                Cell(l.extent.x + px / 2 - 1, cy),
                                Cell(l.extent.x + px / 2, cy),
                            ))
                        }
                        (1, 0) => {
                            let cx = l.extent.x + (px - 1) / 2;
                            Some((
                                Cell(cx, l.extent.y + py / 2 - 1),
                                Cell(cx, l.extent.y + py / 2),
                            ))
                        }
                        // A junction carries no boundary of its own.
                        _ => None,
                    };
                    let Some((a, b)) = pair else { continue };
                    let walled = l.walls.contains(&(a.min(b), a.max(b)));
                    let crosses = matches!(
                        (l.owner.get(&a), l.owner.get(&b)),
                        (Some(x), Some(y)) if x != y
                    );
                    let want = if walled {
                        WALL
                    } else if crosses {
                        DOORWAY
                    } else {
                        FLOOR
                    };
                    assert_eq!(
                        g, want,
                        "({px},{py}) draws {g:?} for the boundary {a:?}-{b:?}, which \
                         the lattice says is {want:?}"
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
        // the render would be 1:1 plus a border; the render is doubled, so the
        // claim is re-founded on the picture the render actually draws.
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
