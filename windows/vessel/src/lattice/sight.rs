//! `shadowcast` — what can be seen from a cell, symmetrically.
//!
//! # Symmetric, deliberately (spec §4)
//!
//! [`crate::interior::Interior`]'s `adjacency` is a `BTreeSet` of pairs and so
//! is symmetric by construction. Ordinary recursive shadowcasting is famously
//! **asymmetric** — A sees B while B cannot see A — and in a sim where
//! perception feeds belief that is a modelling commitment rather than an
//! implementation detail. An asymmetry the relational layer does not have
//! would be an artifact of the metric embedding, which spec §2.1 exists to
//! prevent. So the variant here is the **symmetric** one, and
//! `sight_is_symmetric` is a property test over every ordered pair of floor
//! cells rather than a comment.
//!
//! # The variant: Ford's symmetric shadowcasting, over four quadrants
//!
//! The recursion is the ordinary one — scan a row, split the slope band at
//! every floor/wall transition, recurse a row deeper — with one change, and
//! the change is the whole point: a **passable** cell is lit only when its
//! CENTRE lies inside the surviving slope band ([`is_symmetric`]), never
//! merely when the band clips its square. Centre-in-band is a relation
//! between two cell centres and the fabric on the segment between them, and
//! nothing in it distinguishes the two ends — which is where the symmetry
//! comes from. The permissive rule ("lit if any part of the square is
//! reachable") is what makes plain shadowcasting one-sided.
//!
//! Impassable cells are the deliberate exception: a wall is lit whenever the
//! band touches it at all, so a room's fabric is drawn rather than showing as
//! a ragged fringe of gaps. Sight between two WALLS is therefore not claimed
//! to be symmetric, and the property test asks only about passable cells —
//! which are the cells anything can stand in, and the only ones a `sensed`
//! channel will ever narrow to.
//!
//! # No floats, anywhere
//!
//! Every slope is an exact rational `num / den` with `den > 0` ([`Slope`]),
//! and every comparison is a cross-multiplication. This is the reason
//! `kernel/src/astar.rs` gives for its `u64` costs: a `f64` slope comparison
//! decides which cells are lit, so a last-ULP difference would be a
//! determinism defect, not a rounding cosmetic one. The magnitudes stay tiny
//! — no product here exceeds `4 * radius²` — so `i32` is not near its edge.
//!
//! # Radius is CHEBYSHEV
//!
//! `radius` bounds `max(|dx|, |dy|)`, not the Euclidean distance: a scan at
//! depth `d` only ever emits columns in `-d..=d`, so the lit set is exactly
//! the square of side `2 * radius + 1` around the origin, minus what the
//! fabric hides. Chebyshev is symmetric, so bounding it costs the property
//! above nothing. `radius_bounds_the_result` states the metric and
//! `the_radius_is_reached_as_well_as_respected` checks the bound is not
//! vacuous.

use crate::lattice::{Cell, Lattice, kind_of};
use std::collections::BTreeSet;

/// Which cells of `lattice` are visible from `from`, out to Chebyshev
/// `radius`.
///
/// Symmetric between passable cells: for any two passable `a` and `b`, `b` is
/// in `shadowcast(l, a, r)` exactly when `a` is in `shadowcast(l, b, r)`. See
/// the module docs for the variant and for why walls are the stated
/// exception.
///
/// `from` is always in the result, whatever it is and whatever the radius —
/// you can always see where you are standing. Cells outside
/// [`Lattice::extent`] never are: [`kind_of`] is total over the extent, so
/// "outside" and "wall" are the same to the scan and neither is a place.
///
/// A `radius` below 1 lights the origin alone.
/// type-audit: bare-ok(count: radius)
pub fn shadowcast(lattice: &Lattice, from: Cell, radius: i32) -> BTreeSet<Cell> {
    let mut lit = BTreeSet::new();
    lit.insert(from);
    if radius < 1 {
        return lit;
    }
    for quadrant in 0..4 {
        scan(
            lattice,
            from,
            quadrant,
            radius,
            1,
            Slope { num: -1, den: 1 },
            Slope { num: 1, den: 1 },
            &mut lit,
        );
    }
    lit
}

/// An exact rational slope `num / den`, with `den > 0` always.
///
/// Held unreduced: nothing here needs a canonical form, only comparisons, and
/// every comparison is a cross-multiplication that reduction would not change.
/// type-audit: bare-ok(count: num), bare-ok(count: den)
#[derive(Clone, Copy, Debug)]
struct Slope {
    /// Numerator; may be negative.
    num: i32,
    /// Denominator; always strictly positive, which is what lets a
    /// cross-multiplied comparison keep its direction.
    den: i32,
}

/// The slope of the leading edge of the cell at `(depth, col)`: `(2col - 1) /
/// 2depth`, exactly. Half-integers are why the denominator carries the 2.
fn edge(depth: i32, col: i32) -> Slope {
    Slope {
        num: 2 * col - 1,
        den: 2 * depth,
    }
}

/// `floor(a / b)` for `b > 0`.
/// type-audit: bare-ok(count: a), bare-ok(count: b), bare-ok(count: return)
fn floor_div(a: i32, b: i32) -> i32 {
    a.div_euclid(b)
}

/// `ceil(a / b)` for `b > 0`.
/// type-audit: bare-ok(count: a), bare-ok(count: b), bare-ok(count: return)
fn ceil_div(a: i32, b: i32) -> i32 {
    -((-a).div_euclid(b))
}

/// The first column of the row at `depth` that `start` admits: `depth * start`
/// rounded to nearest with **ties up**, which is `floor(depth * start + 1/2)`
/// and here `floor((2 * depth * num + den) / (2 * den))`.
fn first_col(depth: i32, start: Slope) -> i32 {
    floor_div(2 * depth * start.num + start.den, 2 * start.den)
}

/// The last column of the row at `depth` that `end` admits: rounded to nearest
/// with **ties down**, which is `ceil(depth * end - 1/2)`.
///
/// Ties break the opposite way from [`first_col`] on purpose — a cell whose
/// centre sits exactly on a band edge belongs to one side of it, and letting
/// both ends claim it is one of the ways a shadowcast stops being symmetric.
fn last_col(depth: i32, end: Slope) -> i32 {
    ceil_div(2 * depth * end.num - end.den, 2 * end.den)
}

/// Is the CENTRE of the cell at `(depth, col)` inside the band `start..=end`?
///
/// **The symmetry condition.** A passable cell is lit only when this holds,
/// so being lit means "the centre-to-centre line survives the fabric", which
/// reads the same from either end. Cross-multiplied, and both denominators
/// are positive, so the inequalities keep their direction.
/// type-audit: bare-ok(flag: return)
fn is_symmetric(depth: i32, col: i32, start: Slope, end: Slope) -> bool {
    col * start.den >= depth * start.num && col * end.den <= depth * end.num
}

/// Where the cell at `(depth, col)` of `quadrant` lands, in lattice
/// coordinates. Four quadrants, not eight octants: the diagonal cells belong
/// to two of them and are simply scanned twice, which costs a little and
/// removes the whole class of octant-boundary asymmetries.
fn transform(quadrant: u8, origin: Cell, depth: i32, col: i32) -> Cell {
    match quadrant {
        0 => Cell(origin.0 + col, origin.1 - depth),
        1 => Cell(origin.0 + col, origin.1 + depth),
        2 => Cell(origin.0 + depth, origin.1 + col),
        _ => Cell(origin.0 - depth, origin.1 + col),
    }
}

/// May sight pass through `cell`?
///
/// Asks [`CellKind::passable`](crate::lattice::CellKind::passable), never
/// `== CellKind::Wall` — that predicate's own doc says why, and a rule written
/// against the variant breaks the day `Rubble` arrives. Outside the extent
/// counts as opaque: `kind_of` is total over the extent, so `None` means
/// "not a place" and nothing else.
/// type-audit: bare-ok(flag: return)
fn transparent(lattice: &Lattice, cell: Cell) -> bool {
    kind_of(lattice, cell).is_some_and(|k| k.passable())
}

/// Scan one row of one quadrant, lighting what it admits and recursing into
/// the rows behind it.
///
/// The row's column range is fixed ONCE, from the band the row was entered
/// with, while `start` narrows as the scan leaves each wall behind it. The two
/// are deliberately different: `start` governs what is lit and what the next
/// row inherits, and recomputing the range from the narrowed value would drop
/// columns the scan is standing in the middle of.
///
/// Recursion is bounded by `radius`: every call goes exactly one row deeper,
/// and the depth guard is the first thing it checks.
#[allow(clippy::too_many_arguments)] // a quadrant scan's frame; splitting it into a struct would hide the recursion
fn scan(
    lattice: &Lattice,
    origin: Cell,
    quadrant: u8,
    radius: i32,
    depth: i32,
    start: Slope,
    end: Slope,
    lit: &mut BTreeSet<Cell>,
) {
    if depth > radius {
        return;
    }
    let (first, last) = (first_col(depth, start), last_col(depth, end));
    let mut start = start;
    // `None` before the first column of the row; `Some(true)` means the
    // previous cell was opaque.
    let mut previous_was_opaque: Option<bool> = None;
    for col in first..=last {
        let cell = transform(quadrant, origin, depth, col);
        let opaque = !transparent(lattice, cell);
        // A wall is lit whenever the band touches it; a floor only when its
        // centre is inside the band. That asymmetry between the two KINDS is
        // what buys symmetry between the two ENDS.
        if (opaque || is_symmetric(depth, col, start, end)) && lattice.cells.contains_key(&cell) {
            lit.insert(cell);
        }
        if previous_was_opaque == Some(true) && !opaque {
            // Leaving a wall's shadow: the rest of this row sees past it.
            start = edge(depth, col);
        }
        if previous_was_opaque == Some(false) && opaque {
            // Entering one: everything behind it, up to here, is still lit.
            scan(
                lattice,
                origin,
                quadrant,
                radius,
                depth + 1,
                start,
                edge(depth, col),
                lit,
            );
        }
        previous_was_opaque = Some(opaque);
    }
    if previous_was_opaque == Some(false) {
        scan(
            lattice,
            origin,
            quadrant,
            radius,
            depth + 1,
            start,
            end,
            lit,
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::brief::Brief;
    use crate::lattice::{Cell, Lattice, embed_with, extent_for, kind_of, shadowcast};
    use crate::structure::{Structure, structure_at};
    use hornvale_kernel::{RoomAddr, Seed};
    use std::collections::{BTreeMap, BTreeSet};

    const WALK: u32 = 12;

    fn built() -> Brief {
        Brief::from_parts(None, None, None, None, 0, true, true)
    }

    /// The brief that selects the GROWN embedding, passed to `embed_with` and
    /// nothing else — the same fixture idiom `anchor_cells` settled on.
    fn wild() -> Brief {
        Brief::from_parts(None, None, None, None, 0, false, true)
    }

    fn locale_number(n: u64) -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| ((n >> (2 * i)) & 0b11) as u8).collect(),
        }
    }

    fn structure_of(chamber_count: usize, seed: Seed) -> (RoomAddr, Structure) {
        for n in 0u64..4096 {
            let locale = locale_number(n);
            let s = structure_at(&locale, &built(), seed, WALK).expect("built");
            if s.chambers.len() == chamber_count {
                return (locale, s);
            }
        }
        panic!("no locale in 4096 draws a {chamber_count}-chamber structure at {seed:?}");
    }

    fn embedded(chamber_count: usize, seed: Seed, method: &Brief) -> Lattice {
        let (locale, structure) = structure_of(chamber_count, seed);
        embed_with(
            &structure,
            method,
            extent_for(&structure),
            locale.seed(seed),
        )
    }

    /// The rectilinear lattice production actually reaches.
    fn fixture(chamber_count: usize, seed: Seed) -> Lattice {
        embedded(chamber_count, seed, &built())
    }

    /// The GROWN lattice: non-convex, pinched, sometimes disconnected floor.
    /// The hostile geometry, where a naive sight test shows itself.
    fn wild_fixture(chamber_count: usize, seed: Seed) -> Lattice {
        embedded(chamber_count, seed, &wild())
    }

    fn floors(lattice: &Lattice) -> Vec<Cell> {
        lattice
            .cells
            .iter()
            .filter(|(_, k)| k.passable())
            .map(|(c, _)| *c)
            .collect()
    }

    fn some_floor_cell(lattice: &Lattice) -> Cell {
        *floors(lattice).first().expect("a lattice holds floor")
    }

    /// Every ordered pair of floor cells, in both directions, against ONE
    /// shadowcast per origin.
    ///
    /// The memo is the only concession: the property still compares both
    /// directions of every pair, because a sampled symmetry check cannot see
    /// the asymmetry it exists to find.
    fn assert_symmetric(lattice: &Lattice, label: &str) {
        let floors = floors(lattice);
        assert!(floors.len() > 8, "{label}: too little floor to be a test");
        let seen: BTreeMap<Cell, BTreeSet<Cell>> = floors
            .iter()
            .map(|&c| (c, shadowcast(lattice, c, 12)))
            .collect();
        for &a in &floors {
            for &b in &floors {
                let sees = seen[&a].contains(&b);
                let back = seen[&b].contains(&a);
                assert_eq!(
                    sees, back,
                    "{label}: {a:?} sees {b:?} = {sees}, but reverse = {back}"
                );
            }
        }
    }

    /// EVERY pair of floor cells this lattice separates by a solid span, in
    /// both directions.
    ///
    /// Axis-aligned on purpose, which is what makes the claim independent of
    /// the algorithm under test rather than a restatement of it: two floor
    /// cells on one row have a horizontal centre-to-centre line, so a run of
    /// impassable cells between them interrupts that line and nothing a
    /// centre-tracing sight test does can put it back. No slope arithmetic is
    /// involved in deciding the pair, so the pair cannot be wrong for the same
    /// reason `shadowcast` is.
    ///
    /// The whole list rather than one example. `fixture(2, Seed(4))` splits
    /// its two chambers on a DIAGONAL — the first thing this helper found was
    /// that a 19x10 plan has no three-cell-thick wall band anywhere — so a
    /// single hand-picked pair would be testing the one wall the fixture
    /// happened to grow.
    fn pairs_across_a_wall(lattice: &Lattice) -> Vec<(Cell, Cell)> {
        let solid = |c: Cell| !kind_of(lattice, c).is_some_and(|k| k.passable());
        let mut out = Vec::new();
        for (&from, kf) in &lattice.cells {
            if !kf.passable() {
                continue;
            }
            for (&to, kt) in &lattice.cells {
                if !kt.passable() {
                    continue;
                }
                let across = if from.1 == to.1 && to.0 > from.0 + 1 && to.0 - from.0 <= 12 {
                    (from.0 + 1..to.0).all(|x| solid(Cell(x, from.1)))
                } else if from.0 == to.0 && to.1 > from.1 + 1 && to.1 - from.1 <= 12 {
                    (from.1 + 1..to.1).all(|y| solid(Cell(from.0, y)))
                } else {
                    false
                };
                if across {
                    out.push((from, to));
                    out.push((to, from));
                }
            }
        }
        out
    }

    /// The campaign's stated modelling commitment (spec §4). `Interior`'s
    /// adjacency is a `BTreeSet` of pairs and symmetric by construction, so an
    /// asymmetry here would be an artifact of the embedding — the thing §2.1
    /// exists to prevent.
    ///
    /// Ordinary recursive shadowcasting FAILS this.
    #[test]
    fn sight_is_symmetric() {
        assert_symmetric(&fixture(3, Seed(1)), "rectilinear n=3 seed=1");
        assert_symmetric(&fixture(4, Seed(2)), "rectilinear n=4 seed=2");
        // The grown corpus is where a pinched, non-convex blob can force the
        // one-sided reveal that plain shadowcasting is famous for.
        assert_symmetric(&wild_fixture(3, Seed(1)), "grown n=3 seed=1");
        assert_symmetric(&wild_fixture(4, Seed(2)), "grown n=4 seed=2");
    }

    #[test]
    fn a_wall_blocks_what_lies_behind_it() {
        // The negative control. Without it, a `shadowcast` that returned every
        // cell in radius would pass the symmetry test perfectly.
        for (label, lattice) in [
            ("rectilinear n=2 seed=4", fixture(2, Seed(4))),
            ("grown n=2 seed=4", wild_fixture(2, Seed(4))),
            ("grown n=4 seed=22", wild_fixture(4, Seed(22))),
        ] {
            let pairs = pairs_across_a_wall(&lattice);
            assert!(
                pairs.len() >= 8,
                "{label}: only {} blocked pairs to check, which is too few to \
                 call this a control",
                pairs.len()
            );
            for (from, blocked) in pairs {
                assert!(
                    !shadowcast(&lattice, from, 12).contains(&blocked),
                    "{label}: sight passed through a wall, {from:?} -> {blocked:?}"
                );
            }
        }
    }

    #[test]
    fn you_always_see_your_own_cell() {
        let lattice = fixture(1, Seed(2));
        let here = some_floor_cell(&lattice);
        assert!(shadowcast(&lattice, here, 0).contains(&here));
    }

    #[test]
    fn radius_bounds_the_result() {
        let lattice = fixture(4, Seed(9));
        let here = some_floor_cell(&lattice);
        for cell in shadowcast(&lattice, here, 3) {
            let (dx, dy) = ((cell.0 - here.0).abs(), (cell.1 - here.1).abs());
            assert!(
                dx <= 3 && dy <= 3,
                "{cell:?} is outside radius 3 of {here:?}"
            );
        }
    }

    /// A ceiling is only a ceiling if the radius can actually reach it: a
    /// bound nothing approaches would pass for a `shadowcast` that lit one
    /// cell.
    #[test]
    fn the_radius_is_reached_as_well_as_respected() {
        let lattice = fixture(4, Seed(9));
        let here = some_floor_cell(&lattice);
        let wide = shadowcast(&lattice, here, 12);
        let narrow = shadowcast(&lattice, here, 1);
        assert!(
            wide.len() > narrow.len(),
            "radius 12 lit no more than radius 1 ({} vs {})",
            wide.len(),
            narrow.len()
        );
        assert!(
            wide.iter()
                .any(|c| (c.0 - here.0).abs() > 3 || (c.1 - here.1).abs() > 3),
            "nothing past Chebyshev 3 is ever lit, so `radius_bounds_the_result` is vacuous"
        );
    }

    #[test]
    fn sight_is_deterministic() {
        let lattice = fixture(2, Seed(6));
        let here = some_floor_cell(&lattice);
        assert_eq!(shadowcast(&lattice, here, 8), shadowcast(&lattice, here, 8));
    }
}
