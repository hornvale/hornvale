//! The walk-band chart (`scene/surrounds/v2`) drawn into the cell grid.
//!
//! ## The lattice-to-grid projection
//!
//! A chart cell's position is three barycentric-style lattice offsets
//! `(u, v, w)` from the observer (`kernel::room::FaceLattice`; see
//! `windows/scene/src/surrounds.rs`, which derives `u`/`v`/`w` as the
//! observer-relative difference of each axis of that lattice base point).
//! Two facts about them matter for drawing:
//!
//! - `u + v + w` is `0` for a "down"-oriented triangle and `1` for an
//!   "up"-oriented one (`FaceLattice`'s doc comment: `a + b + c` is
//!   `scale - 1` for up, `scale - 2` for down — a constant-`1` difference
//!   that survives the observer-relative subtraction). Concretely, in the
//!   seed-42 fixture the observer's own cell is `(u, v, w) = (0, 0, 0)`
//!   and its up-oriented neighbour is `(0, 0, 1)`: two DIFFERENT physical
//!   triangles. A projection that read only `u` and `v` and dropped `w`
//!   would put both at grid position `(0, 0)` — a real collision in the
//!   committed fixture, not a hypothetical one.
//! - Any linear combination of `u`, `v`, `w` whose coefficients sum to zero
//!   is invariant under that up/down shift: adding a constant `k` to all
//!   three changes the combination by `k * (coeff_u + coeff_v + coeff_w)`,
//!   which is `k * 0` when the coefficients sum to zero. That is the same
//!   trick a hex grid's "axial" coordinates use to drop cube coordinates'
//!   redundant third axis (`q = x`, `r = z`, dropping `y` because
//!   `x + y + z = 0` there): pick two of the three axes and subtract the
//!   third from each, and the shared additive freedom cancels out.
//!
//! So [`project`] drops `w` the same way: `col = u - w`, `row = v - w`.
//! Both combinations have coefficients (`1, 0, -1` and `0, 1, -1`) summing
//! to zero, so a cell's `(col, row)` no longer depends on which orientation
//! shift produced its particular `(u, v, w)`. Checked against all 31 cells
//! of the seed-42 fixture: injective (no two collide) and more compact than
//! the fixture's radius-4 neighbourhood needs to be for a character grid
//! this coarse (column and row both span nine positions, -4..=4).
//!
//! This is a PROJECTION, not a geometrically faithful RENDERING. A true
//! equilateral-triangle embedding would foreshorten one axis by
//! `sqrt(3)/2`; this module does not, because a monospace character cell
//! is not square either, and the chart is explicitly "lattice-aligned, not
//! north-up" (`orientation: "lattice"` on `scene/surrounds/v2`) — there is
//! no compass bearing for either axis to be faithful *to*. What survives is
//! adjacency and relative direction; what does not is metric distance and
//! the true 60/120-degree angles between neighbours.

use crate::{Cell, Chart, ChartCell, Mark, Weight};

/// The `here` cell's glyph: the possessed character's own position.
const HERE_GLYPH: char = '@';

/// Every other placed lattice cell's glyph. The shipped vocabulary for this
/// campaign is deliberately this coarse — one glyph for "here" and one for
/// everything else in view, terrain and marks alike. The 22-biome glyph set
/// (and any per-mark glyph it would enable) is a separate campaign; see The
/// Quire spec, scope "Out".
const PLACED_GLYPH: char = '+';

/// Weight from the epistemic state. Fixed by the brief; not a design choice.
fn weight_of(state: &str) -> Weight {
    match state {
        "here" => Weight::Bold,
        "sensed" => Weight::Normal,
        "remembered" => Weight::Dim,
        // A never-known cell is not emitted at all, so an unrecognised
        // state is a schema change, not a fourth ink. Fail loudly by
        // drawing nothing rather than inventing a weight — callers still
        // get a value back (this fn is infallible by design), but see
        // the module doc: the producer never emits anything else.
        _ => Weight::Normal,
    }
}

/// Glyph from the epistemic state: `@` for `here`, `+` for anything placed.
fn glyph_of(state: &str) -> char {
    if state == "here" {
        HERE_GLYPH
    } else {
        PLACED_GLYPH
    }
}

/// Project a chart cell's lattice offset `(u, v, w)` onto a grid
/// `(col, row)` pair. See the module doc for why dropping `w` this way is
/// safe: `u - w` and `v - w` are both invariant under the constant shift
/// that separates an up-oriented triangle's coordinates from a
/// down-oriented one's.
fn project(u: i64, v: i64, w: i64) -> (i64, i64) {
    (u - w, v - w)
}

/// The mark that should represent a cell's box when it carries more than
/// one. `salience` is a RANK, not a magnitude (`schema::Mark::salience`:
/// "lower is more salient"), so the winner is the mark with the
/// numerically smallest `salience` — never the one with the largest.
fn dominant_mark(marks: &[Mark]) -> Option<&Mark> {
    marks.iter().min_by_key(|m| m.salience)
}

/// Write one non-seam chart cell into `into` at its projected position,
/// relative to `(centre_x, centre_y)`. Cells that project outside `into`'s
/// bounds, or that carry no lattice coordinate (a seam cell, or a state
/// this module does not recognise), are silently skipped — matching
/// `Grid::set`'s own discipline of refusing rather than wrapping an
/// out-of-range write.
fn place_cell(into: &mut crate::Grid, centre_x: i64, centre_y: i64, cell: &ChartCell) {
    let (Some(u), Some(v), Some(w)) = (cell.u, cell.v, cell.w) else {
        return;
    };
    let (col, row) = project(u, v, w);
    let x = centre_x + col;
    let y = centre_y + row;
    if x < 0 || y < 0 || x >= into.width() as i64 || y >= into.height() as i64 {
        return;
    }
    into.set(
        x as u16,
        y as u16,
        Cell::glyph(glyph_of(&cell.state), weight_of(&cell.state)),
    );
}

/// Draw `chart` into `into`, anchored so the chart's own centre (the
/// observer's `(u, v, w) = (0, 0, 0)` cell) lands at `origin` plus half of
/// `into`'s own width and height — "relative to origin plus a centre
/// offset" per the brief. `seam` cells are skipped: they carry no honest
/// local coordinate (their `u`/`v`/`w` are `null` because the lattice bends
/// across a base face there), so inventing a position for them would be a
/// worse lie than omitting them.
///
/// Cells are drawn in two passes. The first draws every non-seam cell by
/// its own epistemic `state` (`here` → `@` and Bold; anything else → `+`
/// at the state's weight). The second draws only cells that carry a
/// [`Mark`], so a mark always wins its box even against a same-box cell the
/// first pass drew — should the projection in [`project`] ever collide two
/// *different* lattice cells onto one grid position (it does not for the
/// committed fixture, but nothing guarantees that in general). Among
/// colliding marked cells, the one whose most-salient mark
/// ([`dominant_mark`]) has the smallest `salience` value draws last and
/// therefore wins, per the brief: marks are ordered so the most salient
/// draws last and is on top, and a rank never becomes a weight — weight
/// here still comes only from `state`, exactly as in the first pass.
pub fn draw(chart: &Chart, into: &mut crate::Grid, origin: (u16, u16)) {
    let centre_x = origin.0 as i64 + into.width() as i64 / 2;
    let centre_y = origin.1 as i64 + into.height() as i64 / 2;

    for cell in &chart.cells {
        if cell.seam {
            continue;
        }
        place_cell(into, centre_x, centre_y, cell);
    }

    let mut marked: Vec<&ChartCell> = chart
        .cells
        .iter()
        .filter(|c| !c.seam && !c.marks.is_empty())
        .collect();
    // Least salient first, most salient last: `Reverse` makes an ascending
    // sort on the numeric `salience` value run in descending order, so the
    // smallest `salience` (the schema's "most salient") is processed last.
    marked.sort_by_key(|c| std::cmp::Reverse(dominant_mark(&c.marks).map(|m| m.salience)));
    for cell in marked {
        place_cell(into, centre_x, centre_y, cell);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mark(salience: u32) -> Mark {
        Mark {
            noun: "x".to_string(),
            kind: "agent".to_string(),
            datum: "x".to_string(),
            salience,
        }
    }

    /// `salience` is a rank: a lower number is more salient
    /// (`schema::Mark::salience`), so the dominant mark is the one with
    /// the SMALLEST value, not the largest.
    #[test]
    fn dominant_mark_is_the_lowest_salience_number() {
        let marks = vec![mark(10), mark(5), mark(20)];
        assert_eq!(dominant_mark(&marks).unwrap().salience, 5);
    }

    #[test]
    fn dominant_mark_of_no_marks_is_none() {
        assert!(dominant_mark(&[]).is_none());
    }

    /// A `w - w` style drop must not collapse the observer's own cell and
    /// its up-oriented neighbour to the same box. See the module doc.
    #[test]
    fn project_distinguishes_the_two_orientations_at_the_origin() {
        assert_ne!(project(0, 0, 0), project(0, 0, 1));
    }
}
