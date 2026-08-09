//! The walk-band chart (`scene/surrounds/v2`) drawn into the cell grid.
//!
//! ## The lattice-to-grid projection
//!
//! Hornvale already has a canonical ASCII renderer for this exact schema:
//! `windows/scene/src/surrounds_ascii.rs::render_surrounds_ascii`. This
//! module cannot *depend* on it — `hornvale-game-core` carries no hornvale
//! crate in its graph, by design (see this crate's `Cargo.toml`) — so
//! [`project`] below is a second implementation of that module's own
//! `row`/`col` formula, not an invented one. If the two ever need to change,
//! read both comments together; they describe the same geometry twice.
//!
//! A chart cell's position is three barycentric-style lattice offsets
//! `(u, v, w)` from the observer, plus `up` (whether the triangle points
//! the same way as its base face). `surrounds_ascii.rs` places a cell at:
//!
//! ```text
//! row = -w
//! col = 2*v + (up ? 0 : 1) + w
//! ```
//!
//! `u` never enters the formula — worth calling out explicitly, because an
//! earlier version of this module used `col = u - w, row = v - w` instead
//! (a plausible-looking axis drop: see the retired
//! `project_distinguishes_the_two_orientations_at_the_origin` test this
//! replaced) and produced a shape that was internally consistent,
//! collision-free, and geometrically WRONG — sparse and nine rows tall,
//! sheared through the middle, where the sim's own render of the identical
//! seed-42 chart is dense and five rows tall. Every test in this file
//! passed anyway; only comparing against the sim's real output caught it
//! (`the_shape_matches_the_sims_own_ascii_render`, `tests/chart.rs`). The
//! `+ w` term is what a naive `row = -w, col = 2v + up-parity` formula is
//! missing: without it, an up-triangle's horizontal-edge neighbour directly
//! below it lands down-and-to-the-right instead of straight down, drawing a
//! breadth-first neighbourhood as a right-leaning parallelogram instead of
//! the symmetric hexagon it actually is.
//!
//! This is still a PROJECTION, not a geometrically faithful RENDERING — it
//! does not attempt the `sqrt(3)/2` foreshortening a true equilateral
//! embedding would need, because a monospace character cell is not square
//! either, and the chart is explicitly "lattice-aligned, not north-up"
//! (`orientation: "lattice"` on `scene/surrounds/v2`) — there is no compass
//! bearing for either axis to be faithful *to*. What survives is adjacency
//! and relative direction; what does not is metric distance and the true
//! 60/120-degree angles between neighbours.

use crate::{Cell, Chart, ChartCell, Mark, Source, Weight};

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
        // state would be a schema change, not a fourth ink. This function
        // is infallible by design (a render must not panic on a document
        // that parsed), so an unknown state falls back to `Normal` and the
        // cell is still drawn — it does NOT fail, and it does not skip.
        // The safety here is upstream: the producer emits exactly these
        // three, `tests/chart.rs` pins the mapping, and a fourth state
        // would arrive with a schema version bump.
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

/// Project a chart cell's `(v, w, up)` onto a `(row, col)` pair. This is
/// `windows/scene/src/surrounds_ascii.rs`'s own formula, reimplemented (see
/// the module doc for why this can't just call that module, and for what
/// went wrong the one time this crate used a different formula).
fn project(v: i64, w: i64, up: bool) -> (i64, i64) {
    (-w, 2 * v + i64::from(!up) + w)
}

/// Where the `here` cell itself projects to, so [`draw`] can anchor the
/// chart to that position rather than assuming it is `(0, 0)`. It is not:
/// [`project`]'s `up`-parity term means the observer's own cell lands at
/// `col = 0` or `col = 1` depending on which way its own triangle points,
/// a per-room fact this function reads rather than guesses. Falls back to
/// `(0, 0)` if no cell is in state `here` or its lattice coordinate is
/// missing — both schema violations the producer does not allow, but this
/// function stays infallible rather than panicking on a malformed chart.
fn here_offset(chart: &Chart) -> (i64, i64) {
    chart
        .cells
        .iter()
        .find(|c| c.state == "here")
        .and_then(|c| match (c.v, c.w, c.up) {
            (Some(v), Some(w), Some(up)) => Some(project(v, w, up)),
            _ => None,
        })
        .unwrap_or((0, 0))
}

/// The mark that should represent a cell's box when it carries more than
/// one. `salience` is a RANK, not a magnitude (`schema::Mark::salience`:
/// "lower is more salient"), so the winner is the mark with the
/// numerically smallest `salience` — never the one with the largest.
fn dominant_mark(marks: &[Mark]) -> Option<&Mark> {
    marks.iter().min_by_key(|m| m.salience)
}

/// Write one non-seam chart cell into `into` at its projected position:
/// [`project`]'s `(row, col)`, minus `(here_row, here_col)` so the `here`
/// cell itself lands exactly at `(centre_x, centre_y)`, whatever its own
/// `up`-parity happens to be. Cells that land outside `into`'s bounds, or
/// that carry no lattice coordinate (a seam cell, or a state this module
/// does not recognise), are silently skipped — matching `Grid::set`'s own
/// discipline of refusing rather than wrapping an out-of-range write.
fn place_cell(
    into: &mut crate::Grid,
    centre_x: i64,
    centre_y: i64,
    here_row: i64,
    here_col: i64,
    cell: &ChartCell,
) {
    let (Some(v), Some(w), Some(up)) = (cell.v, cell.w, cell.up) else {
        return;
    };
    let (row, col) = project(v, w, up);
    let x = centre_x + (col - here_col);
    let y = centre_y + (row - here_row);
    if x < 0 || y < 0 || x >= into.width() as i64 || y >= into.height() as i64 {
        return;
    }
    into.set(
        x as u16,
        y as u16,
        Cell::glyph(glyph_of(&cell.state), weight_of(&cell.state), Source::Chart),
    );
}

/// Draw `chart` into `into`, anchored so the chart's own centre (the
/// `here` cell) lands at `origin` plus half of `into`'s own width and
/// height — "relative to origin plus a centre offset" per the brief.
/// `seam` cells are skipped: they carry no honest local coordinate (their
/// `u`/`v`/`w`/`up` are `null` because the lattice bends across a base face
/// there), so inventing a position for them would be a worse lie than
/// omitting them.
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
    let (here_row, here_col) = here_offset(chart);

    for cell in &chart.cells {
        if cell.seam {
            continue;
        }
        place_cell(into, centre_x, centre_y, here_row, here_col, cell);
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
        place_cell(into, centre_x, centre_y, here_row, here_col, cell);
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

    /// Pinned against real seed-42 fixture cells, not invented numbers: room
    /// `750518284` (`v=0, w=0, up=false`) is the fixture's `here` cell, and
    /// room `748945420` (`v=0, w=1, up=true`) is its up-oriented
    /// `w`-neighbour. Hand-checked against
    /// `windows/scene/src/surrounds_ascii.rs`'s own formula — see the module
    /// doc for why this crate cannot just call that module instead.
    #[test]
    fn project_matches_the_known_fixture_values() {
        assert_eq!(project(0, 0, false), (0, 1));
        assert_eq!(project(0, 1, true), (-1, 1));
    }

    fn minimal_chart(cells: Vec<ChartCell>) -> Chart {
        Chart {
            radius: 1,
            depth: 12,
            biome_legend: vec![],
            water_legend: vec![],
            relief_legend: vec![],
            cells,
            legend: vec![],
        }
    }

    fn chart_cell(v: i64, w: i64, up: bool, state: &str) -> ChartCell {
        ChartCell {
            u: Some(0),
            v: Some(v),
            w: Some(w),
            up: Some(up),
            seam: false,
            state: state.to_string(),
            biome: 0,
            water: 0,
            relief: 0,
            marks: vec![],
        }
    }

    /// A down-oriented `here` cell (`up: false`) projects to `col = 1`, not
    /// `col = 0` — the `up`-parity term is a per-room fact, not always
    /// zero, so [`draw`] must read the `here` cell's own projection rather
    /// than assume it sits at the projection's origin.
    #[test]
    fn here_offset_reads_the_here_cells_own_up_parity() {
        let chart = minimal_chart(vec![chart_cell(0, 0, false, "here")]);
        assert_eq!(here_offset(&chart), (0, 1));
    }

    #[test]
    fn here_offset_of_no_here_cell_falls_back_to_the_projection_origin() {
        let chart = minimal_chart(vec![chart_cell(0, 0, false, "sensed")]);
        assert_eq!(here_offset(&chart), (0, 0));
    }

    /// The whole state → weight mapping, pinned including the arm no
    /// committed fixture reaches. Neither fixture contains a `remembered`
    /// cell (the seed-42 openings are turn 0, so nothing has fallen out of
    /// presence yet), so `Weight::Dim` has no coverage from a real render at
    /// all and this is the only thing holding it. The unknown arm is pinned
    /// too, because its comment used to claim it drew nothing.
    #[test]
    fn every_epistemic_state_maps_to_its_documented_weight() {
        assert_eq!(weight_of("here"), Weight::Bold);
        assert_eq!(weight_of("sensed"), Weight::Normal);
        assert_eq!(weight_of("remembered"), Weight::Dim);
        assert_eq!(
            weight_of("no-such-state"),
            Weight::Normal,
            "the unknown arm falls back to Normal and the cell is still drawn"
        );
    }
}
