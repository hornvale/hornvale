//! The walk-band chart (`scene/surrounds/v2`) drawn into the cell grid.
//!
//! ## The north-up projection
//!
//! Hornvale already has a canonical ASCII renderer for this exact schema:
//! `windows/scene/src/surrounds_ascii.rs::render_surrounds_ascii`. This
//! module cannot *depend* on it — `hornvale-game-core` carries no hornvale
//! crate in its graph, by design (see this crate's `Cargo.toml`) — so
//! [`project`] below is a second implementation of the same geometry, and
//! `tests/chart.rs` compares this crate's output against a fixture the sim
//! itself generates. That comparison is the only thing that has ever caught
//! a wrong formula here; see the note at the end of this doc.
//!
//! A cell arrives as a polar coordinate about the observer: `bearing_deg`,
//! the great-circle azimuth clockwise from north, and `distance_rad`, the
//! great-circle angle. So
//!
//! ```text
//! r   = distance_rad / farthest * rings
//! row = round(-cos(bearing) * r)
//! col = round( sin(bearing) * r * 2)
//! ```
//!
//! with `farthest` the largest `distance_rad` in the band and `rings` its
//! BFS `radius`. Three things that formula is saying, each of which is a
//! decision rather than an accident:
//!
//! - **North is up and east is right.** Negative row is up the page, so
//!   north takes the minus sign; east is positive column.
//! - **The scale comes off the band, not off the sphere.** A client cannot
//!   compute a room's angular size — that is exactly the spherical
//!   trigonometry the wire's bearing/distance pair exists to avoid — so the
//!   band's own outermost cell sets the scale, landing `rings` row-units
//!   out. A band with every cell at distance zero collapses to the origin
//!   rather than dividing by it.
//! - **The column doubles** because a monospace character cell is about
//!   twice as tall as it is wide. Drop the factor and the chart is an
//!   ellipse claiming to be a circle.
//!
//! The observer's own cell is at distance zero, so it lands on `(0, 0)` by
//! construction and [`draw`] anchors it to the grid centre directly. The
//! `here_offset` helper this replaced existed only because the old
//! lattice projection put the observer at `col = 0` or `col = 1` depending
//! on which way its own triangle pointed.
//!
//! ## What this replaced, and why the agreement test is the guard
//!
//! Until The Illumination this module projected the lattice offsets
//! (`row = -w`, `col = 2v + up-parity + w`), which meant it could not place
//! a **seam** cell at all — where the lattice bends across a base face those
//! offsets are `null`, and [`draw`] skipped such cells outright. Bearing and
//! distance exist for a seam cell, so the whole band draws now.
//!
//! Two earlier formulas were wrong here in ways every test in this file
//! tolerated. `col = u - w, row = v - w` (a plausible-looking axis drop)
//! produced a shape that was internally consistent, collision-free, and
//! geometrically WRONG — sparse and nine rows tall, sheared through the
//! middle. Only comparing against the sim's real output caught it
//! (`the_shape_matches_the_sims_own_ascii_render`, `tests/chart.rs`). That
//! reference is now a **generated** fixture rather than a pasted literal, so
//! it can only ever be re-captured from the sim.
//!
//! This is a projection onto a character grid, so it is not injective the
//! way the lattice one was: two cells can round into one box. [`box_rank`]
//! decides who keeps it, and states the rule the sim and the vessel pane
//! implement identically.

use crate::{Cell, Chart, ChartCell, Mark, Source, Weight};
use std::collections::BTreeMap;

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

/// A cell's box on the character grid, from its polar coordinate about the
/// observer. See the module doc for the geometry and for why the scale is
/// read off the band.
///
/// Rounding is **half away from zero** — Rust's `f64::round`, which is what
/// `windows/scene`'s reference projection uses and what the TypeScript
/// replica goes out of its way to reproduce (`Math.round` rounds half toward
/// `+∞` instead, and would disagree on every negative half-integer).
// `f64::sin`/`f64::cos` are workspace-disallowed (`clippy.toml`: "platform
// libm diverges; use hornvale_kernel::math"), and this crate inherits that
// lint by directory ancestry even though it is outside the cargo workspace.
// The remedy the lint names is unavailable here BY DESIGN: this crate carries
// no hornvale crate in its dependency graph (the containment rule, The Quire
// spec §6), so `hornvale_kernel::math` cannot be reached.
//
// That is sound rather than merely unavoidable. Decision 0055 puts the
// determinism guarantee at the repo boundary — what a client does with the
// sim's output is explicitly unconstrained — and this projection produces a
// picture, never a committed artifact. The one place it could bite is the
// agreement test: a last-ULP difference between platform libm and the sim's
// portable one could flip a `round()` sitting exactly on a half-integer, and
// `the_shape_matches_the_sims_own_ascii_render` would go red on one platform
// and not another. That test is the empirical check on this, and it is what
// would tell us; the third renderer (`clients/vessel/src/pane_chart.ts`) has
// the same exposure through JavaScript's `Math.cos` and no remedy at all.
#[allow(clippy::disallowed_methods)]
fn project(bearing_deg: f64, distance_rad: f64, farthest: f64, rings: i64) -> (i64, i64) {
    if !farthest.is_finite() || farthest <= 0.0 {
        return (0, 0);
    }
    let theta = bearing_deg.to_radians();
    let r = distance_rad / farthest * rings as f64;
    let row = (-theta.cos() * r).round() as i64;
    let col = (theta.sin() * r * 2.0).round() as i64;
    (row, col)
}

/// The mark that should represent a cell's box when it carries more than
/// one. `salience` is a RANK, not a magnitude (`schema::Mark::salience`:
/// "lower is more salient"), so the winner is the mark with the
/// numerically smallest `salience` — never the one with the largest.
fn dominant_mark(marks: &[Mark]) -> Option<&Mark> {
    marks.iter().min_by_key(|m| m.salience)
}

/// The ordering key [`box_rank`] returns; see its doc for the clauses, in
/// the tuple's own order.
type BoxRank = (bool, bool, u32, usize);

/// The rank that decides which cell keeps a box when two of them land in the
/// same one. Smallest wins.
///
/// **The rule, stated once: salience ranks, weight inks, and neither becomes
/// the other.** It is written out here, in `windows/scene/src/
/// surrounds_ascii.rs::box_rank`, and in `clients/vessel/src/pane_chart.ts`
/// in identical terms — the three renderers may differ in *vocabulary*
/// (this one draws `@` and `+` for everything) but never in this rule:
///
/// 1. **The observer never loses their own box.** The chart is egocentric;
///    a band that drew over `@` would have lost the one cell the reader is
///    standing in.
/// 2. **A marked cell beats an unmarked one, and among marked cells the
///    numerically smallest `salience` wins.**
/// 3. **Ties break on document order**, which the producer fixes as
///    ascending `room` — available even here, where `room` is not mirrored.
///
/// What is deliberately absent is the epistemic state. A `remembered` cell
/// holding the flagship settlement **wins its box and draws dim**: it is the
/// most salient thing standing there, and the chart still says you are
/// remembering it. Weight never promotes a cell and salience never
/// brightens one — [`weight_of`] still reads `state` and nothing else.
fn box_rank(cell: &ChartCell, index: usize) -> BoxRank {
    let dominant = dominant_mark(&cell.marks).map(|m| m.salience);
    (
        cell.state != "here",
        dominant.is_none(),
        dominant.unwrap_or(0),
        index,
    )
}

/// Draw `chart` into `into`, anchored so the observer lands at `origin`
/// plus half of `into`'s own width and height — "relative to origin plus a
/// centre offset" per the brief. The observer is at distance zero from
/// itself, so [`project`] puts it at `(0, 0)` and no per-chart offset is
/// needed.
///
/// **Every cell is drawn, seam cells included.** They used to be skipped
/// because their lattice offsets were `null`; they carry a bearing and a
/// distance like any other cell, so there is nothing left to skip them for.
///
/// Where two cells round into one box, [`box_rank`] decides who keeps it —
/// one pass over a rank-ordered map rather than the two overdraw passes
/// this replaced, so the winner is a stated rule and not a consequence of
/// which cell the loop happened to write last. Cells that land outside
/// `into`'s bounds are silently skipped, matching `Grid::set`'s own
/// discipline of refusing rather than wrapping an out-of-range write.
pub fn draw(chart: &Chart, into: &mut crate::Grid, origin: (u16, u16)) {
    let centre_x = origin.0 as i64 + into.width() as i64 / 2;
    let centre_y = origin.1 as i64 + into.height() as i64 / 2;
    let farthest = chart
        .cells
        .iter()
        .map(|c| c.distance_rad)
        .fold(0.0f64, f64::max);
    let rings = i64::from(chart.radius.max(1));

    let mut boxes: BTreeMap<(i64, i64), (BoxRank, &ChartCell)> = BTreeMap::new();
    for (i, cell) in chart.cells.iter().enumerate() {
        let at = project(cell.bearing_deg, cell.distance_rad, farthest, rings);
        let rank = box_rank(cell, i);
        match boxes.get(&at) {
            Some((held, _)) if *held <= rank => {}
            _ => {
                boxes.insert(at, (rank, cell));
            }
        }
    }

    for ((row, col), (_, cell)) in boxes {
        let x = centre_x + col;
        let y = centre_y + row;
        if x < 0 || y < 0 || x >= into.width() as i64 || y >= into.height() as i64 {
            continue;
        }
        into.set(
            x as u16,
            y as u16,
            Cell::glyph(glyph_of(&cell.state), weight_of(&cell.state), Source::Chart),
        );
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

    /// North is up and east is right, at the scale the band sets. Pinned as
    /// exact boxes rather than as inequalities: with `farthest` and
    /// `distance` equal and `rings = 4`, the cell is on the rim, so it lands
    /// four rows north or eight columns east — the 2x column factor, which
    /// nothing else in this file would catch.
    #[test]
    fn project_puts_north_up_east_right_and_doubles_the_column() {
        assert_eq!(project(0.0, 1.0, 1.0, 4), (-4, 0), "north is up");
        assert_eq!(project(90.0, 1.0, 1.0, 4), (0, 8), "east is right, doubled");
        assert_eq!(project(180.0, 1.0, 1.0, 4), (4, 0), "south is down");
        assert_eq!(project(270.0, 1.0, 1.0, 4), (0, -8), "west is left");
    }

    /// The observer is at distance zero from itself, so it is at the origin
    /// whatever its bearing field says — which is what lets [`draw`] anchor
    /// the chart to the grid centre with no per-chart offset.
    #[test]
    fn the_observer_projects_to_the_origin() {
        assert_eq!(project(0.0, 0.0, 1.0, 4), (0, 0));
        assert_eq!(project(137.0, 0.0, 1.0, 4), (0, 0));
    }

    /// A band with no extent at all (radius 0, the observer alone) has
    /// nothing to scale against. Collapse to the origin rather than divide
    /// by zero and place a NaN.
    #[test]
    fn a_band_with_no_distance_collapses_to_the_origin() {
        assert_eq!(project(90.0, 0.0, 0.0, 1), (0, 0));
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

    /// A cell addressed the way the renderer reads one: by its polar
    /// coordinate about the observer. `distance` is in units of the band's
    /// own rim, so `1.0` is "as far out as this band goes" and `0.0` is the
    /// observer. The lattice offsets are filled in because a non-seam cell
    /// carries them on the wire, but nothing reads them.
    fn chart_cell(bearing_deg: f64, distance_rad: f64, state: &str) -> ChartCell {
        ChartCell {
            u: Some(0),
            v: Some(0),
            w: Some(0),
            up: Some(true),
            seam: false,
            state: state.to_string(),
            biome: 0,
            water: 0,
            relief: 0,
            marks: vec![],
            bearing_deg,
            distance_rad,
        }
    }

    /// A seam cell — every lattice offset `null` — is DRAWN now. It used to
    /// be skipped outright, which is why half a seam-crossing band was
    /// simply missing from this client's chart.
    #[test]
    fn a_seam_cell_is_drawn_under_north_up() {
        let mut seam = chart_cell(90.0, 1.0, "sensed");
        seam.seam = true;
        seam.u = None;
        seam.v = None;
        seam.w = None;
        seam.up = None;
        let chart = minimal_chart(vec![chart_cell(0.0, 0.0, "here"), seam]);
        let mut g = crate::Grid::new(9, 5);
        draw(&chart, &mut g, (0, 0));
        // Centre is (4, 2); the seam cell is due east on the rim, so two
        // columns right of it.
        assert!(!g.get(4, 2).unwrap().is_blank(), "the observer is drawn");
        assert!(
            !g.get(6, 2).unwrap().is_blank(),
            "the seam cell must be drawn at its bearing, not dropped"
        );
    }

    /// The collision rule ([`box_rank`]) on a FORCED collision — two cells
    /// at the identical bearing and distance. Measured across seventy real
    /// bands the shipped projection collided zero times, so nothing but a
    /// forced fixture exercises this at all.
    #[test]
    fn the_more_salient_of_two_colliding_cells_keeps_the_box() {
        let salient = |salience: u32, state: &str| {
            let mut c = chart_cell(90.0, 1.0, state);
            c.marks = vec![mark(salience)];
            c
        };
        for cells in [
            vec![
                chart_cell(0.0, 0.0, "here"),
                salient(20, "sensed"),
                salient(5, "remembered"),
            ],
            vec![
                chart_cell(0.0, 0.0, "here"),
                salient(5, "remembered"),
                salient(20, "sensed"),
            ],
        ] {
            let mut g = crate::Grid::new(9, 5);
            draw(&minimal_chart(cells), &mut g, (0, 0));
            // **Salience ranks; weight inks.** The remembered cell is the
            // more salient of the two, so it keeps the box (clause 2) AND
            // draws Dim (its own epistemic state) — neither channel becomes
            // the other. Asserting both document orders is what separates
            // "the rule ran" from "the later write won".
            assert_eq!(
                g.get(6, 2).unwrap().weight,
                Weight::Dim,
                "the remembered flagship must win its box and still draw dim"
            );
        }
    }

    /// A marked cell keeps its box against an unmarked one in either
    /// document order — clause 2 of [`box_rank`], and the half that would
    /// silently regress if the map kept whichever cell it saw last.
    #[test]
    fn a_marked_cell_outranks_an_unmarked_one_in_either_document_order() {
        let mut marked = chart_cell(90.0, 1.0, "remembered");
        marked.marks = vec![mark(9)];
        for cells in [
            vec![
                chart_cell(0.0, 0.0, "here"),
                marked.clone(),
                chart_cell(90.0, 1.0, "sensed"),
            ],
            vec![
                chart_cell(0.0, 0.0, "here"),
                chart_cell(90.0, 1.0, "sensed"),
                marked.clone(),
            ],
        ] {
            let mut g = crate::Grid::new(9, 5);
            draw(&minimal_chart(cells), &mut g, (0, 0));
            assert_eq!(
                g.get(6, 2).unwrap().weight,
                Weight::Dim,
                "the marked cell must keep the box, so the box reads its \
                 remembered weight rather than the unmarked cell's Normal"
            );
        }
    }

    /// Clause 1 of [`box_rank`]: the chart is egocentric, so the observer
    /// keeps their own box even against the most salient mark in the band.
    #[test]
    fn the_observer_never_loses_its_own_box() {
        let mut rival = chart_cell(0.0, 0.0, "remembered");
        rival.marks = vec![mark(0)];
        let chart = minimal_chart(vec![chart_cell(0.0, 0.0, "here"), rival]);
        let mut g = crate::Grid::new(9, 5);
        draw(&chart, &mut g, (0, 0));
        assert_eq!(g.get(4, 2).unwrap().weight, Weight::Bold);
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
