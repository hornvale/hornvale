use hornvale_game_core::{Grid, Ink, Snapshot, Spatial, Weight, chart};

const FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");

fn walk_chart() -> hornvale_game_core::Chart {
    match Snapshot::parse(FIXTURE).unwrap().spatial {
        Spatial::Walk { chart } => chart,
        Spatial::Chamber { .. } => panic!("fixture must be a walk-band turn"),
    }
}

/// `here` is the boldest thing on the page, and there is exactly one of it.
#[test]
fn exactly_one_cell_is_here_and_it_is_bold() {
    let c = walk_chart();
    let mut g = Grid::new(40, 12);
    chart::draw(&c, &mut g, (0, 0));

    let bold: Vec<_> = (0..g.height())
        .flat_map(|y| (0..g.width()).map(move |x| (x, y)))
        .filter(|&(x, y)| {
            g.get(x, y)
                .is_some_and(|c| c.weight == Weight::Bold && !c.is_blank())
        })
        .collect();
    assert_eq!(bold.len(), 1, "exactly one `here` cell, and it is bold");
}

/// Never-known is unmarked paper. The chart does not emit such cells at
/// all, so the grid around the drawn neighbourhood must stay blank.
#[test]
fn the_unknown_is_paper_not_void() {
    let c = walk_chart();
    let mut g = Grid::new(40, 12);
    chart::draw(&c, &mut g, (0, 0));
    assert!(
        g.get(39, 11).unwrap().is_blank(),
        "unreached paper must stay unmarked"
    );
}

/// Weight is legible with colour switched off entirely — acceptance test 8.
#[test]
fn the_weight_map_discriminates_the_epistemic_states() {
    let c = walk_chart();
    let mut g = Grid::new(40, 12);
    chart::draw(&c, &mut g, (0, 0));
    let map = g.to_weight_map();
    assert!(map.contains('B'), "a `here` cell must be bold");
    assert!(map.contains('N'), "sensed cells must be normal weight");
    assert!(map.contains('.'), "unreached cells must be unmarked");
}

/// The fixture's cells carry scene colours, and at least one drawn cell
/// must show a non-Plain ink — the chart pane tints from the wire.
#[test]
fn the_fixture_cells_carry_colour_off_the_wire() {
    let c = walk_chart();
    assert!(
        c.cells.iter().any(|cell| cell.color.is_some()),
        "the fixture's chart cells must carry colour claims"
    );
    let mut g = Grid::new(40, 12);
    chart::draw(&c, &mut g, (0, 0));
    let tinted = (0..g.height())
        .flat_map(|y| (0..g.width()).map(move |x| (x, y)))
        .filter(|&(x, y)| g.get(x, y).is_some_and(|c| !matches!(c.ink, Ink::Plain)))
        .count();
    assert!(tinted > 0, "at least one drawn cell must be tinted");
}

/// The reference picture for this fixture, straight from Hornvale's OWN
/// canonical renderer for `scene/surrounds/v2`
/// (`windows/scene/src/surrounds_ascii.rs::render_surrounds_ascii`, the
/// "terrain"/"colour" lens — both draw identical glyphs for this fixture,
/// since every one of its 31 cells shares the same `water: "river"` and
/// the only mark sits on the `here` cell, so nothing is tinted) — NOT this
/// crate's own logic. `hornvale-game-core` may not depend on
/// `hornvale-scene` (the containment rule: no hornvale crate in this
/// client's graph), so this is a golden, not a shared function call.
///
/// A **generated** fixture (spec §5.3), not a hand-pasted copy: this used
/// to be a raw-string literal here, maintained by whoever remembered the
/// rule that re-capturing it must always start from the sim's own render,
/// never from this crate's output (which would make the comparison below
/// vacuous — see the module doc's account of a plausible-looking wrong
/// projection formula that once passed every other test in this file).
/// `scripts/regenerate-artifacts.sh`'s `gen_chart_reference` now IS that
/// rule: it types `map` at the flagship possession's opening room and
/// keeps the five grid lines between the `sight:` caption and the
/// `ways on:` footer, so a stale reference reddens the artifact drift
/// check (`docs/generated-paths.txt`) instead of waiting for a human to
/// notice. `make rebaseline` regenerates it.
///
/// If this ever needs to change, the first question is whether
/// `chart::project`'s formula still matches `surrounds_ascii.rs`'s own
/// `row`/`col` comment — see `src/chart.rs`'s module doc before touching
/// either side.
const REFERENCE_SHAPE: &str = include_str!("fixtures/chart-reference-seed-42.txt");

/// Reduce a small ASCII picture to its SHAPE only: which positions are
/// filled, not what glyph fills them. This is what makes comparing this
/// crate's render against the sim's own legitimate despite the two using
/// different glyph vocabularies in general (the sim's per-biome/water
/// alphabet vs. this crate's deliberately coarse `@`/`+`) — a real
/// disagreement in cell PLACEMENT still shows up as a shape mismatch; a
/// difference in which character was chosen for an otherwise-correctly-
/// placed cell would not, and this test is not trying to catch that.
fn shape_of(text: &str) -> Vec<String> {
    text.lines()
        .map(|l| {
            l.chars()
                .map(|c| if c == ' ' { ' ' } else { '#' })
                .collect::<String>()
        })
        .collect()
}

/// Extract the same kind of shape from a rendered [`Grid`]: the tight
/// bounding box of every non-blank cell, read row by row, trimmed on the
/// right the way the sim's own renderer trims (leading space is kept —
/// it is part of the shape's horizontal alignment across rows — trailing
/// space is not, since nothing is there to align against).
fn grid_shape(g: &Grid) -> Vec<String> {
    let mut bounds: Option<(u16, u16, u16, u16)> = None;
    for y in 0..g.height() {
        for x in 0..g.width() {
            if g.get(x, y).is_some_and(|c| !c.is_blank()) {
                bounds = Some(match bounds {
                    None => (x, x, y, y),
                    Some((x0, x1, y0, y1)) => (x0.min(x), x1.max(x), y0.min(y), y1.max(y)),
                });
            }
        }
    }
    let Some((x0, x1, y0, y1)) = bounds else {
        return Vec::new();
    };
    (y0..=y1)
        .map(|y| {
            let line: String = (x0..=x1)
                .map(|x| {
                    if g.get(x, y).is_some_and(|c| !c.is_blank()) {
                        '#'
                    } else {
                        ' '
                    }
                })
                .collect();
            line.trim_end().to_string()
        })
        .collect()
}

/// The client's chart must place cells exactly where Hornvale's own
/// canonical ASCII renderer places them (`REFERENCE_SHAPE` above), even
/// though this campaign's glyph vocabulary is deliberately coarser. A
/// projection that is internally consistent but geometrically wrong
/// (dropping the wrong lattice axis, say — this module's `chart.rs` doc
/// comment records exactly that mistake and how it looked) would still
/// pass every other test in this file; only a shape comparison against the
/// sim's own real output catches it.
#[test]
fn the_shape_matches_the_sims_own_ascii_render() {
    let c = walk_chart();
    let mut g = Grid::new(40, 12);
    chart::draw(&c, &mut g, (0, 0));
    assert_eq!(grid_shape(&g), shape_of(REFERENCE_SHAPE));
}
