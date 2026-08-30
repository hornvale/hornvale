use hornvale_game_core::{Chart, ChartCell, Grid, Ink, Micro, Snapshot, Spatial, Weight, chart};

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
/// Hermetic against a developer's exported `NO_COLOR`: saved, removed,
/// restored around the draw and its assertion. The core crate's locked
/// `test_env` helper is `pub(crate)` and unit-test-only, so it is not
/// reachable from this integration binary; the save/restore here is safe
/// because no other test in this binary touches the environment.
#[test]
fn the_fixture_cells_carry_colour_off_the_wire() {
    let saved = std::env::var_os("NO_COLOR");
    // SAFETY: env mutation is UB under concurrency; no sibling test in this
    // integration binary reads or writes the environment, so none can
    // observe the mutation or the restore.
    unsafe { std::env::remove_var("NO_COLOR") };
    let c = walk_chart();
    assert!(
        c.cells.iter().any(|cell| cell.color.is_some()),
        "the fixture's chart cells must carry colour claims"
    );
    let mut g = Grid::new(40, 12);
    chart::draw(&c, &mut g, (0, 0));
    match saved {
        // SAFETY: as above — no concurrent env access in this binary.
        Some(v) => unsafe { std::env::set_var("NO_COLOR", v) },
        // SAFETY: as above.
        None => unsafe { std::env::remove_var("NO_COLOR") },
    }
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
/// alphabet, which also distinguishes water and marks, vs. this crate's
/// impedance ladder alone plus `@`) — a real
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

/// `chart.rs:79`'s old `PLACED_GLYPH` drew `+` for "everything else in
/// view, terrain and marks alike" — one glyph, no matter what. This
/// fixture's own 30 non-`here` cells span relief bands 1 and 2 with real
/// `micro.openness`/`micro.relief` spread (see `task-8-report.md`), so an
/// ordinal ladder over the RENDERED output must show more than one glyph.
#[test]
fn the_walk_band_draws_an_ordinal_ladder_not_one_glyph() {
    let c = walk_chart();
    let mut g = Grid::new(80, 24);
    chart::draw(&c, &mut g, (0, 0));
    let terrain_glyphs: std::collections::BTreeSet<char> = g
        .to_plain_text()
        .chars()
        .filter(|&ch| ch != ' ' && ch != '\n' && ch != '@')
        .collect();
    assert!(
        terrain_glyphs.len() > 1,
        "still one glyph: {terrain_glyphs:?}"
    );
}

/// A minimal chart cell addressed by its polar coordinate, matching how
/// [`chart::draw`] reads one — see `src/chart.rs`'s own `chart_cell` test
/// helper, which this mirrors for this external integration binary (no
/// access to that private helper from here).
fn synth_cell(
    bearing_deg: f64,
    distance_rad: f64,
    relief: u32,
    openness: f64,
    roughness: f64,
) -> ChartCell {
    ChartCell {
        u: Some(0),
        v: Some(0),
        w: Some(0),
        up: Some(true),
        seam: false,
        state: "sensed".to_string(),
        biome: 0,
        water: 0,
        relief,
        color: None,
        micro: Micro {
            relief: roughness,
            aspect: 0.0,
            wetness: 0.0,
            openness,
        },
        marks: vec![],
        bearing_deg,
        distance_rad,
    }
}

fn synth_chart(radius: u32, cells: Vec<ChartCell>) -> Chart {
    Chart {
        radius,
        depth: 12,
        biome_legend: vec![],
        water_legend: vec![],
        relief_legend: vec![],
        cells,
        legend: vec![],
        sight: None,
    }
}

/// Ordinality on the RENDERED output, not on a private helper: a ladder
/// that is ordinal in `impedance_glyph` and shuffled at `glyph_of`'s call
/// site would still read as nominal from outside this crate. Six synthetic
/// cells due east at increasing distance land at six distinct, predictable
/// columns (the documented projection: due east, `row = 0`, `col` scales
/// with distance — see `src/chart.rs`'s module doc), each carrying relief
/// `0..=5` with canopy fully open and roughness flat, so each cell's own
/// impedance is exactly its relief index. The five-glyph ladder
/// (`_ . : ^ A`) must therefore read non-decreasing rank left to right,
/// including the doubly-overloaded top rung (relief 5 and 6 both draw `A`).
#[test]
fn the_ladder_ascends_with_impedance() {
    let cells: Vec<ChartCell> = (0..=5)
        .map(|relief| synth_cell(90.0, f64::from(relief) + 1.0, relief, 1.0, 0.0))
        .collect();
    let farthest = 6.0; // the relief-5 cell's own distance_rad
    let radius = 6;
    let chart = synth_chart(radius, cells);
    let mut g = Grid::new(40, 4);
    chart::draw(&chart, &mut g, (0, 0));

    let centre_x = 20i64;
    let centre_y = 2i64;
    let rank = |glyph: char| -> usize {
        ['_', '.', ':', '^', 'A']
            .iter()
            .position(|&g| g == glyph)
            .unwrap_or_else(|| panic!("{glyph} is not a ladder rung"))
    };
    let mut ranks = Vec::new();
    for relief in 0..=5i64 {
        let distance = f64::from(relief as u32) + 1.0;
        let r = distance / farthest * radius as f64;
        let col = (r * 2.0).round() as i64; // sin(90deg) == 1
        let cell = g
            .get((centre_x + col) as u16, centre_y as u16)
            .expect("in bounds");
        let glyph = cell.glyph.expect("a drawn cell");
        ranks.push(rank(glyph));
    }
    assert!(
        ranks.windows(2).all(|w| w[0] <= w[1]),
        "impedance ladder must be non-decreasing: {ranks:?}"
    );
}
