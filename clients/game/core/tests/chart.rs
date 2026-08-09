use hornvale_game_core::{Grid, Snapshot, Spatial, Weight, chart};

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
