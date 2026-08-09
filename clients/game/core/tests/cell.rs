use hornvale_game_core::{Cell, Grid, Weight};

/// An unwritten cell is UNMARKED PAPER, not a space with a weight. The
/// brief calls this the single most important visual decision in it.
#[test]
fn a_fresh_grid_is_unmarked_paper() {
    let g = Grid::new(4, 2);
    assert_eq!(g.to_plain_text(), "    \n    ");
    assert!(
        g.get(0, 0).unwrap().is_blank(),
        "unwritten must be blank, not drawn"
    );
}

#[test]
fn writing_a_cell_places_it_exactly() {
    let mut g = Grid::new(3, 2);
    g.set(1, 0, Cell::glyph('@', Weight::Bold));
    assert_eq!(g.to_plain_text(), " @ \n   ");
}

/// The weight map is how the monochrome acceptance test reads: it must be
/// legible with no colour at all.
#[test]
fn the_weight_map_renders_three_levels_and_absence() {
    let mut g = Grid::new(4, 1);
    g.set(0, 0, Cell::glyph('a', Weight::Bold));
    g.set(1, 0, Cell::glyph('b', Weight::Normal));
    g.set(2, 0, Cell::glyph('c', Weight::Dim));
    assert_eq!(g.to_weight_map(), "BNd.", "bold/normal/dim/unwritten");
}

#[test]
fn out_of_bounds_writes_are_refused_not_wrapped() {
    let mut g = Grid::new(2, 2);
    g.set(9, 9, Cell::glyph('x', Weight::Normal));
    assert_eq!(
        g.to_plain_text(),
        "  \n  ",
        "a wrapped write is a layout bug"
    );
}
