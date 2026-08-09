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

/// `get`'s bounds contract is the read half of the same law `set` upholds:
/// an out-of-range coordinate must read as nothing, not as some wrapped-in
/// real cell.
#[test]
fn out_of_bounds_reads_return_none_not_wrapped() {
    let g = Grid::new(2, 2);
    assert!(
        g.get(9, 9).is_none(),
        "a wrapped read is a layout bug, same as a wrapped write"
    );
}

/// `to_plain_text`'s row join is covered by `writing_a_cell_places_it_exactly`
/// (an asymmetric grid where only row 0 differs from row 1). The weight map
/// needs the same: distinct content per row, so a row-reversal or
/// mis-joined output is caught.
#[test]
fn the_weight_map_joins_multiple_rows_in_order() {
    let mut g = Grid::new(2, 2);
    g.set(0, 0, Cell::glyph('a', Weight::Bold));
    g.set(0, 1, Cell::glyph('b', Weight::Dim));
    assert_eq!(g.to_weight_map(), "B.\nd.", "rows must stay in y order");
}

/// A transposed grid (width and height swapped) is exactly the class of bug
/// that renders every later plate plausibly but wrong — assert both readers
/// directly rather than only through string output that happens to have
/// square dimensions in other tests.
#[test]
fn width_and_height_are_read_correctly() {
    let g = Grid::new(5, 3);
    assert_eq!(g.width(), 5, "width must not be height in disguise");
    assert_eq!(g.height(), 3, "height must not be width in disguise");
}
