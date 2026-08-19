//! Integration tests for The Adit's level generator, against real
//! `Chamber`/`Cave` values built the same way
//! `windows/worldgen/tests/deep_realm_chamber.rs` does — not a hand-built
//! fixture, so a change to how those types are constructed is caught here
//! too.

use hornvale_kernel::Seed;
use hornvale_terrain::{CaveKind, DelveRung};
use hornvale_vessel::{LevelCellKind, generate_descent};
use hornvale_worldgen::chamber::ChamberOrigin;

/// A small ASCII dump for visual sanity-checking during development —
/// not the production `map` verb (a later Delving campaign's job), and
/// not asserted against pixel-for-pixel; only exercised here for its own
/// non-panicking, non-empty output.
fn render_debug(level: &hornvale_vessel::Level) -> String {
    let mut out = String::new();
    for y in level.extent.y..(level.extent.y + level.extent.h) {
        for x in level.extent.x..(level.extent.x + level.extent.w) {
            let cell = hornvale_vessel::Cell(x, y);
            let glyph = match level.cells.get(&cell) {
                Some(LevelCellKind::Floor) => '.',
                Some(LevelCellKind::Wall) | None => '#',
                Some(LevelCellKind::Flooded) => '~',
                Some(LevelCellKind::StairsDown) => '>',
                Some(LevelCellKind::StairsUp) => '<',
            };
            out.push(glyph);
        }
        out.push('\n');
    }
    out
}

#[test]
fn a_real_descent_is_deterministic_connected_and_renders() {
    let rungs = [DelveRung::Undercroft, DelveRung::Shallows, DelveRung::Deeps];
    let origins = [
        ChamberOrigin::Found,
        ChamberOrigin::Made,
        ChamberOrigin::Found,
    ];
    let depths_m = [20.0, 65.0, 140.0];

    let a = generate_descent(&rungs, CaveKind::Karst, &origins, &depths_m, 90.0, Seed(42));
    let b = generate_descent(&rungs, CaveKind::Karst, &origins, &depths_m, 90.0, Seed(42));
    assert_eq!(
        a, b,
        "a real descent must be byte-identical for the same seed"
    );

    assert_eq!(a.len(), 3);
    for (i, level) in a.iter().enumerate() {
        let dump = render_debug(level);
        assert!(!dump.is_empty(), "level {i}'s debug dump must not be empty");
        assert!(
            level
                .cells
                .values()
                .any(|k| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded)),
            "level {i} must have at least one walkable cell"
        );
    }
}

#[test]
fn a_second_seed_produces_a_different_shape() {
    let rungs = [DelveRung::Undercroft];
    let origins = [ChamberOrigin::Found];
    let depths_m = [20.0];

    let a = generate_descent(
        &rungs,
        CaveKind::LavaTube,
        &origins,
        &depths_m,
        90.0,
        Seed(1),
    );
    let b = generate_descent(
        &rungs,
        CaveKind::LavaTube,
        &origins,
        &depths_m,
        90.0,
        Seed(2),
    );
    assert_ne!(
        a[0].cells, b[0].cells,
        "two different seeds must not coincidentally produce the same level"
    );
}
