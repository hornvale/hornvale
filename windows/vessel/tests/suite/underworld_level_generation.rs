//! Integration tests for The Adit's level generator, against real
//! `Chamber`/`Cave` values built the same way
//! `windows/worldgen/tests/deep_realm_chamber.rs` does — not a hand-built
//! fixture, so a change to how those types are constructed is caught here
//! too.
//!
//! **Task review finding 1 (2026-08-19):** the original draft of this file
//! inherited a contradiction in the task brief itself — the brief's own
//! Interfaces section promised real `Chamber` construction via `chamber_at`,
//! but its Step 2 code block fed `generate_descent` hand-picked literal
//! `rungs`/`origins` arrays instead. `rungs` and `origins` are derived below
//! from real `Chamber` values found through `chamber_exists`/`chamber_at`
//! over a real `Cave`/`StratigraphicColumn`/`GeothermalGradient` fixture, the
//! same construction pattern `deep_realm_chamber.rs` uses. `depths_m` has no
//! `Chamber`-derived equivalent yet (see the comment where it is built) and
//! stays literal.
//!
//! **Task review finding 2 (2026-08-19, revised remedy):** the original draft
//! asserted only "at least one walkable cell" under the name "connected" —
//! this file now also flood-fills each level's walkable cells and asserts
//! they form one connected component, mirroring (with some deliberate
//! duplication — this is a different compilation unit) Task 9's
//! `every_walkable_cell_is_reachable_from_every_other`
//! (`windows/vessel/src/underworld_level/mod.rs`), which is the test that
//! caught composite levels not actually being connected until Task 9 added a
//! cross-leaf connector.

use std::collections::{BTreeSet, VecDeque};

use hornvale_kernel::{Band, CellId, Seed};
use hornvale_terrain::{Cave, CaveKind, GeothermalGradient, StratigraphicColumn};
use hornvale_vessel::{Cell, Level, LevelCellKind, generate_descent};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, Chamber, ChamberAddr, ChamberOrigin, ChamberOverrides, chamber_at,
    chamber_exists,
};

/// The column every fixture below is built against — the same 401 m-cover
/// continental column `deep_realm_chamber.rs`'s own `fixture_column` uses.
fn fixture_column() -> StratigraphicColumn {
    hornvale_terrain::column(
        35.0,
        0.3,
        true,
        400.0,
        1.0,
        hornvale_terrain::RockClass::Sandstone,
        hornvale_terrain::Basement::Continental,
    )
}

/// The geothermal gradient every fixture below is placed under, K/km — same
/// value as `deep_realm_chamber.rs`'s own `fixture_gradient`.
fn fixture_gradient() -> GeothermalGradient {
    GeothermalGradient::new(24.0)
}

/// A depth budget that reaches well past the `Deeps` rung (rank 2), so a
/// fixture built against it can supply real chambers for the 3-rung descent
/// below. Same value as `deep_realm_chamber.rs`'s own `DEEP_REACH_M`.
const REACH_M: f64 = 2000.0;

/// Search cells `0..50` under `seed`/`cave`/`gradient` for one whose lattice
/// has a real, *existing* chamber address at every band in `bands` (in any
/// branch) — existence is sparse (`deep_realm_chamber.rs`'s own
/// `the_lattice_is_fixed_and_existence_is_sparse`), so not every cell
/// qualifies. Returns one `ChamberAddr` per requested band, in the same
/// order.
fn find_addrs_at_bands(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    bands: &[u8],
) -> Vec<ChamberAddr> {
    for raw_cell in 0u32..50 {
        let cell = CellId(raw_cell);
        let mut found = Vec::new();
        for &band in bands {
            let mut hit = None;
            for branch in 0..BRANCHES_PER_SYSTEM {
                let addr = ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    branch,
                    floor: 0,
                };
                if chamber_exists(seed, cave, gradient, addr) {
                    hit = Some(addr);
                    break;
                }
            }
            match hit {
                Some(addr) => found.push(addr),
                None => {
                    found.clear();
                    break;
                }
            }
        }
        if found.len() == bands.len() {
            return found;
        }
    }
    panic!(
        "no cell among 0..50 had chambers existing at every requested band \
         {bands:?} under this fixture — widen the search or check the \
         density model"
    );
}

/// A small ASCII dump for visual sanity-checking during development —
/// not the production `map` verb (a later Delving campaign's job), and
/// not asserted against pixel-for-pixel; only exercised here for its own
/// non-panicking, non-empty output.
fn render_debug(level: &Level) -> String {
    let mut out = String::new();
    for y in level.extent.y..(level.extent.y + level.extent.h) {
        for x in level.extent.x..(level.extent.x + level.extent.w) {
            let cell = Cell(x, y);
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

/// Every `Floor`/`Flooded` cell of `level` — used only for the "at least one
/// walkable cell" non-degeneracy check, matching `LevelCellKind`'s own
/// pre-stairs walkable set (Task 9's in-crate connectivity invariant is
/// measured over exactly this set, on a level with no stairs carved into
/// it).
fn walkable_cells(level: &Level) -> BTreeSet<Cell> {
    level
        .cells
        .iter()
        .filter(|(_, k)| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded))
        .map(|(&c, _)| c)
        .collect()
}

/// Every cell a player could actually stand on: `walkable_cells` plus
/// `StairsDown`/`StairsUp`. **Used for the connectivity BFS, not the
/// non-degeneracy check above, and the difference matters**: `generate_descent`
/// (unlike Task 9's fixture, which calls `generate_level_with_origin`
/// directly) also runs `place_connections`, which overwrites one `Floor`
/// cell per level with `StairsDown`/`StairsUp`. If that overwritten cell was
/// an articulation point in the leaf-connector graph, excluding it from the
/// connectivity search would report a false disconnection that has nothing
/// to do with carving — first observed exactly this way when this test was
/// written: a `Floor`-only BFS over a real descent found 176/277 cells
/// unreachable, entirely explained by a stairs cell sitting on the level's
/// one connecting corridor.
fn standable_cells(level: &Level) -> BTreeSet<Cell> {
    level
        .cells
        .iter()
        .filter(|(_, k)| {
            matches!(
                k,
                LevelCellKind::Floor
                    | LevelCellKind::Flooded
                    | LevelCellKind::StairsDown
                    | LevelCellKind::StairsUp
            )
        })
        .map(|(&c, _)| c)
        .collect()
}

/// Flood-fills `standable` from one of its own cells (4-directional
/// adjacency) and asserts every standable cell was reached. An
/// integration-level sanity check over a REAL descent, deliberately
/// duplicating (in miniature) Task 9's own
/// `every_walkable_cell_is_reachable_from_every_other` invariant sweep
/// (`windows/vessel/src/underworld_level/mod.rs`) — this file is a different
/// compilation unit (an integration test, not a lib test), so there is no
/// clean way to share the helper without exposing new pub test
/// infrastructure just for this.
fn assert_standable_cells_are_connected(standable: &BTreeSet<Cell>, level_index: usize) {
    let Some(&start) = standable.iter().next() else {
        return; // nothing to connect; the caller asserts non-emptiness separately
    };
    let mut seen = BTreeSet::new();
    let mut queue = VecDeque::new();
    seen.insert(start);
    queue.push_back(start);
    while let Some(Cell(x, y)) = queue.pop_front() {
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let next = Cell(x + dx, y + dy);
            if standable.contains(&next) && seen.insert(next) {
                queue.push_back(next);
            }
        }
    }
    assert_eq!(
        seen.len(),
        standable.len(),
        "level {level_index}: {} of {} standable cells unreachable from {start:?}",
        standable.len() - seen.len(),
        standable.len()
    );
}

#[test]
fn a_real_descent_is_deterministic_connected_and_renders() {
    let col = fixture_column();
    let gradient = fixture_gradient();
    let cave = Cave::from_reach(CaveKind::Karst, REACH_M, &col);

    // A seed used only to pick which real chambers exist under this fixture
    // — independent of the seeds `generate_descent` is exercised with below,
    // the same way a world's chamber lattice and a level's own geometry are
    // independent draws in the real pipeline.
    let fixture_seed = Seed(90210);
    let addrs = find_addrs_at_bands(fixture_seed, &cave, gradient, &[0, 1, 2]);

    // Exercise the override seam too (spec §3.3): the middle rung's chamber
    // is `Made`, the other two are the address-derived default (`Found`) —
    // real variety, not a literal array.
    let mut overrides = ChamberOverrides::new();
    overrides.insert(addrs[1], ChamberOrigin::Made);

    let chambers: Vec<Chamber> = addrs
        .iter()
        .map(|&addr| {
            chamber_at(fixture_seed, &cave, gradient, &col, addr, &overrides)
                .expect("address was confirmed to exist by find_addrs_at_bands")
        })
        .collect();
    let rungs: Vec<Band> = chambers.iter().map(|c| c.rung).collect();
    let origins: Vec<ChamberOrigin> = chambers.iter().map(|c| c.origin).collect();
    assert_eq!(
        origins[1],
        ChamberOrigin::Made,
        "the override must have taken"
    );

    // `depths_m` has no `Chamber`-derived equivalent yet: `Chamber` carries a
    // rung (a depth CLASS) and a stratum, never a metres depth, and
    // `generate_descent`'s own doc comment names a `Chamber` -> metres
    // adapter as a later Delving campaign's job. Representative literals
    // stand in until that adapter exists.
    let depths_m = [20.0, 65.0, 140.0];

    let a = generate_descent(&rungs, CaveKind::Karst, &origins, &depths_m, 90.0, Seed(42));
    let b = generate_descent(&rungs, CaveKind::Karst, &origins, &depths_m, 90.0, Seed(42));
    assert_eq!(
        a, b,
        "a real descent must be byte-identical for the same seed"
    );

    assert_eq!(a.len(), rungs.len());
    for (i, level) in a.iter().enumerate() {
        let dump = render_debug(level);
        assert!(!dump.is_empty(), "level {i}'s debug dump must not be empty");
        let walkable = walkable_cells(level);
        assert!(
            !walkable.is_empty(),
            "level {i} must have at least one walkable cell"
        );
        let standable = standable_cells(level);
        assert_standable_cells_are_connected(&standable, i);
    }
}

#[test]
fn a_second_seed_produces_a_different_shape() {
    let col = fixture_column();
    let gradient = fixture_gradient();
    let cave = Cave::from_reach(CaveKind::LavaTube, REACH_M, &col);

    let fixture_seed = Seed(90210);
    let addrs = find_addrs_at_bands(fixture_seed, &cave, gradient, &[0]);
    let no_overrides = ChamberOverrides::new();
    let chamber = chamber_at(fixture_seed, &cave, gradient, &col, addrs[0], &no_overrides)
        .expect("address was confirmed to exist by find_addrs_at_bands");
    let rungs = [chamber.rung];
    let origins = [chamber.origin];
    // See the comment on the sibling test: no Chamber->metres adapter exists
    // yet, so this stays a representative literal.
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
