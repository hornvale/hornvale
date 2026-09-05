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

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Seed, Vertex};
use hornvale_terrain::{Cave, CaveKind, GeothermalGradient, StratigraphicColumn, TerrainPins};
use hornvale_vessel::{
    Cell, Level, LevelCellKind, generate_descent, generate_descent_for_character,
};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, Chamber, ChamberAddr, ChamberOrigin, ChamberOverrides, chamber_at,
    chamber_exists,
};
use hornvale_worldgen::character::Character;
use hornvale_worldgen::circuit::plan_descent;
use hornvale_worldgen::{
    BarrierPins, BarrierState, BuildDepth, SettlementPins, WorldComponents, barrier_of,
    build_world_to_with_artifacts,
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

/// Search vertices `0..50` under `seed`/`cave`/`gradient` for one whose lattice
/// has a real, *existing* chamber address at every band in `bands` (in any
/// branch) — existence is sparse (`deep_realm_chamber.rs`'s own
/// `the_lattice_is_fixed_and_existence_is_sparse`), so not every vertex
/// qualifies. Returns one `ChamberAddr` per requested band, in the same
/// order.
fn find_addrs_at_bands(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    bands: &[Band],
) -> Vec<ChamberAddr> {
    for raw_vertex in 0u32..50 {
        let vertex = Vertex(raw_vertex);
        let mut found = Vec::new();
        for &band in bands {
            let mut hit = None;
            for branch in 0..BRANCHES_PER_SYSTEM {
                let addr = ChamberAddr {
                    vertex,
                    band,
                    branch,
                    level: 0,
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
        "no vertex among 0..50 had chambers existing at every requested band \
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
            let glyph = match level.cells.get(cell) {
                Some(LevelCellKind::Floor) => '.',
                Some(LevelCellKind::Wall) | None => '#',
                Some(LevelCellKind::Flooded) => '~',
                Some(LevelCellKind::StairsDown) => '>',
                Some(LevelCellKind::StairsUp) => '<',
                // The Brattice, spec §3.5. Same three glyphs the session's
                // own `map` verb picks, so a dump read beside one is not
                // two vocabularies.
                Some(LevelCellKind::Threshold) => '\'',
                Some(LevelCellKind::Deep) => '=',
                Some(LevelCellKind::Drop) => 'v',
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
        .map(|(c, _)| c)
        .collect()
}

/// Every cell a player could actually stand on: `walkable_cells` plus
/// `StairsDown`/`StairsUp`. **Used for the connectivity BFS, not the
/// non-degeneracy check above, and the difference matters**: a stairs cell
/// the plan places (`place_stair`,
/// `windows/vessel/src/underworld_level/mod.rs`) is always standable and,
/// if the carve had left it in rock, joined to its own region — so
/// excluding stairs cells from the connectivity search would report a false
/// disconnection that has nothing to do with carving — first observed
/// exactly this way when this test was written: a `Floor`-only BFS over a
/// real descent found 176/277 cells unreachable, entirely explained by a
/// stairs cell sitting on the level's one connecting corridor.
///
/// **Since The Brattice the set IS the `movement_mode` seam**, not a kind
/// list: a `Threshold` sits in the middle of every passage's corridor and a
/// `Deep` run replaces a sump's, so a kind list would have reported exactly
/// the false disconnection this doc already warns about, one campaign later
/// and for a new reason. `Deep` is included because it is passable — to a
/// swimmer; this is the resident's view of the level, which is what a
/// connectivity claim about a level is for.
fn standable_cells(level: &Level) -> BTreeSet<Cell> {
    level
        .cells
        .iter()
        .filter(|(_, k)| hornvale_vessel::underworld_level::movement_mode(*k).is_some())
        .map(|(c, _)| c)
        .collect()
}

/// Flood-fills across the WHOLE descent — same-level 4-adjacency plus a
/// paired `StairsDown`/`StairsUp` cell crossing into the neighbouring
/// rung — and asserts every standable cell of every level is reached. An
/// integration-level sanity check over a REAL descent, deliberately
/// duplicating (in miniature) `windows/vessel/src/underworld_level/mod.rs`'s
/// own `every_character_engine_keeps_every_level_connected` — this file is
/// a different compilation unit (an integration test, not a lib test), so
/// there is no clean way to share the helper without exposing new pub test
/// infrastructure just for this.
///
/// **Per-level connectivity alone is NOT guaranteed under The Crosscut,**
/// and asserting it here used to pass only because an earlier revision of
/// this campaign's stair-repair (`reconnect_region`) carved a bypass
/// THROUGH a neighbouring region whenever a region's only same-level link
/// happened to sit under a stair — exactly the spec §3.3 violation
/// `unlinked_neighbours_keep_their_wall`
/// (`windows/vessel/src/underworld_level/mod.rs`) now pins. The plan's own
/// graph connectivity
/// (`every_node_is_reachable_from_the_entrance`,
/// `windows/worldgen/src/circuit.rs`) is proven across same-level passages
/// AND cross-level stairs together, never per level, so a region can
/// legitimately have no same-level passage at all and be reachable only
/// by taking its stair to a neighbouring rung and back.
fn assert_whole_descent_is_connected(levels: &[Level], standables: &[BTreeSet<Cell>]) {
    let total: usize = standables.iter().map(BTreeSet::len).sum();
    let Some(&start_cell) = standables[0].iter().next() else {
        return; // nothing to connect; the caller asserts non-emptiness separately
    };
    let start = (0usize, start_cell);
    let mut seen: BTreeSet<(usize, Cell)> = BTreeSet::new();
    let mut queue = VecDeque::new();
    seen.insert(start);
    queue.push_back(start);
    while let Some((lvl, Cell(x, y))) = queue.pop_front() {
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let next = (lvl, Cell(x + dx, y + dy));
            if standables[lvl].contains(&next.1) && seen.insert(next) {
                queue.push_back(next);
            }
        }
        let kind = levels[lvl].cells.get(Cell(x, y));
        if kind == Some(LevelCellKind::StairsDown) && lvl + 1 < levels.len() {
            let below = (lvl + 1, Cell(x, y));
            if standables[lvl + 1].contains(&below.1) && seen.insert(below) {
                queue.push_back(below);
            }
        }
        if kind == Some(LevelCellKind::StairsUp) && lvl > 0 {
            let above = (lvl - 1, Cell(x, y));
            if standables[lvl - 1].contains(&above.1) && seen.insert(above) {
                queue.push_back(above);
            }
        }
        // A chute's lip crosses DOWN only (The Brattice, spec §3.5) — `up`
        // from beneath it needs `Fly`, and this descent is walked from rung
        // 0 downward, so the one-way edge is the one the search needs.
        if kind == Some(LevelCellKind::Drop) && lvl + 1 < levels.len() {
            let below = (lvl + 1, Cell(x, y));
            if standables[lvl + 1].contains(&below.1) && seen.insert(below) {
                queue.push_back(below);
            }
        }
    }
    assert_eq!(
        seen.len(),
        total,
        "{} of {} standable cells unreachable across the whole descent",
        total - seen.len(),
        total
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
    let addrs = find_addrs_at_bands(
        fixture_seed,
        &cave,
        gradient,
        &[Band::Undercroft, Band::Shallows, Band::Deeps],
    );

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

    let vertex = addrs[0].vertex;
    let plan = plan_descent(
        Seed(42),
        vertex,
        &rungs,
        CaveKind::Karst,
        Character::WildCave,
    );
    let a = generate_descent(
        &rungs,
        CaveKind::Karst,
        &origins,
        &depths_m,
        90.0,
        &plan,
        Seed(42),
    );
    let b = generate_descent(
        &rungs,
        CaveKind::Karst,
        &origins,
        &depths_m,
        90.0,
        &plan,
        Seed(42),
    );
    assert_eq!(
        a, b,
        "a real descent must be byte-identical for the same seed"
    );

    assert_eq!(a.len(), rungs.len());
    let mut standables = Vec::with_capacity(a.len());
    for (i, level) in a.iter().enumerate() {
        let dump = render_debug(level);
        assert!(!dump.is_empty(), "level {i}'s debug dump must not be empty");
        let walkable = walkable_cells(level);
        assert!(
            !walkable.is_empty(),
            "level {i} must have at least one walkable cell"
        );
        standables.push(standable_cells(level));
    }
    assert_whole_descent_is_connected(&a, &standables);
}

#[test]
fn a_second_seed_produces_a_different_shape() {
    let col = fixture_column();
    let gradient = fixture_gradient();
    let cave = Cave::from_reach(CaveKind::LavaTube, REACH_M, &col);

    let fixture_seed = Seed(90210);
    let addrs = find_addrs_at_bands(fixture_seed, &cave, gradient, &[Band::Undercroft]);
    let no_overrides = ChamberOverrides::new();
    let chamber = chamber_at(fixture_seed, &cave, gradient, &col, addrs[0], &no_overrides)
        .expect("address was confirmed to exist by find_addrs_at_bands");
    let rungs = [chamber.rung];
    let origins = [chamber.origin];
    // See the comment on the sibling test: no Chamber->metres adapter exists
    // yet, so this stays a representative literal.
    let depths_m = [20.0];

    let vertex = addrs[0].vertex;
    let plan_a = plan_descent(
        Seed(1),
        vertex,
        &rungs,
        CaveKind::LavaTube,
        Character::WildCave,
    );
    let plan_b = plan_descent(
        Seed(2),
        vertex,
        &rungs,
        CaveKind::LavaTube,
        Character::WildCave,
    );
    let a = generate_descent(
        &rungs,
        CaveKind::LavaTube,
        &origins,
        &depths_m,
        90.0,
        &plan_a,
        Seed(1),
    );
    let b = generate_descent(
        &rungs,
        CaveKind::LavaTube,
        &origins,
        &depths_m,
        90.0,
        &plan_b,
        Seed(2),
    );
    assert_ne!(
        a[0].cells, b[0].cells,
        "two different seeds must not coincidentally produce the same level"
    );
}

// --- Task 0 (the flooded-cell rule, spec §3.2): measurement, not shipped
// behaviour. Everything below this line reports; it asserts nothing about
// the fraction flooded or the fraction reachable — see the probe's own doc
// comment for the posture and why.

/// The chamber-entrance address `windows/vessel/src/session.rs`'s private
/// `cave_entrance_addr` also builds (branch 0, level 0, `Undercroft`).
/// Duplicated here rather than called: that helper (and
/// `find_open_cave_vertex` below it) live inside a `#[cfg(test)] mod tests`
/// block in `session.rs`'s own crate, unreachable from this integration
/// binary, which compiles as a separate crate.
fn probe_cave_entrance_addr(vertex: Vertex) -> ChamberAddr {
    ChamberAddr {
        vertex,
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    }
}

/// Reproduces `windows/vessel/src/session.rs`'s private `find_open_cave_vertex`:
/// the first non-ocean, cave-bearing vertex whose entrance chamber resolves
/// (`chamber_at` is `Some`) AND whose seeded barrier is `BarrierState::Open`.
/// Returns `None` rather than panicking — unlike the original, this is swept
/// across many seeds and a seed with no such vertex is skipped, not fatal.
fn probe_find_open_cave_vertex(
    terrain: &hornvale_terrain::GeneratedTerrain,
    seed: Seed,
) -> Option<(Vertex, Cave)> {
    let overrides = ChamberOverrides::new();
    let pins = BarrierPins::default();
    terrain.geosphere().vertices().find_map(|vertex| {
        if terrain.is_ocean(vertex) {
            return None;
        }
        let cave = terrain.cave_at(vertex)?;
        let gradient = terrain.geothermal_gradient_at(vertex);
        let column = terrain.column_at(vertex);
        let addr = probe_cave_entrance_addr(vertex);
        let realized = chamber_at(seed, &cave, gradient, &column, addr, &overrides).is_some();
        let open =
            barrier_of(seed, addr.vertex, addr.band, addr.branch, &pins) == BarrierState::Open;
        (realized && open).then_some((vertex, cave))
    })
}

/// Every `Floor`/`StairsDown`/`StairsUp` cell of `level` — `standable_cells`
/// minus every `Flooded` one, i.e. the passable set under this probe's
/// "`Flooded` is impassable" treatment, compared against `standable_cells`'s
/// own "`Flooded` is passable" treatment.
fn dry_standable_cells(level: &Level) -> BTreeSet<Cell> {
    level
        .cells
        .iter()
        .filter(|(_, k)| {
            matches!(
                k,
                LevelCellKind::Floor | LevelCellKind::StairsDown | LevelCellKind::StairsUp
            )
        })
        .map(|(c, _)| c)
        .collect()
}

/// The FIRST cell holding `kind`; a rung may hold several since The
/// Crosscut.
fn find_cell_of_kind(level: &Level, kind: LevelCellKind) -> Option<Cell> {
    level.cells.iter().find(|(_, k)| *k == kind).map(|(c, _)| c)
}

/// Rung 0's entry cell (The Crosscut, Task 3): the first `Floor`/`Flooded`
/// cell of the plan's own entrance region — the region a descending player
/// actually stands in (`Underground::enter`'s own cell pick), used here
/// because rung 0 carries no `StairsUp` to search for instead.
fn entrance_cell(level: &Level, plan: &hornvale_worldgen::circuit::DescentPlan) -> Option<Cell> {
    let r = plan.region_of(plan.entrance);
    for x in r.x..(r.x + r.w) {
        for y in r.y..(r.y + r.h) {
            let cell = Cell(x, y);
            if matches!(
                level.cells.get(cell),
                Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
            ) {
                return Some(cell);
            }
        }
    }
    None
}

/// Flood-fills `passable` from `start` (4-directional adjacency), returning
/// every cell reached including `start` itself — empty if `start` is not
/// itself a member of `passable`.
fn flood_fill(start: Cell, passable: &BTreeSet<Cell>) -> BTreeSet<Cell> {
    let mut seen = BTreeSet::new();
    if !passable.contains(&start) {
        return seen;
    }
    let mut queue = VecDeque::new();
    seen.insert(start);
    queue.push_back(start);
    while let Some(Cell(x, y)) = queue.pop_front() {
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let next = Cell(x + dx, y + dy);
            if passable.contains(&next) && seen.insert(next) {
                queue.push_back(next);
            }
        }
    }
    seen
}

/// Task 0's own probe: how much of a real descent is under water, and can a
/// descending player still get through it.
///
/// **REPORTED, never asserted** (spec §3.2) — the posture
/// `windows/worldgen/tests/suite/deep_realm_rehome.rs` states explicitly at
/// its own head: whichever way the numbers land is the finding this test
/// exists to produce. The only assertion below guards against a silently
/// vacuous sweep (too few seeds contributed a real measurement), never
/// against a particular flooded fraction or reachable fraction.
///
/// **The descent's inputs are the production ones, and this is the whole
/// validity of the measurement** — copied verbatim from
/// `windows/worldgen/src/lib.rs:3259-3267` (gradient / porosity /
/// `water_table_depth_m` / per-rung `rung_evaluation_depth_m`), with
/// `cave.kind` rather than a hardcoded `CaveKind`. Flooding is decided by
/// depth against the water table, so an invented depth or water table would
/// measure a fiction, not the world.
///
/// Every rung uses `ChamberOrigin::Found`: `is_sump` (the flooding gate
/// `generate_level_with_water` consults) treats `ChamberOrigin::Made` as
/// always-drained, but nothing in the shipped path ever constructs a
/// `ChamberOverrides` that would produce `Made` (see `is_sump`'s own doc
/// comment) — every chamber a player can reach resolves `Found` today, so
/// that is what this probe measures too.
///
/// **Entry cells** (F3 in the plan's pre-flight scan): rung 0 has no
/// `StairsUp`, so its entry is the first walkable cell of the plan's own
/// entrance region (`entrance_cell`) — the same region `delve` will place
/// the possession in, since the entrance IS where a descending player
/// stands. Every deeper rung's entry is its `StairsUp` cell.
#[test]
fn measure_flooded_cell_reachability_across_the_descent() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let habitation_rungs: Vec<Band> = hornvale_terrain::rungs()
        .iter()
        .copied()
        .filter(|&r| r != Band::Surface)
        .collect();
    let rung_count = habitation_rungs.len();

    let mut total_cells_sum = vec![0usize; rung_count];
    let mut walkable_cells_sum = vec![0usize; rung_count];
    let mut flooded_cells_sum = vec![0usize; rung_count];
    let mut reach_impassable_frac_sum = vec![0.0f64; rung_count];
    let mut reach_passable_frac_sum = vec![0.0f64; rung_count];
    let mut down_reachable_impassable_count = vec![0usize; rung_count];

    let mut seeds_measured = 0usize;
    let mut seeds_fully_reachable_impassable = 0usize;

    // Sweep seeds until at least 60 have contributed a real measurement (the
    // brief's "at least 50", with headroom for a seed whose terrain has no
    // open, unbarred cave mouth at all), or the search runs out of budget.
    let mut seed_val = 1u64;
    while seeds_measured < 60 && seed_val <= 500 {
        let seed = Seed(seed_val);
        seed_val += 1;

        let Ok(artifacts) = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Terrain,
        ) else {
            continue;
        };
        let Some(terrain) = artifacts.terrain.as_ref() else {
            continue;
        };
        let Some((vertex, cave)) = probe_find_open_cave_vertex(terrain, seed) else {
            continue;
        };

        // The production recipe, verbatim (windows/worldgen/src/lib.rs:3259-3267).
        let gradient = terrain.geothermal_gradient_at(vertex);
        let porosity = terrain.material_at(vertex).porosity;
        let height_asl_m = terrain
            .elevation_at(vertex)
            .above(terrain.sea_level())
            .get();
        let water_table_m = hornvale_terrain::water_table_depth_m(
            terrain.drainage_at(vertex),
            porosity,
            height_asl_m,
        );
        let depths_m: Vec<f64> = habitation_rungs
            .iter()
            .map(|&rung| {
                hornvale_terrain::rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m)
                    .expect("every non-Surface rung has an evaluation depth")
            })
            .collect();
        let origins = vec![ChamberOrigin::Found; rung_count];

        let plan = plan_descent(
            seed,
            vertex,
            &habitation_rungs,
            cave.kind,
            Character::WildCave,
        );
        let levels = generate_descent_for_character(
            &habitation_rungs,
            cave.kind,
            &origins,
            &depths_m,
            water_table_m,
            Character::WildCave,
            &plan,
            seed,
        );

        seeds_measured += 1;
        let mut seed_fully_reachable = true;
        for (i, level) in levels.iter().enumerate() {
            let total_cells = (level.extent.w as usize) * (level.extent.h as usize);
            let standable = standable_cells(level);
            let dry = dry_standable_cells(level);
            let flooded_cells = standable.len() - dry.len();

            let entry = if i == 0 {
                entrance_cell(level, &plan)
            } else {
                find_cell_of_kind(level, LevelCellKind::StairsUp)
            };
            let down = find_cell_of_kind(level, LevelCellKind::StairsDown);

            let (reach_impassable_frac, down_reachable) = match entry {
                Some(entry_cell) => {
                    let reached = flood_fill(entry_cell, &dry);
                    let frac = if standable.is_empty() {
                        0.0
                    } else {
                        reached.len() as f64 / standable.len() as f64
                    };
                    let down_ok = down.is_none_or(|d| reached.contains(&d));
                    (frac, down_ok)
                }
                None => (0.0, false),
            };
            let reach_passable_frac = match entry {
                Some(entry_cell) => {
                    let reached = flood_fill(entry_cell, &standable);
                    if standable.is_empty() {
                        0.0
                    } else {
                        reached.len() as f64 / standable.len() as f64
                    }
                }
                None => 0.0,
            };

            total_cells_sum[i] += total_cells;
            walkable_cells_sum[i] += standable.len();
            flooded_cells_sum[i] += flooded_cells;
            reach_impassable_frac_sum[i] += reach_impassable_frac;
            reach_passable_frac_sum[i] += reach_passable_frac;
            if down_reachable {
                down_reachable_impassable_count[i] += 1;
            } else {
                seed_fully_reachable = false;
            }
        }
        if seed_fully_reachable {
            seeds_fully_reachable_impassable += 1;
        }
    }

    assert!(
        seeds_measured >= 50,
        "only {seeds_measured} of {} attempted seeds produced a measurable descent \
         (an open, unbarred cave mouth) — widen the seed sweep; this guards the \
         sweep against being silently vacuous, not any flooded or reachable fraction",
        seed_val - 1
    );

    println!(
        "=== Task 0: flooded-cell reachability across the descent ({seeds_measured} seeds, seeds 1..{} scanned) ===",
        seed_val - 1
    );
    println!(
        "{:<12}{:>10}{:>10}{:>10}{:>9}{:>16}{:>15}{:>10}",
        "rung", "cells", "walk", "flood", "flood%", "reach%imp", "reach%pass", "down_ok%"
    );
    let n = seeds_measured as f64;
    for (i, &rung) in habitation_rungs.iter().enumerate() {
        let avg_total = total_cells_sum[i] as f64 / n;
        let avg_walk = walkable_cells_sum[i] as f64 / n;
        let avg_flood = flooded_cells_sum[i] as f64 / n;
        let flood_pct = 100.0 * avg_flood / avg_walk.max(1.0);
        let reach_impassable_pct = 100.0 * reach_impassable_frac_sum[i] / n;
        let reach_passable_pct = 100.0 * reach_passable_frac_sum[i] / n;
        let down_ok_pct = 100.0 * down_reachable_impassable_count[i] as f64 / n;
        println!(
            "{:<12}{:>10.1}{:>10.1}{:>10.1}{:>9.1}{:>16.1}{:>15.1}{:>10.1}",
            format!("{rung:?}"),
            avg_total,
            avg_walk,
            avg_flood,
            flood_pct,
            reach_impassable_pct,
            reach_passable_pct,
            down_ok_pct
        );
    }
    let seeds_pct = 100.0 * seeds_fully_reachable_impassable as f64 / n;
    println!(
        "seeds with EVERY rung's onward stairs reachable from its entry when Flooded is \
         impassable: {seeds_fully_reachable_impassable}/{seeds_measured} ({seeds_pct:.1}%)"
    );
}
