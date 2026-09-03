//! Leaf content: the four styles a region can be filled with (spec §4.3).
//! Three underlying techniques — a fourth (`RoomsAndCorridors`) reuses
//! `AngularRooms`'s partitioned-rooms carver with different tuning, since
//! sharing the technique and varying its parameters is the same move
//! `lattice::allocate`/`lattice::grow` make with `extent_for` (DRY over a
//! false four-way split).
//!
//! **No label here is dead data.** All four [`Algorithm`] variants are
//! constructed by real production code: `choose_leaf_style` (The Gallery,
//! Task 4) selects among them from `CaveKind` and `ChamberOrigin`. This
//! note used to lead [`Algorithm`]'s own rustdoc, where a reader met an
//! obituary for a retired partition label where they expected a type (The
//! Crosscut's deferred minor); it is a fact about the module, so it lives
//! here.

use std::collections::BTreeMap;

use hornvale_kernel::Stream;

use crate::lattice::{Cell, Rect};
use crate::underworld_level::CellGrid;
use crate::underworld_level::LevelCellKind;
use crate::underworld_level::region::cut;

/// Which content generator fills a leaf.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Algorithm {
    /// Karst-biased: an organic cavern via cellular automata.
    CellularCave,
    /// LavaTube-biased: a carved tube via a drunkard's walk.
    Tunneler,
    /// Fracture-biased: small angular rooms via tight partitioning.
    AngularRooms,
    /// Worked-biased: larger, straighter rooms via loose partitioning.
    RoomsAndCorridors,
}

/// Carve `algorithm`'s content into `rect`'s interior of `cells`, drawing
/// from `stream`. Returns the number of draws made.
///
/// **Takes an already-derived `&mut Stream`, not a `Seed`.** Two leaves in
/// one level can share an algorithm (composite levels are the point of
/// this campaign), and if each carve call re-derived its own fresh stream
/// from the same top-level seed, two same-algorithm leaves would draw
/// byte-identical content — the same defect class `lattice::allocate`'s
/// `split` and `lattice::grow` avoid by deriving their stream ONCE per
/// pass and threading `&mut Stream` through every draw. The caller
/// (`generate_level_with_origin`, Task 4) derives one stream per algorithm
/// family, once per level, and reuses it across every leaf that draws that
/// family — consecutive leaves differ because the stream's state advances,
/// not because a new stream was created per leaf.
pub(super) fn carve(
    algorithm: Algorithm,
    rect: Rect,
    stream: &mut Stream,
    cells: &mut CellGrid,
) -> u32 {
    match algorithm {
        Algorithm::CellularCave => carve_cellular_cave(rect, stream, cells),
        Algorithm::Tunneler => carve_tunneler(rect, stream, cells),
        Algorithm::AngularRooms => carve_partitioned_rooms(rect, stream, cells, 3, 3),
        Algorithm::RoomsAndCorridors => carve_partitioned_rooms(rect, stream, cells, 5, 2),
    }
}

/// plumb: pending(wave-1)
const CA_FILL_PROB: f64 = 0.45;
/// plumb: pending(wave-1)
const CA_ITERATIONS: u32 = 4;

fn carve_cellular_cave(rect: Rect, stream: &mut Stream, cells: &mut CellGrid) -> u32 {
    let mut dof = 0u32;
    let mut alive: BTreeMap<Cell, bool> = BTreeMap::new();
    for x in rect.x..(rect.x + rect.w) {
        for y in rect.y..(rect.y + rect.h) {
            let border =
                x == rect.x || y == rect.y || x == rect.x + rect.w - 1 || y == rect.y + rect.h - 1;
            let draw = stream.next_f64();
            dof += 1;
            alive.insert(Cell(x, y), !border && draw < CA_FILL_PROB);
        }
    }
    for _ in 0..CA_ITERATIONS {
        let snapshot = alive.clone();
        for x in (rect.x + 1)..(rect.x + rect.w - 1) {
            for y in (rect.y + 1)..(rect.y + rect.h - 1) {
                let count = neighbor_alive_count(&snapshot, x, y);
                let cur = *snapshot.get(&Cell(x, y)).unwrap_or(&false);
                let next = if cur { count >= 4 } else { count >= 5 };
                alive.insert(Cell(x, y), next);
            }
        }
    }
    for (cell, is_floor) in alive {
        if is_floor {
            cells.set(cell, LevelCellKind::Floor);
        }
    }
    connect_components_within(rect, cells);
    dof
}

/// Cellular-automata fill-and-smooth commonly settles into more than one
/// disconnected cavern; join every component into one before returning,
/// reusing the exact nearest-pair-corridor primitives `mod.rs` already
/// uses for sibling-leaf connections (Task 9) — the same operation
/// (join two disconnected walkable regions) at a different scope.
fn connect_components_within(rect: Rect, cells: &mut CellGrid) {
    loop {
        let components = find_components(rect, cells);
        if components.len() <= 1 {
            break;
        }
        match super::nearest_pair(&components[0], &components[1]) {
            Some((pa, pb)) => super::connect_cells(pa, pb, cells),
            None => break,
        }
    }
}

/// Every connected component (4-directional adjacency) of `Floor` cells
/// within `rect`, via breadth-first flood-fill. `BTreeSet`/`VecDeque`
/// only, no `HashSet` (workspace-wide determinism rule).
fn find_components(rect: Rect, cells: &CellGrid) -> Vec<Vec<Cell>> {
    use std::collections::{BTreeSet, VecDeque};

    let mut visited: BTreeSet<Cell> = BTreeSet::new();
    let mut components = Vec::new();
    for x in rect.x..(rect.x + rect.w) {
        for y in rect.y..(rect.y + rect.h) {
            let cell = Cell(x, y);
            if visited.contains(&cell) || cells.get(cell) != Some(LevelCellKind::Floor) {
                continue;
            }
            let mut component = Vec::new();
            let mut queue = VecDeque::new();
            visited.insert(cell);
            queue.push_back(cell);
            while let Some(Cell(cx, cy)) = queue.pop_front() {
                component.push(Cell(cx, cy));
                for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                    let next = Cell(cx + dx, cy + dy);
                    if rect.contains(next)
                        && cells.get(next) == Some(LevelCellKind::Floor)
                        && visited.insert(next)
                    {
                        queue.push_back(next);
                    }
                }
            }
            components.push(component);
        }
    }
    components
}

fn neighbor_alive_count(alive: &BTreeMap<Cell, bool>, x: i32, y: i32) -> u32 {
    let mut n = 0;
    for dx in -1..=1 {
        for dy in -1..=1 {
            if dx == 0 && dy == 0 {
                continue;
            }
            if *alive.get(&Cell(x + dx, y + dy)).unwrap_or(&false) {
                n += 1;
            }
        }
    }
    n
}

/// plumb: pending(wave-1)
const TUNNEL_STEPS: u32 = 30;
/// plumb: pending(wave-1)
const TUNNEL_MAX_RUN: u64 = 3;

fn carve_tunneler(rect: Rect, stream: &mut Stream, cells: &mut CellGrid) -> u32 {
    let interior = rect.inset(1);
    if interior.w < 1 || interior.h < 1 {
        // Degenerate rect (too small to have an interior once inset) — draw
        // nothing, same graceful degradation `carve_cellular_cave` and
        // `carve_partitioned_rooms`'s own subdivision bound already give
        // small/degenerate rects, rather than panicking in `.clamp()` below.
        return 0;
    }
    let mut dof = 0u32;
    let mut x = interior.x + (stream.next_u64() % interior.w.max(1) as u64) as i32;
    let mut y = interior.y + interior.h / 2;
    dof += 1;
    cells.set(Cell(x, y), LevelCellKind::Floor);
    for _ in 0..TUNNEL_STEPS {
        let heading = stream.next_u64();
        let run = 1 + (stream.next_u64() % TUNNEL_MAX_RUN) as i32;
        dof += 2;
        let (dx, dy) = match heading % 4 {
            0 => (1, 0),
            1 => (-1, 0),
            2 => (0, 1),
            _ => (0, -1),
        };
        for _ in 0..run {
            x = (x + dx).clamp(interior.x, interior.x + interior.w - 1);
            y = (y + dy).clamp(interior.y, interior.y + interior.h - 1);
            cells.set(Cell(x, y), LevelCellKind::Floor);
            let widened = if dx != 0 {
                Cell(x, (y + 1).min(interior.y + interior.h - 1))
            } else {
                Cell((x + 1).min(interior.x + interior.w - 1), y)
            };
            cells.set(widened, LevelCellKind::Floor);
        }
    }
    dof
}

fn carve_partitioned_rooms(
    rect: Rect,
    stream: &mut Stream,
    cells: &mut CellGrid,
    min_room_span: i32,
    max_room_depth: u32,
) -> u32 {
    let mut dof = 0u32;
    let mut rooms = Vec::new();
    subdivide_for_rooms(
        rect.inset(1),
        0,
        max_room_depth,
        min_room_span,
        stream,
        &mut dof,
        &mut rooms,
    );
    for room in &rooms {
        for x in room.x..(room.x + room.w) {
            for y in room.y..(room.y + room.h) {
                cells.set(Cell(x, y), LevelCellKind::Floor);
            }
        }
    }
    for pair in rooms.windows(2) {
        connect_centers(pair[0], pair[1], cells);
    }
    dof
}

fn subdivide_for_rooms(
    r: Rect,
    depth: u32,
    max_depth: u32,
    min_span: i32,
    stream: &mut Stream,
    dof: &mut u32,
    out: &mut Vec<Rect>,
) {
    let shorter = r.w.min(r.h);
    if depth >= max_depth || shorter < 2 * min_span + 1 {
        out.push(r);
        return;
    }
    let roll = stream.next_f64();
    *dof += 1;
    if roll < 0.6 {
        let (a, b) = cut(r, min_span, stream, dof);
        subdivide_for_rooms(a, depth + 1, max_depth, min_span, stream, dof, out);
        subdivide_for_rooms(b, depth + 1, max_depth, min_span, stream, dof, out);
    } else {
        out.push(r);
    }
}

fn connect_centers(a: Rect, b: Rect, cells: &mut CellGrid) {
    let a_center = Cell(a.x + a.w / 2, a.y + a.h / 2);
    let b_center = Cell(b.x + b.w / 2, b.y + b.h / 2);
    super::connect_cells(a_center, b_center, cells);
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    const RECT: Rect = Rect {
        x: 0,
        y: 0,
        w: 20,
        h: 14,
    };

    fn floor_count(cells: &CellGrid) -> usize {
        cells
            .iter()
            .filter(|(_, k)| *k == LevelCellKind::Floor)
            .count()
    }

    /// Each algorithm's own label, exactly as `generate_level_with_origin`
    /// (Task 4) will derive it — one fresh stream per call here, since a
    /// bare test has no other leaf to collide with, but `carve` itself
    /// takes `&mut Stream` rather than `Seed` precisely so the real caller
    /// can thread ONE stream across several same-algorithm leaves instead
    /// of every leaf re-deriving an identical one.
    fn stream_for(algorithm: Algorithm, seed: Seed) -> Stream {
        let label = match algorithm {
            Algorithm::CellularCave => crate::streams::UNDERWORLD_LEVEL_CELLULAR,
            Algorithm::Tunneler => crate::streams::UNDERWORLD_LEVEL_TUNNELER,
            Algorithm::AngularRooms | Algorithm::RoomsAndCorridors => {
                crate::streams::UNDERWORLD_LEVEL_ROOMS
            }
        };
        seed.derive(label).stream()
    }

    #[test]
    fn every_algorithm_produces_at_least_one_floor_cell() {
        for algorithm in [
            Algorithm::CellularCave,
            Algorithm::Tunneler,
            Algorithm::AngularRooms,
            Algorithm::RoomsAndCorridors,
        ] {
            let mut stream = stream_for(algorithm, Seed(3));
            let mut cells = CellGrid::new(RECT, LevelCellKind::Wall);
            carve(algorithm, RECT, &mut stream, &mut cells);
            assert!(
                floor_count(&cells) > 0,
                "{algorithm:?} produced no floor at all"
            );
        }
    }

    #[test]
    fn carving_is_deterministic() {
        for algorithm in [
            Algorithm::CellularCave,
            Algorithm::Tunneler,
            Algorithm::AngularRooms,
            Algorithm::RoomsAndCorridors,
        ] {
            let mut stream_a = stream_for(algorithm, Seed(11));
            let mut stream_b = stream_for(algorithm, Seed(11));
            let mut a = CellGrid::new(RECT, LevelCellKind::Wall);
            let mut b = CellGrid::new(RECT, LevelCellKind::Wall);
            carve(algorithm, RECT, &mut stream_a, &mut a);
            carve(algorithm, RECT, &mut stream_b, &mut b);
            assert_eq!(a, b, "{algorithm:?} was not deterministic");
        }
    }

    #[test]
    fn carving_never_touches_outside_the_rect() {
        // A dense `CellGrid` is total over its own extent, so unlike the old
        // `BTreeMap` (which only ever held keys `carve` explicitly wrote), a
        // grid sized exactly to `RECT` could never observe an escape — every
        // cell it can address is already inside `RECT` by construction.
        // Back it with a larger extent than `RECT`, all `Wall`, so a write
        // outside `RECT` (but still inside the grid) is observable as a
        // surviving non-`Wall` cell.
        let margin = 5;
        let backing = Rect {
            x: RECT.x - margin,
            y: RECT.y - margin,
            w: RECT.w + 2 * margin,
            h: RECT.h + 2 * margin,
        };
        for algorithm in [
            Algorithm::CellularCave,
            Algorithm::Tunneler,
            Algorithm::AngularRooms,
            Algorithm::RoomsAndCorridors,
        ] {
            let mut stream = stream_for(algorithm, Seed(5));
            let mut cells = CellGrid::new(backing, LevelCellKind::Wall);
            carve(algorithm, RECT, &mut stream, &mut cells);
            for x in backing.x..(backing.x + backing.w) {
                for y in backing.y..(backing.y + backing.h) {
                    let cell = Cell(x, y);
                    if RECT.contains(cell) {
                        continue;
                    }
                    assert_eq!(
                        cells.get(cell),
                        Some(LevelCellKind::Wall),
                        "{algorithm:?}: cell {cell:?} escaped its own leaf rect"
                    );
                }
            }
        }
    }

    #[test]
    fn two_leaves_sharing_an_algorithm_and_a_stream_differ() {
        // The regression this whole restructuring exists to prevent: one
        // stream threaded across two carve calls (as the real caller does)
        // must NOT produce identical content twice.
        let mut stream = stream_for(Algorithm::CellularCave, Seed(9));
        let mut first = CellGrid::new(RECT, LevelCellKind::Wall);
        let mut second = CellGrid::new(RECT, LevelCellKind::Wall);
        carve(Algorithm::CellularCave, RECT, &mut stream, &mut first);
        carve(Algorithm::CellularCave, RECT, &mut stream, &mut second);
        assert_ne!(
            first, second,
            "two carves sharing one advancing stream must differ, or generate_level_with_origin's per-family stream reuse would silently duplicate leaves"
        );
    }

    /// BFS-reachability check shared by the three internal-connectivity
    /// tests below: from an arbitrary floor cell, every other floor cell
    /// produced by `carve_fn` must be reachable via 4-directional adjacency.
    fn assert_internally_connected(
        carve_fn: impl Fn(Rect, &mut Stream, &mut CellGrid) -> u32,
        label: hornvale_kernel::seed::StreamLabel<'_>,
        seed_value: u64,
    ) {
        use std::collections::{BTreeSet, VecDeque};

        let mut stream = Seed(seed_value).derive(label).stream();
        let mut cells = CellGrid::new(RECT, LevelCellKind::Wall);
        carve_fn(RECT, &mut stream, &mut cells);

        let floor: BTreeSet<Cell> = cells
            .iter()
            .filter(|(_, k)| *k == LevelCellKind::Floor)
            .map(|(c, _)| c)
            .collect();
        let Some(&start) = floor.iter().next() else {
            return;
        };
        let mut seen = BTreeSet::new();
        let mut queue = VecDeque::new();
        seen.insert(start);
        queue.push_back(start);
        while let Some(Cell(x, y)) = queue.pop_front() {
            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let next = Cell(x + dx, y + dy);
                if floor.contains(&next) && seen.insert(next) {
                    queue.push_back(next);
                }
            }
        }
        assert_eq!(
            seen.len(),
            floor.len(),
            "seed {seed_value}: {} of {} floor cells unreachable from {start:?}",
            floor.len() - seen.len(),
            floor.len()
        );
    }

    #[test]
    /// claim: invariant(seed: 0..20) — every floor cell `carve_cellular_cave`
    /// produces is reachable from every other, for every seed in the range.
    /// This is the property Task 3's original cellular-automata design
    /// never guaranteed, and `connect_components_within` now enforces.
    fn cellular_cave_output_is_internally_connected() {
        for seed_value in 0..20u64 {
            assert_internally_connected(
                carve_cellular_cave,
                crate::streams::UNDERWORLD_LEVEL_CELLULAR,
                seed_value,
            );
        }
    }

    #[test]
    /// claim: invariant(seed: 0..20) — every floor cell `carve_tunneler`
    /// produces is reachable from every other, for every seed in the range.
    /// Empirically verify (not merely reason about) that `Tunneler` is
    /// already internally connected by construction — a single random
    /// walk never leaves a gap for its own path to fall into.
    fn tunneler_output_is_internally_connected() {
        for seed_value in 0..20u64 {
            assert_internally_connected(
                carve_tunneler,
                crate::streams::UNDERWORLD_LEVEL_TUNNELER,
                seed_value,
            );
        }
    }

    #[test]
    /// claim: invariant(seed: 0..20) — every floor cell
    /// `carve_partitioned_rooms` produces (both `AngularRooms` and
    /// `RoomsAndCorridors` tunings) is reachable from every other, for
    /// every seed in the range. Empirically verify that `PartitionedRooms`
    /// is already internally connected by construction —
    /// `connect_centers` over every consecutive pair in `rooms.windows(2)`
    /// joins the whole chain.
    fn partitioned_rooms_output_is_internally_connected() {
        for seed_value in 0..20u64 {
            assert_internally_connected(
                |rect, stream, cells| carve_partitioned_rooms(rect, stream, cells, 3, 3),
                crate::streams::UNDERWORLD_LEVEL_ROOMS,
                seed_value,
            );
            assert_internally_connected(
                |rect, stream, cells| carve_partitioned_rooms(rect, stream, cells, 5, 2),
                crate::streams::UNDERWORLD_LEVEL_ROOMS,
                seed_value,
            );
        }
    }
}
