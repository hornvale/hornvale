//! Junctions — the underworld becomes a network (The Stope, Task 6).
//!
//! `junctions_at` derives, from terrain FACTS alone, which other cave
//! systems a chamber's system joins at a shared delve band. This realizes
//! `MAP-underworld-shortcut`: "hard to enter, easy to traverse once inside;
//! two points far apart on the surface can be close below" — false by
//! construction before this task, because one cave never connected to
//! another.
//!
//! The three tests here are the task's own Step 1 list:
//!
//! 1. `a_junction_is_derived_not_drawn` — THE constraint. A junction is a
//!    fact about the geology, not a die roll: the derivation consumes no
//!    stream of its own, so interleaving unrelated draws cannot move an
//!    answer.
//! 2. `junctions_are_symmetric` — if A reaches B, B reaches A.
//! 3. `a_junction_never_crosses_a_band` — two systems join at a SHARED band
//!    or not at all; otherwise a junction is a vertical teleport and the
//!    depth ladder means nothing.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, Geosphere, Seed};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::chamber::{ChamberAddr, junctions_at};

/// The production mesh level and seed the panel tests share with this
/// crate's other seed-42 batteries (`gazetteer_naming.rs`'s idiom), so the
/// junction population measured here is the population of the canonical
/// globe.
const LEVEL: u32 = 6;

/// Seed-42 terrain built through the ordinary genesis path — the same
/// `hornvale_terrain::generate` -> `GeneratedTerrain::new` chain every other
/// seed-42 test in this crate uses.
fn panel_terrain() -> GeneratedTerrain {
    let geo = Geosphere::new(LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate");
    GeneratedTerrain::new(geo, outcome)
}

/// Every distinct junction edge in the world, each pair listed once (the
/// lexicographically smaller cell first). The scan walks every cave-bearing
/// land cell and every habitation band through the SHIPPED entry point.
/// Claim shape: one seed (42) over one shared terrain fixture; no per-item
/// draws.
fn all_junction_edges(terrain: &GeneratedTerrain) -> Vec<(ChamberAddr, ChamberAddr)> {
    let mut edges = Vec::new();
    for cell in terrain.geosphere().cells() {
        let Some(_cave) = terrain.cave_at(cell) else {
            continue;
        };
        if terrain.is_ocean(cell) {
            continue;
        }
        for band in 0..5u8 {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                branch: 0,
                band,
                floor: 0,
            };
            for far in junctions_at(Seed(42), terrain, addr) {
                if far.cell > addr.cell {
                    edges.push((addr, far));
                }
            }
        }
    }
    edges
}

/// THE constraint (Task 6, step 1): a junction is DERIVED, not drawn. The
/// derivation consumes no stream of its own, so calling it again — including
/// after a thousand unrelated draws have advanced nothing it reads — gives
/// byte-identical answers. A drawn link would be a new draw on top of an
/// epoch and would make a shortcut a die roll rather than a fact about the
/// geology.
#[test]
fn a_junction_is_derived_not_drawn() {
    let terrain = panel_terrain();
    // Non-vacuity first: the seed-42 world must actually join at least two
    // systems somewhere, or every assertion below is true of an empty set.
    let edges = all_junction_edges(&terrain);
    assert!(
        !edges.is_empty(),
        "seed 42 produced no junctions at all — the network never formed"
    );
    let (a, _b) = edges[0];

    let first = junctions_at(Seed(42), &terrain, a);
    assert!(
        !first.is_empty(),
        "edge found by the scan but not re-derived"
    );

    // Advance an unrelated leg hard, then ask again: the answer cannot move.
    let mut stream = Seed(42)
        .derive(StreamLabel::dynamic("junction-probe/unrelated-leg"))
        .stream();
    for _ in 0..1000 {
        let _ = stream.next_f64();
    }
    let second = junctions_at(Seed(42), &terrain, a);
    assert_eq!(first, second, "junctions_at moved after unrelated draws");
}

/// If A reaches B, B reaches A (Task 6, step 1). Asked from either endpoint,
/// the same relation answers — there is one junction, not two directed ones.
#[test]
fn junctions_are_symmetric() {
    let terrain = panel_terrain();
    let edges = all_junction_edges(&terrain);
    assert!(
        !edges.is_empty(),
        "seed 42 produced no junctions at all — symmetry holds vacuously"
    );
    for (a, b) in &edges {
        let back = junctions_at(Seed(42), &terrain, *b);
        assert!(
            back.contains(a),
            "junction {a:?} -> {b:?} is not symmetric: asked from B, A is absent"
        );
    }
}

/// Two systems join at a SHARED band or not at all (Task 6, step 1): every
/// junction endpoint sits at exactly the band it was asked from, on the
/// neighbour system's main line, in a geographically adjacent cell. Anything
/// else would be a vertical teleport, and the depth ladder would mean
/// nothing.
#[test]
fn a_junction_never_crosses_a_band() {
    let terrain = panel_terrain();
    let geo = terrain.geosphere();
    for (a, b) in all_junction_edges(&terrain) {
        assert_eq!(b.band, a.band, "junction {a:?} -> {b:?} changed bands");
        assert_eq!(b.entrance, 0, "junction endpoint left the main line");
        assert_eq!(b.branch, 0, "junction endpoint left the main line");
        assert_eq!(b.floor, 0, "junction endpoint named a floor");
        let neighbours: Vec<CellId> = geo.neighbors(a.cell).to_vec();
        assert!(
            neighbours.contains(&b.cell),
            "junction {a:?} -> {b:?} crossed to a non-adjacent cell"
        );
    }
}
