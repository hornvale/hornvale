//! A one-off calibration: the distribution of `approach_ease` over habitable
//! cells, pooled across seeds. Run once, by hand, to set `DEF_SCALE` so the
//! median cell's defensibility is ~1.0 — i.e. the median world is unchanged
//! and only the extremes of the terrain move. `#[ignore]`d because it is a
//! measurement, not a gate; it asserts nothing about the result.
//!
//! **Era choice: present.** `history_bake`'s bake holds one
//! [`ConnectionGraph`] per paleoclimate era (`graphs: &[ConnectionGraph]`,
//! `windows/worldgen/src/history_bake.rs`), but the private `bake_history_from`
//! that assembles that per-era array is not reachable from an integration
//! test. This harness instead reads the graph through the crate's public
//! seam, [`connection_graph_of`] — the same present-day graph
//! `sundered_landmasses` and the almanac's connectivity view already read
//! elsewhere. That is, in effect, the bake's *last* era: `bake_eras`' final
//! entry sits at `era_day == 0` (present) with its `sea_level_change`
//! sampled at ice-history `t == 0`, so it reduces to the same present sea
//! level `connection_graph_of` reads off the built terrain. Picking ONE era
//! (present) rather than pooling across eras keeps the measured distribution
//! from blurring a glacial low-stand's wide land bridges together with a
//! high-stand's narrow ones — the brief is explicit that averaging across
//! eras would do exactly that.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_topology::ConnectionGraph;
use hornvale_worldgen::{
    BuildDepth, GraphConfig, SETTLERS_PER_CAPACITY, SettlementPins, SkyChoice, WorldComponents,
    build_world_to_with_artifacts, carrying_inputs_of, connection_graph_of,
};

/// The summed conductance of `cell`'s traversable (`conductance > 0.0`)
/// edges. Mirrors `history_bake::approach_ease` deliberately: that fn is
/// private to its module, and a measurement harness is not a reason to make
/// it `pub` or add a second `#[doc(hidden)] pub` wrapper — this is the
/// two-line fold over the public `ConnectionGraph::edges` the brief asks
/// for instead.
fn approach_ease(graph: &ConnectionGraph, cell: CellId) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.conductance)
        .sum()
}

#[test]
#[ignore = "calibration: run by hand, prints the approach_ease quantiles"]
fn print_approach_ease_quantiles() {
    let mut all: Vec<f64> = Vec::new();
    for seed in 1u64..=30 {
        // Build to Settlements depth: the terrain/climate the present-day
        // connection graph and the capacity field both read off exist there,
        // and the full stack (culture/religion/deep-history) is not needed.
        let wc = WorldComponents::assemble().expect("registries well-formed");
        let artifacts = build_world_to_with_artifacts(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .expect("seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Settlements");
        let climate = artifacts
            .climate
            .expect("climate is Some at BuildDepth::Settlements");
        let geo = terrain.geosphere();

        // "Habitable" == non-zero capacity, the same field the deep-history
        // bake feeds communities from (`bake_history_from`'s `capacity`).
        let suitability = hornvale_demography::carrying_capacity(
            geo,
            &carrying_inputs_of(geo, &terrain, &climate),
        );
        let capacity =
            hornvale_kernel::CellMap::from_fn(geo, |c| *suitability.get(c) * SETTLERS_PER_CAPACITY);

        let graph = connection_graph_of(&artifacts.world, &GraphConfig::default());

        for (cell, cap) in capacity.iter() {
            if *cap > 0.0 {
                all.push(approach_ease(&graph, cell));
            }
        }
    }
    all.sort_by(f64::total_cmp);
    for q in [0.05, 0.25, 0.50, 0.75, 0.95] {
        let i = ((all.len() as f64 - 1.0) * q).round() as usize;
        println!("q{:.2} = {:.6}", q, all[i]);
    }
}
