//! The preregistered readout (spec §6). Live worldgen; heavy tier only.

use hornvale_hearsay::{hops_about, lineage::lineage_of, median_hops};

/// claim: structural(seed: 42) — false-positive seed-loop flag; `s` binds an
/// occupation id from `lin.all()`, not a seed. One fixed world, the full
/// (event, holder) population over it, asserted universally per spec §6's
/// preregistered decision rule.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn transmission_depth_on_seed_42_has_a_population_and_a_median() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let lin = lineage_of(&world.ledger);
    let mut all: Vec<u32> = Vec::new();
    for s in lin.all() {
        all.extend(hops_about(
            &world.ledger,
            &lin,
            s,
            hornvale_history::OCC_ENDED,
        ));
    }
    assert!(
        all.len() >= 500,
        "NO VERDICT: {} (event, holder) pairs, spec section 6 requires >= 500",
        all.len()
    );
    all.sort_unstable();
    let median = all[all.len() / 2];
    let tail = all.iter().filter(|h| **h >= 10).count() as f64 / all.len() as f64;
    println!(
        "hops: pairs={} median={median} tail_ge_10={tail:.4} max={}",
        all.len(),
        all[all.len() - 1]
    );
    assert_eq!(
        median_hops(&world.ledger, &lin, hornvale_history::OCC_ENDED),
        Some(f64::from(median)),
        "median_hops must agree with the battery's own computation"
    );
}
