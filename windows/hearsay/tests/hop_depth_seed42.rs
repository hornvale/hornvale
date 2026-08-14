//! The preregistered readout (spec §6). Live worldgen; heavy tier only.

use hornvale_hearsay::derive::witnesses_of;
use hornvale_hearsay::{
    divergent_witnesses, echo_ratio, hops_about, lineage::lineage_of, median_hops,
};

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
    // H3: echo_ratio of every qualifying ending (spec §6.2, >= 3 holders).
    let mut ratios: Vec<f64> = Vec::new();
    // H5: qualifying endings with two or more divergent witness lines (§6.3).
    let mut qualifying: usize = 0;
    let mut divergent_or_more: usize = 0;
    for s in lin.all() {
        all.extend(hops_about(
            &world.ledger,
            &lin,
            s,
            hornvale_history::OCC_ENDED,
        ));
        if let Some(r) = echo_ratio(&world.ledger, &lin, s, hornvale_history::OCC_ENDED) {
            ratios.push(r);
            qualifying += 1;
            let witnesses = witnesses_of(&world.ledger, &lin, s, hornvale_history::OCC_ENDED);
            let divergent = divergent_witnesses(&lin, &witnesses);
            if divergent.len() >= 2 {
                divergent_or_more += 1;
            }
        }
    }
    assert!(
        all.len() >= 500,
        "NO VERDICT: {} (event, holder) pairs, spec section 6 requires >= 500",
        all.len()
    );
    all.sort_unstable();
    let median = all[all.len() / 2];
    let tail = all.iter().filter(|h| **h >= 10).count() as f64 / all.len() as f64;

    for r in &ratios {
        assert!(
            *r > 0.0 && *r <= 1.0,
            "echo_ratio must be in (0, 1]: got {r}"
        );
    }
    ratios.sort_by(f64::total_cmp);
    let h3 = ratios.get(ratios.len() / 2).copied().unwrap_or(f64::NAN);
    let n_ratio = ratios.len();

    let h5 = if qualifying == 0 {
        f64::NAN
    } else {
        divergent_or_more as f64 / qualifying as f64
    };
    if qualifying > 0 {
        assert!(
            (0.0..=1.0).contains(&h5),
            "divergent_fraction must be in [0, 1]: got {h5}"
        );
    }

    println!(
        "hops: pairs={} median={median} tail_ge_10={tail:.4} max={} | \
         H3 median_echo_ratio={h3:.4} over {n_ratio} endings | \
         H5 divergent_fraction={h5:.4}",
        all.len(),
        all[all.len() - 1]
    );
    assert_eq!(
        median_hops(&world.ledger, &lin, hornvale_history::OCC_ENDED),
        Some(f64::from(median)),
        "median_hops must agree with the battery's own computation"
    );
}
