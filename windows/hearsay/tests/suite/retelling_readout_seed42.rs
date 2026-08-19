//! The preregistered readout (spec §6). Live worldgen.
//!
//! Reports H1, H2 and H3 against their decision tables. It asserts only the
//! NO VERDICT floors and the stated ceilings; the hypotheses themselves are
//! REPORTED, because a falsified prediction is a finding and this file must
//! not be edited to rescue one.

use hornvale_hearsay::contact::contact_of;
use hornvale_hearsay::derive::{variants_about, witnesses_of};
use hornvale_hearsay::divergence::maximum_antichain;
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_hearsay::transmission::{Transmission, Walk};
use hornvale_hearsay::{finest_precision_hops, lineage::lineage_of, spearman, variant_count};

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loop binds
/// occupation ids, not seeds.
#[test]
fn the_retelling_readout_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);
    let graph = contact_of(led);
    let walk = Walk {
        ledger: led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };
    let ladder = PrecisionLadder::of(led);

    // --- H1: per-hop counts of claims still at the finest precision ---
    let mut by_hop: std::collections::BTreeMap<u32, usize> = std::collections::BTreeMap::new();
    let mut finest_total = 0usize;
    for s in lin.all() {
        for h in finest_precision_hops(&walk, &ladder, s, hornvale_history::OCC_ENDED) {
            *by_hop.entry(h).or_default() += 1;
            finest_total += 1;
        }
    }
    let ratios: Vec<f64> = (1..=8)
        .filter_map(|k| {
            let a = *by_hop.get(&k)? as f64;
            let b = *by_hop.get(&(k + 1))? as f64;
            if a == 0.0 { None } else { Some(b / a) }
        })
        .collect();
    let mean = if ratios.is_empty() {
        f64::NAN
    } else {
        ratios.iter().sum::<f64>() / ratios.len() as f64
    };
    let var = if ratios.is_empty() {
        f64::NAN
    } else {
        ratios.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / ratios.len() as f64
    };

    // --- H2 and H3 ---
    let mut counts: Vec<f64> = Vec::new();
    let mut widths: Vec<f64> = Vec::new();
    for s in lin.all() {
        let Some(n) = variant_count(&walk, &ladder, s, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let ws = witnesses_of(led, &lin, s, hornvale_history::OCC_ENDED);
        counts.push(n as f64);
        widths.push(maximum_antichain(&lin, &ws).len() as f64);
    }
    let qualifying = counts.len();
    let mut sorted = counts.clone();
    sorted.sort_by(f64::total_cmp);
    let median = sorted.get(sorted.len() / 2).copied().unwrap_or(f64::NAN);
    let rho = spearman(&widths, &counts);

    println!(
        "H1 finest_precision_pairs={finest_total} per_hop={by_hop:?} \
         ratios={ratios:?} mean={mean:.4} var={var:.4}"
    );
    println!("H2 qualifying={qualifying} median_variants={median} distribution={sorted:?}");
    println!("H3 spearman_rho={rho:?}");

    // The only assertions are the floors and ceilings the spec states.
    assert!(
        finest_total >= 500 || qualifying < 100,
        "H1 NO VERDICT floor: {finest_total} finest-precision pairs"
    );
    // The ceiling is THIS WORLD'S ladder length, not a constant -- a
    // two-mooned world offers rungs a moonless one does not, which is why H2
    // reports the ladder alongside its distribution rather than comparing raw
    // variant counts across worlds.
    let ceiling = ladder.len() as f64;
    for n in &counts {
        assert!(
            *n <= ceiling,
            "a variant count above this world's {ceiling} rungs is impossible: {n}"
        );
    }
    println!("LADDER len={} rungs={:?}", ladder.len(), ladder.labels());

    // WHICH RUNGS ARE ACTUALLY REACHED. H2 asserts a ceiling of ladder.len(),
    // and a ceiling is only informative next to the floor it bounds: if the
    // deepest rung any claim reaches is far below the ladder's end, the ladder
    // is over-provisioned and H2's ceiling was never in play. Measured rather
    // than inferred from H2's distribution -- variants <= 2 PER EVENT does not
    // imply a rung is unused GLOBALLY, since different events could use
    // different rungs.
    let mut rungs: std::collections::BTreeMap<u8, usize> = std::collections::BTreeMap::new();
    for s in lin.all() {
        for v in variants_about(&walk, &ladder, s, hornvale_history::OCC_ENDED) {
            *rungs.entry(v.precision.rung()).or_default() += 1;
        }
    }
    println!(
        "RUNGS REACHED {rungs:?} (ladder has {} rungs)",
        ladder.len()
    );
}
