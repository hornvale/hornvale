//! Substrate probe (The Retelling): where can a claim cross a people
//! boundary? Measured, not assumed — spec §6.1 of The Hearsay established the
//! precedent that substrate counts are reported before any hypothesis is
//! frozen against them.
//!
//! The two-filter model (producer -> productive filter -> receptive filter ->
//! receiver) predicts that accounts can only diverge where the teller's and
//! hearer's filters are MISMATCHED. If filters key on `occ-people`, then
//! mismatch is possible only on an edge whose two ends hold different peoples.
//! This probe counts those edges. It asserts nothing about outcomes.

use hornvale_hearsay::derive::witnesses_of;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// The people occupying `occ`, as committed text, if any.
fn people_of(ledger: &Ledger, occ: EntityId) -> Option<String> {
    match ledger.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loop binds
/// occupation ids, not seeds. One fixed world, reported as substrate.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn where_can_a_claim_cross_a_people_boundary_on_seed_42() {
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

    // --- 1. Inheritance edges: does fission ever cross a people boundary? ---
    let mut edges = 0usize;
    let mut edges_typed = 0usize;
    let mut edges_crossing = 0usize;
    for child in lin.all() {
        let Some(parent) = lin.parent(child) else {
            continue;
        };
        edges += 1;
        let (Some(pc), Some(pp)) = (people_of(led, child), people_of(led, parent)) else {
            continue;
        };
        edges_typed += 1;
        if pc != pp {
            edges_crossing += 1;
        }
    }

    // --- 2. Witness seams at hops 0: victim vs the other parties present. ---
    let mut endings = 0usize;
    let mut endings_with_foreign_witness = 0usize;
    let mut witness_pairs = 0usize;
    let mut witness_pairs_crossing = 0usize;
    for subject in lin.all() {
        if led.value_of(subject, hornvale_history::OCC_ENDED).is_none() {
            continue;
        }
        endings += 1;
        let ws = witnesses_of(led, &lin, subject, hornvale_history::OCC_ENDED);
        let Some(pv) = people_of(led, subject) else {
            continue;
        };
        let mut foreign = false;
        for w in &ws {
            if *w == subject {
                continue;
            }
            let Some(pw) = people_of(led, *w) else {
                continue;
            };
            witness_pairs += 1;
            if pw != pv {
                witness_pairs_crossing += 1;
                foreign = true;
            }
        }
        if foreign {
            endings_with_foreign_witness += 1;
        }
    }

    // --- 3. How many peoples, and how concentrated? ---
    let mut census: BTreeMap<String, usize> = BTreeMap::new();
    for occ in lin.all() {
        if let Some(p) = people_of(led, occ) {
            *census.entry(p).or_default() += 1;
        }
    }
    let peoples: BTreeSet<&String> = census.keys().collect();

    println!(
        "INHERITANCE  edges={edges} typed={edges_typed} crossing={edges_crossing} \
         ({:.4} of typed)",
        if edges_typed == 0 {
            f64::NAN
        } else {
            edges_crossing as f64 / edges_typed as f64
        }
    );
    println!(
        "WITNESS      endings={endings} with_foreign_witness={endings_with_foreign_witness} \
         ({:.4}) | non-subject witness pairs={witness_pairs} crossing={witness_pairs_crossing}",
        if endings == 0 {
            f64::NAN
        } else {
            endings_with_foreign_witness as f64 / endings as f64
        }
    );
    println!("PEOPLES      distinct={} census={census:?}", peoples.len());
}
