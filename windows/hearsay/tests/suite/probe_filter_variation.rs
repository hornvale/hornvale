//! Substrate probe 2 (The Retelling): do the ROLE-and-HISTORY filter keys
//! actually vary, and — the question that decides the campaign — do they vary
//! BETWEEN THE WITNESSES OF ONE EVENT?
//!
//! Probe 1 killed `occ-people` as a filter key: 0 of 658 inheritance edges
//! cross a people boundary, so a species-keyed filter is inert. These two keys
//! are derived from committed facts instead of identity:
//!
//!   productive filter — IS THIS COMMUNITY A RAIDER? (it appears as the
//!     `Entity` value of some `occ-ended-by`). An incentive: a raider
//!     narrating a raid it committed has an interest in the telling.
//!   receptive filter — WAS THIS COMMUNITY BORN OF A CATASTROPHE? (founded on
//!     exactly its parent's ending day — campaign 1's survivor rule).
//!
//! Divergent accounts require the witnesses of ONE event to hold DIFFERENT
//! keys. If almost every event's witnesses share a key, the two-filter model
//! is another constant dressed as a finding, and it must be found here.

use hornvale_hearsay::derive::witnesses_of;
use hornvale_hearsay::{echo_ratio, lineage::lineage_of};
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

fn people_of(ledger: &Ledger, occ: EntityId) -> Option<String> {
    match ledger.value_of(occ, hornvale_history::OCC_PEOPLE) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

fn number(ledger: &Ledger, occ: EntityId, predicate: &str) -> Option<f64> {
    match ledger.value_of(occ, predicate) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loops bind
/// occupation ids, not seeds. One fixed world, reported as substrate.
#[test]
#[ignore = "probe: whether filter keys vary between witnesses of one event on seed 42; run by hand (The Retelling answered its question; demoted by The Governor 2026-08-28)"]
fn do_the_filter_keys_vary_between_witnesses_of_one_event_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);

    // --- the productive key: who has ever raided? ---
    let mut raiders: BTreeSet<EntityId> = BTreeSet::new();
    for fact in led.find(hornvale_history::OCC_ENDED_BY) {
        if let Value::Entity(attacker) = &fact.object {
            raiders.insert(*attacker);
        }
    }

    // --- the receptive key: who was born of a catastrophe? ---
    let mut survivors: BTreeSet<EntityId> = BTreeSet::new();
    let mut edges_with_ended_parent = 0usize;
    let mut edges_parent_never_ended = 0usize;
    for child in lin.all() {
        let Some(parent) = lin.parent(child) else {
            continue;
        };
        match number(led, parent, hornvale_history::OCC_ENDED) {
            None => edges_parent_never_ended += 1,
            Some(ended) => {
                edges_with_ended_parent += 1;
                if number(led, child, hornvale_history::OCC_FOUNDED) == Some(ended) {
                    survivors.insert(child);
                }
            }
        }
    }

    // does the raider key vary WITHIN a people? (the thing species-keying
    // could not do)
    let mut per_people: BTreeMap<String, (usize, usize)> = BTreeMap::new();
    for occ in lin.all() {
        if let Some(p) = people_of(led, occ) {
            let e = per_people.entry(p).or_insert((0, 0));
            e.0 += 1;
            if raiders.contains(&occ) {
                e.1 += 1;
            }
        }
    }
    let peoples_with_both = per_people
        .values()
        .filter(|(n, r)| *r > 0 && *r < *n)
        .count();

    // --- THE DECIDING NUMBER: distinct keys among one event's witnesses ---
    let key_of = |occ: EntityId| (raiders.contains(&occ), survivors.contains(&occ));
    let mut qualifying = 0usize;
    let mut distinct_hist: BTreeMap<usize, usize> = BTreeMap::new();
    let mut divergent_capable = 0usize;
    for subject in lin.all() {
        if echo_ratio(led, &lin, subject, hornvale_history::OCC_ENDED).is_none() {
            continue;
        }
        qualifying += 1;
        let ws = witnesses_of(led, &lin, subject, hornvale_history::OCC_ENDED);
        let keys: BTreeSet<(bool, bool)> = ws.iter().map(|w| key_of(*w)).collect();
        *distinct_hist.entry(keys.len()).or_default() += 1;
        if keys.len() >= 2 {
            divergent_capable += 1;
        }
    }

    let all = lin.all().len();
    println!(
        "RAIDERS      {} of {all} occupations ({:.4}); peoples with BOTH raiders \
         and non-raiders = {peoples_with_both} of {}",
        raiders.len(),
        raiders.len() as f64 / all as f64,
        per_people.len()
    );
    println!(
        "SURVIVORS    {} of {all} ({:.4}); edges with an ended parent={edges_with_ended_parent}, \
         parent never ended={edges_parent_never_ended}",
        survivors.len(),
        survivors.len() as f64 / all as f64
    );
    println!(
        "DIVERGENCE-CAPABLE  {divergent_capable} of {qualifying} qualifying endings ({:.4}) \
         have >= 2 distinct witness keys | distinct-key histogram {distinct_hist:?}",
        if qualifying == 0 {
            f64::NAN
        } else {
            divergent_capable as f64 / qualifying as f64
        }
    );
}
