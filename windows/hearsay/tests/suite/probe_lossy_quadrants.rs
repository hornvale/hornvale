//! Substrate probe 3 (The Retelling): how often does each (teller, hearer)
//! quadrant actually occur along the 658 inheritance edges?
//!
//! The spec's lossy predicate compares two DIFFERENT properties with `!=`:
//! `raids(teller) != born_of_catastrophe(hearer)`. That makes a raider telling
//! the community that fled his raid come out FRICTIONLESS, which is backwards.
//! Before choosing a replacement, count the quadrants — a predicate that is
//! elegant and fires on 2% of edges is worse than an awkward one that fires on
//! 30%, and neither can be judged without the denominators.
//!
//! Reports only. Asserts nothing about outcomes.

use hornvale_hearsay::lineage::lineage_of;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeSet;

fn number(led: &Ledger, occ: EntityId, pred: &str) -> Option<f64> {
    match led.value_of(occ, pred) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// claim: structural(seed: 42) — false-positive seed-loop flag; the loop binds
/// occupation ids, not seeds.
#[test]
#[ignore = "probe: how often each teller/hearer quadrant occurs on seed 42; run by hand (The Retelling answered its question; demoted by The Governor 2026-08-28)"]
fn how_often_does_each_teller_hearer_quadrant_occur_on_seed_42() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let led = &world.ledger;
    let lin = lineage_of(led);

    let mut raiders: BTreeSet<EntityId> = BTreeSet::new();
    for f in led.find(hornvale_history::OCC_ENDED_BY) {
        if let Value::Entity(a) = &f.object {
            raiders.insert(*a);
        }
    }
    let mut survivors: BTreeSet<EntityId> = BTreeSet::new();
    for child in lin.all() {
        if let Some(parent) = lin.parent(child)
            && let (Some(e), Some(f)) = (
                number(led, parent, hornvale_history::OCC_ENDED),
                number(led, child, hornvale_history::OCC_FOUNDED),
            )
            && e == f
        {
            survivors.insert(child);
        }
    }

    // Quadrants over every teller->hearer inheritance edge.
    let (mut rr, mut rn, mut nr, mut nn) = (0usize, 0usize, 0usize, 0usize);
    for child in lin.all() {
        let Some(parent) = lin.parent(child) else {
            continue;
        };
        match (raiders.contains(&parent), survivors.contains(&child)) {
            (true, true) => rr += 1,
            (true, false) => rn += 1,
            (false, true) => nr += 1,
            (false, false) => nn += 1,
        }
    }
    let total = rr + rn + nr + nn;
    let pct = |n: usize| 100.0 * n as f64 / total as f64;

    println!("EDGES total={total}");
    println!(
        "  teller RAIDS  x hearer BORN-OF-CATASTROPHE : {rr:4} ({:.1}%)",
        pct(rr)
    );
    println!(
        "  teller RAIDS  x hearer ordinary            : {rn:4} ({:.1}%)",
        pct(rn)
    );
    println!(
        "  teller quiet  x hearer BORN-OF-CATASTROPHE : {nr:4} ({:.1}%)",
        pct(nr)
    );
    println!(
        "  teller quiet  x hearer ordinary            : {nn:4} ({:.1}%)",
        pct(nn)
    );
    println!();
    println!("CANDIDATE PREDICATES, as a share of edges:");
    println!(
        "  spec's  raids(t) != born(h)      -> lossy on {:.1}%",
        pct(rn + nr)
    );
    println!(
        "  inverse raids(t) == born(h)      -> lossy on {:.1}%",
        pct(rr + nn)
    );
    println!(
        "  AND     raids(t) && born(h)      -> lossy on {:.1}%",
        pct(rr)
    );
    println!(
        "  OR      raids(t) || born(h)      -> lossy on {:.1}%",
        pct(rr + rn + nr)
    );
}
