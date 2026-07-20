//! Fast gate tests: the spike is byte-deterministic and non-trivial.

use hornvale_chronicle::{SoundingConfig, biography_digest, run};
use hornvale_kernel::Seed;

fn cfg(seed: u64) -> SoundingConfig {
    SoundingConfig {
        seed: Seed(seed),
        communities: 200,
        species: 8,
        epochs: 300,
        avg_degree: 4.0,
        long_range_fraction: 0.05,
    }
}

#[test]
fn same_seed_same_config_is_byte_identical() {
    let a = biography_digest(&run(&cfg(42)));
    let b = biography_digest(&run(&cfg(42)));
    assert_eq!(
        a, b,
        "same seed + config must produce byte-identical biographies"
    );
}

#[test]
fn different_seeds_diverge() {
    let a = biography_digest(&run(&cfg(42)));
    let b = biography_digest(&run(&cfg(43)));
    assert_ne!(a, b, "a different seed must produce a different history");
}

#[test]
fn the_workload_fires_at_volume() {
    // The workload census must show the *measured* phenomena firing heavily —
    // else the benchmark measures noise (the failure the review caught: the
    // coupling had fired ~11 times). Grow, the coupling (Raided/Fled), and
    // collapses must all be present in quantity.
    use hornvale_chronicle::census;
    let c = census(&run(&cfg(42)));
    assert!(c.grew > 1_000, "too few grow events: {c:?}");
    assert!(c.raided > 1_000, "the coupling barely fired: {c:?}");
    assert!(
        c.fled > 1_000,
        "raids barely delivered (coupling inert): {c:?}"
    );
    assert!(c.collapsed > 0, "no collapses/ruins produced: {c:?}");
}

#[test]
fn scan_and_index_deliveries_produce_identical_worlds() {
    // The scan-vs-index timing comparison is only honest if both modes derive
    // the SAME world — one alive community per node makes them agree.
    use hornvale_chronicle::{DeliveryMode, run_with};
    let scan = biography_digest(&run_with(&cfg(42), DeliveryMode::Scan));
    let index = biography_digest(&run_with(&cfg(42), DeliveryMode::Index));
    assert_eq!(
        scan, index,
        "scan and index delivery must produce byte-identical worlds"
    );
}

#[test]
fn replay_derives_events_and_the_intervention_matters() {
    // Non-vacuous (the "asserting nothing" trap the review flagged): the replay
    // must (a) be reproducible, (b) derive a NON-ZERO number of events (the
    // neighbourhood churns forward), and (c) have that count CHANGED by the
    // intervention — killing a neighbour must propagate through the local raid.
    use hornvale_chronicle::NodeId;
    use hornvale_chronicle::measure::replay_event_count;
    let world = run(&cfg(7));
    let with = replay_event_count(&world, NodeId(3), 40, &cfg(7), true);
    let with_again = replay_event_count(&world, NodeId(3), 40, &cfg(7), true);
    let without = replay_event_count(&world, NodeId(3), 40, &cfg(7), false);
    assert_eq!(
        with, with_again,
        "a replay's derived events must be reproducible"
    );
    assert!(with > 0, "the replay derived no events (degenerate)");
    assert_ne!(
        with, without,
        "the intervention had no consequence — killing a neighbour changed nothing"
    );
}
