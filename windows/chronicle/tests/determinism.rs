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
fn the_loop_actually_produces_events() {
    // Guard against a vacuous spike: a 200-community, 300-epoch run must
    // append a substantial number of events, and exercise the coupling
    // (at least one Raided/Fled pair) — else the benchmark measures nothing.
    let world = run(&cfg(42));
    let total: usize = world.communities.iter().map(|c| c.biography.len()).sum();
    assert!(total > 500, "expected many events, got {total}");
    let raids = world
        .communities
        .iter()
        .flat_map(|c| &c.biography)
        .filter(|e| matches!(e.event, hornvale_chronicle::EventKind::Raided))
        .count();
    assert!(
        raids > 0,
        "the coupling never fired: no raids in {total} events"
    );
}
