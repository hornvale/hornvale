//! Spec §5.3: the raid seam, read out of `occ-ended-by` as an undirected
//! edge set stamped with each raid's day.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::contact::contact_of;
use hornvale_kernel::ledger::Value;

fn raid(led: &mut hornvale_kernel::ledger::Ledger, victim: u64, attacker: u64, day: f64) {
    put(led, victim, hornvale_history::OCC_ENDED, Value::Number(day));
    put(
        led,
        victim,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(attacker)),
    );
}

#[test]
fn a_raid_makes_one_undirected_edge_stamped_with_its_day() {
    let mut led = ledger_with(&[(1, None), (5, None)]);
    raid(&mut led, 1, 5, 400.0);
    let g = contact_of(&led);

    assert_eq!(g.peers_of(eid(1)), &[(eid(5), 400.0)], "victim sees raider");
    assert_eq!(g.peers_of(eid(5)), &[(eid(1), 400.0)], "raider sees victim");
    assert_eq!(g.edges(), 1, "one raid, one edge");
}

#[test]
fn an_ending_with_no_named_attacker_makes_no_edge() {
    let mut led = ledger_with(&[(1, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(400.0),
    );
    let g = contact_of(&led);
    assert_eq!(g.edges(), 0, "no occ-ended-by, no contact");
    assert!(g.peers_of(eid(1)).is_empty());
}

#[test]
fn an_occupation_in_several_raids_carries_several_peers_ascending() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    raid(&mut led, 2, 5, 700.0);
    raid(&mut led, 1, 5, 400.0);
    let g = contact_of(&led);

    let peers = g.peers_of(eid(5));
    assert_eq!(peers.len(), 2, "the raider was in two raids");
    assert!(
        peers.windows(2).all(|w| w[0] <= w[1]),
        "peers must be ascending and therefore deterministic, got {peers:?}"
    );
    assert_eq!(g.edges(), 2);
}

/// A peers list that is not deduplicated would let one raid be walked twice
/// and inflate every reach number the readout reports.
#[test]
fn a_repeated_raid_pair_is_recorded_once_per_ending() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    raid(&mut led, 1, 5, 400.0);
    raid(&mut led, 2, 5, 400.0);
    let g = contact_of(&led);
    let peers = g.peers_of(eid(5));
    assert_eq!(peers.len(), 2, "two distinct victims, two edges: {peers:?}");
}
