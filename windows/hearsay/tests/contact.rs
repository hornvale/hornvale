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

/// A community cannot meet itself: `occ-ended-by` naming the ending
/// occupation as its own attacker must produce no edge and leave its peers
/// list empty, not merely fail to panic.
#[test]
fn a_self_named_attacker_makes_no_edge() {
    let mut led = ledger_with(&[(1, None)]);
    raid(&mut led, 1, 1, 400.0);
    let g = contact_of(&led);
    assert_eq!(g.edges(), 0, "self-raid must not become an edge");
    assert!(g.peers_of(eid(1)).is_empty(), "no self-peer either");
}

/// A raid with no readable day cannot be time-gated, so it must contribute
/// no edge — asserted on the graph's shape, not merely that `contact_of`
/// returned without panicking.
#[test]
fn an_ending_with_a_non_number_day_makes_no_edge() {
    let mut led = ledger_with(&[(1, None), (5, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Text("unknown".to_string()),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    let g = contact_of(&led);
    assert_eq!(g.edges(), 0, "an unreadable day must not become an edge");
    assert!(g.peers_of(eid(1)).is_empty());
    assert!(g.peers_of(eid(5)).is_empty());
}
