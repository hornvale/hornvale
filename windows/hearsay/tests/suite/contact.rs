//! Spec §5.3: the raid seam, read out of `occ-ended-by` as an undirected
//! edge set stamped with each raid's day.

use crate::common;

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

/// One attacker, two DISTINCT victims, one day: two peers, because the two
/// entries differ in their peer id.
///
/// **This test does not exercise `dedup`, and its earlier name
/// (`a_repeated_raid_pair_is_recorded_once_per_ending`) claimed that it did.**
/// Measured: delete `peers.dedup()` from `contact_of` and this test — and the
/// rest of the crate — stays green, because `(1, 400.0)` and `(2, 400.0)` are
/// never equal and nothing is there to collapse. What it does pin is that
/// same-day raids on different victims are NOT collapsed, which is the
/// over-eager half of the same concern.
/// [`a_mutual_same_day_raid_is_one_peer_entry_each_way`] is the test that
/// reddens when `dedup` is removed.
#[test]
fn two_same_day_raids_by_one_attacker_stay_two_peers() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    raid(&mut led, 1, 5, 400.0);
    raid(&mut led, 2, 5, 400.0);
    let g = contact_of(&led);
    let peers = g.peers_of(eid(5));
    assert_eq!(peers.len(), 2, "two distinct victims, two edges: {peers:?}");
}

/// The case `peers.dedup()` actually guards, and the only test in this crate
/// that reddens when it is deleted (measured, both directions).
///
/// A MUTUAL same-day raid: `1` names `5` as its attacker and `5` names `1` as
/// its own, both stamped day 400. `contact_of` writes both directions for
/// every ending, so `1`'s peers list receives `(5, 400.0)` twice and `5`'s
/// receives `(1, 400.0)` twice — an identical pair, which is exactly what
/// `dedup` exists to collapse. Undeduplicated, the augmented walk would
/// traverse one meeting twice and inflate every reach number the readout
/// reports.
///
/// Unreachable from today's bake and reachable through the generic `&Ledger`
/// `contact_of` takes — the same argument that justifies the self-raid and
/// unreadable-day guards below.
#[test]
fn a_mutual_same_day_raid_is_one_peer_entry_each_way() {
    let mut led = ledger_with(&[(1, None), (5, None)]);
    raid(&mut led, 1, 5, 400.0);
    raid(&mut led, 5, 1, 400.0);
    let g = contact_of(&led);

    assert_eq!(
        g.peers_of(eid(1)),
        &[(eid(5), 400.0)],
        "one meeting, one entry -- undeduplicated this list carries (5, 400.0) twice"
    );
    assert_eq!(
        g.peers_of(eid(5)),
        &[(eid(1), 400.0)],
        "and the same the other way round"
    );
    assert_eq!(
        g.edges(),
        1,
        "the two endings describe ONE meeting, and `edges()` documents that collapse"
    );
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

/// Spec §5.1: the crossing penalty is discounted by how many raid edges lie
/// between the two peoples, so the graph must answer that directly rather
/// than making every caller re-scan it.
#[test]
fn the_graph_counts_edges_between_two_peoples() {
    // 1,2 are human; 5,6 are kobold. Three human<->kobold raids, so the
    // cross-people tally must be 3.
    //
    // The third raid deliberately reverses victim/attacker roles from the
    // first two (kobold victim, human attacker rather than human victim,
    // kobold attacker) -- with human as victim in every raid, `people_of
    // (victim) <= people_of(attacker)` already holds without any sorting
    // (since "human" < "kobold"), so a fixture that never flips the roles
    // cannot tell a canonically-ordered storage key apart from an
    // unsorted-as-given one: both would happen to store under
    // ("human","kobold") regardless. Flipping the third raid's roles is
    // what makes the storage-side ordering load-bearing rather than the
    // accessor's own sort alone (measured -- see Step 5 of the task brief).
    let mut led = ledger_with(&[(1, None), (2, None), (5, None), (6, None)]);
    for (occ, people) in [(1, "human"), (2, "human"), (5, "kobold"), (6, "kobold")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    raid(&mut led, 1, 5, 100.0); // human victim <-> kobold attacker
    raid(&mut led, 2, 6, 200.0); // human victim <-> kobold attacker
    raid(&mut led, 6, 2, 250.0); // kobold victim <-> human attacker (roles reversed)
    let g = contact_of(&led);

    assert_eq!(
        g.edges_between("human", "kobold"),
        3,
        "all three crossings count"
    );
    assert_eq!(
        g.edges_between("kobold", "human"),
        3,
        "the pair is unordered -- (a,b) and (b,a) are one key"
    );
    assert_eq!(
        g.edges_between("human", "elf"),
        0,
        "peoples that never met count zero, and that is the EXPENSIVE case: \
         spec §5.1 divides by (1 + this), so zero means the full penalty"
    );
}

/// A same-people raid must not inflate a cross-people tally. This is the
/// discriminating case: 47.4% of endings name an attacker but only 2.33% name
/// a foreign one, so same-people raids are the overwhelming majority and a
/// tally that counted them would be dominated by noise.
#[test]
fn a_same_people_raid_does_not_count_toward_a_cross_people_pair() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    for (occ, people) in [(1, "human"), (2, "human"), (5, "kobold")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    raid(&mut led, 1, 2, 100.0); // human <-> human
    let g = contact_of(&led);

    assert_eq!(
        g.edges_between("human", "kobold"),
        0,
        "no crossing happened"
    );
    assert_eq!(
        g.edges_between("human", "human"),
        1,
        "the same-people raid is still an edge"
    );
}
