//! Maximum antichain over a witness set.

mod common;

use common::{eid, ledger_with};
use hornvale_hearsay::divergence::maximum_antichain;
use hornvale_hearsay::lineage::lineage_of;

#[test]
fn the_motivating_scenario_returns_the_survivors_not_the_village() {
    // A is raided; B and C are its survivors. Campaign 1's minimal-elements
    // rule returned {A} and scored this scenario ZERO. The maximum antichain
    // is {B, C} — the pair the scenario is about.
    let led = ledger_with(&[(2, Some(1)), (3, Some(1))]);
    let lin = lineage_of(&led);
    let ws = vec![eid(1), eid(2), eid(3)];
    assert_eq!(maximum_antichain(&lin, &ws), vec![eid(2), eid(3)]);
}

#[test]
fn a_chain_of_witnesses_has_an_antichain_of_one() {
    let led = ledger_with(&[(2, Some(1)), (3, Some(2))]);
    let lin = lineage_of(&led);
    let ws = vec![eid(1), eid(2), eid(3)];
    assert_eq!(maximum_antichain(&lin, &ws), vec![eid(3)]);
}

#[test]
fn witnesses_in_unrelated_lineages_are_all_incomparable() {
    let led = ledger_with(&[(1, None), (2, None), (3, None)]);
    let lin = lineage_of(&led);
    let ws = vec![eid(1), eid(2), eid(3)];
    assert_eq!(maximum_antichain(&lin, &ws), vec![eid(1), eid(2), eid(3)]);
}

#[test]
fn an_empty_witness_set_has_an_empty_antichain() {
    let led = ledger_with(&[(2, Some(1))]);
    let lin = lineage_of(&led);
    assert_eq!(maximum_antichain(&lin, &[]), Vec::new());
}
