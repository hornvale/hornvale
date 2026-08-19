//! The action layer is reachable as its own module, not through `liveness`.

use hornvale_kernel::room::RoomAddr;
use hornvale_vessel::action::{Action, is_movement, plan_to_water};

#[test]
fn the_action_layer_has_its_own_module() {
    let here = RoomAddr {
        face: 0,
        path: vec![0],
    };
    assert!(is_movement(&Action::MoveTo(here.clone())));
    assert!(!is_movement(&Action::Drink));
    // The planner is reachable here too: standing on the water, the plan is
    // the single `Drink` with no move before it.
    let plan = plan_to_water(&here, &here, 64, &std::collections::BTreeSet::new())
        .expect("standing on the water, a plan always exists");
    assert_eq!(plan, vec![Action::Drink]);
}

#[test]
fn every_action_variant_still_carries_a_concept_name() {
    // The Actants' contract survives the move: each variant answers to a
    // concept, and the roster stays exhaustive.
    for a in Action::all() {
        assert!(!a.concept_name().is_empty(), "{a:?} has no concept name");
    }
    assert!(Action::all().len() >= 5);
}
