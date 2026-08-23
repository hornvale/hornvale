//! The body-state gate's decision table (spec §3.3, The Deed Task 4). Not
//! wired to anything yet — Task 7 consults `verdict` before executing an
//! in-character act. These tests pin the table directly.

use hornvale_vessel::action::Mood;
use hornvale_vessel::gate::{BodyState, Verdict, verdict};

#[test]
fn out_of_character_is_permitted_in_every_body_state() {
    for state in BodyState::all() {
        assert_eq!(
            verdict(state, Mood::OutOfCharacter),
            Verdict::Permitted,
            "{state:?} must not gate an out-of-character act"
        );
    }
}

#[test]
fn in_character_is_refused_while_asleep_and_permitted_awake() {
    assert_eq!(
        verdict(BodyState::Awake, Mood::InCharacter),
        Verdict::Permitted
    );
    match verdict(BodyState::Asleep, Mood::InCharacter) {
        Verdict::Refused(reason) => assert!(
            !reason.is_empty(),
            "a refusal must carry a reason (spec §3.5, decision 0007)"
        ),
        Verdict::Permitted => panic!("a sleeping body must refuse an in-character act"),
    }
}
