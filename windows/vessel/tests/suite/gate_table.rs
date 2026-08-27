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

/// The Coercion: the row's whole point. The name says `ByAnother` because the
/// state is relational in a way no other row is — `Awake`, `Asleep`, and the
/// spec's future `dead`/`blind` are true of the body regardless of who asks.
/// A flat `Possessed` would refuse the player's own acts on a body they hold.
#[test]
fn in_character_is_refused_while_possessed_by_another() {
    assert!(matches!(
        verdict(BodyState::PossessedByAnother, Mood::InCharacter),
        Verdict::Refused(_)
    ));
    assert_eq!(
        verdict(BodyState::PossessedByAnother, Mood::OutOfCharacter),
        Verdict::Permitted,
        "OOC is what lets you observe your own possession — if this refuses, \
         being possessed is indistinguishable from the game having hung"
    );
}

/// The refusal names the condition and NOT the possessor: the body does not
/// know who holds it (spec §3.4 — the ledger records the imposition, the body
/// has no introspective access to it).
#[test]
fn the_refusal_does_not_name_the_possessor() {
    let Verdict::Refused(reason) = verdict(BodyState::PossessedByAnother, Mood::InCharacter) else {
        panic!("in-character must be refused while possessed by another");
    };
    assert!(!reason.is_empty(), "a refusal fails loudly (decision 0007)");
}
