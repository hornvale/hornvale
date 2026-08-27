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
///
/// **This used to assert only `!reason.is_empty()`, which is a different
/// property than the name claims** (The Coercion, final review, M5). The
/// property is now asserted three ways, weakest to strongest: the refusal
/// carries a reason at all (decision 0007); it is the FIXED sentence, so it
/// cannot vary with who holds the body; and it contains neither a digit nor
/// the word "entity", the only two shapes a leaked possessor identity could
/// take here. The structural reason all three hold is that [`verdict`] takes
/// `(BodyState, Mood)` and nothing else — there is no possessor in scope to
/// name — and the exact-string assertion is what keeps that true if the arm
/// is ever rewritten to consult one.
#[test]
fn the_refusal_does_not_name_the_possessor() {
    let Verdict::Refused(reason) = verdict(BodyState::PossessedByAnother, Mood::InCharacter) else {
        panic!("in-character must be refused while possessed by another");
    };
    assert!(!reason.is_empty(), "a refusal fails loudly (decision 0007)");
    assert_eq!(
        reason, "another will holds this body",
        "the refusal is a fixed sentence naming the CONDITION, and must not vary with (or mention) who holds the body"
    );
    assert!(
        !reason.chars().any(|c| c.is_ascii_digit()),
        "an entity id is the only handle a possessor has here, so a digit is how one would leak: {reason:?}"
    );
    assert!(
        !reason.to_ascii_lowercase().contains("entity"),
        "nor may it name the possessor in words: {reason:?}"
    );
}
