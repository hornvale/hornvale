//! Mood is a property of the action (spec §2.1, §3.1): every rostered
//! action classifies as `InCharacter` or `OutOfCharacter`.
//!
//! **The roster went mixed at The Deed's Task 5.** When this file was
//! written (Task 3) every rostered action was a creature act, so a test
//! shaped "every action reads `InCharacter`" could not distinguish a real
//! match from a stub returning a constant — this doc comment said so
//! explicitly, and predicted the fix: "the day an out-of-character variant
//! arrives, a constant return stops matching the now-mixed expected
//! values." Group A's seven operator instruments are that day. Both tests
//! below are rewritten to check the actual per-variant classification
//! rather than a single uniform expectation, which is what makes them
//! discriminating now: a `mood` collapsed to a bare `Mood::InCharacter`
//! return would fail `every_group_a_instrument_is_out_of_character` on the
//! first iteration.

use hornvale_vessel::action::{Action, Mood};

/// The five creature (GOAP-planned) variants — everything `Action::all()`
/// carried before Task 5. Enumerated directly rather than derived from
/// `Action::all()` by exclusion, so this test does not silently start
/// passing an empty set if a future task ever removes every creature
/// variant (an anti-vacuity floor of the same shape the roster's other
/// reverse audits use).
fn creature_actions() -> Vec<Action> {
    vec![
        Action::MoveTo(hornvale_kernel::RoomAddr {
            face: 0,
            path: Vec::new(),
        }),
        Action::Drink,
        Action::Rest,
        Action::Eat,
        Action::MoveWithin(hornvale_vessel::interior::AnchorId(0)),
    ]
}

#[test]
fn every_creature_action_is_in_character() {
    for action in creature_actions() {
        assert_eq!(
            action.mood(),
            Mood::InCharacter,
            "{action:?} is a creature act and must be in character"
        );
    }
}

/// The complement: group A's seven operator instruments (The Deed, spec
/// §3.2) must all read `OutOfCharacter`. Together with the test above, this
/// exercises every one of the twelve rostered variants at least once.
#[test]
fn every_group_a_instrument_is_out_of_character() {
    let instruments = [
        Action::Why,
        Action::Npcs,
        Action::Help,
        Action::Eyes,
        Action::Whoami,
        Action::Provoke,
        Action::Soothe,
    ];
    for action in instruments {
        assert_eq!(
            action.mood(),
            Mood::OutOfCharacter,
            "{action:?} is an operator instrument and must be out of character"
        );
    }
}

/// Anti-vacuity for the pair above: `Action::all()`'s full roster is
/// partitioned exactly into the two sets those tests enumerate by hand, so
/// neither test can have silently drifted from the real roster (an
/// omitted or duplicated variant here would show up as this count
/// disagreeing with `Action::all().len()`).
#[test]
fn every_rostered_action_is_classified() {
    assert_eq!(
        creature_actions().len() + 7,
        Action::all().len(),
        "creature_actions() + the 7 group-A instruments must cover the whole roster"
    );
}
