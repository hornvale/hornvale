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
/// §3.2) must all read `OutOfCharacter`. Together with the tests either side
/// of it, this exercises every one of the sixteen rostered variants at least
/// once.
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

/// Group B's out-of-character halves (The Deed, Task 6 for four of them,
/// Task 7's fix round for `!look`/`!knows`) are out of character too,
/// and they are the discriminating half of this file's mood coverage in a way
/// group A is not: a group-A verb has NO in-character spelling, so classifying
/// one is barely a choice. `!map`/`!examine`/`!needs`/`!wait` each sit beside a
/// bare verb of the same name that is in character, which is the whole content
/// of spec §2.1 — mood is a property of the ACTION, and `examine` and
/// `!examine` are different acts rather than one act with two authorities.
///
/// Read from `action::OBJECTIVE_HALVES` rather than re-listed here on purpose:
/// that constant is what `Session::handle_ooc` is written against, so a fifth
/// half added there without a mood lands in this loop automatically. The
/// length is asserted so the loop cannot silently empty.
#[test]
fn every_group_b_objective_half_is_out_of_character() {
    assert_eq!(
        hornvale_vessel::action::OBJECTIVE_HALVES.len(),
        6,
        "all six of spec §3.2's group-B verbs ship an out-of-character half. \
         Four relax a renderer's own gate; `!look`/`!knows` relax nothing and \
         ship because Task 7's BODY gate is a discriminator its renderer is \
         not — see tests/suite/ooc_objective.rs"
    );
    for action in hornvale_vessel::action::OBJECTIVE_HALVES {
        assert_eq!(
            action.mood(),
            Mood::OutOfCharacter,
            "{action:?} is group B's objective half and must be out of character"
        );
    }
}

/// Anti-vacuity for the three above: `Action::all()`'s full roster is
/// partitioned exactly into the three sets those tests enumerate, so none of
/// them can have silently drifted from the real roster (an omitted or
/// duplicated variant would show up as this count disagreeing with
/// `Action::all().len()`).
#[test]
fn every_rostered_action_is_classified() {
    assert_eq!(
        creature_actions().len() + 7 + hornvale_vessel::action::OBJECTIVE_HALVES.len(),
        Action::all().len(),
        "creature_actions() + the 7 group-A instruments + group B's objective \
         halves must cover the whole roster"
    );
}
