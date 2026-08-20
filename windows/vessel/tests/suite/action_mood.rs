//! Mood is a property of the action (spec §2.1, §3.1): every rostered
//! action classifies as `InCharacter` or `OutOfCharacter`, and today's
//! roster is entirely creature acts, so entirely `InCharacter`.
//!
//! **What these tests can and cannot detect.** They pin the *values* of
//! today's classification — every rostered action reads `InCharacter` — and
//! nothing more. Exhaustiveness (a new `Action` variant must be classified
//! before the crate compiles) is enforced by the compiler's match-arm check
//! inside `Action::mood`, not by anything asserted here: while every variant
//! maps to the same mood, a real match and a constant `Mood::InCharacter`
//! return are observationally identical, so neither test below can tell a
//! match from a stub, and neither would catch someone collapsing `mood`'s
//! body to a bare return "for simplicity". A test shaped like these becomes
//! discriminating only once the roster carries both moods — the day an
//! out-of-character variant arrives, a constant return stops matching the
//! now-mixed expected values, and an assertion of this shape starts
//! catching a collapsed match rather than merely pinning a uniform one.

use hornvale_vessel::action::{Action, Mood};

#[test]
fn every_creature_action_is_in_character() {
    for action in Action::all() {
        assert_eq!(
            action.mood(),
            Mood::InCharacter,
            "{action:?} is a creature act and must be in character"
        );
    }
}

#[test]
fn every_rostered_action_is_classified() {
    assert_eq!(
        Action::all()
            .iter()
            .filter(|a| a.mood() == Mood::InCharacter)
            .count(),
        Action::all().len(),
        "every rostered action must carry a mood"
    );
}
