//! Mood is a property of the action (spec §2.1, §3.1): every rostered
//! action classifies as `InCharacter` or `OutOfCharacter`, and today's
//! roster is entirely creature acts, so entirely `InCharacter`.

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
