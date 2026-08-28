//! The Coercion, spec §7's four preregistered hypotheses (decision 0016),
//! frozen before this task's code existed — a falsified one is a finding,
//! not a failure (see the task-5 report for the full account).
//!
//! **Only H1 lives here.** Its instrument needs nothing but public API
//! (`hornvale_vessel::action::Mood`, `hornvale_vessel::gate::{BodyState,
//! Verdict, verdict}` — the same imports `gate_table.rs` already uses at its
//! own lines 5-6). The other three do not fit this file:
//!
//! - H2 needs [`IN_CHARACTER_VERBS`](hornvale_vessel), which is **not**
//!   `pub` (`session.rs:102`, an 18-element array) — it lives in-module in
//!   `session.rs`'s own `#[cfg(test)] mod tests` instead.
//! - H3 and H4 both need `DriveMovements::step_one_with_controller`, which
//!   is `pub(crate)` — both live in-module in `liveness.rs`'s own test
//!   module, beside the existing controller-comparison test their shape
//!   follows (`a_default_controller_passes_through_and_a_player_controller_holds`).
//!
//! This is the fourth time in this campaign a hypothesis or a test has had
//! to move in-module for exactly this reason (Tasks 1, 3, and 4 all reached
//! the same resolution for their own possession-facts coverage).

use hornvale_vessel::action::Mood;
use hornvale_vessel::gate::{BodyState, Verdict, verdict};

/// H1 (spec §7): over the full `BodyState` x `Mood` cross product, `verdict`
/// returns `Refused` for exactly the two rows that were always going to be
/// refused in-character (the pre-existing `Asleep` row and this campaign's
/// own new `PossessedByAnother` row) and `Permitted` for every other pair.
///
/// **This test does two jobs, and losing either is a real regression.** The
/// obvious one is H1 itself: the gate's cross product. The second, load-
/// bearing one is a ratchet on [`BodyState::all`] — Task 2 found that
/// `all()` (`gate.rs:46`) is a hand-written `vec![]` with no compiler link
/// to the enum, so `verdict`'s exhaustive match catches an ADDED variant
/// (fails to compile until classified) while nothing catches `all()`
/// OMITTING one — mutation-proved twice, the pre-existing cross-product
/// sweep in `gate_table.rs` stayed GREEN when a variant was silently
/// dropped from `all()`. This test's COUNT-OVER-`all()` shape closes that
/// gap by accident of its own structure: an omission shrinks `all()` by one
/// row, so the cross product this test sums over shrinks too, and the
/// exact-count assertion reddens.
///
/// **Do not "simplify" this into three direct `assert!(matches!(verdict(...),
/// Verdict::Refused(_)))` lines.** That reads like a harmless clarification
/// and silently deletes the only check on `all()`'s own completeness — the
/// exact shape decision 0261 names, where the cheapest-looking repair
/// deletes the detector.
///
/// Mutation-proved both directions (pasted output in the task-5 report):
/// flipping the new `(PossessedByAnother, InCharacter)` arm's verdict from
/// `Refused` to `Permitted` reddens this test (`refused` drops to 1); and,
/// separately, dropping `BodyState::PossessedByAnother` out of `all()`'s
/// `vec![]` (leaving `verdict` itself untouched) also reddens this test
/// (`refused` drops to 1, since the row `all()` no longer visits can no
/// longer be counted).
#[test]
fn h1_the_new_row_refuses_in_character_and_permits_out_of_character() {
    // Frozen before the code: exactly two (state, mood) pairs are Refused —
    // the pre-existing Asleep+InCharacter row, and this campaign's own new
    // PossessedByAnother+InCharacter row.
    // (Variable names spelled out — `state`/`mood`, not the spec's own
    // shorthand `s`/`m` — because a bare `s` reads as a seed binding to
    // `cli/tests/suite/claim_shape.rs`'s default-deny seed-loop scan
    // (decision 0093), which would otherwise demand a `claim:` tag this
    // loop has no seed to hold one about.)
    let refused = BodyState::all()
        .into_iter()
        .flat_map(|state| [Mood::InCharacter, Mood::OutOfCharacter].map(move |mood| (state, mood)))
        .filter(|(state, mood)| matches!(verdict(*state, *mood), Verdict::Refused(_)))
        .count();
    assert_eq!(refused, 2, "asleep+IC and possessed+IC, and nothing else");
}
