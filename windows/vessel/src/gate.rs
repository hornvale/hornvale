//! The body-state gate (spec §3.3): whether a body's own state permits an
//! act, decided against the act's [`Mood`](crate::action::Mood).
//!
//! Built as a table from the first row it ever carried, rather than a check
//! grown into one under pressure. The reason is empirical, not anticipatory:
//! `Session::needs` in this crate already carries an ad-hoc perception gate
//! that had to be patched after a verb walked around it as a side channel,
//! and `Session::purview`'s own doc records a second, still-open version of
//! the same shape. A table with an exhaustive match — no wildcard arm — turns
//! the next omission into a compile error: Arc III's `PossessedByAnother` row
//! already went through it (The Coercion), and the spec's own
//! `unconscious`/`blind`/`target invisible` rows must be classified the same
//! way before they can compile.
//!
//! [`verdict`] is consulted by `Session::refused_by_the_body` (The Deed, Task
//! 7), once for the whole in-character namespace and BEFORE any handler runs,
//! so a refusal charges no time and commits no fact. It stands in front of
//! ACTS, not in front of verb resolution: a token that resolves to no verb
//! never reaches here, or a sleeping body would answer "you are asleep" to
//! nonsense — and, worse, to the retired bare spelling of an operator
//! instrument.

use crate::action::Mood;

/// A body's state, as the gate reads it. `Awake`/`Asleep` shipped first; Arc
/// III (The Coercion) added [`BodyState::PossessedByAnother`], and the spec
/// names `unconscious`, `blind`, and `target invisible` as future rows.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BodyState {
    /// The body can act freely; no in-character act is gated.
    Awake,
    /// The body is asleep; an in-character act is refused.
    Asleep,
    /// The body is held by someone other than the player; an in-character act
    /// is refused. **Relational, unlike every other row** — `Awake` and
    /// `Asleep` are true of the body whoever asks, and this is not. The name
    /// carries the relation because the derivation cannot: the player has no
    /// ledger identity of its own to compare against (spec §3.1), so an open
    /// `possessed-by` fact always means someone else.
    PossessedByAnother,
}

impl BodyState {
    /// Every body state, one representative per variant — mirrors
    /// [`Action::all`](crate::action::Action::all)'s roster discipline, and
    /// is the cross product [`verdict`]'s own tests sweep.
    pub fn all() -> Vec<BodyState> {
        vec![
            BodyState::Awake,
            BodyState::Asleep,
            BodyState::PossessedByAnother,
        ]
    }
}

/// Compile-time tripwire: a new [`BodyState`] variant breaks this match —
/// every variant is named and there is no `_` arm — forcing
/// [`BodyState::all`] and [`verdict`] to be revisited. The same discipline
/// as `action.rs`'s `action_variants_must_all_be_rostered`. Never remove,
/// never add a wildcard arm.
#[allow(dead_code)]
fn body_state_variants_must_all_be_rostered(s: &BodyState) -> &'static str {
    match s {
        BodyState::Awake => "awake",
        BodyState::Asleep => "asleep",
        BodyState::PossessedByAnother => "possessed-by-another",
    }
}

/// Whether the gate lets an act through.
/// type-audit: bare-ok(prose: Refused.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Verdict {
    /// The act may proceed.
    Permitted,
    /// The act is refused, with a player-legible reason (decision 0007: a
    /// refusal fails loudly, never silently).
    Refused(String),
}

/// Whether `state` permits an act of the given `mood`. An exhaustive match
/// over `(state, mood)` with **no wildcard arm**, so a new [`BodyState`] row
/// (or a new [`Mood`]) fails to compile here until it is classified.
///
/// An out-of-character act bypasses the body's state entirely (spec §2.2):
/// it is permitted in every row. An in-character act is subject to the
/// body's state; today only sleep gates it.
pub fn verdict(state: BodyState, mood: Mood) -> Verdict {
    match (state, mood) {
        (BodyState::Awake, Mood::InCharacter) => Verdict::Permitted,
        (BodyState::Asleep, Mood::InCharacter) => Verdict::Refused("you are asleep".to_string()),
        (BodyState::Awake, Mood::OutOfCharacter) => Verdict::Permitted,
        (BodyState::Asleep, Mood::OutOfCharacter) => Verdict::Permitted,
        (BodyState::PossessedByAnother, Mood::InCharacter) => {
            Verdict::Refused("another will holds this body".to_string())
        }
        (BodyState::PossessedByAnother, Mood::OutOfCharacter) => Verdict::Permitted,
    }
}
