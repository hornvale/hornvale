//! Keys to actions — the routing table [`action_for`] maps every keypress to
//! an [`Action`]. Most variants (`CaretBy`, `DeleteBack`, `HistoryPrev`/
//! `Next`, `ToggleFocus`, `Zoom`, …) never become a verb line at all; only
//! [`Action::Submit`] sends the buffer's text on to the session.
//!
//! **The client never validates.** Nathan's ruling: the client sends text
//! and renders the reply; `Session::handle` tokenizes and parses, and an
//! invalid move already comes back as the sim's own prose (`"No way n from
//! here."`). So [`Action::Submit`] sends the command buffer's text
//! *unconditionally*, whatever the player typed — no checking against
//! exits, no dimming, no gating, no autocomplete.
//!
//! **This module's containment is convention, not absence — say so plainly.**
//! `hornvale-game-core` genuinely cannot reach exits data: it has no hornvale
//! dependency, so there is no symbol (`scripts/game-no-vessel-dep.sh` asserts
//! that, and it is the campaign's structural claim). This crate is the other
//! one. It depends on `hornvale-vessel`, `Session::ways()` is `pub`, and
//! `driver.rs` already imports `Session` — so the data *is* reachable from
//! here. What holds instead is a designated seam: `driver.rs` is the only
//! module allowed to know `Session` exists, and [`crate::driver::Driver`]
//! hands back nothing but `String`. This module never imports it. That is a
//! weaker guarantee than a missing symbol and is worth naming as such rather
//! than claiming the stronger one.
//!
//! An unmapped key routes to [`Action::None`] and costs no turn: nothing is
//! ever sent to [`crate::driver::Driver::handle`], so no turn advances and
//! nothing is drawn.
//!
//! **A chorded key is an unmapped key, not a letter with a modifier
//! ignored.** The Portolan's original key-to-verb mapping (`verb_for`,
//! since deleted — every live caller now goes through [`action_for`], and
//! its test coverage moved with it) used to match on [`KeyEvent::code`]
//! alone, so `Ctrl-L` (a terminal idiom for "clear/redraw screen" in
//! countless other programs) silently fell through to plain `l`'s mapping
//! and walked the player east, burning a turn on a chord nobody meant as a
//! move. [`action_for`] carries the fix forward: any [`KeyModifiers`]
//! beyond [`KeyModifiers::SHIFT`] (needed to type `Q`, `>`, `<`, and `?` in
//! the first place, and harmless to tolerate if a terminal reports it
//! redundantly alongside an already-shifted char) now yields
//! [`Action::None`] before the key code is even inspected.

use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use hornvale_game_core::Focus;

/// What one key press means, once [`Focus`] is taken into account.
///
/// **The table is TOTAL**: every `KeyCode` maps to exactly one variant in
/// each of the three focus states (Cli, Map, and Walk — Walk is the
/// default), and [`Action::None`] is a destination like any other.
/// H3 is falsified by a key whose destination cannot be predicted from what
/// is on screen, so "unhandled" is not an option this enum offers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Insert this character at the caret — the CLI's answer to almost
    /// every printable key.
    Type(char),
    /// Return focus to the CLI and insert this character. One keypress,
    /// produced only when the map is focused (spec §2).
    FocusAndType(char),
    /// Delete the character before the caret.
    DeleteBack,
    /// Move the text caret by this many characters.
    CaretBy(i16),
    /// Recall the previous line from history.
    HistoryPrev,
    /// Recall the next line from history.
    HistoryNext,
    /// Submit the buffer. On an empty buffer this must cost no turn
    /// (spec §6) — that is the driver's call, not the router's.
    Submit,
    /// Move the map cursor by `(dx, dy)` grid cells.
    CursorBy(i16, i16),
    /// Zoom the map in (`1`) or out (`-1`). **Routed, not implemented** —
    /// zoom itself is The Portolan part II's. The driver accepts and
    /// ignores it; what matters now is that `-` on the map does not fall
    /// through to the buffer and type a `-`.
    Zoom(i8),
    /// Send this movement word (`"north"`, `"up"`, …) to the session as if
    /// submitted. Produced only under [`Focus::Walk`]; executed by the
    /// driver's `apply`, which routes it through the same path `Submit` uses.
    Move(&'static str),
    /// Move focus to the other pane.
    ToggleFocus,
    /// The key does nothing in this focus. Costs no turn, draws
    /// nothing, and is a deliberate destination — `Tab` is the clearest
    /// case (spec §3.3).
    None,
}

/// Map one key press to an [`Action`], given the current [`Focus`].
///
/// **The direction this function enforces:** total in all three focus
/// states. Every key has a defined destination; none falls through unanswered.
///
/// The chord discipline is unchanged from the deleted `verb_for` (see the
/// module doc) and applies before the key code is inspected: only a bare
/// [`KeyEventKind::Press`] with no modifier beyond [`KeyModifiers::SHIFT`]
/// does anything at all, so `Ctrl-L` types nothing just as it used to walk
/// nowhere.
pub fn action_for(key: KeyEvent, focus: Focus) -> Action {
    if key.kind != KeyEventKind::Press {
        return Action::None;
    }
    if !key.modifiers.difference(KeyModifiers::SHIFT).is_empty() {
        return Action::None;
    }
    match focus {
        Focus::Cli => match key.code {
            KeyCode::Char(c) => Action::Type(c),
            KeyCode::Left => Action::CaretBy(-1),
            KeyCode::Right => Action::CaretBy(1),
            KeyCode::Up => Action::HistoryPrev,
            KeyCode::Down => Action::HistoryNext,
            KeyCode::Enter => Action::Submit,
            KeyCode::Backspace => Action::DeleteBack,
            KeyCode::Esc => Action::ToggleFocus,
            _ => Action::None,
        },
        Focus::Map => match key.code {
            KeyCode::Char('-') => Action::Zoom(-1),
            KeyCode::Char('+') | KeyCode::Char('=') => Action::Zoom(1),
            KeyCode::Char(c) => Action::FocusAndType(c),
            KeyCode::Left => Action::CursorBy(-1, 0),
            KeyCode::Right => Action::CursorBy(1, 0),
            KeyCode::Up => Action::CursorBy(0, -1),
            KeyCode::Down => Action::CursorBy(0, 1),
            KeyCode::Esc => Action::ToggleFocus,
            _ => Action::None,
        },
        Focus::Walk => match key.code {
            KeyCode::Up => Action::Move("north"),
            KeyCode::Down => Action::Move("south"),
            KeyCode::Left => Action::Move("west"),
            KeyCode::Right => Action::Move("east"),
            KeyCode::Char('<') => Action::Move("up"),
            KeyCode::Char('>') => Action::Move("down"),
            KeyCode::Esc => Action::ToggleFocus,
            KeyCode::Char(c) => Action::FocusAndType(c),
            _ => Action::None,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// **H1, and the whole point of the campaign.** With the CLI focused,
    /// every printable ASCII character types ITSELF. Not "most keys" and
    /// not a sampled list: the assertion is that the count of printable
    /// characters doing anything other than typing themselves is ZERO.
    ///
    /// Part I's sweep asserted the opposite property against the now-deleted
    /// `verb_for` (a binding count of 27) and is superseded here rather than
    /// deleted: the sweep was always the right test, and it now asserts that
    /// almost every key is text (spec §4).
    #[test]
    fn every_printable_character_types_itself_when_the_cli_is_focused() {
        let mut not_text = Vec::new();
        for b in 0x20u8..=0x7Eu8 {
            let c = b as char;
            let key = KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE);
            match action_for(key, Focus::Cli) {
                Action::Type(got) if got == c => {}
                other => not_text.push((c, format!("{other:?}"))),
            }
        }
        assert!(
            not_text.is_empty(),
            "these printable characters did not type themselves with the CLI \
             focused: {not_text:?}"
        );
    }

    /// The other half of totality: the sweep above says what text does, and
    /// this says every non-printable key has a DEFINED destination too. H3
    /// is falsified by a key whose destination cannot be predicted.
    #[test]
    fn the_named_keys_route_predictably_with_the_cli_focused() {
        let cases = [
            (KeyCode::Left, Action::CaretBy(-1)),
            (KeyCode::Right, Action::CaretBy(1)),
            (KeyCode::Up, Action::HistoryPrev),
            (KeyCode::Down, Action::HistoryNext),
            (KeyCode::Enter, Action::Submit),
            (KeyCode::Backspace, Action::DeleteBack),
            (KeyCode::Esc, Action::ToggleFocus),
            (KeyCode::Tab, Action::None),
        ];
        for (code, want) in cases {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Cli), want, "{code:?}");
        }
    }

    /// With the map focused, a printable character returns focus to the CLI
    /// AND types itself — one keypress, not two (spec §2). The zoom keys
    /// are the deliberate exception, checked separately below.
    #[test]
    fn a_printable_character_bounces_focus_back_to_the_cli_and_types() {
        let mut wrong = Vec::new();
        for b in 0x20u8..=0x7Eu8 {
            let c = b as char;
            if matches!(c, '-' | '+' | '=') {
                continue;
            }
            let key = KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE);
            match action_for(key, Focus::Map) {
                Action::FocusAndType(got) if got == c => {}
                other => wrong.push((c, format!("{other:?}"))),
            }
        }
        assert!(wrong.is_empty(), "did not bounce-and-type: {wrong:?}");
    }

    /// The zoom keys must NOT bounce and must NOT type. This is the
    /// assertion discriminating a routed zoom binding from a character that
    /// merely falls through to the buffer.
    #[test]
    fn the_zoom_keys_zoom_on_the_map_and_type_on_the_cli() {
        let cases = [
            ('-', Action::Zoom(-1)),
            ('+', Action::Zoom(1)),
            ('=', Action::Zoom(1)),
        ];
        for (c, want) in cases {
            let key = KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Map), want, "{c} on the map");
            assert_eq!(
                action_for(key, Focus::Cli),
                Action::Type(c),
                "{c} must be ordinary text on the CLI"
            );
        }
    }

    /// The named keys with the map focused. `Enter` and `Backspace` are
    /// deliberately inert here — not because the buffer itself is hidden
    /// (`entry::draw`'s own doc: the line's TEXT is drawn under either
    /// focus; losing focus only stops reporting the caret's position), but
    /// because both act destructively or irreversibly on a buffer the
    /// player is not currently aimed at, and only the caret's position is
    /// hidden while the map has focus, not the text itself.
    #[test]
    fn the_named_keys_route_predictably_with_the_map_focused() {
        let cases = [
            (KeyCode::Left, Action::CursorBy(-1, 0)),
            (KeyCode::Right, Action::CursorBy(1, 0)),
            (KeyCode::Up, Action::CursorBy(0, -1)),
            (KeyCode::Down, Action::CursorBy(0, 1)),
            (KeyCode::Esc, Action::ToggleFocus),
            (KeyCode::Tab, Action::None),
            (KeyCode::Enter, Action::None),
            (KeyCode::Backspace, Action::None),
        ];
        for (code, want) in cases {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Map), want, "{code:?}");
        }
    }

    /// `Tab` is reserved for completion and bound to NOTHING, in all three
    /// focus states (spec §3.3). Spending it is the mistake this test makes
    /// loud: it fails the moment anyone gives `Tab` a meaning.
    #[test]
    fn tab_is_bound_to_nothing_in_any_focus() {
        let tab = KeyEvent::new(KeyCode::Tab, KeyModifiers::NONE);
        assert_eq!(action_for(tab, Focus::Cli), Action::None);
        assert_eq!(action_for(tab, Focus::Map), Action::None);
        assert_eq!(action_for(tab, Focus::Walk), Action::None);
    }

    /// The chord discipline survives the rewrite: `Ctrl-L` must not type an
    /// `l` any more than it used to walk the player east. Checked in ALL
    /// three focus states, because the routing table is now three tables.
    #[test]
    fn a_chord_is_inert_in_any_focus() {
        let codes = [
            KeyCode::Char('l'),
            KeyCode::Char('c'),
            KeyCode::Enter,
            KeyCode::Left,
        ];
        for code in codes {
            for m in [KeyModifiers::CONTROL, KeyModifiers::ALT] {
                for focus in [Focus::Cli, Focus::Map, Focus::Walk] {
                    assert_eq!(
                        action_for(KeyEvent::new(code, m), focus),
                        Action::None,
                        "{code:?} with {m:?} in {focus:?}"
                    );
                }
            }
        }
    }

    /// A bare SHIFT is not a chord — it is how a capital letter arrives at
    /// all, and `Q` must type a `Q` now rather than releasing.
    #[test]
    fn shift_still_types_a_capital_and_q_no_longer_releases() {
        let q = KeyEvent::new(KeyCode::Char('Q'), KeyModifiers::SHIFT);
        assert_eq!(action_for(q, Focus::Cli), Action::Type('Q'));
    }

    /// A key-release event must never do anything — one physical keystroke
    /// must not type two characters any more than it could cost two turns.
    #[test]
    fn a_release_event_is_inert_in_any_focus() {
        let mut key = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE);
        key.kind = KeyEventKind::Release;
        assert_eq!(action_for(key, Focus::Cli), Action::None);
        assert_eq!(action_for(key, Focus::Map), Action::None);
        assert_eq!(action_for(key, Focus::Walk), Action::None);
    }

    /// The six movement bindings under Walk. `<`/`>` arrive SHIFTed — they are
    /// the only shifted characters that do anything but type.
    #[test]
    fn the_movement_keys_send_move_actions_when_walking() {
        let cases = [
            (
                KeyEvent::new(KeyCode::Up, KeyModifiers::NONE),
                Action::Move("north"),
            ),
            (
                KeyEvent::new(KeyCode::Down, KeyModifiers::NONE),
                Action::Move("south"),
            ),
            (
                KeyEvent::new(KeyCode::Left, KeyModifiers::NONE),
                Action::Move("west"),
            ),
            (
                KeyEvent::new(KeyCode::Right, KeyModifiers::NONE),
                Action::Move("east"),
            ),
            (
                KeyEvent::new(KeyCode::Char('<'), KeyModifiers::SHIFT),
                Action::Move("up"),
            ),
            (
                KeyEvent::new(KeyCode::Char('>'), KeyModifiers::SHIFT),
                Action::Move("down"),
            ),
        ];
        for (key, want) in cases {
            assert_eq!(action_for(key, Focus::Walk), want, "{key:?}");
        }
    }

    /// Every OTHER printable character bounces to the CLI and types itself
    /// under Walk — the same one-keypress convention Map uses, extended to the
    /// third focus. `<`/`>` are the deliberate exceptions (tested above).
    #[test]
    fn a_printable_character_bounces_from_walk_too() {
        let mut wrong = Vec::new();
        for b in 0x20u8..=0x7Eu8 {
            let c = b as char;
            if matches!(c, '<' | '>') {
                continue;
            }
            for m in [KeyModifiers::NONE, KeyModifiers::SHIFT] {
                let key = KeyEvent::new(KeyCode::Char(c), m);
                if !matches!(action_for(key, Focus::Walk), Action::FocusAndType(got) if got == c) {
                    wrong.push((c, format!("{m:?}")));
                }
            }
        }
        assert!(wrong.is_empty(), "did not bounce-and-type: {wrong:?}");
    }

    /// Totality's Walk column: every non-printable key has a defined
    /// destination, and Esc toggles.
    #[test]
    fn the_named_keys_route_predictably_when_walking() {
        let cases = [
            (KeyCode::Esc, Action::ToggleFocus),
            (KeyCode::Tab, Action::None),
            (KeyCode::Enter, Action::None),
            (KeyCode::Backspace, Action::None),
        ];
        for (code, want) in cases {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Walk), want, "{code:?}");
        }
    }

    /// Chords stay inert in the third state too, and a release event never
    /// acts under Walk.
    #[test]
    fn chords_and_releases_stay_inert_when_walking() {
        for code in [KeyCode::Char('l'), KeyCode::Left] {
            for m in [KeyModifiers::CONTROL, KeyModifiers::ALT] {
                assert_eq!(
                    action_for(KeyEvent::new(code, m), Focus::Walk),
                    Action::None,
                    "{code:?} with {m:?}"
                );
            }
        }
        let mut key = KeyEvent::new(KeyCode::Up, KeyModifiers::NONE);
        key.kind = KeyEventKind::Release;
        assert_eq!(action_for(key, Focus::Walk), Action::None);
    }

    /// `SHIFT` combined with `CONTROL` (e.g. a terminal reporting
    /// `Ctrl-Shift-Q`) is still a chord, not a bare shift, and must be
    /// inert — ported from the now-deleted `verb_for` test of the same
    /// property (`shift_plus_control_is_still_unmapped`) so the deletion
    /// does not silently drop the one case `a_chord_is_inert_in_either_
    /// focus` does not itself cover (that test only exercises `CONTROL` and
    /// `ALT` alone, never `SHIFT | CONTROL` together).
    #[test]
    fn shift_plus_control_is_still_inert_in_any_focus() {
        let key = KeyEvent::new(
            KeyCode::Char('Q'),
            KeyModifiers::SHIFT | KeyModifiers::CONTROL,
        );
        assert_eq!(action_for(key, Focus::Cli), Action::None);
        assert_eq!(action_for(key, Focus::Map), Action::None);
    }
}
