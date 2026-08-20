//! Keys to verb lines.
//!
//! **The client never validates.** Nathan's ruling: the client sends text
//! and renders the reply; `Session::handle` tokenizes and parses, and an
//! invalid move already comes back as the sim's own prose (`"No way n from
//! here."`). So every mapped key emits its verb line *unconditionally* — no
//! checking against exits, no dimming, no gating, no autocomplete.
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
//! An unmapped key yields [`None`] and costs no turn: [`verb_for`] returning
//! [`None`] means nothing is ever sent to [`crate::driver::Driver::handle`],
//! so no turn advances and nothing is drawn.
//!
//! **A chorded key is an unmapped key, not a letter with a modifier ignored.**
//! `verb_for` used to match on [`KeyEvent::code`] alone, so `Ctrl-L` (a
//! terminal idiom for "clear/redraw screen" in countless other programs)
//! silently fell through to plain `l`'s mapping and walked the player east,
//! burning a turn on a chord nobody meant as a move. Any [`KeyModifiers`]
//! beyond [`KeyModifiers::SHIFT`] (needed to type `Q`, `>`, `<`, and `?` in
//! the first place, and harmless to tolerate if a terminal reports it
//! redundantly alongside an already-shifted char) now yields `None` before
//! the key code is even inspected.

use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use hornvale_game_core::Focus;

/// Map one key press to a verb line, or `None` if the key is unmapped.
///
/// Movement: arrows, the vi-keys (`hjkl` plus the diagonals `yubn`), and the
/// numeric keypad (`1`-`9`, keypad-compass layout — `5` is the pad's centre
/// and has no compass reading, so it aliases `.` rather than the brief's
/// literal "1-9 all go to a compass", which has no meaning for the middle
/// key). `>`/`<` enter/leave a structure, `.` waits a turn, `m` draws the
/// map, `?` prints help, and `Q` (capital only — lowercase `q` is
/// deliberately unmapped, so a stray tap of the un-shifted key cannot end a
/// possession) releases.
///
/// **`x` is deliberately unbound**, even though the brief specified `x` →
/// `examine`. Played (see the task report): `examine` with no object always
/// answers "Examine what?" and still costs a turn — worse than an unmapped
/// key, which costs nothing. A noun-entry mode belongs to its own task; this
/// crate must not read `narration.nouns` to guess an object on the player's
/// behalf (the catalogue's only legitimate use is *display*, offering a
/// list — substituting a choice for the player is the same class of
/// overreach as validating a move against exits, which this crate also
/// never does).
///
/// Only [`KeyEventKind::Press`] is mapped: a terminal that reports key
/// release/repeat events (the kitty keyboard protocol, which this client
/// never opts into, or an iTerm2 default) must not be able to double a turn.
///
/// Any modifier chord beyond a bare [`KeyModifiers::SHIFT`] is unmapped —
/// see the module doc for why this matters (`Ctrl-L` must not walk east).
pub fn verb_for(key: KeyEvent) -> Option<String> {
    if key.kind != KeyEventKind::Press {
        return None;
    }
    if !key.modifiers.difference(KeyModifiers::SHIFT).is_empty() {
        return None;
    }
    let verb = match key.code {
        KeyCode::Up | KeyCode::Char('k') | KeyCode::Char('8') => "go n",
        KeyCode::Down | KeyCode::Char('j') | KeyCode::Char('2') => "go s",
        KeyCode::Left | KeyCode::Char('h') | KeyCode::Char('4') => "go w",
        KeyCode::Right | KeyCode::Char('l') | KeyCode::Char('6') => "go e",
        KeyCode::Char('y') | KeyCode::Char('7') => "go nw",
        KeyCode::Char('u') | KeyCode::Char('9') => "go ne",
        KeyCode::Char('b') | KeyCode::Char('1') => "go sw",
        KeyCode::Char('n') | KeyCode::Char('3') => "go se",
        KeyCode::Char('>') => "enter",
        KeyCode::Char('<') => "out",
        KeyCode::Char('.') | KeyCode::Char('5') => "wait",
        KeyCode::Char('m') => "map",
        // Sigilled (The Deed, Task 5): bare `help` is retired in
        // `Session::handle`'s vocabulary, and this client has no free-text
        // entry mode for the player to type `!help` themselves — the key
        // mapping is the only route, so it must track the retirement.
        KeyCode::Char('?') => "!help",
        KeyCode::Char('Q') => "release",
        _ => return None,
    };
    Some(verb.to_string())
}

/// What one key press means, once [`Focus`] is taken into account.
///
/// **The table is TOTAL**: every `KeyCode` maps to exactly one variant in
/// each focus state, and [`Action::None`] is a destination like any other.
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
    /// Move focus to the other pane.
    ToggleFocus,
    /// The key does nothing in this focus state. Costs no turn, draws
    /// nothing, and is a deliberate destination — `Tab` is the clearest
    /// case (spec §3.3).
    None,
}

/// Map one key press to an [`Action`], given the current [`Focus`].
///
/// **The direction this function enforces:** total in both focus states.
/// Every key has a defined destination; none falls through unanswered.
///
/// The chord discipline is unchanged from [`verb_for`] and applies before
/// the key code is inspected: only a bare [`KeyEventKind::Press`] with no
/// modifier beyond [`KeyModifiers::SHIFT`] does anything at all, so
/// `Ctrl-L` types nothing just as it used to walk nowhere.
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyEventState;

    fn press(code: KeyCode) -> KeyEvent {
        press_with(code, KeyModifiers::NONE)
    }

    /// Unlike `press`, this does not hard-code `KeyModifiers::NONE` — the
    /// whole reason `press` alone left this crate's suite unable to see the
    /// `Ctrl-L`-moves-the-player defect (a helper that pins a field makes
    /// the entire suite blind to it).
    fn press_with(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent {
            code,
            modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        }
    }

    /// Every direction the brief names, both as an arrow/vi-key and as its
    /// numpad twin, must agree on the same verb line — the two input
    /// methods are aliases for one compass reading, not two.
    #[test]
    fn arrows_vi_keys_and_numpad_agree_on_direction() {
        let cases = [
            (KeyCode::Up, KeyCode::Char('k'), KeyCode::Char('8'), "go n"),
            (
                KeyCode::Down,
                KeyCode::Char('j'),
                KeyCode::Char('2'),
                "go s",
            ),
            (
                KeyCode::Left,
                KeyCode::Char('h'),
                KeyCode::Char('4'),
                "go w",
            ),
            (
                KeyCode::Right,
                KeyCode::Char('l'),
                KeyCode::Char('6'),
                "go e",
            ),
        ];
        for (arrow, vi, pad, want) in cases {
            assert_eq!(verb_for(press(arrow)).as_deref(), Some(want));
            assert_eq!(verb_for(press(vi)).as_deref(), Some(want));
            assert_eq!(verb_for(press(pad)).as_deref(), Some(want));
        }
    }

    /// The four diagonal vi-keys and their numpad twins.
    #[test]
    fn diagonals_agree() {
        let cases = [
            (KeyCode::Char('y'), KeyCode::Char('7'), "go nw"),
            (KeyCode::Char('u'), KeyCode::Char('9'), "go ne"),
            (KeyCode::Char('b'), KeyCode::Char('1'), "go sw"),
            (KeyCode::Char('n'), KeyCode::Char('3'), "go se"),
        ];
        for (vi, pad, want) in cases {
            assert_eq!(verb_for(press(vi)).as_deref(), Some(want));
            assert_eq!(verb_for(press(pad)).as_deref(), Some(want));
        }
    }

    /// The non-movement verbs, one key each.
    #[test]
    fn the_named_single_key_verbs() {
        let cases = [
            (KeyCode::Char('>'), "enter"),
            (KeyCode::Char('<'), "out"),
            (KeyCode::Char('.'), "wait"),
            (KeyCode::Char('5'), "wait"),
            (KeyCode::Char('m'), "map"),
            (KeyCode::Char('?'), "!help"),
            (KeyCode::Char('Q'), "release"),
        ];
        for (code, want) in cases {
            assert_eq!(verb_for(press(code)).as_deref(), Some(want));
        }
    }

    /// An unmapped key must yield `None` — the caller's contract for "costs
    /// no turn". Lowercase `q` is deliberately included: only capital `Q`
    /// releases. `x` is included because it is deliberately UNBOUND (see the
    /// `verb_for` doc): the brief specified `x` -> `examine`, but a bare
    /// `examine` always costs a turn to be told "Examine what?", which is
    /// worse than doing nothing.
    #[test]
    fn unmapped_keys_yield_none() {
        for code in [
            KeyCode::Char('q'),
            KeyCode::Char('x'),
            KeyCode::Char('z'),
            KeyCode::Char('0'),
            KeyCode::Esc,
            KeyCode::Tab,
        ] {
            assert_eq!(verb_for(press(code)), None);
        }
    }

    /// The regression this fix round exists for: `Ctrl-L` must not walk the
    /// player east just because `l` alone means "go e". Covers every
    /// movement letter/digit plus a few non-movement ones, each under
    /// `CONTROL` and separately under `ALT`, so the suite is no longer
    /// structurally blind to the modifiers field the way `press`'s old
    /// hard-coded `KeyModifiers::NONE` made it.
    #[test]
    fn a_control_or_alt_chord_is_never_mapped_even_for_a_movement_letter() {
        let codes = [
            KeyCode::Char('l'),
            KeyCode::Char('h'),
            KeyCode::Char('j'),
            KeyCode::Char('k'),
            KeyCode::Char('y'),
            KeyCode::Char('u'),
            KeyCode::Char('b'),
            KeyCode::Char('n'),
            KeyCode::Char('m'),
            KeyCode::Char('.'),
            KeyCode::Char('8'),
        ];
        for code in codes {
            assert_eq!(
                verb_for(press_with(code, KeyModifiers::CONTROL)),
                None,
                "Ctrl-{code:?} must be unmapped"
            );
            assert_eq!(
                verb_for(press_with(code, KeyModifiers::ALT)),
                None,
                "Alt-{code:?} must be unmapped"
            );
        }
    }

    /// A bare `SHIFT` modifier alongside an already-shifted character (what
    /// some terminals redundantly report for `Q`, `>`, `<`, `?`) must still
    /// map — tolerating `SHIFT` is not the same as tolerating every chord.
    #[test]
    fn a_bare_shift_modifier_still_maps() {
        assert_eq!(
            verb_for(press_with(KeyCode::Char('Q'), KeyModifiers::SHIFT)).as_deref(),
            Some("release")
        );
    }

    /// `SHIFT` combined with `CONTROL` (e.g. a terminal reporting
    /// `Ctrl-Shift-Q`) is still a chord, not a bare shift, and must be
    /// unmapped.
    #[test]
    fn shift_plus_control_is_still_unmapped() {
        assert_eq!(
            verb_for(press_with(
                KeyCode::Char('Q'),
                KeyModifiers::SHIFT | KeyModifiers::CONTROL
            )),
            None
        );
    }

    /// A key-release event (only ever produced under the kitty keyboard
    /// protocol, which this client never enables) must never yield a verb —
    /// otherwise one physical keystroke could cost two turns.
    #[test]
    fn a_release_event_is_never_mapped() {
        let mut key = press(KeyCode::Up);
        key.kind = KeyEventKind::Release;
        assert_eq!(verb_for(key), None);
    }

    /// **H1, and the whole point of the campaign.** With the CLI focused,
    /// every printable ASCII character types ITSELF. Not "most keys" and
    /// not a sampled list: the assertion is that the count of printable
    /// characters doing anything other than typing themselves is ZERO.
    ///
    /// Part I's sweep asserted the opposite property against `verb_for` (a
    /// binding count of 27) and is superseded here rather than deleted: the
    /// sweep was always the right test, and it now asserts that almost
    /// every key is text (spec §4).
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
    /// deliberately inert here: both act on a buffer whose caret is not
    /// being shown, and a destructive or turn-costing key must not fire
    /// against a surface the player cannot see.
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

    /// `Tab` is reserved for completion and bound to NOTHING, in both focus
    /// states (spec §3.3). Spending it is the mistake this test makes loud:
    /// it fails the moment anyone gives `Tab` a meaning.
    #[test]
    fn tab_is_bound_to_nothing_in_either_focus() {
        let tab = KeyEvent::new(KeyCode::Tab, KeyModifiers::NONE);
        assert_eq!(action_for(tab, Focus::Cli), Action::None);
        assert_eq!(action_for(tab, Focus::Map), Action::None);
    }

    /// The chord discipline survives the rewrite: `Ctrl-L` must not type an
    /// `l` any more than it used to walk the player east. Checked in BOTH
    /// focus states, because the routing table is now two tables.
    #[test]
    fn a_chord_is_inert_in_either_focus() {
        let codes = [
            KeyCode::Char('l'),
            KeyCode::Char('c'),
            KeyCode::Enter,
            KeyCode::Left,
        ];
        for code in codes {
            for m in [KeyModifiers::CONTROL, KeyModifiers::ALT] {
                for focus in [Focus::Cli, Focus::Map] {
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
    fn a_release_event_is_inert_in_either_focus() {
        let mut key = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE);
        key.kind = KeyEventKind::Release;
        assert_eq!(action_for(key, Focus::Cli), Action::None);
        assert_eq!(action_for(key, Focus::Map), Action::None);
    }
}
