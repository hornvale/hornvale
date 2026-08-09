//! Keys to verb lines.
//!
//! **The client never validates.** Nathan's ruling: the client sends text
//! and renders the reply; `Session::handle` tokenizes and parses, and an
//! invalid move already comes back as the sim's own prose (`"No way n from
//! here."`). So every mapped key emits its verb line *unconditionally* — no
//! checking against exits, no dimming, no gating, no autocomplete. There is
//! deliberately no exits data available in this crate to consult even if it
//! wanted to.
//!
//! An unmapped key yields [`None`] and costs no turn: [`verb_for`] returning
//! [`None`] means nothing is ever sent to [`crate::driver::Driver::handle`],
//! so no turn advances and nothing is drawn.

use crossterm::event::{KeyCode, KeyEvent, KeyEventKind};

/// Map one key press to a verb line, or `None` if the key is unmapped.
///
/// Movement: arrows, the vi-keys (`hjkl` plus the diagonals `yubn`), and the
/// numeric keypad (`1`-`9`, keypad-compass layout — `5` is the pad's centre
/// and has no compass reading, so it aliases `.` rather than the brief's
/// literal "1-9 all go to a compass", which has no meaning for the middle
/// key). `>`/`<` enter/leave a structure, `.` waits a turn, `x` examines,
/// `m` draws the map, `?` prints help, and `Q` (capital only — lowercase `q`
/// is deliberately unmapped, so a stray tap of the un-shifted key cannot end
/// a possession) releases.
///
/// Only [`KeyEventKind::Press`] is mapped: a terminal that reports key
/// release/repeat events (the kitty keyboard protocol, which this client
/// never opts into, or an iTerm2 default) must not be able to double a turn.
pub fn verb_for(key: KeyEvent) -> Option<String> {
    if key.kind != KeyEventKind::Press {
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
        KeyCode::Char('x') => "examine",
        KeyCode::Char('m') => "map",
        KeyCode::Char('?') => "help",
        KeyCode::Char('Q') => "release",
        _ => return None,
    };
    Some(verb.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyEventState, KeyModifiers};

    fn press(code: KeyCode) -> KeyEvent {
        KeyEvent {
            code,
            modifiers: KeyModifiers::NONE,
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
            (KeyCode::Char('x'), "examine"),
            (KeyCode::Char('m'), "map"),
            (KeyCode::Char('?'), "help"),
            (KeyCode::Char('Q'), "release"),
        ];
        for (code, want) in cases {
            assert_eq!(verb_for(press(code)).as_deref(), Some(want));
        }
    }

    /// An unmapped key must yield `None` — the caller's contract for "costs
    /// no turn". Lowercase `q` is deliberately included: only capital `Q`
    /// releases.
    #[test]
    fn unmapped_keys_yield_none() {
        for code in [
            KeyCode::Char('q'),
            KeyCode::Char('z'),
            KeyCode::Char('0'),
            KeyCode::Esc,
            KeyCode::Tab,
        ] {
            assert_eq!(verb_for(press(code)), None);
        }
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
}
