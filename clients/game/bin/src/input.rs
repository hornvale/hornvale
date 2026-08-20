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
        KeyCode::Char('?') => "help",
        KeyCode::Char('Q') => "release",
        _ => return None,
    };
    Some(verb.to_string())
}

/// Which thing keys currently drive: the character, or a free-roaming
/// cursor.
///
/// This is the mode this module was missing before The Portolan: the client
/// had exactly one interpretation of a keypress. [`action_for`] is built
/// *around* [`verb_for`] to add a second one without touching the first.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Keys drive the character, via [`verb_for`] — unchanged from before
    /// this campaign.
    Normal,
    /// Keys drive a cursor instead of the character. Entered with `x`,
    /// left with `Esc`.
    Look,
}

/// What one key press means, once a [`Mode`] is taken into account.
///
/// This is [`verb_for`]'s `Option<String>` widened to also express mode
/// transitions and cursor motion, so [`action_for`] can return one type
/// regardless of mode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Send this verb line to the driver — exactly what a `Some(verb)` from
    /// [`verb_for`] used to mean on its own.
    Verb(String),
    /// Move the look-mode cursor by `(dx, dy)` grid units. Never produced in
    /// [`Mode::Normal`].
    CursorBy(i16, i16),
    /// Enter look mode. Only ever produced for `x` in [`Mode::Normal`].
    EnterLook,
    /// Leave look mode. Only ever produced for `Esc` in [`Mode::Look`].
    LeaveLook,
    /// The key has no meaning in the current mode. Like `verb_for`
    /// returning `None`, this costs no turn and draws nothing.
    None,
}

/// Map one key press to an [`Action`], given the current [`Mode`].
///
/// **[`Mode::Normal`]** delegates to [`verb_for`] and wraps its result —
/// `Some(verb)` becomes `Action::Verb(verb)`, `None` becomes `Action::None`
/// — with one exception: `x` (a bare press, no chord) becomes
/// `Action::EnterLook` instead of falling through to `verb_for`, where it is
/// deliberately unbound (see that function's doc). Every other key's meaning
/// is untouched, which is what
/// [`normal_mode_dispatches_exactly_what_verb_for_does`](tests::normal_mode_dispatches_exactly_what_verb_for_does)
/// pins.
///
/// **[`Mode::Look`]** maps the twelve movement keys (arrows, `hjkl`, and the
/// diagonal `yubn`) to `Action::CursorBy`, `Esc` to `Action::LeaveLook`, and
/// **everything else to `Action::None`** — look mode never falls through to
/// a normal-mode verb, so a key with no cursor meaning costs no turn and
/// does not walk the character while the player believes they are looking.
/// The same chord/kind discipline as `verb_for` applies: only a
/// [`KeyEventKind::Press`] with no modifier beyond [`KeyModifiers::SHIFT`]
/// is mapped.
pub fn action_for(key: KeyEvent, mode: Mode) -> Action {
    match mode {
        Mode::Normal => {
            let is_bare_press = key.kind == KeyEventKind::Press
                && key.modifiers.difference(KeyModifiers::SHIFT).is_empty();
            if is_bare_press && key.code == KeyCode::Char('x') {
                return Action::EnterLook;
            }
            match verb_for(key) {
                Some(verb) => Action::Verb(verb),
                None => Action::None,
            }
        }
        Mode::Look => {
            if key.kind != KeyEventKind::Press {
                return Action::None;
            }
            if !key.modifiers.difference(KeyModifiers::SHIFT).is_empty() {
                return Action::None;
            }
            match key.code {
                KeyCode::Esc => Action::LeaveLook,
                KeyCode::Up | KeyCode::Char('k') => Action::CursorBy(0, -1),
                KeyCode::Down | KeyCode::Char('j') => Action::CursorBy(0, 1),
                KeyCode::Left | KeyCode::Char('h') => Action::CursorBy(-1, 0),
                KeyCode::Right | KeyCode::Char('l') => Action::CursorBy(1, 0),
                KeyCode::Char('y') => Action::CursorBy(-1, -1),
                KeyCode::Char('u') => Action::CursorBy(1, -1),
                KeyCode::Char('b') => Action::CursorBy(-1, 1),
                KeyCode::Char('n') => Action::CursorBy(1, 1),
                _ => Action::None,
            }
        }
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
            (KeyCode::Char('?'), "help"),
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

    /// Normal mode is UNCHANGED. Every binding that worked before this
    /// campaign must still work identically — the mode is added around
    /// `verb_for`, never inside it, so this is checking that the wrapper is
    /// transparent rather than that the map is correct.
    #[test]
    fn normal_mode_dispatches_exactly_what_verb_for_does() {
        for code in [
            KeyCode::Up,
            KeyCode::Down,
            KeyCode::Left,
            KeyCode::Right,
            KeyCode::Char('h'),
            KeyCode::Char('j'),
            KeyCode::Char('k'),
            KeyCode::Char('l'),
            KeyCode::Char('y'),
            KeyCode::Char('u'),
            KeyCode::Char('b'),
            KeyCode::Char('n'),
            KeyCode::Char('.'),
            KeyCode::Char('<'),
            KeyCode::Char('>'),
            KeyCode::Char('m'),
            KeyCode::Char('?'),
            KeyCode::Char('1'),
            KeyCode::Char('5'),
            KeyCode::Char('9'),
        ] {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            match (verb_for(key), action_for(key, Mode::Normal)) {
                (Some(v), Action::Verb(a)) => assert_eq!(v, a, "{code:?} changed meaning"),
                (None, Action::None) => {}
                (v, a) => panic!("{code:?}: verb_for gave {v:?} but action_for gave {a:?}"),
            }
        }
    }

    /// `x` enters look mode. Chosen because it is FREE — the taken set is
    /// `? . < > 1-9 b h j k l m n Q u y`, checked, and `x` is the roguelike
    /// convention for exactly this.
    #[test]
    fn x_enters_look_mode_and_escape_leaves_it() {
        let x = KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE);
        assert!(matches!(action_for(x, Mode::Normal), Action::EnterLook));
        let esc = KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE);
        assert!(matches!(action_for(esc, Mode::Look), Action::LeaveLook));
    }

    /// In look mode the SAME movement keys drive the cursor instead of the
    /// character. This is the collision the mode exists to resolve, so it is
    /// asserted for every direction rather than sampled.
    #[test]
    fn look_mode_moves_the_cursor_not_the_character() {
        for (code, dx, dy) in [
            (KeyCode::Char('h'), -1i16, 0i16),
            (KeyCode::Char('l'), 1, 0),
            (KeyCode::Char('k'), 0, -1),
            (KeyCode::Char('j'), 0, 1),
            (KeyCode::Char('y'), -1, -1),
            (KeyCode::Char('u'), 1, -1),
            (KeyCode::Char('b'), -1, 1),
            (KeyCode::Char('n'), 1, 1),
            (KeyCode::Left, -1, 0),
            (KeyCode::Right, 1, 0),
            (KeyCode::Up, 0, -1),
            (KeyCode::Down, 0, 1),
        ] {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            match action_for(key, Mode::Look) {
                Action::CursorBy(gx, gy) => assert_eq!((gx, gy), (dx, dy), "{code:?}"),
                other => panic!("{code:?} in look mode gave {other:?}, wanted CursorBy"),
            }
            // The same key in normal mode must still move the CHARACTER.
            assert!(
                matches!(action_for(key, Mode::Normal), Action::Verb(_)),
                "{code:?} lost its normal-mode meaning"
            );
        }
    }

    /// A key with no meaning in look mode does NOT fall through to its
    /// normal-mode verb — that would walk the character while the player
    /// believes they are looking.
    #[test]
    fn look_mode_does_not_fall_through_to_movement_verbs() {
        let enter_room = KeyEvent::new(KeyCode::Char('>'), KeyModifiers::NONE);
        assert!(
            !matches!(action_for(enter_room, Mode::Look), Action::Verb(_)),
            "look mode leaked a verb"
        );
    }
}
