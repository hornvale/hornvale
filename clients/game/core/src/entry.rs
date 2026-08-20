//! The entry: the right page of the spread — the sim's own narration
//! prose, wrapped (never reworded), followed by a command line that reads
//! as the next line being written.
//!
//! ## Overflow is a decision, not an accident
//!
//! A page has finitely many rows; `narration.prose` does not have a
//! bounded length. When the wrapped prose is longer than the space
//! reserved for it, something has to give, and there were two candidate
//! answers:
//!
//! 1. **A continuation marker on the last visible row** — draw as much
//!    prose as fits, then replace the final row with an honest "more
//!    below, not shown" marker ([`TRUNCATION_MARKER`]).
//! 2. **Protect the prose's own trailing exit-list sentence specifically**
//!    — the prose's own convention puts that sentence last (it read
//!    `"Ways on: …"` everywhere when this was written; since The Rhumb it
//!    reads `"No direction here is closed; …"` out of doors and `"Ways on:
//!    …"` indoors, underground, and while submerged — one convention, more
//!    than one wording), so always draw *that* line even when the body
//!    between it and the fold is clipped.
//!
//! This module takes the first option. The second is kinder to a player
//! who only cares about exits, but it requires *knowing* that the last
//! line is special and reaching past the fold to fetch it — which is no
//! longer wrapping, it is reading the prose for meaning and selecting
//! which part of it survives. That is precisely the boundary this crate
//! exists not to cross: `Snapshot`'s own doc says the prose is "carried
//! verbatim... this client never re-derives it," and a renderer that
//! knows "the last sentence is the important one" is one step from a
//! renderer that also knows what the second-to-last sentence means. A
//! future prose convention that moves the exit list, or a species whose
//! narration doesn't end that way, would silently break an
//! exits-protecting renderer in a way nothing here would catch.
//!
//! The marker is the weaker guarantee for the player — the exit list can
//! still be the thing that gets cut — but it never guesses, and the loss
//! is always visible instead of silent. See this module's
//! `overflowing_prose_gets_a_visible_truncation_marker_not_a_silent_drop`
//! test for the regression this decision is pinned against.
//!
//! **This was reconsidered once, and the marker won again.** Task 9b (commit
//! `630d41c0`) tried the second option: a dedicated, always-visible ways-on
//! row (`ways.rs`), re-deriving the exit list from `sensed.room.exits` and
//! the chamber `at`/`of` pair so it would survive truncation here. It shipped
//! a Critical bug — underground, the row disagreed with the prose's own
//! "Ways on:" sentence, because the row read the outdoor locale's exits
//! (unchanged by stepping indoors) while the prose was built by the sim,
//! which knows the band it is in. The Quire (task 9d) removed that row: the
//! prose sentence is the sim's own correct answer in every band, and this
//! client's job is to render it, not to re-derive it a second time from
//! lower-level fields and risk disagreeing with the thing it is supposedly
//! restating. So the truncation trade-off above stands *as originally
//! decided* — the exits sentence can still be the thing that gets cut on an
//! overlong passage — and that is accepted, not an oversight. If a future
//! need makes this cost too high, the fix is a scrollable entry, not a
//! second copy of an exit list this crate has already gotten wrong once by
//! trying to keep one on hand.

use crate::{Cell, CommandLine, Focus, Narration, Source, Weight};

/// The glyph the command line opens with. **This used to be true
/// unqualified as "a prompt, not a text box"** — before Task 2 (The
/// Stylus), the command row carried only this fixed glyph, with nothing
/// editable drawn after it. It is a text box now: [`draw`] renders the
/// live [`CommandLine`] buffer immediately after this glyph and reports the
/// caret's screen position. The constant itself still names only the
/// glyph, not the row's new behaviour.
const PROMPT_GLYPH: char = '>';

/// Columns the prompt and the space after it cost on the command row —
/// `PROMPT_GLYPH` plus one separating space — subtracted from `width` to
/// get the columns available for the buffer's own text.
const PROMPT_COLUMNS: u16 = 2;

/// Drawn on the last visible prose row in place of that row's own text
/// when the wrapped prose is longer than the space available — an honest
/// "there is more, and it is not shown" rather than a silent drop. See
/// the module doc's "Overflow is a decision, not an accident".
const TRUNCATION_MARKER: &str = "\u{2026} more, not shown \u{2026}";

/// Word-wrap `text` into lines no wider than `width` columns.
///
/// Existing newlines in `text` are hard breaks: each source line is
/// wrapped independently, so the prose's own structure — its opening tag,
/// its body, its "Ways on:" sentence — survives exactly, and only an
/// overlong *visual* line is ever split, and only at whitespace between
/// words. No word is dropped, abbreviated, or reordered; this is wrapping,
/// never re-wording. A single word wider than `width` is placed alone on
/// its own line rather than sliced, since slicing mid-word would drop
/// letters — a smaller act of re-wording, but still one.
fn wrap(text: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return Vec::new();
    }
    let mut lines = Vec::new();
    for paragraph in text.split('\n') {
        if paragraph.trim().is_empty() {
            lines.push(String::new());
            continue;
        }
        let mut current = String::new();
        for word in paragraph.split_whitespace() {
            let joined_len = if current.is_empty() {
                word.chars().count()
            } else {
                current.chars().count() + 1 + word.chars().count()
            };
            if joined_len > width && !current.is_empty() {
                lines.push(std::mem::take(&mut current));
            }
            if !current.is_empty() {
                current.push(' ');
            }
            current.push_str(word);
        }
        if !current.is_empty() {
            lines.push(current);
        }
    }
    lines
}

/// Write `line` into `into`, starting at `(x0, y)`, one glyph per column,
/// attributed to [`Source::Prose`] — this draws both the wrapped narration
/// body and [`TRUNCATION_MARKER`], since the marker is an honest signal
/// about the same prose channel, not an invented one. Columns past `into`'s
/// own bounds are silently refused by [`crate::Grid::set`]; this function
/// does not clip `line` itself, relying on that discipline.
fn write_line(into: &mut crate::Grid, x0: u16, y: u16, line: &str) {
    for (i, ch) in line.chars().enumerate() {
        into.set(
            x0 + i as u16,
            y,
            Cell::glyph(ch, Weight::Normal, Source::Prose),
        );
    }
}

/// Draw `line`'s visible window into `into` starting at `(x0, y)`, one
/// glyph per column, attributed to [`Source::Typed`] (the player's own
/// unsent keystrokes — see that variant's doc). Returns the caret's screen
/// column: `x0` plus however many of the visible window's columns precede
/// it, which by construction is always strictly less than `x0 + available`
/// (see the windowing note below), so a caller never has to clamp it again.
///
/// **Windowing.** `line.caret` is a character offset, never a byte offset
/// (matching [`CommandLine::caret`]'s own contract), so it is resolved
/// against `line.text.chars()`, not byte indices. When the whole buffer
/// fits in `available` columns, the window starts at column 0 and the
/// caret sits at its own offset, unscrolled. When it does not, the
/// **simplest rule that keeps the caret on screen** applies: the window's
/// left edge trails the caret by exactly `available - 1` characters, so an
/// overflowing caret always lands on the pane's rightmost visible column
/// rather than running off the edge. There is no left-context lookahead
/// beyond that — the window scrolls exactly as far as the caret has moved
/// past the right edge, never further.
fn write_command_line(
    into: &mut crate::Grid,
    x0: u16,
    y: u16,
    line: CommandLine<'_>,
    available: u16,
) -> u16 {
    let chars: Vec<char> = line.text.chars().collect();
    let caret = line.caret.min(chars.len());
    let avail = available as usize;
    let window_start = caret.saturating_sub(avail.saturating_sub(1));
    for (i, ch) in chars.iter().skip(window_start).take(avail).enumerate() {
        into.set(
            x0 + i as u16,
            y,
            Cell::glyph(*ch, Weight::Normal, Source::Typed),
        );
    }
    x0 + (caret - window_start) as u16
}

/// Draw the entry into `into`: `narration.prose` word-wrapped to `width`
/// columns, filling up to `height - 1` rows from `origin` downward, then
/// the `>` prompt and `line`'s editable buffer on the last of those
/// `height` rows — the command line, read as the next line the character
/// is about to write, now drawn as an actual text box rather than a bare
/// glyph (see [`PROMPT_GLYPH`]'s doc for what changed).
///
/// Returns the caret's screen position — `Some((x, y))` only when `focus`
/// is [`Focus::Cli`], `None` under [`Focus::Map`]. This is the one hardware
/// cursor's location, never ink on the grid (spec §2.1 forbids ornament
/// occupying an informative cell), so a terminal places its own cursor
/// there and this function never draws anything at that position beyond
/// the buffer's own glyphs. `line`'s TEXT is drawn either way — losing
/// focus does not erase what the player typed, only stops reporting where
/// in it they are.
///
/// If the wrapped prose is longer than the rows available, the last
/// visible prose row is replaced with [`TRUNCATION_MARKER`] rather than
/// silently dropping the remainder — see the module doc's "Overflow is a
/// decision, not an accident". For every fixture this campaign ships, the
/// prose is short enough that this never triggers.
///
/// The prose body (and the marker, should it appear) is attributed to
/// [`Source::Prose`]; the prompt is attributed to [`Source::Chrome`] —
/// `PROMPT_GLYPH` is a hardcoded constant, not derived from any
/// `vessel/session/v2` field, so it is UI chrome rather than a datum this
/// client read off the wire. (An earlier draft attributed the prompt to a
/// since-deleted `Source::WaysOn`, on the theory that the command line
/// represents the character's own "ways on" — see [`Source::Chrome`]'s doc
/// for why that was a false provenance claim.) The buffer's own text is
/// attributed to [`Source::Typed`], not `Chrome` and not `Prose` — see that
/// variant's doc for why lumping it into either would be a false
/// provenance claim of the same shape `Chrome`'s doc already warns
/// against. This module never special-cases the prose's own trailing
/// exit-list sentence, under either of its wordings (see the module doc),
/// so that sentence is carried as ordinary [`Source::Prose`] text, same as
/// the rest of the passage.
///
/// `echo`, when `Some`, is the most recently SUBMITTED line (Task 3, The
/// Stylus) — not the live buffer `line` carries — drawn on the row
/// immediately above the command row so the page reads ask-then-answer: the
/// question the player sent, directly above the sim's own reply. That row
/// is RESERVED out of `prose_rows` when `echo` is `Some`, the same
/// never-silently-steal discipline [`TRUNCATION_MARKER`] already uses,
/// rather than being drawn over whatever prose happened to land there.
/// Attributed to [`Source::Echo`] — deliberately not `Typed` (this text has
/// already been sent to `Session::handle`, so `Typed`'s own "not sent yet"
/// reasoning no longer holds) and not `Prose` (it is the player's own line,
/// never derived from any `vessel/session/v2` field — see that variant's
/// doc).
///
/// **A line wider than the pane is clipped with a visible marker, not
/// silently.** Unlike the live buffer (which tracks a caret and scrolls its
/// window — see [`write_command_line`]), the echo is static text with
/// nothing to scroll toward, so there is no caret-tracked window to build
/// for it; the honest analogue of [`TRUNCATION_MARKER`]'s own "say so, don't
/// just drop it" rule is a single `…` in the last column when the line does
/// not fit, rather than a longer marker string that would itself overflow a
/// narrow pane.
#[allow(clippy::too_many_arguments)] // `echo` (Task 3) pushed this to 8; splitting the position/size pair or the two text channels into a struct would hide, not clarify, the ask-then-answer layout this function's own doc explains
pub fn draw(
    narration: &Narration,
    into: &mut crate::Grid,
    origin: (u16, u16),
    width: u16,
    height: u16,
    focus: Focus,
    line: CommandLine<'_>,
    echo: Option<&str>,
) -> Option<(u16, u16)> {
    if height == 0 {
        return None;
    }
    let echo_rows: u16 = if echo.is_some() { 1 } else { 0 };
    let prose_rows = height.saturating_sub(1).saturating_sub(echo_rows);
    let wrapped = wrap(&narration.prose, width as usize);
    let overflows = wrapped.len() as u16 > prose_rows;
    // When the prose overflows, the last visible row is sacrificed to the
    // marker, so only `prose_rows - 1` rows of real prose are drawn.
    let visible_rows = if overflows {
        prose_rows.saturating_sub(1)
    } else {
        prose_rows
    };
    for (i, l) in wrapped.iter().take(visible_rows as usize).enumerate() {
        write_line(into, origin.0, origin.1 + i as u16, l);
    }
    if overflows {
        let marker_row = origin.1 + visible_rows;
        write_line(into, origin.0, marker_row, TRUNCATION_MARKER);
    }
    let command_row = origin.1 + height - 1;
    if let Some(text) = echo {
        let echo_row = command_row.saturating_sub(1);
        let echo_width = width as usize;
        let chars: Vec<char> = text.chars().collect();
        let overflows_echo = chars.len() > echo_width;
        let visible_len = if overflows_echo {
            echo_width.saturating_sub(1)
        } else {
            chars.len()
        };
        for (i, ch) in chars.iter().take(visible_len).enumerate() {
            into.set(
                origin.0 + i as u16,
                echo_row,
                Cell::glyph(*ch, Weight::Normal, Source::Echo),
            );
        }
        if overflows_echo {
            into.set(
                origin.0 + visible_len as u16,
                echo_row,
                Cell::glyph('\u{2026}', Weight::Normal, Source::Echo),
            );
        }
    }
    into.set(
        origin.0,
        command_row,
        Cell::glyph(PROMPT_GLYPH, Weight::Bold, Source::Chrome),
    );
    let available = width.saturating_sub(PROMPT_COLUMNS);
    let caret_col = write_command_line(
        into,
        origin.0 + PROMPT_COLUMNS,
        command_row,
        line,
        available,
    );
    (focus == Focus::Cli).then_some((caret_col, command_row))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wrap_splits_only_at_whitespace() {
        let lines = wrap("one two three four", 9);
        assert_eq!(lines, vec!["one two", "three", "four"]);
    }

    #[test]
    fn wrap_preserves_explicit_newlines_as_hard_breaks() {
        let lines = wrap("first\nsecond", 40);
        assert_eq!(lines, vec!["first", "second"]);
    }

    #[test]
    fn wrap_never_drops_a_word_even_when_it_exceeds_width() {
        let lines = wrap("a supercalifragilisticexpialidocious word", 10);
        assert!(lines.iter().any(|l| l.contains("supercali")));
    }

    #[test]
    fn draw_places_the_prompt_on_the_last_row() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(10, 3);
        draw(
            &n,
            &mut g,
            (0, 0),
            10,
            3,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
        );
        assert_eq!(g.get(0, 2).unwrap().glyph, Some(PROMPT_GLYPH));
        assert_eq!(g.get(0, 0).unwrap().glyph, Some('h'));
    }

    /// The buffer is drawn after the prompt on the command row, and the
    /// caret's screen position is reported for the terminal to place its
    /// own cursor at — never drawn as ink.
    #[test]
    fn the_buffer_is_drawn_after_the_prompt_and_the_caret_is_reported() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 3);
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Cli,
            crate::CommandLine {
                text: "look",
                caret: 4,
            },
            None,
        );
        let row: String = (0..6)
            .map(|x| g.get(x, 2).unwrap().glyph.unwrap_or(' '))
            .collect();
        assert_eq!(row, "> look");
        assert_eq!(
            caret,
            Some((6, 2)),
            "the caret sits one past the last character"
        );
    }

    /// With the map focused the entry pane reports NO cursor position — the
    /// one hardware cursor is over on the plate, and that is how focus is
    /// shown (spec §2.1). The line's TEXT is still drawn: the buffer does
    /// not disappear because the player looked away.
    #[test]
    fn the_caret_is_not_reported_when_the_map_is_focused() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 3);
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Map,
            crate::CommandLine {
                text: "look",
                caret: 4,
            },
            None,
        );
        assert_eq!(caret, None);
        let row: String = (0..6)
            .map(|x| g.get(x, 2).unwrap().glyph.unwrap_or(' '))
            .collect();
        assert_eq!(
            row, "> look",
            "the buffer stays visible while the map has focus"
        );
    }

    /// The caret follows the caret INDEX, not the end of the text.
    #[test]
    fn the_caret_reports_the_index_not_the_end() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 3);
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Cli,
            crate::CommandLine {
                text: "look",
                caret: 1,
            },
            None,
        );
        assert_eq!(caret, Some((3, 2)));
    }

    /// **A line longer than the pane must keep the caret visible.** The
    /// entry pane is 40 columns at the 80x24 floor; a command can be
    /// longer. Whatever windowing you choose, the invariant is that the
    /// reported caret is inside the pane — a caret drawn off-screen is a
    /// cursor the player cannot find.
    #[test]
    fn a_line_wider_than_the_pane_keeps_the_caret_on_screen() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let width = 20u16;
        let mut g = crate::Grid::new(width, 3);
        let long: String = std::iter::repeat_n('a', 60).collect();
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            width,
            3,
            crate::Focus::Cli,
            crate::CommandLine {
                text: &long,
                caret: 60,
            },
            None,
        );
        let (cx, _) = caret.expect("the CLI is focused, so a caret is reported");
        assert!(
            cx < width,
            "caret at column {cx} is outside a {width}-column pane"
        );
    }

    /// The regression this campaign's review found: a 400-word synthetic
    /// prose (modelled on the reviewer's own case) used to be silently
    /// clipped by `.take(prose_rows)` with nothing on the page to say so.
    /// Now the last visible prose row carries [`TRUNCATION_MARKER`]
    /// instead, and the words that would have landed past it are
    /// genuinely gone from the page — this test checks both halves: the
    /// marker is present, and the tail (including a synthetic "Ways on:"
    /// line, mirroring the real prose convention) is not.
    #[test]
    fn overflowing_prose_gets_a_visible_truncation_marker_not_a_silent_drop() {
        let words: Vec<String> = (0..400).map(|i| format!("word{i}")).collect();
        let prose = format!("{}\nWays on: NE, NW, S.", words.join(" "));
        let n = Narration {
            prose,
            nouns: vec![],
        };
        // width 40, height 10 => 9 prose rows, nowhere near enough for
        // 400 words: this must overflow.
        let mut g = crate::Grid::new(40, 10);
        draw(
            &n,
            &mut g,
            (0, 0),
            40,
            10,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
        );
        let text = g.to_plain_text();
        assert!(
            text.contains("more, not shown"),
            "an overflowing entry must carry a visible truncation marker"
        );
        assert!(
            !text.contains("word399"),
            "the tail that does not fit must actually be gone, not just marked"
        );
        assert!(
            !text.contains("Ways on:"),
            "this synthetic case's exit line falls past the fold and is lost \
             — exactly the cost the module doc's decision accepts"
        );
    }

    /// The marker must never appear on prose that fits; otherwise the
    /// truncation signal is meaningless noise on every ordinary turn.
    #[test]
    fn short_prose_never_shows_the_truncation_marker() {
        let n = Narration {
            prose: "Ways on: NE, NW, S.".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(40, 10);
        draw(
            &n,
            &mut g,
            (0, 0),
            40,
            10,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
        );
        let text = g.to_plain_text();
        assert!(!text.contains("more, not shown"));
        assert!(text.contains("Ways on: NE, NW, S."));
    }

    /// The echoed line is drawn immediately above the command row, and the
    /// row it takes is RESERVED out of the prose area (never stolen from
    /// wherever prose would otherwise have landed) — mirroring
    /// `TRUNCATION_MARKER`'s own reservation discipline.
    #[test]
    fn the_echoed_line_is_drawn_immediately_above_the_command_row() {
        let n = Narration {
            prose: "You are here.".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 4);
        draw(
            &n,
            &mut g,
            (0, 0),
            20,
            4,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            Some("look"),
        );
        let echo_row: String = (0..6)
            .map(|x| g.get(x, 2).unwrap().glyph.unwrap_or(' '))
            .collect();
        assert_eq!(
            echo_row, "look  ",
            "the echo sits on the row directly above the command row"
        );
        assert_eq!(
            g.get(0, 3).unwrap().glyph,
            Some(PROMPT_GLYPH),
            "the command row itself is unaffected by the echo above it"
        );
        assert_eq!(g.get(0, 3).unwrap().source, Source::Chrome);
        assert_eq!(g.get(0, 2).unwrap().source, Source::Echo);
    }

    /// A submitted line wider than the pane must clip with a VISIBLE
    /// marker, not silently at the grid edge — review finding on Task 3
    /// (Minor): the echo has no caret to scroll toward the way the live
    /// buffer does, so the honest analogue of `TRUNCATION_MARKER` is a
    /// single `\u{2026}` in the last column rather than a scrolling window.
    #[test]
    fn an_echoed_line_wider_than_the_pane_is_clipped_with_a_visible_marker() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let width = 10u16;
        let mut g = crate::Grid::new(width, 4);
        let long: String = std::iter::repeat_n('a', 20).collect();
        draw(
            &n,
            &mut g,
            (0, 0),
            width,
            4,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            Some(&long),
        );
        let echo_row: String = (0..width)
            .map(|x| g.get(x, 2).unwrap().glyph.unwrap_or(' '))
            .collect();
        assert_eq!(
            echo_row, "aaaaaaaaa\u{2026}",
            "9 columns of the line plus a truncation glyph in the last column"
        );
    }

    /// The marker from the test above must never appear on an echo that
    /// already fits — otherwise every ordinary short command would show a
    /// spurious "truncated" signal.
    #[test]
    fn an_echoed_line_that_fits_shows_no_truncation_marker() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 4);
        draw(
            &n,
            &mut g,
            (0, 0),
            20,
            4,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            Some("look"),
        );
        let text = g.to_plain_text();
        assert!(!text.contains('\u{2026}'));
    }

    /// With no echo yet (a fresh session, nothing submitted), the row above
    /// the command line must be ordinary prose space, not a permanently
    /// reserved blank — the reservation only happens when there is
    /// something to reserve it for.
    #[test]
    fn with_no_echo_the_row_above_the_command_line_is_ordinary_prose_space() {
        let n = Narration {
            prose: "one\ntwo\nthree".to_string(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 4);
        draw(
            &n,
            &mut g,
            (0, 0),
            20,
            4,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
        );
        let text = g.to_plain_text();
        assert!(
            text.contains("three"),
            "with no echo, all three prose rows fit above the command row: {text:?}"
        );
    }
}
