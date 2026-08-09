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
//! 2. **Protect the `Ways on:` line specifically** — the prose's own
//!    convention puts the exit list last, so always draw *that* line even
//!    when the body between it and the fold is clipped.
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

use crate::{Cell, Narration, Source, Weight};

/// The glyph the command line opens with — a prompt, not a text box.
const PROMPT_GLYPH: char = '>';

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

/// Draw the entry into `into`: `narration.prose` word-wrapped to `width`
/// columns, filling up to `height - 1` rows from `origin` downward, then a
/// bare `>` prompt on the last of those `height` rows — the command line,
/// read as the next line the character is about to write rather than as a
/// text field to fill in.
///
/// If the wrapped prose is longer than the rows available, the last
/// visible prose row is replaced with [`TRUNCATION_MARKER`] rather than
/// silently dropping the remainder — see the module doc's "Overflow is a
/// decision, not an accident". For every fixture this campaign ships, the
/// prose is short enough that this never triggers.
///
/// The prose body (and the marker, should it appear) is attributed to
/// [`Source::Prose`]; the prompt is attributed to [`Source::WaysOn`] — a
/// structural distinction (a separate draw call, on a separate row), not a
/// read of the prose for meaning. This module never special-cases the
/// prose's own trailing `"Ways on:"` sentence (see the module doc), so that
/// exit list is carried as ordinary [`Source::Prose`] text, same as the rest
/// of the passage.
pub fn draw(
    narration: &Narration,
    into: &mut crate::Grid,
    origin: (u16, u16),
    width: u16,
    height: u16,
) {
    if height == 0 {
        return;
    }
    let prose_rows = height - 1;
    let wrapped = wrap(&narration.prose, width as usize);
    let overflows = wrapped.len() as u16 > prose_rows;
    // When the prose overflows, the last visible row is sacrificed to the
    // marker, so only `prose_rows - 1` rows of real prose are drawn.
    let visible_rows = if overflows {
        prose_rows.saturating_sub(1)
    } else {
        prose_rows
    };
    for (i, line) in wrapped.iter().take(visible_rows as usize).enumerate() {
        write_line(into, origin.0, origin.1 + i as u16, line);
    }
    if overflows {
        let marker_row = origin.1 + visible_rows;
        write_line(into, origin.0, marker_row, TRUNCATION_MARKER);
    }
    let command_row = origin.1 + height - 1;
    into.set(
        origin.0,
        command_row,
        Cell::glyph(PROMPT_GLYPH, Weight::Bold, Source::WaysOn),
    );
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
        draw(&n, &mut g, (0, 0), 10, 3);
        assert_eq!(g.get(0, 2).unwrap().glyph, Some(PROMPT_GLYPH));
        assert_eq!(g.get(0, 0).unwrap().glyph, Some('h'));
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
        draw(&n, &mut g, (0, 0), 40, 10);
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
        draw(&n, &mut g, (0, 0), 40, 10);
        let text = g.to_plain_text();
        assert!(!text.contains("more, not shown"));
        assert!(text.contains("Ways on: NE, NW, S."));
    }
}
