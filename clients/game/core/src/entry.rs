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
//! ## The weight channel on this surface
//!
//! On a COMPLETION surface (the hint row, Task 9) [`Weight`] carries
//! typed-vs-suggested, not emphasis: the stem — what the buffer already
//! holds — is bold; each candidate's remainder — what completion is
//! *suggesting* — stays normal. This deliberately diverges from the chart
//! surfaces, where weight keeps its perishable ladder (see `chart.rs`).
//! The two are never co-rendered, so one channel can safely mean different
//! things per surface; see [`Hint`]'s doc for the full statement.
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

/// One pending completion ambiguity, threaded from the driver: the stem
/// the command buffer was extended to and every candidate that shares it,
/// in candidate order. Borrowed rather than owned — the driver holds the
/// strings; this pane only renders them.
///
/// **The weight channel on this surface (spec §4.3, decision 0142).** On a
/// COMPLETION surface weight carries typed-vs-suggested, not emphasis:
/// the stem — what the buffer already holds, i.e. what the player typed —
/// is [`Weight::Bold`]; the remainder of each candidate — what completion
/// is *suggesting* — stays [`Weight::Normal`]. This deliberately diverges
/// from the chart surfaces, where weight keeps its perishable ladder; the
/// two are never co-rendered, so one channel can safely mean different
/// things per surface.
pub struct Hint<'a> {
    /// The longest common prefix of all matches — what the buffer holds.
    pub stem: &'a str,
    /// Every matching name, input order preserved.
    pub matches: &'a [&'a str],
}

/// Columns separating two adjacent candidates on the hint row.
const HINT_JOIN_COLUMNS: &str = "  ";

/// Lay `text` out for a `width`-column prose pane, one returned string per
/// visual row.
///
/// Existing newlines in `text` are hard breaks: each source line is laid
/// out independently, so the prose's own structure — its opening tag, its
/// body, its "Ways on:" sentence — survives exactly. What happens to a
/// source line after that turns on one question, asked per line: does it
/// fit?
///
/// - **A line that fits is preserved VERBATIM** — every run of interior
///   spaces, and every column of leading indent, exactly as the sim sent
///   it. This is the half decision 0291 settles. The sim emits
///   pre-formatted pictures on the very same prose channel it emits
///   narration on (the walk-band chart, the chamber plan), and the wire
///   carries no marker saying which a given line is. This function used to
///   split every line on [`str::split_whitespace`] and rejoin on a single
///   space, which collapsed every run of spaces and left-flushed each
///   chart row into a picture of a different place.
/// - **A line that does NOT fit is word-wrapped, if it reads as prose.**
///   Growing downward is what a prose pane is for; see [`reads_as_prose`]
///   for the test and for what it can and cannot tell apart. No word is
///   dropped, abbreviated, or reordered; this is wrapping, never
///   re-wording. A single word wider than `width` is placed alone on its
///   own line rather than sliced, since slicing mid-word would drop
///   letters — a smaller act of re-wording, but still one.
/// - **A line that does not fit and does not read as prose is CLIPPED,
///   never re-flowed.** Prose may grow downward; a picture may not grow at
///   all, and re-flowing an over-wide picture line reproduces the defect
///   above at a wider band rather than fixing it. Clipped without a
///   marker, deliberately: the pane's own [`TRUNCATION_MARKER`] signals
///   the loss it can honestly signal (whole rows, below), while a `…` in a
///   picture's last column would draw a glyph that picture does not
///   contain, and a reader cannot tell an invented glyph from a real one.
///   The pane widening (`crate::spread`) is what actually recovers those
///   columns.
///
/// Every line is passed through [`strip_sgr`] first, so an escape sequence
/// neither reaches the grid nor counts toward the width.
fn wrap(text: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return Vec::new();
    }
    let mut lines = Vec::new();
    for source in text.split('\n') {
        let paragraph = strip_sgr(source);
        if paragraph.trim().is_empty() {
            lines.push(String::new());
            continue;
        }
        if paragraph.chars().count() <= width {
            lines.push(paragraph);
            continue;
        }
        if !reads_as_prose(&paragraph) {
            lines.push(paragraph.chars().take(width).collect());
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

/// Whether an over-wide `line` may be re-flowed — whether it reads as
/// prose rather than as a picture.
///
/// **The test is a WORD: a run of two or more alphabetic characters.**
/// Prose is made of words; a chart row is made of glyphs held apart by
/// spaces, and every glyph in the sim's chart vocabulary
/// (`@ & # ~ = + _ . : ^ A`) is a single character, as is every glyph in a
/// chamber plan. A caption line — `placement: north-up, one row per ring,
/// …` — is full of words and wraps, which is right: it is prose that
/// happens to arrive beside a picture.
///
/// **This classifies per LINE, not per block, and that is a named limit
/// rather than an oversight** (spec §4.2): the wire carries no marker
/// distinguishing pre-formatted output from narration, so there is nothing
/// else to go on. The residual risk is an over-wide prose line built
/// entirely of one-letter words, which would be clipped instead of
/// wrapped. Nothing in the sim emits one.
fn reads_as_prose(line: &str) -> bool {
    let mut run = 0usize;
    for ch in line.chars() {
        if ch.is_alphabetic() {
            run += 1;
            if run >= 2 {
                return true;
            }
        } else {
            run = 0;
        }
    }
    false
}

/// Drop every ANSI escape sequence from `line`.
///
/// **The prose pane has no colour channel at all.** The grid stores one
/// `char` per column, plus a [`Weight`] and a [`Source`]; there is nowhere
/// for an SGR parameter to go. So the bytes of `\x1b[38;2;r;g;bm` — which the sim's
/// `colour` chart lens emits around every tinted glyph — would land in the
/// grid as literal glyphs, one per column: `[`, `3`, `8`, `;`, and the
/// rest. The client has no use for SGR either way; it applies its own ink
/// from the wire's `color` field.
///
/// Latent rather than hypothetical, and latent is why it survived: seed 42
/// at turn 0 draws a band that is all water, marks and observer, so the
/// lens reports "0 tinted, 31 withheld" and emits nothing. The first
/// tinted chart is the first broken one.
///
/// **Stripped BEFORE the width is measured**, which is the half that is
/// easy to get wrong: a tinted row costs ~19 bytes per drawn glyph, so a
/// picture that fits the pane comfortably would measure several times
/// over-wide and be clipped down to its first few glyphs.
///
/// A `\x1b[` sequence runs to its first final byte (`0x40..=0x7e`), per
/// ECMA-48's CSI form — which covers both halves the lens emits, the
/// truecolour `m` and the `\x1b[0m` reset, and the `\x1b[2m` dim the
/// epistemic channel wraps around them. A bare `ESC` not followed by `[`
/// is dropped alone rather than swallowing the rest of the line: a lone
/// escape is already unrenderable, and eating the text after it would turn
/// a stray byte into a missing sentence.
fn strip_sgr(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\u{1b}' {
            out.push(ch);
            continue;
        }
        if chars.peek() != Some(&'[') {
            continue;
        }
        chars.next();
        for c in chars.by_ref() {
            if ('\u{40}'..='\u{7e}').contains(&c) {
                break;
            }
        }
    }
    out
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

/// Draw one candidate's glyphs starting at `(x0, y)`, stopping at `limit`:
/// the stem bold (typed), the remainder normal (suggested). Returns the
/// next free column.
fn write_candidate(
    into: &mut crate::Grid,
    x0: u16,
    y: u16,
    limit: u16,
    candidate: &str,
    stem_len: usize,
) -> u16 {
    let mut x = x0;
    for (i, ch) in candidate.chars().enumerate() {
        if x >= limit {
            break;
        }
        let weight = if i < stem_len {
            Weight::Bold
        } else {
            Weight::Normal
        };
        into.set(x, y, Cell::glyph(ch, weight, Source::Hint));
        x += 1;
    }
    x
}

/// Draw the completion-hint row at `y`, beneath the command row: as many
/// full candidates as fit (joined with two spaces), then — when any are
/// left over — a plain "… +N more" count. See [`Hint`]'s doc for the
/// weight channel and [`draw`]'s doc for the layout.
fn write_hint_line(into: &mut crate::Grid, x0: u16, y: u16, limit: u16, hint: &Hint<'_>) {
    let join = HINT_JOIN_COLUMNS.chars().count() as u16;
    let stem_len = hint.stem.chars().count();

    // PLAN first, then write. Measure where each full candidate would end
    // (joins included), decide how many fit, and — when any will be left
    // over — reserve room for the count marker by dropping whole trailing
    // candidates rather than clipping it. An honest "… +N more" that lost
    // its digits to the pane edge is not honest.
    let mut ends: Vec<u16> = Vec::with_capacity(hint.matches.len());
    let mut x = x0;
    for candidate in hint.matches {
        let w = candidate.chars().count() as u16;
        if !ends.is_empty() {
            x += join;
        }
        x += w;
        ends.push(x);
    }
    let marker_len =
        |remaining: usize| format!("\u{2026} +{remaining} more").chars().count() as u16;
    let mut drawn = ends.len();
    // Two ways the planned row can overflow the fold: a candidate beyond
    // the last was dropped outright, or the LAST candidate itself ran
    // past `limit` while being written. Either way, drop trailing
    // candidates until what remains — plus an honest "… +N more" count,
    // owed whenever anything was dropped — fits. The marker's width
    // shrinks as candidates are dropped ("+10 more" is wider than
    // "+9 more"), so recompute INSIDE the loop.
    loop {
        let dropped = hint.matches.len() - drawn;
        let marker = if dropped > 0 { marker_len(dropped) } else { 0 };
        let join_after = if drawn > 0 { join } else { 0 };
        let occupied = if drawn > 0 {
            ends[drawn - 1] + join_after + marker
        } else {
            marker
        };
        if drawn == 0 || occupied <= limit {
            break;
        }
        drawn -= 1;
    }

    // WRITE the planned row.
    let mut x = x0;
    for (i, candidate) in hint.matches.iter().take(drawn).enumerate() {
        if i > 0 {
            for ch in HINT_JOIN_COLUMNS.chars() {
                if x < limit {
                    into.set(x, y, Cell::glyph(ch, Weight::Normal, Source::Hint));
                    x += 1;
                }
            }
        }
        x = write_candidate(into, x, y, limit, candidate, stem_len);
    }
    let remaining = hint.matches.len() - drawn;
    if remaining > 0 {
        // Plain count, never bold: it is arithmetic, not a suggested name.
        let marker = format!("\u{2026} +{remaining} more");
        let mut mx = x + if drawn > 0 { join } else { 0 };
        for ch in marker.chars() {
            if mx < limit {
                into.set(mx, y, Cell::glyph(ch, Weight::Normal, Source::Hint));
                mx += 1;
            }
        }
    }
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
///
/// `hint`, when `Some`, is the pending completion ambiguity (Task 9, The
/// Lexicon) drawn on the row immediately BENEATH the command row — see
/// [`Hint`] for the weight channel and [`write_hint_line`] for the fitting
/// rule. Like `echo`'s row, it is RESERVED out of `prose_rows` rather than
/// drawn over whatever sits below (the command row lifts one above the
/// pane's floor while a hint is pending), and like `echo` it is dropped in
/// the degenerate `height < 2` pane.
#[allow(clippy::too_many_arguments)] // `hint` (Task 9) pushed this to 9; see the echo-era note above — the parameters ARE the layout
pub fn draw(
    narration: &Narration,
    into: &mut crate::Grid,
    origin: (u16, u16),
    width: u16,
    height: u16,
    focus: Focus,
    line: CommandLine<'_>,
    echo: Option<&str>,
    hint: Option<&Hint<'_>>,
) -> Option<(u16, u16)> {
    if height == 0 {
        return None;
    }
    let echo_rows: u16 = if echo.is_some() { 1 } else { 0 };
    let hint_rows: u16 = if hint.is_some() && height >= 2 { 1 } else { 0 };
    let prose_rows = height
        .saturating_sub(1)
        .saturating_sub(echo_rows)
        .saturating_sub(hint_rows);
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
    // The command row lifts one above the pane's floor while a hint is
    // pending — the same reserved-row discipline `echo` follows, never
    // silently drawing over whatever the plate or strip put below.
    let command_row = origin.1 + height - 1 - hint_rows;
    // Guard the degenerate `height == 1` case: `echo_row` would be
    // `command_row.saturating_sub(1)`, one row ABOVE `origin`'s pane —
    // writing into whatever the plate or strip drew there. Unreachable
    // in-tree (`compose` always passes `origin.1 = 0` under the 80x24
    // floor), but `draw` is `pub`, and the neighbouring `height == 0` case
    // is already guarded above.
    if let Some(text) = echo.filter(|_| height >= 2) {
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
    if let Some(hint) = hint.filter(|_| height >= 2) {
        write_hint_line(into, origin.0, command_row + 1, origin.0 + width, hint);
    }
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

    /// The reported defect, at the width the pane actually gives a chart.
    /// `wrap` split each line on `split_whitespace()` and rejoined on a
    /// single space, so every run of interior spaces collapsed and each
    /// row left-flushed into a picture of a different place.
    ///
    /// **This input discriminates.** Every line here carries a run of two
    /// or more interior spaces, or leading indent, or both — the exact
    /// thing the old rule destroyed. A picture built from single-space
    /// columns would survive the old code unchanged and prove nothing.
    #[test]
    fn wrap_preserves_a_line_that_fits_verbatim() {
        let picture = "      +\n   + +   +\n+ +   @ +   +";
        assert_eq!(
            wrap(picture, 40),
            vec!["      +", "   + +   +", "+ +   @ +   +"]
        );
    }

    /// Prose may grow downward; a picture may not grow at all. Re-flowing
    /// an over-wide picture line reproduces the defect at a wider band,
    /// which is exactly what a naive "wrap whatever does not fit" rule
    /// does — and what the old rule did to this very input.
    #[test]
    fn wrap_clips_an_over_wide_line_and_never_reflows_it() {
        let wide = "+ + + + + + + + + + + + + + + + + + + +";
        let out = wrap(wide, 10);
        assert_eq!(
            out.len(),
            1,
            "an over-wide line was re-flowed onto {} lines",
            out.len()
        );
        assert_eq!(out[0].chars().count(), 10);
    }

    /// The other direction, and the one a rule that preserved everything
    /// would break: narration is the pane's actual job.
    #[test]
    fn wrap_still_wraps_ordinary_prose() {
        let prose = "The sky above: Night. The vast moon is a smear of light.";
        let out = wrap(prose, 20);
        assert!(out.len() > 1, "ordinary prose stopped wrapping");
        assert!(out.iter().all(|l| l.chars().count() <= 20));
    }

    /// A chart's own caption is prose that arrives beside a picture, and
    /// it must still wrap — the classifier is per line, so the two halves
    /// of one reply are laid out differently on purpose.
    #[test]
    fn wrap_wraps_a_captioned_picture_by_the_line() {
        let reply = "  placement: north-up, one row per ring, east doubled\n   + +   +";
        let out = wrap(reply, 20);
        assert!(out.len() > 2, "the caption line did not wrap, got {out:?}");
        assert_eq!(
            out.last().map(String::as_str),
            Some("   + +   +"),
            "the picture line did not survive verbatim, got {out:?}"
        );
    }

    /// The latent half (spec §4.2). The `colour` lens wraps each tinted
    /// glyph in `\x1b[38;2;r;g;bm … \x1b[0m`; the grid has nowhere to put
    /// an SGR parameter, so those bytes would be drawn as glyphs.
    #[test]
    fn wrap_drops_sgr_before_it_reaches_the_grid() {
        let tinted =
            "\u{1b}[38;2;10;20;30m^\u{1b}[0m \u{1b}[2m\u{1b}[38;2;1;2;3m~\u{1b}[0m\u{1b}[0m";
        assert_eq!(wrap(tinted, 40), vec!["^ ~"]);
    }

    /// And it is dropped BEFORE the width is measured. Escaped, this row
    /// is 41 characters against a 10-column pane; stripped it is 3, so
    /// measuring first would clip a picture that fits down to its opening
    /// escape bytes.
    #[test]
    fn sgr_is_stripped_before_the_width_is_measured() {
        let tinted = "\u{1b}[38;2;10;20;30m^\u{1b}[0m \u{1b}[38;2;1;2;3m~\u{1b}[0m";
        assert!(
            tinted.chars().count() > 10,
            "this test needs an over-wide escaped row"
        );
        assert_eq!(wrap(tinted, 10), vec!["^ ~"]);
    }

    /// A lone `ESC` is dropped alone. Swallowing to end-of-line would turn
    /// one stray byte into a missing sentence.
    #[test]
    fn a_bare_escape_does_not_swallow_the_line() {
        assert_eq!(wrap("a\u{1b}b c", 40), vec!["ab c"]);
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
            None,
        );
        let (cx, _) = caret.expect("the CLI is focused, so a caret is reported");
        assert!(
            cx < width,
            "caret at column {cx} is outside a {width}-column pane"
        );
    }

    /// **A line exactly as wide as the editable pane** (`width -
    /// PROMPT_COLUMNS` characters, caret one past the last) is the boundary
    /// case between the unscrolled and the scrolled branch of
    /// [`write_command_line`]'s windowing rule. Pins it at the exact
    /// column: the caret must land on the pane's last visible column, not
    /// one past it.
    #[test]
    fn a_line_exactly_as_wide_as_the_pane_lands_the_caret_on_its_last_column() {
        let n = Narration {
            prose: "hi".to_string(),
            nouns: vec![],
        };
        let width = 20u16;
        let available = width - PROMPT_COLUMNS;
        let mut g = crate::Grid::new(width, 3);
        let exact: String = std::iter::repeat_n('a', available as usize).collect();
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            width,
            3,
            crate::Focus::Cli,
            crate::CommandLine {
                text: &exact,
                caret: available as usize,
            },
            None,
            None,
        );
        let (cx, _) = caret.expect("the CLI is focused, so a caret is reported");
        assert_eq!(cx, width - 1, "caret should sit on the pane's last column");
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
            None,
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
            None,
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
            None,
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
            None,
        );
        let text = g.to_plain_text();
        assert!(
            text.contains("three"),
            "with no echo, all three prose rows fit above the command row: {text:?}"
        );
    }

    /// A pending ambiguity draws one hint row beneath the command row:
    /// the stem — what the buffer already holds — BOLD in every candidate,
    /// each remainder NORMAL (suggested, never typed). See [`Hint`]'s doc
    /// for the typed-vs-suggested weight channel.
    #[test]
    fn ambiguous_hint_renders_beneath_the_command_row_with_bold_stems() {
        let n = Narration {
            prose: String::new(),
            nouns: vec![],
        };
        let mut g = crate::Grid::new(20, 3);
        draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
            Some(&Hint {
                stem: "lo",
                matches: &["look", "loop"],
            }),
        );
        // The command row lifts to make room: prompt on row 1, hint on 2.
        assert_eq!(g.get(0, 1).unwrap().glyph, Some(PROMPT_GLYPH));
        // "lo" bold, "ok" normal; two-space join; "lo" bold, "op" normal.
        let expect = [
            ('l', Weight::Bold),
            ('o', Weight::Bold),
            ('o', Weight::Normal),
            ('k', Weight::Normal),
            (' ', Weight::Normal),
            (' ', Weight::Normal),
            ('l', Weight::Bold),
            ('o', Weight::Bold),
            ('o', Weight::Normal),
            ('p', Weight::Normal),
        ];
        for (i, (glyph, weight)) in expect.iter().enumerate() {
            let cell = g.get(i as u16, 2).unwrap();
            assert_eq!(cell.glyph, Some(*glyph), "column {i}: glyph");
            assert_eq!(cell.weight, *weight, "column {i}: weight");
            assert_eq!(cell.source, Source::Hint, "column {i}: source");
        }
    }

    /// When the match list is wider than the pane, as many full
    /// candidates as fit are drawn and the rest collapse to a PLAIN
    /// "… +N more" count — arithmetic, not a suggested name, so it never
    /// carries the bold stem.
    #[test]
    fn overlong_hint_collapses_to_fitting_matches_plus_a_count() {
        let n = Narration {
            prose: String::new(),
            nouns: vec![],
        };
        // 22 columns: "look  loop  … +2 more" is 21 columns (10 for the two
        // candidates and join, 2 for the second join, 9 for the marker), so
        // the first two candidates fit beside an honest count — but adding
        // "lord" would end at 16 + 2 + 9 = 27, past the fold.
        let mut g = crate::Grid::new(22, 4);
        draw(
            &n,
            &mut g,
            (0, 0),
            22,
            4,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
            Some(&Hint {
                stem: "lo",
                matches: &["look", "loop", "lord", "long"],
            }),
        );
        let row: String = (0..22)
            .map(|x| g.get(x, 3).unwrap().glyph.unwrap_or(' '))
            .collect();
        let text = row.trim_end();
        assert!(text.starts_with("look  loop"), "got {row:?}");
        assert!(text.ends_with("+2 more"), "got {row:?}");
        assert!(!text.contains("lord") && !text.contains("long"));
        // The count marker is plain throughout — no bold anywhere in it.
        // Char columns, not bytes: the ellipsis is three bytes but one cell.
        let marker_chars = "+2 more".chars().count();
        let total_chars = row.trim_end().chars().count();
        let marker_start = total_chars - marker_chars;
        for x in marker_start..total_chars {
            assert_eq!(
                g.get(x as u16, 3).unwrap().weight,
                Weight::Normal,
                "count marker column {x} must be plain"
            );
        }
    }
}
