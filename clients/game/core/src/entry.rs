//! The entry: the right page of the spread — the sim's own narration
//! prose, wrapped (never reworded), followed by a command line that reads
//! as the next line being written.

use crate::{Cell, Narration, Weight};

/// The glyph the command line opens with — a prompt, not a text box.
const PROMPT_GLYPH: char = '>';

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

/// Write `line` into `into`, starting at `(x0, y)`, one glyph per column.
/// Columns past `into`'s own bounds are silently refused by [`crate::Grid::set`];
/// this function does not clip `line` itself, relying on that discipline.
fn write_line(into: &mut crate::Grid, x0: u16, y: u16, line: &str) {
    for (i, ch) in line.chars().enumerate() {
        into.set(x0 + i as u16, y, Cell::glyph(ch, Weight::Normal));
    }
}

/// Draw the entry into `into`: `narration.prose` word-wrapped to `width`
/// columns, filling up to `height - 1` rows from `origin` downward, then a
/// bare `>` prompt on the last of those `height` rows — the command line,
/// read as the next line the character is about to write rather than as a
/// text field to fill in.
///
/// If the wrapped prose runs longer than the rows available, the
/// remainder is not drawn (`Vec::truncate`'s counterpart, `Iterator::take`)
/// rather than bleeding into the command line or the endpaper below it;
/// for every fixture this campaign ships, the prose is short enough that
/// this never triggers.
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
    for (i, line) in wrapped.iter().take(prose_rows as usize).enumerate() {
        write_line(into, origin.0, origin.1 + i as u16, line);
    }
    let command_row = origin.1 + height - 1;
    into.set(
        origin.0,
        command_row,
        Cell::glyph(PROMPT_GLYPH, Weight::Bold),
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
}
