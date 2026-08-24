//! The command line's editable buffer — the one reversible thing on screen.
//!
//! Everything else the client shows is append-only: the turn counter, the
//! accreted prose, the world itself. A half-typed command can be erased,
//! which is why `Enter` on an empty buffer must cost nothing (spec §6) and
//! why this type is unit-tested in isolation from the terminal.
//!
//! **Indexed by character, never by byte.** The caret is a `char` offset
//! into `chars`, so a multi-byte glyph moves it by one and can never be
//! split. `text()` re-materialises a `String` for rendering.

/// An editable single-line buffer with a caret.
#[derive(Debug, Default, Clone)]
pub struct Line {
    /// The characters typed so far, in order.
    chars: Vec<char>,
    /// The insertion point, as an index into `chars`. Ranges over
    /// `0..=chars.len()`; one past the end is where typing normally happens.
    caret: usize,
}

impl Line {
    /// An empty buffer with the caret at the start.
    pub fn new() -> Line {
        Line::default()
    }

    /// Insert `c` at the caret and advance past it.
    pub fn insert(&mut self, c: char) {
        self.chars.insert(self.caret, c);
        self.caret += 1;
    }

    /// Delete the character before the caret. A no-op at the start — it
    /// never deletes forward, which would be a surprise on the one
    /// reversible surface the client has.
    pub fn backspace(&mut self) {
        if self.caret > 0 {
            self.caret -= 1;
            self.chars.remove(self.caret);
        }
    }

    /// Move the caret one character left, stopping at the start.
    pub fn caret_left(&mut self) {
        self.caret = self.caret.saturating_sub(1);
    }

    /// Move the caret one character right, stopping one past the last
    /// character.
    pub fn caret_right(&mut self) {
        if self.caret < self.chars.len() {
            self.caret += 1;
        }
    }

    /// The buffer's contents.
    pub fn text(&self) -> String {
        self.chars.iter().collect()
    }

    /// The caret's position, as a character offset.
    pub fn caret(&self) -> usize {
        self.caret
    }

    /// Whether the buffer holds nothing.
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    /// Empty the buffer and return what it held, resetting the caret.
    pub fn take(&mut self) -> String {
        self.caret = 0;
        std::mem::take(&mut self.chars).into_iter().collect()
    }

    /// Replace the whitespace-delimited word containing-or-ending-at the
    /// caret with `word`, for tab completion. The scan runs back from the
    /// caret to the first whitespace; everything from that word-start
    /// through the CARET is replaced, so any suffix after the caret belongs
    /// to a later word and is preserved untouched. The caret ends up just
    /// past the inserted word. If the character before the caret is
    /// whitespace (or the buffer is empty) there is no word to complete and
    /// this is a documented no-op.
    pub fn replace_word_at_caret(&mut self, word: &str) {
        if self.caret == 0 || self.chars[self.caret - 1].is_whitespace() {
            return;
        }
        let start = self.chars[..self.caret]
            .iter()
            .rposition(|c| c.is_whitespace())
            .map_or(0, |i| i + 1);
        let replacement: Vec<char> = word.chars().collect();
        self.chars.splice(start..self.caret, replacement);
        self.caret = start + word.chars().count();
    }

    /// Replace the contents, putting the caret at the end — how history
    /// recall lands a remembered line (Task 3).
    pub fn set(&mut self, text: String) {
        self.chars = text.chars().collect();
        self.caret = self.chars.len();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typing_appends_at_the_caret_and_advances_it() {
        let mut l = Line::new();
        for c in "look".chars() {
            l.insert(c);
        }
        assert_eq!(l.text(), "look");
        assert_eq!(l.caret(), 4);
    }

    #[test]
    fn the_caret_moves_and_insertion_follows_it() {
        let mut l = Line::new();
        for c in "lok".chars() {
            l.insert(c);
        }
        l.caret_left();
        l.insert('o');
        assert_eq!(l.text(), "look");
        assert_eq!(l.caret(), 3);
    }

    #[test]
    fn backspace_deletes_before_the_caret_and_is_a_no_op_at_the_start() {
        let mut l = Line::new();
        for c in "ab".chars() {
            l.insert(c);
        }
        l.caret_left();
        l.backspace();
        assert_eq!(l.text(), "b");
        assert_eq!(l.caret(), 0);
        l.backspace();
        assert_eq!(
            l.text(),
            "b",
            "backspace at the start must not delete forward"
        );
        assert_eq!(l.caret(), 0);
    }

    #[test]
    fn the_caret_never_leaves_the_buffer() {
        let mut l = Line::new();
        l.caret_left();
        assert_eq!(l.caret(), 0);
        l.insert('a');
        l.caret_right();
        l.caret_right();
        assert_eq!(l.caret(), 1, "the caret stops one past the last character");
    }

    #[test]
    fn take_empties_the_buffer_and_resets_the_caret() {
        let mut l = Line::new();
        for c in "look".chars() {
            l.insert(c);
        }
        assert_eq!(l.take(), "look");
        assert_eq!(l.text(), "");
        assert_eq!(l.caret(), 0);
        assert!(l.is_empty());
    }

    /// The buffer is indexed by CHARACTER, not by byte. A multi-byte glyph
    /// must not panic the caret arithmetic or split a character in half.
    /// The sim's own prose carries non-ASCII (`entry.rs`'s truncation
    /// marker is `\u{2026}`), so this is reachable, not theoretical.
    #[test]
    fn replace_word_at_caret_swaps_only_the_trailing_token() {
        let mut l = Line::new();
        l.set(String::from("examine vng"));
        l.replace_word_at_caret("Vngashngatva");
        assert_eq!(l.text(), "examine Vngashngatva");
        assert_eq!(l.caret(), l.text().chars().count());
    }

    #[test]
    fn replace_word_at_caret_replaces_a_partial_word_and_preserves_the_suffix() {
        // Caret mid-word: "examine vng" with the caret after "vn". Only the
        // token up to the caret is replaced — the trailing "g" belongs to a
        // later edit, not to this one.
        let mut l = Line::new();
        l.set(String::from("examine vng"));
        for _ in 0..1 {
            l.caret_left();
        }
        l.replace_word_at_caret("Vngash");
        assert_eq!(l.text(), "examine Vngashg");
        assert_eq!(l.caret(), "examine Vngash".chars().count());
    }

    #[test]
    fn replace_word_at_caret_at_whitespace_is_a_documented_noop() {
        let mut l = Line::new();
        l.set(String::from("examine "));
        l.replace_word_at_caret("Vngashngatva");
        assert_eq!(l.text(), "examine ");
        assert_eq!(l.caret(), 8);
    }

    #[test]
    fn replace_word_at_caret_on_empty_buffer_inserts_nothing() {
        let mut l = Line::new();
        l.replace_word_at_caret("Vngashngatva");
        assert!(l.is_empty());
        assert_eq!(l.caret(), 0);
    }

    #[test]
    fn a_multi_byte_character_does_not_break_the_caret() {
        let mut l = Line::new();
        l.insert('é');
        l.insert('x');
        assert_eq!(l.caret(), 2);
        l.caret_left();
        l.backspace();
        assert_eq!(l.text(), "x");
    }
}
