//! The map strip: one row beneath the plate, reporting the resolved feature
//! name under the free-roaming cursor, shown whenever the map is focused.
//!
//! **Plate width, not full width.** The endpaper (`endpaper.rs`) already
//! owns a full-width row and its own job — an identity strip (who/where/
//! when). This strip's job is different (what the cursor is pointing at
//! right now), so it gets its own row rather than sharing the endpaper's,
//! and it is clipped to the plate's own width so it can never bleed into
//! the entry's columns.
//!
//! **Truncated, never wrapped.** A name longer than the plate is cut, not
//! carried onto the next row — wrapping would silently steal a row from
//! whatever is reserved beneath it, which is exactly the "ornament may
//! never occupy a cell that carries information" failure this crate exists
//! to avoid.

use crate::{Cell, Grid, Source, Weight};

/// Draw `text` at `origin`, clipped to `width` columns (never wrapped to a
/// second row), starting at the `offset`-th character of `text` — the
/// window The Portolan part II's Task 4 (F3) scrolls through when `text`
/// is longer than `width`. `offset` is a plain character count, never a
/// clock: what advances it lives entirely in the caller (`bin`'s `Driver`
/// counts its own redraws — see `Driver::strip_offset`'s doc), and this
/// function stays a pure slice-and-draw either way. An `offset` at or past
/// `text`'s own length draws nothing, the same honest emptiness a `width`
/// of `0` already produces — never a wrap back to the start, which would
/// make this function itself carry the "when does it loop" policy its own
/// caller owns.
///
/// Every glyph is attributed to [`Source::Look`] — see that variant's doc
/// for why this is not [`Source::Chrome`].
pub fn draw(text: &str, into: &mut Grid, origin: (u16, u16), width: u16, offset: u16) {
    for (i, ch) in text
        .chars()
        .skip(offset as usize)
        .take(width as usize)
        .enumerate()
    {
        into.set(
            origin.0 + i as u16,
            origin.1,
            Cell::glyph(ch, Weight::Normal, Source::Look),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The strip is PLATE width, not full width — the endpaper keeps its
    /// own full-width row and its own job (an identity strip).
    #[test]
    fn the_strip_never_writes_past_the_plate() {
        let mut grid = Grid::new(80, 24);
        draw(
            &"z".repeat(200),
            &mut grid,
            (0, 20),
            crate::spread::PLATE_WIDTH,
            0,
        );
        for x in crate::spread::PLATE_WIDTH..80 {
            assert!(
                grid.get(x, 20).unwrap().glyph.is_none(),
                "strip wrote into the entry at x={x}"
            );
        }
    }

    /// A strip longer than the plate is TRUNCATED, never wrapped — wrapping
    /// would steal a row from the plate silently.
    #[test]
    fn an_overlong_strip_truncates_rather_than_wrapping() {
        let mut grid = Grid::new(80, 24);
        draw(
            &"z".repeat(200),
            &mut grid,
            (0, 20),
            crate::spread::PLATE_WIDTH,
            0,
        );
        assert!(
            grid.get(0, 21).unwrap().glyph.is_none(),
            "the strip wrapped into the next row"
        );
    }

    /// The strip's content is a resolved, world-derived feature name, not
    /// UI chrome — see [`Source::Chrome`]'s doc for the false-provenance
    /// mistake this pins against in reverse.
    #[test]
    fn drawn_text_is_attributed_to_look_not_chrome() {
        let mut grid = Grid::new(10, 1);
        draw("a cairn", &mut grid, (0, 0), 10, 0);
        assert_eq!(grid.get(0, 0).unwrap().source, Source::Look);
    }

    /// Read a row's drawn glyphs back out as plain text, trimmed of
    /// trailing blanks — the same shape `to_plain_text` gives per-row,
    /// scoped to one row so a scrolling test can compare two draws at the
    /// SAME row without reading the whole grid.
    fn row_text(grid: &Grid, y: u16) -> String {
        (0..grid.width())
            .map(|x| grid.get(x, y).and_then(|c| c.glyph).unwrap_or(' '))
            .collect::<String>()
            .trim_end()
            .to_string()
    }

    /// F3: the strip scrolls without a clock. `offset` alone selects which
    /// window of `text` is drawn — two draws of the SAME overlong text at
    /// two different offsets show two different windows, and the window is
    /// always a literal SLICE of `text`, never a rewrite of it (a
    /// scrambled or ellipsised window would defeat
    /// `long.contains(scrolled.trim_end())` below even though it would
    /// still "look like" scrolling).
    #[test]
    fn a_different_offset_draws_a_different_window_and_never_rewrites_the_text() {
        let long =
            "Vngashngatva (a volcano), on Kxsokxkxzhakx (a landmass), in Zherqvadvoshao (a sea)";
        let mut g = Grid::new(80, 24);
        draw(long, &mut g, (0, 21), 40, 0);
        let head = row_text(&g, 21);
        draw(long, &mut g, (0, 21), 40, 12);
        let scrolled = row_text(&g, 21);
        assert_ne!(
            head, scrolled,
            "a different offset must show a different window on the text"
        );
        assert!(
            long.contains(scrolled.trim_end()),
            "the window is a SLICE of the real text, never a rewrite: {scrolled:?}"
        );
    }

    /// An offset at or past the text's own length draws nothing at that
    /// row — not a panic, and not a wrap back to the start (wrapping is
    /// the CALLER's cycle policy — see `draw`'s own doc — not something
    /// this function decides on its own).
    #[test]
    fn an_offset_past_the_end_draws_nothing_rather_than_wrapping_or_panicking() {
        let mut g = Grid::new(80, 24);
        draw("a cairn", &mut g, (0, 0), 40, 7);
        assert_eq!(
            row_text(&g, 0),
            "",
            "an offset at the text's own length draws nothing"
        );
        draw("a cairn", &mut g, (0, 0), 40, 200);
        assert_eq!(
            row_text(&g, 0),
            "",
            "an offset past the text's own length draws nothing"
        );
    }
}
