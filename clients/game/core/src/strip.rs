//! The look-mode strip: one row beneath the plate, reporting the resolved
//! feature name under the free-roaming cursor.
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
/// second row). Every glyph is attributed to [`Source::Look`] — see that
/// variant's doc for why this is not [`Source::Chrome`].
pub fn draw(text: &str, into: &mut Grid, origin: (u16, u16), width: u16) {
    for (i, ch) in text.chars().take(width as usize).enumerate() {
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
        draw("a cairn", &mut grid, (0, 0), 10);
        assert_eq!(grid.get(0, 0).unwrap().source, Source::Look);
    }
}
