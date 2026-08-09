//! Compose the full two-page spread: the plate on the left, the entry on
//! the right, and the endpaper strip beneath both — the bound journal the
//! possessed creature keeps, open to a spread.
//!
//! ## Layout
//!
//! For a `w`-by-`h` grid:
//! - the **plate** (whatever is being drawn — the chart outdoors, the
//!   floor plan indoors) occupies columns `0..PLATE_WIDTH`;
//! - the **entry** (the narration prose, wrapped, plus the command line)
//!   occupies columns `PLATE_WIDTH..w`;
//! - both share rows `0..(h - 3)`, leaving a blank gutter row, the
//!   **endpaper** strip at row `h - 2`, and a blank margin row at `h - 1`.
//!   Rules and gutters carry no ink of their own — "ornament may never
//!   occupy a cell that carries information" — so reserving them is
//!   simply *not drawing there*, never a drawn border.
//!
//! **The gutter row briefly carried a ways-on line (Task 9b).** The Quire
//! (task 9d) removed it — see `entry.rs`'s module doc for why — and this
//! row went back to being blank rather than being claimed by anything else.
//! Do not re-add a ways-on element here without reading that doc first: the
//! sim's own prose already states the same fact, correctly in every band,
//! and the row's job was to reproduce that conclusion a second time.
//!
//! `chart::draw` and `plan::draw` read their own target region's size
//! from the `Grid` they are handed (see those modules' docs), so the
//! plate is composed in a same-sized scratch `Grid` and then copied
//! ([`blit`]) into the page at the right offset, rather than handed the
//! full page and a size it would misread as its own.

use crate::{Grid, Spatial};

/// The column where the entry begins; the plate occupies `0..PLATE_WIDTH`.
pub const PLATE_WIDTH: u16 = 40;

/// Rows reserved below the shared plate/entry region: one blank gutter
/// row, the endpaper's own row, and one blank margin row beneath it.
const RESERVED_ROWS: u16 = 3;

/// Copy every non-blank cell of `src` into `dst`, offset by `origin`.
/// Blank cells are skipped rather than overwriting whatever `dst` already
/// carries there, so drawing order between panes never matters.
fn blit(src: &Grid, dst: &mut Grid, origin: (u16, u16)) {
    for y in 0..src.height() {
        for x in 0..src.width() {
            if let Some(cell) = src.get(x, y)
                && !cell.is_blank()
            {
                dst.set(origin.0 + x, origin.1 + y, *cell);
            }
        }
    }
}

/// Compose `snapshot` into a `w`-by-`h` grid. See the module doc for the
/// column and row layout. The plate dispatches on [`Spatial`]: the
/// walk-band chart outdoors, the chamber-band floor plan indoors — the
/// register switches picture, never prose.
pub fn compose(snapshot: &crate::Snapshot, w: u16, h: u16) -> Grid {
    let mut page = Grid::new(w, h);
    let content_height = h.saturating_sub(RESERVED_ROWS);
    let plate_width = PLATE_WIDTH.min(w);
    let entry_width = w.saturating_sub(plate_width);

    let mut plate = Grid::new(plate_width, content_height);
    match &snapshot.spatial {
        Spatial::Walk { chart } => crate::chart::draw(chart, &mut plate, (0, 0)),
        Spatial::Chamber { plan } => crate::plan::draw(plan, &mut plate, (0, 0)),
    }
    blit(&plate, &mut page, (0, 0));

    crate::entry::draw(
        &snapshot.narration,
        &mut page,
        (plate_width, 0),
        entry_width,
        content_height,
    );

    let endpaper_row = h.saturating_sub(2);
    crate::endpaper::draw(
        &snapshot.me,
        snapshot.day,
        snapshot.turn,
        &mut page,
        (0, endpaper_row),
    );

    page
}

#[cfg(test)]
mod tests {
    use super::*;

    const WALK_FIXTURE: &str = include_str!("../tests/fixtures/session-seed-42-turn-0.json");
    const CHAMBER_FIXTURE: &str = include_str!("../tests/fixtures/session-seed-42-chamber.json");

    #[test]
    fn compose_fills_the_requested_dimensions() {
        let s = crate::Snapshot::parse(WALK_FIXTURE).unwrap();
        let g = compose(&s, 80, 24);
        assert_eq!(g.width(), 80);
        assert_eq!(g.height(), 24);
    }

    #[test]
    fn compose_dispatches_the_plate_by_band() {
        let walk = crate::Snapshot::parse(WALK_FIXTURE).unwrap();
        let chamber = crate::Snapshot::parse(CHAMBER_FIXTURE).unwrap();
        // Both bands must draw *something* onto the plate — the specific
        // glyph vocabulary differs (chart.rs/plan.rs each test their own),
        // this just proves compose() actually calls one of them either
        // way rather than leaving the plate blank for one band.
        for s in [walk, chamber] {
            let g = compose(&s, 80, 24);
            let text = g.to_plain_text();
            let plate_has_ink = text
                .lines()
                .take(21)
                .any(|l| l.chars().take(40).any(|c| c != ' '));
            assert!(plate_has_ink, "the plate must draw for every band");
        }
    }
}
