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
//! - both share rows `0..(h - 4)`, leaving the **map strip** (plate
//!   width only, see `strip.rs`) at row `h - 4`, a blank gutter row at
//!   `h - 3`, the **endpaper** strip at row `h - 2`, and a blank margin row
//!   at `h - 1`. Rules and gutters carry no ink of their own — "ornament may
//!   never occupy a cell that carries information" — so reserving them is
//!   simply *not drawing there*, never a drawn border.
//!
//! **The map strip's row is reserved unconditionally**, whether or not
//! [`compose`] is given anything to put there — a player toggling focus
//! onto and off the map must never see the plate resize under them, so the
//! row's presence cannot be conditional on the strip actually having text.
//! This is the row The Portolan costs the plate: it held content up through
//! Task 1, and Task 2 claims it (see `lib.rs`'s `render_with` doc for the
//! before/after count at 80×24).
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

/// Rows reserved below the shared plate/entry region: the map strip's own
/// row, one blank gutter row, the endpaper's own row, and one blank margin
/// row beneath it.
const RESERVED_ROWS: u16 = 4;

/// The plate's content height for a `w`-by-`h` grid — `h` minus
/// [`RESERVED_ROWS`]. `pub` because the plate's real height is NOT the
/// 80x24 floor's fixed 20 rows once a terminal is taller than the floor
/// (`main.rs`'s `h = rows.max(MIN_HEIGHT)` is a lower bound only, no
/// upper), and a caller outside this crate (`bin`'s cursor resolver) needs
/// the SAME number [`compose`] itself draws into, not a second copy of this
/// one-line subtraction. Two callers computing "the plate's content
/// height" independently is exactly the shape that let a fixed-height
/// assumption silently name the wrong cell at any non-floor terminal size.
pub const fn content_height(h: u16) -> u16 {
    h.saturating_sub(RESERVED_ROWS)
}

/// How many plate COLUMNS a Mercator character spans per ROW. A terminal
/// glyph reads roughly twice as tall as it is wide, so a projection that is
/// square in its own coordinate space (The Portolan part II spec §4.1) needs
/// twice as many columns as rows to read as square on screen. Matches
/// `windows/worldgen/examples/portolan_spike.rs`'s own `GLYPH_ASPECT`
/// (`2.0`, deleted at this campaign's close -- git history at
/// `0292de87f^`) — `pub` so `bin` derives the plate's HEIGHT from this
/// SAME ratio
/// (`world_plate_width(w, h) / GLYPH_ASPECT`) rather than hardcoding a
/// second `2`, the same one-source-of-truth reason [`world_plate_width`]
/// itself is exposed rather than recomputed.
pub const GLYPH_ASPECT: u16 = 2;

/// The world plate's width while the map is focused, for a `w`-by-`h`
/// terminal: the largest Mercator (aspect [`GLYPH_ASPECT`]:1) that fits
/// BOTH the terminal's width and [`content_height`], never stretched past
/// either — the smaller of `w` itself and `GLYPH_ASPECT * content_height(h)`.
/// When the height bound is smaller, the plate is narrower than the full
/// terminal and the entry pane keeps the remainder; when the width bound is
/// smaller, the plate claims every column and the resulting Mercator is
/// shorter than `content_height(h)` — letterboxed, not stretched (see
/// `bin`'s `Driver::world_plate`, which derives the matching height from
/// this same number rather than a second copy of the fit).
///
/// `pub` for the same reason [`content_height`] is: [`compose`] uses this
/// number to size the plate region it draws into, and `bin`'s driver needs
/// the SAME number to size the [`Grid`] it hands back — not a second,
/// possibly-diverging copy of this fit. [`content_height`]'s own doc
/// records why: two callers computing "the plate's content height"
/// independently is exactly the shape that let a fixed-height assumption
/// silently name the wrong cell at any non-floor terminal size. A second
/// copy of the width formula reproduces that defect on the other axis.
pub const fn world_plate_width(w: u16, h: u16) -> u16 {
    let by_height = content_height(h).saturating_mul(GLYPH_ASPECT);
    if by_height <= w { by_height } else { w }
}

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

/// Compose `snapshot` into a `w`-by-`h` grid, plus the caret's screen
/// position (see [`crate::entry::draw`]), `Some` only when `focus` is
/// [`crate::Focus::Cli`]. See the module doc for the column and row
/// layout. The plate dispatches on [`Spatial`]: the walk-band chart
/// outdoors, the chamber-band floor plan indoors — the register switches
/// picture, never prose. `strip` is the map strip's text (see `strip.rs`),
/// `None` unless the map is focused — either way the row beneath the plate
/// is reserved (see the module doc). `strip_offset` is the character
/// offset [`crate::strip::draw`] starts at (F3 — see that function's own
/// doc for why this is never a clock); it is unused when `strip` is
/// `None`, so a caller with nothing to scroll may pass `0`. `line` is
/// the command line's contents, drawn into the entry pane regardless of
/// `focus` — only whether its caret is *reported* depends on focus, never
/// whether its text is drawn (see `entry::draw`'s doc). `echo` is the most
/// recently SUBMITTED line, drawn above the command row (see `entry::draw`'s
/// doc for the ask-then-answer layout and why that row is reserved).
///
/// `world_plate`, when `Some`, is an already-rendered whole-world Mercator
/// plate (The Portolan part II, `bin`'s own `plate::draw` -- `core` carries
/// no hornvale crate, so it cannot draw the plate itself) drawn into the
/// plate region INSTEAD OF the walk-band chart or the chamber-band floor
/// plan: the world view is a lens over whichever band the character
/// occupies, not a new band, so the character's own position in the
/// snapshot is untouched either way. `None` draws the band's own plate
/// exactly as before this parameter existed.
///
/// **The plate's width is focus-dependent (The Portolan part II, Task 3a).**
/// With [`crate::Focus::Map`] focused AND a `world_plate` supplied, the
/// plate claims [`world_plate_width`] columns — up to the terminal's own
/// width, per that function's fit — rather than the old fixed
/// [`PLATE_WIDTH`]; every other combination (any other focus, or no
/// `world_plate` at all) keeps [`PLATE_WIDTH`] unchanged, so a caller-
/// supplied plate wider than the old fixed width is CLIPPED, never
/// stretched into, outside `Focus::Map`. This is computed exactly once,
/// here — see [`world_plate_width`]'s own doc for why a second copy
/// elsewhere would reproduce a defect this campaign already fixed once, on
/// the height axis.
// `echo` (Task 3) pushed this to 7, `world_plate` (The Portolan part II,
// Task 2) to 8, `strip_offset` (Task 4, F3) to 9 — mirroring `render_with`'s
// own allow, which this function's own parameter list mirrors one-for-one
// plus `snapshot`
#[allow(clippy::too_many_arguments)]
pub fn compose(
    snapshot: &crate::Snapshot,
    w: u16,
    h: u16,
    strip: Option<&str>,
    focus: crate::Focus,
    line: crate::CommandLine<'_>,
    echo: Option<&str>,
    world_plate: Option<&Grid>,
    strip_offset: u16,
) -> (Grid, Option<(u16, u16)>) {
    let mut page = Grid::new(w, h);
    let content_height = content_height(h);
    let plate_width = if focus == crate::Focus::Map && world_plate.is_some() {
        world_plate_width(w, h)
    } else {
        PLATE_WIDTH
    }
    .min(w);
    let entry_width = w.saturating_sub(plate_width);

    let mut plate = Grid::new(plate_width, content_height);
    match world_plate {
        Some(world) => blit(world, &mut plate, (0, 0)),
        None => match &snapshot.spatial {
            Spatial::Walk { chart } => crate::chart::draw(chart, &mut plate, (0, 0)),
            Spatial::Chamber { plan } => crate::plan::draw(plan, &mut plate, (0, 0)),
        },
    }
    blit(&plate, &mut page, (0, 0));

    let caret = crate::entry::draw(
        &snapshot.narration,
        &mut page,
        (plate_width, 0),
        entry_width,
        content_height,
        focus,
        line,
        echo,
    );

    if let Some(text) = strip {
        crate::strip::draw(
            text,
            &mut page,
            (0, content_height),
            plate_width,
            strip_offset,
        );
    }

    let endpaper_row = h.saturating_sub(2);
    crate::endpaper::draw(
        &snapshot.me,
        snapshot.day,
        snapshot.turn,
        &mut page,
        (0, endpaper_row),
    );

    (page, caret)
}

#[cfg(test)]
mod tests {
    use super::*;

    const WALK_FIXTURE: &str = include_str!("../tests/fixtures/session-seed-42-turn-0.json");
    const CHAMBER_FIXTURE: &str = include_str!("../tests/fixtures/session-seed-42-chamber.json");

    #[test]
    fn compose_fills_the_requested_dimensions() {
        let s = crate::Snapshot::parse(WALK_FIXTURE).unwrap();
        let (g, _) = compose(
            &s,
            80,
            24,
            None,
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
            None,
            0,
        );
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
        // The binding is `snap`, not the shorter `s`: the repo-wide scan in
        // `cli/tests/suite/claim_shape.rs` treats a bare `s` as a seed-shaped name
        // (the census corpora spell that loop the short way), and this loop
        // walks two Snapshots. Tagging it `/// claim:` instead would declare
        // a quantified claim over seeds that this test does not make.
        for snap in [walk, chamber] {
            let (g, _) = compose(
                &snap,
                80,
                24,
                None,
                crate::Focus::Cli,
                crate::CommandLine::default(),
                None,
                None,
                0,
            );
            let text = g.to_plain_text();
            let plate_has_ink = text
                .lines()
                .take(20)
                .any(|l| l.chars().take(40).any(|c| c != ' '));
            assert!(plate_has_ink, "the plate must draw for every band");
        }
    }

    /// The map strip draws beneath the plate's own content, still clipped
    /// to the plate's width — `compose` must hand `strip::draw` the right
    /// row and the right width, not just delegate blindly.
    #[test]
    fn compose_draws_the_strip_beneath_the_plate() {
        let s = crate::Snapshot::parse(WALK_FIXTURE).unwrap();
        let (g, _) = compose(
            &s,
            80,
            24,
            Some("a cairn"),
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
            None,
            0,
        );
        let text = g.to_plain_text();
        let strip_row = text.lines().nth(20).unwrap();
        assert!(
            strip_row.starts_with("a cairn"),
            "expected the strip text at row 20, got: {strip_row:?}"
        );
        let past_plate: String = strip_row.chars().skip(PLATE_WIDTH as usize).collect();
        assert!(
            past_plate.trim().is_empty(),
            "the strip must not bleed into the entry's columns"
        );
    }

    /// `compose` must actually HAND `strip_offset` to `strip::draw`, not
    /// merely accept it — a text long enough to scroll shows a different
    /// window at a nonzero offset.
    #[test]
    fn compose_threads_strip_offset_through_to_the_drawn_window() {
        let s = crate::Snapshot::parse(WALK_FIXTURE).unwrap();
        // Distinguishable characters, not a repeated one: scrolling an
        // all-`z` string would look identical at every offset and the
        // assertion below would pass vacuously.
        let long: String = (0..200).map(|i| (b'a' + (i % 26) as u8) as char).collect();
        let (head_grid, _) = compose(
            &s,
            80,
            24,
            Some(&long),
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
            None,
            0,
        );
        let (scrolled_grid, _) = compose(
            &s,
            80,
            24,
            Some(&long),
            crate::Focus::Cli,
            crate::CommandLine::default(),
            None,
            None,
            5,
        );
        let head_row = head_grid
            .to_plain_text()
            .lines()
            .nth(20)
            .unwrap()
            .to_string();
        let scrolled_row = scrolled_grid
            .to_plain_text()
            .lines()
            .nth(20)
            .unwrap()
            .to_string();
        assert_ne!(
            head_row, scrolled_row,
            "compose must pass strip_offset through, not silently drop it"
        );
    }
}
