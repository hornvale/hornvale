//! Compose the full two-page spread: the plate on the left, the entry on
//! the right, and the endpaper strip beneath both — the bound journal the
//! possessed creature keeps, open to a spread.
//!
//! ## Layout
//!
//! For a `w`-by-`h` grid:
//! - the **plate** (whatever is being drawn — the world raster when one is
//!   supplied, otherwise the chart outdoors or the floor plan indoors)
//!   occupies columns `0..plate_width`, which is [`world_plate_width`] when
//!   a plate is supplied and the fixed [`PLATE_WIDTH`] when none is;
//! - the **entry** (the narration prose, wrapped, plus the command line)
//!   occupies the remaining columns, never fewer than [`MIN_ENTRY_WIDTH`]
//!   of them;
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

/// The plate's fixed width when no plate is supplied: the plate then occupies
/// `0..PLATE_WIDTH` and the entry begins at that column. A supplied plate is
/// sized by [`world_plate_width`] instead (module doc).
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
/// `0292de87f^`) — `pub` because [`world_plate_width`] states the plate's
/// preferred shape in terms of it and `bin` reads the same ratio rather
/// than hardcoding a second `2`, the same one-source-of-truth reason
/// [`world_plate_width`] itself is exposed rather than recomputed.
///
/// **It no longer derives the plate's HEIGHT.** It used to
/// (`world_plate_width(w, h) / GLYPH_ASPECT`), which was correct only
/// while the width could never exceed `GLYPH_ASPECT * content_height(h)`.
/// The Quadrat's Task 9 raised the width floor past that bound on wide
/// terminals, so the plate's height is now [`content_height`] outright —
/// the region [`compose`] actually draws into — and a height derived from
/// the width would name rows the page has no room for.
pub const GLYPH_ASPECT: u16 = 2;

/// The narrowest the entry pane may ever be squeezed to, in columns —
/// the width it has at the 80x24 floor, which is the narrowest entry pane
/// this client has ever shipped and therefore the only "legible" number
/// with evidence behind it rather than taste. "Legible" here means
/// exactly that: wrapped narration at this width is what every reader of
/// the floor spread has already been reading, so a wider terminal can
/// never be given a worse entry pane than the smallest supported one.
///
/// It exists because [`world_plate_width`]'s floor is stated as a
/// FRACTION of the terminal and a fraction alone has only one bound. A
/// rule that said only "the plate takes at least half" would keep taking
/// half of a shrinking terminal until the prose had nowhere to go; this
/// is the other bound, and it wins when the two disagree.
pub const MIN_ENTRY_WIDTH: u16 = 40;

/// The world plate's width for a `w`-by-`h` terminal — the plate region's
/// own column count whenever [`compose`] is handed a plate at all.
///
/// **Two bounds and a preference, in that order of authority.**
///
/// 1. **The floor: half the terminal, rounded up.** The map is what the
///    player looks at while walking, so it may never be the smaller half
///    of the spread. `div_ceil` rather than `/ 2` so an ODD width still
///    satisfies "at least half" rather than landing one column under it.
/// 2. **The preference: a square screen footprint.** A terminal glyph
///    reads about [`GLYPH_ASPECT`] times as tall as it is wide, so a
///    plate `GLYPH_ASPECT * content_height(h)` columns across occupies a
///    roughly SQUARE region of the screen — a good default shape for a
///    map pane, and the widest shape worth taking before the pane starts
///    reading as a letterbox. When the terminal is tall enough to afford
///    more than half its width on this rule, it gets it.
/// 3. **The ceiling: `w - MIN_ENTRY_WIDTH`.** Whichever of the two above
///    wins, the entry pane keeps a legible minimum, and this bound beats
///    both — prose with nowhere to go is a worse failure than a plate
///    that is merely under half.
///
/// **This is no longer an anti-STRETCH fit, and the difference is why the
/// floor could be raised at all.** The `GLYPH_ASPECT`:1 shape used to be a
/// hard maximum, on the theory that a plate wider than that would stretch
/// the projection. The Quadrat's Task 1 removed that theory's premise:
/// `bin`'s `plate::virtual_dims` derives the virtual chart's size from the
/// MESH DEPTH alone and no longer takes a plate width, so the plate is a
/// WINDOW onto that chart — a subrect — and widening it uncovers more
/// chart columns rather than stretching the ones already shown. (The
/// residual vertical exaggeration, one chart facet drawn one glyph tall on
/// a glyph twice as tall as it is wide, is `MAP-vertical-axis-undersamples-
/// the-mesh`: a property of the tile-to-glyph mapping, identical at every
/// window shape, and untouched either way.) So the shape above is a
/// PREFERENCE about how a pane reads, not a correctness constraint, and a
/// floor may legitimately override it.
///
/// `pub` for the same reason [`content_height`] is: [`compose`] uses this
/// number to size the plate region it draws into, and `bin`'s driver needs
/// the SAME number to size the [`Grid`] it hands back — not a second,
/// possibly-diverging copy of this rule. [`content_height`]'s own doc
/// records why: two callers computing "the plate's content height"
/// independently is exactly the shape that let a fixed-height assumption
/// silently name the wrong vertex at any non-floor terminal size. A second
/// copy of the width formula reproduces that defect on the other axis.
pub const fn world_plate_width(w: u16, h: u16) -> u16 {
    let square_footprint = content_height(h).saturating_mul(GLYPH_ASPECT);
    let half = w.div_ceil(2);
    let wanted = if square_footprint > half {
        square_footprint
    } else {
        half
    };
    let ceiling = w.saturating_sub(MIN_ENTRY_WIDTH);
    if wanted > ceiling { ceiling } else { wanted }
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
/// `hint` is the pending tab-completion ambiguity, drawn beneath the
/// command row when present (see [`crate::entry::Hint`] — the stem bold as
/// typed, the suggested remainder normal, overlong lists collapsing to an
/// honest "… +N more" count).
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
/// **A SUPPLIED PLATE WIDENS THE PANE, IN EVERY FOCUS (The Quadrat, Task
/// 9).** Whenever `world_plate` is `Some`, the plate region claims
/// [`world_plate_width`] columns rather than the fixed [`PLATE_WIDTH`];
/// `None` keeps [`PLATE_WIDTH`] unchanged, which is the chamber band's
/// floor plan and the walk band's own chart.
///
/// The rule used to carry a second clause — [`crate::Focus::Map`] had to
/// be focused too (The Portolan part II, Task 3a) — and that clause is
/// what made the campaign's third reported defect only half-fixed. The
/// default focus is [`crate::Focus::Walk`], so the view a player actually
/// looks at while walking took the narrow arm and drew the old hex
/// scatter at [`PLATE_WIDTH`] columns no matter how wide the terminal
/// was. Focus is a question about where the KEYS go; it was never a
/// question about how wide the picture should be. Whether a plate exists
/// at all is now the whole of it, and `bin`'s driver answers that with a
/// question about the BAND.
///
/// The width is computed exactly once, here — see [`world_plate_width`]'s
/// own doc for why a second copy elsewhere would reproduce a defect this
/// campaign already fixed once, on the height axis.
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
    hint: Option<&crate::entry::Hint<'_>>,
) -> (Grid, Option<(u16, u16)>) {
    let mut page = Grid::new(w, h);
    let content_height = content_height(h);
    let plate_width = if world_plate.is_some() {
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
        hint,
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
            None,
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
                None,
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
            None,
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
            None,
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
            None,
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
