#![warn(missing_docs)]
//! Hornvale's game client, renderer half: `vessel/session/v2` to a
//! character grid. This crate does not depend on any hornvale crate.

pub mod cell;
pub mod chart;
pub mod endpaper;
pub mod entry;
pub mod plan;
pub mod schema;
pub mod spread;
pub mod strip;
pub use cell::*;
pub use schema::*;

impl Snapshot {
    /// Parse an emitted `vessel/session/v2` document.
    pub fn parse(json: &str) -> Result<Snapshot, serde_json::Error> {
        serde_json::from_str(json)
    }
}

/// The minimum grid width the spread renders at. "Monochrome at 80×24 is
/// the floor" — The Quire's register: if it only works larger, it is
/// wrong, so [`render`] refuses anything smaller rather than silently
/// degrading.
pub const MIN_WIDTH: u16 = 80;

/// The minimum grid height the spread renders at. See [`MIN_WIDTH`].
pub const MIN_HEIGHT: u16 = 24;

/// Everything that can keep [`render`] from producing a [`Grid`].
#[derive(Debug)]
pub enum Error {
    /// `json` was not a valid `vessel/session/v2` document.
    Parse(serde_json::Error),
    /// The requested grid was smaller than the monochrome floor
    /// ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
    TooSmall {
        /// The width that was requested.
        w: u16,
        /// The height that was requested.
        h: u16,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(e) => write!(f, "failed to parse vessel/session/v2: {e}"),
            Error::TooSmall { w, h } => write!(
                f,
                "grid {w}x{h} is smaller than the {MIN_WIDTH}x{MIN_HEIGHT} floor"
            ),
        }
    }
}

impl std::error::Error for Error {}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Error {
        Error::Parse(e)
    }
}

/// A free-roaming cursor's screen position, in grid cells.
///
/// **The cursor is not ink.** It is the terminal's own hardware cursor,
/// which occupies no character cell at all — "ornament may never occupy a
/// cell that carries information" holds absolutely for the cursor rather
/// than by argument, because there is no cell to occupy in the first
/// place. [`render_with`] therefore never draws it onto the [`Grid`]; it
/// only reports where the terminal should place its own cursor, as the
/// second element of its return tuple.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor {
    /// Column.
    pub x: u16,
    /// Row.
    pub y: u16,
}

/// Render `json` into a `w`-by-`h` character grid, plus the **screen**
/// position the terminal cursor should sit at (see [`Cursor`]).
///
/// `cursor`, when `Some`, is reported back verbatim as the second element
/// of the returned tuple — never drawn onto the [`Grid`]. `strip`, when
/// `Some`, is drawn as the look-mode strip beneath the plate (see
/// `strip::draw`); the row it occupies is reserved either way (see
/// `spread`'s module doc), so a caller that starts passing `Some` never
/// resizes the plate a second time.
///
/// **This is the row The Portolan's Task 2 costs the plate.** At exactly
/// 80×24, the plate's content height was 21 rows before this row was
/// reserved and is 20 after — the number the task's own report is required
/// to state explicitly, because the 80×24 floor below is unweakened by
/// either figure: `render_with` still refuses anything smaller than
/// [`MIN_WIDTH`]×[`MIN_HEIGHT`], strip present or not.
///
/// Fails if `json` does not parse, or if the requested grid is smaller
/// than the monochrome floor ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
pub fn render_with(
    json: &str,
    w: u16,
    h: u16,
    cursor: Option<Cursor>,
    strip: Option<&str>,
) -> Result<(Grid, Option<(u16, u16)>), Error> {
    if w < MIN_WIDTH || h < MIN_HEIGHT {
        return Err(Error::TooSmall { w, h });
    }
    let snapshot = Snapshot::parse(json)?;
    let grid = spread::compose(&snapshot, w, h, strip);
    Ok((grid, cursor.map(|c| (c.x, c.y))))
}

/// Render `json` — an emitted `vessel/session/v2` document — into a
/// `w`-by-`h` character grid: the plate left, the entry right, the
/// endpaper strip below (see [`spread::compose`]). Fails if `json` does
/// not parse, or if the requested grid is smaller than the monochrome
/// floor ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
///
/// Delegates to [`render_with`] with no cursor and no look-mode strip —
/// kept as its own entry point because existing callers (`tests/`, the
/// terminal binary) depend on this exact signature.
pub fn render(json: &str, w: u16, h: u16) -> Result<Grid, Error> {
    render_with(json, w, h, None, None).map(|(grid, _)| grid)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `render` of a document that is not even valid JSON must surface
    /// `Error::Parse`, not panic and not `Error::TooSmall`.
    #[test]
    fn render_of_invalid_json_is_a_parse_error() {
        match render("not json at all", MIN_WIDTH, MIN_HEIGHT) {
            Err(Error::Parse(_)) => {}
            other => panic!("expected Error::Parse, got {other:?}"),
        }
    }

    /// A width one short of the floor is refused, and the error carries
    /// the exact `w`/`h` that was requested (not the floor, and not some
    /// clamped value) — the whole reason `TooSmall` is a struct variant
    /// rather than a unit one.
    #[test]
    fn render_one_column_short_of_the_floor_is_too_small() {
        let got = render("{}", MIN_WIDTH - 1, MIN_HEIGHT);
        assert!(matches!(
            got,
            Err(Error::TooSmall {
                w,
                h
            }) if w == MIN_WIDTH - 1 && h == MIN_HEIGHT
        ));
    }

    /// Symmetric case on the height axis, so a bug that only checked one
    /// dimension would still be caught.
    #[test]
    fn render_one_row_short_of_the_floor_is_too_small() {
        let got = render("{}", MIN_WIDTH, MIN_HEIGHT - 1);
        assert!(matches!(
            got,
            Err(Error::TooSmall {
                w,
                h
            }) if w == MIN_WIDTH && h == MIN_HEIGHT - 1
        ));
    }

    /// `TooSmall` is checked before the document is even parsed — an
    /// invalid document at an undersized grid must still report the size
    /// problem, not a parse problem, so a caller sees the more actionable
    /// error first.
    #[test]
    fn too_small_is_reported_even_for_invalid_json() {
        let got = render("not json", MIN_WIDTH - 1, MIN_HEIGHT);
        assert!(matches!(got, Err(Error::TooSmall { .. })));
    }

    /// A committed `vessel/session/v2` fixture, reused rather than minted —
    /// the same file `spread.rs`'s own tests and `tests/provenance.rs`
    /// already read.
    fn fixture_json() -> String {
        include_str!("../tests/fixtures/session-seed-42-turn-0.json").to_string()
    }

    /// H3: the floor holds. At exactly 80x24, with the strip added, the
    /// spread still renders — and 79x24 is still REFUSED rather than
    /// degraded. This is the constraint the campaign inherits and may not
    /// weaken, so it is asserted in both directions.
    #[test]
    fn the_eighty_by_twentyfour_floor_survives_the_strip() {
        let json = fixture_json();
        let (grid, _) =
            render_with(&json, 80, 24, None, Some("Vngashngatva")).expect("renders at the floor");
        assert_eq!(grid.width(), 80);
        assert_eq!(grid.height(), 24);
        assert!(
            matches!(
                render_with(&json, 79, 24, None, None),
                Err(Error::TooSmall { .. })
            ),
            "79 columns must still be refused, not degraded"
        );
    }

    /// The cursor is NOT ink. It must occupy no grid cell — `render_with`
    /// reports a position for the terminal to place its own cursor at, and
    /// the grid is byte-identical with and without one.
    #[test]
    fn the_cursor_occupies_no_cell() {
        let json = fixture_json();
        let (plain, none_at) = render_with(&json, 80, 24, None, None).expect("renders");
        let (with, some_at) =
            render_with(&json, 80, 24, Some(Cursor { x: 3, y: 4 }), None).expect("renders");
        assert!(none_at.is_none());
        assert_eq!(
            some_at,
            Some((3, 4)),
            "the cursor position is reported, not drawn"
        );
        for y in 0..24 {
            for x in 0..80 {
                assert_eq!(
                    plain.get(x, y),
                    with.get(x, y),
                    "cursor inked cell ({x},{y})"
                );
            }
        }
    }
}
