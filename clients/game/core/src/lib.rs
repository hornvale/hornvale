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

/// Render `json` — an emitted `vessel/session/v2` document — into a
/// `w`-by-`h` character grid: the plate left, the entry right, the
/// endpaper strip below (see [`spread::compose`]). Fails if `json` does
/// not parse, or if the requested grid is smaller than the monochrome
/// floor ([`MIN_WIDTH`] by [`MIN_HEIGHT`]).
pub fn render(json: &str, w: u16, h: u16) -> Result<Grid, Error> {
    if w < MIN_WIDTH || h < MIN_HEIGHT {
        return Err(Error::TooSmall { w, h });
    }
    let snapshot = Snapshot::parse(json)?;
    Ok(spread::compose(&snapshot, w, h))
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
}
