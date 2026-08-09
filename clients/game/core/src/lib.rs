#![warn(missing_docs)]
//! Hornvale's game client, renderer half: `vessel/session/v1` to a
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
    /// Parse an emitted `vessel/session/v1` document.
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
    /// `json` was not a valid `vessel/session/v1` document.
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
            Error::Parse(e) => write!(f, "failed to parse vessel/session/v1: {e}"),
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

/// Render `json` — an emitted `vessel/session/v1` document — into a
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
