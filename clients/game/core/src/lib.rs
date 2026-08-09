#![warn(missing_docs)]
//! Hornvale's game client, renderer half: `vessel/session/v1` to a
//! character grid. This crate does not depend on any hornvale crate.

pub mod schema;
pub use schema::*;

impl Snapshot {
    /// Parse an emitted `vessel/session/v1` document.
    pub fn parse(json: &str) -> Result<Snapshot, serde_json::Error> {
        serde_json::from_str(json)
    }
}
