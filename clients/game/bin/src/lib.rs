#![warn(missing_docs)]
//! Hornvale's game client, driver half: links `hornvale-vessel` and
//! `hornvale-game-core`, so the two never need to meet in the same crate.
//! See `driver`'s module doc for the containment rule this exists to hold.

pub mod driver;
pub mod input;
pub mod line;
pub mod term;
