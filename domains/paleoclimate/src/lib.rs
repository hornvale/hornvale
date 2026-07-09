//! Hornvale paleoclimate domain: Milankovitch glacial cycling from the sky's
//! orbital forcing, and the strata (fossil shorelines, refugia, ice-extent
//! envelopes) a deep-time glacial history leaves on the present. Kernel-only
//! (spec: reads climate outputs as bare kernel types, supplied by the
//! composition root — never a climate or astronomy import).
#![warn(missing_docs)]

pub mod forcing_index;
pub mod ice;
pub mod units;

pub use forcing_index::caloric_summer_index;
pub use ice::{IceState, integrate_ice};
pub use units::{IceVolume, SeaLevelChange, UnitError};
