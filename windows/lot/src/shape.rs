//! The Lot's public compatibility surface for the shared population trajectory.
//!
//! The reconstruction is owned by `hornvale-history`, which is the lower-layer
//! owner of the committed occupation facts. The Lot re-exports it so existing
//! consumers cannot accidentally acquire a second implementation.

pub use hornvale_history::trajectory::{EPOCH_YEARS, Shape, integral, population_at, shape_of};
