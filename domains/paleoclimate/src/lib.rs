//! Hornvale paleoclimate domain: Milankovitch glacial cycling from the sky's
//! orbital forcing, and the strata (fossil shorelines, refugia, ice-extent
//! envelopes) a deep-time glacial history leaves on the present. Kernel-only
//! (spec: reads climate outputs as bare kernel types, supplied by the
//! composition root — never a climate or astronomy import).
#![warn(missing_docs)]

pub mod facts;
pub mod forcing_index;
pub mod ice;
pub mod render;
pub mod strata;
pub mod units;

pub use facts::{genesis, register_concepts};
pub use forcing_index::caloric_summer_index;
pub use ice::{ALBEDO_GAIN_C, IceState, integrate_ice};
pub use strata::{EraClimate, PaleoRecord, extract, glaciated};
pub use units::{Celsius, IceVolume, SeaLevelChange, TempAnomaly, UnitError};

/// Paleoclimate as a registrable unit for the composition-root roster.
/// It draws no seed streams, so it takes the empty `stream_labels` default.
/// type-audit: bare-ok(identifier-text: return)
pub struct Paleoclimate;

impl hornvale_kernel::Domain for Paleoclimate {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
}
