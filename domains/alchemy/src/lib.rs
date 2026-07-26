//! The alchemy domain: the material ground truth, and nothing that believes
//! anything about it.
//!
//! A substance is a bundle of LATENT qualities ([`quality::QualityVector`]).
//! Nothing perceives a quality directly; what an observer gets are manifest
//! [`sign::SignVector`] channels — weak, partly misleading functions of the
//! qualities — and the outcomes of [`production`]s. That latent/manifest split
//! is what makes a practitioner's doctrine capable of being WRONG, and it is
//! the whole reason this domain exists (spec: The Assay §2).
//!
//! THIS DOMAIN DRAWS NOTHING. There is no `streams.rs`, no `StreamLabel`, and
//! no `Seed` parameter anywhere in it. Chemistry is universal; per-world
//! variation arrives through which substances a world HAS, which is derived at
//! the composition root from state other domains already drew.
//!
//! LAYERING: like `hornvale_language`'s `Envelope`, [`Substrate`] is this
//! domain's OWN copy of the material dimensions it needs, populated by the
//! composition root. This crate never imports `hornvale-terrain` or
//! `hornvale-species`.

#![warn(missing_docs)]

pub mod production;
pub mod quality;
pub mod sign;

/// Alchemy's own copy of the material dimensions a substance source carries.
///
/// Populated by `windows/worldgen` from terrain's `Commodity`/`RockClass`/
/// `SoilOrder` and species' `BiosphereTraits`. Every field is a dimensionless
/// ratio in [0,1].
/// type-audit: bare-ok(ratio: metallic), bare-ok(ratio: organic), bare-ok(ratio: saline), bare-ok(ratio: refractory), bare-ok(ratio: purity)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Substrate {
    /// How metal-like the source is.
    pub metallic: f64,
    /// Living or once-living origin.
    pub organic: f64,
    /// Evaporite/salt character.
    pub saline: f64,
    /// Resistance to heat.
    pub refractory: f64,
    /// Freedom from contaminants — carries The Lode's drawn ore `grade`.
    pub purity: f64,
}

/// Clamp to the unit interval. Free of transcendentals by construction, so
/// this crate cannot introduce a cross-platform ULP difference.
pub(crate) fn clamp01(x: f64) -> f64 {
    x.clamp(0.0, 1.0)
}
