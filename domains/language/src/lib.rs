//! Language, tier 1: a feature-bearing phoneme model (spellings are views),
//! a per-species phonology drawn under an authored articulation envelope,
//! generative naming grammars, and a register renderer behind a permanent
//! content→render seam. Kernel-only; it defines its own input structs the
//! composition root populates and never imports another domain.
#![warn(missing_docs)]

/// The phoneme model: segments as articulatory feature-bundles.
pub mod phoneme;

pub use phoneme::{Backness, Height, Manner, Place, Segment, ipa, romanize, sonority};
