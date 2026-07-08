//! Language, tier 1: a feature-bearing phoneme model (spellings are views),
//! a per-species phonology drawn under an authored articulation envelope,
//! generative naming grammars, and a register renderer behind a permanent
//! content→render seam. Kernel-only; it defines its own input structs the
//! composition root populates and never imports another domain.
#![warn(missing_docs)]

/// The naming grammars: stems, kind-keyed morphology, and uniqueness
/// re-draw, built from a drawn phonology.
pub mod naming;
/// The phoneme model: segments as articulatory feature-bundles.
pub mod phoneme;
/// The phonology engine: per-species inventory and phonotactics drawn
/// under the articulation envelope.
pub mod phonology;
/// The register renderer: `render_line`, the permanent content→render
/// seam.
pub mod register;

pub use naming::{GeneratedName, MorphOptions, NameKind, Namer};
pub use phoneme::{Backness, Height, Manner, Place, Segment, ipa, romanize, sonority};
pub use phonology::{Envelope, ExoticSeg, Phonology, draw_phonology, permits};
pub use register::{LineContent, LineSentiment, VoiceParams, render_line};
