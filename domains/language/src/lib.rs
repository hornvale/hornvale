//! Language, tier 1: a feature-bearing phoneme model (spellings are views),
//! a per-species phonology drawn under an authored articulation envelope,
//! generative naming grammars, and a register renderer behind a permanent
//! content→render seam. Kernel-only; it defines its own input structs the
//! composition root populates and never imports another domain.
#![warn(missing_docs)]

/// The naming grammars: stems and kind-keyed morphology, a single
/// deterministic draw per `(seed, species, kind, salt)` (no re-draw), built
/// from a drawn phonology.
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

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// `<species>` stands for the concrete species leg of the path (e.g.
/// `goblin`, `kobold`) and the per-entity salt leg (the settlement cell id,
/// the belief id) is omitted, matching the documentation convention of the
/// other domains' `stream_labels()`. Labels are permanent save-format
/// contracts (spec §3); regeneration uses epoch suffixes, never renames.
///
/// These are the REAL derivation paths behind every generated proper noun in
/// the world: the phonology draw (which the composition root reconstructs
/// per world) and the three name kinds (each a single deterministic draw).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (
            "language/<species>/phonology/inventory",
            "per-species phoneme inventory draw under the articulation envelope",
        ),
        (
            "language/<species>/phonology/phonotactics",
            "per-species syllable phonotactic templates (onsets, nuclei, codas)",
        ),
        (
            "language/<species>/name/settlement",
            "per-settlement name (salted by cell id): a bare stem",
        ),
        (
            "language/<species>/name/deity",
            "per-deity name (salted by belief id): a bare stem biased toward closed syllables",
        ),
        (
            "language/<species>/name/epithet",
            "per-deity epithet (salted by belief id): a descriptive root, optionally reduplicated and honorific-prefixed",
        ),
    ]
}
