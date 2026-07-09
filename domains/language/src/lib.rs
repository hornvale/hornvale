//! Language, tier 1: a feature-bearing phoneme model (spellings are views),
//! a per-species phonology drawn under an authored articulation envelope,
//! generative naming grammars, and a register renderer behind a permanent
//! content→render seam. Kernel-only; it defines its own input structs the
//! composition root populates and never imports another domain.
#![warn(missing_docs)]

/// The etymology engine: proto-roots drawn from a phonology, and a drawn
/// cascade of sound-change rules (`evolve`, pure and total, Neogrammarian)
/// that turns a proto-root into its modern form.
pub mod etymology;
/// The lexicon: two-pass assembly over a culture's concept exposures —
/// `Steeped` concepts become roots (Task 6's `proto_root`/`evolve`),
/// `KnowsOf` concepts become recipe compounds joined under a drawn
/// `Headedness`, and everything else is a reasoned `Gap`.
pub mod lexicon;
/// The naming grammars: stems and kind-keyed morphology, a single
/// deterministic draw per `(seed, species, kind, salt)` (no re-draw), built
/// from a drawn phonology.
pub mod naming;
/// The Swadesh packs: authored core vocabulary (the universal stratum,
/// color, body, and kin packs), Berlin & Kay acquisition ladders, the
/// closed compound-recipe table, and `register_concepts`.
pub mod packs;
/// The phoneme model: segments as articulatory feature-bundles.
pub mod phoneme;
/// The phonology engine: per-species inventory and phonotactics drawn
/// under the articulation envelope.
pub mod phonology;
/// The register renderer: `render_line`, the permanent content→render
/// seam.
pub mod register;

pub use etymology::{
    AppliedRule, Cascade, Derivation, RuleKind, SoundRule, draw_cascade, evolve, proto_root,
};
pub use lexicon::{
    ExposureClass, GapReason, Headedness, LexEntry, Lexicon, WordViews, build_lexicon,
    draw_headedness,
};
pub use naming::{GeneratedName, MorphOptions, NameKind, Namer};
pub use packs::{
    PackDepths, PackEntry, body_pack, color_pack, compound_recipe, in_ladder, kin_pack,
    register_concepts, universal_stratum,
};
pub use phoneme::{
    Backness, Height, Manner, Place, Segment, espeak, espeak_word, ipa, romanize, sonority,
};
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
            "(retired at The Words, superseded by name/settlement/v2) per-settlement name (salted by cell id): a bare stem",
        ),
        (
            "language/<species>/name/deity",
            "(retired at The Words, superseded by name/deity/v2) per-deity name (salted by belief id): a bare stem biased toward closed syllables",
        ),
        (
            "language/<species>/name/epithet",
            "(retired at The Words, superseded by name/epithet/v2) per-deity epithet (salted by belief id): a descriptive root, optionally reduplicated and honorific-prefixed",
        ),
        (
            "language/<species>/name/settlement/v2",
            "the glossed settlement name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above",
        ),
        (
            "language/<species>/name/deity/v2",
            "the glossed deity name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above",
        ),
        (
            "language/<species>/name/epithet/v2",
            "the glossed epithet (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the v1 draw above",
        ),
        (
            "language/<species>/lexicon/root/<concept>",
            "per-concept proto-root (1-2 syllables, from the phonotactic templates)",
        ),
        (
            "language/<species>/lexicon/cascade",
            "the species' 2-4 rule sound-change cascade, applied by evolve() to every proto-root",
        ),
        (
            "language/<species>/lexicon/headedness",
            "the species' drawn compound-joining order (HeadFirst/HeadLast), gating LexEntry::Compound component order",
        ),
    ]
}
