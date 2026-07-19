//! Language, tier 1: a feature-bearing phoneme model (spellings are views),
//! a per-species phonology drawn under an authored articulation envelope,
//! generative naming grammars, and a register renderer behind a permanent
//! content→render seam. Kernel-only; it defines its own input structs the
//! composition root populates and never imports another domain.
#![warn(missing_docs)]

/// The epistemic account (C4, The Chorus, LANG-36): the four-filter stack
/// (lexicon → knowledge → ontology → valence) that turns a ground-truth
/// fact list into one culture's `Account`, plus the dial's distance
/// measures (distortion, distinctiveness, recoverability) that read it.
/// Pure and surface-free — the caller supplies the observability table.
pub mod account;
/// The clause layer: a language-neutral `ClauseSpec` and the Common
/// realizer that turns it into a sentence. Generalizes the `render_line`
/// seam from a bespoke tenet spec to any clause.
pub mod clause;
/// The etymology engine: proto-roots drawn from a phonology, and a drawn
/// cascade of sound-change rules (`evolve`, pure and total, Neogrammarian)
/// that turns a proto-root into its modern form.
pub mod etymology;
/// A tongue's drawn surface grammar (C3, The Tongues): constituent order,
/// copula presence and drawn form, and article presence — the floor slice
/// of LANG-40's grammaticalization-depth vector.
pub mod grammar;
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
/// The causal-schema library and lexicalization substrate (C5, LANG-37 /
/// LANG-38): the closed 12-row schema table, the fact-shape admission
/// gate, the β-sharpened schema draw, and the closed agentive lexeme
/// tables.
pub mod schemas;

pub use account::{
    Account, AccountEntry, AccountParams, Disposition, GroundFact, LossReason, NeededConcept,
    Observability, OrderPolicy, Requirement, Stance, account_of, distinctiveness, distortion,
    domain_distortion, identity_params, recoverability,
};
pub use clause::{
    ClauseSpec, Definiteness, Frame, Number, ParseContext, ParseError, parse_common, realize_common,
};
pub use etymology::{
    AppliedRule, Cascade, Daughter, Derivation, RuleKind, SoundRule, assign_proto_roots,
    draw_cascade, evolve, proto_root,
};
pub use grammar::{
    ConstituentOrder, TongueClause, TongueGap, TongueGrammar, realize_tongue, tongue_grammar,
};
pub use lexicon::{
    ExposureClass, GapReason, Headedness, LexEntry, Lexicon, WordViews, build_lexicon,
    draw_headedness,
};
pub use naming::{GeneratedName, MorphOptions, NameKind, Namer, SiteConcepts, render_views};
pub use packs::{
    PackDepths, PackEntry, body_pack, color_pack, compound_recipe, concept_domain, in_ladder,
    is_core_concept, kin_pack, register_concepts, universal_stratum,
};
pub use phoneme::{
    Backness, Height, Manner, Place, Segment, Tone, espeak, espeak_word, ipa, romanize, sonority,
    tone_mark_ipa, tone_mark_roman, tone_of,
};
pub use phonology::{
    Envelope, ExoticSeg, Phonology, distinguishable_capacity, draw_phonology, permits,
    tone_inventory,
};
pub use register::{LineContent, LineSentiment, VoiceParams, render_line};
// `schemas::Manner` (a cyclic deity's told pace) is deliberately NOT
// re-exported unqualified here — it would collide with `phoneme::Manner`
// (articulatory manner) above; reach it as `schemas::Manner` or
// `crate::schemas::Manner`, the same qualified-access precedent
// `speech::Lexicon` sets below for its own `lexicon::Lexicon` collision.
pub use schemas::{
    FactShape, LexemeId, Schema, SchemaId, SlotKind, SourceDomain, SubFrame, admitted, lexemes_for,
    schema_table, select_lexeme, select_schema,
};

/// The speech cluster (ECS c3): the phonology envelope type
/// ([`speech::ArticulationVector`]) and the stopgap social vocabulary
/// ([`speech::Lexicon`]), moved here from the former species peopled
/// component (ECS c3) — the
/// phonology component's owner is language. A nested module (not a sibling
/// file) because its own [`speech::Lexicon`] would otherwise collide with
/// the generated-vocabulary [`Lexicon`] re-exported from [`lexicon`] at this
/// same crate root; only the registry *functions* are re-exported
/// unqualified below, not the type name.
pub mod speech {
    use hornvale_kernel::{Component, ComponentStore, KindId};

    /// An exotic manner of articulation found in a kind's phonology.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ExoticManner {
        /// No exotic manner (the goblin baseline).
        None,
        /// Trill: rapid vibration of an articulator.
        Trill,
        /// Click: sharp ingressive oral sound.
        Click,
        /// Ejective: sharp egressive sound made with trapped air.
        Ejective,
    }

    /// The closed seven-dimension articulation vector (spec §5, extended by
    /// the phonology epoch with `tonality`). Scalars are bare ratios in
    /// `[0, 1]` with 0.5 ≡ the goblin baseline (tonality 0.0 ≡ atonal, the
    /// humanoid default); widening the vector requires its own campaign.
    /// Every dimension is authored — nothing drawn. Moved here from
    /// `species` (ECS c3): the phonology component's owner is language.
    /// type-audit: bare-ok(ratio)
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct ArticulationVector {
        /// Lip-rounding and jaw-closure degree: unrounded 0 ↔ rounded 1.
        pub labiality: f64,
        /// Vowel-space size: compressed 0 ↔ expanded 1.
        pub vowel_space: f64,
        /// Voicing emphasis: voiceless 0 ↔ voiced 1.
        pub voicing: f64,
        /// Sibilance emphasis: minimal 0 ↔ pronounced 1.
        pub sibilance: f64,
        /// Voice-loudness range: quiet 0 ↔ loud 1.
        pub voice_loudness: f64,
        /// Tonal propensity, authored from body plan: atonal 0 (humanoid
        /// default) ↔ fully tonal 1. Maps to a tone-inventory size in
        /// `draw_phonology` (1 = atonal Neutral-only, 2–3 tone-capable) and
        /// makes tonogenesis effective. The shipped humanoids stay 0.0; the
        /// value earns its keep as the bestiary grows (serpentine, avian).
        pub tonality: f64,
        /// Exotic manner of articulation.
        pub exotic: ExoticManner,
    }

    /// The peopled social lexicon (stopgap vocabulary The Tongues will
    /// generate). Moved here from the former species peopled component (ECS c3).
    /// type-audit: bare-ok(identifier-text)
    #[derive(Clone, Debug, PartialEq)]
    pub struct Lexicon {
        /// The settlement noun ("village", "warren").
        pub noun: &'static str,
        /// Worker-role override; `None` = the subsistence word.
        pub worker_override: Option<&'static str>,
        /// The warrior-rung word.
        pub warrior: &'static str,
        /// The artisan-rung word.
        pub artisan: &'static str,
        /// The shaman-rung word.
        pub shaman: &'static str,
        /// The top-rung word.
        pub top: &'static str,
    }

    impl Component for ArticulationVector {}
    impl Component for Lexicon {}

    /// Peopled phonology, one per speaking kind. Values are the
    /// byte-identical articulation vectors formerly on the species peopled
    /// component.
    /// type-audit: bare-ok(identifier-text)
    pub fn articulation_registry() -> ComponentStore<KindId, ArticulationVector> {
        [
            (
                KindId("goblin"),
                ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.5,
                    voicing: 0.5,
                    sibilance: 0.5,
                    voice_loudness: 0.5,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("kobold"),
                ArticulationVector {
                    labiality: 0.1,
                    vowel_space: 0.3,
                    voicing: 0.6,
                    sibilance: 0.9,
                    voice_loudness: 0.2,
                    tonality: 0.0,
                    exotic: ExoticManner::Trill,
                },
            ),
            (
                KindId("hobgoblin"),
                ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.5,
                    voicing: 0.6,
                    sibilance: 0.4,
                    voice_loudness: 0.8,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("bugbear"),
                ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.4,
                    voicing: 0.7,
                    sibilance: 0.2,
                    voice_loudness: 0.3,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
        ]
        .into_iter()
        .collect()
    }

    /// Peopled lexicon, one per speaking kind. Byte-identical to the former
    /// species peopled component's noun + rung words.
    /// type-audit: bare-ok(identifier-text)
    pub fn lexicon_registry() -> ComponentStore<KindId, Lexicon> {
        [
            (
                KindId("goblin"),
                Lexicon {
                    noun: "village",
                    worker_override: None,
                    warrior: "warrior",
                    artisan: "artisan",
                    shaman: "shaman",
                    top: "chief",
                },
            ),
            (
                KindId("kobold"),
                Lexicon {
                    noun: "warren",
                    worker_override: Some("digger"),
                    warrior: "warden",
                    artisan: "shaper",
                    shaman: "keeper",
                    top: "elders",
                },
            ),
            (
                KindId("hobgoblin"),
                Lexicon {
                    noun: "legion",
                    worker_override: Some("laborer"),
                    warrior: "soldier",
                    artisan: "smith",
                    shaman: "augur",
                    top: "warlord",
                },
            ),
            (
                KindId("bugbear"),
                Lexicon {
                    noun: "lair",
                    worker_override: Some("forager"),
                    warrior: "mauler",
                    artisan: "tanner",
                    shaman: "omen-reader",
                    top: "headman",
                },
            ),
        ]
        .into_iter()
        .collect()
    }

    /// Proto ancestral articulation vectors keyed by family (goblinoid/
    /// draconic/plant) — moved here from species (ECS c3).
    /// type-audit: bare-ok(identifier-text)
    pub fn family_proto() -> ComponentStore<KindId, ArticulationVector> {
        [
            (
                KindId("goblinoid"),
                ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.5,
                    voicing: 0.55,
                    sibilance: 0.45,
                    voice_loudness: 0.55,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("draconic"),
                ArticulationVector {
                    labiality: 0.3,
                    vowel_space: 0.6,
                    voicing: 0.7,
                    sibilance: 0.6,
                    voice_loudness: 0.8,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("plant"),
                ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.4,
                    voicing: 0.4,
                    sibilance: 0.3,
                    voice_loudness: 0.3,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
        ]
        .into_iter()
        .collect()
    }
}

pub use speech::{
    ArticulationVector, ExoticManner, articulation_registry, family_proto, lexicon_registry,
};

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// `<species>` stands for the concrete species leg of the path (e.g.
/// `goblin`, `kobold`) and the per-entity salt leg (the settlement cell id,
/// the belief id) is omitted, matching the documentation convention of the
/// other domains' `stream_labels()`. Labels are permanent save-format
/// contracts (spec §3); regeneration uses epoch suffixes, never renames.
///
/// These are the REAL derivation paths behind every generated word and
/// proper noun in the world: the phonology draw (which the composition root
/// reconstructs per world), the lexicon draws (per-concept roots, the
/// sound-change cascade, compound headedness), and the name kinds (each a
/// single deterministic draw).
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (
            "language/<species>/phonology/inventory",
            "per-species phoneme inventory draw under the articulation envelope; for a family's shared proto-language (e.g. goblinoid) a family name occupies the <species> slot — a language with no speakers",
        ),
        (
            "language/<species>/phonology/phonotactics",
            "per-species syllable phonotactic templates (onsets, nuclei, codas)",
        ),
        (
            "language/<species>/phonology/tones",
            "the phonology epoch's tone-inventory draw: which contrastive level tone (High/Low) joins Neutral for a partly-tonal species (tonality → 2 tones); atonal (1) and fully tonal (3) draw nothing here",
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
            "language/<family>/lexicon/root/v3/<concept>",
            "per-concept family proto-root, injectively and MERGER-AWARELY assigned (epoch root/v3): the open-addressing draw also rejects a core candidate whose evolved form would merge with an already-placed core concept in any daughter, so core homophony is zero; family == species for a singleton stock. Probe re-draws key a /probe/<n> sub-stream",
        ),
        (
            "language/<family>/lexicon/root/v2/<concept>",
            "(retired by the merger-aware assignment, superseded by root/v3) the injective-but-proto-only family assignment",
        ),
        (
            "language/goblin/lexicon/root/<concept>",
            "(retired at The Branches, superseded by language/goblinoid/lexicon/root/<concept>) pre-Branches per-species goblin proto-root",
        ),
        (
            "language/<species>/lexicon/cascade",
            "the species' 2-4 rule sound-change cascade, applied by evolve() to every proto-root",
        ),
        (
            "language/<species>/lexicon/headedness",
            "the species' drawn compound-joining order (HeadFirst/HeadLast), gating LexEntry::Compound component order",
        ),
        (
            "language/<species>/grammar/constituent-order",
            "the tongue's drawn constituent order for predication (SOV/SVO dominant, per authored typological weights)",
        ),
        (
            "language/<species>/grammar/copula",
            "whether nominal predication carries an overt copula, and (when it does) the copula's one-syllable drawn form from the tongue's own phonology",
        ),
        (
            "language/<species>/grammar/articles",
            "whether the tongue has articles (floor: drawn but surfaces no article lexeme until the morphology campaign)",
        ),
        (
            "language/<species>/schema/<domain>/<fact-shape>",
            "C5's causal-schema draw (render-time, `schemas::select_schema`): the β-sharpened pick among the fact-shape's admitted schemas for one culture's account of one (source-domain, fact-shape) pair — `<domain>` and `<fact-shape>` are the salt legs (e.g. `sky`/`cyclic-event` for the day)",
        ),
        (
            "language/<species>/lexeme/<fact-key>",
            "C5's lexeme draw (render-time, `schemas::select_lexeme`): the uniform pick among a fired schema's gate-surviving verb candidates for one explained fact — `<fact-key>` salts by the ground fact's predicate (e.g. `day-length-std`)",
        ),
    ]
}

/// Language as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Language;

impl hornvale_kernel::Domain for Language {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}
