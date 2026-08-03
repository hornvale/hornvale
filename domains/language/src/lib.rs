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
pub mod accession;
pub mod account;
/// The anthroponymic schema (The Namesake, spec §3.2): what a personal name
/// is made of — an ordered list of `(source, author)` elements, plus the new
/// [`naming::NameKind::Person`] seed path a given name draws off. Plain data,
/// kernel-only; it never learns which people a name belongs to.
pub mod anthroponym;
/// The clause layer: a language-neutral `ClauseSpec` and the Common
/// realizer that turns it into a sentence. Generalizes the `render_line`
/// seam from a bespoke tenet spec to any clause.
pub mod clause;
/// The etymology engine: proto-roots drawn from a phonology, and a drawn
/// cascade of sound-change rules (`evolve`, pure and total, Neogrammarian)
/// that turns a proto-root into its modern form.
pub mod etymology;
/// Canonical exemplar reflectances for the colour lexicon's hue ladder —
/// what a colour term is compared *against*, stored as a reflectance so it
/// passes through the same illuminant and the same eye as the sample.
pub mod exemplars;
/// A tongue's drawn surface grammar (C3, The Tongues): constituent order,
/// copula presence and drawn form, and article presence — the floor slice
/// of LANG-40's grammaticalization-depth vector.
pub mod grammar;
/// The lexicon: two-pass assembly over a culture's concept exposures —
/// `Steeped` concepts become roots (Task 6's `proto_root`/`evolve`),
/// `KnowsOf` concepts become recipe compounds joined under a drawn
/// `Headedness`, and everything else is a reasoned `Gap`.
pub mod lexicon;
/// C7 (The Deep Grammar) morphology substrate: the depth vector, family-
/// cognate evidential/noun-class marker proto-forms evolved per daughter,
/// and segment-level affixation — the primitives `grammar`'s
/// `realize_tongue_deep` assembles.
pub mod morphology;
/// The naming grammars: stems and kind-keyed morphology, a single
/// deterministic draw per `(seed, species, kind, salt)` (no re-draw), built
/// from a drawn phonology.
pub mod naming;
/// LANG-44: numeracy as a per-listener quantity register — the per-species
/// drawn numeral-system rung and the shared render codec both a speaker's
/// own rendering and a listener's comprehension degradation reuse.
pub mod numeracy;
/// The Swadesh packs: authored core vocabulary (the universal stratum,
/// color, body, and kin packs), Berlin & Kay acquisition ladders, the
/// closed compound-recipe table, and `register_concepts`.
pub mod packs;
/// LANG-43: paradigm slots (Number, Tense) whose cascade-native form can
/// diverge from a mechanically-regular one.
pub mod paradigm;
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
/// Seed-derivation labels for this crate (PROC-17): the centralized
/// `StreamLabel` constants every draw site derives through.
pub mod streams;

pub use accession::{EPOCH_COHORTS, concept_epoch};
pub use account::{
    Account, AccountEntry, AccountParams, Disposition, GroundFact, LossReason, NeededConcept,
    Observability, OrderPolicy, Requirement, Stance, account_of, distinctiveness, distortion,
    domain_distortion, identity_params, recoverability,
};
pub use clause::{
    ClauseSpec, Definiteness, Frame, Number, ParseContext, ParseError, parse_common, realize_common,
};
pub use etymology::{
    AppliedRule, Cascade, CascadeRegime, Daughter, Derivation, RuleKind, SoundRule,
    assign_proto_roots, draw_cascade, draw_cascade_with_regime, draw_wear_cascade, evolve,
    proto_root,
};

/// Test-only door into [`etymology::assign_proto_roots_with_epoch`], whose
/// injected `epoch_of` lets a property test exercise the accession-epoch
/// carve (LANG-55) over a synthetic concept universe — the real table is a
/// `const`, and a test cannot append a cohort to it. `#[doc(hidden)]` rather
/// than widening `assign_proto_roots_with_epoch` itself to `pub`: the real
/// function stays `pub(crate)`, with `assign_proto_roots` (fixed to
/// [`accession::concept_epoch`]) as its only production entry point.
/// type-audit: bare-ok(identifier-text)
#[doc(hidden)]
pub fn assign_proto_roots_with_epoch_for_test(
    seed: &hornvale_kernel::Seed,
    family: &str,
    proto_ph: &Phonology,
    concepts: &[&str],
    daughters: &[Daughter],
    epoch_of: impl Fn(&str) -> u32,
) -> std::collections::BTreeMap<String, Vec<Segment>> {
    etymology::assign_proto_roots_with_epoch(seed, family, proto_ph, concepts, daughters, epoch_of)
}
pub use exemplars::{HUE_CONCEPTS, hue_exemplar};
pub use grammar::{
    ConstituentOrder, TongueClause, TongueGap, TongueGrammar, realize_tongue, realize_tongue_deep,
    tongue_grammar,
};
pub use lexicon::{
    ExposureClass, GapReason, Headedness, LexEntry, Lexicon, WordViews, build_lexicon,
    draw_headedness,
};
pub use morphology::{
    ClassPosition, Evidential, MorphDepth, MorphForm, NounClass, TongueMorphology, affix,
    morph_depths, morph_forms,
};
pub use naming::{
    GeneratedName, MorphOptions, NameCorpus, NameKind, NameShape, Namer, SiteConcepts, render_views,
};
pub use packs::{
    BEARINGS, PackDepths, PackEntry, bearing_compounds, body_pack, color_pack, compound_recipe,
    concept_domain, in_ladder, is_core_concept, kin_pack, register_concepts, universal_stratum,
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
    ConflictState, FactShape, LexemeId, Schema, SchemaId, SlotKind, SourceDomain, SubFrame,
    admitted, conflict_of, lexemes_for, schema_table, select_lexeme, select_schema,
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
            // The Solitary Tongue: the three chromatic dragons speak a shared
            // Draconic tongue (per-chromatic differentiation is deferred —
            // one vector for white/red/black-dragon). Harsh and hissing: a
            // high sibilance and loud voice from a huge, reptilian throat, a
            // wide-open vowel space and low labiality (dragons have no lips
            // to round), moderately voiced, and atonal like the peoples.
            // `ExoticManner` offers only Trill/Click/Ejective — none reads as
            // a hiss/growl (the high `sibilance` already carries the hiss),
            // so this stays `None`, matching the "draconic" `family_proto`
            // entry below.
            (
                KindId("white-dragon"),
                ArticulationVector {
                    labiality: 0.2,
                    vowel_space: 0.4,
                    voicing: 0.7,
                    sibilance: 0.9,
                    voice_loudness: 0.9,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("red-dragon"),
                ArticulationVector {
                    labiality: 0.2,
                    vowel_space: 0.4,
                    voicing: 0.7,
                    sibilance: 0.9,
                    voice_loudness: 0.9,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("black-dragon"),
                ArticulationVector {
                    labiality: 0.2,
                    vowel_space: 0.4,
                    voicing: 0.7,
                    sibilance: 0.9,
                    voice_loudness: 0.9,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            // The Vacancy (T9): the fifth people. Follows kobold's singleton-
            // family shape — a species crate `family_of` entry ("gnoll") with
            // no `family_proto` counterpart, so this row is the gnoll tongue's
            // whole articulation, not a daughter of a shared ancestral vector.
            // A long muzzle constrains lip rounding (low labiality) and
            // vowel space; loud whooping/yipping calls carry across open
            // desert distances (high voice_loudness), a real behavioural
            // trait of the pack-hunting canid/hyena body plan this kind's
            // condition niche and biosphere already commit to.
            (
                KindId("gnoll"),
                ArticulationVector {
                    labiality: 0.35,
                    vowel_space: 0.35,
                    voicing: 0.6,
                    sibilance: 0.55,
                    voice_loudness: 0.85,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
        ]
        .into_iter()
        .collect()
    }

    /// Peopled lexicon, one per speaking kind. Byte-identical to the former
    /// species peopled component's noun + rung words for the four peoples;
    /// The Solitary Tongue adds a shared stopgap row for the three
    /// dragons (a solitary hoarder has no settlement or castes — these
    /// words are placeholders satisfying the `articulation.ids ==
    /// lexicon.ids` invariant, exercised only if a dragon is ever placed).
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
            // The Solitary Tongue: a shared stopgap Draconic vocabulary — a
            // solitary hoarder's "settlement" is its hoard, not a village;
            // there is no warrior/artisan/shaman caste, only the hoard's one
            // dweller, so every rung word names the same wyrm. Identical for
            // all three chromatics (per-chromatic differentiation deferred).
            (
                KindId("white-dragon"),
                Lexicon {
                    noun: "hoard",
                    worker_override: None,
                    warrior: "wyrm",
                    artisan: "wyrm",
                    shaman: "wyrm",
                    top: "wyrm",
                },
            ),
            (
                KindId("red-dragon"),
                Lexicon {
                    noun: "hoard",
                    worker_override: None,
                    warrior: "wyrm",
                    artisan: "wyrm",
                    shaman: "wyrm",
                    top: "wyrm",
                },
            ),
            (
                KindId("black-dragon"),
                Lexicon {
                    noun: "hoard",
                    worker_override: None,
                    warrior: "wyrm",
                    artisan: "wyrm",
                    shaman: "wyrm",
                    top: "wyrm",
                },
            ),
            // The Vacancy (T9): the fifth people's stopgap vocabulary.
            (
                KindId("gnoll"),
                Lexicon {
                    noun: "camp",
                    worker_override: Some("gleaner"),
                    warrior: "hunter",
                    artisan: "fletcher",
                    shaman: "bonecaster",
                    top: "packlord",
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
            "(retired at The Wearing, superseded by name/settlement/v3) the glossed settlement name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above, PLUS a per-salt 2-3 syllable drawn stem that v3 retires",
        ),
        (
            "language/<species>/name/deity/v2",
            "(retired at The Wearing, superseded by name/deity/v3) the glossed deity name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above",
        ),
        (
            "language/<species>/name/epithet/v2",
            "(retired at The Wearing, superseded by name/epithet/v3) the glossed epithet (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the v1 draw above",
        ),
        (
            "language/<species>/name/settlement/v3",
            "the glossed settlement name (The Wearing): composed from the lexicon's roots/compounds under the species' drawn headedness, each morpheme first worn to its frequency in this culture's own name corpus. The epoch bump is owed to two changes in what this stream consumes — the wear, and the RETIREMENT of v2's per-salt drawn stem (decision 0024: uniqueness is reference-time, and no future work fixes collisions by adding entropy)",
        ),
        (
            "language/<species>/name/person",
            "the given-name element of a personal name (The Namesake), salted by the bearer's role handle: a bare 2-3 syllable stem. No epoch suffix — this label is new, not a regeneration of an existing one (decision 0084: an epoch is declared only when a derivation moved)",
        ),
        (
            "language/<species>/name/deity/v3",
            "the glossed deity name (The Wearing): as v2, reseeded by the epoch bump the settlement stream owes. Deity names carry no name corpus (their space is one-per-belief, not a scatter), so nothing wears here",
        ),
        (
            "language/<species>/name/epithet/v3",
            "the glossed epithet (The Wearing): as v2, reseeded by the epoch bump. No name corpus, so nothing wears here",
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
            "language/<species>/lexicon/cascade/v2",
            "the species' 2-4 rule sound-change cascade, applied by evolve() to every proto-root. The Witness (2026-07-30) epoch bump: draw_rule is position-aware, offering Tonogenesis only once a prior ClusterSimplify/FinalLoss has been drawn — a leading Tonogenesis is provably the identity (evolve opens with no pending conditioning), so drawing it unconditioned wasted the roster slot on every world. Task 8b (same unreleased v2 epoch, per decision 0089: it lands together, so one suffix is the truthful count) adds a second, orthogonal gate: draw_rule now also checks the SPECIES' OWN drawn phonology (via draw_cascade_with_regime/draw_wear_cascade's new Phonology parameter) and drops Tonogenesis unless the phonology can host a toned vowel, and drops VowelShift unless it admits an adjacent-height vowel pair — a cascade may not draw a rule its phonology cannot host, one level up from Task 7's cannot-condition guard. Draw count is unchanged either way (Stream::pick is one draw at any slice length); only the drawn values move",
        ),
        (
            "language/<species>/lexicon/cascade/v2/wear",
            "the species' 1-2 rule TOPONYMIC WEAR cascade (The Wearing), run over a name morpheme whose share of this culture's names reaches the wear floor. A leg of its own, deliberately: drawn from lexicon/cascade directly it is a strict PREFIX of the historical cascade above, whose own output the lexicon's modern forms already are, so every rule would re-apply to its own fixpoint (measured on seed 42: 154 of 154 applications changed nothing). Reseeded by the same v2 epoch bump as its parent leg",
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
            "language/<species>/grammar/depth/evidential",
            "C7's depth vector: how deeply evidentiality grammaticalizes (None/Particle/Affix, weighted [60,25,15])",
        ),
        (
            "language/<species>/grammar/depth/noun-class",
            "C7's depth vector: how deeply noun class grammaticalizes (None/Particle/Affix, weighted [55,15,30])",
        ),
        (
            "language/<species>/grammar/class-position",
            "C7: which side of the noun the class marker binds when noun-class depth is Particle/Affix (prefix 40 / suffix 60)",
        ),
        (
            "language/<species>/grammar/depth/number",
            "The Residue: the species' drawn Number grammaticalization depth (None/Particle/Affix), independent of evidentiality/noun-class",
        ),
        (
            "language/<species>/grammar/depth/tense",
            "The Residue: the species' drawn Tense grammaticalization depth (None/Particle/Affix)",
        ),
        (
            "language/<species>/grammar/number-position",
            "The Residue: which side of the marked word the Number affix binds",
        ),
        (
            "language/<species>/grammar/tense-position",
            "The Residue: which side of the marked word the Tense affix binds",
        ),
        (
            "language/family/<family>/morph/evidential/<value>",
            "C7: the family's one-syllable evidential-marker proto-form for <value> (witnessed/taught/inferred), drawn once per family and evolved per daughter via its own cascade — the cognate law",
        ),
        (
            "language/family/<family>/morph/class/<value>",
            "C7: the family's one-syllable noun-class-marker proto-form for <value> (animate/inanimate), drawn once per family and evolved per daughter via its own cascade — the cognate law",
        ),
        (
            "language/family/<family>/morph/number/plural",
            "The Residue: the family's Plural affix proto-form, shared by every daughter (family-cognate law)",
        ),
        (
            "language/family/<family>/morph/tense/past",
            "The Residue: the family's Past-tense affix proto-form, shared by every daughter",
        ),
        (
            "language/<species>/grammar/numeracy-rung",
            "The species' drawn numeral-system rung (Subitizing/FullCounting/Decimals) — how far counting words go past the universal subitizing floor",
        ),
        (
            "language/<species>/schema/<domain>/<fact-shape>",
            "C5's causal-schema draw (render-time, `schemas::select_schema`): the β-sharpened pick among the fact-shape's admitted schemas for one culture's account of one (source-domain, fact-shape) pair — `<domain>` and `<fact-shape>` are the salt legs (e.g. `sky`/`cyclic-event` for the day)",
        ),
        (
            "language/<species>/lexeme/<fact-key>",
            "C5's lexeme draw (render-time, `schemas::select_lexeme`): the uniform pick among a fired schema's gate-surviving verb candidates for one explained fact — `<fact-key>` salts by the ground fact's predicate (e.g. `day-length-std`)",
        ),
        (
            "language/<species>/doctrine-schema/<domain>/<fact-shape>",
            "C6's institutional causal-schema draw (The Doctrine, render-time, `schemas::select_schema` again): the β-sharpened pick among the fact-shape's admitted schemas for the SAME culture's doctrine account (folk prior × the authored mediation column, before β) — a sibling stream to `schema/<domain>/<fact-shape>` above, never a shared draw, so the folk and doctrine schema picks can diverge independently",
        ),
        (
            "language/<species>/doctrine-lexeme/<fact-key>",
            "C6's institutional lexeme draw (The Doctrine, render-time, `schemas::select_lexeme` again): the uniform pick among a fired doctrine schema's gate-surviving verb candidates for one explained fact — a sibling stream to `lexeme/<fact-key>` above, salted the same way (the ground fact's predicate)",
        ),
        (
            "language/<species>/schema/sky/<shape>/<predicate>",
            "The Consonance: schema selection for a fact sharing FactShape::CyclicEvent with another predicate (moon-period-ratio vs day-length-std) — the extra predicate leg keeps their streams distinct",
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
