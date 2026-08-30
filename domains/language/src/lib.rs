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
/// The clause layer: a language-neutral `Clause` and the Common
/// realizer that turns it into a sentence. Generalizes the `render_line`
/// seam from a bespoke tenet spec to any clause.
pub mod clause;
/// Common's declared vocabulary (The Vernacular, Task 3): a TOTAL id→word map
/// for the author's out-of-world register, which — unlike a people's tongue —
/// has no speakers and so no `Lexicon`. Holds no domain's concept ids; each
/// domain exposes its own pairs and the composition root assembles them.
pub mod common_vocab;
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
/// Typology bundles (The Burr, Stage 3): which rules build a family's
/// words, as against which values a shared rule uses. Four authored rows,
/// keyed by family label exactly as `family_proto()` is keyed.
pub mod typology;

pub use accession::{EPOCH_COHORTS, concept_epoch};
pub use account::{
    Account, AccountEntry, AccountParams, Disposition, GroundFact, LossReason, NeededConcept,
    Observability, OrderPolicy, Requirement, Stance, account_of, distinctiveness, distortion,
    domain_distortion, identity_params, recoverability,
};
pub use clause::{
    Adjunct, AdjunctPosition, Argument, COPULA_PARADIGM, Clause, Coordination, CopulaRow,
    Definiteness, Number, PRONOUN_PARADIGM, ParseContext, ParseError, Person, Polarity,
    PronounCase, PronounRow, Subject, Tense, VERB_PARADIGM, Valence, VerbRow, common_pronoun,
    nominative_person, parse_common, parse_common_with_tail, predicate_valence, realize_common,
    realize_common_coordination, realize_common_polar_question, realize_common_verbless,
};
pub use common_vocab::{CommonVocabulary, MissingCommonWords};
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
    typ: &typology::Typology,
    concepts: &[&str],
    daughters: &[Daughter],
    epoch_of: impl Fn(&str) -> u32,
) -> std::collections::BTreeMap<String, Vec<Segment>> {
    etymology::assign_proto_roots_with_epoch(
        seed, family, proto_ph, typ, concepts, daughters, epoch_of,
    )
}
pub use exemplars::{HUE_CONCEPTS, hue_exemplar};
pub use grammar::{
    ConstituentOrder, TongueGap, TongueGrammar, TongueParadigm, realize_tongue,
    realize_tongue_coordination, realize_tongue_deep, realize_tongue_deep_coordination,
    realize_tongue_polar_question, tongue_grammar,
};
pub use lexicon::{
    ExposureClass, GapReason, Headedness, LexEntry, Lexicon, WordViews, build_lexicon,
    draw_headedness, proto_root_universe,
};
pub use morphology::{
    ClassPosition, Evidential, MorphDepth, MorphForm, NounClass, SKY_OVERRIDE, TongueMorphology,
    affix, morph_depths, morph_forms, noun_class_with_sky, pronoun_forms,
};
pub use naming::{
    GeneratedName, MorphOptions, NameCorpus, NameKind, NameShape, Namer, SiteConcepts, render_views,
};
pub use packs::{
    BEARINGS, PackDepths, PackEntry, action_suite_pack, bearing_compounds, body_pack, color_pack,
    compound_recipe, concept_domain, extradiegetic_pack, felt_state_pack, in_ladder,
    is_core_concept, is_extradiegetic, kin_pack, object_property_pack, register_concepts,
    universal_stratum,
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
pub use typology::{
    ALL_BUNDLE_NAMES, CodaLaw, Harmony, Morphology, OnsetLaw, Orthography, Typology,
    family_typology, typology_for,
};
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
        /// No exotic manner — the manikin's designated default (an exotic
        /// manner is a kind, not a quantity, so it has no neutral middle).
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
    /// `[0, 1]` with 0.5 ≡ the manikin's neutral midpoint (see
    /// [`ArticulationVector::MANIKIN`], whose `tonality` is instead the
    /// designated default 0.0 ≡ atonal); widening the vector requires its own
    /// campaign. Every dimension is authored — nothing drawn. Moved here from
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

    impl ArticulationVector {
        /// The manikin's voice: the reference articulation the phonology
        /// pipeline is framed against.
        ///
        /// The manikin is a body that is nobody — the model's reference
        /// figure, in the lineage of the CIE standard observer and ICRP's
        /// "standard man". It has no `KindId` and no registry row, so it can
        /// never be placed in a world and can never be heard speaking. The
        /// species crate's `MindVector::MANIKIN`, `SocietyVector::MANIKIN`
        /// and `PerceptionVector::MANIKIN` are the same construction on the
        /// other three vector families; this const is the language-owned
        /// fourth. (It is redeclared here rather than imported: a domain
        /// crate depends on the kernel and never on a sibling domain.)
        ///
        /// Note the asymmetry, which is real and not papered over. Five
        /// dimensions are scalars sitting at `0.5`, a principled **neutral
        /// midpoint**. `tonality` is also a scalar, and so does have a
        /// middle, but the reference vector deliberately does not sit at it:
        /// `0.0` (atonal) is a designated **default**, the value every
        /// shipped kind carries, not a neutral reading. `ExoticManner` has no
        /// middle at all, so `None` is a designated default in the stronger
        /// sense.
        /// type-audit: bare-ok(ratio)
        pub const MANIKIN: Self = Self {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.5,
            sibilance: 0.5,
            voice_loudness: 0.5,
            tonality: 0.0,
            exotic: ExoticManner::None,
        };
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
    /// component. Goblin's row happens to sit at
    /// [`ArticulationVector::MANIKIN`] — a fact about goblin's authorship,
    /// not about what the manikin is.
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
            // The Generalist (C2-0). Humans are authored at the envelope's
            // neutral settings, and this is the ONE vector family where that
            // is an argument rather than a default: the phonology envelope is
            // built on IPA, a human-calibrated inventory, so a human anchor
            // here is better founded than any other kind's (The Manikin §2).
            //
            // These values coincide with goblin's, which are legacy — goblin
            // sits at 0.5 because it was the baseline, not because anyone
            // decided goblins sound unremarkable. The collision is a known
            // artifact of a deferred campaign (goblin characterization) and
            // resolves when goblin moves, not when human does.
            (
                KindId("human"),
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
            // THE DELVERS (C2c): three daughters of one proto, and the first
            // family since goblinoid where that word means anything. Each row
            // below DIVERGES from `family_proto`'s `KindId("dwarf")` vector
            // (labiality 0.40, vowel_space 0.35, voicing 0.60, sibilance
            // 0.35, voice_loudness 0.60) — the divergence is precisely what
            // the sound-change cascade consumes, so three identical daughters
            // would be three names for one tongue.
            //
            // The proto reading is a low, back, consonant-heavy tongue:
            // narrow vowel space, moderate labiality, voiced, unsibilant,
            // carrying. Each daughter moves it in the direction its own
            // ecology pushes.
            (
                KindId("desert-dwarf"),
                ArticulationVector {
                    // long calls across open ground: the loudest daughter,
                    // with the sibilance a dry-air whistling register buys.
                    labiality: 0.35,
                    vowel_space: 0.50,
                    voicing: 0.55,
                    sibilance: 0.65,
                    voice_loudness: 0.80,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("gully-dwarf"),
                ArticulationVector {
                    // the most open and least conserved: no caste of speakers
                    // holds this tongue to anything, so it has drifted toward
                    // wide vowels, full voicing and volume.
                    labiality: 0.55,
                    vowel_space: 0.55,
                    voicing: 0.70,
                    sibilance: 0.50,
                    voice_loudness: 0.70,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("hill-dwarf"),
                ArticulationVector {
                    // the conservative daughter, closest to the proto on
                    // every dimension — the surface homeland's tongue, which
                    // is usually the one that moves least.
                    labiality: 0.45,
                    vowel_space: 0.45,
                    voicing: 0.65,
                    sibilance: 0.30,
                    voice_loudness: 0.55,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            // THE RADIATION (C2d): six daughters of one proto — the roster's
            // largest family, six against goblinoid's three and dwarf's three.
            // Each row below DIVERGES from `family_proto`'s `KindId("elf")`
            // vector (labiality 0.45, vowel_space 0.70, voicing 0.55,
            // sibilance 0.45, voice_loudness 0.35) on at least three
            // dimensions — the divergence is precisely what the sound-change
            // cascade consumes, so six identical daughters would be six names
            // for one tongue and P5's divergence clause would correctly read
            // false.
            //
            // The proto reading is a wide-vowelled, quiet, unremarkable-
            // consonant tongue: an expanded vowel space, moderate labiality
            // and sibilance, and the lowest `voice_loudness` of any proto in
            // the registry.
            //
            // **No row below is argued from its kind's environment, and none
            // may be.** The same hand authors the articulation and the niche,
            // so any correlation between them measures the authoring
            // convention and nothing else (spec §6). The rationales are
            // INSTITUTIONAL — who holds the tongue, and how tightly — which is
            // the same axis the goblinoid and dwarf rows already use.
            (
                KindId("desert-elf"),
                ArticulationVector {
                    // held by nobody in particular and spoken across
                    // distance: the family's loudest daughter, with the
                    // sibilance a carrying register buys.
                    labiality: 0.35,
                    vowel_space: 0.70,
                    voicing: 0.50,
                    sibilance: 0.65,
                    voice_loudness: 0.70,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("drow"),
                ArticulationVector {
                    // a rank-ordered society holds its tongue to a standard,
                    // and a standard compresses: the narrowest vowel space in
                    // the family and the only innovated exotic manner.
                    labiality: 0.30,
                    vowel_space: 0.35,
                    voicing: 0.40,
                    sibilance: 0.75,
                    voice_loudness: 0.30,
                    tonality: 0.0,
                    exotic: ExoticManner::Ejective,
                },
            ),
            (
                KindId("high-elf"),
                ArticulationVector {
                    // LANGUAGE IS HIGH'S THIRD IDENTITY CHANNEL, and the only
                    // one of the three that reaches a rendered product. A
                    // people whose institutions outlive its members keeps its
                    // tongue: this is the family's most conservative daughter,
                    // nearest the proto on every dimension, and deliberately
                    // NOT identical to it — a daughter that had not moved at
                    // all would be a proto with a second name.
                    labiality: 0.45,
                    vowel_space: 0.65,
                    voicing: 0.60,
                    sibilance: 0.40,
                    voice_loudness: 0.40,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("sea-elf"),
                ArticulationVector {
                    // a consensus people with no caste of speakers: full
                    // voicing, rounded, and the family's second-loudest.
                    labiality: 0.70,
                    vowel_space: 0.75,
                    voicing: 0.75,
                    sibilance: 0.30,
                    voice_loudness: 0.60,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            (
                KindId("snow-elf"),
                ArticulationVector {
                    // the family's quietest and least voiced, and the ROSTER'S
                    // FIRST `Click` — the one `ExoticManner` variant no kind
                    // had ever carried, so this row is what gives that arm of
                    // `exotic_seg_of` its first live witness. A small band
                    // that mostly speaks to people already beside it.
                    labiality: 0.55,
                    vowel_space: 0.45,
                    voicing: 0.35,
                    sibilance: 0.25,
                    voice_loudness: 0.25,
                    tonality: 0.0,
                    exotic: ExoticManner::Click,
                },
            ),
            (
                KindId("wood-elf"),
                ArticulationVector {
                    // custom without bureaucracy: nothing holds this tongue to
                    // a standard, so it has drifted furthest from the proto on
                    // labiality and sibilance while keeping the proto's wide
                    // vowels.
                    labiality: 0.20,
                    vowel_space: 0.80,
                    voicing: 0.65,
                    sibilance: 0.15,
                    voice_loudness: 0.45,
                    tonality: 0.0,
                    exotic: ExoticManner::Trill,
                },
            ),
        ]
        .into_iter()
        .collect()
    }

    /// Peopled lexicon, one per speaking kind. Byte-identical to the former
    /// species peopled component's noun + rung words for the six peoples;
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
            // The Generalist (C2-0): a settled agricultural people's rungs.
            (
                KindId("human"),
                Lexicon {
                    noun: "town",
                    worker_override: Some("farmer"),
                    warrior: "guard",
                    artisan: "wright",
                    shaman: "priest",
                    top: "steward",
                },
            ),
            // THE DELVERS (C2c): the dwarf family's three vocabularies. Each
            // kind's `top` rung names what its `SocietyVector.status_basis`
            // says earns standing — `loremaster` for Knowledge, `overseer`
            // for Rank, `eldest` for a Communal band — so the words and the
            // society model agree rather than merely coexisting.
            (
                KindId("desert-dwarf"),
                Lexicon {
                    noun: "waterhold",
                    worker_override: Some("well-tender"),
                    warrior: "outrider",
                    artisan: "glassmith",
                    // navigation by the sky is this people's real lore, and
                    // its 0.75 `sky_attention` is the same claim.
                    shaman: "starreader",
                    top: "waterwarden",
                },
            ),
            (
                KindId("gully-dwarf"),
                Lexicon {
                    noun: "midden",
                    worker_override: Some("scrounger"),
                    warrior: "cudgeler",
                    artisan: "patcher",
                    shaman: "mutterer",
                    // a Communal band's only authority is age.
                    top: "eldest",
                },
            ),
            (
                KindId("hill-dwarf"),
                Lexicon {
                    noun: "steading",
                    worker_override: Some("crofter"),
                    warrior: "hearthguard",
                    artisan: "smith",
                    shaman: "stonespeaker",
                    top: "thane",
                },
            ),
            // THE RADIATION (C2d): the elf family's six vocabularies, in the
            // same order and with the same six keys as `articulation_registry`
            // above — `check_integrity` requires the two stores to share ONE
            // key-set, so a row added to either without the other fails the
            // whole workspace at load.
            //
            // Each kind's `top` rung names what its `SocietyVector.status_basis`
            // says earns standing, as the dwarf block does: `Knowledge` reads
            // as lore, `Generosity` as what the holder sets out, `Rank` as
            // position held.
            (
                KindId("desert-elf"),
                Lexicon {
                    noun: "wellcamp",
                    worker_override: Some("waterfinder"),
                    warrior: "outrider",
                    artisan: "glassblower",
                    shaman: "skyreader",
                    // Knowledge: the route and the season are this people's
                    // capital, and whoever holds them holds standing.
                    top: "pathkeeper",
                },
            ),
            (
                KindId("drow"),
                Lexicon {
                    noun: "hold",
                    worker_override: Some("delver"),
                    warrior: "blademaster",
                    artisan: "webwright",
                    shaman: "dark-speaker",
                    // Rank: position held, and nothing else.
                    top: "matron",
                },
            ),
            (
                KindId("high-elf"),
                Lexicon {
                    noun: "citadel",
                    worker_override: Some("steward"),
                    warrior: "bladesinger",
                    artisan: "artificer",
                    shaman: "loresinger",
                    // Knowledge, held by an institution rather than a person —
                    // the rung word names the archive, not the archivist.
                    top: "archivist",
                },
            ),
            (
                KindId("sea-elf"),
                Lexicon {
                    noun: "haven",
                    worker_override: Some("netter"),
                    warrior: "wavewarden",
                    artisan: "hullwright",
                    shaman: "tidereader",
                    // Generosity: whoever fed the others through the lean
                    // season.
                    top: "provider",
                },
            ),
            (
                KindId("snow-elf"),
                Lexicon {
                    noun: "wintering",
                    worker_override: Some("herder"),
                    warrior: "iceguard",
                    artisan: "boneworker",
                    shaman: "frostspeaker",
                    // Generosity, the winter reading: the one who keeps the
                    // store and opens it.
                    top: "hearthkeeper",
                },
            ),
            (
                KindId("wood-elf"),
                Lexicon {
                    noun: "grove",
                    worker_override: Some("gatherer"),
                    warrior: "ranger",
                    artisan: "bowyer",
                    shaman: "greenspeaker",
                    // Generosity in a people with no hall: the word names what
                    // is given, not who governs.
                    top: "boughgiver",
                },
            ),
        ]
        .into_iter()
        .collect()
    }

    /// Proto ancestral articulation vectors keyed by family (goblinoid/
    /// draconic/plant/dwarf/elf) — moved here from species (ECS c3).
    ///
    /// Keyed by the FAMILY LABEL `hornvale_species::family_of` carries, not
    /// by a kind. `check_integrity` (`windows/worldgen/src/components.rs`)
    /// requires an entry here for every label held by two or more kinds, so a
    /// campaign that adds a second member to a family MUST add its proto in
    /// the same commit — The Delvers added three dwarves and this row
    /// together for exactly that reason.
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
                    // THE TONE TIER'S FIRST SHIPPED SPECIES (Task 14, The
                    // Burr). `draw_tone_inventory`, the capacity floor's
                    // tone-widening and `RuleKind::Tonogenesis` were built,
                    // unit-tested with synthetic envelopes, and unreached by
                    // any real species — `tonality` sat at 0.0 on all 23
                    // authored rows. Draconic is already the one family
                    // mapped to `typology::isolating_tonal()`
                    // (`family_typology`), so it is the natural first mover:
                    // a large, non-humanoid dragon vocal tract — long
                    // resonant chambers, no lips shaping the airstream the
                    // way a humanoid mouth does — is read here as carrying
                    // pitch contrast rather than the labial/dental place
                    // contrasts a humanoid tongue leans on. 0.7 clears
                    // `tone_count`'s 0.25 threshold for a second, contrastive
                    // tone (`1 + round(0.7 * 2) = 2`, clamped to
                    // `MAX_TONE_COUNT`), giving draconic a two-tone inventory
                    // (Neutral + one contrastive) — reaching the tier without
                    // maxing it: a deliberately modest first step, not the
                    // full three-tone ceiling.
                    tonality: 0.7,
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
            // THE DELVERS (C2c): proto-Dwarf, ancestor of all three daughters
            // in `articulation_registry`. A low, back, consonant-heavy tongue:
            // narrow vowel space, moderate lip rounding, well voiced, little
            // sibilance, and carrying — the reading a people that speaks over
            // stone and wind arrives at. `exotic: None` — the daughters
            // innovate away from the proto rather than losing an inherited
            // manner.
            (
                KindId("dwarf"),
                ArticulationVector {
                    labiality: 0.40,
                    vowel_space: 0.35,
                    voicing: 0.60,
                    sibilance: 0.35,
                    voice_loudness: 0.60,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
            // THE RADIATION (C2d): proto-Elvish, ancestor of all six daughters
            // in `articulation_registry` — the roster's largest family, six
            // against goblinoid's three and dwarf's three. Mandatory the moment
            // the label is carried twice: `check_integrity` requires a proto for
            // every family label held by >= 2 kinds, and this one is held by six,
            // so this row lands in the same commit as the `family_of` rows
            // because it must.
            //
            // The reading is the dwarf proto's opposite on the two dimensions
            // where the two families are most legibly distinct: an expanded
            // vowel space (0.70 against 0.35) and the quietest voice of any
            // proto (0.35 against 0.60). `exotic: None`, as every proto is —
            // the daughters innovate a manner away from the ancestor rather
            // than losing an inherited one, which is what makes drow's
            // `Ejective`, snow's `Click` and wood's `Trill` three independent
            // innovations rather than three survivals.
            //
            // **A STAR, NOT A TREE** (spec §8, LANG-53). With no time-since-
            // split, all six daughters are equidistant from this vector. The
            // model can say six tongues descend from proto-Elvish; it cannot say
            // Drow split before Snow, and there is no field in which that
            // sentence could be written. The tree stays blocked on
            // LANG-split-time-from-history, with this family as its motivating
            // case.
            (
                KindId("elf"),
                ArticulationVector {
                    labiality: 0.45,
                    vowel_space: 0.70,
                    voicing: 0.55,
                    sibilance: 0.45,
                    voice_loudness: 0.35,
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
/// `goblin`, `kobold`) and the per-entity salt leg (the settlement vertex id,
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
            "(retired at The Words, superseded by name/settlement/v2) per-settlement name (salted by vertex id): a bare stem",
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
            "language/<species>/name/person",
            "(The Particular, Task 2) per-founder name: a bare stem, like the settlement v1 draw above. Not an epoch of `name/settlement` — a fourth, disjoint `NameKind`, so it consumes nothing from any existing stream",
        ),
        (
            "language/<species>/name/landform",
            "(The Repose, Task 3) per-landform name, keyed at the composition root by (seed, vertex, species) rather than by the landform's own identity — one landform has many names, one per people with a word for it: a bare 2-3 syllable stem, like the settlement/person v1 draw above. A fifth, disjoint `NameKind` — no epoch suffix, since this label is new rather than a regeneration (decision 0084)",
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
            "language/<family>/lexicon/root/v4/<concept>",
            "per-concept family proto-root, injectively and MERGER-AWARELY assigned (epoch root/v4): the open-addressing draw also rejects a core candidate whose evolved form would merge with an already-placed core concept in any daughter, so core homophony is zero; family == species for a singleton stock. Probe re-draws key a /probe/<n> sub-stream. The Burr: an alveolar trill is no longer gated behind the exotic-consonant capability (a decision recorded at this campaign's close), which inserts extra candidate-consonant draws ahead of every species' inventory and so reseeds every root, not only the trill-drawing ones",
        ),
        (
            "language/<family>/lexicon/root/v3/<concept>",
            "(retired at The Burr, superseded by root/v4) the merger-aware assignment as v4, before the trill's exotic gate was lifted",
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
            "language/<species>/grammar/subordinator",
            "whether an embedded clause is marked with an overt complementizer, and (when it is) the complementizer's one-syllable drawn form from the tongue's own phonology — a tongue that draws none subordinates by bare parataxis, a legitimate grammar and not a gap (The Mortise, Task 5, spec §4.6)",
        ),
        (
            "language/<species>/grammar/conjunction",
            "whether coordinated clauses are joined with an overt coordinating conjunction, and (when they are) the conjunction's one-syllable drawn form from the tongue's own phonology — a tongue that draws none coordinates by bare juxtaposition, a legitimate grammar and not a gap (The Mortise, Task 6, spec §4.10). A function word earns this label because its PRESENCE is typological, not lexical; a vocabulary word costs zero labels, drawn instead as a `dynamic(concept)` value on the existing `lexicon/root` axis",
        ),
        (
            "language/<species>/grammar/interrogative",
            "whether a polar question is marked with an overt free particle, and (when it is) the particle's one-syllable drawn form from the tongue's own phonology, skewed toward absent — a tongue that draws none questions by INTONATION, the cross-linguistic majority strategy (Ultan 1978; Dryer, WALS 116), which a text renderer cannot show, so it questions by a transcription convention instead: its declarative surface plus `?` (The Rail, Task 9, spec §4)",
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
            "language/<species>/grammar/depth/polarity",
            "The Inquest: the species' drawn Polarity grammaticalization depth (None/Particle/Affix) — how a tongue marks a negated clause; an independent stream, added additively (spec §3.4)",
        ),
        (
            "language/<species>/grammar/polarity-position",
            "The Inquest: which side of the marked word the Polarity affix binds",
        ),
        (
            "language/<species>/grammar/depth/person",
            "The Rail (Task 7): the species' drawn Person (subject-agreement) grammaticalization depth (None/Particle/Affix), independent of number/tense/polarity — a tongue's own take on how deeply it grammaticalizes person, distinct from Common's fixed rules",
        ),
        (
            "language/<species>/grammar/person-position",
            "The Rail (Task 7): which side of the marked word the Person affix binds",
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
            "language/family/<family>/morph/polarity/negative",
            "The Inquest: the family's Negative affix proto-form, shared by every daughter — negative is the marked member and positive is zero, so no positive form is ever drawn",
        ),
        (
            "language/family/<family>/morph/pronoun/<person-number>",
            "The Inquest: the family's personal-pronoun proto-form for one person-number slot, drawn once per family and evolved per daughter via its own cascade — the cognate law. Six slots, `<person-number>` ranging over 1sg/2sg/3sg/1pl/2pl/3pl: person and number ONLY, no gender, because nothing in the ledger assigns grammatical gender. Written with a placeholder leg rather than six rows on the same precedent the multi-valued morph/evidential/<value> and morph/class/<value> rows above set; the singular number/tense/polarity rows are spelled out because each of those axes draws exactly ONE marked member",
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

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::KindId;

    /// CHARACTERIZATION, NOT CONTRACT.
    ///
    /// Goblin is currently authored at exactly
    /// [`ArticulationVector::MANIKIN`]. That is authorship, not definition:
    /// goblin was the first people written down, and nobody ever decided
    /// that goblins speak with the reference voice. Nothing in the model
    /// requires a kind to sit on the manikin, and this test does not make it
    /// a requirement.
    ///
    /// It exists so that characterising goblin's phonology — giving it a
    /// voice that is its own rather than the reference figure's — arrives as
    /// a visible diff on this test rather than as a silent shift baked into
    /// the doc comment's unpinned claim. When that campaign comes, DELETE
    /// this test; do not "fix" it.
    ///
    /// The sibling in `hornvale-species`,
    /// `goblin_is_currently_authored_at_the_manikin`, does the same for
    /// mind/society/perception; this is the fourth vector family's half,
    /// redeclared here rather than shared because a domain crate depends on
    /// the kernel and never on a sibling domain.
    #[test]
    fn goblin_is_currently_authored_at_the_manikin() {
        let row = articulation_registry()
            .get(&KindId("goblin"))
            .copied()
            .unwrap();
        assert_eq!(
            row,
            ArticulationVector::MANIKIN,
            "goblin's articulation is authored at the manikin (characterization)"
        );
    }

    /// The isolating-tonal bundle must actually reach the tone tier — the
    /// whole reason it earned a slot is that `tonality` is 0.0 on all 23
    /// authored rows today, so `draw_tone_inventory`, the capacity floor's
    /// tone-widening and `RuleKind::Tonogenesis` are built, tested and
    /// unreached (spec §3.4). Draconic is already mapped to
    /// `typology::isolating_tonal()` in `family_typology`; this test proves
    /// the *phonology* side (the proto's `tonality` scalar) actually reaches
    /// a contrastive tone, not just the typology label.
    ///
    /// The envelope is built INLINE at the neutral midpoint (the same values
    /// `phonology::tests::manikin_env()` uses) rather than via a helper: the
    /// real species→envelope conversion (`envelope_of`) lives in
    /// `windows/worldgen`, a window, and this domain crate may depend on the
    /// kernel only — never a window (layering, `cli/tests/architecture.rs`).
    /// `tone_count` depends on `tonality` alone, so a hand-built envelope
    /// with the proto's `tonality` copied in is sufficient to prove the
    /// value reaches the tier.
    /// The stream roster is HAND-MAINTAINED and nothing checks it for
    /// completeness, so a draw added without its row ships a silently
    /// incomplete manifest with every gate green. This pins the one row The
    /// Inquest's pronoun draw adds; it is a spot check, not the missing
    /// completeness test.
    #[test]
    fn stream_labels_declare_the_pronoun_draw() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        assert!(
            labels.contains(&"language/family/<family>/morph/pronoun/<person-number>"),
            "the pronoun proto draw is missing from the stream roster"
        );
    }

    #[test]
    fn the_draconic_family_draws_a_contrastive_tone() {
        let proto = family_proto();
        let draconic = proto.get(&KindId("draconic")).expect("draconic proto");
        assert!(
            draconic.tonality > 0.0,
            "draconic tonality is still 0.0 — the tone tier stays unreached"
        );
        let env = Envelope {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.5,
            sibilance: 0.5,
            voice_loudness: 0.5,
            tonality: draconic.tonality,
            exotic: ExoticSeg::None,
        };
        let ph = draw_phonology(
            &hornvale_kernel::Seed(42),
            "draconic",
            &env,
            &crate::typology::isolating_tonal(),
        );
        assert!(
            tone_inventory(&ph).len() > 1,
            "draconic drew only the neutral tone: {:?}",
            tone_inventory(&ph)
        );
    }
}
