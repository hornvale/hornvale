//! The Swadesh packs: the authored core vocabulary a culture's language
//! draws its root words from. A pack is a flat list of [`PackEntry`]
//! values; `register_concepts` folds every entry not already owned by
//! another domain into the [`ConceptRegistry`] under domain `"language"` —
//! **the pack refers, the owner registers** (astronomy's sun/moon/star/
//! night, terrain's stone/mountain/sea, and so on already have a home; this
//! module lists them so the lexicon can name them, but never re-registers
//! them).
//!
//! Two ladders (Berlin & Kay's basic color term acquisition order) gate how
//! much of [`color_pack`] a culture has lexicalized, keyed by
//! [`PackDepths`] and read with [`in_ladder`]. [`compound_recipe`] is the
//! closed authored table of KNOWS-OF compounds: concepts with no root word
//! of their own, expressed instead as a modifier+head compound built from
//! two rooted concepts (e.g. `sea` = "many water").
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptDef, ConceptKind, ConceptRegistry, Correspondent, Lexicalization, Manifest, PerceptKind,
    RegistryError, Void,
};

/// The `eat` concept's id.
///
/// Named once so the pack entry that REGISTERS the concept and the Common
/// construction that REALIZES it cannot drift apart — the same discipline
/// `hornvale_kernel::world::IS_A` carries for the classification, applied at
/// the layer that actually owns this concept. A future epoch bump must break
/// the render rather than recompile cleanly and panic at every call site.
/// type-audit: bare-ok(identifier-text)
pub const EAT: &str = "eat";

/// The `kill` concept's id.
///
/// Named for the same reason [`EAT`] is: the pack entry that REGISTERS the
/// concept and the `clause::PREDICATE_VALENCE` row that REALIZES it must
/// not drift apart, and a future epoch bump must break the render
/// rather than recompile cleanly and panic at every call site.
///
/// **It is the causative of the core `die`, and it creates no obligation to
/// implement combat.** `ConceptKind::Act`'s reconciliation runs in exactly
/// one direction — `cli/src/concepts.rs`'s `orphan_acts` walks
/// `Action::all()` and reports acts that no concept names, so it is
/// structurally blind to a concept with no action. `kill` enters as
/// vocabulary: the world gains the ability to *say* it long before anything
/// can *do* it.
/// type-audit: bare-ok(identifier-text)
pub const KILL: &str = "kill";

/// The `know` concept's id.
///
/// Named for the same reason [`EAT`] and [`KILL`] are: the pack row that
/// REGISTERS the concept (in [`action_suite_pack`]) and the
/// `clause::PREDICATE_VALENCE` row that REALIZES it must not drift apart.
/// Before this constant existed the pack row was a bare `"know"` literal and
/// `PREDICATE_VALENCE` had no row for it at all — `realize_common` panicked
/// with "Common has no construction for predicate \"know\"" the moment a
/// caller tried, which is the red `cli/tests/suite/sentence_corpus.rs`'s
/// `every_covered_entry_realizes_in_common` witness exists to catch.
/// type-audit: bare-ok(identifier-text)
pub const KNOW: &str = "know";

/// The `think` concept's id.
///
/// Named for the same reason [`EAT`], [`KILL`], and [`KNOW`] are: the pack
/// row that REGISTERS the concept (in [`universal_stratum`]) and the
/// `clause::PREDICATE_VALENCE` row that REALIZES it must not drift apart.
///
/// **Placed in the universal stratum, not beside [`KNOW`] in
/// [`action_suite_pack`].** `know`'s exposure-gated placement rests on an
/// UNRESOLVED question — `action_suite_pack`'s own doc says a culture's
/// exposure to literacy, cartography, or reading another's state is a
/// question it does not resolve (registry row
/// `LANG-in-character-acts-are-unspeakable`). `think` has no such gate:
/// the same reasoning [`KILL`]'s doc gives — no biome, climate, or
/// perception ladder a people's word for a private mental act could hang
/// off — applies to `think` and not to `know`. The consequence is a real,
/// named asymmetry, not an oversight: *"I think…"* (m09) realizes in every
/// tongue; *"I don't know…"* (m06) still gaps in all of them, because
/// `know` was deliberately left where it is (out of scope for this task —
/// moving it belongs to a future campaign, would shift exposure and
/// byte-goldens, and is not this decision to make).
/// type-audit: bare-ok(identifier-text)
pub const THINK: &str = "think";

/// The `sleep` concept's id.
///
/// Named for the same reason [`EAT`], [`KILL`], [`KNOW`] and [`THINK`] are:
/// the pack row that REGISTERS the concept and the
/// `clause::PREDICATE_VALENCE` row that REALIZES it must not drift apart.
///
/// **It adds no pack entry.** `sleep` has been in [`universal_stratum`]
/// since long before this campaign — it is Swadesh-core and needs no
/// exposure gate — so this constant names an existing registration rather
/// than creating one. That is why The Rail registers no concept and moves
/// no keystone golden.
/// type-audit: bare-ok(identifier-text)
pub const SLEEP: &str = "sleep";

/// The `old` concept's id.
///
/// Named for the same reason [`EAT`], [`KILL`], [`KNOW`], [`THINK`] and
/// [`SLEEP`] are: the pack row that REGISTERS the concept and the
/// `clause::PREDICATE_VALENCE` row that REALIZES it must not drift apart.
///
/// **It adds no pack entry, the same way [`SLEEP`]'s doc explains for
/// itself.** `old` has been in [`universal_stratum`] since long before this
/// campaign (`ConceptKind::Quality`, alongside `new`, `great`, `high`,
/// `low` and `little`), so this constant names an existing registration
/// rather than creating one.
///
/// **It stands in for the rung's own `long`.** `r003`'s authored text is
/// *"The road is long"*, and `long` is not a registered concept anywhere in
/// this crate — registering it would move `world-seed-42.json`, a
/// byte-golden `make rebaseline` cannot write, and Task 0 established that
/// this campaign registers no concept. `old` is the substitution, recorded
/// again at its witness (`cli/tests/suite/sentence_corpus.rs`'s
/// `ladder_construction`, `"r003"` arm) the same way `r006`'s witness
/// records `kill` standing in for the unregistered `strike`.
/// type-audit: bare-ok(identifier-text)
pub const OLD: &str = "old";

/// The `under` concept's id.
///
/// Named for the same reason [`EAT`], [`KILL`], [`KNOW`], [`THINK`],
/// [`SLEEP`] and [`OLD`] are: the pack row that REGISTERS the concept and
/// the `clause::PREDICATE_VALENCE` row that REALIZES it must not drift
/// apart.
///
/// **It adds no pack entry, the same way [`SLEEP`]'s and [`OLD`]'s docs
/// explain for themselves.** `under` has been in [`universal_stratum`]
/// since long before this campaign (`ConceptKind::Quality`, doc "beneath;
/// below"), so this constant names an existing registration rather than
/// creating one.
///
/// **It stands in for the rung's own `at`.** `r005`'s authored text is
/// *"The merchant is at the gate."*, and neither `at` nor `gate` is a
/// registered concept anywhere in this crate — registering either would
/// move `world-seed-42.json`, a byte-golden `make rebaseline` cannot write,
/// and Task 0 established that this campaign registers no concept. `under`
/// and `tree` are the substitution, recorded again at their witness
/// (`cli/tests/suite/sentence_corpus.rs`'s `ladder_construction`, `"r005"`
/// arm) the same way `r003`'s witness records `old` standing in for the
/// unregistered `long`, and `r006`'s records `kill` for `strike`.
///
/// **Its registry KIND is a second, quieter compromise, recorded here so it
/// is not rediscovered.** `under` is a `ConceptKind::Quality` — the same
/// kind [`OLD`] carries, and [`OLD`] is the crate's only
/// `Valence::Property` predicate. An adposition is not a quality, and in a
/// registry designed around this crate's needs it would carry a kind of its
/// own; it carries `Quality` because the no-new-concept constraint above
/// forced the choice to be made from the kinds `universal_stratum` already
/// had, and `Quality` was the nearest. **Nothing checks kind against
/// valence.** The only thing separating a locative relation from a property
/// word in this crate is `clause::PREDICATE_VALENCE`'s two rows, so a
/// reader who infers valence from `ConceptKind` will infer it wrongly for
/// exactly this concept. A campaign free to move `world-seed-42.json` could
/// register an adposition kind and delete this paragraph.
/// type-audit: bare-ok(identifier-text)
pub const UNDER: &str = "under";

/// One entry in a vocabulary pack: a concept id, its broad category, a doc,
/// and its rank on whichever acquisition ladder it belongs to (0 for
/// entries outside any ladder — always in the lexicon once the pack is
/// registered).
/// type-audit: bare-ok(identifier-text: concept), bare-ok(prose: doc), bare-ok(count: ladder_rank)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackEntry {
    /// The concept id (the registry key, and the compound-recipe key).
    pub concept: &'static str,
    /// The broad category this concept belongs to.
    pub kind: ConceptKind,
    /// Human-readable description, passed through to the concept registry.
    pub doc: &'static str,
    /// Rank on this entry's acquisition ladder ([`color_pack`]'s hue or
    /// luminance ladder); `0` for unranked entries, which are always in the
    /// lexicon regardless of depth.
    pub ladder_rank: u8,
}

/// The universal stratum: the handful of concepts every culture names
/// first, regardless of climate, biome, or species — water, stone, sky
/// bodies, fire, wind, the ground, a tree, the basic acts of living and
/// dying, one's own name, and the first few counts. Some of these concepts
/// are owned by other domains (`sun`, `moon`, `star`, `night` by astronomy;
/// `stone` by terrain) and are listed here only so the lexicon can refer to
/// them; `register_concepts` skips re-registering them.
pub fn universal_stratum() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Substance;
    &[
        PackEntry {
            concept: "water",
            kind: KIND,
            doc: "the drinkable liquid",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "stone",
            kind: ConceptKind::Substance,
            doc: "rock",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "sun",
            kind: ConceptKind::Celestial,
            doc: "the sun",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "moon",
            kind: ConceptKind::Celestial,
            doc: "a moon",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "star",
            kind: ConceptKind::Celestial,
            doc: "a fixed point of light in the night sky",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "night",
            kind: ConceptKind::Celestial,
            doc: "the dark half of the day-night cycle",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "day",
            kind: ConceptKind::Celestial,
            doc: "the light half of the day-night cycle",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "fire",
            kind: ConceptKind::Substance,
            doc: "flame and heat",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "wind",
            kind: ConceptKind::Substance,
            doc: "moving air",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "earth",
            kind: ConceptKind::Terrain,
            doc: "the ground underfoot",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "tree",
            kind: ConceptKind::Living,
            doc: "a woody plant",
            ladder_rank: 0,
        },
        PackEntry {
            concept: EAT,
            kind: ConceptKind::Act,
            doc: "to consume food",
            ladder_rank: 0,
        },
        PackEntry {
            concept: SLEEP,
            kind: ConceptKind::Act,
            doc: "to rest unconscious",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "die",
            kind: ConceptKind::Act,
            doc: "to cease living",
            ladder_rank: 0,
        },
        // The causative of `die` above, and Swadesh-core on the same list
        // that put `die` here. Universal stratum rather than a gated pack:
        // there is no biome, climate or perception ladder a people's word
        // for killing could hang off, so gating it would be authoring a
        // silence rather than deriving one. `ladder_rank: 0` follows from
        // the stratum — every member is unranked and unconditionally in.
        PackEntry {
            concept: KILL,
            kind: ConceptKind::Act,
            doc: "to cause to cease living",
            ladder_rank: 0,
        },
        // The epistemic-hedge predicate (m09, "I think her name was
        // Gilda"). Universal stratum rather than beside `know` in
        // `action_suite_pack`: `know`'s exposure gate is an acknowledged
        // open question (registry row
        // `LANG-in-character-acts-are-unspeakable`), not a principled
        // placement, and there is no biome, climate or perception ladder a
        // people's word for thinking could hang off — the same argument
        // `KILL` above already carries. `ladder_rank: 0` follows from the
        // stratum for the same reason `KILL`'s does.
        PackEntry {
            concept: THINK,
            kind: ConceptKind::Act,
            doc: "to hold an uncertain belief",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "name",
            kind: ConceptKind::Social,
            doc: "a word that identifies one who bears it",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "one",
            kind: ConceptKind::Quality,
            doc: "the cardinal number 1",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "two",
            kind: ConceptKind::Quality,
            doc: "the cardinal number 2",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "many",
            kind: ConceptKind::Quality,
            doc: "an indefinitely large count",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "drink",
            kind: ConceptKind::Act,
            doc: "to swallow liquid",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "move",
            kind: ConceptKind::Act,
            doc: "to go from one place to another",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "rest",
            kind: ConceptKind::Act,
            doc: "to stop and recover strength",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "person",
            kind: ConceptKind::Living,
            doc: "a person; a member of a people (the autonym root)",
            ladder_rank: 0,
        },
        // The nine toponymic terrain concepts (hill, river, lake, valley,
        // coast, island, ford, marsh, spring) are deliberately NOT listed
        // here. Seven of them (all but `coast`/`lake`, which are
        // `KnowsOf`-only and so never win a root — see `TOPONYMIC_CORE`'s
        // doc comment below) ARE core, via `TOPONYMIC_CORE`, which
        // `concept_domain`/`is_core_concept` also consult — core-ness and
        // unconditional exposure used to be the same lever (membership in
        // this list), but they are different concerns for a terrain
        // concept: a rootable one should win the short proto-root form
        // (core), while whether a given culture holds the word at all
        // should depend on real geography
        // (`windows/worldgen::exposure_from`), the same gate-by-exposure
        // `sea`/`mountain` already use. Listing them here
        // (The Wearing's Task 3) conflated the two: `universal_stratum`
        // grants its members `ExposureClass::Steeped` unconditionally, so
        // every culture held every one of the nine before a single
        // exposure rule ran (Task 4 discovered this: the terrain gates it
        // adds were provably inert against a 100%-everyone baseline).
        // Fixed as part of Task 4 by moving core-ness to `TOPONYMIC_CORE`
        // and leaving exposure to the real gates — the terrain concepts
        // stay terrain-owned (`domains/terrain::register_concepts`
        // registers them) throughout.
        PackEntry {
            concept: "high",
            kind: ConceptKind::Quality,
            doc: "far above the ground",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "low",
            kind: ConceptKind::Quality,
            doc: "near the ground",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "great",
            kind: ConceptKind::Quality,
            doc: "large in size or extent",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "little",
            kind: ConceptKind::Quality,
            doc: "small in size or extent",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "new",
            kind: ConceptKind::Quality,
            doc: "recently come to be",
            ladder_rank: 0,
        },
        PackEntry {
            concept: OLD,
            kind: ConceptKind::Quality,
            doc: "long in existence",
            ladder_rank: 0,
        },
        PackEntry {
            concept: UNDER,
            kind: ConceptKind::Quality,
            doc: "beneath; below",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "over",
            kind: ConceptKind::Quality,
            doc: "above; atop",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "north",
            kind: ConceptKind::Quality,
            doc: "toward the pole of increasing latitude",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "south",
            kind: ConceptKind::Quality,
            doc: "toward the pole of decreasing latitude",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "east",
            kind: ConceptKind::Quality,
            doc: "the direction of increasing longitude",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "west",
            kind: ConceptKind::Quality,
            doc: "the direction of decreasing longitude",
            ladder_rank: 0,
        },
    ]
}

/// Every bearing a [`Compass`](../../../windows/locale) variant answers to —
/// the four cardinal roots above plus the four intercardinal compounds below.
/// One list drives the roster test and the reverse audit, so a bearing cannot
/// enter the vocabulary without both noticing.
/// type-audit: bare-ok(identifier-text)
pub const BEARINGS: &[&str] = &[
    "north",
    "north-east",
    "east",
    "south-east",
    "south",
    "south-west",
    "west",
    "north-west",
];

/// The intercardinal bearings: registered concepts with **no root of their
/// own**, realized through [`compound_recipe`] exactly as `sea` and `mountain`
/// are. Deliberately not in [`universal_stratum`] — giving them roots would
/// mint an unanalysable word where every attested language builds these by
/// composition, and would waste the compound machinery that already exists.
///
/// **The glosses name the mesh frame, never the sun.** "East is where the sun
/// rises" is false on a retrograde world — the `retrograde-spin` predicate says
/// so in as many words — and undefined on a tidally locked one, which has no
/// solar day at all. Retrograde spin mirrors the SUN, not the compass: the
/// frame is fixed by the geosphere, where latitude is north-positive and
/// longitude increases eastward.
pub fn bearing_compounds() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Quality;
    &[
        PackEntry {
            concept: "north-east",
            kind: KIND,
            doc: "between north and east",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "south-east",
            kind: KIND,
            doc: "between south and east",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "south-west",
            kind: KIND,
            doc: "between south and west",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "north-west",
            kind: KIND,
            doc: "between north and west",
            ladder_rank: 0,
        },
    ]
}

/// The luminance-ladder concept ids within [`color_pack`], as opposed to
/// the hue-ladder ids. [`in_ladder`] gates these against
/// [`PackDepths::luminance`] rather than [`PackDepths::hue`].
const LUMINANCE_CONCEPTS: &[&str] = &["gloom", "shadow", "starlit"];

/// The color pack: the hue ladder (Berlin & Kay's basic color term
/// acquisition stages — dark/light, then red, then green and yellow, then
/// blue, then brown) and a parallel luminance ladder for the ambient dark
/// (gloom, shadow, starlit) that a culture without much daylight lexicalizes
/// on its own schedule. All entries are [`ConceptKind::Quality`].
pub fn color_pack() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Quality;
    &[
        PackEntry {
            concept: "dark",
            kind: KIND,
            doc: "the color term for black/dark hues",
            ladder_rank: 1,
        },
        PackEntry {
            concept: "light",
            kind: KIND,
            doc: "the color term for white/light hues",
            ladder_rank: 1,
        },
        PackEntry {
            concept: "red",
            kind: KIND,
            doc: "the color term for red",
            ladder_rank: 2,
        },
        PackEntry {
            concept: "green",
            kind: KIND,
            doc: "the color term for green",
            ladder_rank: 3,
        },
        PackEntry {
            concept: "yellow",
            kind: KIND,
            doc: "the color term for yellow",
            ladder_rank: 3,
        },
        PackEntry {
            concept: "blue",
            kind: KIND,
            doc: "the color term for blue",
            ladder_rank: 4,
        },
        PackEntry {
            concept: "brown",
            kind: KIND,
            doc: "the color term for brown",
            ladder_rank: 5,
        },
        PackEntry {
            concept: "gloom",
            kind: KIND,
            doc: "the deepest, starless dark",
            ladder_rank: 1,
        },
        PackEntry {
            concept: "shadow",
            kind: KIND,
            doc: "cast dark, distinct from open gloom",
            ladder_rank: 2,
        },
        PackEntry {
            concept: "starlit",
            kind: KIND,
            doc: "dark faintly lit by stars",
            ladder_rank: 3,
        },
    ]
}

/// The body pack: shared humanoid anatomy, model-carded and banked to
/// BIO-1. All entries are [`ConceptKind::Body`] and unranked.
pub fn body_pack() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Body;
    &[
        PackEntry {
            concept: "eye",
            kind: KIND,
            doc: "the organ of sight",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "mouth",
            kind: KIND,
            doc: "the organ of eating and speech",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "hand",
            kind: KIND,
            doc: "the manipulating limb-end",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "foot",
            kind: KIND,
            doc: "the walking limb-end",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "blood",
            kind: KIND,
            doc: "the circulating fluid of a body",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "bone",
            kind: KIND,
            doc: "the rigid frame of a body",
            ladder_rank: 0,
        },
    ]
}

/// The kin pack: the three relations every kinship system distinguishes,
/// regardless of how it further splits them. All entries are
/// [`ConceptKind::Kin`] and unranked.
pub fn kin_pack() -> &'static [PackEntry] {
    const KIND: ConceptKind = ConceptKind::Kin;
    &[
        PackEntry {
            concept: "parent",
            kind: KIND,
            doc: "one's father or mother",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "child",
            kind: KIND,
            doc: "one's son or daughter",
            ladder_rank: 0,
        },
        PackEntry {
            concept: "sibling",
            kind: KIND,
            doc: "one's brother or sister",
            ladder_rank: 0,
        },
    ]
}

/// Seven of The Wearing's nine toponymic terrain concepts: **core** for
/// word-FORM purposes (short-form priority in the proto-root walk — `hill`
/// and `river` are the highest-frequency morphemes in the name corpus, so
/// a periphery-length form for either is backwards) without being members
/// of [`universal_stratum`] (which would ALSO grant unconditional
/// `ExposureClass::Steeped` — the bug Task 4 found and fixed: every
/// culture held every one of the nine regardless of geography, before a
/// single exposure rule in `windows/worldgen::exposure_from` ever ran).
/// Core-ness and unconditional exposure used to be the same lever (pack
/// membership); this list is what keeps them separable now that terrain
/// concepts need the first without the second.
///
/// **Deliberately excludes `coast` and `lake`** (Task 4 review, Important
/// 5): both are `KnowsOf`-only in `exposure_from` by construction (a culture
/// can know a shore or a salt basin without living on either), so neither
/// ever wins the `Steeped` pass that hands out roots — every occurrence
/// renders as a `Compound` (`compound_recipe`) or a `Gap`, never a `Root`.
/// "Core" buys short-form priority in exactly one place,
/// `assign_proto_roots`'s proto-root walk, and only for concepts that walk
/// EVER assigns a form to reach for at all: `windows/worldgen`'s
/// `build_lexicon` only calls it for concepts a `Steeped` culture roots.
/// A concept that can never be `Steeped` has no root to prioritize, so
/// short-form priority for `coast`/`lake` would tighten the minimal-pair
/// and merger-aware constraints on every other core root (`etymology.rs`'s
/// `core_forms`/`placed_modern`) for a form that is never spoken —
/// `hill`/`river`'s stated rationale (highest-frequency morphemes in the
/// name corpus) does not apply to either. They join `sea`/`mountain` as
/// periphery — `KnowsOf`-only terrain concepts named by compound, never by
/// their own root.
/// type-audit: bare-ok(identifier-text)
const TOPONYMIC_CORE: &[&str] = &[
    "hill", "river", "valley", "island", "ford", "marsh", "spring",
];

/// Whether `concept` is **core** vocabulary — [`concept_domain`] returns
/// `Some`.
/// type-audit: bare-ok(identifier-text)
pub fn is_core_concept(concept: &str) -> bool {
    concept_domain(concept).is_some()
}

/// The **semantic domain** of a core concept: the authored Swadesh stratum
/// it belongs to (`"universal"` / `"body"` / `"kin"`), `"toponymic"` for a
/// [`TOPONYMIC_CORE`] member, or `None` for periphery (the ranked color
/// ladder, [`color_pack`], and the exposure-gated concepts owned by other
/// domains that never win a root — `sea`, `mountain`, and `coast`/`lake`
/// alongside them — where incidental homophony is tolerable). Two core
/// concepts are *confusable* when their domains match (they compete in one
/// context; a listener cannot separate them by topic) and *free* when they
/// differ — the split the merger-aware proto assignment drives to zero for
/// the confusable case (the codon-degeneracy argument leaves cross-domain
/// collisions alone); `"toponymic"` is its own domain rather than folded
/// into `"universal"` so a `hill`/`river` collision is scored as
/// same-topic-confusable while a `hill`/`water` collision is not. The
/// homophony fix assigns core concepts their proto-roots first — so they
/// win the short, distinct forms — and holds a core root to a minimal-pair
/// distance from every other core root. The lab's `core-homophony-*` and
/// `confusable-homophony-*` metrics (`windows/lab/src/metrics.rs`) call
/// this function directly, so they measure against this same definition by
/// construction, not by a second, independently-maintained copy.
/// type-audit: bare-ok(identifier-text)
pub fn concept_domain(concept: &str) -> Option<&'static str> {
    if universal_stratum().iter().any(|e| e.concept == concept) {
        Some("universal")
    } else if body_pack().iter().any(|e| e.concept == concept) {
        Some("body")
    } else if kin_pack().iter().any(|e| e.concept == concept) {
        Some("kin")
    } else if TOPONYMIC_CORE.contains(&concept) {
        Some("toponymic")
    } else {
        None
    }
}

/// The closed authored recipe table for KNOWS-OF compounds: concepts with
/// no root word of their own in any pack, expressed instead as a
/// `(modifier, head)` compound of two concept ids that *do* have roots
/// (whether in a pack above or owned by another domain). `None` means
/// `concept` cannot compound — it must already be a root, or it is a gap
/// this lexicon cannot yet name.
///
/// `sea` has no Swadesh root (it isn't in [`universal_stratum`]) but is
/// owned by terrain as a distinct concept from `water`; a culture without a
/// dedicated word for it names it "many water". `mountain`, likewise owned
/// by terrain, is named "many stone". The Wearing (Task 4) adds `coast`
/// ("earth water") and `lake` ("little water") on the same principle — both
/// are `KnowsOf`-only in `exposure_from` (a culture can know the shore or a
/// salt lake without living on either), so neither ever gets a root of its
/// own; every ingredient is drawn from [`universal_stratum`] so the compound
/// always resolves once its `KnowsOf` gate fires, exactly as `sea`'s and
/// `mountain`'s do.
/// type-audit: bare-ok(identifier-text)
pub fn compound_recipe(concept: &str) -> Option<(&'static str, &'static str)> {
    RECIPES
        .iter()
        .find(|(c, _, _)| *c == concept)
        .map(|(_, modifier, head)| (*modifier, *head))
}

/// The authored recipe rows behind [`compound_recipe`]:
/// `(concept, modifier, head)`. One table drives both the lookup and the
/// recipe-closure test, so a new recipe can never silently escape test
/// coverage. Closed and tiny; a linear scan is deterministic and cheap.
const RECIPES: &[(&str, &str, &str)] = &[
    ("sea", "many", "water"),
    ("mountain", "many", "stone"),
    ("coast", "earth", "water"),
    ("lake", "little", "water"),
    ("north-east", "north", "east"),
    ("south-east", "south", "east"),
    ("south-west", "south", "west"),
    ("north-west", "north", "west"),
];

/// The action suite's in-character concepts with no existing name (The
/// Deed, Task 2, spec §3.2 groups B/C): `(concept, doc)` pairs naming WHAT
/// a world-act is, folding every verb that differs only by scale or gating
/// onto the concept it resolves to — `map`/`examine`/`look`/`needs`/
/// `knows`/`wait`/`write`/`consult` fold onto these seven, while
/// `go`/`back`/`enter`/`out`/`dive`/`surface`/`delve`/`climb` are all
/// *going* and reuse [`universal_stratum`]'s existing `move`, minting
/// nothing.
///
/// Registered directly by [`register_concepts`], deliberately NOT chained
/// into any Swadesh pack: none of these seven is core, and nothing today
/// grants any of them `ExposureClass::Steeped` or `KnowsOf` — a culture's
/// exposure to literacy, cartography, or reading another's state is a
/// question this task does not resolve, so each registers an honest
/// `Void::Gap` lexeme (see [`register_concepts`]) rather than a promised
/// one; every species falls through to the generic `Experiential` gap
/// until a later task grants one of these an exposure rule.
/// type-audit: bare-ok(identifier-text)
pub fn action_suite_pack() -> &'static [(&'static str, &'static str)] {
    &[
        (
            "look",
            "to visually attend to something, near or far — `look`, `examine`",
        ),
        (
            "chart",
            "to form a picture of the space around oneself — `map`",
        ),
        ("sense", "to perceive another's felt bodily state — `needs`"),
        (
            KNOW,
            "to hold something in memory or understanding — `knows`",
        ),
        ("wait", "to let time pass without acting — `wait`"),
        ("write", "to set words down in writing — `write`"),
        ("read", "to take meaning from written words — `consult`"),
    ]
}

/// The action suite's OUT-OF-CHARACTER concepts (The Deed, Task 2, spec
/// §3.2 group A): operator instruments with no referent in the world at
/// all — `!why`/`!npcs`/`!help`/`!eyes`/`!whoami`/`!provoke`/`!soothe`.
/// `(concept, doc)` pairs, deliberately NOT a [`PackEntry`] roster and NOT
/// chained into [`register_concepts`]'s pack loop: pack membership maps
/// straight to `ExposureClass::Steeped` (`windows/worldgen`'s
/// `exposure_of_impl`, its first loop over [`universal_stratum`]), and an
/// operator instrument silently acquiring core vocabulary in every culture
/// is exactly the failure this task exists to close.
/// `hornvale_worldgen::exposure_of_impl` reads this list directly — not any
/// registry `Void` reading, see the note below for why — and classifies
/// every name here `Unknown { reason: GapReason::Extradiegetic }`,
/// unconditionally, for every species in every world.
///
/// **The registered `Void` choice, and why it is a documented compromise
/// rather than a clean fit.** None of the four `Void` variants was built
/// for "this referent does not exist in the world at all":
/// `Void::Unnamed` is the closest in the astronomy spectral-class
/// precedent's own words ("a star HAS a class... the fact is objective")
/// but asserts the opposite of what is true of an operator instrument
/// (there is no referent, objective or otherwise, for any culture to fail
/// to have met). Reusing it would not misroute anything downstream —
/// `exposure_of_impl`'s Extradiegetic block runs LAST and unconditionally,
/// so it overwrites whatever the Unnameable block assigned regardless of
/// which `Void` a concept was registered with. The actual problem sits one
/// level up from that: a bare registry `Void::Unnamed` reading cannot
/// DISTINGUISH "objectively real, nobody here has named it" (a spectral
/// class) from "not real at all" (an operator instrument) — the two are
/// indistinguishable from the `Void` alone, which is exactly why
/// classification here is driven by pack membership
/// (`extradiegetic_pack()`) rather than by any generic Void-reading rule.
/// `Void::Gap` says the hole is "expected to be filled later", which
/// contradicts `GapReason::Extradiegetic`'s own doc ("this gap can never
/// close"). `Void::Uncognized` carries a `pending_wave` field built for the
/// cognition edge specifically. Below, every entry registers
/// `Void::Imperceptible` on its lexeme edge instead: its literal claim —
/// "this edge cannot realize the concept" — holds for a structural reason
/// (nothing in the world ever emits an operator instrument as a
/// phenomenon, so no culture could ever come to perceive, then name, one),
/// even though its doc prose was written with the percept edge
/// specifically in mind. Named here as the finding it is, not silently
/// forced.
/// type-audit: bare-ok(identifier-text)
pub fn extradiegetic_pack() -> &'static [(&'static str, &'static str)] {
    &[
        (
            "recount",
            "to narrate the dated history of who someone is — `!why`",
        ),
        (
            "survey",
            "to enumerate every creature the world holds — `!npcs`",
        ),
        ("help", "to list the operator's own instructions — `!help`"),
        (
            "lens",
            "to choose or report which colour lens one's sight uses — `!eyes`",
        ),
        (
            "identify",
            "to report which body one currently occupies — `!whoami`",
        ),
        (
            "provoke",
            "to make someone hostile by an act the simulation itself did not choose — `!provoke`",
        ),
        (
            "soothe",
            "to ease someone's hostility by an act the simulation itself did not choose — `!soothe`",
        ),
    ]
}

/// True when `concept` is one of [`extradiegetic_pack`]'s operator
/// instruments — no referent in the world at all, so no `Void` reading over
/// the registry could ever answer this question on its own (see that
/// function's own doc). The single home for this predicate: both of its
/// callers (`cli/src/proto.rs`'s reference-page renderer and
/// `windows/worldgen`'s test-side proto-goblinoid golden) sit above
/// `hornvale_language` in the layering, so hoisting it here — rather than
/// duplicating a `&str`-keyed lookup in each — is a plain import, not a
/// layering violation the way importing `windows/worldgen`'s `&World`-keyed
/// `is_unnameable` into a domain would be.
/// type-audit: bare-ok(flag)
pub fn is_extradiegetic(concept: &str) -> bool {
    extradiegetic_pack()
        .iter()
        .any(|(name, _)| *name == concept)
}

/// The six felt states a creature can undergo, one per region of the vessel
/// window's valence x arousal circumplex (`AffectLabel`, spec §7):
/// `content`/`eager` (positive), `searching` (neutral seeking), `frustrated`/
/// `lost` (the two negative shapes — a known target out of reach versus no
/// target to move toward), and `helpless` (the persistent scar the other two
/// upgrade into). `(concept, doc)` pairs, registered directly by
/// [`register_concepts`] under [`hornvale_kernel::ConceptKind::Affect`],
/// deliberately NOT chained into any Swadesh pack: nothing today grants a
/// culture `ExposureClass::Steeped` or `KnowsOf` over another creature's felt
/// state, so each registers an honest `Void::Gap` lexeme, the same footing
/// [`action_suite_pack`] uses and for the same reason.
///
/// `hornvale_language` cannot import `AffectLabel` itself — it lives in
/// `windows/vessel`, a window, and a domain depends on the kernel and
/// nothing else (`domains/CLAUDE.md`'s one rule). The two rosters are kept
/// in step by a test in `windows/vessel`, which already depends on this
/// crate, rather than by an import running the wrong way across the
/// kernel -> domains -> windows layering.
/// type-audit: bare-ok(identifier-text)
pub fn felt_state_pack() -> &'static [(&'static str, &'static str)] {
    &[
        ("content", "positive, low arousal: needs met, at rest"),
        (
            "eager",
            "positive, high arousal: chasing a satisfiable need",
        ),
        (
            "frustrated",
            "negative: blocked with a known target out of reach",
        ),
        (
            "helpless",
            "negative and persistent: given up despite an active drive",
        ),
        ("lost", "negative: blocked with no target to move toward"),
        ("searching", "neutral, mid arousal: seeking with a gradient"),
    ]
}

/// The eight object properties a thing-kind may carry (`ObjectProperty`, The
/// Offer, spec §3.1/§3.3/§8/§12): what an object OFFERS, independent of any
/// verb that reads it — `supports-rest` (a place a body may lie down and
/// sleep), `holds-liquid` (a place a body may drink from), `affords-passage`
/// (a seam between two rooms a body may pass through), `encloses` (an anchor
/// that reveals what lies within it), `radiates-heat` (an anchor that emits
/// warmth), and The Chattel's three (spec §3.8, each earned by a verb that
/// campaign ships): `portable` (take/drop), `openable` (open/close),
/// `lockable` (open, requiring a key in custody).
/// `(concept, doc)` pairs, registered directly by
/// [`register_concepts`] under [`hornvale_kernel::ConceptKind::Quality`]
/// (its definition is already "an abstract property or attribute", the fit
/// The Offer's G3 ruling names for a property a thing HAS, spec §12) —
/// deliberately NOT chained into any Swadesh pack, the same footing
/// [`felt_state_pack`] uses and for the same reason: nothing today grants a
/// culture `ExposureClass::Steeped` or `KnowsOf` over an object's carried
/// property, so each registers an honest `Void::Gap` lexeme.
///
/// `hornvale_language` cannot import `ObjectProperty` itself — it lives in
/// `windows/vessel`, a window, and a domain depends on the kernel and
/// nothing else (`domains/CLAUDE.md`'s one rule). The two rosters are kept
/// in step by a test in `windows/vessel`, which already depends on this
/// crate, rather than by an import running the wrong way across the
/// kernel -> domains -> windows layering — the exact shape
/// `felt_state_pack`'s own doc explains for `AffectLabel`.
/// type-audit: bare-ok(identifier-text)
pub fn object_property_pack() -> &'static [(&'static str, &'static str)] {
    &[
        (
            "affords-passage",
            "a seam between two rooms a body may pass through",
        ),
        ("encloses", "an anchor that reveals what lies within it"),
        ("holds-liquid", "a place a body may drink from"),
        (
            "lockable",
            "a thing whose opening needs the key that matches it",
        ),
        ("openable", "a thing that can be opened and closed again"),
        ("portable", "a thing small enough for a body to carry"),
        ("radiates-heat", "an anchor that emits warmth"),
        ("supports-rest", "a place a body may lie down and sleep"),
    ]
}

/// Input to [`in_ladder`]: how many acquisition-ladder stages are unlocked,
/// per ladder in [`color_pack`]. Derivation from a culture's perception
/// vector lives in worldgen (Task 8) — this struct is just the input shape.
/// type-audit: bare-ok(count)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct PackDepths {
    /// Hue-ladder depth: dark/light (1) through brown (5).
    pub hue: u8,
    /// Luminance-ladder depth: gloom (1) through starlit (3).
    pub luminance: u8,
}

/// Whether `entry` is in the lexicon at `depths`. Unranked entries
/// (`ladder_rank == 0` — the universal stratum, body pack, and kin pack)
/// are always in. Ranked entries from [`color_pack`]'s luminance ladder
/// (gloom/shadow/starlit) gate against [`PackDepths::luminance`]; every
/// other ranked entry (the hue ladder) gates against [`PackDepths::hue`].
/// type-audit: bare-ok(flag)
pub fn in_ladder(entry: &PackEntry, depths: &PackDepths) -> bool {
    if entry.ladder_rank == 0 {
        return true;
    }
    if LUMINANCE_CONCEPTS.contains(&entry.concept) {
        entry.ladder_rank <= depths.luminance
    } else {
        entry.ladder_rank <= depths.hue
    }
}

/// Register every pack entry not already owned by another domain, under
/// domain `"language"`. A concept already present in `registry` — the
/// owning domain got there first (astronomy's sun/moon/star/night,
/// climate's snow/rain/ice and biomes, terrain's stone/mountain/sea,
/// species' goblin-kind/kobold-kind, settlement's home/hearth, religion's
/// god/spirit) — is skipped rather than re-registered, so this function is
/// order-independent: it may run before or after the other domains'
/// `register_concepts` in `register_all` without conflict.
///
/// Each language-owned concept registers through its correspondence
/// [`Manifest`]. These are the lexeme owner, so every one declares `Expected`
/// (the concrete word is realized per-species later, per the hybrid model);
/// language emits no phenomenon of its own, so the percept edge is a `Gap` —
/// except `wind`, perceived through climate's `ambient` phenomenon (referenced
/// by key); and cognition voids to the future cognition wave.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    let packs = universal_stratum()
        .iter()
        .chain(color_pack())
        .chain(body_pack())
        .chain(kin_pack())
        .chain(bearing_compounds());
    for entry in packs {
        if registry.concept(entry.concept).is_some() {
            continue;
        }
        // `wind` is perceived through climate's `ambient` phenomenon — the
        // composition root glosses that phenomenon back to this concept. Its
        // percept edge references that kind by key (a cross-domain reference,
        // not a crate dependency); `register_manifest` validates the key is
        // registered, so a climate rename would fail loudly here. In the roster
        // climate registers `ambient` before language runs. Every other
        // language concept has no phenomenon of its own, so its percept is a Gap.
        let percept = if entry.concept == "wind" {
            Correspondent::Present(PerceptKind("ambient".to_string()))
        } else {
            Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet"))
        };
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: entry.concept.to_string(),
                domain: "language".to_string(),
                kind: entry.kind,
                doc: entry.doc.to_string(),
            },
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept,
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }

    // The action suite's in-character concepts (The Deed, Task 2): honest
    // `Void::Gap` lexemes — nothing grants any of these `Steeped`/`KnowsOf`
    // today, so declaring `Expected` here would be a broken promise
    // (`cli/tests/suite/correspondence.rs`'s `every_expected_lexeme_is_
    // actually_lexicalizable`). See `action_suite_pack`'s own doc.
    for (concept, doc) in action_suite_pack() {
        if registry.concept(concept).is_some() {
            continue;
        }
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: concept.to_string(),
                domain: "language".to_string(),
                kind: ConceptKind::Act,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap(
                "no exposure rule grants this concept Steeped or KnowsOf yet",
            )),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }

    // The action suite's out-of-character concepts (The Deed, Task 2):
    // registered directly, never chained into `packs` above, so membership
    // here confers no `ExposureClass` on its own — see `extradiegetic_pack`'s
    // own doc for the full reasoning, including the `Void::Imperceptible`
    // choice on the lexeme edge.
    for (concept, doc) in extradiegetic_pack() {
        if registry.concept(concept).is_some() {
            continue;
        }
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: concept.to_string(),
                domain: "language".to_string(),
                kind: ConceptKind::Act,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Imperceptible(
                "an operator instrument; no referent in the world exists for any \
                 culture to come to name",
            )),
            percept: Correspondent::Absent(Void::Imperceptible(
                "the world never emits this as a phenomenon; it has no in-world referent",
            )),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }

    // The six felt states (The Confidant, Task 3): honest `Void::Gap`
    // lexemes for the same reason the action suite's in-character concepts
    // get one — nothing grants any of these `Steeped`/`KnowsOf` today. See
    // `felt_state_pack`'s own doc.
    for (concept, doc) in felt_state_pack() {
        if registry.concept(concept).is_some() {
            continue;
        }
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: concept.to_string(),
                domain: "language".to_string(),
                kind: ConceptKind::Affect,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap(
                "no exposure rule grants this concept Steeped or KnowsOf yet",
            )),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }

    // The five object properties (The Offer, Task 9): honest `Void::Gap`
    // lexemes for the same reason the felt states get one — nothing grants
    // any of these `Steeped`/`KnowsOf` today. See `object_property_pack`'s
    // own doc.
    for (concept, doc) in object_property_pack() {
        if registry.concept(concept).is_some() {
            continue;
        }
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: concept.to_string(),
                domain: "language".to_string(),
                kind: ConceptKind::Quality,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap(
                "no exposure rule grants this concept Steeped or KnowsOf yet",
            )),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A registry with climate's `ambient` phenomenon kind pre-registered, so
    /// language's `wind` manifest — whose percept references that kind — can be
    /// registered standalone. In the real roster climate registers it first;
    /// these unit tests register language in isolation, so they supply it.
    fn registry_with_ambient() -> ConceptRegistry {
        let mut r = ConceptRegistry::default();
        r.register_phenomenon_kind("ambient", "a pervasive atmospheric condition")
            .unwrap();
        r
    }

    #[test]
    fn water_is_registered_with_domain_language() {
        let mut r = registry_with_ambient();
        register_concepts(&mut r).unwrap();
        let def = r
            .concept("water")
            .unwrap_or_else(|| panic!("water should be registered"));
        assert_eq!(def.domain, "language");
    }

    #[test]
    fn blue_is_gated_by_hue_depth() {
        let blue = color_pack()
            .iter()
            .find(|e| e.concept == "blue")
            .unwrap_or_else(|| panic!("color_pack should contain blue"));
        assert_eq!(blue.ladder_rank, 4);
        assert!(
            !in_ladder(
                blue,
                &PackDepths {
                    hue: 3,
                    luminance: 5
                }
            ),
            "blue (rank 4) should be out at hue depth 3"
        );
        assert!(
            in_ladder(
                blue,
                &PackDepths {
                    hue: 4,
                    luminance: 5
                }
            ),
            "blue (rank 4) should be in at hue depth 4"
        );
    }

    #[test]
    fn every_recipe_ingredient_is_a_registered_concept() {
        let mut r = registry_with_ambient();
        register_concepts(&mut r).unwrap();
        for (concept, _, _) in RECIPES {
            let (modifier, head) = compound_recipe(concept)
                .unwrap_or_else(|| panic!("{concept} should have a recipe"));
            assert!(
                r.concept(modifier).is_some(),
                "recipe modifier '{modifier}' for '{concept}' is not a registered concept"
            );
            assert!(
                r.concept(head).is_some(),
                "recipe head '{head}' for '{concept}' is not a registered concept"
            );
        }
    }

    /// Every `Compass` variant answers to a registered concept. The four
    /// cardinals are roots in [`universal_stratum`]; the four intercardinals
    /// are compound-only, exactly as `sea` and `mountain` are.
    #[test]
    fn every_bearing_is_a_registered_concept() {
        let mut r = registry_with_ambient();
        register_concepts(&mut r).unwrap();
        for bearing in BEARINGS {
            assert!(
                r.concept(bearing).is_some(),
                "bearing '{bearing}' is not a registered concept"
            );
        }
    }

    /// The cardinals get roots; the intercardinals must NOT, or a culture
    /// would mint an atomic word where every attested language compounds.
    #[test]
    fn cardinals_are_roots_and_intercardinals_are_compounds() {
        for c in ["north", "south", "east", "west"] {
            assert_eq!(concept_domain(c), Some("universal"), "{c} is a root");
            assert!(compound_recipe(c).is_none(), "{c} needs no recipe");
        }
        for c in ["north-east", "south-east", "south-west", "north-west"] {
            assert_eq!(concept_domain(c), None, "{c} must have no root");
            let (modifier, head) =
                compound_recipe(c).unwrap_or_else(|| panic!("{c} should have a recipe"));
            assert!(
                ["north", "south"].contains(&modifier),
                "{c} modifier '{modifier}' should be a north/south cardinal"
            );
            assert!(
                ["east", "west"].contains(&head),
                "{c} head '{head}' should be an east/west cardinal"
            );
        }
    }

    /// The bug this change exists to fix: `north` and `south` were each
    /// glossed as the compass point opposite the other, a loop that grounds
    /// nothing. Every bearing must be defined against the mesh frame.
    #[test]
    fn no_bearing_is_defined_circularly() {
        for entry in universal_stratum()
            .iter()
            .chain(bearing_compounds())
            .filter(|e| BEARINGS.contains(&e.concept))
        {
            assert!(
                !entry.doc.contains("opposite"),
                "bearing '{}' is defined circularly: {:?}",
                entry.concept,
                entry.doc
            );
        }
    }

    #[test]
    fn person_is_a_registered_universal_concept() {
        assert!(
            universal_stratum().iter().any(|e| e.concept == "person"),
            "person is in the universal stratum"
        );
    }

    #[test]
    fn registering_after_astronomy_does_not_conflict_on_sun() {
        let mut r = registry_with_ambient();
        r.register_manifest(Manifest {
            concept: ConceptDef {
                name: "sun".to_string(),
                domain: "astronomy".to_string(),
                kind: ConceptKind::Celestial,
                doc: "the sun".to_string(),
            },
            lexeme: Correspondent::Present(Lexicalization::Expected),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })
        .unwrap();
        register_concepts(&mut r).unwrap();
        assert_eq!(
            r.concept("sun").unwrap().domain,
            "astronomy",
            "language must not re-register a concept another domain already owns"
        );
    }
}
