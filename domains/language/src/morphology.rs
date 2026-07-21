//! C7 (The Deep Grammar) morphology substrate: the depth vector (how deeply
//! evidentiality and noun class grammaticalize in a tongue), family-cognate
//! morpheme proto-forms evolved per daughter through the same cascade
//! machinery every other root uses, and segment-level affixation. These are
//! the primitives [`crate::grammar::realize_tongue_deep`] (C7's extended
//! realizer, living in `grammar.rs` beside the [`crate::grammar::TongueGrammar`]/
//! [`crate::grammar::TongueClause`] it extends) assembles into a rendered
//! clause. Split into its own sibling module — rather than folding into
//! `grammar.rs` directly — because C7 adds enough new surface area (four
//! types, three functions, their own test suite) that inlining it would make
//! `grammar.rs` unwieldy; this is the documented call the plan's Task 1
//! sanctions.
#![warn(missing_docs)]

use crate::etymology::{Cascade, evolve};
use crate::naming::{Namer, render_views, segments_of};
use crate::phoneme::Segment;
use crate::phonology::Phonology;
use crate::streams;
use hornvale_kernel::Seed;
use hornvale_kernel::seed::StreamLabel;
use std::collections::BTreeMap;

/// How a rendered clause's content was epistemically grounded: witnessed
/// firsthand, taught by institutional doctrine, or inferred. `Inferred` is
/// floor-unreachable (no T1/T2 readout path constructs it yet — the readout
/// functions beyond this task own that guard); it is defined here, and
/// matched exhaustively wherever [`Evidential`] is consumed, purely so a
/// future readout can construct it without touching this enum again.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Evidential {
    /// Grounded in the speaker's own lived experience.
    Witnessed,
    /// Grounded in institutional doctrine, not lived experience.
    Taught,
    /// Grounded in inference rather than direct experience or doctrine.
    Inferred,
}

/// How deeply a grammatical category has grammaticalized in a tongue: not
/// marked at all, marked by a free-standing particle word, or fused onto the
/// marked word as a bound affix. Drawn per species per category (never
/// derived from culture vectors — the anti-astrology line, spec §3).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MorphDepth {
    /// The category is not marked at all — the C3 floor surface.
    None,
    /// The category is marked by a free-standing word.
    Particle,
    /// The category is marked by a bound affix, joined at the segment
    /// level (see [`affix`]).
    Affix,
}

/// A concept's noun class, as DERIVED (never drawn — anti-astrology line)
/// from shipped world-state; see the plan's animacy base table
/// (`docs/superpowers/plans/2026-07-19-the-deep-grammar.md`). This crate
/// only defines the category; deriving it is the composition root's
/// business (a `noun_class_of` callback, per
/// [`crate::grammar::realize_tongue_deep`]'s signature).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NounClass {
    /// A living, agentive referent (or, for a handful of sky bodies, one a
    /// culture's own agentive day-schema treats as living).
    Animate,
    /// A non-agentive referent.
    Inanimate,
}

/// Which side of the marked word a class marker binds. Drawn per species
/// (`language/<species>/grammar/class-position`, 40% prefix / 60% suffix);
/// meaningful only when [`TongueMorphology::noun_class_depth`] is
/// `Particle` or `Affix`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClassPosition {
    /// The marker precedes the marked word.
    Prefix,
    /// The marker follows the marked word.
    Suffix,
}

/// A drawn morpheme's segments (kept so a further [`affix`] join stays
/// segment-level) alongside its rendered romanization — the same
/// segments-then-render reduction every other generated word in this crate
/// goes through ([`render_views`] over its own segments, never a string
/// built by hand).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MorphForm {
    /// The morpheme's segments, for a further [`affix`] join.
    pub segments: Vec<Segment>,
    /// The morpheme's rendered romanization.
    pub roman: String,
}

/// A tongue's drawn C7 morphology bundle: how deeply each category
/// grammaticalizes, which side the class marker binds, and the family's
/// evidential/class marker forms (already evolved into this daughter) —
/// exactly what [`crate::grammar::realize_tongue_deep`] consumes. The marker
/// maps are keyed by value label (`"witnessed"`/`"taught"`/`"inferred"` for
/// `evidential`; `"animate"`/`"inanimate"` for `class`), so a caller-built
/// bundle missing a key degrades to "no marking" for that value rather than
/// panicking — useful for a synthetic fixture that only cares about one
/// value.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueMorphology {
    /// How deeply evidentiality grammaticalizes.
    pub evidential_depth: MorphDepth,
    /// How deeply noun class grammaticalizes.
    pub noun_class_depth: MorphDepth,
    /// Which side of the noun the class marker binds.
    pub class_position: ClassPosition,
    /// The family's evidential marker forms, keyed `"witnessed"`/`"taught"`/
    /// `"inferred"`.
    pub evidential: BTreeMap<&'static str, MorphForm>,
    /// The family's noun-class marker forms, keyed `"animate"`/`"inanimate"`.
    pub class: BTreeMap<&'static str, MorphForm>,
}

/// Preregistered evidential-depth weights over `[None, Particle, Affix]`
/// (`docs/superpowers/plans/2026-07-19-the-deep-grammar.md`): mostly
/// undifferentiated, a folk/priesthood contrast the minority.
const EVIDENTIAL_DEPTH_WEIGHTS: [f64; 3] = [60.0, 25.0, 15.0];

/// Preregistered noun-class-depth weights over `[None, Particle, Affix]`.
const NOUN_CLASS_DEPTH_WEIGHTS: [f64; 3] = [55.0, 15.0, 30.0];

/// Preregistered class-marker position: the percentage chance (out of 100)
/// the marker binds as a suffix rather than a prefix.
const CLASS_POSITION_SUFFIX_CHANCE: u32 = 60;

/// The `weighted_index` bucket order every depth axis shares: 0 = `None`,
/// 1 = `Particle`, 2 = `Affix` (frozen by the preregistered weight tables
/// above — the order the weights were authored in).
fn depth_from_bucket(bucket: usize) -> MorphDepth {
    match bucket {
        0 => MorphDepth::None,
        1 => MorphDepth::Particle,
        _ => MorphDepth::Affix,
    }
}

/// Draw `species`' C7 depth vector: how deeply evidentiality and noun class
/// grammaticalize, and which side of the noun the class marker binds — three
/// permanent streams: `language/<species>/grammar/depth/evidential`,
/// `language/<species>/grammar/depth/noun-class`,
/// `language/<species>/grammar/class-position`. Drawn, never derived from
/// any culture vector (the anti-astrology line, spec §3).
/// type-audit: bare-ok(identifier-text)
pub fn morph_depths(seed: &Seed, species: &str) -> (MorphDepth, MorphDepth, ClassPosition) {
    let mut evidential_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::DEPTH)
        .derive_typed(streams::EVIDENTIAL)
        .stream();
    let evidential_depth = depth_from_bucket(
        evidential_stream
            .weighted_index(&EVIDENTIAL_DEPTH_WEIGHTS)
            .expect("EVIDENTIAL_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut noun_class_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::DEPTH)
        .derive_typed(streams::NOUN_CLASS)
        .stream();
    let noun_class_depth = depth_from_bucket(
        noun_class_stream
            .weighted_index(&NOUN_CLASS_DEPTH_WEIGHTS)
            .expect("NOUN_CLASS_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut position_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::CLASS_POSITION)
        .stream();
    let class_position = if position_stream.range_u32(1, 100) <= CLASS_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    (evidential_depth, noun_class_depth, class_position)
}

/// The evidential axis's three value labels, in the fixed order
/// [`morph_forms`] draws and evolves them.
const EVIDENTIAL_VALUES: [&str; 3] = ["witnessed", "taught", "inferred"];

/// The noun-class axis's two value labels.
const CLASS_VALUES: [&str; 2] = ["animate", "inanimate"];

/// Draw one axis-value's one-syllable proto-form for `family`, from
/// `seed.derive_typed(streams::ROOT).derive_typed(streams::FAMILY_LEG).derive_typed(StreamLabel::dynamic(family)).derive_typed(streams::MORPH).derive_typed(StreamLabel::dynamic(axis)).derive_typed(StreamLabel::dynamic(value))`
/// — the same syllable-fill mechanism
/// [`crate::grammar::draw_copula_form`](../grammar/fn.draw_copula_form.html)
/// (and, further back, [`crate::etymology::proto_root`]) use: one template
/// syllable via [`Namer::draw_syllables`], flattened via [`segments_of`].
/// Keyed only by `(seed, family, axis, value)` — never by daughter or
/// cascade — so every daughter of the family draws the IDENTICAL proto-form;
/// daughters' surface forms diverge only through [`evolve`] (the cognate
/// law [`morph_forms`] upholds).
pub(crate) fn draw_morph_proto(
    seed: &Seed,
    family: &str,
    axis: &str,
    value: &str,
    proto_ph: &Phonology,
) -> Vec<Segment> {
    let mut stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(streams::FAMILY_LEG)
        .derive_typed(StreamLabel::dynamic(family))
        .derive_typed(streams::MORPH)
        .derive_typed(StreamLabel::dynamic(axis))
        .derive_typed(StreamLabel::dynamic(value))
        .stream();
    let namer = Namer::new(seed, family, proto_ph);
    let syllables = namer.draw_syllables(&mut stream, 1, 1, false);
    segments_of(&syllables)
}

/// Draw and evolve one axis's whole value set: each value's family proto-form
/// (via [`draw_morph_proto`]), evolved into `daughter_ph` via `cascade`
/// ([`evolve`]), rendered via [`render_views`].
fn evolve_axis(
    seed: &Seed,
    family: &str,
    axis: &str,
    values: &[&'static str],
    proto_ph: &Phonology,
    cascade: &Cascade,
    daughter_ph: &Phonology,
) -> BTreeMap<&'static str, MorphForm> {
    values
        .iter()
        .map(|&value| {
            let proto = draw_morph_proto(seed, family, axis, value, proto_ph);
            let derivation = evolve(&proto, cascade, daughter_ph);
            let roman = render_views(&derivation.modern).roman;
            (
                value,
                MorphForm {
                    segments: derivation.modern,
                    roman,
                },
            )
        })
        .collect()
}

/// Draw `family`'s evidential and noun-class morpheme proto-forms (one
/// syllable each, at `proto`'s phonology — the same `proto_root`-style
/// syllable fill the copula's overt form uses) and evolve them into
/// `daughter` via `cascade` — the family-cognate law: every daughter's
/// marker traces to the SAME family proto (only `family`/axis/value key the
/// draw, never the daughter or its cascade), so daughters diverge only
/// through their own cascade, exactly like every other root in the family's
/// lexicon ([`crate::lexicon::build_lexicon`]'s own Steeped roots). New
/// permanent streams: `language/family/<family>/morph/evidential/<value>`
/// (`witnessed`/`taught`/`inferred`) and
/// `language/family/<family>/morph/class/<value>` (`animate`/`inanimate`).
/// type-audit: bare-ok(identifier-text)
pub fn morph_forms(
    seed: &Seed,
    family: &str,
    proto: &Phonology,
    cascade: &Cascade,
    daughter: &Phonology,
) -> (
    BTreeMap<&'static str, MorphForm>,
    BTreeMap<&'static str, MorphForm>,
) {
    let evidential = evolve_axis(
        seed,
        family,
        "evidential",
        &EVIDENTIAL_VALUES,
        proto,
        cascade,
        daughter,
    );
    let class = evolve_axis(
        seed,
        family,
        "class",
        &CLASS_VALUES,
        proto,
        cascade,
        daughter,
    );
    (evidential, class)
}

/// Join `stem` and `affix` at the SEGMENT level — never string
/// concatenation across the boundary — in `position`'s order, then render
/// the joined sequence's romanization in one pass via [`render_views`]. This
/// is the assembly law: rendering the whole joined sequence at once (rather
/// than rendering each side separately and gluing the two strings) is what
/// lets a boundary-sensitive rendering rule (this crate's capitalization
/// rule: [`render_views`] capitalizes only the WHOLE string's first letter)
/// take effect correctly — see `affix_is_segment_level_not_string_concat`
/// in this module's tests for the measured difference.
/// type-audit: bare-ok(identifier-text)
pub fn affix(stem: &[Segment], affix: &[Segment], position: ClassPosition) -> MorphForm {
    let mut segments = Vec::with_capacity(stem.len() + affix.len());
    match position {
        ClassPosition::Prefix => {
            segments.extend_from_slice(affix);
            segments.extend_from_slice(stem);
        }
        ClassPosition::Suffix => {
            segments.extend_from_slice(stem);
            segments.extend_from_slice(affix);
        }
    }
    let roman = render_views(&segments).roman;
    MorphForm { segments, roman }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::etymology::{RuleKind, SoundRule, proto_root};
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};

    /// A permissive phonology (full vowel space, every place/manner
    /// combination reachable) — matching `etymology.rs`/`lexicon.rs`'s own
    /// test fixture idiom, reconstructed locally per this file's own test
    /// module (that helper is private to its own module).
    fn test_phonology() -> Phonology {
        draw_phonology(
            &Seed(37),
            "test",
            &Envelope {
                labiality: 1.0,
                vowel_space: 1.0,
                voicing: 1.0,
                sibilance: 1.0,
                voice_loudness: 1.0,
                tonality: 0.0,
                exotic: ExoticSeg::None,
            },
        )
    }

    #[test]
    fn depth_draws_are_deterministic_and_weighted() {
        let seed = Seed(42);
        let a = morph_depths(&seed, "goblin");
        let b = morph_depths(&seed, "goblin");
        assert_eq!(a, b, "same seed+species must draw the same triple twice");

        // Authored typology (~60% None vs ~15% Affix): None must dominate
        // over 200 derived species-name streams.
        let mut none_count = 0u32;
        let mut affix_count = 0u32;
        for s in 1..=200u64 {
            let (evidential_depth, _, _) = morph_depths(&Seed(s), "goblin");
            match evidential_depth {
                MorphDepth::None => none_count += 1,
                MorphDepth::Affix => affix_count += 1,
                MorphDepth::Particle => {}
            }
        }
        assert!(
            none_count > affix_count,
            "None should be the evidential-depth mode: none={none_count} affix={affix_count}/200"
        );
    }

    #[test]
    fn morph_forms_are_family_cognate() {
        let proto_ph = test_phonology();
        let daughter_ph = test_phonology();
        // Two contrasting cascades stand in for "two daughters": the empty
        // cascade changes nothing, the second applies real rules, so their
        // evolved forms are expected to diverge — measured below, not
        // assumed.
        let cascade_a = Cascade { rules: vec![] };
        let cascade_b = Cascade {
            rules: vec![
                SoundRule {
                    kind: RuleKind::FinalLoss,
                    param: 0,
                },
                SoundRule {
                    kind: RuleKind::Lenition,
                    param: 0,
                },
            ],
        };

        let seed = (1..=30u64)
            .map(Seed)
            .find(|seed| {
                let (evid_a, _) =
                    morph_forms(seed, "goblinoid", &proto_ph, &cascade_a, &daughter_ph);
                let (evid_b, _) =
                    morph_forms(seed, "goblinoid", &proto_ph, &cascade_b, &daughter_ph);
                evid_a
                    .iter()
                    .any(|(value, form_a)| evid_b[value].roman != form_a.roman)
            })
            .expect(
                "at least one seed in 1..=30 must show a cascade-divergent roman; if none does, \
                 this fixture's cascade contrast is too weak and needs strengthening",
            );

        let (evid_a, class_a) =
            morph_forms(&seed, "goblinoid", &proto_ph, &cascade_a, &daughter_ph);
        let (evid_b, class_b) =
            morph_forms(&seed, "goblinoid", &proto_ph, &cascade_b, &daughter_ph);

        // The cognate law itself: the SAME proto (independently re-drawn
        // here via the exact same path `morph_forms` uses) evolved through
        // each daughter's own cascade must equal what `morph_forms` returned.
        for value in EVIDENTIAL_VALUES {
            let proto = draw_morph_proto(&seed, "goblinoid", "evidential", value, &proto_ph);
            let expected_a = evolve(&proto, &cascade_a, &daughter_ph);
            let expected_b = evolve(&proto, &cascade_b, &daughter_ph);
            assert_eq!(evid_a[value].segments, expected_a.modern);
            assert_eq!(evid_a[value].roman, render_views(&expected_a.modern).roman);
            assert_eq!(evid_b[value].segments, expected_b.modern);
            assert_eq!(evid_b[value].roman, render_views(&expected_b.modern).roman);
        }
        for value in CLASS_VALUES {
            let proto = draw_morph_proto(&seed, "goblinoid", "class", value, &proto_ph);
            let expected_a = evolve(&proto, &cascade_a, &daughter_ph);
            assert_eq!(class_a[value].segments, expected_a.modern);
            let _ = &class_b;
        }

        assert!(
            evid_a
                .iter()
                .any(|(value, form_a)| evid_b[value].roman != form_a.roman),
            "the two daughters' romans must differ where the cascades differ"
        );
    }

    #[test]
    fn affix_is_segment_level_not_string_concat() {
        // The assembly law, measured: `render_views` capitalizes only the
        // WHOLE joined string's first letter (naming.rs's `capitalize_first`
        // is not per-segment), so joining segments THEN rendering once
        // yields exactly one capital, while gluing two independently
        // rendered (and independently capitalized) roman strings leaves a
        // stray internal capital wherever the affix begins. This boundary
        // is structural (a romanization rule), not a lucky phonological
        // digraph pick, so it holds for any nonempty stem+affix pair —
        // the arm actually measured true, no digraph hunt needed.
        let ph = test_phonology();
        let seed = Seed(3);
        let stem = proto_root(&seed, "test-tongue", "stem-concept", &ph);
        let affix_segs = proto_root(&seed, "test-tongue", "affix-concept", &ph);
        assert!(
            !stem.is_empty() && !affix_segs.is_empty(),
            "fixture needs nonempty segments on both sides of the boundary"
        );

        let joined = affix(&stem, &affix_segs, ClassPosition::Suffix);
        let joined_segments: Vec<Segment> = stem.iter().chain(&affix_segs).copied().collect();
        assert_eq!(
            joined.roman,
            render_views(&joined_segments).roman,
            "affix must equal joining segments then rendering ONCE"
        );

        let naive = format!(
            "{}{}",
            render_views(&stem).roman,
            render_views(&affix_segs).roman
        );
        assert_ne!(
            joined.roman, naive,
            "naive concatenation of two independently-rendered roman strings \
             (each capitalized on its own first letter) must differ from the \
             true segment-level join, which carries exactly one capital"
        );
    }
}
