//! C7 (The Deep Grammar) morphology substrate: the depth vector (how deeply
//! evidentiality and noun class grammaticalize in a tongue), family-cognate
//! morpheme proto-forms evolved per daughter through the same cascade
//! machinery every other root uses, and segment-level affixation. These are
//! the primitives [`crate::grammar::realize_tongue_deep`] (C7's extended
//! realizer, living in `grammar.rs` beside the [`crate::grammar::TongueGrammar`]
//! it extends, over the [`crate::clause::Clause`] every realizer now
//! takes) assembles into a rendered
//! clause. Split into its own sibling module — rather than folding into
//! `grammar.rs` directly — because C7 adds enough new surface area (four
//! types, three functions, their own test suite) that inlining it would make
//! `grammar.rs` unwieldy; this is the documented call the plan's Task 1
//! sanctions.
#![warn(missing_docs)]

use crate::etymology::{Cascade, evolve};
use crate::naming::{Namer, render_views_with, segments_of};
use crate::phoneme::Segment;
use crate::phonology::Phonology;
use crate::streams;
use crate::typology::Orthography;
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
/// defines the category and answers the pure-string half of it
/// ([`noun_class_with_sky`]); the sky-override half's own input —
/// `sky_animate`, the C5 day-schema draw — still needs a built
/// terrain/climate, so DERIVING *that* stays the composition root's
/// business (a `noun_class_of` callback, per
/// [`crate::grammar::realize_tongue_deep`]'s signature — `windows/worldgen`'s
/// `noun_class_from` is the one that owns the day-schema draw and hands the
/// answer to [`noun_class_with_sky`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NounClass {
    /// A living, agentive referent (or, for a handful of sky bodies, one a
    /// culture's own agentive day-schema treats as living).
    Animate,
    /// A non-agentive referent.
    Inanimate,
}

/// The four sky-body concepts whose animacy needs a per-culture agentive
/// day-schema draw rather than the plain string rule below; every other
/// concept answers from [`noun_class_plain`] alone. Moved here from
/// `windows/worldgen::chorus` (Campaign The Vernacular, Task 6, correcting a
/// controller error in Task 5's dispatch): the four-name list and the
/// plain/override split are a pure `(bool, &str)` language fact with no
/// world/terrain/climate dependency of their own — nothing about them is
/// composition-root work, even though the ANSWER a caller supplies for the
/// sky-override arm (`sky_animate`) is composition-root-derived.
/// type-audit: bare-ok(identifier-text)
pub const SKY_OVERRIDE: [&str; 4] = ["sun", "moon", "star", "earth"];

/// The non-sky-override half of the animacy coherence law (plan header,
/// `docs/superpowers/plans/2026-07-19-the-deep-grammar.md`): every `*-kind`
/// concept (e.g. `"goblin-kind"`) and `"person"` are [`NounClass::Animate`];
/// every other concept is [`NounClass::Inanimate`]. Pure string comparison —
/// needs no world, terrain, or climate — so [`noun_class_with_sky`] can
/// answer it before its caller pays for a sculpt, and it can never be
/// affected by a `BuildError` on an unrelated (sky) concept. Moved from
/// `windows/worldgen::chorus` alongside [`SKY_OVERRIDE`] and
/// [`noun_class_with_sky`].
fn noun_class_plain(concept: &str) -> NounClass {
    if concept == "person" || concept.ends_with("-kind") {
        NounClass::Animate
    } else {
        NounClass::Inanimate
    }
}

/// C7's derived noun-class assignment, the animacy-coherence branch shared
/// by every caller that already holds (or has just computed) `sky_animate`
/// — the C5 day-schema draw's `Some(SchemaId::Agentive)` answer, per
/// species/culture — rather than deriving it itself: [`SKY_OVERRIDE`]
/// concepts answer `sky_animate` (mapped to `Animate`/`Inanimate`); every
/// other concept answers [`noun_class_plain`], unaffected by `sky_animate`.
///
/// Moved from `windows/worldgen::chorus` (Campaign The Vernacular, Task 6):
/// it is a pure function of `(bool, &str)`, nothing about it is
/// composition-root work, and moving it down here lets a consumer that
/// cannot depend on `hornvale-worldgen` reach it directly. That is not a
/// hypothetical — it is why this move happened: `windows/almanac`'s
/// phenomenon renderer needs exactly this function, and `hornvale-worldgen`
/// depends on `hornvale-almanac` (it constructs `hornvale_almanac::Speaker`),
/// so an almanac → worldgen edge is a dependency cycle. Re-exported from
/// `hornvale_worldgen::chorus` so every caller that reached it as
/// `hornvale_worldgen::noun_class_with_sky` (`windows/book`, and `chorus`'s
/// own `noun_class_from`) keeps compiling unchanged.
/// type-audit: bare-ok(flag: sky_animate), bare-ok(identifier-text: concept)
pub fn noun_class_with_sky(sky_animate: bool, concept: &str) -> NounClass {
    if SKY_OVERRIDE.contains(&concept) {
        if sky_animate {
            NounClass::Animate
        } else {
            NounClass::Inanimate
        }
    } else {
        noun_class_plain(concept)
    }
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
    /// The family's personal-pronoun forms, keyed `"1sg"` … `"3pl"` — the
    /// keys [`crate::clause::Person::paradigm_key`] names and
    /// [`pronoun_forms`] returns.
    ///
    /// **No depth field beside it, unlike the three axes above.** A pronoun
    /// is a free word by definition, so there is nothing for a
    /// `MorphDepth` to decide; [`pronoun_forms`]' own doc records why no
    /// such axis is drawn.
    ///
    /// **This is the field that retired The Scarf's pronoun gap.** A tongue
    /// gapped on a pronoun subject *because no tongue drew a pronoun
    /// inventory*; this is the inventory, so the antecedent is false and the
    /// rule stops firing without being reversed (spec §4.6).
    pub pronouns: BTreeMap<&'static str, MorphForm>,
}

/// Preregistered evidential-depth weights over `[None, Particle, Affix]`
/// (`docs/superpowers/plans/2026-07-19-the-deep-grammar.md`): mostly
/// undifferentiated, a folk/priesthood contrast the minority.
const EVIDENTIAL_DEPTH_WEIGHTS: [f64; 3] = [60.0, 25.0, 15.0];

/// Preregistered noun-class-depth weights over `[None, Particle, Affix]`.
const NOUN_CLASS_DEPTH_WEIGHTS: [f64; 3] = [55.0, 15.0, 30.0];

/// Preregistered class-marker position: the percentage chance (out of 100)
/// the marker binds as a suffix rather than a prefix.
/// plumb: pending(wave-1)
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
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::DEPTH)
        .derive(streams::EVIDENTIAL)
        .stream();
    let evidential_depth = depth_from_bucket(
        evidential_stream
            .weighted_index(&EVIDENTIAL_DEPTH_WEIGHTS)
            .expect("EVIDENTIAL_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut noun_class_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::DEPTH)
        .derive(streams::NOUN_CLASS)
        .stream();
    let noun_class_depth = depth_from_bucket(
        noun_class_stream
            .weighted_index(&NOUN_CLASS_DEPTH_WEIGHTS)
            .expect("NOUN_CLASS_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut position_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::CLASS_POSITION)
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

/// The personal-pronoun paradigm's six value labels — person crossed with
/// number — in the order [`pronoun_forms`] iterates them. The order is
/// presentational only: every value derives its own independent stream by
/// its own label path, so reordering this array would move no drawn form.
///
/// **Person and number only: no gender.** Nothing in the ledger assigns
/// grammatical gender to anything, so a gendered third person would be
/// authored rather than derived (The Inquest, spec §4.5). Common renders the
/// third-person singular as `them`, which is a slightly awkward register and
/// the accepted trade.
const PRONOUN_VALUES: [&str; 6] = ["1sg", "2sg", "3sg", "1pl", "2pl", "3pl"];

/// Draw one axis-value's one-syllable proto-form for `family`, from
/// `seed.derive(streams::ROOT).derive(streams::FAMILY_LEG).derive(StreamLabel::dynamic(family)).derive(streams::MORPH).derive(StreamLabel::dynamic(axis)).derive(StreamLabel::dynamic(value))`
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
        .derive(streams::ROOT)
        .derive(streams::FAMILY_LEG)
        .derive(StreamLabel::dynamic(family))
        .derive(streams::MORPH)
        .derive(StreamLabel::dynamic(axis))
        .derive(StreamLabel::dynamic(value))
        .stream();
    let namer = Namer::new(seed, family, proto_ph);
    let syllables = namer.draw_syllables(&mut stream, 1, 1, false);
    segments_of(&syllables)
}

/// Draw and evolve one axis's whole value set: each value's family proto-form
/// (via [`draw_morph_proto`]), evolved into `daughter_ph` via `cascade`
/// ([`evolve`]), rendered via [`render_views`].
///
/// `pub(crate)` rather than private because [`crate::paradigm::paradigm_forms`]
/// is the same operation on the tense/polarity axes: it must draw and evolve
/// through THIS function, not a copy of it, or the two axis families could
/// drift apart in how a proto becomes a daughter form.
pub(crate) fn evolve_axis(
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
            let roman = render_views_with(&derivation.modern, daughter_ph.orthography).roman;
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

/// Draw `family`'s six personal-pronoun proto-forms (one syllable each, at
/// `proto`'s phonology — the same syllable fill every family-cognate
/// morpheme in this crate uses) and evolve them into `daughter` via
/// `cascade`. This is [`morph_forms`]'s cognate law applied to a new axis:
/// only `family` and the value label key the draw, never the daughter or its
/// cascade, so all of a family's daughters carry COGNATE pronouns that
/// diverge only through each daughter's own sound changes — the same way the
/// rest of the family's inherited vocabulary does. Keyed
/// `"1sg"`/`"2sg"`/`"3sg"`/`"1pl"`/`"2pl"`/`"3pl"` (`PRONOUN_VALUES`), whose
/// doc records why no gender is drawn. New permanent stream:
/// `language/family/<family>/morph/pronoun/<person-number>`, additive in the
/// strong sense (spec §3.4): it consumes nothing from any existing stream, so
/// no already-generated world's bytes move.
///
/// **A pronoun is a free word, so there is no depth draw here.** Number,
/// tense and polarity each draw a [`MorphDepth`] because each may go
/// unmarked, be carried by a separate particle, or fuse onto the marked word.
/// Being a free word is precisely what distinguishes a pronoun from
/// person-agreement marking on the verb, so a `pronoun_depth` axis would draw
/// a value the definition has already fixed.
///
/// **Pro-drop is deliberately out of scope.** Whether a tongue may omit a
/// pronominal subject is a real typological axis, and this draws none of it:
/// no tongue is given permission to drop a pronoun. Recorded here so a later
/// reader sees the shape was considered rather than missed.
/// type-audit: bare-ok(identifier-text)
pub fn pronoun_forms(
    seed: &Seed,
    family: &str,
    proto: &Phonology,
    cascade: &Cascade,
    daughter: &Phonology,
) -> BTreeMap<&'static str, MorphForm> {
    evolve_axis(
        seed,
        family,
        "pronoun",
        &PRONOUN_VALUES,
        proto,
        cascade,
        daughter,
    )
}

/// Join `stem` and `affix` at the SEGMENT level — never string
/// concatenation across the boundary — in `position`'s order, then render
/// the joined sequence's romanization in one pass via [`render_views_with`]
/// under `orth`. This is the assembly law: rendering the whole joined
/// sequence at once (rather than rendering each side separately and gluing
/// the two strings) is what lets a boundary-sensitive rendering rule (this
/// crate's capitalization rule: [`render_views_with`] capitalizes only the
/// WHOLE string's first letter) take effect correctly — see
/// `affix_is_segment_level_not_string_concat` in this module's tests for the
/// measured difference. `orth` is a VIEW (spec §3.6): pass the joined word's
/// own tongue's [`Phonology::orthography`], or [`Orthography::Digraph`] at a
/// call site with no `Phonology` in scope.
/// type-audit: bare-ok(identifier-text)
pub fn affix(
    stem: &[Segment],
    affix: &[Segment],
    position: ClassPosition,
    orth: Orthography,
) -> MorphForm {
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
    let roman = render_views_with(&segments, orth).roman;
    MorphForm { segments, roman }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::etymology::{RuleKind, SoundRule, proto_root};
    use crate::naming::render_views;
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
            &crate::typology::concatenative(),
        )
    }

    /// claim: rate(forall-seed, none_count > affix_count / 200) — authored
    /// typology weight, with an embedded single-seed determinism check
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

    /// The two-way agreement between the DRAWING side (`PRONOUN_VALUES`, and
    /// therefore [`pronoun_forms`]' keys) and the CLAUSE side
    /// ([`crate::clause::Person::paradigm_key`]).
    ///
    /// **Both directions, deliberately.** A one-sided check is an echo: if
    /// only "every key a clause asks for exists" were asserted, the drawing
    /// side could grow a seventh row nothing can ever ask for, and the
    /// cheapest repair for a failure would be to delete the assertion. If
    /// only the converse were asserted, the clause side could name a row no
    /// tongue draws and every realization of it would gap. The two sets must
    /// be equal, so the test says so.
    #[test]
    fn the_pronoun_paradigm_keys_are_exactly_person_crossed_with_number() {
        let ph = test_phonology();
        let cascade = crate::etymology::draw_cascade(&Seed(7), "goblin", &ph);
        let drawn = pronoun_forms(&Seed(7), "goblinoid", &ph, &cascade, &ph);

        let asked: std::collections::BTreeSet<&'static str> = crate::clause::Person::ALL
            .into_iter()
            .flat_map(|person| {
                [crate::clause::Number::Sg, crate::clause::Number::Pl]
                    .into_iter()
                    .map(move |number| person.paradigm_key(number))
            })
            .collect();
        let offered: std::collections::BTreeSet<&'static str> = drawn.keys().copied().collect();
        assert_eq!(
            asked, offered,
            "every row a clause can ask for must be drawn, and nothing may be \
             drawn that no clause can ask for"
        );
        // And the constant the drawing side reads is the same set, so the
        // agreement is against the declared inventory rather than only
        // against one seed's output.
        let declared: std::collections::BTreeSet<&'static str> =
            PRONOUN_VALUES.into_iter().collect();
        assert_eq!(asked, declared);
    }

    #[test]
    fn pronoun_forms_cover_six_person_number_slots() {
        // The inventory itself: three persons crossed with two numbers, and
        // NOTHING ELSE. If a later campaign draws gender, this is the test
        // that must be argued with first (spec §4.5).
        let ph = test_phonology();
        let cascade = Cascade { rules: vec![] };
        let forms = pronoun_forms(&Seed(42), "goblinoid", &ph, &cascade, &ph);
        let keys: Vec<&str> = forms.keys().copied().collect();
        assert_eq!(
            keys,
            vec!["1pl", "1sg", "2pl", "2sg", "3pl", "3sg"],
            "the drawn inventory is exactly person x number, no gender"
        );
        for (value, form) in &forms {
            assert!(
                !form.segments.is_empty() && !form.roman.is_empty(),
                "{value} drew an empty pronoun"
            );
        }
        assert_eq!(
            forms,
            pronoun_forms(&Seed(42), "goblinoid", &ph, &cascade, &ph),
            "the same seed and family must draw the same inventory twice"
        );
    }

    #[test]
    fn pronoun_forms_are_family_cognate() {
        // The cognate law for the pronoun axis, measured the same way
        // `morph_forms_are_family_cognate` measures it for evidential and
        // noun class: two daughters of one family draw the SAME proto and
        // diverge only through their own cascades.
        let proto_ph = test_phonology();
        let daughter_ph = test_phonology();
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
                let a = pronoun_forms(seed, "goblinoid", &proto_ph, &cascade_a, &daughter_ph);
                let b = pronoun_forms(seed, "goblinoid", &proto_ph, &cascade_b, &daughter_ph);
                a.iter()
                    .any(|(value, form_a)| b[value].roman != form_a.roman)
            })
            .expect(
                "at least one seed in 1..=30 must show a cascade-divergent pronoun; if none \
                 does, this fixture's cascade contrast is too weak and needs strengthening",
            );

        let a = pronoun_forms(&seed, "goblinoid", &proto_ph, &cascade_a, &daughter_ph);
        let b = pronoun_forms(&seed, "goblinoid", &proto_ph, &cascade_b, &daughter_ph);

        for value in PRONOUN_VALUES {
            // The proto re-drawn here independently, through the exact path
            // `pronoun_forms` uses, keyed only by (seed, family, axis, value)
            // — no daughter, no cascade.
            let proto = draw_morph_proto(&seed, "goblinoid", "pronoun", value, &proto_ph);
            let expected_a = evolve(&proto, &cascade_a, &daughter_ph);
            let expected_b = evolve(&proto, &cascade_b, &daughter_ph);
            assert_eq!(
                a[value].segments, expected_a.modern,
                "{value} in daughter A is not the family proto under A's cascade"
            );
            assert_eq!(
                b[value].segments, expected_b.modern,
                "{value} in daughter B is not the family proto under B's cascade"
            );
            assert_eq!(a[value].roman, render_views(&expected_a.modern).roman);
        }

        assert!(
            a.iter()
                .any(|(value, form_a)| b[value].roman != form_a.roman),
            "the two daughters' pronoun romans must differ where the cascades differ"
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

        let joined = affix(
            &stem,
            &affix_segs,
            ClassPosition::Suffix,
            Orthography::Digraph,
        );
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
