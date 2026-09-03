//! LANG-43: paradigm slots (Number, Tense) whose cascade-native form can
//! diverge from a mechanically-regular one, purely as a byproduct of
//! `evolve`'s word-edge rules ([`RuleKind::FinalLoss`],
//! [`RuleKind::ClusterSimplify`]) being positional over whatever sequence
//! they are handed — the divergence is irregularity, derived, never
//! authored. See `docs/superpowers/specs/2026-07-20-the-residue-design.md`.
#![warn(missing_docs)]

use crate::etymology::{Cascade, Derivation, evolve};
use crate::morphology::{ClassPosition, MorphDepth, MorphForm, affix};
use crate::phoneme::Segment;
use crate::phonology::Phonology;
use crate::streams;
use hornvale_kernel::Seed;
use hornvale_kernel::seed::StreamLabel;
use std::collections::{BTreeMap, BTreeSet};

/// A tongue's drawn Number/Tense/Polarity grammaticalization depths and
/// attachment sides — the LANG-43 sibling of [`crate::morphology::TongueMorphology`]'s
/// evidential/noun-class depths, kept as its own additive struct (not
/// folded into `TongueMorphology`) so this campaign touches no existing
/// call site: nothing in the shipped grammar renderer consumes these
/// fields yet (spec §6, no rendering surface in V1).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParadigmDepths {
    /// How deeply Number (Singular/Plural) grammaticalizes.
    pub number_depth: MorphDepth,
    /// How deeply Tense (Present/Past) grammaticalizes.
    pub tense_depth: MorphDepth,
    /// How deeply Polarity (Positive/Negative) grammaticalizes.
    pub polarity_depth: MorphDepth,
    /// How deeply Person (subject agreement) grammaticalizes (The Rail,
    /// Task 7).
    pub person_depth: MorphDepth,
    /// Which side of the marked word the Number affix binds.
    pub number_position: ClassPosition,
    /// Which side of the marked word the Tense affix binds.
    pub tense_position: ClassPosition,
    /// Which side of the marked word the Polarity affix binds.
    pub polarity_position: ClassPosition,
    /// Which side of the marked word the Person affix binds (The Rail,
    /// Task 7).
    pub person_position: ClassPosition,
}

/// Preregistered Number-depth weights over `[None, Particle, Affix]` —
/// most attested languages mark number morphologically at least
/// optionally, so this skews toward `Affix` more than the epistemic
/// evidential/noun-class weights ([`crate::morphology`]) do; a purely
/// typological prior, unrelated to their worldview-grammaticalization
/// story.
const NUMBER_DEPTH_WEIGHTS: [f64; 3] = [30.0, 20.0, 50.0];

/// Preregistered Tense-depth weights over `[None, Particle, Affix]`.
const TENSE_DEPTH_WEIGHTS: [f64; 3] = [25.0, 25.0, 50.0];

/// Preregistered Polarity-depth weights over `[None, Particle, Affix]` —
/// the only axis in this file whose skew INVERTS the others', for two
/// typological reasons that pull in the same direction.
///
/// First, `None` is the smallest weight of any axis here: standard clausal
/// negation is essentially universal, with no attested language lacking a
/// way to deny a clause, whereas a language with no number, tense or
/// noun-class morphology is ordinary. It is deliberately not ZERO — a
/// zero-weight bucket would make an axis this crate's `depth_from_bucket`
/// convention defines as three-valued binary in fact, while still claiming
/// three, and this file's own reachability test asserts every bucket is
/// reachable.
///
/// Second, `Particle` outweighs `Affix` here, the reverse of Number and
/// Tense: the commonest negation strategy cross-linguistically is a free
/// negative particle (English `not`, French `pas`, Mandarin `bu`), with
/// affixal negation the next commonest and negative auxiliaries rarer
/// still. A purely typological prior, drawn independently of every other
/// axis.
const POLARITY_DEPTH_WEIGHTS: [f64; 3] = [10.0, 50.0, 40.0];

/// Preregistered Person-depth weights over `[None, Particle, Affix]` (The
/// Rail, Task 7) — a tongue's own drawn take on how deeply subject
/// agreement grammaticalizes, independent of Number/Tense/Polarity and of
/// Common's fixed agreement rules ([`crate::morphology`]'s epistemic axes
/// and this file's siblings all draw their own weights the same way).
///
/// **The claim: verbal person agreement is very widely attested, and where
/// it is marked at all it is overwhelmingly affixal rather than a free
/// particle.** Siewierska, *Person* (Cambridge, 2004), surveys person
/// marking cross-linguistically and finds bound (affixal or clitic) subject
/// marking on the verb far outstripping free-standing person particles as
/// the exponent of agreement; Dryer's WALS chapter 102, "Verbal Person
/// Marking" (in Dryer & Haspelmath, eds., *WALS Online*), finds that only a
/// small minority of the sampled languages mark no person on the verb at
/// all, with the remainder overwhelmingly bound rather than free.
///
/// `None` (15) is smaller than Number's (30) and Tense's (25) — "very
/// widely attested" places Person's absence rate below its paradigm
/// siblings — but not as small as Polarity's (10), because standard
/// negation is attested as *essentially universal* while person agreement
/// is only *very widely* attested, a weaker claim. `Affix` (70) so far
/// outweighs `Particle` (15) that Affix accounts for 70/85 ≈ 82% of the
/// marked mass — a sharper affixal skew than Number's 50/70 ≈ 71% or
/// Tense's 50/75 ≈ 67% — the "overwhelmingly" half of the claim.
///
/// **`None` and `Particle` are tied at 15, deliberately, rather than one
/// set below the other.** The cited sources support person agreement being
/// widely attested (constraining `None`) and, where present, overwhelmingly
/// bound (constraining `Affix`'s dominance over `Particle`); neither source
/// is cited here for a claim about whether a free-standing person
/// *particle* is itself more or less common than *no* person marking at
/// all, which is a third, independent comparison the two source claims do
/// not jointly entail. An earlier draft set `Particle` below `None`
/// (10 vs. 15) as an arithmetic byproduct of enforcing the other two
/// relationships with a fixed 100-point budget, not from any claim this
/// doc could cite for that specific ordering — exactly the "decorated
/// after the fact" shape this file's own weight-choice discipline exists to
/// catch. The tie removes the unsupported ordering without inventing a
/// citation for it.
const PERSON_DEPTH_WEIGHTS: [f64; 3] = [15.0, 15.0, 70.0];

/// The percentage chance (out of 100) the Number affix binds as a suffix
/// rather than a prefix.
/// plumb: pending(wave-1)
const NUMBER_POSITION_SUFFIX_CHANCE: u32 = 70;

/// The percentage chance (out of 100) the Tense affix binds as a suffix
/// rather than a prefix.
/// plumb: pending(wave-1)
const TENSE_POSITION_SUFFIX_CHANCE: u32 = 65;

/// The percentage chance (out of 100) the Polarity affix binds as a suffix
/// rather than a prefix — below 50, unlike Number (70) and Tense (65),
/// because the negative morpheme's cross-linguistically dominant position
/// is BEFORE the verb it negates, not after it.
/// plumb: pending(wave-1)
const POLARITY_POSITION_SUFFIX_CHANCE: u32 = 40;

/// The percentage chance (out of 100) the Person affix binds as a suffix
/// rather than a prefix (The Rail, Task 7) — below 50, like Polarity's (40),
/// but for a distinct typological reason specific to agreement morphology,
/// and by a MILDER margin than Polarity's, not a sharper one.
///
/// Bybee, Perkins & Pagliuca, "On the Asymmetries in the Affixation of
/// Grammatical Material" (in Croft, Denning & Kemmer, eds., *Studies in
/// Typology and Diachrony*, 1990), find that grammatical morphemes overall
/// show a strong cross-linguistic preference for suffixing over prefixing
/// — the preference this file's Number (70) and Tense (65) weights already
/// encode — but that person/subject-agreement morphology departs from
/// that general suffixing preference more than tense-aspect or number
/// marking does, leaning closer to an even split between the two sides.
/// The source is cited for THAT comparison only — person vs. Number/Tense
/// — and is not cited for, and does not speak to, how large that
/// departure is relative to Polarity's own.
///
/// 45 encodes exactly the cited comparison: closer to an even split than
/// Number's 70 or Tense's 65, i.e. a real departure from the general
/// suffixing preference. It is deliberately a MILDER prefix lean than
/// Polarity's 40, not a stronger one: Polarity's number rests on its own,
/// unrelated, negation-specific reason (pre-verbal negative-particle
/// placement) that this source neither supports nor contradicts, so this
/// doc makes no claim ranking person's departure against Polarity's. An
/// earlier draft's doc claimed person "departs furthest" and prefixes "far
/// more readily" than every other axis here, including Polarity — a
/// superlative the cited source never makes and that 45 (milder than
/// Polarity's 40) does not itself support; the claim is tempered to what
/// the source and the number actually agree on.
/// plumb: pending(wave-1)
const PERSON_POSITION_SUFFIX_CHANCE: u32 = 45;

/// The `weighted_index` bucket order both depth axes share: 0 = `None`,
/// 1 = `Particle`, 2 = `Affix` (matching
/// [`crate::morphology`]'s own `depth_from_bucket` convention).
fn depth_from_bucket(bucket: usize) -> MorphDepth {
    match bucket {
        0 => MorphDepth::None,
        1 => MorphDepth::Particle,
        _ => MorphDepth::Affix,
    }
}

/// Draw `species`' Number/Tense/Polarity/Person grammaticalization depths
/// and attachment sides — eight permanent streams:
/// `language/<species>/grammar/depth/number`,
/// `language/<species>/grammar/depth/tense`,
/// `language/<species>/grammar/depth/polarity`,
/// `language/<species>/grammar/depth/person`,
/// `language/<species>/grammar/number-position`,
/// `language/<species>/grammar/tense-position`,
/// `language/<species>/grammar/polarity-position`,
/// `language/<species>/grammar/person-position`. Drawn, independent of
/// evidentiality/noun-class (never shares a stream or a weight table with
/// [`crate::morphology::morph_depths`]).
///
/// **Every axis derives its OWN stream by label path** (spec §3.4 of The
/// Inquest): the polarity legs added at The Inquest, and the person legs
/// added here (The Rail, Task 7), consume nothing from the number, tense
/// or polarity streams, so adding them perturbs no existing consumption
/// order and moves no already-generated world's bytes.
/// type-audit: bare-ok(identifier-text)
pub fn paradigm_depths(seed: &Seed, species: &str) -> ParadigmDepths {
    let mut number_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::DEPTH)
        .derive(streams::NUMBER)
        .stream();
    let number_depth = depth_from_bucket(
        number_stream
            .weighted_index(&NUMBER_DEPTH_WEIGHTS)
            .expect("NUMBER_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut tense_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::DEPTH)
        .derive(streams::TENSE)
        .stream();
    let tense_depth = depth_from_bucket(
        tense_stream
            .weighted_index(&TENSE_DEPTH_WEIGHTS)
            .expect("TENSE_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut polarity_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::DEPTH)
        .derive(streams::POLARITY)
        .stream();
    let polarity_depth = depth_from_bucket(
        polarity_stream
            .weighted_index(&POLARITY_DEPTH_WEIGHTS)
            .expect("POLARITY_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut person_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::DEPTH)
        .derive(streams::PERSON)
        .stream();
    let person_depth = depth_from_bucket(
        person_stream
            .weighted_index(&PERSON_DEPTH_WEIGHTS)
            .expect("PERSON_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut number_pos_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::NUMBER_POSITION)
        .stream();
    let number_position = if number_pos_stream.range_u32(1, 100) <= NUMBER_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    let mut tense_pos_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::TENSE_POSITION)
        .stream();
    let tense_position = if tense_pos_stream.range_u32(1, 100) <= TENSE_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    let mut polarity_pos_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::POLARITY_POSITION)
        .stream();
    let polarity_position =
        if polarity_pos_stream.range_u32(1, 100) <= POLARITY_POSITION_SUFFIX_CHANCE {
            ClassPosition::Suffix
        } else {
            ClassPosition::Prefix
        };

    let mut person_pos_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::PERSON_POSITION)
        .stream();
    let person_position = if person_pos_stream.range_u32(1, 100) <= PERSON_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    ParadigmDepths {
        number_depth,
        tense_depth,
        polarity_depth,
        person_depth,
        number_position,
        tense_position,
        polarity_position,
        person_position,
    }
}

/// Draw `family`'s proto-affix for one paradigm axis-value (e.g.
/// `axis="number", value="plural"`; `axis="tense", value="past"`) — the
/// family-cognate law: every daughter's affix traces to the SAME family
/// proto (only `family`/`axis`/`value` key the draw, never the daughter or
/// its cascade), diverging only through each daughter's own [`crate::
/// etymology::evolve`], exactly like every other family-shared morpheme in
/// this crate. Delegates to [`crate::morphology::draw_morph_proto`] (same
/// one-syllable-fill mechanism every family-cognate proto in this crate
/// uses) rather than duplicating it. Permanent streams:
/// `language/family/<family>/morph/number/plural`,
/// `language/family/<family>/morph/tense/past`,
/// `language/family/<family>/morph/polarity/negative`. `axis` and `value`
/// are already free `&str` legs, so a new paradigm axis needs no new
/// function here — only its roster entry in
/// [`crate::stream_labels`].
/// type-audit: bare-ok(identifier-text)
pub fn draw_paradigm_affix_proto(
    seed: &Seed,
    family: &str,
    axis: &str,
    value: &str,
    proto_ph: &crate::phonology::Phonology,
) -> Vec<Segment> {
    crate::morphology::draw_morph_proto(seed, family, axis, value, proto_ph)
}

/// The tense axis's marked value labels — `past` only. Present is the zero
/// member (spec §4.1) and no marker is ever drawn for it, so inventing a
/// present form here would be authoring.
const TENSE_VALUES: [&str; 1] = ["past"];

/// The polarity axis's marked value labels — `negative` only, for the same
/// reason [`TENSE_VALUES`] holds only `past`: positive is the zero member.
const POLARITY_VALUES: [&str; 1] = ["negative"];

/// Draw `family`'s tense and polarity marker proto-affixes (one syllable
/// each, at `proto`'s phonology) and evolve them into `daughter` via
/// `cascade`, returning `(tense, polarity)` keyed by the MARKED value's
/// label. This is [`crate::morphology::morph_forms`]' cognate law applied to
/// the paradigm axes — only `family` and the value label key the draw, never
/// the daughter or its cascade, so all of a family's daughters carry cognate
/// markers that diverge only through their own sound changes.
///
/// Draws the same streams [`draw_paradigm_affix_proto`] documents
/// (`language/family/<family>/morph/tense/past`,
/// `language/family/<family>/morph/polarity/negative`) through the same
/// [`crate::morphology::draw_morph_proto`] mechanism, because it shares
/// `morphology`'s `evolve_axis` rather than re-implementing it.
///
/// **Number draws nothing here.** [`TongueParadigm`](crate::TongueParadigm)
/// carries tense and polarity marker maps and no number map, because no
/// realizer reads a number marker yet; drawing one would put an unread form
/// in the bundle. The number axis's own proto stream stays reachable through
/// [`draw_paradigm_affix_proto`] for whoever adds that realizer.
/// type-audit: bare-ok(identifier-text)
pub fn paradigm_forms(
    seed: &Seed,
    family: &str,
    proto: &Phonology,
    cascade: &Cascade,
    daughter: &Phonology,
) -> (
    BTreeMap<&'static str, MorphForm>,
    BTreeMap<&'static str, MorphForm>,
) {
    let tense = crate::morphology::evolve_axis(
        seed,
        family,
        "tense",
        &TENSE_VALUES,
        proto,
        cascade,
        daughter,
    );
    let polarity = crate::morphology::evolve_axis(
        seed,
        family,
        "polarity",
        &POLARITY_VALUES,
        proto,
        cascade,
        daughter,
    );
    (tense, polarity)
}

/// One root's paradigm-vertex computation for one axis value (spec §3.3):
/// both candidate modern forms, kept fully traceable, and whether they
/// diverge. `regular_root`/`regular_affix` are kept as their own
/// [`Derivation`]s (rather than folding the regular path into one
/// `Derivation`) because "regular" is never a single `evolve` call — it is
/// two independent calls, joined — so keeping both sub-derivations is
/// MORE traceable than a single composite would be, not less.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParadigmCell {
    /// Root-proto and affix-proto joined at the segment level BEFORE
    /// evolution, then evolved as one sequence — the order that lets
    /// `evolve`'s word-edge rules see the true joined boundary.
    pub cascade_native: Derivation,
    /// The root's own derivation, evolved independently of the affix.
    pub regular_root: Derivation,
    /// The affix's own derivation, evolved independently of the root.
    pub regular_affix: Derivation,
    /// `regular_root.modern` and `regular_affix.modern` joined at the
    /// segment level AFTER both were already evolved — today's implicit
    /// grammaticalization order (`crate::morphology::affix`'s existing
    /// call sites all join two already-evolved sides).
    pub regular_form: MorphForm,
    /// Whether `cascade_native.modern` differs from `regular_form.segments`
    /// — the irregularity signal itself, derived, never drawn.
    pub is_irregular: bool,
}

/// Compute both candidate modern forms for one root's one paradigm vertex
/// (spec §3.3) and whether they diverge. Pure and total: same inputs
/// always produce the same [`ParadigmCell`], mirroring [`evolve`]'s own
/// purity law — this function calls `evolve` and
/// [`crate::morphology::affix`] and nothing else, so it inherits their
/// purity directly.
pub fn realize_paradigm_cell(
    root_proto: &[Segment],
    affix_proto: &[Segment],
    position: ClassPosition,
    cascade: &Cascade,
    ph: &Phonology,
) -> ParadigmCell {
    let regular_root = evolve(root_proto, cascade, ph);
    let regular_affix = evolve(affix_proto, cascade, ph);
    let regular_form = affix(
        &regular_root.modern,
        &regular_affix.modern,
        position,
        ph.orthography,
    );

    let mut joined_proto = Vec::with_capacity(root_proto.len() + affix_proto.len());
    match position {
        ClassPosition::Prefix => {
            joined_proto.extend_from_slice(affix_proto);
            joined_proto.extend_from_slice(root_proto);
        }
        ClassPosition::Suffix => {
            joined_proto.extend_from_slice(root_proto);
            joined_proto.extend_from_slice(affix_proto);
        }
    }
    let cascade_native = evolve(&joined_proto, cascade, ph);

    let is_irregular = cascade_native.modern != regular_form.segments;

    ParadigmCell {
        cascade_native,
        regular_root,
        regular_affix,
        regular_form,
        is_irregular,
    }
}

/// One root's paradigm vertex after analogical leveling (spec §3.4): which
/// form actually surfaces.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LeveledCell {
    /// The underlying vertex computation (Task 3), unchanged.
    pub vertex: ParadigmCell,
    /// True if this vertex's `cascade_native` form survived leveling (stayed
    /// irregular); false if it regularized to `regular_form`, or if it was
    /// never divergent in the first place (nothing to level).
    pub survived: bool,
}

/// Apply analogical leveling (spec §3.4) across one paradigm category's
/// vertices: rank the DIVERGENT roots by their own proto-root segment length
/// (shortest = most resistant, per Zipf's law of abbreviation), and let the
/// shortest `leveling_fraction` (0.0-1.0, rounded to the nearest whole
/// survivor count) keep their `cascade_native` form; the rest regularize.
/// Non-divergent vertices are untouched (nothing to level). Deterministic: a
/// `Vec` sort by `(length, id)` — never a `HashMap`, never a draw — so
/// equal-length roots break ties by their own `RootId`'s `Ord` (the
/// `BTreeMap` key's natural alphabetical order), never by insertion order.
/// type-audit: bare-ok(identifier-text: vertices), bare-ok(identifier-text: root_protos), bare-ok(ratio: leveling_fraction), bare-ok(identifier-text: return)
pub fn level_paradigm(
    vertices: &BTreeMap<String, ParadigmCell>,
    root_protos: &BTreeMap<String, Vec<Segment>>,
    leveling_fraction: f64,
) -> BTreeMap<String, LeveledCell> {
    let mut divergent: Vec<&String> = vertices
        .iter()
        .filter(|(_, vertex)| vertex.is_irregular)
        .map(|(id, _)| id)
        .collect();
    divergent.sort_by(|a, b| {
        let len_a = root_protos[*a].len();
        let len_b = root_protos[*b].len();
        len_a.cmp(&len_b).then_with(|| a.cmp(b))
    });

    let survivor_count = ((divergent.len() as f64) * leveling_fraction).round() as usize;
    let survivors: BTreeSet<&String> = divergent.iter().take(survivor_count).copied().collect();

    vertices
        .iter()
        .map(|(id, vertex)| {
            let survived = vertex.is_irregular && survivors.contains(id);
            (
                id.clone(),
                LeveledCell {
                    vertex: vertex.clone(),
                    survived,
                },
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phoneme::{Backness, Height, Tone};
    use crate::phonology::Phonology;

    fn tiny_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                Segment::Consonant {
                    place: crate::phoneme::Place::Alveolar,
                    manner: crate::phoneme::Manner::Stop,
                    voiced: false,
                },
                Segment::Vowel {
                    height: Height::Mid,
                    backness: Backness::Front,
                    rounded: false,
                    tone: Tone::Neutral,
                },
            ],
            onsets: vec![vec![]],
            nuclei: vec![1],
            codas: vec![vec![]],
            harmony: crate::typology::Harmony::None,
            orthography: crate::typology::Orthography::Digraph,
        }
    }

    #[test]
    fn paradigm_affix_proto_is_family_cognate() {
        // Same (seed, family, axis, value) → identical proto, twice — the
        // family-cognate law every other family-shared proto in this crate
        // upholds (draw_morph_proto's own doc, mirrored here).
        let ph = tiny_phonology();
        let a = draw_paradigm_affix_proto(&Seed(3), "goblinoid", "number", "plural", &ph);
        let b = draw_paradigm_affix_proto(&Seed(3), "goblinoid", "number", "plural", &ph);
        assert_eq!(a, b);
        assert!(!a.is_empty());
    }

    #[test]
    fn paradigm_depths_is_pure() {
        let a = paradigm_depths(&Seed(1), "test");
        let b = paradigm_depths(&Seed(1), "test");
        assert_eq!(a, b);
    }

    /// claim: reachability(seed: 0..200) — forall MorphDepth variant, exists
    /// seed (Fix round 1, Class 2, same shape as numeracy.rs's sibling)
    #[test]
    fn paradigm_depths_covers_all_three_buckets_across_many_seeds() {
        // Not a single fixed outcome — confirm the weighted draw actually
        // reaches every MorphDepth bucket over enough seeds, the same
        // sanity check style morph_depths' own test suite uses.
        let mut saw_none = false;
        let mut saw_particle = false;
        let mut saw_affix = false;
        // The polarity axis (The Inquest) is checked in the same sweep
        // because its `None` weight is the smallest in the file (10 of
        // 100): deliberately small, since standard negation is essentially
        // universal, but deliberately NOT zero, and only a reachability
        // assertion keeps that distinction honest.
        let mut saw_polarity_none = false;
        let mut saw_polarity_particle = false;
        let mut saw_polarity_affix = false;
        for i in 0..200u64 {
            let d = paradigm_depths(&Seed(i), "test");
            match d.number_depth {
                MorphDepth::None => saw_none = true,
                MorphDepth::Particle => saw_particle = true,
                MorphDepth::Affix => saw_affix = true,
            }
            match d.polarity_depth {
                MorphDepth::None => saw_polarity_none = true,
                MorphDepth::Particle => saw_polarity_particle = true,
                MorphDepth::Affix => saw_polarity_affix = true,
            }
        }
        assert!(saw_none && saw_particle && saw_affix);
        assert!(
            saw_polarity_none && saw_polarity_particle && saw_polarity_affix,
            "every polarity bucket must be reachable, `None` included"
        );
    }

    /// claim: reachability(seed: 0..200) — forall MorphDepth variant, exists
    /// seed (The Rail, Task 7, same shape as
    /// [`paradigm_depths_covers_all_three_buckets_across_many_seeds`] above
    /// and numeracy.rs's own sibling).
    ///
    /// The person axis is drawn, and every bucket is reachable. A separate
    /// test rather than folded into the sweep above, so a person-only
    /// regression is legible on its own rather than buried in an assertion
    /// that also covers number/polarity.
    #[test]
    fn person_depth_is_drawn_and_every_bucket_is_reachable() {
        let mut saw_none = false;
        let mut saw_particle = false;
        let mut saw_affix = false;
        for i in 0..200u64 {
            let d = paradigm_depths(&Seed(i), "test");
            match d.person_depth {
                MorphDepth::None => saw_none = true,
                MorphDepth::Particle => saw_particle = true,
                MorphDepth::Affix => saw_affix = true,
            }
        }
        assert!(
            saw_none && saw_particle && saw_affix,
            "every person-depth bucket must be reachable"
        );
    }

    /// **Pin isolation: the person legs consume nothing from an existing
    /// stream.** Every axis in this file derives its own stream by label
    /// path, which is what makes adding one additive rather than a
    /// save-format break (this file's own `paradigm_depths` doc). The proof
    /// is that number, tense and polarity draw the SAME values before and
    /// after this campaign for the same seed and species.
    ///
    /// The values below are the exact `paradigm_depths(&Seed(42), species)`
    /// output recorded from a throwaway probe run BEFORE the person legs
    /// existed (The Rail, Task 7 report carries the raw run) — hard-coded
    /// here rather than read back from the post-change run, which would
    /// make this assert its own output.
    #[test]
    fn adding_the_person_axis_moves_no_existing_paradigm_draw() {
        let goblin = paradigm_depths(&Seed(42), "goblin");
        assert_eq!(goblin.number_depth, MorphDepth::Affix);
        assert_eq!(goblin.tense_depth, MorphDepth::Particle);
        assert_eq!(goblin.polarity_depth, MorphDepth::Affix);
        assert_eq!(goblin.number_position, ClassPosition::Suffix);
        assert_eq!(goblin.tense_position, ClassPosition::Suffix);
        assert_eq!(goblin.polarity_position, ClassPosition::Prefix);

        let kobold = paradigm_depths(&Seed(42), "kobold");
        assert_eq!(kobold.number_depth, MorphDepth::None);
        assert_eq!(kobold.tense_depth, MorphDepth::Affix);
        assert_eq!(kobold.polarity_depth, MorphDepth::Affix);
        assert_eq!(kobold.number_position, ClassPosition::Suffix);
        assert_eq!(kobold.tense_position, ClassPosition::Suffix);
        assert_eq!(kobold.polarity_position, ClassPosition::Suffix);

        let draconic = paradigm_depths(&Seed(42), "draconic");
        assert_eq!(draconic.number_depth, MorphDepth::Particle);
        assert_eq!(draconic.tense_depth, MorphDepth::None);
        assert_eq!(draconic.polarity_depth, MorphDepth::Particle);
        assert_eq!(draconic.number_position, ClassPosition::Prefix);
        assert_eq!(draconic.tense_position, ClassPosition::Suffix);
        assert_eq!(draconic.polarity_position, ClassPosition::Prefix);
    }

    use crate::etymology::{Cascade, RuleKind, SoundRule};
    use crate::phoneme::{Manner, Place};

    fn t() -> Segment {
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        }
    }

    fn a() -> Segment {
        Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone: Tone::Neutral,
        }
    }

    fn e() -> Segment {
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Front,
            rounded: false,
            tone: Tone::Neutral,
        }
    }

    fn edge_test_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                t(),
                a(),
                e(),
                Segment::Consonant {
                    place: Place::Alveolar,
                    manner: Manner::Stop,
                    voiced: true,
                }, // d, for the lenition case's Lenition output
            ],
            onsets: vec![vec![]],
            nuclei: vec![1],
            codas: vec![vec![Manner::Stop], vec![]],
            harmony: crate::typology::Harmony::None,
            orthography: crate::typology::Orthography::Digraph,
        }
    }

    #[test]
    fn final_loss_makes_a_suffixed_root_irregular() {
        // Root "tat" (t,a,t) ends in a consonant: evolved ALONE under
        // FinalLoss, the final /t/ drops ("ta"). But joined with a
        // vowel-initial suffix /e/ BEFORE evolution ("tate"), that same
        // /t/ is no longer word-final — FinalLoss checks the LAST segment
        // of whatever sequence it's given (etymology.rs's apply_final_loss),
        // and the joined sequence's last segment is the suffix's /e/, a
        // vowel, so FinalLoss never fires on the joined form at all.
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a(), t()];
        let affix_proto = vec![e()];

        let vertex = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );

        assert_eq!(
            vertex.regular_form.segments,
            vec![t(), a(), e()],
            "regular: root's own /t/ already dropped before affixing"
        );
        assert_eq!(
            vertex.cascade_native.modern,
            vec![t(), a(), t(), e()],
            "cascade-native: /t/ survives, no longer word-final in the joined form"
        );
        assert!(vertex.is_irregular);
    }

    #[test]
    fn a_position_independent_rule_never_diverges() {
        // Lenition is per-segment and position-independent (etymology.rs's
        // apply_segment_rule maps every segment regardless of index), so
        // joining before or after evolution can never change its outcome —
        // confirms the mechanism is non-degenerate: SOME cascades/roots
        // diverge (above), this one never does.
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::Lenition,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a()];
        let affix_proto = vec![e()];

        let vertex = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );

        assert_eq!(vertex.cascade_native.modern, vertex.regular_form.segments);
        assert!(!vertex.is_irregular);
    }

    #[test]
    fn realize_paradigm_vertex_is_pure() {
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a(), t()];
        let affix_proto = vec![e()];

        let a_vertex = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );
        let b_vertex = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );
        assert_eq!(
            a_vertex.cascade_native.modern,
            b_vertex.cascade_native.modern
        );
        assert_eq!(
            a_vertex.regular_form.segments,
            b_vertex.regular_form.segments
        );
        assert_eq!(a_vertex.is_irregular, b_vertex.is_irregular);
    }

    #[test]
    fn leveling_keeps_the_shortest_quartile_irregular() {
        // 8 roots, all ending in /t/ (all diverge under FinalLoss, per
        // final_loss_makes_a_suffixed_root_irregular's own mechanism),
        // lengths 2..=9 segments. leveling_fraction=0.25 → the 2 shortest
        // survive as irregular; the other 6 regularize.
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let affix_proto = vec![e()];

        let mut root_protos: BTreeMap<String, Vec<Segment>> = BTreeMap::new();
        let mut vertices: BTreeMap<String, ParadigmCell> = BTreeMap::new();
        for len in 2..=9usize {
            let id = format!("root-{len:02}");
            // Alternate a/t to keep it a legal onset-free CV*C shape, always
            // ending in /t/ so every one of these diverges under FinalLoss.
            // Indexed from the END (not the start) so the proto's actual
            // length always equals `len` exactly: alternating from the
            // start and then padding-if-needed-to-end-in-t (the naive
            // approach) collapses even/odd `len` pairs onto the same
            // actual length (e.g. len=2 and len=3 both produce "tat"),
            // which would make this test's own premise of 8 DISTINCT
            // lengths 2..=9 unsatisfiable.
            let mut proto = Vec::with_capacity(len);
            for i in 0..len {
                proto.push(if (len - 1 - i) % 2 == 0 { t() } else { a() });
            }
            assert_eq!(proto.len(), len);
            assert!(matches!(proto.last(), Some(s) if *s == t()));
            let vertex =
                realize_paradigm_cell(&proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
            assert!(
                vertex.is_irregular,
                "root-{len:02} must diverge for this test's premise to hold"
            );
            root_protos.insert(id.clone(), proto);
            vertices.insert(id, vertex);
        }

        let leveled = level_paradigm(&vertices, &root_protos, 0.25);

        let survivor_count = leveled.values().filter(|lc| lc.survived).count();
        assert_eq!(survivor_count, 2, "round(8 * 0.25) == 2 survivors");

        let mut survivor_lengths: Vec<usize> = leveled
            .iter()
            .filter(|(_, lc)| lc.survived)
            .map(|(id, _)| root_protos[id].len())
            .collect();
        survivor_lengths.sort_unstable();
        assert_eq!(
            survivor_lengths,
            vec![2, 3],
            "the two SHORTEST divergent roots survive"
        );

        let leveled_away_count = leveled
            .values()
            .filter(|lc| lc.vertex.is_irregular && !lc.survived)
            .count();
        assert_eq!(
            leveled_away_count, 6,
            "the remaining 6 divergent roots regularize"
        );
    }
}
