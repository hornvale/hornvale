//! The etymology engine: proto-roots drawn from a species' phonology, and a
//! drawn cascade of sound-change rules that turns a proto-root into its
//! modern form. `evolve` is the Neogrammarian core — a *pure, total*
//! function: every rule applies uniformly wherever its conditioning
//! environment occurs (no rule is ever sporadic/lexical), and a rule whose
//! output segment is not in the phonology's inventory applies as identity
//! (the codomain constraint made mechanical — the cascade always lands on
//! the shipped phonology, never invents an off-menu segment).
//!
//! `proto_root` draws from the same stem machinery [`crate::naming`] uses
//! (onset/nucleus/coda templates filled by picking from the phonology's
//! inventory), over its own seed-derivation path, via
//! [`crate::naming::Namer::draw_syllables`] and
//! [`crate::naming::views_of`] — never constructing a
//! [`crate::phoneme::Segment`] itself, the same carry-forward invariant
//! naming relies on to keep the `"?"` fallback glyph unreachable.
#![warn(missing_docs)]

use crate::naming::Namer;
use crate::phoneme::{Height, Manner, Segment};
use crate::phonology::Phonology;
use hornvale_kernel::{Seed, Stream};

/// The closed family of sound-change rules a cascade may draw from. Each is
/// a total function on a segment (or, for the two structural rules, on a
/// word-position); see [`evolve`] for how a rule composes with the
/// inventory codomain constraint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuleKind {
    /// A voiceless stop becomes its voiced counterpart at the same place.
    Lenition,
    /// A fricative becomes a stop at the same place and voicing.
    Fortition,
    /// A vowel's height is raised (`param` 0) or lowered (`param` 1) one
    /// step.
    VowelShift,
    /// A two-consonant word-initial (onset) cluster drops its first
    /// segment.
    ClusterSimplify,
    /// A word-final coda consonant drops.
    FinalLoss,
}

/// One drawn sound rule: its kind, and `param` selecting the kind's drawn
/// variant (currently only meaningful to [`RuleKind::VowelShift`], which
/// reads 0 as "raise" and 1 as "lower"; every other kind ignores `param`).
/// The rule family is this closed enum, nothing extensible — `param` is not
/// a way to add new rule shapes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SoundRule {
    /// Which rule this is.
    pub kind: RuleKind,
    /// The kind's drawn variant selector (see the field's use in
    /// [`RuleKind::VowelShift`]'s docs).
    pub param: u32,
}

/// A drawn sequence of 2–4 sound rules, applied in order by [`evolve`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cascade {
    /// The rules, in application order.
    pub rules: Vec<SoundRule>,
}

/// One rule's application to a whole word during [`evolve`]: the rule
/// itself, and whether it changed anything (a rule whose conditioning
/// environment never occurred, or whose every candidate output fell outside
/// the phonology's inventory, records `changed: false`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AppliedRule {
    /// The rule that was applied.
    pub rule: SoundRule,
    /// Whether this rule changed the word.
    pub changed: bool,
}

/// A committed, replayable sound-change derivation: the drawn proto-root,
/// the cascade's step-by-step record, and the resulting modern form.
/// Replayable exactly: `evolve(d.proto, cascade, ph).modern == d.modern`
/// always holds, because `evolve` is pure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Derivation {
    /// The proto-root `evolve` started from.
    pub proto: Vec<Segment>,
    /// The cascade's rules, each with whether it changed the word.
    pub steps: Vec<AppliedRule>,
    /// The resulting modern form.
    pub modern: Vec<Segment>,
}

/// The closed, canonically ordered list of rule kinds [`draw_cascade`]
/// draws from.
const RULE_KINDS: [RuleKind; 5] = [
    RuleKind::Lenition,
    RuleKind::Fortition,
    RuleKind::VowelShift,
    RuleKind::ClusterSimplify,
    RuleKind::FinalLoss,
];

/// The `param` range every drawn rule consumes, regardless of kind (only
/// [`RuleKind::VowelShift`] gives it meaning) — keeping the draw uniform
/// across kinds keeps stream consumption independent of which kind is
/// picked.
const RULE_PARAM_RANGE: (u32, u32) = (0, 1);

/// The inclusive range of rules a drawn cascade contains.
const CASCADE_LEN_RANGE: (u32, u32) = (2, 4);

/// Draw one rule: a kind, then a param, in that order.
fn draw_rule(stream: &mut Stream) -> SoundRule {
    let kind = *stream
        .pick(&RULE_KINDS)
        .expect("RULE_KINDS is a fixed non-empty array");
    let param = stream.range_u32(RULE_PARAM_RANGE.0, RULE_PARAM_RANGE.1);
    SoundRule { kind, param }
}

/// Draw a 2–4 rule cascade for `species`, from
/// `seed.derive("language").derive(species).derive("lexicon").derive("cascade")`.
pub fn draw_cascade(seed: &Seed, species: &str) -> Cascade {
    let mut stream = seed
        .derive("language")
        .derive(species)
        .derive("lexicon")
        .derive("cascade")
        .stream();
    let count = stream.range_u32(CASCADE_LEN_RANGE.0, CASCADE_LEN_RANGE.1);
    let rules = (0..count).map(|_| draw_rule(&mut stream)).collect();
    Cascade { rules }
}

/// The number of syllables a drawn proto-root spans.
const PROTO_ROOT_SYLLABLE_RANGE: (u32, u32) = (1, 2);

/// Draw a proto-root for `concept` under `species`'s phonology `ph`, from
/// `seed.derive("language").derive(species).derive("lexicon").derive("root").derive(concept)`.
/// 1–2 syllables, filled from `ph`'s phonotactic templates by the same
/// mechanism [`crate::naming::Namer`] uses to build names.
pub fn proto_root(seed: &Seed, species: &str, concept: &str, ph: &Phonology) -> Vec<Segment> {
    let mut stream = seed
        .derive("language")
        .derive(species)
        .derive("lexicon")
        .derive("root")
        .derive(concept)
        .stream();
    let namer = Namer::new(seed, species, ph);
    let syllables = namer.draw_syllables(
        &mut stream,
        PROTO_ROOT_SYLLABLE_RANGE.0,
        PROTO_ROOT_SYLLABLE_RANGE.1,
        false,
    );
    crate::naming::views_of(&syllables).0
}

/// Whether `seg` is a consonant (used by the two structural rules, which
/// condition on consonant-hood rather than a specific place/manner).
fn is_consonant(seg: Segment) -> bool {
    matches!(seg, Segment::Consonant { .. })
}

/// Lenition's total function on a single segment: a voiceless stop becomes
/// its voiced counterpart at the same place; anything else is identity.
fn lenition(seg: Segment) -> Segment {
    match seg {
        Segment::Consonant {
            place,
            manner: Manner::Stop,
            voiced: false,
        } => Segment::Consonant {
            place,
            manner: Manner::Stop,
            voiced: true,
        },
        other => other,
    }
}

/// Fortition's total function on a single segment: a fricative becomes a
/// stop at the same place and voicing; anything else is identity.
fn fortition(seg: Segment) -> Segment {
    match seg {
        Segment::Consonant {
            place,
            manner: Manner::Fricative,
            voiced,
        } => Segment::Consonant {
            place,
            manner: Manner::Stop,
            voiced,
        },
        other => other,
    }
}

/// VowelShift's total function on a single segment: raise (`param` 0) or
/// lower (`param` 1) a vowel's height one step. A consonant, a vowel
/// already at the extreme height for the requested direction, or an
/// unrecognized `param`, is identity.
fn vowel_shift(seg: Segment, param: u32) -> Segment {
    match seg {
        Segment::Vowel {
            height,
            backness,
            rounded,
        } => {
            let height = match (param, height) {
                (0, Height::Low) => Height::Mid,
                (0, Height::Mid) => Height::High,
                (1, Height::High) => Height::Mid,
                (1, Height::Mid) => Height::Low,
                (_, unchanged) => unchanged,
            };
            Segment::Vowel {
                height,
                backness,
                rounded,
            }
        }
        other => other,
    }
}

/// Apply a per-segment rule function `f` uniformly to every segment of
/// `segs`: `f`'s proposed output replaces the original only when it differs
/// AND is present in `ph.inventory` (the codomain constraint); otherwise
/// that occurrence is left unchanged. Because `f` is a pure function of the
/// segment alone, every occurrence of the same segment is decided the same
/// way — this is what makes the rule Neogrammarian-regular (never
/// sporadic).
fn apply_segment_rule(
    segs: &[Segment],
    ph: &Phonology,
    f: impl Fn(Segment) -> Segment,
) -> (Vec<Segment>, bool) {
    let mut changed = false;
    let out = segs
        .iter()
        .map(|seg| {
            let proposed = f(*seg);
            if proposed != *seg && ph.inventory.contains(&proposed) {
                changed = true;
                proposed
            } else {
                *seg
            }
        })
        .collect();
    (out, changed)
}

/// ClusterSimplify's total function on a whole word: if the word begins
/// with two consecutive consonants (a two-consonant onset — the only onset
/// position a flat segment sequence still carries once syllable boundaries
/// are gone), drop the first. Otherwise identity.
fn apply_cluster_simplify(segs: &[Segment]) -> (Vec<Segment>, bool) {
    if segs.len() >= 2 && is_consonant(segs[0]) && is_consonant(segs[1]) {
        let mut out = segs.to_vec();
        out.remove(0);
        (out, true)
    } else {
        (segs.to_vec(), false)
    }
}

/// FinalLoss's total function on a whole word: if the word's last segment
/// is a consonant (a word-final coda), drop it. Otherwise identity.
fn apply_final_loss(segs: &[Segment]) -> (Vec<Segment>, bool) {
    match segs.last() {
        Some(last) if is_consonant(*last) => {
            let mut out = segs.to_vec();
            out.pop();
            (out, true)
        }
        _ => (segs.to_vec(), false),
    }
}

/// Dispatch one rule's application to a whole word under `ph`.
fn apply_rule(segs: &[Segment], rule: &SoundRule, ph: &Phonology) -> (Vec<Segment>, bool) {
    match rule.kind {
        RuleKind::Lenition => apply_segment_rule(segs, ph, lenition),
        RuleKind::Fortition => apply_segment_rule(segs, ph, fortition),
        RuleKind::VowelShift => apply_segment_rule(segs, ph, |s| vowel_shift(s, rule.param)),
        RuleKind::ClusterSimplify => apply_cluster_simplify(segs),
        RuleKind::FinalLoss => apply_final_loss(segs),
    }
}

/// Apply `cascade` to `proto` under `ph`, in order, recording each rule's
/// effect. Pure and total: same inputs always produce the same
/// [`Derivation`]; never panics; a rule's proposed output segment is only
/// ever adopted when it is present in `ph.inventory` (the codomain
/// constraint), otherwise that occurrence is left unchanged.
pub fn evolve(proto: &[Segment], cascade: &Cascade, ph: &Phonology) -> Derivation {
    let mut current = proto.to_vec();
    let mut steps = Vec::with_capacity(cascade.rules.len());
    for rule in &cascade.rules {
        let (next, changed) = apply_rule(&current, rule, ph);
        steps.push(AppliedRule {
            rule: *rule,
            changed,
        });
        current = next;
    }
    let current = nativize(&current, ph);
    Derivation {
        proto: proto.to_vec(),
        steps,
        modern: current,
    }
}

/// Merge each segment not already in `ph.inventory` to the nearest
/// same-class inventory segment (consonant→consonant, vowel→vowel) by
/// feature-mismatch count, ties broken by `Segment`'s total order. A segment
/// with no same-class neighbour in the inventory is left unchanged. Pure and
/// draw-free: this is how descent absorbs an inherited sound the daughter's
/// inventory no longer keeps (spec §2.2).
pub fn nativize(segs: &[Segment], ph: &Phonology) -> Vec<Segment> {
    segs.iter()
        .map(|&s| {
            if ph.inventory.contains(&s) {
                return s;
            }
            ph.inventory
                .iter()
                .copied()
                .filter(|c| same_class(*c, s))
                .min_by(|a, b| {
                    feature_distance(*a, s)
                        .cmp(&feature_distance(*b, s))
                        .then(a.cmp(b))
                })
                .unwrap_or(s)
        })
        .collect()
}

/// Whether two segments are both consonants or both vowels.
fn same_class(a: Segment, b: Segment) -> bool {
    matches!(
        (a, b),
        (Segment::Consonant { .. }, Segment::Consonant { .. })
            | (Segment::Vowel { .. }, Segment::Vowel { .. })
    )
}

/// Count of differing features between two same-class segments (place,
/// manner, voicing for consonants; height, backness, rounding for vowels).
/// Cross-class pairs never reach here (filtered by `same_class`).
fn feature_distance(a: Segment, b: Segment) -> u8 {
    match (a, b) {
        (
            Segment::Consonant {
                place: p1,
                manner: m1,
                voiced: v1,
            },
            Segment::Consonant {
                place: p2,
                manner: m2,
                voiced: v2,
            },
        ) => (p1 != p2) as u8 + (m1 != m2) as u8 + (v1 != v2) as u8,
        (
            Segment::Vowel {
                height: h1,
                backness: b1,
                rounded: r1,
            },
            Segment::Vowel {
                height: h2,
                backness: b2,
                rounded: r2,
            },
        ) => (h1 != h2) as u8 + (b1 != b2) as u8 + (r1 != r2) as u8,
        _ => u8::MAX,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};

    /// A permissive envelope, drawn at a fixed seed verified (by search) to
    /// contain every consonant [`evolve`]'s tests exercise across the
    /// Lenition/Fortition/codomain paths: the six oral stops (p/b, t/d,
    /// k/g) and the three non-sibilant fricatives (f/v, x), alongside every
    /// canonical vowel (always kept under a full `vowel_space`). NOTE: the
    /// fixed seed is coupled to `draw_phonology`'s stream-consumption
    /// order — if that draw algorithm ever changes, this inventory changes
    /// and the seed must be re-searched (these tests will fail loudly, not
    /// silently).
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
                exotic: ExoticSeg::None,
            },
        )
    }

    fn is_voiceless_stop(seg: &Segment) -> bool {
        matches!(
            seg,
            Segment::Consonant {
                manner: Manner::Stop,
                voiced: false,
                ..
            }
        )
    }

    fn proto_with_two_voiceless_stops() -> Vec<Segment> {
        use crate::phoneme::{Backness, Place};
        vec![
            Segment::Consonant {
                place: Place::Labial,
                manner: Manner::Stop,
                voiced: false,
            }, // p
            Segment::Vowel {
                height: Height::Low,
                backness: Backness::Central,
                rounded: false,
            }, // a
            Segment::Consonant {
                place: Place::Velar,
                manner: Manner::Stop,
                voiced: false,
            }, // k
        ]
    }

    #[test]
    fn evolve_is_regular() {
        // Neogrammarian: every segment matching the environment changes; replaying
        // evolve on the recorded proto reproduces the recorded modern exactly.
        let ph = test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::Lenition,
                param: 0,
            }],
        };
        let d = evolve(&proto_with_two_voiceless_stops(), &cascade, &ph);
        let voiceless_left = d.modern.iter().filter(|s| is_voiceless_stop(s)).count();
        assert_eq!(
            voiceless_left, 0,
            "lenition must hit every voiceless stop or none"
        );
    }

    #[test]
    fn evolve_lands_in_inventory() {
        // Codomain: every modern segment is in the phonology's inventory.
        let ph = test_phonology();
        let cascade = draw_cascade(&Seed(5), "test");
        let proto = proto_root(&Seed(5), "test", "water", &ph);
        let d = evolve(&proto, &cascade, &ph);
        for seg in &d.modern {
            assert!(
                ph.inventory.contains(seg),
                "modern segment {seg:?} is not in the phonology's inventory"
            );
        }
    }

    #[test]
    fn derivation_replays() {
        // evolve(d.proto, cascade, ph).modern == d.modern — byte-stable replay.
        let ph = test_phonology();
        let cascade = draw_cascade(&Seed(9), "test");
        let proto = proto_root(&Seed(9), "test", "fire", &ph);
        let d = evolve(&proto, &cascade, &ph);
        let replayed = evolve(&d.proto, &cascade, &ph);
        assert_eq!(replayed.modern, d.modern);
    }

    #[test]
    fn proto_root_is_pure() {
        // Same (seed, species, concept) → identical segments, twice.
        let ph = test_phonology();
        let a = proto_root(&Seed(11), "test", "stone", &ph);
        let b = proto_root(&Seed(11), "test", "stone", &ph);
        assert_eq!(a, b);
        assert!(!a.is_empty());
    }

    /// Pin-isolation (Task 7, spec §3): `proto_root`'s `species` parameter
    /// is really a `family` key once `lexicon::build_lexicon` calls it
    /// (`language/<family>/lexicon/root/<concept>`) — every daughter
    /// species sharing a family reads this same stream for a given
    /// concept. That draw must be a pure function of `(seed, family,
    /// concept, proto_ph)` alone: reading it once (as if for a lone
    /// daughter) or reading it again (as if a second daughter had already
    /// consumed it) must land on the identical stream position and yield
    /// the identical segments — the family's proto-root can never depend on
    /// how many daughters exist or the order they're built in.
    #[test]
    fn family_proto_root_is_independent_of_daughter_count() {
        let ph = test_phonology();
        let a = proto_root(&Seed(9), "goblinoid", "water", &ph);
        let b = proto_root(&Seed(9), "goblinoid", "water", &ph); // second reader
        assert_eq!(a, b);
        assert!(!a.is_empty());
    }

    // ---- Per-rule transformation tests: each constructs a single-rule
    // cascade directly (no drawing), feeds a hand-built proto whose
    // segments are all in `test_phonology()`'s inventory, and asserts the
    // exact expected modern sequence.

    use crate::phoneme::{Backness, Place};

    /// A consonant literal, for hand-built protos.
    fn c(place: Place, manner: Manner, voiced: bool) -> Segment {
        Segment::Consonant {
            place,
            manner,
            voiced,
        }
    }

    /// A vowel literal, for hand-built protos.
    fn v(height: Height, backness: Backness, rounded: bool) -> Segment {
        Segment::Vowel {
            height,
            backness,
            rounded,
        }
    }

    /// A one-rule cascade, for targeting a single rule's semantics.
    fn one_rule(kind: RuleKind, param: u32) -> Cascade {
        Cascade {
            rules: vec![SoundRule { kind, param }],
        }
    }

    #[test]
    fn fortition_turns_fricatives_into_stops_at_same_place_and_voicing() {
        let ph = test_phonology();
        // f-a-v: both fricatives (one voiceless, one voiced) must harden.
        let proto = vec![
            c(Place::Labial, Manner::Fricative, false), // f
            v(Height::Low, Backness::Central, false),   // a
            c(Place::Labial, Manner::Fricative, true),  // v
        ];
        let d = evolve(&proto, &one_rule(RuleKind::Fortition, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Labial, Manner::Stop, false), // p: same place, still voiceless
                v(Height::Low, Backness::Central, false), // a: untouched
                c(Place::Labial, Manner::Stop, true),  // b: same place, still voiced
            ]
        );
        assert!(d.steps[0].changed);
    }

    #[test]
    fn vowel_shift_param_0_raises_one_step_and_high_stays_high() {
        let ph = test_phonology();
        // t-e-t-i: e (Mid) raises to i (High); i is already High — identity.
        let proto = vec![
            c(Place::Alveolar, Manner::Stop, false), // t
            v(Height::Mid, Backness::Front, false),  // e
            c(Place::Alveolar, Manner::Stop, false), // t
            v(Height::High, Backness::Front, false), // i
        ];
        let d = evolve(&proto, &one_rule(RuleKind::VowelShift, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false), // t: consonants untouched
                v(Height::High, Backness::Front, false), // i: raised from e
                c(Place::Alveolar, Manner::Stop, false), // t
                v(Height::High, Backness::Front, false), // i: High stays High
            ]
        );
        assert!(d.steps[0].changed);
    }

    #[test]
    fn vowel_shift_param_1_lowers_one_step() {
        let ph = test_phonology();
        // k-i-k-u: i (High Front) lowers to e (Mid Front); u (High Back
        // rounded) lowers to o (Mid Back rounded).
        let proto = vec![
            c(Place::Velar, Manner::Stop, false),    // k
            v(Height::High, Backness::Front, false), // i
            c(Place::Velar, Manner::Stop, false),    // k
            v(Height::High, Backness::Back, true),   // u
        ];
        let d = evolve(&proto, &one_rule(RuleKind::VowelShift, 1), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Velar, Manner::Stop, false),   // k
                v(Height::Mid, Backness::Front, false), // e: lowered from i
                c(Place::Velar, Manner::Stop, false),   // k
                v(Height::Mid, Backness::Back, true),   // o: lowered from u
            ]
        );
        assert!(d.steps[0].changed);
    }

    #[test]
    fn cluster_simplify_drops_the_first_of_a_word_initial_two_consonant_onset() {
        let ph = test_phonology();
        // f-t-a: two-consonant word-initial onset drops its first segment.
        let proto = vec![
            c(Place::Labial, Manner::Fricative, false), // f
            c(Place::Alveolar, Manner::Stop, false),    // t
            v(Height::Low, Backness::Central, false),   // a
        ];
        let d = evolve(&proto, &one_rule(RuleKind::ClusterSimplify, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false), // t: now word-initial
                v(Height::Low, Backness::Central, false), // a
            ]
        );
        assert!(d.steps[0].changed);

        // t-a: a single-consonant onset is untouched.
        let simple = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let d = evolve(&simple, &one_rule(RuleKind::ClusterSimplify, 0), &ph);
        assert_eq!(d.modern, simple);
        assert!(!d.steps[0].changed);
    }

    #[test]
    fn final_loss_drops_a_word_final_consonant_but_not_a_final_vowel() {
        let ph = test_phonology();
        // t-a-k: the word-final coda consonant drops.
        let proto = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
            c(Place::Velar, Manner::Stop, false),     // k
        ];
        let d = evolve(&proto, &one_rule(RuleKind::FinalLoss, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),  // t
                v(Height::Low, Backness::Central, false), // a: now word-final
            ]
        );
        assert!(d.steps[0].changed);

        // t-a: a word-final vowel (an open syllable) is untouched.
        let open = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let d = evolve(&open, &one_rule(RuleKind::FinalLoss, 0), &ph);
        assert_eq!(d.modern, open);
        assert!(!d.steps[0].changed);
    }

    // ---- Nativization fixtures and tests.

    /// A restrictive phonology whose inventory genuinely lacks the
    /// postalveolar sibilant ʃ (and its voiced counterpart ʒ), built by
    /// filtering [`test_phonology`]'s permissive inventory — every other
    /// consonant (the stops and non-sibilant fricatives) remains.
    fn restrictive_no_postalveolar() -> Phonology {
        let mut ph = test_phonology();
        ph.inventory.retain(|s| {
            !matches!(
                s,
                Segment::Consonant {
                    place: Place::Postalveolar,
                    ..
                }
            )
        });
        ph
    }

    /// A restrictive phonology whose inventory genuinely lacks the velar
    /// nasal ŋ, built by filtering [`test_phonology`]'s permissive
    /// inventory — every other consonant remains.
    fn restrictive_no_velar_nasal() -> Phonology {
        let mut ph = test_phonology();
        ph.inventory.retain(|s| {
            !matches!(
                s,
                Segment::Consonant {
                    place: Place::Velar,
                    manner: Manner::Nasal,
                    ..
                }
            )
        });
        ph
    }

    #[test]
    fn nativize_keeps_in_inventory_segments_untouched() {
        let ph = test_phonology(); // permissive: every segment in-inventory
        let word = proto_root(&Seed(1), "test", "water", &ph);
        assert_eq!(nativize(&word, &ph), word); // no-op when all in-inventory
    }

    #[test]
    fn nativize_merges_off_inventory_to_nearest_same_class() {
        // A restrictive inventory lacking the postalveolar sibilant ʃ; ʃ must
        // merge to the nearest same-class (consonant) segment present, never to
        // a vowel.
        let ph = restrictive_no_postalveolar();
        assert!(!ph.inventory.contains(&Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        }));
        let sh = Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        };
        let out = nativize(&[sh], &ph);
        assert!(ph.inventory.contains(&out[0]));
        assert!(matches!(out[0], Segment::Consonant { .. }));
    }

    #[test]
    fn evolve_output_is_subset_of_inventory_even_from_foreign_proto() {
        // proto drawn from a permissive inventory, evolved into a restrictive one
        let proto_ph = test_phonology();
        let daughter_ph = restrictive_no_postalveolar();
        let proto = proto_root(&Seed(2), "goblinoid", "water", &proto_ph);
        let cascade = draw_cascade(&Seed(2), "bugbear");
        let d = evolve(&proto, &cascade, &daughter_ph);
        assert!(d.modern.iter().all(|s| daughter_ph.inventory.contains(s)));
    }

    #[test]
    fn nativization_is_load_bearing_not_codomain_identity() {
        // NON-VACUITY GUARD. A nasal is untouched by every rule kind (Lenition
        // hits voiceless stops, Fortition fricatives, VowelShift vowels,
        // Cluster/FinalLoss only drop EDGE consonants). Put a MEDIAL nasal the
        // daughter inventory lacks into a proto word: no rule can move it, so the
        // ONLY way `modern ⊆ inventory` can hold is nativization actually firing.
        // Without this guard, the subset test above could pass via plain
        // codomain-identity and never exercise `nativize` at all.
        let daughter_ph = restrictive_no_velar_nasal(); // inventory without ŋ
        let a = Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
        };
        let eng = Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        };
        assert!(
            !daughter_ph.inventory.contains(&eng),
            "ŋ must be off-inventory for this test"
        );
        let proto = vec![a, eng, a]; // /aŋa/: the nasal is medial, untouched by all rules
        let d = evolve(&proto, &draw_cascade(&Seed(5), "bugbear"), &daughter_ph);
        assert!(d.modern.iter().all(|s| daughter_ph.inventory.contains(s)));
        assert_ne!(
            d.modern, proto,
            "nativization must have replaced the off-inventory ŋ"
        );
    }
}
