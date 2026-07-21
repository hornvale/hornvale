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

/// A tongue's drawn Number/Tense grammaticalization depths and attachment
/// sides — the LANG-43 sibling of [`crate::morphology::TongueMorphology`]'s
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
    /// Which side of the marked word the Number affix binds.
    pub number_position: ClassPosition,
    /// Which side of the marked word the Tense affix binds.
    pub tense_position: ClassPosition,
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

/// The percentage chance (out of 100) the Number affix binds as a suffix
/// rather than a prefix.
const NUMBER_POSITION_SUFFIX_CHANCE: u32 = 70;

/// The percentage chance (out of 100) the Tense affix binds as a suffix
/// rather than a prefix.
const TENSE_POSITION_SUFFIX_CHANCE: u32 = 65;

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

/// Draw `species`' Number/Tense grammaticalization depths and attachment
/// sides — four permanent streams:
/// `language/<species>/grammar/depth/number`,
/// `language/<species>/grammar/depth/tense`,
/// `language/<species>/grammar/number-position`,
/// `language/<species>/grammar/tense-position`. Drawn, independent of
/// evidentiality/noun-class (never shares a stream or a weight table with
/// [`crate::morphology::morph_depths`]).
/// type-audit: bare-ok(identifier-text)
pub fn paradigm_depths(seed: &Seed, species: &str) -> ParadigmDepths {
    let mut number_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::DEPTH)
        .derive_typed(streams::NUMBER)
        .stream();
    let number_depth = depth_from_bucket(
        number_stream
            .weighted_index(&NUMBER_DEPTH_WEIGHTS)
            .expect("NUMBER_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut tense_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::DEPTH)
        .derive_typed(streams::TENSE)
        .stream();
    let tense_depth = depth_from_bucket(
        tense_stream
            .weighted_index(&TENSE_DEPTH_WEIGHTS)
            .expect("TENSE_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut number_pos_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::NUMBER_POSITION)
        .stream();
    let number_position = if number_pos_stream.range_u32(1, 100) <= NUMBER_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    let mut tense_pos_stream = seed
        .derive_typed(streams::ROOT)
        .derive_typed(StreamLabel::dynamic(species))
        .derive_typed(streams::GRAMMAR)
        .derive_typed(streams::TENSE_POSITION)
        .stream();
    let tense_position = if tense_pos_stream.range_u32(1, 100) <= TENSE_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    ParadigmDepths {
        number_depth,
        tense_depth,
        number_position,
        tense_position,
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
/// uses) rather than duplicating it. New permanent streams:
/// `language/family/<family>/morph/number/plural`,
/// `language/family/<family>/morph/tense/past`.
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

/// One root's paradigm-cell computation for one axis value (spec §3.3):
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

/// Compute both candidate modern forms for one root's one paradigm cell
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
    let regular_form = affix(&regular_root.modern, &regular_affix.modern, position);

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

/// One root's paradigm cell after analogical leveling (spec §3.4): which
/// form actually surfaces.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LeveledCell {
    /// The underlying cell computation (Task 3), unchanged.
    pub cell: ParadigmCell,
    /// True if this cell's `cascade_native` form survived leveling (stayed
    /// irregular); false if it regularized to `regular_form`, or if it was
    /// never divergent in the first place (nothing to level).
    pub survived: bool,
}

/// Apply analogical leveling (spec §3.4) across one paradigm category's
/// cells: rank the DIVERGENT roots by their own proto-root segment length
/// (shortest = most resistant, per Zipf's law of abbreviation), and let the
/// shortest `leveling_fraction` (0.0-1.0, rounded to the nearest whole
/// survivor count) keep their `cascade_native` form; the rest regularize.
/// Non-divergent cells are untouched (nothing to level). Deterministic: a
/// `Vec` sort by `(length, id)` — never a `HashMap`, never a draw — so
/// equal-length roots break ties by their own `RootId`'s `Ord` (the
/// `BTreeMap` key's natural alphabetical order), never by insertion order.
/// type-audit: bare-ok(identifier-text: cells), bare-ok(identifier-text: root_protos), bare-ok(ratio: leveling_fraction), bare-ok(identifier-text: return)
pub fn level_paradigm(
    cells: &BTreeMap<String, ParadigmCell>,
    root_protos: &BTreeMap<String, Vec<Segment>>,
    leveling_fraction: f64,
) -> BTreeMap<String, LeveledCell> {
    let mut divergent: Vec<&String> = cells
        .iter()
        .filter(|(_, cell)| cell.is_irregular)
        .map(|(id, _)| id)
        .collect();
    divergent.sort_by(|a, b| {
        let len_a = root_protos[*a].len();
        let len_b = root_protos[*b].len();
        len_a.cmp(&len_b).then_with(|| a.cmp(b))
    });

    let survivor_count = ((divergent.len() as f64) * leveling_fraction).round() as usize;
    let survivors: BTreeSet<&String> = divergent.iter().take(survivor_count).copied().collect();

    cells
        .iter()
        .map(|(id, cell)| {
            let survived = cell.is_irregular && survivors.contains(id);
            (
                id.clone(),
                LeveledCell {
                    cell: cell.clone(),
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
            nuclei: 1,
            codas: vec![vec![]],
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

    #[test]
    fn paradigm_depths_covers_all_three_buckets_across_many_seeds() {
        // Not a single fixed outcome — confirm the weighted draw actually
        // reaches every MorphDepth bucket over enough seeds, the same
        // sanity check style morph_depths' own test suite uses.
        let mut saw_none = false;
        let mut saw_particle = false;
        let mut saw_affix = false;
        for i in 0..200u64 {
            let d = paradigm_depths(&Seed(i), "test");
            match d.number_depth {
                MorphDepth::None => saw_none = true,
                MorphDepth::Particle => saw_particle = true,
                MorphDepth::Affix => saw_affix = true,
            }
        }
        assert!(saw_none && saw_particle && saw_affix);
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
            nuclei: 1,
            codas: vec![vec![Manner::Stop], vec![]],
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

        let cell = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );

        assert_eq!(
            cell.regular_form.segments,
            vec![t(), a(), e()],
            "regular: root's own /t/ already dropped before affixing"
        );
        assert_eq!(
            cell.cascade_native.modern,
            vec![t(), a(), t(), e()],
            "cascade-native: /t/ survives, no longer word-final in the joined form"
        );
        assert!(cell.is_irregular);
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

        let cell = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );

        assert_eq!(cell.cascade_native.modern, cell.regular_form.segments);
        assert!(!cell.is_irregular);
    }

    #[test]
    fn realize_paradigm_cell_is_pure() {
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a(), t()];
        let affix_proto = vec![e()];

        let a_cell = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );
        let b_cell = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );
        assert_eq!(a_cell.cascade_native.modern, b_cell.cascade_native.modern);
        assert_eq!(a_cell.regular_form.segments, b_cell.regular_form.segments);
        assert_eq!(a_cell.is_irregular, b_cell.is_irregular);
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
        let mut cells: BTreeMap<String, ParadigmCell> = BTreeMap::new();
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
            let cell =
                realize_paradigm_cell(&proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
            assert!(
                cell.is_irregular,
                "root-{len:02} must diverge for this test's premise to hold"
            );
            root_protos.insert(id.clone(), proto);
            cells.insert(id, cell);
        }

        let leveled = level_paradigm(&cells, &root_protos, 0.25);

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
            .filter(|lc| lc.cell.is_irregular && !lc.survived)
            .count();
        assert_eq!(
            leveled_away_count, 6,
            "the remaining 6 divergent roots regularize"
        );
    }
}
