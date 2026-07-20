//! LANG-43 property tests: the four standing laws from
//! `docs/superpowers/specs/2026-07-20-the-residue-design.md` §4, measured
//! directly against a real multi-root scenario — never narrated.
#![warn(missing_docs)]

use hornvale_language::etymology::{Cascade, RuleKind, SoundRule};
use hornvale_language::morphology::ClassPosition;
use hornvale_language::paradigm::{level_paradigm, realize_paradigm_cell};
use hornvale_language::phoneme::{Backness, Height, Manner, Place, Segment, Tone};
use hornvale_language::phonology::Phonology;
use std::collections::BTreeMap;

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

fn i() -> Segment {
    Segment::Vowel {
        height: Height::High,
        backness: Backness::Front,
        rounded: false,
        tone: Tone::Neutral,
    }
}

fn scenario_phonology() -> Phonology {
    Phonology {
        inventory: vec![t(), a(), e(), i()],
        onsets: vec![vec![]],
        nuclei: 1,
        codas: vec![vec![Manner::Stop], vec![]],
    }
}

/// 12 roots: 8 end in a consonant (/t/, diverge under FinalLoss), 4 end in
/// a vowel (/i/, never diverge — FinalLoss's own trigger, `is_consonant`
/// on the last segment, never fires). Lengths span 2..=9 for the
/// consonant-final group (3,5,7,9 + 1 for the vowel-final group), deliberately
/// mixed between the two groups so divergence tracks ROOT SHAPE, not root
/// length or insertion order.
fn build_scenario() -> (BTreeMap<String, Vec<Segment>>, Vec<Segment>) {
    let mut root_protos = BTreeMap::new();
    for (idx, len) in [2, 3, 4, 5, 6, 7, 8, 9].into_iter().enumerate() {
        // Indexed from the END (not the start) so the proto's actual length
        // always equals `len` exactly: alternating from the start and then
        // padding-if-needed-to-end-in-t (the naive approach) collapses
        // even/odd `len` pairs onto the same actual length (e.g. len=2 and
        // len=3 both produce "tat"), which would make the doc comment's
        // premise of 8 DISTINCT lengths unsatisfiable. See Task 4's
        // `leveling_keeps_the_shortest_quartile_irregular` test for the same
        // fix applied first.
        let mut proto = Vec::with_capacity(len);
        for i in 0..len {
            proto.push(if (len - 1 - i) % 2 == 0 { t() } else { a() });
        }
        assert_eq!(
            proto.len(),
            len,
            "consonant-final-{idx:02} must have length {len}"
        );
        assert!(matches!(proto.last(), Some(s) if *s == t()));
        root_protos.insert(format!("consonant-final-{idx:02}"), proto);
    }
    for (idx, len) in [3, 5, 7, 9].into_iter().enumerate() {
        let mut proto = Vec::with_capacity(len);
        for i in 0..len {
            proto.push(if i % 2 == 0 { t() } else { a() });
        }
        proto.push(i());
        root_protos.insert(format!("vowel-final-{idx:02}"), proto);
    }

    // Confirm all 8 consonant-final protos really do have distinct, correct
    // lengths before relying on them in the four law tests below.
    let mut consonant_final_lengths: Vec<usize> = root_protos
        .iter()
        .filter(|(id, _)| id.starts_with("consonant-final"))
        .map(|(_, proto)| proto.len())
        .collect();
    consonant_final_lengths.sort_unstable();
    assert_eq!(
        consonant_final_lengths,
        vec![2, 3, 4, 5, 6, 7, 8, 9],
        "the 8 consonant-final roots must have 8 distinct lengths 2..=9"
    );

    (root_protos, vec![e()])
}

#[test]
fn purity_same_inputs_same_output() {
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();
    let root = &root_protos["consonant-final-03"];

    let a_run = realize_paradigm_cell(root, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
    let b_run = realize_paradigm_cell(root, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
    assert_eq!(a_run.cascade_native.modern, b_run.cascade_native.modern);
    assert_eq!(a_run.regular_form.segments, b_run.regular_form.segments);
    assert_eq!(a_run.is_irregular, b_run.is_irregular);
}

#[test]
fn non_degeneracy_some_diverge_some_dont() {
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();

    let mut irregular_count = 0;
    let mut regular_count = 0;
    for (id, proto) in &root_protos {
        let cell = realize_paradigm_cell(proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
        if cell.is_irregular {
            irregular_count += 1;
            assert!(
                id.starts_with("consonant-final"),
                "{id} diverged unexpectedly"
            );
        } else {
            regular_count += 1;
            assert!(
                id.starts_with("vowel-final"),
                "{id} failed to diverge unexpectedly"
            );
        }
    }
    assert_eq!(irregular_count, 8, "all 8 consonant-final roots diverge");
    assert_eq!(regular_count, 4, "all 4 vowel-final roots stay regular");
}

#[test]
fn leveling_suppresses_a_strict_subset() {
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();

    let cells: BTreeMap<String, _> = root_protos
        .iter()
        .map(|(id, proto)| {
            (
                id.clone(),
                realize_paradigm_cell(proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph),
            )
        })
        .collect();
    let raw_divergence_count = cells.values().filter(|c| c.is_irregular).count();
    assert_eq!(raw_divergence_count, 8);

    let leveled = level_paradigm(&cells, &root_protos, 0.25);
    let survivor_count = leveled.values().filter(|lc| lc.survived).count();

    assert!(
        survivor_count < raw_divergence_count,
        "leveling must suppress a strict subset: {survivor_count} survivors of {raw_divergence_count} divergent"
    );
    assert_eq!(survivor_count, 2, "round(8 * 0.25) == 2");
}

#[test]
fn the_frequency_prediction_shorter_roots_survive() {
    // Preregistered directional claim (spec §4 law 4): mean proto length of
    // post-leveling survivors is strictly less than the mean proto length
    // of the divergent roots leveling erased. Measured, not narrated.
    //
    // Regression guard on level_paradigm's own selection logic, not
    // independent empirical support for the Zipf-length prior itself (which
    // nothing in a synthetic unit test could provide).
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();

    let cells: BTreeMap<String, _> = root_protos
        .iter()
        .map(|(id, proto)| {
            (
                id.clone(),
                realize_paradigm_cell(proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph),
            )
        })
        .collect();
    let leveled = level_paradigm(&cells, &root_protos, 0.25);

    let survivor_lengths: Vec<f64> = leveled
        .iter()
        .filter(|(_, lc)| lc.survived)
        .map(|(id, _)| root_protos[id].len() as f64)
        .collect();
    let leveled_away_lengths: Vec<f64> = leveled
        .iter()
        .filter(|(_, lc)| lc.cell.is_irregular && !lc.survived)
        .map(|(id, _)| root_protos[id].len() as f64)
        .collect();

    assert!(!survivor_lengths.is_empty());
    assert!(!leveled_away_lengths.is_empty());

    let mean = |xs: &[f64]| xs.iter().sum::<f64>() / xs.len() as f64;
    let survivor_mean = mean(&survivor_lengths);
    let leveled_away_mean = mean(&leveled_away_lengths);

    assert!(
        survivor_mean < leveled_away_mean,
        "survivors' mean proto length ({survivor_mean}) must be less than leveled-away roots' mean ({leveled_away_mean})"
    );
}

#[test]
fn every_candidate_form_carries_a_derivation() {
    // Law 5: no candidate form is ever a bare string — both paths remain
    // full Derivations (regular via its two sub-derivations) through to
    // whichever form survives.
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();
    let root = &root_protos["consonant-final-00"];

    let cell = realize_paradigm_cell(root, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
    assert_eq!(
        &cell.cascade_native.proto,
        root_proto_plus_affix(root, &affix_proto).as_slice()
    );
    assert_eq!(&cell.regular_root.proto, root);
    assert_eq!(&cell.regular_affix.proto, &affix_proto);
    assert!(
        !cell.cascade_native.steps.is_empty(),
        "a real Derivation records its applied-rule steps, not just proto/modern"
    );
}

fn root_proto_plus_affix(root: &[Segment], affix: &[Segment]) -> Vec<Segment> {
    let mut joined = root.to_vec();
    joined.extend_from_slice(affix);
    joined
}
