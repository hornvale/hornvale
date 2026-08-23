//! Property tests for Task 2: the weight-vector (`weight_vector`) and the
//! warmth×competence projection (`snap_judgment`). As with Task 1's
//! `axes.rs`, every assertion is STRUCTURAL — a property the law must have —
//! never a specific real pair's magnitude or emotion (0021).
//!
//! Step 1 (weight-vector properties) and Step 3 (snap_judgment properties)
//! both live here, per the plan brief's single test-file target.

use std::collections::BTreeSet;

use hornvale_kernel::{ConditionResponse, Mass, PLANT_FORAGE, ResourceVector};
use hornvale_language::speech::{ArticulationVector, ExoticManner};
use hornvale_sentiment::{Axis, Emotion, PeopleTraits, snap_judgment, weight_vector};
use hornvale_species::{
    ActivityCycle, ConditionNiche, HabitatRealm, LifeSchedule, MetabolicClass, MindVector,
    PerceptionVector, Sociality, SocietyVector, StatusBasis,
};

/// A synthetic, self-consistent `PeopleTraits` for probing weight-vector and
/// snap_judgment properties in isolation — the same round-numbers
/// discipline `axes.rs`'s `synthetic()` uses (0021: nothing here can be read
/// as an opinion about a named people).
fn synthetic(id: &'static str) -> PeopleTraits {
    PeopleTraits {
        id: hornvale_species::KindId(id),
        habitat: HabitatRealm::SURFACE,
        niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).expect("valid weights"),
        condition_niche: ConditionNiche {
            temperature: ConditionResponse {
                optimum: 15.0,
                width: 10.0,
                devotion: 0.5,
            },
            moisture: ConditionResponse {
                optimum: 0.5,
                width: 0.3,
                devotion: 0.5,
            },
            insolation: ConditionResponse {
                optimum: 0.5,
                width: 0.3,
                devotion: 0.5,
            },
            elevation: ConditionResponse {
                optimum: 500.0,
                width: 500.0,
                devotion: 0.5,
            },
        },
        mass: Mass::new(70.0).expect("valid mass"),
        metabolic_class: MetabolicClass::Endotherm,
        schedule: LifeSchedule::Allometric,
        society: SocietyVector {
            sociality: Sociality::Communal,
            status_basis: StatusBasis::Knowledge,
            in_group_radius: 1.0,
        },
        mind: MindVector {
            threat_response: 0.5,
            ..MindVector::MANIKIN
        },
        perception: PerceptionVector {
            activity: ActivityCycle::Diurnal,
            night_vision: 0.5,
            sky_attention: 0.5,
        },
        articulation: ArticulationVector {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.5,
            sibilance: 0.5,
            voice_loudness: 0.5,
            tonality: 0.0,
            exotic: ExoticManner::None,
        },
        preys_on: BTreeSet::new(),
    }
}

fn close(x: f64, y: f64) -> bool {
    (x - y).abs() < 1e-9
}

fn index_of(axis: Axis) -> usize {
    Axis::ALL
        .iter()
        .position(|a| *a == axis)
        .expect("axis is in Axis::ALL")
}

// --- Step 1: weight_vector properties --------------------------------------

#[test]
fn insular_judger_weights_every_axis_at_least_as_much_as_cosmopolitan() {
    // D1: in_group_radius is "insular 0 <-> expansive 1", so the MINIMUM
    // radius is the insular judger, not the maximum.
    let mut insular = synthetic("test-insular");
    insular.society.in_group_radius = 0.0;
    let mut cosmopolitan = synthetic("test-cosmopolitan");
    cosmopolitan.society.in_group_radius = 1.0;

    let insular_weights = weight_vector(&insular);
    let cosmopolitan_weights = weight_vector(&cosmopolitan);

    for axis in Axis::ALL {
        let i = index_of(axis);
        assert!(
            insular_weights[i] >= cosmopolitan_weights[i],
            "{}: insular weight {} should be >= cosmopolitan weight {}",
            axis.label(),
            insular_weights[i],
            cosmopolitan_weights[i]
        );
    }
    // Not a degenerate tie: insularity must up-weight SOMETHING.
    assert!(
        Axis::ALL
            .iter()
            .any(|a| insular_weights[index_of(*a)] > cosmopolitan_weights[index_of(*a)]),
        "insularity should strictly up-weight at least one axis"
    );
}

#[test]
fn weight_vector_is_non_negative() {
    for radius in [0.0, 0.25, 0.5, 0.75, 1.0] {
        let mut t = synthetic("test-nonneg");
        t.society.in_group_radius = radius;
        for w in weight_vector(&t) {
            assert!(w >= 0.0, "weight_vector must be non-negative, got {w}");
        }
    }
}

#[test]
fn higher_threat_response_up_weights_the_threat_axes() {
    let mut low_threat = synthetic("test-low-threat");
    low_threat.mind = MindVector {
        threat_response: 0.0,
        ..MindVector::MANIKIN
    };
    let mut high_threat = synthetic("test-high-threat");
    high_threat.mind = MindVector {
        threat_response: 1.0,
        ..MindVector::MANIKIN
    };

    let low_weights = weight_vector(&low_threat);
    let high_weights = weight_vector(&high_threat);

    for axis in [Axis::SizeThreat, Axis::DietPredation] {
        let i = index_of(axis);
        assert!(
            high_weights[i] > low_weights[i],
            "{}: high-threat weight {} should exceed low-threat weight {}",
            axis.label(),
            high_weights[i],
            low_weights[i]
        );
    }

    // A property, not merely incidental: axes untouched by threat_response
    // should not move at all (same in_group_radius, same society fields).
    for axis in Axis::ALL
        .into_iter()
        .filter(|a| !matches!(a, Axis::SizeThreat | Axis::DietPredation))
    {
        let i = index_of(axis);
        assert!(
            close(low_weights[i], high_weights[i]),
            "{}: threat_response should not move an unrelated axis's weight \
             (low={}, high={})",
            axis.label(),
            low_weights[i],
            high_weights[i]
        );
    }
}

// --- Step 3: snap_judgment properties ---------------------------------------

#[test]
fn snap_judgment_is_relationally_asymmetric_across_a_predation_edge() {
    let mut predator = synthetic("test-pred");
    predator.niche =
        ResourceVector::new(&[(hornvale_kernel::ANIMAL_PREY, 1.0)]).expect("valid weights");
    predator.mass = Mass::new(80.0).expect("valid mass");
    let mut prey = synthetic("test-prey");
    prey.niche = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).expect("valid weights");
    prey.mass = Mass::new(40.0).expect("valid mass");
    predator.preys_on.insert(prey.id);

    let predator_view = snap_judgment(&predator, &prey);
    let prey_view = snap_judgment(&prey, &predator);

    assert_ne!(
        predator_view, prey_view,
        "an asymmetric axis feeding both directions must give differing judgments: \
         predator_view={predator_view:?} prey_view={prey_view:?}"
    );
}

#[test]
fn identical_traits_target_lands_at_max_warmth_and_admiration() {
    let judger = synthetic("test-self-a");
    let mut identical = synthetic("test-self-b");
    identical.mind = judger.mind;
    identical.society = SocietyVector {
        sociality: judger.society.sociality,
        status_basis: judger.society.status_basis,
        in_group_radius: judger.society.in_group_radius,
    };

    let j = snap_judgment(&judger, &identical);

    assert!(
        close(j.warmth, 1.0),
        "an identical-traits target should land at max warmth, got {}",
        j.warmth
    );
    assert_eq!(
        j.emotion,
        Emotion::Admiration,
        "self-similarity must never read as Contempt: {j:?}"
    );
}

#[test]
fn self_judgment_lands_at_max_warmth_and_admiration() {
    let p = synthetic("test-literal-self");
    let j = snap_judgment(&p, &p);
    assert!(close(j.warmth, 1.0), "warmth={}", j.warmth);
    assert_eq!(j.emotion, Emotion::Admiration);
}

/// The same threshold rule `snap_judgment`'s classifier uses, restated
/// independently here so this test can check the field-vs-emotion
/// CONSISTENCY property rather than re-asserting a magic number.
fn expected_quadrant(warmth: f64, competence: f64) -> Emotion {
    match (warmth >= 0.5, competence >= 0.5) {
        (true, true) => Emotion::Admiration,
        (false, true) => Emotion::Envy,
        (true, false) => Emotion::Pity,
        (false, false) => Emotion::Contempt,
    }
}

#[test]
fn emotion_always_matches_the_warmth_competence_quadrant() {
    let judger = synthetic("test-quadrant-judger");

    // Baseline (self-similar): Admiration.
    let self_similar = synthetic("test-quadrant-self");
    // Habitat-only mismatch: pushes warmth down, leaves competence alone.
    let mut habitat_mismatch = synthetic("test-quadrant-habitat");
    habitat_mismatch.habitat = HabitatRealm::Subterranean;
    // Status-basis-only mismatch: pushes competence down, leaves warmth alone.
    let mut order_mismatch = synthetic("test-quadrant-order");
    order_mismatch.society.status_basis = StatusBasis::Rank;
    // Both at once.
    let mut both_mismatch = synthetic("test-quadrant-both");
    both_mismatch.habitat = HabitatRealm::Subterranean;
    both_mismatch.society.status_basis = StatusBasis::Rank;

    for target in [
        &self_similar,
        &habitat_mismatch,
        &order_mismatch,
        &both_mismatch,
    ] {
        let j = snap_judgment(&judger, target);
        assert_eq!(
            j.emotion,
            expected_quadrant(j.warmth, j.competence),
            "emotion {:?} does not match its own (warmth={}, competence={}) quadrant",
            j.emotion,
            j.warmth,
            j.competence
        );
    }

    // And the four constructed pairs above should visit (at least) more than
    // one quadrant — otherwise this test would vacuously pass no matter what
    // the classifier does.
    let emotions: BTreeSet<Emotion> = [
        &self_similar,
        &habitat_mismatch,
        &order_mismatch,
        &both_mismatch,
    ]
    .into_iter()
    .map(|target| snap_judgment(&judger, target).emotion)
    .collect();
    assert!(
        emotions.len() > 1,
        "the constructed pairs should not all classify identically: {emotions:?}"
    );
}
