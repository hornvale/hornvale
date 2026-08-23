//! Per-axis structural-property tests (Task 1, Step 2/3): for each of the
//! eight snap-judgment axes, assert the PROPERTY the axis must have —
//! normalized to `[0,1]`, symmetric axes agree both directions, asymmetric
//! ones may differ, and a bigger underlying attribute gap gives a bigger
//! distance. Deliberately no test asserts a specific real pair's magnitude
//! (0021: that would author an outcome).
//!
//! Most tests build synthetic, hand-authored `PeopleTraits` (via the
//! crate's public fields) rather than reading two real peoples, so the
//! gaps under test are exactly the ones each assertion names. A handful of
//! whole-catalog tests run every axis over the real fifteen-people roster
//! to confirm the range property holds on authored data too.

use std::collections::BTreeSet;

use hornvale_kernel::{ANIMAL_PREY, ConditionResponse, Mass, PLANT_FORAGE, ResourceVector};
use hornvale_language::speech::{ArticulationVector, ExoticManner};
use hornvale_sentiment::{Axis, PeopleId, PeopleTraits, axis_distance, catalog};
use hornvale_species::{
    ActivityCycle, ConditionNiche, HabitatRealm, LifeSchedule, MetabolicClass, MindVector,
    PerceptionVector, Sociality, SocietyVector, StatusBasis,
};

/// A synthetic, self-consistent `PeopleTraits` for probing a single axis's
/// structural properties in isolation. Deliberately not drawn from the real
/// catalog: every field below is a round authoring choice (0.5, 70 kg,
/// Diurnal, …), so nothing here can be read as an opinion about a named
/// people (0021) — only the perturbations the tests apply to it can.
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
            sociality: Sociality::Hierarchic,
            status_basis: StatusBasis::Rank,
            in_group_radius: 0.5,
        },
        mind: MindVector::MANIKIN,
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

// --- Whole-catalog range and symmetry checks (real, authored data) --------

#[test]
fn every_axis_distance_is_in_unit_range_over_the_real_roster() {
    let cat = catalog();
    let ids: Vec<PeopleId> = cat.keys().copied().collect();
    for axis in Axis::ALL {
        for &a in &ids {
            for &b in &ids {
                let d = axis_distance(axis, &cat[&a], &cat[&b]);
                assert!(
                    d.is_finite() && (0.0..=1.0).contains(&d),
                    "{}: distance({a:?},{b:?}) = {d} outside [0,1]",
                    axis.label()
                );
            }
        }
    }
}

#[test]
fn symmetric_axes_agree_both_directions_over_the_real_roster() {
    let cat = catalog();
    let ids: Vec<PeopleId> = cat.keys().copied().collect();
    for axis in Axis::ALL.into_iter().filter(|a| !a.is_asymmetric()) {
        for &a in &ids {
            for &b in &ids {
                let ab = axis_distance(axis, &cat[&a], &cat[&b]);
                let ba = axis_distance(axis, &cat[&b], &cat[&a]);
                assert!(
                    close(ab, ba),
                    "{}: distance({a:?},{b:?})={ab} != distance({b:?},{a:?})={ba}",
                    axis.label()
                );
            }
        }
    }
}

#[test]
fn every_axis_self_distance_is_zero_over_the_real_roster() {
    let cat = catalog();
    for axis in Axis::ALL {
        for traits in cat.values() {
            let d = axis_distance(axis, traits, traits);
            assert!(
                close(d, 0.0),
                "{}: self-distance for {:?} should be 0, got {d}",
                axis.label(),
                traits.id
            );
        }
    }
}

// --- Per-axis structural properties, on synthetic (controlled) pairs ------

#[test]
fn habitat_distance_is_categorical_and_symmetric() {
    assert!(!Axis::Habitat.is_asymmetric());
    let mut a = synthetic("test-a");
    let mut b = synthetic("test-b");
    assert!(close(axis_distance(Axis::Habitat, &a, &b), 0.0));
    b.habitat = HabitatRealm::Subterranean;
    assert!(close(axis_distance(Axis::Habitat, &a, &b), 1.0));
    assert!(close(
        axis_distance(Axis::Habitat, &b, &a),
        axis_distance(Axis::Habitat, &a, &b)
    ));
    a.habitat = HabitatRealm::Subterranean;
    assert!(close(axis_distance(Axis::Habitat, &a, &b), 0.0));
}

#[test]
fn diet_predation_distance_grows_with_niche_divergence() {
    assert!(Axis::DietPredation.is_asymmetric());
    let a = synthetic("test-a");
    let mut b_near = synthetic("test-b");
    b_near.niche =
        ResourceVector::new(&[(PLANT_FORAGE, 0.9), (ANIMAL_PREY, 0.1)]).expect("valid weights");
    let mut b_far = synthetic("test-c");
    b_far.niche = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).expect("valid weights");

    let d_near = axis_distance(Axis::DietPredation, &a, &b_near);
    let d_far = axis_distance(Axis::DietPredation, &a, &b_far);
    assert!(
        d_far > d_near,
        "bigger diet-niche gap should give bigger distance: near={d_near} far={d_far}"
    );
}

#[test]
fn diet_predation_direction_differs_across_a_real_predation_edge() {
    let mut predator = synthetic("test-pred");
    predator.niche = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).expect("valid weights");
    predator.mass = Mass::new(80.0).expect("valid mass");
    let mut prey = synthetic("test-prey");
    prey.niche = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).expect("valid weights");
    prey.mass = Mass::new(40.0).expect("valid mass");
    predator.preys_on.insert(prey.id);

    let predator_view = axis_distance(Axis::DietPredation, &predator, &prey);
    let prey_view = axis_distance(Axis::DietPredation, &prey, &predator);
    assert!(
        !close(predator_view, prey_view),
        "an asymmetric axis with a real predation edge must differ by direction: \
         predator_view={predator_view} prey_view={prey_view}"
    );
    assert!(
        predator_view < prey_view,
        "the predator's distance to its own prey should be narrower (familiarity) \
         than the prey's distance to its predator (fear): predator_view={predator_view} \
         prey_view={prey_view}"
    );
}

#[test]
fn condition_niche_distance_grows_with_optimum_gap() {
    assert!(!Axis::ConditionNiche.is_asymmetric());
    let a = synthetic("test-a");
    let mut b_near = synthetic("test-b");
    b_near.condition_niche.temperature.optimum = 18.0;
    let mut b_far = synthetic("test-c");
    b_far.condition_niche.temperature.optimum = 35.0;

    let d_near = axis_distance(Axis::ConditionNiche, &a, &b_near);
    let d_far = axis_distance(Axis::ConditionNiche, &a, &b_far);
    assert!(d_far > d_near, "near={d_near} far={d_far}");
    assert!(close(
        axis_distance(Axis::ConditionNiche, &b_near, &a),
        d_near
    ));
}

#[test]
fn sociality_distance_grows_with_in_group_radius_gap() {
    assert!(!Axis::Sociality.is_asymmetric());
    let a = synthetic("test-a");
    let mut b_near = synthetic("test-b");
    b_near.society.in_group_radius = 0.6;
    let mut b_far = synthetic("test-c");
    b_far.society.in_group_radius = 1.0;

    let d_near = axis_distance(Axis::Sociality, &a, &b_near);
    let d_far = axis_distance(Axis::Sociality, &a, &b_far);
    assert!(d_far > d_near, "near={d_near} far={d_far}");
}

#[test]
fn activity_cycle_distance_grows_with_night_vision_gap() {
    assert!(!Axis::ActivityCycle.is_asymmetric());
    let a = synthetic("test-a");
    let mut b_near = synthetic("test-b");
    b_near.perception.night_vision = 0.6;
    let mut b_far = synthetic("test-c");
    b_far.perception.night_vision = 1.0;

    let d_near = axis_distance(Axis::ActivityCycle, &a, &b_near);
    let d_far = axis_distance(Axis::ActivityCycle, &a, &b_far);
    assert!(d_far > d_near, "near={d_near} far={d_far}");
}

#[test]
fn reproductive_distance_grows_with_mass_gap() {
    assert!(!Axis::Reproductive.is_asymmetric());
    let a = synthetic("test-a");
    let mut b_near = synthetic("test-b");
    b_near.mass = Mass::new(100.0).expect("valid mass");
    let mut b_far = synthetic("test-c");
    b_far.mass = Mass::new(5_000.0).expect("valid mass");

    let d_near = axis_distance(Axis::Reproductive, &a, &b_near);
    let d_far = axis_distance(Axis::Reproductive, &a, &b_far);
    assert!(d_far > d_near, "near={d_near} far={d_far}");
}

#[test]
fn language_distance_grows_with_labiality_gap() {
    assert!(!Axis::Language.is_asymmetric());
    let a = synthetic("test-a");
    let mut b_near = synthetic("test-b");
    b_near.articulation.labiality = 0.6;
    let mut b_far = synthetic("test-c");
    b_far.articulation.labiality = 1.0;

    let d_near = axis_distance(Axis::Language, &a, &b_near);
    let d_far = axis_distance(Axis::Language, &a, &b_far);
    assert!(d_far > d_near, "near={d_near} far={d_far}");
}

#[test]
fn language_distance_reacts_to_exotic_manner_mismatch() {
    let a = synthetic("test-a");
    let mut b = synthetic("test-b");
    let baseline = axis_distance(Axis::Language, &a, &b);
    b.articulation.exotic = ExoticManner::Click;
    let with_mismatch = axis_distance(Axis::Language, &a, &b);
    assert!(
        with_mismatch > baseline,
        "an exotic-manner mismatch should widen the language distance: \
         baseline={baseline} with_mismatch={with_mismatch}"
    );
}

#[test]
fn size_threat_distance_grows_with_how_much_bigger_b_is_and_is_directional() {
    assert!(Axis::SizeThreat.is_asymmetric());
    let a = synthetic("test-a"); // 70 kg
    let mut b_near = synthetic("test-b");
    b_near.mass = Mass::new(100.0).expect("valid mass");
    let mut b_far = synthetic("test-c");
    b_far.mass = Mass::new(10_000.0).expect("valid mass");
    let mut b_tiny = synthetic("test-d");
    b_tiny.mass = Mass::new(0.1).expect("valid mass");

    let d_near = axis_distance(Axis::SizeThreat, &a, &b_near);
    let d_far = axis_distance(Axis::SizeThreat, &a, &b_far);
    let d_tiny = axis_distance(Axis::SizeThreat, &a, &b_tiny);
    assert!(d_far > d_near, "near={d_near} far={d_far}");
    assert!(
        d_tiny < d_near,
        "a much-smaller B should register less threat than a somewhat-bigger one: \
         tiny={d_tiny} near={d_near}"
    );

    // Directional: A's view of a much-bigger B is not B's view of A.
    let a_view = axis_distance(Axis::SizeThreat, &a, &b_far);
    let b_view = axis_distance(Axis::SizeThreat, &b_far, &a);
    assert!(
        !close(a_view, b_view),
        "SizeThreat must differ by direction: a_view={a_view} b_view={b_view}"
    );
    assert!(
        a_view > b_view,
        "the smaller party's view of the larger one should carry the signal: \
         a_view={a_view} b_view={b_view}"
    );
}
