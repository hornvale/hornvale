//! The Grammar, Task 4: the species-to-demography composition-root boundary.
//!
//! Test fixture (decision 0092): the zero-drift test reconstructs terrain and
//! climate from the committed seed-42 world because those non-serialized
//! artifacts are the existing seam the additive adapter must leave unchanged.
#![allow(clippy::disallowed_methods)]

use hornvale_demography::{
    HybridOutcome, IndependenceOutcome, ReproductivePopulationInput, summarize_reproduction,
};
use hornvale_kernel::{ComponentStore, KindId, Years};
use hornvale_species::{
    CompatibilityContext, CompatibilityRule, DevelopmentSite, DevelopmentalTiming, GuardStatus,
    MaterialCompatibility, ReproductiveAffordances, ReproductiveOperation, ReproductiveRole,
    SupportMode,
};
use hornvale_worldgen::{
    BuildArtifacts, HybridPartnerConfig, ReproductivePopulationConfig, climate_from,
    occupation_records, reproductive_substrate_from, seed_42_world, terrain_of,
};

const PAIRBORN: KindId = KindId("pairborn");
const DEVELOPER: KindId = KindId("developer");
const STILL: KindId = KindId("still");

fn pairborn() -> ReproductiveAffordances {
    use ReproductiveOperation::{Grow, Join, Make, Release, Support};
    use ReproductiveRole::{
        DevelopmentCarrier, DevelopmentSupporter, MaterialContributor, MaterialProducer,
    };

    ReproductiveAffordances::new(
        vec![Make, Join, Grow, Support, Release],
        vec![DevelopmentSite::Body],
        vec![SupportMode::Individual],
        vec![
            MaterialProducer,
            MaterialContributor,
            DevelopmentCarrier,
            DevelopmentSupporter,
        ],
        vec![],
    )
}

fn developer_only() -> ReproductiveAffordances {
    use ReproductiveOperation::{Grow, Join, Release};
    use ReproductiveRole::{DevelopmentCarrier, MaterialContributor};

    ReproductiveAffordances::new(
        vec![Join, Grow, Release],
        vec![DevelopmentSite::Body],
        vec![],
        vec![MaterialContributor, DevelopmentCarrier],
        vec![],
    )
}

fn context() -> CompatibilityContext {
    CompatibilityContext {
        available_roles: vec![
            ReproductiveRole::MaterialProducer,
            ReproductiveRole::MaterialContributor,
            ReproductiveRole::DevelopmentCarrier,
            ReproductiveRole::DevelopmentSupporter,
        ],
        development_sites: vec![DevelopmentSite::Body],
        support_modes: vec![SupportMode::Individual],
        timing: DevelopmentalTiming::Ready,
        environment: GuardStatus::Satisfied,
        resources: GuardStatus::Satisfied,
        assistance: vec![],
        first_to_second: CompatibilityRule {
            material: MaterialCompatibility::Fertile,
            required_assistance: vec![],
        },
        second_to_first: CompatibilityRule {
            material: MaterialCompatibility::Fertile,
            required_assistance: vec![],
        },
    }
}

fn ordinary_config() -> ReproductivePopulationConfig {
    ReproductivePopulationConfig {
        context: context(),
        maturity_age: Years::new(12.0).unwrap(),
        generation_length: Years::new(20.0).unwrap(),
        offspring: vec![(1, 1.0), (3, 3.0)],
        survival_to_independence: vec![
            (IndependenceOutcome::Survives, 3.0),
            (IndependenceOutcome::DoesNotSurvive, 1.0),
        ],
        dependency_duration: Years::new(4.0).unwrap(),
        care_burden: vec![(2.0, 1.0), (6.0, 3.0)],
        reproductive_roles: vec![
            (ReproductiveRole::MaterialProducer, 1.0),
            (ReproductiveRole::DevelopmentCarrier, 3.0),
        ],
        hybrid_partners: vec![],
        reproductive_events_per_generation: 0.8,
        replacement_requirement: 1.5,
    }
}

fn no_reproduction_config() -> ReproductivePopulationConfig {
    ReproductivePopulationConfig {
        context: context(),
        maturity_age: Years::new(0.0).unwrap(),
        generation_length: Years::new(0.0).unwrap(),
        offspring: vec![],
        survival_to_independence: vec![],
        dependency_duration: Years::new(0.0).unwrap(),
        care_burden: vec![],
        reproductive_roles: vec![],
        hybrid_partners: vec![],
        reproductive_events_per_generation: 0.0,
        replacement_requirement: 0.0,
    }
}

fn registry(
    entries: Vec<(KindId, ReproductiveAffordances)>,
) -> ComponentStore<KindId, ReproductiveAffordances> {
    entries.into_iter().collect()
}

fn configurations(
    entries: Vec<(KindId, ReproductivePopulationConfig)>,
) -> ComponentStore<KindId, ReproductivePopulationConfig> {
    entries.into_iter().collect()
}

/// This test fixes the direction of the boundary: worldgen consumes the
/// species-owned registry and emits demography-owned plain data. The workspace
/// architecture test separately enforces that neither domain imports its
/// sibling.
#[test]
fn worldgen_owns_the_species_to_demography_boundary() {
    let affordances = registry(vec![(PAIRBORN, pairborn())]);
    let configs = configurations(vec![(PAIRBORN, ordinary_config())]);

    let substrate = reproductive_substrate_from(&affordances, &configs).unwrap();
    let input: &ReproductivePopulationInput = substrate.get(&PAIRBORN).unwrap();

    assert_eq!(input.possibility.pathway_count, 1);
    assert!(input.possibility.hybrid_outcomes.is_empty());
    assert_eq!(input.typicality.maturity_age.get(), 12.0);
    assert_eq!(input.typicality.generation_length.get(), 20.0);
    assert_eq!(
        input.typicality.offspring.entries(),
        &[(1, 0.25), (3, 0.75)]
    );
    assert_eq!(
        input.typicality.reproductive_roles.entries(),
        &[
            (
                hornvale_demography::ReproductiveRole::MaterialProducer,
                0.25
            ),
            (
                hornvale_demography::ReproductiveRole::DevelopmentCarrier,
                0.75
            ),
        ]
    );
    assert_eq!(input.persistence.reproductive_events_per_generation(), 0.8);
    assert_eq!(input.persistence.replacement_requirement(), 1.5);
}

#[test]
fn ordinary_reproduction_without_hybrid_partners_summarizes_successfully() {
    let affordances = registry(vec![(PAIRBORN, pairborn())]);
    let configs = configurations(vec![(PAIRBORN, ordinary_config())]);

    let substrate = reproductive_substrate_from(&affordances, &configs).unwrap();
    let input = substrate.get(&PAIRBORN).unwrap();
    let summary = summarize_reproduction(input).unwrap();

    assert_eq!(summary.possibility.pathway_count, 1);
    assert!(!summary.possibility.hybrid_applicable);
    assert!(summary.hybrid_outcomes.is_empty());
}

#[test]
fn hybrid_relations_are_converted_directionally_without_species_pair_exceptions() {
    let affordances = registry(vec![(PAIRBORN, pairborn()), (DEVELOPER, developer_only())]);
    let mut config = ordinary_config();
    config.hybrid_partners.push(HybridPartnerConfig {
        partner: DEVELOPER,
        context: context(),
        first_to_second_weight: 3.0,
        second_to_first_weight: 1.0,
    });
    let configs = configurations(vec![(PAIRBORN, config)]);

    let substrate = reproductive_substrate_from(&affordances, &configs).unwrap();
    let input = substrate.get(&PAIRBORN).unwrap();

    assert!(input.possibility.hybrid_applicable);
    assert_eq!(
        input.possibility.hybrid_outcomes,
        vec![HybridOutcome::Fertile]
    );
    assert_eq!(
        input.typicality.hybrid_outcomes.entries(),
        &[
            (HybridOutcome::Fertile, 0.75),
            (HybridOutcome::Impossible, 0.25)
        ]
    );
}

#[test]
fn a_non_reproducing_profile_crosses_the_boundary_as_valid_zero_input() {
    let affordances = registry(vec![(STILL, ReproductiveAffordances::empty())]);
    let configs = configurations(vec![(STILL, no_reproduction_config())]);

    let substrate = reproductive_substrate_from(&affordances, &configs).unwrap();
    let input = substrate.get(&STILL).unwrap();

    assert_eq!(input.possibility.pathway_count, 0);
    assert!(input.possibility.hybrid_outcomes.is_empty());
    assert!(input.typicality.offspring.is_empty());
    assert!(input.typicality.survival_to_independence.is_empty());
    assert!(input.typicality.care_burden.is_empty());
    assert!(input.typicality.reproductive_roles.is_empty());
    assert!(input.typicality.hybrid_outcomes.is_empty());
}

#[test]
fn repeated_resolution_is_identical_and_keeps_kind_order() {
    let affordances = registry(vec![
        (STILL, ReproductiveAffordances::empty()),
        (PAIRBORN, pairborn()),
    ]);
    let configs = configurations(vec![
        (STILL, no_reproduction_config()),
        (PAIRBORN, ordinary_config()),
    ]);

    let first = reproductive_substrate_from(&affordances, &configs).unwrap();
    let second = reproductive_substrate_from(&affordances, &configs).unwrap();

    assert_eq!(first, second);
    assert_eq!(
        first.ids().copied().collect::<Vec<_>>(),
        vec![PAIRBORN, STILL]
    );
}

#[test]
fn adapter_refusals_name_the_requested_kind_and_failed_boundary() {
    let affordances = registry(vec![]);
    let configs = configurations(vec![(PAIRBORN, ordinary_config())]);
    assert_eq!(
        reproductive_substrate_from(&affordances, &configs)
            .unwrap_err()
            .to_string(),
        "reproductive configuration for pairborn has no affordance profile"
    );

    let invalid = ReproductiveAffordances::new(
        vec![],
        vec![],
        vec![],
        vec![ReproductiveRole::MaterialProducer],
        vec![],
    );
    let affordances = registry(vec![(PAIRBORN, invalid)]);
    assert_eq!(
        reproductive_substrate_from(&affordances, &configs)
            .unwrap_err()
            .to_string(),
        "reproductive affordances for pairborn are invalid: reproductive roles require at least one operation"
    );
}

#[test]
fn explicit_reproductive_resolution_is_inert_for_current_world_builds() {
    let world = seed_42_world();
    let before = world.to_json();
    let terrain = terrain_of(&world).expect("fixture terrain re-derives");
    let climate = climate_from(&world, &terrain).expect("fixture climate re-derives");
    let artifacts = BuildArtifacts {
        world,
        terrain: Some(terrain),
        climate: Some(climate),
    };

    let affordances = registry(vec![(PAIRBORN, pairborn())]);
    let configs = configurations(vec![(PAIRBORN, ordinary_config())]);
    let substrate = reproductive_substrate_from(&affordances, &configs).unwrap();
    assert!(substrate.get(&PAIRBORN).is_some());

    let BuildArtifacts {
        world,
        terrain,
        climate,
    } = artifacts;
    assert!(!hornvale_terrain::places(&world).is_empty());
    assert!(!occupation_records(&world).is_empty());
    assert_eq!(before, world.to_json());
    assert!(terrain.is_some());
    assert!(climate.is_some());
}
