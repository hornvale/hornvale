//! Synthetic-only cohort realization at the worldgen composition boundary.

use hornvale_astronomy::SkyPins;
use hornvale_demography::{
    AssociationDistribution, BiologicalCareTopology, BiologicalDevelopmentSite,
    BiologicalTransitionCapability, CareTopology, DescentDistribution, DescentMode,
    DescentRelation, HybridOutcomeDistribution, InheritanceDistribution, LifecycleTransition,
    LifecycleTransitionKind, MigrationDistribution, OffspringOrigin, OffspringPathway,
    ReproductivePopulationSummary, ReproductivePossibility, ReproductiveRole,
    RoleAvailabilityDistribution, SocialCohortInput, SocialCohortSummary, SocialSubstrateInput,
    summarize_social_cohort,
};
use hornvale_kernel::{Seed, Value, World, WorldTime, Years};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SocialProjection, SocialProjectionPins, SyntheticSociety,
    WorldComponents, build_world_to, emit_social_projection, project_social_cohort,
};

fn summary() -> SocialCohortSummary {
    summarize_social_cohort(&SocialCohortInput {
        substrate: SocialSubstrateInput {
            reproductive: ReproductivePopulationSummary {
                possibility: ReproductivePossibility {
                    pathway_count: 1,
                    hybrid_applicable: false,
                    hybrid_outcomes: vec![],
                },
                maturity_age: Years::new(0.0).unwrap(),
                generation_length: Years::new(0.0).unwrap(),
                dependency_duration: Years::new(0.0).unwrap(),
                expected_offspring: 0.0,
                survival_to_independence: 0.0,
                expected_care_burden: 0.0,
                expected_independent_offspring_per_event: 0.0,
                expected_independent_offspring_per_generation: 0.0,
                persistence_balance: 0.0,
                reproductive_roles: RoleAvailabilityDistribution::new(vec![
                    (ReproductiveRole::MaterialProducer, 1.0),
                    (ReproductiveRole::DevelopmentCarrier, 1.0),
                ])
                .unwrap(),
                hybrid_outcomes: HybridOutcomeDistribution::new(vec![]).unwrap(),
            },
        },
        offspring_pathways: vec![OffspringPathway {
            origin: OffspringOrigin::JoinedInputs,
            development_site: BiologicalDevelopmentSite::Body,
            care_topology: Some(BiologicalCareTopology::BodyGroup),
            prerequisite_transition: None,
        }],
        descent_relations: vec![DescentRelation {
            mode: DescentMode::CombinedSources,
            contributing_source_count: 2,
        }],
        compatibility_relations: vec![],
        transition_capabilities: vec![BiologicalTransitionCapability::DevelopmentalMaturation],
        lifecycle_transitions: vec![
            LifecycleTransition::new(LifecycleTransitionKind::Independence, 0.5).unwrap(),
            LifecycleTransition::new(LifecycleTransitionKind::AssociationFormation, 0.5).unwrap(),
            LifecycleTransition::new(LifecycleTransitionKind::AssociationDissolution, 0.25)
                .unwrap(),
            LifecycleTransition::new(LifecycleTransitionKind::Migration, 0.25).unwrap(),
            LifecycleTransition::new(LifecycleTransitionKind::ParentalDeath, 0.125).unwrap(),
        ],
        associations: AssociationDistribution::new(vec![(3, 1.0)]).unwrap(),
        descent: DescentDistribution::new(vec![(2, 1.0)]).unwrap(),
        care_topology: CareTopology::new(vec![(2, 1.0)]).unwrap(),
        migration: MigrationDistribution::new(vec![(2, 1.0)]).unwrap(),
        inheritance: InheritanceDistribution::new(vec![(1, 1.0)]).unwrap(),
    })
    .unwrap()
}

fn pins() -> SocialProjectionPins {
    SocialProjectionPins::new(6, WorldTime::GENESIS).unwrap()
}

fn project(society: SyntheticSociety) -> SocialProjection {
    project_social_cohort(&summary(), Seed(42), &pins(), society).unwrap()
}

fn event_count(projection: &SocialProjection, predicate: &str) -> usize {
    projection
        .events()
        .iter()
        .flat_map(|event| event.facts())
        .filter(|fact| fact.predicate == predicate)
        .count()
}

fn person_fact_count(projection: &SocialProjection, predicate: &str) -> usize {
    projection
        .persons()
        .iter()
        .flat_map(|person| person.facts())
        .filter(|fact| fact.predicate == predicate)
        .count()
}

/// Dropping independent origins, communal care, or the absence of a required
/// pair association makes one of these anti-vacuity counters fail.
#[test]
fn independent_origin_realizes_origin_and_communal_care_without_pair_bond() {
    let projection = project(SyntheticSociety::IndependentOrigin);
    let reproductive_roles = projection
        .persons()
        .iter()
        .flat_map(|person| person.social_facts())
        .filter(|fact| fact.predicate() == hornvale_person::REPRODUCTIVE_ROLE)
        .collect::<Vec<_>>();

    assert!(event_count(&projection, "origin") >= 2);
    assert!(event_count(&projection, "care") >= 2);
    assert_eq!(event_count(&projection, "association"), 0);
    assert!(reproductive_roles.len() >= 2);
    assert_ne!(
        reproductive_roles[0].object(),
        reproductive_roles[1].object()
    );
    assert_eq!(
        person_fact_count(&projection, hornvale_person::GENDER_IDENTITY),
        0
    );
    assert_eq!(
        person_fact_count(&projection, hornvale_person::GENDER_RECOGNITION),
        0
    );
}

/// A sampled social descent count selects only among reproductive paths,
/// descent shapes, and distinct roles that the aggregate explicitly admits.
#[test]
fn independent_origin_requires_explicit_aggregate_reproductive_support() {
    let mut no_possible_pathway = summary();
    no_possible_pathway.reproductive.possibility.pathway_count = 0;
    let error = project_social_cohort(
        &no_possible_pathway,
        Seed(42),
        &pins(),
        SyntheticSociety::IndependentOrigin,
    )
    .unwrap_err();
    assert!(error.to_string().contains("offspring pathway"));

    let mut no_authored_pathway = summary();
    no_authored_pathway.offspring_pathways.clear();
    let error = project_social_cohort(
        &no_authored_pathway,
        Seed(42),
        &pins(),
        SyntheticSociety::IndependentOrigin,
    )
    .unwrap_err();
    assert!(error.to_string().contains("offspring pathway"));

    let mut no_descent_shape = summary();
    no_descent_shape.descent_relations.clear();
    let error = project_social_cohort(
        &no_descent_shape,
        Seed(42),
        &pins(),
        SyntheticSociety::IndependentOrigin,
    )
    .unwrap_err();
    assert!(error.to_string().contains("descent relation"));

    let mut no_roles = summary();
    no_roles.reproductive.reproductive_roles = RoleAvailabilityDistribution::new(vec![]).unwrap();
    let error = project_social_cohort(
        &no_roles,
        Seed(42),
        &pins(),
        SyntheticSociety::IndependentOrigin,
    )
    .unwrap_err();
    assert!(error.to_string().contains("reproductive roles"));
}

/// Letting a synthetic configuration manufacture a path with zero aggregate
/// support would invert the cohort-to-projection causal boundary.
#[test]
fn synthetic_configuration_cannot_override_zero_aggregate_support() {
    let mut aggregate = summary();
    aggregate.care_topology = CareTopology::new(vec![(0, 1.0)]).unwrap();

    let error = project_social_cohort(
        &aggregate,
        Seed(42),
        &pins(),
        SyntheticSociety::IndependentOrigin,
    )
    .unwrap_err();

    assert!(error.to_string().contains("communal care"));
}

/// A lifecycle probe may not create a parental death that its aggregate
/// cohort says never occurs.
#[test]
fn lifecycle_projection_requires_aggregate_parental_death_support() {
    let mut aggregate = summary();
    aggregate
        .lifecycle_transitions
        .retain(|transition| transition.kind() != LifecycleTransitionKind::ParentalDeath);

    let error = project_social_cohort(
        &aggregate,
        Seed(42),
        &pins(),
        SyntheticSociety::LifecycleTransition,
    )
    .unwrap_err();

    assert!(error.to_string().contains("parental-death"));
}

/// Selecting the lifecycle probe cannot manufacture a life-stage transition
/// when the aggregate substrate exposes no matching transition capability.
#[test]
fn lifecycle_projection_requires_aggregate_life_stage_transition_support() {
    let mut aggregate = summary();
    aggregate.transition_capabilities.clear();

    let error = project_social_cohort(
        &aggregate,
        Seed(42),
        &pins(),
        SyntheticSociety::LifecycleTransition,
    )
    .unwrap_err();

    assert!(error.to_string().contains("life-stage transition"));
}

/// Flattening two descent lines to one or omitting their post-death transfer
/// makes these independently derived counters fail.
#[test]
fn dual_descent_realizes_both_lines_and_an_inheritance_transfer() {
    let projection = project(SyntheticSociety::DualDescent);

    assert!(event_count(&projection, "descent") >= 2);
    assert!(event_count(&projection, "transfer") >= 1);
}

/// Replacing overlapping care groups with one household makes either the
/// membership or care witness fall below its literal floor.
#[test]
fn care_cluster_realizes_overlapping_membership_and_multi_party_care() {
    let projection = project(SyntheticSociety::CareCluster);

    assert!(projection.groups().len() >= 2);
    assert!(event_count(&projection, "membership") >= 5);
    assert!(event_count(&projection, "care") >= 2);
}

/// Omitting migration, separation, dissolution, or serial recomposition makes
/// its named path counter fail instead of allowing an empty graph to pass.
#[test]
fn recomposing_mobility_exercises_every_recomposition_path() {
    let projection = project(SyntheticSociety::RecomposingMobility);

    assert!(event_count(&projection, "association") >= 2);
    assert!(event_count(&projection, "residence") >= 2);
    assert!(event_count(&projection, "separate") >= 1);
    assert!(event_count(&projection, "dissolve") >= 1);
}

/// Conflating association with recognition makes the event counts or their
/// strictly ordered timestamps fail.
#[test]
fn institutional_recognition_follows_an_existing_association() {
    let projection = project(SyntheticSociety::InstitutionalRecognition);
    let association = projection
        .events()
        .iter()
        .flat_map(|event| event.facts())
        .find(|fact| fact.predicate == "association")
        .expect("association path is exercised");
    let recognition = projection
        .events()
        .iter()
        .flat_map(|event| event.facts())
        .find(|fact| fact.predicate == "recognition")
        .expect("recognition path is exercised");

    assert!(association.day < recognition.day);
}

/// Erasing historical care/descent on death, retaining the old dependency,
/// failing to assign future care, or collapsing lifecycle categories and
/// social roles makes a separate witness fail.
#[test]
fn lifecycle_transition_preserves_history_and_reassigns_future_care() {
    let projection = project(SyntheticSociety::LifecycleTransition);
    let transitioned = projection
        .persons()
        .iter()
        .find(|person| {
            person
                .social_facts()
                .iter()
                .any(|fact| fact.predicate() == hornvale_person::TRANSITIONED)
        })
        .expect("life-stage transition path is exercised");
    let categories = transitioned
        .social_facts()
        .iter()
        .filter(|fact| fact.predicate() == hornvale_person::GENDER_RECOGNITION)
        .collect::<Vec<_>>();
    let roles = transitioned
        .social_facts()
        .iter()
        .filter(|fact| fact.predicate() == hornvale_person::SOCIAL_ROLE)
        .collect::<Vec<_>>();
    let transition_markers = transitioned
        .social_facts()
        .iter()
        .filter(|fact| fact.predicate() == hornvale_person::TRANSITIONED)
        .collect::<Vec<_>>();

    assert!(person_fact_count(&projection, hornvale_person::TRANSITIONED) >= 1);
    assert_eq!(categories.len(), 2);
    let transition = categories[1].start();
    assert_eq!(
        categories[0].object(),
        &Value::Text("pre-independence".to_string())
    );
    assert_eq!(
        categories[1].object(),
        &Value::Text("post-independence".to_string())
    );
    assert_eq!(categories[0].end(), Some(transition));
    assert_eq!(categories[1].end(), None);
    assert_eq!(roles.len(), 2);
    assert_eq!(roles[0].object(), &Value::Text("dependent".to_string()));
    assert_eq!(roles[1].object(), &Value::Text("independent".to_string()));
    assert_eq!(roles[0].end(), Some(transition));
    assert_eq!(roles[1].start(), transition);
    assert_eq!(roles[1].end(), None);
    assert_eq!(transition_markers.len(), 1);
    assert_eq!(
        transition_markers[0].object(),
        &Value::Text("life-stage-social-role".to_string())
    );
    assert_eq!(transition_markers[0].start(), transition);
    assert_eq!(transition_markers[0].end(), None);
    let emitted_facts = transitioned.facts();
    let lifecycle_facts = emitted_facts
        .iter()
        .filter(|fact| {
            fact.predicate == hornvale_person::GENDER_RECOGNITION
                || fact.predicate == hornvale_person::GENDER_RECOGNITION_ENDED
                || fact.predicate == hornvale_person::SOCIAL_ROLE
                || fact.predicate == hornvale_person::SOCIAL_ROLE_ENDED
                || fact.predicate == hornvale_person::TRANSITIONED
        })
        .collect::<Vec<_>>();
    assert_eq!(lifecycle_facts.len(), 7);
    assert!(
        lifecycle_facts
            .iter()
            .all(|fact| { fact.provenance == "social/projection/v1" && fact.day.is_some() })
    );
    let role_end = lifecycle_facts
        .iter()
        .find(|fact| fact.predicate == hornvale_person::SOCIAL_ROLE_ENDED)
        .expect("the pre-transition role has an exclusive end companion");
    assert_eq!(role_end.object, Value::Text("dependent".to_string()));
    assert_eq!(role_end.day, Some(transition));
    let provenance_facts = emitted_facts
        .iter()
        .filter(|fact| fact.predicate == hornvale_person::PERSON_SOCIAL_PROVENANCE)
        .collect::<Vec<_>>();
    assert_eq!(provenance_facts.len(), 5);
    assert!(provenance_facts.iter().all(|fact| {
        fact.object == Value::Text("social/projection/v1".to_string())
            && fact.provenance == "social/projection/v1"
    }));
    assert!(event_count(&projection, "descent") >= 1);
    assert!(event_count(&projection, "care") >= 2);
    assert!(event_count(&projection, "care-ended") >= 1);
    assert!(event_count(&projection, "dependency") >= 2);
    assert!(event_count(&projection, "dependency-ended") >= 1);
    assert!(event_count(&projection, "die") >= 1);

    let mut world = projection_world(Seed(42));
    emit_social_projection(&mut world, Some(&projection)).unwrap();
    let deceased = world
        .ledger
        .find("die")
        .next()
        .expect("parental-death path is exercised")
        .subject;
    assert!(
        world
            .ledger
            .facts_about(deceased)
            .any(|fact| fact.predicate == "descent")
    );
    assert!(
        world
            .ledger
            .facts_about(deceased)
            .any(|fact| fact.predicate == "care")
    );
    assert!(
        world
            .ledger
            .facts_about(deceased)
            .any(|fact| fact.predicate == "die")
    );
}

fn projection_facts(projection: &SocialProjection) -> Vec<hornvale_kernel::Fact> {
    projection
        .persons()
        .iter()
        .flat_map(|person| person.facts())
        .chain(projection.events().iter().flat_map(|event| event.facts()))
        .collect()
}

fn projection_world(seed: Seed) -> World {
    let mut world = World::new(seed);
    hornvale_person::register_concepts(&mut world.registry).unwrap();
    hornvale_history::register_concepts(&mut world.registry).unwrap();
    world
}

/// Consuming a different draw, iterating an unordered collection, or changing
/// mint order makes the two complete save-format byte strings differ.
#[test]
fn identical_seed_pins_and_society_emit_byte_identical_history() {
    let a = project_social_cohort(
        &summary(),
        Seed(77),
        &pins(),
        SyntheticSociety::RecomposingMobility,
    )
    .unwrap();
    let b = project_social_cohort(
        &summary(),
        Seed(77),
        &pins(),
        SyntheticSociety::RecomposingMobility,
    )
    .unwrap();
    let mut world_a = projection_world(Seed(77));
    let mut world_b = projection_world(Seed(77));
    emit_social_projection(&mut world_a, Some(&a)).unwrap();
    emit_social_projection(&mut world_b, Some(&b)).unwrap();

    assert_eq!(world_a.to_json().as_bytes(), world_b.to_json().as_bytes());
}

/// Ignoring the named realization stream makes distinct seeds produce the
/// same event bytes even though the aggregate input and pins are identical.
#[test]
fn distinct_seeds_change_only_the_opt_in_projection_facts() {
    let a = project_social_cohort(
        &summary(),
        Seed(41),
        &pins(),
        SyntheticSociety::RecomposingMobility,
    )
    .unwrap();
    let b = project_social_cohort(
        &summary(),
        Seed(42),
        &pins(),
        SyntheticSociety::RecomposingMobility,
    )
    .unwrap();

    assert_ne!(
        serde_json::to_vec(&projection_facts(&a)).unwrap(),
        serde_json::to_vec(&projection_facts(&b)).unwrap()
    );
}

/// Mutating aggregate input or feeding synthetic probe state into the authored
/// registry/default build makes one of these before/after byte witnesses fail.
#[test]
fn disabled_projection_preserves_aggregate_registry_and_default_world_bytes() {
    let aggregate = summary();
    let aggregate_before = aggregate.clone();
    let species_before: Vec<&'static str> = hornvale_species::biosphere_registry()
        .iter()
        .map(|(kind, _)| kind.0)
        .collect();
    let wc = WorldComponents::assemble().unwrap();
    let default_before = build_world_to(
        Seed(9),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Astronomy,
    )
    .unwrap();

    let projection =
        project_social_cohort(&aggregate, Seed(9), &pins(), SyntheticSociety::CareCluster).unwrap();
    let mut disabled = default_before.clone();
    emit_social_projection(&mut disabled, None).unwrap();

    let species_after: Vec<&'static str> = hornvale_species::biosphere_registry()
        .iter()
        .map(|(kind, _)| kind.0)
        .collect();
    assert_eq!(aggregate, aggregate_before);
    assert_eq!(species_before, species_after);
    assert_eq!(disabled.to_json(), default_before.to_json());
    assert!(!projection.events().is_empty());
    assert_eq!(
        default_before
            .ledger
            .find(hornvale_person::IS_PERSON)
            .count(),
        0
    );
    assert_eq!(
        default_before
            .ledger
            .find("association")
            .filter(|fact| matches!(fact.object, Value::Entity(_)))
            .count(),
        0
    );
}
