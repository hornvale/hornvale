//! Aggregate social-substrate contract tests.

use hornvale_demography::{
    AssociationDistribution, CareTopology, DescentDistribution, HybridOutcomeDistribution,
    InheritanceDistribution, LifecycleTransition, LifecycleTransitionKind, MigrationDistribution,
    ReproductivePopulationSummary, ReproductivePossibility, RoleAvailabilityDistribution,
    SocialCohortInput, SocialCohortSummary, SocialSubstrateInput, summarize_social_cohort,
    validate_social_cohort,
};
use hornvale_kernel::Years;

fn zero_reproduction() -> SocialSubstrateInput {
    SocialSubstrateInput {
        reproductive: ReproductivePopulationSummary {
            possibility: ReproductivePossibility {
                pathway_count: 0,
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
            reproductive_roles: RoleAvailabilityDistribution::new(vec![]).unwrap(),
            hybrid_outcomes: HybridOutcomeDistribution::new(vec![]).unwrap(),
        },
    }
}

fn representative_input() -> SocialCohortInput {
    SocialCohortInput {
        substrate: zero_reproduction(),
        lifecycle_transitions: vec![
            LifecycleTransition::new(LifecycleTransitionKind::AssociationDissolution, 0.25)
                .unwrap(),
            LifecycleTransition::new(LifecycleTransitionKind::ParentalDeath, 0.125).unwrap(),
        ],
        associations: AssociationDistribution::new(vec![(0, 0.0), (3, 4.0)]).unwrap(),
        descent: DescentDistribution::new(vec![(0, 0.0), (2, 1.0)]).unwrap(),
        care_topology: CareTopology::new(vec![(0, 1.0), (2, 3.0)]).unwrap(),
        migration: MigrationDistribution::new(vec![(0, 1.0), (2, 1.0)]).unwrap(),
        inheritance: InheritanceDistribution::new(vec![(0, 1.0), (2, 3.0)]).unwrap(),
    }
}

/// Removing support for zero outcomes, multi-party associations, overlapping
/// care, migration, dissolution, or parental death makes this test fail.
#[test]
fn legitimate_zeroes_and_non_pair_social_patterns_are_preserved() {
    let input = representative_input();
    validate_social_cohort(&input).unwrap();
    let summary = summarize_social_cohort(&input).unwrap();
    let no_care = CareTopology::new(vec![(0, 1.0)]).unwrap();

    assert_eq!(summary.reproductive.expected_offspring, 0.0);
    assert_eq!(no_care.entries(), &[(0, 1.0)]);
    assert_eq!(summary.associations.entries(), &[(0, 0.0), (3, 1.0)]);
    assert_eq!(summary.care_topology.entries(), &[(0, 0.25), (2, 0.75)]);
    assert_eq!(summary.migration.entries(), &[(0, 0.5), (2, 0.5)]);
    assert_eq!(summary.inheritance.entries(), &[(0, 0.25), (2, 0.75)]);
    assert_eq!(summary.expected_association_participants, 3.0);
    assert_eq!(summary.expected_caregiver_groups_per_dependent, 1.5);
    assert_eq!(summary.expected_migrations_per_person_lifetime, 1.0);
    assert_eq!(
        summary.expected_inheritance_transfers_per_parental_death,
        1.5
    );
    assert_eq!(
        summary
            .lifecycle_transitions
            .iter()
            .map(|transition| (transition.kind(), transition.events_per_person_year()))
            .collect::<Vec<_>>(),
        vec![
            (LifecycleTransitionKind::AssociationDissolution, 0.25),
            (LifecycleTransitionKind::ParentalDeath, 0.125),
        ]
    );
}

/// Flattening care to its expected scalar and discarding its ordered support
/// makes the exact topology comparison fail.
#[test]
fn summary_preserves_overlapping_care_topology() {
    let input = representative_input();
    let authored_topology = input.care_topology.clone();

    let summary = summarize_social_cohort(&input).unwrap();

    assert_eq!(summary.care_topology, authored_topology);
    assert_eq!(summary.care_topology.entries(), &[(0, 0.25), (2, 0.75)]);
}

/// Skipping per-field finite/non-negative checks makes one of these malformed
/// values construct successfully or report the wrong field.
#[test]
fn malformed_social_scalars_identify_their_field() {
    assert_eq!(
        LifecycleTransition::new(LifecycleTransitionKind::Migration, -0.5)
            .unwrap_err()
            .to_string(),
        "lifecycle transition events per person-year must not be negative"
    );
    assert_eq!(
        AssociationDistribution::new(vec![(3, f64::NAN)])
            .unwrap_err()
            .to_string(),
        "association distribution weight at index 0 must be finite"
    );
    assert_eq!(
        DescentDistribution::new(vec![(2, f64::INFINITY)])
            .unwrap_err()
            .to_string(),
        "descent distribution weight at index 0 must be finite"
    );
    assert_eq!(
        CareTopology::new(vec![(2, -1.0)]).unwrap_err().to_string(),
        "care topology weight at index 0 must not be negative"
    );
    assert_eq!(
        MigrationDistribution::new(vec![(1, f64::NEG_INFINITY)])
            .unwrap_err()
            .to_string(),
        "migration distribution weight at index 0 must be finite"
    );
    assert_eq!(
        InheritanceDistribution::new(vec![(1, -0.25)])
            .unwrap_err()
            .to_string(),
        "inheritance distribution weight at index 0 must not be negative"
    );
}

/// Validation must reject the complete cohort before producing a partially
/// normalized summary; mutating the input while checking it fails the final
/// equality.
#[test]
fn cohort_validation_rejects_without_partial_normalization() {
    let mut input = representative_input();
    input.substrate.reproductive.expected_care_burden = f64::NAN;
    let associations_before = input.associations.clone();
    let care_topology_before = input.care_topology.clone();

    assert_eq!(
        validate_social_cohort(&input).unwrap_err().to_string(),
        "reproductive expected care burden must be finite"
    );
    assert_eq!(
        summarize_social_cohort(&input).unwrap_err().to_string(),
        "reproductive expected care burden must be finite"
    );
    assert!(input.substrate.reproductive.expected_care_burden.is_nan());
    assert_eq!(input.associations, associations_before);
    assert_eq!(input.care_topology, care_topology_before);
}

/// Exhaustive destructuring fixes this check's direction: the aggregate
/// summary carries possibility and typicality but no entity, person, gender,
/// or household object. Adding such a field makes this test fail to compile.
#[test]
fn summary_surface_is_aggregate_plain_data() {
    let SocialCohortSummary {
        reproductive,
        lifecycle_transitions,
        associations,
        descent,
        care_topology,
        migration,
        inheritance,
        expected_association_participants,
        expected_descent_lines_per_person,
        expected_caregiver_groups_per_dependent,
        expected_migrations_per_person_lifetime,
        expected_inheritance_transfers_per_parental_death,
    } = summarize_social_cohort(&representative_input()).unwrap();

    assert_eq!(reproductive.possibility.pathway_count, 0);
    assert_eq!(lifecycle_transitions.len(), 2);
    assert_eq!(associations.entries(), &[(0, 0.0), (3, 1.0)]);
    assert_eq!(descent.entries(), &[(0, 0.0), (2, 1.0)]);
    assert_eq!(care_topology.entries(), &[(0, 0.25), (2, 0.75)]);
    assert_eq!(migration.entries(), &[(0, 0.5), (2, 0.5)]);
    assert_eq!(inheritance.entries(), &[(0, 0.25), (2, 0.75)]);
    assert_eq!(expected_association_participants, 3.0);
    assert_eq!(expected_descent_lines_per_person, 2.0);
    assert_eq!(expected_caregiver_groups_per_dependent, 1.5);
    assert_eq!(expected_migrations_per_person_lifetime, 1.0);
    assert_eq!(expected_inheritance_transfers_per_parental_death, 1.5);
}
