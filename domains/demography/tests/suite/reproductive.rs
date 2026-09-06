//! The Grammar, Task 3: the species-independent population handoff.

use hornvale_demography::{
    CareBurdenDistribution, HybridOutcome, HybridOutcomeDistribution, IndependenceOutcome,
    OffspringDistribution, PopulationPersistenceInputs, ReproductivePopulationInput,
    ReproductivePopulationSummary, ReproductivePossibility, ReproductiveRole,
    ReproductiveTypicality, RoleAvailabilityDistribution, SocialSubstrateInput,
    SurvivalDistribution, social_substrate_input, summarize_reproduction,
};
use hornvale_kernel::Years;

fn typical_input() -> ReproductivePopulationInput {
    ReproductivePopulationInput {
        possibility: ReproductivePossibility {
            pathway_count: 2,
            hybrid_outcomes: vec![HybridOutcome::Fertile, HybridOutcome::ViableButSterile],
        },
        typicality: ReproductiveTypicality {
            maturity_age: Years::new(12.0).unwrap(),
            generation_length: Years::new(20.0).unwrap(),
            offspring: OffspringDistribution::new(vec![(1, 1.0), (3, 3.0)]).unwrap(),
            survival_to_independence: SurvivalDistribution::new(vec![
                (IndependenceOutcome::Survives, 3.0),
                (IndependenceOutcome::DoesNotSurvive, 1.0),
            ])
            .unwrap(),
            dependency_duration: Years::new(4.0).unwrap(),
            care_burden: CareBurdenDistribution::new(vec![(2.0, 1.0), (6.0, 3.0)]).unwrap(),
            reproductive_roles: RoleAvailabilityDistribution::new(vec![
                (ReproductiveRole::MaterialProducer, 1.0),
                (ReproductiveRole::DevelopmentCarrier, 3.0),
            ])
            .unwrap(),
            hybrid_outcomes: HybridOutcomeDistribution::new(vec![
                (HybridOutcome::Fertile, 1.0),
                (HybridOutcome::ViableButSterile, 1.0),
            ])
            .unwrap(),
        },
        persistence: PopulationPersistenceInputs::new(0.8, 1.5).unwrap(),
    }
}

#[test]
fn summary_carries_the_complete_population_handoff() {
    let summary = summarize_reproduction(&typical_input()).unwrap();

    assert_eq!(summary.possibility.pathway_count, 2);
    assert_eq!(summary.maturity_age.get(), 12.0);
    assert_eq!(summary.generation_length.get(), 20.0);
    assert_eq!(summary.dependency_duration.get(), 4.0);
    assert_eq!(summary.expected_offspring, 2.5);
    assert_eq!(summary.survival_to_independence, 0.75);
    assert_eq!(summary.expected_care_burden, 5.0);
    assert_eq!(summary.expected_independent_offspring_per_event, 1.875);
    assert_eq!(summary.expected_independent_offspring_per_generation, 1.5);
    assert_eq!(summary.persistence_balance, 0.0);
    assert_eq!(summary.reproductive_roles.total_weight(), 1.0);
    assert_eq!(summary.hybrid_outcomes.total_weight(), 1.0);
}

/// Exhaustive destructuring fixes the direction of this check: the public
/// handoff contains exactly these plain biological fields, so adding an
/// identity, registry, anatomy, social-gender, or projection field fails here.
#[test]
fn handoff_surface_is_species_independent_plain_data() {
    let input = typical_input();
    let ReproductivePopulationInput {
        possibility,
        typicality,
        persistence,
    } = input;
    let ReproductivePossibility {
        pathway_count,
        hybrid_outcomes: possible_hybrids,
    } = possibility;
    let ReproductiveTypicality {
        maturity_age,
        generation_length,
        offspring,
        survival_to_independence,
        dependency_duration,
        care_burden,
        reproductive_roles,
        hybrid_outcomes,
    } = typicality;

    assert_eq!(pathway_count, 2);
    assert_eq!(possible_hybrids.len(), 2);
    assert_eq!(maturity_age.get(), 12.0);
    assert_eq!(generation_length.get(), 20.0);
    assert_eq!(offspring.len(), 2);
    assert_eq!(survival_to_independence.len(), 2);
    assert_eq!(dependency_duration.get(), 4.0);
    assert_eq!(care_burden.len(), 2);
    assert_eq!(reproductive_roles.len(), 2);
    assert_eq!(hybrid_outcomes.len(), 2);
    assert_eq!(persistence.reproductive_events_per_generation(), 0.8);
    assert_eq!(persistence.replacement_requirement(), 1.5);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SocialContext {
    PairHousehold,
    CommunalCare,
}

fn consume_socially(
    input: &SocialSubstrateInput,
    context: SocialContext,
) -> (ReproductivePopulationSummary, SocialContext) {
    (input.reproductive.clone(), context)
}

#[test]
fn different_social_contexts_do_not_rewrite_biological_summary() {
    let biological = summarize_reproduction(&typical_input()).unwrap();
    let social = social_substrate_input(biological.clone());
    let pair = consume_socially(&social, SocialContext::PairHousehold);
    let communal = consume_socially(&social, SocialContext::CommunalCare);

    assert_ne!(pair.1, communal.1);
    assert_eq!(pair.0, biological);
    assert_eq!(communal.0, biological);
}

#[test]
fn identical_plain_inputs_produce_identical_summaries() {
    let input = typical_input();
    assert_eq!(
        summarize_reproduction(&input),
        summarize_reproduction(&input)
    );
}

#[test]
fn distributions_normalize_authored_weights_without_losing_zero_entries() {
    let offspring = OffspringDistribution::new(vec![(0, 0.0), (2, 2.0), (4, 6.0)]).unwrap();

    assert_eq!(offspring.weight_of(0), 0.0);
    assert_eq!(offspring.weight_of(2), 0.25);
    assert_eq!(offspring.weight_of(4), 0.75);
    assert_eq!(offspring.total_weight(), 1.0);
}

#[test]
fn consumers_can_read_normalized_outcomes_without_mutating_them() {
    let input = typical_input();
    assert_eq!(
        input.typicality.offspring.entries(),
        &[(1, 0.25), (3, 0.75)]
    );
    assert_eq!(
        input.typicality.survival_to_independence.entries(),
        &[
            (IndependenceOutcome::Survives, 0.75),
            (IndependenceOutcome::DoesNotSurvive, 0.25)
        ]
    );
    assert_eq!(
        input.typicality.care_burden.entries(),
        &[(2.0, 0.25), (6.0, 0.75)]
    );
    let summary = summarize_reproduction(&input).unwrap();
    assert_eq!(
        summary.reproductive_roles.entries(),
        &[
            (ReproductiveRole::MaterialProducer, 0.25),
            (ReproductiveRole::DevelopmentCarrier, 0.75)
        ]
    );
    assert_eq!(
        summary.hybrid_outcomes.entries(),
        &[
            (HybridOutcome::Fertile, 0.5),
            (HybridOutcome::ViableButSterile, 0.5)
        ]
    );
}

#[test]
fn every_distribution_rejects_invalid_weights_and_overflowing_totals() {
    for weight in [-1.0, f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(OffspringDistribution::new(vec![(1, weight)]).is_err());
        assert!(SurvivalDistribution::new(vec![(IndependenceOutcome::Survives, weight)]).is_err());
        assert!(CareBurdenDistribution::new(vec![(1.0, weight)]).is_err());
        assert!(
            RoleAvailabilityDistribution::new(vec![(ReproductiveRole::Builder, weight)]).is_err()
        );
        assert!(HybridOutcomeDistribution::new(vec![(HybridOutcome::Fertile, weight)]).is_err());
    }
    assert_eq!(
        OffspringDistribution::new(vec![(1, f64::MAX), (2, f64::MAX)])
            .unwrap_err()
            .to_string(),
        "offspring distribution total weight must be finite"
    );
}

#[test]
fn non_finite_and_negative_values_are_rejected_with_context() {
    assert_eq!(
        OffspringDistribution::new(vec![(1, f64::NAN)])
            .unwrap_err()
            .to_string(),
        "offspring distribution weight at index 0 must be finite"
    );
    assert_eq!(
        CareBurdenDistribution::new(vec![(-0.5, 1.0)])
            .unwrap_err()
            .to_string(),
        "care burden value at index 0 must not be negative"
    );
    assert_eq!(
        PopulationPersistenceInputs::new(f64::INFINITY, 1.0)
            .unwrap_err()
            .to_string(),
        "reproductive events per generation must be finite"
    );
    assert_eq!(
        PopulationPersistenceInputs::new(1.0, -1.0)
            .unwrap_err()
            .to_string(),
        "replacement requirement must not be negative"
    );
}

#[test]
fn zero_and_no_reproduction_are_valid_inputs() {
    let input = ReproductivePopulationInput {
        possibility: ReproductivePossibility {
            pathway_count: 0,
            hybrid_outcomes: vec![],
        },
        typicality: ReproductiveTypicality {
            maturity_age: Years::new(0.0).unwrap(),
            generation_length: Years::new(0.0).unwrap(),
            offspring: OffspringDistribution::new(vec![(0, 0.0)]).unwrap(),
            survival_to_independence: SurvivalDistribution::new(vec![]).unwrap(),
            dependency_duration: Years::new(0.0).unwrap(),
            care_burden: CareBurdenDistribution::new(vec![(0.0, 0.0)]).unwrap(),
            reproductive_roles: RoleAvailabilityDistribution::new(vec![]).unwrap(),
            hybrid_outcomes: HybridOutcomeDistribution::new(vec![]).unwrap(),
        },
        persistence: PopulationPersistenceInputs::new(0.0, 0.0).unwrap(),
    };

    let summary = summarize_reproduction(&input).unwrap();
    assert_eq!(summary.expected_offspring, 0.0);
    assert_eq!(summary.survival_to_independence, 0.0);
    assert_eq!(summary.expected_care_burden, 0.0);
    assert_eq!(summary.expected_independent_offspring_per_generation, 0.0);
    assert_eq!(summary.persistence_balance, 0.0);
}

#[test]
fn overflowing_generation_summary_is_rejected_before_the_social_handoff() {
    let mut input = typical_input();
    input.typicality.offspring = OffspringDistribution::new(vec![(2, 1.0)]).unwrap();
    input.typicality.survival_to_independence =
        SurvivalDistribution::new(vec![(IndependenceOutcome::Survives, 1.0)]).unwrap();
    input.persistence = PopulationPersistenceInputs::new(f64::MAX, 0.0).unwrap();

    assert_eq!(
        summarize_reproduction(&input).unwrap_err().to_string(),
        "expected independent offspring per generation must be finite after summary"
    );
}

#[test]
fn overflowing_care_mean_is_rejected_before_the_social_handoff() {
    let mut input = typical_input();
    input.typicality.care_burden = CareBurdenDistribution::new(vec![(f64::MAX, 1.0); 11]).unwrap();

    assert_eq!(
        summarize_reproduction(&input).unwrap_err().to_string(),
        "expected care burden must be finite after summary"
    );
}
