//! Composition-root adapter from species affordances to demography inputs.

use hornvale_demography::{
    CareBurdenDistribution, HybridOutcome, HybridOutcomeDistribution, IndependenceOutcome,
    OffspringDistribution, PopulationPersistenceInputs, ReproductivePopulationInput,
    ReproductivePossibility, ReproductiveTypicality, RoleAvailabilityDistribution,
    SurvivalDistribution,
};
use hornvale_kernel::{ComponentStore, KindId, Years};
use hornvale_species::{
    CompatibilityContext, CompatibilityOutcome, ReproductiveAffordances, ReproductiveRole,
    compatibility, possible_pathways,
};
use std::fmt;

/// One partner relation to evaluate while resolving a reproductive profile.
/// type-audit: bare-ok(ratio: first_to_second_weight), bare-ok(ratio: second_to_first_weight)
#[derive(Clone, Debug, PartialEq)]
pub struct HybridPartnerConfig {
    /// The other affordance profile's registry key.
    pub partner: KindId,
    /// Species-owned guards and directional material rules for this relation.
    pub context: CompatibilityContext,
    /// Typicality weight for the configured kind donating material.
    pub first_to_second_weight: f64,
    /// Typicality weight for the partner donating material.
    pub second_to_first_weight: f64,
}

/// Caller-authored typicality and persistence configuration for one profile.
///
/// Species types remain on this side of the adapter. The resolved substrate
/// contains only demography and kernel values.
/// type-audit: bare-ok(count: offspring), bare-ok(ratio: offspring), bare-ok(ratio: survival_to_independence), bare-ok(ratio: care_burden), bare-ok(ratio: reproductive_roles), bare-ok(ratio: reproductive_events_per_generation), bare-ok(count: replacement_requirement)
#[derive(Clone, Debug, PartialEq)]
pub struct ReproductivePopulationConfig {
    /// Conditions under which the profile's own pathways are queried.
    pub context: CompatibilityContext,
    /// Typical age at biological maturity.
    pub maturity_age: Years,
    /// Typical interval between successive generations.
    pub generation_length: Years,
    /// `(offspring count, typicality weight)` outcomes.
    pub offspring: Vec<(u32, f64)>,
    /// Independence outcomes and their typicality weights.
    pub survival_to_independence: Vec<(IndependenceOutcome, f64)>,
    /// Typical duration of offspring dependency.
    pub dependency_duration: Years,
    /// `(care burden, typicality weight)` outcomes.
    pub care_burden: Vec<(f64, f64)>,
    /// Species-owned causal roles and their typicality weights.
    pub reproductive_roles: Vec<(ReproductiveRole, f64)>,
    /// Partner relations to evaluate in caller order.
    pub hybrid_partners: Vec<HybridPartnerConfig>,
    /// Reproductive events represented by one population generation.
    pub reproductive_events_per_generation: f64,
    /// Independent offspring needed to replace the represented population.
    pub replacement_requirement: f64,
}

/// The additive, species-independent BIO-3 output available to a future
/// population bake.
///
/// Current world builds do not request or store this value. Once population
/// realization opts in, this substrate is authoritative; projection windows
/// must consume it instead of recomputing biology from species registries.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ReproductiveSubstrate {
    inputs: ComponentStore<KindId, ReproductivePopulationInput>,
}

impl ReproductiveSubstrate {
    /// The resolved input for `kind`, if that kind was configured.
    pub fn get(&self, kind: &KindId) -> Option<&ReproductivePopulationInput> {
        self.inputs.get(kind)
    }

    /// Configured kind keys in deterministic ascending order.
    pub fn ids(&self) -> impl Iterator<Item = &KindId> {
        self.inputs.ids()
    }
}

/// A descriptive refusal at the species-to-demography boundary.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReproductiveAdapterError(String);

impl fmt::Display for ReproductiveAdapterError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for ReproductiveAdapterError {}

/// Resolve caller-authored species affordances into additive demography
/// inputs without invoking population realization or changing a world build.
pub fn reproductive_substrate_from(
    affordances: &ComponentStore<KindId, ReproductiveAffordances>,
    configurations: &ComponentStore<KindId, ReproductivePopulationConfig>,
) -> Result<ReproductiveSubstrate, ReproductiveAdapterError> {
    let mut inputs = ComponentStore::new();
    for (kind, configuration) in configurations.iter() {
        let profile = affordances.get(kind).ok_or_else(|| {
            ReproductiveAdapterError(format!(
                "reproductive configuration for {} has no affordance profile",
                kind.0
            ))
        })?;
        validate_affordances(*kind, profile)?;
        let input = population_input_from(*kind, profile, affordances, configuration)?;
        inputs.insert(*kind, input);
    }
    Ok(ReproductiveSubstrate { inputs })
}

fn population_input_from(
    kind: KindId,
    profile: &ReproductiveAffordances,
    affordances: &ComponentStore<KindId, ReproductiveAffordances>,
    configuration: &ReproductivePopulationConfig,
) -> Result<ReproductivePopulationInput, ReproductiveAdapterError> {
    let pathway_count = u32::try_from(possible_pathways(profile, &configuration.context).len())
        .map_err(|_| {
            ReproductiveAdapterError(format!(
                "reproductive pathway count for {} exceeds the demography handoff",
                kind.0
            ))
        })?;
    let mut possible_hybrids = Vec::new();
    let mut typical_hybrids = Vec::new();
    for partner in &configuration.hybrid_partners {
        let partner_profile = affordances.get(&partner.partner).ok_or_else(|| {
            ReproductiveAdapterError(format!(
                "hybrid partner {} requested by {} has no affordance profile",
                partner.partner.0, kind.0
            ))
        })?;
        validate_affordances(partner.partner, partner_profile)?;
        let relation = compatibility(profile, partner_profile, &partner.context);
        for (direction, weight) in [
            (&relation.first_to_second, partner.first_to_second_weight),
            (&relation.second_to_first, partner.second_to_first_weight),
        ] {
            let outcome = hybrid_outcome_from(direction.outcome);
            if outcome != HybridOutcome::Impossible {
                possible_hybrids.push(outcome);
            }
            typical_hybrids.push((outcome, weight));
        }
    }

    let population_error = |error: hornvale_demography::ReproductiveInputError| {
        ReproductiveAdapterError(format!(
            "reproductive population input for {} is invalid: {error}",
            kind.0
        ))
    };
    Ok(ReproductivePopulationInput {
        possibility: ReproductivePossibility {
            pathway_count,
            hybrid_outcomes: possible_hybrids,
        },
        typicality: ReproductiveTypicality {
            maturity_age: configuration.maturity_age,
            generation_length: configuration.generation_length,
            offspring: OffspringDistribution::new(configuration.offspring.clone())
                .map_err(&population_error)?,
            survival_to_independence: SurvivalDistribution::new(
                configuration.survival_to_independence.clone(),
            )
            .map_err(&population_error)?,
            dependency_duration: configuration.dependency_duration,
            care_burden: CareBurdenDistribution::new(configuration.care_burden.clone())
                .map_err(&population_error)?,
            reproductive_roles: RoleAvailabilityDistribution::new(
                configuration
                    .reproductive_roles
                    .iter()
                    .map(|(role, weight)| (reproductive_role_from(*role), *weight))
                    .collect(),
            )
            .map_err(&population_error)?,
            hybrid_outcomes: HybridOutcomeDistribution::new(typical_hybrids)
                .map_err(&population_error)?,
        },
        persistence: PopulationPersistenceInputs::new(
            configuration.reproductive_events_per_generation,
            configuration.replacement_requirement,
        )
        .map_err(population_error)?,
    })
}

fn validate_affordances(
    kind: KindId,
    profile: &ReproductiveAffordances,
) -> Result<(), ReproductiveAdapterError> {
    profile.validate().map_err(|reason| {
        ReproductiveAdapterError(format!(
            "reproductive affordances for {} are invalid: {reason}",
            kind.0
        ))
    })
}

fn reproductive_role_from(role: ReproductiveRole) -> hornvale_demography::ReproductiveRole {
    match role {
        ReproductiveRole::MaterialProducer => {
            hornvale_demography::ReproductiveRole::MaterialProducer
        }
        ReproductiveRole::MaterialContributor => {
            hornvale_demography::ReproductiveRole::MaterialContributor
        }
        ReproductiveRole::DevelopmentCarrier => {
            hornvale_demography::ReproductiveRole::DevelopmentCarrier
        }
        ReproductiveRole::DevelopmentSupporter => {
            hornvale_demography::ReproductiveRole::DevelopmentSupporter
        }
        ReproductiveRole::Host => hornvale_demography::ReproductiveRole::Host,
        ReproductiveRole::Builder => hornvale_demography::ReproductiveRole::Builder,
    }
}

fn hybrid_outcome_from(outcome: CompatibilityOutcome) -> HybridOutcome {
    match outcome {
        CompatibilityOutcome::Fertile => HybridOutcome::Fertile,
        CompatibilityOutcome::ViableButSterile => HybridOutcome::ViableButSterile,
        CompatibilityOutcome::Unstable => HybridOutcome::Unstable,
        CompatibilityOutcome::Assisted(_) => HybridOutcome::Assisted,
        CompatibilityOutcome::MagicOnly(_) => HybridOutcome::MagicOnly,
        CompatibilityOutcome::Impossible => HybridOutcome::Impossible,
    }
}
