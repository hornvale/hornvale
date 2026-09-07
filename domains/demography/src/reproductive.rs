//! Species-independent reproductive inputs for the population substrate.
//!
//! This module is the plain-data seam between BIO-3 and population dynamics.
//! [`ReproductivePossibility`] records what may happen;
//! [`ReproductiveTypicality`] records distributions and durations describing
//! what usually happens. [`PopulationPersistenceInputs`] supplies rates to the
//! aggregate population transform. None of these values says what happened to
//! a cohort or person.
//!
//! A future two-tier population realization owns seeded draws and cohort
//! state. Projection-only identity, anatomy, social gender, and species
//! registry keys do not cross this boundary.

use hornvale_kernel::Years;
use std::fmt;

/// A descriptive refusal from a reproductive population input constructor.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReproductiveInputError {
    field: String,
    requirement: &'static str,
}

impl ReproductiveInputError {
    fn new(field: String, requirement: &'static str) -> Self {
        Self { field, requirement }
    }
}

impl fmt::Display for ReproductiveInputError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{} {}", self.field, self.requirement)
    }
}

impl std::error::Error for ReproductiveInputError {}

#[derive(Clone, Debug, PartialEq)]
struct Distribution<T> {
    entries: Vec<(T, f64)>,
}

impl<T> Distribution<T> {
    fn new(field: &'static str, entries: Vec<(T, f64)>) -> Result<Self, ReproductiveInputError> {
        let mut total = 0.0;
        for (index, (_, weight)) in entries.iter().enumerate() {
            validate_scalar(&format!("{field} weight at index {index}"), *weight)?;
            total += weight;
        }
        if !total.is_finite() {
            return Err(ReproductiveInputError::new(
                format!("{field} total weight"),
                "must be finite",
            ));
        }
        let entries = if total > 0.0 {
            entries
                .into_iter()
                .map(|(value, weight)| (value, weight / total))
                .collect()
        } else {
            entries
        };
        Ok(Self { entries })
    }

    fn len(&self) -> usize {
        self.entries.len()
    }

    fn total_weight(&self) -> f64 {
        self.entries.iter().map(|(_, weight)| weight).sum()
    }
}

fn validate_scalar(field: &str, value: f64) -> Result<(), ReproductiveInputError> {
    if !value.is_finite() {
        return Err(ReproductiveInputError::new(
            field.to_string(),
            "must be finite",
        ));
    }
    if value < 0.0 {
        return Err(ReproductiveInputError::new(
            field.to_string(),
            "must not be negative",
        ));
    }
    Ok(())
}

/// A normalized typicality distribution over offspring counts.
#[derive(Clone, Debug, PartialEq)]
pub struct OffspringDistribution(Distribution<u32>);

impl OffspringDistribution {
    /// Authored outcomes and normalized weights, in caller order.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn entries(&self) -> &[(u32, f64)] {
        &self.0.entries
    }

    /// Validate and normalize `(offspring count, weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(u32, f64)>) -> Result<Self, ReproductiveInputError> {
        Distribution::new("offspring distribution", entries).map(Self)
    }

    /// Number of authored outcomes, including zero-weight outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the distribution has no authored outcomes.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.entries.is_empty()
    }

    /// Normalized weight for `count`, or zero when it is absent.
    /// type-audit: bare-ok(count: count), bare-ok(ratio: return)
    pub fn weight_of(&self, count: u32) -> f64 {
        self.0
            .entries
            .iter()
            .filter(|(value, _)| *value == count)
            .map(|(_, weight)| weight)
            .sum()
    }

    /// Sum of normalized weights: one for a live distribution, zero for a
    /// valid empty or all-zero distribution.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }

    fn mean(&self) -> f64 {
        self.0
            .entries
            .iter()
            .map(|(count, weight)| f64::from(*count) * weight)
            .sum()
    }
}

/// Whether an offspring reaches biological independence.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndependenceOutcome {
    /// The offspring survives to independence.
    Survives,
    /// The offspring does not survive to independence.
    DoesNotSurvive,
}

/// A normalized typicality distribution over independence outcomes.
#[derive(Clone, Debug, PartialEq)]
pub struct SurvivalDistribution(Distribution<IndependenceOutcome>);

impl SurvivalDistribution {
    /// Authored outcomes and normalized weights, in caller order.
    /// type-audit: bare-ok(ratio: return)
    pub fn entries(&self) -> &[(IndependenceOutcome, f64)] {
        &self.0.entries
    }

    /// Validate and normalize `(outcome, weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(IndependenceOutcome, f64)>) -> Result<Self, ReproductiveInputError> {
        Distribution::new("survival distribution", entries).map(Self)
    }

    /// Number of authored outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the distribution has no authored outcomes.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.entries.is_empty()
    }

    /// Sum of normalized weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }

    fn survival_probability(&self) -> f64 {
        self.0
            .entries
            .iter()
            .filter(|(outcome, _)| *outcome == IndependenceOutcome::Survives)
            .map(|(_, weight)| weight)
            .sum()
    }
}

/// A normalized typicality distribution over non-negative care burdens.
#[derive(Clone, Debug, PartialEq)]
pub struct CareBurdenDistribution(Distribution<f64>);

impl CareBurdenDistribution {
    /// Authored burdens and normalized weights, in caller order.
    /// type-audit: bare-ok(ratio: return)
    pub fn entries(&self) -> &[(f64, f64)] {
        &self.0.entries
    }

    /// Validate care values and weights, then normalize the weights.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(f64, f64)>) -> Result<Self, ReproductiveInputError> {
        for (index, (burden, _)) in entries.iter().enumerate() {
            validate_scalar(&format!("care burden value at index {index}"), *burden)?;
        }
        Distribution::new("care burden distribution", entries).map(Self)
    }

    /// Number of authored outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the distribution has no authored outcomes.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.entries.is_empty()
    }

    /// Sum of normalized weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }

    fn mean(&self) -> f64 {
        self.0
            .entries
            .iter()
            .map(|(burden, weight)| burden * weight)
            .sum()
    }
}

/// A causal biological role in population reproduction.
///
/// These roles deliberately carry no social gender, identity, kinship, or
/// institutional meaning.
/// placement: deliberate(BIO-3 population role weights are independent of
/// species pathway requirements; domains cannot depend on siblings) shape(04a33f)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReproductiveRole {
    /// Produces reproductive material or a template.
    MaterialProducer,
    /// Contributes material to a joined pathway.
    MaterialContributor,
    /// Carries or contains development.
    DevelopmentCarrier,
    /// Sustains development without necessarily carrying it.
    DevelopmentSupporter,
    /// Serves as developmental substrate.
    Host,
    /// Manufactures a body.
    Builder,
}

/// A normalized typicality distribution over available biological roles.
#[derive(Clone, Debug, PartialEq)]
pub struct RoleAvailabilityDistribution(Distribution<ReproductiveRole>);

impl RoleAvailabilityDistribution {
    /// Authored roles and normalized weights, in caller order.
    /// type-audit: bare-ok(ratio: return)
    pub fn entries(&self) -> &[(ReproductiveRole, f64)] {
        &self.0.entries
    }

    /// Validate and normalize `(role, weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(ReproductiveRole, f64)>) -> Result<Self, ReproductiveInputError> {
        Distribution::new("reproductive-role distribution", entries).map(Self)
    }

    /// Number of authored role entries.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the distribution has no authored roles.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.entries.is_empty()
    }

    /// Sum of normalized role weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// A species-independent hybrid compatibility outcome.
/// placement: deliberate(population outcome categories omit species compatibility
/// assistance payloads; the composition root owns conversion) shape(c45575)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HybridOutcome {
    /// Viable offspring capable of reproduction.
    Fertile,
    /// Viable offspring without reproductive fertility.
    ViableButSterile,
    /// Unstable development or reduced survival.
    Unstable,
    /// Viability requires non-magical developmental assistance.
    Assisted,
    /// Viability requires future magical assistance.
    MagicOnly,
    /// No viable outcome under the supplied conditions.
    Impossible,
}

/// A normalized typicality distribution over hybrid outcomes.
#[derive(Clone, Debug, PartialEq)]
pub struct HybridOutcomeDistribution(Distribution<HybridOutcome>);

impl HybridOutcomeDistribution {
    /// Authored outcomes and normalized weights, in caller order.
    /// type-audit: bare-ok(ratio: return)
    pub fn entries(&self) -> &[(HybridOutcome, f64)] {
        &self.0.entries
    }

    /// Validate and normalize `(outcome, weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(HybridOutcome, f64)>) -> Result<Self, ReproductiveInputError> {
        Distribution::new("hybrid-outcome distribution", entries).map(Self)
    }

    /// Number of authored outcome entries.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the distribution has no authored outcomes.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.entries.is_empty()
    }

    /// Sum of normalized outcome weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// Possibility facts from the reproductive grammar, with no frequencies.
/// type-audit: bare-ok(count: pathway_count), bare-ok(flag: hybrid_applicable)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReproductivePossibility {
    /// Number of complete pathways permitted by the queried conditions.
    pub pathway_count: u32,
    /// Whether hybrid compatibility was queried for at least one partner.
    pub hybrid_applicable: bool,
    /// Hybrid outcomes that are possible; order is caller-authored.
    pub hybrid_outcomes: Vec<HybridOutcome>,
}

/// Typicality values consumed by aggregate population dynamics.
#[derive(Clone, Debug, PartialEq)]
pub struct ReproductiveTypicality {
    /// Typical age at biological maturity.
    pub maturity_age: Years,
    /// Typical interval between successive generations.
    pub generation_length: Years,
    /// Offspring count distribution per reproductive event.
    pub offspring: OffspringDistribution,
    /// Distribution of survival to biological independence.
    pub survival_to_independence: SurvivalDistribution,
    /// Typical duration of offspring dependency.
    pub dependency_duration: Years,
    /// Distribution of aggregate care burden per offspring.
    pub care_burden: CareBurdenDistribution,
    /// Distribution of causal reproductive-role availability.
    pub reproductive_roles: RoleAvailabilityDistribution,
    /// Typical hybrid outcomes under the supplied compatibility relation.
    pub hybrid_outcomes: HybridOutcomeDistribution,
}

/// Aggregate rates used to assess population replacement without drawing a
/// cohort.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PopulationPersistenceInputs {
    reproductive_events_per_generation: f64,
    replacement_requirement: f64,
}

impl PopulationPersistenceInputs {
    /// Validate non-negative finite event and replacement rates.
    /// type-audit: bare-ok(ratio: reproductive_events_per_generation), bare-ok(count: replacement_requirement)
    pub fn new(
        reproductive_events_per_generation: f64,
        replacement_requirement: f64,
    ) -> Result<Self, ReproductiveInputError> {
        validate_scalar(
            "reproductive events per generation",
            reproductive_events_per_generation,
        )?;
        validate_scalar("replacement requirement", replacement_requirement)?;
        Ok(Self {
            reproductive_events_per_generation,
            replacement_requirement,
        })
    }

    /// Reproductive events represented by one generation of population.
    /// type-audit: bare-ok(ratio: return)
    pub fn reproductive_events_per_generation(self) -> f64 {
        self.reproductive_events_per_generation
    }

    /// Independent offspring required to replace the represented population.
    /// type-audit: bare-ok(count: return)
    pub fn replacement_requirement(self) -> f64 {
        self.replacement_requirement
    }
}

/// Complete BIO-3 input to the population substrate.
#[derive(Clone, Debug, PartialEq)]
pub struct ReproductivePopulationInput {
    /// Possibility, kept separate from frequency.
    pub possibility: ReproductivePossibility,
    /// Typicality distributions and durations, not realized outcomes.
    pub typicality: ReproductiveTypicality,
    /// Aggregate persistence inputs, not cohort state.
    pub persistence: PopulationPersistenceInputs,
}

/// Deterministic aggregate reproductive summary.
/// type-audit: bare-ok(count: expected_offspring), bare-ok(ratio: survival_to_independence), bare-ok(ratio: expected_care_burden), bare-ok(count: expected_independent_offspring_per_event), bare-ok(count: expected_independent_offspring_per_generation), bare-ok(count: persistence_balance)
#[derive(Clone, Debug, PartialEq)]
pub struct ReproductivePopulationSummary {
    /// Unchanged possibility facts.
    pub possibility: ReproductivePossibility,
    /// Typical maturity age.
    pub maturity_age: Years,
    /// Typical generation length.
    pub generation_length: Years,
    /// Typical dependency duration.
    pub dependency_duration: Years,
    /// Expected offspring per reproductive event.
    pub expected_offspring: f64,
    /// Probability that an offspring survives to independence.
    pub survival_to_independence: f64,
    /// Expected aggregate care burden per offspring.
    pub expected_care_burden: f64,
    /// Expected independent offspring from one reproductive event.
    pub expected_independent_offspring_per_event: f64,
    /// Expected independent offspring over one generation.
    pub expected_independent_offspring_per_generation: f64,
    /// Expected independent offspring minus the replacement requirement.
    pub persistence_balance: f64,
    /// Normalized causal-role typicality, retained as a distribution.
    pub reproductive_roles: RoleAvailabilityDistribution,
    /// Normalized hybrid typicality, retained separately from possibility.
    pub hybrid_outcomes: HybridOutcomeDistribution,
}

/// Population-owned input presented to the future SOC-2 grammar.
///
/// Social contexts consume this value; they are not stored inside it and
/// cannot rewrite its biological summary.
#[derive(Clone, Debug, PartialEq)]
pub struct SocialSubstrateInput {
    /// The aggregate biological summary SOC-2 may organize or reinterpret.
    pub reproductive: ReproductivePopulationSummary,
}

/// Summarize BIO-3's plain input without random draws or cohort creation.
///
/// Derived aggregate values are checked before they cross into the population
/// substrate, so finite authored inputs cannot produce a non-finite handoff.
pub fn summarize_reproduction(
    input: &ReproductivePopulationInput,
) -> Result<ReproductivePopulationSummary, ReproductiveInputError> {
    let ordinary_applicable = input.possibility.pathway_count > 0;
    require_applicable_distribution(
        "offspring",
        input.typicality.offspring.len(),
        input.typicality.offspring.total_weight(),
        ordinary_applicable,
    )?;
    require_applicable_distribution(
        "survival to independence",
        input.typicality.survival_to_independence.len(),
        input.typicality.survival_to_independence.total_weight(),
        ordinary_applicable,
    )?;
    require_applicable_distribution(
        "care burden",
        input.typicality.care_burden.len(),
        input.typicality.care_burden.total_weight(),
        ordinary_applicable,
    )?;
    require_applicable_distribution(
        "reproductive roles",
        input.typicality.reproductive_roles.len(),
        input.typicality.reproductive_roles.total_weight(),
        ordinary_applicable,
    )?;
    require_applicable_distribution(
        "hybrid outcomes",
        input.typicality.hybrid_outcomes.len(),
        input.typicality.hybrid_outcomes.total_weight(),
        input.possibility.hybrid_applicable,
    )?;

    let expected_offspring = input.typicality.offspring.mean();
    validate_derived("expected offspring", expected_offspring)?;
    let survival_to_independence = input
        .typicality
        .survival_to_independence
        .survival_probability();
    validate_derived("survival to independence", survival_to_independence)?;
    let expected_care_burden = input.typicality.care_burden.mean();
    validate_derived("expected care burden", expected_care_burden)?;
    let expected_independent_offspring_per_event = expected_offspring * survival_to_independence;
    validate_derived(
        "expected independent offspring per event",
        expected_independent_offspring_per_event,
    )?;
    let expected_independent_offspring_per_generation = expected_independent_offspring_per_event
        * input.persistence.reproductive_events_per_generation;
    validate_derived(
        "expected independent offspring per generation",
        expected_independent_offspring_per_generation,
    )?;
    let persistence_balance =
        expected_independent_offspring_per_generation - input.persistence.replacement_requirement;
    validate_derived("persistence balance", persistence_balance)?;

    Ok(ReproductivePopulationSummary {
        possibility: input.possibility.clone(),
        maturity_age: input.typicality.maturity_age,
        generation_length: input.typicality.generation_length,
        dependency_duration: input.typicality.dependency_duration,
        expected_offspring,
        survival_to_independence,
        expected_care_burden,
        expected_independent_offspring_per_event,
        expected_independent_offspring_per_generation,
        persistence_balance,
        reproductive_roles: input.typicality.reproductive_roles.clone(),
        hybrid_outcomes: input.typicality.hybrid_outcomes.clone(),
    })
}

fn validate_derived(field: &'static str, value: f64) -> Result<(), ReproductiveInputError> {
    if value.is_finite() {
        Ok(())
    } else {
        Err(ReproductiveInputError::new(
            field.to_string(),
            "must be finite after summary",
        ))
    }
}

fn require_applicable_distribution(
    field: &'static str,
    authored_entries: usize,
    total_weight: f64,
    applicable: bool,
) -> Result<(), ReproductiveInputError> {
    if !applicable || (authored_entries > 0 && total_weight > 0.0) {
        Ok(())
    } else {
        Err(ReproductiveInputError::new(
            format!("{field} distribution"),
            "must contain a positive-weight measurement for applicable reproduction",
        ))
    }
}

/// Form the explicit population-to-SOC-2 handoff without adding social state.
pub fn social_substrate_input(reproductive: ReproductivePopulationSummary) -> SocialSubstrateInput {
    SocialSubstrateInput { reproductive }
}
