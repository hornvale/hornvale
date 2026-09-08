//! Species-independent aggregate social inputs and summaries.

use crate::reproductive::{HybridOutcome, ReproductivePopulationSummary, SocialSubstrateInput};
use std::fmt;

/// A descriptive refusal from an aggregate social input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SocialInputError {
    field: String,
    requirement: String,
}

impl fmt::Display for SocialInputError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{} {}", self.field, self.requirement)
    }
}

impl std::error::Error for SocialInputError {}

impl SocialInputError {
    fn new(field: String, requirement: impl Into<String>) -> Self {
        Self {
            field,
            requirement: requirement.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct CountDistribution {
    entries: Vec<(u32, f64)>,
}

impl CountDistribution {
    fn new(field: &'static str, entries: Vec<(u32, f64)>) -> Result<Self, SocialInputError> {
        let mut total_weight = 0.0;
        for (index, (_, weight)) in entries.iter().enumerate() {
            validate_non_negative_finite(&format!("{field} weight at index {index}"), *weight)?;
            total_weight += weight;
        }
        if !total_weight.is_finite() {
            return Err(SocialInputError::new(
                format!("{field} total weight"),
                "must be finite",
            ));
        }
        if total_weight == 0.0 {
            return Err(SocialInputError::new(
                field.to_string(),
                "must contain a positive-weight measurement",
            ));
        }

        let entries = entries
            .into_iter()
            .map(|(count, weight)| (count, weight / total_weight))
            .collect();
        Ok(Self { entries })
    }

    fn mean(&self) -> f64 {
        self.entries
            .iter()
            .map(|(count, weight)| f64::from(*count) * weight)
            .sum()
    }

    fn len(&self) -> usize {
        self.entries.len()
    }

    fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    fn total_weight(&self) -> f64 {
        self.entries.iter().map(|(_, weight)| weight).sum()
    }

    fn validate(&self, field: &'static str) -> Result<(), SocialInputError> {
        for (index, (_, weight)) in self.entries.iter().enumerate() {
            validate_non_negative_finite(&format!("{field} weight at index {index}"), *weight)?;
        }
        let total_weight = self.total_weight();
        validate_non_negative_finite(&format!("{field} total weight"), total_weight)?;
        if total_weight == 0.0 {
            Err(SocialInputError::new(
                field.to_string(),
                "must contain a positive-weight measurement",
            ))
        } else {
            Ok(())
        }
    }
}

fn validate_finite(field: &str, value: f64) -> Result<(), SocialInputError> {
    if value.is_finite() {
        Ok(())
    } else {
        Err(SocialInputError::new(field.to_string(), "must be finite"))
    }
}

fn validate_non_negative_finite(field: &str, value: f64) -> Result<(), SocialInputError> {
    validate_finite(field, value)?;
    if value < 0.0 {
        Err(SocialInputError::new(
            field.to_string(),
            "must not be negative",
        ))
    } else {
        Ok(())
    }
}

/// Species-independent origin of an offspring pathway.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OffspringOrigin {
    /// Distinct inherited inputs are combined.
    JoinedInputs,
    /// One inherited input is copied.
    CopiedInput,
    /// A host or substrate is converted into a new organism.
    ConvertedHost,
    /// A body is constructed without biological descent.
    ConstructedBody,
}

/// Species-independent site at which offspring develops.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BiologicalDevelopmentSite {
    /// Within or on a supporting body.
    Body,
    /// Within a released capsule such as an egg or spore case.
    Capsule,
    /// Within a nest, brood chamber, or equivalent structure.
    BroodStructure,
    /// Within a shared colony or matrix.
    Colony,
    /// Within or on another organism used as a host.
    Host,
    /// Directly in a suitable environment.
    Environment,
    /// Within an artificial manufacturing site.
    Workshop,
}

/// Structural source of biological support during development.
///
/// This is deliberately distinct from [`CareTopology`], which counts social
/// caregiver groups per dependent after reproduction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BiologicalCareTopology {
    /// Support supplied by one body.
    SingleBody,
    /// Support jointly supplied by a pair.
    BodyPair,
    /// Support supplied by a group or colony.
    BodyGroup,
    /// Support supplied by a host organism.
    HostOrganism,
    /// Support supplied by ambient environmental conditions.
    AmbientEnvironment,
    /// Support supplied by an artificial process or apparatus.
    ArtificialProcess,
}

/// Natural biological transition a pathway may require or permit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BiologicalTransitionCapability {
    /// Mature from one developmental state into another.
    DevelopmentalMaturation,
    /// Undergo a body-plan metamorphosis.
    BodyMetamorphosis,
    /// Change state in response to a recurring environment.
    SeasonalChange,
    /// Move naturally between reproductive roles over a lifetime.
    SequentialReproductiveRole,
}

/// One complete aggregate offspring possibility, not a realized birth.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OffspringPathway {
    /// How the pathway originates inherited or constructed material.
    pub origin: OffspringOrigin,
    /// Site at which development occurs.
    pub development_site: BiologicalDevelopmentSite,
    /// Structural biological support, explicitly absent when not required.
    pub care_topology: Option<BiologicalCareTopology>,
    /// Natural transition required before the pathway, when any.
    pub prerequisite_transition: Option<BiologicalTransitionCapability>,
}

/// Species-independent mechanism by which biological descent is established.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DescentMode {
    /// Inherited material combines multiple sources.
    CombinedSources,
    /// Inherited material copies one source.
    CopiedSource,
    /// A host or substrate supplies transformed inherited material.
    ConvertedSource,
    /// No biological source contributes inherited material.
    Constructed,
}

/// Aggregate biological descent shape with no person identity.
/// type-audit: bare-ok(count: contributing_source_count)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DescentRelation {
    /// Structural descent mechanism.
    pub mode: DescentMode,
    /// Number of inherited-material sources contributing to the relation.
    pub contributing_source_count: u32,
}

/// Directional aggregate compatibility without species or participant IDs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CompatibilityRelation {
    /// Outcome when the first population contributes material to the second.
    pub first_material_second_development: HybridOutcome,
    /// Outcome when the second population contributes material to the first.
    pub second_material_first_development: HybridOutcome,
}

/// A species-independent category of aggregate lifecycle transition.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LifecycleTransitionKind {
    /// Reaches social or biological independence.
    Independence,
    /// Enters an association.
    AssociationFormation,
    /// Leaves or closes an association without erasing its history.
    AssociationDissolution,
    /// Migrates between places or groups.
    Migration,
    /// Experiences the death of a parent or descent contributor.
    ParentalDeath,
}

/// An aggregate rate for one lifecycle transition.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LifecycleTransition {
    kind: LifecycleTransitionKind,
    events_per_person_year: f64,
}

impl LifecycleTransition {
    /// Construct a transition rate measured in events per person-year.
    /// type-audit: bare-ok(ratio: events_per_person_year)
    pub fn new(
        kind: LifecycleTransitionKind,
        events_per_person_year: f64,
    ) -> Result<Self, SocialInputError> {
        validate_non_negative_finite(
            "lifecycle transition events per person-year",
            events_per_person_year,
        )?;
        Ok(Self {
            kind,
            events_per_person_year,
        })
    }

    /// Transition category.
    pub fn kind(&self) -> LifecycleTransitionKind {
        self.kind
    }

    /// Aggregate transition rate in events per person-year.
    /// type-audit: bare-ok(ratio: return)
    pub fn events_per_person_year(&self) -> f64 {
        self.events_per_person_year
    }
}

/// Ordered typicality over participant counts per association.
#[derive(Clone, Debug, PartialEq)]
pub struct AssociationDistribution(CountDistribution);

impl AssociationDistribution {
    /// Construct from `(participants per association, typicality weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(u32, f64)>) -> Result<Self, SocialInputError> {
        CountDistribution::new("association distribution", entries).map(Self)
    }

    /// Authored possibility support and normalized typicality weights.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn entries(&self) -> &[(u32, f64)] {
        &self.0.entries
    }

    /// Number of authored participant-count outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether no participant-count outcomes were authored.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Sum of normalized typicality weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// Ordered typicality over descent-line counts per person.
#[derive(Clone, Debug, PartialEq)]
pub struct DescentDistribution(CountDistribution);

impl DescentDistribution {
    /// Construct from `(descent lines per person, typicality weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(u32, f64)>) -> Result<Self, SocialInputError> {
        CountDistribution::new("descent distribution", entries).map(Self)
    }

    /// Authored possibility support and normalized typicality weights.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn entries(&self) -> &[(u32, f64)] {
        &self.0.entries
    }

    /// Number of authored descent-line-count outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether no descent-line-count outcomes were authored.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Sum of normalized typicality weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// Ordered topology over concurrent caregiver-group counts per dependent.
#[derive(Clone, Debug, PartialEq)]
pub struct CareTopology(CountDistribution);

impl CareTopology {
    /// Construct from `(caregiver groups per dependent, typicality weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(u32, f64)>) -> Result<Self, SocialInputError> {
        CountDistribution::new("care topology", entries).map(Self)
    }

    /// Authored possibility support and normalized typicality weights.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn entries(&self) -> &[(u32, f64)] {
        &self.0.entries
    }

    /// Number of authored caregiver-group-count outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether no caregiver-group-count outcomes were authored.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Sum of normalized typicality weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// Ordered typicality over migrations per person-lifetime.
#[derive(Clone, Debug, PartialEq)]
pub struct MigrationDistribution(CountDistribution);

impl MigrationDistribution {
    /// Construct from `(migrations per person-lifetime, typicality weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(u32, f64)>) -> Result<Self, SocialInputError> {
        CountDistribution::new("migration distribution", entries).map(Self)
    }

    /// Authored possibility support and normalized typicality weights.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn entries(&self) -> &[(u32, f64)] {
        &self.0.entries
    }

    /// Number of authored migration-count outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether no migration-count outcomes were authored.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Sum of normalized typicality weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// Ordered typicality over inheritance transfers per parental death.
#[derive(Clone, Debug, PartialEq)]
pub struct InheritanceDistribution(CountDistribution);

impl InheritanceDistribution {
    /// Construct from `(transfers per parental death, typicality weight)` entries.
    /// type-audit: bare-ok(ratio: entries)
    pub fn new(entries: Vec<(u32, f64)>) -> Result<Self, SocialInputError> {
        CountDistribution::new("inheritance distribution", entries).map(Self)
    }

    /// Authored possibility support and normalized typicality weights.
    /// type-audit: bare-ok(count: return), bare-ok(ratio: return)
    pub fn entries(&self) -> &[(u32, f64)] {
        &self.0.entries
    }

    /// Number of authored transfer-count outcomes.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether no transfer-count outcomes were authored.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Sum of normalized typicality weights.
    /// type-audit: bare-ok(ratio: return)
    pub fn total_weight(&self) -> f64 {
        self.0.total_weight()
    }
}

/// Complete aggregate social input; contains no realized person or group.
#[derive(Clone, Debug, PartialEq)]
pub struct SocialCohortInput {
    /// Landed BIO-3 to SOC-2 population handoff.
    pub substrate: SocialSubstrateInput,
    /// Ordered complete offspring possibilities from the biological grammar.
    pub offspring_pathways: Vec<OffspringPathway>,
    /// Ordered aggregate biological descent shapes.
    pub descent_relations: Vec<DescentRelation>,
    /// Ordered directional compatibility relations, with no species IDs.
    pub compatibility_relations: Vec<CompatibilityRelation>,
    /// Ordered natural transition capabilities; realized history comes later.
    pub transition_capabilities: Vec<BiologicalTransitionCapability>,
    /// Ordered lifecycle rates in events per person-year.
    pub lifecycle_transitions: Vec<LifecycleTransition>,
    /// Participant-count support and typicality for associations.
    pub associations: AssociationDistribution,
    /// Descent-line-count support and typicality.
    pub descent: DescentDistribution,
    /// Social caregiver-group-count topology, including zero and overlap.
    /// Biological developmental support remains on [`OffspringPathway`].
    pub care_topology: CareTopology,
    /// Migration-count support and typicality per person-lifetime.
    pub migration: MigrationDistribution,
    /// Transfer-count support and typicality per parental death.
    pub inheritance: InheritanceDistribution,
}

/// Pure aggregate summary consumed by a later realization layer.
/// type-audit: bare-ok(count: expected_association_participants), bare-ok(count: expected_descent_lines_per_person), bare-ok(count: expected_caregiver_groups_per_dependent), bare-ok(count: expected_migrations_per_person_lifetime), bare-ok(count: expected_inheritance_transfers_per_parental_death)
#[derive(Clone, Debug, PartialEq)]
pub struct SocialCohortSummary {
    /// Biological possibility and typicality, unchanged by social context.
    pub reproductive: ReproductivePopulationSummary,
    /// Ordered complete offspring possibilities, unchanged from input.
    pub offspring_pathways: Vec<OffspringPathway>,
    /// Ordered biological descent shapes, unchanged from input.
    pub descent_relations: Vec<DescentRelation>,
    /// Ordered directional compatibility relations, unchanged from input.
    pub compatibility_relations: Vec<CompatibilityRelation>,
    /// Ordered natural transition capabilities, unchanged from input.
    pub transition_capabilities: Vec<BiologicalTransitionCapability>,
    /// Ordered lifecycle rates in events per person-year.
    pub lifecycle_transitions: Vec<LifecycleTransition>,
    /// Association participant support and typicality.
    pub associations: AssociationDistribution,
    /// Descent-line support and typicality.
    pub descent: DescentDistribution,
    /// Social caregiver-group-count topology, retained rather than flattened.
    /// Biological developmental support remains on [`OffspringPathway`].
    pub care_topology: CareTopology,
    /// Migration support and typicality.
    pub migration: MigrationDistribution,
    /// Inheritance-transfer support and typicality.
    pub inheritance: InheritanceDistribution,
    /// Expected participants per association.
    pub expected_association_participants: f64,
    /// Expected descent lines per person.
    pub expected_descent_lines_per_person: f64,
    /// Expected concurrent caregiver groups per dependent.
    pub expected_caregiver_groups_per_dependent: f64,
    /// Expected migrations per person-lifetime.
    pub expected_migrations_per_person_lifetime: f64,
    /// Expected inheritance transfers per parental death.
    pub expected_inheritance_transfers_per_parental_death: f64,
}

/// Validate a complete cohort input without mutating or normalizing it.
pub fn validate_social_cohort(input: &SocialCohortInput) -> Result<(), SocialInputError> {
    let reproductive = &input.substrate.reproductive;
    validate_non_negative_finite(
        "reproductive expected offspring",
        reproductive.expected_offspring,
    )?;
    validate_non_negative_finite(
        "reproductive survival to independence",
        reproductive.survival_to_independence,
    )?;
    if reproductive.survival_to_independence > 1.0 {
        return Err(SocialInputError::new(
            "reproductive survival to independence".to_string(),
            "must not exceed one",
        ));
    }
    validate_non_negative_finite(
        "reproductive expected care burden",
        reproductive.expected_care_burden,
    )?;
    validate_non_negative_finite(
        "reproductive expected independent offspring per event",
        reproductive.expected_independent_offspring_per_event,
    )?;
    validate_non_negative_finite(
        "reproductive expected independent offspring per generation",
        reproductive.expected_independent_offspring_per_generation,
    )?;
    validate_finite(
        "reproductive persistence balance",
        reproductive.persistence_balance,
    )?;
    for (index, transition) in input.lifecycle_transitions.iter().enumerate() {
        if let Some(previous_index) = input.lifecycle_transitions[..index]
            .iter()
            .position(|previous| previous.kind == transition.kind)
        {
            return Err(SocialInputError::new(
                format!("lifecycle transition kind at index {index}"),
                format!("duplicates index {previous_index}"),
            ));
        }
        validate_non_negative_finite(
            "lifecycle transition events per person-year",
            transition.events_per_person_year,
        )?;
    }
    input.associations.0.validate("association distribution")?;
    input.descent.0.validate("descent distribution")?;
    input.care_topology.0.validate("care topology")?;
    input.migration.0.validate("migration distribution")?;
    input.inheritance.0.validate("inheritance distribution")?;
    Ok(())
}

/// Summarize an aggregate cohort without random draws or realized entities.
pub fn summarize_social_cohort(
    input: &SocialCohortInput,
) -> Result<SocialCohortSummary, SocialInputError> {
    validate_social_cohort(input)?;
    Ok(SocialCohortSummary {
        reproductive: input.substrate.reproductive.clone(),
        offspring_pathways: input.offspring_pathways.clone(),
        descent_relations: input.descent_relations.clone(),
        compatibility_relations: input.compatibility_relations.clone(),
        transition_capabilities: input.transition_capabilities.clone(),
        lifecycle_transitions: input.lifecycle_transitions.clone(),
        associations: input.associations.clone(),
        descent: input.descent.clone(),
        care_topology: input.care_topology.clone(),
        migration: input.migration.clone(),
        inheritance: input.inheritance.clone(),
        expected_association_participants: input.associations.0.mean(),
        expected_descent_lines_per_person: input.descent.0.mean(),
        expected_caregiver_groups_per_dependent: input.care_topology.0.mean(),
        expected_migrations_per_person_lifetime: input.migration.0.mean(),
        expected_inheritance_transfers_per_parental_death: input.inheritance.0.mean(),
    })
}
