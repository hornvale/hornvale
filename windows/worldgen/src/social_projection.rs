//! Opt-in realization of aggregate social cohorts for synthetic probes.

use hornvale_demography::{
    BiologicalTransitionCapability, DescentMode, LifecycleTransitionKind, OffspringOrigin,
    ReproductiveRole, SocialCohortSummary,
};
use hornvale_history::{
    AssociationForm, GroupMembershipEvent, LifecycleEvent, RelationEvent, RelationKind,
    SocialEvent, validate_social_events,
};
use hornvale_kernel::{EntityId, Lineage, Seed, Stream, WorldTime, derive_entity_id};
use hornvale_person::{PersonSocialFact, PersonSocialSeed};

pub(crate) const PERSON_ROLE: &str = "synthetic-social-person";
pub(crate) const GROUP_ROLE: &str = "synthetic-social-group";

fn provenance() -> &'static str {
    crate::streams::SOCIAL_PROJECTION.as_str()
}

/// Explicitly non-default probe configurations from the approved design.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SyntheticSociety {
    /// Communal care without a required pair association.
    IndependentOrigin,
    /// Two descent lines whose residence is not fixed to either.
    DualDescent,
    /// Overlapping multi-party caregiver groups.
    CareCluster,
    /// Migration, separation, dissolution, and recomposition.
    RecomposingMobility,
    /// Associations whose institutional recognition is separate and later.
    InstitutionalRecognition,
    /// Independent social transition plus parental death and care transfer.
    LifecycleTransition,
}

/// Bounded realization controls for a synthetic projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SocialProjectionPins {
    person_count: u16,
    start: WorldTime,
}

impl SocialProjectionPins {
    /// Construct explicit synthetic-probe bounds.
    /// type-audit: bare-ok(count: person_count)
    pub fn new(person_count: u16, start: WorldTime) -> Result<Self, SocialProjectionError> {
        if !(3..=12).contains(&person_count) {
            return Err(SocialProjectionError::new(
                "synthetic social projection requires 3..=12 persons",
            ));
        }
        Ok(Self {
            person_count,
            start,
        })
    }

    /// Number of people this bounded probe realizes.
    /// type-audit: bare-ok(count: return)
    pub fn person_count(&self) -> u16 {
        self.person_count
    }

    /// First possible event time for this projection.
    pub fn start(&self) -> WorldTime {
        self.start
    }
}

/// One bounded, ordered realization with no aggregate-state mutation.
#[derive(Clone, Debug, PartialEq)]
pub struct SocialProjection {
    persons: Vec<PersonSocialSeed>,
    events: Vec<SocialEvent>,
    groups: Vec<EntityId>,
}

impl SocialProjection {
    /// Realized people in deterministic mint order.
    pub fn persons(&self) -> &[PersonSocialSeed] {
        &self.persons
    }

    /// Realized social events in nondecreasing time order.
    pub fn events(&self) -> &[SocialEvent] {
        &self.events
    }

    /// Realized group entities in deterministic mint order.
    pub fn groups(&self) -> &[EntityId] {
        &self.groups
    }
}

/// Descriptive refusal from synthetic social realization.
/// type-audit: bare-ok(prose: SocialProjectionError.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SocialProjectionError(String);

impl SocialProjectionError {
    fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl std::fmt::Display for SocialProjectionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for SocialProjectionError {}

impl From<hornvale_person::PersonSocialError> for SocialProjectionError {
    fn from(error: hornvale_person::PersonSocialError) -> Self {
        Self::new(error.to_string())
    }
}

impl From<hornvale_history::SocialEventError> for SocialProjectionError {
    fn from(error: hornvale_history::SocialEventError) -> Self {
        Self::new(error.to_string())
    }
}

fn sample_count(
    entries: &[(u32, f64)],
    stream: &mut Stream,
    field: &str,
) -> Result<u32, SocialProjectionError> {
    let weights: Vec<f64> = entries.iter().map(|(_, weight)| *weight).collect();
    let index = stream
        .weighted_index(&weights)
        .ok_or_else(|| SocialProjectionError::new(format!("{field} has no realizable outcome")))?;
    Ok(entries[index].0)
}

fn supports_transition(summary: &SocialCohortSummary, kind: LifecycleTransitionKind) -> bool {
    summary
        .lifecycle_transitions
        .iter()
        .any(|transition| transition.kind() == kind && transition.events_per_person_year() > 0.0)
}

fn supports_capability(
    summary: &SocialCohortSummary,
    capability: BiologicalTransitionCapability,
) -> bool {
    summary.transition_capabilities.contains(&capability)
}

fn independent_origin_roles(
    summary: &SocialCohortSummary,
    descent_count: u32,
) -> Result<Vec<ReproductiveRole>, SocialProjectionError> {
    if summary.reproductive.possibility.pathway_count == 0
        || !summary
            .offspring_pathways
            .iter()
            .any(|pathway| pathway.origin == OffspringOrigin::JoinedInputs)
    {
        return Err(SocialProjectionError::new(
            "independent-origin society requires an aggregate joined-input offspring pathway",
        ));
    }
    if !summary.descent_relations.iter().any(|relation| {
        relation.mode == DescentMode::CombinedSources
            && relation.contributing_source_count == descent_count
    }) {
        return Err(SocialProjectionError::new(
            "independent-origin society requires an aggregate descent relation matching its sampled origins",
        ));
    }

    let mut roles = Vec::new();
    for (role, weight) in summary.reproductive.reproductive_roles.entries() {
        if *weight > 0.0 && !roles.contains(role) {
            roles.push(*role);
        }
    }
    if roles.len() < 2 {
        return Err(SocialProjectionError::new(
            "independent-origin society requires multiple aggregate reproductive roles",
        ));
    }
    Ok(roles)
}

fn reproductive_role_entity(role: ReproductiveRole) -> EntityId {
    let ordinal = match role {
        ReproductiveRole::MaterialProducer => 0,
        ReproductiveRole::MaterialContributor => 1,
        ReproductiveRole::DevelopmentCarrier => 2,
        ReproductiveRole::DevelopmentSupporter => 3,
        ReproductiveRole::Host => 4,
        ReproductiveRole::Builder => 5,
    };
    entity("synthetic-reproductive-role", ordinal)
}

fn entity(role: &'static str, ordinal: u16) -> EntityId {
    derive_entity_id(Lineage {
        parent: None,
        role,
        ordinal,
    })
}

fn at(
    start: WorldTime,
    base_days: u32,
    cadence_days: u32,
    step: u32,
) -> Result<WorldTime, SocialProjectionError> {
    let days = i64::from(base_days)
        .checked_add(i64::from(cadence_days) * i64::from(step))
        .ok_or_else(|| SocialProjectionError::new("social projection day offset overflowed"))?;
    let ticks = days
        .checked_mul(WorldTime::TICKS_PER_STD_DAY)
        .and_then(|offset| start.ticks().checked_add(offset))
        .ok_or_else(|| SocialProjectionError::new("social projection time overflowed"))?;
    Ok(WorldTime::from_ticks(ticks))
}

fn relation(
    kind: RelationKind,
    source: EntityId,
    target: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    form: Option<&str>,
    interpretation: Option<&str>,
) -> Result<SocialEvent, SocialProjectionError> {
    let form = form.map(AssociationForm::new).transpose()?;
    Ok(SocialEvent::Relation(RelationEvent::new(
        kind,
        source,
        target,
        start,
        end,
        form,
        interpretation.map(str::to_string),
        provenance(),
    )?))
}

fn membership(
    member: EntityId,
    group: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
) -> Result<SocialEvent, SocialProjectionError> {
    Ok(SocialEvent::Membership(GroupMembershipEvent::new(
        member,
        group,
        start,
        end,
        provenance(),
    )?))
}

/// Realize one synthetic cohort on the dedicated social-projection stream.
pub fn project_social_cohort(
    summary: &SocialCohortSummary,
    seed: Seed,
    pins: &SocialProjectionPins,
    society: SyntheticSociety,
) -> Result<SocialProjection, SocialProjectionError> {
    // Save-format contract: one stream, consumed in this fixed order for
    // every synthetic society: association count, descent count, care-group
    // count, migration count, inheritance count, participant rotation,
    // initial day offset, event cadence. Society-specific emission performs
    // no further draws, so adding an event cannot reshuffle any other stream.
    let mut stream = seed.derive(crate::streams::SOCIAL_PROJECTION).stream();
    let association_count =
        sample_count(summary.associations.entries(), &mut stream, "association")?;
    let descent_count = sample_count(summary.descent.entries(), &mut stream, "descent")?;
    let care_count = sample_count(
        summary.care_topology.entries(),
        &mut stream,
        "care topology",
    )?;
    let migration_count = sample_count(summary.migration.entries(), &mut stream, "migration")?;
    let inheritance_count =
        sample_count(summary.inheritance.entries(), &mut stream, "inheritance")?;
    let rotation = stream.range_u32(0, u32::from(pins.person_count) - 1) as usize;
    let base_days = stream.range_u32(0, 6);
    let cadence_days = stream.range_u32(1, 2);

    let person_ids: Vec<EntityId> = (0..pins.person_count)
        .map(|ordinal| entity(PERSON_ROLE, ordinal))
        .collect();
    let person = |index: usize| person_ids[(index + rotation) % person_ids.len()];
    let groups = vec![entity(GROUP_ROLE, 0), entity(GROUP_ROLE, 1)];
    let time = |step| at(pins.start, base_days, cadence_days, step);

    let participant_cap = pins.person_count.saturating_sub(1) as usize;
    let person_index = |index: usize| (index + rotation) % person_ids.len();
    let mut person_facts = vec![Vec::new(); person_ids.len()];
    let mut events = Vec::new();
    match society {
        SyntheticSociety::IndependentOrigin => {
            if descent_count < 2 || care_count < 2 {
                return Err(SocialProjectionError::new(
                    "independent-origin society requires multiple origins and communal care in the aggregate cohort",
                ));
            }
            let reproductive_roles = independent_origin_roles(summary, descent_count)?;
            let origins = usize::try_from(descent_count)
                .unwrap_or(usize::MAX)
                .min(participant_cap);
            let caregivers = usize::try_from(care_count)
                .unwrap_or(usize::MAX)
                .min(participant_cap);
            let child = person(participant_cap);
            events.push(membership(child, groups[0], time(0)?, None)?);
            for index in 0..origins {
                person_facts[person_index(index)].push(PersonSocialFact::reproductive_role(
                    reproductive_role_entity(reproductive_roles[index % reproductive_roles.len()]),
                    time(0)?,
                    None,
                    provenance(),
                )?);
                events.push(relation(
                    RelationKind::Origin,
                    person(index),
                    child,
                    time(1)?,
                    None,
                    None,
                    None,
                )?);
            }
            for index in 0..caregivers {
                events.push(relation(
                    RelationKind::Care,
                    person(index),
                    child,
                    time(2)?,
                    None,
                    None,
                    None,
                )?);
                events.push(relation(
                    RelationKind::Dependency,
                    child,
                    person(index),
                    time(2)?,
                    None,
                    None,
                    None,
                )?);
            }
        }
        SyntheticSociety::DualDescent => {
            if descent_count < 2
                || inheritance_count == 0
                || !supports_transition(summary, LifecycleTransitionKind::ParentalDeath)
            {
                return Err(SocialProjectionError::new(
                    "dual-descent society requires two descent lines, parental-death, and inheritance support in the aggregate cohort",
                ));
            }
            let lines = usize::try_from(descent_count)
                .unwrap_or(usize::MAX)
                .min(participant_cap);
            let child = person(participant_cap);
            for index in 0..lines {
                events.push(relation(
                    RelationKind::Descent,
                    person(index),
                    child,
                    time(0)?,
                    None,
                    None,
                    None,
                )?);
            }
            events.push(SocialEvent::Lifecycle(LifecycleEvent::die(
                person(0),
                time(1)?,
                provenance(),
            )?));
            for _ in 0..inheritance_count {
                events.push(relation(
                    RelationKind::Transfer,
                    person(0),
                    child,
                    time(2)?,
                    None,
                    None,
                    None,
                )?);
            }
        }
        SyntheticSociety::CareCluster => {
            if care_count < 2 {
                return Err(SocialProjectionError::new(
                    "care-cluster society requires overlapping care support in the aggregate cohort",
                ));
            }
            let child = person(participant_cap);
            events.extend([
                membership(person(0), groups[0], time(0)?, None)?,
                membership(person(0), groups[1], time(0)?, None)?,
                membership(person(1), groups[1], time(0)?, None)?,
                membership(child, groups[0], time(0)?, None)?,
                membership(child, groups[1], time(0)?, None)?,
            ]);
            let caregivers = usize::try_from(care_count)
                .unwrap_or(usize::MAX)
                .min(participant_cap);
            for index in 0..caregivers {
                events.push(relation(
                    RelationKind::Care,
                    person(index),
                    child,
                    time(1)?,
                    None,
                    None,
                    None,
                )?);
            }
        }
        SyntheticSociety::RecomposingMobility => {
            if association_count < 2
                || migration_count == 0
                || !supports_transition(summary, LifecycleTransitionKind::Migration)
                || !supports_transition(summary, LifecycleTransitionKind::AssociationDissolution)
            {
                return Err(SocialProjectionError::new(
                    "recomposing mobility requires multi-party association, migration, and dissolution support",
                ));
            }
            let migration = time(4)?;
            events.extend([
                membership(person(0), groups[0], time(0)?, Some(migration))?,
                membership(person(1), groups[0], time(0)?, Some(migration))?,
                relation(
                    RelationKind::Association,
                    person(0),
                    person(1),
                    time(1)?,
                    None,
                    Some("shared-work"),
                    None,
                )?,
                relation(
                    RelationKind::Residence,
                    person(0),
                    groups[0],
                    time(2)?,
                    Some(migration),
                    None,
                    None,
                )?,
                SocialEvent::Lifecycle(LifecycleEvent::separate(
                    person(0),
                    person(1),
                    time(3)?,
                    provenance(),
                )?),
                membership(person(0), groups[1], migration, None)?,
                relation(
                    RelationKind::Residence,
                    person(0),
                    groups[1],
                    migration,
                    None,
                    None,
                    None,
                )?,
                SocialEvent::Lifecycle(LifecycleEvent::dissolve(
                    groups[0],
                    time(5)?,
                    provenance(),
                )?),
                relation(
                    RelationKind::Association,
                    person(0),
                    person(2),
                    time(6)?,
                    None,
                    Some("seasonal-alliance"),
                    None,
                )?,
            ]);
        }
        SyntheticSociety::InstitutionalRecognition => {
            if association_count < 2
                || !supports_transition(summary, LifecycleTransitionKind::AssociationFormation)
            {
                return Err(SocialProjectionError::new(
                    "institutional recognition requires association-formation support",
                ));
            }
            events.extend([
                relation(
                    RelationKind::Association,
                    person(0),
                    person(1),
                    time(0)?,
                    None,
                    Some("mutual-aid"),
                    None,
                )?,
                relation(
                    RelationKind::Recognition,
                    groups[0],
                    person(0),
                    time(2)?,
                    None,
                    None,
                    Some("recognized-mutual-aid"),
                )?,
            ]);
        }
        SyntheticSociety::LifecycleTransition => {
            if descent_count == 0
                || care_count == 0
                || !summary
                    .descent_relations
                    .iter()
                    .any(|relation| relation.contributing_source_count > 0)
                || !supports_transition(summary, LifecycleTransitionKind::ParentalDeath)
            {
                return Err(SocialProjectionError::new(
                    "lifecycle-transition society requires descent relation, care, and parental-death support in the aggregate cohort",
                ));
            }
            if !supports_capability(
                summary,
                BiologicalTransitionCapability::DevelopmentalMaturation,
            ) || !supports_transition(summary, LifecycleTransitionKind::Independence)
            {
                return Err(SocialProjectionError::new(
                    "lifecycle-transition society requires aggregate life-stage transition support",
                ));
            }
            let transition = time(2)?;
            person_facts[person_index(2)].extend([
                PersonSocialFact::gender_recognition(
                    "pre-independence",
                    time(0)?,
                    Some(transition),
                    provenance(),
                )?,
                PersonSocialFact::gender_recognition(
                    "post-independence",
                    transition,
                    None,
                    provenance(),
                )?,
                PersonSocialFact::transitioned(
                    "life-stage-social-role",
                    transition,
                    None,
                    provenance(),
                )?,
            ]);
            let death = time(3)?;
            let child = person(2);
            events.extend([
                relation(
                    RelationKind::Descent,
                    person(0),
                    child,
                    time(0)?,
                    None,
                    None,
                    None,
                )?,
                relation(
                    RelationKind::Care,
                    person(0),
                    child,
                    time(1)?,
                    Some(death),
                    None,
                    None,
                )?,
                relation(
                    RelationKind::Dependency,
                    child,
                    person(0),
                    time(1)?,
                    Some(death),
                    None,
                    None,
                )?,
                SocialEvent::Lifecycle(LifecycleEvent::die(person(0), death, provenance())?),
                relation(
                    RelationKind::Care,
                    person(1),
                    child,
                    time(4)?,
                    None,
                    None,
                    None,
                )?,
                relation(
                    RelationKind::Dependency,
                    child,
                    person(1),
                    time(4)?,
                    None,
                    None,
                    None,
                )?,
            ]);
        }
    }

    let persons = person_ids
        .iter()
        .copied()
        .zip(person_facts)
        .map(|(id, facts)| PersonSocialSeed::new(id, facts).map_err(Into::into))
        .collect::<Result<Vec<_>, SocialProjectionError>>()?;

    validate_social_events(&events)?;
    Ok(SocialProjection {
        persons,
        events,
        groups,
    })
}
