//! Ordered, append-only relation and lifecycle events for realized people.
//!
//! This module records what happened and in which direction. It deliberately
//! does not derive kinship words, household membership, marriage, or any other
//! cultural reading; later projections consume these events with explicit
//! social context.

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, RegistryError, Value, WorldTime};

const MEMBERSHIP: &str = "membership";
/// The exclusive end of a group-membership interval.
/// type-audit: bare-ok(identifier-text)
pub const MEMBERSHIP_ENDED: &str = "membership-ended";
/// The declared culture-neutral form attached to an association edge.
/// type-audit: bare-ok(identifier-text)
pub const ASSOCIATION_FORM: &str = "association-form";
/// The cultural or institutional interpretation attached to a recognition edge.
/// type-audit: bare-ok(identifier-text)
pub const RECOGNITION_INTERPRETATION: &str = "recognition-interpretation";
const SEPARATE: &str = "separate";
const DISSOLVE: &str = "dissolve";
const DIE: &str = "die";

/// Validation failure for a realized social event or ordered event history.
/// type-audit: bare-ok(prose: InvalidSocialEvent.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SocialEventError {
    /// A descriptive social-event contract violation.
    InvalidSocialEvent(String),
}

impl SocialEventError {
    fn new(message: impl Into<String>) -> Self {
        Self::InvalidSocialEvent(message.into())
    }
}

impl std::fmt::Display for SocialEventError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidSocialEvent(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for SocialEventError {}

/// Foundational relation kinds and their source-to-target direction.
///
/// placement: deliberate(history owns realized events while demography owns
/// pure projection inputs; sibling domains stay independent and worldgen
/// performs the lossless conversion) shape(11b622)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelationKind {
    /// Originator or origin source → originated person.
    Origin,
    /// Earlier or contributing person → descendant.
    Descent,
    /// Caregiver → care recipient.
    Care,
    /// Dependent → provider on whom they rely.
    Dependency,
    /// First named associate → second named associate.
    Association,
    /// Resident → place or co-resident group.
    Residence,
    /// Custodian → person or thing held in custody.
    Custody,
    /// Prior holder or source → recipient.
    Transfer,
    /// Recognizing person or institution → recognized subject.
    Recognition,
}

impl RelationKind {
    /// Ledger predicate preserving this kind's documented direction.
    /// type-audit: bare-ok(identifier-text: return)
    pub const fn predicate(self) -> &'static str {
        match self {
            Self::Origin => "origin",
            Self::Descent => "descent",
            Self::Care => "care",
            Self::Dependency => "dependency",
            Self::Association => "association",
            Self::Residence => "residence",
            Self::Custody => "custody",
            Self::Transfer => "transfer",
            Self::Recognition => "recognition",
        }
    }

    /// Ledger predicate that repeats this relation's participants at its
    /// exclusive end.
    /// type-audit: bare-ok(identifier-text: return)
    pub const fn ended_predicate(self) -> &'static str {
        match self {
            Self::Origin => "origin-ended",
            Self::Descent => "descent-ended",
            Self::Care => "care-ended",
            Self::Dependency => "dependency-ended",
            Self::Association => "association-ended",
            Self::Residence => "residence-ended",
            Self::Custody => "custody-ended",
            Self::Transfer => "transfer-ended",
            Self::Recognition => "recognition-ended",
        }
    }

    const fn allows_deceased_source(self) -> bool {
        matches!(self, Self::Transfer)
    }

    const fn allows_deceased_target(self) -> bool {
        matches!(self, Self::Recognition)
    }
}

/// Explicit, culture-neutral form of an association.
///
/// Values such as `shared-work`, `seasonal-co-residence`, or a later
/// culture's own term remain data. The foundational type has no privileged
/// `marriage` variant.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssociationForm(String);

impl AssociationForm {
    /// Construct a non-empty association form.
    /// type-audit: bare-ok(identifier-text: label)
    pub fn new(label: &str) -> Result<Self, SocialEventError> {
        if label.trim().is_empty() {
            return Err(SocialEventError::new("association form must not be empty"));
        }
        Ok(Self(label.to_string()))
    }

    /// The explicit authored form value.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// One directional relation with a half-open applicability interval.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RelationEvent {
    kind: RelationKind,
    source: EntityId,
    target: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    association_form: Option<AssociationForm>,
    interpretation: Option<String>,
    provenance: String,
}

impl RelationEvent {
    /// Construct and validate one realized relation.
    ///
    /// `association_form` is required only for an association;
    /// `interpretation` is required only for recognition. This keeps the
    /// event itself separate from the social reading applied to it.
    /// type-audit: bare-ok(identifier-text: interpretation), bare-ok(prose: provenance)
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        kind: RelationKind,
        source: EntityId,
        target: EntityId,
        start: WorldTime,
        end: Option<WorldTime>,
        association_form: Option<AssociationForm>,
        interpretation: Option<String>,
        provenance: &str,
    ) -> Result<Self, SocialEventError> {
        if source == target {
            return Err(SocialEventError::new(format!(
                "{} requires distinct source and target participants",
                kind.predicate()
            )));
        }
        validate_interval("relation", start, end)?;
        validate_provenance("relation", provenance)?;
        match kind {
            RelationKind::Association if association_form.is_none() => {
                return Err(SocialEventError::new(
                    "association relation requires an explicit association form",
                ));
            }
            RelationKind::Association => {}
            _ if association_form.is_some() => {
                return Err(SocialEventError::new(
                    "association form is only valid for association relations",
                ));
            }
            _ => {}
        }
        match kind {
            RelationKind::Recognition
                if interpretation
                    .as_deref()
                    .is_none_or(|value| value.trim().is_empty()) =>
            {
                return Err(SocialEventError::new(
                    "recognition relation requires an explicit interpretation",
                ));
            }
            RelationKind::Recognition => {}
            _ if interpretation.is_some() => {
                return Err(SocialEventError::new(
                    "interpretation is only valid for recognition relations",
                ));
            }
            _ => {}
        }
        Ok(Self {
            kind,
            source,
            target,
            start,
            end,
            association_form,
            interpretation,
            provenance: provenance.to_string(),
        })
    }

    /// Foundational relation kind.
    pub fn kind(&self) -> RelationKind {
        self.kind
    }

    /// Directional source participant.
    pub fn source(&self) -> EntityId {
        self.source
    }

    /// Directional target participant.
    pub fn target(&self) -> EntityId {
        self.target
    }

    /// Inclusive start of applicability.
    pub fn start(&self) -> WorldTime {
        self.start
    }

    /// Exclusive end of applicability, or no known end.
    pub fn end(&self) -> Option<WorldTime> {
        self.end
    }

    /// Explicit association form, present only for association relations.
    pub fn association_form(&self) -> Option<&AssociationForm> {
        self.association_form.as_ref()
    }

    /// Explicit institutional or cultural interpretation, present only for
    /// recognition relations.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn interpretation(&self) -> Option<&str> {
        self.interpretation.as_deref()
    }

    fn fact(&self) -> Fact {
        Fact {
            subject: self.source,
            predicate: self.kind.predicate().to_string(),
            object: Value::Entity(self.target),
            place: None,
            day: Some(self.start),
            provenance: self.provenance.clone(),
        }
    }

    fn facts(&self) -> Vec<Fact> {
        let mut facts = Vec::with_capacity(3);
        facts.push(self.fact());
        if let Some(form) = &self.association_form {
            facts.push(Fact {
                subject: self.source,
                predicate: ASSOCIATION_FORM.to_string(),
                object: Value::Text(form.as_str().to_string()),
                place: None,
                day: Some(self.start),
                provenance: self.provenance.clone(),
            });
        }
        if let Some(interpretation) = &self.interpretation {
            facts.push(Fact {
                subject: self.source,
                predicate: RECOGNITION_INTERPRETATION.to_string(),
                object: Value::Text(interpretation.clone()),
                place: None,
                day: Some(self.start),
                provenance: self.provenance.clone(),
            });
        }
        if let Some(end) = self.end {
            facts.push(Fact {
                subject: self.source,
                predicate: self.kind.ended_predicate().to_string(),
                object: Value::Entity(self.target),
                place: None,
                day: Some(end),
                provenance: self.provenance.clone(),
            });
        }
        facts
    }
}

/// One person's time-bounded membership in a derived group.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupMembershipEvent {
    member: EntityId,
    group: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    provenance: String,
}

impl GroupMembershipEvent {
    /// Construct and validate a membership interval.
    /// type-audit: bare-ok(prose: provenance)
    pub fn new(
        member: EntityId,
        group: EntityId,
        start: WorldTime,
        end: Option<WorldTime>,
        provenance: &str,
    ) -> Result<Self, SocialEventError> {
        if member == group {
            return Err(SocialEventError::new(
                "group membership requires distinct member and group entities",
            ));
        }
        validate_interval("group membership", start, end)?;
        validate_provenance("group membership", provenance)?;
        Ok(Self {
            member,
            group,
            start,
            end,
            provenance: provenance.to_string(),
        })
    }

    /// Member participant.
    pub fn member(&self) -> EntityId {
        self.member
    }

    /// Group in which the person participates.
    pub fn group(&self) -> EntityId {
        self.group
    }

    /// Inclusive membership start.
    pub fn start(&self) -> WorldTime {
        self.start
    }

    /// Exclusive membership end, or no known end.
    pub fn end(&self) -> Option<WorldTime> {
        self.end
    }

    fn fact(&self) -> Fact {
        Fact {
            subject: self.member,
            predicate: MEMBERSHIP.to_string(),
            object: Value::Entity(self.group),
            place: None,
            day: Some(self.start),
            provenance: self.provenance.clone(),
        }
    }

    fn facts(&self) -> Vec<Fact> {
        let mut facts = vec![self.fact()];
        if let Some(end) = self.end {
            facts.push(Fact {
                subject: self.member,
                predicate: MEMBERSHIP_ENDED.to_string(),
                object: Value::Entity(self.group),
                place: None,
                day: Some(end),
                provenance: self.provenance.clone(),
            });
        }
        facts
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LifecycleKind {
    Separate,
    Dissolve,
    Die,
}

/// One append-only lifecycle closure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LifecycleEvent {
    kind: LifecycleKind,
    subject: EntityId,
    target: Option<EntityId>,
    at: WorldTime,
    provenance: String,
}

impl LifecycleEvent {
    /// Close the directed association from `source` to `target`.
    /// type-audit: bare-ok(prose: provenance)
    pub fn separate(
        source: EntityId,
        target: EntityId,
        at: WorldTime,
        provenance: &str,
    ) -> Result<Self, SocialEventError> {
        if source == target {
            return Err(SocialEventError::new(
                "separate requires distinct source and target participants",
            ));
        }
        validate_provenance("separate", provenance)?;
        Ok(Self {
            kind: LifecycleKind::Separate,
            subject: source,
            target: Some(target),
            at,
            provenance: provenance.to_string(),
        })
    }

    /// Close a group's future membership activity.
    /// type-audit: bare-ok(prose: provenance)
    pub fn dissolve(
        group: EntityId,
        at: WorldTime,
        provenance: &str,
    ) -> Result<Self, SocialEventError> {
        validate_provenance("dissolve", provenance)?;
        Ok(Self {
            kind: LifecycleKind::Dissolve,
            subject: group,
            target: None,
            at,
            provenance: provenance.to_string(),
        })
    }

    /// Close a person's future lifecycle participation.
    /// type-audit: bare-ok(prose: provenance)
    pub fn die(
        person: EntityId,
        at: WorldTime,
        provenance: &str,
    ) -> Result<Self, SocialEventError> {
        validate_provenance("die", provenance)?;
        Ok(Self {
            kind: LifecycleKind::Die,
            subject: person,
            target: None,
            at,
            provenance: provenance.to_string(),
        })
    }

    fn fact(&self) -> Fact {
        let (predicate, object) = match self.kind {
            LifecycleKind::Separate => (
                SEPARATE,
                Value::Entity(self.target.expect("separation has a target")),
            ),
            LifecycleKind::Dissolve => (DISSOLVE, Value::Flag(true)),
            LifecycleKind::Die => (DIE, Value::Flag(true)),
        };
        Fact {
            subject: self.subject,
            predicate: predicate.to_string(),
            object,
            place: None,
            day: Some(self.at),
            provenance: self.provenance.clone(),
        }
    }
}

/// Any realized social event in deterministic input order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SocialEvent {
    /// A directional relation event.
    Relation(RelationEvent),
    /// A group-membership event.
    Membership(GroupMembershipEvent),
    /// A lifecycle closure.
    Lifecycle(LifecycleEvent),
}

impl SocialEvent {
    /// Convert this event to its append-only ledger fact.
    pub fn fact(&self) -> Fact {
        match self {
            Self::Relation(event) => event.fact(),
            Self::Membership(event) => event.fact(),
            Self::Lifecycle(event) => event.fact(),
        }
    }

    /// Convert this event to its complete ordered ledger-fact bundle.
    ///
    /// The primary edge is first. Cultural detail facts follow it, then an
    /// optional exclusive-end fact. Lifecycle events contain only their
    /// primary closure fact. Commit each returned bundle in order: a cultural
    /// detail is the companion of the primary edge immediately before it.
    pub fn facts(&self) -> Vec<Fact> {
        match self {
            Self::Relation(event) => event.facts(),
            Self::Membership(event) => event.facts(),
            Self::Lifecycle(event) => vec![event.fact()],
        }
    }

    fn time(&self) -> WorldTime {
        match self {
            Self::Relation(event) => event.start,
            Self::Membership(event) => event.start,
            Self::Lifecycle(event) => event.at,
        }
    }
}

/// Validate an ordered event history without mutating or deleting any event.
///
/// The scan enforces nondecreasing time and the three closure rules in the
/// approved substrate: separation closes that directed association,
/// dissolution closes its group's social participation, and death closes the
/// person's future participation according to the role each relation requires,
/// while preserving posthumous recognition and estate transfer.
pub fn validate_social_events(events: &[SocialEvent]) -> Result<(), SocialEventError> {
    let mut previous_time = None;
    let mut associations: Vec<AssociationInterval> = Vec::new();
    let mut dissolved_groups = Vec::new();
    let mut dead_people = Vec::new();

    for event in events {
        let at = event.time();
        if previous_time.is_some_and(|previous| at < previous) {
            return Err(SocialEventError::new(
                "social events must be ordered by nondecreasing time",
            ));
        }
        previous_time = Some(at);

        match event {
            SocialEvent::Relation(relation) => {
                if dissolved_groups.contains(&relation.source)
                    || dissolved_groups.contains(&relation.target)
                {
                    return Err(SocialEventError::new(
                        "relation activity involves a group after dissolution",
                    ));
                }
                if !relation.kind.allows_deceased_source() && dead_people.contains(&relation.source)
                {
                    return Err(SocialEventError::new(
                        "relation activity is performed by a person after death",
                    ));
                }
                if !relation.kind.allows_deceased_target() && dead_people.contains(&relation.target)
                {
                    return Err(SocialEventError::new(
                        "relation activity requires participation by a person after death",
                    ));
                }
                if relation.kind == RelationKind::Association {
                    associations.push(AssociationInterval {
                        source: relation.source,
                        target: relation.target,
                        start: relation.start,
                        end: relation.end,
                        separated: false,
                    });
                }
            }
            SocialEvent::Membership(membership) => {
                if dead_people.contains(&membership.member) {
                    return Err(SocialEventError::new(
                        "group membership activity involves a person after death",
                    ));
                }
                if dissolved_groups.contains(&membership.group) {
                    return Err(SocialEventError::new(
                        "group membership activity occurs after dissolution",
                    ));
                }
                if dissolved_groups.contains(&membership.member) {
                    return Err(SocialEventError::new(
                        "group membership activity involves a group after dissolution",
                    ));
                }
            }
            SocialEvent::Lifecycle(lifecycle) => match lifecycle.kind {
                LifecycleKind::Separate => {
                    let target = lifecycle.target.expect("separation has a target");
                    if dissolved_groups.contains(&lifecycle.subject)
                        || dissolved_groups.contains(&target)
                    {
                        return Err(SocialEventError::new(
                            "separation activity involves a group after dissolution",
                        ));
                    }
                    if dead_people.contains(&lifecycle.subject) {
                        return Err(SocialEventError::new(
                            "separation is performed by a person after death",
                        ));
                    }
                    if let Some(active) = associations.iter_mut().rev().find(|relation| {
                        relation.source == lifecycle.subject
                            && relation.target == target
                            && relation.is_active_at(lifecycle.at)
                    }) {
                        active.separated = true;
                    } else if associations.iter().any(|relation| {
                        relation.source == target
                            && relation.target == lifecycle.subject
                            && relation.is_active_at(lifecycle.at)
                    }) {
                        return Err(SocialEventError::new(
                            "separate participants reverse the active association direction",
                        ));
                    } else if associations.iter().any(|relation| {
                        relation.source == lifecycle.subject && relation.target == target
                    }) {
                        return Err(SocialEventError::new(
                            "separate must occur strictly inside an active association interval",
                        ));
                    } else {
                        return Err(SocialEventError::new(
                            "separate has no matching active association",
                        ));
                    }
                }
                LifecycleKind::Dissolve => {
                    if !dissolved_groups.contains(&lifecycle.subject) {
                        dissolved_groups.push(lifecycle.subject);
                    }
                }
                LifecycleKind::Die => {
                    if !dead_people.contains(&lifecycle.subject) {
                        dead_people.push(lifecycle.subject);
                    }
                }
            },
        }
    }
    Ok(())
}

#[derive(Clone, Copy, Debug)]
struct AssociationInterval {
    source: EntityId,
    target: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    separated: bool,
}

impl AssociationInterval {
    fn is_active_at(&self, at: WorldTime) -> bool {
        !self.separated && self.start < at && self.end.is_none_or(|end| at < end)
    }
}

fn validate_interval(
    label: &str,
    start: WorldTime,
    end: Option<WorldTime>,
) -> Result<(), SocialEventError> {
    if end.is_some_and(|end| end <= start) {
        return Err(SocialEventError::new(format!(
            "{label} interval end must be after its start"
        )));
    }
    Ok(())
}

fn validate_provenance(label: &str, provenance: &str) -> Result<(), SocialEventError> {
    if provenance.trim().is_empty() {
        return Err(SocialEventError::new(format!(
            "{label} provenance must not be empty"
        )));
    }
    Ok(())
}

pub(crate) fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    for (predicate, description) in [
        (
            "origin",
            "the originated person, directed from their origin source",
        ),
        (
            "descent",
            "the descendant, directed from an earlier contributor",
        ),
        ("care", "the care recipient, directed from their caregiver"),
        ("dependency", "the provider, directed from the dependent"),
        (
            "association",
            "the second participant in an explicitly formed association",
        ),
        (
            "residence",
            "the place or group in which the subject resides",
        ),
        (
            "custody",
            "the person or thing for which the subject has custody",
        ),
        ("transfer", "the recipient of a transfer from the subject"),
        (
            "recognition",
            "the recognized subject, directed from the recognizing person or institution",
        ),
        (
            ASSOCIATION_FORM,
            "the declared form of the subject's association beginning at this time",
        ),
        (
            RECOGNITION_INTERPRETATION,
            "the interpretation assigned by the recognizing subject at this time",
        ),
        (MEMBERSHIP, "the group in which the subject participates"),
        (
            MEMBERSHIP_ENDED,
            "the group in which the subject's membership ended at this time",
        ),
        (SEPARATE, "the participant from whom the subject separated"),
        (DISSOLVE, "the subject group ceased future activity"),
        (DIE, "the subject person ceased future lifecycle activity"),
    ] {
        registry.register_predicate(predicate, false, description)?;
    }
    for kind in [
        RelationKind::Origin,
        RelationKind::Descent,
        RelationKind::Care,
        RelationKind::Dependency,
        RelationKind::Association,
        RelationKind::Residence,
        RelationKind::Custody,
        RelationKind::Transfer,
        RelationKind::Recognition,
    ] {
        registry.register_predicate(
            kind.ended_predicate(),
            false,
            &format!(
                "the {} target whose relation from the subject ended at this time",
                kind.predicate()
            ),
        )?;
    }
    Ok(())
}
