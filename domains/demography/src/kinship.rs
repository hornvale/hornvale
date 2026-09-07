//! Pure, bounded readings over realized social-event observations.

use crate::groups::SocialContext;
use hornvale_kernel::{EntityId, WorldTime};
use std::fmt;

/// Realized relation vocabulary accepted by the domain projection boundary.
///
/// placement: deliberate(history owns realized events while demography owns
/// pure projection inputs; sibling domains stay independent and worldgen
/// performs the lossless conversion) shape(11b622)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProjectionRelationKind {
    /// Origin source to originated person.
    Origin,
    /// Descent contributor to descendant.
    Descent,
    /// Care provider to recipient.
    Care,
    /// Dependent to provider.
    Dependency,
    /// Time-bounded association.
    Association,
    /// Resident to place or group.
    Residence,
    /// Custodian to subject.
    Custody,
    /// Prior holder to recipient.
    Transfer,
    /// Recognizer to recognized subject.
    Recognition,
}

/// Kernel-only, provenance-bearing observation consumed by pure projections.
/// type-audit: bare-ok(identifier-text: Relation.detail), bare-ok(prose: Relation.provenance), bare-ok(prose: Membership.provenance), bare-ok(prose: Separation.provenance), bare-ok(prose: Dissolution.provenance), bare-ok(prose: Death.provenance)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProjectionEvent {
    /// One directional relation.
    Relation {
        /// Relation meaning and direction.
        kind: ProjectionRelationKind,
        /// Directional source.
        source: EntityId,
        /// Directional target.
        target: EntityId,
        /// Inclusive relation start.
        start: WorldTime,
        /// Exclusive relation end.
        end: Option<WorldTime>,
        /// Explicit association form or recognition interpretation.
        detail: Option<String>,
        /// Source-event provenance.
        provenance: String,
    },
    /// One time-bounded membership in a declared group.
    Membership {
        /// Member participant.
        member: EntityId,
        /// Group participant.
        group: EntityId,
        /// Inclusive membership start.
        start: WorldTime,
        /// Exclusive membership end.
        end: Option<WorldTime>,
        /// Source-event provenance.
        provenance: String,
    },
    /// Closure of one directed association.
    Separation {
        /// Association source.
        source: EntityId,
        /// Association target.
        target: EntityId,
        /// Closure time.
        at: WorldTime,
        /// Source-event provenance.
        provenance: String,
    },
    /// Closure of one declared group.
    Dissolution {
        /// Dissolved group.
        group: EntityId,
        /// Closure time.
        at: WorldTime,
        /// Source-event provenance.
        provenance: String,
    },
    /// Death of one realized person.
    Death {
        /// Deceased person.
        person: EntityId,
        /// Death time.
        at: WorldTime,
        /// Source-event provenance.
        provenance: String,
    },
}

impl ProjectionEvent {
    /// Construct one relation observation.
    /// type-audit: bare-ok(identifier-text: detail), bare-ok(prose: provenance)
    #[allow(clippy::too_many_arguments)]
    pub fn relation(
        kind: ProjectionRelationKind,
        source: EntityId,
        target: EntityId,
        start: WorldTime,
        end: Option<WorldTime>,
        detail: Option<String>,
        provenance: &str,
    ) -> Result<Self, ProjectionError> {
        validate_pair("relation", source, target)?;
        validate_interval("relation", start, end)?;
        validate_provenance("relation", provenance)?;
        match kind {
            ProjectionRelationKind::Association | ProjectionRelationKind::Recognition
                if detail
                    .as_deref()
                    .is_none_or(|value| value.trim().is_empty()) =>
            {
                return Err(ProjectionError::new(
                    "association and recognition observations require explicit detail",
                ));
            }
            ProjectionRelationKind::Association | ProjectionRelationKind::Recognition => {}
            _ if detail.is_some() => {
                return Err(ProjectionError::new(
                    "event detail is only valid for association or recognition observations",
                ));
            }
            _ => {}
        }
        Ok(Self::Relation {
            kind,
            source,
            target,
            start,
            end,
            detail,
            provenance: provenance.to_string(),
        })
    }

    /// Construct one membership observation.
    /// type-audit: bare-ok(prose: provenance)
    pub fn membership(
        member: EntityId,
        group: EntityId,
        start: WorldTime,
        end: Option<WorldTime>,
        provenance: &str,
    ) -> Result<Self, ProjectionError> {
        validate_pair("membership", member, group)?;
        validate_interval("membership", start, end)?;
        validate_provenance("membership", provenance)?;
        Ok(Self::Membership {
            member,
            group,
            start,
            end,
            provenance: provenance.to_string(),
        })
    }

    /// Construct one association closure.
    /// type-audit: bare-ok(prose: provenance)
    pub fn separation(
        source: EntityId,
        target: EntityId,
        at: WorldTime,
        provenance: &str,
    ) -> Result<Self, ProjectionError> {
        validate_pair("separation", source, target)?;
        validate_provenance("separation", provenance)?;
        Ok(Self::Separation {
            source,
            target,
            at,
            provenance: provenance.to_string(),
        })
    }

    /// Construct one group closure.
    /// type-audit: bare-ok(prose: provenance)
    pub fn dissolution(
        group: EntityId,
        at: WorldTime,
        provenance: &str,
    ) -> Result<Self, ProjectionError> {
        validate_provenance("dissolution", provenance)?;
        Ok(Self::Dissolution {
            group,
            at,
            provenance: provenance.to_string(),
        })
    }

    /// Construct one death observation.
    /// type-audit: bare-ok(prose: provenance)
    pub fn death(
        person: EntityId,
        at: WorldTime,
        provenance: &str,
    ) -> Result<Self, ProjectionError> {
        validate_provenance("death", provenance)?;
        Ok(Self::Death {
            person,
            at,
            provenance: provenance.to_string(),
        })
    }

    pub(crate) fn time(&self) -> WorldTime {
        match self {
            Self::Relation { start, .. } | Self::Membership { start, .. } => *start,
            Self::Separation { at, .. } | Self::Dissolution { at, .. } | Self::Death { at, .. } => {
                *at
            }
        }
    }
}

/// Explicit resource limits for deterministic projections.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProjectionBounds {
    pub(crate) max_events: usize,
    pub(crate) max_depth: usize,
}

impl ProjectionBounds {
    /// Construct positive event and descent-depth limits.
    /// type-audit: bare-ok(count: max_events), bare-ok(count: max_depth)
    pub fn new(max_events: usize, max_depth: usize) -> Result<Self, ProjectionError> {
        if max_events == 0 {
            return Err(ProjectionError::new(
                "projection event bound must be positive",
            ));
        }
        if max_depth == 0 {
            return Err(ProjectionError::new(
                "projection traversal depth bound must be positive",
            ));
        }
        Ok(Self {
            max_events,
            max_depth,
        })
    }
}

/// Descriptive refusal from a pure social projection.
/// type-audit: bare-ok(prose: ProjectionError.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProjectionError(String);

impl ProjectionError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl fmt::Display for ProjectionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for ProjectionError {}

/// A culture-neutral kinship reading derived from origin or descent.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KinshipKind {
    /// Source of an origin event.
    OriginSource,
    /// Target of an origin event.
    Originated,
    /// Direct descent source.
    Parent,
    /// Direct descendant.
    Child,
    /// Two people sharing a direct descent source.
    Sibling,
    /// Transitive descent source.
    Ancestor,
    /// Transitive descendant.
    Descendant,
}

/// One provenance-bearing kinship reading.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KinshipRelation {
    kind: KinshipKind,
    subject: EntityId,
    relative: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    provenance: Vec<String>,
}

impl KinshipRelation {
    /// Derived reading kind.
    pub fn kind(&self) -> KinshipKind {
        self.kind
    }
    /// Person from whose perspective the reading is directed.
    pub fn subject(&self) -> EntityId {
        self.subject
    }
    /// Related person.
    pub fn relative(&self) -> EntityId {
        self.relative
    }
    /// Inclusive start of all supporting events.
    pub fn start(&self) -> WorldTime {
        self.start
    }
    /// Exclusive common end of supporting events.
    pub fn end(&self) -> Option<WorldTime> {
        self.end
    }
    /// Ordered source-event provenance.
    /// type-audit: bare-ok(prose: return)
    pub fn provenance(&self) -> &[String] {
        &self.provenance
    }
}

/// Distinct care or care-adjacent reading.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CareKind {
    /// Actual support provision.
    Care,
    /// Recognized responsibility or authority.
    Custody,
    /// Care and custody jointly support an adoptive reading.
    Adoption,
    /// Institutional interpretation, distinct from care and custody.
    InstitutionalRecognition,
}

/// One provenance-bearing care reading.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CareProjection {
    kind: CareKind,
    provider: EntityId,
    recipient: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    label: Option<String>,
    provenance: Vec<String>,
}

impl CareProjection {
    /// Care reading kind.
    pub fn kind(&self) -> CareKind {
        self.kind
    }
    /// Provider, custodian, or recognizing institution.
    pub fn provider(&self) -> EntityId {
        self.provider
    }
    /// Recipient or recognized subject.
    pub fn recipient(&self) -> EntityId {
        self.recipient
    }
    /// Inclusive start of the reading.
    pub fn start(&self) -> WorldTime {
        self.start
    }
    /// Exclusive end of the reading.
    pub fn end(&self) -> Option<WorldTime> {
        self.end
    }
    /// Explicit recognition label, when the source event carries one.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(&self) -> Option<&str> {
        self.label.as_deref()
    }
    /// Ordered source-event provenance.
    /// type-audit: bare-ok(prose: return)
    pub fn provenance(&self) -> &[String] {
        &self.provenance
    }
}

/// One post-death transfer interpreted as an inheritance claim.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InheritanceClaim {
    deceased: EntityId,
    claimant: EntityId,
    at: WorldTime,
    descendant: bool,
    recognized_label: Option<String>,
    provenance: Vec<String>,
}

impl InheritanceClaim {
    /// Deceased transfer source.
    pub fn deceased(&self) -> EntityId {
        self.deceased
    }
    /// Transfer recipient.
    pub fn claimant(&self) -> EntityId {
        self.claimant
    }
    /// Transfer time.
    pub fn at(&self) -> WorldTime {
        self.at
    }
    /// Whether descent evidence connects deceased to claimant.
    /// type-audit: bare-ok(flag: return)
    pub fn is_descendant(&self) -> bool {
        self.descendant
    }
    /// Caller-authored property-context label.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn recognized_label(&self) -> Option<&str> {
        self.recognized_label.as_deref()
    }
    /// Ordered death, transfer, and optional descent provenance.
    /// type-audit: bare-ok(prose: return)
    pub fn provenance(&self) -> &[String] {
        &self.provenance
    }
}

/// Derive culture-neutral origin, descent, and sibling readings.
pub fn derive_kinship(
    events: &[ProjectionEvent],
    bounds: ProjectionBounds,
) -> Result<Vec<KinshipRelation>, ProjectionError> {
    validate_events(events, bounds)?;
    let descent = relation_edges(events, ProjectionRelationKind::Descent);
    validate_descent_depth(&descent, bounds.max_depth)?;

    let mut relations = Vec::new();
    for edge in relation_edges(events, ProjectionRelationKind::Origin) {
        relations.push(kinship_from_edge(KinshipKind::OriginSource, &edge));
        relations.push(reverse_kinship_from_edge(KinshipKind::Originated, &edge));
    }
    for edge in &descent {
        relations.push(kinship_from_edge(KinshipKind::Parent, edge));
        relations.push(reverse_kinship_from_edge(KinshipKind::Child, edge));
    }

    for first in &descent {
        for second in &descent {
            if first.source != second.source || first.target >= second.target {
                continue;
            }
            if let Some((start, end)) =
                intersect_intervals(first.start, first.end, second.start, second.end)
            {
                relations.push(KinshipRelation {
                    kind: KinshipKind::Sibling,
                    subject: first.target,
                    relative: second.target,
                    start,
                    end,
                    provenance: vec![first.provenance.clone(), second.provenance.clone()],
                });
                relations.push(KinshipRelation {
                    kind: KinshipKind::Sibling,
                    subject: second.target,
                    relative: first.target,
                    start,
                    end,
                    provenance: vec![second.provenance.clone(), first.provenance.clone()],
                });
            }
        }
    }

    let mut sources = Vec::new();
    for source in descent.iter().map(|edge| edge.source) {
        if !sources.contains(&source) {
            sources.push(source);
        }
    }
    for source in sources {
        let mut paths = Vec::new();
        collect_descent_paths(source, source, &descent, &mut Vec::new(), &mut paths)?;
        for path in paths.into_iter().filter(|path| path.len() > 1) {
            let first = path.first().expect("nonempty descent path");
            let last = path.last().expect("nonempty descent path");
            let (start, end) = path_interval(&path)
                .expect("a traversed descent path has a shared active interval");
            let provenance = path
                .iter()
                .map(|edge| edge.provenance.clone())
                .collect::<Vec<_>>();
            relations.push(KinshipRelation {
                kind: KinshipKind::Ancestor,
                subject: first.source,
                relative: last.target,
                start,
                end,
                provenance: provenance.clone(),
            });
            relations.push(KinshipRelation {
                kind: KinshipKind::Descendant,
                subject: last.target,
                relative: first.source,
                start,
                end,
                provenance,
            });
        }
    }

    Ok(relations)
}

/// Derive distinct care, custody, adoption, and recognition readings.
pub fn derive_care(
    events: &[ProjectionEvent],
    bounds: ProjectionBounds,
) -> Result<Vec<CareProjection>, ProjectionError> {
    validate_events(events, bounds)?;
    let care = relation_edges(events, ProjectionRelationKind::Care);
    let custody = relation_edges(events, ProjectionRelationKind::Custody);
    let mut projections = Vec::new();

    for event in events {
        let ProjectionEvent::Relation {
            kind,
            source,
            target,
            start,
            end,
            detail,
            provenance,
        } = event
        else {
            continue;
        };
        let care_kind = match kind {
            ProjectionRelationKind::Care => Some(CareKind::Care),
            ProjectionRelationKind::Custody => Some(CareKind::Custody),
            _ => None,
        };
        if let Some(kind) = care_kind {
            projections.push(CareProjection {
                kind,
                provider: *source,
                recipient: *target,
                start: *start,
                end: *end,
                label: detail.clone(),
                provenance: vec![provenance.clone()],
            });
        }
    }

    for care_edge in &care {
        for custody_edge in &custody {
            if care_edge.source != custody_edge.source || care_edge.target != custody_edge.target {
                continue;
            }
            if let Some((start, end)) = intersect_intervals(
                care_edge.start,
                care_edge.end,
                custody_edge.start,
                custody_edge.end,
            ) {
                projections.push(CareProjection {
                    kind: CareKind::Adoption,
                    provider: care_edge.source,
                    recipient: care_edge.target,
                    start,
                    end,
                    label: None,
                    provenance: vec![
                        care_edge.provenance.clone(),
                        custody_edge.provenance.clone(),
                    ],
                });
            }
        }
    }

    for event in events {
        let ProjectionEvent::Relation {
            kind: ProjectionRelationKind::Recognition,
            source,
            target,
            start,
            end,
            detail,
            provenance,
        } = event
        else {
            continue;
        };
        projections.push(CareProjection {
            kind: CareKind::InstitutionalRecognition,
            provider: *source,
            recipient: *target,
            start: *start,
            end: *end,
            label: detail.clone(),
            provenance: vec![provenance.clone()],
        });
    }
    Ok(projections)
}

/// Derive post-death transfer claims under external property context.
pub fn derive_inheritance(
    events: &[ProjectionEvent],
    context: &SocialContext,
    bounds: ProjectionBounds,
) -> Result<Vec<InheritanceClaim>, ProjectionError> {
    validate_events(events, bounds)?;
    let descent = relation_edges(events, ProjectionRelationKind::Descent);
    validate_descent_depth(&descent, bounds.max_depth)?;
    let deaths = events
        .iter()
        .filter_map(|event| match event {
            ProjectionEvent::Death {
                person,
                at,
                provenance,
            } => Some((*person, *at, provenance)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut claims = Vec::new();

    for event in events {
        let ProjectionEvent::Relation {
            kind: ProjectionRelationKind::Transfer,
            source,
            target,
            start,
            provenance,
            ..
        } = event
        else {
            continue;
        };
        let Some((_, _, death_provenance)) = deaths
            .iter()
            .rev()
            .find(|(person, at, _)| person == source && *at <= *start)
        else {
            continue;
        };

        let path = first_descent_path(*source, *target, &descent)?;
        let mut sources = vec![(*death_provenance).clone(), provenance.clone()];
        if let Some(path) = &path {
            sources.extend(path.iter().map(|edge| edge.provenance.clone()));
        }
        claims.push(InheritanceClaim {
            deceased: *source,
            claimant: *target,
            at: *start,
            descendant: path.is_some(),
            recognized_label: context.property.recognized_label.clone(),
            provenance: sources,
        });
    }

    Ok(claims)
}

pub(crate) fn validate_events(
    events: &[ProjectionEvent],
    bounds: ProjectionBounds,
) -> Result<(), ProjectionError> {
    if events.len() > bounds.max_events {
        return Err(ProjectionError::new(format!(
            "projection has {} events, exceeding the configured bound of {}",
            events.len(),
            bounds.max_events
        )));
    }
    if events
        .windows(2)
        .any(|pair| pair[1].time() < pair[0].time())
    {
        return Err(ProjectionError::new(
            "projection events must be ordered by nondecreasing time",
        ));
    }
    Ok(())
}

fn validate_pair(label: &str, source: EntityId, target: EntityId) -> Result<(), ProjectionError> {
    if source == target {
        Err(ProjectionError::new(format!(
            "{label} requires distinct participants"
        )))
    } else {
        Ok(())
    }
}

fn validate_interval(
    label: &str,
    start: WorldTime,
    end: Option<WorldTime>,
) -> Result<(), ProjectionError> {
    if end.is_some_and(|end| end <= start) {
        Err(ProjectionError::new(format!(
            "{label} interval end must be after its start"
        )))
    } else {
        Ok(())
    }
}

fn validate_provenance(label: &str, provenance: &str) -> Result<(), ProjectionError> {
    if provenance.trim().is_empty() {
        Err(ProjectionError::new(format!(
            "{label} provenance must not be empty"
        )))
    } else {
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct RelationEdge {
    source: EntityId,
    target: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    provenance: String,
}

fn relation_edges(events: &[ProjectionEvent], wanted: ProjectionRelationKind) -> Vec<RelationEdge> {
    events
        .iter()
        .filter_map(|event| match event {
            ProjectionEvent::Relation {
                kind,
                source,
                target,
                start,
                end,
                provenance,
                ..
            } if *kind == wanted => Some(RelationEdge {
                source: *source,
                target: *target,
                start: *start,
                end: *end,
                provenance: provenance.clone(),
            }),
            _ => None,
        })
        .collect()
}

fn kinship_from_edge(kind: KinshipKind, edge: &RelationEdge) -> KinshipRelation {
    KinshipRelation {
        kind,
        subject: edge.source,
        relative: edge.target,
        start: edge.start,
        end: edge.end,
        provenance: vec![edge.provenance.clone()],
    }
}

fn reverse_kinship_from_edge(kind: KinshipKind, edge: &RelationEdge) -> KinshipRelation {
    KinshipRelation {
        kind,
        subject: edge.target,
        relative: edge.source,
        start: edge.start,
        end: edge.end,
        provenance: vec![edge.provenance.clone()],
    }
}

pub(crate) fn intersect_intervals(
    first_start: WorldTime,
    first_end: Option<WorldTime>,
    second_start: WorldTime,
    second_end: Option<WorldTime>,
) -> Option<(WorldTime, Option<WorldTime>)> {
    let start = first_start.max(second_start);
    let end = match (first_end, second_end) {
        (Some(first), Some(second)) => Some(first.min(second)),
        (Some(end), None) | (None, Some(end)) => Some(end),
        (None, None) => None,
    };
    if end.is_some_and(|end| end <= start) {
        None
    } else {
        Some((start, end))
    }
}

fn validate_descent_depth(
    descent: &[RelationEdge],
    max_depth: usize,
) -> Result<(), ProjectionError> {
    let mut sources = Vec::new();
    for source in descent.iter().map(|edge| edge.source) {
        if !sources.contains(&source) {
            sources.push(source);
        }
    }
    for source in sources {
        let mut stack = vec![(source, vec![source])];
        while let Some((node, path)) = stack.pop() {
            for edge in descent.iter().filter(|edge| edge.source == node) {
                if path.contains(&edge.target) {
                    return Err(ProjectionError::new("descent graph contains a cycle"));
                }
                let depth = path.len();
                if depth > max_depth {
                    return Err(ProjectionError::new(format!(
                        "descent traversal exceeds the configured depth bound of {max_depth}"
                    )));
                }
                let mut next_path = path.clone();
                next_path.push(edge.target);
                stack.push((edge.target, next_path));
            }
        }
    }
    Ok(())
}

fn collect_descent_paths<'a>(
    origin: EntityId,
    node: EntityId,
    descent: &'a [RelationEdge],
    path: &mut Vec<&'a RelationEdge>,
    paths: &mut Vec<Vec<&'a RelationEdge>>,
) -> Result<(), ProjectionError> {
    for edge in descent.iter().filter(|edge| edge.source == node) {
        if edge.target == origin || path.iter().any(|prior| prior.source == edge.target) {
            return Err(ProjectionError::new("descent graph contains a cycle"));
        }
        path.push(edge);
        if path_interval(path).is_some() {
            paths.push(path.clone());
            collect_descent_paths(origin, edge.target, descent, path, paths)?;
        }
        path.pop();
    }
    Ok(())
}

fn path_interval(path: &[&RelationEdge]) -> Option<(WorldTime, Option<WorldTime>)> {
    let first = path.first()?;
    path.iter()
        .skip(1)
        .try_fold((first.start, first.end), |interval, edge| {
            intersect_intervals(interval.0, interval.1, edge.start, edge.end)
        })
}

fn first_descent_path(
    source: EntityId,
    target: EntityId,
    descent: &[RelationEdge],
) -> Result<Option<Vec<&RelationEdge>>, ProjectionError> {
    let mut paths = Vec::new();
    collect_descent_paths(source, source, descent, &mut Vec::new(), &mut paths)?;
    Ok(paths
        .into_iter()
        .find(|path| path.last().is_some_and(|edge| edge.target == target)))
}
