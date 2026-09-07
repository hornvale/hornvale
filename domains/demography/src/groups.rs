//! Pure group projections under caller-supplied social context.

use crate::kinship::{
    ProjectionBounds, ProjectionError, ProjectionEvent, ProjectionRelationKind,
    intersect_intervals, validate_events,
};
use hornvale_kernel::{EntityId, WorldTime};

/// One external-context rule; labels and matching forms are caller-authored.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ContextRule {
    pub(crate) recognized_label: Option<String>,
    pub(crate) association_forms: Vec<String>,
}

impl ContextRule {
    /// Ignore this context axis.
    pub fn ignored() -> Self {
        Self::default()
    }

    /// Recognize matching projections with a non-empty caller-authored label.
    /// type-audit: bare-ok(identifier-text: label), bare-ok(identifier-text: association_forms)
    pub fn recognized(
        label: &str,
        association_forms: Vec<String>,
    ) -> Result<Self, ProjectionError> {
        if label.trim().is_empty() {
            return Err(ProjectionError::new("context label must not be empty"));
        }
        if association_forms.iter().any(|form| form.trim().is_empty()) {
            return Err(ProjectionError::new(
                "context association forms must not contain empty values",
            ));
        }
        Ok(Self {
            recognized_label: Some(label.to_string()),
            association_forms,
        })
    }
}

/// Material and institutional context supplied from outside group structure.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SocialContext {
    /// Subsistence interpretation.
    pub subsistence: ContextRule,
    /// Property and inheritance interpretation.
    pub property: ContextRule,
    /// Mobility interpretation.
    pub mobility: ContextRule,
    /// Authority and institutional interpretation.
    pub authority: ContextRule,
    /// Ritual interpretation.
    pub religion: ContextRule,
    /// Contact and association interpretation.
    pub contact: ContextRule,
    /// Population-pressure and care interpretation.
    pub population_pressure: ContextRule,
}

impl SocialContext {
    /// Context with no recognition labels or association-form mappings.
    pub fn uninterpreted() -> Self {
        Self::default()
    }
}

/// Stable identity of a projected group without a universal household type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum GroupKey {
    /// Group named by realized membership events.
    Declared(EntityId),
    /// Group formed by one directed association event.
    Association {
        /// Association source.
        source: EntityId,
        /// Association target.
        target: EntityId,
        /// Association start.
        start: WorldTime,
    },
}

/// Evidence basis for one group projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum GroupBasis {
    /// Explicit group membership.
    Membership,
    /// Co-residence or residential affiliation.
    Residence,
    /// Shared care topology.
    Care,
    /// Shared subsistence recognized by external context.
    Subsistence,
    /// Shared property recognized by external context.
    Property,
    /// Ritual association recognized by external context.
    Ritual,
    /// Institutional recognition.
    InstitutionalRecognition,
    /// A non-domestic association.
    Association,
}

/// One member interval preserved inside a projected group.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupMember {
    person: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
    provenance: String,
}

impl GroupMember {
    /// Member person.
    pub fn person(&self) -> EntityId {
        self.person
    }
    /// Inclusive membership start.
    pub fn start(&self) -> WorldTime {
        self.start
    }
    /// Exclusive membership end.
    pub fn end(&self) -> Option<WorldTime> {
        self.end
    }
    /// Membership-event provenance.
    /// type-audit: bare-ok(prose: return)
    pub fn provenance(&self) -> &str {
        &self.provenance
    }
}

/// Time-bounded group reading with declared bases and source evidence.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupProjection {
    key: GroupKey,
    members: Vec<GroupMember>,
    bases: Vec<GroupBasis>,
    start: WorldTime,
    end: Option<WorldTime>,
    recognized_labels: Vec<String>,
    provenance: Vec<String>,
}

impl GroupProjection {
    /// Stable projection key.
    pub fn key(&self) -> GroupKey {
        self.key
    }
    /// Ordered member intervals.
    pub fn members(&self) -> &[GroupMember] {
        &self.members
    }
    /// Ordered declared bases.
    pub fn bases(&self) -> &[GroupBasis] {
        &self.bases
    }
    /// Earliest supporting start.
    pub fn start(&self) -> WorldTime {
        self.start
    }
    /// Exclusive group end, when closed.
    pub fn end(&self) -> Option<WorldTime> {
        self.end
    }
    /// Ordered caller-authored recognition labels.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn recognized_labels(&self) -> &[String] {
        &self.recognized_labels
    }
    /// Ordered source-event provenance.
    /// type-audit: bare-ok(prose: return)
    pub fn provenance(&self) -> &[String] {
        &self.provenance
    }
}

/// Derive overlapping, time-bounded groups from realized observations.
pub fn derive_groups(
    events: &[ProjectionEvent],
    context: &SocialContext,
    bounds: ProjectionBounds,
) -> Result<Vec<GroupProjection>, ProjectionError> {
    validate_events(events, bounds)?;
    let mut groups = declared_groups(events);
    groups.extend(association_groups(events, context));

    for group in &mut groups {
        add_group_evidence(group, events, context);
        group
            .members
            .sort_by_key(|member| (member.person, member.start));
        group.bases.sort();
        group.bases.dedup();
        group.recognized_labels.dedup();
    }
    groups.sort_by_key(|group| group.key);
    Ok(groups)
}

fn declared_groups(events: &[ProjectionEvent]) -> Vec<GroupProjection> {
    let mut keys = Vec::new();
    for event in events {
        if let ProjectionEvent::Membership { group, .. } = event {
            push_unique(&mut keys, *group);
        }
    }
    keys.sort();

    keys.into_iter()
        .filter_map(|group| {
            let mut members = Vec::new();
            for event in events {
                if let ProjectionEvent::Membership {
                    member,
                    group: event_group,
                    start,
                    end,
                    provenance: source,
                } = event
                    && *event_group == group
                {
                    members.push(GroupMember {
                        person: *member,
                        start: *start,
                        end: *end,
                        provenance: source.clone(),
                    });
                }
            }
            let start = members.iter().map(|member| member.start).min()?;
            let dissolution = events.iter().find_map(|event| match event {
                ProjectionEvent::Dissolution {
                    group: dissolved,
                    at,
                    provenance,
                } if *dissolved == group => Some((*at, provenance.clone())),
                _ => None,
            });
            if let Some((at, _)) = &dissolution {
                for member in &mut members {
                    if member.end.is_none_or(|end| *at < end) {
                        member.end = Some(*at);
                    }
                }
            }
            let end = dissolution.map(|(at, _)| at).or_else(|| {
                if members.iter().all(|member| member.end.is_some()) {
                    members.iter().filter_map(|member| member.end).max()
                } else {
                    None
                }
            });
            Some(GroupProjection {
                key: GroupKey::Declared(group),
                members,
                bases: vec![GroupBasis::Membership],
                start,
                end,
                recognized_labels: Vec::new(),
                provenance: Vec::new(),
            })
        })
        .collect()
}

fn association_groups(events: &[ProjectionEvent], context: &SocialContext) -> Vec<GroupProjection> {
    let mut groups = Vec::new();
    for event in events {
        match event {
            ProjectionEvent::Relation {
                kind: ProjectionRelationKind::Association,
                source,
                target,
                start,
                end,
                detail,
                provenance,
            } => {
                let mut group = GroupProjection {
                    key: GroupKey::Association {
                        source: *source,
                        target: *target,
                        start: *start,
                    },
                    members: vec![
                        GroupMember {
                            person: *source,
                            start: *start,
                            end: *end,
                            provenance: provenance.clone(),
                        },
                        GroupMember {
                            person: *target,
                            start: *start,
                            end: *end,
                            provenance: provenance.clone(),
                        },
                    ],
                    bases: vec![GroupBasis::Association],
                    start: *start,
                    end: *end,
                    recognized_labels: Vec::new(),
                    provenance: vec![provenance.clone()],
                };
                if let Some(form) = detail.as_deref() {
                    apply_form_context(&mut group, form, context);
                }
                groups.push(group);
            }
            ProjectionEvent::Separation {
                source,
                target,
                at,
                provenance,
            } => {
                if let Some(group) = groups.iter_mut().rev().find(|group| {
                    matches!(
                        group.key,
                        GroupKey::Association {
                            source: group_source,
                            target: group_target,
                            start,
                        } if group_source == *source
                            && group_target == *target
                            && start < *at
                            && group.end.is_none_or(|end| *at < end)
                    )
                }) {
                    group.end = Some(*at);
                    for member in &mut group.members {
                        member.end = Some(*at);
                    }
                    group.provenance.push(provenance.clone());
                }
            }
            _ => {}
        }
    }
    groups
}

fn add_group_evidence(
    group: &mut GroupProjection,
    events: &[ProjectionEvent],
    context: &SocialContext,
) {
    let GroupKey::Declared(group_id) = group.key else {
        return;
    };
    for event in events {
        match event {
            ProjectionEvent::Membership {
                group: event_group,
                provenance,
                ..
            } if *event_group == group_id => group.provenance.push(provenance.clone()),
            ProjectionEvent::Dissolution {
                group: dissolved,
                provenance,
                ..
            } if *dissolved == group_id => group.provenance.push(provenance.clone()),
            _ => {}
        }
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
        match kind {
            ProjectionRelationKind::Residence
                if *target == group_id && member_active(group, *source, *start, *end) =>
            {
                add_basis(group, GroupBasis::Residence);
                group.provenance.push(provenance.clone());
            }
            ProjectionRelationKind::Care
                if members_share_relation(group, *source, *target, *start, *end) =>
            {
                add_basis(group, GroupBasis::Care);
                group.provenance.push(provenance.clone());
            }
            ProjectionRelationKind::Transfer if *source == group_id || *target == group_id => {
                add_basis(group, GroupBasis::Property);
                group.provenance.push(provenance.clone());
            }
            ProjectionRelationKind::Recognition
                if *target == group_id
                    || group.members.iter().any(|member| member.person == *target) =>
            {
                add_basis(group, GroupBasis::InstitutionalRecognition);
                group.provenance.push(provenance.clone());
                if let Some(label) = detail {
                    push_unique(&mut group.recognized_labels, label.clone());
                }
            }
            ProjectionRelationKind::Association => {
                if let Some(form) = detail.as_deref()
                    && group.members.iter().any(|member| member.person == *source)
                    && group.members.iter().any(|member| member.person == *target)
                {
                    apply_form_context(group, form, context);
                    group.provenance.push(provenance.clone());
                }
            }
            _ => {}
        }
    }

    if group.bases.contains(&GroupBasis::Residence) {
        apply_basis_label(group, &context.mobility);
    }
    if group.bases.contains(&GroupBasis::Care) {
        apply_basis_label(group, &context.population_pressure);
    }
    if group.bases.contains(&GroupBasis::Property) {
        apply_basis_label(group, &context.property);
    }
    if group.bases.contains(&GroupBasis::InstitutionalRecognition) {
        apply_basis_label(group, &context.authority);
    }
}

fn apply_form_context(group: &mut GroupProjection, form: &str, context: &SocialContext) {
    for (rule, basis) in [
        (&context.subsistence, GroupBasis::Subsistence),
        (&context.property, GroupBasis::Property),
        (&context.mobility, GroupBasis::Residence),
        (&context.authority, GroupBasis::InstitutionalRecognition),
        (&context.religion, GroupBasis::Ritual),
        (&context.contact, GroupBasis::Association),
        (&context.population_pressure, GroupBasis::Care),
    ] {
        if rule
            .association_forms
            .iter()
            .any(|candidate| candidate == form)
        {
            add_basis(group, basis);
            apply_basis_label(group, rule);
        }
    }
}

fn apply_basis_label(group: &mut GroupProjection, rule: &ContextRule) {
    if let Some(label) = &rule.recognized_label {
        push_unique(&mut group.recognized_labels, label.clone());
    }
}

fn add_basis(group: &mut GroupProjection, basis: GroupBasis) {
    push_unique(&mut group.bases, basis);
}

fn member_active(
    group: &GroupProjection,
    person: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
) -> bool {
    group.members.iter().any(|member| {
        member.person == person
            && intersect_intervals(member.start, member.end, start, end).is_some()
    })
}

fn members_share_relation(
    group: &GroupProjection,
    source: EntityId,
    target: EntityId,
    start: WorldTime,
    end: Option<WorldTime>,
) -> bool {
    group.members.iter().any(|first| {
        first.person == source
            && group.members.iter().any(|second| {
                second.person == target
                    && intersect_intervals(first.start, first.end, second.start, second.end)
                        .and_then(|shared| intersect_intervals(shared.0, shared.1, start, end))
                        .is_some()
            })
    })
}

fn push_unique<T: PartialEq>(values: &mut Vec<T>, value: T) {
    if !values.contains(&value) {
        values.push(value);
    }
}
