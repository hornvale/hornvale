//! Semantic-light, deterministic relation assertions for worldgen read views.

use hornvale_kernel::WorldTime;
use std::cmp::Ordering;
use std::collections::BTreeSet;

/// An opaque endpoint identifier, suitable for a locus or aggregate cohort.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RelationReference(String);

impl RelationReference {
    /// Create a reference without assigning meaning to its identifier.
    pub fn new(reference: impl Into<String>) -> Self {
        Self(reference.into())
    }
    /// Return the producer-owned identifier.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A producer-owned role in a relation assertion.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RelationRole(String);

impl RelationRole {
    /// Create an opaque role label.
    pub fn new(role: impl Into<String>) -> Self {
        Self(role.into())
    }
    /// Return the producer-owned role label.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A closed interval on the world's exact time axis.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RelationInterval {
    /// Inclusive interval start.
    pub start: WorldTime,
    /// Inclusive interval end.
    pub end: WorldTime,
}

/// Recurrence metadata carried by an assertion.
/// type-audit: bare-ok(count)
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RelationRecurrence {
    /// The assertion is supported by one interval.
    Once,
    /// The assertion repeats at the given positive number of world ticks.
    Periodic {
        /// Number of ticks between occurrences.
        /// type-audit: bare-ok(count: period_ticks)
        period_ticks: i64,
    },
}

/// Whether participant order carries direction.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RelationDirection {
    /// Participant order is not semantically directional.
    Symmetric,
    /// The participant sequence is source-to-destination.
    Directed,
    /// Both directions are asserted as one reciprocal relation.
    Reciprocal,
}

/// Connectivity interpretation applied within one relation basis.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RelationDirectionPolicy {
    /// Admit only assertions explicitly declared symmetric.
    Symmetric,
    /// Treat every admitted assertion as connectivity evidence regardless of direction.
    WeaklyConnected,
    /// Admit symmetric or reciprocal evidence and directed evidence whose endpoints are mutually
    /// reachable through the complete basis-local graph.
    StronglyConnected,
    /// Admit only evidence traversable outward from this source, including transitive reach.
    SourceReachable(RelationReference),
    /// Admit only assertions explicitly declared reciprocal.
    Reciprocal,
}

/// A finite, basis-local scalar measure.
/// type-audit: bare-ok(diagnostic-value)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RelationMeasure {
    /// The producer-owned scalar value.
    /// type-audit: bare-ok(diagnostic-value: value)
    pub value: f64,
}

impl RelationMeasure {
    /// Construct a measure. Validation is performed by the assertion envelope.
    /// type-audit: bare-ok(diagnostic-value: value)
    pub const fn new(value: f64) -> Self {
        Self { value }
    }
}

/// A scalar filter applied only after assertions have been isolated to one basis.
/// type-audit: bare-ok(diagnostic-value)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RelationMeasureFilter {
    minimum: Option<f64>,
    maximum: Option<f64>,
}

impl RelationMeasureFilter {
    /// Admit every finite measure in the selected basis.
    pub const fn all() -> Self {
        Self {
            minimum: None,
            maximum: None,
        }
    }

    /// Admit measures greater than or equal to `minimum`.
    /// type-audit: bare-ok(diagnostic-value: minimum)
    pub const fn at_least(minimum: f64) -> Self {
        Self {
            minimum: Some(minimum),
            maximum: None,
        }
    }

    /// Admit measures less than or equal to `maximum`.
    /// type-audit: bare-ok(diagnostic-value: maximum)
    pub const fn at_most(maximum: f64) -> Self {
        Self {
            minimum: None,
            maximum: Some(maximum),
        }
    }

    /// Admit measures inside the inclusive interval.
    /// type-audit: bare-ok(diagnostic-value: minimum), bare-ok(diagnostic-value: maximum)
    pub const fn between(minimum: f64, maximum: f64) -> Self {
        Self {
            minimum: Some(minimum),
            maximum: Some(maximum),
        }
    }

    fn admits(self, measure: RelationMeasure) -> bool {
        self.minimum.is_none_or(|minimum| measure.value >= minimum)
            && self.maximum.is_none_or(|maximum| measure.value <= maximum)
    }
}

/// Provenance supplied by the producer of an assertion.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RelationProvenance {
    /// An opaque producer or evidence label.
    /// type-audit: bare-ok(identifier-text: source)
    pub source: String,
}

impl RelationProvenance {
    /// Create provenance from an opaque source label.
    /// type-audit: bare-ok(identifier-text: source)
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
        }
    }
}

/// Relation kinds currently admitted by the R3 envelope.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RelationKind {
    /// Two loci are adjacent in space.
    SpatialAdjacency,
    /// An aggregate cohort is present at a locus.
    Presence,
    /// A symmetric, directed, or reciprocal route is available.
    Access,
    /// A directed or reciprocal exchange is observed.
    Exchange,
}

/// A relation basis interpreted by R3.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RelationBasis {
    /// Physical neighborhood or connected extent.
    Spatial,
    /// Aggregate or realized participation at a locus.
    Presence,
    /// Directed or undirected reachability.
    Access,
    /// Repeated or directed flow.
    Exchange,
}

impl RelationBasis {
    const fn kind(self) -> RelationKind {
        match self {
            Self::Spatial => RelationKind::SpatialAdjacency,
            Self::Presence => RelationKind::Presence,
            Self::Access => RelationKind::Access,
            Self::Exchange => RelationKind::Exchange,
        }
    }
}

/// A role-bearing endpoint of an assertion.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RelationParticipant {
    /// The opaque endpoint reference.
    pub reference: RelationReference,
    /// The producer-owned role of this endpoint.
    pub role: RelationRole,
}

/// One producer-owned semantic relation assertion.
#[derive(Clone, Debug, PartialEq)]
pub struct RelationAssertion {
    /// The supported relation kind.
    pub kind: RelationKind,
    /// Ordered participants; order is meaningful for directed assertions.
    pub participants: Vec<RelationParticipant>,
    /// The assertion's validity interval.
    pub interval: RelationInterval,
    /// Recurrence metadata.
    pub recurrence: RelationRecurrence,
    /// Direction policy for participant order.
    pub direction: RelationDirection,
    /// A finite, basis-local measure.
    pub measure: RelationMeasure,
    /// Producer/evidence provenance.
    pub provenance: RelationProvenance,
}

/// Validation failures for relation assertions and binary read views.
/// type-audit: bare-ok(count)
#[derive(Clone, Debug, PartialEq)]
pub enum RelationError {
    /// An assertion has fewer than two participants.
    TooFewParticipants {
        /// Number of participants supplied.
        /// type-audit: bare-ok(count: count)
        count: usize,
    },
    /// A view accepts only binary assertions and never lowers higher arity.
    UnsupportedParticipantCount {
        /// Kind whose arity was rejected.
        kind: RelationKind,
        /// Number of participants supplied.
        /// type-audit: bare-ok(count: count)
        count: usize,
        /// The producer assertion that the binary view refused to lower.
        assertion: RelationAssertion,
    },
    /// The interval end precedes its start.
    ReversedInterval {
        /// Supplied start.
        start: WorldTime,
        /// Supplied end.
        end: WorldTime,
    },
    /// A recurrence period is zero or negative.
    InvalidRecurrence,
    /// A scalar measure is NaN or infinite.
    NonFiniteMeasure,
    /// The direction policy is not meaningful for this kind.
    UnsupportedDirection {
        /// Kind whose direction was rejected.
        kind: RelationKind,
        /// Supplied direction.
        direction: RelationDirection,
    },
}

/// Why a basis-specific view refused a source assertion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RelationRefusalReason {
    /// The assertion belongs to a different relation basis.
    UnsupportedKind {
        /// Basis requested by the consumer.
        basis: RelationBasis,
        /// Producer-owned kind that was not interpreted as that basis.
        kind: RelationKind,
    },
}

/// A refused assertion together with explicit, deterministic metadata.
#[derive(Clone, Debug, PartialEq)]
pub struct RelationRefusal {
    /// The original producer assertion, including provenance.
    pub assertion: RelationAssertion,
    /// The reason the basis view did not interpret it.
    pub reason: RelationRefusalReason,
}

impl RelationAssertion {
    /// Validate shape, temporal bounds, recurrence, measure, and direction.
    pub fn validate(&self) -> Result<(), RelationError> {
        if self.participants.len() < 2 {
            return Err(RelationError::TooFewParticipants {
                count: self.participants.len(),
            });
        }
        if self.interval.start > self.interval.end {
            return Err(RelationError::ReversedInterval {
                start: self.interval.start,
                end: self.interval.end,
            });
        }
        if let RelationRecurrence::Periodic { period_ticks } = self.recurrence
            && period_ticks <= 0
        {
            return Err(RelationError::InvalidRecurrence);
        }
        if !self.measure.value.is_finite() {
            return Err(RelationError::NonFiniteMeasure);
        }
        let direction_allowed = match self.kind {
            RelationKind::SpatialAdjacency | RelationKind::Presence => {
                self.direction == RelationDirection::Symmetric
            }
            RelationKind::Access => matches!(
                self.direction,
                RelationDirection::Symmetric
                    | RelationDirection::Directed
                    | RelationDirection::Reciprocal
            ),
            RelationKind::Exchange => matches!(
                self.direction,
                RelationDirection::Directed | RelationDirection::Reciprocal
            ),
        };
        if !direction_allowed {
            return Err(RelationError::UnsupportedDirection {
                kind: self.kind,
                direction: self.direction,
            });
        }
        Ok(())
    }
}

/// A deterministic binary relation read view.
#[derive(Clone, Debug, PartialEq)]
pub struct RelationView {
    assertions: Vec<RelationAssertion>,
}

impl RelationView {
    /// Validate assertions and order them by stable semantic fields.
    pub fn new(assertions: Vec<RelationAssertion>) -> Result<Self, RelationError> {
        for assertion in &assertions {
            assertion.validate()?;
            if assertion.participants.len() != 2 {
                return Err(RelationError::UnsupportedParticipantCount {
                    kind: assertion.kind,
                    count: assertion.participants.len(),
                    assertion: assertion.clone(),
                });
            }
        }
        let mut view = Self { assertions };
        for assertion in &mut view.assertions {
            if assertion.direction == RelationDirection::Symmetric {
                assertion.participants.sort();
            }
        }
        view.assertions.sort_by(relation_order);
        Ok(view)
    }
    /// Iterate in deterministic order.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &RelationAssertion> {
        self.assertions.iter()
    }
    /// Return the number of assertions in the view.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.assertions.len()
    }
    /// Return whether the view contains no assertions.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.assertions.is_empty()
    }

    /// Derive the symmetric spatial-adjacency view.
    pub fn spatial(&self, measure: RelationMeasureFilter) -> RelationBasisView {
        self.basis_view(
            RelationBasis::Spatial,
            RelationDirectionPolicy::Symmetric,
            measure,
        )
    }

    /// Derive the symmetric presence view.
    pub fn presence(&self, measure: RelationMeasureFilter) -> RelationBasisView {
        self.basis_view(
            RelationBasis::Presence,
            RelationDirectionPolicy::Symmetric,
            measure,
        )
    }

    /// Derive an access view under an explicit direction policy.
    pub fn access(
        &self,
        policy: RelationDirectionPolicy,
        measure: RelationMeasureFilter,
    ) -> RelationBasisView {
        self.basis_view(RelationBasis::Access, policy, measure)
    }

    /// Derive an exchange view under an explicit direction policy.
    pub fn exchange(
        &self,
        policy: RelationDirectionPolicy,
        measure: RelationMeasureFilter,
    ) -> RelationBasisView {
        self.basis_view(RelationBasis::Exchange, policy, measure)
    }

    fn basis_view(
        &self,
        basis: RelationBasis,
        policy: RelationDirectionPolicy,
        measure: RelationMeasureFilter,
    ) -> RelationBasisView {
        let mut assertions = Vec::new();
        let mut refusals = Vec::new();
        for assertion in &self.assertions {
            if assertion.kind != basis.kind() {
                refusals.push(RelationRefusal {
                    assertion: assertion.clone(),
                    reason: RelationRefusalReason::UnsupportedKind {
                        basis,
                        kind: assertion.kind,
                    },
                });
            } else if measure.admits(assertion.measure) {
                assertions.push(assertion.clone());
            }
        }
        assertions = apply_direction_policy(assertions, &policy);
        RelationBasisView {
            basis,
            policy,
            assertions,
            refusals,
        }
    }
}

/// A deterministic, single-basis relation view with source evidence intact.
#[derive(Clone, Debug, PartialEq)]
pub struct RelationBasisView {
    basis: RelationBasis,
    policy: RelationDirectionPolicy,
    assertions: Vec<RelationAssertion>,
    refusals: Vec<RelationRefusal>,
}

impl RelationBasisView {
    /// Return the selected basis.
    pub const fn basis(&self) -> RelationBasis {
        self.basis
    }

    /// Return the explicit direction policy used to derive this view.
    pub fn policy(&self) -> &RelationDirectionPolicy {
        &self.policy
    }

    /// Iterate accepted assertions in deterministic source-view order.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &RelationAssertion> {
        self.assertions.iter()
    }

    /// Return explicit refusals in deterministic source-view order.
    pub fn refusals(&self) -> &[RelationRefusal] {
        &self.refusals
    }

    /// Return the number of accepted assertions.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.assertions.len()
    }

    /// Return whether no assertions were accepted.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.assertions.is_empty()
    }
}

fn apply_direction_policy(
    assertions: Vec<RelationAssertion>,
    policy: &RelationDirectionPolicy,
) -> Vec<RelationAssertion> {
    match policy {
        RelationDirectionPolicy::Symmetric => assertions
            .into_iter()
            .filter(|assertion| assertion.direction == RelationDirection::Symmetric)
            .collect(),
        RelationDirectionPolicy::WeaklyConnected => assertions,
        RelationDirectionPolicy::StronglyConnected => assertions
            .iter()
            .filter(|assertion| strongly_supported(assertion, &assertions))
            .cloned()
            .collect(),
        RelationDirectionPolicy::SourceReachable(source) => source_reachable(assertions, source),
        RelationDirectionPolicy::Reciprocal => assertions
            .into_iter()
            .filter(|assertion| assertion.direction == RelationDirection::Reciprocal)
            .collect(),
    }
}

fn strongly_supported(assertion: &RelationAssertion, assertions: &[RelationAssertion]) -> bool {
    match assertion.direction {
        RelationDirection::Symmetric | RelationDirection::Reciprocal => true,
        RelationDirection::Directed => {
            let from = &assertion.participants[0].reference;
            let to = &assertion.participants[1].reference;
            directed_path_exists(from, to, assertions) && directed_path_exists(to, from, assertions)
        }
    }
}

fn directed_path_exists(
    source: &RelationReference,
    target: &RelationReference,
    assertions: &[RelationAssertion],
) -> bool {
    let mut reachable = BTreeSet::from([source.clone()]);
    loop {
        let previous_count = reachable.len();
        for assertion in assertions {
            let from = &assertion.participants[0].reference;
            let to = &assertion.participants[1].reference;
            if reachable.contains(from) {
                reachable.insert(to.clone());
            }
            if assertion.direction != RelationDirection::Directed && reachable.contains(to) {
                reachable.insert(from.clone());
            }
        }
        if reachable.contains(target) {
            return true;
        }
        if reachable.len() == previous_count {
            return false;
        }
    }
}

fn source_reachable(
    assertions: Vec<RelationAssertion>,
    source: &RelationReference,
) -> Vec<RelationAssertion> {
    let mut reachable = BTreeSet::from([source.clone()]);
    let mut selected = BTreeSet::new();
    loop {
        let previous_count = selected.len();
        for (index, assertion) in assertions.iter().enumerate() {
            let from = &assertion.participants[0].reference;
            let to = &assertion.participants[1].reference;
            let traversable = reachable.contains(from)
                || (assertion.direction != RelationDirection::Directed && reachable.contains(to));
            if traversable {
                selected.insert(index);
                reachable.insert(from.clone());
                reachable.insert(to.clone());
            }
        }
        if selected.len() == previous_count {
            break;
        }
    }
    selected
        .into_iter()
        .map(|index| assertions[index].clone())
        .collect()
}

fn relation_order(left: &RelationAssertion, right: &RelationAssertion) -> Ordering {
    left.kind
        .cmp(&right.kind)
        .then_with(|| participant_order(left).cmp(&participant_order(right)))
        .then_with(|| left.interval.start.cmp(&right.interval.start))
        .then_with(|| left.interval.end.cmp(&right.interval.end))
        .then_with(|| left.recurrence.cmp(&right.recurrence))
        .then_with(|| left.direction.cmp(&right.direction))
        .then_with(|| left.measure.value.total_cmp(&right.measure.value))
        .then_with(|| left.provenance.cmp(&right.provenance))
}

fn participant_order(assertion: &RelationAssertion) -> Vec<(&RelationReference, &RelationRole)> {
    assertion
        .participants
        .iter()
        .map(|p| (&p.reference, &p.role))
        .collect()
}
