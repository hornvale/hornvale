//! Semantic-light, deterministic relation assertions for worldgen read views.

use hornvale_kernel::WorldTime;
use std::cmp::Ordering;

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
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
    /// A directed or reciprocal route is available.
    Access,
    /// A directed or reciprocal exchange is observed.
    Exchange,
}

/// A role-bearing endpoint of an assertion.
#[derive(Clone, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
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
            RelationKind::Access | RelationKind::Exchange => matches!(
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
                });
            }
        }
        let mut view = Self { assertions };
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
}

fn relation_order(left: &RelationAssertion, right: &RelationAssertion) -> Ordering {
    left.kind
        .cmp(&right.kind)
        .then_with(|| participant_order(left).cmp(&participant_order(right)))
        .then_with(|| left.interval.start.cmp(&right.interval.start))
        .then_with(|| left.interval.end.cmp(&right.interval.end))
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
