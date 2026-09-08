//! Pure, deterministic district projection over typed relation views.

use crate::relation::{
    RelationAssertion, RelationBasisView, RelationDirection, RelationDirectionPolicy,
    RelationMeasureFilter, RelationRecurrence, RelationReference, RelationRefusal, RelationView,
};
use hornvale_kernel::WorldTime;
use std::collections::{BTreeMap, BTreeSet};

/// A closed interval on the world's exact time axis requested by a projection.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct DistrictInterval {
    /// Inclusive interval start.
    pub start: WorldTime,
    /// Inclusive interval end.
    pub end: WorldTime,
}

/// The relation basis from which a district is projected.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum DistrictBasis {
    /// Physical neighborhood or connected extent.
    Spatial,
    /// Aggregate or realized participation at a locus.
    Presence,
    /// Directed or undirected reachability.
    Access,
    /// Repeated or directed flow.
    Exchange,
}

/// Explicit support, direction, persistence, and structural bounds.
/// type-audit: bare-ok(count)
#[derive(Clone, Debug, PartialEq)]
pub struct DistrictConfig {
    /// Direction semantics passed to access and exchange relation views.
    pub direction: RelationDirectionPolicy,
    /// Basis-local support threshold applied before graph formation.
    pub measure: RelationMeasureFilter,
    /// Smallest supported member set; two refuses singletons by default.
    /// type-audit: bare-ok(count: minimum_members)
    pub minimum_members: usize,
    /// Smallest number of source assertions supporting one district.
    /// type-audit: bare-ok(count: minimum_evidence)
    pub minimum_evidence: usize,
    /// Smallest common evidence duration, in exact world ticks.
    /// type-audit: bare-ok(count: minimum_duration_ticks)
    pub minimum_duration_ticks: i64,
    /// Maximum number of strict parent links from a root projection.
    /// type-audit: bare-ok(count: maximum_containment_depth)
    pub maximum_containment_depth: usize,
    /// Maximum partial-overlap links retained for any one district.
    /// type-audit: bare-ok(count: maximum_overlaps_per_district)
    pub maximum_overlaps_per_district: usize,
}

/// Structural result of a district projection.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum DistrictStatus {
    /// Sufficient evidence supports one or more districts.
    Resolved,
    /// Candidates exist but fail configured member or evidence support.
    InsufficientEvidence,
    /// Supported candidates cannot fit the configured structural bounds.
    ContradictoryEvidence,
    /// No qualifying relation component exists.
    Disconnected,
    /// Supported candidates exist only for a shorter-than-required interval.
    TransientOnly,
}

/// Projection-local identity derived only from basis, interval, and anchor.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct DistrictId {
    /// Relation basis used by the projection.
    pub basis: DistrictBasis,
    /// Exact interval used by the projection.
    pub interval: DistrictInterval,
    /// Canonical component or directed-reachability anchor.
    pub anchor: RelationReference,
}

/// A deterministic partial intersection between two projected districts.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct DistrictOverlap {
    /// First district in identity order.
    pub left: DistrictId,
    /// Second district in identity order.
    pub right: DistrictId,
    /// Members shared by both districts.
    pub members: BTreeSet<RelationReference>,
}

/// One evidence-backed district in a projection set.
#[derive(Clone, Debug, PartialEq)]
pub struct DistrictProjection {
    /// Projection-local identity.
    pub id: DistrictId,
    /// Opaque member references in deterministic order.
    pub members: BTreeSet<RelationReference>,
    /// Members with at most one structural neighbor inside this projection.
    pub boundary: BTreeSet<RelationReference>,
    /// Immediate strict-superset projection, when unambiguous and in bounds.
    pub parent: Option<DistrictId>,
    /// Partially intersecting projections in deterministic identity order.
    pub overlaps: BTreeSet<DistrictId>,
    /// Articulation members whose removal disconnects this projection.
    pub bridge_members: BTreeSet<RelationReference>,
    /// Original assertions supporting this membership decision.
    pub evidence: Vec<RelationAssertion>,
    /// Structural and interval status for this projection.
    pub status: DistrictStatus,
}

/// Complete output of one pure district-projection request.
#[derive(Clone, Debug, PartialEq)]
pub struct DistrictProjectionSet {
    /// Requested basis.
    pub basis: DistrictBasis,
    /// Requested interval.
    pub interval: DistrictInterval,
    /// Overall result status, distinct even when `districts` is empty.
    pub status: DistrictStatus,
    /// Supported projections in deterministic identity order.
    pub districts: Vec<DistrictProjection>,
    /// Basis-view refusals with original assertion provenance intact.
    pub refusals: Vec<RelationRefusal>,
    /// Partial overlaps among this set's projections.
    pub overlaps: BTreeSet<DistrictOverlap>,
    overlap_limit: usize,
}

/// Explicit temporal relation between two district projection intervals.
/// type-audit: bare-ok(count)
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum DistrictContinuityState {
    /// Uninterrupted evidence supports one district in both intervals.
    EventContinuity {
        /// Projection-local identity in the earlier interval.
        previous: DistrictId,
        /// Projection-local identity in the later interval.
        current: DistrictId,
    },
    /// Periodic evidence supports corresponding districts in both intervals.
    Recurrence {
        /// Projection-local identity in the earlier interval.
        previous: DistrictId,
        /// Projection-local identity in the later interval.
        current: DistrictId,
        /// Exact recurrence period in world ticks.
        period_ticks: i64,
    },
    /// A supported projection exists for less than the configured duration.
    Transient {
        /// Projection-local identity of the short-lived district.
        district: DistrictId,
    },
    /// A previously supported district has no evidence-backed successor.
    Dissolved {
        /// Projection-local identity in the earlier interval.
        previous: DistrictId,
        /// Start of the interval in which no successor exists.
        at: WorldTime,
    },
    /// Evidence from one or more prior districts supports a changed set of successors.
    Recomposed {
        /// Earlier projection-local identities in deterministic order.
        previous: Vec<DistrictId>,
        /// Later projection-local identities in deterministic order.
        current: Vec<DistrictId>,
    },
}

/// Complete, deterministic temporal comparison of two projection sets.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DistrictContinuity {
    /// Interval supplying the earlier projections.
    pub previous_interval: DistrictInterval,
    /// Interval supplying the later projections.
    pub current_interval: DistrictInterval,
    /// Explicit continuity relations in deterministic order.
    pub states: Vec<DistrictContinuityState>,
}

/// Compare two projection sets without reading or mutating world state.
pub fn compare_districts(
    previous: &DistrictProjectionSet,
    current: &DistrictProjectionSet,
    config: &DistrictConfig,
) -> DistrictContinuity {
    let mut previous_districts: Vec<&DistrictProjection> = previous
        .districts
        .iter()
        .filter(|district| persistent_and_supported(district, config))
        .collect();
    let mut current_districts: Vec<&DistrictProjection> = current
        .districts
        .iter()
        .filter(|district| persistent_and_supported(district, config))
        .collect();
    previous_districts.sort_by(|left, right| left.id.cmp(&right.id));
    current_districts.sort_by(|left, right| left.id.cmp(&right.id));

    let links: Vec<BTreeSet<usize>> = previous_districts
        .iter()
        .map(|earlier| {
            current_districts
                .iter()
                .enumerate()
                .filter_map(|(index, later)| continuity_relation(earlier, later).map(|_| index))
                .collect()
        })
        .collect();
    let reverse_links: Vec<BTreeSet<usize>> = (0..current_districts.len())
        .map(|current_index| {
            links
                .iter()
                .enumerate()
                .filter_map(|(previous_index, successors)| {
                    successors
                        .contains(&current_index)
                        .then_some(previous_index)
                })
                .collect()
        })
        .collect();

    let mut states: Vec<DistrictContinuityState> = current
        .districts
        .iter()
        .filter(|district| district.status == DistrictStatus::TransientOnly)
        .map(|district| DistrictContinuityState::Transient {
            district: district.id.clone(),
        })
        .collect();
    let mut visited_previous = BTreeSet::new();

    for start in 0..previous_districts.len() {
        if links[start].is_empty() || visited_previous.contains(&start) {
            continue;
        }
        let mut component_previous = BTreeSet::from([start]);
        let mut component_current = BTreeSet::new();
        let mut previous_frontier = BTreeSet::from([start]);
        let mut current_frontier = BTreeSet::new();
        while !previous_frontier.is_empty() || !current_frontier.is_empty() {
            while let Some(previous_index) = previous_frontier.pop_first() {
                visited_previous.insert(previous_index);
                for &current_index in &links[previous_index] {
                    if component_current.insert(current_index) {
                        current_frontier.insert(current_index);
                    }
                }
            }
            while let Some(current_index) = current_frontier.pop_first() {
                for &previous_index in &reverse_links[current_index] {
                    if component_previous.insert(previous_index) {
                        previous_frontier.insert(previous_index);
                    }
                }
            }
        }

        if component_previous.len() == 1 && component_current.len() == 1 {
            let previous_index = *component_previous.first().expect("one previous district");
            let current_index = *component_current.first().expect("one current district");
            let earlier = previous_districts[previous_index];
            let later = current_districts[current_index];
            let relation = continuity_relation(earlier, later)
                .expect("component edges are evidence-backed continuity relations");
            states.push(match relation {
                PairContinuity::Event => DistrictContinuityState::EventContinuity {
                    previous: earlier.id.clone(),
                    current: later.id.clone(),
                },
                PairContinuity::Recurrence(period_ticks) => DistrictContinuityState::Recurrence {
                    previous: earlier.id.clone(),
                    current: later.id.clone(),
                    period_ticks,
                },
            });
        } else {
            states.push(DistrictContinuityState::Recomposed {
                previous: component_previous
                    .into_iter()
                    .map(|index| previous_districts[index].id.clone())
                    .collect(),
                current: component_current
                    .into_iter()
                    .map(|index| current_districts[index].id.clone())
                    .collect(),
            });
        }
    }

    for (index, district) in previous_districts.into_iter().enumerate() {
        if !visited_previous.contains(&index) {
            states.push(DistrictContinuityState::Dissolved {
                previous: district.id.clone(),
                at: current.interval.start,
            });
        }
    }
    states.sort();

    DistrictContinuity {
        previous_interval: previous.interval,
        current_interval: current.interval,
        states,
    }
}

fn persistent_and_supported(district: &DistrictProjection, config: &DistrictConfig) -> bool {
    district.status == DistrictStatus::Resolved
        && district.members.len() >= config.minimum_members
        && district.evidence.len() >= config.minimum_evidence
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PairContinuity {
    Event,
    Recurrence(i64),
}

fn continuity_relation(
    previous: &DistrictProjection,
    current: &DistrictProjection,
) -> Option<PairContinuity> {
    if previous.id.basis != current.id.basis {
        return None;
    }

    let mut event_continuity = false;
    let mut recurrence_periods = BTreeSet::new();
    for earlier in &previous.evidence {
        let Some(earlier_occurrence) = occurrence_for(earlier, previous.id.interval) else {
            continue;
        };
        for later in &current.evidence {
            if !same_evidence_lineage(earlier, later) {
                continue;
            }
            let Some(later_occurrence) = occurrence_for(later, current.id.interval) else {
                continue;
            };
            if let RelationRecurrence::Periodic { period_ticks } = earlier.recurrence
                && later_occurrence.start > earlier_occurrence.start
                && (later_occurrence.start - earlier_occurrence.start) % i128::from(period_ticks)
                    == 0
            {
                recurrence_periods.insert(period_ticks);
            } else if earlier_occurrence.end.saturating_add(1) >= later_occurrence.start {
                event_continuity = true;
            }
        }
    }

    recurrence_periods
        .pop_first()
        .map(PairContinuity::Recurrence)
        .or_else(|| event_continuity.then_some(PairContinuity::Event))
}

fn same_evidence_lineage(left: &RelationAssertion, right: &RelationAssertion) -> bool {
    left.kind == right.kind
        && left.participants == right.participants
        && left.recurrence == right.recurrence
        && left.direction == right.direction
        && left.measure == right.measure
        && left.provenance == right.provenance
}

impl DistrictProjectionSet {
    /// Compare two projection sets without inferring cross-interval identity.
    ///
    /// Intersections are bounded independently for each district by the
    /// smaller limit carried by the two source configurations. Containment
    /// and equality are excluded within one basis; across bases, any shared
    /// membership is explicitly reported.
    pub fn overlaps_with(&self, other: &Self) -> Vec<DistrictOverlap> {
        let limit = self.overlap_limit.min(other.overlap_limit);
        let mut left_counts: BTreeMap<&DistrictId, usize> = BTreeMap::new();
        let mut right_counts: BTreeMap<&DistrictId, usize> = BTreeMap::new();
        let mut overlaps = Vec::new();
        for left in &self.districts {
            for right in &other.districts {
                if left_counts.get(&left.id).copied().unwrap_or(0) >= limit
                    || right_counts.get(&right.id).copied().unwrap_or(0) >= limit
                {
                    continue;
                }
                let members = intersection(&left.members, &right.members);
                let same_basis_containment = left.id.basis == right.id.basis
                    && (left.members.is_subset(&right.members)
                        || right.members.is_subset(&left.members));
                if members.is_empty() || same_basis_containment {
                    continue;
                }
                let (first, second) = ordered_ids(&left.id, &right.id);
                overlaps.push(DistrictOverlap {
                    left: first,
                    right: second,
                    members,
                });
                *left_counts.entry(&left.id).or_default() += 1;
                *right_counts.entry(&right.id).or_default() += 1;
            }
        }
        overlaps.sort();
        overlaps
    }
}

#[derive(Clone, Debug)]
struct Candidate {
    anchor: RelationReference,
    members: BTreeSet<RelationReference>,
    evidence: Vec<RelationAssertion>,
}

/// Project evidence-backed districts without reading or mutating world state.
///
/// The input [`RelationView`] has already validated binary arity. Basis-view
/// refusals remain explicit in the returned set; this function never lowers
/// an unsupported higher-arity assertion or fabricates pairwise evidence.
pub fn project_districts(
    view: &RelationView,
    basis: DistrictBasis,
    interval: DistrictInterval,
    config: &DistrictConfig,
) -> DistrictProjectionSet {
    if interval.start > interval.end {
        return empty_set(
            basis,
            interval,
            DistrictStatus::ContradictoryEvidence,
            Vec::new(),
            config.maximum_overlaps_per_district,
        );
    }

    let selected = RelationView::new(
        view.iter()
            .filter(|assertion| occurrence_for(assertion, interval).is_some())
            .cloned()
            .collect(),
    )
    .expect("a subset of a validated relation view remains valid");
    let basis_view = select_basis_view(&selected, basis, config);
    let refusals = basis_view.refusals().to_vec();
    let assertions: Vec<RelationAssertion> = basis_view.iter().cloned().collect();

    let candidates = match &config.direction {
        RelationDirectionPolicy::SourceReachable(source)
            if matches!(basis, DistrictBasis::Access | DistrictBasis::Exchange) =>
        {
            directed_candidates(source, &assertions)
        }
        _ => component_candidates(&assertions),
    };

    if candidates.is_empty() {
        return empty_set(
            basis,
            interval,
            DistrictStatus::Disconnected,
            refusals,
            config.maximum_overlaps_per_district,
        );
    }

    let mut districts: Vec<DistrictProjection> = candidates
        .into_iter()
        .filter(|candidate| {
            candidate.members.len() >= config.minimum_members
                && candidate.evidence.len() >= config.minimum_evidence
        })
        .map(|candidate| projection_from(candidate, basis, interval, config))
        .collect();

    if districts.is_empty() {
        return empty_set(
            basis,
            interval,
            DistrictStatus::InsufficientEvidence,
            refusals,
            config.maximum_overlaps_per_district,
        );
    }

    districts.sort_by(|left, right| left.id.cmp(&right.id));
    let containment_contradiction = assign_parents(&mut districts, config);
    let (overlaps, overlap_contradiction) = assign_overlaps(&mut districts, config);
    let status = if containment_contradiction || overlap_contradiction {
        DistrictStatus::ContradictoryEvidence
    } else if districts
        .iter()
        .all(|district| district.status == DistrictStatus::TransientOnly)
    {
        DistrictStatus::TransientOnly
    } else {
        DistrictStatus::Resolved
    };

    DistrictProjectionSet {
        basis,
        interval,
        status,
        districts,
        refusals,
        overlaps,
        overlap_limit: config.maximum_overlaps_per_district,
    }
}

fn empty_set(
    basis: DistrictBasis,
    interval: DistrictInterval,
    status: DistrictStatus,
    refusals: Vec<RelationRefusal>,
    overlap_limit: usize,
) -> DistrictProjectionSet {
    DistrictProjectionSet {
        basis,
        interval,
        status,
        districts: Vec::new(),
        refusals,
        overlaps: BTreeSet::new(),
        overlap_limit,
    }
}

fn select_basis_view(
    view: &RelationView,
    basis: DistrictBasis,
    config: &DistrictConfig,
) -> RelationBasisView {
    match basis {
        DistrictBasis::Spatial => view.spatial(config.measure),
        DistrictBasis::Presence => view.presence(config.measure),
        DistrictBasis::Access => view.access(config.direction.clone(), config.measure),
        DistrictBasis::Exchange => view.exchange(config.direction.clone(), config.measure),
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Occurrence {
    start: i128,
    end: i128,
}

fn occurrence_for(assertion: &RelationAssertion, interval: DistrictInterval) -> Option<Occurrence> {
    let source_start = i128::from(assertion.interval.start.ticks());
    let source_end = i128::from(assertion.interval.end.ticks());
    let requested_start = i128::from(interval.start.ticks());
    let requested_end = i128::from(interval.end.ticks());
    let offset = match assertion.recurrence {
        RelationRecurrence::Once => 0,
        RelationRecurrence::Periodic { period_ticks } => {
            let period = i128::from(period_ticks);
            if requested_start <= source_end {
                0
            } else {
                (requested_start - source_end + period - 1) / period
            }
        }
    };
    let period = match assertion.recurrence {
        RelationRecurrence::Once => 0,
        RelationRecurrence::Periodic { period_ticks } => i128::from(period_ticks),
    };
    let start = source_start + offset * period;
    let end = source_end + offset * period;
    (start <= requested_end && requested_start <= end).then_some(Occurrence { start, end })
}

fn component_candidates(assertions: &[RelationAssertion]) -> Vec<Candidate> {
    let adjacency = undirected_adjacency(assertions);
    let mut unvisited: BTreeSet<RelationReference> = adjacency.keys().cloned().collect();
    let mut candidates = Vec::new();
    while let Some(start) = unvisited.pop_first() {
        let mut members = BTreeSet::from([start.clone()]);
        let mut frontier = BTreeSet::from([start.clone()]);
        while let Some(node) = frontier.pop_first() {
            for neighbor in adjacency.get(&node).into_iter().flatten() {
                if unvisited.remove(neighbor) {
                    members.insert(neighbor.clone());
                    frontier.insert(neighbor.clone());
                }
            }
        }
        let evidence = evidence_inside(assertions, &members);
        candidates.push(Candidate {
            anchor: start,
            members,
            evidence,
        });
    }
    candidates
}

fn directed_candidates(
    requested_source: &RelationReference,
    assertions: &[RelationAssertion],
) -> Vec<Candidate> {
    let root = reachable_from(requested_source, assertions);
    let mut by_members: BTreeMap<BTreeSet<RelationReference>, Candidate> = BTreeMap::new();
    for anchor in &root.members {
        let reached = reachable_from(anchor, &root.evidence);
        let candidate = Candidate {
            anchor: anchor.clone(),
            members: reached.members.clone(),
            evidence: reached.evidence,
        };
        by_members
            .entry(candidate.members.clone())
            .and_modify(|existing| {
                if candidate.anchor < existing.anchor {
                    existing.anchor = candidate.anchor.clone();
                }
            })
            .or_insert(candidate);
    }
    by_members.into_values().collect()
}

fn reachable_from(source: &RelationReference, assertions: &[RelationAssertion]) -> Candidate {
    let mut members = BTreeSet::from([source.clone()]);
    let mut selected = BTreeSet::new();
    loop {
        let previous_count = selected.len();
        for (index, assertion) in assertions.iter().enumerate() {
            let from = &assertion.participants[0].reference;
            let to = &assertion.participants[1].reference;
            let traversable = members.contains(from)
                || (assertion.direction != RelationDirection::Directed && members.contains(to));
            if traversable {
                selected.insert(index);
                members.insert(from.clone());
                members.insert(to.clone());
            }
        }
        if selected.len() == previous_count {
            break;
        }
    }
    Candidate {
        anchor: source.clone(),
        members,
        evidence: selected
            .into_iter()
            .map(|index| assertions[index].clone())
            .collect(),
    }
}

fn projection_from(
    candidate: Candidate,
    basis: DistrictBasis,
    interval: DistrictInterval,
    config: &DistrictConfig,
) -> DistrictProjection {
    let adjacency = undirected_adjacency(&candidate.evidence);
    let boundary = candidate
        .members
        .iter()
        .filter(|member| adjacency.get(*member).map_or(0, BTreeSet::len) <= 1)
        .cloned()
        .collect();
    let bridge_members = articulation_members(&candidate.members, &adjacency);
    let status =
        if common_duration_ticks(&candidate.evidence, interval) < config.minimum_duration_ticks {
            DistrictStatus::TransientOnly
        } else {
            DistrictStatus::Resolved
        };
    DistrictProjection {
        id: DistrictId {
            basis,
            interval,
            anchor: candidate.anchor,
        },
        members: candidate.members,
        boundary,
        parent: None,
        overlaps: BTreeSet::new(),
        bridge_members,
        evidence: candidate.evidence,
        status,
    }
}

fn undirected_adjacency(
    assertions: &[RelationAssertion],
) -> BTreeMap<RelationReference, BTreeSet<RelationReference>> {
    let mut adjacency: BTreeMap<RelationReference, BTreeSet<RelationReference>> = BTreeMap::new();
    for assertion in assertions {
        let left = assertion.participants[0].reference.clone();
        let right = assertion.participants[1].reference.clone();
        adjacency
            .entry(left.clone())
            .or_default()
            .insert(right.clone());
        adjacency.entry(right).or_default().insert(left);
    }
    adjacency
}

fn evidence_inside(
    assertions: &[RelationAssertion],
    members: &BTreeSet<RelationReference>,
) -> Vec<RelationAssertion> {
    assertions
        .iter()
        .filter(|assertion| {
            members.contains(&assertion.participants[0].reference)
                && members.contains(&assertion.participants[1].reference)
        })
        .cloned()
        .collect()
}

fn common_duration_ticks(evidence: &[RelationAssertion], interval: DistrictInterval) -> i64 {
    let start = evidence
        .iter()
        .filter_map(|assertion| occurrence_for(assertion, interval))
        .map(|occurrence| occurrence.start.max(i128::from(interval.start.ticks())))
        .fold(i128::from(interval.start.ticks()), i128::max);
    let end = evidence
        .iter()
        .filter_map(|assertion| occurrence_for(assertion, interval))
        .map(|occurrence| occurrence.end.min(i128::from(interval.end.ticks())))
        .fold(i128::from(interval.end.ticks()), i128::min);
    end.saturating_sub(start)
        .saturating_add(1)
        .clamp(0, i128::from(i64::MAX)) as i64
}

fn articulation_members(
    members: &BTreeSet<RelationReference>,
    adjacency: &BTreeMap<RelationReference, BTreeSet<RelationReference>>,
) -> BTreeSet<RelationReference> {
    if members.len() < 3 {
        return BTreeSet::new();
    }
    let baseline = component_count(members, adjacency, None);
    members
        .iter()
        .filter(|member| component_count(members, adjacency, Some(member)) > baseline)
        .cloned()
        .collect()
}

fn component_count(
    members: &BTreeSet<RelationReference>,
    adjacency: &BTreeMap<RelationReference, BTreeSet<RelationReference>>,
    excluded: Option<&RelationReference>,
) -> usize {
    let mut unvisited: BTreeSet<RelationReference> = members
        .iter()
        .filter(|member| excluded != Some(*member))
        .cloned()
        .collect();
    let mut count = 0;
    while let Some(start) = unvisited.pop_first() {
        count += 1;
        let mut frontier = vec![start];
        while let Some(node) = frontier.pop() {
            for neighbor in adjacency.get(&node).into_iter().flatten() {
                if excluded != Some(neighbor) && unvisited.remove(neighbor) {
                    frontier.push(neighbor.clone());
                }
            }
        }
    }
    count
}

fn assign_parents(districts: &mut [DistrictProjection], config: &DistrictConfig) -> bool {
    let mut contradictory = false;
    for child_index in 0..districts.len() {
        let child_members = &districts[child_index].members;
        let minimum_parent_size = districts
            .iter()
            .filter(|parent| {
                child_members.len() < parent.members.len()
                    && child_members.is_subset(&parent.members)
            })
            .map(|parent| parent.members.len())
            .min();
        let parents: Vec<DistrictId> = minimum_parent_size
            .into_iter()
            .flat_map(|size| {
                districts
                    .iter()
                    .filter(move |parent| {
                        parent.members.len() == size && child_members.is_subset(&parent.members)
                    })
                    .map(|parent| parent.id.clone())
            })
            .collect();
        match parents.as_slice() {
            [] => {}
            [parent] => districts[child_index].parent = Some(parent.clone()),
            _ => contradictory = true,
        }
    }

    let parents: BTreeMap<DistrictId, Option<DistrictId>> = districts
        .iter()
        .map(|district| (district.id.clone(), district.parent.clone()))
        .collect();
    for district in districts.iter() {
        let mut depth = 0;
        let mut cursor = district.parent.as_ref();
        while let Some(parent) = cursor {
            depth += 1;
            if depth > config.maximum_containment_depth {
                contradictory = true;
                break;
            }
            cursor = parents.get(parent).and_then(Option::as_ref);
        }
    }
    contradictory
}

fn assign_overlaps(
    districts: &mut [DistrictProjection],
    config: &DistrictConfig,
) -> (BTreeSet<DistrictOverlap>, bool) {
    let mut overlaps = BTreeSet::new();
    let mut contradictory = false;
    for left_index in 0..districts.len() {
        for right_index in (left_index + 1)..districts.len() {
            let members = intersection(
                &districts[left_index].members,
                &districts[right_index].members,
            );
            if members.is_empty()
                || districts[left_index]
                    .members
                    .is_subset(&districts[right_index].members)
                || districts[right_index]
                    .members
                    .is_subset(&districts[left_index].members)
            {
                continue;
            }
            if districts[left_index].overlaps.len() >= config.maximum_overlaps_per_district
                || districts[right_index].overlaps.len() >= config.maximum_overlaps_per_district
            {
                contradictory = true;
                continue;
            }
            let left_id = districts[left_index].id.clone();
            let right_id = districts[right_index].id.clone();
            districts[left_index].overlaps.insert(right_id.clone());
            districts[right_index].overlaps.insert(left_id.clone());
            overlaps.insert(DistrictOverlap {
                left: left_id,
                right: right_id,
                members,
            });
        }
    }
    (overlaps, contradictory)
}

fn intersection(
    left: &BTreeSet<RelationReference>,
    right: &BTreeSet<RelationReference>,
) -> BTreeSet<RelationReference> {
    left.intersection(right).cloned().collect()
}

fn ordered_ids(left: &DistrictId, right: &DistrictId) -> (DistrictId, DistrictId) {
    if left <= right {
        (left.clone(), right.clone())
    } else {
        (right.clone(), left.clone())
    }
}
