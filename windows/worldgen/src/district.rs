//! Pure, deterministic district projection over typed relation views.

use crate::relation::{
    RelationAssertion, RelationBasisView, RelationDirection, RelationDirectionPolicy,
    RelationMeasureFilter, RelationReference, RelationRefusal, RelationView,
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

    let basis_view = select_basis_view(view, basis, config);
    let refusals = basis_view.refusals().to_vec();
    let assertions: Vec<RelationAssertion> = basis_view
        .iter()
        .filter(|assertion| intersects(assertion, interval))
        .cloned()
        .collect();

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

fn intersects(assertion: &RelationAssertion, interval: DistrictInterval) -> bool {
    assertion.interval.start <= interval.end && interval.start <= assertion.interval.end
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
        .map(|assertion| assertion.interval.start)
        .fold(interval.start, WorldTime::max);
    let end = evidence
        .iter()
        .map(|assertion| assertion.interval.end)
        .fold(interval.end, WorldTime::min);
    end.ticks()
        .saturating_sub(start.ticks())
        .saturating_add(1)
        .max(0)
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
