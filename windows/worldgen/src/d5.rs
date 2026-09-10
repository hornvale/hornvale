//! Pure comparative-apex vocabulary for the Staple D5 diagnostic.
//!
//! Callers prepare settlement observations and peer sets. This module only
//! validates, normalizes, compares, and classifies those values; it does not
//! read world state, consume a stream, or assign a city label.

/// Typed inbound and outbound flow magnitudes, counterparty counts, and
/// channel availability.
///
/// The two array positions preserve the existing D2/D4 flow kinds. Source and
/// destination counts stay typed as well, so equal aggregate throughput does
/// not erase flow composition.
/// type-audit: bare-ok(diagnostic-value: inbound_magnitudes), bare-ok(diagnostic-value: outbound_magnitudes), bare-ok(count: inbound_source_counts), bare-ok(count: inbound_distinct_source_count), bare-ok(count: outbound_destination_counts)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct D5FlowVector {
    /// Inbound magnitude ordered by flow kind.
    pub inbound_magnitudes: [f64; 2],
    /// Outbound magnitude ordered by flow kind.
    pub outbound_magnitudes: [f64; 2],
    /// Distinct inbound source count ordered by flow kind.
    pub inbound_source_counts: [usize; 2],
    /// Union-level distinct inbound source count across all flow kinds.
    pub inbound_distinct_source_count: usize,
    /// Distinct outbound destination count ordered by flow kind.
    pub outbound_destination_counts: [usize; 2],
    /// Availability of each typed inbound channel.
    pub inbound_availability: [D5ObservationAvailability; 2],
    /// Availability of each typed outbound channel.
    pub outbound_availability: [D5ObservationAvailability; 2],
}

impl D5FlowVector {
    /// An observed vector containing no flow or counterparties.
    pub const fn zero() -> Self {
        Self {
            inbound_magnitudes: [0.0; 2],
            outbound_magnitudes: [0.0; 2],
            inbound_source_counts: [0; 2],
            inbound_distinct_source_count: 0,
            outbound_destination_counts: [0; 2],
            inbound_availability: [D5ObservationAvailability::Available; 2],
            outbound_availability: [D5ObservationAvailability::Available; 2],
        }
    }

    fn magnitudes(self) -> [f64; 4] {
        [
            self.inbound_magnitudes[0],
            self.inbound_magnitudes[1],
            self.outbound_magnitudes[0],
            self.outbound_magnitudes[1],
        ]
    }

    fn magnitude_total(self) -> f64 {
        self.magnitudes().iter().sum()
    }

    fn is_well_formed(self) -> bool {
        self.magnitudes()
            .iter()
            .all(|magnitude| magnitude.is_finite() && *magnitude >= 0.0)
            && self.magnitude_total().is_finite()
    }

    fn normalized(self) -> Option<Self> {
        let total = self.magnitude_total();
        (total > 0.0).then(|| Self {
            inbound_magnitudes: self.inbound_magnitudes.map(|value| value / total),
            outbound_magnitudes: self.outbound_magnitudes.map(|value| value / total),
            inbound_source_counts: self.inbound_source_counts,
            inbound_distinct_source_count: self.inbound_distinct_source_count,
            outbound_destination_counts: self.outbound_destination_counts,
            inbound_availability: self.inbound_availability,
            outbound_availability: self.outbound_availability,
        })
    }

    fn availabilities(self) -> [D5ObservationAvailability; 4] {
        [
            self.inbound_availability[0],
            self.inbound_availability[1],
            self.outbound_availability[0],
            self.outbound_availability[1],
        ]
    }

    fn all_channels_available(self) -> bool {
        self.availabilities()
            .iter()
            .all(|availability| *availability == D5ObservationAvailability::Available)
    }
}

impl Default for D5FlowVector {
    fn default() -> Self {
        Self::zero()
    }
}

/// The six preregistered controls retained beside convergence observations.
///
/// These values expose size- or age-driven false positives. They never make a
/// settlement's convergence evidence adequate.
/// type-audit: bare-ok(count: population), bare-ok(diagnostic-value: density), bare-ok(diagnostic-value: throughput), bare-ok(count: catchment_size), bare-ok(diagnostic-value: settlement_age), bare-ok(count: relation_count)
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct D5ControlValues {
    /// Alive population at the observation instant.
    pub population: f64,
    /// Local population density.
    pub density: f64,
    /// Total observed flow throughput.
    pub throughput: f64,
    /// Size of the observed catchment.
    pub catchment_size: f64,
    /// Settlement age in the caller's prepared unit.
    pub settlement_age: f64,
    /// Count of joined relations.
    pub relation_count: usize,
}

impl D5ControlValues {
    fn is_well_formed(self) -> bool {
        [
            self.population,
            self.density,
            self.throughput,
            self.catchment_size,
            self.settlement_age,
        ]
        .iter()
        .all(|value| value.is_finite() && *value >= 0.0)
    }
}

/// Raw typed flows separated by their observed provenance.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct D5FlowProvenance {
    /// Voluntary exchange or movement.
    pub voluntary: D5FlowVector,
    /// Tribute or another imposed transfer.
    pub coercive: D5FlowVector,
    /// Flow mediated by protection or patronage.
    pub protection: D5FlowVector,
}

impl D5FlowProvenance {
    fn is_well_formed(self) -> bool {
        self.voluntary.is_well_formed()
            && self.coercive.is_well_formed()
            && self.protection.is_well_formed()
    }
}

/// Whether a caller observed a required channel or complete window.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D5ObservationAvailability {
    /// The channel or window was observed completely.
    Available,
    /// A required part of the observation window is incomplete.
    Incomplete,
    /// The current trace cannot provide the required channel.
    Unavailable,
}

/// One ordered, phase-identified raw observation window.
/// type-audit: bare-ok(index: phase), bare-ok(index: window)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct D5PhaseRecord {
    /// Caller-supplied phase identity.
    pub phase: u16,
    /// Caller-supplied window identity within the observation sequence.
    pub window: u32,
    /// Unmodified typed flow observation for this window.
    pub raw: D5FlowVector,
    /// Whether this complete-window record can contribute evidence.
    pub completeness: D5ObservationAvailability,
}

/// One prepared settlement observation; never a city or specialization label.
/// type-audit: bare-ok(index: settlement_id)
#[derive(Clone, Debug, PartialEq)]
pub struct D5SettlementProfile {
    /// Stable settlement identity supplied by the caller.
    pub settlement_id: u64,
    /// Unmodified typed flow vector at the observation instant.
    pub raw: D5FlowVector,
    /// Preregistered non-definitional controls.
    pub controls: D5ControlValues,
    /// Ordered raw phase windows used for recurrence classification.
    pub phase_records: Vec<D5PhaseRecord>,
    /// Voluntary, coercive, and protection-mediated provenance.
    pub provenance: D5FlowProvenance,
}

/// Recurrence class of a settlement's ordered complete windows.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D5Recurrence {
    /// No complete evidentiary window exists.
    Incomplete,
    /// Only one complete window exists.
    Transient,
    /// A typed profile recurs in the same phase.
    Seasonal,
    /// Profiles return to an earlier structure after changing.
    Rotating,
    /// Profiles change without returning to the initial structure.
    Drifting,
    /// Stable typed structure recurs across phases or years.
    PersistentCandidate,
}

/// An adequacy or refusal branch retained instead of imputing absent evidence.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D5EvidenceBranch {
    /// The local evidence satisfies the preregistered structural requirements.
    Adequate,
    /// A required phase or channel is incomplete.
    Incomplete,
    /// A required source channel is unavailable.
    Unavailable,
    /// At least one prepared numeric observation is invalid.
    Malformed,
    /// The complete observation contains no flow.
    Zero,
    /// No source, destination, or relation joins the settlement to a peer.
    Isolated,
    /// Voluntary inbound flow comes from fewer than two typed sources.
    InsufficientSourceDiversity,
    /// Voluntary inbound flow occupies fewer than two flow kinds.
    InsufficientTypeDiversity,
    /// Apparent prominence is supplied only by imposed flow.
    CoerciveOnly,
    /// Too few adequate peer observations support comparison.
    UnderpoweredPeers,
    /// Structural ordering collapses to a preregistered control ordering.
    ControlCollapse,
}

/// Derived comparative-convergence evidence with its raw observation intact.
/// type-audit: bare-ok(index: settlement_id), bare-ok(count: source_diversity), bare-ok(count: type_diversity), bare-ok(ratio: directional_balance), bare-ok(index: peer_rank)
#[derive(Clone, Debug, PartialEq)]
pub struct D5ConvergenceEvidence {
    /// Settlement identity from the prepared profile.
    pub settlement_id: u64,
    /// Unmodified typed observation.
    pub raw: D5FlowVector,
    /// Unmodified provenance-separated typed observations and availability.
    pub provenance: D5FlowProvenance,
    /// Typed composition normalized by total magnitude when non-zero.
    pub normalized: Option<D5FlowVector>,
    /// Union-level distinct voluntary inbound source count.
    pub source_diversity: usize,
    /// Count of voluntary inbound flow kinds with observed positive flow.
    pub type_diversity: usize,
    /// Smaller directional magnitude divided by the larger, when non-zero.
    pub directional_balance: Option<f64>,
    /// One-based structural rank among adequate peers, absent before or when
    /// comparison is underpowered.
    pub peer_rank: Option<usize>,
    /// Phase-aware recurrence of the typed profile.
    pub recurrence: D5Recurrence,
    /// Explicit adequacy and refusal branches in deterministic order.
    pub branches: Vec<D5EvidenceBranch>,
}

/// Comparison-level regime plurality retained beside settlement evidence.
/// type-audit: bare-ok(count: distinct_regimes)
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct D5RegimeEvidence {
    /// Number of distinct preregistered regimes represented by adequate peers.
    pub distinct_regimes: usize,
}

/// Per-seed comparative-apex verdict.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D5ApexVerdict {
    /// No settlement has materially differentiated convergence.
    NoRealizedApex,
    /// Apparent differentiation collapses to a control variable.
    MeasurementCollapse,
    /// Apparent convergence is explained by a preregistered refusal branch.
    QualifiedFailure,
    /// Convergence differs but does not recur.
    TransientApex,
    /// Same-phase convergence recurs.
    SeasonalApexCandidate,
    /// Convergence recurs across phases or years.
    PersistentApexCandidate,
    /// Evidence is empty, incomplete, flat in mixed ways, or underpowered.
    MixedOrUnderpowered,
}

fn recurrence_of(records: &[D5PhaseRecord]) -> D5Recurrence {
    let signatures: Option<Vec<_>> = records
        .iter()
        .map(|record| {
            (record.completeness == D5ObservationAvailability::Available
                && record.raw.is_well_formed()
                && record.raw.all_channels_available())
            .then(|| record.raw.normalized())
            .flatten()
        })
        .collect();
    let Some(signatures) = signatures else {
        return D5Recurrence::Incomplete;
    };
    if signatures.is_empty() {
        return D5Recurrence::Incomplete;
    }
    if signatures.len() == 1 {
        return D5Recurrence::Transient;
    }
    for period in 1..=(signatures.len() / 2) {
        if signatures.len() >= period * 2
            && (period..signatures.len()).all(|index| {
                signatures[index] == signatures[index % period]
                    && records[index].phase == records[index % period].phase
            })
        {
            return D5Recurrence::Seasonal;
        }
    }
    if signatures
        .iter()
        .all(|signature| *signature == signatures[0])
    {
        return if records
            .iter()
            .all(|record| record.phase == records[0].phase)
        {
            D5Recurrence::Seasonal
        } else {
            D5Recurrence::PersistentCandidate
        };
    }
    if signatures.len() > 2 && signatures.first() == signatures.last() {
        D5Recurrence::Rotating
    } else {
        D5Recurrence::Drifting
    }
}

/// Derive local convergence evidence from one prepared settlement profile.
///
/// Only voluntary inbound provenance supplies source and type diversity.
/// Coercive and protection-mediated flow remain visible in `raw` and the
/// profile but cannot silently satisfy the voluntary evidence requirement.
pub fn d5_convergence_evidence(profile: &D5SettlementProfile) -> D5ConvergenceEvidence {
    let source_diversity = profile.provenance.voluntary.inbound_distinct_source_count;
    let type_diversity = profile
        .provenance
        .voluntary
        .inbound_magnitudes
        .iter()
        .filter(|magnitude| **magnitude > 0.0)
        .count();
    let inbound: f64 = profile.raw.inbound_magnitudes.iter().sum();
    let outbound: f64 = profile.raw.outbound_magnitudes.iter().sum();
    let directional_balance =
        if inbound.is_finite() && outbound.is_finite() && (inbound > 0.0 || outbound > 0.0) {
            Some(inbound.min(outbound) / inbound.max(outbound))
        } else {
            None
        };
    let well_formed = profile.raw.is_well_formed()
        && profile.provenance.is_well_formed()
        && profile.controls.is_well_formed()
        && profile
            .phase_records
            .iter()
            .all(|record| record.raw.is_well_formed());
    let normalized = well_formed.then(|| profile.raw.normalized()).flatten();
    let recurrence = recurrence_of(&profile.phase_records);
    let mut branches = Vec::new();

    let mut has_incomplete_channel = false;
    let mut has_unavailable_channel = false;
    for vector in [
        profile.raw,
        profile.provenance.voluntary,
        profile.provenance.coercive,
        profile.provenance.protection,
    ]
    .into_iter()
    .chain(profile.phase_records.iter().map(|record| record.raw))
    {
        for availability in vector.availabilities() {
            match availability {
                D5ObservationAvailability::Available => {}
                D5ObservationAvailability::Incomplete => has_incomplete_channel = true,
                D5ObservationAvailability::Unavailable => has_unavailable_channel = true,
            }
        }
    }

    let has_unavailable_window = profile
        .phase_records
        .iter()
        .any(|record| record.completeness == D5ObservationAvailability::Unavailable);
    let has_incomplete_window = profile.phase_records.is_empty()
        || profile
            .phase_records
            .iter()
            .any(|record| record.completeness == D5ObservationAvailability::Incomplete);
    let has_zero_window = profile.phase_records.iter().any(|record| {
        record.completeness == D5ObservationAvailability::Available
            && record.raw.all_channels_available()
            && record.raw.magnitude_total() == 0.0
    });
    if has_unavailable_channel || has_unavailable_window {
        branches.push(D5EvidenceBranch::Unavailable);
    }
    if has_incomplete_channel || has_incomplete_window || has_zero_window {
        branches.push(D5EvidenceBranch::Incomplete);
    }

    if !well_formed {
        branches.push(D5EvidenceBranch::Malformed);
    } else if !has_incomplete_channel
        && !has_unavailable_channel
        && !has_incomplete_window
        && !has_unavailable_window
        && normalized.is_none()
    {
        branches.push(D5EvidenceBranch::Zero);
    }
    if profile.raw.inbound_source_counts.iter().sum::<usize>() == 0
        && profile
            .raw
            .outbound_destination_counts
            .iter()
            .sum::<usize>()
            == 0
        && profile.controls.relation_count == 0
    {
        branches.push(D5EvidenceBranch::Isolated);
    }
    let voluntary_inbound: f64 = profile.provenance.voluntary.inbound_magnitudes.iter().sum();
    let imposed_inbound: f64 = profile
        .provenance
        .coercive
        .inbound_magnitudes
        .iter()
        .chain(profile.provenance.protection.inbound_magnitudes.iter())
        .sum();
    if voluntary_inbound == 0.0 && imposed_inbound > 0.0 {
        branches.push(D5EvidenceBranch::CoerciveOnly);
    }
    if source_diversity < 2 {
        branches.push(D5EvidenceBranch::InsufficientSourceDiversity);
    }
    if type_diversity < 2 {
        branches.push(D5EvidenceBranch::InsufficientTypeDiversity);
    }
    if branches.is_empty() {
        branches.push(D5EvidenceBranch::Adequate);
    }

    D5ConvergenceEvidence {
        settlement_id: profile.settlement_id,
        raw: profile.raw,
        provenance: profile.provenance,
        normalized,
        source_diversity,
        type_diversity,
        directional_balance,
        peer_rank: None,
        recurrence,
        branches,
    }
}

fn structural_cmp(
    left: &D5ConvergenceEvidence,
    right: &D5ConvergenceEvidence,
) -> std::cmp::Ordering {
    use std::cmp::Ordering;

    left.source_diversity
        .cmp(&right.source_diversity)
        .then_with(|| left.type_diversity.cmp(&right.type_diversity))
        .then_with(|| {
            left.directional_balance
                .partial_cmp(&right.directional_balance)
                .unwrap_or(Ordering::Equal)
        })
        .then_with(|| {
            let left = left
                .normalized
                .map(D5FlowVector::magnitudes)
                .unwrap_or([0.0; 4]);
            let right = right
                .normalized
                .map(D5FlowVector::magnitudes)
                .unwrap_or([0.0; 4]);
            left.iter()
                .zip(right)
                .map(|(left, right)| left.partial_cmp(&right).unwrap_or(Ordering::Equal))
                .find(|ordering| *ordering != Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        })
}

fn ranks_by<T>(values: &[T], compare: impl Fn(&T, &T) -> std::cmp::Ordering) -> Vec<usize> {
    values
        .iter()
        .map(|value| {
            1 + values
                .iter()
                .filter(|peer| compare(peer, value).is_gt())
                .count()
        })
        .collect()
}

fn control_ranks(profiles: &[&D5SettlementProfile]) -> [Vec<usize>; 6] {
    let float_ranks = |get: fn(&D5ControlValues) -> f64| {
        ranks_by(profiles, |left, right| {
            get(&left.controls)
                .partial_cmp(&get(&right.controls))
                .unwrap_or(std::cmp::Ordering::Equal)
        })
    };
    [
        float_ranks(|controls| controls.population),
        float_ranks(|controls| controls.density),
        float_ranks(|controls| controls.throughput),
        float_ranks(|controls| controls.catchment_size),
        float_ranks(|controls| controls.settlement_age),
        ranks_by(profiles, |left, right| {
            left.controls
                .relation_count
                .cmp(&right.controls.relation_count)
        }),
    ]
}

/// Compare prepared profiles by a structural rank signature.
///
/// Ranking is lexicographic over source diversity, type diversity,
/// directional balance, and normalized typed composition. It deliberately
/// does not sum these views into a score. Fewer than two adequate profiles is
/// an explicit underpowered comparison. A control collapse is reported only
/// when any preregistered control reproduces the complete structural rank
/// ordering; an isolated large control value can never create adequacy.
pub fn d5_compare_peers(profiles: &[D5SettlementProfile]) -> Vec<D5ConvergenceEvidence> {
    let mut evidence: Vec<_> = profiles.iter().map(d5_convergence_evidence).collect();
    let adequate_indices: Vec<_> = evidence
        .iter()
        .enumerate()
        .filter_map(|(index, observation)| {
            (observation.branches == [D5EvidenceBranch::Adequate]).then_some(index)
        })
        .collect();

    if adequate_indices.len() < 2 {
        for index in adequate_indices {
            evidence[index].branches = vec![D5EvidenceBranch::UnderpoweredPeers];
        }
        return evidence;
    }

    let adequate_evidence: Vec<_> = adequate_indices
        .iter()
        .map(|index| &evidence[*index])
        .collect();
    let structural_ranks = ranks_by(&adequate_evidence, |left, right| {
        structural_cmp(left, right)
    });
    for (index, rank) in adequate_indices.iter().zip(&structural_ranks) {
        evidence[*index].peer_rank = Some(*rank);
    }

    let adequate_profiles: Vec<_> = adequate_indices
        .iter()
        .map(|index| &profiles[*index])
        .collect();
    if structural_ranks.iter().any(|rank| *rank > 1)
        && control_ranks(&adequate_profiles)
            .iter()
            .any(|ranks| ranks == &structural_ranks)
    {
        for index in adequate_indices {
            evidence[index].branches = vec![D5EvidenceBranch::ControlCollapse];
        }
    }

    evidence
}

/// Select the preregistered per-seed verdict from compared observations and
/// comparison-level regime evidence.
pub fn d5_apex_verdict(
    evidence: &[D5ConvergenceEvidence],
    regimes: D5RegimeEvidence,
) -> D5ApexVerdict {
    if evidence.is_empty() {
        return D5ApexVerdict::MixedOrUnderpowered;
    }
    if evidence.iter().any(|observation| {
        observation
            .branches
            .contains(&D5EvidenceBranch::ControlCollapse)
    }) {
        return D5ApexVerdict::MeasurementCollapse;
    }

    let ranked: Vec<_> = evidence
        .iter()
        .filter(|observation| observation.peer_rank.is_some())
        .collect();
    if ranked.len() < 2 {
        let apparent_but_refused = evidence.iter().any(|observation| {
            observation.normalized.is_some()
                && observation.branches.iter().any(|branch| {
                    matches!(
                        branch,
                        D5EvidenceBranch::Isolated
                            | D5EvidenceBranch::InsufficientSourceDiversity
                            | D5EvidenceBranch::InsufficientTypeDiversity
                            | D5EvidenceBranch::CoerciveOnly
                    )
                })
        });
        return if apparent_but_refused {
            D5ApexVerdict::QualifiedFailure
        } else {
            D5ApexVerdict::MixedOrUnderpowered
        };
    }
    if evidence.iter().any(|observation| {
        observation.branches.iter().any(|branch| {
            matches!(
                branch,
                D5EvidenceBranch::Incomplete
                    | D5EvidenceBranch::Unavailable
                    | D5EvidenceBranch::Malformed
                    | D5EvidenceBranch::UnderpoweredPeers
            )
        })
    }) {
        return D5ApexVerdict::MixedOrUnderpowered;
    }
    if ranked
        .iter()
        .all(|observation| observation.peer_rank == Some(1))
    {
        return D5ApexVerdict::NoRealizedApex;
    }
    if regimes.distinct_regimes < 2 {
        return D5ApexVerdict::MixedOrUnderpowered;
    }

    let apex_recurrences: Vec<_> = ranked
        .iter()
        .filter(|observation| observation.peer_rank == Some(1))
        .map(|observation| observation.recurrence)
        .collect();
    if apex_recurrences
        .iter()
        .all(|recurrence| *recurrence == D5Recurrence::PersistentCandidate)
    {
        D5ApexVerdict::PersistentApexCandidate
    } else if apex_recurrences
        .iter()
        .all(|recurrence| *recurrence == D5Recurrence::Seasonal)
    {
        D5ApexVerdict::SeasonalApexCandidate
    } else if apex_recurrences
        .iter()
        .all(|recurrence| matches!(recurrence, D5Recurrence::Transient | D5Recurrence::Drifting))
    {
        D5ApexVerdict::TransientApex
    } else {
        D5ApexVerdict::MixedOrUnderpowered
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flow(
        inbound_magnitudes: [f64; 2],
        outbound_magnitudes: [f64; 2],
        inbound_source_counts: [usize; 2],
    ) -> D5FlowVector {
        D5FlowVector {
            inbound_magnitudes,
            outbound_magnitudes,
            inbound_source_counts,
            inbound_distinct_source_count: inbound_source_counts.iter().sum(),
            outbound_destination_counts: [0; 2],
            inbound_availability: [D5ObservationAvailability::Available; 2],
            outbound_availability: [D5ObservationAvailability::Available; 2],
        }
    }

    fn controls(throughput: f64) -> D5ControlValues {
        D5ControlValues {
            population: 100.0,
            density: 10.0,
            throughput,
            catchment_size: 20.0,
            settlement_age: 5.0,
            relation_count: 4,
        }
    }

    fn profile(
        settlement_id: u64,
        raw: D5FlowVector,
        provenance: D5FlowProvenance,
        controls: D5ControlValues,
    ) -> D5SettlementProfile {
        D5SettlementProfile {
            settlement_id,
            raw,
            controls,
            phase_records: vec![D5PhaseRecord {
                phase: 0,
                window: 0,
                raw,
                completeness: D5ObservationAvailability::Available,
            }],
            provenance,
        }
    }

    fn regimes(distinct_regimes: usize) -> D5RegimeEvidence {
        D5RegimeEvidence { distinct_regimes }
    }

    fn voluntary(raw: D5FlowVector) -> D5FlowProvenance {
        D5FlowProvenance {
            voluntary: raw,
            ..D5FlowProvenance::default()
        }
    }

    #[test]
    fn equal_throughput_keeps_typed_and_source_composition_distinct() {
        // Break caught: replacing typed magnitudes and source counts with one
        // throughput scalar makes these observations compare equal.
        let convergent_raw = flow([6.0, 4.0], [0.0; 2], [2, 2]);
        let single_type_raw = flow([10.0, 0.0], [0.0; 2], [4, 0]);
        let convergent = profile(1, convergent_raw, voluntary(convergent_raw), controls(10.0));
        let single_type = profile(
            2,
            single_type_raw,
            voluntary(single_type_raw),
            controls(10.0),
        );

        let convergent_evidence = d5_convergence_evidence(&convergent);
        let single_type_evidence = d5_convergence_evidence(&single_type);

        assert_eq!(convergent.controls.throughput, 10.0);
        assert_eq!(single_type.controls.throughput, 10.0);
        assert_ne!(
            convergent_evidence.normalized,
            single_type_evidence.normalized
        );
        assert_eq!(convergent_evidence.source_diversity, 4);
        assert_eq!(single_type_evidence.type_diversity, 1);
    }

    #[test]
    fn one_source_across_two_kinds_is_not_two_sources() {
        // Break caught: summing per-kind counts treats one counterpart that
        // supplies both kinds as two distinct inbound sources.
        let raw = D5FlowVector {
            inbound_magnitudes: [6.0, 4.0],
            inbound_source_counts: [1, 1],
            inbound_distinct_source_count: 1,
            ..D5FlowVector::zero()
        };
        let evidence = d5_convergence_evidence(&profile(1, raw, voluntary(raw), controls(10.0)));

        assert_eq!(evidence.source_diversity, 1);
        assert!(
            evidence
                .branches
                .contains(&D5EvidenceBranch::InsufficientSourceDiversity)
        );
    }

    #[test]
    fn coercive_flow_cannot_supply_voluntary_convergence_evidence() {
        // Break caught: pooling coercive and voluntary provenance lets an
        // imposed hub satisfy the voluntary source/type requirements.
        let raw = flow([7.0, 3.0], [0.0; 2], [3, 2]);
        let coerced = profile(
            1,
            raw,
            D5FlowProvenance {
                coercive: raw,
                ..D5FlowProvenance::default()
            },
            controls(10.0),
        );

        let evidence = d5_convergence_evidence(&coerced);

        assert_eq!(evidence.source_diversity, 0);
        assert_eq!(evidence.type_diversity, 0);
        assert!(evidence.branches.contains(&D5EvidenceBranch::CoerciveOnly));
        assert!(!evidence.branches.contains(&D5EvidenceBranch::Adequate));
    }

    #[test]
    fn unavailable_channels_and_windows_are_not_observed_zeroes() {
        // Break caught: reducing channel/window refusal to zero or incomplete
        // loses both the unavailable branch and the exact refused channel.
        let raw = flow([6.0, 4.0], [0.0; 2], [2, 2]);
        let mut unavailable_channel = profile(1, raw, voluntary(raw), controls(10.0));
        unavailable_channel
            .provenance
            .voluntary
            .inbound_availability[1] = D5ObservationAvailability::Unavailable;
        unavailable_channel.raw = D5FlowVector::zero();

        let channel_evidence = d5_convergence_evidence(&unavailable_channel);
        assert!(
            channel_evidence
                .branches
                .contains(&D5EvidenceBranch::Unavailable)
        );
        assert!(!channel_evidence.branches.contains(&D5EvidenceBranch::Zero));

        let mut unavailable_window = profile(2, raw, voluntary(raw), controls(10.0));
        unavailable_window.phase_records[0].completeness = D5ObservationAvailability::Unavailable;

        let window_evidence = d5_convergence_evidence(&unavailable_window);
        assert!(
            window_evidence
                .branches
                .contains(&D5EvidenceBranch::Unavailable)
        );
        assert!(
            !window_evidence
                .branches
                .contains(&D5EvidenceBranch::Incomplete)
        );
    }

    #[test]
    fn normalization_preserves_the_raw_typed_vector() {
        // Break caught: normalization overwrites raw evidence or drops the
        // source-count side of the typed observation.
        let raw = flow([6.0, 4.0], [0.0; 2], [3, 2]);
        let observed = profile(1, raw, voluntary(raw), controls(10.0));

        let evidence = d5_convergence_evidence(&observed);

        assert_eq!(observed.raw, raw);
        assert_eq!(evidence.raw, raw);
        assert_eq!(
            evidence.normalized,
            Some(flow([0.6, 0.4], [0.0; 2], [3, 2]))
        );
    }

    #[test]
    fn zero_non_finite_and_missing_inputs_are_explicitly_inadequate() {
        // Break caught: absent, all-zero, or malformed observations are
        // silently treated as observed zeroes that can enter comparison.
        let zero = profile(
            1,
            D5FlowVector::zero(),
            D5FlowProvenance::default(),
            controls(0.0),
        );
        let zero_evidence = d5_convergence_evidence(&zero);
        assert!(zero_evidence.branches.contains(&D5EvidenceBranch::Zero));
        assert!(!zero_evidence.branches.contains(&D5EvidenceBranch::Adequate));

        let malformed_raw = flow([f64::NAN, 1.0], [0.0; 2], [1, 1]);
        let malformed = profile(
            2,
            malformed_raw,
            voluntary(malformed_raw),
            controls(f64::NAN),
        );
        let malformed_evidence = d5_convergence_evidence(&malformed);
        assert!(
            malformed_evidence
                .branches
                .contains(&D5EvidenceBranch::Malformed)
        );
        assert_eq!(malformed_evidence.normalized, None);

        for (availability, branch) in [
            (
                D5ObservationAvailability::Incomplete,
                D5EvidenceBranch::Incomplete,
            ),
            (
                D5ObservationAvailability::Unavailable,
                D5EvidenceBranch::Unavailable,
            ),
        ] {
            let raw = flow([6.0, 4.0], [0.0; 2], [2, 2]);
            let mut missing = profile(3, raw, voluntary(raw), controls(10.0));
            missing.provenance.voluntary.inbound_availability[0] = availability;
            let evidence = d5_convergence_evidence(&missing);
            assert!(evidence.branches.contains(&branch));
            assert!(!evidence.branches.contains(&D5EvidenceBranch::Adequate));
        }

        let raw = flow([6.0, 4.0], [0.0; 2], [2, 2]);
        let mut channel_missing = profile(4, raw, voluntary(raw), controls(10.0));
        channel_missing.provenance.coercive.outbound_availability[1] =
            D5ObservationAvailability::Unavailable;
        let evidence = d5_convergence_evidence(&channel_missing);
        assert!(evidence.branches.contains(&D5EvidenceBranch::Unavailable));
        assert!(!evidence.branches.contains(&D5EvidenceBranch::Adequate));
    }

    #[test]
    fn large_controls_do_not_rescue_isolation_or_one_type_flow() {
        // Break caught: population, throughput, age, or relation count is
        // allowed to manufacture adequacy without convergent typed sources.
        let mut large_controls = controls(10_000.0);
        large_controls.population = 50_000.0;
        large_controls.density = 500.0;
        large_controls.catchment_size = 5_000.0;
        large_controls.settlement_age = 1_000.0;
        large_controls.relation_count = 0;
        let isolated_raw = flow([9_000.0, 1_000.0], [0.0; 2], [0, 0]);
        let isolated = profile(1, isolated_raw, voluntary(isolated_raw), large_controls);

        let one_type_raw = flow([10_000.0, 0.0], [0.0; 2], [40, 0]);
        let one_type = profile(2, one_type_raw, voluntary(one_type_raw), controls(10_000.0));

        let mut old_controls = controls(10.0);
        old_controls.settlement_age = 10_000.0;
        let no_inbound_sources_raw = flow([6.0, 4.0], [0.0; 2], [0, 0]);
        let no_inbound_sources = profile(
            3,
            no_inbound_sources_raw,
            voluntary(no_inbound_sources_raw),
            old_controls,
        );

        let compared = d5_compare_peers(&[isolated, one_type, no_inbound_sources]);

        assert!(compared[0].branches.contains(&D5EvidenceBranch::Isolated));
        assert!(
            compared[1]
                .branches
                .contains(&D5EvidenceBranch::InsufficientTypeDiversity)
        );
        assert!(
            compared[2]
                .branches
                .contains(&D5EvidenceBranch::InsufficientSourceDiversity)
        );
        assert!(compared.iter().all(|evidence| evidence.peer_rank.is_none()));
        assert_eq!(
            d5_apex_verdict(&compared, regimes(1)),
            D5ApexVerdict::QualifiedFailure
        );
    }

    #[test]
    fn multiple_typed_inbound_sources_are_ranked_against_adequate_peers() {
        // Break caught: peer comparison ignores typed/source convergence or
        // admits a singleton without an adequately observed peer.
        let apex_raw = flow([6.0, 4.0], [3.0, 2.0], [3, 2]);
        let peer_raw = flow([7.0, 3.0], [1.0, 1.0], [1, 1]);
        let apex = profile(1, apex_raw, voluntary(apex_raw), controls(10.0));
        let peer = profile(2, peer_raw, voluntary(peer_raw), controls(10.0));

        let compared = d5_compare_peers(&[apex, peer]);

        assert_eq!(compared[0].peer_rank, Some(1));
        assert_eq!(compared[1].peer_rank, Some(2));
        assert_eq!(compared[0].branches, vec![D5EvidenceBranch::Adequate]);
        assert_eq!(
            d5_apex_verdict(&compared, regimes(2)),
            D5ApexVerdict::TransientApex
        );
    }

    #[test]
    fn candidate_requires_multiple_adequate_settlements() {
        // Break caught: one structurally adequate settlement can receive a
        // positive verdict without an adequate comparison settlement.
        let raw = flow([6.0, 4.0], [3.0, 2.0], [3, 2]);
        let compared = d5_compare_peers(&[profile(1, raw, voluntary(raw), controls(10.0))]);

        assert_eq!(
            compared[0].branches,
            vec![D5EvidenceBranch::UnderpoweredPeers]
        );
        assert_eq!(
            d5_apex_verdict(&compared, regimes(2)),
            D5ApexVerdict::MixedOrUnderpowered
        );
    }

    #[test]
    fn candidate_requires_multiple_observed_regimes() {
        // Break caught: two adequate settlements in one comparison regime
        // are enough to produce a positive apex verdict.
        let apex_raw = flow([6.0, 4.0], [3.0, 2.0], [3, 2]);
        let peer_raw = flow([7.0, 3.0], [1.0, 1.0], [1, 1]);
        let apex = profile(1, apex_raw, voluntary(apex_raw), controls(10.0));
        let peer = profile(2, peer_raw, voluntary(peer_raw), controls(10.0));
        let compared = d5_compare_peers(&[apex, peer]);

        assert_eq!(
            d5_apex_verdict(&compared, regimes(1)),
            D5ApexVerdict::MixedOrUnderpowered
        );
    }

    #[test]
    fn any_single_explanatory_control_collapses_structural_ranks() {
        // Break caught: requiring every control to reproduce the structural
        // ranks lets a profile explained by any one preregistered control pass.
        let apex_raw = flow([6.0, 4.0], [3.0, 2.0], [3, 2]);
        let peer_raw = flow([7.0, 3.0], [1.0, 1.0], [1, 1]);
        let vary_one: [fn(&mut D5ControlValues); 6] = [
            |value| value.population = 200.0,
            |value| value.density = 20.0,
            |value| value.throughput = 20.0,
            |value| value.catchment_size = 40.0,
            |value| value.settlement_age = 10.0,
            |value| value.relation_count = 8,
        ];

        for vary in vary_one {
            let mut apex_controls = controls(10.0);
            vary(&mut apex_controls);
            let apex = profile(1, apex_raw, voluntary(apex_raw), apex_controls);
            let peer = profile(2, peer_raw, voluntary(peer_raw), controls(10.0));

            assert_eq!(
                d5_apex_verdict(&d5_compare_peers(&[apex, peer]), regimes(2)),
                D5ApexVerdict::MeasurementCollapse
            );
        }
    }

    #[test]
    fn verdict_distinguishes_flat_control_collapsed_and_recurrent_apices() {
        // Break caught: the verdict helper merges the dead pole, control
        // collapse, transient, seasonal, and persistent branches.
        let apex_raw = flow([6.0, 4.0], [3.0, 2.0], [3, 2]);
        let peer_raw = flow([7.0, 3.0], [1.0, 1.0], [1, 1]);

        let flat_a = profile(1, apex_raw, voluntary(apex_raw), controls(15.0));
        let flat_b = profile(2, apex_raw, voluntary(apex_raw), controls(15.0));
        assert_eq!(
            d5_apex_verdict(&d5_compare_peers(&[flat_a, flat_b]), regimes(1)),
            D5ApexVerdict::NoRealizedApex
        );

        let mut collapsed_apex_controls = controls(15.0);
        collapsed_apex_controls.population = 200.0;
        collapsed_apex_controls.density = 20.0;
        collapsed_apex_controls.catchment_size = 30.0;
        collapsed_apex_controls.settlement_age = 10.0;
        collapsed_apex_controls.relation_count = 6;
        let mut collapsed_peer_controls = controls(12.0);
        collapsed_peer_controls.population = 100.0;
        let collapsed_apex = profile(1, apex_raw, voluntary(apex_raw), collapsed_apex_controls);
        let collapsed_peer = profile(2, peer_raw, voluntary(peer_raw), collapsed_peer_controls);
        assert_eq!(
            d5_apex_verdict(
                &d5_compare_peers(&[collapsed_apex, collapsed_peer]),
                regimes(2),
            ),
            D5ApexVerdict::MeasurementCollapse
        );

        let mut seasonal_apex = profile(1, apex_raw, voluntary(apex_raw), controls(10.0));
        seasonal_apex.phase_records.push(D5PhaseRecord {
            phase: 1,
            window: 1,
            raw: peer_raw,
            completeness: D5ObservationAvailability::Available,
        });
        seasonal_apex.phase_records.push(D5PhaseRecord {
            phase: 0,
            window: 2,
            raw: apex_raw,
            completeness: D5ObservationAvailability::Available,
        });
        seasonal_apex.phase_records.push(D5PhaseRecord {
            phase: 1,
            window: 3,
            raw: peer_raw,
            completeness: D5ObservationAvailability::Available,
        });
        let mut seasonal_peer = profile(2, peer_raw, voluntary(peer_raw), controls(10.0));
        seasonal_peer.phase_records.push(D5PhaseRecord {
            phase: 1,
            window: 1,
            raw: apex_raw,
            completeness: D5ObservationAvailability::Available,
        });
        seasonal_peer.phase_records.push(D5PhaseRecord {
            phase: 0,
            window: 2,
            raw: peer_raw,
            completeness: D5ObservationAvailability::Available,
        });
        seasonal_peer.phase_records.push(D5PhaseRecord {
            phase: 1,
            window: 3,
            raw: apex_raw,
            completeness: D5ObservationAvailability::Available,
        });
        assert_eq!(
            d5_apex_verdict(
                &d5_compare_peers(&[seasonal_apex, seasonal_peer]),
                regimes(2),
            ),
            D5ApexVerdict::SeasonalApexCandidate
        );

        let mut persistent_apex = profile(1, apex_raw, voluntary(apex_raw), controls(10.0));
        persistent_apex.phase_records.push(D5PhaseRecord {
            phase: 1,
            window: 1,
            raw: apex_raw,
            completeness: D5ObservationAvailability::Available,
        });
        let mut persistent_peer = profile(2, peer_raw, voluntary(peer_raw), controls(10.0));
        persistent_peer.phase_records.push(D5PhaseRecord {
            phase: 1,
            window: 1,
            raw: peer_raw,
            completeness: D5ObservationAvailability::Available,
        });
        assert_eq!(
            d5_apex_verdict(
                &d5_compare_peers(&[persistent_apex, persistent_peer]),
                regimes(2),
            ),
            D5ApexVerdict::PersistentApexCandidate
        );

        assert_eq!(
            d5_apex_verdict(&[], regimes(0)),
            D5ApexVerdict::MixedOrUnderpowered
        );
    }
}
