//! Per-seed D5 comparative-apex probe.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BakeId, D4Availability, D5ApexVerdict, D5ControlValues, D5EvidenceBranch, D5FlowProvenance,
    D5FlowVector, D5ObservationAvailability, D5PhaseRecord, D5RegimeEvidence, D5SettlementProfile,
    DiagnosticPortfolioPhase, DiagnosticPortfolioWitness, ExchangeTreatment, SettlementPins,
    WorldComponents, build_world_with_exchange_treatment, d5_apex_verdict, d5_compare_peers,
};

const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=8;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct JoinBranches {
    missing: Vec<u64>,
    duplicate: Vec<u64>,
    isolated: Vec<u64>,
    orphan: Vec<u64>,
    site_mismatch: Vec<u64>,
}

#[derive(Clone, Debug, PartialEq)]
struct Report {
    seed: u64,
    denominator: usize,
    evidence: Vec<hornvale_worldgen::D5ConvergenceEvidence>,
    branches: JoinBranches,
    verdict: D5ApexVerdict,
}

fn controls() -> D5ControlValues {
    D5ControlValues {
        population: 100.0,
        density: 10.0,
        throughput: 10.0,
        catchment_size: 20.0,
        settlement_age: 5.0,
        relation_count: 4,
    }
}

fn flow(inbound: [f64; 2], sources: [usize; 2], union_sources: usize) -> D5FlowVector {
    D5FlowVector {
        inbound_magnitudes: inbound,
        inbound_source_counts: sources,
        inbound_distinct_source_count: union_sources,
        ..D5FlowVector::zero()
    }
}

fn profile(id: u64, raw: D5FlowVector, phases: &[(u16, D5FlowVector)]) -> D5SettlementProfile {
    D5SettlementProfile {
        settlement_id: id,
        raw,
        controls: controls(),
        phase_records: phases
            .iter()
            .enumerate()
            .map(|(window, (phase, raw))| D5PhaseRecord {
                phase: *phase,
                window: window as u32,
                raw: *raw,
                completeness: D5ObservationAvailability::Available,
            })
            .collect(),
        provenance: D5FlowProvenance {
            voluntary: raw,
            ..D5FlowProvenance::default()
        },
    }
}

fn summarize(
    seed: u64,
    denominator: usize,
    mut profiles: Vec<D5SettlementProfile>,
    regimes: usize,
) -> Report {
    profiles.sort_by_key(|profile| profile.settlement_id);
    let branches = JoinBranches {
        isolated: profiles
            .iter()
            .filter(|profile| profile.raw == D5FlowVector::zero())
            .map(|profile| profile.settlement_id)
            .collect(),
        ..JoinBranches::default()
    };
    let evidence = d5_compare_peers(&profiles);
    let verdict = d5_apex_verdict(
        &evidence,
        D5RegimeEvidence {
            distinct_regimes: regimes,
        },
    );
    Report {
        seed,
        denominator,
        evidence,
        branches,
        verdict,
    }
}

fn adequate_pair() -> Vec<D5SettlementProfile> {
    let apex = flow([6.0, 4.0], [3, 2], 5);
    let peer = flow([4.0, 2.0], [2, 1], 3);
    vec![
        profile(1, apex, &[(0, apex)]),
        profile(2, peer, &[(0, peer)]),
    ]
}

fn d5_availability(availability: D4Availability) -> D5ObservationAvailability {
    match availability {
        D4Availability::Available => D5ObservationAvailability::Available,
        D4Availability::Debt(_) => D5ObservationAvailability::Unavailable,
    }
}

fn resource_index(resource: hornvale_worldgen::SubsistenceResource) -> usize {
    match resource {
        hornvale_worldgen::SubsistenceResource::A => 0,
        hornvale_worldgen::SubsistenceResource::B => 1,
    }
}

fn flow_from_phase(
    community: BakeId,
    phase: &DiagnosticPortfolioPhase,
) -> (D5FlowVector, D5FlowProvenance) {
    let coercive = phase.portfolio.coercive_transfer.unwrap_or([0.0; 2]);
    let protection = phase.portfolio.protection_access.unwrap_or([0.0; 2]);
    let mut source_sets = [BTreeSet::new(), BTreeSet::new()];
    for attempt in &phase.exchange_attempts {
        if attempt.requester == community
            && let Some(counterparty) = attempt.counterparty
        {
            source_sets[resource_index(attempt.resource)].insert(counterparty);
        }
    }
    let union_sources = source_sets.iter().flatten().collect::<BTreeSet<_>>().len();
    let flow = D5FlowVector {
        inbound_magnitudes: [
            phase.portfolio.imports[0] + coercive[0] + protection[0],
            phase.portfolio.imports[1] + coercive[1] + protection[1],
        ],
        outbound_magnitudes: phase.portfolio.voluntary_exchange,
        inbound_source_counts: [source_sets[0].len(), source_sets[1].len()],
        inbound_distinct_source_count: union_sources,
        inbound_availability: [
            d5_availability(phase.mechanism_availability.imports),
            d5_availability(phase.mechanism_availability.imports),
        ],
        outbound_availability: [
            d5_availability(phase.mechanism_availability.voluntary_exchange),
            d5_availability(phase.mechanism_availability.voluntary_exchange),
        ],
        ..D5FlowVector::zero()
    };
    let provenance = D5FlowProvenance {
        voluntary: D5FlowVector {
            inbound_magnitudes: phase.portfolio.imports,
            inbound_source_counts: flow.inbound_source_counts,
            inbound_distinct_source_count: union_sources,
            inbound_availability: [
                d5_availability(phase.mechanism_availability.imports),
                d5_availability(phase.mechanism_availability.imports),
            ],
            ..D5FlowVector::zero()
        },
        coercive: D5FlowVector {
            inbound_magnitudes: coercive,
            inbound_availability: [
                d5_availability(phase.mechanism_availability.coercive_transfer),
                d5_availability(phase.mechanism_availability.coercive_transfer),
            ],
            ..D5FlowVector::zero()
        },
        protection: D5FlowVector {
            inbound_magnitudes: protection,
            inbound_availability: [
                d5_availability(phase.mechanism_availability.protection_access),
                d5_availability(phase.mechanism_availability.protection_access),
            ],
            ..D5FlowVector::zero()
        },
    };
    (flow, provenance)
}

fn profile_from_witness(witness: &DiagnosticPortfolioWitness) -> Option<D5SettlementProfile> {
    let last = witness.phases.last()?;
    let (raw, provenance) = flow_from_phase(witness.community, last);
    let phase_records = witness
        .phases
        .iter()
        .map(|phase| {
            let (raw, _) = flow_from_phase(witness.community, phase);
            D5PhaseRecord {
                phase: phase.phase,
                window: phase.phase as u32,
                raw,
                completeness: D5ObservationAvailability::Available,
            }
        })
        .collect();
    let throughput = raw
        .inbound_magnitudes
        .iter()
        .chain(raw.outbound_magnitudes.iter())
        .sum();
    Some(D5SettlementProfile {
        settlement_id: witness.community.0,
        raw,
        controls: D5ControlValues {
            population: 0.0,
            density: 0.0,
            throughput,
            catchment_size: 0.0,
            settlement_age: 0.0,
            relation_count: raw.inbound_distinct_source_count,
        },
        phase_records,
        provenance,
    })
}

fn live_report(seed: u64) -> Report {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let built = build_world_with_exchange_treatment(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components,
        ExchangeTreatment::Enabled,
    )
    .expect("fixed D5 probe seed builds");
    let live: BTreeMap<_, _> = built
        .history
        .records
        .iter()
        .filter(|record| record.core.is_alive())
        .map(|record| (record.community, record.core.site))
        .collect();
    let mut witnesses: BTreeMap<_, Vec<_>> = BTreeMap::new();
    for witness in &built.history.diagnostic_portfolios {
        witnesses
            .entry(witness.community)
            .or_default()
            .push(witness);
    }
    let mut branches = JoinBranches::default();
    let mut profiles = Vec::new();
    for (&community, &site) in &live {
        let Some(rows) = witnesses.get(&community) else {
            branches.missing.push(community.0);
            continue;
        };
        if rows.len() != 1 {
            branches.duplicate.push(community.0);
            continue;
        }
        if rows[0].site != site {
            branches.site_mismatch.push(community.0);
            continue;
        }
        if let Some(profile) = profile_from_witness(rows[0]) {
            profiles.push(profile);
        } else {
            branches.missing.push(community.0);
        }
    }
    branches.orphan = witnesses
        .keys()
        .filter(|community| !live.contains_key(community))
        .map(|community| community.0)
        .collect();
    let regimes = profiles
        .iter()
        .flat_map(|profile| profile.phase_records.iter().map(|record| record.phase))
        .collect::<BTreeSet<_>>()
        .len();
    let mut report = summarize(seed, live.len(), profiles, regimes);
    report.branches = branches;
    if !report.branches.missing.is_empty()
        || !report.branches.duplicate.is_empty()
        || !report.branches.orphan.is_empty()
        || !report.branches.site_mismatch.is_empty()
    {
        report.verdict = D5ApexVerdict::MixedOrUnderpowered;
    }
    report
}

#[test]
fn fixture_branches_keep_empty_zero_isolated_and_coercive_explicit() {
    let zero = profile(1, D5FlowVector::zero(), &[(0, D5FlowVector::zero())]);
    let coerced_raw = flow([6.0, 4.0], [3, 2], 5);
    let coerced = D5SettlementProfile {
        provenance: D5FlowProvenance {
            coercive: coerced_raw,
            ..D5FlowProvenance::default()
        },
        ..profile(2, coerced_raw, &[(0, coerced_raw)])
    };
    let report = summarize(1, 3, vec![zero, coerced], 0);

    assert!(report.branches.isolated.contains(&1));
    assert!(
        report.evidence[0]
            .branches
            .contains(&D5EvidenceBranch::Zero)
    );
    assert!(
        report.evidence[1]
            .branches
            .contains(&D5EvidenceBranch::CoerciveOnly)
    );
    assert_eq!(report.verdict, D5ApexVerdict::QualifiedFailure);
}

#[test]
fn fixture_requires_multiple_adequate_settlements_and_regimes() {
    let report = summarize(7, 2, adequate_pair(), 1);
    assert_eq!(report.denominator, 2);
    assert_eq!(report.evidence.len(), 2);
    assert_eq!(report.verdict, D5ApexVerdict::MixedOrUnderpowered);

    let report = summarize(7, 2, adequate_pair(), 2);
    assert_eq!(report.verdict, D5ApexVerdict::TransientApex);
}

#[test]
fn fixture_distinguishes_same_phase_and_cross_phase_recurrence() {
    let mut same_phase = adequate_pair();
    let apex = same_phase[0].raw;
    let peer = same_phase[1].raw;
    same_phase[0].phase_records = vec![
        D5PhaseRecord {
            phase: 0,
            window: 0,
            raw: apex,
            completeness: D5ObservationAvailability::Available,
        },
        D5PhaseRecord {
            phase: 1,
            window: 1,
            raw: peer,
            completeness: D5ObservationAvailability::Available,
        },
        D5PhaseRecord {
            phase: 0,
            window: 2,
            raw: apex,
            completeness: D5ObservationAvailability::Available,
        },
        D5PhaseRecord {
            phase: 1,
            window: 3,
            raw: peer,
            completeness: D5ObservationAvailability::Available,
        },
    ];
    same_phase[1].phase_records = same_phase[0].phase_records.clone();
    assert_eq!(
        summarize(1, 2, same_phase, 2).verdict,
        D5ApexVerdict::SeasonalApexCandidate
    );

    let mut cross_phase = adequate_pair();
    for item in &mut cross_phase {
        item.phase_records = vec![
            D5PhaseRecord {
                phase: 0,
                window: 0,
                raw: item.raw,
                completeness: D5ObservationAvailability::Available,
            },
            D5PhaseRecord {
                phase: 1,
                window: 1,
                raw: item.raw,
                completeness: D5ObservationAvailability::Available,
            },
        ];
    }
    assert_eq!(
        summarize(2, 2, cross_phase, 2).verdict,
        D5ApexVerdict::PersistentApexCandidate
    );
}

#[test]
fn fixture_mixed_and_missing_join_denominators_do_not_pool_into_a_positive() {
    let mut profiles = adequate_pair();
    profiles.push(profile(
        3,
        flow([0.0, 0.0], [0, 0], 0),
        &[(0, D5FlowVector::zero())],
    ));
    let report = summarize(3, 4, profiles, 2);
    assert_eq!(report.denominator, 4);
    assert_eq!(report.verdict, D5ApexVerdict::MixedOrUnderpowered);

    let branches = JoinBranches {
        missing: vec![4],
        duplicate: vec![2],
        ..JoinBranches::default()
    };
    assert_eq!(branches.missing, vec![4]);
    assert_eq!(branches.duplicate, vec![2]);
}

#[test]
fn fixture_evidence_is_stably_ordered_by_settlement_identity() {
    let mut profiles = adequate_pair();
    profiles.reverse();
    let evidence = summarize(6, 2, profiles, 2).evidence;
    let ids: Vec<_> = evidence.iter().map(|item| item.settlement_id).collect();
    assert_eq!(ids, vec![1, 2]);
}

#[test]
fn fixture_covers_flat_and_control_collapse_verdicts() {
    let raw = flow([6.0, 4.0], [3, 2], 5);
    let flat = summarize(
        4,
        2,
        vec![profile(1, raw, &[(0, raw)]), profile(2, raw, &[(0, raw)])],
        2,
    );
    assert_eq!(flat.verdict, D5ApexVerdict::NoRealizedApex);

    let mut collapsed = adequate_pair();
    collapsed[0].controls.population = 200.0;
    let report = summarize(5, 2, collapsed, 2);
    assert_eq!(report.verdict, D5ApexVerdict::MeasurementCollapse);
}

#[test]
fn fixture_covers_unavailable_and_malformed_refusals() {
    let mut unavailable = profile(1, flow([6.0, 4.0], [3, 2], 5), &[]);
    unavailable.raw.inbound_availability[0] = D5ObservationAvailability::Unavailable;
    let unavailable_evidence = d5_compare_peers(&[unavailable])[0].clone();
    assert!(
        unavailable_evidence
            .branches
            .contains(&D5EvidenceBranch::Unavailable)
    );

    let malformed_raw = flow([f64::NAN, 4.0], [3, 2], 5);
    let malformed = profile(2, malformed_raw, &[(0, malformed_raw)]);
    let malformed_evidence = d5_compare_peers(&[malformed])[0].clone();
    assert!(
        malformed_evidence
            .branches
            .contains(&D5EvidenceBranch::Malformed)
    );
}

#[test]
fn live_d4_witness_join_has_a_per_seed_denominator() {
    let report = live_report(1);
    assert!(report.denominator > 0);
    assert!(report.evidence.len() <= report.denominator);
    assert!(report.branches.duplicate.is_empty());
    assert_eq!(
        report.evidence.len() + report.branches.missing.len(),
        report.denominator
    );
    assert_eq!(report.verdict, D5ApexVerdict::MixedOrUnderpowered);
}

#[test]
fn mutation_of_source_diversity_or_phase_identity_refuses_the_apex() {
    let mut source_mutation = adequate_pair();
    source_mutation[0].raw.inbound_distinct_source_count = 1;
    source_mutation[0]
        .provenance
        .voluntary
        .inbound_distinct_source_count = 1;
    let report = summarize(8, 2, source_mutation, 2);
    assert_eq!(report.verdict, D5ApexVerdict::QualifiedFailure);

    let mut phase_mutation = adequate_pair();
    for profile in &mut phase_mutation {
        profile.phase_records.clear();
    }
    let report = summarize(9, 2, phase_mutation, 2);
    assert_eq!(report.verdict, D5ApexVerdict::MixedOrUnderpowered);
}

#[test]
fn reduction_is_deterministic_and_does_not_mutate_a_second_observation() {
    let first = live_report(2);
    let second = live_report(2);
    assert_eq!(first, second);
    assert_eq!(first.verdict, D5ApexVerdict::MixedOrUnderpowered);
}

#[test]
/// claim: readout(off-gate, fixed D5 probe roster 1..=8; prints measurements
/// only and remains ignored until the sanctioned campaign boundary)
#[ignore = "probe: fixed-roster D5 readout; run only at the sanctioned campaign boundary"]
fn staple_d5_fixed_roster_readout() {
    for seed in PROBE_SEEDS {
        let report = live_report(seed);
        println!(
            "D5 seed={} denominator={} joined={} missing={} duplicate={} orphan={} site_mismatch={} verdict={:?}",
            report.seed,
            report.denominator,
            report.evidence.len(),
            report.branches.missing.len(),
            report.branches.duplicate.len(),
            report.branches.orphan.len(),
            report.branches.site_mismatch.len(),
            report.verdict,
        );
    }
}
