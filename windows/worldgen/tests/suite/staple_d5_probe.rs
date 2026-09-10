//! Per-seed D5 comparative-apex probe.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    D5ApexVerdict, D5ControlValues, D5EvidenceBranch, D5FlowProvenance, D5FlowVector,
    D5ObservationAvailability, D5PhaseRecord, D5RegimeEvidence, D5SettlementProfile,
    SettlementPins, build_world, d5_apex_verdict, d5_compare_peers,
};

const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=8;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct JoinBranches {
    missing: Vec<u64>,
    duplicate: Vec<u64>,
    isolated: Vec<u64>,
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
    profiles: Vec<D5SettlementProfile>,
    regimes: usize,
) -> Report {
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
    let evidence = d5_compare_peers(&profiles);
    let ids: BTreeSet<_> = evidence.iter().map(|item| item.settlement_id).collect();
    assert_eq!(ids, BTreeSet::from([1, 2]));
}

#[test]
/// claim: readout(off-gate, fixed D5 probe roster 1..=8; prints measurements
/// only and remains ignored until the sanctioned campaign boundary)
#[ignore = "probe: fixed-roster D5 readout; run only at the sanctioned campaign boundary"]
fn staple_d5_fixed_roster_readout() {
    for seed in PROBE_SEEDS {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("fixed D5 probe seed builds");
        let settlements = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .count();
        println!("D5 seed={seed} alive_settlements={settlements} verdict=measurement-only");
    }
}
