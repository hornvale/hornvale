//! Per-seed D4 portfolio-regime falsifier.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeMap;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, Vertex, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BakeId, D4Availability, D4AxisDebt, D4Completeness, D4MechanismAvailability, D4MechanismClass,
    D4PortfolioProfile, D4PortfolioVector, D4RecurrenceClass, D4RegimeEvidence, D4RegimeVerdict,
    DiagnosticPortfolioPhase, DiagnosticPortfolioValues, DiagnosticPortfolioWitness,
    ExchangeTreatment, ExchangeTreatmentBuild, History, SettlementPins, WorldComponents,
    build_world_with_exchange_treatment, census, d4_profile_signature, d4_recurrence_class,
    d4_regime_verdict, emit_history,
};

const PROBE_WORLD_DENOMINATOR: usize = 200;
const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;

#[derive(Clone, Debug)]
struct Input {
    seed: u64,
    denominator: u64,
    live: Vec<(BakeId, Vertex)>,
    witnesses: Vec<DiagnosticPortfolioWitness>,
    sources: Vec<(BakeId, Vertex, [f64; 2])>,
    treatment: ExchangeTreatment,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Branches {
    duplicate_live: Vec<BakeId>,
    duplicate_witness: Vec<BakeId>,
    duplicate_source: Vec<BakeId>,
    missing_witness: Vec<BakeId>,
    missing_source: Vec<BakeId>,
    orphan_witness: Vec<BakeId>,
    site_mismatch: Vec<BakeId>,
    incomplete: Vec<BakeId>,
    axis_debt: Vec<D4AxisDebt>,
}

#[derive(Clone, Debug, PartialEq)]
struct Joined {
    community: BakeId,
    site: Vertex,
    profiles: Vec<D4PortfolioProfile>,
    source: [f64; 2],
}

#[derive(Clone, Debug, PartialEq)]
struct Report {
    seed: u64,
    denominator: u64,
    joined: Vec<Joined>,
    branches: Branches,
    verdict: D4RegimeVerdict,
}

fn raw_values(values: DiagnosticPortfolioValues) -> D4PortfolioVector {
    D4PortfolioVector {
        realized_output: values.realized_output,
        voluntary_exchange: values.voluntary_exchange,
        imports: values.imports,
        shortfall: values.shortfall,
        coercive_transfer: values.coercive_transfer.unwrap_or([0.0; 2]),
        protection_access: values.protection_access.unwrap_or([0.0; 2]),
    }
}

fn profile(phase: &DiagnosticPortfolioPhase) -> D4PortfolioProfile {
    D4PortfolioProfile::complete(
        phase.phase,
        raw_values(phase.portfolio),
        phase.mechanism_availability,
    )
    .expect("portfolio values are finite and non-negative")
}

fn add_debt(branches: &mut Branches, debt: D4AxisDebt) {
    if !branches.axis_debt.contains(&debt) {
        branches.axis_debt.push(debt);
    }
}

fn record_witness_debt(branches: &mut Branches, witness: &DiagnosticPortfolioWitness) {
    for availability in witness.phases.iter().flat_map(|phase| {
        [
            phase.mechanism_availability.realized_output,
            phase.mechanism_availability.voluntary_exchange,
            phase.mechanism_availability.imports,
            phase.mechanism_availability.shortfall,
            phase.mechanism_availability.coercive_transfer,
            phase.mechanism_availability.protection_access,
        ]
    }) {
        if let D4Availability::Debt(debt) = availability {
            add_debt(branches, debt);
        }
    }
}

fn mechanism_class(raw: &D4PortfolioVector) -> D4MechanismClass {
    let voluntary = raw.voluntary_exchange.iter().any(|value| *value > 0.0);
    let coercive = raw.coercive_transfer.iter().any(|value| *value > 0.0);
    match (voluntary, coercive) {
        (true, true) => D4MechanismClass::Mixed,
        (true, false) => D4MechanismClass::Voluntary,
        (false, true) => D4MechanismClass::CoerciveOnly,
        (false, false) => D4MechanismClass::Unavailable,
    }
}

fn summarize(input: &Input) -> Report {
    let mut branches = Branches::default();
    if input.treatment != ExchangeTreatment::Enabled {
        add_debt(&mut branches, D4AxisDebt::Mechanism);
    }
    let mut live = BTreeMap::<BakeId, Vec<Vertex>>::new();
    let mut witnesses = BTreeMap::<BakeId, Vec<&DiagnosticPortfolioWitness>>::new();
    let mut sources = BTreeMap::<BakeId, Vec<(Vertex, [f64; 2])>>::new();
    for &(community, site) in &input.live {
        live.entry(community).or_default().push(site);
    }
    for witness in &input.witnesses {
        witnesses
            .entry(witness.community)
            .or_default()
            .push(witness);
    }
    for &(community, site, source) in &input.sources {
        sources.entry(community).or_default().push((site, source));
    }
    branches.duplicate_live = live
        .iter()
        .filter_map(|(&id, rows)| (rows.len() != 1).then_some(id))
        .collect();
    branches.duplicate_witness = witnesses
        .iter()
        .filter_map(|(&id, rows)| (rows.len() != 1).then_some(id))
        .collect();
    branches.duplicate_source = sources
        .iter()
        .filter_map(|(&id, rows)| (rows.len() != 1).then_some(id))
        .collect();
    branches.orphan_witness = witnesses
        .keys()
        .filter(|id| !live.contains_key(id))
        .copied()
        .collect();

    let mut joined = Vec::new();
    for (&community, sites) in &live {
        if !sources.contains_key(&community) {
            branches.missing_source.push(community);
            add_debt(&mut branches, D4AxisDebt::Source);
        }
        if sites.len() != 1 {
            continue;
        }
        let Some(rows) = witnesses.get(&community) else {
            branches.missing_witness.push(community);
            add_debt(&mut branches, D4AxisDebt::Temporal);
            continue;
        };
        for witness in rows {
            record_witness_debt(&mut branches, witness);
        }
        let Some(source_rows) = sources.get(&community) else {
            continue;
        };
        if rows.len() != 1 || source_rows.len() != 1 {
            continue;
        }
        let witness = rows[0];
        let (source_site, source) = source_rows[0];
        if witness.site != sites[0] || source_site != sites[0] {
            branches.site_mismatch.push(community);
            continue;
        }
        let profiles: Vec<_> = witness.phases.iter().map(profile).collect();
        if profiles.is_empty()
            || profiles
                .iter()
                .any(|p| p.completeness != D4Completeness::Complete)
        {
            branches.incomplete.push(community);
            add_debt(&mut branches, D4AxisDebt::Temporal);
            add_debt(&mut branches, D4AxisDebt::Mechanism);
        }
        joined.push(Joined {
            community,
            site: sites[0],
            profiles,
            source,
        });
    }

    let adequate: Vec<_> = joined
        .iter()
        .filter(|unit| {
            !unit.profiles.is_empty()
                && unit
                    .profiles
                    .iter()
                    .all(|p| p.completeness == D4Completeness::Complete)
        })
        .collect();
    let mut regime_groups = BTreeMap::<Vec<String>, usize>::new();
    for unit in &adequate {
        let signature = unit
            .profiles
            .iter()
            .filter_map(d4_profile_signature)
            .map(|signature| format!("{signature:?}"))
            .collect::<Vec<_>>();
        *regime_groups.entry(signature).or_default() += 1;
    }
    let distinct = regime_groups.values().filter(|count| **count >= 2).count();
    let recurrence = if adequate.len() < 2 {
        D4RecurrenceClass::Incomplete
    } else {
        let recurrences: Vec<_> = adequate
            .iter()
            .map(|unit| d4_recurrence_class(&unit.profiles))
            .collect();
        if recurrences
            .iter()
            .all(|recurrence| *recurrence == D4RecurrenceClass::PersistentCandidate)
        {
            D4RecurrenceClass::PersistentCandidate
        } else if recurrences.iter().all(|recurrence| {
            matches!(
                recurrence,
                D4RecurrenceClass::Seasonal | D4RecurrenceClass::PersistentCandidate
            )
        }) {
            D4RecurrenceClass::Seasonal
        } else if recurrences.iter().all(|recurrence| {
            matches!(
                recurrence,
                D4RecurrenceClass::Transient | D4RecurrenceClass::Drifting
            )
        }) {
            D4RecurrenceClass::Transient
        } else if recurrences.contains(&D4RecurrenceClass::Rotating) {
            D4RecurrenceClass::Rotating
        } else if recurrences.contains(&D4RecurrenceClass::Drifting) {
            D4RecurrenceClass::Drifting
        } else {
            D4RecurrenceClass::Incomplete
        }
    };
    let evidence = D4RegimeEvidence {
        adequate_communities: adequate.len(),
        distinct_regimes: distinct,
        source_or_access_varies: adequate
            .first()
            .is_some_and(|first| adequate.iter().any(|unit| unit.source != first.source)),
        differentiation_is_vacuous: adequate.iter().any(|unit| {
            unit.profiles
                .iter()
                .any(|profile| profile.raw.coercive_transfer != [0.0; 2])
        }),
        recurrence,
        some_units_underpowered: !branches.incomplete.is_empty()
            || !branches.missing_witness.is_empty()
            || !branches.missing_source.is_empty()
            || !branches.duplicate_source.is_empty(),
    };
    let invalid_join = input.denominator != input.live.len() as u64
        || !branches.duplicate_live.is_empty()
        || !branches.duplicate_witness.is_empty()
        || !branches.duplicate_source.is_empty()
        || !branches.missing_witness.is_empty()
        || !branches.missing_source.is_empty()
        || !branches.orphan_witness.is_empty()
        || !branches.site_mismatch.is_empty();
    let verdict = if input.treatment != ExchangeTreatment::Enabled || invalid_join {
        D4RegimeVerdict::MixedOrUnderpowered
    } else {
        d4_regime_verdict(evidence)
    };
    Report {
        seed: input.seed,
        denominator: input.denominator,
        joined,
        branches,
        verdict,
    }
}

fn fixture(phases: &[u16]) -> Input {
    let witnesses = (0..4)
        .map(|i| DiagnosticPortfolioWitness {
            community: BakeId(i),
            site: Vertex(i as u32),
            phases: phases
                .iter()
                .map(|&phase| DiagnosticPortfolioPhase {
                    phase,
                    portfolio: DiagnosticPortfolioValues {
                        realized_output: if i < 2 { [3.0, 1.0] } else { [1.0, 3.0] },
                        voluntary_exchange: if i < 2 { [1.0, 0.0] } else { [0.0, 1.0] },
                        imports: if i < 2 { [0.0, 1.0] } else { [1.0, 0.0] },
                        shortfall: [0.0; 2],
                        coercive_transfer: Some([0.0; 2]),
                        protection_access: Some([0.0; 2]),
                    },
                    exchange_attempts: Vec::new(),
                    mechanism_availability: D4MechanismAvailability::all_available(),
                })
                .collect(),
        })
        .collect();
    Input {
        seed: 1,
        denominator: 4,
        live: (0..4).map(|i| (BakeId(i), Vertex(i as u32))).collect(),
        witnesses,
        sources: (0..4)
            .map(|i| {
                (
                    BakeId(i),
                    Vertex(i as u32),
                    if i < 2 { [0.2, 0.8] } else { [0.8, 0.2] },
                )
            })
            .collect(),
        treatment: ExchangeTreatment::Enabled,
    }
}

fn enabled_build(seed: u64, components: &WorldComponents) -> ExchangeTreatmentBuild {
    build_world_with_exchange_treatment(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        components,
        ExchangeTreatment::Enabled,
    )
    .expect("fixed enabled-treatment seed builds")
}

fn input_from_build(seed: u64, built: &ExchangeTreatmentBuild) -> Input {
    let live: Vec<_> = built
        .history
        .records
        .iter()
        .filter(|record| record.core.is_alive())
        .map(|record| (record.community, record.core.site))
        .collect();
    // No phase-resolved source/access observation exists yet. Do not inject a
    // constant source vector: the live branch must remain explicitly
    // underpowered until an existing observation can support this axis.
    let sources = Vec::new();
    Input {
        seed,
        denominator: census(&built.history).alive_at_now,
        live,
        witnesses: built.history.diagnostic_portfolios.clone(),
        sources,
        treatment: ExchangeTreatment::Enabled,
    }
}

fn emitted_ledger_bytes(history: &History) -> Vec<u8> {
    let mut world = World::new(Seed(42));
    hornvale_history::register_concepts(&mut world.registry).expect("history concepts register");
    hornvale_settlement::register_concepts(&mut world.registry)
        .expect("settlement concepts register");
    hornvale_epidemiology::register_concepts(&mut world.registry)
        .expect("epidemiology concepts register");
    emit_history(&mut world, history).expect("history emits");
    serde_json::to_vec(&world.ledger).expect("ledger serializes")
}

#[test]
fn one_window_is_transient_same_phase_is_seasonal_and_cross_phase_is_persistent() {
    assert_eq!(
        summarize(&fixture(&[0])).verdict,
        D4RegimeVerdict::TransientContrast
    );
    assert_eq!(
        summarize(&fixture(&[0, 0])).verdict,
        D4RegimeVerdict::SeasonalRegime
    );
    assert_eq!(
        summarize(&fixture(&[0, 1])).verdict,
        D4RegimeVerdict::PersistentCandidate
    );
}

#[test]
fn typed_dependencies_and_mechanisms_remain_distinct() {
    let mut input = fixture(&[0, 0]);
    input.witnesses[0].phases[0].portfolio.voluntary_exchange = [0.0; 2];
    input.witnesses[0].phases[0].portfolio.coercive_transfer = Some([2.0, 0.0]);
    let phase = profile(&input.witnesses[0].phases[0]);
    assert_eq!(phase.raw.voluntary_exchange, [0.0; 2]);
    assert_eq!(phase.raw.coercive_transfer, [2.0, 0.0]);
    assert_eq!(mechanism_class(&phase.raw), D4MechanismClass::CoerciveOnly);
}

#[test]
fn missing_mechanisms_and_vacuity_cannot_clear_positive_branch() {
    let mut missing = fixture(&[0, 0]);
    missing.witnesses[0].phases[0].portfolio.protection_access = None;
    missing.witnesses[0].phases[0]
        .mechanism_availability
        .protection_access = D4Availability::Debt(D4AxisDebt::Mechanism);
    let report = summarize(&missing);
    assert!(report.branches.axis_debt.contains(&D4AxisDebt::Mechanism));
    assert_eq!(report.verdict, D4RegimeVerdict::MixedOrUnderpowered);
    let mut zero = fixture(&[0, 0]);
    for witness in &mut zero.witnesses {
        for phase in &mut witness.phases {
            phase.portfolio = DiagnosticPortfolioValues::default();
        }
    }
    assert_ne!(summarize(&zero).verdict, D4RegimeVerdict::SeasonalRegime);
    assert_ne!(
        summarize(&zero).verdict,
        D4RegimeVerdict::PersistentCandidate
    );
}

#[test]
fn duplicate_missing_disabled_and_site_mismatch_inputs_are_explicit() {
    let mut duplicate = fixture(&[0, 0]);
    duplicate.live.push((BakeId(0), Vertex(0)));
    assert_eq!(
        summarize(&duplicate).verdict,
        D4RegimeVerdict::MixedOrUnderpowered
    );
    let mut disabled = fixture(&[0, 0]);
    disabled.treatment = ExchangeTreatment::Disabled;
    assert_eq!(
        summarize(&disabled).verdict,
        D4RegimeVerdict::MixedOrUnderpowered
    );
    let mut mismatch = fixture(&[0, 0]);
    mismatch.witnesses[0].site = Vertex(99);
    assert_eq!(
        summarize(&mismatch).verdict,
        D4RegimeVerdict::MixedOrUnderpowered
    );
    let mut missing_source = fixture(&[0, 0]);
    missing_source.sources.pop();
    let missing_source_report = summarize(&missing_source);
    assert_eq!(
        missing_source_report.verdict,
        D4RegimeVerdict::MixedOrUnderpowered
    );
    assert_eq!(missing_source_report.branches.missing_source.len(), 1);
    let mut duplicate_source = fixture(&[0, 0]);
    duplicate_source.sources.push(duplicate_source.sources[0]);
    let duplicate_source_report = summarize(&duplicate_source);
    assert_eq!(
        duplicate_source_report.verdict,
        D4RegimeVerdict::MixedOrUnderpowered
    );
    assert_eq!(duplicate_source_report.branches.duplicate_source.len(), 1);
}

#[test]
fn recurrence_and_contrast_must_share_the_same_communities() {
    let mut input = fixture(&[0, 0]);
    for witness in input.witnesses.iter_mut().take(2) {
        witness.phases.pop();
    }
    let report = summarize(&input);
    assert_eq!(report.verdict, D4RegimeVerdict::MixedOrUnderpowered);
}

#[test]
fn live_sidecar_is_deterministic_and_save_inert() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let first = enabled_build(11, &components);
    let second = enabled_build(11, &components);
    assert_eq!(
        first.history.diagnostic_portfolios,
        second.history.diagnostic_portfolios
    );
    let report = summarize(&input_from_build(11, &first));
    assert_eq!(
        report.denominator as usize,
        report.joined.len() + report.branches.missing_source.len()
    );
    assert_eq!(report.verdict, D4RegimeVerdict::MixedOrUnderpowered);
    let mut without_sidecar = first.history.clone();
    without_sidecar.diagnostic_portfolios.clear();
    assert_eq!(
        emitted_ledger_bytes(&first.history),
        emitted_ledger_bytes(&without_sidecar)
    );
}

#[test]
fn equal_normalized_profiles_with_different_raw_scale_are_not_distinct_by_scale() {
    let mut input = fixture(&[0, 0]);
    for phase in &mut input.witnesses[1].phases {
        phase.portfolio.realized_output = [30.0, 10.0];
        phase.portfolio.voluntary_exchange = [10.0, 0.0];
        phase.portfolio.imports = [0.0, 10.0];
    }
    let report = summarize(&input);
    assert_eq!(report.verdict, D4RegimeVerdict::SeasonalRegime);
    assert_eq!(
        d4_profile_signature(&report.joined[0].profiles[0]),
        d4_profile_signature(&report.joined[1].profiles[0])
    );
}

#[test]
fn tribute_only_evidence_is_mixed_not_voluntary_specialization() {
    let mut input = fixture(&[0, 0]);
    for witness in &mut input.witnesses {
        for phase in &mut witness.phases {
            phase.portfolio.voluntary_exchange = [0.0; 2];
            phase.portfolio.coercive_transfer = Some([1.0, 0.0]);
        }
    }
    assert_eq!(
        summarize(&input).verdict,
        D4RegimeVerdict::VacuousDifferentiation
    );
    input.witnesses[0].phases[0].portfolio.coercive_transfer = Some([0.0; 2]);
    assert_eq!(
        summarize(&input).verdict,
        D4RegimeVerdict::VacuousDifferentiation
    );
    input.witnesses[1].phases[0].portfolio.voluntary_exchange = [1.0, 0.0];
    assert_eq!(
        summarize(&input).verdict,
        D4RegimeVerdict::VacuousDifferentiation
    );
}

/// claim: sanctioned-sweep(forall-seed 1..=200, ignored:) — fixed-roster D4
/// portfolio report, run only through the sanctioned campaign gate.
#[ignore = "probe: fixed 200-seed D4 report; run only through the sanctioned campaign gate"]
#[test]
fn fixed_200_seed_d4_portfolio_regime_report() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let reports: Vec<_> = PROBE_SEEDS
        .map(|seed| summarize(&input_from_build(seed, &enabled_build(seed, &components))))
        .collect();
    assert_eq!(reports.len(), PROBE_WORLD_DENOMINATOR);
    for report in &reports {
        println!(
            "seed={} denominator={} joined={} verdict={:?} axis_debt={:?}",
            report.seed,
            report.denominator,
            report.joined.len(),
            report.verdict,
            report.branches.axis_debt
        );
    }
    let mut totals = BTreeMap::<String, usize>::new();
    for report in reports {
        *totals.entry(format!("{:?}", report.verdict)).or_default() += 1;
    }
    println!("D4 pooled descriptive verdict totals: {totals:?}");
}
