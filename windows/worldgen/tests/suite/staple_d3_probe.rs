//! The Staple D3 diagnostic-witness contract and Task 0 readout.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::Function;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    DiagnosticReturnBand, DiagnosticReturnClass, History, SettlementPins, WorldComponents,
    classify_diagnostic_return, emit_history, history_for,
};

const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct ReturnBands {
    low: usize,
    medium: usize,
    high: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct ProbeReport {
    relations_total: usize,
    d3_function_relations: usize,
    d3_ratio: f64,
    return_bands: ReturnBands,
    conservation_residuals: Vec<f64>,
}

impl ProbeReport {
    fn from_history(history: &History) -> Self {
        assert!(
            !history.tribute.is_empty(),
            "D3 ratio requires a non-zero relation denominator"
        );
        assert_eq!(
            history.diagnostic_returns.len(),
            history.tribute.len(),
            "every standing relation must have exactly one diagnostic return witness"
        );

        let mut return_bands = ReturnBands::default();
        let mut d3_function_relations = 0;
        let mut conservation_residuals = Vec::with_capacity(history.diagnostic_returns.len());
        for witness in &history.diagnostic_returns {
            match witness.band {
                DiagnosticReturnBand::Low => return_bands.low += 1,
                DiagnosticReturnBand::Medium => return_bands.medium += 1,
                DiagnosticReturnBand::High => return_bands.high += 1,
            }
            if witness.classification != DiagnosticReturnClass::None {
                d3_function_relations += 1;
            }
            conservation_residuals.push(witness.conservation_residual);
        }

        let relations_total = history.tribute.len();
        Self {
            relations_total,
            d3_function_relations,
            d3_ratio: d3_function_relations as f64 / relations_total as f64,
            return_bands,
            conservation_residuals,
        }
    }
}

fn history(seed: u64, components: &WorldComponents) -> History {
    history_for(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        components,
    )
    .expect("fixed probe seed builds")
}

fn emitted_ledger_bytes(history: &History) -> Vec<u8> {
    let mut world = World::new(Seed(42));
    hornvale_history::register_concepts(&mut world.registry).expect("history concepts register");
    hornvale_settlement::register_concepts(&mut world.registry)
        .expect("settlement concepts register");
    emit_history(&mut world, history).expect("history emits");
    serde_json::to_vec(&world.ledger).expect("ledger serializes")
}

#[test]
fn diagnostic_classification_uses_continuous_components_with_explicit_precedence() {
    assert_eq!(
        classify_diagnostic_return(0.25, 1.0, 1.0),
        DiagnosticReturnClass::Fort,
        "protection wins the recorded Fort > Trade > Cult precedence"
    );
    assert_eq!(
        classify_diagnostic_return(0.24, 1.0, 1.0),
        DiagnosticReturnClass::Trade,
        "a continuous protection change exposes the next qualifying component"
    );
    assert_eq!(
        classify_diagnostic_return(0.24, 0.99, 1.0),
        DiagnosticReturnClass::Cult,
        "a continuous goods change exposes the legitimacy classification"
    );
    assert_eq!(
        classify_diagnostic_return(0.24, 0.99, 0.99),
        DiagnosticReturnClass::None,
        "no continuous component over its named threshold is inactive"
    );
}

#[test]
fn diagnostic_witness_covers_each_standing_relation_in_identity_order() {
    let components = WorldComponents::assemble().expect("components assemble");
    let history = history(42, &components);

    assert!(
        !history.tribute.is_empty(),
        "fixture must have standing relations"
    );
    assert_eq!(history.diagnostic_returns.len(), history.tribute.len());
    for (relation, witness) in history.tribute.iter().zip(&history.diagnostic_returns) {
        assert_eq!(witness.subordinate, relation.subordinate);
        assert_eq!(witness.patron, relation.patron);
    }
}

#[test]
fn same_seed_emits_the_same_diagnostic_witness() {
    let components = WorldComponents::assemble().expect("components assemble");
    let first = history(42, &components);
    let second = history(42, &components);

    assert_eq!(first.diagnostic_returns, second.diagnostic_returns);
}

#[test]
fn diagnostic_sidecar_does_not_change_save_facing_history_or_endpoint_label_readout() {
    let components = WorldComponents::assemble().expect("components assemble");
    let with_sidecar = history(42, &components);
    let report = ProbeReport::from_history(&with_sidecar);
    let mut without_sidecar = with_sidecar.clone();
    without_sidecar.diagnostic_returns.clear();

    assert_eq!(
        emitted_ledger_bytes(&with_sidecar),
        emitted_ledger_bytes(&without_sidecar),
        "diagnostic witness must not change emitted save-facing History"
    );

    let mut relabeled = with_sidecar.clone();
    for record in &mut relabeled.records {
        record.core.function = Function::Fort;
    }
    assert_eq!(
        ProbeReport::from_history(&relabeled),
        report,
        "endpoint Function labels are not diagnostic-return inputs"
    );
}

#[test]
/// claim: readout(fixed-seed-roster: 1..=200) — counts only emitted,
/// relation-local D3 classifications and reports the diagnostic no-outflow
/// residuals beside their magnitude bands.
#[ignore = "probe: one fixed 200-seed Staple D3 diagnostic return report; run exactly once"]
fn fixed_200_seed_d3_relation_return_report() {
    let components = WorldComponents::assemble().expect("components assemble");
    let reports: Vec<_> = PROBE_SEEDS
        .map(|seed| {
            let history = history(seed, &components);
            assert!(
                !history.tribute.is_empty(),
                "seed {seed} has no D3 relation denominator"
            );
            ProbeReport::from_history(&history)
        })
        .collect();

    let relations_total = reports
        .iter()
        .map(|report| report.relations_total)
        .sum::<usize>();
    assert!(
        relations_total > 0,
        "D3 ratio requires a non-zero relation denominator"
    );
    let d3_function_relations = reports
        .iter()
        .map(|report| report.d3_function_relations)
        .sum::<usize>();
    let d3_ratio = reports
        .iter()
        .map(|report| report.d3_ratio * report.relations_total as f64)
        .sum::<f64>()
        / relations_total as f64;
    let return_bands = reports
        .iter()
        .fold(ReturnBands::default(), |mut total, report| {
            total.low += report.return_bands.low;
            total.medium += report.return_bands.medium;
            total.high += report.return_bands.high;
            total
        });
    let conservation_residuals: Vec<_> = reports
        .iter()
        .flat_map(|report| report.conservation_residuals.iter().copied())
        .collect();
    let nonzero_residuals = conservation_residuals
        .iter()
        .filter(|residual| **residual != 0.0)
        .count();
    let max_abs_residual = conservation_residuals
        .iter()
        .map(|residual| residual.abs())
        .fold(0.0f64, f64::max);

    println!("D3 diagnostic return report");
    println!("N={relations_total}");
    println!("C={d3_function_relations}");
    println!("C/N={d3_function_relations}/{relations_total}");
    println!("d3_ratio={d3_ratio:.17}");
    println!(
        "return_bands low={} medium={} high={}",
        return_bands.low, return_bands.medium, return_bands.high
    );
    println!(
        "conservation_residuals nonzero={nonzero_residuals}/{} max_abs={max_abs_residual:.17}",
        conservation_residuals.len()
    );
    println!("dead_pole_sink={}", d3_function_relations == 0);
    println!(
        "dead_pole_uniform={}",
        d3_function_relations == relations_total
    );

    assert_eq!(
        return_bands.low + return_bands.medium + return_bands.high,
        relations_total,
        "each standing relation occupies exactly one diagnostic magnitude band"
    );
    assert_eq!(
        nonzero_residuals, 0,
        "the diagnostic witness must not clear or create stock"
    );
}
