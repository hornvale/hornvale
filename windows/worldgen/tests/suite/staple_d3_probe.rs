use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, WorldComponents, history_for};

const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ReturnBand;

#[derive(Clone, Debug, PartialEq)]
struct ProbeReport {
    relations_total: usize,
    d3_function_relations: Option<usize>,
    d3_ratio: Option<f64>,
    return_bands: Vec<ReturnBand>,
    conservation_residuals: Vec<f64>,
}

impl ProbeReport {
    fn from_relation_count(relations_total: usize) -> Self {
        assert!(
            relations_total > 0,
            "D3 ratio requires a non-zero relation denominator"
        );
        Self {
            relations_total,
            d3_function_relations: None,
            d3_ratio: None,
            return_bands: Vec::new(),
            conservation_residuals: Vec::new(),
        }
    }
}

/// Scaffold only: the live relation return witness and conservation residual
/// are private to `Bake`. `History::tribute` exposes the relation count, but
/// not those requested per-relation values.
fn report_for_history(history: &hornvale_worldgen::History) -> ProbeReport {
    ProbeReport::from_relation_count(history.tribute.len())
}

#[test]
fn d3_report_does_not_claim_from_endpoint_labels() {
    let report = ProbeReport::from_relation_count(2);

    assert_eq!(report.relations_total, 2);
    assert_eq!(report.d3_function_relations, None);
    assert_eq!(report.d3_ratio, None);
    assert!(report.return_bands.is_empty());
    assert!(report.conservation_residuals.is_empty());
}

#[test]
/// claim: readout(fixed-seed-roster: 1..=200) — prints the available D3
/// relation readout and records the public-boundary gap for return witnesses.
#[ignore = "probe: D3 relation return scaffold; blocked at the public integration-test boundary"]
fn fixed_200_seed_d3_relation_return_report() {
    let components = WorldComponents::assemble().expect("components assemble");
    let mut reports = Vec::new();
    for seed in PROBE_SEEDS {
        let history = history_for(
            Seed(seed),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &components,
        )
        .expect("fixed probe seed builds");
        let report = report_for_history(&history);
        assert!(
            report.relations_total > 0,
            "seed {seed} has no relation denominator"
        );
        reports.push(report);
    }

    let relations_total = reports
        .iter()
        .map(|report| report.relations_total)
        .sum::<usize>();
    assert!(
        relations_total > 0,
        "D3 ratio requires a non-zero relation denominator"
    );
    println!("relations_total={relations_total}");
    println!("d3_function_relations=unavailable-at-integration-test-boundary");
    println!("d3_ratio=unavailable-at-integration-test-boundary");
    println!("return_bands=unavailable-at-integration-test-boundary");
    println!("conservation_residuals=unavailable-at-integration-test-boundary");
}
