use hornvale_history::record::Function;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, WorldComponents, history_for};

const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RelationObservation {
    subordinate: Function,
    patron: Function,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ReturnBand;

#[derive(Clone, Debug, PartialEq)]
struct ProbeReport {
    relations_total: usize,
    d3_function_relations: usize,
    d3_ratio: f64,
    return_bands: Vec<ReturnBand>,
    conservation_residuals: Vec<f64>,
}

impl ProbeReport {
    fn from_observations(observations: Vec<RelationObservation>) -> Self {
        let relations_total = observations.len();
        let d3_function_relations = observations
            .iter()
            .filter(|relation| {
                is_d3_function(relation.subordinate) && is_d3_function(relation.patron)
            })
            .count();
        assert!(
            relations_total > 0,
            "D3 ratio requires a non-zero relation denominator"
        );
        Self {
            relations_total,
            d3_function_relations,
            d3_ratio: d3_function_relations as f64 / relations_total as f64,
            return_bands: Vec::new(),
            conservation_residuals: Vec::new(),
        }
    }
}

fn is_d3_function(function: Function) -> bool {
    matches!(function, Function::Trade | Function::Cult | Function::Fort)
}

/// Scaffold only: the live relation return witness and conservation residual
/// are private to `Bake`. `History::tribute` and `BakeOccupation::core`
/// expose the relation endpoints, but not those requested per-relation values.
fn report_for_history(history: &hornvale_worldgen::History) -> ProbeReport {
    let functions = history
        .records
        .iter()
        .map(|record| (record.community, record.core.function))
        .collect::<std::collections::BTreeMap<_, _>>();
    let observations = history
        .tribute
        .iter()
        .map(|relation| RelationObservation {
            subordinate: *functions
                .get(&relation.subordinate)
                .expect("tribute subordinate has a bake occupation"),
            patron: *functions
                .get(&relation.patron)
                .expect("tribute patron has a bake occupation"),
        })
        .collect();
    ProbeReport::from_observations(observations)
}

#[test]
fn d3_report_restricts_function_relations_to_the_preregistered_set() {
    let report = ProbeReport::from_observations(vec![
        RelationObservation {
            subordinate: Function::Trade,
            patron: Function::Cult,
        },
        RelationObservation {
            subordinate: Function::Fort,
            patron: Function::Trade,
        },
    ]);

    assert_eq!(report.relations_total, 2);
    assert_eq!(report.d3_function_relations, 2);
    assert_eq!(report.d3_ratio, 1.0);
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
    let d3_function_relations = reports
        .iter()
        .map(|report| report.d3_function_relations)
        .sum::<usize>();
    assert!(
        relations_total > 0,
        "D3 ratio requires a non-zero relation denominator"
    );
    println!("relations_total={relations_total}");
    println!("d3_function_relations={d3_function_relations}");
    println!(
        "d3_ratio={}",
        d3_function_relations as f64 / relations_total as f64
    );
    println!("return_bands=unavailable-at-integration-test-boundary");
    println!("conservation_residuals=unavailable-at-integration-test-boundary");
}
