//! The Murrain's preregistered H-P1..H-P6 readout.
//!
//! The predictions are counts over nine named worlds (and 200 named Lot
//! indices in each), never ratios. The ignored readout prints every count and
//! verdict once for the campaign ledger. The ordinary boundary test keeps the
//! frozen count predicates executable without turning a falsified prediction
//! into a red gate.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lab::{Extractor, FullView, MetricValue, registry};

const SEEDS: [u64; 9] = [1, 2, 3, 7, 13, 42, 100, 256, 777];
const METRICS: [&str; 6] = [
    "epidemic-largest-metapopulation-now",
    "epidemic-crowd-endemic",
    "epidemic-plague-endings",
    "epidemic-outbreak-events",
    "lot-named-disease-deaths",
    "lot-slots-filled-mean",
];

const CENSUS_WORLDS: f64 = 1_000.0;
const CENSUS_CPU_RATIO: f64 = 30.70;
const CENSUS_BASELINE_SECONDS: f64 = 1_186.0;
const RECORDED_CPU_SECONDS_PER_WORLD: f64 = 0.944;
const RECORDED_PROJECTED_CENSUS_SECONDS: f64 = 1_216.749;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Reading {
    seed: u64,
    largest_metapopulation: f64,
    crowd_endemic: bool,
    plague_endings: u64,
    outbreak_events: u64,
    first_day_occ_cause_plague: Option<f64>,
    named_disease_deaths: u64,
    mean_filled_slots: f64,
}

fn extract(view: &FullView, name: &str) -> MetricValue {
    let metric = registry()
        .into_iter()
        .find(|metric| metric.name == name)
        .unwrap_or_else(|| panic!("Murrain metric {name} is registered"));
    match metric.extract {
        Extractor::Full(extract) => extract(view),
        _ => panic!("Murrain metric {name} must use the shared FullView"),
    }
}

fn number(view: &FullView, name: &str) -> f64 {
    match extract(view, name) {
        MetricValue::Number(value) => value,
        other => panic!("Murrain metric {name} must be numeric, got {other:?}"),
    }
}

fn count(view: &FullView, name: &str) -> u64 {
    let value = number(view, name);
    assert_eq!(
        value.fract(),
        0.0,
        "Murrain metric {name} is a count, not a ratio"
    );
    value as u64
}

fn reading(seed: u64) -> Reading {
    let view = FullView::build(Seed(seed), &SkyPins::default())
        .unwrap_or_else(|error| panic!("seed {seed} builds: {error:?}"));
    Reading {
        seed,
        largest_metapopulation: number(&view, METRICS[0]),
        crowd_endemic: match extract(&view, METRICS[1]) {
            MetricValue::Flag(value) => value,
            other => panic!("{} must be a flag, got {other:?}", METRICS[1]),
        },
        plague_endings: count(&view, METRICS[2]),
        outbreak_events: count(&view, METRICS[3]),
        first_day_occ_cause_plague: match extract(&view, "first-day-occ-cause-plague") {
            MetricValue::Number(value) => Some(value),
            MetricValue::Absent => None,
            other => panic!("first-day-occ-cause-plague must be numeric or absent, got {other:?}"),
        },
        named_disease_deaths: count(&view, METRICS[4]),
        mean_filled_slots: number(&view, METRICS[5]),
    }
}

fn consumption_ccs() -> f64 {
    let registry = hornvale_species::pathogen_registry();
    let (_, traits) = registry
        .iter()
        .find(|(kind, _)| kind.0 == "the-consumption")
        .expect("the frozen catalogue carries the-consumption");
    hornvale_epidemiology::critical_community_size(
        traits.r0.expect("the-consumption has R0"),
        traits
            .infectious_years
            .expect("the-consumption has an infectious period"),
        1.0 / 30.0,
    )
}

fn verdicts(rows: &[Reading]) -> [bool; 6] {
    let ccs = consumption_ccs();
    let consumption_endemic = |row: &&Reading| row.largest_metapopulation >= ccs;
    let growing = rows.iter().filter(|row| row.seed != 100);
    let consumption_count = rows.iter().filter(consumption_endemic).count();
    [
        rows.len() == SEEDS.len() && rows.iter().filter(|row| row.crowd_endemic).count() == 0,
        (4..=7).contains(&consumption_count)
            && rows
                .iter()
                .filter(|row| row.seed == 100 || row.seed == 256)
                .all(|row| !consumption_endemic(&row)),
        growing
            .clone()
            .all(|row| (5..=60).contains(&row.plague_endings))
            && growing
                .clone()
                .all(|row| row.first_day_occ_cause_plague.is_some())
            && rows
                .iter()
                .find(|row| row.seed == 100)
                .is_some_and(|row| row.plague_endings <= 8),
        growing
            .clone()
            .all(|row| (40..=400).contains(&row.outbreak_events))
            && rows
                .iter()
                .find(|row| row.seed == 100)
                .is_some_and(|row| row.outbreak_events <= 60),
        rows.iter()
            .all(|row| (40..=160).contains(&row.named_disease_deaths)),
        rows.iter().all(|row| row.mean_filled_slots >= 15.0),
    ]
}

fn passing_rows() -> Vec<Reading> {
    SEEDS
        .into_iter()
        .enumerate()
        .map(|(index, seed)| Reading {
            seed,
            largest_metapopulation: if index < 4 {
                consumption_ccs() + 1.0
            } else {
                consumption_ccs() - 1.0
            },
            crowd_endemic: false,
            plague_endings: if seed == 100 { 8 } else { 5 },
            outbreak_events: if seed == 100 { 60 } else { 40 },
            first_day_occ_cause_plague: Some(1.0),
            named_disease_deaths: 40,
            mean_filled_slots: 15.0,
        })
        .collect()
}

fn projected_census_seconds(cpu_seconds_per_world: f64) -> f64 {
    CENSUS_BASELINE_SECONDS + cpu_seconds_per_world * CENSUS_WORLDS / CENSUS_CPU_RATIO
}

#[test]
fn recorded_cost_projection_is_below_canonical_census_limits() {
    let projected = projected_census_seconds(RECORDED_CPU_SECONDS_PER_WORLD);
    assert!((projected - RECORDED_PROJECTED_CENSUS_SECONDS).abs() < 0.001);
    assert!(projected < hornvale_lab::census_guard::CENSUS_ALARM_SECS);
    assert!(projected < hornvale_lab::census_guard::CENSUS_REFUSAL_SECS);
}

#[test]
fn isolated_cost_projection_is_compared_to_alarm_and_refusal_limits() {
    let alarm_headroom_per_world = (hornvale_lab::census_guard::CENSUS_ALARM_SECS
        - CENSUS_BASELINE_SECONDS)
        * CENSUS_CPU_RATIO
        / CENSUS_WORLDS;
    let refusal_headroom_per_world = (hornvale_lab::census_guard::CENSUS_REFUSAL_SECS
        - CENSUS_BASELINE_SECONDS)
        * CENSUS_CPU_RATIO
        / CENSUS_WORLDS;

    assert_eq!(projected_census_seconds(0.0), CENSUS_BASELINE_SECONDS);
    assert!(
        projected_census_seconds(alarm_headroom_per_world - 1.0e-6)
            < hornvale_lab::census_guard::CENSUS_ALARM_SECS
    );
    assert!(
        projected_census_seconds(alarm_headroom_per_world + 1.0e-6)
            > hornvale_lab::census_guard::CENSUS_ALARM_SECS
    );
    assert!(
        projected_census_seconds(refusal_headroom_per_world - 1.0e-6)
            < hornvale_lab::census_guard::CENSUS_REFUSAL_SECS
    );
    assert!(
        projected_census_seconds(refusal_headroom_per_world + 1.0e-6)
            > hornvale_lab::census_guard::CENSUS_REFUSAL_SECS
    );
}

#[test]
fn prediction_verdicts_use_preregistered_counts_not_ratios() {
    let rows = passing_rows();
    assert_eq!(verdicts(&rows), [true; 6]);

    let mut below = rows.clone();
    below[0].plague_endings = 4;
    below[4].first_day_occ_cause_plague = None;
    below[1].outbreak_events = 39;
    below[2].named_disease_deaths = 39;
    below[3].mean_filled_slots = 14.99;
    let result = verdicts(&below);
    assert!(!result[2], "H-P3 rejects four endings, not a ratio");
    assert!(!result[3], "H-P4 rejects 39 events, not a ratio");
    assert!(!result[4], "H-P5 rejects 39 named deaths, not a ratio");
    assert!(!result[5], "H-P6 rejects a mean count below 15 of 23");
}

#[test]
fn h_p3_requires_first_plague_cause_on_every_growing_seed() {
    let mut rows = passing_rows();
    rows[0].first_day_occ_cause_plague = None;
    assert!(
        !verdicts(&rows)[2],
        "H-P3 must not pass from Plague-ending counts alone"
    );
}

#[test]
fn one_full_view_repeats_the_six_metric_readings_byte_for_byte() {
    let view = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    let first: Vec<MetricValue> = METRICS.iter().map(|name| extract(&view, name)).collect();
    let second: Vec<MetricValue> = METRICS.iter().map(|name| extract(&view, name)).collect();
    assert_eq!(first, second);
}

/// A process-cost control for the isolated era derivation measurement below:
/// it builds the same FullView and touches the same inputs, but performs no
/// graph, population, or ecological-substrate derivation.
#[test]
#[ignore = "probe: control for the Murrain isolated era-derivation cost measurement"]
fn murrain_era_derivation_cost_control() {
    let view = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    for _ in 0..10 {
        std::hint::black_box((view.world(), view.terrain(), view.climate()));
    }
}

/// Measures only the three era products the Murrain metrics and Lot context
/// share. World construction is outside the internal clock; the paired
/// process-level control above lets `/usr/bin/time -lp` subtract its CPU cost.
#[test]
#[ignore = "probe: the Murrain era-graph/substrate derivation cost is measured in isolation"]
#[allow(
    clippy::disallowed_types,
    reason = "Instant times an ignored out-of-sim cost probe; it never enters world state (decision 0001)"
)]
fn murrain_era_derivation_cost() {
    let view = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    let started = std::time::Instant::now();
    for _ in 0..10 {
        let graphs =
            hornvale_worldgen::bake_era_graphs_from(view.world(), view.terrain(), view.climate())
                .expect("era graphs derive");
        let population = hornvale_worldgen::bake_era_population_view_from(view.world(), &graphs);
        let substrates = hornvale_worldgen::bake_era_substrates_from(
            view.world(),
            view.terrain(),
            view.climate(),
        )
        .expect("era substrates derive");
        std::hint::black_box((graphs, population, substrates));
    }
    let elapsed = started.elapsed().as_secs_f64();
    println!(
        "murrain-era-derivation reps=10 isolated-wall-seconds={elapsed:.6} wall-seconds-per-world={:.6}",
        elapsed / 10.0
    );
}

/// claim: readout(off-gate, prints counts and verdicts, no prediction
/// assertion) — The Murrain H-P1..H-P6 over the nine frozen seeds and 200
/// lots per seed. A failed verdict is a finding and is recorded unchanged.
#[test]
#[ignore = "probe: The Murrain H-P1..H-P6 nine-seed readout costs nine full worlds; run once by hand"]
fn murrain_readout() {
    let rows: Vec<Reading> = SEEDS.into_iter().map(reading).collect();
    let ccs = consumption_ccs();
    for row in &rows {
        println!(
            "seed {:>3}: largest-now={:>8.3} crowd-endemic={} consumption-endemic={} plague-endings={} first-day-occ-cause-plague={} outbreak-events={} named-disease-deaths={}/200 slots-filled-mean={:.3}/23",
            row.seed,
            row.largest_metapopulation,
            row.crowd_endemic,
            row.largest_metapopulation >= ccs,
            row.plague_endings,
            row.first_day_occ_cause_plague
                .map_or_else(|| "Absent".to_string(), |day| format!("{day:.3}")),
            row.outbreak_events,
            row.named_disease_deaths,
            row.mean_filled_slots,
        );
    }
    for (index, pass) in verdicts(&rows).into_iter().enumerate() {
        println!("H-P{}: {}", index + 1, if pass { "PASS" } else { "FAIL" });
    }
}
