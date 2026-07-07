//! Calibration: at tier 0, belief kind is a pure function of rotation.
//! The instrument must reproduce known ground truth exactly (spec §2.5).
use hornvale_culture::{BiomeClass, subsistence};
use hornvale_lab::{MetricValue, load_study, run};
use std::path::Path;

/// Map a `flagship-biome` metric's kebab-case name back to culture's coarse
/// `BiomeClass`, mirroring `hornvale_worldgen::biome_class`'s grouping. A
/// small duplicate is unavoidable here: the metric reports the biome as a
/// committed Text fact (a `String`), not the `hornvale_climate::Biome` enum
/// that `biome_class` maps from, so the calibration re-derives its
/// expectation from this independent metric column rather than the enum
/// itself — which is the point (spec §10): it checks the committed
/// subsistence fact against biome + coastal, not against its own inputs.
fn biome_class_from_name(name: &str) -> BiomeClass {
    match name {
        "temperate-forest"
        | "temperate-rainforest"
        | "tropical-seasonal-forest"
        | "tropical-rainforest"
        | "taiga" => BiomeClass::Forest,
        "savanna" | "temperate-grassland" => BiomeClass::Grassland,
        "desert" | "shrubland" => BiomeClass::Arid,
        "tundra" => BiomeClass::Cold,
        _ => BiomeClass::Barren,
    }
}

#[test]
fn eternal_beliefs_coincide_exactly_with_tidal_locking() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (locked_i, belief_i) = (idx("tidally-locked"), idx("belief-kind"));
    for row in &result.rows {
        let locked = matches!(row.values[locked_i], MetricValue::Flag(true));
        let eternal = matches!(&row.values[belief_i], MetricValue::Text(t) if t == "eternal");
        assert_eq!(locked, eternal, "seed {}: calibration violated", row.seed);
    }
}

#[test]
fn band_count_matches_the_known_function_of_rotation() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (day_i, band_i) = (idx("day-length-hours"), idx("band-count"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        // Locked worlds report Absent day length and "locked" band count.
        let expected = match &row.values[day_i] {
            MetricValue::Number(hours) => {
                if *hours >= 40.0 {
                    "1".to_string()
                } else if *hours >= 20.0 {
                    "3".to_string()
                } else if *hours >= 10.0 {
                    "5".to_string()
                } else {
                    "7".to_string()
                }
            }
            _ => "locked".to_string(),
        };
        let actual = match &row.values[band_i] {
            MetricValue::Text(t) => t.clone(),
            other => panic!("seed {}: band-count not text: {other:?}", row.seed),
        };
        assert_eq!(
            actual, expected,
            "seed {}: band-count calibration violated",
            row.seed
        );
    }
}

#[test]
fn flagship_subsistence_matches_biome_and_coastal_columns() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (subsistence_i, biome_i, coastal_i) = (
        idx("flagship-subsistence"),
        idx("flagship-biome"),
        idx("flagship-coastal"),
    );
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        // Absent means no flagship (or no committed subsistence) in this
        // world; nothing to calibrate.
        let MetricValue::Text(actual) = &row.values[subsistence_i] else {
            continue;
        };
        let biome = match &row.values[biome_i] {
            MetricValue::Text(b) => b,
            other => panic!("seed {}: flagship-biome not text: {other:?}", row.seed),
        };
        let coastal = match &row.values[coastal_i] {
            MetricValue::Flag(c) => *c,
            other => panic!("seed {}: flagship-coastal not a flag: {other:?}", row.seed),
        };
        let class = biome_class_from_name(biome);
        let expected = subsistence(class, coastal).name();
        assert_eq!(
            actual, expected,
            "seed {}: subsistence-biome calibration violated (biome={}, coastal={})",
            row.seed, biome, coastal
        );
    }
}

#[test]
fn pantheon_verticality_matches_stratification() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (vert_i, size_i) = (idx("pantheon-verticality"), idx("flagship-structure-size"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        if matches!(row.values[vert_i], MetricValue::Absent) {
            continue;
        }
        let ranked = matches!(&row.values[vert_i], MetricValue::Text(t) if t == "ranked");
        let stratified = matches!(&row.values[size_i], MetricValue::Number(n) if *n >= 4.0);
        assert_eq!(
            ranked, stratified,
            "seed {}: verticality calibration violated",
            row.seed
        );
    }
}

#[test]
fn head_deity_is_eternal_exactly_when_tidally_locked() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (head_i, lock_i) = (idx("head-deity-periodicity"), idx("tidally-locked"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        if matches!(row.values[head_i], MetricValue::Absent) {
            continue;
        }
        let eternal = matches!(&row.values[head_i], MetricValue::Text(t) if t == "eternal");
        let locked = matches!(row.values[lock_i], MetricValue::Flag(true));
        assert_eq!(
            eternal, locked,
            "seed {}: head-deity calibration violated",
            row.seed
        );
    }
}
