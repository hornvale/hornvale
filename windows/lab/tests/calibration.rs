//! Calibration: at tier 0, belief kind is a pure function of rotation.
//! The instrument must reproduce known ground truth exactly (spec §2.5).
use hornvale_lab::{MetricValue, load_study, run};
use std::path::Path;

#[test]
fn eternal_beliefs_coincide_exactly_with_tidal_locking() {
    let study = load_study(Path::new("../../studies/census-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (locked_i, belief_i) = (idx("tidally-locked"), idx("belief-kind"));
    for row in &result.rows {
        let locked = matches!(row.values[locked_i], MetricValue::Flag(true));
        let eternal = matches!(&row.values[belief_i], MetricValue::Text(t) if t == "eternal");
        assert_eq!(locked, eternal, "seed {}: calibration violated", row.seed);
    }
}
