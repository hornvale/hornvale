//! Deterministic sweep engine: run studies, collect metrics, write results.

use crate::{MetricValue, Study, StudyError, WorldView};
use hornvale_kernel::Seed;
use hornvale_worldgen::BuildError;
use std::fs;
use std::path::{Path, PathBuf};

/// One world's measurements (or its refusal).
#[derive(Debug, Clone, PartialEq)]
pub struct Row {
    /// The random seed used to generate this world.
    pub seed: u64,
    /// The label of the pin set applied to this world.
    pub pin_set: String,
    /// Extracted metric values, one per selected metric.
    pub values: Vec<MetricValue>,
    /// If genesis refused, the error message; otherwise None.
    pub refusal: Option<String>,
}

/// The result of running a complete study.
#[derive(Debug, Clone, PartialEq)]
pub struct RunResult {
    /// The study that was run.
    pub study: Study,
    /// Metric names in order, matching the order of values in each row.
    pub metric_names: Vec<&'static str>,
    /// One row per (pin_set, seed) pair.
    pub rows: Vec<Row>,
}

/// Run a study: build worlds for each pin set × seed combination,
/// extract metrics, and record results or refusals.
///
/// Returns a RunResult on success. On any BuildError other than Genesis,
/// returns a StudyError with a detailed message.
pub fn run(study: &Study) -> Result<RunResult, StudyError> {
    let metrics = study.selected_metrics()?;
    let pin_sets = study.pin_sets_parsed()?;

    let metric_names: Vec<&'static str> = metrics.iter().map(|m| m.name).collect();

    let mut rows = Vec::new();

    // Iterate: pin_sets in file order, seeds ascending from seeds.from
    for (label, pins) in pin_sets {
        for seed_offset in 0..study.seeds.count {
            let seed_value = study.seeds.from + seed_offset;
            let seed = Seed(seed_value);

            match WorldView::build(seed, &pins) {
                Ok(view) => {
                    let values: Vec<MetricValue> =
                        metrics.iter().map(|m| (m.extract)(&view)).collect();
                    rows.push(Row {
                        seed: seed_value,
                        pin_set: label.clone(),
                        values,
                        refusal: None,
                    });
                }
                Err(BuildError::Genesis(e)) => {
                    let refusal_msg = e.to_string();
                    let values = vec![MetricValue::Absent; metrics.len()];
                    rows.push(Row {
                        seed: seed_value,
                        pin_set: label.clone(),
                        values,
                        refusal: Some(refusal_msg),
                    });
                }
                Err(e) => {
                    return Err(StudyError {
                        message: format!("seed {}, set {}: {}", seed_value, label, e),
                    });
                }
            }
        }
    }

    Ok(RunResult {
        study: study.clone(),
        metric_names,
        rows,
    })
}

/// Write a RunResult to CSV.
///
/// Creates `out_root/<study.name>/` directories and writes `rows.csv` with:
/// - Header: seed,pin_set,<metric names...>,refusal
/// - Number values: displayed with default Rust formatting
/// - Text values: written verbatim
/// - Flag values: "true" or "false"
/// - Absent values: empty field
/// - Refusal column: the error message or empty
///
/// Returns the path to the written CSV file.
pub fn write_csv(result: &RunResult, out_root: &Path) -> std::io::Result<PathBuf> {
    let study_dir = out_root.join(&result.study.name);
    fs::create_dir_all(&study_dir)?;

    let csv_path = study_dir.join("rows.csv");
    let mut csv_content = String::new();

    // Write header
    csv_content.push_str("seed,pin_set");
    for name in &result.metric_names {
        csv_content.push(',');
        csv_content.push_str(name);
    }
    csv_content.push_str(",refusal\n");

    // Write rows
    for row in &result.rows {
        csv_content.push_str(&format!("{},{}", row.seed, row.pin_set));
        for value in &row.values {
            csv_content.push(',');
            match value {
                MetricValue::Number(n) => csv_content.push_str(&format!("{}", n)),
                MetricValue::Text(t) => csv_content.push_str(t),
                MetricValue::Flag(f) => csv_content.push_str(if *f { "true" } else { "false" }),
                MetricValue::Absent => {
                    // empty field
                }
            }
        }
        csv_content.push(',');
        if let Some(refusal) = &row.refusal {
            csv_content.push_str(refusal);
        }
        csv_content.push('\n');
    }

    fs::write(&csv_path, csv_content)?;
    Ok(csv_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricSelection, PinSet, Seeds};

    #[test]
    fn five_seed_study_runs_and_is_deterministic() {
        let study = Study {
            name: "t".to_string(),
            description: "test".to_string(),
            seeds: Seeds { from: 0, count: 5 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
            }],
            metrics: MetricSelection::Named(vec![
                "star-class".to_string(),
                "moons-admitted".to_string(),
            ]),
        };

        // Run twice
        let result1 = run(&study).expect("First run should succeed");
        let result2 = run(&study).expect("Second run should succeed");

        // Assert rows equal
        assert_eq!(result1.rows, result2.rows);

        // Write to temp dirs and verify bytes are identical
        let temp1 = std::env::temp_dir().join(format!("hornvale-test-{}-1", std::process::id()));
        let temp2 = std::env::temp_dir().join(format!("hornvale-test-{}-2", std::process::id()));

        fs::create_dir_all(&temp1).unwrap();
        fs::create_dir_all(&temp2).unwrap();

        let path1 = write_csv(&result1, &temp1).expect("Write 1 should succeed");
        let path2 = write_csv(&result2, &temp2).expect("Write 2 should succeed");

        let bytes1 = fs::read(&path1).expect("Read 1 should succeed");
        let bytes2 = fs::read(&path2).expect("Read 2 should succeed");

        assert_eq!(bytes1, bytes2, "CSV bytes should be identical");

        // Cleanup
        let _ = fs::remove_dir_all(&temp1);
        let _ = fs::remove_dir_all(&temp2);
    }

    #[test]
    fn refusals_are_rows_not_errors() {
        let study = Study {
            name: "t".to_string(),
            description: "test".to_string(),
            seeds: Seeds { from: 6, count: 2 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec!["moons=3".to_string()],
            }],
            metrics: MetricSelection::All("all".to_string()),
        };

        let result = run(&study).expect("Run should succeed despite Genesis refusal");

        // Should have 2 rows (seeds 6 and 7)
        assert_eq!(result.rows.len(), 2);

        // Find the row for seed 7 (which refuses)
        let seed_7_row = result
            .rows
            .iter()
            .find(|r| r.seed == 7)
            .expect("Should have seed 7 row");

        // Seed 7 refuses at exact-3 (see astronomy tests)
        assert!(
            seed_7_row.refusal.is_some(),
            "Seed 7 should have refusal: {:?}",
            seed_7_row.refusal
        );
        assert!(
            seed_7_row
                .refusal
                .as_ref()
                .unwrap()
                .to_lowercase()
                .contains("no stable orbit"),
            "Refusal should mention orbit"
        );

        // All values should be Absent
        for value in &seed_7_row.values {
            assert_eq!(
                *value,
                MetricValue::Absent,
                "Refusal row should have Absent values"
            );
        }

        // Other row should have no refusal
        let seed_6_row = result
            .rows
            .iter()
            .find(|r| r.seed == 6)
            .expect("Should have seed 6 row");
        assert!(
            seed_6_row.refusal.is_none(),
            "Seed 6 should not refuse: {:?}",
            seed_6_row.refusal
        );
    }

    #[test]
    fn row_count_is_seeds_times_pin_sets() {
        let study = Study {
            name: "t".to_string(),
            description: "test".to_string(),
            seeds: Seeds { from: 0, count: 3 },
            pin_sets: vec![
                PinSet {
                    label: "a".to_string(),
                    pins: vec![],
                },
                PinSet {
                    label: "b".to_string(),
                    pins: vec![],
                },
            ],
            metrics: MetricSelection::All("all".to_string()),
        };

        let result = run(&study).expect("Run should succeed");

        // 3 seeds × 2 pin_sets = 6 rows
        assert_eq!(result.rows.len(), 6);

        // Verify pin_set fields are correct
        let a_rows: Vec<_> = result.rows.iter().filter(|r| r.pin_set == "a").collect();
        let b_rows: Vec<_> = result.rows.iter().filter(|r| r.pin_set == "b").collect();

        assert_eq!(a_rows.len(), 3);
        assert_eq!(b_rows.len(), 3);
    }

    #[test]
    fn csv_text_fields_contain_no_commas() {
        let study = Study {
            name: "t".to_string(),
            description: "test".to_string(),
            seeds: Seeds { from: 0, count: 10 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
            }],
            metrics: MetricSelection::All("all".to_string()),
        };

        let result = run(&study).expect("Run should succeed");

        // Verify no Text value contains a comma
        for row in &result.rows {
            for value in &row.values {
                if let MetricValue::Text(t) = value {
                    assert!(
                        !t.contains(','),
                        "Text value should not contain comma: {}",
                        t
                    );
                }
            }
        }

        // Cleanup temp
        let temp = std::env::temp_dir().join(format!("hornvale-test-csv-{}", std::process::id()));
        let _ = fs::remove_dir_all(&temp);
    }
}
