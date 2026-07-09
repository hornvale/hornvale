//! Deterministic sweep engine: run studies, collect metrics, write results.

use crate::{Metric, MetricValue, Study, StudyError, WorldView};
use hornvale_astronomy::SkyPins;
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

    // Pin sets in file order; within each, seeds ascending from seeds.from. The
    // seed sweep runs in parallel across the available CPUs, but the output is
    // byte-identical to a sequential run: every world is a pure function of its
    // seed, results are reassembled by seed offset (never by which thread
    // finished first), and the thread count affects only speed. The
    // `parallel_run_matches_sequential` test pins that equivalence.
    let mut rows = Vec::new();
    for (label, pins) in &pin_sets {
        rows.extend(run_pin_set(study, label, pins, &metrics)?);
    }

    Ok(RunResult {
        study: study.clone(),
        metric_names,
        rows,
    })
}

/// Build the row for one (seed offset, pin set): a successful measurement, a
/// genesis refusal (recorded as a row, not an error), or a fatal `StudyError`.
/// A pure function of its inputs — the unit of parallel work.
fn build_row(
    study: &Study,
    label: &str,
    pins: &SkyPins,
    seed_offset: u64,
    metrics: &[Metric],
) -> Result<Row, StudyError> {
    let seed_value = study.seeds.from + seed_offset;
    match WorldView::build(Seed(seed_value), pins) {
        Ok(view) => Ok(Row {
            seed: seed_value,
            pin_set: label.to_string(),
            values: metrics.iter().map(|m| (m.extract)(&view)).collect(),
            refusal: None,
        }),
        Err(BuildError::Genesis(e)) => Ok(Row {
            seed: seed_value,
            pin_set: label.to_string(),
            values: vec![MetricValue::Absent; metrics.len()],
            refusal: Some(e.to_string()),
        }),
        Err(e) => Err(StudyError {
            message: format!("seed {seed_value}, set {label}: {e}"),
        }),
    }
}

/// Build every seed's row for one pin set, in ascending seed order, sweeping
/// the seeds in parallel across the available CPUs. Returns the earliest
/// (lowest seed offset) fatal error, matching the sequential path's
/// first-error-wins semantics.
fn run_pin_set(
    study: &Study,
    label: &str,
    pins: &SkyPins,
    metrics: &[Metric],
) -> Result<Vec<Row>, StudyError> {
    let count = study.seeds.count;
    if count == 0 {
        return Ok(Vec::new());
    }
    let threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
        .clamp(1, count as usize);

    if threads == 1 {
        return (0..count)
            .map(|off| build_row(study, label, pins, off, metrics))
            .collect();
    }

    // One result slot per seed offset, filled by worker threads and read back
    // in offset order on the main thread — so output order never depends on
    // scheduling. Each thread owns a contiguous seed range (no shared writes).
    let mut slots: Vec<Option<Result<Row, StudyError>>> = (0..count).map(|_| None).collect();
    let chunk = count.div_ceil(threads as u64);

    std::thread::scope(|scope| {
        let mut handles = Vec::with_capacity(threads);
        for t in 0..threads as u64 {
            let lo = t * chunk;
            if lo >= count {
                break;
            }
            let hi = ((t + 1) * chunk).min(count);
            handles.push(scope.spawn(move || {
                (lo..hi)
                    .map(|off| (off, build_row(study, label, pins, off, metrics)))
                    .collect::<Vec<_>>()
            }));
        }
        for handle in handles {
            for (off, res) in handle.join().expect("a study worker thread panicked") {
                slots[off as usize] = Some(res);
            }
        }
    });

    let mut rows = Vec::with_capacity(count as usize);
    for slot in slots {
        rows.push(slot.expect("every seed offset produced a result")?);
    }
    Ok(rows)
}

/// Quote a CSV field per RFC 4180 if it contains a comma, quote, or newline
/// (embedded quotes are doubled); otherwise return it unchanged. Some Text
/// metrics (e.g. the per-species flagship role ladders) are themselves
/// comma-joined, so the writer must be able to round-trip them.
fn csv_field(text: &str) -> String {
    if text.contains(',') || text.contains('"') || text.contains('\n') {
        format!("\"{}\"", text.replace('"', "\"\""))
    } else {
        text.to_string()
    }
}

/// Write a RunResult to CSV.
///
/// Creates `out_root/<study.name>/` directories and writes `rows.csv` with:
/// - Header: seed,pin_set,<metric names...>,refusal
/// - Number values: displayed with default Rust formatting
/// - Text values: quoted per RFC 4180 when they contain a comma, quote, or
///   newline; written verbatim otherwise
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
                MetricValue::Text(t) => csv_content.push_str(&csv_field(t)),
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

    /// The reference sequential sweep — the oracle the parallel `run` is
    /// checked against, byte for byte, by `parallel_run_matches_sequential`.
    fn run_sequential(study: &Study) -> Result<RunResult, StudyError> {
        let metrics = study.selected_metrics()?;
        let pin_sets = study.pin_sets_parsed()?;
        let metric_names: Vec<&'static str> = metrics.iter().map(|m| m.name).collect();
        let mut rows = Vec::new();
        for (label, pins) in &pin_sets {
            for off in 0..study.seeds.count {
                rows.push(build_row(study, label, pins, off, &metrics)?);
            }
        }
        Ok(RunResult {
            study: study.clone(),
            metric_names,
            rows,
        })
    }

    #[test]
    fn parallel_run_matches_sequential() {
        // Two pin sets × enough seeds to span multiple worker threads. If the
        // parallel sweep ever diverged from the sequential one — a reassembly
        // bug, a shared-state leak, a nondeterministic metric — either equality
        // below would fail.
        let study = Study {
            name: "par".to_string(),
            description: "parallel-equivalence".to_string(),
            seeds: Seeds { from: 0, count: 16 },
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
            metrics: MetricSelection::Named(vec![
                "star-class".to_string(),
                "moons-admitted".to_string(),
                "tidally-locked".to_string(),
            ]),
        };

        let parallel = run(&study).expect("parallel run");
        let sequential = run_sequential(&study).expect("sequential run");
        assert_eq!(
            parallel, sequential,
            "parallel sweep must be byte-identical to sequential"
        );

        // And identical in the committed-artifact form (CSV bytes).
        let t1 = std::env::temp_dir().join(format!("hv-par-{}", std::process::id()));
        let t2 = std::env::temp_dir().join(format!("hv-seq-{}", std::process::id()));
        fs::create_dir_all(&t1).unwrap();
        fs::create_dir_all(&t2).unwrap();
        let par_bytes = fs::read(write_csv(&parallel, &t1).unwrap()).unwrap();
        let seq_bytes = fs::read(write_csv(&sequential, &t2).unwrap()).unwrap();
        assert_eq!(
            par_bytes, seq_bytes,
            "parallel and sequential CSV bytes must match"
        );
        let _ = fs::remove_dir_all(&t1);
        let _ = fs::remove_dir_all(&t2);
    }

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

    /// Split one CSV line into fields, honoring RFC 4180 quoting (a quoted
    /// field may itself contain commas; a doubled quote is an escaped
    /// literal quote). Mirrors what any real CSV reader (and Excel) does, so
    /// this is what the round-trip test below checks against.
    fn parse_csv_line(line: &str) -> Vec<String> {
        let mut fields = Vec::new();
        let mut field = String::new();
        let mut in_quotes = false;
        let mut chars = line.chars().peekable();
        while let Some(c) = chars.next() {
            if in_quotes {
                if c == '"' {
                    if chars.peek() == Some(&'"') {
                        field.push('"');
                        chars.next();
                    } else {
                        in_quotes = false;
                    }
                } else {
                    field.push(c);
                }
            } else if c == '"' {
                in_quotes = true;
            } else if c == ',' {
                fields.push(std::mem::take(&mut field));
            } else {
                field.push(c);
            }
        }
        fields.push(field);
        fields
    }

    #[test]
    fn csv_round_trips_comma_containing_text_fields() {
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
        assert!(
            result.rows.iter().any(|row| row
                .values
                .iter()
                .any(|v| matches!(v, MetricValue::Text(t) if t.contains(',')))),
            "expected at least one comma-containing Text value (e.g. a flagship role \
             ladder) to exercise the quoting path"
        );

        let temp = std::env::temp_dir().join(format!(
            "hornvale-test-csv-roundtrip-{}",
            std::process::id()
        ));
        let csv_path = write_csv(&result, &temp).expect("Write should succeed");
        let content = fs::read_to_string(&csv_path).expect("Read should succeed");
        let mut lines = content.lines();
        let header = parse_csv_line(lines.next().expect("header line"));
        let expected_columns = header.len();

        for (row, line) in result.rows.iter().zip(lines) {
            let fields = parse_csv_line(line);
            assert_eq!(
                fields.len(),
                expected_columns,
                "seed {}: CSV line split into the wrong number of fields \
                 (a comma-containing Text field was not quoted correctly)",
                row.seed
            );
            // seed, pin_set, then one column per metric, then refusal.
            for (value, field) in row.values.iter().zip(&fields[2..]) {
                if let MetricValue::Text(t) = value {
                    assert_eq!(field, t, "seed {}: Text value did not round-trip", row.seed);
                }
            }
        }

        // Cleanup temp
        let _ = fs::remove_dir_all(&temp);
    }
}
