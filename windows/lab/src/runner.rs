//! Deterministic sweep engine: run studies, collect metrics, write results.

use crate::{Metric, MetricValue, Study, StudyError, SummaryKind, WorldView};
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::SpeciesDef;
use hornvale_worldgen::BuildError;
use std::fs;
use std::path::{Path, PathBuf};

/// Resolve a pin set's roster name to a concrete species roster. The closed
/// set the Lab knows (spec §5): `None`/`"default"` = shipped; the two solo
/// null-control rosters otherwise. Unknown ⇒ loud `StudyError`.
fn resolve_roster(name: Option<&str>) -> Result<Vec<SpeciesDef>, StudyError> {
    match name {
        None | Some("default") => Ok(hornvale_worldgen::default_roster()),
        Some("goblin-solo") => Ok(crate::goblin_solo_roster()),
        Some("goblin-twin-solo") => Ok(crate::goblin_twin_solo_roster()),
        Some(other) => Err(StudyError {
            message: format!(
                "unknown roster '{other}'; known: default, goblin-solo, goblin-twin-solo"
            ),
        }),
    }
}

/// One world's measurements (or its refusal).
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(identifier-text: pin_set), bare-ok(prose: refusal)
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
/// type-audit: bare-ok(identifier-text: metric_names)
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
    for (label, pins, roster) in &pin_sets {
        rows.extend(run_pin_set(
            study,
            label,
            pins,
            roster.as_deref(),
            &metrics,
        )?);
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
    roster: Option<&str>,
    seed_offset: u64,
    metrics: &[Metric],
) -> Result<Row, StudyError> {
    let seed_value = study.seeds.from + seed_offset;
    let roster = resolve_roster(roster)?;
    match WorldView::build_with_roster(Seed(seed_value), pins, roster) {
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
    roster: Option<&str>,
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
            .map(|off| build_row(study, label, pins, roster, off, metrics))
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
                    .map(|off| (off, build_row(study, label, pins, roster, off, metrics)))
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

/// Render a RunResult as CSV text — the body of `rows.csv`.
///
/// Shared by [`write_csv`] (the ephemeral `lab-out/` copy) and the book
/// publisher (the committed, drift-checked fixture), so the two are always
/// byte-identical. The format:
/// - Header: seed,pin_set,<metric names...>,refusal
/// - Number values: quantized to the kernel's platform-stable canonical
///   form (8 significant digits), then displayed with default Rust
///   formatting — [`load_rows`] parses the emitted text back exactly
/// - Text values: quoted per RFC 4180 when they contain a comma, quote, or
///   newline; written verbatim otherwise
/// - Flag values: "true" or "false"
/// - Absent values: empty field
/// - Refusal column: the error message (unquoted) or empty
pub(crate) fn render_csv(result: &RunResult) -> String {
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
                // Quantize to the platform-stable canonical form (see the
                // kernel `quantize` module): full-precision metric values
                // otherwise carry last-ULP libm divergence into the committed
                // census fixture and break the cross-platform drift check.
                MetricValue::Number(n) => {
                    csv_content.push_str(&format!("{}", hornvale_kernel::quantize(*n)))
                }
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

    csv_content
}

/// Write a RunResult's `rows.csv` under `out_root/<study.name>/`.
///
/// Creates the directory as needed and writes [`render_csv`]'s output.
/// Returns the path to the written CSV file.
pub fn write_csv(result: &RunResult, out_root: &Path) -> std::io::Result<PathBuf> {
    let study_dir = out_root.join(&result.study.name);
    fs::create_dir_all(&study_dir)?;

    let csv_path = study_dir.join("rows.csv");
    fs::write(&csv_path, render_csv(result))?;
    Ok(csv_path)
}

/// Split CSV text into records of fields, honoring RFC 4180 quoting: a `"`
/// opens a quoted field in which commas and newlines are literal and a doubled
/// `""` is an escaped quote. The inverse of [`render_csv`]'s field encoding.
fn parse_csv_records(csv: &str) -> Vec<Vec<String>> {
    let mut records = Vec::new();
    let mut record: Vec<String> = Vec::new();
    let mut field = String::new();
    let mut in_quotes = false;
    let mut chars = csv.chars().peekable();

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
        } else {
            match c {
                '"' => in_quotes = true,
                ',' => record.push(std::mem::take(&mut field)),
                '\n' => {
                    record.push(std::mem::take(&mut field));
                    records.push(std::mem::take(&mut record));
                }
                '\r' => {} // tolerate CRLF
                other => field.push(other),
            }
        }
    }
    // A trailing field/record only exists if the text didn't end in a newline;
    // `render_csv` always terminates the last row, so this is defensive.
    if !field.is_empty() || !record.is_empty() {
        record.push(field);
        records.push(record);
    }
    records
}

/// Reconstruct one `MetricValue` from a CSV field, typed by the owning metric's
/// kind. An empty field is `Absent`; otherwise Categorical → Text, Flag → Flag,
/// Numeric → Number. Lossless because the *kind* comes from the study schema,
/// never guessed from the field's shape.
fn value_from_field(field: &str, kind: &SummaryKind) -> Result<MetricValue, StudyError> {
    if field.is_empty() {
        return Ok(MetricValue::Absent);
    }
    Ok(match kind {
        SummaryKind::Numeric { .. } => {
            MetricValue::Number(field.parse::<f64>().map_err(|_| StudyError {
                message: format!("rows.csv: '{field}' is not a number"),
            })?)
        }
        SummaryKind::Flag => match field {
            "true" => MetricValue::Flag(true),
            "false" => MetricValue::Flag(false),
            other => {
                return Err(StudyError {
                    message: format!("rows.csv: '{other}' is not a flag"),
                });
            }
        },
        SummaryKind::Categorical => MetricValue::Text(field.to_string()),
    })
}

/// Reconstruct a `RunResult` from `rows.csv` text produced by [`render_csv`].
///
/// The schema — metric names, their order, and each metric's kind — comes from
/// `study`, not the CSV, so `MetricValue` reconstruction is lossless (see
/// [`value_from_field`]); the CSV supplies only per-seed values. The header is
/// validated against the study, so a stale or foreign fixture fails loudly
/// rather than silently mis-parsing. This is how the calibration suite reads
/// its census without recomputing it — the fixture is regenerated and
/// drift-checked in CI, so `load_rows(fixture)` equals `run(study)` by
/// construction.
/// type-audit: bare-ok(artifact: csv)
pub fn load_rows(study: &Study, csv: &str) -> Result<RunResult, StudyError> {
    let metrics = study.selected_metrics()?;
    let metric_names: Vec<&'static str> = metrics.iter().map(|m| m.name).collect();
    let n = metric_names.len();

    let mut records = parse_csv_records(csv).into_iter();
    let header = records.next().ok_or_else(|| StudyError {
        message: format!("rows.csv for study '{}' is empty", study.name),
    })?;

    // The header must match the study's schema exactly, or the fixture is stale
    // or belongs to a different study.
    let mut expected = Vec::with_capacity(n + 3);
    expected.push("seed".to_string());
    expected.push("pin_set".to_string());
    expected.extend(metric_names.iter().map(|name| name.to_string()));
    expected.push("refusal".to_string());
    if header != expected {
        return Err(StudyError {
            message: format!(
                "rows.csv header does not match study '{}' schema:\n  found:    {header:?}\n  expected: {expected:?}",
                study.name
            ),
        });
    }

    let mut rows = Vec::new();
    for rec in records {
        if rec.len() == 1 && rec[0].is_empty() {
            continue; // tolerate a blank trailing line
        }
        if rec.len() < n + 2 {
            return Err(StudyError {
                message: format!(
                    "rows.csv for study '{}': a row has {} fields, need at least {}",
                    study.name,
                    rec.len(),
                    n + 2
                ),
            });
        }
        let seed = rec[0].parse::<u64>().map_err(|_| StudyError {
            message: format!(
                "rows.csv for study '{}': '{}' is not a seed",
                study.name, rec[0]
            ),
        })?;
        let pin_set = rec[1].clone();
        let values = metrics
            .iter()
            .enumerate()
            .map(|(i, m)| value_from_field(&rec[2 + i], &m.summary))
            .collect::<Result<Vec<_>, _>>()?;
        // The refusal column is written unquoted and may itself contain commas,
        // so it is whatever remains after the metric columns, rejoined.
        let refusal_text = rec[2 + n..].join(",");
        let refusal = (!refusal_text.is_empty()).then_some(refusal_text);
        rows.push(Row {
            seed,
            pin_set,
            values,
            refusal,
        });
    }

    Ok(RunResult {
        study: study.clone(),
        metric_names,
        rows,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricSelection, PinSet, Seeds};

    #[test]
    fn csv_round_trips_all_value_kinds() {
        // A study selecting one metric of each SummaryKind, so `load_rows` types
        // every column correctly. The row VALUES below are synthetic — the codec
        // is agnostic to whether a real world would ever produce them; it must
        // only invert `render_csv` exactly.
        let study = Study {
            name: "codec".to_string(),
            description: "csv-round-trip".to_string(),
            seeds: Seeds { from: 0, count: 0 },
            pin_sets: vec![],
            metrics: MetricSelection::Named(vec![
                "star-class".to_string(),       // Categorical -> Text
                "tidally-locked".to_string(),   // Flag
                "day-length-hours".to_string(), // Numeric -> Number
            ]),
        };
        let metric_names: Vec<&'static str> = study
            .selected_metrics()
            .unwrap()
            .iter()
            .map(|m| m.name)
            .collect();
        let rows = vec![
            Row {
                seed: 7,
                pin_set: "default".to_string(),
                values: vec![
                    MetricValue::Text("G".to_string()),
                    MetricValue::Flag(true),
                    MetricValue::Number(23.5),
                ],
                refusal: None,
            },
            Row {
                seed: 8,
                pin_set: "default".to_string(),
                // A comma- and quote-bearing Text (must be RFC-4180 quoted) and
                // an Absent numeric.
                values: vec![
                    MetricValue::Text("a,b\"c".to_string()),
                    MetricValue::Flag(false),
                    MetricValue::Absent,
                ],
                refusal: None,
            },
            Row {
                seed: 9,
                pin_set: "default".to_string(),
                // A refusal whose message contains a comma (written unquoted),
                // with all metric columns Absent — exactly how `run` records a
                // genesis refusal.
                values: vec![MetricValue::Absent; 3],
                refusal: Some("genesis refused: too hot, too bright".to_string()),
            },
        ];
        let original = RunResult {
            study: study.clone(),
            metric_names,
            rows,
        };

        let csv = render_csv(&original);
        let loaded = load_rows(&study, &csv).expect("load_rows inverts render_csv");
        assert_eq!(loaded, original);
    }

    #[test]
    fn csv_numbers_are_quantized_for_cross_platform_stability() {
        // Full-precision metric values carry last-ULP libm divergence between
        // platforms; render_csv must emit the platform-stable quantized form.
        let study = Study {
            name: "q".to_string(),
            description: "quantize".to_string(),
            seeds: Seeds { from: 0, count: 0 },
            pin_sets: vec![],
            metrics: MetricSelection::Named(vec!["day-length-hours".to_string()]),
        };
        let raw = 39.46846507151197_f64;
        let original = RunResult {
            study: study.clone(),
            metric_names: study
                .selected_metrics()
                .unwrap()
                .iter()
                .map(|m| m.name)
                .collect(),
            rows: vec![Row {
                seed: 0,
                pin_set: "default".to_string(),
                values: vec![MetricValue::Number(raw)],
                refusal: None,
            }],
        };
        let csv = render_csv(&original);
        assert!(
            csv.contains(&hornvale_kernel::quantize(raw).to_string()),
            "csv must contain the quantized value"
        );
        assert!(
            !csv.contains(&raw.to_string()),
            "csv must not contain the raw full-precision value"
        );
    }

    /// The reference sequential sweep — the oracle the parallel `run` is
    /// checked against, byte for byte, by `parallel_run_matches_sequential`.
    fn run_sequential(study: &Study) -> Result<RunResult, StudyError> {
        let metrics = study.selected_metrics()?;
        let pin_sets = study.pin_sets_parsed()?;
        let metric_names: Vec<&'static str> = metrics.iter().map(|m| m.name).collect();
        let mut rows = Vec::new();
        for (label, pins, roster) in &pin_sets {
            for off in 0..study.seeds.count {
                rows.push(build_row(
                    study,
                    label,
                    pins,
                    roster.as_deref(),
                    off,
                    &metrics,
                )?);
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
                    roster: None,
                },
                PinSet {
                    label: "b".to_string(),
                    pins: vec![],
                    roster: None,
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
                roster: None,
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
                roster: None,
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
                    roster: None,
                },
                PinSet {
                    label: "b".to_string(),
                    pins: vec![],
                    roster: None,
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
                roster: None,
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

    #[test]
    fn unknown_roster_is_a_loud_error() {
        let study = Study {
            name: "t".into(),
            description: "d".into(),
            seeds: Seeds { from: 0, count: 1 },
            pin_sets: vec![PinSet {
                label: "x".into(),
                pins: vec![],
                roster: Some("bogus".into()),
            }],
            metrics: MetricSelection::Named(vec!["star-class".into()]),
        };
        let err = run(&study).unwrap_err();
        assert!(
            err.message.contains("bogus"),
            "error must name the bad roster: {}",
            err.message
        );
    }

    #[test]
    fn goblin_twin_solo_roster_builds_and_populates_twin_metrics() {
        let study = Study {
            name: "t".into(),
            description: "d".into(),
            seeds: Seeds { from: 42, count: 1 },
            pin_sets: vec![PinSet {
                label: "twin".into(),
                pins: vec![],
                roster: Some("goblin-twin-solo".into()),
            }],
            metrics: MetricSelection::Named(vec!["head-deity-domain-goblin-twin".into()]),
        };
        let r = run(&study).unwrap();
        assert!(
            matches!(r.rows[0].values[0], MetricValue::Text(_)),
            "twin metric must populate"
        );
    }
}
