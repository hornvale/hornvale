//! Human-readable diff of two census snapshots: which metric moved, and by
//! how much (TOOL-19).
//!
//! The review-surface companion to the CI drift check: when a study's
//! committed `rows.csv` changes, `hornvale lab diff` (and `make lab-diff
//! STUDY=<name>`) renders per-metric distribution deltas and numeric mean
//! shifts instead of a wall of quantized CSV. Deterministic: pin sets in
//! study order, metrics in registry order, floats displayed through the
//! kernel's `quantize`.

use crate::summary::distribution;
use crate::{MetricValue, RunResult, Study, StudyError, SummaryKind, load_rows};
use hornvale_kernel::quantize;
use std::collections::BTreeMap;

/// Render a markdown report of which metrics moved between two `rows.csv`
/// snapshots of the same study, and by how much.
/// type-audit: bare-ok(artifact: old_csv), bare-ok(artifact: new_csv), bare-ok(artifact: return)
pub fn render_diff(study: &Study, old_csv: &str, new_csv: &str) -> Result<String, StudyError> {
    Ok(render_diff_results(
        &load_rows(study, old_csv)?,
        &load_rows(study, new_csv)?,
    ))
}

/// The [`render_diff`] core over already-loaded results (separated so tests
/// and future callers can diff without re-parsing CSV).
/// type-audit: bare-ok(artifact: return)
pub fn render_diff_results(old: &RunResult, new: &RunResult) -> String {
    let mut out = String::new();
    out.push_str(&format!("## Lab diff: {}\n\n", new.study.name));

    let old_refusals = old.rows.iter().filter(|r| r.refusal.is_some()).count();
    let new_refusals = new.rows.iter().filter(|r| r.refusal.is_some()).count();
    out.push_str(&format!(
        "Rows {} → {}; refusals {} → {}.\n\n",
        old.rows.len(),
        new.rows.len(),
        old_refusals,
        new_refusals
    ));

    let metrics = new
        .study
        .selected_metrics()
        .expect("the study validated at load time");
    let mut moved_sections: Vec<String> = Vec::new();
    let mut pair_count = 0usize;

    for pin_set in &new.study.pin_sets {
        let old_rows: Vec<_> = old
            .rows
            .iter()
            .filter(|r| r.pin_set == pin_set.label)
            .collect();
        let new_rows: Vec<_> = new
            .rows
            .iter()
            .filter(|r| r.pin_set == pin_set.label)
            .collect();
        for (idx, name) in new.metric_names.iter().enumerate() {
            pair_count += 1;
            let metric = metrics
                .iter()
                .find(|m| m.name == *name)
                .expect("metric_names come from the registry");
            let old_values: Vec<&MetricValue> = old_rows.iter().map(|r| &r.values[idx]).collect();
            let new_values: Vec<&MetricValue> = new_rows.iter().map(|r| &r.values[idx]).collect();
            let dist_old = distribution(&metric.summary, &old_values);
            let dist_new = distribution(&metric.summary, &new_values);
            let (old_mean, new_mean) = match metric.summary {
                SummaryKind::Numeric { .. } => (mean_of(&old_values), mean_of(&new_values)),
                _ => (None, None),
            };
            let dist_moved = dist_old != dist_new;
            let mean_moved = old_mean != new_mean;
            if !dist_moved && !mean_moved {
                continue;
            }

            let mut section = format!("### {} — {}\n\n", name, pin_set.label);
            if dist_moved {
                section.push_str("| value | old | new | Δ |\n|---|---|---|---|\n");
                for (label, o, n) in merged_counts(&dist_old, &dist_new) {
                    if o != n {
                        section.push_str(&format!(
                            "| {} | {} | {} | {:+} |\n",
                            label,
                            o,
                            n,
                            n as i64 - o as i64
                        ));
                    }
                }
                section.push('\n');
            }
            if let (Some(o), Some(n)) = (old_mean, new_mean) {
                section.push_str(&format!(
                    "mean {} → {} (Δ {:+})\n\n",
                    quantize(o),
                    quantize(n),
                    quantize(n - o)
                ));
            }
            moved_sections.push(section);
        }
    }

    if moved_sections.is_empty() {
        out.push_str("No metric moved.\n");
    } else {
        out.push_str(&format!(
            "{} of {} metric × pin-set distributions moved.\n\n",
            moved_sections.len(),
            pair_count
        ));
        for section in moved_sections {
            out.push_str(&section);
        }
    }
    out
}

/// Mean of the present numeric values, or `None` when none are present.
fn mean_of(values: &[&MetricValue]) -> Option<f64> {
    let numbers: Vec<f64> = values
        .iter()
        .filter_map(|v| match v {
            MetricValue::Number(n) => Some(*n),
            _ => None,
        })
        .collect();
    if numbers.is_empty() {
        None
    } else {
        Some(numbers.iter().sum::<f64>() / numbers.len() as f64)
    }
}

/// Merge two `(label, count)` distributions into `(label, old, new)`,
/// keeping the new distribution's order and appending old-only labels at
/// the end (both inputs arrive already deterministically ordered from
/// [`distribution`]).
fn merged_counts(old: &[(String, u64)], new: &[(String, u64)]) -> Vec<(String, u64, u64)> {
    let old_map: BTreeMap<&str, u64> = old.iter().map(|(l, c)| (l.as_str(), *c)).collect();
    let mut merged: Vec<(String, u64, u64)> = new
        .iter()
        .map(|(l, c)| (l.clone(), old_map.get(l.as_str()).copied().unwrap_or(0), *c))
        .collect();
    for (label, count) in old {
        if !new.iter().any(|(l, _)| l == label) {
            merged.push((label.clone(), *count, 0));
        }
    }
    merged
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Row;

    /// Two-seed study over one Categorical and one Numeric metric, with
    /// per-test value overrides. `star-class` is Categorical and
    /// `ocean-fraction` is Numeric in the registry.
    fn result_with(star: [&str; 2], ocean: [f64; 2]) -> RunResult {
        let study: Study = serde_json::from_str(
            r#"{
                "name": "diff-test",
                "description": "diff unit-test study",
                "seeds": { "from": 0, "count": 2 },
                "pin_sets": [ { "label": "default", "pins": [] } ],
                "metrics": ["star-class", "ocean-fraction"]
            }"#,
        )
        .unwrap();
        study.validate().unwrap();
        RunResult {
            study,
            metric_names: vec!["star-class", "ocean-fraction"],
            rows: (0..2usize)
                .map(|i| Row {
                    seed: i as u64,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text(star[i].to_string()),
                        MetricValue::Number(ocean[i]),
                    ],
                    refusal: None,
                })
                .collect(),
        }
    }

    #[test]
    fn identical_results_report_no_movement() {
        let a = result_with(["K", "M"], [0.5, 0.7]);
        let b = result_with(["K", "M"], [0.5, 0.7]);
        let report = render_diff_results(&a, &b);
        assert!(report.contains("No metric moved."), "got:\n{report}");
        assert!(!report.contains("###"), "no per-metric sections:\n{report}");
    }

    #[test]
    fn a_categorical_shift_shows_the_changed_labels_with_deltas() {
        let old = result_with(["K", "M"], [0.5, 0.7]);
        let new = result_with(["K", "K"], [0.5, 0.7]);
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("### star-class — default"),
            "got:\n{report}"
        );
        assert!(report.contains("| K | 1 | 2 | +1 |"), "got:\n{report}");
        assert!(report.contains("| M | 1 | 0 | -1 |"), "got:\n{report}");
        // The numeric metric did not move — no section for it.
        assert!(
            !report.contains("### ocean-fraction — default"),
            "got:\n{report}"
        );
        assert!(
            report.contains("1 of 2 metric × pin-set distributions moved."),
            "got:\n{report}"
        );
    }

    #[test]
    fn a_numeric_shift_shows_the_mean_movement() {
        let old = result_with(["K", "M"], [0.50, 0.70]);
        let new = result_with(["K", "M"], [0.50, 0.80]);
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("### ocean-fraction — default"),
            "got:\n{report}"
        );
        // mean 0.6 → 0.65, quantized display, signed delta.
        assert!(report.contains("mean 0.6 → 0.65"), "got:\n{report}");
        assert!(report.contains("Δ +0.05"), "got:\n{report}");
    }

    #[test]
    fn a_mean_only_shift_renders_the_mean_line_without_a_table() {
        // 0.50 → 0.55 stays inside one histogram bucket, so the distribution
        // is unchanged while the mean moves: the section must render with
        // the mean line only — no distribution table.
        let old = result_with(["K", "M"], [0.50, 0.70]);
        let new = result_with(["K", "M"], [0.55, 0.70]);
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("### ocean-fraction — default"),
            "got:\n{report}"
        );
        assert!(report.contains("mean 0.6 → 0.625"), "got:\n{report}");
        assert!(report.contains("Δ +0.025"), "got:\n{report}");
        assert!(
            !report.contains("| value | old | new | Δ |"),
            "no distribution table when only the mean moved:\n{report}"
        );
        assert!(
            report.contains("1 of 2 metric × pin-set distributions moved."),
            "got:\n{report}"
        );
    }

    #[test]
    fn header_reports_row_and_refusal_counts() {
        let old = result_with(["K", "M"], [0.5, 0.7]);
        let mut new = result_with(["K", "M"], [0.5, 0.7]);
        new.rows[1].refusal = Some("pin refused".to_string());
        new.rows[1].values = vec![MetricValue::Absent, MetricValue::Absent];
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("Rows 2 → 2; refusals 0 → 1."),
            "got:\n{report}"
        );
    }

    #[test]
    fn csv_round_trip_diffs_through_render_diff() {
        let old = result_with(["K", "M"], [0.5, 0.7]);
        let new = result_with(["K", "K"], [0.5, 0.7]);
        let old_csv = crate::runner::render_csv(&old);
        let new_csv = crate::runner::render_csv(&new);
        let report = render_diff(&old.study, &old_csv, &new_csv).unwrap();
        assert!(report.contains("| K | 1 | 2 | +1 |"), "got:\n{report}");
    }
}
