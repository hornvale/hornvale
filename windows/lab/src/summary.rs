//! Deterministic markdown summaries of lab run results.

use crate::{MetricValue, RunResult, SummaryKind};
use std::collections::BTreeMap;

/// Aggregate one metric's values into (label, count) pairs, deterministic order.
/// Shared with Task 6's charts so table and chart can never disagree.
///
/// Semantics:
/// - Categorical: BTreeMap over Text values; Absent counted under "absent" label if present
/// - Flag: labels exactly "true","false" in that order; both rows always present; "absent" only if any
/// - Numeric: rows in order: `< first`, `[a,b)` per consecutive pair, `>= last`, then `absent` if any
pub(crate) fn distribution(kind: &SummaryKind, values: &[&MetricValue]) -> Vec<(String, u64)> {
    match kind {
        SummaryKind::Categorical => {
            let mut counts = BTreeMap::new();
            let mut absent_count = 0u64;

            for value in values {
                match value {
                    MetricValue::Text(t) => {
                        *counts.entry(t.clone()).or_insert(0) += 1;
                    }
                    MetricValue::Absent => {
                        absent_count += 1;
                    }
                    _ => {} // Flag/Number impossible in Categorical
                }
            }

            let mut result: Vec<(String, u64)> = counts.into_iter().collect();

            if absent_count > 0 {
                result.push(("absent".to_string(), absent_count));
            }

            result
        }

        SummaryKind::Flag => {
            let mut true_count = 0u64;
            let mut false_count = 0u64;
            let mut absent_count = 0u64;

            for value in values {
                match value {
                    MetricValue::Flag(true) => true_count += 1,
                    MetricValue::Flag(false) => false_count += 1,
                    MetricValue::Absent => absent_count += 1,
                    _ => {} // Number/Text impossible in Flag
                }
            }

            let mut result = vec![
                ("true".to_string(), true_count),
                ("false".to_string(), false_count),
            ];

            if absent_count > 0 {
                result.push(("absent".to_string(), absent_count));
            }

            result
        }

        SummaryKind::Numeric { bucket_edges } => {
            let mut buckets: Vec<u64> = vec![0; bucket_edges.len() + 1]; // +1 for overflow
            let mut absent_count = 0u64;

            for value in values {
                match value {
                    MetricValue::Number(n) => {
                        let n = *n;
                        if n < bucket_edges[0] {
                            buckets[0] += 1;
                        } else if n >= bucket_edges[bucket_edges.len() - 1] {
                            buckets[bucket_edges.len()] += 1;
                        } else {
                            // Find the interval [a, b) where a <= n < b
                            for i in 0..bucket_edges.len() - 1 {
                                if n >= bucket_edges[i] && n < bucket_edges[i + 1] {
                                    buckets[i + 1] += 1;
                                    break;
                                }
                            }
                        }
                    }
                    MetricValue::Absent => absent_count += 1,
                    _ => {} // Flag/Text impossible in Numeric
                }
            }

            let mut result = Vec::new();

            // < first
            result.push((format!("< {}", bucket_edges[0]), buckets[0]));

            // [a, b) for each consecutive pair
            for i in 0..bucket_edges.len() - 1 {
                let label = format!("[{}, {})", bucket_edges[i], bucket_edges[i + 1]);
                result.push((label, buckets[i + 1]));
            }

            // >= last
            result.push((
                format!(">= {}", bucket_edges[bucket_edges.len() - 1]),
                buckets[bucket_edges.len()],
            ));

            // absent (only if any)
            if absent_count > 0 {
                result.push(("absent".to_string(), absent_count));
            }

            result
        }
    }
}

/// Render a study result as a deterministic markdown summary with tables.
/// type-audit: bare-ok(artifact: return)
pub fn render_summary(result: &RunResult) -> String {
    let mut output = String::new();

    // 1. Header comment
    output
        .push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab run`. -->\n\n");

    // 2. Study header
    output.push_str(&format!("## Study: {}\n\n", result.study.name));
    output.push_str(&format!("{}\n\n", result.study.description));

    // 3. Seeds and rows info
    //
    // The displayed upper bound is exclusive (`from..from+count`), which is
    // one past the last seed the runner actually visits
    // (`from + (count - 1)`, see `Study::validate`'s overflow check). Use
    // saturating arithmetic so a seed range validated only against the
    // inclusive last-seed bound can never panic or wrap here.
    let pin_set_count = result.study.pin_sets.len();
    let refusal_count = result.rows.iter().filter(|r| r.refusal.is_some()).count();
    output.push_str(&format!(
        "Seeds {}..{} × {} pin set(s); {} rows; {} refusals.\n\n",
        result.study.seeds.from,
        result
            .study
            .seeds
            .from
            .saturating_add(result.study.seeds.count),
        pin_set_count,
        result.rows.len(),
        refusal_count
    ));

    // 4. Tables for each pin set and metric
    let metrics = &result.study.selected_metrics().unwrap();
    for pin_set in &result.study.pin_sets {
        // Filter rows for this pin set
        let pin_rows: Vec<_> = result
            .rows
            .iter()
            .filter(|r| r.pin_set == pin_set.label)
            .collect();
        let pin_set_row_count = pin_rows.len();

        for (metric_idx, metric_name) in result.metric_names.iter().enumerate() {
            output.push_str(&format!("### {} — {}\n\n", metric_name, pin_set.label));
            output.push_str("| value | count | share |\n");
            output.push_str("|---|---|---|\n");

            // Get the metric's summary kind
            let metric = metrics.iter().find(|m| m.name == *metric_name).unwrap();

            // Collect values for this metric from pin set rows
            let values: Vec<&MetricValue> =
                pin_rows.iter().map(|r| &r.values[metric_idx]).collect();

            // Get distribution
            let dist = distribution(&metric.summary, &values);

            // Render each row
            for (label, count) in dist {
                let share = if pin_set_row_count > 0 {
                    (count as f64 / pin_set_row_count as f64) * 100.0
                } else {
                    0.0
                };
                output.push_str(&format!("| {} | {} | {:.1}% |\n", label, count, share));
            }

            output.push('\n');
        }
    }

    // 5. Refusals section (if any)
    let has_refusals = result.rows.iter().any(|r| r.refusal.is_some());
    if has_refusals {
        output.push_str("### Refusals\n\n");
        output.push_str("| pin set | count | example reason |\n");
        output.push_str("|---|---|---|\n");

        // Group refusals by pin_set in BTree order
        let mut refusal_map: BTreeMap<String, (u64, String)> = BTreeMap::new();
        for row in &result.rows {
            if let Some(reason) = &row.refusal {
                let entry = refusal_map
                    .entry(row.pin_set.clone())
                    .or_insert((0, reason.clone()));
                entry.0 += 1;
                // Keep the first reason as the example
                if entry.1 != *reason {
                    // Already set, don't update
                }
            }
        }

        // Ensure we get the first refusal message, not just any
        for pin_set in &result.study.pin_sets {
            if let Some((count, reason)) = refusal_map.get(&pin_set.label) {
                output.push_str(&format!("| {} | {} | {} |\n", pin_set.label, count, reason));
            }
        }

        output.push('\n');
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricValue, Row, Study};

    #[test]
    fn distribution_buckets_use_half_open_intervals() {
        let values = vec![
            &MetricValue::Number(0.0),
            &MetricValue::Number(4.9),
            &MetricValue::Number(5.0),
            &MetricValue::Number(10.0),
            &MetricValue::Number(12.0),
            &MetricValue::Absent,
        ];

        let result = distribution(
            &SummaryKind::Numeric {
                bucket_edges: &[0.0, 5.0, 10.0],
            },
            &values,
        );

        // Expect: "< 0" = 0, "[0, 5)" = 2 (0, 4.9), "[5, 10)" = 1 (5), ">= 10" = 2 (10, 12), "absent" = 1
        assert_eq!(result.len(), 5);
        assert_eq!(result[0], ("< 0".to_string(), 0));
        assert_eq!(result[1], ("[0, 5)".to_string(), 2));
        assert_eq!(result[2], ("[5, 10)".to_string(), 1));
        assert_eq!(result[3], (">= 10".to_string(), 2));
        assert_eq!(result[4], ("absent".to_string(), 1));
    }

    #[test]
    fn flag_distribution_always_has_both_rows() {
        let values = vec![&MetricValue::Flag(true)];

        let result = distribution(&SummaryKind::Flag, &values);

        // Expect: true=1, false=0, no absent row
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], ("true".to_string(), 1));
        assert_eq!(result[1], ("false".to_string(), 0));
    }

    #[test]
    fn categorical_orders_lexicographically() {
        let v_b1 = MetricValue::Text("b".to_string());
        let v_a = MetricValue::Text("a".to_string());
        let v_b2 = MetricValue::Text("b".to_string());
        let values = vec![&v_b1, &v_a, &v_b2];

        let result = distribution(&SummaryKind::Categorical, &values);

        // Expect: a=1 first, then b=2
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], ("a".to_string(), 1));
        assert_eq!(result[1], ("b".to_string(), 2));
    }

    #[test]
    fn summary_renders_expected_fragments() {
        let json = r#"{
            "name": "test-study",
            "description": "A test study",
            "seeds": { "from": 1, "count": 2 },
            "pin_sets": [
                { "label": "default", "pins": [] }
            ],
            "metrics": ["star-class"]
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        study.validate().unwrap();

        // Hand-build a RunResult with 2 rows, 1 pin set, 1 metric (star-class which is Categorical)
        let result = RunResult {
            study,
            metric_names: vec!["star-class"],
            rows: vec![
                Row {
                    seed: 1,
                    pin_set: "default".to_string(),
                    values: vec![MetricValue::Text("A".to_string())],
                    refusal: None,
                },
                Row {
                    seed: 2,
                    pin_set: "default".to_string(),
                    values: vec![MetricValue::Text("B".to_string())],
                    refusal: None,
                },
            ],
        };

        let output = render_summary(&result);

        // Check header comment
        assert!(output.contains(
            "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab run`. -->"
        ));

        // Check Study header
        assert!(output.contains("## Study: test-study"));
        assert!(output.contains("A test study"));

        // Check seeds line
        assert!(output.contains("Seeds 1..3 × 1 pin set(s); 2 rows; 0 refusals."));

        // Check metric header
        assert!(output.contains("### star-class — default"));

        // Check table header
        assert!(output.contains("| value | count | share |"));

        // Check rows
        assert!(output.contains("| A | 1 | 50.0% |"));
        assert!(output.contains("| B | 1 | 50.0% |"));
    }

    #[test]
    fn summary_is_deterministic() {
        let json = r#"{
            "name": "determinism-test",
            "description": "Test determinism",
            "seeds": { "from": 0, "count": 3 },
            "pin_sets": [
                { "label": "default", "pins": [] }
            ],
            "metrics": ["star-class", "moons-admitted"]
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        study.validate().unwrap();

        let result = RunResult {
            study,
            metric_names: vec!["star-class", "moons-admitted"],
            rows: vec![
                Row {
                    seed: 0,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("K".to_string()),
                        MetricValue::Text("2".to_string()),
                    ],
                    refusal: None,
                },
                Row {
                    seed: 1,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("M".to_string()),
                        MetricValue::Text("1".to_string()),
                    ],
                    refusal: None,
                },
                Row {
                    seed: 2,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("K".to_string()),
                        MetricValue::Text("2".to_string()),
                    ],
                    refusal: None,
                },
            ],
        };

        let output1 = render_summary(&result);
        let output2 = render_summary(&result);

        assert_eq!(output1, output2);
    }

    #[test]
    fn refusal_section_appears_when_present() {
        let json = r#"{
            "name": "refusal-test",
            "description": "Test refusals",
            "seeds": { "from": 0, "count": 2 },
            "pin_sets": [
                { "label": "default", "pins": [] }
            ],
            "metrics": ["star-class"]
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        study.validate().unwrap();

        let result = RunResult {
            study,
            metric_names: vec!["star-class"],
            rows: vec![
                Row {
                    seed: 0,
                    pin_set: "default".to_string(),
                    values: vec![MetricValue::Text("G".to_string())],
                    refusal: None,
                },
                Row {
                    seed: 1,
                    pin_set: "default".to_string(),
                    values: vec![MetricValue::Absent],
                    refusal: Some("test refusal reason".to_string()),
                },
            ],
        };

        let output = render_summary(&result);

        // Check refusal section
        assert!(output.contains("### Refusals"));
        assert!(output.contains("| pin set | count | example reason |"));
        assert!(output.contains("| default | 1 | test refusal reason |"));
        assert!(output.contains("Seeds 0..2 × 1 pin set(s); 2 rows; 1 refusals."));
    }
}
