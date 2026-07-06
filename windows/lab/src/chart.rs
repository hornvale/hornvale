//! Deterministic hand-rolled SVG bar charts, sharing aggregation with `summary`.

use crate::summary::distribution;
use crate::{MetricValue, RunResult};

const VIEW_WIDTH: f64 = 640.0;
const VIEW_HEIGHT: f64 = 300.0;
const MARGIN_LEFT: f64 = 56.0;
const MARGIN_RIGHT: f64 = 16.0;
const MARGIN_TOP: f64 = 36.0;
const MARGIN_BOTTOM: f64 = 64.0;
const PLOT_LEFT: f64 = MARGIN_LEFT;
const PLOT_RIGHT: f64 = VIEW_WIDTH - MARGIN_RIGHT;
const PLOT_TOP: f64 = MARGIN_TOP;
const PLOT_BOTTOM: f64 = VIEW_HEIGHT - MARGIN_BOTTOM;

/// Round a value up to a "nice" number: 1, 2, or 5 times a power of 10.
/// 0 rounds up to 1.
fn nice_ceiling(value: f64) -> f64 {
    if value <= 0.0 {
        return 1.0;
    }

    let exponent = value.log10().floor();
    let mut power = 10f64.powf(exponent);
    // Guard against float drift landing just above the true power of ten.
    if power > value {
        power /= 10.0;
    }

    for candidate_mult in [1.0, 2.0, 2.5, 5.0, 10.0] {
        let candidate = candidate_mult * power;
        if candidate >= value - f64::EPSILON {
            return candidate;
        }
    }

    // Unreachable in practice: 10 * power always covers value.
    10.0 * power
}

/// Format a nice-number gridline value with no decimals for integers.
fn format_axis_value(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{}", value as i64)
    } else {
        format!("{}", value)
    }
}

/// Render a single-series bar chart as a deterministic SVG string.
pub fn bar_chart_svg(title: &str, x_labels: &[String], counts: &[u64]) -> String {
    let mut svg = String::new();

    svg.push_str(&format!(
        "<svg viewBox=\"0 0 {:.0} {:.0}\" width=\"100%\" xmlns=\"http://www.w3.org/2000/svg\" font-family=\"ui-sans-serif, system-ui, sans-serif\">\n",
        VIEW_WIDTH, VIEW_HEIGHT
    ));

    svg.push_str(&format!(
        "<text x=\"56\" y=\"20\" font-size=\"13\" font-weight=\"600\" fill=\"currentColor\">{}</text>\n",
        title
    ));

    let max_count = counts.iter().copied().max().unwrap_or(0);
    let nice_max = nice_ceiling(max_count as f64);

    // Gridlines at 25/50/75/100% of nice-max.
    for fraction in [0.25, 0.5, 0.75, 1.0] {
        let value = nice_max * fraction;
        let y = PLOT_BOTTOM - (PLOT_BOTTOM - PLOT_TOP) * fraction;
        svg.push_str(&format!(
            "<line x1=\"{:.1}\" y1=\"{:.1}\" x2=\"{:.1}\" y2=\"{:.1}\" stroke=\"currentColor\" stroke-opacity=\"0.15\" stroke-width=\"1\" />\n",
            PLOT_LEFT, y, PLOT_RIGHT, y
        ));
        svg.push_str(&format!(
            "<text x=\"50\" y=\"{:.1}\" font-size=\"11\" fill=\"currentColor\" opacity=\"0.75\" text-anchor=\"end\">{}</text>\n",
            y,
            format_axis_value(value)
        ));
    }

    let n = x_labels.len();
    let plot_width = PLOT_RIGHT - PLOT_LEFT;
    let any_long_label = x_labels.iter().any(|l| l.len() > 8);

    if n > 0 {
        let slot = plot_width / n as f64;
        let gap = (slot * 0.15_f64).max(2.0);
        let bar_width = slot - gap;

        for (i, (label, count)) in x_labels.iter().zip(counts.iter()).enumerate() {
            let slot_start = PLOT_LEFT + i as f64 * slot;
            let bar_x = slot_start + gap / 2.0;
            let cx = slot_start + slot / 2.0;

            if *count > 0 {
                let height = (*count as f64 / nice_max) * (PLOT_BOTTOM - PLOT_TOP);
                let bar_y = PLOT_BOTTOM - height;
                svg.push_str(&format!(
                    "<rect x=\"{:.1}\" y=\"{:.1}\" width=\"{:.1}\" height=\"{:.1}\" fill=\"#5b8dd9\" rx=\"2\" />\n",
                    bar_x, bar_y, bar_width, height
                ));
                svg.push_str(&format!(
                    "<text x=\"{:.1}\" y=\"{:.1}\" font-size=\"11\" fill=\"currentColor\" text-anchor=\"middle\">{}</text>\n",
                    cx,
                    bar_y - 4.0,
                    count
                ));
            } else {
                svg.push_str(&format!(
                    "<text x=\"{:.1}\" y=\"{:.1}\" font-size=\"11\" fill=\"currentColor\" text-anchor=\"middle\">{}</text>\n",
                    cx, 232.0, count
                ));
            }

            let ly = 252.0_f64;
            if any_long_label {
                svg.push_str(&format!(
                    "<text x=\"{:.1}\" y=\"{:.1}\" font-size=\"11\" fill=\"currentColor\" opacity=\"0.85\" text-anchor=\"end\" transform=\"rotate(-30 {:.1} {:.1})\">{}</text>\n",
                    cx, ly, cx, ly, label
                ));
            } else {
                svg.push_str(&format!(
                    "<text x=\"{:.1}\" y=\"{:.1}\" font-size=\"11\" fill=\"currentColor\" opacity=\"0.85\" text-anchor=\"middle\">{}</text>\n",
                    cx, ly, label
                ));
            }
        }
    }

    svg.push_str("</svg>\n");
    svg
}

/// Build one bar chart per (pin set, metric) pair for a run result, sharing
/// the same aggregation code path as `render_summary` so tables and charts
/// can never disagree.
///
/// Returns `(file_stem, svg)` pairs where `file_stem` is
/// `"{study.name}-{pinset_label}-{metric_name}"`.
pub fn charts_for(result: &RunResult) -> Vec<(String, String)> {
    let mut charts = Vec::new();

    let metrics = result
        .study
        .selected_metrics()
        .expect("RunResult was built from a validated study");

    for pin_set in &result.study.pin_sets {
        let pin_rows: Vec<_> = result
            .rows
            .iter()
            .filter(|r| r.pin_set == pin_set.label)
            .collect();

        for (metric_idx, metric_name) in result.metric_names.iter().enumerate() {
            let metric = metrics.iter().find(|m| m.name == *metric_name).unwrap();

            let values: Vec<&MetricValue> =
                pin_rows.iter().map(|r| &r.values[metric_idx]).collect();

            let dist = distribution(&metric.summary, &values);
            let x_labels: Vec<String> = dist.iter().map(|(label, _)| label.clone()).collect();
            let counts: Vec<u64> = dist.iter().map(|(_, count)| *count).collect();

            let title = format!("{} — {}", metric_name, pin_set.label);
            let svg = bar_chart_svg(&title, &x_labels, &counts);

            let stem = format!("{}-{}-{}", result.study.name, pin_set.label, metric_name);
            charts.push((stem, svg));
        }
    }

    charts
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricSelection, PinSet, Row, Seeds, Study};

    #[test]
    fn nice_ceiling_rounds_up_to_1_2_5_pattern() {
        assert_eq!(nice_ceiling(7.0), 10.0);
        assert_eq!(nice_ceiling(23.0), 25.0);
        assert_eq!(nice_ceiling(480.0), 500.0);
        assert_eq!(nice_ceiling(0.0), 1.0);
    }

    #[test]
    fn nice_ceiling_exact_nice_numbers_stay_put() {
        assert_eq!(nice_ceiling(10.0), 10.0);
        assert_eq!(nice_ceiling(25.0), 25.0);
        assert_eq!(nice_ceiling(50.0), 50.0);
        assert_eq!(nice_ceiling(100.0), 100.0);
    }

    #[test]
    fn svg_contains_required_fragments() {
        let labels = vec!["A".to_string(), "B".to_string()];
        let counts = vec![3u64, 5u64];
        let svg = bar_chart_svg("My Title", &labels, &counts);

        assert!(svg.contains("#5b8dd9"));
        assert!(svg.contains("currentColor"));
        assert!(svg.contains("viewBox=\"0 0 640 300\""));
        assert!(svg.contains("My Title"));

        // One <rect per nonzero count.
        let rect_count = svg.matches("<rect").count();
        assert_eq!(rect_count, 2);
    }

    #[test]
    fn zero_counts_draw_no_rect() {
        let labels = vec!["A".to_string(), "B".to_string()];
        let counts = vec![0u64, 5u64];
        let svg = bar_chart_svg("Title", &labels, &counts);

        let rect_count = svg.matches("<rect").count();
        assert_eq!(rect_count, 1);

        // Zero-bar label still rendered, at baseline.
        assert!(svg.contains(">0</text>"));
    }

    #[test]
    fn rotation_present_when_a_label_exceeds_eight_chars() {
        let labels = vec!["short".to_string(), "a-very-long-label".to_string()];
        let counts = vec![1u64, 2u64];
        let svg = bar_chart_svg("Title", &labels, &counts);

        assert!(svg.contains("rotate(-30"));
        assert!(svg.contains("text-anchor=\"end\" transform=\"rotate(-30"));
    }

    #[test]
    fn no_rotation_when_all_labels_short() {
        let labels = vec!["ab".to_string(), "cd".to_string()];
        let counts = vec![1u64, 2u64];
        let svg = bar_chart_svg("Title", &labels, &counts);

        assert!(!svg.contains("rotate(-30"));
    }

    #[test]
    fn svg_is_deterministic() {
        let labels = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        let counts = vec![3u64, 0u64, 7u64];

        let svg1 = bar_chart_svg("Title", &labels, &counts);
        let svg2 = bar_chart_svg("Title", &labels, &counts);

        assert_eq!(svg1, svg2);
    }

    fn build_result() -> RunResult {
        let study = Study {
            name: "chart-study".to_string(),
            description: "for chart tests".to_string(),
            seeds: Seeds { from: 0, count: 2 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
            }],
            metrics: MetricSelection::Named(vec!["star-class".to_string()]),
        };

        RunResult {
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
                    values: vec![MetricValue::Text("K".to_string())],
                    refusal: None,
                },
            ],
        }
    }

    #[test]
    fn charts_for_produces_expected_stems() {
        let result = build_result();
        let charts = charts_for(&result);

        assert_eq!(charts.len(), 1);
        assert_eq!(charts[0].0, "chart-study-default-star-class");
        assert!(charts[0].1.contains("star-class — default"));
    }
}
