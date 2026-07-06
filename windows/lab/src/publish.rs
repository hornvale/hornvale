//! Publish a run result's summary and charts as book artifacts.

use crate::RunResult;
use crate::chart::charts_for;
use crate::summary::render_summary;
use std::fs;
use std::path::{Path, PathBuf};

/// Delete every existing file in `dir` whose file name starts with `prefix`,
/// in deterministic (sorted) order. Used to clear stale artifacts from a
/// previous `publish` run before writing fresh ones.
fn remove_stale_artifacts(dir: &Path, prefix: &str) -> std::io::Result<()> {
    let mut stale: Vec<PathBuf> = fs::read_dir(dir)?
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with(prefix))
        })
        .collect();

    stale.sort();

    for path in stale {
        fs::remove_file(path)?;
    }

    Ok(())
}

/// Write `{study.name}-summary.md` and every `{stem}.svg` chart for a run
/// result into `generated_dir`, creating it if needed.
///
/// Before writing, every existing file in `generated_dir` whose name starts
/// with `{study.name}-` is deleted, so artifacts from a previous run whose
/// metrics or pin sets no longer exist don't linger. Files belonging to
/// other studies are left untouched.
///
/// Returns the written paths, sorted.
pub fn publish(result: &RunResult, generated_dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    fs::create_dir_all(generated_dir)?;

    let prefix = format!("{}-", result.study.name);
    remove_stale_artifacts(generated_dir, &prefix)?;

    let mut written = Vec::new();

    let summary_path = generated_dir.join(format!("{}-summary.md", result.study.name));
    fs::write(&summary_path, render_summary(result))?;
    written.push(summary_path);

    for (stem, svg) in charts_for(result) {
        let chart_path = generated_dir.join(format!("{}.svg", stem));
        fs::write(&chart_path, svg)?;
        written.push(chart_path);
    }

    written.sort();
    Ok(written)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricSelection, MetricValue, PinSet, Row, Seeds, Study};

    fn build_result() -> RunResult {
        let study = Study {
            name: "publish-study".to_string(),
            description: "for publish tests".to_string(),
            seeds: Seeds { from: 0, count: 2 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
            }],
            metrics: MetricSelection::Named(vec![
                "star-class".to_string(),
                "moons-admitted".to_string(),
            ]),
        };

        RunResult {
            study,
            metric_names: vec!["star-class", "moons-admitted"],
            rows: vec![
                Row {
                    seed: 0,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("G".to_string()),
                        MetricValue::Text("1".to_string()),
                    ],
                    refusal: None,
                },
                Row {
                    seed: 1,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("K".to_string()),
                        MetricValue::Text("2".to_string()),
                    ],
                    refusal: None,
                },
            ],
        }
    }

    #[test]
    fn publish_writes_summary_and_all_charts_sorted() {
        let result = build_result();
        let dir =
            std::env::temp_dir().join(format!("hornvale-publish-test-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);

        let written = publish(&result, &dir).expect("publish should succeed");

        // 1 summary + 2 metrics * 1 pin set = 3 files.
        assert_eq!(written.len(), 3);

        let mut sorted_expected = written.clone();
        sorted_expected.sort();
        assert_eq!(written, sorted_expected, "paths must be returned sorted");

        let summary_path = dir.join("publish-study-summary.md");
        assert!(written.contains(&summary_path));
        assert!(summary_path.exists());

        let chart1 = dir.join("publish-study-default-star-class.svg");
        let chart2 = dir.join("publish-study-default-moons-admitted.svg");
        assert!(written.contains(&chart1));
        assert!(written.contains(&chart2));
        assert!(chart1.exists());
        assert!(chart2.exists());

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn publish_removes_stale_artifacts_from_previous_run() {
        let result = build_result();
        let dir = std::env::temp_dir().join(format!(
            "hornvale-publish-stale-test-{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let stale_path = dir.join("publish-study-oldpin-oldmetric.svg");
        fs::write(&stale_path, "stale").unwrap();

        let written = publish(&result, &dir).expect("publish should succeed");

        assert!(!stale_path.exists(), "stale artifact should be removed");
        for path in &written {
            assert!(path.exists());
        }

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn publish_leaves_other_studies_artifacts_untouched() {
        let result = build_result();
        let dir = std::env::temp_dir().join(format!(
            "hornvale-publish-other-test-{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let other_path = dir.join("other-study-summary.md");
        fs::write(&other_path, "other study content").unwrap();

        publish(&result, &dir).expect("publish should succeed");

        assert!(other_path.exists(), "other study's artifacts must survive");
        assert_eq!(
            fs::read_to_string(&other_path).unwrap(),
            "other study content"
        );

        fs::remove_dir_all(&dir).unwrap();
    }
}
