//! Recorded durations for the repo's own test suite (The Timekeeper).

use serde_json::Value;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// One test's measured wall time from a nextest run.
/// type-audit: bare-ok(identifier-text: id), bare-ok(diagnostic-value: seconds)
#[derive(Debug, Clone, PartialEq)]
pub struct TestDuration {
    /// Fully-qualified nextest test id, e.g. `hornvale-kernel::lib$mod::name`.
    pub id: String,
    /// Wall seconds the test took, as nextest reported it.
    pub seconds: f64,
}

/// Parse a `libtest-json-plus` stream into per-test durations, sorted by id.
///
/// Only `{"type":"test"}` records carrying an `exec_time` are kept; `suite`
/// lines and `started` events are ignored. A stream with no test records is an
/// ERROR, not an empty result: the format is experimental (spec A2), and a
/// silent empty parse would leave the alarm green forever.
/// type-audit: bare-ok(prose: json_lines), bare-ok(prose: return)
pub fn parse_run(json_lines: &str) -> Result<Vec<TestDuration>, String> {
    let mut out = Vec::new();
    for line in json_lines.lines() {
        let line = line.trim();
        if line.is_empty() || !line.starts_with('{') {
            continue;
        }
        let Ok(v) = serde_json::from_str::<Value>(line) else {
            continue; // interleaved human output; not our business
        };
        if v.get("type").and_then(Value::as_str) != Some("test") {
            continue;
        }
        let (Some(id), Some(secs)) = (
            v.get("name").and_then(Value::as_str),
            v.get("exec_time").and_then(Value::as_f64),
        ) else {
            continue; // `started` events carry no exec_time
        };
        out.push(TestDuration {
            id: id.to_string(),
            seconds: secs,
        });
    }
    if out.is_empty() {
        return Err(
            "no test records in the nextest stream — libtest-json-plus is \
             experimental and its shape may have changed; refusing to report \
             an empty run as a clean one"
                .to_string(),
        );
    }
    out.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(out)
}

/// Where this machine's baseline lives. One file PER HOST: the Mac and the
/// canonical box differ by roughly 4x, so a shared baseline would alarm on
/// every cross-machine run.
/// type-audit: bare-ok(identifier-text: host)
pub fn baseline_path(repo_root: &Path, host: &str) -> PathBuf {
    repo_root
        .join("docs/timings")
        .join(format!("test-baseline-{host}.tsv"))
}

/// Render a baseline: sorted, tab-separated, one row per test.
///
/// The file holds only the PRESENT; git holds the history, which is what makes
/// `git log -p` the archaeology tool (spec N1). Appending every run instead
/// would grow without bound.
/// type-audit: bare-ok(identifier-text: sha), bare-ok(prose: return)
pub fn render_baseline(rows: &[TestDuration], sha: &str) -> String {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| a.id.cmp(&b.id));
    let mut s = String::from(
        "# Hornvale test-duration baseline (The Timekeeper). One row per test:\n\
         # <test-id>\\t<seconds>\\t<sha-recorded-at>. Rewritten by `make ci`;\n\
         # history lives in git, so `git log -p` on this file is the record.\n",
    );
    for r in &sorted {
        let _ = writeln!(s, "{}\t{}\t{}", r.id, r.seconds, sha);
    }
    s
}

/// Parse a baseline back. Comment lines (`#`) and blanks are skipped; a
/// malformed data row is an error rather than a skipped line, so a corrupted
/// baseline cannot quietly disable the alarm. "Malformed" covers: not
/// exactly three tab-separated fields (a fourth means a tab leaked into the
/// id, and silently keeping it would corrupt the mapping), a non-numeric
/// duration, and a duration that is negative, `NaN`, or infinite — any of
/// which would compare false against a threshold downstream and silently
/// defeat the regression alarm for that row.
/// type-audit: bare-ok(prose: text), bare-ok(prose: return)
pub fn parse_baseline(text: &str) -> Result<Vec<TestDuration>, String> {
    let mut out = Vec::new();
    for (n, line) in text.lines().enumerate() {
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        let [id, secs, _sha] = fields[..] else {
            return Err(format!(
                "baseline line {}: expected <id>\\t<seconds>\\t<sha>, got {} field(s)",
                n + 1,
                fields.len()
            ));
        };
        let seconds: f64 = secs
            .parse()
            .map_err(|_| format!("baseline line {}: '{secs}' is not a number", n + 1))?;
        if !seconds.is_finite() || seconds < 0.0 {
            return Err(format!(
                "baseline line {}: '{secs}' is not a finite, non-negative duration",
                n + 1
            ));
        }
        out.push(TestDuration {
            id: id.to_string(),
            seconds,
        });
    }
    out.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(out)
}

/// Below this, a doubling is scheduler noise: most of the suite runs in
/// single-digit milliseconds. CHOSEN, not derived (spec A1) — revisit against
/// the baseline's own spread once several runs exist.
/// type-audit: bare-ok(diagnostic-value)
pub const PER_TEST_FLOOR_SECS: f64 = 5.0;

/// A test must exceed this multiple of its baseline to alarm.
/// CHOSEN, not derived (spec A1).
/// type-audit: bare-ok(ratio)
pub const PER_TEST_MULTIPLE: f64 = 2.0;

/// Fractional growth of the whole suite's total that alarms.
/// CHOSEN, not derived (spec A1).
/// type-audit: bare-ok(ratio)
pub const SUITE_TOLERANCE: f64 = 0.25;

/// One duration that moved beyond tolerance.
/// type-audit: bare-ok(identifier-text: id), bare-ok(diagnostic-value: baseline), bare-ok(diagnostic-value: current)
#[derive(Debug, Clone, PartialEq)]
pub struct Shift {
    /// Test id, or `<whole suite>` for the aggregate.
    pub id: String,
    /// The recorded baseline, in seconds.
    pub baseline: f64,
    /// What this run measured, in seconds.
    pub current: f64,
}

fn lookup(rows: &[TestDuration], id: &str) -> Option<f64> {
    rows.binary_search_by(|r| r.id.as_str().cmp(id))
        .ok()
        .map(|i| rows[i].seconds)
}

/// Tests that crossed BOTH the absolute floor and the multiple. A test absent
/// from the baseline is new and never alarms.
/// type-audit: bare-ok(diagnostic-value: current), bare-ok(diagnostic-value: baseline)
pub fn per_test_shifts(current: &[TestDuration], baseline: &[TestDuration]) -> Vec<Shift> {
    let mut out = Vec::new();
    for c in current {
        let Some(b) = lookup(baseline, &c.id) else {
            continue;
        };
        if c.seconds >= PER_TEST_FLOOR_SECS && c.seconds > b * PER_TEST_MULTIPLE {
            out.push(Shift {
                id: c.id.clone(),
                baseline: b,
                current: c.seconds,
            });
        }
    }
    out.sort_by(|a, b| b.current.total_cmp(&a.current));
    out
}

/// The aggregate alarm. Load-bearing and easy to omit: a suite can grow by
/// half without any single test doubling, which is the 234s -> 934s shape the
/// per-test alarm cannot see. An empty baseline is a first run: record only.
/// type-audit: bare-ok(diagnostic-value: current), bare-ok(diagnostic-value: baseline)
pub fn suite_shift(current: &[TestDuration], baseline: &[TestDuration]) -> Option<Shift> {
    if baseline.is_empty() {
        return None;
    }
    let now: f64 = current.iter().map(|r| r.seconds).sum();
    let was: f64 = baseline.iter().map(|r| r.seconds).sum();
    if was > 0.0 && now > was * (1.0 + SUITE_TOLERANCE) {
        return Some(Shift {
            id: "<whole suite>".to_string(),
            baseline: was,
            current: now,
        });
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"{"type":"suite","event":"started","test_count":2}
{"type":"test","event":"started","name":"crate::bin$mod::alpha"}
{"type":"test","event":"ok","name":"crate::bin$mod::alpha","exec_time":0.25}
{"type":"test","event":"failed","name":"crate::bin$mod::beta","exec_time":1.5}
{"type":"suite","event":"ok","passed":1,"failed":1,"exec_time":1.75}
"#;

    #[test]
    fn parse_run_reads_every_test_record_and_ignores_the_rest() {
        let rows = parse_run(SAMPLE).expect("parses");
        assert_eq!(rows.len(), 2, "two test records, suite lines ignored");
        assert_eq!(rows[0].id, "crate::bin$mod::alpha");
        assert_eq!(rows[0].seconds, 0.25);
        assert_eq!(rows[1].id, "crate::bin$mod::beta");
        assert_eq!(rows[1].seconds, 1.5, "a failed test still has a duration");
    }

    #[test]
    fn parse_run_errors_rather_than_reporting_an_empty_run() {
        // An unrecognised shape must fail LOUDLY: libtest-json-plus is
        // experimental, and silently recording zero durations would make the
        // alarm permanently, invisibly green (spec A2).
        let err = parse_run(r#"{"type":"suite","event":"ok"}"#).unwrap_err();
        assert!(err.contains("no test records"), "got: {err}");
    }

    #[test]
    fn a_baseline_round_trips() {
        let rows = vec![
            TestDuration {
                id: "b::two".into(),
                seconds: 2.5,
            },
            TestDuration {
                id: "a::one".into(),
                seconds: 0.125,
            },
        ];
        let text = render_baseline(&rows, "deadbeef");
        assert!(text.starts_with("# "), "carries a header comment");
        let back = parse_baseline(&text).expect("parses");
        assert_eq!(back.len(), 2);
        assert_eq!(back[0].id, "a::one", "rendered sorted by id");
        assert_eq!(back[0].seconds, 0.125);
        assert_eq!(back[1].id, "b::two");
    }

    #[test]
    fn the_baseline_path_is_per_host() {
        let p = baseline_path(std::path::Path::new("/repo"), "lefford");
        assert_eq!(
            p,
            std::path::Path::new("/repo/docs/timings/test-baseline-lefford.tsv")
        );
    }

    #[test]
    fn parse_baseline_rejects_a_row_missing_the_sha() {
        let err = parse_baseline("a::one\t1.5\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("2 field"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_row_with_an_extra_field() {
        let err = parse_baseline("a::one\t1.5\tdeadbeef\textra\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("4 field"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_non_numeric_duration() {
        let err = parse_baseline("a::one\tabc\tdeadbeef\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("not a number"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_nan() {
        let err = parse_baseline("a::one\tnan\tdeadbeef\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("finite"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_infinity() {
        let err = parse_baseline("a::one\tinf\tdeadbeef\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("finite"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_negative_duration() {
        let err = parse_baseline("a::one\t-1.5\tdeadbeef\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("finite"), "got: {err}");
    }

    fn d(id: &str, s: f64) -> TestDuration {
        TestDuration {
            id: id.into(),
            seconds: s,
        }
    }

    #[test]
    fn a_fast_test_doubling_is_below_the_floor_and_does_not_alarm() {
        // 7ms -> 15ms is scheduler noise across 2548 tests.
        let shifts = per_test_shifts(&[d("a", 0.015)], &[d("a", 0.007)]);
        assert!(shifts.is_empty(), "got {shifts:?}");
    }

    #[test]
    fn a_slow_test_doubling_alarms() {
        let shifts = per_test_shifts(&[d("a", 20.0)], &[d("a", 6.0)]);
        assert_eq!(shifts.len(), 1);
        assert_eq!(shifts[0].id, "a");
    }

    #[test]
    fn a_slow_test_growing_under_the_multiple_does_not_alarm() {
        let shifts = per_test_shifts(&[d("a", 9.0)], &[d("a", 6.0)]);
        assert!(shifts.is_empty(), "1.5x is under the 2x multiple");
    }

    #[test]
    fn a_test_with_no_baseline_never_alarms() {
        let shifts = per_test_shifts(&[d("brand-new", 600.0)], &[]);
        assert!(
            shifts.is_empty(),
            "a new test has nothing to regress against"
        );
    }

    #[test]
    fn death_by_a_thousand_cuts_alarms_on_the_suite_total() {
        // No single test doubles; the suite grows 40%. This is the 234s -> 934s
        // shape, and the per-test alarm is structurally blind to it.
        let base: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.0)).collect();
        let now: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.4)).collect();
        assert!(
            per_test_shifts(&now, &base).is_empty(),
            "no single test alarms"
        );
        let s = suite_shift(&now, &base).expect("the suite alarms");
        assert_eq!(s.id, "<whole suite>");
        assert_eq!(s.baseline, 100.0);
    }

    #[test]
    fn a_suite_within_tolerance_does_not_alarm() {
        let base: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.0)).collect();
        let now: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.2)).collect();
        assert!(
            suite_shift(&now, &base).is_none(),
            "20% is under the 25% bound"
        );
    }

    #[test]
    fn an_empty_baseline_never_alarms_the_suite() {
        assert!(
            suite_shift(&[d("a", 99.0)], &[]).is_none(),
            "first run records only"
        );
    }
}
