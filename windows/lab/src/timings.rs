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
}
