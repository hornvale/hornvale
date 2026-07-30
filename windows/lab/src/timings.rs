//! Recorded durations for the repo's own test suite (The Timekeeper).

use serde_json::Value;

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
}
