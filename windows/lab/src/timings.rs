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
/// silent empty parse would leave the alarm green forever. A record whose
/// `exec_time` is negative, `NaN`, or infinite is also an ERROR (mirroring
/// `parse_baseline`): a `NaN` `current` duration compares false against every
/// threshold, so that test would silently never alarm, and a `NaN` folded into
/// `suite_shift`'s sum poisons the WHOLE suite total to `NaN` — masking every
/// other regression in the run, not just the one bad record.
/// type-audit: bare-ok(prose: json_lines), bare-ok(prose: return)
pub fn parse_run(json_lines: &str) -> Result<Vec<TestDuration>, String> {
    let mut out = Vec::new();
    for (n, line) in json_lines.lines().enumerate() {
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
        if !secs.is_finite() || secs < 0.0 {
            return Err(format!(
                "line {}: test '{id}' has exec_time {secs}, which is not a \
                 finite, non-negative duration",
                n + 1
            ));
        }
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
/// would grow without bound. `git log -p` on this file is the file's whole
/// stated purpose, and that purpose only works if a row that did not
/// meaningfully move does not appear as a diff line. Two things previously
/// defeated it on every single run across ~2574 rows: the sha was stamped
/// PER ROW (constant across the file, so it carried zero per-row
/// information but still touched every line), and durations were written at
/// full nanosecond precision, which never repeats bit-for-bit run to run.
/// The sha now lives once, in the header; durations are rounded to
/// milliseconds (3 decimal places) — below that, `PER_TEST_FLOOR_SECS`
/// (5s) means the noise is irrelevant to the alarm anyway.
/// type-audit: bare-ok(identifier-text: sha), bare-ok(prose: return)
pub fn render_baseline(rows: &[TestDuration], sha: &str) -> String {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| a.id.cmp(&b.id));
    let mut s = String::new();
    let _ = writeln!(
        s,
        "# Hornvale test-duration baseline (The Timekeeper). Recorded at {sha}.\n\
         # One row per test: <test-id>\\t<seconds, millisecond precision>.\n\
         # Rewritten by `make ci`; history lives in git, so `git log -p` on\n\
         # this file is the record."
    );
    for r in &sorted {
        let _ = writeln!(s, "{}\t{:.3}", r.id, r.seconds);
    }
    s
}

/// Parse a baseline back. Comment lines (`#`) and blanks are skipped; a
/// malformed data row is an error rather than a skipped line, so a corrupted
/// baseline cannot quietly disable the alarm. "Malformed" covers: not
/// exactly two tab-separated fields (a third means a tab leaked into the
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
        let [id, secs] = fields[..] else {
            return Err(format!(
                "baseline line {}: expected <id>\\t<seconds>, got {} field(s)",
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

/// Whether `rows` is sorted by `id` — the precondition `lookup`'s
/// `binary_search_by` silently relies on. An unsorted slice makes
/// `binary_search_by` return "not found" for present ids at random, and
/// "not found" reads as "new test, never alarm" — so an unsorted baseline
/// would quietly disable the alarm instead of failing loudly. Checked with
/// `debug_assert!` at the call sites rather than returning a `Result`,
/// because both `parse_run` and `parse_baseline` already sort their output;
/// this exists to catch a hand-built slice in a test or a future caller, not
/// a real production path.
fn is_sorted_by_id(rows: &[TestDuration]) -> bool {
    rows.windows(2).all(|w| w[0].id <= w[1].id)
}

/// Tests that crossed BOTH the absolute floor and the multiple. A test absent
/// from the baseline is new and never alarms.
///
/// PRECONDITION: `baseline` must be sorted by `id` (as `parse_run` and
/// `parse_baseline` both return it). See `is_sorted_by_id`'s doc for why a
/// violation is silent rather than loud.
/// type-audit: bare-ok(diagnostic-value: current), bare-ok(diagnostic-value: baseline)
pub fn per_test_shifts(current: &[TestDuration], baseline: &[TestDuration]) -> Vec<Shift> {
    debug_assert!(
        is_sorted_by_id(baseline),
        "per_test_shifts: baseline must be sorted by id, or binary_search_by \
         silently drops present ids from the lookup"
    );
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

/// Total measured seconds across `rows` — the number a human wants beside an
/// alarm when deciding whether growth is legitimate (e.g. alongside
/// `suite_shift`'s intersection-only totals, which deliberately exclude
/// added or removed tests).
/// type-audit: bare-ok(diagnostic-value: return)
pub fn total_seconds(rows: &[TestDuration]) -> f64 {
    rows.iter().map(|r| r.seconds).sum()
}

/// The aggregate alarm, summed over the INTERSECTION of `current` and
/// `baseline` by id — not the full totals of each side. Summing full totals
/// would trip the alarm on pure test-count growth (this repo adds tests
/// constantly; an alarm that fires on normal development gets tuned out and
/// ignored) and would let a real regression hide behind removed tests
/// (fewer rows can shrink the `current` total even while the surviving tests
/// got slower). The intersection isolates the one question this alarm
/// exists to answer: did the tests present on both sides get slower. An
/// empty baseline is a first run: record only.
///
/// PRECONDITION: `baseline` must be sorted by `id`. See `is_sorted_by_id`'s
/// doc for why a violation is silent rather than loud.
/// type-audit: bare-ok(diagnostic-value: current), bare-ok(diagnostic-value: baseline)
pub fn suite_shift(current: &[TestDuration], baseline: &[TestDuration]) -> Option<Shift> {
    if baseline.is_empty() {
        return None;
    }
    debug_assert!(
        is_sorted_by_id(baseline),
        "suite_shift: baseline must be sorted by id, or binary_search_by \
         silently drops present ids from the intersection"
    );
    let mut now = 0.0;
    let mut was = 0.0;
    for c in current {
        if let Some(b) = lookup(baseline, &c.id) {
            now += c.seconds;
            was += b;
        }
    }
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
    fn the_sha_appears_once_in_the_header_not_per_row() {
        // Finding 5: a per-row sha (constant across the file) touched every
        // one of ~2574 lines on every run, defeating `git log -p` as a
        // review surface. The sha belongs in the header exactly once.
        let rows = vec![TestDuration {
            id: "a::one".into(),
            seconds: 0.125,
        }];
        let text = render_baseline(&rows, "deadbeef");
        assert_eq!(
            text.matches("deadbeef").count(),
            1,
            "the sha should appear exactly once (in the header), got: {text}"
        );
        let data_line = text
            .lines()
            .find(|l| !l.starts_with('#') && !l.trim().is_empty())
            .expect("a data row");
        assert_eq!(
            data_line.split('\t').count(),
            2,
            "a data row is <id>\\t<seconds> now that the sha lives in the header, got: {data_line}"
        );
    }

    #[test]
    fn durations_are_rendered_at_millisecond_precision() {
        // Full nanosecond precision (e.g. 0.010709583) never repeats
        // bit-for-bit run to run, so every row changed on every `make ci` —
        // the opposite of a reviewable diff. Rounding to 3 decimal places
        // (milliseconds) means a test that did not meaningfully move
        // produces an identical row.
        let rows = vec![TestDuration {
            id: "a::one".into(),
            seconds: 0.010_709_583,
        }];
        let text = render_baseline(&rows, "deadbeef");
        assert!(
            text.contains("a::one\t0.011"),
            "expected a row rounded to 3 decimal places, got: {text}"
        );
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
    fn parse_baseline_rejects_a_row_missing_the_duration() {
        let err = parse_baseline("a::one\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("1 field"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_row_with_an_extra_field() {
        let err = parse_baseline("a::one\t1.5\textra\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("3 field"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_non_numeric_duration() {
        let err = parse_baseline("a::one\tabc\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("not a number"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_nan() {
        let err = parse_baseline("a::one\tnan\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("finite"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_infinity() {
        let err = parse_baseline("a::one\tinf\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("finite"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_negative_duration() {
        let err = parse_baseline("a::one\t-1.5\n").unwrap_err();
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

    // --- Fix round 1 -------------------------------------------------------

    #[test]
    fn a_correctly_sorted_multi_entry_baseline_still_alarms() {
        // Every other per-test-shifts test uses a single-entry baseline, which
        // can't distinguish "lookup works" from "lookup always succeeds
        // trivially." This pins that binary_search_by finds the right row
        // among several when the baseline precondition (sorted by id) holds.
        let baseline = vec![d("a", 6.0), d("b", 6.0), d("c", 6.0)];
        let current = vec![d("a", 1.0), d("b", 20.0), d("c", 1.0)];
        let shifts = per_test_shifts(&current, &baseline);
        assert_eq!(shifts.len(), 1, "got {shifts:?}");
        assert_eq!(shifts[0].id, "b");
    }

    #[test]
    fn parse_run_rejects_a_negative_exec_time() {
        let stream = r#"{"type":"test","event":"ok","name":"a","exec_time":-1.5}"#;
        let err = parse_run(stream).unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("finite, non-negative"), "got: {err}");
    }

    #[test]
    fn parse_run_silently_drops_an_out_of_range_exec_time_rather_than_reporting_it() {
        // Empirically verified: serde_json's own number parser rejects any
        // literal that overflows f64 ("number out of range") BEFORE our code
        // ever sees a `Value`, so `1e400` can never reach this module's
        // finiteness check via valid JSON text — it falls into the existing
        // "not our business, might be interleaved output" skip, same as any
        // other malformed line. This pins that reality: the bad line is
        // dropped, not fatal, and a well-formed sibling still parses. The
        // finiteness check on `secs` is retained anyway as defense against a
        // future field that COMPUTES rather than parses a value into `secs`.
        let stream = "{\"type\":\"test\",\"event\":\"ok\",\"name\":\"a\",\"exec_time\":1e400}\n\
                       {\"type\":\"test\",\"event\":\"ok\",\"name\":\"b\",\"exec_time\":1.0}\n";
        let rows = parse_run(stream).expect("the overflowing line is dropped, not fatal");
        assert_eq!(rows.len(), 1, "only the well-formed record survives");
        assert_eq!(rows[0].id, "b");
    }

    #[test]
    fn parse_run_silently_drops_a_bare_nan_or_infinity_token() {
        // JSON (RFC 8259) has no literal spelling for NaN or Infinity, so a
        // stream emitting the bare word (as some non-conformant loggers do)
        // fails serde_json's parse outright and is skipped by the existing
        // malformed-line fallback — it never reaches this module's
        // finiteness check either. Confirmed empirically alongside the
        // out-of-range case above.
        let stream = "{\"type\":\"test\",\"event\":\"ok\",\"name\":\"a\",\"exec_time\":NaN}\n\
                       {\"type\":\"test\",\"event\":\"ok\",\"name\":\"b\",\"exec_time\":1.0}\n";
        let rows = parse_run(stream).expect("the malformed line is dropped, not fatal");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].id, "b");
    }

    #[test]
    fn adding_new_tests_does_not_alarm_the_suite() {
        // Pure test-count growth (this repo adds tests constantly) must not
        // trip the alarm: an alarm that fires on ordinary development gets
        // tuned out and stops meaning anything.
        let base: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.0)).collect();
        let mut now = base.clone();
        now.extend((100..150).map(|i| d(&format!("t{i:03}"), 0.01)));
        assert!(
            suite_shift(&now, &base).is_none(),
            "50 new fast tests must not move the intersection total"
        );
    }

    #[test]
    fn removing_tests_does_not_mask_a_regression_in_the_survivors() {
        // Full-totals math would compare the unchanged 100.0 baseline total
        // against a current total of only 100.0 (50 tests at 2.0s each) and
        // see 0% growth -- masking that the 50 surviving tests DOUBLED.
        // Intersection math compares only the 50 ids present on both sides.
        let base: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.0)).collect();
        let now: Vec<_> = (0..50).map(|i| d(&format!("t{i:03}"), 2.0)).collect();
        let s = suite_shift(&now, &base).expect("the intersection alarms");
        assert_eq!(s.id, "<whole suite>");
        assert_eq!(
            s.baseline, 50.0,
            "intersection baseline sums only the 50 surviving ids, not all 100"
        );
        assert_eq!(s.current, 100.0);
    }

    #[test]
    fn total_seconds_of_an_empty_slice_is_zero() {
        assert_eq!(total_seconds(&[]), 0.0);
    }

    #[test]
    fn total_seconds_sums_every_row() {
        let rows = vec![d("a", 1.5), d("b", 2.5)];
        assert_eq!(total_seconds(&rows), 4.0);
    }
}
