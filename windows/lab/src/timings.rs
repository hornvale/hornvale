//! Recorded durations for the repo's own test suite (The Timekeeper).

use serde_json::Value;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// One test's measured wall time from a nextest run, or (for the reserved
/// [`BELOW_FLOOR_ID`] row) the folded sum of every test that measured below
/// [`BASELINE_FLOOR_SECS`] on that run.
/// type-audit: bare-ok(identifier-text: id), bare-ok(diagnostic-value: seconds), bare-ok(count: folded_count)
#[derive(Debug, Clone, PartialEq)]
pub struct TestDuration {
    /// Fully-qualified nextest test id, e.g. `hornvale-kernel::lib$mod::name`.
    /// [`BELOW_FLOOR_ID`] for the aggregate row.
    pub id: String,
    /// Wall seconds the test took, as nextest reported it. For the
    /// aggregate row this is the SUM over every folded test.
    pub seconds: f64,
    /// `None` for an ordinary per-test row (always exactly one test).
    /// `Some(n)` only on the aggregate row: how many tests were folded into
    /// it this run. A change in `n` run to run is itself information (a
    /// test crossed the floor), so it is carried alongside the summed
    /// seconds rather than discarded.
    pub folded_count: Option<u32>,
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
            folded_count: None,
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

/// Below this, a test is folded into the aggregate [`BELOW_FLOOR_ID`] row
/// instead of getting its own stored line.
///
/// Measured on this box across two consecutive quiet `make ci` runs, before
/// this floor existed: of 2573 tests, the whole-suite median jitter was
/// 16.9% (p90 60.7%, p99 162.1%) — noise, not signal, since none of it can
/// ever cross `PER_TEST_MULTIPLE`'s 2x. Restricted to tests >= 1.0s the
/// median jitter fell to 3.8% (p90 9.3%), and among the >= 5s
/// `PER_TEST_FLOOR_SECS` alarm-eligible tests it fell to 2.9% (p90 7.0%,
/// p99 11.7%) — real machine variance, not the reason the file was
/// unreviewable. The tests below 1.0s are 2037 of 2573 rows (the entire
/// source of the file's churn) but only 0.8% of total suite runtime, so
/// folding them costs the alarm nothing: `PER_TEST_FLOOR_SECS` (5s) is
/// already five times this floor, so no individually-stored row is lost
/// from per-test alarm eligibility. CHOSEN from this measurement, not
/// derived — same status as `PER_TEST_MULTIPLE`/`SUITE_TOLERANCE` (spec A1).
/// type-audit: bare-ok(diagnostic-value)
pub const BASELINE_FLOOR_SECS: f64 = 1.0;

/// Reserved test id for the folded aggregate row. Cannot collide with a real
/// nextest id, which always look like `crate::binary$module::name` (a `::`
/// and a `$`, neither of which this string contains).
/// type-audit: bare-ok(identifier-text)
pub const BELOW_FLOOR_ID: &str = "<below-floor>";

/// Fold every row under [`BASELINE_FLOOR_SECS`] into one [`BELOW_FLOOR_ID`]
/// row carrying the summed seconds and the count folded in; rows at or above
/// the floor pass through unchanged. Applied to BOTH the measurement being
/// written to the baseline and (symmetrically) the current run being
/// compared against it, so the two shapes always match by id — an
/// unfolded `current` against a folded `baseline` would silently drop every
/// sub-floor test from `suite_shift`'s intersection instead of rolling it
/// into the aggregate.
/// type-audit: bare-ok(diagnostic-value: floor)
pub fn fold_below_floor(rows: &[TestDuration], floor: f64) -> Vec<TestDuration> {
    let mut kept = Vec::new();
    let mut folded_seconds = 0.0;
    let mut folded_count: u32 = 0;
    for r in rows {
        if r.seconds >= floor {
            kept.push(r.clone());
        } else {
            folded_seconds += r.seconds;
            folded_count += 1;
        }
    }
    if folded_count > 0 {
        kept.push(TestDuration {
            id: BELOW_FLOOR_ID.to_string(),
            seconds: folded_seconds,
            folded_count: Some(folded_count),
        });
    }
    kept.sort_by(|a, b| a.id.cmp(&b.id));
    kept
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
/// (5s) means the noise is irrelevant to the alarm anyway. A third source of
/// churn — ~2037 of 2573 rows moving on ordinary machine jitter, none of it
/// ever alarm-eligible — is handled upstream of this function: callers pass
/// already-folded ([`fold_below_floor`]) and already-hysteresis-applied
/// ([`apply_hysteresis`]) rows, so this function only ever sees what should
/// actually be written.
/// type-audit: bare-ok(identifier-text: sha), bare-ok(prose: return)
pub fn render_baseline(rows: &[TestDuration], sha: &str) -> String {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| a.id.cmp(&b.id));
    let mut s = String::new();
    let _ = writeln!(
        s,
        "# Hornvale test-duration baseline (The Timekeeper). Recorded at {sha}.\n\
         # One row per test: <test-id>\\t<seconds, millisecond precision>.\n\
         # The reserved id {BELOW_FLOOR_ID} carries a THIRD field instead:\n\
         # {BELOW_FLOOR_ID}\\t<summed seconds>\\t<count of tests folded in>.\n\
         # Rewritten by `make ci`; history lives in git, so `git log -p` on\n\
         # this file is the record."
    );
    for r in &sorted {
        match r.folded_count {
            Some(n) => {
                let _ = writeln!(s, "{}\t{:.3}\t{}", r.id, r.seconds, n);
            }
            None => {
                let _ = writeln!(s, "{}\t{:.3}", r.id, r.seconds);
            }
        }
    }
    s
}

/// Parse a baseline back. Comment lines (`#`) and blanks are skipped; a
/// malformed data row is an error rather than a skipped line, so a corrupted
/// baseline cannot quietly disable the alarm. "Malformed" covers: not
/// exactly two tab-separated fields (a third means a tab leaked into the
/// id, and silently keeping it would corrupt the mapping) UNLESS the id is
/// the reserved [`BELOW_FLOOR_ID`], which carries exactly three fields (see
/// `render_baseline`); a non-numeric duration or count; and a duration that
/// is negative, `NaN`, or infinite — any of which would compare false
/// against a threshold downstream and silently defeat the regression alarm
/// for that row.
/// type-audit: bare-ok(prose: text), bare-ok(prose: return)
pub fn parse_baseline(text: &str) -> Result<Vec<TestDuration>, String> {
    let mut out = Vec::new();
    for (n, line) in text.lines().enumerate() {
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        let (id, secs, folded_count) = match fields[..] {
            [id, secs] => (id, secs, None),
            [id, secs, count] if id == BELOW_FLOOR_ID => {
                let count: u32 = count.parse().map_err(|_| {
                    format!("baseline line {}: '{count}' is not a valid count", n + 1)
                })?;
                if count == 0 {
                    return Err(format!(
                        "baseline line {}: {BELOW_FLOOR_ID} carries a folded count of 0, which \
                         means it should not have been written at all",
                        n + 1
                    ));
                }
                (id, secs, Some(count))
            }
            _ => {
                return Err(format!(
                    "baseline line {}: expected <id>\\t<seconds>, got {} field(s)",
                    n + 1,
                    fields.len()
                ));
            }
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
            folded_count,
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

/// A stored duration is kept unchanged unless the new measurement differs
/// from it by more than this fraction — see [`apply_hysteresis`]. Measured
/// (see [`BASELINE_FLOOR_SECS`]'s doc): among the >= 1.0s tests this floor
/// alone leaves individually stored, median run-to-run jitter was 3.8%
/// (p90 9.3%), so 20% sits comfortably above ordinary noise and far below
/// `PER_TEST_MULTIPLE`'s 2x (100%) — it can never mask a per-test alarm.
/// Modelled against two real consecutive runs: floor + this deadband
/// together left 6 changed rows, vs 2405 with neither. CHOSEN from this
/// measurement, not derived (spec A1).
/// type-audit: bare-ok(ratio)
pub const BASELINE_DEADBAND: f64 = 0.20;

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

impl Shift {
    /// Seconds gained (positive) or lost (negative) versus the baseline —
    /// the ranking key `top_contributors` sorts by.
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn delta_seconds(&self) -> f64 {
        self.current - self.baseline
    }
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
/// from the baseline is new and never alarms. The reserved [`BELOW_FLOOR_ID`]
/// aggregate row is skipped here even if it happens to cross the threshold —
/// it is not a specific test, so it cannot be the "something specific went
/// pathological" this alarm exists to name; accumulated growth among the
/// folded tests is `suite_shift`/`top_contributors`'s story to tell, not
/// this one's.
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
        if c.id == BELOW_FLOOR_ID {
            continue;
        }
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

/// For each test in `current`, keep the OLD stored value from `previous`
/// unless the new measurement differs from it by more than
/// [`BASELINE_DEADBAND`]; otherwise take the new measurement. A test absent
/// from `previous` (new) always takes its current measurement — there is no
/// old value to hold onto. A test absent from `current` (removed since the
/// last record) is simply not emitted; the caller drops it by construction.
///
/// The stored value therefore becomes "the last significantly-different
/// measurement", not "the last measurement" — deliberate: it stops the
/// baseline ratcheting upward on noise, so genuine slow creep accumulates
/// against a FIXED reference instead of being continuously re-absorbed by a
/// baseline that moves every run. The deadband (20%) is far below the
/// per-test alarm's multiple (2x, i.e. 100%), so it can never mask an alarm
/// — a shift big enough to alarm is always far outside the deadband too.
///
/// `folded_count` is never smoothed: it always reflects `current`'s own
/// count, because a change in it is itself information (spec, §3b) that
/// hysteresis on `seconds` must not hide.
///
/// PRECONDITION: `previous` must be sorted by `id`. See `is_sorted_by_id`'s
/// doc for why a violation is silent rather than loud.
/// type-audit: bare-ok(diagnostic-value: current), bare-ok(diagnostic-value: previous)
pub fn apply_hysteresis(current: &[TestDuration], previous: &[TestDuration]) -> Vec<TestDuration> {
    debug_assert!(
        is_sorted_by_id(previous),
        "apply_hysteresis: previous must be sorted by id, or binary_search_by \
         silently drops present ids from the lookup"
    );
    let mut out: Vec<TestDuration> = current
        .iter()
        .map(|c| {
            let seconds = match lookup(previous, &c.id) {
                Some(p) if p > 0.0 && ((c.seconds - p).abs() / p) <= BASELINE_DEADBAND => p,
                _ => c.seconds,
            };
            TestDuration {
                id: c.id.clone(),
                seconds,
                folded_count: c.folded_count,
            }
        })
        .collect();
    out.sort_by(|a, b| a.id.cmp(&b.id));
    out
}

/// The `n` tests present in BOTH `current` and `baseline` (same
/// intersection discipline as `suite_shift`) with the largest ABSOLUTE
/// seconds gained, largest first — the profile list for the whole-suite
/// alarm message. Unlike `per_test_shifts`, the reserved [`BELOW_FLOOR_ID`]
/// row is eligible here: "the fast tests collectively grew" is exactly the
/// kind of accumulated-feature story this ranking exists to surface, not a
/// pathological single test.
///
/// PRECONDITION: `baseline` must be sorted by `id`. See `is_sorted_by_id`'s
/// doc for why a violation is silent rather than loud.
/// type-audit: bare-ok(count: n)
pub fn top_contributors(
    current: &[TestDuration],
    baseline: &[TestDuration],
    n: usize,
) -> Vec<Shift> {
    debug_assert!(
        is_sorted_by_id(baseline),
        "top_contributors: baseline must be sorted by id, or binary_search_by \
         silently drops present ids from the intersection"
    );
    let mut shifts: Vec<Shift> = current
        .iter()
        .filter_map(|c| {
            lookup(baseline, &c.id).map(|b| Shift {
                id: c.id.clone(),
                baseline: b,
                current: c.seconds,
            })
        })
        .collect();
    shifts.sort_by(|a, b| b.delta_seconds().total_cmp(&a.delta_seconds()));
    shifts.truncate(n);
    shifts
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
                folded_count: None,
            },
            TestDuration {
                id: "a::one".into(),
                seconds: 0.125,
                folded_count: None,
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
            folded_count: None,
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
            folded_count: None,
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
            folded_count: None,
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

    // --- fold_below_floor ---------------------------------------------------

    #[test]
    fn fold_below_floor_keeps_rows_at_or_above_the_floor_unchanged() {
        let rows = vec![d("a", 1.0), d("b", 5.0)];
        let folded = fold_below_floor(&rows, 1.0);
        assert_eq!(folded, vec![d("a", 1.0), d("b", 5.0)], "nothing to fold");
    }

    #[test]
    fn fold_below_floor_sums_rows_under_the_floor_into_one_aggregate_row() {
        let rows = vec![d("a", 0.1), d("b", 0.2), d("c", 5.0)];
        let folded = fold_below_floor(&rows, 1.0);
        assert_eq!(folded.len(), 2, "the two sub-floor rows collapse to one");
        let agg = folded
            .iter()
            .find(|r| r.id == BELOW_FLOOR_ID)
            .expect("aggregate row present");
        assert!(
            (agg.seconds - 0.3).abs() < 1e-9,
            "summed seconds, got {}",
            agg.seconds
        );
        assert_eq!(agg.folded_count, Some(2));
        assert!(
            folded.iter().any(|r| r.id == "c" && r.seconds == 5.0),
            "the floor-or-above row is untouched"
        );
    }

    #[test]
    fn fold_below_floor_emits_no_aggregate_row_when_nothing_is_under_the_floor() {
        let rows = vec![d("a", 1.0), d("b", 2.0)];
        let folded = fold_below_floor(&rows, 1.0);
        assert!(
            !folded.iter().any(|r| r.id == BELOW_FLOOR_ID),
            "no sub-floor tests -> no aggregate row, got {folded:?}"
        );
    }

    #[test]
    fn fold_below_floor_of_an_empty_slice_is_empty() {
        assert_eq!(fold_below_floor(&[], 1.0), Vec::new());
    }

    // --- the aggregate row's three-field baseline shape ---------------------

    #[test]
    fn a_baseline_with_the_aggregate_row_round_trips() {
        let rows = vec![
            d("a::one", 1.5),
            TestDuration {
                id: BELOW_FLOOR_ID.to_string(),
                seconds: 0.437,
                folded_count: Some(2037),
            },
        ];
        let text = render_baseline(&rows, "deadbeef");
        let back = parse_baseline(&text).expect("parses");
        let agg = back
            .iter()
            .find(|r| r.id == BELOW_FLOOR_ID)
            .expect("aggregate row survives the round trip");
        assert_eq!(agg.seconds, 0.437);
        assert_eq!(agg.folded_count, Some(2037), "the count round-trips too");
    }

    #[test]
    fn render_baseline_emits_three_fields_for_the_aggregate_row_and_two_for_the_rest() {
        let rows = vec![
            d("a::one", 1.5),
            TestDuration {
                id: BELOW_FLOOR_ID.to_string(),
                seconds: 0.437,
                folded_count: Some(3),
            },
        ];
        let text = render_baseline(&rows, "deadbeef");
        let mut data_lines = text.lines().filter(|l| !l.starts_with('#'));
        let agg_line = data_lines
            .clone()
            .find(|l| l.starts_with(BELOW_FLOOR_ID))
            .expect("aggregate row present");
        assert_eq!(agg_line.split('\t').count(), 3, "got: {agg_line}");
        let ordinary_line = data_lines
            .find(|l| !l.starts_with(BELOW_FLOOR_ID))
            .expect("row");
        assert_eq!(ordinary_line.split('\t').count(), 2, "got: {ordinary_line}");
    }

    #[test]
    fn parse_baseline_rejects_three_fields_on_an_ordinary_id() {
        // Only the reserved BELOW_FLOOR_ID id may carry a third field; on any
        // other id a third field means a tab leaked into the id (Finding 5's
        // failure mode), and silently keeping it would corrupt the mapping.
        let err = parse_baseline("a::one\t1.5\t3\n").unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("3 field"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_non_numeric_folded_count() {
        let err = parse_baseline(&format!("{BELOW_FLOOR_ID}\t0.5\tabc\n")).unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("not a valid count"), "got: {err}");
    }

    #[test]
    fn parse_baseline_rejects_a_zero_folded_count() {
        // A row that folded zero tests should never have been written at
        // all (fold_below_floor only emits the aggregate row when count > 0)
        // — a stored 0 is corruption, not a valid empty aggregate.
        let err = parse_baseline(&format!("{BELOW_FLOOR_ID}\t0.0\t0\n")).unwrap_err();
        assert!(err.contains("line 1"), "got: {err}");
        assert!(err.contains("folded count of 0"), "got: {err}");
    }

    // --- apply_hysteresis -----------------------------------------------------

    #[test]
    fn apply_hysteresis_keeps_the_old_value_within_the_deadband() {
        // 6.0 -> 6.5 is an 8.3% move, under the 20% deadband: keep 6.0.
        let current = vec![d("a", 6.5)];
        let previous = vec![d("a", 6.0)];
        let result = apply_hysteresis(&current, &previous);
        assert_eq!(result, vec![d("a", 6.0)], "stays at the old value");
    }

    #[test]
    fn apply_hysteresis_takes_the_new_value_outside_the_deadband() {
        // 6.0 -> 8.0 is a 33% move, over the 20% deadband: take 8.0.
        let current = vec![d("a", 8.0)];
        let previous = vec![d("a", 6.0)];
        let result = apply_hysteresis(&current, &previous);
        assert_eq!(result, vec![d("a", 8.0)], "moves to the new value");
    }

    #[test]
    fn apply_hysteresis_takes_the_new_value_for_a_test_with_no_previous_entry() {
        let current = vec![d("brand-new", 42.0)];
        let result = apply_hysteresis(&current, &[]);
        assert_eq!(result, vec![d("brand-new", 42.0)], "nothing to hold onto");
    }

    #[test]
    fn apply_hysteresis_drops_a_test_absent_from_current() {
        let current = vec![d("a", 1.0)];
        let previous = vec![d("a", 1.0), d("removed", 9.0)];
        let result = apply_hysteresis(&current, &previous);
        assert_eq!(
            result,
            vec![d("a", 1.0)],
            "the removed test is not carried forward"
        );
    }

    #[test]
    fn apply_hysteresis_never_smooths_folded_count() {
        // Even when `seconds` stays at its old value (within the deadband),
        // `folded_count` always reflects the CURRENT run: a change in count
        // is itself information the deadband must not hide.
        let current = vec![TestDuration {
            id: BELOW_FLOOR_ID.to_string(),
            seconds: 0.44,
            folded_count: Some(2040),
        }];
        let previous = vec![TestDuration {
            id: BELOW_FLOOR_ID.to_string(),
            seconds: 0.43,
            folded_count: Some(2037),
        }];
        let result = apply_hysteresis(&current, &previous);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].seconds, 0.43, "seconds held by the deadband");
        assert_eq!(
            result[0].folded_count,
            Some(2040),
            "count always tracks the current run"
        );
    }

    #[test]
    fn apply_hysteresis_zero_previous_value_falls_back_to_current() {
        // Division by a zero previous value must not panic or produce NaN.
        let current = vec![d("a", 0.5)];
        let previous = vec![d("a", 0.0)];
        let result = apply_hysteresis(&current, &previous);
        assert_eq!(result, vec![d("a", 0.5)]);
    }

    // --- top_contributors -----------------------------------------------------

    /// claim: structural(seed: none) — false-positive seed-loop flag; `s` binds
    /// a Contributor row, no world seed at all (pure timing-analysis fixtures)
    #[test]
    fn top_contributors_ranks_by_absolute_seconds_gained_descending() {
        let baseline = vec![d("a", 10.0), d("b", 10.0), d("c", 10.0)];
        // a gained 1s, b gained 5s, c gained 3s.
        let current = vec![d("a", 11.0), d("b", 15.0), d("c", 13.0)];
        let top = top_contributors(&current, &baseline, 10);
        assert_eq!(
            top.iter().map(|s| s.id.as_str()).collect::<Vec<_>>(),
            vec!["b", "c", "a"],
            "got {top:?}"
        );
    }

    #[test]
    fn top_contributors_truncates_to_n() {
        let baseline: Vec<_> = (0..5).map(|i| d(&format!("t{i}"), 1.0)).collect();
        let current: Vec<_> = (0..5)
            .map(|i| d(&format!("t{i}"), 1.0 + i as f64))
            .collect();
        let top = top_contributors(&current, &baseline, 2);
        assert_eq!(top.len(), 2, "truncated to n, got {top:?}");
    }

    #[test]
    fn top_contributors_excludes_ids_absent_from_baseline() {
        let baseline = vec![d("a", 10.0)];
        let current = vec![d("a", 11.0), d("brand-new", 99.0)];
        let top = top_contributors(&current, &baseline, 10);
        assert_eq!(top.len(), 1);
        assert_eq!(
            top[0].id, "a",
            "the new test has no baseline to compare against"
        );
    }

    #[test]
    fn top_contributors_includes_the_below_floor_aggregate_row() {
        // BELOW_FLOOR_ID ("<below-floor>") sorts before ordinary ids
        // ('<' < any lowercase letter), so it comes first once sorted — the
        // precondition `lookup`'s binary_search_by relies on.
        let baseline = vec![
            TestDuration {
                id: BELOW_FLOOR_ID.to_string(),
                seconds: 5.0,
                folded_count: Some(2000),
            },
            d("a", 10.0),
        ];
        let current = vec![
            TestDuration {
                id: BELOW_FLOOR_ID.to_string(),
                seconds: 20.0,
                folded_count: Some(2000),
            },
            d("a", 10.5),
        ];
        let top = top_contributors(&current, &baseline, 1);
        assert_eq!(
            top[0].id, BELOW_FLOOR_ID,
            "the aggregate's 15s gain dwarfs a's 0.5s, and it must be eligible to rank"
        );
    }

    // --- per_test_shifts and the aggregate row ---------------------------------

    #[test]
    fn per_test_shifts_never_alarms_on_the_below_floor_aggregate_row() {
        // A pathological sum across thousands of folded tests could easily
        // cross 5s and 2x, but the aggregate is not a specific test — that
        // story belongs to suite_shift/top_contributors, not this alarm.
        let baseline = vec![TestDuration {
            id: BELOW_FLOOR_ID.to_string(),
            seconds: 3.0,
            folded_count: Some(2000),
        }];
        let current = vec![TestDuration {
            id: BELOW_FLOOR_ID.to_string(),
            seconds: 30.0,
            folded_count: Some(2000),
        }];
        assert!(
            per_test_shifts(&current, &baseline).is_empty(),
            "the aggregate row must never itself trigger the per-test alarm"
        );
    }
}
