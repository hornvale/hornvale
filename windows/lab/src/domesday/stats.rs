//! Per-metric summary statistics over the committed census.

use crate::domesday::census::Census;
use std::collections::BTreeMap;

/// Summary statistics for one numeric metric.
/// type-audit: bare-ok(count: n), bare-ok(count: absent), bare-ok(artifact: min), bare-ok(artifact: p25), bare-ok(artifact: median), bare-ok(artifact: p75), bare-ok(artifact: max), bare-ok(artifact: mean)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NumericStats {
    /// Worlds with a present value.
    pub n: usize,
    /// Worlds with no value for this metric.
    pub absent: usize,
    /// Smallest present value.
    pub min: f64,
    /// 25th percentile (nearest-rank).
    pub p25: f64,
    /// The median (see [`median`] — deliberately *not* `percentile(0.5)`).
    pub median: f64,
    /// 75th percentile (nearest-rank).
    pub p75: f64,
    /// Largest present value.
    pub max: f64,
    /// Arithmetic mean of present values.
    pub mean: f64,
}

/// Nearest-rank percentile over a sorted slice. Deterministic and index-based;
/// no interpolation, so the result is always a value that actually occurs.
/// Matches `windows/lab/tests/the_fare_calibration.rs`'s `percentile`
/// (`rank = ceil(p/100 · n)`, clamped, 1-indexed then converted to a
/// 0-index) — see [`median`]'s doc for why the two are never interchanged.
fn percentile(sorted: &[f64], q: f64) -> f64 {
    assert!(!sorted.is_empty(), "percentile of an empty population");
    let idx = ((q * sorted.len() as f64).ceil() as usize).saturating_sub(1);
    sorted[idx.min(sorted.len() - 1)]
}

/// The median of a nonempty, ascending-sorted slice: the middle value for an
/// odd-length sample, the average of the two middle values for an
/// even-length one.
///
/// Deliberately **not** `percentile(sorted, 0.5)`: that nearest-rank formula
/// picks the *lower* of the two middle values on an even-length sample,
/// which silently differs from this whenever the pair disagrees. This
/// matches the project's existing precedent —
/// `windows/lab/tests/the_fare_calibration.rs`'s `median` (itself matching
/// `the_mire_calibration.rs`) — whose own doc comment warns against exactly
/// this collapse: *"A bare `sorted[len / 2]` is an upper-median on an
/// even-length sample, which silently differs from this."* Do not "clean
/// up" `numeric`'s call to use `percentile(&vals, 0.5)` instead — the two
/// are intentionally distinct (see `median_and_percentile_diverge_on_an_
/// even_length_sample` below).
fn median(sorted: &[f64]) -> f64 {
    assert!(!sorted.is_empty(), "median of an empty population");
    let n = sorted.len();
    if n % 2 == 1 {
        sorted[n / 2]
    } else {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    }
}

/// Statistics for a numeric metric, or `None` if no world has a value.
/// type-audit: bare-ok(identifier-text: metric)
pub fn numeric(c: &Census, metric: &str) -> Option<NumericStats> {
    let mut vals: Vec<f64> = c
        .values(metric)
        .iter()
        .filter_map(|v| v.parse::<f64>().ok())
        .filter(|v| v.is_finite())
        .collect();
    if vals.is_empty() {
        return None;
    }
    vals.sort_by(|a, b| a.total_cmp(b));
    let n = vals.len();
    Some(NumericStats {
        n,
        absent: c.absent_count(metric),
        min: vals[0],
        p25: percentile(&vals, 0.25),
        median: median(&vals),
        p75: percentile(&vals, 0.75),
        max: vals[n - 1],
        mean: vals.iter().sum::<f64>() / n as f64,
    })
}

/// Value counts for a categorical or flag metric, count descending with
/// lexicographic tie-breaks.
/// type-audit: bare-ok(identifier-text: metric), bare-ok(identifier-text: return), bare-ok(count: return)
pub fn categorical(c: &Census, metric: &str) -> Vec<(String, usize)> {
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for v in c.values(metric) {
        *counts.entry(v.to_string()).or_insert(0) += 1;
    }
    let mut out: Vec<(String, usize)> = counts.into_iter().collect();
    out.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    out
}

#[cfg(test)]
mod tests {
    use crate::domesday::census::{Census, Column};
    use std::collections::BTreeMap;

    fn table(kind: &str, name: &str, vals: &[&str]) -> Census {
        Census {
            columns: vec![Column {
                name: name.into(),
                kind: kind.into(),
                doc: String::new(),
                domain: "climate".into(),
                role: "descriptor".into(),
            }],
            rows: vals
                .iter()
                .map(|v| BTreeMap::from([(name.to_string(), (*v).to_string())]))
                .collect(),
        }
    }

    #[test]
    fn numeric_quartiles_on_a_known_set() {
        let c = table("numeric", "m", &["1", "2", "3", "4", "5"]);
        let s = super::numeric(&c, "m").expect("stats");
        assert_eq!(s.n, 5);
        assert_eq!(s.min, 1.0);
        assert_eq!(s.p25, 2.0, "nearest-rank: ceil(0.25*5)=2 -> index 1");
        assert_eq!(s.median, 3.0);
        assert_eq!(s.p75, 4.0, "nearest-rank: ceil(0.75*5)=4 -> index 3");
        assert_eq!(s.max, 5.0);
        assert_eq!(s.mean, 3.0);
    }

    #[test]
    fn absent_values_are_reported_not_dropped() {
        let c = table("numeric", "m", &["1", "", "3"]);
        let s = super::numeric(&c, "m").expect("stats");
        assert_eq!(s.n, 2, "two present");
        assert_eq!(s.absent, 1, "one absent, and it is REPORTED");
    }

    #[test]
    fn categorical_orders_by_count_then_lexicographically() {
        let c = table("categorical", "m", &["b", "a", "a", "c", "c"]);
        let got = super::categorical(&c, "m");
        assert_eq!(
            got,
            vec![
                ("a".to_string(), 2),
                ("c".to_string(), 2),
                ("b".to_string(), 1)
            ],
            "count descending, ties lexicographic — deterministic"
        );
    }

    #[test]
    fn a_metric_with_no_present_values_yields_none() {
        let c = table("numeric", "m", &["", ""]);
        assert!(super::numeric(&c, "m").is_none());
    }

    #[test]
    fn matches_the_known_live_median() {
        let c = crate::domesday::census::load(
            &crate::domesday::census::repo_root().join("book/src/laboratory/generated/the-census"),
        )
        .expect("census");

        let s = super::numeric(&c, "mean-land-temperature-c").expect("stats");
        // Verified directly against the committed CSV (1000 worlds, this
        // metric absent on none of them) using the repo's `median()`
        // convention (average the two middle values on an even-length
        // sample). An earlier hand measurement pinned median = -11.90,
        // computed with a numpy-`nearest`-style formula (0-indexed
        // `q·(n-1)`, round-half-to-even) that this project explicitly does
        // NOT use for `median` — `the_fare_calibration.rs`'s doc comment
        // warns against exactly that collapse. min/max are unaffected by
        // the median-vs-percentile distinction, so they carry over exactly.
        // THE GLASSHOUSE (Stage B, k = 0.30) re-measure. Every figure below
        // moved, because this is the metric the campaign exists to re-centre
        // and the census behind it was refreshed on the canonical box:
        //
        //     median  -11.988568 -> -3.649021     (+8.34 K)
        //     min     -47.151131 -> -35.378763
        //     max      23.141691 ->  21.844769
        //     p25     -22.551606 -> -10.781073    (+11.77 K)
        //     p75       2.037309 ->   4.502500
        //
        // The distribution did not merely shift, it TIGHTENED: the p25-p75
        // span narrows 24.59 -> 15.28 K and the min rises 11.77 K while the
        // max FALLS 1.30 K. That is the thermostat doing what a thermostat
        // does — compensating the extremes harder than the middle — and it is
        // visible here in a test that pins percentiles for an unrelated
        // reason (proving `median` and `percentile` use different formulae).
        // The formula distinction this comment block was originally written
        // to defend is untouched; only the sample moved.
        assert!(
            (s.median - (-3.649021)).abs() < 1e-4,
            "median was {}",
            s.median
        );
        assert!((s.min - (-35.378763)).abs() < 1e-4, "min was {}", s.min);
        assert!((s.max - 21.844769).abs() < 1e-4, "max was {}", s.max);
        // p25/p75 are `percentile`'s nearest-rank formula (ceil(q·n)),
        // independently verified against the committed CSV.
        assert!((s.p25 - (-10.781073)).abs() < 1e-4, "p25 was {}", s.p25);
        assert!((s.p75 - 4.502500).abs() < 1e-4, "p75 was {}", s.p75);

        // A second metric with a different distribution shape, so a
        // percentile formula that happens to land right on one metric
        // (e.g. an off-by-one that only shows up at certain n or certain
        // clustering) cannot hide behind a single sample.
        // Ocean-fraction moved too (p25 0.559787 -> 0.556443, p75 0.683219 ->
        // 0.683316), and it is worth saying WHY it is here at all, because it
        // is the one metric in this test that `k` cannot touch: this second
        // sample exists to stop a percentile off-by-one hiding behind a single
        // distribution shape, and it keeps doing that job. Its movement is the
        // TERRAIN epoch's — the committed census predates this campaign's Task
        // 2 craton rescale — not the thermostat's. Two different causes, one
        // refresh; a reader attributing all of this test's movement to the
        // climate work would be wrong about half of it.
        let o = super::numeric(&c, "ocean-fraction").expect("stats");
        assert!(
            (o.p25 - 0.556443).abs() < 1e-4,
            "ocean-fraction p25 was {}",
            o.p25
        );
        assert!(
            (o.p75 - 0.683316).abs() < 1e-4,
            "ocean-fraction p75 was {}",
            o.p75
        );
    }

    #[test]
    fn median_and_percentile_diverge_on_an_even_length_sample() {
        // Four values, two middles differ (2.0 vs 3.0): `median` must
        // average them; `percentile(0.5)` (nearest-rank) must pick the
        // lower one alone. If a later "cleanup" collapses `numeric`'s call
        // from `median(&vals)` to `percentile(&vals, 0.5)`, this fails.
        let c = table("numeric", "m", &["1", "2", "3", "10"]);
        let s = super::numeric(&c, "m").expect("stats");
        assert_eq!(s.median, 2.5, "average of the two middle values (2, 3)");
        assert_ne!(
            s.median, s.p75,
            "sanity: p75 is a different rank than the median in this set"
        );
        // The nearest-rank pick for p=0.5 on this same sorted set is the
        // *lower* middle (2.0), which must NOT equal the reported median.
        let nearest_rank_p50 = 2.0;
        assert_ne!(
            s.median, nearest_rank_p50,
            "median and percentile(0.5) must diverge on an even-length sample \
             with unequal middles — see stats.rs's `median` doc comment"
        );
    }
}
