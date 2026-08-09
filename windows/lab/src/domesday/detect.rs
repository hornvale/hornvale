//! The eight preregistered weakness detectors (spec §4.4).
//!
//! Every threshold here is frozen at authoring time (decision 0016): D1's
//! 80 % share, D3's 5 % IQR-of-range, D6's per-comparator `band`, and D8's
//! measured-crate roster are preregistered and are **not** tuning knobs. If a
//! detector fires on more or fewer metrics than expected, that is itself a
//! finding to report — the falsification clause anticipates exactly this
//! (spec §4.4a) — not a threshold to move after seeing the output.
//!
//! D1 and D2 look only at `descriptor` metrics; an `invariant` is a claim the
//! project makes about every world, and a claim the data contradicts is a
//! **D7** finding (broken invariant), not a degeneracy. Without that split,
//! D1 alone would fire on 40 of 57 categorical/flag metrics (spec §4.1a). The
//! role split does real work — down to 27 — but S2c's preregistered ceiling
//! of 10 was a guess made without measuring the post-split distribution and
//! is FALSIFIED: it is retired, not raised to a flattering number. See
//! `the_live_census_reproduces_the_preregistered_findings`'s S2c comment.

use crate::domesday::census::Census;
use crate::domesday::comparators::{Comparator, Expectation};
use crate::domesday::stats::{categorical, numeric};
use std::collections::BTreeSet;

/// One finding a detector raised against a metric.
///
/// `detector` names which of D1-D8 raised it; `metric` is the census column
/// (or, for D8, the `domains/` crate) it concerns; `detail` is a
/// human-readable explanation carrying the actual numbers involved — every
/// number in it is read from the committed census at render time, never
/// invented (spec §4.6).
/// type-audit: bare-ok(identifier-text: detector), bare-ok(identifier-text: metric), bare-ok(prose: detail)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    /// Which detector raised this finding, e.g. `"D1"`.
    pub detector: &'static str,
    /// The census metric (or, for D8, the `domains/` crate name) involved.
    pub metric: String,
    /// A human-readable explanation, carrying the numbers that triggered it.
    pub detail: String,
}

/// Run all eight detectors over the committed census, in deterministic
/// order (sorted by detector, then metric — never `HashMap` iteration
/// order).
/// type-audit: bare-ok(identifier-text: return-is-not-primitive)
pub fn detect(c: &Census, cmps: &[Comparator], exps: &[Expectation]) -> Vec<Finding> {
    let mut out = Vec::new();
    out.extend(detect_d1(c));
    out.extend(detect_d2(c));
    out.extend(detect_d3(c));
    out.extend(detect_d4(c));
    out.extend(detect_d5(c, exps));
    out.extend(detect_d6(c, cmps));
    out.extend(detect_d7(c));
    out.extend(detect_d8());
    out.sort_by(|a, b| {
        a.detector
            .cmp(b.detector)
            .then_with(|| a.metric.cmp(&b.metric))
    });
    out
}

/// D1 Degenerate: a `descriptor` `categorical`/`flag` metric where one value
/// holds ≥ 80 % of present worlds. The 0.80 bar is frozen (spec §4.4a) —
/// 65.1 % ice-dominance on `dominant-land-biome` is a skew, not a
/// degeneracy, and must NOT fire here (that finding belongs to D6).
fn detect_d1(c: &Census) -> Vec<Finding> {
    let mut out = Vec::new();
    for col in &c.columns {
        if col.domain.is_empty() || col.role.is_empty() || col.role != "descriptor" {
            continue;
        }
        if col.kind != "categorical" && col.kind != "flag" {
            continue;
        }
        let counts = categorical(c, &col.name);
        let total: usize = counts.iter().map(|(_, n)| n).sum();
        if total == 0 {
            continue;
        }
        if let Some((top_value, top_count)) = counts.first() {
            let share = *top_count as f64 / total as f64;
            if share >= 0.80 {
                out.push(Finding {
                    detector: "D1",
                    metric: col.name.clone(),
                    detail: format!(
                        "{top_value:?} holds {top_count}/{total} worlds ({:.1}%), at or above the 80% threshold",
                        share * 100.0
                    ),
                });
            }
        }
    }
    out
}

/// D2 Frozen: a `descriptor` `numeric`/`integer` metric with `min == max`
/// across every present world.
fn detect_d2(c: &Census) -> Vec<Finding> {
    let mut out = Vec::new();
    for col in &c.columns {
        if col.domain.is_empty() || col.role.is_empty() || col.role != "descriptor" {
            continue;
        }
        if col.kind != "numeric" && col.kind != "integer" {
            continue;
        }
        if let Some(s) = numeric(c, &col.name)
            && s.min == s.max
        {
            out.push(Finding {
                detector: "D2",
                metric: col.name.clone(),
                detail: format!("min == median == max == {} across {} worlds", s.min, s.n),
            });
        }
    }
    out
}

/// D3 Narrow: a `numeric`/`integer` metric whose p25..p75 spans less than
/// 5 % of its min..max range (and the range is nonzero — a fully frozen
/// metric is D2's finding, not D3's).
fn detect_d3(c: &Census) -> Vec<Finding> {
    let mut out = Vec::new();
    for col in &c.columns {
        if col.domain.is_empty() || col.role.is_empty() {
            continue;
        }
        if col.kind != "numeric" && col.kind != "integer" {
            continue;
        }
        if let Some(s) = numeric(c, &col.name) {
            let range = s.max - s.min;
            if range <= 0.0 {
                continue;
            }
            let iqr = s.p75 - s.p25;
            if iqr < 0.05 * range {
                out.push(Finding {
                    detector: "D3",
                    metric: col.name.clone(),
                    detail: format!(
                        "p25..p75 spans {iqr} ({:.2}% of the {range} min..max range), under the 5% bar",
                        (iqr / range) * 100.0
                    ),
                });
            }
        }
    }
    out
}

/// D4 At-rail: a `numeric`/`integer` metric whose median equals its min or
/// its max (e.g. "median waterfall count is 0").
fn detect_d4(c: &Census) -> Vec<Finding> {
    let mut out = Vec::new();
    for col in &c.columns {
        if col.domain.is_empty() || col.role.is_empty() {
            continue;
        }
        if col.kind != "numeric" && col.kind != "integer" {
            continue;
        }
        if let Some(s) = numeric(c, &col.name)
            && (s.median == s.min || s.median == s.max)
        {
            let rail = if s.median == s.min { "min" } else { "max" };
            out.push(Finding {
                detector: "D4",
                metric: col.name.clone(),
                detail: format!(
                    "median {} equals the {rail} ({} .. {})",
                    s.median, s.min, s.max
                ),
            });
        }
    }
    out
}

/// Pearson correlation coefficient over paired samples, or `None` when
/// either side has zero variance (an undefined correlation) or fewer than
/// two pairs.
fn pearson(xs: &[f64], ys: &[f64]) -> Option<f64> {
    let n = xs.len();
    if n < 2 || n != ys.len() {
        return None;
    }
    let mean_x = xs.iter().sum::<f64>() / n as f64;
    let mean_y = ys.iter().sum::<f64>() / n as f64;
    let mut cov = 0.0;
    let mut var_x = 0.0;
    let mut var_y = 0.0;
    for i in 0..n {
        let dx = xs[i] - mean_x;
        let dy = ys[i] - mean_y;
        cov += dx * dy;
        var_x += dx * dx;
        var_y += dy * dy;
    }
    if var_x <= 0.0 || var_y <= 0.0 {
        return None;
    }
    Some(cov / (var_x.sqrt() * var_y.sqrt()))
}

/// Map an observed `|r|` to a conventional effect-size band (spec §4.4):
/// `>= 0.7` dominant, `0.5-0.7` strong, `0.3-0.5` moderate, `0.1-0.3` weak,
/// `< 0.1` none. These bands are external convention, not ours, and carry no
/// tuning freedom (spec §4.4, superseding the earlier `min_abs_r`
/// formulation).
fn band_of(abs_r: f64) -> &'static str {
    if abs_r >= 0.7 {
        "dominant"
    } else if abs_r >= 0.5 {
        "strong"
    } else if abs_r >= 0.3 {
        "moderate"
    } else if abs_r >= 0.1 {
        "weak"
    } else {
        "none"
    }
}

/// D5 Mis-declared strength: an expectation declares a relationship class;
/// fire when the observed Pearson `|r|`, mapped to a band, differs from it.
/// Pairs are formed from worlds where BOTH `metric` and `tracks` are
/// present, in row order (deterministic — no `HashMap`).
fn detect_d5(c: &Census, exps: &[Expectation]) -> Vec<Finding> {
    let mut out = Vec::new();
    for e in exps {
        let mut xs = Vec::new();
        let mut ys = Vec::new();
        for row in &c.rows {
            let mx = row
                .get(&e.metric)
                .filter(|v| !v.is_empty())
                .and_then(|v| v.parse::<f64>().ok());
            let my = row
                .get(&e.tracks)
                .filter(|v| !v.is_empty())
                .and_then(|v| v.parse::<f64>().ok());
            if let (Some(x), Some(y)) = (mx, my) {
                xs.push(x);
                ys.push(y);
            }
        }
        if let Some(r) = pearson(&xs, &ys) {
            let observed = band_of(r.abs());
            if observed != e.declared {
                out.push(Finding {
                    detector: "D5",
                    metric: e.metric.clone(),
                    detail: format!(
                        "declared {} tracking {}, but observed |r| = {:.3} ({} pairs) is {}",
                        e.declared,
                        e.tracks,
                        r.abs(),
                        xs.len(),
                        observed
                    ),
                });
            }
        }
    }
    out
}

/// D6 Off-comparator: a metric's median differs from a comparator's value
/// by more than its declared `band`.
fn detect_d6(c: &Census, cmps: &[Comparator]) -> Vec<Finding> {
    let mut out = Vec::new();
    for cmp in cmps {
        for (metric, value) in &cmp.values {
            let Some(band) = cmp.band.get(metric) else {
                continue;
            };
            if !c.has(metric) {
                continue;
            }
            if let Some(s) = numeric(c, metric) {
                let gap = (s.median - value).abs();
                if gap > *band {
                    out.push(Finding {
                        detector: "D6",
                        metric: metric.clone(),
                        detail: format!(
                            "median {} vs {}'s {} differs by {:.6}, exceeding the band {}",
                            s.median, cmp.name, value, gap, band
                        ),
                    });
                }
            }
        }
    }
    out
}

/// D7 Broken invariant: a metric declared `invariant` that is not constant
/// across present worlds — worth more than a degeneracy, because it
/// contradicts a claim rather than merely skewing a distribution.
fn detect_d7(c: &Census) -> Vec<Finding> {
    let mut out = Vec::new();
    for col in &c.columns {
        if col.domain.is_empty() || col.role.is_empty() || col.role != "invariant" {
            continue;
        }
        let distinct: BTreeSet<&str> = c.values(&col.name).into_iter().collect();
        if distinct.len() > 1 {
            out.push(Finding {
                detector: "D7",
                metric: col.name.clone(),
                detail: format!(
                    "declared invariant but takes {} distinct values: {:?}",
                    distinct.len(),
                    distinct
                ),
            });
        }
    }
    out
}

/// Every crate under `domains/`, frozen at authoring time (spec §4.4a — 13
/// crates as of this campaign). `domain_crates_are_still_exactly_this_list`
/// cross-checks this against the live tree so it cannot silently drift.
const DOMAIN_CRATES: &[&str] = &[
    "alchemy",
    "astronomy",
    "climate",
    "culture",
    "demography",
    "history",
    "language",
    "paleoclimate",
    "religion",
    "settlement",
    "species",
    "terrain",
    "topology",
];

/// Crates under `domains/` that at least one census metric reaches —
/// directly (a `hornvale_<crate>::` reference in `windows/lab/src/`) or
/// transitively (e.g. `topology` is never imported directly but
/// `defensibility-capacity-rank-corr` reaches it through
/// `hornvale_worldgen::connection_graph_of`). Hand-verified against the
/// source at authoring time (spec §4.4a); a metric is Rust code living in
/// this crate, so Rust's own dependency rules make this a real fact about
/// the source, not a guess.
const MEASURED_CRATES: &[&str] = &[
    "astronomy",
    "climate",
    "culture",
    "demography",
    "history",
    "language",
    "religion",
    "settlement",
    "species",
    "terrain",
    "topology",
];

/// D8 Unmeasured domain: a `domains/` crate no census metric reaches at
/// all — a gap in the WORLD (spec §4.6a), rendered rather than fixed.
/// Expected to fire on exactly `alchemy` and `paleoclimate` (spec §4.4a).
fn detect_d8() -> Vec<Finding> {
    DOMAIN_CRATES
        .iter()
        .filter(|crate_name| !MEASURED_CRATES.contains(crate_name))
        .map(|crate_name| Finding {
            detector: "D8",
            metric: crate_name.to_string(),
            detail: format!("no census metric reaches the `{crate_name}` crate under domains/"),
        })
        .collect()
}

/// The names of every directory entry directly under `dir` (crate names, for
/// D8's live cross-check). Files (e.g. `domains/CLAUDE.md`) are excluded.
/// Pure and filesystem-only — kept separate from [`detect_d8`] itself so the
/// frozen roster's *shape* can be tested against a synthetic temp directory
/// without touching the real `domains/` tree on every test run.
#[cfg(test)]
fn crate_names_under(dir: &std::path::Path) -> Vec<String> {
    let mut names: Vec<String> = std::fs::read_dir(dir)
        .expect("read_dir")
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_dir())
        .filter_map(|entry| entry.file_name().into_string().ok())
        .collect();
    names.sort();
    names
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domesday::census::{Column, repo_root};
    use crate::domesday::comparators::{load_comparators, load_expectations};
    use std::collections::BTreeMap;

    fn fires(findings: &[Finding]) -> bool {
        !findings.is_empty()
    }

    /// A minimal one-column `Census` with the given role, mirroring Task
    /// 3's `table` helper (`stats.rs`). `kind` is fixed to `categorical` —
    /// every caller of this helper is exercising D1 or D7, both of which
    /// only need a categorical/flag-shaped column.
    fn table_cat(role: &str, vals: &[&str]) -> Census {
        Census {
            columns: vec![Column {
                name: "m".to_string(),
                kind: "categorical".to_string(),
                doc: String::new(),
                domain: "climate".to_string(),
                role: role.to_string(),
            }],
            rows: vals
                .iter()
                .map(|v| BTreeMap::from([("m".to_string(), (*v).to_string())]))
                .collect(),
        }
    }

    /// A minimal one-column numeric `Census`, for D2/D3/D4.
    fn table_num(role: &str, vals: &[&str]) -> Census {
        Census {
            columns: vec![Column {
                name: "m".to_string(),
                kind: "numeric".to_string(),
                doc: String::new(),
                domain: "climate".to_string(),
                role: role.to_string(),
            }],
            rows: vals
                .iter()
                .map(|v| BTreeMap::from([("m".to_string(), (*v).to_string())]))
                .collect(),
        }
    }

    fn detect_only_d1(c: &Census) -> Vec<Finding> {
        detect_d1(c)
    }

    fn detect_only_d2(c: &Census) -> Vec<Finding> {
        detect_d2(c)
    }

    fn detect_only_d3(c: &Census) -> Vec<Finding> {
        detect_d3(c)
    }

    fn detect_only_d4(c: &Census) -> Vec<Finding> {
        detect_d4(c)
    }

    fn detect_only_d7(c: &Census) -> Vec<Finding> {
        detect_d7(c)
    }

    // --- D1 ---

    #[test]
    fn d1_fires_at_the_threshold_and_not_below_it() {
        let at = table_cat(
            "descriptor",
            &vec!["a"; 80]
                .into_iter()
                .chain(vec!["b"; 20])
                .collect::<Vec<_>>(),
        );
        let below = table_cat(
            "descriptor",
            &vec!["a"; 79]
                .into_iter()
                .chain(vec!["b"; 21])
                .collect::<Vec<_>>(),
        );
        assert!(fires(&detect_only_d1(&at)), "80% must fire");
        assert!(!fires(&detect_only_d1(&below)), "79% must not");
    }

    #[test]
    fn d1_never_fires_on_an_invariant() {
        let inv = table_cat("invariant", &vec!["true"; 100]);
        assert!(
            !fires(&detect_only_d1(&inv)),
            "invariants are not degeneracies"
        );
    }

    #[test]
    fn d1_ignores_numeric_columns() {
        let c = table_num("descriptor", &vec!["1"; 100]);
        assert!(
            !fires(&detect_only_d1(&c)),
            "D1 only looks at categorical/flag columns"
        );
    }

    // --- D2 ---

    #[test]
    fn d2_fires_on_zero_variance_and_not_on_any_variance() {
        let frozen = table_num("descriptor", &vec!["0.42"; 50]);
        let mut varying: Vec<&str> = vec!["0.42"; 49];
        varying.push("0.43");
        let varying = table_num("descriptor", &varying);
        assert!(fires(&detect_only_d2(&frozen)), "min == max must fire");
        assert!(
            !fires(&detect_only_d2(&varying)),
            "even one differing value must not fire"
        );
    }

    #[test]
    fn d2_never_fires_on_an_invariant() {
        let c = table_num("invariant", &vec!["1"; 100]);
        assert!(
            !fires(&detect_only_d2(&c)),
            "a frozen invariant is correct, not a D2 finding"
        );
    }

    // --- D3 ---

    #[test]
    fn d3_fires_just_under_five_percent_and_not_at_it() {
        // 8 sorted values so `percentile`'s ceil(q*8) formula lands on
        // exact indices: p25 = sorted[1], p75 = sorted[5] (see stats.rs's
        // `percentile`). min=0, max=100 -> range=100, so an IQR of exactly
        // 5.0 sits AT the 5% bar (must not fire, since D3 is strict `<`)
        // and an IQR of 4.98 sits just under it (must fire).
        let at: Vec<&str> = vec!["0", "47.5", "50", "50", "50", "52.5", "60", "100"];
        let under: Vec<&str> = vec!["0", "47.51", "50", "50", "50", "52.49", "60", "100"];

        let c_at = table_num("descriptor", &at);
        let s_at = numeric(&c_at, "m").expect("stats");
        assert_eq!(s_at.p75 - s_at.p25, 5.0, "IQR must be exactly 5.0");
        assert_eq!(s_at.max - s_at.min, 100.0, "range must be exactly 100.0");
        assert!(
            !fires(&detect_only_d3(&c_at)),
            "IQR exactly 5% must not fire"
        );

        let c_under = table_num("descriptor", &under);
        assert!(
            fires(&detect_only_d3(&c_under)),
            "IQR just under 5% must fire"
        );
    }

    #[test]
    fn d3_does_not_fire_when_min_equals_max() {
        let c = table_num("descriptor", &["7"; 10]);
        assert!(
            !fires(&detect_only_d3(&c)),
            "a frozen metric is D2's finding, not D3's (max > min guard)"
        );
    }

    // --- D4 ---

    #[test]
    fn d4_fires_at_a_rail_and_not_in_the_middle() {
        // Median 0, min 0, max 10 -> at the min rail.
        let mut at_rail: Vec<String> = vec!["0".to_string(); 6];
        at_rail.push("10".to_string());
        let at_rail_refs: Vec<&str> = at_rail.iter().map(String::as_str).collect();
        let c = table_num("descriptor", &at_rail_refs);
        assert!(
            fires(&detect_only_d4(&c)),
            "median at the min rail must fire"
        );

        // 1..=9: median 5, min 1, max 9 -> squarely in the middle.
        let middle: Vec<String> = (1..=9).map(|i| i.to_string()).collect();
        let middle_refs: Vec<&str> = middle.iter().map(String::as_str).collect();
        let c = table_num("descriptor", &middle_refs);
        assert!(
            !fires(&detect_only_d4(&c)),
            "a centered median must not fire"
        );
    }

    // --- D5 ---

    #[test]
    fn band_of_pins_the_conventional_edges() {
        assert_eq!(band_of(0.95), "dominant");
        assert_eq!(band_of(0.7), "dominant");
        assert_eq!(band_of(0.699), "strong");
        assert_eq!(band_of(0.5), "strong");
        assert_eq!(band_of(0.499), "moderate");
        assert_eq!(band_of(0.3), "moderate");
        assert_eq!(band_of(0.299), "weak");
        assert_eq!(band_of(0.1), "weak");
        assert_eq!(band_of(0.099), "none");
        assert_eq!(band_of(0.0), "none");
    }

    fn table_two_cols(a_vals: &[&str], b_vals: &[&str]) -> Census {
        Census {
            columns: vec![
                Column {
                    name: "a".to_string(),
                    kind: "numeric".to_string(),
                    doc: String::new(),
                    domain: "climate".to_string(),
                    role: "descriptor".to_string(),
                },
                Column {
                    name: "b".to_string(),
                    kind: "numeric".to_string(),
                    doc: String::new(),
                    domain: "climate".to_string(),
                    role: "descriptor".to_string(),
                },
            ],
            rows: a_vals
                .iter()
                .zip(b_vals.iter())
                .map(|(a, b)| {
                    BTreeMap::from([
                        ("a".to_string(), (*a).to_string()),
                        ("b".to_string(), (*b).to_string()),
                    ])
                })
                .collect(),
        }
    }

    #[test]
    fn d5_fires_when_declared_differs_from_observed_and_not_when_it_matches() {
        // Perfectly correlated (r = 1.0 -> "dominant").
        let vals: Vec<String> = (1..=10).map(|i| i.to_string()).collect();
        let refs: Vec<&str> = vals.iter().map(String::as_str).collect();
        let c = table_two_cols(&refs, &refs);

        let matching = Expectation {
            metric: "a".to_string(),
            tracks: "b".to_string(),
            why: "test".to_string(),
            declared: "dominant".to_string(),
        };
        let mismatched = Expectation {
            declared: "weak".to_string(),
            ..matching.clone()
        };

        assert!(
            !fires(&detect_d5(&c, std::slice::from_ref(&matching))),
            "declared dominant, observed dominant -> must not fire"
        );
        assert!(
            fires(&detect_d5(&c, std::slice::from_ref(&mismatched))),
            "declared weak, observed dominant -> must fire"
        );
    }

    #[test]
    fn d5_only_pairs_worlds_present_on_both_sides() {
        // Absences on either side must exclude that world from the pairing,
        // not be treated as 0 or dropped from the whole comparison.
        let c = table_two_cols(&["1", "2", "", "4"], &["1", "", "3", "4"]);
        let e = Expectation {
            metric: "a".to_string(),
            tracks: "b".to_string(),
            why: "test".to_string(),
            declared: "dominant".to_string(),
        };
        // Only rows 0 and 3 have both present: (1,1) and (4,4) -> perfectly
        // correlated, "dominant", matching the declaration.
        assert!(!fires(&detect_d5(&c, std::slice::from_ref(&e))));
    }

    // --- D6 ---

    #[test]
    fn d6_fires_just_over_the_band_and_not_at_it() {
        let c = table_num("descriptor", &["20"; 10]); // median 20
        let cmp_at = Comparator {
            name: "Test".to_string(),
            kind: "real".to_string(),
            values: BTreeMap::from([("m".to_string(), 10.0)]),
            band: BTreeMap::from([("m".to_string(), 10.0)]), // gap exactly 10
        };
        let mut cmp_over = cmp_at.clone();
        cmp_over.band = BTreeMap::from([("m".to_string(), 9.999)]);

        assert!(
            !fires(&detect_d6(&c, std::slice::from_ref(&cmp_at))),
            "gap == band must not fire (strict >)"
        );
        assert!(
            fires(&detect_d6(&c, std::slice::from_ref(&cmp_over))),
            "gap > band must fire"
        );
    }

    // --- D7 ---

    #[test]
    fn d7_fires_on_an_invariant_that_varies() {
        let broken = table_cat(
            "invariant",
            &vec!["true"; 99]
                .into_iter()
                .chain(vec!["false"; 1])
                .collect::<Vec<_>>(),
        );
        let f = detect_only_d7(&broken);
        assert!(
            fires(&f),
            "a broken invariant is worth more than a degeneracy"
        );
    }

    #[test]
    fn d7_does_not_fire_on_a_true_invariant() {
        let held = table_cat("invariant", &vec!["true"; 100]);
        assert!(
            !fires(&detect_only_d7(&held)),
            "an invariant that holds is not a finding"
        );
    }

    #[test]
    fn d7_ignores_descriptors_no_matter_how_skewed() {
        let skewed = table_cat(
            "descriptor",
            &vec!["a"; 99]
                .into_iter()
                .chain(vec!["b"; 1])
                .collect::<Vec<_>>(),
        );
        assert!(
            !fires(&detect_only_d7(&skewed)),
            "D7 only looks at role == invariant"
        );
    }

    // --- D8 ---

    #[test]
    fn crate_names_under_lists_directories_and_excludes_files() {
        let dir =
            std::env::temp_dir().join(format!("hv-domesday-d8-synthetic-{}", std::process::id()));
        std::fs::create_dir_all(dir.join("measured-crate")).expect("mkdir");
        std::fs::create_dir_all(dir.join("unmeasured-crate")).expect("mkdir");
        std::fs::write(dir.join("NOTES.md"), "not a crate").expect("write");

        let names = crate_names_under(&dir);
        assert_eq!(
            names,
            vec!["measured-crate".to_string(), "unmeasured-crate".to_string()],
            "directories only, sorted, files excluded"
        );

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn d8_fires_on_an_unmeasured_crate_and_not_on_a_measured_one() {
        let found = ["astronomy".to_string(), "unmeasured-crate".to_string()];
        let measured: Vec<&str> = MEASURED_CRATES.to_vec();
        let findings: Vec<Finding> = found
            .iter()
            .filter(|n| !measured.contains(&n.as_str()))
            .map(|n| Finding {
                detector: "D8",
                metric: n.clone(),
                detail: String::new(),
            })
            .collect();
        assert!(
            findings.iter().any(|f| f.metric == "unmeasured-crate"),
            "an unmeasured crate must fire"
        );
        assert!(
            !findings.iter().any(|f| f.metric == "astronomy"),
            "a measured crate must not fire"
        );
    }

    #[test]
    fn domain_crates_are_still_exactly_this_list() {
        // Live assertion: the frozen DOMAIN_CRATES roster must match the
        // real domains/ tree, so a new crate silently added there cannot
        // escape D8's coverage check by omission.
        let live = crate_names_under(&repo_root().join("domains"));
        let mut frozen: Vec<String> = DOMAIN_CRATES.iter().map(|s| s.to_string()).collect();
        frozen.sort();
        assert_eq!(
            live, frozen,
            "domains/ has drifted from the frozen DOMAIN_CRATES roster"
        );
    }

    #[test]
    fn d8_fires_on_exactly_alchemy_and_paleoclimate() {
        let f = detect_d8();
        let names: Vec<&str> = f.iter().map(|x| x.metric.as_str()).collect();
        assert_eq!(
            names,
            vec!["alchemy", "paleoclimate"],
            "D8 must fire on exactly the two crates no metric reaches"
        );
    }

    // --- Live census: S2/S2b/S2c, the campaign's acceptance test ---

    fn census() -> Census {
        crate::domesday::census::load(&repo_root().join("book/src/laboratory/generated/the-census"))
            .expect("the committed census loads")
    }

    fn comparators() -> Vec<Comparator> {
        load_comparators(&repo_root().join("studies/comparators.json")).expect("comparators load")
    }

    fn expectations() -> Vec<Expectation> {
        load_expectations(&repo_root().join("studies/expectations.json"))
            .expect("expectations load")
    }

    #[test]
    fn the_live_census_reproduces_the_preregistered_findings() {
        let c = census();
        let cmps = comparators();
        let exps = expectations();
        let f = detect(&c, &cmps, &exps);
        let hit = |d: &str, m: &str| f.iter().any(|x| x.detector == d && x.metric == m);

        // S2 -- SKY-19's climate defect, by the routes the spec names.
        assert!(
            hit("D6", "mean-land-temperature-c"),
            "median -11.90 vs Earth 14.0"
        );
        assert!(
            hit("D5", "mean-land-temperature-c"),
            "r = -0.245 vs year-std-days, under the 0.50 dominant-driver floor"
        );
        assert!(
            !hit("D1", "dominant-land-biome"),
            "65.1% is a skew, NOT a degeneracy -- must not fire"
        );

        // S2b -- found while testing, not sought.
        assert!(
            hit("D2", "reproductive-tempo-goblin"),
            "min=median=max=0.42"
        );

        // S2c was FALSIFIED: the preregistered ceiling of 10 was a guess made
        // without measuring the post-split distribution. D1 fires 27 times and
        // the hits are overwhelmingly real -- ten metrics never vary at all
        // across 1,000 worlds. The ceiling is retired, NOT raised to a
        // flattering number; the threshold is untouched. This assertion now
        // pins the measured count so a change to D1 or to `role` is still
        // caught. If it moves, find out why before updating it.
        let d1 = f.iter().filter(|x| x.detector == "D1").count();
        assert_eq!(
            d1, 27,
            "D1 hit count changed; investigate before re-pinning"
        );
    }

    #[test]
    fn findings_are_sorted_by_detector_then_metric() {
        let c = census();
        let cmps = comparators();
        let exps = expectations();
        let f = detect(&c, &cmps, &exps);
        let mut sorted = f.clone();
        sorted.sort_by(|a, b| {
            a.detector
                .cmp(b.detector)
                .then_with(|| a.metric.cmp(&b.metric))
        });
        assert_eq!(
            f, sorted,
            "detect() must already return findings in sorted order"
        );
    }
}
