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
//! D1 alone would fire on 39 of 58 categorical/flag metric columns (46
//! `descriptor` + 12 `invariant`; corrected from the spec's original "40 of
//! 57" — the 57 undercounted the real total by one, and the 40 double-counted
//! the structural `pin_set` column, which is not a metric but is
//! categorical-shaped and 100% single-valued). The role split does real
//! work — down to 27 — but S2c's preregistered ceiling of 10 was a guess
//! made without measuring the post-split distribution and is FALSIFIED: it
//! is retired, not raised to a flattering number. See
//! `the_live_census_reproduces_the_preregistered_findings`'s S2c comment.

use crate::domesday::census::Census;
use crate::domesday::comparators::{Comparator, Expectation};
use crate::domesday::stats::{categorical, numeric};
use std::collections::BTreeSet;

/// One finding a detector raised against a metric.
///
/// `detector` names the detector that raised it. That name is an open set,
/// not the closed D1-D8 roster: a detector may report under several names to
/// distinguish outcomes it must not conflate, and D5 does exactly that with
/// `"D5 strength"`, `"D5 direction"` and `"D5 unmeasurable"`. A reader must
/// treat the field as an opaque label and must never *filter* on an
/// enumeration of it — [`DECLARED_DETECTORS`] exists only so a renderer can
/// show a zero for a detector that fired nothing, and is used as a union
/// with the observed names, never as a whitelist. `metric` is the census column
/// (or, for D8, the `domains/` crate) it concerns; `detail` is a
/// human-readable explanation carrying the actual numbers involved — every
/// number in it is read from the committed census at render time, never
/// invented (spec §4.6).
/// type-audit: bare-ok(identifier-text: detector), bare-ok(identifier-text: metric), bare-ok(prose: detail)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    /// Which detector raised this finding, e.g. `"D1"` or `"D5 unmeasurable"`.
    pub detector: &'static str,
    /// The census metric (or, for D8, the `domains/` crate name) involved.
    pub metric: String,
    /// A human-readable explanation, carrying the numbers that triggered it.
    pub detail: String,
}

/// Every detector name the functions in this module can emit — ten, because
/// D5 reports under three.
///
/// It exists for exactly one purpose: so a renderer can publish a count of
/// `0` for a detector that ran and found nothing, which decision 0119
/// requires (an absent row would make "does not exist" and "found nothing"
/// share one channel). It is **not** a closed set and must never be used to
/// filter findings — a renderer takes `declared ∪ observed`, so a detector
/// whose name is missing here still shows its true count, and a name here
/// that nothing emits renders as a harmless zero row. `guard::
/// every_emitted_detector_name_is_declared` scans this file for the names
/// actually constructed and fails if one is missing from this list.
/// type-audit: bare-ok(identifier-text)
pub const DECLARED_DETECTORS: &[&str] = &[
    "D1",
    "D2",
    "D3",
    "D4",
    "D5 direction",
    "D5 strength",
    "D5 unmeasurable",
    "D6",
    "D7",
    "D8",
];

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
/// its max (e.g. "median waterfall count is 0"). **D2 ⊆ D4 exactly**: a
/// `min == max` metric trivially has `median == min == max` too, so every D2
/// hit also fires D4 (on the live census, D2's 31 hits and D4's 39 hits
/// cover only 39 distinct metrics between them, not 70 — D2's 31 are a
/// subset of D4's 39). D3 overlaps D4 on a further 6 metrics. This overlap
/// is expected, not a bug — D2/D3/D4 answer different questions ("is it
/// frozen", "is the middle 50% narrow", "is the typical value extreme") —
/// but a renderer must not present one metric as three independent
/// weaknesses.
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

/// The single value `vals` holds if every entry is bit-identical, or `None`
/// if it varies (or is empty). Exact equality is correct and sufficient
/// here, not a tuned epsilon: census floats are quantized to 8 significant
/// digits at emit (`hornvale_kernel::quantize`), so a genuinely constant
/// metric yields bit-identical `f64`s across every world.
fn constant_value(vals: &[f64]) -> Option<f64> {
    let first = *vals.first()?;
    if vals.iter().all(|v| *v == first) {
        Some(first)
    } else {
        None
    }
}

/// Pearson correlation coefficient over paired samples, or `None` when
/// either side is constant (an undefined correlation), either side has zero
/// variance, or there are fewer than two pairs.
fn pearson(xs: &[f64], ys: &[f64]) -> Option<f64> {
    let n = xs.len();
    if n < 2 || n != ys.len() {
        return None;
    }
    // A constant column has no correlation to report. Test this EXACTLY
    // rather than via the variance: `sum()` accumulates left to right, so a
    // thousand copies of one value leave ~1e-22 of rounding residue in the
    // variance -- strictly greater than zero, which defeated the guard below
    // and let this function return a correlation computed from noise.
    if constant_value(xs).is_some() || constant_value(ys).is_some() {
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
    // Kept as a backstop, not a replacement: it is not wrong, only
    // insufficient on its own (see the exact check above).
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

/// D5 Mis-declared relationship: an expectation declares a relationship
/// class and sign; fire `"D5 strength"` when the observed Pearson `|r|`,
/// mapped to a band, differs from the declared class, `"D5 direction"` when
/// the band matches but the observed sign is backwards from the declared one
/// (spec §4.2) — a link that runs the wrong way is a more serious defect
/// than a merely over- or under-claimed strength — or `"D5 unmeasurable"`
/// when either column is constant across the whole census, naming which
/// side is frozen: a declared link whose inputs cannot support a
/// correlation is a statement that the census cannot test the claim, not a
/// measurement and not silence (Task 4b, spec §1). The sign is never
/// reported when the observed band is `none`: a near-zero `r`'s sign is
/// noise. Whenever the band is anything else, the strength branch names the
/// observed sign too — a link can be both mis-strengthed and backwards, and
/// a bare `|r|` would hide the second defect (Task 4c) — calling it out
/// explicitly when it contradicts the declared direction, since that row is
/// a backwards link that also happens to have the wrong strength. Pairs are
/// formed from worlds where BOTH `metric` and `tracks` are present, in row
/// order (deterministic — no `HashMap`).
///
/// A pairing with fewer than two rows is left silent, not reported as
/// `"D5 unmeasurable"`: that is a different failure (a too-small census),
/// and conflating it with a frozen column through the same message would
/// hide which one actually happened. It does not occur on the live census —
/// every declared pairing has hundreds of paired worlds — so this is an
/// unexercised edge case decided on its merits, not a tuned outcome.
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
        if xs.len() >= 2 {
            let mut frozen_sides = Vec::new();
            if let Some(v) = constant_value(&xs) {
                frozen_sides.push(format!("`{}` is frozen at {v}", e.metric));
            }
            if let Some(v) = constant_value(&ys) {
                frozen_sides.push(format!("`{}` is frozen at {v}", e.tracks));
            }
            if !frozen_sides.is_empty() {
                out.push(Finding {
                    detector: "D5 unmeasurable",
                    metric: e.metric.clone(),
                    detail: format!(
                        "declared {} tracking {}, but {} across {} paired worlds — the census \
                         cannot test this link",
                        e.declared,
                        e.tracks,
                        frozen_sides.join(" and "),
                        xs.len()
                    ),
                });
                continue;
            }
        }
        if let Some(r) = pearson(&xs, &ys) {
            let observed = band_of(r.abs());
            let observed_dir = if r < 0.0 { "negative" } else { "positive" };
            if observed != e.declared {
                // Strength mismatch. The sign is reported whenever it was
                // measured, and suppressed exactly when it was not: a
                // near-zero r's sign is noise (spec §4.2), so `none` still
                // gets a bare |r|. Anything else names the observed sign --
                // and, when that sign contradicts the declared direction,
                // says so plainly rather than leaving the reader to notice a
                // minus sign buried in a signed r: this row is a backwards
                // link that also happens to have the wrong strength.
                let sign_note = if observed == "none" {
                    String::new()
                } else if e.direction != "none" && observed_dir != e.direction {
                    format!(
                        " — and the sign is backwards: the coupling is {}, not the declared {}",
                        observed_dir, e.direction
                    )
                } else {
                    format!(" ({observed_dir})")
                };
                let r_display = if observed == "none" {
                    format!("|r| = {:.3}", r.abs())
                } else {
                    format!("r = {r:+.3}")
                };
                out.push(Finding {
                    detector: "D5 strength",
                    metric: e.metric.clone(),
                    detail: format!(
                        "declared {} tracking {}, but observed {} ({} pairs) is {}{}",
                        e.declared,
                        e.tracks,
                        r_display,
                        xs.len(),
                        observed,
                        sign_note
                    ),
                });
            } else if observed != "none" && observed_dir != e.direction {
                // Right strength, WRONG SIGN: the link exists and runs
                // backwards. More serious than an over-claim.
                out.push(Finding {
                    detector: "D5 direction",
                    metric: e.metric.clone(),
                    detail: format!(
                        "declared {} {} tracking {}, but the observed coupling is {} \
                         (r = {:+.3}, {} pairs) — the link runs backwards",
                        e.direction,
                        e.declared,
                        e.tracks,
                        observed_dir,
                        r,
                        xs.len()
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
    // The Particular's domain. Deliberately absent from `MEASURED_CRATES`: no
    // census metric reads it yet, so D8 reports it as an unmeasured domain —
    // a real gap in the world, rendered rather than papered over.
    "person",
    "religion",
    "settlement",
    "species",
    "terrain",
    "topology",
];

/// Crates under `domains/` that at least one census metric MEASURES: the
/// metric's output is computed from a value or type the crate itself
/// produces. This is narrower than mere call-graph reachability —
/// `windows/worldgen` calls into BOTH `paleoclimate`
/// (`glaciated`/`extract`/`genesis`, inside the terrain build) and `alchemy`
/// while constructing every world, so by a reachability rule every terrain
/// metric would count as "reaching" paleoclimate. No metric's `extract`
/// function reads a value either crate produces (verified by a zero-hit grep
/// for `glaci|fossil|refugium|paleo|EraClimate` and
/// `alchem|substrate|reagent|transmut` across `windows/lab/src/`), so both
/// are absent from this list despite being reachable.
///
/// Most entries here are direct (a `hornvale_<crate>::` reference somewhere
/// in `windows/lab/src/`, cross-checked live by
/// `direct_references_match_measured_crates`). `topology` is the one
/// exception, allowlisted as `TRANSITIVELY_MEASURED`: it is never imported
/// directly, but `defensibility-capacity-rank-corr` (`metrics.rs:4519`)
/// computes over the `ConnectionGraph` `hornvale_worldgen::
/// connection_graph_of` returns — a topology-produced value the metric's
/// output is actually derived from, which is what distinguishes it from
/// paleoclimate/alchemy's mere reachability.
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

/// The subset of `MEASURED_CRATES` that clears the bar only *transitively*
/// (no `hornvale_<crate>::` reference exists anywhere in
/// `windows/lab/src/`) — see `MEASURED_CRATES`'s doc for `topology`'s
/// justification. `direct_references_match_measured_crates` checks that
/// every OTHER entry in `MEASURED_CRATES` is a genuine direct reference, so
/// this allowlist can only grow by a deliberate, reviewed edit here.
#[cfg(test)]
const TRANSITIVELY_MEASURED: &[&str] = &["topology"];

/// D8 Unmeasured domain: a `domains/` crate no census metric measures — a
/// gap in the WORLD (spec §4.6a), rendered rather than fixed. Expected to
/// fire on exactly `alchemy` and `paleoclimate` (spec §4.4a).
fn detect_d8() -> Vec<Finding> {
    detect_d8_over(DOMAIN_CRATES, MEASURED_CRATES)
}

/// D8's actual logic, parameterised over the two rosters so a test can
/// exercise the real filter against a synthetic pair instead of
/// re-implementing it and asserting on its own output.
fn detect_d8_over(all_crates: &[&str], measured_crates: &[&str]) -> Vec<Finding> {
    all_crates
        .iter()
        .filter(|crate_name| !measured_crates.contains(crate_name))
        .map(|crate_name| Finding {
            detector: "D8",
            metric: crate_name.to_string(),
            detail: format!(
                "no census metric measures any quantity the `{crate_name}` crate produces"
            ),
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

/// Every `.rs` file under `dir`, recursively (for the `MEASURED_CRATES`
/// live cross-check — `windows/lab/src/` nests `domesday/` and others).
#[cfg(test)]
fn rs_files_under(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("read_dir") {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            rs_files_under(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
            out.push(path);
        }
    }
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
    fn pearson_reads_a_nontrivial_correlation_and_its_sign() {
        // Every other pearson-exercising test here uses perfectly
        // correlated pairs (r = 1.0), and D5 takes `.abs()` of the result,
        // so a sign error is structurally invisible to them (fix round 1/5,
        // Finding 4). This pins a non-trivial magnitude and confirms the
        // sign survives.
        let xs = [1.0, 2.0, 3.0, 4.0, 5.0];
        let ys = [2.0, 4.0, 5.0, 4.0, 5.0];
        let r = pearson(&xs, &ys).expect("a defined correlation");
        assert!((r - 0.7745966692414834).abs() < 1e-12, "r was {r}");

        let neg_ys: Vec<f64> = ys.iter().map(|y| -y).collect();
        let r_neg = pearson(&xs, &neg_ys).expect("a defined correlation");
        assert!(
            (r_neg + 0.7745966692414834).abs() < 1e-12,
            "negating one side must flip the sign; r_neg was {r_neg}"
        );
    }

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
            direction: "positive".to_string(),
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
            direction: "positive".to_string(),
        };
        // Only rows 0 and 3 have both present: (1,1) and (4,4) -> perfectly
        // correlated, "dominant", matching the declaration.
        assert!(!fires(&detect_d5(&c, std::slice::from_ref(&e))));
    }

    /// A two-column synthetic census, both numeric descriptors.
    fn two_numeric_columns(a: &str, av: &[f64], b: &str, bv: &[f64]) -> Census {
        Census {
            columns: vec![
                Column {
                    name: a.to_string(),
                    kind: "numeric".to_string(),
                    doc: String::new(),
                    domain: "climate".to_string(),
                    role: "descriptor".to_string(),
                },
                Column {
                    name: b.to_string(),
                    kind: "numeric".to_string(),
                    doc: String::new(),
                    domain: "climate".to_string(),
                    role: "descriptor".to_string(),
                },
            ],
            rows: av
                .iter()
                .zip(bv.iter())
                .map(|(x, y)| {
                    BTreeMap::from([
                        (a.to_string(), x.to_string()),
                        (b.to_string(), y.to_string()),
                    ])
                })
                .collect(),
        }
    }

    /// An expectation with the given declared class and direction.
    fn expectation(metric: &str, tracks: &str, declared: &str, direction: &str) -> Expectation {
        Expectation {
            metric: metric.to_string(),
            tracks: tracks.to_string(),
            why: String::new(),
            declared: declared.to_string(),
            direction: direction.to_string(),
        }
    }

    #[test]
    fn d5_fires_direction_when_the_sign_is_backwards() {
        // Perfectly anti-correlated, declared positive at the same strength.
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "dominant", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1, "a backwards coupling must fire");
        assert_eq!(f[0].detector, "D5 direction");
        assert!(
            f[0].detail.contains("negative"),
            "must name what was observed: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_is_silent_when_strength_and_sign_both_match() {
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "dominant", "negative");
        assert!(
            detect_d5(&c, &[e]).is_empty(),
            "a correct claim must be silent"
        );
    }

    #[test]
    fn d5_reports_strength_not_sign_when_the_observed_band_is_none() {
        // Zero correlation (verified exactly, not merely small): the sign
        // is noise and must not be reported. The brief's original fixture
        // here, `&[1.0, 1.0, 1.0, 1.0001]` against `&[1.0, 2.0, 3.0, 4.0]`,
        // does not actually land in the "none" band -- the lone perturbed
        // point coincides with the largest x, which pearson reads as a
        // strong pattern (r = 0.7745966692414834, "dominant" -- the same
        // magnitude `pearson_reads_a_nontrivial_correlation_and_its_sign`
        // pins above as "nontrivial"), not noise. This fixture is chosen so
        // cov = 0 exactly: dx = [-1.5, -0.5, 0.5, 1.5], dy = [-0.5, 1.5,
        // -1.5, 0.5], dx . dy = 0.75 - 0.75 - 0.75 + 0.75 = 0.
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[2.0, 4.0, 1.0, 3.0]);
        let e = expectation("m", "d", "dominant", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(
            f[0].detector, "D5 strength",
            "a none-band result is a strength finding"
        );
        assert!(
            !f[0].detail.contains("direction"),
            "the sign of a near-zero r is noise and must not be reported: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_strength_omits_the_sign_when_the_observed_band_is_none() {
        // Reuses `d5_reports_strength_not_sign_when_the_observed_band_is_none`'s
        // cov = 0 fixture (the sign of a near-zero r is noise), but checks a
        // different invariant: Task 4c teaches the strength branch to name
        // "positive"/"negative" when the band is anything else, so this
        // guards that it must still say neither word when the band is
        // `none` -- a regression this task's own change could introduce,
        // which the older test (checking only for the word "direction")
        // does not cover.
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[2.0, 4.0, 1.0, 3.0]);
        let e = expectation("m", "d", "dominant", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(f[0].detector, "D5 strength");
        assert!(
            !f[0].detail.contains("positive") && !f[0].detail.contains("negative"),
            "a none-band result must carry no sign claim at all: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_strength_names_the_observed_sign_when_it_contradicts_the_declaration() {
        // Mirrors row #10 (shelf-fraction tracking ocean-fraction): declared
        // moderate positive, but the data is perfectly anti-correlated, so
        // the observed band is dominant negative -- both the strength AND
        // the sign are wrong. Before Task 4c the strength branch reported
        // only `|r|` and never named the sign, so a dominant-strength link
        // running backwards published as a bare strength mismatch.
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "moderate", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(
            f[0].detector, "D5 strength",
            "the detector taxonomy must not move: this is still a strength finding"
        );
        assert!(
            f[0].detail.contains("negative"),
            "must name the observed sign: {}",
            f[0].detail
        );
        assert!(
            f[0].detail.contains("backwards"),
            "must make the contradiction legible as prose, not just embed a minus \
             sign in the number: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_fires_strength_when_the_band_differs() {
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "weak", "negative");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(f[0].detector, "D5 strength");
    }

    #[test]
    fn pearson_returns_none_for_a_constant_column() {
        // 1000 copies of a value with a long mantissa, so naive left-to-right
        // summation leaves realistic rounding residue in the mean/variance
        // (the brief's ~1e-13/~3.4e-22 figures for exactly this shape). The
        // OLD `var_x <= 0.0` guard does not catch this -- that is the defect
        // Task 4b exists to fix, and this test is the proof: it must FAIL
        // before Step 3's exact-equality guard lands.
        let xs = vec![29.835811; 1000];
        let ys: Vec<f64> = (0..1000).map(|i| i as f64).collect();
        assert!(
            pearson(&xs, &ys).is_none(),
            "a constant column has no correlation to report, but pearson returned Some(_) \
             computed from summation noise"
        );
    }

    #[test]
    fn d5_reports_unmeasurable_when_the_metric_is_frozen() {
        let frozen = vec![29.835811; 1000];
        let varying: Vec<f64> = (0..1000).map(|i| i as f64).collect();
        let c = two_numeric_columns("m", &frozen, "d", &varying);
        let e = expectation("m", "d", "moderate", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(
            f.len(),
            1,
            "a frozen metric must report exactly one finding"
        );
        assert_eq!(f[0].detector, "D5 unmeasurable");
        assert!(
            f[0].detail.contains('m') && f[0].detail.contains("29.835811"),
            "the detail must name the frozen column and its single value: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_reports_unmeasurable_when_the_tracked_column_is_frozen() {
        let varying: Vec<f64> = (0..1000).map(|i| i as f64).collect();
        let frozen = vec![29.835811; 1000];
        let c = two_numeric_columns("m", &varying, "d", &frozen);
        let e = expectation("m", "d", "moderate", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(
            f.len(),
            1,
            "a frozen tracked column must report exactly one finding"
        );
        assert_eq!(f[0].detector, "D5 unmeasurable");
        assert!(
            f[0].detail.contains('d') && f[0].detail.contains("29.835811"),
            "the detail must name the frozen column and its single value: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_does_not_report_unmeasurable_when_both_columns_vary() {
        // Finding 2's replacement for a verbatim duplicate of
        // `d5_fires_strength_when_the_band_differs` (same fixture, same
        // assertions -- it could never fail independently of that test). The
        // guard this test actually names: a fixture sitting right at the
        // boundary of `constant_value`'s exact-equality check -- 999
        // identical values plus one differing by only 1e-9 -- must still be
        // treated as varying (report "D5 strength") and not swallowed by
        // "D5 unmeasurable" as if the column were frozen.
        let mut xs = vec![1.0; 999];
        xs.push(1.0 + 1e-9);
        let ys: Vec<f64> = (0..999).map(|i| i as f64).chain([999.0]).collect();
        let c = two_numeric_columns("m", &xs, "d", &ys);
        let e = expectation("m", "d", "weak", "negative");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(
            f[0].detector, "D5 strength",
            "a column with one value 1e-9 off its other 999 is varying, not frozen: {:?}",
            f
        );
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
        // Calls the real filter (`detect_d8_over`) against a synthetic
        // roster pair, rather than re-implementing the filter in the test
        // body — a test that re-implements what it tests stays green even
        // if `detect_d8_over`'s body is deleted.
        let all = ["astronomy", "unmeasured-crate"];
        let measured = ["astronomy"];
        let f = detect_d8_over(&all, &measured);
        assert!(
            f.iter().any(|x| x.metric == "unmeasured-crate"),
            "an unmeasured crate must fire"
        );
        assert!(
            !f.iter().any(|x| x.metric == "astronomy"),
            "a measured crate must not fire"
        );
    }

    // claim: structural(seed: none — false-positive seed-loop flag; the
    // `.map(|s| ...)` closure below binds a `&str` crate name from
    // DOMAIN_CRATES, not a seed) — the frozen roster vs. the live tree,
    // Task 6's workspace-wide run is what surfaced this tag was missing.
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
    fn direct_references_match_measured_crates() {
        // Live cross-check for MEASURED_CRATES' unguarded half (fix round
        // 1/5, Finding 2): grep windows/lab/src/*.rs for `hornvale_<crate>::`
        // and assert the direct-reference set equals MEASURED_CRATES minus
        // the explicit transitive allowlist. If `alchemy` gains a metric
        // tomorrow this goes red on the "missing" side; if `topology` (the
        // one transitive entry) loses its indirection, THIS test cannot see
        // that regression, and neither can `d8_fires_on_exactly_alchemy_and_
        // paleoclimate` or `the_live_census_reproduces_the_preregistered_
        // findings`: `detect_d8()` only filters DOMAIN_CRATES against the
        // MEASURED_CRATES const, so deleting `defensibility-capacity-rank-
        // corr` (topology's only derived metric) leaves D8 emitting exactly
        // `[alchemy, paleoclimate]` with the suite green while this const
        // still claims topology is measured. `defensibility_capacity_rank_
        // corr_still_exists_in_the_registry`, below, is the guard that
        // actually ties topology's entry here to the metric that makes it
        // true.
        let mut files = Vec::new();
        rs_files_under(&repo_root().join("windows/lab/src"), &mut files);
        let sources: Vec<String> = files
            .iter()
            .map(|p| std::fs::read_to_string(p).expect("read source file"))
            .collect();

        let mut found_direct: Vec<&str> = DOMAIN_CRATES
            .iter()
            .filter(|crate_name| {
                let needle = format!("hornvale_{crate_name}::");
                sources.iter().any(|src| src.contains(&needle))
            })
            .copied()
            .collect();
        found_direct.sort();

        let mut expected: Vec<&str> = MEASURED_CRATES
            .iter()
            .filter(|c| !TRANSITIVELY_MEASURED.contains(c))
            .copied()
            .collect();
        expected.sort();

        assert_eq!(
            found_direct, expected,
            "the direct-reference crates in windows/lab/src/ no longer match \
             MEASURED_CRATES minus TRANSITIVELY_MEASURED -- update whichever \
             one drifted"
        );
    }

    #[test]
    fn d8_fires_on_exactly_alchemy_paleoclimate_and_person() {
        let f = detect_d8();
        let names: Vec<&str> = f.iter().map(|x| x.metric.as_str()).collect();
        assert_eq!(
            names,
            vec!["alchemy", "paleoclimate", "person"],
            "D8 must fire on exactly the crates no metric reaches. `person` \
             joined the roster with The Particular and no census metric reads \
             it yet — a real gap in the world, which D8 exists to render \
             rather than to hide."
        );
    }

    #[test]
    fn defensibility_capacity_rank_corr_still_exists_in_the_registry() {
        // `detect_d8()` never reads the census -- it only filters
        // DOMAIN_CRATES against the frozen MEASURED_CRATES const, so
        // `topology`'s entry there is a claim, not a derivation. This is the
        // guard that ties the claim to the fact that makes it true: if
        // `defensibility-capacity-rank-corr` (topology's only derived
        // metric, see MEASURED_CRATES' doc comment) is ever deleted from the
        // registry, this test goes red even though D8 itself would keep
        // emitting `[alchemy, paleoclimate]` unchanged.
        let names: Vec<&str> = crate::metrics::registry().iter().map(|m| m.name).collect();
        assert!(
            names.contains(&"defensibility-capacity-rank-corr"),
            "topology's only derived metric was removed from the registry -- \
             MEASURED_CRATES still lists `topology` as measured, but nothing \
             derives from it any more; update MEASURED_CRATES and \
             TRANSITIVELY_MEASURED to match"
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
            "median -11.988568 vs Earth's 14, a 25.988568 gap exceeding the comparator band"
        );
        assert!(
            // D5 split into "D5 strength"/"D5 direction" names (Task 2); this
            // hit is a strength mismatch (band moved, not sign), so it now
            // lives under "D5 strength".
            hit("D5 strength", "mean-land-temperature-c"),
            "declared dominant tracking year-std-days, but observed |r| = 0.245 is weak"
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

        // The Armature's measurement (Task 4), corrected by Task 4b: all
        // thirty frozen expectations run against the live census. Task 4's
        // first pass reported 25 "D5 strength" / 0 "D5 direction" / 5
        // silent, but six of those 25 "strength" hits (the six Biology rows,
        // #16-21 in studies/expectations.json -- both goblin/kobold
        // basal-metabolic-rate, pace-of-life-goblin, both reproductive-
        // tempo, and lifespan-years-goblin) were computed from a Pearson
        // correlation over a CONSTANT column: `pearson`'s zero-variance
        // guard tested `var <= 0.0`, but left-to-right summation over 1000
        // copies of one value leaves ~1e-22 of rounding residue, so the
        // guard never fired and the "strength" it reported was noise, not a
        // measurement. Task 4b added an exact-equality guard and a third
        // detector, "D5 unmeasurable", for exactly this case, so the
        // corrected split is:
        //   19 D5 strength    -- a real measurement over two varying
        //                        columns whose band disagrees with the
        //                        declared one (25 - 6 reclassified = 19).
        //    6 D5 unmeasurable -- the six Biology rows above; the census
        //                        cannot test these links at all.
        //    0 D5 direction    -- unchanged; of the 30 rows, only 3 were
        //                        even ELIGIBLE for a direction check
        //                        (observed band == declared AND observed !=
        //                        "none": karst-fraction/mean-land-
        //                        temperature-c, pop-weighted-abs-latitude/
        //                        mean-land-temperature-c, and climate-
        //                        displacement-events/habitable-fraction --
        //                        rows #15, #25, #28), and all 3 also had
        //                        the declared sign, so "zero backwards
        //                        links" means zero out of 3, not zero out
        //                        of 30.
        //    5 silent          -- unchanged: the 3 eligible-and-matching
        //                        rows above, plus the 2 rows declared
        //                        none/none whose observed band was also
        //                        "none" (mean-land-temperature-c/day-
        //                        length-hours, total-population/plate-
        //                        size-gini; rows #4, #29).
        //
        // RE-MEASURED at the merge against a DIFFERENT census. The Signet
        // shipped an epoch while the campaign ran and replaced the
        // committed census -- 78 of the 196 shared columns moved -- so the
        // frame was run again over the new file with
        // studies/expectations.json byte-unchanged. All four counts came
        // back identical, as did the three direction-eligible rows, the
        // three backwards rows and the six frozen Biology values; nine
        // correlations moved, all in the settlement/population/tribute
        // families the epoch touched, and none crossed a band edge in a
        // way the tally can see. Two rows now sit within 0.011 of one:
        // #28 climate-displacement-events/habitable-fraction at r = -0.310
        // against the 0.300 edge, and #15 karst-fraction/mean-land-
        // temperature-c at +0.111 against 0.100. Either crossing drops
        // that row out of the direction-eligible set and takes the "zero
        // out of 3" denominator to 2 -- so if `d5_direction` is still 0
        // but this file's story stops matching, check the denominator
        // before trusting the null.
        //
        // These counts are a measurement to investigate, not a target: if
        // any moves, find out why before updating it.
        let d5_strength = f.iter().filter(|x| x.detector == "D5 strength").count();
        assert_eq!(
            d5_strength, 19,
            "D5 strength hit count changed; investigate before re-pinning"
        );
        let d5_unmeasurable = f.iter().filter(|x| x.detector == "D5 unmeasurable").count();
        assert_eq!(
            d5_unmeasurable, 6,
            "D5 unmeasurable hit count changed; investigate before re-pinning"
        );
        let d5_direction = f.iter().filter(|x| x.detector == "D5 direction").count();
        assert_eq!(
            d5_direction, 0,
            "D5 direction hit count changed; investigate before re-pinning"
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
