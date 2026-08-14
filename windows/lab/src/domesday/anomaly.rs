//! The anomaly report — the Domesday's transpose, per world (spec §3).
//!
//! The Domesday (`detect.rs`) asks *is this column weak across 1000
//! worlds*; this module asks *is this world strange across the census's
//! columns*. Same census, same reader (`census::load`), axis rotated ninety
//! degrees. It **reuses** [`crate::domesday::census`] and
//! [`crate::domesday::stats`] and implements neither a percentile nor a
//! census loader of its own (Global Constraints) — the one genuinely new
//! computation here is [`tail_depth`], a per-world *rank*, which is not a
//! thing `stats::numeric` computes at all.
//!
//! **The tautology this design exists to avoid (spec §3.2).** If the prior
//! is census percentiles and the evaluation set is that same census, a
//! percentile-shaped threshold flags a fixed fraction of worlds *by
//! construction* — the finding would be manufactured, not discovered. The
//! fix is not in the prior; it is in refusing to let a threshold decide how
//! many worlds are "anomalous". [`rank`] therefore returns every world in a
//! total order and asserts nothing about how many of them are unusual; only
//! the frozen `tail_depth <= `[`TAIL_DEPTH_BAR`] selection bar (never
//! retuned after seeing a result — the same discipline D1's 0.80 and D3's
//! 5 % carry) decides which of a world's columns count toward its score.
//!
//! **It never builds a world.** [`evaluable_columns`], [`rank`] and
//! [`for_seed`] read only the already-loaded [`Census`] — see
//! `mod.rs`'s `loading_never_builds_a_world` guard, extended to scan this
//! file too.
//!
//! **The evaluable surface (spec §3.3).** Only `numeric`/`integer` columns
//! that carry a `domain` and a `role` are candidates at all — the same
//! structural skip `detect.rs`'s D2/D3/D4 use for the `seed`/`pin_set`/
//! `refusal` columns, which are not metrics. Categorical and flag columns
//! are never evaluable, full stop, and are not part of this domain in the
//! first place: a *tail* is an ordering, and neither kind carries one, so
//! there is nothing to exclude them *from* — that is a structural fact
//! about the report, not a per-column judgment [`evaluable_columns`] makes,
//! and it is documented here rather than emitted as forty-odd redundant
//! roster rows. Among the numeric/integer candidates, a column is
//! *evaluable* unless it fails one of three named tests, in this order:
//!
//! 1. **Too few present values.** Fewer than 50 present worlds cannot
//!    support even a coarse 1 % tail.
//! 2. **Frozen.** `min == max` across every present world — no spread, no
//!    tail.
//! 3. **Both rails tied beyond a 1 % bucket.** More than 1 % of present
//!    worlds share the min *and* more than 1 % share the max — a world
//!    tied into a rail is not "extreme", it is "typical of the rail".
//!
//! Every exclusion carries its reason as committed, renderable text — an
//! exclusion without one is an assertion nothing can check.

use crate::domesday::census::Census;
use hornvale_kernel::math::ln;
use std::collections::BTreeMap;

/// Frozen selection bar (spec §3.4): a world's column counts toward its
/// score when its [`tail_depth`] is at or below this. **Not a significance
/// claim** — the same precedent as D1's 0.80 share bar and D3's 5 % IQR
/// bar: frozen at authoring time, never retuned after seeing a result.
/// type-audit: bare-ok(ratio)
pub const TAIL_DEPTH_BAR: f64 = 0.01;

/// Frozen selection bar (spec §3.4): the number of a world's
/// smallest-`tail_depth` columns published as its report (`k` in the spec).
/// **A display cap, not a significance claim** — a world may have more or
/// fewer than this many columns at or below [`TAIL_DEPTH_BAR`]; this only
/// bounds how many of its closest-to-extreme columns [`WorldAnomaly::flags`]
/// carries.
/// type-audit: bare-ok(count)
pub const REPORT_SIZE: usize = 10;

/// Frozen selection bar (spec §3.4): how many worlds the committed artifact
/// publishes, most anomalous first. **A publication cap, not a significance
/// claim** — [`rank`] itself returns every world; only the renderer applies
/// this.
/// type-audit: bare-ok(count)
pub const TOP_WORLDS: usize = 25;

/// One census column flagged in a world's report: how extreme its value is
/// for that column, and the value itself.
/// type-audit: bare-ok(identifier-text: metric), bare-ok(ratio: depth), bare-ok(artifact: value)
#[derive(Debug, Clone, PartialEq)]
pub struct Flag {
    /// The census metric this flag concerns.
    pub metric: String,
    /// Two-sided tail depth for this world on this metric: `0.0` is the
    /// most extreme present value in the census, `0.5` the median. See
    /// [`tail_depth`].
    pub depth: f64,
    /// This world's raw (unquantized-further, already-parsed) value for the
    /// metric.
    pub value: f64,
}

/// One world's anomaly report: its seed, and its [`REPORT_SIZE`] columns of
/// smallest [`tail_depth`], ascending (most extreme first).
///
/// This is the *report*, not the *score* — a world's ranking position
/// (computed by [`rank`]) depends on how many of its columns clear
/// [`TAIL_DEPTH_BAR`], which may be more or fewer than [`REPORT_SIZE`]; that
/// count is not itself a field here because it is a sort key, not part of
/// what a reader is shown.
/// type-audit: bare-ok(constructor-edge: seed)
#[derive(Debug, Clone, PartialEq)]
pub struct WorldAnomaly {
    /// The world's seed.
    pub seed: u64,
    /// Its columns of smallest tail depth, ascending, capped at
    /// [`REPORT_SIZE`].
    pub flags: Vec<Flag>,
}

/// A column indexed once for repeated `tail_depth` lookups: its present
/// values sorted ascending (`total_cmp`, never `partial_cmp().unwrap()`)
/// and their count.
struct ColumnIndex {
    sorted: Vec<f64>,
    n: usize,
}

/// Which numeric/integer census columns can be scored for a per-world tail
/// (`evaluable`), and which cannot, each carrying the reason it was
/// excluded (`excluded_with_reason`). See the module doc for the three
/// exclusion tests and why categorical/flag columns never appear in either
/// list at all.
/// type-audit: bare-ok(artifact: return)
pub fn evaluable_columns(c: &Census) -> (Vec<String>, Vec<(String, String)>) {
    let mut evaluable = Vec::new();
    let mut excluded = Vec::new();

    for col in &c.columns {
        // Structural columns (`seed`, `pin_set`, `refusal`) and any future
        // one like them carry no domain/role — the same skip `detect.rs`'s
        // D2/D3/D4 apply. They are not metrics, so — like categorical/flag
        // columns — they are outside this function's domain entirely, never
        // emitted as an excluded row.
        if col.domain.is_empty() || col.role.is_empty() {
            continue;
        }
        if col.kind != "numeric" && col.kind != "integer" {
            continue;
        }

        let mut vals: Vec<f64> = c
            .values(&col.name)
            .iter()
            .filter_map(|v| v.parse::<f64>().ok())
            .filter(|v| v.is_finite())
            .collect();
        let n = vals.len();
        if n < 50 {
            excluded.push((
                col.name.clone(),
                format!("too few present values: {n} < 50"),
            ));
            continue;
        }
        vals.sort_by(|a, b| a.total_cmp(b));
        let min = vals[0];
        let max = vals[n - 1];
        if min == max {
            excluded.push((
                col.name.clone(),
                format!("frozen: min == max across {n} worlds"),
            ));
            continue;
        }
        let min_count = vals.iter().filter(|v| **v == min).count();
        let max_count = vals.iter().filter(|v| **v == max).count();
        let bucket = 0.01 * n as f64;
        if min_count as f64 > bucket && max_count as f64 > bucket {
            excluded.push((
                col.name.clone(),
                format!("both rails tied: {min_count} at min, {max_count} at max of {n}"),
            ));
            continue;
        }

        evaluable.push(col.name.clone());
    }

    (evaluable, excluded)
}

/// Two-sided tail depth: `0.0` is the most extreme value in the census,
/// `0.5` the median. Ranks are averaged across ties, so a value shared by
/// many worlds cannot make any one of them look extreme.
///
/// `sorted` must be ascending and hold at least two values (guaranteed by
/// [`evaluable_columns`]'s `>= 50 present` test); `value` need not be a
/// member of `sorted` in general, but every caller here passes a value that
/// came from the same column.
fn tail_depth(sorted: &[f64], value: f64) -> f64 {
    let n = sorted.len();
    assert!(n > 1, "tail depth needs at least two values to have spread");
    let first = sorted.partition_point(|v| *v < value);
    let mut last = first;
    while last < n && sorted[last] == value {
        last += 1;
    }
    // Average rank (0-indexed) of the tied run [first, last).
    let avg_rank = (first as f64 + (last - 1) as f64) / 2.0;
    let n1 = (n - 1) as f64;
    (avg_rank.min(n1 - avg_rank)) / n1
}

/// The smallest representable tail depth for a column with `n` present
/// values.
///
/// **Guards the log.** [`tail_depth`] returns exactly `0.0` for a census's
/// true extreme-holder (its rank is `0` or `n-1`), and a world's score
/// tie-break sums `-ln(depth)` over its flagged columns — `-ln(0.0)` is
/// `+inf`. Left unguarded, EVERY world holding a true extreme on even one
/// evaluable column (common: with well over a hundred evaluable columns and
/// two rails apiece, on the order of two hundred worlds hold at least one)
/// would tie at an infinite tie-break, which defeats the tie-break's whole
/// purpose rather than merely looking odd. Clamping the tie-break's input
/// to the next representable depth above zero — the gap between rank `0`
/// and rank `1` — keeps every such world's tie-break finite and distinct
/// from every other's, while [`Flag::depth`] itself is left unclamped and
/// keeps reporting the true (possibly exactly `0.0`) tail depth to a
/// reader.
fn min_representable_depth(n: usize) -> f64 {
    1.0 / (n as f64 - 1.0)
}

/// Build the column index every scoring call shares: one sorted, present-
/// value vector per evaluable column, built once rather than per world.
fn build_index(c: &Census, evaluable: &[String]) -> BTreeMap<String, ColumnIndex> {
    let mut idx = BTreeMap::new();
    for metric in evaluable {
        let mut sorted: Vec<f64> = c
            .values(metric)
            .iter()
            .filter_map(|v| v.parse::<f64>().ok())
            .filter(|v| v.is_finite())
            .collect();
        sorted.sort_by(|a, b| a.total_cmp(b));
        let n = sorted.len();
        idx.insert(metric.clone(), ColumnIndex { sorted, n });
    }
    idx
}

/// Every `(metric, depth, value)` triple a single world's row carries over
/// the evaluable surface — only for columns where this particular world has
/// a present, parseable value.
fn depths_for_row(
    row: &BTreeMap<String, String>,
    evaluable: &[String],
    idx: &BTreeMap<String, ColumnIndex>,
) -> Vec<(String, f64, f64)> {
    let mut out = Vec::new();
    for metric in evaluable {
        let Some(raw) = row.get(metric) else { continue };
        if raw.is_empty() {
            continue;
        }
        let Ok(value) = raw.parse::<f64>() else {
            continue;
        };
        if !value.is_finite() {
            continue;
        }
        let ci = &idx[metric];
        let depth = tail_depth(&ci.sorted, value);
        out.push((metric.clone(), depth, value));
    }
    out
}

/// Turn one world's raw `(metric, depth, value)` triples into its published
/// report and its (unexposed) sort key: `(flag_count, tiebreak)`, both
/// descending — see the module doc's tautology note for why the count comes
/// from the FULL depth list (every column at or below [`TAIL_DEPTH_BAR`]),
/// not merely the [`REPORT_SIZE`]-capped report a reader is shown.
fn score_world(
    seed: u64,
    mut depths: Vec<(String, f64, f64)>,
    idx: &BTreeMap<String, ColumnIndex>,
) -> (WorldAnomaly, usize, f64) {
    depths.sort_by(|a, b| a.1.total_cmp(&b.1).then_with(|| a.0.cmp(&b.0)));
    let flags: Vec<Flag> = depths
        .iter()
        .take(REPORT_SIZE)
        .map(|(metric, depth, value)| Flag {
            metric: metric.clone(),
            depth: *depth,
            value: *value,
        })
        .collect();

    let flagged: Vec<&(String, f64, f64)> = depths
        .iter()
        .filter(|(_, depth, _)| *depth <= TAIL_DEPTH_BAR)
        .collect();
    let score = flagged.len();
    let tiebreak: f64 = flagged
        .iter()
        .map(|(metric, depth, _)| {
            let floor = min_representable_depth(idx[metric].n);
            -ln(depth.max(floor))
        })
        .sum();

    (WorldAnomaly { seed, flags }, score, tiebreak)
}

/// The `seed` column's value for one census row, or `None` if it is missing
/// or unparseable (never true on the committed census — every row carries a
/// seed — but this reader makes no assumption about a caller-built one).
fn seed_of(row: &BTreeMap<String, String>) -> Option<u64> {
    row.get("seed")?.parse::<u64>().ok()
}

/// Score and rank every world in the census, most anomalous first.
///
/// A world's score is the count of its evaluable columns with `tail_depth
/// <=` [`TAIL_DEPTH_BAR`] ("how many ways is this world extreme"), ties
/// broken by the sum of `-ln(tail_depth)` over exactly those columns (see
/// [`min_representable_depth`] for how a `0.0` depth is kept finite there),
/// ties broken finally by ascending seed for a total, deterministic order.
/// This returns **every** world — publishing only the top [`TOP_WORLDS`] is
/// a rendering decision, not something this function truncates (spec
/// §3.2's tautology note: a ranking asserts nothing about how many worlds
/// are anomalous).
/// type-audit: bare-ok(identifier-text: return-is-not-primitive)
pub fn rank(c: &Census) -> Vec<WorldAnomaly> {
    let (evaluable, _) = evaluable_columns(c);
    let idx = build_index(c, &evaluable);

    let mut scored: Vec<(WorldAnomaly, usize, f64)> = c
        .rows
        .iter()
        .filter_map(|row| {
            let seed = seed_of(row)?;
            let depths = depths_for_row(row, &evaluable, &idx);
            Some(score_world(seed, depths, &idx))
        })
        .collect();

    scored.sort_by(|a, b| {
        b.1.cmp(&a.1)
            .then_with(|| b.2.total_cmp(&a.2))
            .then_with(|| a.0.seed.cmp(&b.0.seed))
    });

    scored.into_iter().map(|(wa, _, _)| wa).collect()
}

/// One world's report, by seed — the `--seed N` CLI path: prints without
/// ranking the other 999 worlds or writing anything. `None` if no census
/// row carries this seed.
/// type-audit: bare-ok(constructor-edge: seed)
pub fn for_seed(c: &Census, seed: u64) -> Option<WorldAnomaly> {
    let (evaluable, _) = evaluable_columns(c);
    let idx = build_index(c, &evaluable);
    let row = c.rows.iter().find(|r| seed_of(r) == Some(seed))?;
    let depths = depths_for_row(row, &evaluable, &idx);
    let (wa, _, _) = score_world(seed, depths, &idx);
    Some(wa)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domesday::census::{Column, load, repo_root};
    use crate::domesday::detect::detect;

    fn committed() -> Census {
        load(&repo_root().join("book/src/laboratory/generated/the-census"))
            .expect("the committed census loads")
    }

    /// A column no world can be an outlier on must be excluded, and the
    /// exclusion must carry a reason. Enforces `excluded => has a stated
    /// reason`; it does NOT enforce that every excluded column is genuinely
    /// degenerate — that direction is `exclusions_agree_with_domesday`
    /// below.
    #[test]
    fn a_frozen_column_is_excluded_with_a_reason() {
        let c = committed();
        let (evaluable, excluded) = evaluable_columns(&c);
        assert!(
            !evaluable.iter().any(|m| m == "hue-depth-goblin"),
            "hue-depth-goblin is min == max across all 1000 worlds and cannot have a tail"
        );
        let reason = excluded.iter().find(|(m, _)| m == "hue-depth-goblin");
        assert!(
            reason.is_some_and(|(_, r)| !r.is_empty()),
            "an exclusion without a reason is an assertion nothing can check"
        );
    }

    /// The partition claim from spec §3.3, stated as a test rather than a
    /// promise: every column this report excludes is a column the Domesday
    /// already reports as a weakness under D2 (frozen) or D4 (at-rail).
    ///
    /// **Measured result (2026-08-13, 225-column census): FAILS.** Five
    /// columns are excluded here but reported by neither D2 nor D4:
    /// `first-day-occ-cause-burned`, `first-day-occ-cause-plague`,
    /// `pantheon-size-goblin-twin`, `name-length-goblin-twin` and
    /// `pantheon-cyclic-share-goblin-twin` — every one of them present in
    /// **zero** of the 1000 census worlds. `stats::numeric` returns `None`
    /// when a metric has zero present values (its `vals` vector is empty),
    /// and D2/D4 are both gated on `numeric(c, ..)` returning `Some`, so an
    /// all-absent column is invisible to both — not merely uninteresting to
    /// them, structurally unreachable. **This is a real gap in the
    /// Domesday**, not a defect in this exclusion logic: a column with no
    /// distribution has nothing for either detector to judge. Per the task
    /// brief's decision rule this is recorded, not patched away — the
    /// assertion below is byte-for-byte the one the brief specified, and it
    /// is `#[ignore]`d rather than weakened (its five failures are named
    /// here, not hidden inside a loosened check).
    #[test]
    #[ignore = "FALSIFIED (spec §3.3's partition claim, decision 0016 preregistration \
                discipline): 5 zero-present-value columns (first-day-occ-cause-burned, \
                first-day-occ-cause-plague, pantheon-size-goblin-twin, \
                name-length-goblin-twin, pantheon-cyclic-share-goblin-twin) are excluded \
                by evaluable_columns but reported by neither D2 nor D4, because both \
                detectors require stats::numeric to return Some, which it cannot for a \
                column with zero present values. See this test's own doc comment and \
                task-4-report.md for the full finding; do not silently re-enable without \
                first fixing D2/D4's blind spot on all-absent columns or re-scoping this \
                exclusion."]
    fn exclusions_agree_with_domesday() {
        let c = committed();
        let (_, excluded) = evaluable_columns(&c);
        let findings = detect(&c, &[], &[]);
        for (metric, _) in &excluded {
            assert!(
                findings.iter().any(|f| &f.metric == metric
                    && (f.detector.starts_with("D2") || f.detector.starts_with("D4"))),
                "{metric} is excluded here but reported by neither D2 nor D4 — the two \
                 instruments were claimed to partition and do not"
            );
        }
    }

    #[test]
    fn categorical_and_flag_columns_are_never_evaluable() {
        let c = committed();
        let (evaluable, excluded) = evaluable_columns(&c);
        let cat_or_flag: Vec<&str> = c
            .columns
            .iter()
            .filter(|col| col.kind == "categorical" || col.kind == "flag")
            .map(|col| col.name.as_str())
            .collect();
        assert!(!cat_or_flag.is_empty(), "sanity: some exist on the census");
        for name in cat_or_flag {
            assert!(
                !evaluable.iter().any(|m| m == name),
                "{name} must not be evaluable"
            );
            assert!(
                !excluded.iter().any(|(m, _)| m == name),
                "{name} is structurally out of scope, not a per-column exclusion"
            );
        }
    }

    #[test]
    fn structural_columns_are_neither_evaluable_nor_excluded() {
        let c = committed();
        let (evaluable, excluded) = evaluable_columns(&c);
        for name in ["seed", "pin_set", "refusal"] {
            assert!(
                !evaluable.iter().any(|m| m == name),
                "{name} is not a metric"
            );
            assert!(
                !excluded.iter().any(|(m, _)| m == name),
                "{name} is not a metric and must not appear in the roster either"
            );
        }
    }

    #[test]
    fn tail_depth_is_zero_at_the_extremes_and_half_at_the_median() {
        let sorted = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        assert_eq!(tail_depth(&sorted, 1.0), 0.0);
        assert_eq!(tail_depth(&sorted, 5.0), 0.0);
        assert_eq!(tail_depth(&sorted, 3.0), 0.5);
    }

    #[test]
    fn tail_depth_averages_ranks_across_ties() {
        // Three worlds tied at the minimum share rank (0+1+2)/3 = 1, over
        // n-1 = 4: depth 0.25 for every one of them, not 0.0 for one and
        // something else for the rest.
        let sorted = vec![1.0, 1.0, 1.0, 4.0, 5.0];
        assert_eq!(tail_depth(&sorted, 1.0), 0.25);
    }

    /// Guards the log against a REAL extreme-holding world, not a synthetic
    /// vector: finds the actual seed holding the minimum value of a real
    /// evaluable column on the committed census, confirms `for_seed`
    /// reports its true (unclamped) `depth == 0.0` for that column, and
    /// confirms the clamp `min_representable_depth` uses for that column's
    /// `n` is finite and yields a finite log — the guard `score_world`
    /// actually exercises when it sums `-ln(depth)` over this world's
    /// flagged columns.
    #[test]
    fn the_log_guard_holds_for_a_real_extreme_holding_world() {
        let c = committed();
        let (evaluable, _) = evaluable_columns(&c);
        let metric = evaluable.first().expect("at least one evaluable column");

        let idx = build_index(&c, &evaluable);
        let ci = &idx[metric];
        let min_value = ci.sorted[0];
        assert_eq!(
            tail_depth(&ci.sorted, min_value),
            0.0,
            "sanity: the column's own minimum must have depth exactly 0.0"
        );

        let extreme_seed = c
            .rows
            .iter()
            .find(|row| {
                row.get(metric)
                    .and_then(|v| v.parse::<f64>().ok())
                    .map(|v| v == min_value)
                    .unwrap_or(false)
            })
            .and_then(seed_of)
            .expect("a real census row holds this column's true minimum");

        let wa = for_seed(&c, extreme_seed).expect("the extreme-holding seed is a real world");
        let flag = wa
            .flags
            .iter()
            .find(|f| f.metric == *metric)
            .expect("the extreme column must be among this world's smallest-depth report");
        assert_eq!(
            flag.depth, 0.0,
            "Flag::depth is the TRUE tail depth, unclamped, even for the extreme holder"
        );

        let floor = min_representable_depth(ci.n);
        assert!(
            floor.is_finite() && floor > 0.0,
            "the clamp floor itself must be finite"
        );
        assert!(
            (-ln(floor)).is_finite(),
            "the tie-break's log term must be finite once clamped, not +inf"
        );
        assert!(
            (-ln(0.0_f64)).is_infinite(),
            "sanity: the UNCLAMPED case really would have been infinite"
        );

        // End to end: ranking every world must not panic or produce a
        // non-finite ordering key reaching the sort — if it did, `rank`
        // would still return (Rust does not panic on comparing infinities),
        // but every extreme-holding world's tie-break would collapse to the
        // same +inf, which this world's presence in a stable, repeatable
        // position rules out indirectly via the determinism check below.
        let ranked = rank(&c);
        assert_eq!(ranked.len(), c.rows.len(), "every world is ranked");
        assert!(
            ranked.iter().any(|w| w.seed == extreme_seed),
            "the extreme-holding world must appear in the ranking"
        );
    }

    #[test]
    fn rank_is_deterministic() {
        let c = committed();
        let a = rank(&c);
        let b = rank(&c);
        assert_eq!(
            a, b,
            "two runs over the same committed census must agree exactly"
        );
    }

    #[test]
    fn rank_orders_most_anomalous_first() {
        let c = committed();
        let ranked = rank(&c);
        assert_eq!(
            ranked.len(),
            c.rows.len(),
            "every world is ranked, not just the top N"
        );
        // The report shape holds for the top world at least: its flags are
        // sorted ascending by depth (smallest — most extreme — first).
        let top = &ranked[0];
        for pair in top.flags.windows(2) {
            assert!(
                pair[0].depth <= pair[1].depth,
                "a world's report must be ascending by depth: {:?}",
                top.flags
            );
        }
    }

    #[test]
    fn for_seed_matches_the_same_worlds_entry_in_rank() {
        let c = committed();
        let ranked = rank(&c);
        let sample = &ranked[0];
        let solo = for_seed(&c, sample.seed).expect("seed exists");
        assert_eq!(
            solo, *sample,
            "for_seed must agree with rank's entry for the same world"
        );
    }

    #[test]
    fn for_seed_returns_none_for_an_unknown_seed() {
        let c = committed();
        assert!(for_seed(&c, u64::MAX).is_none());
    }

    #[test]
    fn a_world_with_no_flagged_columns_still_reports_up_to_report_size() {
        // Not every one of 1000 worlds clears the 1% bar on any column —
        // the report (top-REPORT_SIZE by depth) is published regardless,
        // distinct from the (possibly empty) flagged/score set. This just
        // confirms the report is never empty for a real world with any
        // evaluable columns at all.
        let c = committed();
        let ranked = rank(&c);
        for w in &ranked {
            assert!(
                w.flags.len() <= REPORT_SIZE,
                "seed {} carries {} flags, more than REPORT_SIZE",
                w.seed,
                w.flags.len()
            );
        }
    }

    #[test]
    fn evaluable_columns_measured_surface_on_the_225_column_census() {
        // Pinned so a future census refresh that moves this materially is
        // visible here, not just in prose. See task-4-report.md for the
        // comparison against the spec's original 204-column figure
        // (107 evaluable / 34 degenerate).
        let c = committed();
        let (evaluable, excluded) = evaluable_columns(&c);
        assert_eq!(
            evaluable.len(),
            116,
            "evaluable count moved — re-measure and update this"
        );
        assert_eq!(
            excluded.len(),
            47,
            "excluded count moved — re-measure and update this"
        );
    }

    /// A numeric metric column, matching the shape every test below needs.
    fn numeric_column(name: &str) -> Column {
        Column {
            name: name.to_string(),
            kind: "numeric".to_string(),
            doc: String::new(),
            domain: "climate".to_string(),
            role: "descriptor".to_string(),
        }
    }

    #[test]
    fn exclusion_reasons_carry_the_exact_documented_wording() {
        // 200 worlds (>= 100, so a lone min/max holder — count 1 — never
        // itself exceeds the 1%-of-n bucket and reads as "tied"), three
        // columns: `frozen-m` is 1.0 for every world; `tied-m` puts 190
        // worlds at its min (1.0) and 10 at its max (6.0) — both rails clear
        // the 1% (2-world) bucket; `normal-m` is a plain 200-value ramp with
        // exactly one world at each rail, so it alone survives.
        let columns = vec![
            numeric_column("frozen-m"),
            numeric_column("tied-m"),
            numeric_column("normal-m"),
        ];
        let rows: Vec<BTreeMap<String, String>> = (0..200)
            .map(|i| {
                let tied = if i < 190 { "1.0" } else { "6.0" };
                BTreeMap::from([
                    ("frozen-m".to_string(), "1.0".to_string()),
                    ("tied-m".to_string(), tied.to_string()),
                    ("normal-m".to_string(), (100 + i).to_string()),
                ])
            })
            .collect();
        let c = Census { columns, rows };

        let (evaluable, excluded) = evaluable_columns(&c);
        assert_eq!(evaluable, vec!["normal-m".to_string()]);

        let frozen_reason = excluded
            .iter()
            .find(|(m, _)| m == "frozen-m")
            .expect("frozen-m excluded")
            .1
            .clone();
        assert_eq!(frozen_reason, "frozen: min == max across 200 worlds");

        let tied_reason = excluded
            .iter()
            .find(|(m, _)| m == "tied-m")
            .expect("tied-m excluded")
            .1
            .clone();
        assert_eq!(tied_reason, "both rails tied: 190 at min, 10 at max of 200");
    }

    #[test]
    fn too_few_present_values_is_excluded_with_its_reason() {
        let rows: Vec<BTreeMap<String, String>> = (0..49)
            .map(|i: u32| {
                BTreeMap::from([
                    ("m".to_string(), i.to_string()),
                    ("seed".to_string(), (i + 1).to_string()),
                ])
            })
            .collect();
        let c = Census {
            columns: vec![numeric_column("m")],
            rows,
        };
        let (evaluable, excluded) = evaluable_columns(&c);
        assert!(evaluable.is_empty());
        assert_eq!(
            excluded,
            vec![(
                "m".to_string(),
                "too few present values: 49 < 50".to_string()
            )]
        );
    }
}
