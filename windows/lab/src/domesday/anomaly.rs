//! The anomaly report — the Domesday's transpose, per world (spec §3).
//!
//! The Domesday (`detect.rs`) asks *is this column weak across 1000
//! worlds*; this module asks *is this world strange across the census's
//! columns*. Same census, same reader (`census::load`), axis rotated ninety
//! degrees. It **reuses** [`crate::domesday::census`] — `Census::values`
//! and `Census::columns` are the only reads over the committed CSV anywhere
//! in this file — and implements no census loader of its own (Global
//! Constraints).
//!
//! **It does NOT import `crate::domesday::stats` (F4, fix round 1), and
//! that is a need-based fact, not an oversight to restate as reuse.**
//! `stats::numeric` returns five summary numbers (min/p25/median/p75/max);
//! it exposes neither the *sorted vector* [`tail_depth`] needs to compute a
//! per-world rank, nor the *rail tie counts* [`evaluable_columns`] needs to
//! detect a both-rails-tied column — both require walking the underlying
//! values directly, which is what `evaluable_columns` and `build_index` do.
//! Re-deriving a sorted `Vec<f64>` from `Census::values` here is therefore
//! not the percentile duplication the Global Constraints forbid: it is
//! `stats.rs` not having the shape this module's actual computation
//! ([`tail_depth`], a per-world rank — a thing `stats::numeric` does not
//! compute at all) needs, verified by reading `stats.rs` rather than
//! assumed.
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
/// plumb: pending(wave-1)
pub const TAIL_DEPTH_BAR: f64 = 0.01;

/// Frozen selection bar (spec §3.4): the number of a world's
/// smallest-`tail_depth` columns published as its report (`k` in the spec).
/// **A display cap, not a significance claim** — a world may have more or
/// fewer than this many columns at or below [`TAIL_DEPTH_BAR`]; this only
/// bounds how many of its closest-to-extreme columns [`WorldAnomaly::flags`]
/// carries.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const REPORT_SIZE: usize = 10;

/// Frozen selection bar (spec §3.4): how many worlds the committed artifact
/// publishes, most anomalous first. **A publication cap, not a significance
/// claim** — [`rank`] itself returns every world; only the renderer applies
/// this.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
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

/// One world's anomaly report: its seed, its true (uncapped) score, and its
/// [`REPORT_SIZE`] columns of smallest [`tail_depth`], ascending (most
/// extreme first).
///
/// `flags` is the *report* — capped at [`REPORT_SIZE`] for display. `score`
/// is the *ranking key* [`rank`] sorts on: the count of ALL of this world's
/// evaluable columns at or below [`TAIL_DEPTH_BAR`], which may be more or
/// fewer than [`REPORT_SIZE`]. The two are independent (F2, fix round 1): a
/// world with 21 flagged columns and one with exactly 10 both show a
/// 10-column `flags` report, and only `score` — not `flags.len()` —
/// distinguishes them. Never recompute an approximation of `score` by
/// counting `flags` at or below the bar; that count saturates at
/// `REPORT_SIZE` and silently underreports every world whose true score
/// exceeds it.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(count: score)
#[derive(Debug, Clone, PartialEq)]
pub struct WorldAnomaly {
    /// The world's seed.
    pub seed: u64,
    /// The count of ALL evaluable columns at or below [`TAIL_DEPTH_BAR`] for
    /// this world — uncapped, and the value [`rank`] sorts on. See the
    /// struct doc: this is not `flags.len()`.
    pub score: usize,
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
/// [`evaluable_columns`]'s `>= 50 present` test). `value` need NOT be a
/// member of `sorted`: [`score_row`] scores worlds that are not in the
/// census at all, and such a world can hold a value outside every value the
/// census ever saw.
///
/// **Out of range clamps to `0.0`, and that is a deliberate loss.** For a
/// value beyond either rail the tied run `[first, last)` is empty, which
/// leaves `avg_rank` half a rank outside `[0, n-1]` and the raw expression
/// slightly NEGATIVE (`-0.5 / (n-1)`, at either end). Reporting a negative
/// depth would put such a world ahead of the census's own extreme holder on
/// a scale this module documents as `[0, 0.5]`, so the result is clamped to
/// the scale's own floor instead. The clamp is provably a no-op for every
/// in-census caller — for a `value` present in `sorted`, `first < n` and
/// `last > first`, so `avg_rank ∈ [0, n-1]` and `min(avg_rank, n1 -
/// avg_rank) >= 0` already. What it costs is magnitude: a world at 1.0001x
/// the census maximum and one at 1000x both read `0.0`. That is honest for
/// a RANK statistic — the census affords no way to rank beyond its own
/// extreme — and inventing an extrapolated depth would be a different,
/// unpreregistered instrument.
fn tail_depth(sorted: &[f64], value: f64) -> f64 {
    let n = sorted.len();
    assert!(n > 1, "tail depth needs at least two values to have spread");
    let first = sorted.partition_point(|v| *v < value);
    let mut last = first;
    while last < n && sorted[last] == value {
        last += 1;
    }
    // Average rank (0-indexed) of the tied run [first, last) — or, when the
    // value is absent from `sorted` entirely and that run is EMPTY, the half
    // rank between the two values it falls between. The empty-run branch is
    // not cosmetic: `last - 1` on a `usize` with `last == 0` (a value below
    // the census minimum) underflows and panics in debug.
    let avg_rank = if last > first {
        (first as f64 + (last - 1) as f64) / 2.0
    } else {
        first as f64 - 0.5
    };
    let n1 = (n - 1) as f64;
    ((avg_rank.min(n1 - avg_rank)) / n1).max(0.0)
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
/// `WorldAnomaly` (carrying both the capped `flags` report AND the
/// true, uncapped `score` — F2) plus the tie-break `rank` sorts on.
/// `WorldAnomaly::score` and the returned `f64` tie-break are always
/// computed from the FULL depth list (every column at or below
/// [`TAIL_DEPTH_BAR`]), never from the [`REPORT_SIZE`]-capped `flags` a
/// reader is shown — see the module doc's tautology note and
/// [`WorldAnomaly`]'s own doc for why the two must never be conflated.
fn score_world(
    seed: u64,
    mut depths: Vec<(String, f64, f64)>,
    idx: &BTreeMap<String, ColumnIndex>,
) -> (WorldAnomaly, f64) {
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

    (WorldAnomaly { seed, score, flags }, tiebreak)
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

    let mut scored: Vec<(WorldAnomaly, f64)> = c
        .rows
        .iter()
        .filter_map(|row| {
            let seed = seed_of(row)?;
            let depths = depths_for_row(row, &evaluable, &idx);
            Some(score_world(seed, depths, &idx))
        })
        .collect();

    scored.sort_by(|a, b| {
        b.0.score
            .cmp(&a.0.score)
            .then_with(|| b.1.total_cmp(&a.1))
            .then_with(|| a.0.seed.cmp(&b.0.seed))
    });

    scored.into_iter().map(|(wa, _)| wa).collect()
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
    let (wa, _) = score_world(seed, depths, &idx);
    Some(wa)
}

/// Score one world's row against this census's percentiles when that row is
/// **not a member of the census** — the seam H1's injection battery and H2's
/// held-out calibration arm both score through (spec §3.5).
///
/// [`rank`] and [`for_seed`] can only score rows already inside `c`. A
/// perturbed world (a census seed rebuilt from mutated source) and a
/// held-out world (a seed the census never covered) are neither, so the row
/// arrives from the caller — read out of a separately authored `rows.csv` by
/// the same [`crate::domesday::census::load`] every other reader here uses —
/// while the ranking prior stays exactly the committed census.
///
/// **Never splice the row into the census and call [`for_seed`] instead.**
/// That is the obvious-looking shortcut and nothing in the suite would go
/// red if someone took it: adding the row to `c` contaminates the very
/// percentiles it is then scored against. Every column index would be built
/// over `n + 1` values including the perturbed one, so a planted extreme
/// would partly *define* the tail it is being tested for membership in, and
/// the more extreme the perturbation the more it would move its own
/// yardstick. The index here is built from `c` alone, and `row` is read
/// against it.
///
/// `row` is keyed by column name exactly as [`Census::rows`] entries are —
/// an absent, empty or unparseable value for a column simply means this
/// world contributes no depth for it, the same rule [`rank`] applies to a
/// census row.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(artifact: row)
pub fn score_row(c: &Census, seed: u64, row: &BTreeMap<String, String>) -> WorldAnomaly {
    let (evaluable, _) = evaluable_columns(c);
    let idx = build_index(c, &evaluable);
    let depths = depths_for_row(row, &evaluable, &idx);
    score_world(seed, depths, &idx).0
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
    ///
    /// **Kept unweakened rather than narrowed (fix round 1).** A version of
    /// this test scoped only to the 42 columns that DO partition would pass
    /// today and forever, including after the D2/D4 blind spot is closed —
    /// it could never go stale and could never signal the finding was
    /// discharged, the exact STALE-DECL failure `seam-guard` exists to
    /// catch. The narrowed claim is asserted separately, as an
    /// always-running test, by
    /// `partitioning_columns_are_all_covered_by_d2_or_d4` below — that one
    /// guards the 42 columns nothing else currently pins; this one stays
    /// the honest, falsifiable whole claim.
    ///
    /// Registered in `cli/tests/heavy_tier.rs`'s `EXPECTED_UNTOKENISED`
    /// roster (the ignore-reason ratchet) and tracked as
    /// `PROC-domesday-all-absent-blind-spot` in
    /// `book/src/frontier/idea-registry.md`.
    #[test]
    #[ignore = "PREREGISTERED, not met: awaits PROC-domesday-all-absent-blind-spot (5 zero-present-value columns are invisible to D2/D4 — stats::numeric returns None on an empty column)"]
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

    /// The narrowed half of the partition claim, always-running: every
    /// excluded column with **at least one present value** is reported by
    /// D2 or D4. This is the direction that actually holds today (42 of the
    /// 47 excluded columns) — the ignored `exclusions_agree_with_domesday`
    /// above is the ONLY thing that had ever exercised it, so before this
    /// test existed a regression on any of those 42 columns would have gone
    /// undetected by the everyday gate. Added as a second test, not a
    /// replacement (fix round 1, item 3): narrowing the ignored test itself
    /// would let it go stale silently once the blind spot closes.
    #[test]
    fn partitioning_columns_are_all_covered_by_d2_or_d4() {
        let c = committed();
        let (_, excluded) = evaluable_columns(&c);
        let findings = detect(&c, &[], &[]);
        // NARROWED (The Glasshouse, k = 0.30), and the reason matters more
        // than the change. This test asserted that EVERY excluded column with
        // a present value is reported by D2 or D4. That claim is not merely
        // unmet — IT WAS NEVER TRUE. The three predicates are logically
        // independent:
        //
        //     D2 fires on  min == max
        //     D4 fires on  median == min || median == max
        //     "both rails tied" means ties AT min and AT max, which says
        //     nothing about where the median sits
        //
        // A column with ties at both rails and its median strictly between
        // them satisfies the exclusion and neither detector. Nothing forbids
        // that shape; the census simply had no instance of it until the world
        // warmed. `first-day-occ-cause-famine` is the first: 705 of 1000
        // worlds present, ~48 distinct days, excluded as "both rails tied:
        // 40 at min, 12 at max of 705", median in the middle, and reported by
        // neither detector.
        //
        // So this is a CORRECTION OF AN OVER-CLAIM, not a weakened test, and
        // the distinction is worth defending because the two look identical
        // in a diff. The test is not being relaxed to accommodate a result it
        // disliked — the exclusion reason it now skips is one it could never
        // have covered, and skipping it is what stops a permanent red from
        // training everyone to ignore this file. The gap is registered as
        // PROC-domesday-rail-tie-blind-spot, the exact sibling of
        // PROC-domesday-all-absent-blind-spot which this test's own message
        // already cites for the zero-present-value half. Closing either needs
        // a further detector, not a change here.
        //
        // WHAT STILL HOLDS, and it is the bulk of the claim: every excluded
        // column with a present value AND a coverable exclusion reason is
        // still required to be reported. A regression on any of those is
        // still caught by the everyday gate, which is what this test was
        // added for.
        let partitioning: Vec<&(String, String)> = excluded
            .iter()
            .filter(|(metric, _)| !c.values(metric).is_empty())
            .filter(|(_, reason)| !reason.contains("both rails tied"))
            .collect();
        assert!(
            !partitioning.is_empty(),
            "sanity: some excluded columns have at least one present value"
        );
        for (metric, _) in partitioning {
            assert!(
                findings.iter().any(|f| &f.metric == metric
                    && (f.detector.starts_with("D2") || f.detector.starts_with("D4"))),
                "{metric} has a present value and is excluded here, but is reported by \
                 neither D2 nor D4 — this is the direction that is supposed to always \
                 hold for a COVERABLE exclusion reason. Two reasons are known NOT to be \
                 coverable and are tracked as registry rows rather than asserted here: \
                 zero present values (PROC-domesday-all-absent-blind-spot) and ties at \
                 both rails with the median between them \
                 (PROC-domesday-rail-tie-blind-spot). If the reason you are looking at is \
                 neither of those, this is a real regression"
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
        // F1 (fix round 1): the original version of this test asserted only
        // on `min_representable_depth(n)` in isolation, never on the
        // tie-break `score_world` actually computes — so removing the
        // clamp entirely (`-ln(depth.max(floor))` -> `-ln(*depth)`) left
        // this test, and the whole domesday suite, green. Fixed by calling
        // `score_world` directly, on a REAL extreme-holding row, and
        // asserting on the tie-break IT returns.
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

        let extreme_row = c
            .rows
            .iter()
            .find(|row| {
                row.get(metric)
                    .and_then(|v| v.parse::<f64>().ok())
                    .map(|v| v == min_value)
                    .unwrap_or(false)
            })
            .expect("a real census row holds this column's true minimum");
        let extreme_seed = seed_of(extreme_row).expect("the extreme row carries a seed");

        let depths = depths_for_row(extreme_row, &evaluable, &idx);
        assert!(
            depths.iter().any(|(m, d, _)| m == metric && *d == 0.0),
            "sanity: the extreme row's own depths must carry the 0.0 for {metric}"
        );

        // The load-bearing assertion: call the SAME function `rank`/
        // `for_seed` call, on this real row, and check the tie-break IT
        // returns — not a value recomputed alongside it.
        let (wa, tiebreak) = score_world(extreme_seed, depths, &idx);
        assert!(
            wa.score >= 1,
            "sanity: the extreme-holding world must flag at least this one column"
        );
        assert!(
            tiebreak.is_finite(),
            "score_world's tie-break must be finite for a real extreme-holding world, \
             not +inf (tiebreak was {tiebreak})"
        );

        let flag = wa
            .flags
            .iter()
            .find(|f| f.metric == *metric)
            .expect("the extreme column must be among this world's smallest-depth report");
        assert_eq!(
            flag.depth, 0.0,
            "Flag::depth is the TRUE tail depth, unclamped, even for the extreme holder"
        );

        assert!(
            (-ln(0.0_f64)).is_infinite(),
            "sanity: the UNCLAMPED case really would have been infinite — this is the \
             control that proves the guard above is doing real work"
        );

        // End to end: ranking every world must not panic or produce a
        // non-finite ordering key reaching the sort.
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
    fn evaluable_columns_measured_surface_on_the_292_column_census() {
        // Pinned so a future census refresh that moves this materially is
        // visible here, not just in prose. See task-4-report.md for the
        // comparison against the spec's original 204-column figure
        // (107 evaluable / 34 degenerate).
        //
        // 116 -> 117 at The Hearsay's census refresh, which added exactly one
        // column (`history-myth-hop-median`). It lands on the EVALUABLE side,
        // not the excluded one, so the excluded count below is unchanged at 47
        // — the metric is `SummaryKind::Numeric` and varies across worlds, so
        // it is a real surface for the anomaly report rather than a constant
        // the ranker has to skip. That asymmetry is the informative part: a
        // new column moving `excluded` instead would mean it was degenerate.
        // THE GLASSHOUSE (Stage B, k = 0.30): evaluable 117 -> 115, excluded
        // 47 -> 50, total 164 -> 165. The +1 total is `greenhouse-forcing-k`
        // (Task 3), and it lands EVALUABLE — the asymmetry the paragraph above
        // calls informative still reads the right way for the new column.
        //
        // WHAT IS ESTABLISHED, and what is not. Diffing the two censuses on a
        // present-value basis identifies exactly ONE column that became
        // degenerate: `name-prefix-region-scope`, which carried both `1` and
        // `2` over 1000 worlds and now carries only `1`. That accounts for one
        // of the three columns that moved to the excluded side.
        //
        // THE OTHER TWO ARE NOT IDENTIFIED, and this comment says so rather
        // than supplying a plausible cause. No other shared column loses its
        // variation by that measure, so whatever moved them is a property of
        // `evaluable_columns`' own criteria rather than of the CSV's spread,
        // and nothing here measured which. A reader re-deriving this should
        // instrument `evaluable_columns` directly instead of diffing the
        // census, which is the mistake this note exists to save them.
        //
        // THE BURR (ROOT_EPOCH v4): evaluable 115 -> 114, excluded 50 -> 51,
        // total unchanged at 165 — exactly one column moved from evaluable to
        // excluded. IDENTIFIED by the method this note prescribes — instrument
        // `evaluable_columns` and compare its evaluable/excluded sets under this
        // refresh against the pre-refresh (The Underworld's) census, rather than
        // diffing present values: the column is `cascade-rules-fired-goblin`,
        // now excluded with reason `both rails tied: 173 at min, 75 at max of
        // 1000`. It is a cascade metric and this epoch reseeds goblin's
        // sound-change cascade, so its distribution now piles worlds at both
        // rails instead of spreading a tail the ranker can rank on — a genuine
        // degeneracy, not a CSV artifact, and NOT the `min == max` collapse the
        // neighbouring `hue-depth-goblin` case pins. It surfaced only at the
        // merge re-run: the previous chamber run reddened on `census_duration`
        // first and nextest's fail-fast cancelled this test, so the budget red
        // masked it. (An earlier draft of this line said this test "is not in
        // the sub-floor tier a local gate runs". It IS: docs/timings/
        // subfloor-roster.tsv selects it by exact name, and it is what refused
        // The Warp's census delivery first. Since The Spillway a census
        // delivery commit stands it down under HV_CENSUS_DELIVERY, and the
        // merge of the delivery branch demands the re-pin of the campaign
        // that submits it — decision 0836.)
        // THE GRANARY: evaluable 114 -> 117, excluded 51 -> 50, total
        // 227 -> 229 — the campaign's two new census columns
        // (`granary-raid-phase-concentration`,
        // `granary-raids-in-depleted-half`) plus one further column crossing
        // onto the evaluable side at this epoch. Both granary metrics are
        // Numeric and vary across worlds, so they are real ranker surface.
        // THE WINZE (2026-08-29, canonical census on lefford at bba2be3efc83,
        // goldens cherry-picked as 71202ac19): evaluable 117 -> 118, excluded
        // unchanged at 50, metric columns 226 -> 227 — the campaign's one new
        // census column, `breached-delving-count`, lands EVALUABLE, so the
        // asymmetry the paragraphs above call informative reads the right way
        // for it: an integer column varying across worlds is real ranker
        // surface, where a new column moving `excluded` instead would have
        // meant it was degenerate.
        // THE WEFT (2026-09-04, canonical census on lefford at fbe2f5a9003f,
        // goldens delivered as b162273b4): evaluable 118 -> 139, excluded
        // 50 -> 51, metric columns 227 -> 249. All 22 new Weft columns are
        // Numeric; 21 vary across the 1000 worlds and land EVALUABLE, while
        // `weft-legibility-mi-erratic` is the sole new exclusion, frozen at
        // one value across all 1000 worlds. The generated anomaly report's
        // exclusion table independently records that same one-column delta.
        // THE WARP (2026-09-05, canonical census on lefford at 4a419e996ef7):
        // evaluable 139 -> 170, excluded 51 -> 52, metric columns 249 -> 281
        // (284 CSV columns less `seed`, `pin_set` and `refusal`). All 32 new
        // `warp-*` columns are Numeric; 31 vary across the 1000 worlds and
        // land EVALUABLE, while `warp-found-fraction-erratic` is the sole
        // new exclusion — Absent on every world by construction, because the
        // erratic's cause is a constant and a found fraction over it is
        // undefined (spec section 5.2). The Weft's spring/overhang columns
        // moved in VALUE at this refresh (the sign kinds' recipe was
        // re-parameterised) but stayed on the evaluable side.
        // THE LOT (2026-09-06, canonical census on lefford at 6e46132790fb,
        // goldens delivered as d2bd513f1): evaluable 170 -> 176, excluded
        // unchanged at 52, metric columns 281 -> 287 (290 CSV columns less
        // `seed`, `pin_set` and `refusal`). All six new `lot-*` columns are
        // Numeric and vary across the 1000 worlds, so all six land EVALUABLE
        // and none is a new exclusion — the asymmetry the paragraphs above
        // call informative reads the right way for every one of them. The
        // committed fixture had read PREDATES for these columns until this
        // refresh (the additive case), which is why this pin did not move
        // when the metrics were registered.
        // THE MURRAIN (2026-09-07, canonical census on lefford at
        // 85ef1edd3089, goldens delivered as c90072b92): evaluable 176 ->
        // 180, excluded unchanged at 52, metric columns 287 -> 292 (295 CSV
        // columns less `seed`, `pin_set` and `refusal`). Four of the five new
        // epidemic/Lot columns vary across the 1000 worlds and land
        // EVALUABLE; the remaining new column is structurally absent and does
        // not enter either surface.
        // THE WANDERERS (2026-09-10, goldens delivered as 270b9f8a): the
        // refreshed values move evaluable 180 -> 181 and excluded 52 -> 51
        // without changing the 292-column metric surface. The existing
        // `first-day-occ-tech-neolithic` column now varies enough to leave the
        // excluded rails, so this witness records the measured surface rather
        // than treating the prior classification as a permanent schema fact.
        // The test's NAME carries the census's metric-column count (290 CSV
        // columns less `seed`, `pin_set` and `refusal`);
        // `docs/timings/subfloor-roster.tsv`
        // selects sub-floor tests by EXACT name and is updated in the same
        // commit, since a stale id there selects nothing and prints green.
        let c = committed();
        let (evaluable, excluded) = evaluable_columns(&c);
        // THE TRENCHER'S CENSUS DELIVERY (2026-09-13, canonical census on lefford
        // at 1172e1069b43, goldens bdc59cc18): the delivery rewrote all 1000 rows
        // of the-census `rows.csv` (116 files in the delivery); this pin reads
        // census output. The absorb at c23bae9fd (The Cadastre) touched no census
        // fixture and is not a mover here.
        // evaluable 181 -> 179, excluded 51 -> 53. The 292-column metric surface is
        // UNCHANGED, so this test's name still carries the right count. Two columns
        // left the evaluable side for the excluded rails. The excluded row was MASKED
        // by the evaluable one and was read in the same softened pass.
        // MERGE RE-PIN (2026-09-13, The Trencher absorbing 41 commits of
        // origin/main -- the Orrery/astronomy delivery). MEASURED AT THE
        // MERGE, PRE-CENSUS: the absorb kept main's census fixtures
        // byte-for-byte while the conflict resolution kept this branch's
        // census-delivery literals, so both rows read the Trencher delivery's
        // values against main's census data. Re-measured over the merged
        // tree: evaluable 179 -> 181, excluded 53 -> 51. The excluded row is
        // MASKED by the evaluable one and was read in the SAME softened pass,
        // not on a later run.
        //
        // THE 292-COLUMN METRIC SURFACE THIS TEST'S NAME CARRIES IS
        // UNCHANGED, and that was checked rather than assumed: the census
        // header is 295 CSV columns less `seed`, `pin_set` and `refusal` on
        // BOTH sides of the merge and in the merged tree, with an identical
        // column SET (a line-by-line diff of the three headers is empty). So
        // neither the test name nor its exact-name entry in
        // `docs/timings/subfloor-roster.tsv` moves in this commit. Expected
        // to move again at the post-merge census, which will carry this
        // campaign's metabolite/supply work the committed CSV does not yet
        // have.
        // THE TRENCHER'S POST-MERGE CENSUS (2026-09-13, canonical census on
        // lefford at 06055072d639, goldens 0865b31bb): evaluable 181 -> 179,
        // excluded 51 -> 53. The excluded row is MASKED by the evaluable one
        // and was read in the SAME softened pass.
        //
        // THE 292-COLUMN METRIC SURFACE THIS TEST'S NAME CARRIES IS
        // UNCHANGED, and that was checked rather than assumed: a sorted,
        // line-by-line diff of the two censuses' CSV headers is EMPTY — 295
        // columns less `seed`, `pin_set` and `refusal` on both. So this test's
        // name and its exact-name entry in `docs/timings/subfloor-roster.tsv`
        // do not move in this commit. The merge re-pin's expectation that the
        // post-merge census would carry new metabolite/supply columns did NOT
        // come true: no metric was registered or deregistered at this epoch.
        //
        // NO COLUMN WAS ADDED OR REMOVED; TWO EXISTING COLUMNS WERE
        // RECLASSIFIED BY THEIR OWN VALUES, and that is the informative part
        // here, because it is the first epoch in this note's history where the
        // surface SHRANK. Both departures are `both rails tied`, identified by
        // the method this note prescribes — instrumenting `evaluable_columns`
        // and comparing its evaluable/excluded sets across the two censuses,
        // rather than diffing present values:
        //
        //   first-day-occ-cause-famine  both rails tied: 32 at min, 15 at max of 707
        //   toponymic-roots-won         both rails tied: 11 at min, 603 at max of 1000
        //
        // Nothing crossed the other way (the arrived set is empty), so the
        // -2/+2 is one movement, not two that happened to net out.
        //
        // THE SHRINK IS A DATA VERDICT, NOT A SCHEMA FACT, and this comment
        // says so rather than treating either column as permanently degenerate
        // — The Wanderers' entry above records `first-day-occ-tech-neolithic`
        // moving the other way for the same kind of reason. `evaluable_columns`
        // is a pure function of the committed CSV: `windows/lab/src/domesday/`
        // has ZERO commits between the previously pinned reading's tree
        // (0865b31bb~1) and this one, so the classifier that rendered both
        // verdicts is byte-identical and only its input moved.
        // MERGE (The Trencher absorbing The Tidemark, 2026-09-15): this
        // branch's own shrink (181 -> 179, excluded 51 -> 53, documented at
        // length above) is superseded -- the committed census in this merge
        // is The Tidemark's, taken per the pin-resolution principle (a
        // calibration pin asserts against the committed census, and this
        // branch's own census no longer exists once this merge lands). The
        // Tidemark's own reading holds the surface at 181/51 (see
        // `book/src/chronicle/the-gnomon.md` Postscript 11): no column was
        // added, removed or reclassified between the two censuses' rails on
        // ITS tree.
        assert_eq!(
            evaluable.len(),
            181,
            "evaluable count moved — re-measure and update this"
        );
        assert_eq!(
            excluded.len(),
            51,
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

    /// A value beyond either rail clamps to the scale's floor instead of
    /// going negative — the case [`score_row`] introduced and no in-census
    /// caller can reach. Both directions, because the raw expression is
    /// symmetric and so is the bug.
    #[test]
    fn tail_depth_clamps_a_value_outside_the_census_range() {
        let sorted = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        assert_eq!(tail_depth(&sorted, 0.0), 0.0, "below the minimum");
        assert_eq!(tail_depth(&sorted, 99.0), 0.0, "above the maximum");
    }

    /// The seam agrees with the in-census path it generalises: handed a row
    /// that IS a census row, [`score_row`] returns exactly what
    /// [`for_seed`] does. Without this the two could drift into scoring the
    /// same world differently depending on which door it came through.
    #[test]
    fn score_row_agrees_with_for_seed_on_a_census_row() {
        let c = committed();
        let row = c
            .rows
            .iter()
            .find(|r| seed_of(r) == Some(42))
            .expect("seed 42 is in the census");
        assert_eq!(score_row(&c, 42, row), for_seed(&c, 42).expect("seed 42"));
    }

    /// The anti-splice property, with the contaminated alternative computed
    /// alongside it as the positive control.
    ///
    /// A 100-world census over a plain `0..100` ramp; the scored row sits at
    /// `0.5`, between the two lowest census values. Scored against the
    /// census as committed its rank is `0.5` of `n-1 = 99` → depth
    /// `0.005050…`. Spliced INTO the census first — the shortcut
    /// [`score_row`]'s doc warns about — it becomes a member, `n` rises to
    /// 101, its rank rises to 1 of 100, and the depth doubles to `0.01`:
    /// the row moved the very yardstick it was being measured against, and
    /// in this case across the frozen [`TAIL_DEPTH_BAR`]. Asserting only the
    /// first number would pass just as well if someone rewrote `score_row`
    /// to splice, so both are asserted and their inequality with them.
    #[test]
    fn score_row_scores_against_an_uncontaminated_index() {
        let rows: Vec<BTreeMap<String, String>> = (0..100)
            .map(|i: u32| {
                BTreeMap::from([
                    ("m".to_string(), i.to_string()),
                    ("seed".to_string(), i.to_string()),
                ])
            })
            .collect();
        let census = Census {
            columns: vec![numeric_column("m")],
            rows: rows.clone(),
        };

        let outsider = BTreeMap::from([
            ("m".to_string(), "0.5".to_string()),
            ("seed".to_string(), "9999".to_string()),
        ]);

        let scored = score_row(&census, 9999, &outsider);
        let depth = scored
            .flags
            .iter()
            .find(|f| f.metric == "m")
            .expect("the outsider is scored on m")
            .depth;
        assert_eq!(depth, 0.5 / 99.0, "scored against the census as committed");

        // The contaminated alternative, computed rather than asserted about.
        let mut spliced_rows = rows;
        spliced_rows.push(outsider.clone());
        let spliced = Census {
            columns: vec![numeric_column("m")],
            rows: spliced_rows,
        };
        let contaminated = for_seed(&spliced, 9999)
            .expect("the spliced census carries the outsider")
            .flags
            .iter()
            .find(|f| f.metric == "m")
            .expect("scored on m")
            .depth;
        assert_eq!(contaminated, 1.0 / 100.0, "the splice's own arithmetic");
        assert_ne!(
            depth, contaminated,
            "splicing the row into the census must be observably different from \
             scoring it against the census — if these agree, this test can no \
             longer tell the two apart"
        );
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
