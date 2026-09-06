//! The Hidage — The Staple's D1 Task 0 probe. Measurement only.
//!
//! Does growing a community toward its CATCHMENT's summed capacity, instead of
//! its own vertex's, make anything other than a hamlet — and only some? The
//! spec (`docs/superpowers/specs/2026-09-06-the-hidage-design.md`) freezes the
//! field (§3.1: the bake's present-era growth field), the catchment (§3.2:
//! `hornvale_demography::flow`, unchanged), the populations (§3.3) and the
//! decision rule (§4) before this file was written. The probe commits nothing.
//!
//! The statistics below are the instrument, unit-tested on constructed
//! vectors (H1) and on a constructed field (H2), so that "every seed agrees"
//! can be read as a finding rather than a vacuous check.

use hornvale_demography::{Flow, flow};
use hornvale_kernel::{Geosphere, Vertex, VertexMap};

/// Gini coefficient — mean absolute difference over twice the mean. `0.0` for
/// a constant, empty or all-zero vector.
fn gini(xs: &[f64]) -> f64 {
    let n = xs.len();
    if n == 0 {
        return 0.0;
    }
    let mean = xs.iter().sum::<f64>() / n as f64;
    if mean <= 0.0 {
        return 0.0;
    }
    let mut sum = 0.0;
    for a in xs {
        for b in xs {
            sum += (a - b).abs();
        }
    }
    sum / (2.0 * (n * n) as f64 * mean)
}

/// Average ranks, 1-based; ties share the mean of the ranks they span.
fn ranks(xs: &[f64]) -> Vec<f64> {
    let mut idx: Vec<usize> = (0..xs.len()).collect();
    idx.sort_by(|&a, &b| xs[a].total_cmp(&xs[b]).then(a.cmp(&b)));
    let mut out = vec![0.0; xs.len()];
    let mut i = 0;
    while i < idx.len() {
        let mut j = i;
        while j + 1 < idx.len() && xs[idx[j + 1]] == xs[idx[i]] {
            j += 1;
        }
        // Ranks are 1-based: positions i..=j share the mean rank.
        let avg = (i + j) as f64 / 2.0 + 1.0;
        for &pos in &idx[i..=j] {
            out[pos] = avg;
        }
        i = j + 1;
    }
    out
}

/// Spearman rank correlation: Pearson over average ranks. `0.0` when either
/// side is constant (no ranking to agree with).
fn spearman(xs: &[f64], ys: &[f64]) -> f64 {
    assert_eq!(xs.len(), ys.len(), "spearman needs paired vectors");
    let (rx, ry) = (ranks(xs), ranks(ys));
    let n = rx.len() as f64;
    if n == 0.0 {
        return 0.0;
    }
    let mx = rx.iter().sum::<f64>() / n;
    let my = ry.iter().sum::<f64>() / n;
    let (mut sxy, mut sxx, mut syy) = (0.0, 0.0, 0.0);
    for (x, y) in rx.iter().zip(&ry) {
        sxy += (x - mx) * (y - my);
        sxx += (x - mx) * (x - mx);
        syy += (y - my) * (y - my);
    }
    if sxx == 0.0 || syy == 0.0 {
        return 0.0;
    }
    sxy / (sxx * syy).sqrt()
}

/// How many entries clear `bar` (inclusive).
fn count_at_or_above(xs: &[f64], bar: f64) -> usize {
    xs.iter().filter(|x| **x >= bar).count()
}

/// Median of a copy sorted by `total_cmp`; `0.0` for an empty slice.
fn median(xs: &[f64]) -> f64 {
    if xs.is_empty() {
        return 0.0;
    }
    let mut v = xs.to_vec();
    v.sort_by(|a, b| a.total_cmp(b));
    let n = v.len();
    if n % 2 == 1 {
        v[n / 2]
    } else {
        (v[n / 2 - 1] + v[n / 2]) / 2.0
    }
}

/// Smallest entry, or `0.0` for an empty slice (a printing helper).
fn min_or_zero(xs: &[f64]) -> f64 {
    if xs.is_empty() {
        0.0
    } else {
        xs.iter().copied().fold(f64::INFINITY, f64::min)
    }
}

/// Spec §4's four branches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Verdict {
    NoCity,
    Rescale,
    Lives,
    Mixed,
}

/// Spec §4, verbatim. `per_seed` holds `(c_s, N_s)` — the number of top-N
/// attractors whose accumulation clears the hamlet ceiling, and the seed's
/// alive settlement count. Every clause is "on every seed".
fn verdict(per_seed: &[(usize, usize)]) -> Verdict {
    assert!(!per_seed.is_empty(), "a verdict needs at least one seed");
    let share = |c: usize, n: usize| if n == 0 { 0.0 } else { c as f64 / n as f64 };
    if per_seed.iter().all(|&(c, _)| c == 0) {
        return Verdict::NoCity;
    }
    if per_seed.iter().all(|&(c, n)| share(c, n) > 0.5) {
        return Verdict::Rescale;
    }
    if per_seed.iter().all(|&(c, n)| c >= 1 && share(c, n) <= 0.25) {
        return Verdict::Lives;
    }
    Verdict::Mixed
}

/// The top-`n` attractors of a flow field by accumulation, descending, ties to
/// the lower vertex id (spec §3.3, P2). Fewer than `n` if the field has fewer.
fn top_attractors(geo: &Geosphere, f: &Flow, n: usize) -> Vec<Vertex> {
    let mut a: Vec<(Vertex, f64)> = geo
        .vertices()
        .filter(|&v| f.attractor.get(v).is_some_and(|x| x == v))
        .map(|v| (v, *f.accumulation.get(v)))
        .collect();
    a.sort_by(|x, y| y.1.total_cmp(&x.1).then(x.0.0.cmp(&y.0.0)));
    a.truncate(n);
    a.into_iter().map(|(v, _)| v).collect()
}

#[test]
fn gini_is_zero_for_a_constant_vector_and_near_one_for_a_one_hot() {
    assert_eq!(gini(&[3.0, 3.0, 3.0, 3.0]), 0.0);
    assert_eq!(gini(&[]), 0.0);
    // One-hot of length n has Gini (n-1)/n.
    let mut one_hot = vec![0.0; 100];
    one_hot[7] = 5.0;
    assert!((gini(&one_hot) - 0.99).abs() < 1e-12, "{}", gini(&one_hot));
    // Scale-free.
    assert!((gini(&[1.0, 2.0, 4.0]) - gini(&[10.0, 20.0, 40.0])).abs() < 1e-12);
}

#[test]
fn spearman_is_one_on_agreement_minus_one_on_reversal_and_zero_on_a_constant() {
    let a = [1.0, 5.0, 2.0, 9.0, 3.0];
    let up = [10.0, 50.0, 20.0, 90.0, 30.0];
    let down = [90.0, 30.0, 70.0, 10.0, 60.0];
    assert!((spearman(&a, &up) - 1.0).abs() < 1e-12);
    assert!((spearman(&a, &down) + 1.0).abs() < 1e-12);
    assert_eq!(spearman(&a, &[4.0; 5]), 0.0);
    // Ties take the average rank: (1,1,3) ranks as (1.5,1.5,3).
    assert_eq!(ranks(&[1.0, 1.0, 3.0]), vec![1.5, 1.5, 3.0]);
}

#[test]
fn count_against_a_bar_returns_the_straddle_inclusively() {
    let xs = [10.0, 150.0, 149.999, 300.0];
    assert_eq!(count_at_or_above(&xs, 150.0), 2);
    assert_eq!(count_at_or_above(&xs, 1000.0), 0);
    assert_eq!(count_at_or_above(&xs, 0.0), 4);
    assert_eq!(median(&[3.0, 1.0, 2.0]), 2.0);
    assert_eq!(min_or_zero(&[]), 0.0);
    assert_eq!(min_or_zero(&[3.0, 1.0]), 1.0);
}

#[test]
fn the_verdict_rule_is_spec_section_4_on_every_seed() {
    // NO CITY: c == 0 everywhere.
    assert_eq!(verdict(&[(0, 200), (0, 180), (0, 220)]), Verdict::NoCity);
    // RESCALE: c/N > 0.5 everywhere.
    assert_eq!(
        verdict(&[(120, 200), (100, 180), (200, 220)]),
        Verdict::Rescale
    );
    // LIVES: 1 <= c and c/N <= 0.25 everywhere (0.25 inclusive).
    assert_eq!(verdict(&[(1, 200), (45, 180), (55, 220)]), Verdict::Lives);
    // MIXED: one seed at zero, the rest alive.
    assert_eq!(verdict(&[(0, 200), (10, 180), (12, 220)]), Verdict::Mixed);
    // MIXED: the 0.25..=0.5 gap.
    assert_eq!(verdict(&[(80, 200), (70, 180), (90, 220)]), Verdict::Mixed);
    // MIXED: a majority on one seed, a minority on another.
    assert_eq!(verdict(&[(150, 200), (10, 180), (12, 220)]), Verdict::Mixed);
}

/// H2 — the pipeline (field -> flow -> top-N -> count -> verdict) on a
/// constructed field: four sharp bumps of unequal height on a coarse
/// geosphere, so exactly one basin dominates. The property this asserts is
/// "of the four attractors, exactly one clears a bar set between the largest
/// and second-largest accumulation" — if the construction yields a different
/// attractor count, change the CONSTRUCTION (peak spacing, sharpness), never
/// the assertion.
#[test]
fn a_constructed_field_with_one_dominant_basin_reads_lives_no_city_and_rescale() {
    let geo = Geosphere::new(2);
    let peaks = [Vertex(0), Vertex(2), Vertex(4), Vertex(8)];
    let heights = [10.0, 1.0, 1.0, 1.0];
    let k = VertexMap::from_fn(&geo, |c| {
        let p = geo.position(c);
        peaks
            .iter()
            .zip(heights)
            .map(|(&pk, h)| {
                let q = geo.position(pk);
                let dot = (p[0] * q[0] + p[1] * q[1] + p[2] * q[2]).max(0.0);
                h * dot.powi(8)
            })
            .sum::<f64>()
    });
    let f = flow(&geo, &k);
    let attractors: Vec<Vertex> = geo
        .vertices()
        .filter(|&v| f.attractor.get(v).is_some_and(|a| a == v))
        .collect();
    assert_eq!(
        attractors.len(),
        4,
        "the construction must yield exactly four basins; got {attractors:?} — move the peaks apart or sharpen the bumps"
    );
    let top = top_attractors(&geo, &f, 4);
    let acc: Vec<f64> = top.iter().map(|&v| *f.accumulation.get(v)).collect();
    assert!(acc[0] > acc[1], "descending: {acc:?}");
    let between = (acc[0] + acc[1]) / 2.0;
    let c = count_at_or_above(&acc, between);
    assert_eq!(c, 1);
    let five_seeds = [(c, 4); 5];
    assert_eq!(verdict(&five_seeds), Verdict::Lives);
    let above_all = count_at_or_above(&acc, acc[0] * 2.0);
    assert_eq!(verdict(&[(above_all, 4); 5]), Verdict::NoCity);
    let below_all = count_at_or_above(&acc, 0.0);
    assert_eq!(verdict(&[(below_all, 4); 5]), Verdict::Rescale);
    // Conservation, so the field is the one condense.rs's own tests describe.
    let total_k: f64 = geo.vertices().map(|c| *k.get(c)).sum();
    let total_sink: f64 = attractors.iter().map(|&a| *f.accumulation.get(a)).sum();
    assert!((total_k - total_sink).abs() < 1e-9);
}
