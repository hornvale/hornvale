//! The early degeneracy probe (Task 1, Step 4 — the Cupel discipline): for
//! each of the eight axes, print the distribution of its pairwise
//! distances across the full 15×15 ordered-pair grid of real peoples (min,
//! max, mean, sd, distinct-value count), and assert only anti-degeneracy —
//! a **majority** of the eight axes must show spread (sd > 0). One or two
//! flat axes are tolerated and expected (Habitat: only drow is
//! Subterranean among the fifteen peoples, so it contributes only two
//! distinct values and a small sd); read the printed table for the full
//! landscape, and record any degenerate axis as a finding rather than
//! silently dropping it.
//!
//! Run with `cargo test -p hornvale-sentiment --test suite -- \
//! axis_spread_probe --nocapture` to see the table.

use hornvale_sentiment::{Axis, PeopleId, axis_distance, catalog};

#[test]
fn axis_spread_probe() {
    let cat = catalog();
    let ids: Vec<PeopleId> = cat.keys().copied().collect();
    assert_eq!(
        ids.len(),
        25,
        "the catalog must hold exactly the minded, social peoples — \
         `society_registry` is `minded ∧ social` (decision 0068), which was \
         extensionally `Settled` until The Tidemark's Gregarious merfolk"
    );

    println!(
        "{:<16} {:>10} {:>10} {:>10} {:>12} {:>10}",
        "axis", "min", "max", "mean", "sd", "distinct"
    );

    let mut varying = 0usize;
    let mut report = Vec::with_capacity(Axis::ALL.len());
    for axis in Axis::ALL {
        let mut values = Vec::with_capacity(ids.len() * ids.len());
        for &a in &ids {
            for &b in &ids {
                values.push(axis_distance(axis, &cat[&a], &cat[&b]));
            }
        }

        let min = values.iter().copied().fold(f64::INFINITY, f64::min);
        let max = values.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let variance = values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / values.len() as f64;
        let sd = variance.sqrt();

        let mut distinct_bits: Vec<u64> = values.iter().map(|v| v.to_bits()).collect();
        distinct_bits.sort_unstable();
        distinct_bits.dedup();
        let distinct = distinct_bits.len();

        println!(
            "{:<16} {:>10.6} {:>10.6} {:>10.6} {:>12.8} {:>10}",
            axis.label(),
            min,
            max,
            mean,
            sd,
            distinct
        );

        if sd > 0.0 {
            varying += 1;
        }
        report.push((axis, sd, distinct));
    }

    let flat: Vec<&str> = report
        .iter()
        .filter(|(_, sd, _)| *sd <= 0.0)
        .map(|(axis, _, _)| axis.label())
        .collect();
    if !flat.is_empty() {
        println!("flat (sd == 0) axes: {flat:?} — see the task report for the finding");
    }

    assert!(
        varying * 2 > Axis::ALL.len(),
        "a MAJORITY of the eight axes must show spread (sd > 0) across the real \
         fifteen-people roster; only {varying}/{} did. A near-degenerate axis \
         (Habitat, where only drow is Subterranean) is expected and tolerated, but \
         most axes going flat would mean the distance functions are not \
         discriminating anything: {report:?}",
        Axis::ALL.len()
    );
}
