//! Three co-equal accumulation rules, and emit-time rung resolution.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::{Accumulation, precision_at};
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::Ledger;

#[test]
fn every_rule_leaves_a_zero_span_step_unchanged() {
    for rule in Accumulation::ALL {
        assert_eq!(rule.step(5.0, 0.0), 5.0, "{}", rule.label());
    }
}

#[test]
fn every_rule_is_non_decreasing() {
    for rule in Accumulation::ALL {
        let after = rule.step(5.0, 2.0);
        assert!(after >= 5.0, "{} decreased: {after}", rule.label());
    }
}

#[test]
fn the_three_rules_are_actually_different() {
    let (w, s) = (3.0, 4.0);
    let vals: Vec<f64> = Accumulation::ALL.iter().map(|r| r.step(w, s)).collect();
    assert_eq!(vals[0], 7.0); // additive
    assert_eq!(vals[1], 5.0); // quadrature: sqrt(9+16)
    assert_eq!(vals[2], 15.0); // multiplicative: 3 * (1+4)
}

#[test]
fn an_empty_ladder_always_reports_the_finest_precision() {
    let l = PrecisionLadder::of(&Ledger::default());
    assert_eq!(precision_at(&l, 999.0), Precision::FINEST);
}

#[test]
fn precision_is_the_coarsest_rung_the_width_reaches() {
    let l = PrecisionLadder::with_social(
        &Ledger::default(),
        Some(StdDays::new(100.0).expect("positive")),
        Some(StdDays::new(1000.0).expect("positive")),
    );
    // Ladder is [generation(100), lifespan(1000)]. A width lands on the
    // COARSEST rung whose span it reaches, so 150 is still the generation
    // rung — it has not reached 1000.
    assert_eq!(precision_at(&l, 0.0), Precision(0));
    assert_eq!(precision_at(&l, 99.0), Precision(0));
    assert_eq!(precision_at(&l, 100.0), Precision(0));
    assert_eq!(precision_at(&l, 150.0), Precision(0));
    assert_eq!(precision_at(&l, 1000.0), Precision(1));
    assert_eq!(precision_at(&l, 99999.0), Precision(1)); // saturates, never overruns
}
