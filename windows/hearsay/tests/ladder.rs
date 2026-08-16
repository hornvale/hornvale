//! The per-world ladder, over hand-built ledgers.

mod common;

use common::put_on;
use hornvale_hearsay::ladder::PrecisionLadder;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{Ledger, Value};

fn sky(day: Option<f64>, moons: &[f64], year: Option<f64>) -> Ledger {
    let mut led = Ledger::default();
    if let Some(d) = day {
        put_on(
            &mut led,
            1,
            hornvale_astronomy::facts::DAY_LENGTH_STD,
            Value::Number(d),
        );
    }
    for m in moons {
        put_on(
            &mut led,
            1,
            hornvale_astronomy::facts::MOON_PERIOD_STD,
            Value::Number(*m),
        );
    }
    if let Some(y) = year {
        put_on(
            &mut led,
            1,
            hornvale_astronomy::facts::YEAR_LENGTH_STD,
            Value::Number(y),
        );
    }
    led
}

#[test]
fn both_moons_of_a_two_mooned_world_become_rungs() {
    // If this reports 3, someone reached for value_of and lost a moon.
    let l = PrecisionLadder::of(&sky(Some(1.0), &[29.3, 41.7], Some(372.4)));
    assert_eq!(l.len(), 4, "day, two moons, year: {l:?}");
}

#[test]
fn rungs_sort_by_actual_span_so_the_order_is_world_derived() {
    let l = PrecisionLadder::of(&sky(Some(1.0), &[41.7], Some(372.4)));
    assert_eq!(l.label(Precision(0)), Some("day"));
    assert_eq!(l.label(Precision(2)), Some("year"));
}

#[test]
fn a_moonless_world_simply_has_no_lunar_rung() {
    let l = PrecisionLadder::of(&sky(Some(1.0), &[], Some(300.0)));
    assert_eq!(l.len(), 2);
    assert_eq!(
        l.coarser(Precision(1)),
        Precision(1),
        "saturates at the year"
    );
}

#[test]
fn an_empty_ladder_loses_no_precision() {
    let l = PrecisionLadder::of(&Ledger::default());
    assert!(l.is_empty());
    assert_eq!(l.apply(Precision::FINEST, 3661.75), 3661.75);
}

#[test]
fn two_moons_of_equal_period_contribute_one_rung() {
    let l = PrecisionLadder::of(&sky(Some(1.0), &[30.0, 30.0], Some(300.0)));
    assert_eq!(l.len(), 3);
}

#[test]
fn re_rounding_an_already_rounded_day_can_exclude_the_event() {
    // The consequence of NOT nesting. Do not repair a failure here by
    // snapping the rungs -- see spec section 5.2.
    let l = PrecisionLadder::of(&sky(Some(1.0), &[41.7], Some(372.4)));
    let year = Precision(2);
    let truth = 745.0;
    let width = l.span(year).expect("year rung").get();
    let twice = l.apply(year, l.apply(Precision(1), truth));
    assert!(
        truth < twice || truth >= twice + width,
        "re-rounding must be able to exclude the event: {truth} still in [{twice}, {})",
        twice + width
    );
    let direct = l.apply(year, truth);
    assert!(
        truth >= direct && truth < direct + width,
        "control: rounding the truth once must still contain it"
    );
}
