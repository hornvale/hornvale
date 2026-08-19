//! The per-world ladder, over hand-built ledgers.

use crate::common;

use common::put_on;
use hornvale_astronomy::units::StdDays;
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

#[test]
fn social_rungs_append_above_the_astronomical_ones() {
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    // NOT `let gen = ...` — `gen` is a RESERVED KEYWORD in Rust edition 2024
    // and will not compile. Task 1 hit this in the plan's own sample code.
    let generation = StdDays::new(11362.0).expect("positive");
    let life = StdDays::new(25399.0).expect("positive");
    let l = PrecisionLadder::with_social(&led, Some(generation), Some(life));
    assert_eq!(
        l.labels(),
        vec!["day", "moon 1", "year", "generation", "lifespan"]
    );
}

#[test]
fn a_people_with_no_life_history_gets_the_astronomical_ladder_only() {
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let bare = PrecisionLadder::of(&led);
    let l = PrecisionLadder::with_social(&led, None, None);
    assert_eq!(l.labels(), bare.labels());
}

#[test]
fn social_rungs_sort_by_span_like_every_other_rung() {
    // A generation SHORTER than the year must sort below it. Nothing about
    // a rung's origin gives it a fixed position.
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let generation = StdDays::new(100.0).expect("positive");
    let l = PrecisionLadder::with_social(&led, Some(generation), None);
    assert_eq!(l.labels(), vec!["day", "moon 1", "generation", "year"]);
}

#[test]
fn an_unknown_people_falls_back_to_the_astronomical_ladder() {
    let led = sky(Some(1.0), &[41.7], Some(372.4));
    let mut d = hornvale_hearsay::durations::PeopleDurations::default();
    d.insert(
        "human",
        Some(StdDays::new(11362.0).expect("positive")),
        None,
    );
    let ls = hornvale_hearsay::ladder::PeopleLadders::of(&led, &d);
    assert_eq!(
        ls.for_people("gnoll").labels(),
        PrecisionLadder::of(&led).labels()
    );
    assert!(ls.for_people("human").labels().contains(&"generation"));
}
