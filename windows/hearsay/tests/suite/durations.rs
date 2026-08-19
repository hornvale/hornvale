//! `PeopleDurations` is plain data: it never looks anything up.

use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::durations::PeopleDurations;

#[test]
fn a_people_with_no_entry_reports_neither_duration() {
    let d = PeopleDurations::default();
    assert_eq!(d.get("gnoll"), (None, None));
}

#[test]
fn inserted_durations_come_back_out() {
    let mut d = PeopleDurations::default();
    let generation = StdDays::new(11362.0).expect("positive");
    let life = StdDays::new(25399.0).expect("positive");
    d.insert("human", Some(generation), Some(life));
    let (g, l) = d.get("human");
    assert_eq!(g.map(|days| days.get()), Some(11362.0));
    assert_eq!(l.map(|days| days.get()), Some(25399.0));
}

#[test]
fn a_people_may_carry_a_generation_but_no_lifespan() {
    let mut d = PeopleDurations::default();
    d.insert(
        "construct",
        Some(StdDays::new(100.0).expect("positive")),
        None,
    );
    let (g, l) = d.get("construct");
    assert!(g.is_some());
    assert!(l.is_none());
}
