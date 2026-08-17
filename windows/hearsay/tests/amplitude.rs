//! The amplitude is the generational span of ONE retelling.

mod common;

use common::eid;
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::amplitude::gen_span;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_kernel::ledger::{Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// A ledger where each entry is (occupation, people, founded-day).
fn founded(rows: &[(u64, &str, f64)]) -> Ledger {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_history::OCC_PEOPLE, true, "people")
        .expect("register");
    reg.register_predicate(hornvale_history::OCC_FOUNDED, true, "founded")
        .expect("register");
    let mut led = Ledger::default();
    for (occ, people, day) in rows {
        for (pred, object) in [
            (
                hornvale_history::OCC_PEOPLE,
                Value::Text(people.to_string()),
            ),
            (hornvale_history::OCC_FOUNDED, Value::Number(*day)),
        ] {
            led.commit(
                Fact {
                    subject: eid(*occ),
                    predicate: pred.to_string(),
                    object,
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &reg,
            )
            .expect("commit");
        }
    }
    led
}

fn durations(people: &str, generation_days: f64) -> PeopleDurations {
    let mut d = PeopleDurations::default();
    d.insert(
        people,
        Some(StdDays::new(generation_days).expect("positive")),
        None,
    );
    d
}

#[test]
fn one_generation_of_gap_is_an_amplitude_of_one() {
    let led = founded(&[(1, "human", 0.0), (2, "human", 100.0)]);
    let d = durations("human", 100.0);
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 1.0);
}

#[test]
fn the_same_gap_is_fewer_generations_for_a_longer_lived_people() {
    let led = founded(&[(1, "elf", 0.0), (2, "elf", 100.0)]);
    let d = durations("elf", 400.0);
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 0.25);
}

#[test]
fn the_amplitude_is_unsigned() {
    let led = founded(&[(1, "human", 100.0), (2, "human", 0.0)]);
    let d = durations("human", 100.0);
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 1.0);
}

#[test]
fn a_people_with_no_generation_length_yields_zero_rather_than_infinity() {
    let led = founded(&[(1, "construct", 0.0), (2, "construct", 100.0)]);
    let d = PeopleDurations::default();
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 0.0);
}

#[test]
fn the_span_uses_the_tellers_generation_length_not_the_hearers() {
    // An artificial ledger: fission never crosses a people boundary in a
    // real world (teller and hearer always share one), so the only way to
    // pin which side the lookup reads is to give them different peoples.
    let led = founded(&[(1, "human", 0.0), (2, "elf", 100.0)]);
    let mut d = PeopleDurations::default();
    d.insert("human", Some(StdDays::new(100.0).expect("positive")), None);
    d.insert("elf", Some(StdDays::new(400.0).expect("positive")), None);
    // The teller's ("human") generation gives 100.0/100.0 = 1.0; the
    // hearer's ("elf") would give 100.0/400.0 = 0.25.
    assert_eq!(gen_span(&led, &d, eid(1), eid(2)), 1.0);
}
