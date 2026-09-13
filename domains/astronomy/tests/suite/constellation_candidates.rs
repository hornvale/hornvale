//! The physical side of the culture seam: astronomy exposes stable,
//! observer-filtered candidates without naming or grouping them.

use hornvale_astronomy::night_sky::{StarObserver, catalog_stars_at};
use hornvale_astronomy::{SkyPins, calendar_of, generate};
use hornvale_kernel::Seed;

#[test]
fn catalog_candidates_are_stable_physical_records() {
    let system = generate(Seed(42), &SkyPins::default()).unwrap().value;
    let calendar = calendar_of(&system);
    let candidates = catalog_stars_at(
        &system,
        &calendar,
        hornvale_astronomy::StdInstant::new(0.0).unwrap(),
        &StarObserver::default(),
    );

    assert!(!candidates.is_empty());
    assert!(candidates.windows(2).all(|pair| pair[0].id < pair[1].id));
    assert!(candidates.iter().all(|candidate| {
        candidate.position.ra_deg.is_finite()
            && candidate.position.dec_deg.is_finite()
            && candidate.apparent_magnitude.is_finite()
            && candidate.apparent_magnitude <= 6.0
    }));
}

#[test]
fn physical_candidate_query_can_legitimately_be_empty() {
    let system = generate(Seed(42), &SkyPins::default()).unwrap().value;
    let calendar = calendar_of(&system);
    let none = StarObserver {
        limiting_magnitude: -100.0,
        ..StarObserver::default()
    };
    assert!(
        catalog_stars_at(
            &system,
            &calendar,
            hornvale_astronomy::StdInstant::new(0.0).unwrap(),
            &none,
        )
        .is_empty()
    );
}
