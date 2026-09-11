//! Public-contract checks for the structured eclipse recurrence API.

use hornvale_astronomy::calendar_of;
use hornvale_astronomy::{EclipseBody, MoonsPin, SkyPins, eclipse_recurrences, generate};
use hornvale_kernel::Seed;

/// A loop that groups by eclipse family before moon would break the stable
/// distance-sorted moon order consumed by scene and almanac adapters.
#[test]
fn recurrence_records_are_moon_ordered_with_solar_before_lunar() {
    let pins = SkyPins {
        moons: Some(MoonsPin::exact(2).unwrap()),
        ..SkyPins::default()
    };
    let system = generate(Seed(42), &pins).unwrap().value;
    let calendar = calendar_of(&system);

    let records = eclipse_recurrences(&system, &calendar);
    let order: Vec<_> = records
        .iter()
        .map(|record| (record.moon, record.body))
        .collect();

    assert_eq!(
        order,
        vec![
            (0, EclipseBody::Solar),
            (0, EclipseBody::Lunar),
            (1, EclipseBody::Solar),
            (1, EclipseBody::Lunar),
        ]
    );
}
