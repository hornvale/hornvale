//! Public-contract checks for the structured eclipse recurrence API.

use hornvale_astronomy::calendar_of;
use hornvale_astronomy::{
    Au, EclipseBody, MoonsPin, SkyPins, StdInstant, anchor_state_at, eclipse_events,
    eclipse_recurrences, generate, ground_track, moon_ecliptic_longitude_deg, node_longitude_at,
    solar_eclipse_threshold_deg, sub_solar_longitude_deg, sun_angular_rel_at,
};
use hornvale_kernel::{Seed, math};

fn eccentric_single_moon_system() -> hornvale_astronomy::StarSystem {
    let pins = SkyPins {
        moons: Some(MoonsPin::exact(1).unwrap()),
        ..SkyPins::default()
    };
    let mut system = hornvale_astronomy::generate(Seed(42), &pins).unwrap().value;
    system.forcing.ecc_mean = 0.2;
    system.forcing.ecc_amp = 0.0;
    system
}

fn near(left: f64, right: f64) {
    assert!((left - right).abs() < 1e-10, "{left} != {right}");
}

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

/// Reintroducing the legacy mean-phase radius or longitude would detach eclipse
/// alignment from the physical anchor state, especially before genesis.
#[test]
fn eclipse_anchor_coherence_uses_physical_radius_and_longitude_at_negative_time() {
    let system = eccentric_single_moon_system();
    let calendar = calendar_of(&system);
    let instant = StdInstant::new(-0.375 * system.anchor.year.get()).unwrap();
    let state = anchor_state_at(&system, instant).unwrap();

    let expected_sun = hornvale_astronomy::star::sun_angular_diameter_rel(
        &system.star,
        Au::new(state.radius_au).unwrap(),
    );
    near(
        sun_angular_rel_at(&system, &calendar, instant),
        expected_sun,
    );

    let phase = calendar.moon_phase(instant, 0).unwrap();
    let expected_longitude = (360.0 * state.true_longitude_turns + 360.0 * phase).rem_euclid(360.0);
    near(
        moon_ecliptic_longitude_deg(&calendar, 0, instant).unwrap(),
        expected_longitude,
    );
}

/// Ground-track latitude, duration and sweep must be projections of the same
/// event-time anchor state rather than a mixture of physical and legacy clocks.
#[test]
fn eclipse_anchor_coherence_ground_track_uses_event_state_geometry() {
    let system = eccentric_single_moon_system();
    let calendar = calendar_of(&system);
    let year = system.anchor.year.get();
    let event = eclipse_events(
        &system,
        &calendar,
        StdInstant::new(-5.0 * year).unwrap(),
        StdInstant::new(0.0).unwrap(),
    )
    .into_iter()
    .find(|event| event.body == EclipseBody::Solar)
    .expect("a one-moon system has a solar eclipse in five years");
    let track = ground_track(&system, &calendar, &event).unwrap();
    let state = anchor_state_at(&system, event.day).unwrap();
    let moon = &system.moons[event.moon];
    let phase = calendar.moon_phase(event.day, event.moon).unwrap();
    let moon_longitude = (360.0 * state.true_longitude_turns + 360.0 * phase).rem_euclid(360.0);
    let node = node_longitude_at(moon, system.anchor.year, event.day);
    let beta = math::asin(
        (math::sin(moon.inclination_deg.to_radians())
            * math::sin((moon_longitude - node).to_radians()))
        .clamp(-1.0, 1.0),
    )
    .to_degrees();
    let sun_angular = hornvale_astronomy::star::sun_angular_diameter_rel(
        &system.star,
        Au::new(state.radius_au).unwrap(),
    );
    let threshold = solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
    let declination = calendar.solar_declination(event.day);
    let expected_center =
        (declination + (beta / threshold) * (90.0 - declination.abs())).clamp(-90.0, 90.0);
    let synodic = calendar.synodic_month(event.moon).unwrap().get();
    let expected_duration = hornvale_astronomy::eclipses::ANGULAR_UNIT_DEG
        * (sun_angular + moon.angular_diameter_rel)
        / (360.0 / synodic);
    let expected_start = sub_solar_longitude_deg(
        &calendar,
        StdInstant::new(event.day.get() - expected_duration / 2.0).unwrap(),
    );
    let expected_sweep = calendar.day_length().map_or(0.0, |day| {
        let direction = if calendar.is_retrograde() { 1.0 } else { -1.0 };
        direction * expected_duration / day.get() * 360.0
    });
    let expected_end = (expected_start + expected_sweep + 180.0).rem_euclid(360.0) - 180.0;

    near(track.center_lat_deg, expected_center);
    near(track.duration_days, expected_duration);
    near(track.start_lon_deg, expected_start);
    near(track.sweep_deg, expected_sweep);
    near(track.end_lon_deg, expected_end);
}

/// Absence of a syzygy in an otherwise valid interval is an empty event set,
/// not an orbital evaluation failure.
#[test]
fn eclipse_anchor_coherence_valid_quarter_phase_has_no_event() {
    let system = eccentric_single_moon_system();
    let calendar = calendar_of(&system);
    let synodic = calendar.synodic_month(0).unwrap().get();
    let phase_at_genesis = calendar
        .moon_phase(StdInstant::new(0.0).unwrap(), 0)
        .unwrap();
    let quarter_phase = StdInstant::new((0.25 - phase_at_genesis) * synodic).unwrap();
    let margin = 0.01 * synodic;

    assert!(
        eclipse_events(
            &system,
            &calendar,
            StdInstant::new(quarter_phase.get() - margin).unwrap(),
            StdInstant::new(quarter_phase.get() + margin).unwrap(),
        )
        .is_empty()
    );
}
