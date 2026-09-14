//! Cross-consumer qualification for the shared physical anchor state.

use hornvale_astronomy::{
    Au, MoonsPin, OrbitalError, RotationPin, SkyPins, SpinPin, StarSystem, StdDays, StdInstant,
    anchor_state_at, calendar_of, generate, moon_ecliptic_longitude_deg, stellar_illumination_at,
    sun_angular_rel_at,
};
use hornvale_kernel::{Seed, math};

const TOLERANCE: f64 = 1e-12;

#[derive(Debug, Clone, PartialEq)]
struct ConsumerReadout {
    instant: StdInstant,
    frame: &'static str,
    position_au: [f64; 2],
    radius_au: f64,
    true_longitude_turns: f64,
    calendar_longitude_turns: f64,
    primary_distance_au: f64,
    primary_flux_rel: f64,
    expected_primary_flux_rel: f64,
    eclipse_sun_angular_rel: f64,
    expected_sun_angular_rel: f64,
    eclipse_moon_longitude_deg: f64,
    expected_moon_longitude_deg: f64,
}

fn eccentric_system(rotation: RotationPin, spin: Option<SpinPin>) -> StarSystem {
    let pins = SkyPins {
        rotation: Some(rotation),
        spin,
        moons: Some(MoonsPin::exact(1).expect("one moon is valid")),
        ..SkyPins::default()
    };
    let mut system = generate(Seed(42), &pins).expect("seed 42 builds").value;
    system.forcing.ecc_mean = 0.2;
    system.forcing.ecc_amp = 0.0;
    system
}

fn readout(system: &StarSystem, instant: StdInstant) -> ConsumerReadout {
    let state = anchor_state_at(system, instant).expect("bounded probe is evaluable");
    let calendar = calendar_of(system);
    let illumination = stellar_illumination_at(system, instant);
    let primary = illumination.sources.first().expect("anchor has a primary");
    let phase = calendar
        .moon_phase(instant, 0)
        .expect("one-moon fixture has a synodic cycle");

    ConsumerReadout {
        instant: state.instant,
        frame: state.frame,
        position_au: state.position_au,
        radius_au: state.radius_au,
        true_longitude_turns: state.true_longitude_turns,
        calendar_longitude_turns: calendar
            .season_phase(instant)
            .expect("eccentric fixture has a season"),
        primary_distance_au: primary.distance.get(),
        primary_flux_rel: primary.flux_rel,
        expected_primary_flux_rel: hornvale_astronomy::luminosity_at(&system.star, instant).get()
            / (state.radius_au * state.radius_au),
        eclipse_sun_angular_rel: sun_angular_rel_at(system, &calendar, instant),
        expected_sun_angular_rel: hornvale_astronomy::star::sun_angular_diameter_rel(
            &system.star,
            Au::new(state.radius_au).expect("physical radius is positive"),
        ),
        eclipse_moon_longitude_deg: moon_ecliptic_longitude_deg(&calendar, 0, instant)
            .expect("one-moon fixture has a longitude"),
        expected_moon_longitude_deg: (360.0 * state.true_longitude_turns + 360.0 * phase)
            .rem_euclid(360.0),
    }
}

/// Direction enforced: every consumer readout must agree with independently
/// projected state geometry. Each label names the migration that would break it.
fn coherence_failures(readout: &ConsumerReadout, requested: StdInstant) -> Vec<&'static str> {
    let mut failures = Vec::new();
    let position_radius = (readout.position_au[0].powi(2) + readout.position_au[1].powi(2)).sqrt();
    let position_longitude = (math::atan2(readout.position_au[1], readout.position_au[0])
        / std::f64::consts::TAU)
        .rem_euclid(1.0);

    if readout.instant != requested {
        failures.push("explicit instant");
    }
    if readout.frame != "system-plane-au" {
        failures.push("state frame");
    }
    if (position_radius - readout.radius_au).abs() > TOLERANCE {
        failures.push("state radius");
    }
    if turn_distance(position_longitude, readout.true_longitude_turns) > TOLERANCE {
        failures.push("state longitude");
    }
    if turn_distance(
        readout.calendar_longitude_turns,
        readout.true_longitude_turns,
    ) > TOLERANCE
    {
        failures.push("calendar longitude");
    }
    if (readout.primary_distance_au - readout.radius_au).abs() > TOLERANCE {
        failures.push("illumination distance");
    }
    if (readout.primary_flux_rel - readout.expected_primary_flux_rel).abs() > TOLERANCE {
        failures.push("illumination flux");
    }
    if (readout.eclipse_sun_angular_rel - readout.expected_sun_angular_rel).abs() > TOLERANCE {
        failures.push("eclipse radius");
    }
    if degree_distance(
        readout.eclipse_moon_longitude_deg,
        readout.expected_moon_longitude_deg,
    ) > TOLERANCE
    {
        failures.push("eclipse longitude");
    }
    failures
}

fn turn_distance(left: f64, right: f64) -> f64 {
    ((left - right + 0.5).rem_euclid(1.0) - 0.5).abs()
}

fn degree_distance(left: f64, right: f64) -> f64 {
    ((left - right + 180.0).rem_euclid(360.0) - 180.0).abs()
}

/// Restoring any consumer's legacy mean-orbit reconstruction makes this fail,
/// including at negative time and under locked or retrograde spin conventions.
#[test]
fn anchor_coherence_cross_consumer_battery_covers_time_and_spin_regimes() {
    for system in [
        eccentric_system(RotationPin::PeriodHours(24.0), Some(SpinPin::Prograde)),
        eccentric_system(RotationPin::PeriodHours(24.0), Some(SpinPin::Retrograde)),
        eccentric_system(RotationPin::Locked, None),
    ] {
        for day in [-100_000.0, -1234.5, 0.0, 9876.5, 100_000.0] {
            let instant = StdInstant::new(day).expect("finite instant");
            let projection = readout(&system, instant);
            assert_eq!(
                coherence_failures(&projection, instant),
                Vec::<&'static str>::new(),
                "day {day}, rotation {:?}",
                system.anchor.rotation
            );
        }
    }
}

/// Hidden state or ambient time would make the same explicit probes depend on
/// query order. The exact readout comparison also guards repeat determinism.
#[test]
fn anchor_coherence_queries_are_order_independent() {
    let system = eccentric_system(RotationPin::PeriodHours(24.0), Some(SpinPin::Prograde));
    let instants: Vec<_> = [-10_000.0, -1.0, 0.0, 1.0, 10_000.0]
        .into_iter()
        .map(|day| StdInstant::new(day).expect("finite instant"))
        .collect();
    let forward: Vec<_> = instants
        .iter()
        .copied()
        .map(|instant| readout(&system, instant))
        .collect();
    let mut reverse: Vec<_> = instants
        .iter()
        .rev()
        .copied()
        .map(|instant| readout(&system, instant))
        .collect();
    reverse.reverse();

    assert_eq!(forward, reverse);
}

/// Positive control for the battery: a test-only alternate eclipse projection
/// is observably changed before the checker is required to reject it. If this
/// remains green after the checker stops inspecting eclipse longitude, the
/// exact failure label below changes or disappears.
#[test]
fn anchor_coherence_tripwire_detects_a_perturbed_eclipse_projection() {
    let system = eccentric_system(RotationPin::PeriodHours(24.0), Some(SpinPin::Prograde));
    let instant = StdInstant::new(-1234.5).expect("finite instant");
    let original = readout(&system, instant);
    let mut perturbed = original.clone();
    perturbed.eclipse_moon_longitude_deg =
        (perturbed.eclipse_moon_longitude_deg + 45.0).rem_euclid(360.0);

    assert_ne!(
        original.eclipse_moon_longitude_deg, perturbed.eclipse_moon_longitude_deg,
        "mutation target must change before the tripwire can count"
    );
    assert_eq!(
        coherence_failures(&perturbed, instant),
        vec!["eclipse longitude"]
    );
}

/// Malformed physical state stays distinct from a valid no-event interval:
/// the evaluator diagnoses the former while eclipse tests own the latter.
#[test]
fn anchor_coherence_malformed_period_is_a_descriptive_physical_error() {
    let mut system = eccentric_system(RotationPin::PeriodHours(24.0), Some(SpinPin::Prograde));
    system.anchor.year = StdDays::new(0.0).expect("zero is constructible for malformed fixtures");

    let error = anchor_state_at(&system, StdInstant::new(12.0).expect("finite instant"))
        .expect_err("zero period cannot describe an orbit");

    assert!(matches!(error, OrbitalError::DegenerateOrbit { .. }));
    assert_eq!(
        error.to_string(),
        "orbital period is degenerate: 0 (must be positive)"
    );
}
