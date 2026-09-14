use hornvale_astronomy::{OrbitalError, SkyPins, StdInstant, anchor_state_at, generate};
use hornvale_kernel::Seed;

fn seed_42_system() -> hornvale_astronomy::StarSystem {
    generate(Seed(42), &SkyPins::default())
        .expect("seed 42 builds")
        .value
}

#[test]
fn anchor_state_at_is_deterministic_and_records_the_requested_instant() {
    let system = seed_42_system();
    let instant = StdInstant::new(123.5).expect("finite instant");

    let first = anchor_state_at(&system, instant).expect("valid anchor orbit");
    let second = anchor_state_at(&system, instant).expect("valid anchor orbit");

    assert_eq!(first, second);
    assert_eq!(first.instant, instant);
    assert_eq!(first.valid_from, None);
    assert_eq!(first.valid_until, None);
}

#[test]
fn anchor_state_at_returns_finite_geometry_for_valid_seed_42_instants() {
    let system = seed_42_system();

    for day in [-100_000.0, 0.0, 100_000.0] {
        let state = anchor_state_at(&system, StdInstant::new(day).expect("finite instant"))
            .expect("valid anchor orbit");
        assert!(state.position_au.into_iter().all(f64::is_finite));
        assert!(state.velocity_au_per_day.into_iter().all(f64::is_finite));
        assert!(state.radius_au.is_finite() && state.radius_au > 0.0);
    }
}

#[test]
fn anchor_state_at_evaluates_negative_instants_instead_of_clamping_to_genesis() {
    let system = seed_42_system();
    let before = anchor_state_at(
        &system,
        StdInstant::new(-10.0).expect("finite negative instant"),
    )
    .expect("pre-genesis anchor orbit is valid");
    let genesis = anchor_state_at(&system, StdInstant::new(0.0).expect("finite instant"))
        .expect("genesis anchor orbit is valid");

    assert_eq!(before.instant.get(), -10.0);
    assert_ne!(before.position_au, genesis.position_au);
}

#[test]
fn anchor_state_at_names_invalid_non_finite_elements() {
    let mut system = seed_42_system();
    system.forcing.ecc_mean = f64::NAN;
    system.forcing.ecc_amp = 0.0;

    let error = anchor_state_at(&system, StdInstant::new(0.0).expect("finite instant"))
        .expect_err("non-finite eccentricity must be rejected");

    assert!(matches!(error, OrbitalError::InvalidInput { .. }));
    assert!(error.to_string().contains("eccentricity"));
}

#[test]
fn anchor_state_at_names_degenerate_orbits() {
    let mut system = seed_42_system();
    system.forcing.ecc_mean = 1.0;
    system.forcing.ecc_amp = 0.0;

    let error = anchor_state_at(&system, StdInstant::new(0.0).expect("finite instant"))
        .expect_err("parabolic anchor orbit must be rejected");

    assert!(matches!(error, OrbitalError::DegenerateOrbit { .. }));
    assert!(error.to_string().contains("eccentricity"));
}
