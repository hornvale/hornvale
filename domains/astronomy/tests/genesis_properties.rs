//! The property battery (spec §10): every generated system, across many
//! seeds and the pin matrix, satisfies the model card's invariants.

use hornvale_astronomy::{
    Degrees, GenesisError, LocalDays, MoonsPin, NeighborClass, Rotation, RotationPin, SkyPins,
    generate, hill_radius_mm,
};
use hornvale_kernel::Seed;

#[test]
fn every_default_system_satisfies_every_invariant() {
    for seed in 0..128 {
        let outcome = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        let system = &outcome.system;
        let (inner, outer) = system.star.habitable_zone;
        assert!(
            (inner.get()..=outer.get()).contains(&system.anchor.orbit.get()),
            "seed {seed}: anchor out of zone"
        );
        let expected_year =
            365.25 * (system.anchor.orbit.get().powi(3) / system.star.mass.get()).sqrt();
        assert!((system.anchor.year.get() - expected_year).abs() < 1e-9);
        let hill = hill_radius_mm(&system.star, &system.anchor);
        let mut total_tide = 0.0;
        for pair in system.moons.windows(2) {
            assert!(
                pair[1].distance.get() / pair[0].distance.get() >= 1.5,
                "seed {seed}: spacing"
            );
        }
        for moon in &system.moons {
            assert!(moon.distance.get() >= 20.0 && moon.distance.get() <= 0.4 * hill);
            total_tide += moon.tide_rel;
        }
        assert!(total_tide <= 8.0, "seed {seed}: tide cap");
        assert!((2..=5).contains(&system.neighbors.len()));
    }
}

#[test]
fn the_pin_matrix_is_honored() {
    for moons in 0..=3u32 {
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(moons).unwrap()),
            ..SkyPins::default()
        };
        assert_eq!(
            generate(Seed(42), &pins).unwrap().system.moons.len() as u32,
            moons
        );
    }
    let pins = SkyPins {
        rotation: Some(RotationPin::Locked),
        ..SkyPins::default()
    };
    assert_eq!(
        generate(Seed(42), &pins).unwrap().system.anchor.rotation,
        Rotation::Locked
    );
    let pins = SkyPins {
        obliquity: Some(Degrees::new(0.0).unwrap()),
        ..SkyPins::default()
    };
    assert_eq!(
        generate(Seed(42), &pins)
            .unwrap()
            .system
            .anchor
            .obliquity
            .get(),
        0.0
    );
    let pins = SkyPins {
        neighbor: Some(NeighborClass::BlueGiant),
        ..SkyPins::default()
    };
    assert_eq!(
        generate(Seed(42), &pins).unwrap().system.neighbors[0].class,
        NeighborClass::BlueGiant
    );
}

#[test]
fn unsatisfiable_pins_fail_loudly_with_the_physical_reason() {
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        year_local_days: Some(LocalDays::new(4000.0).unwrap()),
        ..SkyPins::default()
    };
    match generate(Seed(42), &pins) {
        Err(GenesisError::UnsatisfiablePin { pin, reason }) => {
            assert_eq!(pin, "year-days");
            assert!(reason.contains("habitable zone"));
        }
        other => panic!("expected UnsatisfiablePin, got {other:?}"),
    }
}

#[test]
fn pinned_worlds_differ_from_unpinned_only_downstream_of_the_pin() {
    // Same seed, moons pinned to the drawn count => identical system.
    let default = generate(Seed(42), &SkyPins::default()).unwrap().system;
    let pinned_same = SkyPins {
        moons: Some(MoonsPin::exact(default.moons.len() as u32).unwrap()),
        ..SkyPins::default()
    };
    assert_eq!(generate(Seed(42), &pinned_same).unwrap().system, default);
}

#[test]
fn pin_isolation_holds_at_the_system_level() {
    // A Normal rotation pin re-affirms the drawn regime; it must not perturb
    // any other draw. Seed 42's default anchor draws Spinning, so the pinned
    // system must be byte-for-byte identical to the default.
    let default = generate(Seed(42), &SkyPins::default()).unwrap().system;
    assert!(matches!(default.anchor.rotation, Rotation::Spinning { .. }));
    let pins = SkyPins {
        rotation: Some(RotationPin::Normal),
        ..SkyPins::default()
    };
    assert_eq!(generate(Seed(42), &pins).unwrap().system, default);

    // A neighbor pin overrides the showpiece's class only; the rest of the
    // neighborhood (count and distance draws) must be untouched. Compare the
    // sorted multiset of distances rather than positional order, since
    // sort-by-brightness can reorder entries when the pinned class's
    // brightness changes.
    let pins = SkyPins {
        neighbor: Some(NeighborClass::BlueGiant),
        ..SkyPins::default()
    };
    let pinned = generate(Seed(42), &pins).unwrap();
    assert_eq!(default.neighbors.len(), pinned.system.neighbors.len());
    let mut default_distances: Vec<f64> =
        default.neighbors.iter().map(|n| n.distance.get()).collect();
    let mut pinned_distances: Vec<f64> = pinned
        .system
        .neighbors
        .iter()
        .map(|n| n.distance.get())
        .collect();
    default_distances.sort_by(f64::total_cmp);
    pinned_distances.sort_by(f64::total_cmp);
    assert_eq!(default_distances, pinned_distances);
    assert!(
        pinned
            .system
            .neighbors
            .iter()
            .any(|n| n.class == NeighborClass::BlueGiant)
    );
}
