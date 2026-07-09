//! The property battery (spec §10): every generated system, across many
//! seeds and the pin matrix, satisfies the model card's invariants.

use hornvale_astronomy::{
    Degrees, ForcingPin, GenesisError, LocalDays, MoonsPin, NeighborClass, Rotation, RotationPin,
    SkyPins, generate, hill_radius_mm,
};
use hornvale_kernel::Seed;

#[test]
fn every_default_system_satisfies_every_invariant() {
    for seed in 0..128 {
        let outcome = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        let system = &outcome.system;
        let (inner, outer) = (
            system.star.habitable_zone.inner(),
            system.star.habitable_zone.outer(),
        );
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

#[test]
fn obliquity_at_zero_matches_the_anchor_drift_anchor_identity() {
    // The forcing's obliquity element is anchored so that day 0 exactly
    // recovers genesis' drawn obliquity — a save-format contract for any
    // consumer (Calendar, paleoclimate) that reads `element_at(0)` as "the
    // present".
    for seed in 0..64u64 {
        let system = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"))
            .system;
        assert_eq!(
            system.forcing.obliquity_at(0.0),
            system.anchor.obliquity.get(),
            "seed {seed}: obliquity_at(0) must equal the anchor's genesis obliquity"
        );
    }
}

#[test]
fn forcing_is_pin_isolated() {
    // A world built with forcing=zero must consume the SAME draws as the
    // default — only the forcing amplitudes differ (zeroed). This is
    // `normal_pin_matches_the_unpinned_draw`'s pattern applied to forcing:
    // the pin re-affirms/overrides downstream of the draw, it must never
    // perturb any other stream.
    for seed in 0..64u64 {
        let base = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"))
            .system;
        let zeroed = generate(
            Seed(seed),
            &SkyPins {
                forcing: Some(ForcingPin::Zero),
                ..SkyPins::default()
            },
        )
        .unwrap_or_else(|e| panic!("seed {seed} failed zeroed genesis: {e}"))
        .system;
        assert_eq!(
            base.star, zeroed.star,
            "seed {seed}: star drifted under the pin"
        );
        assert_eq!(base.anchor, zeroed.anchor, "seed {seed}: anchor drifted");
        assert_eq!(base.moons, zeroed.moons, "seed {seed}: moons drifted");
        assert_eq!(
            base.neighbors, zeroed.neighbors,
            "seed {seed}: neighbors drifted"
        );
        assert_eq!(
            base.forcing.obliquity_phase, zeroed.forcing.obliquity_phase,
            "seed {seed}: obliquity phase must be drawn identically"
        );
        assert_eq!(
            base.forcing.ecc_phase, zeroed.forcing.ecc_phase,
            "seed {seed}: eccentricity phase must be drawn identically"
        );
        assert_eq!(
            base.forcing.precession_phase, zeroed.forcing.precession_phase,
            "seed {seed}: precession phase must be drawn identically"
        );
        assert_eq!(
            base.forcing.year_phase_offset, zeroed.forcing.year_phase_offset,
            "seed {seed}: year phase offset must be drawn identically"
        );
        assert_eq!(
            base.forcing.day_phase_offset, zeroed.forcing.day_phase_offset,
            "seed {seed}: day phase offset must be drawn identically"
        );
        assert_eq!(
            base.forcing.moon_phase_offsets, zeroed.forcing.moon_phase_offsets,
            "seed {seed}: moon phase offsets must be drawn identically"
        );
        // Only the amplitudes/means differ, and only via the zeroing.
        assert_eq!(
            zeroed.forcing.obliquity_amp, 0.0,
            "seed {seed}: obliquity_amp not zeroed"
        );
        assert_eq!(
            zeroed.forcing.ecc_mean, 0.0,
            "seed {seed}: ecc_mean not zeroed"
        );
        assert_eq!(
            zeroed.forcing.ecc_amp, 0.0,
            "seed {seed}: ecc_amp not zeroed"
        );
        assert_eq!(
            base.forcing.obliquity_mean, zeroed.forcing.obliquity_mean,
            "seed {seed}: obliquity_mean must be untouched by the pin (it mirrors the anchor)"
        );
    }
}

#[test]
fn forcing_zero_pin_yields_zeroed_amplitudes() {
    for seed in 0..64u64 {
        let pins = SkyPins {
            forcing: Some(ForcingPin::Zero),
            ..SkyPins::default()
        };
        let forcing = generate(Seed(seed), &pins)
            .unwrap_or_else(|e| panic!("seed {seed} failed zeroed genesis: {e}"))
            .system
            .forcing;
        assert_eq!(forcing.ecc_mean, 0.0, "seed {seed}: ecc_mean not zeroed");
        assert_eq!(
            forcing.obliquity_amp, 0.0,
            "seed {seed}: obliquity_amp not zeroed"
        );
        assert_eq!(forcing.ecc_amp, 0.0, "seed {seed}: ecc_amp not zeroed");
    }
}
