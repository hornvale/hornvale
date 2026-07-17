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

/// Save-format contract: pinning the wanderer count moves NOTHING else.
/// The count draw still happens (then is overridden); per-wanderer draws
/// live on their own stream, so star/anchor/moons/neighbors/forcing bytes
/// are identical pinned vs unpinned.
#[test]
fn wanderers_pin_leaves_the_rest_of_the_sky_untouched() {
    for seed in [1u64, 7, 42, 99] {
        let unpinned = generate(Seed(seed), &SkyPins::default()).unwrap().system;
        for n in 0..=4u32 {
            let pins = SkyPins {
                wanderers: Some(n),
                ..SkyPins::default()
            };
            let pinned = generate(Seed(seed), &pins).unwrap().system;
            assert_eq!(pinned.wanderers.len() as u32, n);
            assert_eq!(unpinned.star, pinned.star);
            assert_eq!(unpinned.anchor, pinned.anchor);
            assert_eq!(unpinned.moons, pinned.moons);
            assert_eq!(unpinned.neighbors, pinned.neighbors);
            assert_eq!(unpinned.forcing, pinned.forcing);
        }
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

#[test]
fn equatorial_daylight_is_flat_and_every_latitude_stays_in_range() {
    use hornvale_astronomy::{StdDays, calendar_of};
    for seed in 0..64u64 {
        let system = generate(
            Seed(seed),
            &SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                ..SkyPins::default()
            },
        )
        .unwrap()
        .system;
        let cal = calendar_of(&system);
        let year = system.anchor.year.get();
        for k in 0..8 {
            let t = StdDays::new(k as f64 * year / 8.0).unwrap();
            let equator = cal.daylight_fraction_at(t, 0.0).unwrap();
            assert!(
                (equator - 0.5).abs() < 1e-9,
                "seed {seed}: equator not flat: {equator}"
            );
            for lat in [-80.0, -30.0, 30.0, 80.0] {
                let f = cal.daylight_fraction_at(t, lat).unwrap();
                assert!(
                    (0.0..=1.0).contains(&f),
                    "seed {seed}: daylight {f} out of range at lat {lat}"
                );
            }
        }
    }
}

#[test]
fn a_locked_worlds_hemispheres_cull_the_sky() {
    use hornvale_astronomy::{GeneratedSky, NIGHT_STAR};
    use hornvale_kernel::{EntityId, GeoCoord, ObserverContext, PhenomenaSource, Venue, WorldTime};
    for seed in 0..32u64 {
        let outcome = generate(
            Seed(seed),
            &SkyPins {
                rotation: Some(RotationPin::Locked),
                ..SkyPins::default()
            },
        )
        .unwrap();
        let sky = GeneratedSky::new(outcome);
        let day = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord {
                latitude: 5.0,
                longitude: 0.0,
            },
        );
        let night = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord {
                latitude: 5.0,
                longitude: 179.0,
            },
        );
        let day_ph = sky.phenomena(&day);
        let night_ph = sky.phenomena(&night);
        assert!(
            day_ph.iter().any(|p| p.venue == Venue::DaySky),
            "seed {seed}: day side sees no sun"
        );
        assert!(
            !day_ph.iter().any(|p| p.kind == NIGHT_STAR),
            "seed {seed}: day side sees stars"
        );
        assert!(
            !night_ph.iter().any(|p| p.venue == Venue::DaySky),
            "seed {seed}: night side sees the sun"
        );
        assert!(
            night_ph.iter().any(|p| p.kind == NIGHT_STAR),
            "seed {seed}: night side sees no stars"
        );
    }
}

#[test]
fn a_spinning_worlds_sky_is_whole_from_any_placed_vantage() {
    use hornvale_astronomy::{CELESTIAL_BODY, GeneratedSky, NIGHT_STAR};
    use hornvale_kernel::{EntityId, GeoCoord, ObserverContext, PhenomenaSource, Venue, WorldTime};
    for seed in 0..32u64 {
        // A handful of seeds draw an anchor whose Hill radius is too small
        // to admit two stable, spaced moons within the attempt budget —
        // a genuine physical unsatisfiability (see
        // `unsatisfiable_pins_fail_loudly_with_the_physical_reason`), not a
        // defect in the placed-observer culling under test here. Skip those
        // seeds rather than asserting on a system that never came to exist.
        let outcome = match generate(
            Seed(seed),
            &SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                moons: Some(MoonsPin::exact(2).unwrap()),
                ..SkyPins::default()
            },
        ) {
            Ok(outcome) => outcome,
            Err(GenesisError::UnsatisfiablePin { pin, .. }) if pin == "moons" => continue,
            Err(e) => panic!("seed {seed}: unexpected genesis failure: {e}"),
        };
        let sky = GeneratedSky::new(outcome);
        let obs = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 3.5 },
            GeoCoord {
                latitude: 55.0,
                longitude: -120.0,
            },
        );
        let ph = sky.phenomena(&obs);
        assert!(
            ph.iter().any(|p| p.venue == Venue::DaySky),
            "seed {seed}: no sun"
        );
        assert!(
            ph.iter().any(|p| p.kind == NIGHT_STAR),
            "seed {seed}: no stars (should be whole sky)"
        );
        assert_eq!(
            ph.iter()
                .filter(|p| p.kind == CELESTIAL_BODY && p.description.contains("moon"))
                .count(),
            2,
            "seed {seed}: both moons should be visible on a spinning world"
        );
    }
}

/// The neighbor-class pin must not move any star: positions draw from
/// their own stream (spec §3), so the pinned and unpinned skies hold the
/// same position multiset even though one star's class differs.
#[test]
fn neighbor_pin_does_not_move_any_star() {
    let seed = Seed(42);
    let unpinned = generate(seed, &SkyPins::default()).unwrap();
    let pinned = generate(
        seed,
        &SkyPins {
            neighbor: Some(NeighborClass::RedGiant),
            ..SkyPins::default()
        },
    )
    .unwrap();
    let mut a: Vec<(f64, f64)> = unpinned
        .system
        .neighbors
        .iter()
        .map(|n| (n.declination, n.right_ascension))
        .collect();
    let mut b: Vec<(f64, f64)> = pinned
        .system
        .neighbors
        .iter()
        .map(|n| (n.declination, n.right_ascension))
        .collect();
    a.sort_by(|x, y| x.0.total_cmp(&y.0).then(x.1.total_cmp(&y.1)));
    b.sort_by(|x, y| x.0.total_cmp(&y.0).then(x.1.total_cmp(&y.1)));
    assert_eq!(a, b);
}

/// Eclipse Seasons pin isolation: pinning the moon count consumes node
/// draws identically to the unpinned path — a pinned world with the same
/// admitted moons carries byte-identical node longitudes.
#[test]
fn pinned_moon_counts_draw_identical_node_longitudes() {
    for seed in 0..32u64 {
        let unpinned = generate(Seed(seed), &SkyPins::default()).unwrap().system;
        let n = unpinned.moons.len() as u32;
        if n == 0 {
            continue;
        }
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(n).unwrap()),
            ..SkyPins::default()
        };
        let pinned = generate(Seed(seed), &pins).unwrap().system;
        let a: Vec<f64> = unpinned
            .moons
            .iter()
            .map(|m| m.node_longitude_deg)
            .collect();
        let b: Vec<f64> = pinned.moons.iter().map(|m| m.node_longitude_deg).collect();
        assert_eq!(a, b, "seed {seed}");
    }
}

/// SKY-23 close-out, star battery: over 256 seeds, the drawn mass stays
/// in range and every derivation is monotone in it (luminosity, HZ,
/// brightening).
#[test]
fn star_battery_mass_bounds_and_monotone_derivations() {
    use hornvale_astronomy::{brightening_per_gyr, generate_star};
    let mut last: Option<(f64, f64, f64, f64, f64)> = None;
    let mut stars: Vec<_> = (0..256u64)
        .map(|s| generate_star(hornvale_kernel::Seed(s).derive("astronomy")))
        .collect();
    stars.sort_by(|a, b| a.mass.get().total_cmp(&b.mass.get()));
    for s in &stars {
        assert!((0.6..=1.4).contains(&s.mass.get()));
        assert!(s.habitable_zone.inner().get() < s.habitable_zone.outer().get());
        let row = (
            s.mass.get(),
            s.luminosity.get(),
            s.habitable_zone.inner().get(),
            s.habitable_zone.outer().get(),
            brightening_per_gyr(s),
        );
        if let Some(prev) = last {
            assert!(
                row.1 >= prev.1 && row.2 >= prev.2 && row.3 >= prev.3 && row.4 >= prev.4,
                "derivations must be monotone in mass: {prev:?} -> {row:?}"
            );
        }
        last = Some(row);
    }
}

/// The Reckoning's containment rule (spec §3): drawing an age must not move
/// luminosity or the habitable zone. If this ever fails, the epoch has
/// leaked out of the moons and into climate.
#[test]
fn stellar_age_does_not_touch_luminosity_or_the_habitable_zone() {
    use hornvale_astronomy::generate_star;
    for seed in 0..500u64 {
        let star = generate_star(Seed(seed));
        // Luminosity is M^3.5 and NOTHING else — recompute it independently.
        let expected_l = hornvale_kernel::math::powf(star.mass.get(), 3.5);
        assert!(
            (star.luminosity.get() - expected_l).abs() < 1e-12,
            "seed {seed}: luminosity {} != M^3.5 {}",
            star.luminosity.get(),
            expected_l
        );
        let sqrt_l = expected_l.sqrt();
        assert!(
            (star.habitable_zone.inner().get() - 0.95 * sqrt_l).abs() < 1e-12,
            "seed {seed}"
        );
        assert!(
            (star.habitable_zone.outer().get() - 1.37 * sqrt_l).abs() < 1e-12,
            "seed {seed}"
        );
    }
}

/// SKY-23 close-out, anchor battery: over 256 seeds the orbit sits inside
/// the habitable zone, the Kepler relation holds, and locked worlds never
/// have a solar hour.
#[test]
fn anchor_battery_orbit_kepler_and_rotation_invariants() {
    use hornvale_astronomy::{Rotation, SkyPins, calendar_of, generate};
    let mut saw_locked = false;
    let mut saw_spinning = false;
    let mut saw_retrograde = false;
    for seed in 0..256u64 {
        let outcome = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let s = &outcome.system;
        let zone = s.star.habitable_zone;
        assert!(
            (zone.inner().get()..=zone.outer().get()).contains(&s.anchor.orbit.get()),
            "seed {seed}: orbit outside the zone"
        );
        // Kepler III in the model card's own units: Y = 365.25·√(a³/M).
        let expected_year = 365.25 * (s.anchor.orbit.get().powi(3) / s.star.mass.get()).sqrt();
        assert!(
            (s.anchor.year.get() - expected_year).abs() < 1e-6,
            "seed {seed}: year {} vs Kepler {expected_year}",
            s.anchor.year.get()
        );
        // Obliquity drawn in range 0–35 degrees.
        assert!((0.0..=35.0).contains(&s.anchor.obliquity.get()));
        let calendar = calendar_of(s);
        match s.anchor.rotation {
            Rotation::Locked => {
                saw_locked = true;
                assert!(calendar.day_length().is_none());
            }
            Rotation::Spinning { retrograde, .. } => {
                saw_spinning = true;
                saw_retrograde |= retrograde;
                assert!(calendar.day_length().is_some());
            }
        }
    }
    assert!(
        saw_locked && saw_spinning && saw_retrograde,
        "every regime reachable"
    );
}

/// SKY-23 close-out, neighbors battery: over 256 seeds the count stays in
/// 2–5, every position is a legal sky coordinate, distances are positive,
/// and regeneration is byte-identical.
#[test]
fn neighbor_battery_counts_coordinates_and_determinism() {
    use hornvale_astronomy::{SkyPins, generate};
    for seed in 0..256u64 {
        let a = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let b = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        assert_eq!(a, b, "seed {seed}: regeneration must be byte-identical");
        let n = &a.system.neighbors;
        assert!(
            (2..=5).contains(&n.len()),
            "seed {seed}: {} neighbors",
            n.len()
        );
        for x in n {
            assert!((0.0..360.0).contains(&x.right_ascension));
            assert!((-90.0..=90.0).contains(&x.declination));
            assert!(x.distance.get() > 0.0);
        }
    }
}

/// The Long Count, alignment battery: the dating inverse round-trips
/// across seeds and latitudes.
#[test]
fn alignment_battery_dating_round_trip() {
    use hornvale_astronomy::{Rotation, SkyPins, StdDays, calendar_of, generate};
    for seed in 0..128u64 {
        let outcome = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let s = &outcome.system;
        if matches!(s.anchor.rotation, Rotation::Locked) || s.forcing.obliquity_amp == 0.0 {
            continue;
        }
        let calendar = calendar_of(s);
        for lat in [-55.0, -20.0, 20.0, 55.0] {
            let t = StdDays::new(0.27 * hornvale_astronomy::forcing::P_OBLIQUITY).unwrap();
            let Some(az) = calendar.solstice_rise_azimuth_at(lat, t) else {
                continue;
            };
            let epoch = calendar
                .alignment_epoch_of(az, lat, StdDays::new(t.get() + 1.0).unwrap())
                .expect("a wobbling sky dates its own alignments");
            assert!((epoch.get() - t.get()).abs() < 1.0, "seed {seed} lat {lat}");
        }
    }
}
