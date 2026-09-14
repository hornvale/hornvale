use hornvale_astronomy::{ephemeris::*, *};
use hornvale_kernel::{Seed, math};
fn near(a: f64, b: f64) {
    assert!((a - b).abs() < 1e-10, "{a} != {b}");
}
#[test]
fn native_basis_preserves_calendar_sightline_and_handedness() {
    for (rotation, spin) in [
        (RotationPin::PeriodHours(24.0), SpinPin::Prograde),
        (RotationPin::PeriodHours(24.0), SpinPin::Retrograde),
        (RotationPin::Locked, SpinPin::Prograde),
    ] {
        let s = generate(
            Seed(42),
            &SkyPins {
                rotation: Some(rotation.clone()),
                spin: (!matches!(rotation, RotationPin::Locked)).then_some(spin),
                obliquity: Some(Degrees::new(35.0).unwrap()),
                ..Default::default()
            },
        )
        .unwrap()
        .value;
        let c = calendar_of(&s);
        for t in [-123.0, 0.0, 71.5] {
            let t = StdInstant::new(t).unwrap();
            let p = anchor_position_at(&s, t);
            let phase = std::f64::consts::TAU * c.year_phase(t);
            near(p.x_au, s.anchor.orbit.get() * math::cos(phase));
            near(p.y_au, s.anchor.orbit.get() * math::sin(phase));
            let state = anchor_state_at(&s, t).unwrap();
            let b = anchor_body_to_frame_at(&s, t);
            let eq = equatorial_at(
                &EclipticCoord {
                    lon_deg: 360.0 * state.true_longitude_turns,
                    lat_deg: 0.0,
                },
                s.forcing.obliquity_at(t.get()),
                0.0,
            );
            let v = math::unit_sphere_from_lat_lon(eq.dec_deg, sub_solar_longitude_deg(&c, t));
            let mapped: [f64; 3] = std::array::from_fn(|r| (0..3).map(|i| b[i][r] * v[i]).sum());
            near(mapped[0], -state.position_au[0] / state.radius_au);
            near(mapped[1], -state.position_au[1] / state.radius_au);
            near(mapped[2], 0.0);
            for i in 0..3 {
                for j in 0..3 {
                    near(
                        (0..3).map(|r| b[i][r] * b[j][r]).sum(),
                        if i == j { 1.0 } else { 0.0 },
                    );
                }
            }
            for r in 0..3 {
                near(
                    b[0][(r + 1) % 3] * b[1][(r + 2) % 3] - b[0][(r + 2) % 3] * b[1][(r + 1) % 3],
                    b[2][r],
                );
            }
        }
    }
}
#[test]
fn native_moons_follow_existing_longitude_latitude_and_node() {
    let s = generate(Seed(42), &SkyPins::default()).unwrap().value;
    let c = calendar_of(&s);
    assert!(s.moons.iter().any(|m| m.inclination_deg > 90.0));
    for (i, m) in s.moons.iter().enumerate() {
        for t in [-120.0, 0.0, 73.0] {
            let t = StdInstant::new(t).unwrap();
            let p = moon_position_at(&s, i, t).unwrap();
            let lon = moon_ecliptic_longitude_deg(&c, i, t).unwrap().to_radians();
            let lat = moon_ecliptic_latitude_deg(&c, m, i, t)
                .unwrap()
                .to_radians();
            near(p[0] / m.distance.get(), -math::cos(lat) * math::cos(lon));
            near(p[1] / m.distance.get(), -math::cos(lat) * math::sin(lon));
            near(p[2] / m.distance.get(), math::sin(lat));
            near(
                math::sin(lat),
                math::sin(m.inclination_deg.to_radians())
                    * math::sin(lon - node_longitude_at(m, s.anchor.year, t).to_radians()),
            );
        }
    }
}
#[test]
fn missing_moon_cycle_is_absent() {
    let mut s = generate(Seed(42), &SkyPins::default()).unwrap().value;
    s.moons[0].period = s.anchor.year;
    assert!(moon_position_at(&s, 0, StdInstant::new(0.0).unwrap()).is_none());
}

#[test]
fn locked_orientation_turns_synchronously_and_keeps_prime_meridian_substellar() {
    let s = generate(
        Seed(42),
        &SkyPins {
            rotation: Some(RotationPin::Locked),
            ..Default::default()
        },
    )
    .unwrap()
    .value;
    let c = calendar_of(&s);
    let a = StdInstant::new(0.0).unwrap();
    let b = StdInstant::new(s.anchor.year.get() / 4.0).unwrap();
    near(sub_solar_longitude_deg(&c, a), 0.0);
    near(sub_solar_longitude_deg(&c, b), 0.0);
    assert_ne!(
        anchor_body_to_frame_at(&s, a),
        anchor_body_to_frame_at(&s, b)
    );
}
