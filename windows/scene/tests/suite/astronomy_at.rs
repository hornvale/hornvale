use hornvale_astronomy::{
    Degrees, EclipticCoord, RotationPin, SkyPins, SpinPin, StdInstant, StellarTopology,
    calendar_of, ephemeris::*, equatorial_at, sub_solar_longitude_deg,
};
use hornvale_kernel::{Seed, WorldTime, math};
use hornvale_scene::*;
fn world(topology: StellarTopology) -> hornvale_kernel::World {
    world_with_pins(&SkyPins {
        topology: Some(topology),
        ..Default::default()
    })
}
fn world_with_pins(pins: &SkyPins) -> hornvale_kernel::World {
    hornvale_worldgen::build_world_to(
        Seed(42),
        pins,
        &Default::default(),
        &Default::default(),
        &hornvale_worldgen::components::WorldComponents::assemble().unwrap(),
        hornvale_worldgen::BuildDepth::Astronomy,
    )
    .unwrap()
}

fn near(left: f64, right: f64) {
    assert!((left - right).abs() < 1e-10, "{left} != {right}");
}
#[test]
fn astronomy_at_matches_native_all_topologies_and_preserves_missing_dimensions() {
    for topology in [
        StellarTopology::Single,
        StellarTopology::WideBinary,
        StellarTopology::CloseBinary,
    ] {
        let w = world(topology);
        let ctx = AstronomyContext::build(&w).unwrap();
        let sky = hornvale_worldgen::sky_of(&w).unwrap();
        let s = sky.system();
        for ticks in [-100000, 0, 12345678, 1_000_000_000_000_000] {
            let at = WorldTime::from_ticks(ticks);
            let t = StdInstant::new(at.as_std_days()).unwrap();
            let scene = astronomy_at_scene_in(&ctx, at).unwrap();
            let state = hornvale_astronomy::anchor_state_at(s, t).unwrap();
            let a = OrbitalPosition {
                x_au: state.position_au[0],
                y_au: state.position_au[1],
            };
            let doc: serde_json::Value = serde_json::from_str(&astronomy_at_json(&scene)).unwrap();
            assert_eq!(doc["ticks"].as_i64(), Some(ticks));
            for (i, p) in stellar_positions_at(s, t).iter().enumerate() {
                let b = &scene.bodies[i + 1];
                assert!((b.position_km[0] - (p.x_au - a.x_au) * 149597870.7).abs() < 1e-6);
                assert!((b.position_km[1] - (p.y_au - a.y_au) * 149597870.7).abs() < 1e-6);
                assert_eq!(b.radius_km, None);
                assert_eq!(b.id, scene.lights[i].star_id);
                let distance = (b.position_km[0] * b.position_km[0]
                    + b.position_km[1] * b.position_km[1])
                    .sqrt();
                for r in 0..3 {
                    assert!(
                        (scene.lights[i].direction_from_anchor[r] - b.position_km[r] / distance)
                            .abs()
                            < 1e-10
                    );
                }
                let star = if i == 0 {
                    &s.star
                } else {
                    &s.stellar.companion.as_ref().unwrap().star
                };
                assert_eq!(
                    scene.lights[i].luminosity_rel,
                    hornvale_astronomy::luminosity_at(star, t).get()
                );
                if ticks == 1_000_000_000_000_000 {
                    assert!((scene.lights[i].luminosity_rel - star.luminosity.get()).abs() > 1e-6);
                }
                assert!(
                    (scene.lights[i].flux_rel - stellar_illumination_at(s, t).sources[i].flux_rel)
                        .abs()
                        < 1e-12
                );
            }
            for b in &scene.bodies {
                if b.kind == "moon" {
                    assert!(b.radius_km.is_some());
                    assert!(b.body_to_frame.is_none());
                }
                let emitted_body = doc["bodies"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .find(|v| v["id"] == b.id)
                    .unwrap();
                if let Some(radius) = b.radius_km {
                    assert!(
                        (emitted_body["radius_km"].as_f64().unwrap() - radius).abs() <= 0.00005
                    );
                }
                if b.kind == "moon" {
                    for r in 0..3 {
                        assert!(
                            (emitted_body["position_km"][r].as_f64().unwrap() - b.position_km[r])
                                .abs()
                                <= 0.005
                        );
                    }
                }

                if b.kind == "wanderer" {
                    assert!(b.radius_km.is_none());
                }
                let emitted = &doc["bodies"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .find(|x| x["id"] == b.id)
                    .unwrap()["position_km"];
                for r in 0..3 {
                    assert!(
                        (emitted[r].as_f64().unwrap() - b.position_km[r]).abs()
                            <= b.position_km[r].abs() * 5e-8 + 1e-8
                    );
                }
            }
            for (i, col) in scene.bodies[0].body_to_frame.unwrap().iter().enumerate() {
                for (r, v) in col.iter().enumerate() {
                    assert!(
                        (doc["bodies"][0]["body_to_frame"][i][r].as_f64().unwrap() - v).abs()
                            < 5e-9
                    );
                }
            }
            assert_eq!(
                astronomy_at_json(&scene),
                astronomy_at_json(&astronomy_at_scene(&w, at).unwrap())
            );
        }
    }
}
#[test]
fn astronomy_at_repeated_time_is_byte_identical_and_ticks_are_integer() {
    let w = world(StellarTopology::Single);
    let ctx = AstronomyContext::build(&w).unwrap();
    let docs: Vec<_> = [0, 100000, -1, 0, i64::MAX]
        .into_iter()
        .map(|t| astronomy_at_json(&astronomy_at_scene_in(&ctx, WorldTime::from_ticks(t)).unwrap()))
        .collect();
    assert_eq!(docs[0], docs[3]);
    let last: serde_json::Value = serde_json::from_str(&docs[4]).unwrap();
    assert_eq!(last["ticks"].as_i64(), Some(i64::MAX));
}
#[test]
fn astronomy_at_rejects_epochs_with_nonphysical_native_luminosity() {
    let w = world(StellarTopology::Single);
    let result = astronomy_at_scene(&w, WorldTime::from_ticks(i64::MIN));
    assert!(
        matches!(result,Err(SceneError::AstronomyQuery(ref message)) if message.contains("luminosity"))
    );
}

/// Reintroducing mean-phase orientation would point the physical subsolar
/// surface normal away from the star on an eccentric orbit.
#[test]
fn scene_anchor_orientation_coherence_uses_physical_state_for_all_spin_regimes() {
    for (rotation, spin) in [
        (RotationPin::PeriodHours(24.0), Some(SpinPin::Prograde)),
        (RotationPin::PeriodHours(24.0), Some(SpinPin::Retrograde)),
        (RotationPin::Locked, None),
    ] {
        let w = world_with_pins(&SkyPins {
            topology: Some(StellarTopology::Single),
            rotation: Some(rotation),
            spin,
            obliquity: Some(Degrees::new(35.0).unwrap()),
            ..Default::default()
        });
        let ctx = AstronomyContext::build(&w).unwrap();
        let sky = hornvale_worldgen::sky_of(&w).unwrap();
        let system = sky.system();
        let calendar = calendar_of(system);
        for ticks in [-12_345_678_i64, 0, 98_765_432] {
            let at = WorldTime::from_ticks(ticks);
            let instant = StdInstant::new(at.as_std_days()).unwrap();
            let state = hornvale_astronomy::anchor_state_at(system, instant).unwrap();
            let scene = astronomy_at_scene_in(&ctx, at).unwrap();
            let basis = scene.bodies[0].body_to_frame.unwrap();
            let solar_equatorial = equatorial_at(
                &EclipticCoord {
                    lon_deg: 360.0 * state.true_longitude_turns,
                    lat_deg: 0.0,
                },
                system.forcing.obliquity_at(instant.get()),
                0.0,
            );
            let body_normal = math::unit_sphere_from_lat_lon(
                solar_equatorial.dec_deg,
                sub_solar_longitude_deg(&calendar, instant),
            );
            let mapped: [f64; 3] = std::array::from_fn(|row| {
                (0..3)
                    .map(|column| basis[column][row] * body_normal[column])
                    .sum()
            });
            let expected_star_direction = [
                -state.position_au[0] / state.radius_au,
                -state.position_au[1] / state.radius_au,
                0.0,
            ];

            for component in 0..3 {
                near(mapped[component], expected_star_direction[component]);
            }
            let primary = &scene.bodies[1].position_km;
            let distance: f64 = primary.iter().map(|value| value * value).sum();
            let distance = distance.sqrt();
            for component in 0..3 {
                near(
                    primary[component] / distance,
                    expected_star_direction[component],
                );
            }
        }
    }
}
