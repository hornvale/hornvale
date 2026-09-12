use hornvale_astronomy::{SkyPins, StdInstant, StellarTopology, ephemeris::*};
use hornvale_kernel::{Seed, WorldTime};
use hornvale_scene::*;
fn world(topology: StellarTopology) -> hornvale_kernel::World {
    hornvale_worldgen::build_world_to(
        Seed(42),
        &SkyPins {
            topology: Some(topology),
            ..Default::default()
        },
        &Default::default(),
        &Default::default(),
        &hornvale_worldgen::components::WorldComponents::assemble().unwrap(),
        hornvale_worldgen::BuildDepth::Astronomy,
    )
    .unwrap()
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
            let a = anchor_position_at(s, t);
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
