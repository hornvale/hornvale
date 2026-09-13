use super::*;
fn setup() -> (World, ObservationMirror, SceneCatalog) {
    let mut world = World::new();
    world.init_resource::<Assets<Mesh>>();
    world.init_resource::<Assets<Image>>();
    world.init_resource::<Assets<StandardMaterial>>();
    world.init_resource::<Assets<ScatteringMedium>>();
    world.init_resource::<PendingScene>();
    world.init_resource::<AppliedObservation>();
    let mut m = ObservationMirror::new(include_str!("../tests/fixtures/initial.json")).unwrap();
    m.request(0).unwrap();
    m.accept(include_str!("../tests/fixtures/reply.json"))
        .unwrap();
    let mut c = SceneCatalog::default();
    c.populate(&mut world, &m).unwrap();
    (world, m, c)
}

#[test]
fn surface_evidence_reports_ecs_fallback_visibility() {
    let (mut world, _mirror, catalog) = setup();
    assert!(catalog.surface_render_evidence(&world).fallback_visible);
    world
        .entity_mut(catalog.fallback_surface.expect("anchor fallback"))
        .insert(Visibility::Hidden);
    assert!(!catalog.surface_render_evidence(&world).fallback_visible);
}

#[test]
fn surface_evidence_reports_patch_ownership_over_fallback_region() {
    let (mut world, _mirror, mut catalog) = setup();
    let fallback = catalog.fallback_surface.expect("anchor fallback");
    let patch = world.spawn((Visibility::Visible,)).id();
    catalog.surface.ready.push(ReadySurfacePatch {
        key: SurfacePatchCacheKey {
            revision: "current".into(),
            macro_face: 1 << 17,
            child_path: vec![],
        },
        entity: patch,
        mesh: Handle::default(),
        material: Handle::default(),
        feature_entities: vec![],
        feature_meshes: vec![],
        feature_materials: vec![],
        feature_color: None,
    });

    assert!(catalog.surface_render_evidence(&world).fallback_visible);
    world.entity_mut(fallback).insert(Visibility::Hidden);
    assert!(!catalog.surface_render_evidence(&world).fallback_visible);
}
#[test]
fn reset_removes_entities_assets_and_selection_even_when_ids_repeat() {
    let (mut world, mut m, mut c) = setup();
    c.select("anchor", &mut world).unwrap();
    let entities = c.entities.clone();
    let materials = c.materials.clone();
    let meshes = c.meshes.clone();
    let textures = c.textures.clone();
    let medium = c.medium.clone().unwrap();
    m.reset(include_str!("../tests/fixtures/initial.json"))
        .unwrap();
    c.reset(&mut world, &m).unwrap();
    assert_eq!(c.entity_count(), 0);
    assert_eq!(c.selected(), None);
    assert!(entities.iter().all(|e| world.get_entity(*e).is_err()));
    assert!(materials.iter().all(|h| {
        world
            .resource::<Assets<StandardMaterial>>()
            .get(h)
            .is_none()
    }));
    assert!(
        meshes
            .iter()
            .all(|h| world.resource::<Assets<Mesh>>().get(h).is_none())
    );
    assert!(
        textures
            .iter()
            .all(|h| world.resource::<Assets<Image>>().get(h).is_none())
    );
    assert!(
        world
            .resource::<Assets<ScatteringMedium>>()
            .get(&medium)
            .is_none()
    );
    assert!(c.begin_capture(&output_path(), Some((0, 0))).is_err());
    let q: serde_json::Value = serde_json::from_str(&m.request(0).unwrap()).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../tests/fixtures/reply.json")).unwrap();
    reply["request_id"] = q["request_id"].clone();
    m.accept(&reply.to_string()).unwrap();
    c.populate(&mut world, &m).unwrap();
    assert_eq!(c.selected(), None);
    assert!(c.materials.iter().all(|h| !materials.contains(h)));
}
#[test]
fn capture_cannot_span_reset() {
    let (mut w, m, mut c) = setup();
    c.begin_capture(&output_path(), Some((0, 0))).unwrap();
    assert!(c.reset(&mut w, &m).is_err());
    assert!(c.entity_count() > 0);
    c.end_capture();
    c.reset(&mut w, &m).unwrap();
}
#[test]
fn replacement_optional_inventory_and_atomic_camera_preparation() {
    let (mut world, mut m, mut c) = setup();
    let mut initial: serde_json::Value =
        serde_json::from_str(include_str!("../tests/fixtures/initial.json")).unwrap();
    initial["system"]["moons"] = serde_json::json!([]);
    initial["system"]["wanderers"] = serde_json::json!([]);
    initial["moons"]["moons"] = serde_json::json!([]);
    initial["binding"]["source_revision"] = serde_json::json!("f".repeat(40));
    m.reset(&initial.to_string()).unwrap();
    c.reset(&mut world, &m).unwrap();
    assert_eq!(c.entity_count(), 0);
    let request: serde_json::Value = serde_json::from_str(&m.request(0).unwrap()).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../tests/fixtures/reply.json")).unwrap();
    reply["request_id"] = request["request_id"].clone();
    reply["binding"] = initial["binding"].clone();
    reply["astronomy"]["bodies"]
        .as_array_mut()
        .unwrap()
        .retain(|b| b["kind"] == "anchor" || b["kind"] == "star");
    m.accept(&reply.to_string()).unwrap();
    let camera = world.spawn(Transform::IDENTITY).id();
    let mut pose = CameraPose {
        eye_km: [30000., 0., 0.],
        target_km: [0.; 3],
        up: [0., 0., 1.],
        vertical_fov_radians: 0.7,
        focus_distance_km: 30000.,
    };
    pose.eye_km = [0.; 3];
    assert!(
        c.apply(
            &mut world,
            &m,
            &pose,
            SceneTarget {
                camera,
                width: 1920,
                height: 1080
            }
        )
        .is_err()
    );
    assert_eq!(c.entity_count(), 0);
    pose.eye_km = [30000., 0., 0.];
    c.apply(
        &mut world,
        &m,
        &pose,
        SceneTarget {
            camera,
            width: 1920,
            height: 1080,
        },
    )
    .unwrap();
    apply_pending_scene(&mut world);
    assert_eq!(world.query::<&BodyVisual>().iter(&world).count(), 1);
    assert_eq!(world.query::<&PointVisual>().iter(&world).count(), 0);
    // One physical anchor and one nonselectable cosmetic cloud material.
    assert_eq!(world.query::<&CosmeticCloud>().iter(&world).count(), 1);
    assert_eq!(c.materials.len(), 2);
    assert_eq!(world.resource::<AppliedObservation>().0, Some((1, 0)));
    c.select("star:0", &mut world).unwrap();
    assert_eq!(c.selected(), Some("star:0"));
}

fn output_path() -> std::path::PathBuf {
    std::env::temp_dir().join(format!(
        "planetarium-capture-guard-{}.png",
        std::process::id()
    ))
}

#[test]
fn surface_features_follow_subsequent_anchor_transforms_only_for_current_identity() {
    let (mut world, mirror, mut catalog) = setup();
    let visual = SurfaceFeatureVisual {
        feature: SurfaceFeatureId {
            kind: "channel_reach".into(),
            macro_anchor: 1,
            ordinal: 0,
        },
        binding: mirror.initial().binding.clone(),
        generation: mirror.generation(),
        key: SurfacePatchCacheKey {
            revision: "current".into(),
            macro_face: 1 << 17,
            child_path: vec![],
        },
    };
    let current = world.spawn((visual.clone(), Transform::IDENTITY)).id();
    let mut stale = visual;
    stale.generation += 1;
    let stale = world.spawn((stale, Transform::IDENTITY)).id();
    let camera = world.spawn(Transform::IDENTITY).id();
    let pose = CameraPose {
        eye_km: [30000., 0., 0.],
        target_km: [0.; 3],
        up: [0., 0., 1.],
        vertical_fov_radians: 0.7,
        focus_distance_km: 30000.,
    };
    for angle in [0.4, 0.8] {
        catalog
            .apply(
                &mut world,
                &mirror,
                &pose,
                SceneTarget {
                    camera,
                    width: 1920,
                    height: 1080,
                },
            )
            .unwrap();
        let expected = Transform::from_translation(Vec3::new(3., 4., 5.))
            .with_rotation(Quat::from_rotation_z(angle));
        world
            .resource_mut::<PendingScene>()
            .0
            .as_mut()
            .unwrap()
            .transforms
            .insert("anchor".into(), expected);
        apply_pending_scene(&mut world);
        assert_eq!(*world.get::<Transform>(current).unwrap(), expected);
        assert_eq!(*world.get::<Transform>(stale).unwrap(), Transform::IDENTITY);
    }
}
#[test]
fn capture_preconditions_do_not_poison_but_actual_failure_requires_rebuild() {
    let (mut world, m, mut c) = setup();
    assert!(
        c.begin_capture(
            &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml"),
            Some((0, 0))
        )
        .is_err()
    );
    assert!(c.begin_capture(&output_path(), None).is_err());
    c.begin_capture(&output_path(), Some((0, 0))).unwrap();
    c.finish_capture(&Err(ViewError::Capture("readback failed".into())));
    assert!(c.reset(&mut world, &m).is_err());
    assert!(c.begin_capture(&output_path(), Some((0, 0))).is_err());
}
#[test]
fn reset_discards_queued_scene_before_application() {
    let (mut world, mut mirror, mut catalog) = setup();
    let camera = world.spawn(Transform::IDENTITY).id();
    let pose = CameraPose {
        eye_km: [30000., 0., 0.],
        target_km: [0.; 3],
        up: [0., 0., 1.],
        vertical_fov_radians: 0.7,
        focus_distance_km: 30000.,
    };
    catalog
        .apply(
            &mut world,
            &mirror,
            &pose,
            SceneTarget {
                camera,
                width: 1920,
                height: 1080,
            },
        )
        .unwrap();
    assert!(world.resource::<PendingScene>().0.is_some());
    mirror
        .reset(include_str!("../tests/fixtures/initial.json"))
        .unwrap();
    catalog.reset(&mut world, &mirror).unwrap();
    apply_pending_scene(&mut world);
    assert_eq!(catalog.entity_count(), 0);
    assert_eq!(world.query::<&BodyVisual>().iter(&world).count(), 0);
    assert_eq!(world.query::<&PointVisual>().iter(&world).count(), 0);
    assert_eq!(world.query::<&StarLight>().iter(&world).count(), 0);
    assert_eq!(world.query::<&StellarPoint>().iter(&world).count(), 0);
    assert_eq!(world.query::<&Atmosphere>().iter(&world).count(), 0);
    assert_eq!(world.resource::<AppliedObservation>().0, None);
}
