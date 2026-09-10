use hornvale_bevy_view::{
    CameraPose,
    camera::{BodyBound, OrbitCamera, PickTarget},
};
use planetarium::controls::Playback;
#[test]
fn pause_reverse_scrub_and_rate_are_bounded() {
    let mut p = Playback::default();
    p.advance(1., 300, 30);
    assert_eq!(p.frame(), 0);
    p.paused = false;
    p.advance(1., 300, 30);
    assert_eq!(p.frame(), 30);
    p.set_rate(-2.);
    p.advance(0.5, 300, 30);
    assert_eq!(p.frame(), 0);
    p.seek(150, 300);
    assert_eq!(p.frame(), 150);
    p.set_rate(f64::NAN);
    assert_eq!(p.rate, -2.);
    p.seek(999, 300);
    assert_eq!(p.frame(), 299);
}
fn pose() -> CameraPose {
    CameraPose {
        eye_km: [0., -40., 10.],
        target_km: [0.; 3],
        up: [0., 0., 1.],
        vertical_fov_radians: 0.8,
        focus_distance_km: 40.,
    }
}
#[test]
fn orbit_pan_dolly_focus_keep_physical_bounds_and_reset_is_exact() {
    let initial = pose();
    let bounds = [BodyBound {
        id: "anchor".into(),
        position_km: [0.; 3],
        outer_radius_km: 10.,
    }];
    let mut c = OrbitCamera::new(initial.clone());
    c.orbit(0.3, 0.1, &bounds).unwrap();
    c.pan(0.1, 0.1, &bounds).unwrap();
    c.dolly(100., &bounds).unwrap();
    c.pose.validate_body([0.; 3], 10.).unwrap();
    c.focus(&bounds[0], &bounds).unwrap();
    c.pose.validate_body([0.; 3], 10.).unwrap();
    c.reset(initial.clone());
    assert_eq!(
        serde_json::to_string(&c.pose).unwrap(),
        serde_json::to_string(&initial).unwrap()
    );
}
#[test]
fn picking_uses_screen_markers_without_enlarging_bodies() {
    let c = OrbitCamera::new(pose());
    let b = PickTarget {
        id: "anchor".into(),
        position_km: [0.; 3],
        physical_radius_km: Some(10.),
    };
    assert_eq!(
        c.pick([0., 0.], 16. / 9., &[b], 0.03).as_deref(),
        Some("anchor")
    );
}

#[test]
fn pointer_gesture_keeps_the_surface_that_received_the_press() {
    use planetarium::controls::{DragOwner, PointerGesture};
    let mut gesture = PointerGesture::default();
    gesture.begin(true);
    assert_eq!(gesture.owner(false), DragOwner::Ui);
    gesture.end();
    gesture.begin(false);
    assert_eq!(gesture.owner(true), DragOwner::Scene);
    gesture.end();
    assert_eq!(gesture.owner(true), DragOwner::Ui);
}

#[test]
fn extreme_outward_scroll_clamps_to_supported_camera_range() {
    let mut camera = OrbitCamera::new(pose());
    let bounds = [BodyBound {
        id: "anchor".into(),
        position_km: [0.; 3],
        outer_radius_km: 10.,
    }];
    camera.dolly(-10000., &bounds).unwrap();
    let distance = camera.pose.eye_km.iter().map(|x| x * x).sum::<f64>().sqrt();
    assert!(distance <= 2e9);
}

#[test]
fn app_candidates_pick_radiusless_wanderer_and_focus_without_moving_eye() {
    use hornvale_bevy_view::ObservationMirror;
    let mut mirror =
        ObservationMirror::new(include_str!("../../../bevy/tests/fixtures/initial.json")).unwrap();
    let request: serde_json::Value = serde_json::from_str(&mirror.request(0).unwrap()).unwrap();
    let mut reply: serde_json::Value =
        serde_json::from_str(include_str!("../../../bevy/tests/fixtures/reply.json")).unwrap();
    reply["request_id"] = request["request_id"].clone();
    assert!(mirror.accept(&reply.to_string()).unwrap());
    let bounds = planetarium::live::bounds(&mirror);
    let targets = planetarium::live::pick_targets(&mirror);
    let marker = targets.iter().find(|b| b.id == "wanderer:0").unwrap();
    assert_eq!(marker.physical_radius_km, None);
    assert!(!bounds.iter().any(|b| b.id == marker.id));
    let mut camera = OrbitCamera::new(CameraPose {
        eye_km: [0., -100000., 10000.],
        target_km: marker.position_km,
        ..pose()
    });
    assert_eq!(
        camera.pick([0., 0.], 16. / 9., &targets, 0.02).as_deref(),
        Some("wanderer:0")
    );
    let eye = camera.pose.eye_km;
    camera.pose.target_km = [0.; 3];
    camera.focus_target(marker, &bounds).unwrap();
    assert_eq!(camera.pose.eye_km, eye);
    assert_eq!(camera.pose.target_km, marker.position_km);
    for b in bounds {
        camera
            .pose
            .validate_body(b.position_km, b.outer_radius_km)
            .unwrap();
    }
}
