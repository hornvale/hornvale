#![allow(
    clippy::disallowed_methods,
    reason = "screen-projection reference math is client-side and must not import the simulation kernel"
)]
use hornvale_bevy_view::coordinates::render_position;
#[test]
fn subtracts_before_converting_to_float() {
    let near = [1000.125, 240.25, -4.5];
    let origin = [1e12, 1e12, 1e12];
    let far = std::array::from_fn(|i| origin[i] + near[i]);
    assert_eq!(
        render_position(near, [0.; 3], 1000.).unwrap(),
        render_position(far, origin, 1000.).unwrap()
    );
}
#[test]
fn bounds_projection_error_to_quarter_pixel_at_4k() {
    for origin in [[0.; 3], [1e12, -1e12, 1e12]] {
        for distance in [8000., 20000., 307744., 1e7] {
            for offset in [1599., 7126., -2274.] {
                let relative = [offset, offset * 0.4, distance];
                let p = std::array::from_fn(|i| origin[i] + relative[i]);
                let actual = render_position(p, origin, 1000.).unwrap();
                let focal = 2160.0 / (2.0 * (0.005_f64 / 2.0).tan());
                let expected = (p[0] - origin[0]) / (p[2] - origin[2]) * focal;
                let projected = f64::from(actual[0]) / f64::from(actual[2]) * focal;
                assert!(
                    (expected - projected).abs() < 0.25,
                    "{expected} {projected}"
                );
            }
        }
    }
}
#[test]
fn refuses_invalid_scale_and_range() {
    for scale in [0., -1., f64::NAN] {
        assert!(render_position([0.; 3], [0.; 3], scale).is_err());
    }
    assert!(render_position([f64::INFINITY; 3], [0.; 3], 1.).is_err());
    assert!(render_position([1e15; 3], [0.; 3], 1000.).is_err());
}

#[test]
fn real_rotated_camera_and_body_projection_stays_within_quarter_pixel() {
    use hornvale_bevy_view::{
        CameraPose,
        bevy::{
            camera::CameraProjection,
            math::{DMat4, DQuat, DVec3},
            prelude::*,
        },
    };
    let mut worst = 0.0_f64;
    let mut samples = 0;
    for origin in [DVec3::ZERO, DVec3::new(1e12, -1e12, 1e12)] {
        for direction in [
            DVec3::NEG_Z,
            DVec3::new(0.3, -0.4, -0.85).normalize(),
            DVec3::new(1.0, 0.9, 0.2).normalize(),
            DVec3::new(0.0002, 1.0, -0.0001).normalize(),
        ] {
            for distance in [1.0, 8000.0, 20000.0, 307744.0, 1e6, 2e6, 1e7, 2e10] {
                for fov in [0.005_f64, 0.078, 0.80, 2.5] {
                    let pose = CameraPose {
                        eye_km: origin.to_array(),
                        target_km: (origin + direction * distance).to_array(),
                        up: DVec3::Y.to_array(),
                        vertical_fov_radians: fov,
                        focus_distance_km: distance,
                    };
                    let camera = pose.transform(1000.0).unwrap();
                    let view = Mat4::from(camera.compute_affine().inverse());
                    let projection = PerspectiveProjection {
                        fov: fov as f32,
                        aspect_ratio: 3840.0 / 2160.0,
                        near: 0.0001,
                        far: 2e7,
                        ..default()
                    }
                    .get_clip_from_view();
                    let actual_direction = (DVec3::from_array(pose.target_km) - origin).normalize();
                    let right = actual_direction.cross(DVec3::Y).normalize();
                    let up = right.cross(actual_direction);
                    let reference_view = DMat4::look_at_rh(DVec3::ZERO, actual_direction, DVec3::Y);
                    let reference_projection =
                        DMat4::perspective_infinite_reverse_rh(fov, 3840.0 / 2160.0, 0.0001);
                    for (radius, screen_x, screen_y) in [
                        (0.001, 0.0, 0.0),
                        (2274.7776, 0.0, 0.0),
                        (7126.4059, 0.45, -0.35),
                        (1599.1254, -0.5, 0.4),
                        (1e6, 0.0, 0.0),
                        (999999.8, 0.0, 0.0),
                        (distance / 2.0, 0.0, 0.0),
                    ] {
                        let center = origin
                            + actual_direction * distance
                            + (right * screen_x + up * screen_y) * distance * (fov / 2.0).tan();
                        // Test only positions in the declared coordinate envelope.
                        if (center - origin).abs().max_element() > 2e10 {
                            continue;
                        }
                        if radius > 1e6 || pose.validate_body(center.to_array(), radius).is_err() {
                            continue;
                        }
                        let rotation =
                            DQuat::from_axis_angle(DVec3::new(0.3, 0.5, 0.8).normalize(), 0.73);
                        let body = Transform::from_translation(Vec3::from_array(
                            render_position(center.to_array(), origin.to_array(), 1000.0).unwrap(),
                        ))
                        .with_rotation(Quat::from_array(rotation.to_array().map(|v| v as f32)));
                        for local in [
                            DVec3::ZERO,
                            DVec3::X * radius,
                            DVec3::Y * radius,
                            DVec3::Z * radius,
                            DVec3::new(1.0, 2.0, -1.0).normalize() * radius,
                            rotation.inverse() * (-actual_direction * radius),
                        ] {
                            let reference_point =
                                (center - origin) / 1000.0 + rotation * (local / 1000.0);
                            let expected_clip =
                                reference_projection * reference_view * reference_point.extend(1.0);
                            let expected = expected_clip.truncate() / expected_clip.w;
                            if expected.z < 0.0
                                || expected.z > 1.0
                                || expected_clip.w <= 0.0
                                || expected.x.abs() > 1.0
                                || expected.y.abs() > 1.0
                            {
                                continue;
                            }
                            let clip = projection
                                * view
                                * body.to_matrix()
                                * Vec3::from_array((local / 1000.0).to_array().map(|x| x as f32))
                                    .extend(1.0);
                            let actual = clip.truncate() / clip.w;
                            let error_x = (f64::from(actual.x) - expected.x).abs() * 1920.0;
                            let error_y = (f64::from(actual.y) - expected.y).abs() * 1080.0;
                            worst = worst.max(error_x).max(error_y);
                            assert!(
                                error_x < 0.25 && error_y < 0.25,
                                "projection error {error_x},{error_y}; distance={distance} fov={fov} direction={direction:?}"
                            );
                            samples += 1;
                        }
                    }
                }
            }
        }
    }
    assert!(samples > 1000);
    println!("full f32 camera/body projection: {samples} visible samples; worst={worst:.9} pixels");
}

#[test]
fn orbital_envelope_rejects_near_surface_precision_loss() {
    use hornvale_bevy_view::CameraPose;
    let pose = CameraPose {
        eye_km: [0.0; 3],
        target_km: [0.0, 0.0, -1e6],
        up: [0.0, 1.0, 0.0],
        vertical_fov_radians: 0.005,
        focus_distance_km: 1e6,
    };
    assert!(pose.validate_body([0.0, 0.0, -1e6], 999999.8).is_err());
    assert!(pose.validate_body([0.0, 0.0, -1e6], 500000.0).is_ok());
    assert!(pose.validate_body([0.0, 0.0, -1e6], 500000.001).is_err());
}
