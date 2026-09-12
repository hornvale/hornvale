//! Validated camera-relative transforms; no ECS mutation occurs during preparation.
use crate::{
    CameraPose, ObservationMirror, ViewError, astronomy::lighting, coordinates::render_position,
    lifecycle::KM_PER_UNIT,
};
use bevy::prelude::*;
pub(crate) struct Geometry {
    pub transforms: std::collections::BTreeMap<String, Transform>,
    pub lights: std::collections::BTreeMap<String, (Transform, f32, f32)>,
    pub camera_transform: Transform,
}
pub(crate) fn prepare(
    mirror: &ObservationMirror,
    pose: &CameraPose,
) -> Result<Geometry, ViewError> {
    let reply = mirror
        .current()
        .ok_or_else(|| ViewError::Document("no current observation".into()))?;
    // Validate every conversion before touching ECS: a bad snapshot never half-applies.
    let camera_transform = pose.transform(KM_PER_UNIT)?;
    let mut transforms = std::collections::BTreeMap::new();
    for body in &reply.astronomy.bodies {
        if let Some(radius) = body.radius_km {
            let relief = if body.id == "anchor" {
                mirror.initial().tiles.max_relief_km()
            } else {
                0.0
            };
            pose.validate_body(body.position_km, radius + relief)?;
        }
        let position =
            Vec3::from_array(render_position(body.position_km, pose.eye_km, KM_PER_UNIT)?);
        let rotation = body
            .body_to_frame
            .map(|c| {
                Quat::from_mat3(&Mat3::from_cols_array_2d(
                    &c.map(|col| col.map(|x| x as f32)),
                ))
            })
            .unwrap_or(Quat::IDENTITY);
        transforms.insert(
            body.id.clone(),
            Transform::from_translation(position).with_rotation(rotation),
        );
    }
    let mut lights = std::collections::BTreeMap::new();
    let anchor = reply
        .astronomy
        .bodies
        .iter()
        .find(|b| b.id == "anchor")
        .expect("validated anchor");
    for light in &reply.astronomy.lights {
        let star = reply
            .astronomy
            .bodies
            .iter()
            .find(|b| b.id == light.star_id)
            .expect("validated light star");
        let (direction, flux) = lighting::at_body(light, star, anchor)?;
        let direction = Vec3::from_array(direction);
        let up = if direction.dot(Vec3::Z).abs() < 0.99 {
            Vec3::Z
        } else {
            Vec3::Y
        };
        let intensity = lighting::point_intensity(light.luminosity_rel, KM_PER_UNIT)?;
        lights.insert(
            light.star_id.clone(),
            (
                Transform::IDENTITY.looking_to(-direction, up),
                127_000.0 * flux,
                intensity,
            ),
        );
    }
    Ok(Geometry {
        transforms,
        lights,
        camera_transform,
    })
}
