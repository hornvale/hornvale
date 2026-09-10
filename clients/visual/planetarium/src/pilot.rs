use hornvale_bevy_view::{CameraPose, ObservationMirror, ViewError, bevy::math::DVec3};
use serde::{Deserialize, Serialize};
#[derive(Deserialize, Serialize)]
pub struct Pilot {
    pub schema: String,
    pub start_ticks: i64,
    pub step_ticks: i64,
    pub frames: u32,
    pub width: u32,
    pub height: u32,
    pub shot: String,
    #[serde(default)]
    pub body_only: bool,
    #[serde(default)]
    pub font_path: Option<String>,
}
pub fn pose(mirror: &ObservationMirror, shot: &str) -> Result<CameraPose, ViewError> {
    let a = &mirror
        .current()
        .ok_or_else(|| ViewError::Document("camera needs observation".into()))?
        .astronomy;
    let anchor = a
        .bodies
        .iter()
        .find(|b| b.id == "anchor")
        .expect("validated anchor");
    let radius = anchor.radius_km.expect("validated radius");
    let (eye, target, fov) = match shot {
        "limb" => {
            let light = DVec3::from_array(a.lights[0].direction_from_anchor);
            let side = light.cross(DVec3::Z).normalize();
            let eye = (light * 0.30 + side * 0.88 + DVec3::Z * 0.26).normalize() * radius * 3.5;
            (eye, side * radius * 0.42, 0.80)
        }
        "companions" => {
            let moon = a
                .bodies
                .iter()
                .find(|b| b.id == "moon:0")
                .ok_or_else(|| ViewError::Document("companions shot requires moon:0".into()))?;
            let m = DVec3::from_array(moon.position_km);
            let side = m.normalize().cross(DVec3::Z).normalize();
            let eye = m * 1.55 + side * radius * 2.1 + DVec3::Z * radius * 0.45;
            let aim = ((-eye).normalize() + (m - eye).normalize()).normalize();
            let target = eye + aim * eye.length();
            (eye, target, 0.078)
        }
        _ => return Err(ViewError::Document("unknown draft shot".into())),
    };
    Ok(CameraPose {
        eye_km: eye.to_array(),
        target_km: target.to_array(),
        up: DVec3::Z.to_array(),
        vertical_fov_radians: fov,
        focus_distance_km: eye.distance(target),
    })
}
