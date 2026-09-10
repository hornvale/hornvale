use crate::{ViewError, coordinates::render_position};
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
/// Camera coordinates are kilometres in the observation frame, not render units.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CameraPose {
    pub eye_km: [f64; 3],
    pub target_km: [f64; 3],
    pub up: [f64; 3],
    pub vertical_fov_radians: f64,
    pub focus_distance_km: f64,
}
impl CameraPose {
    pub fn transform(&self, scale: f64) -> Result<Transform, ViewError> {
        if !(0.005..=2.5).contains(&self.vertical_fov_radians)
            || !self.focus_distance_km.is_finite()
            || self.focus_distance_km <= 0.
        {
            return Err(ViewError::Range(
                "camera outside supported FOV/focus range".into(),
            ));
        }
        let target = Vec3::from_array(render_position(self.target_km, self.eye_km, scale)?);
        let up = Vec3::from_array(self.up.map(|x| x as f32));
        if !up.is_finite()
            || up.length_squared() < 1e-12
            || target.length_squared() < 1e-12
            || target.normalize().cross(up.normalize()).length_squared() < 1e-8
        {
            return Err(ViewError::Range("degenerate camera basis".into()));
        }
        Ok(Transform::IDENTITY.looking_at(target, up))
    }
}
