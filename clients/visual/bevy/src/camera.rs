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
    /// The f32 scene is qualified for orbital views, not surface landings.
    /// Keep the center at least two outer radii away to bound subtraction error
    /// between a camera-relative body translation and mesh-local vertices.
    pub fn validate_body(
        &self,
        position_km: [f64; 3],
        outer_radius_km: f64,
    ) -> Result<(), ViewError> {
        let delta = std::array::from_fn::<_, 3, _>(|i| position_km[i] - self.eye_km[i]);
        let distance = delta.iter().map(|x| x * x).sum::<f64>().sqrt();
        if !outer_radius_km.is_finite()
            || outer_radius_km <= 0.0
            || !distance.is_finite()
            || distance < 2.0 * outer_radius_km
        {
            return Err(ViewError::Range(
                "orbital camera requires center distance >= twice outer body radius".into(),
            ));
        }
        Ok(())
    }

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
