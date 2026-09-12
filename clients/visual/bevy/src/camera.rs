#![allow(
    clippy::disallowed_methods,
    reason = "client camera math must not import the simulation kernel"
)]
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

/// Kilometre bounds from the accepted document; markers do not affect these.
#[derive(Clone, Debug)]
pub struct BodyBound {
    pub id: String,
    pub position_km: [f64; 3],
    pub outer_radius_km: f64,
}
/// Selection geometry is independent of collision bounds. Absent radii remain
/// screen-space markers and never acquire a physical size through picking.
#[derive(Clone, Debug)]
pub struct PickTarget {
    pub id: String,
    pub position_km: [f64; 3],
    pub physical_radius_km: Option<f64>,
}
#[derive(Clone, Debug)]
pub struct OrbitCamera {
    pub pose: CameraPose,
}
impl OrbitCamera {
    pub fn new(pose: CameraPose) -> Self {
        Self { pose }
    }
    pub fn reset(&mut self, pose: CameraPose) {
        self.pose = pose;
    }
    fn basis(&self) -> (bevy::math::DVec3, bevy::math::DVec3, bevy::math::DVec3) {
        use bevy::math::DVec3;
        let forward = (DVec3::from_array(self.pose.target_km)
            - DVec3::from_array(self.pose.eye_km))
        .normalize();
        let right = forward.cross(DVec3::from_array(self.pose.up)).normalize();
        (forward, right, right.cross(forward).normalize())
    }
    fn commit(&mut self, pose: CameraPose, bounds: &[BodyBound]) -> Result<(), ViewError> {
        pose.transform(crate::lifecycle::KM_PER_UNIT)?;
        for b in bounds {
            pose.validate_body(b.position_km, b.outer_radius_km)?;
        }
        if bevy::math::DVec3::from_array(pose.eye_km).length() > 2e9 {
            return Err(ViewError::Range(
                "camera exceeds qualified 2 billion km origin range".into(),
            ));
        }
        self.pose = pose;
        Ok(())
    }
    pub fn orbit(&mut self, yaw: f64, pitch: f64, bounds: &[BodyBound]) -> Result<(), ViewError> {
        use bevy::math::{DQuat, DVec3};
        let mut p = self.pose.clone();
        let target = DVec3::from_array(p.target_km);
        let (_, right, _) = self.basis();
        let rotation = DQuat::from_axis_angle(DVec3::from_array(p.up).normalize(), yaw)
            * DQuat::from_axis_angle(right, pitch);
        p.eye_km = (target + rotation * (DVec3::from_array(p.eye_km) - target)).to_array();
        self.commit(p, bounds)
    }
    pub fn pan(&mut self, x: f64, y: f64, bounds: &[BodyBound]) -> Result<(), ViewError> {
        use bevy::math::DVec3;
        let mut p = self.pose.clone();
        let (_, right, up) = self.basis();
        let distance = DVec3::from_array(p.eye_km).distance(DVec3::from_array(p.target_km));
        let delta = (right * x + up * y) * distance;
        p.eye_km = (DVec3::from_array(p.eye_km) + delta).to_array();
        p.target_km = (DVec3::from_array(p.target_km) + delta).to_array();
        self.commit(p, bounds)
    }
    pub fn dolly(&mut self, amount: f64, bounds: &[BodyBound]) -> Result<(), ViewError> {
        use bevy::math::DVec3;
        let mut p = self.pose.clone();
        let target = DVec3::from_array(p.target_km);
        let eye = DVec3::from_array(p.eye_km);
        let proposed = target + (eye - target) * (-amount.clamp(-20., 20.)).exp();
        // Clamp along the requested segment at the first body exclusion sphere.
        let mut fraction = 1.0_f64;
        let delta = proposed - eye;
        for b in bounds {
            let offset = eye - DVec3::from_array(b.position_km);
            let a = delta.length_squared();
            let c = offset.length_squared() - (2. * b.outer_radius_km).powi(2);
            let q = offset.dot(delta);
            let discriminant = q * q - a * c;
            if a > 0. && discriminant >= 0. {
                let entry = (-q - discriminant.sqrt()) / a;
                if (0.0..=1.0).contains(&entry) {
                    fraction = fraction.min(entry * 0.999999);
                }
            }
        }
        if proposed.length() > 2e9 {
            let a = delta.length_squared();
            let q = eye.dot(delta);
            let c = eye.length_squared() - 4e18;
            let exit = (-q + (q * q - a * c).max(0.).sqrt()) / a;
            fraction = fraction.min(exit.clamp(0., 1.) * 0.999999);
        }
        p.eye_km = (eye + delta * fraction).to_array();
        p.focus_distance_km = DVec3::from_array(p.eye_km).distance(target).max(0.001);
        self.commit(p, bounds)
    }
    pub fn focus(&mut self, body: &BodyBound, bounds: &[BodyBound]) -> Result<(), ViewError> {
        use bevy::math::DVec3;
        let mut p = self.pose.clone();
        let target = DVec3::from_array(body.position_km);
        let direction = (DVec3::from_array(p.eye_km) - target).normalize_or_zero();
        p.target_km = body.position_km;
        p.eye_km = (target + direction * body.outer_radius_km * 3.5).to_array();
        p.focus_distance_km = body.outer_radius_km * 2.5;
        p.vertical_fov_radians = 0.8;
        self.commit(p, bounds)
    }
    /// Resolved bodies use their physical bound. For unresolved points, keep the
    /// safe eye position and aim/focus at the emitted position, without a radius.
    pub fn focus_target(
        &mut self,
        target: &PickTarget,
        bounds: &[BodyBound],
    ) -> Result<(), ViewError> {
        if let Some(body) = bounds.iter().find(|b| b.id == target.id) {
            return self.focus(body, bounds);
        }
        let mut pose = self.pose.clone();
        pose.target_km = target.position_km;
        pose.focus_distance_km = bevy::math::DVec3::from_array(pose.eye_km)
            .distance(bevy::math::DVec3::from_array(target.position_km));
        self.commit(pose, bounds)
    }
    /// Normalized screen coordinates, y up; resolve closest visible body/marker.
    pub fn pick(
        &self,
        screen: [f64; 2],
        aspect: f64,
        targets: &[PickTarget],
        marker_radius: f64,
    ) -> Option<String> {
        use bevy::math::DVec3;
        let (forward, right, up) = self.basis();
        let tangent = (self.pose.vertical_fov_radians / 2.).tan();
        targets
            .iter()
            .filter_map(|b| {
                let delta = DVec3::from_array(b.position_km) - DVec3::from_array(self.pose.eye_km);
                let z = delta.dot(forward);
                if z <= 0. {
                    return None;
                }
                let xy = [
                    delta.dot(right) / (z * tangent * aspect),
                    delta.dot(up) / (z * tangent),
                ];
                let radius =
                    (b.physical_radius_km.unwrap_or(0.) / (z * tangent)).max(marker_radius);
                (((screen[0] - xy[0]) * aspect).hypot(screen[1] - xy[1]) <= radius)
                    .then_some((z, b.id.clone()))
            })
            .min_by(|a, b| a.0.total_cmp(&b.0))
            .map(|(_, id)| id)
    }
}

/// The qualified shared treatment used by both the visible and offscreen cameras.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ViewSettings {
    pub exposure_ev100: f32,
    pub water_roughness: f32,
    pub reflectance: f32,
    pub pigment_variation: f32,
    pub cloud_opacity: f32,
    pub cloud_shell_km: f32,
    pub focus_sensor_height: f32,
    pub focus_aperture_f_stops: f32,
    pub focus_max_blur_pixels: f32,
}
impl Default for ViewSettings {
    fn default() -> Self {
        Self {
            exposure_ev100: 13.3,
            water_roughness: 0.38,
            reflectance: 0.28,
            pigment_variation: 0.12,
            cloud_opacity: 0.5,
            cloud_shell_km: 12.,
            focus_sensor_height: 0.01866,
            focus_aperture_f_stops: 4.,
            focus_max_blur_pixels: 2.,
        }
    }
}
pub fn camera_components(settings: &ViewSettings) -> impl Bundle {
    use bevy::{
        camera::{Exposure, visibility::RenderLayers},
        core_pipeline::tonemapping::Tonemapping,
        pbr::{AtmosphereMode, AtmosphereSettings},
        post_process::dof::{DepthOfField, DepthOfFieldMode},
    };
    (
        Camera3d::default(),
        bevy::camera::ShadowLodOrigin,
        Transform::IDENTITY,
        Exposure {
            ev100: settings.exposure_ev100,
        },
        Tonemapping::AcesFitted,
        Msaa::Sample4,
        AtmosphereSettings {
            rendering_method: AtmosphereMode::Raymarched,
            ..default()
        },
        RenderLayers::from_layers(&[0, 1]),
        DepthOfField {
            mode: DepthOfFieldMode::Gaussian,
            focal_distance: 20.,
            sensor_height: settings.focus_sensor_height,
            aperture_f_stops: settings.focus_aperture_f_stops,
            max_circle_of_confusion_diameter: settings.focus_max_blur_pixels,
            max_depth: 2e6,
        },
    )
}
