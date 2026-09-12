//! Application-owned direction. Every sample is a pure function of frame and JSON positions.
use hornvale_bevy_view::{Binding, CameraPose, FilmClock, ViewError};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FilmDefinition {
    pub schema: String,
    pub width: u32,
    pub height: u32,
    pub fps: u32,
    pub frames: u32,
    pub binding: Binding,
    pub start_ticks: i64,
    pub end_ticks: i64,
    pub supported_ticks: [i64; 2],
    pub presentation_seed: u32,
    pub settings: hornvale_bevy_view::camera::ViewSettings,
    pub shots: Vec<Shot>,
}
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Shot {
    pub start_frame: u32,
    pub end_frame: u32,
    pub target_body_id: String,
    pub start_offset_km: [f64; 3],
    pub end_offset_km: [f64; 3],
    pub target_offset_km: [f64; 3],
    pub up: [f64; 3],
    pub vertical_fov_radians: f64,
    pub focus_distance_km: f64,
    pub caption: String,
}
impl FilmDefinition {
    pub fn clock(&self) -> FilmClock {
        FilmClock {
            start_ticks: self.start_ticks,
            end_ticks: self.end_ticks,
            frames: self.frames,
        }
    }
    pub fn validate(&self, binding: &Binding) -> Result<(), ViewError> {
        self.binding.validate()?;
        if self.settings != hornvale_bevy_view::camera::ViewSettings::default()
            || self.presentation_seed != 42
        {
            return Err(ViewError::Range("unqualified presentation settings".into()));
        }
        if &self.binding != binding {
            return Err(ViewError::Binding("film/source binding mismatch".into()));
        }
        if self.schema != "planetarium/film/v1"
            || self.width != 3840
            || self.height != 2160
            || self.fps != 30
            || self.frames != 300
            || self.supported_ticks != [0, 3600]
            || [self.start_ticks, self.end_ticks]
                .iter()
                .any(|t| !(self.supported_ticks[0]..=self.supported_ticks[1]).contains(t))
        {
            return Err(ViewError::Range(
                "incompatible pilot dimensions, rate, count or qualified interval".into(),
            ));
        }
        let mut end = 0;
        for shot in &self.shots {
            if shot.start_frame != end
                || shot.end_frame <= end
                || shot.end_frame > self.frames
                || !["anchor", "moon:0", "moon:1"].contains(&shot.target_body_id.as_str())
                || shot.caption.trim().is_empty()
            {
                return Err(ViewError::Document(
                    "shot gap, overlap or unsupported target/caption".into(),
                ));
            }
            for frame in [shot.start_frame, shot.end_frame - 1] {
                sample_shot(
                    self,
                    frame,
                    &BTreeMap::from([(shot.target_body_id.clone(), [0.; 3])]),
                )?
                .transform(hornvale_bevy_view::lifecycle::KM_PER_UNIT)?;
            }
            end = shot.end_frame;
        }
        if end != self.frames {
            return Err(ViewError::Document("shots do not cover the film".into()));
        }
        Ok(())
    }
    pub fn shot(&self, frame: u32) -> Result<&Shot, ViewError> {
        if frame >= self.frames {
            return Err(ViewError::Range("frame outside film".into()));
        }
        let mut matches = self
            .shots
            .iter()
            .filter(|s| s.start_frame <= frame && frame < s.end_frame);
        let shot = matches
            .next()
            .ok_or_else(|| ViewError::Document("shot gap".into()))?;
        if matches.next().is_some() {
            return Err(ViewError::Document("shot overlap".into()));
        }
        Ok(shot)
    }
}
pub fn smoothstep(u: f64) -> f64 {
    let u = u.clamp(0., 1.);
    u * u * (3. - 2. * u)
}
pub fn sample_shot(
    film: &FilmDefinition,
    frame: u32,
    body_positions: &BTreeMap<String, [f64; 3]>,
) -> Result<CameraPose, ViewError> {
    let s = film.shot(frame)?;
    let p = body_positions
        .get(&s.target_body_id)
        .ok_or_else(|| ViewError::Document(format!("missing shot target {}", s.target_body_id)))?;
    let u = smoothstep(
        f64::from(frame - s.start_frame) / f64::from((s.end_frame - s.start_frame - 1).max(1)),
    );
    Ok(CameraPose {
        eye_km: std::array::from_fn(|i| {
            p[i] + s.start_offset_km[i] * (1. - u) + s.end_offset_km[i] * u
        }),
        target_km: std::array::from_fn(|i| p[i] + s.target_offset_km[i]),
        up: s.up,
        vertical_fov_radians: s.vertical_fov_radians,
        focus_distance_km: s.focus_distance_km,
    })
}
pub fn sample_caption(film: &FilmDefinition, frame: u32) -> Result<&str, ViewError> {
    Ok(&film.shot(frame)?.caption)
}
