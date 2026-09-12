//! GPU-independent, frame-tagged capture acknowledgments used by the renderer.
use crate::ViewError;
use serde::{Deserialize, Serialize};
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CaptureSettings {
    pub width: u32,
    pub height: u32,
    pub frames: u32,
    pub warmup_frames: u32,
    pub timeout_seconds: u32,
}
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum CaptureState {
    Preparing,
    AwaitingObservation { frame: u32 },
    Warming { frame: u32, remaining: u32 },
    AwaitingReadback { frame: u32 },
    Writing { frame: u32 },
    Complete,
    Failed(String),
}
/// Time is supplied by the driver; CPU tests need no device or wall clock.
pub struct CaptureMachine {
    settings: CaptureSettings,
    state: CaptureState,
    since_ms: u64,
}
impl CaptureMachine {
    pub fn new(settings: CaptureSettings) -> Result<Self, ViewError> {
        if settings.width == 0
            || settings.height == 0
            || settings.width > 7680
            || settings.height > 4320
            || settings.frames == 0
            || settings.timeout_seconds == 0
        {
            return Err(ViewError::Capture("invalid capture settings".into()));
        }
        Ok(Self {
            settings,
            state: CaptureState::Preparing,
            since_ms: 0,
        })
    }
    pub fn settings(&self) -> &CaptureSettings {
        &self.settings
    }
    pub fn state(&self) -> &CaptureState {
        &self.state
    }
    fn enter(&mut self, state: CaptureState, now_ms: u64) {
        self.state = state;
        self.since_ms = now_ms;
    }
    pub fn prepare(&mut self, now_ms: u64) -> Result<(), ViewError> {
        if self.state != CaptureState::Preparing {
            return Err(ViewError::Capture("capture already prepared".into()));
        }
        self.enter(CaptureState::AwaitingObservation { frame: 0 }, now_ms);
        Ok(())
    }
    pub fn observation_ready(&mut self, frame: u32, now_ms: u64) -> Result<(), ViewError> {
        self.check_timeout(now_ms)?;
        if self.state != (CaptureState::AwaitingObservation { frame }) {
            return Err(ViewError::Capture("unexpected observation frame".into()));
        }
        self.enter(
            if self.settings.warmup_frames == 0 {
                CaptureState::AwaitingReadback { frame }
            } else {
                CaptureState::Warming {
                    frame,
                    remaining: self.settings.warmup_frames,
                }
            },
            now_ms,
        );
        Ok(())
    }
    pub fn warmed(&mut self, frame: u32, now_ms: u64) -> Result<(), ViewError> {
        self.check_timeout(now_ms)?;
        match self.state {
            CaptureState::Warming {
                frame: f,
                remaining,
            } if f == frame => {
                if remaining == 1 {
                    self.enter(CaptureState::AwaitingReadback { frame }, now_ms);
                } else {
                    self.state = CaptureState::Warming {
                        frame,
                        remaining: remaining - 1,
                    };
                }
                Ok(())
            }
            _ => Err(ViewError::Capture("unexpected warmup frame".into())),
        }
    }
    /// An obsolete callback is ignored even when it reports a failure.
    pub fn readback(
        &mut self,
        frame: u32,
        result: Result<(), String>,
        now_ms: u64,
    ) -> Result<bool, ViewError> {
        if self.state != (CaptureState::AwaitingReadback { frame }) {
            return Ok(false);
        }
        self.check_timeout(now_ms)?;
        if let Err(error) = result {
            return Err(self.fail(&error));
        }
        self.enter(CaptureState::Writing { frame }, now_ms);
        Ok(true)
    }
    pub fn written(&mut self, frame: u32, now_ms: u64) -> Result<(), ViewError> {
        self.check_timeout(now_ms)?;
        if self.state != (CaptureState::Writing { frame }) {
            return Err(ViewError::Capture("unexpected written frame".into()));
        }
        self.enter(
            if frame + 1 == self.settings.frames {
                CaptureState::Complete
            } else {
                CaptureState::AwaitingObservation { frame: frame + 1 }
            },
            now_ms,
        );
        Ok(())
    }
    pub fn remaining_ms(&self, now_ms: u64) -> u64 {
        (u64::from(self.settings.timeout_seconds) * 1000)
            .saturating_sub(now_ms.saturating_sub(self.since_ms))
    }
    pub fn check_timeout(&mut self, now_ms: u64) -> Result<(), ViewError> {
        if let CaptureState::Failed(ref message) = self.state {
            return Err(ViewError::Capture(message.clone()));
        }
        if self.state != CaptureState::Complete && self.remaining_ms(now_ms) == 0 {
            return Err(self.fail("timeout"));
        }
        Ok(())
    }
    pub fn fail(&mut self, error: &str) -> ViewError {
        let message = format!("capture {:?}: {error}", self.state);
        self.state = CaptureState::Failed(message.clone());
        ViewError::Capture(message)
    }
}
/// Screenshot::image returns tightly packed RGBA sRGB, top row first. Verify
/// that contract before encoding and decode the encoded bytes before committing.
pub fn write_png(
    image: &bevy::prelude::Image,
    path: &std::path::Path,
    width: u32,
    height: u32,
) -> Result<(), ViewError> {
    use std::io::Write;
    let fail = |e: String| ViewError::Capture(e);
    if image.width() != width
        || image.height() != height
        || image.texture_descriptor.format
            != bevy::render::render_resource::TextureFormat::Rgba8UnormSrgb
        || image.data.as_ref().map(Vec::len) != Some(width as usize * height as usize * 4)
    {
        return Err(fail(
            "invalid readback dimensions, format or row stride".into(),
        ));
    }
    let dynamic = image
        .clone()
        .try_into_dynamic()
        .map_err(|e| fail(e.to_string()))?;
    let mut encoded = std::io::Cursor::new(Vec::new());
    dynamic
        .to_rgb8()
        .write_to(&mut encoded, image::ImageFormat::Png)
        .map_err(|e| fail(e.to_string()))?;
    let bytes = encoded.into_inner();
    let decoded = image::load_from_memory_with_format(&bytes, image::ImageFormat::Png)
        .map_err(|e| fail(e.to_string()))?;
    if decoded.width() != width
        || decoded.height() != height
        || decoded.to_rgb8() != dynamic.to_rgb8()
    {
        return Err(fail("PNG roundtrip differs from readback".into()));
    }
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
        .map_err(|e| fail(e.to_string()))?;
    file.write_all(&bytes)
        .and_then(|_| file.flush())
        .and_then(|_| file.sync_all())
        .map_err(|e| fail(e.to_string()))?;
    drop(file);
    Ok(())
}

/// Source-owned scene assets are synchronous, retained in MAIN_WORLD, and must
/// exist before extraction. Missing assets fail explicitly instead of waiting.
pub fn validate_scene_assets(
    world: &bevy::prelude::World,
    meshes: &[bevy::prelude::Handle<bevy::prelude::Mesh>],
    textures: &[bevy::prelude::Handle<bevy::prelude::Image>],
) -> Result<(), ViewError> {
    use bevy::prelude::*;
    for mesh in meshes {
        if world.resource::<Assets<Mesh>>().get(mesh).is_none() {
            return Err(ViewError::Capture(format!(
                "missing scene mesh {:?}",
                mesh.id()
            )));
        }
    }
    for texture in textures {
        if world.resource::<Assets<Image>>().get(texture).is_none() {
            return Err(ViewError::Capture(format!(
                "missing scene texture {:?}",
                texture.id()
            )));
        }
    }
    Ok(())
}
