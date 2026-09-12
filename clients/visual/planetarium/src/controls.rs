//! Interactive pacing affects only which exact frame is requested.
#[derive(Clone, Debug)]
pub struct Playback {
    pub paused: bool,
    pub rate: f64,
    position: f64,
}
impl Default for Playback {
    fn default() -> Self {
        Self {
            paused: true,
            rate: 1.,
            position: 0.,
        }
    }
}
impl Playback {
    pub fn frame(&self) -> u32 {
        self.position.floor() as u32
    }
    pub fn seek(&mut self, frame: u32, frames: u32) {
        self.position = f64::from(frame.min(frames.saturating_sub(1)));
    }
    pub fn set_rate(&mut self, rate: f64) {
        if rate.is_finite() && rate != 0. {
            self.rate = rate.clamp(-8., 8.);
        }
    }
    pub fn advance(&mut self, seconds: f64, frames: u32, fps: u32) {
        if !self.paused && seconds.is_finite() && seconds >= 0. {
            self.position = (self.position + seconds * f64::from(fps) * self.rate)
                .clamp(0., f64::from(frames.saturating_sub(1)));
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DragOwner {
    Ui,
    Scene,
}
#[derive(Default)]
pub struct PointerGesture {
    start: Option<DragOwner>,
}
impl PointerGesture {
    pub fn begin(&mut self, over_ui: bool) {
        self.start = Some(if over_ui {
            DragOwner::Ui
        } else {
            DragOwner::Scene
        });
    }
    pub fn owner(&self, over_ui: bool) -> DragOwner {
        self.start.unwrap_or(if over_ui {
            DragOwner::Ui
        } else {
            DragOwner::Scene
        })
    }
    pub fn end(&mut self) {
        self.start = None;
    }
}
