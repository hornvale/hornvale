//! App-visible desired/committed state. Transport and wall time never relabel physics.
use crate::bridge::Bridge;
use hornvale_bevy_view::{FrameSample, ObservationMirror, PresentationTimeline};
pub struct ObservationState {
    pub mirror: ObservationMirror,
    pub timeline: PresentationTimeline,
    pub error: Option<String>,
}
impl ObservationState {
    pub fn seek(&mut self, bridge: &mut Bridge, frame: u32) -> Result<FrameSample, String> {
        let result = (|| {
            let sample = self.timeline.seek(frame).map_err(|e| e.to_string())?;
            let request = self
                .mirror
                .request(sample.ticks)
                .map_err(|e| e.to_string())?;
            bridge.submit(request)?;
            Ok(sample)
        })();
        self.error = result.as_ref().err().cloned();
        result
    }
    pub fn poll(&mut self, bridge: &mut Bridge) -> Result<bool, String> {
        let result = (|| match bridge.poll()? {
            Some(reply) => self.mirror.accept(&reply).map_err(|e| e.to_string()),
            None => Ok(false),
        })();
        if let Err(e) = &result {
            self.error = Some(e.clone());
        }
        result
    }
    pub fn pending(&self) -> bool {
        self.mirror.pending_ticks().is_some()
    }
}
