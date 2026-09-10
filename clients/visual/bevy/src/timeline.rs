use crate::ViewError;
pub const HISTORY_RESET_POLICY: &str = "No temporal GPU history; synchronous rendering. Seek/reverse sample frames directly. Source reset clears scene/capture identity. Each accepted observation replaces the complete physical snapshot.";
#[derive(Clone, Debug)]
pub struct FilmClock {
    pub start_ticks: i64,
    pub end_ticks: i64,
    pub frames: u32,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FrameSample {
    pub frame: u32,
    pub frames: u32,
    pub ticks: i64,
}
impl FilmClock {
    pub fn tick_at(&self, frame: u32) -> Result<i64, ViewError> {
        if self.frames == 0 || frame >= self.frames {
            return Err(ViewError::Range(
                "frame outside presentation interval".into(),
            ));
        }
        let overflow = || ViewError::Range("film tick arithmetic overflow".into());
        let delta = i128::from(self.end_ticks)
            .checked_sub(i128::from(self.start_ticks))
            .ok_or_else(overflow)?;
        let numerator = delta.checked_mul(i128::from(frame)).ok_or_else(overflow)?;
        let denominator = i128::from(self.frames);
        let magnitude = numerator.checked_abs().ok_or_else(overflow)?;
        let quotient = magnitude / denominator;
        let round = i128::from(
            (magnitude % denominator)
                .checked_mul(2)
                .ok_or_else(overflow)?
                >= denominator,
        );
        let offset = quotient
            .checked_add(round)
            .and_then(|v| v.checked_mul(numerator.signum()))
            .ok_or_else(overflow)?;
        i64::try_from(
            i128::from(self.start_ticks)
                .checked_add(offset)
                .ok_or_else(overflow)?,
        )
        .map_err(|_| overflow())
    }
}
pub struct PresentationTimeline {
    pub clock: FilmClock,
    frame: u32,
}
impl PresentationTimeline {
    pub fn new(clock: FilmClock) -> Result<Self, ViewError> {
        clock.tick_at(0)?;
        Ok(Self { clock, frame: 0 })
    }
    pub fn seek(&mut self, frame: u32) -> Result<FrameSample, ViewError> {
        let ticks = self.clock.tick_at(frame)?;
        self.frame = frame;
        Ok(FrameSample {
            frame,
            frames: self.clock.frames,
            ticks,
        })
    }
    pub fn sample(&self) -> Result<FrameSample, ViewError> {
        Ok(FrameSample {
            frame: self.frame,
            frames: self.clock.frames,
            ticks: self.clock.tick_at(self.frame)?,
        })
    }
}
