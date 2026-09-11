//! Explicit GPU qualification instrument, disabled in ordinary inspection.
#![allow(
    clippy::disallowed_types,
    reason = "Instant measures real presentation intervals, never simulation time"
)]
use hornvale_bevy_view::bevy::prelude::*;
use std::{
    path::PathBuf,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::Instant,
};
#[derive(Resource)]
pub struct Benchmark {
    pub ready: Arc<AtomicBool>,
    start: Instant,
    warmed: Option<Instant>,
    measuring: Option<Instant>,
    previous: Instant,
    pub output: PathBuf,
    pub frames: Vec<serde_json::Value>,
    pub queries: Vec<u128>,
    pub startup_queries: Vec<u128>,
    first_load: f64,
}
impl Benchmark {
    pub fn new(output: PathBuf, start: Instant) -> Result<Self, std::io::Error> {
        std::fs::create_dir(&output)?;
        Ok(Self {
            ready: Arc::new(AtomicBool::new(false)),
            start,
            warmed: None,
            measuring: None,
            previous: Instant::now(),
            output,
            frames: Vec::new(),
            queries: Vec::new(),
            startup_queries: Vec::new(),
            first_load: 0.,
        })
    }
    /// Returns elapsed measurement time and actual interval. Readiness then two
    /// seconds of warmup precede the fixed sixty-second measured interval.
    pub fn sample(&mut self) -> Option<(f64, f64)> {
        self.sample_at(Instant::now())
    }
    fn sample_at(&mut self, now: Instant) -> Option<(f64, f64)> {
        let interval = now.duration_since(self.previous).as_secs_f64();
        self.previous = now;
        if !self.ready.load(Ordering::Acquire) {
            return None;
        }
        let warmed = *self.warmed.get_or_insert_with(|| {
            self.first_load = now.duration_since(self.start).as_secs_f64();
            now
        });
        if now.duration_since(warmed).as_secs_f64() < 2. {
            return None;
        }
        let measuring = *self.measuring.get_or_insert(now);
        Some((now.duration_since(measuring).as_secs_f64(), interval))
    }
    pub fn finish(
        &self,
        film: &crate::shots::FilmDefinition,
        dimensions: (u32, u32),
        error: &Option<String>,
    ) -> Result<(), std::io::Error> {
        let report = serde_json::json!({"schema":"planetarium/interactive-benchmark/v1", "build_revision":crate::provenance::BUILD_REVISION,"build_tree_clean":crate::provenance::BUILD_CLEAN,"executable_sha256":crate::package::hash(&std::fs::read(std::env::current_exe()?)?), "duration_seconds":60, "first_load_seconds": self.first_load,"warmup_seconds":2,"dimensions":dimensions,"film":film,"screenshot_recording":false,"readiness":"source observation accepted; GPU scene meshes/textures present; nonempty pipeline cache all compiled; then two seconds warmup", "script":"0-10s forward film;10-20s reverse;20-30s paused frame150 with orbit;30-40s paused frame150 with pan;40-50s paused frame150 with dolly;50-60s forward film", "input":"application Playback and OrbitCamera methods; not OS input", "frame_samples":self.frames,"startup_query_service_micros":self.startup_queries,"query_service_micros":self.queries,"error":error});
        std::fs::write(
            self.output.join("samples.json"),
            serde_json::to_vec_pretty(&report)?,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn readiness_excludes_startup_and_two_seconds_of_warmup() {
        let now = Instant::now();
        let mut b = Benchmark {
            ready: Arc::new(AtomicBool::new(false)),
            start: now,
            warmed: None,
            measuring: None,
            previous: now,
            output: PathBuf::new(),
            frames: Vec::new(),
            queries: Vec::new(),
            startup_queries: Vec::new(),
            first_load: 0.,
        };
        assert!(
            b.sample_at(now + std::time::Duration::from_secs(3))
                .is_none()
        );
        b.ready.store(true, Ordering::Release);
        assert!(
            b.sample_at(now + std::time::Duration::from_secs(4))
                .is_none()
        );
        assert_eq!(b.first_load, 4.);
        assert!(
            b.sample_at(now + std::time::Duration::from_secs(5))
                .is_none()
        );
        assert_eq!(
            b.sample_at(now + std::time::Duration::from_secs(6)),
            Some((0., 1.))
        );
        assert_eq!(
            b.sample_at(now + std::time::Duration::from_millis(6017)),
            Some((0.017, 0.017))
        );
    }
}
