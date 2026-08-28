//! The previous run's per-phase durations, and the clock that measures this
//! one — decision 0360: *an estimate comes from the previous run's timings, not
//! from a model.*
//!
//! # The on-disk format
//!
//! One file, `overture-timings.tsv`, in [`crate::state_dir`]'s directory. Tab-
//! separated, one line per phase, `key<TAB>millis`, in the CHRONOLOGICAL phase
//! order [`Phase::ALL`] declares (the map is keyed by `Phase`, whose `Ord` is
//! its declaration order, so the file reads as the run did), LF-terminated, no
//! header:
//!
//! (with `<TAB>` standing for a literal tab, which a doc comment may not
//! contain):
//!
//! ```text
//! sky<TAB>0
//! land<TAB>202
//! peoples<TAB>1840
//! deep-time<TAB>181
//! living<TAB>830
//! ```
//!
//! The keys are [`Phase::key`], never the display label, so rewording a caption
//! cannot orphan the record. **Every read is total**: a missing file, an
//! unreadable one, a truncated line, a non-numeric duration or a key from a
//! future version all reduce to "that phase has no baseline", which the frame
//! already renders honestly as no bar (spec §2). Nothing here ever returns an
//! error to a caller, because there is no caller for whom a bad cache file is
//! worse than no cache file.
//!
//! TSV rather than JSON because `clients/game/bin` has no `serde_json` outside
//! `[dev-dependencies]`, and five integers do not need one.

use super::progress::Phase;
use crate::state_dir;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// The state-directory file name.
pub const TIMINGS_FILE: &str = "overture-timings.tsv";

/// One run's measured phase durations, in milliseconds.
///
/// A `BTreeMap`, so [`to_tsv`](PhaseTimings::to_tsv) is byte-stable for the
/// same content regardless of insertion order — the client is outside the
/// determinism boundary, but a cache file that churns for no reason is still a
/// cache file nobody can diff.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PhaseTimings {
    millis: BTreeMap<Phase, u64>,
}

impl PhaseTimings {
    /// No baselines at all — a first-ever run.
    pub fn empty() -> PhaseTimings {
        PhaseTimings::default()
    }

    /// Whether there is nothing recorded. The frame draws no bar in this state.
    pub fn is_empty(&self) -> bool {
        self.millis.is_empty()
    }

    /// The recorded duration for `phase`, if any. `Some(0)` is deliberately
    /// distinguishable from `None`: the sky phase really does complete in under
    /// a millisecond, and "measured as instant" is not "never measured".
    pub fn get(&self, phase: Phase) -> Option<u64> {
        self.millis.get(&phase).copied()
    }

    /// Record `duration` for `phase`, replacing any previous value.
    pub fn set(&mut self, phase: Phase, duration: Duration) {
        self.millis.insert(
            phase,
            u64::try_from(duration.as_millis()).unwrap_or(u64::MAX),
        );
    }

    /// How far through `phase` a run that has spent `elapsed` in it is,
    /// according to this baseline — or `None` when there is nothing to be a
    /// fraction of.
    ///
    /// `None` for an absent phase AND for a zero-millisecond baseline: dividing
    /// by a measured zero would yield infinity, and the honest report is that
    /// this phase is too fast to have a bar (the sky phase, at 0.4 ms, is
    /// exactly that case rather than a hypothetical one).
    pub fn fraction_through(&self, phase: Phase, elapsed: Duration) -> Option<f64> {
        let baseline = self.get(phase)?;
        if baseline == 0 {
            return None;
        }
        let elapsed = u64::try_from(elapsed.as_millis()).unwrap_or(u64::MAX);
        Some((elapsed as f64 / baseline as f64).clamp(0.0, 1.0))
    }

    /// The file's text form — see the module doc for the format.
    pub fn to_tsv(&self) -> String {
        let mut out = String::new();
        for (phase, millis) in &self.millis {
            out.push_str(phase.key());
            out.push('\t');
            out.push_str(&millis.to_string());
            out.push('\n');
        }
        out
    }

    /// Parse the file's text form. Total: every unparseable line is dropped and
    /// the rest is kept.
    pub fn parse(text: &str) -> PhaseTimings {
        let mut millis = BTreeMap::new();
        for line in text.lines() {
            let Some((key, value)) = line.split_once('\t') else {
                continue;
            };
            let Some(phase) = Phase::from_key(key.trim()) else {
                continue;
            };
            let Ok(ms) = value.trim().parse::<u64>() else {
                continue;
            };
            millis.insert(phase, ms);
        }
        PhaseTimings { millis }
    }

    /// Read the baselines from `path`. An absent or unreadable file is
    /// [`PhaseTimings::empty`], never an error.
    pub fn load_from(path: &Path) -> PhaseTimings {
        match std::fs::read_to_string(path) {
            Ok(text) => PhaseTimings::parse(&text),
            Err(_) => PhaseTimings::empty(),
        }
    }

    /// Read the baselines from the state directory. Nowhere to keep state means
    /// [`PhaseTimings::empty`], which is the first-ever-run state.
    pub fn load() -> PhaseTimings {
        match Self::path() {
            Some(path) => PhaseTimings::load_from(&path),
            None => PhaseTimings::empty(),
        }
    }

    /// Write the baselines to `path`, creating parent directories.
    pub fn save_to(&self, path: &Path) -> std::io::Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(path, self.to_tsv())
    }

    /// Write the baselines to the state directory. `Ok(false)` means there is
    /// nowhere to keep state — a legitimate configuration, not an error.
    pub fn save(&self) -> std::io::Result<bool> {
        match Self::path() {
            Some(path) => self.save_to(&path).map(|()| true),
            None => Ok(false),
        }
    }

    /// Where [`load`](PhaseTimings::load)/[`save`](PhaseTimings::save) read and
    /// write.
    pub fn path() -> Option<PathBuf> {
        state_dir::state_path(TIMINGS_FILE)
    }
}

// `Instant` is banned workspace-wide by the root `clippy.toml`'s
// `disallowed-types` (decision 0001: time is `WorldTime`), and clippy's config
// lookup walks up the directory tree regardless of workspace membership, so it
// reaches `clients/`. The allow is scoped and deliberate, on the same grounds
// `examples/rung_bench.rs` already takes it: sizing a per-phase bar from the
// previous run's duration needs a MONOTONIC clock, and there is no other one.
//
// **No duration measured here ever reaches a world, a ledger, or a committed
// artifact.** It is written to a per-user cache file and read back to decide how
// many `#` to draw. `clients/game` is outside the determinism boundary
// (decision 0055: the repo boundary IS that boundary), and the value's only
// consumer is a paint decision that the same seed would make differently on a
// differently loaded machine — which is exactly why it must never cross back
// in. `std::time::Duration` is NOT banned and needs no allow.
#[allow(clippy::disallowed_types)]
mod clock {
    use std::time::Instant;

    /// A monotonic stopwatch that restarts at each phase boundary.
    pub struct Stopwatch {
        started: Instant,
    }

    impl Stopwatch {
        /// Start now.
        pub fn start() -> Stopwatch {
            Stopwatch {
                started: Instant::now(),
            }
        }

        /// How long since the last [`start`](Stopwatch::start) or
        /// [`restart`](Stopwatch::restart).
        pub fn elapsed(&self) -> std::time::Duration {
            self.started.elapsed()
        }

        /// Take the elapsed time and begin again from now.
        pub fn restart(&mut self) -> std::time::Duration {
            let taken = self.started.elapsed();
            self.started = Instant::now();
            taken
        }
    }
}

pub use clock::Stopwatch;

/// This run's measurements as they accumulate: a stopwatch on the phase in
/// progress, plus everything already finished.
///
/// Separate from [`PhaseTimings`] (which is a plain value with a file format)
/// because only this half needs the clock, and keeping the `Instant` allow in
/// one small module is the point.
pub struct PhaseClock {
    watch: Stopwatch,
    current: Phase,
    measured: PhaseTimings,
}

impl PhaseClock {
    /// Start the clock on `first`.
    pub fn start(first: Phase) -> PhaseClock {
        PhaseClock {
            watch: Stopwatch::start(),
            current: first,
            measured: PhaseTimings::empty(),
        }
    }

    /// The phase being timed.
    pub fn current(&self) -> Phase {
        self.current
    }

    /// How long the current phase has been running.
    pub fn elapsed(&self) -> Duration {
        self.watch.elapsed()
    }

    /// Close `finished`, recording its duration, and begin timing whatever
    /// comes next ([`Phase::next`]). Returns the duration recorded.
    ///
    /// Records against `finished` rather than against `self.current` so a
    /// caller that learns about a skipped phase out of order still attributes
    /// the time to the phase that was named, never to the one the clock
    /// happened to be holding.
    pub fn finish(&mut self, finished: Phase) -> Duration {
        let taken = self.watch.restart();
        self.measured.set(finished, taken);
        if let Some(next) = finished.next() {
            self.current = next;
        }
        taken
    }

    /// What this run has measured so far — the value to [`PhaseTimings::save`]
    /// once startup completes, so the NEXT run has a bar.
    pub fn measured(&self) -> &PhaseTimings {
        &self.measured
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A temporary directory nothing else shares, named from the process id and
    /// a per-test counter. There is no `tempfile` crate here and adding one is
    /// forbidden; five integers do not justify a dependency.
    fn scratch(tag: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "hornvale-overture-timings-{}-{tag}",
            std::process::id()
        ));
        std::fs::create_dir_all(&dir).expect("temp dir");
        dir
    }

    fn sample() -> PhaseTimings {
        let mut t = PhaseTimings::empty();
        t.set(Phase::Sky, Duration::from_millis(0));
        t.set(Phase::Land, Duration::from_millis(202));
        t.set(Phase::Peoples, Duration::from_millis(1840));
        t.set(Phase::DeepTime, Duration::from_millis(181));
        t.set(Phase::Living, Duration::from_millis(830));
        t
    }

    #[test]
    fn the_file_round_trips_every_phase_including_a_measured_zero() {
        // The measured-zero half is the discriminating one: `Some(0)` and `None`
        // render differently (an empty bar vs. no bar), so a format that lost
        // the distinction would look correct on four of the five phases.
        let written = sample();
        let read = PhaseTimings::parse(&written.to_tsv());
        assert_eq!(read, written);
        assert_eq!(read.get(Phase::Sky), Some(0));
        assert_eq!(PhaseTimings::empty().get(Phase::Sky), None);
    }

    #[test]
    fn the_format_is_the_one_the_module_doc_states() {
        // Pinned literally, because two campaigns read and write this file and
        // a silent format change breaks the older one's bar without any error.
        assert_eq!(
            sample().to_tsv(),
            "sky\t0\nland\t202\npeoples\t1840\ndeep-time\t181\nliving\t830\n"
        );
    }

    #[test]
    fn a_damaged_file_degrades_to_the_lines_it_can_read() {
        // Every failure mode at once, with ONE good line among them, so the
        // test can tell "total parse" from "total refusal" — a parser that
        // returned `empty()` on any bad line would pass an `is_empty()` check.
        let text = "\
land\t202
notaphase\t5
peoples\tnotanumber
missing-tab-entirely
deep-time\t-181
sky\t0
";
        let got = PhaseTimings::parse(text);
        assert_eq!(got.get(Phase::Land), Some(202));
        assert_eq!(got.get(Phase::Sky), Some(0));
        assert_eq!(got.get(Phase::Peoples), None);
        assert_eq!(got.get(Phase::DeepTime), None);
    }

    #[test]
    fn an_absent_file_is_the_first_ever_run_not_an_error() {
        let path = scratch("absent").join("nothing-here.tsv");
        assert!(PhaseTimings::load_from(&path).is_empty());
    }

    #[test]
    fn saving_then_loading_is_the_identity_and_creates_the_directory() {
        let path = scratch("roundtrip").join("nested").join(TIMINGS_FILE);
        sample().save_to(&path).expect("save");
        assert_eq!(PhaseTimings::load_from(&path), sample());
        std::fs::remove_dir_all(path.parent().unwrap()).ok();
    }

    #[test]
    fn a_fraction_needs_a_nonzero_baseline_and_saturates_at_one() {
        let t = sample();
        assert_eq!(
            t.fraction_through(Phase::Land, Duration::from_millis(101)),
            Some(0.5)
        );
        assert_eq!(
            t.fraction_through(Phase::Land, Duration::from_millis(10_000)),
            Some(1.0)
        );
        // A measured-zero baseline has no fraction — dividing by it is infinity,
        // and "too fast to bar" is the honest report.
        assert_eq!(
            t.fraction_through(Phase::Sky, Duration::from_millis(5)),
            None
        );
        // An unmeasured phase likewise.
        assert_eq!(
            PhaseTimings::empty().fraction_through(Phase::Land, Duration::from_millis(5)),
            None
        );
    }

    #[test]
    fn the_clock_attributes_each_duration_to_the_phase_it_closed() {
        // Non-vacuity: close TWO different phases and assert the roster, so a
        // clock that recorded everything against one key cannot pass.
        let mut clock = PhaseClock::start(Phase::Sky);
        assert_eq!(clock.current(), Phase::Sky);
        clock.finish(Phase::Sky);
        assert_eq!(clock.current(), Phase::Land, "the clock must advance");
        clock.finish(Phase::Land);
        assert_eq!(clock.current(), Phase::Peoples);
        assert!(clock.measured().get(Phase::Sky).is_some());
        assert!(clock.measured().get(Phase::Land).is_some());
        assert_eq!(clock.measured().get(Phase::Peoples), None);
    }

    #[test]
    fn the_clock_advances_no_further_than_the_last_phase() {
        let mut clock = PhaseClock::start(Phase::Living);
        clock.finish(Phase::Living);
        assert_eq!(clock.current(), Phase::Living);
        assert!(clock.measured().get(Phase::Living).is_some());
    }
}
