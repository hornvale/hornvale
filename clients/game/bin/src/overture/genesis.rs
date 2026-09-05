//! Genesis on a worker thread, and the loop that draws while it runs — ruling
//! R5.
//!
//! # Why a thread at all
//!
//! Because the alternative is a frozen screen. Redrawing only inside the
//! observer callback gives FOUR redraws across the whole build, and the gaps
//! between them are not small: the settlements rung alone is 60.2% of the wait
//! (1,840 ms today, ~36 s at the projected minute). Four frames in three seconds
//! is a progress indicator, not an overture, and both `space` cycling and
//! [`super::SLIDESHOW_DWELL`] are meaningless without a loop to drive them. So
//! the build runs on a worker and the main thread owns the terminal, polls
//! input, cycles views and redraws on a cadence.
//!
//! **Determinism is untouched.** The entire build still runs on ONE thread, in
//! one order, with one stream of draws; nothing about *which* thread that is
//! reaches a world. `clients/game` is outside the determinism boundary anyway
//! (decision 0055 — the repo boundary IS that boundary), but the stronger
//! statement is the true one: `build_world_observed` is called exactly once,
//! from exactly one thread, exactly as the CLI calls it.
//!
//! # The clone, and why it is not a widening of R4
//!
//! [`hornvale_worldgen::RungArtifacts`] hands the observer BORROWS, and it still
//! does. The borrows cannot cross a channel, so this observer — running on the
//! worker, where the borrows are valid — clones what a view needs into an owned
//! [`RungSnapshot`] and sends that. The clone is the observer's own choice at
//! its own call site, not a change to the contract: an observer that does not
//! need to leave the thread (the tests in `windows/worldgen`, for instance)
//! still clones nothing.
//!
//! **Measured, because it is a real cost against a 3,054 ms budget rather than a
//! free move** (`--release`, seed 42, M1 Max, three runs):
//!
//! ```text
//!   rung          facts   world_ms  terrain_ms  climate_ms   total_ms
//!   Astronomy        83      0.090       0.000       0.000      0.090
//!   Terrain         110      0.135       1.851       0.000      1.986
//!   Settlements   18,722     5.642       1.896       2.665     10.204
//!   Full          21,964     7.199       1.619       0.772      9.590
//!   ------------------------------------------------------------------
//!   total                                                      21.870
//! ```
//!
//! **21.9 / 24.5 / 23.7 ms across all four rungs — 0.85% to 0.94% of the
//! build.** So a plain clone is the right answer and `Arc` is not needed: the
//! sharing would buy under a hundredth of the wait and cost every view an
//! indirection plus a lifetime it does not want. Recorded here so a later
//! campaign that changes the ledger's size can tell whether the conclusion still
//! holds.
//!
//! # What is NOT on the worker, and why that is a bound rather than an oversight
//!
//! [`hornvale_kernel::WorldContext::build`] — the post-genesis phase this
//! module's sibling calls [`super::Phase::Living`], 27.2% of the wait — stays on
//! the main thread, so the screen holds its last frame for that stretch instead
//! of animating. It is not a choice: `WorldContext` stores
//! `Box<dyn PhenomenaSource>`, and `PhenomenaSource`
//! (`kernel/src/phenomena.rs:233`) declares no `Send` bound, so the value cannot
//! cross a thread boundary at all. Adding one is a KERNEL change, and this
//! campaign's spec §7 states the observer callback is its only sim-side change.
//! `Driver` is `!Send` for a second, independent reason (it holds `*mut World`),
//! so moving the whole of `Driver::start` over would additionally need an
//! `unsafe impl Send` — a safety claim that wants its own review, not a fix
//! round.
//!
//! The frozen stretch is therefore ~843 ms of 3,054 ms, and the frame it freezes
//! on is correct rather than blank: `living` is marked in progress, with a bar if
//! a previous run measured it. Against today's behaviour — a blank terminal for
//! the entire 3,054 ms — this delivers the campaign's premise for 73% of the
//! wait.

use super::{Frame, Phase};
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, GeneratedClimate, RungArtifacts, SettlementPins, WorldComponents,
    build_world_observed,
};
use std::sync::mpsc::{Receiver, TryRecvError};
use std::time::Duration;

/// How long the loop waits for a keystroke before redrawing.
///
/// The fact count only moves at a rung boundary, but the per-phase bar is
/// clock-driven and moves continuously, so the loop must wake without input.
/// 100 ms: ten frames a second is smooth enough for a bar and cheap enough that
/// the redraw never competes with genesis for a core.
pub const OVERTURE_TICK: Duration = Duration::from_millis(100);

/// One rung's OWNED snapshot — what crosses the channel.
///
/// The owned twin of [`RungArtifacts`], which is borrows. See the module doc for
/// why the clone happens here and what it costs.
pub struct RungSnapshot {
    /// The rung that just completed.
    pub rung: BuildDepth,
    /// The world at that rung.
    pub world: World,
    /// The sculpted terrain, `Some` on exactly the rungs that built it.
    pub terrain: Option<GeneratedTerrain>,
    /// The derived climate, `Some` on exactly the rungs that built it.
    pub climate: Option<GeneratedClimate>,
}

impl RungSnapshot {
    /// Clone what a view needs out of an observer callback's borrows.
    ///
    /// Called on the worker thread, inside the callback, where the borrows are
    /// valid — which is the only place it can be called.
    pub fn take(rung: BuildDepth, world: &World, artifacts: RungArtifacts<'_>) -> RungSnapshot {
        RungSnapshot {
            rung,
            world: world.clone(),
            terrain: artifacts.terrain.cloned(),
            climate: artifacts.climate.cloned(),
        }
    }

    /// Borrow this snapshot back into the shape [`Frame::observe`] takes, so the
    /// main thread hands views exactly what the observer would have.
    pub fn artifacts(&self) -> RungArtifacts<'_> {
        RungArtifacts {
            terrain: self.terrain.as_ref(),
            climate: self.climate.as_ref(),
        }
    }
}

/// What the worker sends up as the build proceeds.
///
/// Boxed payloads: a `World` at the `Full` rung is ~22,000 facts, and an
/// unboxed variant would make every message that large.
pub enum Progress {
    /// A rung completed, with everything a view needs to draw it.
    Rung(Box<RungSnapshot>),
    /// The build finished. Carries the world itself, so the caller starts the
    /// session from the world that was just built rather than building a second
    /// one.
    Done(Box<World>),
    /// The build failed, with the error's own text (the error types are not
    /// `Send`-guaranteed and the caller only reports the message).
    Failed(String),
}

/// Why the overture loop stopped without a world.
#[derive(Debug)]
pub enum OvertureError {
    /// Genesis itself failed; the payload is the sim's own error text.
    Genesis(String),
    /// The worker thread ended without reporting either success or failure —
    /// which in practice means it panicked. Distinguished from
    /// [`OvertureError::Genesis`] deliberately: a panic is a bug here, not a
    /// world that could not exist, and reporting it as a genesis failure would
    /// send the reader looking at their seed.
    WorkerLost,
    /// The terminal could not be drawn to or read from.
    Screen(std::io::Error),
}

impl std::fmt::Display for OvertureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OvertureError::Genesis(e) => write!(f, "{e}"),
            OvertureError::WorkerLost => {
                write!(f, "world generation ended without a result (worker lost)")
            }
            OvertureError::Screen(e) => write!(f, "terminal: {e}"),
        }
    }
}

impl std::error::Error for OvertureError {}

impl From<std::io::Error> for OvertureError {
    fn from(e: std::io::Error) -> Self {
        OvertureError::Screen(e)
    }
}

/// Start the build on a worker thread, returning the channel it reports on.
///
/// The join handle is deliberately dropped: every result the caller needs
/// arrives on the channel, and a detached worker whose channel has closed is
/// exactly the [`OvertureError::WorkerLost`] case the loop already handles. The
/// thread cannot outlive the process it reports to in any way that matters —
/// the receiver's drop makes every subsequent `send` fail and the closure
/// returns.
pub fn spawn(seed: Seed) -> Receiver<Progress> {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let wc = match WorldComponents::assemble() {
            Ok(wc) => wc,
            Err(e) => {
                let _ = tx.send(Progress::Failed(e.to_string()));
                return;
            }
        };
        let outcome = build_world_observed(
            seed,
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Full,
            // The clone the module doc measures. `send` failing means the
            // reader has gone; the build finishes anyway rather than being
            // torn down mid-write, and the final `send` below fails too.
            &mut |rung, world, artifacts| {
                let _ = tx.send(Progress::Rung(Box::new(RungSnapshot::take(
                    rung, world, artifacts,
                ))));
            },
        );
        let _ = match outcome {
            Ok(world) => tx.send(Progress::Done(Box::new(world))),
            Err(e) => tx.send(Progress::Failed(e.to_string())),
        };
    });
    rx
}

/// What the overture loop needs from a screen — two methods, so the loop can be
/// driven by a recording double instead of a real tty (which needs a real
/// terminal and enables raw mode as a side effect of existing). The same
/// reasoning [`crate::boot::TermHandle`] is built on.
pub trait Screen {
    /// The screen's current size in cells, already clamped to whatever floor
    /// the renderer refuses to go under.
    fn size(&self) -> std::io::Result<(u16, u16)>;
    /// Put `grid` on the screen.
    fn draw(&self, grid: &hornvale_game_core::Grid) -> std::io::Result<()>;
}

/// A gesture the overture understands. Everything else is ignored: the world
/// does not exist yet, so there is nothing else to ask it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Gesture {
    /// Show the next view that can speak.
    Cycle,
    /// The terminal changed size.
    Resize,
    /// A key with no meaning here.
    Ignored,
}

/// What the overture loop needs from the keyboard.
pub trait Keys {
    /// Wait up to `timeout` for a gesture. `Ok(None)` means the timeout
    /// elapsed with no input, which is the loop's redraw tick.
    fn next(&mut self, timeout: Duration) -> std::io::Result<Option<Gesture>>;
}

/// Draw the frame while the build runs, and hand back the world it produced.
///
/// The loop, in order: drain every rung waiting on the channel into the frame,
/// wait up to [`OVERTURE_TICK`] for a gesture, advance the slideshow if it is
/// due, redraw. It returns as soon as [`Progress::Done`] arrives — the caller
/// then owns the world and the frame, and is expected to close
/// [`Phase::Living`] itself once the post-genesis work is done (see
/// [`Frame::finish_phase`]).
///
/// **A rung is drained before anything else on every pass**, so a burst of two
/// rungs arriving between ticks is both observed rather than one being held over
/// — the astronomy rung completes in 0.4 ms and the terrain rung 202 ms later,
/// well inside a single 100 ms tick on a fast machine.
pub fn run<S: Screen, K: Keys>(
    frame: &mut Frame,
    progress: &Receiver<Progress>,
    screen: &S,
    keys: &mut K,
) -> Result<World, OvertureError> {
    let (w, h) = screen.size()?;
    frame.resize(w, h);
    let mut since_cycle = Duration::ZERO;
    loop {
        loop {
            match progress.try_recv() {
                Ok(Progress::Rung(snapshot)) => {
                    frame.observe(snapshot.rung, &snapshot.world, snapshot.artifacts());
                }
                Ok(Progress::Done(world)) => {
                    // One last frame, so the deepest rung is actually SEEN
                    // rather than replaced by the session's first screen in the
                    // same instant.
                    screen.draw(&frame.compose())?;
                    return Ok(*world);
                }
                Ok(Progress::Failed(e)) => return Err(OvertureError::Genesis(e)),
                Err(TryRecvError::Empty) => break,
                // No `Done`, no `Failed`, and the sender is gone: the worker
                // panicked. Returning rather than blocking forever is the whole
                // reason this arm is separate.
                Err(TryRecvError::Disconnected) => return Err(OvertureError::WorkerLost),
            }
        }

        match keys.next(OVERTURE_TICK)? {
            Some(Gesture::Cycle) => {
                frame.cycle();
                since_cycle = Duration::ZERO;
            }
            Some(Gesture::Resize) => {
                let (w, h) = screen.size()?;
                frame.resize(w, h);
            }
            Some(Gesture::Ignored) => {}
            None => since_cycle += OVERTURE_TICK,
        }
        if since_cycle >= super::SLIDESHOW_DWELL {
            frame.cycle();
            since_cycle = Duration::ZERO;
        }
        screen.draw(&frame.compose())?;
    }
}

/// The phase the caller must close once the post-genesis work is finished, since
/// no rung reports it. Named here so `main.rs` and any future caller cannot pick
/// a different one.
pub const POST_GENESIS_PHASE: Phase = Phase::Living;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::overture::{PhaseTimings, View};
    use hornvale_game_core::Grid;
    use std::cell::RefCell;
    use std::sync::mpsc::Sender;

    /// A screen that records the plain text of every grid drawn to it.
    struct Recorder {
        size: (u16, u16),
        drawn: RefCell<Vec<String>>,
    }

    impl Recorder {
        fn new() -> Recorder {
            Recorder {
                size: (
                    hornvale_game_core::MIN_WIDTH,
                    hornvale_game_core::MIN_HEIGHT,
                ),
                drawn: RefCell::new(Vec::new()),
            }
        }
        fn frames(&self) -> Vec<String> {
            self.drawn.borrow().clone()
        }
    }

    impl Screen for Recorder {
        fn size(&self) -> std::io::Result<(u16, u16)> {
            Ok(self.size)
        }
        fn draw(&self, grid: &Grid) -> std::io::Result<()> {
            self.drawn.borrow_mut().push(grid.to_plain_text());
            Ok(())
        }
    }

    /// A scripted keyboard: hands back one queued gesture per call, then
    /// `None` (the redraw tick) forever.
    struct Script(Vec<Option<Gesture>>);

    impl Keys for Script {
        fn next(&mut self, _timeout: Duration) -> std::io::Result<Option<Gesture>> {
            if self.0.is_empty() {
                return Ok(None);
            }
            Ok(self.0.remove(0))
        }
    }

    /// A view that always speaks and draws its own name.
    struct Named(&'static str);

    impl View for Named {
        fn name(&self) -> &'static str {
            self.0
        }
        fn can_speak(&self, _rung: BuildDepth) -> bool {
            true
        }
        fn render(
            &mut self,
            _world: &World,
            _rung: BuildDepth,
            _artifacts: RungArtifacts<'_>,
            w: u16,
            _h: u16,
        ) -> Grid {
            let mut grid = Grid::new(w.max(1), 1);
            for (i, ch) in self.0.chars().enumerate() {
                grid.set(
                    u16::try_from(i).unwrap_or(u16::MAX),
                    0,
                    hornvale_game_core::Cell::glyph(
                        ch,
                        hornvale_game_core::Weight::Normal,
                        hornvale_game_core::Source::Overture,
                    ),
                );
            }
            grid
        }
    }

    fn frame_of(views: Vec<Box<dyn View>>) -> Frame {
        Frame::with_baseline(views, PhaseTimings::empty())
    }

    /// A real seed-42 world at the astronomy rung, built once — 0.4 ms of sim,
    /// but a genuine ledger, so the fact count these tests read is real.
    fn sky_world() -> &'static World {
        static SKY: std::sync::OnceLock<World> = std::sync::OnceLock::new();
        SKY.get_or_init(|| {
            let wc = WorldComponents::assemble().expect("registries");
            hornvale_worldgen::build_world_to(
                Seed(42),
                &SkyPins::default(),
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                BuildDepth::Astronomy,
            )
            .expect("seed 42 builds")
        })
    }

    fn send_rung(tx: &Sender<Progress>, rung: BuildDepth) {
        tx.send(Progress::Rung(Box::new(RungSnapshot::take(
            rung,
            sky_world(),
            RungArtifacts::none(),
        ))))
        .expect("send");
    }

    #[test]
    fn a_finished_build_hands_back_the_world_the_worker_built() {
        // And the SAME world, not a rebuilt one: the ledger length must match
        // what was sent, which a second `build_world` would not guarantee to do
        // for a different depth.
        let (tx, rx) = std::sync::mpsc::channel();
        let expected = sky_world().ledger.len();
        tx.send(Progress::Done(Box::new(sky_world().clone())))
            .expect("send");
        let mut frame = frame_of(vec![Box::new(Named("a"))]);
        let screen = Recorder::new();
        let world = run(&mut frame, &rx, &screen, &mut Script(Vec::new())).expect("finish");
        assert_eq!(world.ledger.len(), expected);
        // A final frame is drawn on the way out, so the deepest rung is seen
        // rather than replaced by the session's first screen in the same instant.
        assert_eq!(screen.frames().len(), 1);
    }

    #[test]
    fn every_rung_waiting_on_the_channel_reaches_the_frame_before_a_redraw() {
        // TWO rungs queued before the loop starts, which is the real case: the
        // astronomy rung completes in 0.4 ms and terrain 202 ms later, both well
        // inside one 100 ms tick on a fast machine. A loop that took one message
        // per pass would leave the frame a rung behind, and would still pass a
        // one-rung test.
        let (tx, rx) = std::sync::mpsc::channel();
        send_rung(&tx, BuildDepth::Astronomy);
        send_rung(&tx, BuildDepth::Terrain);
        tx.send(Progress::Done(Box::new(sky_world().clone())))
            .expect("send");
        let mut frame = frame_of(vec![Box::new(Named("a"))]);
        run(&mut frame, &rx, &Recorder::new(), &mut Script(Vec::new())).expect("finish");
        assert_eq!(
            frame.rung(),
            Some(BuildDepth::Terrain),
            "the loop dropped or deferred a rung"
        );
        // Both rungs were CLOSED on the clock, so both are measurable — a loop
        // that only observed the last one would leave `sky` unrecorded.
        assert!(frame.measured().get(Phase::Sky).is_some());
        assert!(frame.measured().get(Phase::Land).is_some());
    }

    #[test]
    fn space_cycles_the_view_and_the_slideshow_does_so_unattended() {
        // Both halves in one test because they share the same timer, and a fix
        // to one that broke the other would otherwise pass.
        let (tx, rx) = std::sync::mpsc::channel();
        send_rung(&tx, BuildDepth::Astronomy);
        let mut frame = frame_of(vec![Box::new(Named("alpha")), Box::new(Named("gamma"))]);
        let screen = Recorder::new();

        // One space, then enough silent ticks for the slideshow to fire, then
        // done. `SLIDESHOW_DWELL / OVERTURE_TICK` ticks are needed for one
        // advance; queue that many `None`s and one more.
        let ticks =
            (super::super::SLIDESHOW_DWELL.as_millis() / OVERTURE_TICK.as_millis()) as usize;
        let mut script = vec![Some(Gesture::Cycle)];
        script.extend(std::iter::repeat_n(None, ticks));
        let mut keys = Script(script);

        // Run until the queue drains, then finish.
        std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(50));
            let _ = tx.send(Progress::Done(Box::new(sky_world().clone())));
        });
        run(&mut frame, &rx, &screen, &mut keys).expect("finish");

        let frames = screen.frames();
        assert!(frames.len() >= 2, "too few frames to show a cycle");
        // `space` moved off `alpha` …
        assert!(
            frames[0].contains("gamma"),
            "space did not cycle: {}",
            &frames[0][..frames[0].len().min(200)]
        );
        // … and the unattended slideshow moved back, with no further input.
        assert!(
            frames.iter().any(|f| f.contains("alpha")),
            "the slideshow never advanced without input"
        );
    }

    #[test]
    fn a_failed_build_is_reported_as_genesis_not_as_a_lost_worker() {
        let (tx, rx) = std::sync::mpsc::channel();
        tx.send(Progress::Failed("no such sky".to_string()))
            .expect("send");
        let mut frame = frame_of(Vec::new());
        let err =
            run(&mut frame, &rx, &Recorder::new(), &mut Script(Vec::new())).expect_err("must fail");
        assert!(
            matches!(err, OvertureError::Genesis(ref m) if m == "no such sky"),
            "{err:?}"
        );
        assert_eq!(err.to_string(), "no such sky");
    }

    #[test]
    fn a_worker_that_dies_silently_ends_the_loop_instead_of_hanging() {
        // The arm that matters most: without it the loop spins forever on a
        // panicked worker, and a suite that HANGS rather than fails is the
        // failure shape this campaign's predecessor was bitten by twice.
        // Distinguished from a genesis failure on purpose — a panic is a bug
        // here, not a world that could not exist.
        let (tx, rx) = std::sync::mpsc::channel::<Progress>();
        drop(tx);
        let mut frame = frame_of(Vec::new());
        let err =
            run(&mut frame, &rx, &Recorder::new(), &mut Script(Vec::new())).expect_err("must fail");
        assert!(matches!(err, OvertureError::WorkerLost), "{err:?}");
    }

    #[test]
    fn the_real_worker_reports_every_rung_in_order_and_then_the_world() {
        // The one test that runs the ACTUAL thread and the ACTUAL build, so the
        // wiring in `spawn` is exercised rather than only its message shapes.
        // Seed 42 at full depth: the expensive test in this file, and the only
        // one that proves a snapshot survives the channel with its artifacts.
        let rx = spawn(Seed(42));
        let mut rungs = Vec::new();
        let mut done = None;
        for message in rx {
            match message {
                Progress::Rung(snapshot) => {
                    let art = snapshot.artifacts();
                    // The Some-iff-depth contract, preserved ACROSS the clone
                    // and the channel — the property a cloned snapshot could
                    // silently lose.
                    assert_eq!(
                        art.terrain.is_some(),
                        snapshot.rung >= BuildDepth::Terrain,
                        "terrain wrong at {:?}",
                        snapshot.rung
                    );
                    assert_eq!(
                        art.climate.is_some(),
                        snapshot.rung >= BuildDepth::Settlements,
                        "climate wrong at {:?}",
                        snapshot.rung
                    );
                    assert!(!snapshot.world.ledger.is_empty());
                    rungs.push(snapshot.rung);
                }
                Progress::Done(world) => done = Some(world),
                Progress::Failed(e) => panic!("seed 42 failed: {e}"),
            }
        }
        assert_eq!(
            rungs,
            vec![
                BuildDepth::Astronomy,
                BuildDepth::Terrain,
                BuildDepth::Settlements,
                BuildDepth::Full,
            ]
        );
        let world = done.expect("the worker never reported a finished world");
        assert!(world.ledger.len() > 1_000);
    }
}
