//! The overture: the startup frame, and the contract its views are written
//! against. Spec "The Overture" §2–§3; decisions 0357–0360.
//!
//! *An overture is not the delay before the opera. It is the first part of it.*
//! World generation costs 3.05 s today and is projected to a minute or more on
//! slower hardware, and the screen was blank for all of it. This module makes
//! that wait into the first part of the game.
//!
//! # `Frame` here is NOT [`crate::mercator::Frame`]
//!
//! The name collision is deliberate and the two are unrelated. [`Frame`] in
//! this module is the **startup chrome**: a header, a progress substrate, and
//! the `space` cycle over the views that own the middle of the screen. It lives
//! for the duration of genesis and is then dropped. [`crate::mercator::Frame`]
//! is the **map viewport** — the projection window `driver.rs` resolves world
//! coordinates through, which exists for the whole session and has nothing to
//! do with startup. They are in different modules; the plan names this one
//! `Frame` because four later tasks consume it under that name.
//!
//! # The shape, and why it is this shape
//!
//! The frame owns the chrome; a view owns the middle. The interesting
//! constraint is that **the derived artifacts a view needs are borrows that
//! live only for the duration of the observer callback** ([`hornvale_worldgen::
//! RungArtifacts`]): the observer is a read, and a `GeneratedTerrain` is far too
//! large to clone into it. So the frame renders **during**
//! [`Frame::observe`] — every view that can speak at the new rung, not only the
//! current one — and keeps the resulting [`Grid`]s. Afterwards `space` and the
//! slideshow are pure switches between grids already in hand, needing no world
//! at all, and the progress substrate (which is the only thing that changes
//! between rungs) recomposes on every draw.
//!
//! Rendering the non-current views costs something. It buys the thing contract
//! rule 2 demands: pressing `space` can never land on an empty region, because
//! there is no rung at which a speaking view has no grid.

pub mod genesis;
pub mod progress;
pub mod timings;
pub mod view;

pub use progress::{BuildState, Phase, progress_line};
pub use timings::{PhaseClock, PhaseTimings};
pub use view::View;

use hornvale_game_core::{Cell, Grid, Source, Weight};
use hornvale_kernel::World;
use hornvale_worldgen::{BuildDepth, RungArtifacts};

/// How long one view holds the screen when nobody presses `space`. Authored,
/// not derived: long enough to read a caption, short enough that a 3 s build
/// shows more than one view. The same register as `main.rs`'s `MARQUEE_TICK`,
/// and the caller drives it the same way — the frame has no timer of its own.
pub const SLIDESHOW_DWELL: std::time::Duration = std::time::Duration::from_millis(2500);

/// The name reported when no registered view can speak yet.
///
/// Not `"—"` or `"…"`: the frame's header shows this verbatim, and the honest
/// statement at a rung where nothing can speak is that the world is still
/// opening, not that a view is loading.
pub const NO_VIEW: &str = "";

/// The startup chrome: the progress substrate, the `space` cycle, and the
/// registered views' most recent renders.
///
/// # Wiring it to the build
///
/// [`Frame::observe`] has exactly the shape Task 2's observer hands over, so
/// the whole wiring is a closure that forwards:
///
/// ```no_run
/// # use hornvale_game::overture::Frame;
/// # use hornvale_kernel::{Seed, World};
/// # use hornvale_worldgen::{BuildDepth, RungArtifacts};
/// # fn demo(frame: &mut Frame) {
/// let mut observer = |rung: BuildDepth, world: &World, art: RungArtifacts<'_>| {
///     frame.observe(rung, world, art);
///     // draw `frame.compose()` here — this is the one moment the partial
///     // world and its artifacts are in scope.
/// };
/// # let _ = &mut observer;
/// # }
/// ```
///
/// passed as `&mut observer` to [`hornvale_worldgen::build_world_observed`].
/// Because the closure borrows the frame mutably, the frame is usable again as
/// soon as the build returns. A caller that wants to redraw *between* rungs —
/// for the live fact count and the slideshow — runs genesis on a worker thread
/// and forwards `(rung, World, ..)` over a channel instead; the observer's
/// borrows cannot cross a channel, so such a caller renders in the callback and
/// sends the composed grid, which is what the cached-grid design above makes
/// cheap.
pub struct Frame {
    /// The registered views, in registration order — which is the cycle order.
    views: Vec<Box<dyn View>>,
    /// Each view's most recent render, parallel to `views`. `None` for a view
    /// that has never been able to speak.
    panels: Vec<Option<Grid>>,
    /// Index into `views` of the view currently on screen.
    cursor: usize,
    /// The deepest rung the build has completed, or `None` before the first
    /// observation.
    rung: Option<BuildDepth>,
    /// Facts committed as of the last observation.
    facts: usize,
    /// The header's subject line, after the client's own name.
    subject: String,
    /// The screen size the panels were rendered for.
    width: u16,
    /// The screen size the panels were rendered for.
    height: u16,
    /// This run's measurements.
    clock: PhaseClock,
    /// The previous run's measurements — the only thing that can size a bar.
    baseline: PhaseTimings,
}

/// Rows the chrome occupies: header, rule, then at the bottom a rule and the
/// progress substrate's TWO rows (the phase roster and the fact count — see
/// [`progress_line`] for why the count is not on the roster's row).
const CHROME_ROWS: u16 = 5;

impl Frame {
    /// Register `views`, in cycle order, and start the clock on the first
    /// phase.
    ///
    /// Reads the previous run's timings from disk here rather than lazily, so
    /// the frame's whole life is free of I/O and a first-ever run's `no bar`
    /// state is decided once, at a point where it can be observed.
    pub fn new(views: Vec<Box<dyn View>>) -> Frame {
        Frame::with_baseline(views, PhaseTimings::load())
    }

    /// [`Frame::new`] with the baseline supplied rather than read from disk —
    /// the seam every test uses, so no test touches the real state directory.
    pub fn with_baseline(views: Vec<Box<dyn View>>, baseline: PhaseTimings) -> Frame {
        let panels = vec![None; views.len()];
        Frame {
            views,
            panels,
            cursor: 0,
            rung: None,
            facts: 0,
            subject: String::new(),
            width: hornvale_game_core::MIN_WIDTH,
            height: hornvale_game_core::MIN_HEIGHT,
            clock: PhaseClock::start(Phase::Sky),
            baseline,
        }
    }

    /// Set the header's subject — `"seed 42"`, say. Returns `self` so it can
    /// follow [`Frame::new`] in one expression.
    pub fn titled(mut self, subject: impl Into<String>) -> Frame {
        self.subject = subject.into();
        self
    }

    /// Tell the frame the terminal's size. Panels already rendered are kept and
    /// clipped into the new region until the next rung re-renders them: a grid
    /// drawn one rung ago is stale in SIZE but not in content, and showing it
    /// clipped is honest where blanking the region is not.
    pub fn resize(&mut self, w: u16, h: u16) {
        self.width = w;
        self.height = h;
    }

    /// The region a view is given, inside the chrome.
    fn region(&self) -> (u16, u16) {
        (self.width, self.height.saturating_sub(CHROME_ROWS))
    }

    /// A rung landed. Records it, closes that phase on the clock, and re-renders
    /// every view that can now speak.
    ///
    /// This is the shape Task 2's observer hands over; see the type's own doc
    /// for the wiring. It is the ONE moment `world` and `artifacts` are in
    /// scope, which is why it renders rather than merely recording.
    pub fn observe(&mut self, rung: BuildDepth, world: &World, artifacts: RungArtifacts<'_>) {
        self.clock.finish(Phase::for_rung(rung));
        self.rung = Some(rung);
        self.facts = world.ledger.len();
        let (w, h) = self.region();
        // `iter_mut`, per ruling R6: a view may memoise what its arguments
        // imply (the atlas's `NearestVertexIndex`, ~200 ms to build) rather
        // than re-deriving it at each of the four rungs. The two field borrows
        // are disjoint, so this needs no interior mutability.
        for (i, view) in self.views.iter_mut().enumerate() {
            if view.can_speak(rung) {
                self.panels[i] = Some(view.render(world, rung, artifacts, w, h));
            }
        }
        self.reseat();
    }

    /// Close a phase that is NOT a [`BuildDepth`] rung, recording its measured
    /// duration and advancing the clock to whatever follows it.
    ///
    /// **The frame needs this because one of the five phases has no rung.**
    /// [`Frame::observe`] closes `Phase::for_rung(rung)`, and the ladder's
    /// deepest rung maps to [`Phase::DeepTime`] — so [`Phase::Living`], the
    /// post-genesis `WorldContext` build that is 27.2% of the wait, is reached
    /// by no observation at all. Without this method it renders `[>]` forever
    /// and `overture-timings.tsv` is permanently missing its `living` line,
    /// which means the phase that costs a quarter of the wait can never be
    /// paced on any run, ever.
    ///
    /// The caller that owns the work closes it:
    ///
    /// ```no_run
    /// # use hornvale_game::overture::{Frame, Phase};
    /// # fn demo(frame: &mut Frame) {
    /// // …after `build_world_observed` returns and the WorldContext is built:
    /// frame.finish_phase(Phase::Living);
    /// let _ = frame.save_timings();
    /// # }
    /// ```
    ///
    /// Idempotent in shape but not in effect: calling it twice for the same
    /// phase records the second, shorter interval. Call it once, when the work
    /// it names is done.
    pub fn finish_phase(&mut self, phase: Phase) {
        self.clock.finish(phase);
    }

    /// Move the cursor onto a speaking view if it is not on one already, without
    /// otherwise disturbing it — a rung landing must not yank the screen away
    /// from the view the player chose.
    fn reseat(&mut self) {
        if self.speaks(self.cursor) {
            return;
        }
        if let Some(next) = self.next_speaking_from(self.cursor) {
            self.cursor = next;
        }
    }

    /// Whether the view at `i` can speak at the current rung. `false` before
    /// the first observation: no rung has landed, so nothing has anything to
    /// say about a world that does not exist yet.
    fn speaks(&self, i: usize) -> bool {
        match (self.rung, self.views.get(i)) {
            (Some(rung), Some(view)) => view.can_speak(rung),
            _ => false,
        }
    }

    /// The next speaking view strictly after `from`, wrapping, or `None` if none
    /// speaks. Scans at most `len` positions, so it terminates whether or not
    /// `from` itself speaks.
    fn next_speaking_from(&self, from: usize) -> Option<usize> {
        let len = self.views.len();
        if len == 0 {
            return None;
        }
        (1..=len)
            .map(|step| (from + step) % len)
            .find(|i| self.speaks(*i))
    }

    /// Advance to the next view that can speak, skipping every one that cannot
    /// (contract rule 2). A no-op when at most one view speaks — cycling to the
    /// same view is the honest answer, not an empty screen.
    pub fn cycle(&mut self) {
        if let Some(next) = self.next_speaking_from(self.cursor) {
            self.cursor = next;
        }
    }

    /// The name of the view on screen, or [`NO_VIEW`] when none can speak.
    pub fn current_name(&self) -> &'static str {
        if self.speaks(self.cursor) {
            self.views[self.cursor].name()
        } else {
            NO_VIEW
        }
    }

    /// The names of every view that can speak at the current rung, in cycle
    /// order — the `space` cycle's membership, and what the frame's header
    /// offers.
    pub fn visible_views(&self) -> Vec<&'static str> {
        (0..self.views.len())
            .filter(|i| self.speaks(*i))
            .map(|i| self.views[i].name())
            .collect()
    }

    /// The deepest rung observed, or `None` before the first observation.
    pub fn rung(&self) -> Option<BuildDepth> {
        self.rung
    }

    /// The build state the progress substrate renders: the phase in progress,
    /// how far through it the baseline says we are, and the fact count.
    ///
    /// The phase is the one AFTER the last completed rung — a rung firing means
    /// its phase is done — and the fraction is `None` whenever the baseline has
    /// nothing to divide by, which is the first-ever-run state (spec §2).
    pub fn state(&self) -> BuildState {
        let phase = self.clock.current();
        let fraction = self.baseline.fraction_through(phase, self.clock.elapsed());
        let state = match fraction {
            Some(f) => BuildState::in_phase(phase, f),
            None => BuildState::unpaced(phase),
        };
        state.with_facts(self.facts)
    }

    /// The PREVIOUS run's measurements — the baseline that sizes the bar, read
    /// at [`Frame::new`]. Empty on a first-ever run, which is what makes the
    /// substrate draw no bar (spec §2).
    pub fn baseline(&self) -> &PhaseTimings {
        &self.baseline
    }

    /// This run's measurements so far — one entry per phase already closed by
    /// [`Frame::observe`] or [`Frame::finish_phase`].
    pub fn measured(&self) -> &PhaseTimings {
        self.clock.measured()
    }

    /// This run's measurements, to be saved once startup completes so the next
    /// run has a bar. Returns `Ok(false)` when there is nowhere to keep state.
    pub fn save_timings(&self) -> std::io::Result<bool> {
        self.clock.measured().save()
    }

    /// The whole screen: header, rules, the current view's panel, and the
    /// progress substrate.
    ///
    /// Recomposed on every call, because the substrate changes continuously
    /// while the panel changes only at a rung boundary.
    pub fn compose(&self) -> Grid {
        let mut grid = Grid::new(self.width, self.height);
        let header = match self.subject.is_empty() {
            true => "hornvale".to_string(),
            false => format!("hornvale — {}", self.subject),
        };
        write_text(&mut grid, 0, 0, &header, Weight::Bold, Source::Chrome);

        let cycle = self.visible_views();
        if cycle.len() > 1 {
            let right = format!("[ {} ]  < space >", self.current_name());
            let x = self.width.saturating_sub(right.chars().count() as u16);
            write_text(&mut grid, x, 0, &right, Weight::Normal, Source::Chrome);
        }

        rule(&mut grid, 1);
        // Gated on `speaks(cursor)`, matching `current_name` (M6): skip-not-blank
        // must be structural in BOTH predicates. A view whose `can_speak` is not
        // monotone in `rung` would otherwise keep drawing the panel from the
        // rung where it last spoke, underneath a `NO_VIEW` header — a stale
        // picture presented as current, which is worse than an empty region.
        if self.speaks(self.cursor)
            && let Some(panel) = self.panels.get(self.cursor).and_then(Option::as_ref)
        {
            blit(&mut grid, panel, 2);
        }
        if self.height >= CHROME_ROWS {
            let state = self.state();
            rule(&mut grid, self.height - 3);
            write_text(
                &mut grid,
                0,
                self.height - 2,
                &progress::phase_roster(&state),
                Weight::Normal,
                Source::Overture,
            );
            // Right-aligned, as spec §2 draws it. It is on its own row so the
            // 80-column floor cannot clip it — see `progress_line`'s doc.
            let count = progress::fact_count(&state);
            let x = self.width.saturating_sub(count.chars().count() as u16);
            write_text(
                &mut grid,
                x,
                self.height - 1,
                &count,
                Weight::Normal,
                Source::Overture,
            );
        }
        grid
    }
}

/// Write `text` left to right from `(x, y)`, clipped at the grid's right edge.
fn write_text(grid: &mut Grid, x: u16, y: u16, text: &str, weight: Weight, source: Source) {
    for (i, ch) in text.chars().enumerate() {
        let Ok(dx) = u16::try_from(i) else { return };
        let Some(col) = x.checked_add(dx) else { return };
        if col >= grid.width() {
            return;
        }
        grid.set(col, y, Cell::glyph(ch, weight, source));
    }
}

/// A full-width horizontal rule — inert decoration, hence [`Source::Chrome`].
fn rule(grid: &mut Grid, y: u16) {
    for x in 0..grid.width() {
        grid.set(x, y, Cell::glyph('-', Weight::Dim, Source::Chrome));
    }
}

/// Copy `panel` into `grid` with its top-left at `(0, top)`, clipping anything
/// that does not fit. Unmarked cells stay unmarked: a panel that does not fill
/// its region leaves paper, never a box of spaces.
fn blit(grid: &mut Grid, panel: &Grid, top: u16) {
    for y in 0..panel.height() {
        let Some(row) = top.checked_add(y) else {
            return;
        };
        // Stop before the bottom chrome (rule + the substrate's two rows).
        if row + 3 >= grid.height() {
            return;
        }
        for x in 0..panel.width() {
            if x >= grid.width() {
                break;
            }
            // Unmarked paper is COPIED AS NOTHING, not as a space: a panel
            // that does not fill its region leaves the frame's own ground
            // showing, which is contract rule 2 at the pixel level.
            if let Some(cell) = panel.get(x, y)
                && cell.glyph.is_some()
            {
                grid.set(x, row, *cell);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, build_world_to};
    use std::sync::OnceLock;

    /// A view that speaks at every rung and renders its own name, so a test can
    /// tell whose panel is on screen.
    struct AlwaysSpeaks {
        name: &'static str,
        /// How many times `render` has been called — writable only because
        /// ruling R6 made `render` take `&mut self`.
        renders: usize,
    }

    impl AlwaysSpeaks {
        fn named(name: &'static str) -> AlwaysSpeaks {
            AlwaysSpeaks { name, renders: 0 }
        }
    }

    impl View for AlwaysSpeaks {
        fn name(&self) -> &'static str {
            self.name
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
            // R6's memo, exercised rather than merely permitted: a real view
            // caches something costly here (the atlas's `NearestVertexIndex`),
            // and `renders` proves the `&mut self` signature actually lets one.
            self.renders += 1;
            // The count is DRAWN, so a test can observe the memo through the
            // frame's own output instead of reaching into the view. A view under
            // an `&self` signature could not produce this at all.
            // `~`, never `#`: a panel glyph must not be confusable with a
            // progress bar, or a `!contains('#')` assertion about the
            // SUBSTRATE fires on the view's own output instead.
            let text = format!("{}~{}", self.name, self.renders);
            let mut grid = Grid::new(w.max(1), 1);
            write_text(&mut grid, 0, 0, &text, Weight::Normal, Source::Overture);
            grid
        }
    }

    /// A view with nothing honest to say until terrain exists — the shape the
    /// atlas really has.
    #[derive(Default)]
    struct SilentUntilTerrain;

    impl View for SilentUntilTerrain {
        fn name(&self) -> &'static str {
            "silent"
        }
        fn can_speak(&self, rung: BuildDepth) -> bool {
            rung >= BuildDepth::Terrain
        }
        fn render(
            &mut self,
            _world: &World,
            _rung: BuildDepth,
            artifacts: RungArtifacts<'_>,
            w: u16,
            _h: u16,
        ) -> Grid {
            // The whole reason this view waits: it needs the artifact, and the
            // frame must only ever call it when the artifact is there.
            assert!(
                artifacts.terrain.is_some(),
                "rendered without the terrain it declared it needs"
            );
            Grid::new(w.max(1), 1)
        }
    }

    /// A view that speaks at EXACTLY one rung, so its panel becomes stale as the
    /// build moves past it. Contrived, and deliberately so: the trait only asks
    /// `can_speak` to be a pure function of `rung`, never a monotone one, so the
    /// frame must not assume otherwise.
    struct SpeaksOnlyAtTerrain;

    impl View for SpeaksOnlyAtTerrain {
        fn name(&self) -> &'static str {
            "fickle"
        }
        fn can_speak(&self, rung: BuildDepth) -> bool {
            rung == BuildDepth::Terrain
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
            write_text(&mut grid, 0, 0, "fickle", Weight::Normal, Source::Overture);
            grid
        }
    }

    /// The phase-roster row of a composed frame, trailing padding trimmed — the
    /// row every substrate assertion is really about. Reading it off the
    /// composed grid rather than calling `phase_roster` directly keeps the
    /// assertion end-to-end: it fails if `compose` puts the roster on the wrong
    /// row, or clips it, as well as if the roster itself is wrong.
    fn roster_row(frame: &Frame) -> String {
        let text = frame.compose().to_plain_text();
        let rows: Vec<&str> = text.lines().collect();
        rows[rows.len() - 2].trim_end().to_string()
    }

    fn frame_of(views: Vec<Box<dyn View>>) -> Frame {
        // `with_baseline`, never `new`: no test in this module reads or writes
        // the real state directory.
        Frame::with_baseline(views, PhaseTimings::empty())
    }

    /// A real seed-42 world at `depth`, built ONCE per test binary.
    ///
    /// Cached because these tests want a genuine world (a synthetic one could
    /// not exercise the fact count coming off a real ledger) and because this
    /// crate's tests are compiled unoptimized: a per-test `build_world_to` at
    /// the terrain rung is seconds of debug-build genesis, and `game-check`
    /// already costs minutes. Every test here reads the world and none mutates
    /// it, so one copy is correct as well as cheap.
    fn world_at(depth: BuildDepth) -> &'static World {
        static SKY: OnceLock<World> = OnceLock::new();
        static LAND: OnceLock<World> = OnceLock::new();
        let build = || {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            build_world_to(
                Seed(42),
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                depth,
            )
            .expect("seed 42 builds")
        };
        match depth {
            BuildDepth::Astronomy => SKY.get_or_init(build),
            BuildDepth::Terrain => LAND.get_or_init(build),
            deeper => panic!("no cached world for {deeper:?}; add one deliberately"),
        }
    }

    /// A stand-in `GeneratedTerrain` for the rungs that have one, built once for
    /// the same reason [`world_at`] is. The same call the composition root
    /// makes, since nothing here needs a ledger.
    fn terrain() -> &'static hornvale_terrain::GeneratedTerrain {
        static TERRAIN: OnceLock<hornvale_terrain::GeneratedTerrain> = OnceLock::new();
        TERRAIN.get_or_init(|| {
            let geo = hornvale_kernel::Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
            let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
                .expect("default pins generate seed 42");
            hornvale_terrain::GeneratedTerrain::new(geo, outcome)
        })
    }

    #[test]
    fn a_view_that_cannot_speak_at_a_rung_is_skipped_not_blanked() {
        // Contract rule 2. A view with nothing honest to say must DECLARE that,
        // so the frame can skip it — rather than rendering an empty region that
        // reads as a hang.
        let mut frame = frame_of(vec![Box::new(SilentUntilTerrain)]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        assert!(
            frame.visible_views().is_empty(),
            "a view with nothing to say must be skipped, not shown blank"
        );
        // And it must not have been RENDERED either: `SilentUntilTerrain::
        // render` asserts on its artifact, so a frame that rendered it anyway
        // would have panicked above rather than reaching here.
        let land = world_at(BuildDepth::Terrain);
        let terrain = terrain();
        frame.observe(
            BuildDepth::Terrain,
            land,
            RungArtifacts {
                terrain: Some(terrain),
                climate: None,
            },
        );
        assert_eq!(frame.visible_views().len(), 1);
    }

    #[test]
    fn space_cycles_only_among_views_that_can_speak() {
        // Non-vacuity guard: with one view registered this passes trivially, so
        // register THREE and assert the cycle visits exactly the speaking ones.
        let mut frame = frame_of(vec![
            Box::new(AlwaysSpeaks::named("a")),
            Box::new(SilentUntilTerrain),
            Box::new(AlwaysSpeaks::named("c")),
        ]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        let mut visited = Vec::new();
        for _ in 0..4 {
            visited.push(frame.current_name().to_string());
            frame.cycle();
        }
        assert_eq!(
            visited,
            vec!["a", "c", "a", "c"],
            "cycle visited a silent view"
        );
        // The silent view is registered BETWEEN the two speakers, so a cycle
        // that merely incremented would have produced "silent" at position 1.
        assert!(!visited.contains(&"silent".to_string()));
    }

    #[test]
    fn the_silent_view_joins_the_cycle_once_its_rung_lands() {
        // The other direction of the same rule: skipping must be a function of
        // the RUNG, not a permanent exclusion. Without this, an implementation
        // that dropped a non-speaking view at registration would pass the test
        // above.
        let mut frame = frame_of(vec![
            Box::new(AlwaysSpeaks::named("a")),
            Box::new(SilentUntilTerrain),
            Box::new(AlwaysSpeaks::named("c")),
        ]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        assert_eq!(frame.visible_views(), vec!["a", "c"]);

        let land = world_at(BuildDepth::Terrain);
        let terrain = terrain();
        frame.observe(
            BuildDepth::Terrain,
            land,
            RungArtifacts {
                terrain: Some(terrain),
                climate: None,
            },
        );
        assert_eq!(frame.visible_views(), vec!["a", "silent", "c"]);
        let mut visited = Vec::new();
        for _ in 0..3 {
            visited.push(frame.current_name().to_string());
            frame.cycle();
        }
        assert_eq!(visited, vec!["a", "silent", "c"]);
    }

    #[test]
    fn before_the_first_rung_nothing_speaks_and_nothing_is_named() {
        // The frame exists before genesis has produced anything at all. It must
        // not name a view it has never been able to render.
        let frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        assert!(frame.visible_views().is_empty());
        assert_eq!(frame.current_name(), NO_VIEW);
    }

    #[test]
    fn a_rung_landing_does_not_yank_the_screen_off_the_players_choice() {
        let mut frame = frame_of(vec![
            Box::new(AlwaysSpeaks::named("a")),
            Box::new(AlwaysSpeaks::named("c")),
        ]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        frame.cycle();
        assert_eq!(frame.current_name(), "c");
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        assert_eq!(
            frame.current_name(),
            "c",
            "a rung landing reset the cycle position"
        );
    }

    #[test]
    fn an_empty_roster_neither_panics_nor_claims_a_view() {
        let mut frame = frame_of(Vec::new());
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        frame.cycle();
        assert_eq!(frame.current_name(), NO_VIEW);
        assert!(frame.visible_views().is_empty());
    }

    #[test]
    fn the_composed_screen_carries_the_current_panel_and_the_substrate() {
        let mut frame = frame_of(vec![
            Box::new(AlwaysSpeaks::named("alpha")),
            Box::new(AlwaysSpeaks::named("gamma")),
        ]);
        // Composed at the FLOOR, not above it. An earlier revision widened this
        // to 140 columns to get a clipped assertion to fire, which was the
        // wrong direction: `hornvale-game-core`'s own register is that 80x24 is
        // the floor and anything that only works larger is wrong. The substrate
        // narrowed instead (it is two rows now), so the assertions below hold at
        // the width the client actually refuses to go under —
        // `the_whole_substrate_is_on_screen_at_the_eighty_column_floor` is the
        // dedicated test for that.
        frame.resize(hornvale_game_core::MIN_WIDTH, 24);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        let text = frame.compose().to_plain_text();
        assert!(text.contains("hornvale"), "no header: {text}");
        assert!(
            text.contains(&format!("{} facts", sky.ledger.len())),
            "the grouped fact count did not reach the screen: {text}"
        );
        assert!(
            text.contains("alpha"),
            "the current panel is missing: {text}"
        );
        assert!(
            !text.contains("gamma"),
            "a view that is not current was drawn: {text}"
        );
        assert!(text.contains("the land"), "no progress substrate: {text}");
        assert!(
            !text.contains('%'),
            "a percentage reached the screen: {text}"
        );

        // Cycling swaps the panel WITHOUT another observation, which is the
        // whole point of caching a grid per view — the world and its artifacts
        // are long out of scope by now.
        frame.cycle();
        let text = frame.compose().to_plain_text();
        assert!(
            text.contains("gamma"),
            "cycling did not swap the panel: {text}"
        );
        assert!(!text.contains("alpha"), "both panels drawn: {text}");
    }

    #[test]
    fn the_fact_count_comes_off_the_observed_world_not_a_guess() {
        let mut frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        let after_sky = frame.state().facts();
        assert_eq!(after_sky, sky.ledger.len());
        assert!(after_sky > 0, "the astronomy rung commits facts");

        // And it GROWS with the ledger — a constant would satisfy the equality
        // above at one rung.
        let land = world_at(BuildDepth::Terrain);
        let terrain = terrain();
        frame.observe(
            BuildDepth::Terrain,
            land,
            RungArtifacts {
                terrain: Some(terrain),
                climate: None,
            },
        );
        assert_eq!(frame.state().facts(), land.ledger.len());
        assert!(frame.state().facts() > after_sky);
    }

    #[test]
    fn with_no_timings_on_disk_the_frame_draws_no_bar() {
        // Spec §2's first-ever run, end to end through the frame rather than
        // through `progress_line` alone: an empty baseline must reach the
        // composed screen as a bar-free substrate.
        let mut frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        frame.resize(100, 24);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        // Scoped to the SUBSTRATE rows, not the whole grid: a view's own panel
        // may legitimately contain any glyph, and asserting over the whole
        // screen makes this test's subject the views rather than the substrate.
        let roster = roster_row(&frame);
        assert!(
            !roster.contains('#'),
            "a first-ever run drew a bar: {roster}"
        );
        // The `#`-free half alone cannot tell "no bar" from "an EMPTY bar",
        // which is a claim of zero rather than the honest absence of a
        // measurement — a mutation setting the fraction to `Some(0.0)` drew
        // `..........` and slipped past it. The marker is what discriminates.
        assert!(
            roster.contains("the land [>]"),
            "a first-ever run must still mark the phase in progress: {roster}"
        );

        // Non-vacuity: the same frame WITH a baseline does draw one, so the
        // assertion above is about the baseline and not about the composer
        // being incapable of bars.
        let mut baseline = PhaseTimings::empty();
        baseline.set(Phase::Land, std::time::Duration::from_millis(5_000));
        let mut paced = Frame::with_baseline(vec![Box::new(AlwaysSpeaks::named("a"))], baseline);
        paced.resize(100, 24);
        paced.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        let roster = roster_row(&paced);
        assert!(
            roster.contains("the land .........."),
            "a paced frame drew no phase bar at all: {roster}"
        );
        assert!(
            !roster.contains("the land [>]"),
            "a paced frame drew the UNPACED marker: {roster}"
        );
    }

    #[test]
    fn the_whole_substrate_is_on_screen_at_the_eighty_column_floor() {
        // I1. `hornvale-game-core`'s register: 80x24 is the floor and `render`
        // refuses anything smaller rather than degrading — so the substrate must
        // be entirely legible THERE, not merely somewhere. The fact count is the
        // half that used to fall off, and spec §2 makes it mandatory.
        let mut frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        frame.resize(
            hornvale_game_core::MIN_WIDTH,
            hornvale_game_core::MIN_HEIGHT,
        );
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        let state = frame.state();
        let grid = frame.compose();
        let text = grid.to_plain_text();

        // Every phase name AND the grouped count, whole, on a floor-sized grid.
        for phase in Phase::ALL {
            assert!(
                text.contains(phase.label()),
                "{} clipped at the floor: {text}",
                phase.label()
            );
        }
        let count = progress::fact_count(&state);
        assert!(
            text.contains(&count),
            "the fact count {count:?} was clipped at the floor: {text}"
        );

        // And no row of the composed grid exceeds the floor — the write path
        // clips silently, so a too-long row is invisible unless measured.
        for row in text.lines() {
            assert!(
                row.chars().count() <= usize::from(hornvale_game_core::MIN_WIDTH),
                "a composed row overran the floor: {row}"
            );
        }
        // Non-vacuity: the count must be a real grouped number, or
        // `contains` above could be satisfied by an empty string.
        assert!(count.ends_with(" facts") && count.len() > 6, "{count:?}");
    }

    #[test]
    fn every_phase_including_the_one_with_no_rung_can_be_closed_and_recorded() {
        // I2. `observe` closes `Phase::for_rung(rung)`, and the deepest rung maps
        // to `DeepTime` — so `Phase::Living` (27.2% of the wait) is reached by no
        // observation at all and was permanently unmeasurable. A full startup
        // must be able to write all FIVE lines, or the phase that costs a quarter
        // of the wait can never be paced on any run.
        let dir = std::env::temp_dir().join(format!(
            "hornvale-overture-frame-living-{}",
            std::process::id()
        ));
        std::fs::remove_dir_all(&dir).ok();
        let path = dir.join("timings.tsv");

        let mut frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        let sky = world_at(BuildDepth::Astronomy);
        let land = world_at(BuildDepth::Terrain);
        let terrain = terrain();
        let art = RungArtifacts {
            terrain: Some(terrain),
            climate: None,
        };
        // Every rung the ladder has…
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        frame.observe(BuildDepth::Terrain, land, art);
        frame.observe(BuildDepth::Settlements, land, art);
        frame.observe(BuildDepth::Full, land, art);
        // …and then the phase that has none.
        assert_eq!(
            frame.state().phase(),
            Phase::Living,
            "after the deepest rung the substrate must be on the living phase"
        );
        frame.finish_phase(Phase::Living);

        frame
            .measured()
            .save_to(&path)
            .expect("write the measured timings");
        let read = PhaseTimings::load_from(&path);
        for phase in Phase::ALL {
            assert!(
                read.get(phase).is_some(),
                "{} was never recorded, so it can never be paced",
                phase.key()
            );
        }
        assert_eq!(
            read.to_tsv().lines().count(),
            5,
            "a full startup must write one line per phase: {}",
            read.to_tsv()
        );
        // Non-vacuity: WITHOUT the `finish_phase` call the file has four lines,
        // so the assertion above is about `finish_phase` and not about
        // `PhaseTimings` writing whatever it is given.
        let mut four = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        four.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        four.observe(BuildDepth::Terrain, land, art);
        four.observe(BuildDepth::Settlements, land, art);
        four.observe(BuildDepth::Full, land, art);
        assert_eq!(four.measured().get(Phase::Living), None);
        assert_eq!(four.measured().to_tsv().lines().count(), 4);

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn a_view_may_memoise_across_rungs_because_render_takes_mut_self() {
        // R6. The atlas needs a `NearestVertexIndex` (~200 ms to build) and can
        // only reach the geosphere through `artifacts.terrain`, i.e. from inside
        // `render`. `observe` renders every speaking view at every rung, so an
        // `&self` signature would force a rebuild per rung: ~600 ms on a
        // 3,054 ms build. This asserts the signature genuinely admits a memo —
        // the stub counts its own renders, which `&self` could not do.
        let mut frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        frame.resize(hornvale_game_core::MIN_WIDTH, 24);
        let text = frame.compose().to_plain_text();
        assert!(
            text.contains("a~2"),
            "the view could not carry state across renders: {text}"
        );
    }

    #[test]
    fn the_composed_screen_never_shows_a_stale_panel_under_a_no_view_header() {
        // M6. `current_name` gates on `speaks(cursor)` and `compose` must too,
        // or a view whose `can_speak` is non-monotone in `rung` keeps drawing the
        // panel from the rung where it last spoke — a stale picture presented as
        // current, which is worse than an empty region.
        let mut frame = frame_of(vec![Box::new(SpeaksOnlyAtTerrain)]);
        frame.resize(hornvale_game_core::MIN_WIDTH, 24);
        let land = world_at(BuildDepth::Terrain);
        let terrain = terrain();
        frame.observe(
            BuildDepth::Terrain,
            land,
            RungArtifacts {
                terrain: Some(terrain),
                climate: None,
            },
        );
        assert!(frame.compose().to_plain_text().contains("fickle"));

        // The rung advances and the view goes quiet again.
        frame.observe(
            BuildDepth::Full,
            land,
            RungArtifacts {
                terrain: Some(terrain),
                climate: None,
            },
        );
        assert_eq!(frame.current_name(), NO_VIEW);
        assert!(
            !frame.compose().to_plain_text().contains("fickle"),
            "a stale panel was drawn under a NO_VIEW header"
        );
    }

    #[test]
    fn the_phase_in_progress_is_the_one_after_the_last_completed_rung() {
        // A rung firing means its phase is DONE. So after the astronomy rung the
        // substrate must be barring `the land`, never `the sky` — the off-by-one
        // that would make every bar describe work already finished.
        let mut frame = frame_of(vec![Box::new(AlwaysSpeaks::named("a"))]);
        let sky = world_at(BuildDepth::Astronomy);
        frame.observe(BuildDepth::Astronomy, sky, RungArtifacts::none());
        assert_eq!(frame.state().phase(), Phase::Land);

        let land = world_at(BuildDepth::Terrain);
        let terrain = terrain();
        frame.observe(
            BuildDepth::Terrain,
            land,
            RungArtifacts {
                terrain: Some(terrain),
                climate: None,
            },
        );
        assert_eq!(frame.state().phase(), Phase::Peoples);
    }
}
