//! The view contract — spec §3, and the one file in this campaign that four
//! later tasks are written against.
//!
//! A **view** owns the middle of the startup screen and nothing else. The
//! frame owns the chrome, the progress substrate and the navigation; a view is
//! handed a real world at a real rung and asked for a grid.

use hornvale_game_core::Grid;
use hornvale_kernel::World;
use hornvale_worldgen::{BuildDepth, RungArtifacts};

/// One screen-level plugin in the startup frame: given the partially built
/// world at some rung of the [`BuildDepth`] ladder, draw it.
///
/// # The contract
///
/// 1. **Render honestly at ANY rung, including the first.** This is
///    [`BuildDepth`]'s prefix property doing work: earlier rungs are a
///    byte-identical prefix of later ones, so a view at rung *n* is looking at
///    a real world, not a half-built one.
/// 2. **Show what EXISTS — never a placeholder for what does not yet.** A view
///    with nothing honest to say at a rung says so through
///    [`can_speak`](View::can_speak) and is **skipped** by the frame; it is
///    never rendered blank, because an empty region reads as a hang.
/// 3. **Fill the time it is given without implying a total it cannot know.** No
///    view draws a global percentage; only the frame draws progress, and only
///    per phase (decision 0359 — one phase is 60% of the whole wait).
///
/// # What is deliberately NOT on this trait
///
/// No pacing hint. The spec's §3 sketch mentioned one; the frame owns pacing
/// end to end (it holds the clock and the previous run's timings), and no view
/// in this campaign needs it. Adding one later is additive.
///
/// A concrete view is of course free to have its own inherent helpers — a sky
/// view's `sky_position`, an atlas's `sample`. The rule that no later task adds
/// a method binds *this trait*, not the impls.
pub trait View {
    /// The name shown in the frame's header and used by the cycle.
    fn name(&self) -> &'static str;

    /// Whether this view has anything HONEST to say at `rung`.
    ///
    /// Contract rule 2: a view that cannot speak is SKIPPED by the frame, never
    /// rendered blank. `rung` is the deepest rung the build has **completed**,
    /// so a view needing terrain answers `false` at
    /// [`BuildDepth::Astronomy`] and `true` from [`BuildDepth::Terrain`] on.
    ///
    /// Must be a pure function of `rung`: the frame calls it to decide the
    /// cycle's membership, including between rung boundaries when no world is
    /// in hand.
    fn can_speak(&self, rung: BuildDepth) -> bool;

    /// Render into a `w` x `h` region. Only called when
    /// [`can_speak`](View::can_speak) is true for `rung`.
    ///
    /// `rung` is the same value `can_speak` was asked about — the deepest
    /// completed rung — and is handed over rather than re-derived so a view can
    /// compose *components* that each declare the rung they need (spec §3's
    /// component level). `artifacts` carries the derived terrain and climate
    /// the build is already holding, `Some` exactly on the rungs that built
    /// them: a view MUST NOT re-derive either with `terrain_of`/`climate_from`
    /// (decision 0092, and ~199 ms + ~69 ms per render if it tried).
    ///
    /// The returned grid may be smaller than `w` x `h` — the frame places it
    /// into the region and leaves the remainder as unmarked paper, which is
    /// honest (rule 2) in a way a padded box of spaces is not.
    ///
    /// # Why `&mut self`
    ///
    /// So a view can build an expensive derived structure ONCE and keep it,
    /// which is this codebase's standing doctrine rather than a new idea:
    /// `crate::plate`'s module doc states that a `NearestVertexIndex` "is built
    /// ONCE by the caller and passed in, never rebuilt per call", because a
    /// per-call `NearestVertexIndex::new(geo)` costs ~200 ms — and `Driver`
    /// duly builds its own at `start` and reuses it for the session. The atlas
    /// view needs exactly that index and can only get the geosphere from
    /// `artifacts.terrain`, i.e. from inside this call. Since
    /// [`super::Frame::observe`] renders every speaking view at every rung, an
    /// `&self` signature would make the atlas re-derive it three times per
    /// startup: ~600 ms on a 3,054 ms build, a ~20% regression in the one path
    /// this campaign exists to improve.
    ///
    /// A `OnceLock` field under an `&self` signature would work and is
    /// deliberately not the answer: it hides mutation behind an immutable
    /// signature and makes every view reinvent the same cell.
    ///
    /// **`&mut self` is a memo, not a licence.** Rendering must stay a pure
    /// function of `(world, rung, artifacts, w, h)` as far as its OUTPUT is
    /// concerned: two calls with the same arguments must return the same grid.
    /// The mutability is for caching what those arguments imply, never for
    /// carrying state between renders that changes what is drawn.
    fn render(
        &mut self,
        world: &World,
        rung: BuildDepth,
        artifacts: RungArtifacts<'_>,
        w: u16,
        h: u16,
    ) -> Grid;
}
