//! The Hand, Task 5 (fix round 1): the controller stack (Arc II of The
//! Bridle). GOAP and player input are the same shape — a decision over one
//! body's ALREADY-ARBITRATED resolution for one tick — so both are
//! implementations of one trait rather than two separate code paths the tick
//! loop has to special-case (spec §3.3: "Nothing about `advance_one` learns
//! who chose").
//!
//! **Fix round 1 widened this trait.** The first cut gave `intend` a
//! `Perceived`/`Mode` pair narrow enough to carry only the Stage-0
//! (thirst-only) decision — genuinely too little to reach `decide_step`'s
//! real per-species multi-drive arbitration, so `DefaultController` could
//! only ever wrap a DIFFERENT, simpler computation than the one that
//! actually drives every NPC, and the whole apparatus ran beside the real
//! walk rather than inside it. `intend` now takes the [`Resolution`] the
//! walk's own `arbitrate` call already produced — the co-present read (spec
//! §2.3: "the loop arbitrates first, unconditionally") — so `DefaultController`
//! is a trivial, byte-identical pass-through of what already happens today,
//! and `PlayerController` can override it without needing to re-derive it.

use crate::action::Action;
use crate::body::Body;
use crate::liveness::{Intent, Resolution};

/// One tick's decision-maker for one body: given the [`Resolution`] the
/// body's own arbitration ALREADY reached this tick (its mode, its affect,
/// and the intent that arbitration itself would act on), what does it
/// actually intend to do? GOAP ([`DefaultController`]) and a human's typed
/// commands ([`PlayerController`]) are the two implementations The Hand
/// ships; Arc III (The Coercion) adds an imposed third without touching this
/// trait. `advance_one` calls this exactly once per decision point, for
/// every body alike (spec §3.3: "Nothing about `advance_one` learns who
/// chose") — the controller answers only "what happens", never "what does
/// the body feel", which `resolution.mode`/`resolution.affect` already
/// settled before this is called.
pub trait Controller {
    /// The intent this controller has for `body`, given the resolution its
    /// own arbitration already reached this tick.
    fn intend(&mut self, body: &Body, resolution: &Resolution) -> Intent;
}

/// GOAP, demoted to "the default controller" (The Bridle's own phrasing,
/// spec §2.1): a body nobody is driving acts on exactly what its own
/// arbitration decided — a trivial pass-through of `resolution.intent`, so a
/// body under this controller is byte-identical to a body with no controller
/// at all. This is what keeps every existing NPC's committed trail unchanged:
/// `DriveMovements`'s per-body walk now asks this controller instead of
/// reading `resolution.intent` directly, and the two are the same value by
/// construction, not by coincidence.
pub struct DefaultController;

impl Controller for DefaultController {
    fn intend(&mut self, _body: &Body, resolution: &Resolution) -> Intent {
        resolution.intent.clone()
    }
}

/// A human's typed commands, as a [`Controller`]: whatever action is
/// [`queue`](Self::queue)d for this tick, taken once and then gone —
/// `resolution.intent` (what the body's OWN arbitration wanted) is read by
/// nobody here and simply discarded. Nothing pending is `Intent::Hold`,
/// never a GOAP fallback: a driven body with nothing queued waits on the
/// player, it does not quietly act for itself.
///
/// **This is NOT what keeps the driven body's own walk out of the ledger**
/// (fix round 3, N4 — an earlier version of this comment claimed it was, "the
/// mechanical content of spec §2.3's commits-on-`Do` argument"; checked
/// directly and that is false). `Session::wait` discards
/// `DriveMovements::step_one_with_controller`'s returned facts
/// UNCONDITIONALLY, regardless of what this `intend` answers — forcing it to
/// `Intent::Do` still leaves the ledger untouched, because those facts never
/// reach `tick()` at all. What THIS controller actually guarantees is
/// narrower and still real: a driven body's own walk (`Session::wait`
/// constructs a fresh one every tick) never autonomously acts on its own
/// drives, because its intent is unconditionally `Hold` until a verb routes
/// a real action through [`queue`](Self::queue) (later Bridle work; today's
/// verb loop — `go`, `drink`, … — still commits directly, spec §1 "Does not
/// ship: the host speaking").
#[derive(Default)]
pub struct PlayerController {
    pending: Option<Action>,
}

impl PlayerController {
    /// A controller with nothing queued yet.
    pub fn new() -> Self {
        Self::default()
    }

    /// Queue `action` as this controller's next [`Controller::intend`] answer.
    pub fn queue(&mut self, action: Action) {
        self.pending = Some(action);
    }
}

impl Controller for PlayerController {
    fn intend(&mut self, _body: &Body, _resolution: &Resolution) -> Intent {
        match self.pending.take() {
            Some(action) => Intent::Do(action),
            None => Intent::Hold,
        }
    }
}

/// A controller supplied by whoever holds the body (The Coercion) — the
/// generalisation The Hand's spec predicted: "`driven: usize` generalises to a
/// controller map in Arc III without the body type changing."
///
/// It wraps another controller rather than inventing intents, because
/// decision 0168 puts the effect with the BODY and not the driver: an imposed
/// driver selects WHICH act, never WHAT the act does. Today it delegates to
/// [`DefaultController`], which is a pure pass-through of
/// `resolution.intent` — so wrapping it here changes **nothing** about the
/// body's behaviour, only who is recorded as choosing it. That is an honest
/// admission, not a shortfall: `DefaultController`'s own doc already states a
/// body under it is "byte-identical to a body with no controller at all", and
/// this type does not touch `intend` at all, so the identity carries through
/// unchanged. A later campaign giving a possessing creature real intent swaps
/// the inner controller for one that reads *that* creature's own arbitration,
/// and nothing else about this type needs to change.
pub struct ImposedController {
    inner: DefaultController,
}

impl ImposedController {
    /// A fresh imposed controller, wrapping a fresh [`DefaultController`].
    pub fn new() -> Self {
        Self {
            inner: DefaultController,
        }
    }
}

impl Default for ImposedController {
    fn default() -> Self {
        Self::new()
    }
}

impl Controller for ImposedController {
    fn intend(&mut self, body: &Body, resolution: &Resolution) -> Intent {
        self.inner.intend(body, resolution)
    }
}
