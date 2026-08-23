//! The Hand, Task 5: the controller stack (Arc II of The Bridle). GOAP and
//! player input are the same shape — a decision over one body's perceived
//! view for one tick — so both are implementations of one trait rather than
//! two separate code paths the tick loop has to special-case (spec §3.3:
//! "Nothing about `advance_one` learns who chose").

use crate::action::Action;
use crate::liveness::{Intent, Mode, Npc, Perceived, SUSTENANCE, decide};

/// One tick's decision-maker for one body: given what the body perceives and
/// the commitment mode it carries into this tick, what does it intend to do?
/// GOAP ([`DefaultController`]) and a human's typed commands
/// ([`PlayerController`]) are the two implementations The Hand ships; Arc III
/// (The Coercion) adds an imposed third without touching this trait.
pub trait Controller {
    /// The intent this controller has for `body` on this tick.
    fn intend(&mut self, body: &Npc, view: &Perceived, mode: Mode) -> Intent;
}

/// GOAP, demoted to "the default controller" (The Bridle's own phrasing,
/// spec §2.1): a body nobody is driving still wants water, still gets tired,
/// still walks home. `intend` is today's [`decide`] call, moved onto the
/// trait **verbatim** — the Stage-0 single-drive (thirst-only) decision,
/// unchanged in value, keyed on the body's own home rather than a bare
/// `RoomAddr` so a caller with an `&Npc` in hand needs nothing else.
pub struct DefaultController;

impl Controller for DefaultController {
    fn intend(&mut self, body: &Npc, view: &Perceived, _mode: Mode) -> Intent {
        decide(view, &body.home, &SUSTENANCE, crate::liveness::PLAN_BUDGET)
    }
}

/// A human's typed commands, as a [`Controller`]: whatever action is
/// [`queue`](Self::queue)d for this tick, taken once and then gone;
/// nothing pending is `Intent::Hold`, never a GOAP fallback — a driven body
/// with nothing queued waits on the player, it does not quietly act for
/// itself. The Hand's tests drive a possessed body's own movement through
/// the existing verb loop (`go`, `drink`, …), which commits directly rather
/// than routing through `queue`; wiring every verb through this controller
/// is later Bridle work (spec §1, "Does not ship: the host speaking").
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
    fn intend(&mut self, _body: &Npc, _view: &Perceived, _mode: Mode) -> Intent {
        match self.pending.take() {
            Some(action) => Intent::Do(action),
            None => Intent::Hold,
        }
    }
}
