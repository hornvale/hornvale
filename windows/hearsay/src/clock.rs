//! Whether a community was still standing when an event it holds took place.
//!
//! Spec §5.1. `variants_about` and its accumulating sibling walk the founding
//! tree as pure structure, with no notion of when anybody existed, so a
//! community that ceased 500 years before an event still inherits a claim
//! about it: 1,959 of 164,822 holders (1.19%), measured in
//! `tests/probe_contact_substrate.rs`.
//!
//! This is also the second world-time clock campaign 2's §7 said did not
//! exist ("`Claim` carries no time, so `hops` is the only clock"). The ledger
//! carries founding and ending days, and this reads them.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};

/// Whether transmission respects world time.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Clock {
    /// Ships today: transmission is pure structure and ignores world time.
    Off,
    /// A holder must not have ended before the event it holds.
    Alive,
}

impl Clock {
    /// Every arm, in a fixed order so a readout's columns are stable.
    pub const ALL: [Clock; 2] = [Clock::Off, Clock::Alive];

    /// This arm's short name, used as a readout column suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Clock::Off => "no-clock",
            Clock::Alive => "clock",
        }
    }
}

/// Whether `occ` may hold a claim about an event on `event_day`.
///
/// `Clock::Off` admits everything, reproducing today's behaviour exactly.
/// Under `Clock::Alive` an occupation with no `occ-ended` fact is still
/// standing and is always admitted; one that ended is admitted iff it ended
/// on or after the event day. **On or after, not strictly after**: the subject
/// of an ending ends on precisely that day and must remain a witness to it.
///
/// Total by construction — a non-`Number` `occ-ended` admits, the same posture
/// [`crate::amplitude::gen_span`] takes toward unreadable durations.
/// type-audit: bare-ok(count: event_day), bare-ok(flag: return)
pub fn admits(clock: Clock, ledger: &Ledger, occ: EntityId, event_day: f64) -> bool {
    match clock {
        Clock::Off => true,
        Clock::Alive => match ledger.value_of(occ, hornvale_history::OCC_ENDED) {
            Some(Value::Number(ended)) => *ended >= event_day,
            _ => true,
        },
    }
}
