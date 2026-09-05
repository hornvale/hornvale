//! One sentence: what a walker is told when standing where an occupation
//! died.
//!
//! **The cause-to-prose mapping lives HERE, not on `CauseOfEnd` itself**
//! (controller ruling, The Weft, Task 2). `domains/history` has no rendering
//! method anywhere in it — this repo's layering is explicit that a domain
//! draws world-state and a window renders it — so adding a `phrase()` to
//! `CauseOfEnd` would introduce the first prose method into a domain crate.
//! The match below is exhaustive with **no `_` arm**: a catch-all would
//! silently render a future `CauseOfEnd` variant as whatever the fallback
//! says, and an exhaustive match instead makes the compiler enumerate the
//! work for whoever adds one.

use crate::brief::RuinSignature;
use hornvale_history::record::CauseOfEnd;

/// The walk-band line for a ruin. Never empty — a causeless ruin still reads
/// as a ruin, because the absence of a recorded cause is not the absence of
/// a ruin.
/// type-audit: bare-ok(prose: return)
#[must_use]
pub fn ruin_line(sig: &RuinSignature) -> String {
    match (&sig.cause, sig.by_hand) {
        (Some(c), _) => format!(" Something ended here: {}.", cause_phrase(*c)),
        (None, true) => " Something was ended here by other hands.".to_string(),
        (None, false) => " Something ended here.".to_string(),
    }
}

/// The prose a cause of ending reads as. Exhaustive on purpose — see the
/// module header.
fn cause_phrase(cause: CauseOfEnd) -> &'static str {
    match cause {
        CauseOfEnd::Famine => "famine",
        CauseOfEnd::Burned => "fire",
        CauseOfEnd::Plague => "plague",
        CauseOfEnd::Fled => "flight",
        CauseOfEnd::Migrated => "migration",
        CauseOfEnd::Breached => "a breach",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every variant renders through the exhaustive match without panicking
    /// and produces non-empty prose — a cheap witness that the match stays
    /// total as `CauseOfEnd` grows.
    #[test]
    fn every_cause_renders_nonempty_prose() {
        let causes = [
            CauseOfEnd::Famine,
            CauseOfEnd::Burned,
            CauseOfEnd::Plague,
            CauseOfEnd::Fled,
            CauseOfEnd::Migrated,
            CauseOfEnd::Breached,
        ];
        for c in causes {
            assert!(!cause_phrase(c).is_empty());
        }
    }
}
