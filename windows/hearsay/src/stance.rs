//! Stance: where a community stands relative to one event.
//!
//! Stance is relative to a *claim's subject*, not a standing property of any
//! community — the same occupation is a `Perpetrator` toward the raid it
//! caused and a `Bystander` toward one it had nothing to do with. Keying on
//! anything else (a fixed teller/hearer relationship) distorts every account
//! a raider ever passes on, which is why `stance_of` always takes the
//! subject as an explicit argument.

use crate::lineage::Lineage;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};

/// Where a community stands relative to one event's subject.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stance {
    /// The community named as the event's attacker (`occ-ended-by`).
    Perpetrator,
    /// The subject itself, or one of its descendants.
    VictimLine,
    /// Neither the perpetrator nor part of the victim's line.
    Bystander,
}

/// `who`'s stance toward the event whose subject is `subject`.
///
/// Checks `occ-ended-by` first: a community named as the attacker is the
/// `Perpetrator` regardless of any lineage relationship to `subject`. Failing
/// that, `who` is `VictimLine` when it *is* `subject` or descends from it
/// (`Lineage::is_ancestor`), and `Bystander` otherwise. An event with no
/// named attacker (no `occ-ended-by` fact) still partitions the world into
/// `VictimLine` and `Bystander` — stance never goes inert just because
/// nobody was blamed.
pub fn stance_of(ledger: &Ledger, lineage: &Lineage, subject: EntityId, who: EntityId) -> Stance {
    if let Some(Value::Entity(attacker)) = ledger.value_of(subject, hornvale_history::OCC_ENDED_BY)
        && *attacker == who
    {
        return Stance::Perpetrator;
    }
    if who == subject || lineage.is_ancestor(subject, who) {
        Stance::VictimLine
    } else {
        Stance::Bystander
    }
}

/// Whether retelling the event about `subject` from `teller` to `hearer`
/// crosses a stance boundary.
///
/// Legitimate here because both sides are values of one axis (`Stance`),
/// unlike comparing two unrelated booleans: a retelling is lossy exactly
/// when the teller and the hearer stand in different relationships to the
/// event, because that is where the retelling's framing has to shift.
/// type-audit: bare-ok(flag: return)
pub fn is_lossy(
    ledger: &Ledger,
    lineage: &Lineage,
    subject: EntityId,
    teller: EntityId,
    hearer: EntityId,
) -> bool {
    stance_of(ledger, lineage, subject, teller) != stance_of(ledger, lineage, subject, hearer)
}
