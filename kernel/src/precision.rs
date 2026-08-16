//! How precisely a retold claim's day is remembered.
//!
//! **This module is an index and nothing else.** The rungs a world offers —
//! its day, each of its moons, its seasons, its year — are that world's own
//! astronomy, and the arithmetic of snapping a day to one of them needs
//! `StdDays`, which lives in `domains/astronomy` and is therefore below no
//! part of the kernel. So the ladder itself lives in `windows/hearsay`, which
//! may depend on a domain, and the kernel carries only the rung index a
//! [`crate::Claim`] is remembered at.
//!
//! Two drafts got this wrong before it landed. The first hard-coded
//! 365/3650/10950 — Earth's calendar wearing a Hornvale type. The second kept
//! the spans here as bare `f64` days, which `type-audit` refused for want of a
//! class, correctly: a day at a `pub` boundary wants the typed quantity, and
//! the kernel cannot have it.
//!
//! **The rungs deliberately do not nest.** Real cycles are incommensurable — a
//! synodic month does not divide a year, which is why intercalation exists —
//! so a teller re-rounds an already-rounded day and error compounds until a
//! claim can name an interval that no longer contains the event. That is a
//! rumour becoming false, not a rounding bug.
//!
//! The invariant is consequently **precision-rank monotonicity** — the rung
//! index only ever rises — and *not* any statement about error. A coarsened
//! claim is an interval that widened, not a point that moved: "sometime that
//! year" is strictly less informative than "on that day" even when its
//! representative value happens to land nearer the truth.

/// Which rung of a world's ladder a claim is remembered at, finest first.
///
/// An INDEX, not a named calendar unit, because rung count is per-world: a
/// two-mooned world offers reckonings a moonless one does not, and their
/// order depends on the actual lengths of that world's cycles rather than on
/// any choice made here.
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precision(pub u8);

impl Precision {
    /// The finest rung any ladder offers.
    pub const FINEST: Precision = Precision(0);

    /// The rung index.
    /// type-audit: bare-ok(index: return)
    pub fn rung(self) -> u8 {
        self.0
    }

    /// The next rung coarser, unbounded.
    ///
    /// Saturation against a world's coarsest rung is the ladder's job, not
    /// this type's — only the world knows where its ladder ends.
    pub fn coarser(self) -> Precision {
        Precision(self.0.saturating_add(1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precision_rank_only_ever_rises() {
        // The surviving invariant, and it is structural: no arithmetic, no
        // spans, nothing a world's cycles can falsify.
        let mut p = Precision::FINEST;
        for _ in 0..16 {
            let next = p.coarser();
            assert!(next >= p, "precision rank fell: {next:?} < {p:?}");
            p = next;
        }
    }

    #[test]
    fn coarsening_saturates_rather_than_wrapping() {
        // u8 wrap-around would silently return a claim to first-hand
        // precision, which is the one transition the model forbids.
        let p = Precision(u8::MAX).coarser();
        assert_eq!(p, Precision(u8::MAX));
    }

    #[test]
    fn the_finest_rung_is_zero_and_orders_below_every_other() {
        assert_eq!(Precision::FINEST.rung(), 0);
        assert!(Precision::FINEST < Precision(1));
    }
}
