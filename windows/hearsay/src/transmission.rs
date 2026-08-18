//! The three choices a transmission walk makes, as one value.
//!
//! Spec §5. The model is three separable layers — the graph's TOPOLOGY
//! ([`crate::contact::Contact`]), the node LABELLING the cost function reads
//! ([`crate::stance::Perpetration`]), and whether world time constrains either
//! ([`crate::clock::Clock`]). Campaigns 2 and 3 both varied edge COST and left
//! the other two untouched; this bundles all three so a readout can vary one
//! at a time.

use crate::clock::Clock;
use crate::contact::{Contact, ContactGraph};
use crate::lineage::Lineage;
use crate::stance::Perpetration;
use hornvale_kernel::ledger::Ledger;

/// One point in the space of transmission models.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
/// [`Transmission::AS_SHIPPED`] is not a default: it is a named baseline, and
/// naming it is the point.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Transmission {
    /// Whether world time constrains who may hold a claim.
    pub clock: Clock,
    /// Whether being a perpetrator is inherited by descent.
    pub perpetration: Perpetration,
    /// Whether transmission may leave the founding tree.
    pub contact: Contact,
}

impl Transmission {
    /// Exactly what shipped before this campaign: no clock, a singleton
    /// perpetrator, descent-only transmission. Every arm is measured as a
    /// difference from this, so `tests/transmission.rs` pins that it
    /// reproduces the committed derivation.
    pub const AS_SHIPPED: Transmission = Transmission {
        clock: Clock::Off,
        perpetration: Perpetration::Singleton,
        contact: Contact::Descent,
    };

    /// This policy's readout column name, `clock/perpetration/contact`.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> String {
        format!(
            "{}/{}/{}",
            self.clock.label(),
            self.perpetration.label(),
            self.contact.label()
        )
    }
}

/// The read-side context every transmission walk needs, whatever its policy.
///
/// Bundled rather than passed as three more arguments because
/// [`crate::derive::variants_about_accumulating`] already takes seven and
/// clippy's `too_many_arguments` fires at eight under `-D warnings`.
pub struct Walk<'a> {
    /// The committed ledger. A window reads this and nothing else.
    pub ledger: &'a Ledger,
    /// The founding tree, from [`crate::lineage::lineage_of`].
    pub lineage: &'a Lineage,
    /// The raid seam, from [`crate::contact::contact_of`]. Built even under
    /// [`Contact::Descent`], where it is simply never consulted — building it
    /// is a cheap ledger scan and a conditional would only add a branch.
    pub contact: &'a ContactGraph,
    /// Which transmission model to walk.
    pub policy: Transmission,
}
