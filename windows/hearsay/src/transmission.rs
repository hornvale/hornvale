//! The three choices a transmission walk makes, as one value.
//!
//! Spec §5. The model is three separable layers — the graph's TOPOLOGY
//! ([`crate::contact::Contact`]), the node LABELLING the cost function reads
//! ([`crate::stance::Perpetration`]), and whether world time constrains either
//! ([`crate::clock::Clock`]). Campaigns 2 and 3 both varied edge COST and left
//! the other two untouched; this bundles all three so a readout can vary one
//! at a time.
//!
//! Campaign 5 adds a fourth ([`Crossing`]), which is the first arm to price an
//! edge by WHO is standing at either end of it rather than by the graph's
//! shape.

use crate::clock::Clock;
use crate::contact::{Contact, ContactGraph};
use crate::lineage::Lineage;
use crate::stance::Perpetration;
use hornvale_kernel::ledger::Ledger;

/// What a step between two peoples costs.
///
/// Spec §5.1. Under [`Crossing::ContactWeighted`] a step whose teller and
/// hearer belong to different peoples widens the accumulated damage by
/// `span(FINEST) / (1 + edges_between(a, b))` on top of its ordinary
/// generational span — one finest rung, discounted by how many raids the two
/// peoples have on record between them. A step within one people pays nothing,
/// which is what makes ingroup preference an OUTPUT of the model rather than
/// an input to it.
///
/// **The magnitude is derived, and that is the whole point** (spec §5.4). A
/// constant would be an authored preference between two peoples, which
/// decision 0021 forbids; `edges_between` is read from the same committed
/// endings the seam itself is built from, so the strength of the preference is
/// a property of the world's history. Nothing here ranks either people, and
/// `tests/crossing.rs` pins that a constant denominator cannot satisfy the
/// assertion this arm exists to make.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Crossing {
    /// Ships today: a people boundary costs a retelling nothing extra.
    Free,
    /// A crossing costs one finest rung, divided by one plus the number of
    /// raid edges between the two peoples.
    ContactWeighted,
}

impl Crossing {
    /// Every arm, in a fixed order so a readout's columns are stable.
    pub const ALL: [Crossing; 2] = [Crossing::Free, Crossing::ContactWeighted];

    /// This arm's short name, used as a readout column suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Crossing::Free => "free",
            Crossing::ContactWeighted => "contact-weighted",
        }
    }
}

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
    /// What a step between two peoples costs.
    pub crossing: Crossing,
}

impl Transmission {
    /// Exactly what shipped before this campaign: no clock, a singleton
    /// perpetrator, descent-only transmission, a free people boundary. Every
    /// arm is measured as a difference from this, so `tests/transmission.rs`
    /// pins that it reproduces the committed derivation.
    pub const AS_SHIPPED: Transmission = Transmission {
        clock: Clock::Off,
        perpetration: Perpetration::Singleton,
        contact: Contact::Descent,
        crossing: Crossing::Free,
    };

    /// This policy's readout column name,
    /// `clock/perpetration/contact/crossing`.
    ///
    /// Every arm appears here, and a campaign that adds one must extend this
    /// in the same commit: two policies that differ printing one name would
    /// make a readout's arm columns ambiguous, which is the wrong-attribution
    /// shape this crate keeps guarding against. The pinned string in
    /// `tests/transmission.rs` therefore MOVES when an arm is added — it is a
    /// description of which arms exist, not a measured outcome.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> String {
        format!(
            "{}/{}/{}/{}",
            self.clock.label(),
            self.perpetration.label(),
            self.contact.label(),
            self.crossing.label()
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
