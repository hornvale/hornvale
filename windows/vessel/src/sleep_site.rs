//! Which anchor a body would sleep on, within the room it already stands in
//! (The Pallet, Task 2) — the refinement `liveness::room_affords_rest`'s own
//! doc names as the thing this campaign adds: today that fold only asks
//! *whether* any anchor in a room offers [`crate::affordance::OfferedVerb::
//! Sleep`]; this asks *which one* a body would actually use.
//!
//! **This module is not wired to anything yet.** Nothing calls
//! [`select_sleep_site`] this task — it exists so Task 3 has a `KindId` to
//! commit as a new `SLEPT_ON` fact at commit time. It is deliberately never
//! consulted by the recovery fold itself: that fold re-derives from
//! committed facts, and decision 0069 forbids committing an anchor's
//! identity, so a per-anchor grade is out of reach for a fold and stays
//! that way (`liveness::room_affords_rest`'s own doc carries the same
//! warning in capitals — read it before touching either function).

use crate::affordance::{OfferedVerb, offered_to};
use crate::body::Body;
use crate::interior::{AnchorId, Interior};

/// The anchor `body` would lie down on if it slept in this room right now,
/// or `None` if nothing in the room offers it anywhere to sleep.
///
/// **Within-room only.** This never proposes movement and never looks past
/// `interior`'s own anchors into another room's — a body does not wander off
/// to find a bed (spec §4a, Nathan's ruling). Pair it with whatever already
/// decided the body is sleeping *here*; it only ever answers "given this
/// room, where in it."
///
/// **The tie-break is ascending [`AnchorId`], and that is a placeholder, not
/// a preference.** [`Interior::ids`] already returns anchors in ascending
/// order, so ranging over it and taking the first match needs no sort of its
/// own — but "first by id" carries no claim that the anchor it picks is
/// *better* than the other candidates. It cannot: "the best" site is not a
/// concept this function has access to yet, because [`Interior::anchor`]
/// carries only a `kind` and a containment edge, nothing describes softness,
/// size or crowding, and the whole point of grading THAT (a `SiteGrade`
/// keyed on kind) is Task 4's job, not this one's. Ascending id is simply the
/// one order every caller and every test can agree on without inventing a
/// quality axis today — deterministic and documented, not smart.
///
/// **The chooser must be ABLE to choose badly, and that is load-bearing, not
/// an oversight to close later** (spec §4b). A chooser that always resolved
/// to the objectively best available site could never produce the signal
/// this campaign exists to give Nathan: a body settling for bracken, or for
/// bare ground next to an unused bed, is itself the observation that
/// something in the room's layout, the body's drives, or the arbitration
/// around sleep needs tuning. **A future campaign that adds cleverness here
/// — grading candidates, preferring a softer or warmer anchor, breaking ties
/// on `SiteGrade` once Task 4 lands — must preserve a body's ability to end
/// up on a worse site than the room actually offers, not engineer that
/// possibility away.** Concretely: it is legal, and expected to keep
/// happening, for this function (or its eventual smarter successor) to
/// return an anchor that is not the room's best by whatever grade exists at
/// the time, provided the tie-break or preference rule that produced it is
/// still deterministic and documented. What must never happen is a version
/// of this chooser that is *guaranteed* to find the optimum — that guarantee
/// is exactly the thing that would erase the tuning signal.
pub(crate) fn select_sleep_site(interior: &Interior, body: &Body) -> Option<AnchorId> {
    interior
        .ids()
        .into_iter()
        .find(|&a| offered_to(interior.anchor(a).kind, body).contains(&OfferedVerb::Sleep))
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_thing::kinds;

    /// A `Body` fixture varying nothing — [`select_sleep_site`] only ever
    /// reads `body` through [`crate::affordance::offered_to`], for the
    /// body-relative mass gate on `SupportsRest`. Same shape as
    /// `affordance::tests::body_with_mass` (kept separate rather than
    /// shared, per that helper's own doc: each exercises a different
    /// module's private surface) — a plain struct literal built from public
    /// constructors, no world or `Ledger`, no suite-only machinery.
    fn test_body() -> Body {
        let home = hornvale_kernel::Facet::containing([0.0, 0.0, 0.0], 6);
        Body {
            entity: hornvale_kernel::EntityId::new(1).expect("1 is a valid entity id"),
            home: home.clone(),
            resource: home,
            species: "test-species".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: hornvale_kernel::ConditionResponse {
                optimum: 15.0,
                width: 10.0,
                devotion: 0.5,
            },
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            thermal_strategy: hornvale_species::ThermalStrategy::Endothermic,
            niche: hornvale_kernel::ResourceVector::new(&[]).expect("the empty niche is valid"),
            boldness: 0.5,
            threat_niche: crate::liveness::ThreatNiche {
                uncanny: 1.0,
                heat: 0.0,
                cold: 0.0,
                predator: 0.5,
            },
            mass_kg: 70.0,
            label: "test-body".into(),
            perception: hornvale_species::PerceptionVector::MANIKIN,
            village: None,
        }
    }

    /// Case 1: a room with exactly one `Sleep`-offering anchor returns it.
    #[test]
    fn single_bed_anchor_is_selected() {
        let mut interior = Interior::new();
        let bed = interior.push(kinds::BED, None);
        assert_eq!(select_sleep_site(&interior, &test_body()), Some(bed));
    }

    /// Case 2: no anchor in the room offers `Sleep` — `None`, not a panic
    /// and not a fallback to "the first anchor regardless."
    #[test]
    fn room_with_no_offering_anchor_returns_none() {
        let mut interior = Interior::new();
        interior.push(kinds::HEARTH, None);
        interior.push(kinds::THRESHOLD, None);
        assert_eq!(select_sleep_site(&interior, &test_body()), None);
    }

    /// Case 3, the one that matters: two anchors both offer `Sleep`. The
    /// function must return a DETERMINISTIC one, and this asserts which —
    /// the ascending `AnchorId`, i.e. the one pushed first — not merely that
    /// something comes back. "The best" of the two is undefined until Task 4
    /// gives sites a grade; until then, ascending id is the one order
    /// [`Interior::ids`] already guarantees, so it is the tie-break rather
    /// than an invented preference.
    #[test]
    fn two_offering_anchors_the_ascending_one_wins() {
        let mut interior = Interior::new();
        let first_bed = interior.push(kinds::BED, None);
        let second_bed = interior.push(kinds::BED, None);
        assert!(
            first_bed < second_bed,
            "the fixture must plant the two anchors in ascending push order \
             for this test to discriminate the tie-break at all"
        );
        assert_eq!(
            select_sleep_site(&interior, &test_body()),
            Some(first_bed),
            "the ascending AnchorId wins the tie, matching Interior::ids()'s \
             own order — not the second bed, and not an arbitrary one"
        );
    }
}
