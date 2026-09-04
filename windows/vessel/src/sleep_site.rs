//! Which anchor a body would sleep on, within the room it already stands in
//! (The Pallet, Task 2) — the refinement `liveness::room_affords_rest`'s own
//! doc names as the thing this campaign adds: today that fold only asks
//! *whether* any anchor in a room offers [`crate::affordance::OfferedVerb::
//! Sleep`]; this asks *which one* a body would actually use.
//!
//! **This module is wired to three callers now, and none of them is the
//! recovery fold** (fix round 1, F3 — this paragraph was stale, written
//! before Task 3 landed and never updated). `liveness::room_affords_rest`
//! delegates to it for the room-level boolean (Task 2); `liveness::
//! advance_one`'s own `Action::Sleep` arm and `Session::sleep` both call it
//! at COMMIT time, to learn the `KindId` a new `SLEPT_ON` fact records
//! (Task 3). What is still true, and is the load-bearing half of this
//! paragraph: it is deliberately never consulted by the recovery FOLD
//! itself. That fold re-derives from committed facts, and decision 0069
//! forbids committing an anchor's identity, so a per-anchor grade is out of
//! reach for a fold and stays that way (`liveness::room_affords_rest`'s own
//! doc carries the same warning in capitals — read it before touching
//! either function).
//!
//! **Two questions over ONE candidate scan** (The Tenon, Task 6). The
//! room-level boolean and the choice of site used to be the same call —
//! `select_sleep_site(..).is_some()` — and could not stay so once choosing
//! needed the sleeper's species-resolved response to a surface, which the
//! fold that asks the boolean does not hold and has no reason to. So
//! [`room_offers_sleep`] and [`select_sleep_site`] are now separate entry
//! points over one private [`sleep_candidates`] iterator: the single
//! definition of "which anchors here offer this body somewhere to sleep" is
//! preserved, and the boolean half costs strictly less than it did, because
//! it no longer resolves a grade it would throw away.

use crate::affordance::{ObjectTraits, OfferedVerb, offered_to_traits};
use crate::body::Body;
use crate::interior::{AnchorId, Interior};
use crate::liveness::{SleepTraits, grade_of};
use hornvale_kernel::KindId;
use hornvale_kernel::component::ComponentStore;

/// Every anchor in `interior` that offers `body` somewhere to sleep, in
/// ascending [`AnchorId`] — the one scan both public questions in this module
/// are asked over.
///
/// **`objects` is the caller's roster, and reading candidacy from it rather
/// than from the global [`crate::affordance::object_registry`] is a cost
/// choice with no behavioural half** (The Tenon, Task 6).
/// [`crate::affordance::offered_to`] rebuilds the registry on every call, so
/// the previous shape built one `ComponentStore` PER ANCHOR, inside the
/// fatigue fold. Every production caller already holds a roster built once —
/// `liveness::object_roster` for the fold, one build per act for the two
/// commit-time sites — and passes it here. Both hold
/// `crate::affordance::object_registry()`, so the answer is unchanged.
fn sleep_candidates<'a>(
    interior: &'a Interior,
    body: &'a Body,
    objects: &'a ComponentStore<KindId, ObjectTraits>,
) -> impl Iterator<Item = AnchorId> + 'a {
    // `Interior::ids` is ascending by its own doc, and nothing below reorders
    // it — which is what makes "the first candidate" and "the lowest-id
    // candidate" the same anchor for [`select_sleep_site`]'s tie-break.
    interior.ids().into_iter().filter(move |&a| {
        let traits = objects
            .get(&interior.anchor(a).kind)
            .cloned()
            .unwrap_or_default();
        offered_to_traits(&traits, body).contains(&OfferedVerb::Sleep)
    })
}

/// Does anything in this room offer `body` somewhere to sleep at all — the
/// room-level boolean `liveness::room_affords_rest` needs and nothing more.
///
/// Split out of [`select_sleep_site`] in The Tenon, Task 6: the fold that asks
/// this holds no [`SleepTraits`], because a yes/no answer does not depend on
/// one — every candidate answers `true` equally, whatever it is worth to lie
/// on. Threading the sleeper's traits down into `rest_timeline` to satisfy a
/// ranking whose result is then discarded by `.is_some()` would have been
/// cost and coupling bought for nothing.
pub(crate) fn room_offers_sleep(
    interior: &Interior,
    body: &Body,
    objects: &ComponentStore<KindId, ObjectTraits>,
) -> bool {
    sleep_candidates(interior, body, objects).next().is_some()
}

/// The anchor `body` would lie down on if it slept in this room right now,
/// or `None` if nothing in the room offers it anywhere to sleep.
///
/// **Within-room only.** This never proposes movement and never looks past
/// `interior`'s own anchors into another room's — a body does not wander off
/// to find a bed (spec §4a, Nathan's ruling). Pair it with whatever already
/// decided the body is sleeping *here*; it only ever answers "given this
/// room, where in it."
///
/// **It now prefers the better site, ranking by
/// [`crate::liveness::grade_of`] descending and breaking ties on ascending
/// [`AnchorId`]** (The Tenon, Task 6, spec §8). The tie-break is what The
/// Pallet shipped as the whole rule, and its doc called that "a placeholder,
/// not a preference" because "the best" site was not a concept this function
/// had access to: [`Interior::anchor`] carries a `kind` and a containment
/// edge, and nothing described what lying on that kind was worth. The Tenon's
/// `grade(species, kind)` is exactly that missing concept, so the placeholder
/// is redeemed rather than merely replaced — ascending id is now the answer
/// to *equal* grades, which is the question it was always able to answer
/// honestly.
///
/// **THE PALLET'S WARNING, AND THIS CAMPAIGN'S ANSWER TO IT.** The paragraph
/// this one replaces said a future chooser "must preserve a body's ability to
/// end up on a worse site than the room actually offers," and that "what must
/// never happen is a version of this chooser that is *guaranteed* to find the
/// optimum — that guarantee is exactly the thing that would erase the tuning
/// signal." Within-room argmax **is** guaranteed to find the room's optimum,
/// so the letter of that warning is not satisfied by the code above, and
/// pretending otherwise would leave a successor trusting a sentence the
/// function no longer honours.
///
/// Its PURPOSE is satisfied, and by the rule stated directly above it in this
/// same doc: **the chooser never proposes movement and never looks past this
/// room's anchors.** The optimum it finds is the room's, never the world's,
/// and a room is a very small place. A body sleeps on bracken while a bed
/// stands in the next room; a body sleeps on bare ground in a room that holds
/// nothing at all. That is precisely the observation the warning existed to
/// protect — *a creature sleeping in an unrestful place is a useful indicator
/// that something needs tuning* — and it lives in the no-travel rule, not in
/// the tie-break. Erasing it would take a chooser that SEARCHES, and adding
/// one is the decision this doc is telling a successor they would be making.
///
/// This is spec §8's ruling, recorded here rather than resolved silently:
/// the warning was answered, not stepped over.
///
/// **`objects` and `sleeper` are the caller's, built once and lent.** See
/// [`sleep_candidates`] for why candidacy reads the passed roster, and
/// `liveness::sleep_traits_of` for the species resolution the two commit-time
/// callers share with the fatigue fold.
pub(crate) fn select_sleep_site(
    interior: &Interior,
    body: &Body,
    sleeper: &SleepTraits,
    objects: &ComponentStore<KindId, ObjectTraits>,
) -> Option<AnchorId> {
    // DETERMINISTIC WITHOUT A SORT, and deliberately not a `sort_by` or a
    // `max_by` on a bare `f64`. Two properties do it between them:
    //
    //   1. `sleep_candidates` yields in ascending `AnchorId` (its own doc), so
    //      the incumbent is always the lower id;
    //   2. the comparison is `total_cmp` — a TOTAL order over every `f64`
    //      including NaN, so no pair of grades is ever "unordered" — and it
    //      admits a challenger only on STRICTLY `Greater`, which leaves an
    //      equal grade with the incumbent.
    //
    // (2) plus (1) is exactly "grade descending, ties on ascending id", with
    // no float ever reaching a sort comparator. `total_cmp` also has no
    // partial-order escape hatch for a caller to get wrong, which is why the
    // workspace bans the naked-float alternative outright.
    let mut best: Option<(AnchorId, f64)> = None;
    for anchor in sleep_candidates(interior, body, objects) {
        let grade = grade_of(sleeper, interior.anchor(anchor).kind, objects);
        let wins = match best {
            None => true,
            Some((_, incumbent)) => grade.total_cmp(&incumbent) == std::cmp::Ordering::Greater,
        };
        if wins {
            best = Some((anchor, grade));
        }
    }
    best.map(|(anchor, _)| anchor)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::affordance::{ObjectProperty, RestSurface, Substrate};
    use hornvale_kernel::ConditionResponse;
    use hornvale_thing::kinds;

    /// A `Body` fixture varying nothing — [`select_sleep_site`] only ever
    /// reads `body` through [`crate::affordance::offered_to_traits`], for the
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

    /// The sleeper these cases rank for: a body that gains from a surface at
    /// all (`afforded_gain > 1.0`, or every grade collapses to `1.0` and no
    /// case here could discriminate) and prefers a YIELDING substrate.
    fn test_sleeper() -> SleepTraits {
        SleepTraits {
            rise: 0.3,
            afforded_gain: 2.0,
            substrate: ConditionResponse {
                optimum: 0.0,
                width: 0.5,
                devotion: 1.0,
            },
        }
    }

    /// The roster the preference case ranks against, built here rather than
    /// read from [`crate::affordance::object_registry`] **because the
    /// production roster cannot state the case at all**: `bed` is the only
    /// kind carrying `SupportsRest` today, so no pair of PRODUCTION kinds
    /// both offers `Sleep`, and a preference between two different kinds is
    /// unwriteable against it until Task 7's epoch lands three more surfaces.
    /// Constructing the roster is also what `affordance::offered`'s own doc
    /// prescribes for exactly this situation — "so a test can construct a
    /// property set independent of whatever the registry happens to hold
    /// today."
    ///
    /// The two keys are INVENTED rather than borrowed from
    /// `hornvale_thing::kinds`, so nothing here can read as a claim about
    /// what a production hearth or threshold offers. `bracken` is soft
    /// (`Natural(0.1)`, near this sleeper's optimum); `slab` is rock
    /// (`Natural(1.0)`). They carry the SAME `offer`, so the two differ in
    /// FIT alone and the case is about the sleeper's substrate curve rather
    /// than about the surfaces' generosity.
    fn test_roster() -> ComponentStore<KindId, ObjectTraits> {
        let surface = |offer: f64, substrate: Substrate| ObjectTraits {
            properties: [ObjectProperty::SupportsRest].into_iter().collect(),
            rest: Some(RestSurface { offer, substrate }),
        };
        [
            (BRACKEN, surface(0.5, Substrate::Natural(0.1))),
            (SLAB, surface(0.5, Substrate::Natural(1.0))),
        ]
        .into_iter()
        .collect()
    }

    /// A soft found surface, invented for [`test_roster`] — see its doc for
    /// why these two keys are not production kinds.
    const BRACKEN: KindId = KindId("bracken");

    /// A rock found surface, invented for [`test_roster`].
    const SLAB: KindId = KindId("slab");

    /// The shipped roster, for the three cases that assert about PRODUCTION
    /// kinds and must keep doing so.
    fn shipped_roster() -> ComponentStore<KindId, ObjectTraits> {
        crate::affordance::object_registry()
    }

    /// Case 1: a room with exactly one `Sleep`-offering anchor returns it.
    #[test]
    fn single_bed_anchor_is_selected() {
        let mut interior = Interior::new();
        let bed = interior.push(kinds::BED, None);
        assert_eq!(
            select_sleep_site(&interior, &test_body(), &test_sleeper(), &shipped_roster()),
            Some(bed)
        );
    }

    /// Case 2: no anchor in the room offers `Sleep` — `None`, not a panic
    /// and not a fallback to "the first anchor regardless."
    #[test]
    fn room_with_no_offering_anchor_returns_none() {
        let mut interior = Interior::new();
        interior.push(kinds::HEARTH, None);
        interior.push(kinds::THRESHOLD, None);
        assert_eq!(
            select_sleep_site(&interior, &test_body(), &test_sleeper(), &shipped_roster()),
            None
        );
    }

    /// Case 3, the TIE: two anchors of the SAME kind both offer `Sleep`, so
    /// their grades are equal and the choice falls through to the tie-break.
    /// The ascending `AnchorId` — the one pushed first — must win, matching
    /// [`Interior::ids`]'s own order.
    ///
    /// **Kept, not replaced, when Case 4 below was added** (The Tenon, Task
    /// 6). The tie-break and the preference are separate claims over separate
    /// branches of the same function: an edit that made the ranking pick the
    /// LAST equal maximum — `max_by`'s own documented behaviour, and the
    /// easiest way to write this wrong — would leave Case 4 green and this one
    /// red. Its doc used to say "'the best' of the two is undefined until Task
    /// 4 gives sites a grade"; the grade exists now, and equal grades are the
    /// case this test holds.
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
            select_sleep_site(&interior, &test_body(), &test_sleeper(), &shipped_roster()),
            Some(first_bed),
            "the ascending AnchorId wins the tie, matching Interior::ids()'s \
             own order — not the second bed, and not an arbitrary one"
        );
    }

    /// Case 4, the PREFERENCE (The Tenon, Task 6): two anchors of DIFFERENT
    /// kinds, both offering `Sleep`, with different grades. The higher-graded
    /// one wins even though it is the higher `AnchorId` — which is what makes
    /// this discriminating: planted second on purpose, so the ascending-id
    /// rule Case 3 pins would give the other answer.
    ///
    /// The two candidates differ only in substrate (`Natural(0.1)` vs
    /// `Natural(1.0)`, same `offer`), so what is being asserted is that the
    /// sleeper's own `substrate` curve reached the choice — the whole point of
    /// spec §5's relation.
    #[test]
    fn the_higher_graded_kind_wins_over_the_lower_anchor_id() {
        let objects = test_roster();
        let body = test_body();
        let sleeper = test_sleeper();
        let mut interior = Interior::new();
        // The ROCK slab first, so it holds the lower id and would win on the
        // tie-break alone.
        let slab = interior.push(SLAB, None);
        let bracken = interior.push(BRACKEN, None);
        assert!(
            slab < bracken,
            "the fixture must plant the WORSE site at the lower AnchorId, or \
             this test cannot tell a preference from the tie-break"
        );
        assert!(
            grade_of(&sleeper, BRACKEN, &objects) > grade_of(&sleeper, SLAB, &objects),
            "the fixture roster must actually grade the soft surface higher \
             for this sleeper, or the assertion below is vacuous"
        );
        assert_eq!(
            select_sleep_site(&interior, &body, &sleeper, &objects),
            Some(bracken),
            "the better-graded anchor must win, not the lower AnchorId"
        );
    }
}
