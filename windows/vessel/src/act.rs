//! Acts — addressable without being stored (decision 0580, spec §4.5).
//!
//! **Nathan's ruling, which is the whole design:** an act is reified —
//! every act — and NOT committed. Addressability does not imply storage:
//! decision 0366 makes passage state "a pure function of the seed and the
//! committed ledger, and only the CHANGE is written"; 0346 makes an
//! affordance derived and never committed; 0368 makes live-play facts
//! persist only when a snapshot (`--out`) asks. An act follows the same
//! shape — [`ActHandle`] is a pure function of the act's own constituents,
//! costing nothing to mint, and the derived reads over it
//! ([`witnessed`], [`present_at`], [`deed_of`], [`act_precedes`],
//! [`act_occurred_on`]) never touch a [`hornvale_kernel::Ledger`]. If a
//! caller ever wants one of these captured durably, that is an ordinary
//! `Fact` commit through the SAME path every other live-play fact already
//! takes (`agent-at`, `drank`, …) — this module does not build that path,
//! it only makes the identity and the reads cost nothing so a future
//! caller is never forced to pay a commit just to *reference* an act.
//!
//! **Two precedents, both read before this module was designed.**
//! [`hornvale_history::flesh::RoleHandle`] is identity without
//! materialization — "a record can reference many unnamed roles without
//! ever materializing them until something actually observes one." An
//! [`ActHandle`] is the same idea one layer up: an act's identity, not its
//! flesh. `windows/worldgen::character::barrier_of` is state without
//! storage — a value re-derived on every read rather than written once and
//! kept in sync; [`witnessed`]/[`present_at`]/[`deed_of`]/[`act_precedes`]/
//! [`act_occurred_on`] are that same shape: total, pure functions over an
//! [`Act`] value, recomputed by whoever asks rather than cached anywhere.
//!
//! **The hazard `hornvale_history::descent::ancestor`'s doc records, and
//! this module does not repeat.** That function's own note: "a fixed
//! permutation iterated has fixed points, and `(RoleHandle(0), Seed(0))`
//! was one — every step collapsed onto the same handle, and `Seed(0)` is a
//! reachable world seed." [`Act::handle`] never iterates one mix step
//! over itself — it folds four *different* constituents (actor, deed,
//! patient, day) through four separate steps, so there is no fixed
//! permutation to have a fixed point in the first place. The degenerate
//! all-zero-shaped case this module's tests probe directly is the closest
//! analogue reachable here: the smallest legal [`EntityId`] (`1`, since `0`
//! is reserved), an empty deed string, no patient, and genesis
//! (`WorldTime::GENESIS`, ticks `0`) — see
//! `act_degenerate_all_zero_shaped_case_still_distinguishes_a_neighbour` in
//! this module's tests.
//!
//! **What "the derived act view" reads, and what it cannot yet read.**
//! [`Session`](crate::Session) exposes exactly three things publicly that
//! an act view needs: [`Session::day`](crate::Session::day) (the
//! constituent [`act_occurred_on`] returns),
//! [`Session::agent_entity`](crate::Session::agent_entity) (the constituent
//! [`deed_of`] returns for the possessed body's own acts), and
//! [`Session::purview`](crate::Session::purview) (the walk-band chart,
//! whose squares — `hornvale_scene::SurroundsCell`, the chart's own AREA // lexicon: SurroundsCell is a chart AREA unit, never a mesh vertex
//! unit, never a mesh vertex — carry `hornvale_scene::Mark`s of kind
//! `"agent"` for anyone else standing there). [`anyone_present`] reads
//! that last one: it takes one square's marks, not a whole
//! `EntityId`-keyed roster, because a mark carries only a noun and a
//! kind, never an entity id — the chart cannot yet say *who* is present,
//! only *that* someone is. That is a known limit, named here rather than
//! worked around: [`present_at`] and [`witnessed`] take an explicit
//! `&[EntityId]` presence pool instead, which is what a caller with
//! entity-level knowledge (not this module) must supply. Widening the
//! chart to carry entity ids is future work this module does not attempt.

use hornvale_kernel::{EntityId, WorldTime};

/// A derived, addressable identity for an act — a pure function of the
/// act's own constituents (decision 0580), never minted, never committed,
/// and never cached: two callers who agree on an act's constituents agree
/// on its handle without coordinating, in the same process or across two.
///
/// type-audit: bare-ok(identifier-text: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ActHandle(pub u64);

/// A splitmix-style mix step, the same arithmetic
/// `hornvale_history::flesh::persona_of` and `hornvale_history::record::mix`
/// use — kept as a private copy rather than depending on either (both are
/// `pub(crate)` to `hornvale-history`, and a domain crate's internal mix
/// constants are not a cross-crate contract this module should reach
/// into). Pure bit arithmetic: no transcendental, no `libm`, no platform
/// dependence, so it is bit-identical everywhere this crate builds.
fn mix(state: u64, x: u64) -> u64 {
    let mut z = state ^ x;
    z = z.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    z ^= z >> 29;
    z = z.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z ^ (z >> 32)
}

/// Fold a string's bytes into a mix state, so two different deed labels of
/// equal prefix (`"go"` vs. `"gone"`) do not fold to the same intermediate
/// state before the length distinguishes them — the length is mixed in
/// last for exactly that reason.
fn mix_str(state: u64, s: &str) -> u64 {
    let mut h = state;
    for b in s.as_bytes() {
        h = mix(h, u64::from(*b));
    }
    mix(h, s.len() as u64)
}

/// Folded in ahead of a present patient's id. Arbitrary, but load-bearing:
/// [`mix`]'s first step is a bare XOR (`state ^ x`), so `mix(a, a) == 0` for
/// *any* `a` — an early draft of [`Act::handle`] mixed the presence tag `1`
/// directly against the patient's raw id and collided the instant a real
/// `EntityId` happened to equal that tag (`EntityId::new(1)`, the smallest
/// legal id — caught by this module's own test sweep, not by inspection).
/// The fix folds each tag against the ALREADY-AVALANCHED accumulator `h`
/// (never against a small raw id directly), so a collision would need `h`
/// itself — the output of at least one prior [`mix`] call — to land on one
/// of these two specific constants, rather than needing only a small
/// `EntityId` to equal a small literal.
const PATIENT_SOME_TAG: u64 = 0xA5A5_A5A5_A5A5_A5A5;
/// Folded in when an act has no patient. Distinct from
/// [`PATIENT_SOME_TAG`], and folded as the *only* step for the `None` arm
/// (the `Some` arm folds this tag's sibling and then the id — two steps),
/// so the two arms differ in step count as well as in tag value.
const PATIENT_NONE_TAG: u64 = 0x5A5A_5A5A_5A5A_5A5A;

/// One act: `actor` did `deed`, to `patient` if the deed has one, on `day`.
/// A plain value, never a query — constructing one costs nothing, the same
/// way a [`hornvale_kernel::field::WorldTime`] costs nothing to construct.
/// Nothing about this type touches a ledger; see the module doc.
///
/// type-audit: bare-ok(identifier-text: deed)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Act {
    /// Who performed the act.
    pub actor: EntityId,
    /// A closed, session-owned vocabulary word naming what was done
    /// (`"provoke"`, `"soothe"`, `"go"`, …) — not free prose. Kept as a
    /// `&'static str` rather than an enum because the vocabulary is the
    /// verb dispatch layer's (`crate::action::Action`), not this module's,
    /// to close; this module only needs the label to be stable and
    /// hashable, which a `&'static str` already is.
    pub deed: &'static str,
    /// Who the deed was done to, if it has a target.
    pub patient: Option<EntityId>,
    /// The day the act occurred — the same [`WorldTime`] every fact a
    /// session commits is stamped with.
    pub day: WorldTime,
}

impl Act {
    /// Derive this act's [`ActHandle`]: fold `actor`, `deed`, `patient` and
    /// `day` through separate [`mix`] steps, in that order. `patient` folds
    /// a presence tag ([`PATIENT_SOME_TAG`]/[`PATIENT_NONE_TAG`]) against
    /// the already-avalanched accumulator ahead of a present id — see
    /// those constants' own doc for the collision this avoids (and the
    /// test that caught it before this comment existed).
    pub fn handle(&self) -> ActHandle {
        let mut h = mix(0, self.actor.get());
        h = mix_str(h, self.deed);
        h = match self.patient {
            Some(e) => {
                let h = mix(h, PATIENT_SOME_TAG);
                mix(h, e.get())
            }
            None => mix(h, PATIENT_NONE_TAG),
        };
        // `WorldTime`'s ticks are a signed `i64`; the bit-cast to `u64` is
        // a bijection (two's complement), so distinct ticks — negative
        // ticks included, decision 0126 — never collide here.
        h = mix(h, self.day.ticks() as u64);
        ActHandle(h)
    }
}

/// Whether `witness` was present to see `act` — co-location, read off the
/// `present` pool a caller supplies. This function never reads `act.day`:
/// the temporal binding (that `present` names who was actually around on
/// the act's own day) is the caller's responsibility, established by how
/// the pool was built, not checked here. The actor witnessing their own act
/// does not count: an act's own performer is not what `predicate:witnessed`
/// means in the corpus this bundle serves (Polti's and tvtropes'
/// `witnessing` bundle is about a *third party* observing).
///
/// type-audit: bare-ok(flag: return)
pub fn witnessed(act: &Act, witness: EntityId, present: &[EntityId]) -> bool {
    witness != act.actor && present_at(witness, present)
}

/// Whether `who` is present in `present` — co-location, read as plain pool
/// membership. The cheaper half of `bundle:witnessing` (spec §4.5): this is
/// what `predicate:present-at` resolves to, and [`witnessed`] is built on
/// top of it rather than duplicating the check.
///
/// type-audit: bare-ok(flag: return)
pub fn present_at(who: EntityId, present: &[EntityId]) -> bool {
    present.contains(&who)
}

/// Whether anyone besides the observer shares the observer's own chart // lexicon: "chart square", the AREA sense, never a mesh vertex
/// square right now, read off that square's marks alone (`Mark::kind ==
/// "agent"`) — the signal [`Session::purview`](crate::Session::purview) can actually
/// supply today. See the module doc's "what the derived act view cannot
/// yet read" note: a mark carries no entity id, so this answers *whether*
/// someone is there, never *who*. A caller that needs *who* must build its
/// own `&[EntityId]` pool and use [`present_at`]/[`witnessed`] instead.
///
/// type-audit: bare-ok(flag: return)
pub fn anyone_present(marks: &[hornvale_scene::Mark]) -> bool {
    marks.iter().any(|m| m.kind == "agent")
}

/// The act's own performer — `predicate:deed-of`'s derived read. Total: an
/// [`Act`] always has an actor.
pub fn deed_of(act: &Act) -> EntityId {
    act.actor
}

/// Whether `a` occurred strictly before `b` — `predicate:act-precedes`'s
/// derived read, a plain [`WorldTime`] comparison. Two acts on the same
/// day never precede one another either way; `WorldTime`'s exact-tick
/// representation (decision 0186) means this needs no epsilon.
///
/// type-audit: bare-ok(flag: return)
pub fn act_precedes(a: &Act, b: &Act) -> bool {
    a.day < b.day
}

/// The day an act occurred — `predicate:act-occurred-on`'s derived read.
pub fn act_occurred_on(act: &Act) -> WorldTime {
    act.day
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eid(raw: u64) -> EntityId {
        EntityId::new(raw).unwrap_or_else(|| panic!("{raw} should be a valid EntityId"))
    }

    fn an_act() -> Act {
        Act {
            actor: eid(1),
            deed: "provoke",
            patient: Some(eid(2)),
            day: WorldTime::from_ticks(1_000),
        }
    }

    // ---- 1. a pure function of its constituents -----------------------

    #[test]
    fn same_constituents_yield_the_same_handle() {
        let a = an_act();
        let b = an_act();
        assert_eq!(
            a.handle(),
            b.handle(),
            "two `Act` values built from the same constituents must hash \
             the same, in-process (the cross-process claim follows from \
             this being pure bit arithmetic with no I/O, no random seed, \
             and no iteration-order dependence)"
        );
        // Repeat-call stability: handle() takes &self, so this is also a
        // check that it has no hidden mutable/interior state.
        assert_eq!(a.handle(), a.handle());
    }

    // ---- 2. distinct acts get distinct handles -------------------------

    #[test]
    fn varying_actor_alone_changes_the_handle() {
        let mut other = an_act();
        other.actor = eid(3);
        assert_ne!(an_act().handle(), other.handle());
    }

    #[test]
    fn varying_deed_alone_changes_the_handle() {
        let mut other = an_act();
        other.deed = "soothe";
        assert_ne!(an_act().handle(), other.handle());
    }

    #[test]
    fn varying_patient_alone_changes_the_handle() {
        let mut other = an_act();
        other.patient = Some(eid(5));
        assert_ne!(an_act().handle(), other.handle());
    }

    #[test]
    fn no_patient_never_collides_with_a_real_one() {
        let mut none_patient = an_act();
        none_patient.patient = None;
        // Some(_) whose id folds through the same `mix(1, id)` shape as
        // `None`'s bare `0` would only collide if `mix(1, id) == 0` for
        // some reachable id — checked empirically below across a wide
        // range rather than proven, matching `ancestor()`'s own standard.
        for raw in 1..=64u64 {
            let mut some_patient = an_act();
            some_patient.patient = Some(eid(raw));
            assert_ne!(
                none_patient.handle(),
                some_patient.handle(),
                "patient: None collided with patient: Some({raw})"
            );
        }
    }

    #[test]
    fn varying_day_alone_changes_the_handle_including_across_zero() {
        let mut at_genesis = an_act();
        at_genesis.day = WorldTime::GENESIS;
        let mut one_tick_later = an_act();
        one_tick_later.day = WorldTime::from_ticks(1);
        let mut one_tick_earlier = an_act();
        one_tick_earlier.day = WorldTime::from_ticks(-1);
        assert_ne!(at_genesis.handle(), one_tick_later.handle());
        assert_ne!(at_genesis.handle(), one_tick_earlier.handle());
        assert_ne!(
            one_tick_later.handle(),
            one_tick_earlier.handle(),
            "a day one tick after genesis must not collide with one tick before it"
        );
    }

    /// **The degenerate all-zero-shaped case that bit `ancestor()`.** The
    /// smallest legal `EntityId` (`1`), an empty deed string, no patient,
    /// and genesis — every field at its most-degenerate reachable value —
    /// must still distinguish itself from a neighbour that varies exactly
    /// one field. `ancestor()`'s own hazard was `(RoleHandle(0), Seed(0))`
    /// collapsing under a fixed permutation iterated on itself; this
    /// module folds four distinct constituents once each, never itself, so
    /// the failure mode cannot recur structurally — this test is the
    /// empirical check anyway, the same discipline `ancestor()`'s own tests
    /// apply.
    #[test]
    fn act_degenerate_all_zero_shaped_case_still_distinguishes_a_neighbour() {
        let degenerate = Act {
            actor: eid(1),
            deed: "",
            patient: None,
            day: WorldTime::GENESIS,
        };
        let mut neighbour_day = degenerate;
        neighbour_day.day = WorldTime::from_ticks(1);
        let mut neighbour_actor = degenerate;
        neighbour_actor.actor = eid(2);
        let mut neighbour_deed = degenerate;
        neighbour_deed.deed = "x";
        let mut neighbour_patient = degenerate;
        neighbour_patient.patient = Some(eid(1));

        let handles = [
            degenerate.handle(),
            neighbour_day.handle(),
            neighbour_actor.handle(),
            neighbour_deed.handle(),
            neighbour_patient.handle(),
        ];
        let distinct: std::collections::BTreeSet<_> = handles.iter().copied().collect();
        assert_eq!(
            distinct.len(),
            handles.len(),
            "the degenerate act and its four single-field neighbours must \
             all hash distinctly, got {handles:?}"
        );
    }

    /// A wider empirical distinctness sweep, the same spirit as
    /// `ancestor()`'s own tests probing "the deepest measured chain": every
    /// combination across a small actor/deed/patient/day grid hashes to a
    /// distinct handle — no accidental collision anywhere in this space.
    #[test]
    fn a_combinatorial_sweep_has_no_collisions() {
        let deeds = ["", "go", "provoke", "soothe", "avow"];
        let mut handles = std::collections::BTreeSet::new();
        let mut count = 0usize;
        for actor_raw in 1..=6u64 {
            for &deed in &deeds {
                for patient_raw in [None, Some(1u64), Some(4u64), Some(9u64)] {
                    for day_ticks in [-1_000_i64, -1, 0, 1, 1_000] {
                        let act = Act {
                            actor: eid(actor_raw),
                            deed,
                            patient: patient_raw.map(eid),
                            day: WorldTime::from_ticks(day_ticks),
                        };
                        handles.insert(act.handle());
                        count += 1;
                    }
                }
            }
        }
        assert_eq!(
            handles.len(),
            count,
            "expected {count} distinct handles across the swept grid, got {}",
            handles.len()
        );
    }

    // ---- the derived reads ---------------------------------------------

    #[test]
    fn deed_of_reads_the_actor() {
        assert_eq!(deed_of(&an_act()), eid(1));
    }

    #[test]
    fn act_occurred_on_reads_the_day() {
        assert_eq!(act_occurred_on(&an_act()), WorldTime::from_ticks(1_000));
    }

    #[test]
    fn act_precedes_orders_by_day_only() {
        let earlier = Act {
            day: WorldTime::from_ticks(0),
            ..an_act()
        };
        let later = Act {
            day: WorldTime::from_ticks(1),
            ..an_act()
        };
        assert!(act_precedes(&earlier, &later));
        assert!(!act_precedes(&later, &earlier));
        assert!(
            !act_precedes(&earlier, &earlier),
            "same day never precedes itself"
        );
    }

    #[test]
    fn present_at_is_plain_pool_membership() {
        let pool = [eid(2), eid(3)];
        assert!(present_at(eid(2), &pool));
        assert!(!present_at(eid(9), &pool));
        assert!(!present_at(eid(2), &[]));
    }

    #[test]
    fn witnessed_requires_presence_and_excludes_the_actor() {
        let act = an_act(); // actor eid(1)
        let pool = [eid(1), eid(2)];
        assert!(
            witnessed(&act, eid(2), &pool),
            "a co-located third party must witness the act"
        );
        assert!(
            !witnessed(&act, eid(1), &pool),
            "an act's own actor does not count as its witness"
        );
        assert!(
            !witnessed(&act, eid(9), &pool),
            "someone absent from the pool cannot witness the act"
        );
    }

    #[test]
    fn anyone_present_reads_agent_marks_only() {
        let agent_mark = hornvale_scene::Mark {
            noun: "a traveller".to_string(),
            kind: "agent".to_string(),
            datum: "someone is here".to_string(),
            salience: 0,
        };
        let settlement_mark = hornvale_scene::Mark {
            noun: "a settlement".to_string(),
            kind: "settlement".to_string(),
            datum: "a town".to_string(),
            salience: 0,
        };
        assert!(anyone_present(std::slice::from_ref(&agent_mark)));
        assert!(!anyone_present(&[settlement_mark]));
        assert!(!anyone_present(&[]));
    }
}
