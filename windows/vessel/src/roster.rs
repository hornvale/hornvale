//! The roster (The Rack, spec §3.1): a session's bodies held as a struct of
//! arrays, with one append that writes every column in one statement.
//!
//! **Why a struct of arrays and not a `Vec<struct>`.** The session already
//! held four parallel vectors — the bodies, their static roll keys, the
//! roll's mask, and (as an index into the first) the driven body — appended
//! at three separate sites and kept aligned by discipline. This campaign adds
//! two more columns, `position` and `felt`, which the tick writes and the
//! turn reads. Six hand-aligned vectors is a defect waiting to be written; a
//! struct of arrays with a single [`Roster::push`] makes misalignment
//! unrepresentable, because every column grows in the same statement or none
//! does.
//!
//! **A slot never moves** (decision 0546). Nothing here removes or reorders a
//! body. That is not an implementation convenience: `list_npcs`/`why`/
//! `colocated_npc` number every other body by its 1-based position among the
//! others, and `narrate_motion` zips a per-body "before" vector against the
//! roster positionally. A reorder silently renumbers a player's handles and
//! silently misattributes motion; an append cannot.
//!
//! **[`other_bodies`] and [`on_roll_others`] are free functions taking
//! slices, not `&self` methods**, and that is load-bearing rather than
//! stylistic — see their own docs.

use std::collections::BTreeMap;

use hornvale_kernel::{EntityId, Facet};

use crate::body::Body;
use crate::liveness::Felt;
use crate::roll::RollKeyStatic;

/// A body's index in the roster: the only way in. Never reassigned
/// (decision 0546).
///
/// A newtype rather than a bare `usize` so that "which body" cannot be
/// confused with any of the other counts this module hands out — a length, a
/// roll size, an ordinal within a settlement's lineage. The inner value is
/// public because [`Roster`]'s own columns are slices and a caller that has a
/// `Slot` must be able to index one.
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Slot(pub usize);

/// The session's bodies as a struct of arrays (The Rack, spec §3.1). Every
/// column is index-aligned by construction: [`Roster::push`] is the only
/// append and it pushes every column in one statement.
///
/// The columns divide into three kinds, and the difference decides what a
/// reader may conclude from a value:
///
/// - **Static** — `bodies` and `keys`, written once at the append and never
///   again.
/// - **A view** — `position`, which must agree with
///   `liveness::agent_position` at every read. A disagreement is a writer
///   bug, never a stale entry to be tolerated.
/// - **Content** — `felt`, which holds the body's own last resolution.
///   Between ticks a body does not re-feel, so an unchanged value is a fact
///   about a body that was not advanced.
///
/// `on_roll` is the mask the tick recomputes wholesale, so it is neither
/// appended to independently nor written per-slot: [`Roster::set_on_roll`]
/// replaces it as a unit, and refuses a mask that is not the roster's own
/// length.
pub struct Roster {
    /// Every body this session has derived, in derivation order.
    bodies: Vec<Body>,
    /// The static half of each body's roll key (see [`RollKeyStatic`]).
    keys: Vec<RollKeyStatic>,
    /// The roll's mask as of the most recent recompute — `true` is "the tick
    /// advances this body", `false` is dormant (spec §3.7). Empty until the
    /// first [`Self::set_on_roll`].
    ///
    /// **Only `DriveMovements.npcs` reads it.** Everything else in
    /// `Session::wait` — the `before` snapshot, `sensed_before`, the
    /// turned-hostile pass, `narrate_motion`'s positional zip — keeps
    /// iterating EVERY other body, which is what keeps that zip correct: a
    /// dormant body's position is constant across the wait, so it neither
    /// arrives nor departs and its slot still lines up.
    on_roll: Vec<bool>,
    /// Each body's room. Seeded at the append with the body's `home` and
    /// written by the tick.
    position: Vec<Facet>,
    /// Each body's felt state as its own last resolution left it. Seeded at
    /// the append with the stateless read and written by the tick.
    felt: Vec<Felt>,
    /// Which slot each appended entity took — the reverse of `bodies`, so a
    /// caller holding an [`EntityId`] does not linear-scan for it.
    slot_of: BTreeMap<EntityId, Slot>,
    /// Which slot is being driven.
    driven: Slot,
}

impl Roster {
    /// An empty roster whose driven slot is `driven`.
    ///
    /// The driven slot is fixed at construction rather than discovered later,
    /// which is why `Session::start_held` resolves
    /// `PossessTarget::Creature`'s entity against its derived `Vec<Body>`
    /// BEFORE building the roster out of it. That ordering is not new — the
    /// old `driven: usize` field was resolved at exactly the same point — and
    /// keeping it means there is never a moment where a roster exists with a
    /// driven slot nobody has chosen.
    pub fn new(driven: Slot) -> Self {
        Roster {
            bodies: Vec::new(),
            keys: Vec::new(),
            on_roll: Vec::new(),
            position: Vec::new(),
            felt: Vec::new(),
            slot_of: BTreeMap::new(),
            driven,
        }
    }

    /// Append one body with its roll key and its seeded felt state, returning
    /// the slot it took. **The only append**: every column grows here, in one
    /// statement, so no caller can add a body and forget a column.
    ///
    /// `position` is seeded from the body's own `home` rather than taken as a
    /// parameter, because a body that has committed no `agent-at` fact yet IS
    /// at its home — that is what `liveness::agent_position` returns for it,
    /// so the seed and the view agree from the first read.
    ///
    /// A repeated entity keeps the slot it first took. Appending the same
    /// entity twice would be a derivation bug (`derived_settlements` /
    /// `derived_herds` exist to make it impossible), and if one ever happens
    /// the FIRST slot is the one every handle and every zip already refers
    /// to.
    pub fn push(&mut self, body: Body, key: RollKeyStatic, felt: Felt) -> Slot {
        let slot = Slot(self.bodies.len());
        self.slot_of.entry(body.entity).or_insert(slot);
        self.position.push(body.home.clone());
        self.bodies.push(body);
        self.keys.push(key);
        self.felt.push(felt);
        slot
    }

    /// How many bodies the roster holds.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.bodies.len()
    }

    /// Whether the roster holds no bodies at all.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.bodies.is_empty()
    }

    /// Every body, in slot order.
    pub fn bodies(&self) -> &[Body] {
        &self.bodies
    }

    /// Every body's static roll key, in slot order.
    pub fn keys(&self) -> &[RollKeyStatic] {
        &self.keys
    }

    /// The roll's mask, in slot order — empty before the first
    /// [`Self::set_on_roll`].
    /// type-audit: bare-ok(flag: return)
    pub fn on_roll(&self) -> &[bool] {
        &self.on_roll
    }

    /// Every body's room, in slot order.
    pub fn positions(&self) -> &[Facet] {
        &self.position
    }

    /// Every body's felt state, in slot order.
    pub fn felts(&self) -> &[Felt] {
        &self.felt
    }

    /// The slot `entity` took, or `None` for an entity this roster never
    /// appended.
    pub fn slot_of(&self, entity: EntityId) -> Option<Slot> {
        self.slot_of.get(&entity).copied()
    }

    /// Which slot is being driven.
    pub fn driven(&self) -> Slot {
        self.driven
    }

    /// The body being driven.
    ///
    /// Panics on an empty roster, exactly as the `bodies[driven]` index it
    /// replaces did — `Session::start_held` refuses an empty cast before it
    /// ever builds a roster, so there is no reachable caller.
    pub fn driven_body(&self) -> &Body {
        &self.bodies[self.driven.0]
    }

    /// Replace the roll's mask.
    ///
    /// # Panics
    ///
    /// If `mask` is not exactly [`Self::len`] long. This is the roster's own
    /// rule, not one inherited from [`crate::roll::roll_of`] (which asserts
    /// nothing about the length of what it returns): the mask is a COLUMN
    /// here, index-aligned with every other column, and a short one would
    /// silently read as "dormant" for every body past its end — the exact
    /// class of misalignment this type exists to make unrepresentable. A
    /// panic beats a silently truncated roll.
    /// type-audit: bare-ok(flag: mask)
    pub fn set_on_roll(&mut self, mask: Vec<bool>) {
        assert_eq!(
            mask.len(),
            self.bodies.len(),
            "a roll mask must have one entry per body: got a mask of {} for {} bodies",
            mask.len(),
            self.bodies.len()
        );
        self.on_roll = mask;
    }

    /// Write one slot's tick-owned columns — the position the tick left the
    /// body at, and the felt state its resolution expressed.
    ///
    /// Static columns (`bodies`, `keys`) are deliberately not writable: a
    /// body's identity, home and roll key are settled at derivation.
    ///
    /// # Panics
    ///
    /// If `slot` is not a slot of this roster — a `Slot` can only come from
    /// [`Self::push`] or [`Self::slot_of`], so an out-of-range one is a
    /// caller mixing two rosters, which has no honest recovery.
    pub fn write(&mut self, slot: Slot, position: Facet, felt: Felt) {
        self.position[slot.0] = position;
        self.felt[slot.0] = felt;
    }

    /// How many bodies are on the roll, the driven one included.
    /// type-audit: bare-ok(count: return)
    pub fn on_roll_len(&self) -> usize {
        self.on_roll.iter().filter(|on| **on).count()
    }

    /// One slot's body, mutably — **a test seam and nothing else**, which is
    /// why it is `#[cfg(test)]` rather than merely `pub(crate)`.
    ///
    /// The static columns are otherwise unwritable on purpose (see
    /// [`Self::write`]). The one thing that needs to reach past that is
    /// `session.rs`'s
    /// `examine_reports_a_genuine_lens_failure_loudly_not_as_an_absence`,
    /// which corrupts the driven body's `home` in place to make
    /// `Session::position` describe nowhere real — the only way to prove the
    /// lens fails LOUDLY rather than silently reporting an absence. It does
    /// not reorder and it does not append, so no column can fall out of
    /// alignment through it.
    #[cfg(test)]
    pub(crate) fn body_mut(&mut self, slot: Slot) -> &mut Body {
        &mut self.bodies[slot.0]
    }
}

/// Every derived body other than the one being driven — what `Session::npcs`
/// meant before The Hand collapsed the two representations (Task 3): every
/// occupancy/social/perception/tick read that used to exclude the possessed
/// `Agent` by construction (it was never a member of that list) now excludes
/// it by this filter instead.
///
/// **A free function taking `bodies`/`driven` directly, not a `&self`
/// method.** `HeldContext`'s own doc explains why: a method call borrows
/// `self` as a whole, where a direct field expression borrows only that
/// field — and several callers (the `wait` tick's `turned-hostile` loop, in
/// particular) iterate this result while mutably borrowing `self.ledger` in
/// the same loop body, exactly as they iterated `self.npcs.iter()` before.
/// The same reasoning survives the move into this module unchanged: it is
/// now `self.roster.bodies()` that is borrowed, and `self.ledger` must stay
/// independently mutable across the iteration.
///
/// An owned `Vec` of borrows, not a slice (The Hand, Task 4 fix round 1):
/// `driven` can now name ANY roster slot, not only `0`
/// (`PossessTarget::Creature`), so "every OTHER body" can no longer be the
/// contiguous `bodies[1..]` this used to slice — it is `bodies` with
/// exactly the driven slot removed, ORDER PRESERVED. Order preservation is
/// load-bearing, not cosmetic: `list_npcs`/`why`/`colocated_npc` number
/// every other body by its 1-based POSITION in this list, and a body
/// uninvolved in the possession choice must keep the same handle number
/// regardless of which OTHER body is driven — an earlier version of this fix
/// swapped the driven body into slot `0` instead of filtering, which kept
/// `driven == 0` true but silently renumbered every handle between the old
/// and new driven slots, a user-visible regression no test caught until spec
/// review measured it directly
/// (`possessing_a_creature_does_not_renumber_other_bodies_handles`).
pub fn other_bodies(bodies: &[Body], driven: Slot) -> Vec<&Body> {
    bodies
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != driven.0)
        .map(|(_, npc)| npc)
        .collect()
}

/// Every other body ON THE ROLL: [`other_bodies`] narrowed to the mask
/// `wait` recomputed this tick (The Roll, spec §3.2/§3.7). The DRIVEN body is
/// excluded here exactly as it is there — it has its own arbitration —
/// even though its own mask slot is always `true`.
///
/// A body past the end of `on_roll` counts as ON, which is what makes an
/// empty mask (a roster whose roll has never been computed) behave like the
/// pre-roll "every body ticks" reading rather than silently ticking nobody.
/// type-audit: bare-ok(flag: on_roll)
pub fn on_roll_others<'a>(bodies: &'a [Body], on_roll: &[bool], driven: Slot) -> Vec<&'a Body> {
    bodies
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != driven.0 && on_roll.get(*i).copied().unwrap_or(true))
        .map(|(_, npc)| npc)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::liveness::{Affect, AffectLabel, Mode, ThreatNiche};
    use hornvale_kernel::{ConditionResponse, ResourceVector};

    /// A hand-built body at `home`, labelled `label` — the roster cares about
    /// exactly three of `Body`'s fields (entity, home, label), so everything
    /// else here is a fixed, uninteresting filler.
    fn body(entity: u64, home: Facet, label: &str) -> Body {
        Body {
            entity: EntityId(std::num::NonZeroU64::new(entity).expect("a nonzero test entity")),
            home,
            resource: room(0),
            species: "goblin".into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: ConditionResponse {
                optimum: 15.0,
                width: 10.0,
                devotion: 0.5,
            },
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            thermal_strategy: hornvale_species::ThermalStrategy::Endothermic,
            niche: ResourceVector::new(&[]).expect("the zero vector is valid"),
            boldness: 0.5,
            threat_niche: ThreatNiche {
                uncanny: 1.0,
                heat: 0.0,
                cold: 0.0,
                predator: 1.0,
            },
            mass_kg: crate::clock::REFERENCE_MASS_KG,
            label: label.into(),
            perception: hornvale_species::PerceptionVector::MANIKIN,
            village: None,
        }
    }

    /// A distinct room per `i` (for `i < 16`) on base face 0 — the roster
    /// stores rooms and never interprets them, so distinctness is all these
    /// tests need of geometry.
    fn room(i: u32) -> Facet {
        Facet {
            face: 0,
            path: vec![(i % 4) as u8, ((i / 4) % 4) as u8],
        }
    }

    /// A resident key at `ordinal` of an arbitrary settlement — the roster
    /// stores keys and never interprets them, so any two distinct ordinals do.
    fn key(ordinal: u16) -> RollKeyStatic {
        RollKeyStatic {
            wild: false,
            parent: 7,
            species: String::new(),
            ordinal,
        }
    }

    /// A quiescent felt state — the seed shape `Session::start_held` uses,
    /// with a fixed affect standing in for the stateless read.
    fn felt() -> Felt {
        Felt {
            affect: Affect {
                arousal: 0.0,
                valence: 0.0,
                label: AffectLabel::Content,
                object: None,
            },
            mode: Mode::Idle,
            suppressed: Vec::new(),
        }
    }

    /// Every column has one length after any sequence of pushes; `slot_of`
    /// answers each pushed entity with its slot.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: push `keys` twice in `push`
    /// (`self.keys.push(key.clone()); self.keys.push(key);`) — `keys()` reads
    /// 6 against the 3 every other column reads.
    #[test]
    fn every_column_shares_one_length() {
        let mut roster = Roster::new(Slot(0));
        assert!(roster.is_empty(), "a fresh roster holds nobody");
        for i in 0..3u32 {
            let slot = roster.push(
                body(u64::from(i) + 1, room(i), &format!("body {i}")),
                key(i as u16),
                felt(),
            );
            assert_eq!(
                slot,
                Slot(i as usize),
                "the {i}th push takes the {i}th slot"
            );
        }
        assert_eq!(roster.len(), 3, "three pushes, three bodies");
        assert_eq!(roster.bodies().len(), 3, "bodies column");
        assert_eq!(roster.keys().len(), 3, "keys column");
        assert_eq!(roster.positions().len(), 3, "position column");
        assert_eq!(roster.felts().len(), 3, "felt column");
        for i in 0..3u32 {
            let entity = EntityId(std::num::NonZeroU64::new(u64::from(i) + 1).expect("nonzero"));
            assert_eq!(
                roster.slot_of(entity),
                Some(Slot(i as usize)),
                "entity {} answers with its own slot",
                i + 1
            );
        }
        assert_eq!(
            roster.slot_of(EntityId(std::num::NonZeroU64::new(99).expect("nonzero"))),
            None,
            "an entity never pushed has no slot"
        );
    }

    /// A slot never moves: pushing more bodies leaves every earlier slot's
    /// entity in place (decision 0546).
    ///
    /// MUTATION THIS MUST FAIL AGAINST: sort `bodies` by label at the end of
    /// `push` (`self.bodies.sort_by(|a, b| a.label.cmp(&b.label));`) — slot 0
    /// holds "zulu" where the test expects "alpha", and `slot_of` then points
    /// at the wrong body.
    #[test]
    fn a_slot_never_moves() {
        let mut roster = Roster::new(Slot(0));
        // Pushed in an order that is NOT the labels' sort order, so a reorder
        // by any body field is visible rather than accidentally agreeing.
        let zulu = roster.push(body(1, room(1), "zulu"), key(0), felt());
        let alpha = roster.push(body(2, room(2), "alpha"), key(1), felt());
        assert_eq!(roster.bodies()[zulu.0].label, "zulu");
        assert_eq!(roster.bodies()[alpha.0].label, "alpha");
        let mike = roster.push(body(3, room(3), "mike"), key(2), felt());
        assert_eq!(
            roster.bodies()[zulu.0].label,
            "zulu",
            "the first body kept its slot across two more pushes"
        );
        assert_eq!(
            roster.bodies()[alpha.0].label,
            "alpha",
            "the second body kept its slot across one more push"
        );
        assert_eq!(roster.bodies()[mike.0].label, "mike");
        assert_eq!(
            roster.slot_of(EntityId(std::num::NonZeroU64::new(1).expect("nonzero"))),
            Some(zulu),
            "the reverse index still names the first body's slot"
        );
    }

    /// `set_on_roll` refuses a mask of the wrong length — the roster's own
    /// documented rule, since the mask is a column and a short one would read
    /// as "dormant" for every body past its end.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: relax the `assert_eq!` in
    /// `set_on_roll` to `assert!(mask.len() <= self.bodies.len(), "mask")` —
    /// the short mask is accepted and no panic occurs.
    #[test]
    #[should_panic(expected = "mask")]
    fn a_short_mask_is_refused() {
        let mut roster = Roster::new(Slot(0));
        roster.push(body(1, room(1), "one"), key(0), felt());
        roster.push(body(2, room(2), "two"), key(1), felt());
        roster.set_on_roll(vec![true]);
    }

    /// `write` replaces a slot's tick-owned columns and touches no other
    /// slot — the tick's write from Task 3, pinned here where the type is
    /// defined rather than at its first caller.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: write `self.position[0] = position;`
    /// instead of `self.position[slot.0] = position;` — slot 1's position is
    /// unchanged and slot 0's is clobbered.
    #[test]
    fn a_write_moves_one_slot_only() {
        let mut roster = Roster::new(Slot(0));
        roster.push(body(1, room(1), "one"), key(0), felt());
        roster.push(body(2, room(2), "two"), key(1), felt());
        assert_eq!(roster.positions()[0], room(1), "seeded from home");
        assert_eq!(roster.positions()[1], room(2), "seeded from home");
        roster.write(Slot(1), room(9), felt());
        assert_eq!(roster.positions()[0], room(1), "slot 0 is untouched");
        assert_eq!(roster.positions()[1], room(9), "slot 1 moved");
    }

    /// `other_bodies` drops exactly the driven slot and preserves order;
    /// `on_roll_others` narrows that to the mask.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: filter `*i != driven.0` to
    /// `*i != 0` in `other_bodies` — with `driven` at slot 1 the driven body
    /// is returned and body 0 is dropped, so the labels read `["one",
    /// "three"]` against the expected `["zero", "three"]`.
    #[test]
    fn the_driven_slot_is_the_one_dropped() {
        let mut roster = Roster::new(Slot(1));
        for i in 0..3u32 {
            roster.push(
                body(
                    u64::from(i) + 1,
                    room(i),
                    ["zero", "one", "three"][i as usize],
                ),
                key(i as u16),
                felt(),
            );
        }
        let others: Vec<&str> = other_bodies(roster.bodies(), roster.driven())
            .iter()
            .map(|b| b.label.as_str())
            .collect();
        assert_eq!(others, vec!["zero", "three"], "the driven slot is dropped");
        roster.set_on_roll(vec![false, true, true]);
        assert_eq!(roster.on_roll_len(), 2, "two slots on the roll");
        let on: Vec<&str> = on_roll_others(roster.bodies(), roster.on_roll(), roster.driven())
            .iter()
            .map(|b| b.label.as_str())
            .collect();
        assert_eq!(on, vec!["three"], "the dormant slot 0 is dropped too");
    }
}
