//! The resident fold store: the session-owned, per-entity accumulation of
//! what the ledger already determines (The Pawl, spec §2). The decisions this
//! layer is ratified under are `decision 0536` (the store is session-owned and
//! never serialized), `decision 0537` (a reader never observes a fold behind
//! its ledger), `decision 0538` (the trail is a resident index, not a cached
//! hub), `decision 0539` (a past-instant read resumes from the reset
//! checkpoint), `decision 0540` (the past-day affect path preserves the
//! unfiltered reset, knowingly) and `decision 0541` (a campaign-time
//! hash-constant witness retires at close).
//!
//! **The currency invariant is that a reader never observes a fold behind the
//! ledger it is given** (spec §2.2). The seam is at READ, not at commit:
//! `Ledger::commit` gains no hook, and every read of a tenant first advances
//! that tenant to the end of the ledger it was handed. `Folded::advance_to` is
//! idempotent in position, so a second evaluation of the same walk, a re-read
//! within a turn, or a snapshot after a `wait` each cost O(facts committed
//! since the last read) and absorb nothing twice — which is what makes the
//! tick's "same walk, read twice" shape (`Session::wait`) a non-issue rather
//! than a design constraint on the commit path. **Nothing here is ever
//! serialized**: the store has no `Serialize`, is not reachable from `World`,
//! and holds only values the ledger re-determines, so discarding it at any
//! instant is unobservable and a save written mid-session contains none of it.

use crate::liveness::{AGENT_AT, DRANK, EATEN, Terrain, is_water, room_from_text};
use hornvale_kernel::fold::{Folded, LedgerFold};
use hornvale_kernel::{EntityId, Facet, Fact, Ledger, Value, WorldTime};
use hornvale_species::ThermalStrategy;
use std::collections::BTreeMap;

/// One entity's committed `agent-at` trail, in the order `agent_sightings`
/// sorted it: by `(day, room)` ascending. Absorbed once per fact, never
/// rebuilt. (That function is DELETED as of The Pawl's Task 3 — the hub is
/// deleted, not cached — and its body survives verbatim as the oracle in
/// `windows/vessel/tests/suite/resident_folds.rs`, which is what this order is
/// pinned against.) A resident permutation index (spec §2.4) — not a
/// cached hub: its contents are a strict subset of the ledger's own, each
/// fact is absorbed exactly once, and no caller ever rebuilds a timeline per
/// call.
///
/// **The insert is at the sorted position, not an append**, and that is the
/// one line of this type worth being deliberate about. Spec §3 rule 2 asks
/// whether an entity's `agent-at` facts ever commit out of day order; the
/// witness lives in `windows/vessel/tests/suite/resident_folds.rs` and prints
/// its verdict. Inserting at the sorted position is correct under either
/// answer, and it is still O(log h) to find the position plus a memmove —
/// never a rebuild. Ties (the same `(day, room)` twice) are inserted AFTER
/// their equals, which is what a stable sort of the commit order does, so the
/// two agree even where the key does not discriminate.
#[derive(Debug, PartialEq, Default)]
pub struct Trail {
    /// Each entity's sightings, ascending by `(day, room)`.
    by_entity: BTreeMap<EntityId, Vec<(WorldTime, Facet)>>,
}

impl Trail {
    /// The entity's trail, ascending by `(day, room)`; empty if never seen.
    pub fn of(&self, entity: EntityId) -> &[(WorldTime, Facet)] {
        self.by_entity
            .get(&entity)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// Index of the first entry with day > `t` — equivalently, the length of
    /// the `day <= t` prefix of [`Self::of`]. The trail is sorted by day, so
    /// this is a binary search rather than a scan; it is the door every
    /// past-instant read goes through.
    /// type-audit: bare-ok(index: return)
    pub fn prefix_len(&self, entity: EntityId, t: WorldTime) -> usize {
        self.of(entity).partition_point(|(d, _)| *d <= t)
    }

    /// Every entity's sighting count summed — every dated `agent-at` fact this
    /// index has absorbed, counted once each (The Kerf, spec §4 K1).
    ///
    /// Equal to [`LatestVisit::entries`] over the same ledger: the two hold the
    /// identical `(instant, room)` population keyed the other way round, so a
    /// disagreement is a defect in one of them and
    /// `the_trail_and_the_visit_lists_hold_the_same_sighting_count` asserts it.
    /// type-audit: bare-ok(count: return)
    pub fn entries(&self) -> usize {
        self.by_entity.values().map(Vec::len).sum()
    }

    /// An estimate of the bytes this index holds, over every entity: one
    /// `size_of::<(WorldTime, Facet)>()` per sighting plus that sighting's own
    /// room `path` heap length, summed the same way
    /// [`crate::ground::GroundHazards::held_bytes`] sums its key set. An
    /// ESTIMATE of held data, not an allocator measurement: it counts neither
    /// `BTreeMap`/`Vec` overhead nor any allocator slack.
    /// type-audit: bare-ok(count: return)
    pub fn held_bytes(&self) -> usize {
        self.by_entity
            .values()
            .flat_map(|trail| trail.iter())
            .map(|(_, room)| std::mem::size_of::<(WorldTime, Facet)>() + room.path.len())
            .sum()
    }
}

impl LedgerFold for Trail {
    fn empty() -> Self {
        Trail::default()
    }

    fn absorb(&mut self, fact: &Fact) {
        if fact.predicate != AGENT_AT {
            return;
        }
        let Value::Text(s) = &fact.object else {
            return;
        };
        // An undated `agent-at` is not a sighting: `agent_sightings` filtered
        // it out (`f.day?`) and so does this — see the type doc for where that
        // function's body now lives.
        let Some(day) = fact.day else {
            return;
        };
        let entry = (day, room_from_text(s));
        let trail = self.by_entity.entry(fact.subject).or_default();
        // Strictly-less keeps equal keys in commit order, matching the stable
        // `sort_by` the scan half uses.
        let at = trail.partition_point(|e| *e < entry);
        trail.insert(at, entry);
    }
}

/// Every entity's visits, INDEXED BY ROOM: for each room the entity has stood
/// in, the sorted list of instants it was sighted there — the state
/// `hazard_memory_memo`'s per-call `latest` map becomes (spec §2.4), and the
/// domain the alarm scan's frightening-room test is applied over.
///
/// **A per-room VISIT LIST, not a latest-day map, and that shape was decided
/// on a measured number rather than assumed.** Spec §3 rule 6 asks whether any
/// production read of this fold runs at an instant strictly before a committed
/// sighting of the same entity; a latest-day map (advanced to the ledger's
/// end) can only answer at the ledger's end, so a past-instant read would need
/// either a rebuild — which spec §2.2 refuses on a hot path — or the list. The
/// witness (`windows/vessel/tests/suite/resident_folds.rs`,
/// `rule_six_witness_hazard_memory_reads_run_at_past_instants`) measured the
/// emitter-gated replay shape at **9 of 9** hazard reads at past instants, so
/// the branch fired and this is the fallback the rule names. Reading it is
/// [`Self::latest_at`]: one `partition_point` per room, over that room's own
/// visits, never over history.
///
/// **It is a permutation index of [`Trail`], the way `Trail` is one of the
/// ledger** — the same `(instant, room)` pairs, keyed the other way round.
/// That is what makes the hazard fold's per-tick cost O(rooms visited × log
/// visits-per-room) where it was O(history): the old loop walked every
/// `agent-at` fact the entity had ever committed, on every call, to rebuild a
/// map whose size is bounded by the rooms it has stood in.
///
/// **Three reads, one state.** [`Self::latest_at`] serves the hazard fold's
/// most-recent-visit-per-room rule; [`Self::rooms_at`] serves the ALARM scan,
/// which needs only the DISTINCT rooms a roster member has stood in at or
/// before `t` so it can ask terrain whether each frightens that member;
/// [`Self::water_at`] serves the belief read with the same first-visit domain
/// and its water predicate. The
/// alarm half is the tenant spec §2.4 lists as `Alarm`, and it holds no state
/// of its own on purpose: everything `build_emitter_scan` accumulates —
/// which members can ever emit, the halo of rooms their alarms could reach —
/// is a function of TERRAIN and the member's own threat niche as well as the
/// ledger, so none of it can live inside a [`LedgerFold`] (the same argument
/// former belief tenant made for `is_water`). What the ledger determines is
/// the visits, and those are here; the predicate is applied at read, and its
/// result is memoised per tick in `PrimaryAfraidMemo::scans` exactly as the
/// scan's output already was.
#[derive(Debug, PartialEq, Default)]
pub struct LatestVisit {
    /// Each entity's rooms, and each room's visit instants, ascending.
    visits: BTreeMap<EntityId, BTreeMap<Facet, Vec<WorldTime>>>,
}

impl LatestVisit {
    /// Every room the entity has stood in, keyed to that room's ascending
    /// visit instants; empty if it was never seen.
    pub fn of(&self, entity: EntityId) -> &BTreeMap<Facet, Vec<WorldTime>> {
        static EMPTY: BTreeMap<Facet, Vec<WorldTime>> = BTreeMap::new();
        self.visits.get(&entity).unwrap_or(&EMPTY)
    }

    /// The MOST RECENT visit at or before `t` for every room the entity has
    /// stood in by then, ascending by room — `hazard_memory_memo`'s `latest`
    /// map, exactly. A room with no visit at or before `t` is absent, which is
    /// what the old loop's `day <= t` filter did before it ever reached the
    /// map.
    ///
    /// O(rooms visited × log visits-per-room): one `partition_point` per room.
    pub fn latest_at(&self, entity: EntityId, t: WorldTime) -> BTreeMap<Facet, WorldTime> {
        let mut latest = BTreeMap::new();
        for (room, days) in self.of(entity) {
            let n = days.partition_point(|d| *d <= t);
            if n > 0 {
                latest.insert(room.clone(), days[n - 1]);
            }
        }
        latest
    }

    /// The DISTINCT rooms the entity had stood in at or before `t`, retaining
    /// only the rooms `keep` admits, ascending by room.
    fn rooms_at_where(
        &self,
        entity: EntityId,
        t: WorldTime,
        keep: impl Fn(&Facet) -> bool,
    ) -> Vec<Facet> {
        self.of(entity)
            .iter()
            .filter(|(room, days)| days.first().is_some_and(|first| *first <= t) && keep(room))
            .map(|(room, _)| room.clone())
            .collect()
    }

    /// The DISTINCT rooms the entity had stood in at or before `t`, ascending
    /// — the alarm scan's domain (see the type doc's "three reads" paragraph).
    ///
    /// The FIRST visit decides membership, so the test is against the list's
    /// first element rather than a `partition_point`: a room is in the set as
    /// soon as any sighting of it is at or before `t`.
    pub fn rooms_at(&self, entity: EntityId, t: WorldTime) -> Vec<Facet> {
        self.rooms_at_where(entity, t, |_| true)
    }

    /// The WATER rooms the entity had stood in at or before `t`, ascending —
    /// `believed_water`'s candidate set.
    ///
    /// Correctness depends on each room's visit list being ascending, so its
    /// first element is the room's first visit. Task 3's descending-order
    /// fixture in `windows/vessel/tests/suite/resident_folds.rs` pins that
    /// invariant against a room whose facts arrive in reverse day order.
    pub fn water_at(&self, entity: EntityId, t: WorldTime, terrain: &dyn Terrain) -> Vec<Facet> {
        self.rooms_at_where(entity, t, |room| is_water(room, terrain))
    }

    /// Every VISIT INSTANT this index holds, summed over every entity and every
    /// one of its rooms — the same population [`Trail::entries`] counts, keyed
    /// the other way round (The Kerf, spec §4 K1).
    ///
    /// It is NOT the room-key count: a room visited three times contributes
    /// three here and one room key. Both identities are asserted in this
    /// module's tests.
    /// type-audit: bare-ok(count: return)
    pub fn entries(&self) -> usize {
        self.visits
            .values()
            .flat_map(|rooms| rooms.values())
            .map(Vec::len)
            .sum()
    }

    /// An estimate of the bytes this index holds, over every entity: every
    /// visited room's key size (`size_of::<Facet>()` plus its `path`'s heap
    /// length) plus one `size_of::<WorldTime>()` per instant in that room's
    /// visit list, summed the same way
    /// [`crate::ground::GroundHazards::held_bytes`] sums its key set. An
    /// ESTIMATE of held data, not an allocator measurement: it counts neither
    /// `BTreeMap`/`Vec` overhead nor any allocator slack.
    /// type-audit: bare-ok(count: return)
    pub fn held_bytes(&self) -> usize {
        self.visits
            .values()
            .flat_map(|rooms| rooms.iter())
            .map(|(room, days)| {
                std::mem::size_of::<Facet>()
                    + room.path.len()
                    + days.len() * std::mem::size_of::<WorldTime>()
            })
            .sum()
    }
}

impl LedgerFold for LatestVisit {
    fn empty() -> Self {
        LatestVisit::default()
    }

    fn absorb(&mut self, fact: &Fact) {
        if fact.predicate != AGENT_AT {
            return;
        }
        let Value::Text(s) = &fact.object else {
            return;
        };
        // An UNDATED `agent-at` is not a sighting: `hazard_memory_memo`'s own
        // loop reads `f.day.filter(|d| *d <= t)`, so an undated fact never
        // reached its map at any `t`, and it never reaches this one.
        let Some(day) = fact.day else {
            return;
        };
        let days = self
            .visits
            .entry(fact.subject)
            .or_default()
            .entry(room_from_text(s))
            .or_default();
        // At the sorted position, for [`Trail`]'s reason and with the same
        // cost — correct whether or not an entity's sightings ever commit out
        // of day order, and never a rebuild. `<=` keeps equal instants in
        // commit order, which the reads above cannot distinguish anyway (both
        // ask only where the `<= t` boundary falls).
        let at = days.partition_point(|d| *d <= day);
        days.insert(at, day);
    }
}

/// One entity's committed RESET instants for one sustenance drive — the
/// `drank` days for thirst, the `eaten` days for hunger — ascending.
///
/// The state is only the resets, and that is the whole design. The thirst and
/// hunger path integrals are `rate × span` summed over the segments between a
/// reset and the read instant, and the segment BOUNDARIES are the entity's
/// sightings, which [`Trail`] already holds. So the reset list plus the trail
/// is everything a read needs: a reset is the integral's own checkpoint (the
/// integral is zero there by construction), and the read resumes from the
/// latest reset and advances over the trail range `(reset, t]`. Nothing is
/// accumulated here that terrain could change, which is what keeps
/// [`LedgerFold::absorb`] a pure function of `(self, fact)`: the dehydration
/// RATE needs a temperature and therefore a room and a day, so every rate
/// multiplication happens at READ, never at absorb.
///
/// Both drives share this type — see [`ThirstResets`] and [`HungerResets`] for
/// the two tenants that choose which predicate is a reset. The list is kept
/// ASCENDING rather than in commit order, for [`Trail`]'s reason and with the
/// same cost: an insert at the sorted position is O(log n) to find plus a
/// memmove, never a rebuild, and it is correct whether or not a reset ever
/// commits out of day order. Sorted is also what both questions below want —
/// today's unfiltered lookup is the list's LAST element, and a filtered one is
/// a binary search — where a commit-order list would have to scan for a
/// maximum.
#[derive(Debug, PartialEq, Default)]
pub struct Sustenance {
    /// Each entity's reset instants, ascending.
    by_entity: BTreeMap<EntityId, Vec<WorldTime>>,
}

impl Sustenance {
    /// Absorb `fact` if it is a dated reset of `predicate` — the shared body
    /// behind both tenants' [`LedgerFold::absorb`].
    ///
    /// An UNDATED reset is not a reset: every existing lookup this replaces
    /// (`drive_at`'s and `WalkState::begin`'s `filter_map(|f| f.day)` folds,
    /// `last_fact_day_at_or_before`'s identical filter) drops it, and so does
    /// this. The fact's `object` is never read, by any of them.
    fn absorb_reset(&mut self, predicate: &str, fact: &Fact) {
        if fact.predicate != predicate {
            return;
        }
        let Some(day) = fact.day else {
            return;
        };
        let resets = self.by_entity.entry(fact.subject).or_default();
        let at = resets.partition_point(|d| *d <= day);
        resets.insert(at, day);
    }

    /// The entity's reset instants, ascending; empty if it never reset.
    pub fn resets(&self, entity: EntityId) -> &[WorldTime] {
        self.by_entity
            .get(&entity)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// The latest reset with NO bound on the read instant — today's
    /// `drive_at`/`hunger_at`/`WalkState::begin` semantics, which fold the max
    /// over EVERY committed reset and never filter to the instant being asked
    /// about (spec §3 rule 1, the Tailrace's trap 5).
    ///
    /// A caller that wants the pre-existing `fold(GENESIS, max)` answer must
    /// still floor the result at [`WorldTime::GENESIS`] itself — see the call
    /// sites, which do so explicitly. Returning `Option` rather than a
    /// `GENESIS` sentinel is `last_fact_day_at_or_before`'s own argument
    /// (decision 0126 makes negative days legal, so "no reset" and "a reset at
    /// genesis" are different answers and only the caller knows which default
    /// it means).
    pub fn last_reset(&self, entity: EntityId) -> Option<WorldTime> {
        self.resets(entity).last().copied()
    }

    /// The latest reset at or before `t`, else `None` — what a fold at the
    /// position `t` implies would see, and exactly what
    /// `last_fact_day_at_or_before` computes for `drank`/`eaten` today.
    /// O(log resets).
    pub fn last_reset_at_or_before(&self, entity: EntityId, t: WorldTime) -> Option<WorldTime> {
        let resets = self.resets(entity);
        let n = resets.partition_point(|d| *d <= t);
        (n > 0).then(|| resets[n - 1])
    }
}

/// The thirst tenant: [`Sustenance`] over [`DRANK`].
///
/// A newtype rather than a field on a configurable `Sustenance`, because
/// [`LedgerFold::empty`] takes no arguments and so cannot be told which
/// predicate resets it. The two tenants are therefore two types over one
/// state, which is also what lets one read function serve both drives.
#[derive(Debug, PartialEq, Default)]
pub struct ThirstResets(Sustenance);

impl ThirstResets {
    /// The resets themselves.
    pub fn get(&self) -> &Sustenance {
        &self.0
    }
}

impl LedgerFold for ThirstResets {
    fn empty() -> Self {
        ThirstResets::default()
    }

    fn absorb(&mut self, fact: &Fact) {
        self.0.absorb_reset(DRANK, fact);
    }
}

/// The hunger tenant: [`Sustenance`] over [`EATEN`] — [`ThirstResets`]'s twin,
/// and see that type for why there are two of them.
#[derive(Debug, PartialEq, Default)]
pub struct HungerResets(Sustenance);

impl HungerResets {
    /// The resets themselves.
    pub fn get(&self) -> &Sustenance {
        &self.0
    }
}

impl LedgerFold for HungerResets {
    fn empty() -> Self {
        HungerResets::default()
    }

    fn absorb(&mut self, fact: &Fact) {
        self.0.absorb_reset(EATEN, fact);
    }
}

/// Which sustenance integral a read is accumulating: the thermal class and
/// base rate the rate function was asked for, paired with the reset the
/// integral resumes from.
///
/// **Every field is one the answer depends on, and none of them is the
/// ledger.** `rise_at` reads exactly two things about a drive — the thermal
/// strategy and `DriveParams::rise` — and the integral's zero is the reset,
/// so two reads that agree on all three accumulate the identical prefix and
/// two that differ on any of them do not. Thirst and hunger of one creature
/// are therefore different keys (their `rise` differs), and so are the same
/// creature's reads before and after it drinks.
///
/// The base rate is carried as its exact `f64` bit pattern rather than as an
/// `f64`, because this is a `BTreeMap` key and the ordering must be total.
/// Bit equality is STRICTER than numeric equality (`-0.0` and `0.0` are
/// different keys), which is the safe direction: a spurious miss costs a
/// recomputation, a spurious hit would return the wrong number.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DriveKey {
    /// The thermal strategy, as the ordinal [`drive_class_ordinal`] assigns.
    class: u8,
    /// [`crate::liveness::DriveParams::rise`]'s exact bit pattern.
    rise: u64,
}

/// The ordinal [`DriveKey`] carries a [`ThermalStrategy`] as.
///
/// A hand-written total match rather than a cast, so that widening the enum is
/// a compile error here instead of a silently-merged key.
/// type-audit: bare-ok(index: return)
fn drive_class_ordinal(class: ThermalStrategy) -> u8 {
    match class {
        ThermalStrategy::Endothermic => 0,
        ThermalStrategy::Ectothermic => 1,
        ThermalStrategy::Unmodelled => 2,
        ThermalStrategy::Absent => 3,
    }
}

impl DriveKey {
    /// The key for a read of `class` at base rate `rise`.
    /// type-audit: bare-ok(ratio: rise)
    pub fn new(class: ThermalStrategy, rise: f64) -> Self {
        DriveKey {
            class: drive_class_ordinal(class),
            rise: rise.to_bits(),
        }
    }
}

/// How many reset partitions one `(entity, drive)` keeps memoised at once.
///
/// Two, because two is what the live shapes ask for and a third would only
/// ever be a stale one: `drive_at`/`hunger_at` read from the UNFILTERED last
/// reset while the tick's own walk reads from a LOCAL reset it has already
/// advanced past a `drank` it is about to commit, so a creature that drinks
/// mid-tick is read from exactly two instants in the same turn. Evicting is
/// unobservable — a partition is a pure function of `(ledger prefix,
/// temperature field, home)` and is rebuilt on demand — so this bounds memory
/// without bounding correctness; it costs a rebuild, never an answer.
/// plumb: pending(wave-1)
const MEMO_PARTITIONS_PER_DRIVE: usize = 2;

/// One boundary of a memoised integral: a distinct sighting instant, the
/// integral accumulated from the partition's reset THROUGH it, and how many
/// trail entries lie at or before it.
///
/// The governing position is NOT stored: it is `trail[consumed - 1]` (the
/// entity's latest sighting at or before this instant, which is what a reverse
/// scan of the sorted trail finds), so an index answers what a cloned
/// [`Facet`] would and cannot fall out of step with the trail it came from.
#[derive(Debug)]
struct MemoBoundary {
    /// The distinct sighting instant this boundary sits at.
    day: WorldTime,
    /// The integral from the partition's reset through `day` — the same `f64`
    /// additions, in the same order, the un-memoised loop made.
    acc: f64,
    /// How many trail entries lie at or before `day`.
    consumed: usize,
}

/// One reset partition's prefix accumulator: the integral from a reset,
/// checkpointed at every distinct sighting instant after it.
///
/// **Prefix sums restart at zero at each reset and are never subtracted.**
/// `acc` at boundary `i` is literally the running total the un-memoised loop
/// held when it reached that boundary, so resuming from it and adding the
/// remaining windows performs the identical additions in the identical order.
/// Differencing two partitions' accumulators would be arithmetically
/// equivalent and NOT bit-identical, which is why each reset gets its own
/// zero.
#[derive(Debug)]
struct MemoPartition {
    /// The reset the integral resumes from — its own zero.
    reset: WorldTime,
    /// How many trail entries lie at or before `reset`.
    reset_consumed: usize,
    /// One entry per distinct sighting instant strictly after `reset`,
    /// ascending.
    boundaries: Vec<MemoBoundary>,
}

impl MemoPartition {
    /// An empty partition anchored at `reset`, against the trail as it stands.
    fn new(reset: WorldTime, trail: &[(WorldTime, Facet)]) -> Self {
        MemoPartition {
            reset,
            reset_consumed: trail.partition_point(|(d, _)| *d <= reset),
            boundaries: Vec::new(),
        }
    }

    /// The last boundary reached: its instant, the integral through it, and
    /// the trail entries consumed by it.
    fn frontier(&self) -> (WorldTime, f64, usize) {
        match self.boundaries.last() {
            Some(b) => (b.day, b.acc, b.consumed),
            None => (self.reset, 0.0, self.reset_consumed),
        }
    }

    /// Whether this partition still describes `trail` — EXACTLY, not
    /// heuristically.
    ///
    /// [`Trail::absorb`] inserts at the sorted position, so a fact committed
    /// out of day order lands BEFORE the frontier and silently changes both a
    /// boundary set and a governing position. The test for that is one binary
    /// search: the number of trail entries at or before the frontier instant
    /// must still be the number this partition consumed to reach it. An
    /// insertion at or before the frontier moves that count and is caught; an
    /// insertion after it is simply unconsumed, and the next advance takes it
    /// in sorted position like any other.
    fn is_current_with(&self, trail: &[(WorldTime, Facet)]) -> bool {
        let (day, _, consumed) = self.frontier();
        trail.partition_point(|(d, _)| *d <= day) == consumed
    }

    /// Absorb every sighting the trail has gained since the frontier, one
    /// boundary per DISTINCT instant, and return how many windows that
    /// integrated.
    ///
    /// `rate` is the caller's own rate function — `(governing position,
    /// segment start in standard days) -> per-day rate` — so this type never
    /// sees terrain, a thermal class or a drive's parameters, and the
    /// arithmetic below is the caller's arithmetic verbatim.
    ///
    /// **It walks to the trail's END, not to the instant being read, and that
    /// is the one case where a read can cost MORE than the un-memoised one
    /// did.** A read at a past `t` on a partition that is fresh (or was
    /// evicted) integrates `reset -> end of trail` where the old code
    /// integrated `reset -> t`. The extra work is bounded by the trail beyond
    /// `t` and is amortised away by the very next read of that partition,
    /// which finds the frontier already there; in practice the catch-up window
    /// is what the ledger grew since the previous read. Advancing to the trail
    /// end rather than to `t` is what makes the memo `t`-independent, which is
    /// what lets one partition serve the live read and every past-instant
    /// replay of the same drive.
    fn advance(
        &mut self,
        trail: &[(WorldTime, Facet)],
        home: &Facet,
        rate: &mut dyn FnMut(&Facet, f64) -> f64,
    ) -> u64 {
        let (mut b, mut acc, mut consumed) = self.frontier();
        let mut integrated = 0u64;
        let mut i = consumed;
        while i < trail.len() {
            // The run of entries sharing this instant — the `dedup` the
            // un-memoised bounds list applied to coincident sightings.
            let u = trail[i].0;
            let mut j = i + 1;
            while j < trail.len() && trail[j].0 == u {
                j += 1;
            }
            // The governing position for the window that STARTS at `b`: the
            // latest sighting at or before it, else `home`.
            let pos = if consumed > 0 {
                &trail[consumed - 1].1
            } else {
                home
            };
            let s = b.as_std_days();
            let e = u.as_std_days();
            acc += rate(pos, s) * (e - s);
            self.boundaries.push(MemoBoundary {
                day: u,
                acc,
                consumed: j,
            });
            b = u;
            consumed = j;
            i = j;
            integrated += 1;
        }
        integrated
    }

    /// The boundary a read at `t` may resume from: the last one strictly
    /// before `t` that the overlay cannot have disturbed.
    ///
    /// `cut` is the earliest instant this tick's uncommitted overlay carries.
    /// A memoised window is valid only if the overlay could neither split it
    /// (an overlay instant strictly inside it) nor change the position
    /// governing it (an overlay sighting at or before its start), and both are
    /// excluded by keeping only boundaries at or before `cut` — see
    /// [`SustenanceMemo`]'s type doc.
    fn resume_at(&self, cut: Option<WorldTime>, t: WorldTime) -> (WorldTime, f64) {
        let mut usable = self.boundaries.partition_point(|b| b.day < t);
        if let Some(c) = cut {
            usable = usable.min(self.boundaries.partition_point(|b| b.day <= c));
        }
        match usable {
            0 => (self.reset, 0.0),
            k => (self.boundaries[k - 1].day, self.boundaries[k - 1].acc),
        }
    }
}

/// The read-side accumulator spec §2.4 named and Task 3 did not build: per
/// entity, per drive, per reset, the sustenance integral checkpointed at every
/// sighting.
///
/// **What it buys.** [`Sustenance`] holds reset instants only, so a read had
/// to re-integrate from the last reset every time — O(sightings since the
/// reset), with one terrain sample each. For a creature that never resets that
/// is O(history) per read forever, which is 21 of 50 agents on the seed-42
/// bench and the mechanism behind spec §11.7's ~200x
/// ecological-versus-synthetic gap. With the accumulator a live read costs the
/// sightings absorbed since the previous read of that drive, and a past-instant
/// read is a binary search into the partition plus the one open segment.
///
/// **It is a pure function of `(ledger prefix, temperature field, home)`, and
/// NOT a [`LedgerFold`].** The rate at a segment needs a temperature, and
/// terrain is not a ledger fact — the same argument [`LatestVisit`]'s doc makes
/// for `is_water`. So this cannot live inside a fold whose FOLD-equals-SCAN
/// property is stated over the ledger alone; it sits beside them, advanced
/// lazily at read, and [`LedgerFold::absorb`] stays terrain-free. **`home` is
/// the third input and is easy to miss**: it is the position governing any
/// window that starts before the entity's first sighting, so it is baked into
/// `acc` exactly as a temperature is. Every production call site passes
/// `&npc.home` off a [`crate::body::Body`] derived once, so it is stable per
/// entity for a store's whole life; a caller that varied it per read would be
/// reading a prefix it never produced, and this is the sentence that says so.
///
/// **ONE TEMPERATURE FUNCTION PER STORE — which is NOT the same as one
/// `Terrain` VALUE per store, and an earlier draft of this doc claimed the
/// stronger thing and was wrong about production.** A partition's accumulated
/// `f64`s are the answers one temperature field gave; resuming from them under
/// a field that disagrees would resume from a prefix that field never
/// produced. What production actually does is build MANY `LocaleTerrain`
/// values against ONE store: `Session` constructs one at five sites
/// (`snapshot`, `hazard_memories`, `terrain_here`, `wait`, `needs`), and the
/// lab's `run_simulation_with_locale` rebuilds one every tick while its
/// `folds` is created once per run. The invariant those satisfy is the
/// context-level one, and it holds for a reason worth stating rather than
/// assuming: [`crate::liveness::LocaleTerrain`]'s `temperature` reads only
/// `LocaleContext::temperature_at_cached`, which resolves corner weights
/// through `corner_weights_for` — a cache HIT returns the memoised integer
/// `[(Vertex, u64); 4]`, a MISS recomputes the same pure function of
/// `(addr, geosphere, index)` — and then blends them in
/// `temperature_with_weights`. The `RoomMeshMemo` is therefore a faithful
/// cache of a pure function, not a parameter of it, so two `LocaleTerrain`
/// values over the same `LocaleContext` are interchangeable here however their
/// mesh memos differ. **So: one `LocaleContext` per store.** A store belongs
/// to one session and one world, which is how production owns it. Discarding
/// the memo is unobservable at any instant, so a caller that must change the
/// temperature field itself — a different context, or a stub terrain beside a
/// real one — builds a new store; the test fixtures that read two terrains do
/// exactly that.
///
/// **Bit-identity is by construction.** The un-memoised loop summed the
/// windows `[reset, s1], [s1, s2], ..., [sn, t]` in order and clamped the
/// total once. A partition's `acc` is that same running total frozen at each
/// `si`; a read resumes from it and adds the remaining windows with the
/// caller's own rate function. Same terms, same order, same clamp — the
/// FOLD-equals-SCAN sweeps in `windows/vessel/tests/suite/resident_folds.rs`
/// compare with `==` on `f64` and are the proof.
#[derive(Debug, Default)]
pub struct SustenanceMemo {
    /// Per `(entity, drive)`, the memoised reset partitions, ascending by
    /// reset instant and capped at [`MEMO_PARTITIONS_PER_DRIVE`].
    partitions: BTreeMap<(EntityId, DriveKey), BTreeMap<WorldTime, MemoPartition>>,
}

impl SustenanceMemo {
    /// Advance the `(entity, drive, reset)` partition to the trail's end and
    /// answer where a read at `t` may resume: `(boundary, integral through it,
    /// windows integrated by the advance)`.
    ///
    /// `cut` is the earliest instant the caller's overlay carries, or `None`
    /// for a read over the committed history alone. `rate` is the caller's own
    /// rate function — see [`MemoPartition::advance`].
    /// type-audit: bare-ok(count: return)
    #[allow(clippy::too_many_arguments)]
    pub fn resume(
        &mut self,
        entity: EntityId,
        drive: DriveKey,
        reset: WorldTime,
        trail: &[(WorldTime, Facet)],
        home: &Facet,
        cut: Option<WorldTime>,
        t: WorldTime,
        rate: &mut dyn FnMut(&Facet, f64) -> f64,
    ) -> (WorldTime, f64, u64) {
        let slots = self.partitions.entry((entity, drive)).or_default();
        let stale = slots.get(&reset).is_none_or(|p| !p.is_current_with(trail));
        if stale {
            slots.insert(reset, MemoPartition::new(reset, trail));
        }
        let partition = slots
            .get_mut(&reset)
            .expect("the partition was just inserted if it was missing");
        let integrated = partition.advance(trail, home, rate);
        let (boundary, acc) = partition.resume_at(cut, t);
        // Evict the oldest reset this drive still holds, never the one just
        // read — see [`MEMO_PARTITIONS_PER_DRIVE`] for why this cannot change
        // an answer.
        while slots.len() > MEMO_PARTITIONS_PER_DRIVE {
            let oldest = *slots
                .keys()
                .find(|k| **k != reset)
                .expect("more partitions than the cap means at least one is not `reset`");
            slots.remove(&oldest);
        }
        (boundary, acc, integrated)
    }
}

/// One entity's held verdicts about the ground it has stood on (The Detent,
/// spec §2.2).
#[derive(Debug, Default)]
struct GroundEntry {
    /// Trail entries already judged; the cursor.
    consumed: usize,
    /// Rooms judged frightening, ascending by FIRST visit, one entry per room.
    frightening: Vec<(WorldTime, Facet)>,
    /// Every room judged, with its verdict.
    judged: BTreeMap<Facet, bool>,
}

/// A read-side index of the frightening verdict per `(entity, room)` — the
/// answer `threat_field(room, niche) × mettle ≥ DANGER_ACT` gives, which over
/// a fixed terrain is a constant per pair. Advanced from the entity's
/// [`Trail`] by a consumed-prefix cursor (the [`MemoPartition`] shape); the
/// predicate is the CALLER's, applied at read, so `LedgerFold::absorb` still
/// sees only facts (The Pawl's `Alarm`-is-not-a-tenant ruling, kept). A
/// pure function of `(ledger prefix, terrain)`; discardable at any instant.
///
/// `frightening` is ordered by first visit so that "the frightening rooms
/// at a past `t`" is a `partition_point` prefix — the lab's waking-instant
/// reads need exactly that (spec §2.2).
///
/// # ONE INDEX PER `(LocaleContext, predator field)` — the same rule the room
/// # memo states, and it is this type's only correctness precondition
///
/// A held verdict is the answer ONE hazard field gave. The predicate is
/// `feels_frightening(threat_field(room, niche, terrain), 0.0, boldness)`,
/// and `threat_field` reads `Terrain::hazards` over the room and its
/// neighbours — so the verdict is a function of the ROOM, the creature, and
/// the terrain's hazard field. The first two are keyed; **the third is
/// supplied by ownership**, exactly as [`crate::ground::GroundHazards`]
/// supplies it and for the same reason. The store that holds this index
/// belongs to one session and one world; its owner builds every terrain the
/// index is advanced through over one `LocaleContext` and one predator field,
/// which do not change after `Session::start`.
///
/// It is NOT one `Terrain` VALUE per store — production builds many
/// (`Session` at five sites, the lab one per tick) — for the reason
/// [`SustenanceMemo`]'s doc gives in full for the temperature field: two
/// `LocaleTerrain` values over the same context and the same field are
/// interchangeable here however their mesh or room memos differ, because both
/// are faithful caches of a pure function rather than parameters of it.
///
/// **A caller that must read under a DIFFERENT hazard field builds a new
/// store.** Discarding this index is unobservable at any instant (it is a
/// pure function of `(ledger prefix, terrain)` and rebuilds on demand), so
/// that is always available and always cheap. Advancing one index under two
/// disagreeing fields is the one thing that makes it wrong, and the failure
/// is SILENT and byte-visible: the second field's read returns the first
/// field's verdict, no room is re-judged, and nothing anywhere objects.
/// `liveness_tests/emitter_scan.rs`'s
/// `a_verdict_index_belongs_to_one_terrain_and_a_second_field_reads_the_first_ones_verdict`
/// demonstrates that aliasing rather than asserting it cannot happen — the
/// same shape as the room memo's own two-terrain test in
/// `tests/suite/the_detent.rs`.
#[derive(Debug, Default)]
pub struct FrighteningGround {
    /// Every entity's held verdicts, keyed by entity.
    by_entity: BTreeMap<EntityId, GroundEntry>,
}

impl FrighteningGround {
    /// Judge the sightings after the cursor, hold the verdicts, move the
    /// cursor to the trail's end. `judge` is asked ONCE per new room.
    /// type-audit: bare-ok(count: return)
    pub fn advance(
        &mut self,
        entity: EntityId,
        trail: &[(WorldTime, Facet)],
        judge: &mut dyn FnMut(&Facet) -> bool,
    ) -> u64 {
        let entry = self.by_entity.entry(entity).or_default();
        let mut judged = 0_u64;
        // The trail is append-only within a store (a `LedgerFold` only ever
        // absorbs) and `consumed` is set only to `trail.len()`, so the cursor
        // can never exceed the trail it indexes; a store discarded is
        // discarded WHOLE, so this index and the trail it indexes are always
        // rebuilt together. A violated invariant panics loudly on the slice
        // below in release rather than being silently clamped.
        debug_assert!(
            entry.consumed <= trail.len(),
            "the trail is append-only and the cursor is only ever set to its length, \
             so a cursor past the trail's end means this index outlived the trail it indexes"
        );
        for (day, room) in &trail[entry.consumed..] {
            if entry.judged.contains_key(room) {
                continue;
            }
            let verdict = judge(room);
            entry.judged.insert(room.clone(), verdict);
            judged += 1;
            if verdict {
                // The trail is ascending by (day, room), so this is the
                // room's first visit and the list stays ascending by first
                // visit without a sort.
                entry.frightening.push((*day, room.clone()));
            }
        }
        entry.consumed = trail.len();
        judged
    }

    /// The frightening rooms first visited at or before `t`, ascending by
    /// first visit.
    pub fn frightening_at(&self, entity: EntityId, t: WorldTime) -> &[(WorldTime, Facet)] {
        match self.by_entity.get(&entity) {
            Some(e) => {
                let n = e.frightening.partition_point(|(d, _)| *d <= t);
                &e.frightening[..n]
            }
            None => &[],
        }
    }

    /// The verdict already held for `room`, if any.
    /// type-audit: bare-ok(flag: return)
    pub fn verdict(&self, entity: EntityId, room: &Facet) -> Option<bool> {
        self.by_entity
            .get(&entity)
            .and_then(|e| e.judged.get(room).copied())
    }

    /// Rooms judged for `entity`, both verdicts.
    /// type-audit: bare-ok(count: return)
    pub fn judged(&self, entity: EntityId) -> usize {
        self.by_entity.get(&entity).map_or(0, |e| e.judged.len())
    }

    /// Every entity's judged-room count summed — M1's entry count.
    /// type-audit: bare-ok(count: return)
    pub fn entries(&self) -> usize {
        self.by_entity.values().map(|e| e.judged.len()).sum()
    }

    /// An estimate of the bytes this index holds, over every entity: every
    /// JUDGED room's key size (`size_of::<Facet>()` plus its `path`'s heap
    /// length) plus one `size_of::<bool>()` verdict, PLUS every FRIGHTENING
    /// entry's `size_of::<(WorldTime, Facet)>()` plus its own room's `path`
    /// heap length (The Detent, spec §4 M1) — the two collections
    /// [`GroundEntry`] actually holds, summed the same way
    /// [`crate::ground::GroundHazards::held_bytes`] sums its own key set. An
    /// ESTIMATE of held data, not an allocator measurement: it counts
    /// neither `BTreeMap`/`Vec` overhead nor any allocator slack.
    /// type-audit: bare-ok(count: return)
    pub fn held_bytes(&self) -> usize {
        let judged: usize = self
            .by_entity
            .values()
            .flat_map(|e| e.judged.keys())
            .map(|room| {
                std::mem::size_of::<Facet>() + room.path.len() + std::mem::size_of::<bool>()
            })
            .sum();
        let frightening: usize = self
            .by_entity
            .values()
            .flat_map(|e| e.frightening.iter())
            .map(|(_, room)| std::mem::size_of::<(WorldTime, Facet)>() + room.path.len())
            .sum();
        judged + frightening
    }
}

/// What the store's reads have cost and what they have seen — the counters two
/// of this campaign's decision rules are asserted through.
///
/// It is a counter beside the state, the same shape (and for the same reason)
/// as `HomeNavCache::searches`: the property being claimed is about how much
/// work a read does and what instants it runs at, and neither is visible in
/// the value a read returns. Nothing in the sim ever reads these back; they
/// hold no ledger truth and are not part of any tenant's state, so they are
/// deliberately OUTSIDE the [`LedgerFold`] states themselves — a counter
/// inside one would put a read count under that tenant's FOLD-equals-SCAN
/// comparison, where two states that folded the identical prefix would differ
/// because one of them had been asked more questions.
#[derive(Debug, Default)]
pub struct ReadWitness {
    /// Per entity, how many integral SEGMENTS its reads have summed, ever.
    segments: BTreeMap<EntityId, u64>,
    /// How many reads have been served, ever.
    reads: u64,
    /// How many UNFILTERED reset lookups have been made — `drive_at` and
    /// `hunger_at` calls, and nothing else. Kept beside the count below
    /// because a rule-1 witness that reported zero offenders out of zero
    /// lookups would be reporting nothing at all.
    reset_lookups: u64,
    /// How many of those ran with a reset of the same entity strictly AFTER
    /// the instant read — spec §3 rule 1's quantity.
    resets_in_the_future: u64,
    /// The first such read, kept for the witness's own evidence line.
    first_reset_in_the_future: Option<(EntityId, WorldTime, WorldTime)>,
    /// How many BELIEF lookups have been made — `believed_water` calls, and
    /// nothing else. The denominator below is a count out of, for the reason
    /// the reset pair above states.
    belief_lookups: u64,
    /// How many of those ran at an instant STRICTLY BEFORE a committed
    /// sighting of the same entity — spec §3 rule 6's quantity, and the exact
    /// condition under which an unfiltered set read would have answered
    /// differently from `believed_water`'s `day <= t` loop.
    beliefs_in_the_past: u64,
    /// The first such read, kept for the witness's own evidence line.
    first_belief_in_the_past: Option<(EntityId, WorldTime, WorldTime)>,
    /// How many HAZARD-MEMORY lookups have been made — `hazard_memory_memo`
    /// calls, and nothing else. Its own denominator, kept separate from the
    /// belief pair because the two functions do NOT share a caller set: three
    /// of `hazard_memory_memo`'s four callers make no paired belief read at
    /// all (`believed_hazard`/`hazard_memory` are their own public entry
    /// points, and the two walk-path calls precede the `WalkState::begin`
    /// that reads belief), so counting one and inferring the other is an
    /// inference dressed as a measurement.
    hazard_lookups: u64,
    /// How many of those ran at an instant STRICTLY BEFORE a committed
    /// sighting of the same entity — spec §3 rule 6's quantity for the
    /// latest-visit map, and what decides `LatestVisit`'s own branch.
    hazards_in_the_past: u64,
    /// The first such read, kept for the witness's own evidence line.
    first_hazard_in_the_past: Option<(EntityId, WorldTime, WorldTime)>,
    /// How many EMITTER SCANS have been built (`build_emitter_scan`) — the
    /// denominator the count below is out of, and the reason it exists: a
    /// world with no scan and a world whose every scan is empty are different
    /// findings that a single number cannot tell apart.
    ///
    /// **What population it counts.** Every scan, over whatever roster the
    /// caller passed — which includes the EMPTY roster the recursion's base
    /// case builds (`emitter_arousal` → `affect_of` passes `band = &[]`, and
    /// the inner `hazard_memory_memo` scans that empty roster). Those are
    /// necessarily emitter-free, so they inflate this denominator without ever
    /// being able to move the numerator. A ratio taken from these two fields
    /// is therefore a floor on "how often a real roster held an emitter", not
    /// that quantity itself; the number to reason about the hazard path with
    /// is [`Self::alarm_replays`] below.
    emitter_scans: u64,
    /// How many of those found at least one member that could ever raise an
    /// alarm — a member whose own terrain threat crosses act somewhere it has
    /// stood, which is the NECESSARY condition for emission, not the decision.
    ///
    /// **It is not zero on a settled world, and an earlier draft of this doc
    /// said it was.** Measured over `common::SIGHT_SEEDS` with a ten-wait
    /// script: seed 42 builds 70 scans and 20 of them find an emitter, and 51
    /// of the 64 seeds have at least one such scan. What makes the seed-42
    /// ledger hash blind to the hazard chain's past-day path is a different
    /// and later gate: no room a creature REMEMBERS visiting ever has an
    /// emitter standing in its one-hop halo at that room's latest-visit day
    /// (the remembered room is either already terrain-frightening, so the
    /// terrain shortcut takes it, or outside `alarm_source_rooms` entirely).
    /// So `emitter_arousal` is never reached from inside the hazard fold —
    /// see [`Self::alarm_replays`], which counts exactly that and is zero on
    /// 62 of the 64 seeds.
    emitter_scans_with_emitters: u64,
    /// How many PAST-DAY AFFECT REPLAYS the hazard fold has performed — the
    /// `emitter_arousal` calls made from inside `hazard_memory_memo`'s
    /// transient loop, at a room's remembered visit day. This is the path
    /// spec §3 rule 5 is about, and the one whose reset semantics the campaign
    /// preserved deliberately; a witness for it that counted `emitter_arousal`
    /// calls in general would be counting `alarm_field_memo`'s present-day
    /// emitter probe too, which seed 42 makes 320 of and which is not this
    /// path at all.
    alarm_replays: u64,
    /// How many ROOMS the frightening-verdict index has judged, ever — the
    /// `judge` calls [`FrighteningGround::advance`] actually made, summed over
    /// every advance on this store.
    ///
    /// It is H6's instrument (spec §4): the campaign's claim is that the
    /// scan's per-tick work is O(NEW sightings), and "new" is exactly what
    /// this counts — a room already judged is skipped inside `advance` and
    /// adds nothing here. A count of `advance` CALLS would not say it (the
    /// number of calls is the roster size either way), and a count of
    /// `hazards()` calls would fold in the room memo's own hit rate, which is
    /// a different mechanism measured by a different number.
    ground_judged: u64,
    /// How many `(WorldTime, Facet)` entries `build_emitter_scan`'s pass 3
    /// has copied out of the trail, ever — spec §3 rule 4's own numerator,
    /// taken at the exact `[..upto].to_vec()` the rule is about.
    emitter_timeline_copied: u64,
    /// Per entity, how many TERRAIN TEMPERATURE samples its sustenance reads
    /// have taken, ever.
    ///
    /// **Today this is the SAME EXPRESSION as [`Self::segments`], passed
    /// twice**, and saying so plainly matters more than the reason the two
    /// fields exist. `sustenance_at` computes `advanced + tail` once and hands
    /// it to [`Self::note_sustenance_read`] as both `segments` and `samples`,
    /// because the rate function samples terrain exactly once per integrated
    /// window. So the cost witness's segments-growth assertion and its
    /// terrain-samples-growth assertion are NOT independent evidence — they
    /// are one measurement asserted twice, and a reader who counts them as two
    /// is over-counting. What the second name buys is only that the
    /// per-read allowance below is stated on the quantity spec §11.7 named
    /// (the terrain sample) rather than on a proxy for it, and that a future
    /// rate function which sampled terrain twice per segment, or memoised a
    /// sample across segments, would separate the two without either
    /// assertion needing to be rewritten.
    terrain_samples: BTreeMap<EntityId, u64>,
    /// Per entity, how many sustenance reads actually INTEGRATED something —
    /// the denominator [`Self::unbounded_reads`] is a count out of. A read
    /// that short-circuits (`t <= last_reset`) is not one of these: it
    /// advances nothing and samples nothing, so bounding its work says
    /// nothing.
    sustenance_reads: BTreeMap<EntityId, u64>,
    /// Per entity, how many of those sampled terrain MORE times than the
    /// ledger grew for it since the previous integrating read of the same
    /// drive — the quantity the cost property is stated on.
    ///
    /// The allowance is `new sightings + overlay entries + 1`: a read may
    /// integrate the segments the ledger has newly determined, the boundaries
    /// this tick's own uncommitted overlay introduces, and the one OPEN
    /// segment that runs from the last boundary to the instant being read.
    /// Anything above that is work proportional to history rather than to what
    /// changed.
    unbounded_reads: BTreeMap<EntityId, u64>,
    /// The first such read as `(entity, samples taken, samples allowed)` — the
    /// evidence the cost witness prints.
    first_unbounded_read: Option<(EntityId, u64, u64)>,
    /// Per `(entity, drive)`, the entity's trail length as of the previous
    /// INTEGRATING read of that drive — the mark the allowance above is
    /// measured from.
    ///
    /// Kept here rather than inside the accumulator it checks, deliberately:
    /// a bound derived from the memo's own bookkeeping would be satisfied by
    /// construction, and the point of the count is that it can fail.
    read_marks: BTreeMap<(EntityId, DriveKey, WorldTime), usize>,
}

impl ReadWitness {
    /// Record that a read for `entity` summed `segments` integral segments.
    /// type-audit: bare-ok(count: segments)
    pub fn note_segments(&mut self, entity: EntityId, segments: u64) {
        *self.segments.entry(entity).or_default() += segments;
        self.reads += 1;
    }

    /// Record one INTEGRATING sustenance read: what it cost, and what the
    /// ledger had newly determined for that entity since the previous read of
    /// the same drive.
    ///
    /// `key` and `reset` together name the integral (see [`DriveKey`]);
    /// `trail_len` is the entity's committed sighting count at this read;
    /// `overlay` is the number of uncommitted in-tick sightings folded on top.
    /// `segments` and `samples` are the work the read actually did.
    /// type-audit: bare-ok(count: trail_len), bare-ok(count: overlay), bare-ok(count: segments), bare-ok(count: samples)
    #[allow(clippy::too_many_arguments)]
    pub fn note_sustenance_read(
        &mut self,
        entity: EntityId,
        key: DriveKey,
        reset: WorldTime,
        trail_len: usize,
        overlay: usize,
        segments: u64,
        samples: u64,
    ) {
        self.note_segments(entity, segments);
        *self.terrain_samples.entry(entity).or_default() += samples;
        *self.sustenance_reads.entry(entity).or_default() += 1;
        let mark = self.read_marks.entry((entity, key, reset)).or_insert(0);
        let grown = trail_len.saturating_sub(*mark) as u64;
        *mark = trail_len;
        let allowed = grown + overlay as u64 + 1;
        if samples > allowed {
            *self.unbounded_reads.entry(entity).or_default() += 1;
            self.first_unbounded_read
                .get_or_insert((entity, samples, allowed));
        }
    }

    /// Record what the UNFILTERED reset lookup answered for a read of `entity`
    /// at `t` — spec §3 rule 1's witness, taken at the two call sites that
    /// actually perform that lookup (`drive_at` and `hunger_at`).
    pub fn note_reset(&mut self, entity: EntityId, t: WorldTime, last_reset: Option<WorldTime>) {
        self.reset_lookups += 1;
        if let Some(reset) = last_reset.filter(|r| *r > t) {
            self.resets_in_the_future += 1;
            self.first_reset_in_the_future
                .get_or_insert((entity, t, reset));
        }
    }

    /// Record what a BELIEF read of `entity` at `t` stood against — spec §3
    /// rule 6's witness, taken on the real path at `believed_water`'s own
    /// call. `latest_sighting` is that entity's last committed `agent-at`
    /// instant, which [`Trail`] holds at O(1).
    ///
    /// **The condition is an UPPER BOUND, conservative in the safe
    /// direction.** It fires when the entity's LATEST sighting of ANY room
    /// lies after `t`; what would actually change a belief answer is a WATER
    /// room whose FIRST visit lies after `t`. Every read that could differ is
    /// therefore counted, and some that could not are counted too. A cheaper
    /// bound was chosen deliberately: the exact test is the tenant's own read,
    /// and running it twice to witness itself would make the counter a copy of
    /// the thing it is checking.
    ///
    /// **This counter survives the branch it decided, and its job changed
    /// when it did.** Before the tenant existed it asked whether a plain
    /// `BTreeSet` fold could serve every reached read; it could not, so
    /// [`LatestVisit`] carries ascending visits and filters. What the
    /// count says NOW is that the filter is LOAD-BEARING — a day filter that
    /// never fired would be indistinguishable from dead code, and the next
    /// reader would have no way to tell.
    pub fn note_belief(
        &mut self,
        entity: EntityId,
        t: WorldTime,
        latest_sighting: Option<WorldTime>,
    ) {
        self.belief_lookups += 1;
        if let Some(latest) = latest_sighting.filter(|d| *d > t) {
            self.beliefs_in_the_past += 1;
            self.first_belief_in_the_past
                .get_or_insert((entity, t, latest));
        }
    }

    /// Record what a HAZARD-MEMORY read of `entity` at `t` stood against —
    /// [`Self::note_belief`]'s twin, taken at `hazard_memory_memo`'s own call
    /// and answering the same question about the same `agent-at` history.
    ///
    /// `hazard_memory_memo` folds a most-recent-visit-per-room map filtered to
    /// `day <= t`, so an unfiltered latest-visit fold would answer differently
    /// under exactly this condition. The count is what spec §3 rule 6's branch
    /// for the `LatestVisit` tenant turns on, and it is measured HERE rather
    /// than inferred from the belief count beside it.
    pub fn note_hazard(
        &mut self,
        entity: EntityId,
        t: WorldTime,
        latest_sighting: Option<WorldTime>,
    ) {
        self.hazard_lookups += 1;
        if let Some(latest) = latest_sighting.filter(|d| *d > t) {
            self.hazards_in_the_past += 1;
            self.first_hazard_in_the_past
                .get_or_insert((entity, t, latest));
        }
    }

    /// Record that an emitter scan was built and how many emitters it found.
    /// type-audit: bare-ok(count: emitters)
    pub fn note_emitter_scan(&mut self, emitters: usize) {
        self.emitter_scans += 1;
        if emitters > 0 {
            self.emitter_scans_with_emitters += 1;
        }
    }

    /// Record one PAST-DAY AFFECT REPLAY — an `emitter_arousal` call made from
    /// inside the hazard fold's transient loop. See the field doc for why this
    /// is counted separately from the alarm field's present-day probe.
    pub fn note_alarm_replay(&mut self) {
        self.alarm_replays += 1;
    }

    /// Record that the frightening-verdict index judged `n` new rooms — the
    /// return of one [`FrighteningGround::advance`]. See the field doc for why
    /// the quantity is judged ROOMS rather than advance calls.
    /// type-audit: bare-ok(count: n)
    pub fn note_ground_judged(&mut self, n: u64) {
        self.ground_judged += n;
    }

    /// Record that `build_emitter_scan`'s pass 3 copied `entries` sightings
    /// out of one emitter's trail — spec §3 rule 4's own instrument.
    /// type-audit: bare-ok(count: entries)
    pub fn note_emitter_timeline_copied(&mut self, entries: u64) {
        self.emitter_timeline_copied += entries;
    }

    /// How many emitter scans have been built — the DENOMINATOR
    /// [`Self::emitter_scans_with_emitters`] is a count out of.
    /// type-audit: bare-ok(count: return)
    pub fn emitter_scans(&self) -> u64 {
        self.emitter_scans
    }

    /// How many of those found at least one possible alarm emitter. Zero means
    /// the hazard fold's transient path was never entered, whatever else the
    /// world did.
    /// type-audit: bare-ok(count: return)
    pub fn emitter_scans_with_emitters(&self) -> u64 {
        self.emitter_scans_with_emitters
    }

    /// How many past-day affect replays the hazard fold has performed — spec
    /// §3 rule 5's own denominator.
    /// type-audit: bare-ok(count: return)
    pub fn alarm_replays(&self) -> u64 {
        self.alarm_replays
    }

    /// How many rooms the frightening-verdict index has judged, ever — H6's
    /// numerator (see the field doc).
    /// type-audit: bare-ok(count: return)
    pub fn ground_judged(&self) -> u64 {
        self.ground_judged
    }

    /// How many `(WorldTime, Facet)` entries the emitter scan's pass 3 has
    /// copied out of a trail, ever — spec §3 rule 4's own numerator.
    /// type-audit: bare-ok(count: return)
    pub fn emitter_timeline_copied(&self) -> u64 {
        self.emitter_timeline_copied
    }

    /// How many hazard-memory lookups have been made — the DENOMINATOR
    /// [`Self::hazards_in_the_past`] is a count out of.
    /// type-audit: bare-ok(count: return)
    pub fn hazard_lookups(&self) -> u64 {
        self.hazard_lookups
    }

    /// How many hazard-memory reads ran at an instant strictly before a
    /// committed sighting of the same entity (spec §3 rule 6, for
    /// `LatestVisit`).
    /// type-audit: bare-ok(count: return)
    pub fn hazards_in_the_past(&self) -> u64 {
        self.hazards_in_the_past
    }

    /// The first such read as `(entity, instant read, the sighting that lies
    /// after it)` — the evidence the rule-6 witness prints.
    pub fn first_hazard_in_the_past(&self) -> Option<(EntityId, WorldTime, WorldTime)> {
        self.first_hazard_in_the_past
    }

    /// How many belief lookups have been made — the DENOMINATOR
    /// [`Self::beliefs_in_the_past`] is a count out of.
    /// type-audit: bare-ok(count: return)
    pub fn belief_lookups(&self) -> u64 {
        self.belief_lookups
    }

    /// How many belief reads ran at an instant strictly before a committed
    /// sighting of the same entity (spec §3 rule 6). Non-zero means
    /// [`LatestVisit`]'s first-visit filter is reached in anger.
    /// type-audit: bare-ok(count: return)
    pub fn beliefs_in_the_past(&self) -> u64 {
        self.beliefs_in_the_past
    }

    /// The first such read as `(entity, instant read, the sighting that lies
    /// after it)` — the evidence the rule-6 witness prints.
    pub fn first_belief_in_the_past(&self) -> Option<(EntityId, WorldTime, WorldTime)> {
        self.first_belief_in_the_past
    }

    /// Every entity's segments summed — the whole store's integration cost.
    /// type-audit: bare-ok(count: return)
    pub fn segments_integrated(&self) -> u64 {
        self.segments.values().sum()
    }

    /// One entity's segments summed.
    /// type-audit: bare-ok(count: return)
    pub fn segments_integrated_for(&self, entity: EntityId) -> u64 {
        self.segments.get(&entity).copied().unwrap_or(0)
    }

    /// Every entity's segments, keyed — for an observer that must sample the
    /// whole roster at each step and only afterwards decide which creature the
    /// property is about (a creature that has never reset accrues segments
    /// with its history by construction, so the cost claim is per creature and
    /// the choice cannot be made in advance).
    /// type-audit: bare-ok(count: return)
    pub fn segments_by_entity(&self) -> &BTreeMap<EntityId, u64> {
        &self.segments
    }

    /// How many reads have been served, ever.
    /// type-audit: bare-ok(count: return)
    pub fn reads(&self) -> u64 {
        self.reads
    }

    /// How many unfiltered reset lookups have been made — the DENOMINATOR
    /// [`Self::resets_in_the_future`] is a count out of.
    /// type-audit: bare-ok(count: return)
    pub fn reset_lookups(&self) -> u64 {
        self.reset_lookups
    }

    /// How many reads ran with a reset of the same entity strictly after the
    /// instant read (spec §3 rule 1). Zero means the unfiltered lookup and a
    /// fold's own filtered one cannot have disagreed on any reached path.
    /// type-audit: bare-ok(count: return)
    pub fn resets_in_the_future(&self) -> u64 {
        self.resets_in_the_future
    }

    /// The first such read as `(entity, instant read, the reset that lies
    /// after it)` — the evidence a `BLOCKED` report would carry.
    pub fn first_reset_in_the_future(&self) -> Option<(EntityId, WorldTime, WorldTime)> {
        self.first_reset_in_the_future
    }

    /// Every entity's terrain-temperature samples, keyed — the per-creature
    /// shape for [`Self::segments_by_entity`]'s reason.
    /// type-audit: bare-ok(count: return)
    pub fn terrain_samples_by_entity(&self) -> &BTreeMap<EntityId, u64> {
        &self.terrain_samples
    }

    /// Every entity's INTEGRATING sustenance reads, keyed — the denominator
    /// [`Self::unbounded_reads_by_entity`] is a count out of.
    /// type-audit: bare-ok(count: return)
    pub fn sustenance_reads_by_entity(&self) -> &BTreeMap<EntityId, u64> {
        &self.sustenance_reads
    }

    /// Every entity's reads that sampled terrain more times than the ledger
    /// grew for it, keyed. Zero for a creature means every one of its reads
    /// cost what changed rather than what accumulated.
    /// type-audit: bare-ok(count: return)
    pub fn unbounded_reads_by_entity(&self) -> &BTreeMap<EntityId, u64> {
        &self.unbounded_reads
    }

    /// The first read that exceeded its allowance, as `(entity, samples taken,
    /// samples allowed)`.
    /// type-audit: bare-ok(count: return)
    pub fn first_unbounded_read(&self) -> Option<(EntityId, u64, u64)> {
        self.first_unbounded_read
    }
}

/// A [`ResidentFolds`] behind interior mutability — what a caller OWNS and
/// threads into `DriveMovements`: the session, a bench, a fixture.
///
/// The store is advanced on READ (spec §2.2) and several of its readers hold
/// only `&self`, which is what rules out a plain `&mut` and what rules out the
/// alternative spec §2.2 refuses outright, a throwaway rebuild per read. This
/// alias exists so the standard library type behind that is spelled in exactly
/// one place in this crate; it is a transparent alias, so a caller may name
/// the underlying type directly wherever that reads better.
pub type OwnedFolds = std::cell::RefCell<ResidentFolds>; // lexicon: std::cell::RefCell is the standard library's interior-mutability type — not a place at all, neither a mesh vertex nor an area

/// The session-owned resident fold store: one [`Folded`] per tenant,
/// advance-on-read.
///
/// [`Trail`], [`ThirstResets`], [`HungerResets`] and [`LatestVisit`] are its
/// tenants — the whole roster spec §2.4 names, with
/// that spec's sixth entry (`Alarm`) folded into [`LatestVisit`]'s second read
/// because none of an alarm scan's state is a function of the ledger alone
/// (see that type's doc). The store knows nothing about what a tenant's state
/// means — a future tenant is a new [`LedgerFold`] impl and a new field, never
/// a new store.
///
/// **Every read advances EVERY tenant** ([`Self::advance`]), not merely the
/// one being asked for. That is what makes [`Self::position`] a single honest
/// number rather than a per-tenant one, and it is what lets a caller take ONE
/// `borrow_mut` guard and pull several tenants out of it: a site that borrowed
/// once per tenant would be a nested `borrow_mut` inside one expression, which
/// panics at runtime rather than failing to compile.
#[derive(Debug, Default)]
pub struct ResidentFolds {
    /// Every entity's `agent-at` trail.
    trail: Folded<Trail>,
    /// Every entity's `drank` days.
    thirst: Folded<ThirstResets>,
    /// Every entity's `eaten` days.
    hunger: Folded<HungerResets>,
    /// Every entity's visits, indexed by room — the hazard fold's latest-visit
    /// map, the alarm scan's room domain, and the belief read's water domain.
    latest_visit: Folded<LatestVisit>,
    /// The sustenance integral's read-side accumulator — not a tenant either,
    /// because it is a function of the temperature field and `home` as well as
    /// the ledger; see [`SustenanceMemo`].
    sustenance_memo: SustenanceMemo,
    /// The per-entity frightening-verdict index — not a tenant either, and
    /// for the same reason: the judge is the caller's threat-field query, not
    /// a function of the ledger alone. It does not join [`Self::advance`]; it
    /// advances at read through its own consumed-prefix cursor, like
    /// [`Self::sustenance_memo`]. See [`FrighteningGround`].
    frightening_ground: FrighteningGround,
    /// What the reads above have cost and seen — not a tenant, and not folded
    /// state; see [`ReadWitness`].
    witness: ReadWitness,
}

impl ResidentFolds {
    /// An empty store, every tenant at position 0.
    pub fn new() -> Self {
        ResidentFolds::default()
    }

    /// Advance EVERY tenant to `ledger`'s end — the currency invariant, and
    /// the one place any tenant's state moves.
    ///
    /// Private, and every accessor below calls it first. A public per-tenant
    /// advance would let one tenant be read current while another sat behind,
    /// which is exactly the divergence [`Self::position`] asserts against.
    fn advance(&mut self, ledger: &Ledger) {
        self.trail.advance_to(ledger);
        self.thirst.advance_to(ledger);
        self.hunger.advance_to(ledger);
        self.latest_visit.advance_to(ledger);
    }

    /// The trail, current with `ledger`.
    pub fn trail(&mut self, ledger: &Ledger) -> &Trail {
        self.advance(ledger);
        self.trail.state()
    }

    /// The `drank` resets, current with `ledger`.
    pub fn sustenance_thirst(&mut self, ledger: &Ledger) -> &Sustenance {
        self.advance(ledger);
        self.thirst.state().get()
    }

    /// The `eaten` resets, current with `ledger`.
    pub fn sustenance_hunger(&mut self, ledger: &Ledger) -> &Sustenance {
        self.advance(ledger);
        self.hunger.state().get()
    }

    /// The trail and the read witness together, from ONE guard — for a caller
    /// that already knows the reset it is integrating from (the tick's own
    /// walk carries a local one, updated as it emits `drank`/`eaten` facts,
    /// which the committed ledger cannot yet know about).
    pub fn trail_and_witness(
        &mut self,
        ledger: &Ledger,
    ) -> (&Trail, &mut SustenanceMemo, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.trail.state(),
            &mut self.sustenance_memo,
            &mut self.witness,
        )
    }

    /// The trail, the `drank` resets and the read witness together, from ONE
    /// guard — see the type doc for why one guard rather than three.
    pub fn trail_and_thirst(
        &mut self,
        ledger: &Ledger,
    ) -> (&Trail, &Sustenance, &mut SustenanceMemo, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.trail.state(),
            self.thirst.state().get(),
            &mut self.sustenance_memo,
            &mut self.witness,
        )
    }

    /// The trail, the `eaten` resets and the read witness together, from ONE
    /// guard.
    pub fn trail_and_hunger(
        &mut self,
        ledger: &Ledger,
    ) -> (&Trail, &Sustenance, &mut SustenanceMemo, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.trail.state(),
            self.hunger.state().get(),
            &mut self.sustenance_memo,
            &mut self.witness,
        )
    }

    /// The room-indexed visit lists and the read witness together, from ONE guard —
    /// see the type doc for why one guard rather than two. [`Trail`] rides
    /// along because the witness's own question ("is `t` before this entity's
    /// last committed sighting?") is answered from the trail's last entry at
    /// O(1), and a second `borrow_mut` to ask it would panic.
    pub fn latest_visit_trail_and_witness(
        &mut self,
        ledger: &Ledger,
    ) -> (&LatestVisit, &Trail, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.latest_visit.state(),
            self.trail.state(),
            &mut self.witness,
        )
    }

    /// The room-indexed visit lists, current with `ledger` — the plain
    /// accessor, for a reader not taking the rule-6 witness. Since The
    /// Detent's Task 9c (`932409875`) that is exactly ONE reader:
    /// `hazard_memory_memo`'s EMITTER path, which builds the per-room latest
    /// visit only once an emitter is in range. The emitter-free path reads
    /// the verdict index instead and calls this not at all. The property
    /// tests this doc used to name do not call it either: they fold their own
    /// [`Folded<LatestVisit>`] against a scan oracle, and the tests that want
    /// a live store's visits take [`Self::latest_visit_and_trail`].
    pub fn latest_visit(&mut self, ledger: &Ledger) -> &LatestVisit {
        self.advance(ledger);
        self.latest_visit.state()
    }

    /// The room-indexed visit lists and the trail together, from ONE guard —
    /// the ALARM scan's read: [`LatestVisit::rooms_at`] gives each roster
    /// member's distinct rooms (the domain the frightening predicate is
    /// applied over) and [`Trail`] gives an emitter's ordered timeline (the
    /// domain its past position is binary-searched in).
    pub fn latest_visit_and_trail(&mut self, ledger: &Ledger) -> (&LatestVisit, &Trail) {
        self.advance(ledger);
        (self.latest_visit.state(), self.trail.state())
    }

    /// The visit lists, the trail, the verdict index and the witness — the
    /// hazard path's read (The Detent).
    pub fn latest_visit_trail_and_ground(
        &mut self,
        ledger: &Ledger,
    ) -> (
        &LatestVisit,
        &Trail,
        &mut FrighteningGround,
        &mut ReadWitness,
    ) {
        self.advance(ledger);
        (
            self.latest_visit.state(),
            self.trail.state(),
            &mut self.frightening_ground,
            &mut self.witness,
        )
    }

    /// The frightening-verdict index alone, read-only.
    pub fn frightening_ground(&self) -> &FrighteningGround {
        &self.frightening_ground
    }

    /// The read witness alone, mutably — for the two hazard-path counters that
    /// record an event rather than a read (`note_emitter_scan`,
    /// `note_alarm_replay`). It still advances every tenant first: a caller
    /// holding this guard is inside a read, and the currency invariant is the
    /// store's, not any one accessor's.
    pub fn witness_mut(&mut self, ledger: &Ledger) -> &mut ReadWitness {
        self.advance(ledger);
        &mut self.witness
    }

    /// What the reads have cost and seen.
    pub fn witness(&self) -> &ReadWitness {
        &self.witness
    }

    /// Every tenant's position — the number of facts absorbed.
    ///
    /// The tenants advance together on every read ([`Self::advance`]), so one
    /// number is the honest answer; the asserts are the witness for that, and
    /// they are `assert!` rather than `debug_assert!` for [`Folded::absorb_at`]'s
    /// own reason — a fold that skipped or repeated a fact is otherwise silent.
    /// type-audit: bare-ok(count: return)
    pub fn position(&self) -> u64 {
        let position = self.trail.position();
        assert_eq!(
            position,
            self.thirst.position(),
            "every tenant advances on every read, so the trail and the thirst resets \
             must stand at the same position"
        );
        assert_eq!(
            position,
            self.hunger.position(),
            "every tenant advances on every read, so the trail and the hunger resets \
             must stand at the same position"
        );
        assert_eq!(
            position,
            self.latest_visit.position(),
            "every tenant advances on every read, so the trail and the room-indexed \
             visit lists must stand at the same position"
        );
        position
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A room address, spelled the way a committed `agent-at` spells one —
    /// copied from `windows/vessel/tests/suite/resident_folds.rs`'s `room()`.
    fn room(face: u8, path: &[u8]) -> Facet {
        Facet {
            face,
            path: path.to_vec(),
        }
    }

    /// A dated `agent-at` fact, spelled the way a committed one is.
    fn sighting(subject: EntityId, day: i64, r: &Facet) -> Fact {
        Fact {
            subject,
            predicate: AGENT_AT.to_string(),
            object: Value::Text(
                crate::thing::room_key(r).expect("the fixture's rooms are within MAX_DEPTH"),
            ),
            place: None,
            day: Some(WorldTime::from_ticks(day * WorldTime::TICKS_PER_STD_DAY)),
            provenance: "the-kerf: held-bytes fixture".to_string(),
        }
    }

    /// The K1 fixture: TWO entities, one of which visits one room THREE times.
    /// The revisits are what make the two identities below differ from each
    /// other — with one visit per room, "every sighting" and "every distinct
    /// room" are the same number and neither assertion would discriminate.
    ///
    /// Also carries the two facts no `agent-at` fold may absorb — a fact of
    /// another predicate, and an UNDATED `agent-at` — so a count that admitted
    /// either would be off by one and visible.
    ///
    /// Returns the facts in commit order alongside the four distinct rooms.
    fn k1_fixture() -> (Vec<Fact>, [Facet; 4]) {
        let a = EntityId::new(1).expect("id");
        let b = EntityId::new(2).expect("id");
        // Path digits are child indices in 0..4 and the face is in 0..6; the
        // `path` lengths deliberately differ (1, 2, 3, 1) so the byte sums
        // below cannot be satisfied by a wrong-but-uniform room size.
        let a1 = room(0, &[1]);
        let a2 = room(0, &[2, 3]);
        let b1 = room(1, &[0, 1, 2]);
        let b2 = room(1, &[3]);
        let mut facts = vec![
            sighting(a, 1, &a1),
            sighting(a, 2, &a2),
            sighting(a, 3, &a1),
            sighting(a, 5, &a1),
            sighting(b, 2, &b1),
            sighting(b, 4, &b2),
        ];
        let mut other = sighting(a, 6, &a1);
        other.predicate = "noise".to_string();
        facts.push(other);
        let mut undated = sighting(b, 6, &b2);
        undated.day = None;
        facts.push(undated);
        (facts, [a1, a2, b1, b2])
    }

    /// Hand-derived: entity 1 posts 4 sightings (rooms A1, A2, A1, A1) and
    /// entity 2 posts 2 (B1, B2) — 6 dated `agent-at` facts over 4 distinct
    /// rooms, plus two facts neither fold may absorb.
    #[test]
    fn the_two_indexes_hold_the_hand_counted_entries_and_bytes() {
        let (facts, [a1, a2, b1, b2]) = k1_fixture();
        let mut trail = Trail::empty();
        let mut visits = LatestVisit::empty();
        for f in &facts {
            trail.absorb(f);
            visits.absorb(f);
        }

        // 6 sightings over 4 distinct rooms.
        assert_eq!(trail.entries(), 6, "every dated `agent-at`, counted once");
        assert_eq!(visits.entries(), 6, "the same population, keyed by room");

        // The `path` heap lengths: 1 + 2 + 1 + 1 over entity 1's four
        // sightings, 3 + 1 over entity 2's two = 9 across the trail; the
        // four distinct room paths sum to 7 for the visit lists.
        let trail_paths: usize = a1.path.len() * 3 + a2.path.len() + b1.path.len() + b2.path.len();
        let room_paths: usize = a1.path.len() + a2.path.len() + b1.path.len() + b2.path.len();
        assert_eq!((trail_paths, room_paths), (9, 7));
        assert_eq!(
            trail.held_bytes(),
            6 * std::mem::size_of::<(WorldTime, Facet)>() + trail_paths
        );
        assert_eq!(
            visits.held_bytes(),
            4 * std::mem::size_of::<Facet>() + room_paths + 6 * std::mem::size_of::<WorldTime>()
        );
    }

    /// Identity 1 of the projection chain: [`Trail`] and [`LatestVisit`] hold
    /// the SAME sighting population, so their entry counts agree on any
    /// ledger. This is the non-vacuity control on K1 — a `held_bytes` summing
    /// the wrong collection would still produce plausible growing numbers, and
    /// a free identity between two independent sums is what catches it.
    #[test]
    fn the_trail_and_the_visit_lists_hold_the_same_sighting_count() {
        let (facts, _) = k1_fixture();
        let mut trail = Trail::empty();
        let mut visits = LatestVisit::empty();
        for (n, f) in facts.iter().enumerate() {
            trail.absorb(f);
            visits.absorb(f);
            assert_eq!(
                trail.entries(),
                visits.entries(),
                "at every prefix, not only at the end (after {} facts)",
                n + 1
            );
        }
        assert_eq!(trail.entries(), 6, "and the shared count is non-trivial");
    }

    #[test]
    fn advance_judges_each_room_once_and_holds_the_verdict() {
        let e = EntityId::new(7).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let trail = vec![
            (t(1), room(0, &[1])),
            (t(2), room(0, &[2])),
            (t(3), room(0, &[1])),
        ];
        let mut g = FrighteningGround::default();
        let mut asked = Vec::new();
        let judged = g.advance(e, &trail, &mut |r| {
            asked.push(r.clone());
            *r == room(0, &[2])
        });
        assert_eq!(judged, 2, "two distinct rooms, the revisit is not re-asked");
        assert_eq!(asked.len(), 2);
        assert_eq!(
            g.advance(e, &trail, &mut |_| panic!("nothing new to judge")),
            0
        );
        assert_eq!(g.frightening_at(e, t(3)), &[(t(2), room(0, &[2]))]);
        assert_eq!(
            g.frightening_at(e, t(1)),
            &[] as &[(WorldTime, Facet)],
            "at t(1) the frightening room was not yet first-visited"
        );
        assert_eq!(g.verdict(e, &room(0, &[1])), Some(false));
        assert_eq!(g.verdict(e, &room(0, &[9])), None);
        assert_eq!((g.judged(e), g.entries()), (2, 2));
    }

    #[test]
    fn a_later_advance_over_a_longer_trail_only_judges_the_new_rooms() {
        let e = EntityId::new(8).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let mut trail = vec![(t(1), room(0, &[1]))];
        let mut g = FrighteningGround::default();
        g.advance(e, &trail, &mut |_| true);
        trail.push((t(2), room(0, &[1])));
        trail.push((t(3), room(0, &[5])));
        let judged = g.advance(e, &trail, &mut |r| *r == room(0, &[5]));
        assert_eq!(judged, 1);
        assert_eq!(g.frightening_at(e, t(3)).len(), 2);
    }

    #[test]
    fn discarding_the_index_and_rebuilding_gives_the_same_prefixes() {
        let e = EntityId::new(9).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let trail: Vec<(WorldTime, Facet)> = (1..=12)
            .map(|i| (t(i), room(0, &[(i % 5) as u8])))
            .collect();
        let judge = |r: &Facet| r.path.last().copied().unwrap_or(0) % 2 == 1;
        let mut whole = FrighteningGround::default();
        whole.advance(e, &trail, &mut {
            let j = judge;
            move |r| j(r)
        });
        for cut in 1..=12 {
            let mut fresh = FrighteningGround::default();
            fresh.advance(e, &trail[..cut], &mut {
                let j = judge;
                move |r| j(r)
            });
            fresh.advance(e, &trail, &mut {
                let j = judge;
                move |r| j(r)
            });
            assert_eq!(
                fresh.frightening_at(e, t(12)),
                whole.frightening_at(e, t(12)),
                "cut at {cut}"
            );
        }
    }

    /// The trail-order claim in `advance` is load-bearing: `Trail` is
    /// ascending by `(day, room)`, so the first occurrence of a room in trail
    /// order IS its earliest day. Room A is visited at day 1 and again at day
    /// 3, interleaved with room B at day 2 — `frightening_at` must carry A at
    /// day 1, not 3.
    #[test]
    fn the_frightening_day_held_is_the_rooms_first_visit_not_a_later_one() {
        let e = EntityId::new(10).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let a = room(0, &[1]);
        let b = room(0, &[2]);
        let trail = vec![(t(1), a.clone()), (t(2), b.clone()), (t(3), a.clone())];
        let mut g = FrighteningGround::default();
        g.advance(e, &trail, &mut |_| true);
        assert_eq!(
            g.frightening_at(e, t(3)),
            &[(t(1), a.clone()), (t(2), b.clone())],
            "A's held day must be its FIRST visit (day 1), not the later revisit (day 3)"
        );
    }
}
