//! The resident fold store: the session-owned, per-entity accumulation of
//! what the ledger already determines (The Pawl, spec §2). The decisions this
//! layer will be ratified under are named in that spec's §8 and are not cited
//! by number here: none is written yet, and a cite to an unratified record is
//! a dangling reference the docs-consistency gate refuses.
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

/// Every entity's DISTINCT visited rooms, each keyed to the FIRST instant it
/// was sighted there — the state `believed_water`'s per-call set build becomes
/// (spec §2.4). A resident permutation index, like [`Trail`]: its contents are
/// a strict subset of what the ledger already says, each fact is absorbed
/// exactly once, and no read rebuilds a set.
///
/// **It holds every visited room, not only the wet ones, and the name is about
/// the job rather than the contents.** Whether a room is water is a fact about
/// TERRAIN, and terrain is not in the ledger: [`LedgerFold::absorb`] is handed
/// a [`Fact`] and nothing else, deliberately (a fold that consulted terrain
/// would stop being a pure function of the ledger prefix, and FOLD-equals-SCAN
/// would be comparing two readings of a mutable world). So the intersection
/// with water-truth happens at READ, in [`Self::water_at`], over a set bounded
/// by the DISTINCT rooms the entity has visited — not by its history's length,
/// which is the whole point.
///
/// **The value is the FIRST sighting instant, and that is what lets a read at
/// a past `t` be exact rather than approximate.** `believed_water` admits a
/// room when the entity has SOME dated `agent-at` there with `day <= t`, which
/// is precisely `first_visit(room) <= t` — the minimum over that room's
/// sightings. A plain `BTreeSet<Facet>` would have had to be rebuilt to the
/// position `t` implies to answer the same question (spec §3 rule 6's
/// fallback, taken: see [`ReadWitness::note_belief`] for the witness that
/// measured it, and the day filter in [`Self::water_at`] for where it is
/// spent).
#[derive(Debug, PartialEq, Default)]
pub struct KnownWater {
    /// Each entity's visited rooms, keyed to the first instant it was sighted
    /// in each.
    rooms: BTreeMap<EntityId, BTreeMap<Facet, WorldTime>>,
}

impl KnownWater {
    /// Every room the entity has stood in, keyed to the FIRST instant it was
    /// sighted there, ascending by room; empty if it was never seen.
    pub fn of(&self, entity: EntityId) -> &BTreeMap<Facet, WorldTime> {
        static EMPTY: BTreeMap<Facet, WorldTime> = BTreeMap::new();
        self.rooms.get(&entity).unwrap_or(&EMPTY)
    }

    /// The WATER rooms the entity had stood in at or before `t`, ascending —
    /// `believed_water`'s candidate set, and the only read this tenant has.
    ///
    /// Both filters are here rather than in `absorb` for the reasons the type
    /// doc gives: `first_visit <= t` because a fold cannot go backwards, and
    /// `is_water` because terrain is not a ledger fact. The cost is O(distinct
    /// rooms visited) — never O(history).
    pub fn water_at(&self, entity: EntityId, t: WorldTime, terrain: &dyn Terrain) -> Vec<Facet> {
        self.of(entity)
            .iter()
            .filter(|(room, first)| **first <= t && is_water(room, terrain))
            .map(|(room, _)| room.clone())
            .collect()
    }
}

impl LedgerFold for KnownWater {
    fn empty() -> Self {
        KnownWater::default()
    }

    fn absorb(&mut self, fact: &Fact) {
        if fact.predicate != AGENT_AT {
            return;
        }
        let Value::Text(s) = &fact.object else {
            return;
        };
        // An UNDATED `agent-at` is not a sighting. `believed_water`'s own loop
        // reads `f.day.map(|d| d <= t).unwrap_or(false)`, so an undated fact
        // is never admitted at any `t`; dropping it here is the same answer,
        // and keeping it would need a sentinel instant that no `t` could
        // exclude.
        let Some(day) = fact.day else {
            return;
        };
        let rooms = self.rooms.entry(fact.subject).or_default();
        rooms
            .entry(room_from_text(s))
            // FIRST sighting, not latest: the read asks whether ANY sighting
            // of the room is at or before `t`, which the minimum answers and
            // the maximum would not.
            .and_modify(|first| {
                if day < *first {
                    *first = day;
                }
            })
            .or_insert(day);
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
/// **Two reads, one state.** [`Self::latest_at`] serves the hazard fold's
/// most-recent-visit-per-room rule; [`Self::rooms_at`] serves the ALARM scan,
/// which needs only the DISTINCT rooms a roster member has stood in at or
/// before `t` so it can ask terrain whether each frightens that member. The
/// alarm half is the tenant spec §2.4 lists as `Alarm`, and it holds no state
/// of its own on purpose: everything `build_emitter_scan` accumulates —
/// which members can ever emit, the halo of rooms their alarms could reach —
/// is a function of TERRAIN and the member's own threat niche as well as the
/// ledger, so none of it can live inside a [`LedgerFold`] (the same argument
/// [`KnownWater`]'s doc makes for `is_water`). What the ledger determines is
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

    /// The DISTINCT rooms the entity had stood in at or before `t`, ascending
    /// — the alarm scan's domain (see the type doc's "two reads" paragraph).
    ///
    /// The FIRST visit decides membership, so the test is against the list's
    /// first element rather than a `partition_point`: a room is in the set as
    /// soon as any sighting of it is at or before `t`.
    pub fn rooms_at(&self, entity: EntityId, t: WorldTime) -> Vec<Facet> {
        self.of(entity)
            .iter()
            .filter(|(_, days)| days.first().is_some_and(|first| *first <= t))
            .map(|(room, _)| room.clone())
            .collect()
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
    /// belief pair because the two functions do NOT share a caller set: four
    /// of `hazard_memory_memo`'s five callers make no paired belief read at
    /// all (`believed_hazard`/`believed_hazard_memo`/`hazard_memory` are their
    /// own public entry points, and the two walk-path calls precede the
    /// `WalkState::begin` that reads belief), so counting one and inferring
    /// the other is an inference dressed as a measurement.
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
}

impl ReadWitness {
    /// Record that a read for `entity` summed `segments` integral segments.
    /// type-audit: bare-ok(count: segments)
    pub fn note_segments(&mut self, entity: EntityId, segments: u64) {
        *self.segments.entry(entity).or_default() += segments;
        self.reads += 1;
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
    /// [`KnownWater`] carries a first-visit instant and filters. What the
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
    /// [`KnownWater`]'s first-visit filter is reached in anger.
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
/// [`Trail`], [`ThirstResets`], [`HungerResets`], [`KnownWater`] and
/// [`LatestVisit`] are its tenants — the whole roster spec §2.4 names, with
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
    /// Every entity's distinct visited rooms and when each was first seen.
    known_water: Folded<KnownWater>,
    /// Every entity's visits, indexed by room — the hazard fold's latest-visit
    /// map and the alarm scan's room domain.
    latest_visit: Folded<LatestVisit>,
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
        self.known_water.advance_to(ledger);
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
    pub fn trail_and_witness(&mut self, ledger: &Ledger) -> (&Trail, &mut ReadWitness) {
        self.advance(ledger);
        (self.trail.state(), &mut self.witness)
    }

    /// The trail, the `drank` resets and the read witness together, from ONE
    /// guard — see the type doc for why one guard rather than three.
    pub fn trail_and_thirst(&mut self, ledger: &Ledger) -> (&Trail, &Sustenance, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.trail.state(),
            self.thirst.state().get(),
            &mut self.witness,
        )
    }

    /// The trail, the `eaten` resets and the read witness together, from ONE
    /// guard.
    pub fn trail_and_hunger(&mut self, ledger: &Ledger) -> (&Trail, &Sustenance, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.trail.state(),
            self.hunger.state().get(),
            &mut self.witness,
        )
    }

    /// The visited-room index, current with `ledger` — the plain accessor,
    /// for a reader that is not taking the rule-6 witness (the property tests,
    /// and any future consumer of the same index).
    pub fn known_water(&mut self, ledger: &Ledger) -> &KnownWater {
        self.advance(ledger);
        self.known_water.state()
    }

    /// The visited-room index and the read witness together, from ONE guard —
    /// see the type doc for why one guard rather than two. [`Trail`] rides
    /// along because the witness's own question ("is `t` before this entity's
    /// last committed sighting?") is answered from the trail's last entry at
    /// O(1), and a second `borrow_mut` to ask it would panic.
    pub fn known_water_and_trail(
        &mut self,
        ledger: &Ledger,
    ) -> (&KnownWater, &Trail, &mut ReadWitness) {
        self.advance(ledger);
        (
            self.known_water.state(),
            self.trail.state(),
            &mut self.witness,
        )
    }

    /// The room-indexed visit lists, current with `ledger` — the plain
    /// accessor, for a reader not taking the rule-6 witness (the property
    /// tests, and the alarm scan, which asks only for rooms).
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

    /// The room-indexed visit lists, the trail and the read witness together,
    /// from ONE guard — `hazard_memory_memo`'s own read. [`Trail`] rides along
    /// for [`Self::known_water_and_trail`]'s reason: the rule-6 witness asks
    /// whether `t` lies before this entity's last committed sighting, which is
    /// the trail's last entry at O(1), and a second `borrow_mut` to ask it
    /// would panic.
    pub fn latest_visit_and_witness(
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
            self.known_water.position(),
            "every tenant advances on every read, so the trail and the visited-room \
             index must stand at the same position"
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
