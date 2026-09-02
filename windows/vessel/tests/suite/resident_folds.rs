//! The Pawl, Task 2: the resident fold store's first tenant, [`Trail`], and
//! the store that owns it.
//!
//! Three things are pinned here, and the third is the one the primitive's
//! module doc says every tenant owes:
//!
//! 1. **The rule-2 witness** (spec §3 rule 2): whether an entity's `agent-at`
//!    facts ever commit out of day order on the real seed-42 session. The
//!    test passes either way — it PRINTS the verdict and the campaign ledger
//!    records which branch was taken — because `Trail::absorb` inserts at the
//!    sorted position and is therefore correct under both.
//! 2. **FOLD equals SCAN**, against a VERBATIM COPY of `agent_sightings`'s
//!    body ([`scan_oracle`]) rather than against the production function: a
//!    later task deletes that function, and an oracle sharing code with the
//!    thing under test is not an oracle. The fold half is driven through
//!    `Folded::absorb_at` one fact at a time — never `advance_to` — because
//!    `Folded::rebuild` is itself implemented by calling `advance_to`, so
//!    comparing the two would compare `advance_to` against itself (the
//!    primitive's own vacuous-comparison finding, `kernel/src/fold.rs`).
//! 3. **Both chaos schedules** (every position, and every third): discarding
//!    the accumulated state and rebuilding it from the ledger is unobservable.
//!    Both are needed — the every-position schedule gives no signal on
//!    `absorb`'s purity, because a bug confined to it cancels when the rebuild
//!    happens immediately after every single absorb.

use crate::common;
use hornvale_kernel::fold::Folded;
use hornvale_kernel::{
    ConceptRegistry, EntityId, Facet, FacetId, Fact, Ledger, Value, WorldTime, test_lineage,
};
use hornvale_species::ThermalStrategy;
use hornvale_vessel::liveness::{
    DriveParams, HomeNavCache, PrimaryAfraidMemo, SUSTENANCE, Terrain, affect_of_memo_occupied,
    sustenance_at,
};
use hornvale_vessel::resident::{KnownWater, LatestVisit, ReadWitness, ResidentFolds, Trail};
use hornvale_vessel::{PossessOpts, Session};

/// `agent-at`'s exact on-disk spelling, written as a literal rather than
/// imported, so this test still fails if the constant is ever repointed
/// (`liveness.rs`'s own spelling test makes the same argument).
const AGENT_AT: &str = "agent-at";

// ---------------------------------------------------------------------------
// Step 1: the rule-2 witness.
// ---------------------------------------------------------------------------

/// Spec §3 rule 2, executed on the real thing: does any entity's `agent-at`
/// trail arrive out of day order in COMMIT order?
///
/// The verdict is printed per entity and the test passes either way. That is
/// deliberate: `Trail::absorb` inserts at the sorted position, which is
/// correct whether the answer is "always appends" or "sometimes inserts", so
/// nothing here gates the tenant. What the witness buys is the ledger entry —
/// whether the insert is ever not an append is a fact about the walk, and the
/// campaign records it rather than assuming it.
#[test]
fn rule_two_witness_agent_at_commit_order_versus_day_order() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..40 {
        session.handle("wait");
    }

    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");

    // Commit-order trails, per subject, of `agent-at` facts only.
    let mut by_entity: std::collections::BTreeMap<EntityId, Vec<WorldTime>> =
        std::collections::BTreeMap::new();
    for f in ledger.iter() {
        if f.predicate == AGENT_AT
            && let Some(d) = f.day
        {
            by_entity.entry(f.subject).or_default().push(d);
        }
    }

    let mut inverted_entities = 0usize;
    let mut inversions = 0usize;
    println!("--- rule 2 witness: agent-at commit order vs day order (seed 42, 40 waits) ---");
    for (e, days) in &by_entity {
        let n = days.windows(2).filter(|w| w[1] < w[0]).count();
        inversions += n;
        if n > 0 {
            inverted_entities += 1;
        }
        println!(
            "entity {:?}: {} agent-at facts, {} out-of-order pairs -> {}",
            e,
            days.len(),
            n,
            if n == 0 { "APPEND" } else { "INSERT" }
        );
    }
    println!(
        "--- verdict: {} of {} entities commit agent-at out of day order ({} inverted pairs \
         total) => on this script Trail's insert {} ever a non-append ---",
        inverted_entities,
        by_entity.len(),
        inversions,
        if inversions == 0 { "is NOT" } else { "IS" }
    );

    assert!(
        !by_entity.is_empty(),
        "the 40-wait seed-42 script must commit at least one agent-at fact, or this \
         witness measured nothing"
    );
}

// ---------------------------------------------------------------------------
// Steps 2 and 3: the oracle, the hand-built ledger, and the properties.
// ---------------------------------------------------------------------------

/// A VERBATIM COPY of `liveness.rs`'s `agent_sightings` body — the SCAN half
/// of FOLD equals SCAN.
///
/// Copied rather than called on purpose. A later task of this campaign
/// deletes the production function, and an oracle that shares code with the
/// thing under test cannot falsify it: a bug in the shared half is applied
/// identically to both sides and cancels.
fn scan_oracle(ledger: &Ledger, entity: EntityId, upto: f64) -> Vec<(f64, Facet)> {
    let mut v: Vec<(f64, Facet)> = ledger
        .facts_of(entity, AGENT_AT)
        .filter_map(|f| {
            let d = f.day?.as_std_days();
            if d > upto {
                return None;
            }
            match &f.object {
                Value::Text(s) => Some((d, room_from_text_copy(s))),
                _ => None,
            }
        })
        .collect();
    v.sort_by(|a, b| a.0.total_cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    v
}

/// The oracle's own decode, copied for the same reason [`scan_oracle`] is.
fn room_from_text_copy(s: &str) -> Facet {
    let id: u64 = s
        .parse()
        .unwrap_or_else(|_| panic!("agent-at text '{s}' is not a decimal FacetId"));
    FacetId(id)
        .unpack()
        .unwrap_or_else(|_| panic!("agent-at FacetId {id} does not unpack to a valid Facet"))
}

/// A room address, spelled the way a committed `agent-at` spells one.
fn room(face: u8, path: &[u8]) -> Facet {
    Facet {
        face,
        path: path.to_vec(),
    }
}

/// The committed spelling of `r` — the same encoding `liveness.rs`'s
/// `room_to_text` writes: the packed `FacetId` as decimal text.
fn room_text(r: &Facet) -> String {
    r.pack().expect("a test room packs").0.to_string()
}

/// The fixture's commit script as `(subject index, day ticks, face, path)`,
/// deliberately NOT in day order.
///
/// It exercises the two things the fold's ordering must get right, neither of
/// which a straight append would:
///
/// - **out-of-day-order commits** (entity `a` arrives at day 7 before day 3),
///   so an appending fold would produce a different `Vec` than the sort does;
/// - **same-day ties on different rooms**, committed in an order that is NOT
///   room order, so the `(day, room)` tie-break is exercised rather than
///   assumed — a tie-break that did nothing would be visible.
///
/// Two entities, interleaved, so a fold leaking one entity's facts into
/// another's trail is caught too.
const SCRIPT: &[(usize, i64, u8, &[u8])] = &[
    (0, 700_000, 0, &[0]),
    (1, 100_000, 2, &[3, 1]),
    (0, 300_000, 0, &[1]),
    (0, 300_000, 2, &[3, 1]), // same-day tie, out of room order
    (0, 300_000, 0, &[0]),    // same-day tie, the smallest room committed last
    (1, 900_000, 1, &[2]),
    (1, 100_000, 0, &[1]), // same-day tie for `b`, out of commit/day order
    (0, 1_500_000, 1, &[2]),
    (1, 400_000, 0, &[0]),
];

/// A ledger holding the first `n` entries of [`SCRIPT`], plus — once the whole
/// script is in — a fact of another predicate and an UNDATED `agent-at`, both
/// of which the fold must ignore.
///
/// Prefixes are built by replaying the script rather than by truncating a
/// ledger, because `Ledger` is append-only and has no truncation door. Entity
/// ids are derived from a fixed lineage, so the same two ids come back from
/// every call.
fn hand_built_upto(n: usize) -> (Ledger, EntityId, EntityId) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    reg.register_predicate("noise", false, "pos").unwrap();
    let mut l = Ledger::default();
    let ids = [
        l.mint_entity(test_lineage(0)),
        l.mint_entity(test_lineage(1)),
    ];

    for (who, ticks, face, path) in SCRIPT.iter().take(n) {
        l.commit(
            Fact {
                subject: ids[*who],
                predicate: AGENT_AT.to_string(),
                object: Value::Text(room_text(&room(*face, path))),
                place: None,
                day: Some(WorldTime::from_ticks(*ticks)),
                provenance: "t".to_string(),
            },
            &reg,
        )
        .unwrap();
    }
    if n >= SCRIPT.len() {
        // A fact of another predicate, and an UNDATED `agent-at`: neither may
        // reach a trail.
        l.commit(
            Fact {
                subject: ids[0],
                predicate: "noise".to_string(),
                object: Value::Text(room_text(&room(0, &[0]))),
                place: None,
                day: Some(WorldTime::from_ticks(200_000)),
                provenance: "t".to_string(),
            },
            &reg,
        )
        .unwrap();
        l.commit(
            Fact {
                subject: ids[1],
                predicate: AGENT_AT.to_string(),
                object: Value::Text(room_text(&room(0, &[0]))),
                place: None,
                day: None,
                provenance: "t".to_string(),
            },
            &reg,
        )
        .unwrap();
    }

    (l, ids[0], ids[1])
}

/// The whole fixture.
fn hand_built() -> (Ledger, EntityId, EntityId) {
    hand_built_upto(SCRIPT.len())
}

/// The fold half, reached through `absorb_at` one fact at a time — the path
/// independent of `advance_to`.
fn fold_one_by_one(ledger: &Ledger) -> Folded<Trail> {
    let mut f: Folded<Trail> = Folded::new();
    for (i, fact) in ledger.iter().enumerate() {
        f.absorb_at(i as u64, fact);
    }
    f
}

/// The trail lifted to the oracle's `(f64 day, room)` shape, so the two can be
/// compared directly.
fn as_days(trail: &[(WorldTime, Facet)]) -> Vec<(f64, Facet)> {
    trail
        .iter()
        .map(|(d, r)| (d.as_std_days(), r.clone()))
        .collect()
}

/// Guards every ordering assertion below from being vacuous: if the fixture
/// ever degraded into one whose commits are already in `(day, room)` order, an
/// appending fold would pass all of them.
#[test]
fn the_hand_built_ledger_is_not_already_in_sorted_order() {
    let (l, a, b) = hand_built();
    for e in [a, b] {
        let commit_order: Vec<f64> = l
            .facts_of(e, AGENT_AT)
            .filter_map(|f| f.day.map(|d| d.as_std_days()))
            .collect();
        let mut sorted = commit_order.clone();
        sorted.sort_by(f64::total_cmp);
        assert_ne!(
            commit_order, sorted,
            "the fixture must commit {e:?}'s agent-at facts out of day order, or every \
             ordering assertion in this file is vacuous"
        );
    }
    // And the tie-break itself: same-day facts on DIFFERENT rooms, committed
    // in an order that is not already room order.
    let ties: Vec<Facet> = l
        .facts_of(a, AGENT_AT)
        .filter(|f| f.day == Some(WorldTime::from_ticks(300_000)))
        .map(|f| match &f.object {
            Value::Text(s) => room_from_text_copy(s),
            other => panic!("an agent-at object is always text, got {other:?}"),
        })
        .collect();
    let mut ties_sorted = ties.clone();
    ties_sorted.sort();
    assert!(
        ties.len() >= 2 && ties != ties_sorted,
        "the fixture must commit at least two same-day agent-at facts for `a`, NOT already in \
         room order, or the (day, room) tie-break is never exercised: {ties:?}"
    );
}

#[test]
fn folding_one_fact_at_a_time_equals_the_scan_oracle() {
    let (l, a, b) = hand_built();
    let folded = fold_one_by_one(&l);

    for e in [a, b] {
        let scanned = scan_oracle(&l, e, f64::INFINITY);
        assert_eq!(
            as_days(folded.state().of(e)),
            scanned,
            "the fold's trail for {e:?} must equal the scan oracle's whole-history sightings"
        );
    }
}

#[test]
fn every_prefix_length_equals_the_oracle_truncated_at_that_day() {
    let (l, a, b) = hand_built();
    let folded = fold_one_by_one(&l);

    for e in [a, b] {
        let whole = folded.state().of(e);
        // Every day present in the trail, plus one strictly before the first
        // and one strictly after the last, so the empty and full prefixes are
        // both covered.
        let mut probes: Vec<WorldTime> = whole.iter().map(|(d, _)| *d).collect();
        probes.push(WorldTime::from_ticks(0));
        probes.push(WorldTime::from_ticks(9_999_999));
        for t in probes {
            let n = folded.state().prefix_len(e, t);
            let scanned = scan_oracle(&l, e, t.as_std_days());
            assert_eq!(
                n,
                scanned.len(),
                "prefix_len for {e:?} at {t:?} must equal the oracle's count at that day"
            );
            assert_eq!(
                as_days(&whole[..n]),
                scanned,
                "the prefix for {e:?} at {t:?} must equal the oracle's sightings at that day"
            );
        }
    }
}

#[test]
fn an_entity_that_never_moved_has_an_empty_trail() {
    let (l, _a, _b) = hand_built();
    let folded = fold_one_by_one(&l);
    // An id no fact in this ledger names as a subject.
    let stranger = EntityId::new(9_999).expect("9999 is non-zero");
    assert!(folded.state().of(stranger).is_empty());
    assert_eq!(
        folded
            .state()
            .prefix_len(stranger, WorldTime::from_ticks(1)),
        0
    );
}

#[test]
fn discarding_the_trail_at_every_position_is_unobservable() {
    let (l, _a, _b) = hand_built();
    let resident = fold_one_by_one(&l);

    let mut chaotic: Folded<Trail> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        chaotic = Folded::rebuild_upto(&l, chaotic.position());
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}

#[test]
fn discarding_the_trail_at_every_third_position_is_unobservable() {
    let (l, _a, _b) = hand_built();
    let resident = fold_one_by_one(&l);

    let mut chaotic: Folded<Trail> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        if i % 3 == 0 {
            chaotic = Folded::rebuild_upto(&l, chaotic.position());
        }
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}

// ---------------------------------------------------------------------------
// The store itself, under the same two schedules.
// ---------------------------------------------------------------------------

/// The store's chaos schedules, both of them: a resident store read against a
/// GROWING ledger, versus one thrown away and rebuilt from scratch at every
/// position (`every == 1`) or at every third (`every == 3`). The store is
/// discardable at any instant, so the two must agree at every step.
fn store_discard_schedule(every: usize) {
    let (full, a, b) = hand_built();
    let mut resident = ResidentFolds::new();
    let mut chaotic = ResidentFolds::new();

    for i in 0..=SCRIPT.len() {
        let (prefix, _, _) = hand_built_upto(i);
        let _ = resident.trail(&prefix);
        if i % every == 0 {
            chaotic = ResidentFolds::new();
        }
        let _ = chaotic.trail(&prefix);
        assert_eq!(
            chaotic.position(),
            resident.position(),
            "position diverged at prefix {i} under a discard-every-{every} schedule"
        );
        for e in [a, b] {
            assert_eq!(
                chaotic.trail(&prefix).of(e),
                resident.trail(&prefix).of(e),
                "{e:?}'s trail diverged at prefix {i} under a discard-every-{every} schedule"
            );
            assert_eq!(
                chaotic.known_water(&prefix).of(e),
                resident.known_water(&prefix).of(e),
                "{e:?}'s visited-room index diverged at prefix {i} under a \
                 discard-every-{every} schedule"
            );
        }
    }

    // And after the whole script (plus its two ignorable trailing facts), both
    // agree with a from-scratch fold of the full ledger.
    let scan = fold_one_by_one(&full);
    let _ = resident.trail(&full);
    let _ = chaotic.trail(&full);
    let water_scan = fold_known_water_one_by_one(&full);
    for e in [a, b] {
        assert_eq!(resident.trail(&full).of(e), scan.state().of(e));
        assert_eq!(chaotic.trail(&full).of(e), scan.state().of(e));
        assert_eq!(resident.known_water(&full).of(e), water_scan.state().of(e));
        assert_eq!(chaotic.known_water(&full).of(e), water_scan.state().of(e));
    }
}

#[test]
fn discarding_the_store_at_every_position_is_unobservable() {
    store_discard_schedule(1);
}

#[test]
fn discarding_the_store_at_every_third_position_is_unobservable() {
    store_discard_schedule(3);
}

#[test]
fn the_store_advances_on_read_and_a_second_read_absorbs_nothing() {
    let (l, _a, _b) = hand_built();
    let mut store = ResidentFolds::new();
    assert_eq!(store.position(), 0, "a fresh store has absorbed nothing");

    let _ = store.trail(&l);
    assert_eq!(
        store.position(),
        l.len() as u64,
        "one read must advance the store to the ledger's end"
    );

    let _ = store.trail(&l);
    assert_eq!(
        store.position(),
        l.len() as u64,
        "a second read must absorb nothing — advance-on-read is idempotent in position"
    );
}

/// Step 7: the same idempotence against a ledger a REAL tick has grown.
///
/// Nothing in production reads the store yet (this task threads it and stops),
/// so this drives it from outside — but it is the same `ResidentFolds` type
/// the session and both benches now own, against the session's own committed
/// ledger.
#[test]
fn the_store_is_current_with_a_real_sessions_ledger() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..5 {
        session.handle("wait");
    }
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");

    let mut store = ResidentFolds::new();
    let _ = store.trail(&ledger);
    assert_eq!(
        store.position() as usize,
        session.committed_fact_count(),
        "after advancing to the session's ledger the store's position is the session's \
         committed fact count"
    );
    let before = store.position();
    let _ = store.trail(&ledger);
    assert_eq!(
        store.position(),
        before,
        "a second read of the same ledger must not move the position"
    );
}

// ---------------------------------------------------------------------------
// The Pawl, Task 3. Below this line: the sustenance tenants, the read that
// replaced `agent_sightings` + `integrate_thirst`, and the two witnesses
// (spec §3 rule 1, and the cost property).
// ---------------------------------------------------------------------------

/// Spec §3 rule 1, executed on the real thing: does any `drive_at`/`hunger_at`
/// call ever run with `t` EARLIER than a reset of the same entity?
///
/// Today's reset lookup in those two functions is unfiltered — the maximum
/// over every committed `drank`/`eaten`, with no `<= t` — while a fold at the
/// position `t` implies would see only resets at or before it (the Tailrace's
/// trap 5). The two answers can differ only on a call whose `t` lies before
/// some reset, so that is what is counted, ON THE PRODUCTION PATH:
/// `ReadWitness` records every unfiltered lookup and how many of them found a
/// reset in the future, so this cannot miss a call by mis-modelling which ones
/// happen. **The denominator is asserted too** — a witness reporting zero
/// offenders out of zero lookups reports nothing, and an earlier draft of this
/// test did exactly that (see below).
///
/// # The finding this witness produced first, and the finding that OVERTURNED
/// its denominator
///
/// **This test used to assert that the seed-42 possession never calls
/// `drive_at` or `hunger_at` at all, and that assertion was measuring the
/// instrument rather than the sim.** The reasoning behind it was sound as far
/// as it went: `affect_of_memo_occupied` is reached by `Session::snapshot` and
/// `Session::needs` once per CO-LOCATED creature, seed 42's flagship stands
/// alone (`possess --seed 42` with a `needs` in the script answers "No one
/// else is here to read"), and every read the tick's nine walking creatures
/// make goes through `decide_step`, which carries the walk's own reset local
/// and makes no unfiltered lookup.
///
/// What it missed is the tick's OTHER caller. `DriveMovements::step_with_
/// occupancy` builds the per-tick alarm field before anyone moves, and
/// `alarm_field_memo` probes every roster member that clears the cheap
/// terrain gate through `emitter_arousal` → `affect_of` → `drive_at`/
/// `hunger_at`. Those calls were always happening; until The Pawl's stage 2
/// they landed on the THROWAWAY store `affect_of_memo` built per call, so
/// their witness died with it and the session's counter read a true zero for
/// a false reason. Threading the session's own store down that chain (this
/// stage) makes them visible: **320 unfiltered reset lookups on the same
/// 40-wait script that reported none.**
///
/// The denominator lesson is this campaign's own, for the fourth time: a zero
/// is only as good as the path the counter sits on. The session's zero is
/// asserted now on the quantity rule 1 is actually about — how many of those
/// lookups found a reset in the read's future — with the lookup count itself
/// asserted non-zero beneath it.
///
/// The rule is then answered on the shape that does call them, which is the
/// same call the lab's `run_simulation` makes every tick for every creature
/// and the one `Session::snapshot` would make if anyone were beside the
/// player: `affect_of_memo_occupied` over the session's own committed ledger
/// and its own derived bodies, at the session's current day, sharing one
/// store. Terrain is a stub, and may be: the reset lookup rule 1 is about
/// reads nothing but the ledger and `t`.
///
/// **A non-zero count is a STOP, not a failure to fix here** — it makes the
/// unfiltered lookup a live semantic choice (a checkpoint fold's filtered rule
/// against today's code) rather than a migration detail, and the campaign
/// ledgers it as a question before any further tenant lands. The decision that
/// would settle it is reserved in the campaign's block and is deliberately not
/// cited by number: nothing is written, and a cite to an unratified record is a
/// dangling reference the docs-consistency gate refuses.
#[test]
fn rule_one_witness_no_read_runs_before_a_reset_of_the_same_entity() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..40 {
        session.handle("wait");
        // `drive_at`'s other production caller, once per committed turn.
        let _ = session.snapshot().expect("seed 42's session snapshots");
    }

    println!("--- rule 1 witness: unfiltered reset lookups (seed 42, 40 waits + snapshots) ---");
    println!(
        "the SESSION itself: {} facts absorbed, {} unfiltered reset lookups, {} with a \
         reset in the future",
        session.resident_position(),
        session.resident_reset_lookups(),
        session.resident_resets_in_the_future()
    );
    assert!(
        session.committed_fact_count() > 0,
        "the 40-wait seed-42 script must commit facts, or this witness measured nothing"
    );
    if let Some((entity, t, reset)) = session.resident_first_reset_in_the_future() {
        println!(
            "first offender in the session: entity {entity:?} read at {t:?} with a reset \
             at {reset:?}"
        );
    }
    assert!(
        session.resident_reset_lookups() > 0,
        "the seed-42 session must reach `drive_at`/`hunger_at` through the tick's own \
         `alarm_field_memo` emitter probe (see this test's doc), or its verdict of zero \
         offenders is zero out of zero and says nothing"
    );
    assert_eq!(
        session.resident_resets_in_the_future(),
        0,
        "spec §3 rule 1, on the SESSION itself: a `drive_at`/`hunger_at` call ran with a \
         reset in its future, so the unfiltered lookup and a filtered one are NOT \
         byte-equivalent on the walk's own path -- STOP, and ledger it"
    );

    // The shape that DOES call them: `affect_of_memo_occupied` per body, at
    // the present instant, over the session's own ledger, sharing one store.
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let bodies: Vec<hornvale_vessel::body::Body> = session.bodies().to_vec();
    let terrain = RippleTerrain;
    let now = session.day();

    let present = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        for npc in &bodies {
            let _ = affect_of_memo_occupied(
                &ledger,
                npc,
                &bodies,
                now,
                &terrain,
                &mut afraid,
                None,
                &mut mesh,
                &mut nav,
                &present,
            );
        }
    }
    let lookups = present.borrow().witness().reset_lookups();
    let offenders = present.borrow().witness().resets_in_the_future();
    println!(
        "the REACHED read shape (`affect_of_memo_occupied` at the present day, {} bodies): \
         {lookups} unfiltered reset lookups, {offenders} with a reset in the future",
        bodies.len()
    );
    if let Some((entity, t, reset)) = present.borrow().witness().first_reset_in_the_future() {
        println!("first offender: entity {entity:?} read at {t:?} with a reset at {reset:?}");
    }
    println!(
        "--- verdict: on every REACHED path, today's UNFILTERED reset lookup {} ---",
        if offenders == 0 {
            "is byte-equivalent to a filtered one"
        } else {
            "DIFFERS from a filtered one -- spec §3 rule 1 has FIRED"
        }
    );

    // Informational, and deliberately NOT asserted on: the PAST-instant sweep.
    // `hazard_memory_memo` -> `frightened_at` -> `alarm_at` -> `alarm_field`
    // -> `emitter_arousal` -> `affect_of` reads a creature's affect at a past
    // visit day, which is where an unfiltered reset lookup would differ. Stage
    // 2 has now threaded the store through that chain, so a nested read is
    // counted where it used to die with a throwaway store -- but the chain is
    // still gated behind a non-empty emitter scan, and seed 42 is
    // emitter-free, so the PAST-DAY reads counted below are the sweep's own
    // outer calls rather than a replay the sim performs. The campaign's
    // standing ruling is that this path keeps today's UNFILTERED semantics
    // deliberately (see `emitter_arousal`'s doc), so a non-zero here is a
    // recorded fact about the sim, not a stop.
    let past = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        for npc in &bodies {
            let days: Vec<WorldTime> = ledger
                .facts_of(npc.entity, AGENT_AT)
                .filter_map(|f| f.day)
                .collect();
            for day in days.into_iter().step_by(37) {
                let _ = affect_of_memo_occupied(
                    &ledger,
                    npc,
                    &bodies,
                    day,
                    &terrain,
                    &mut afraid,
                    None,
                    &mut mesh,
                    &mut nav,
                    &past,
                );
            }
        }
    }
    println!(
        "UNREACHED today, recorded for stage 2 -- the same read at PAST visit days: \
         {} lookups, {} with a reset in the future",
        past.borrow().witness().reset_lookups(),
        past.borrow().witness().resets_in_the_future()
    );

    assert!(
        lookups > 0,
        "the sweep must actually REACH `drive_at`/`hunger_at`, or a verdict of zero \
         offenders is zero out of zero and says nothing"
    );
    assert_eq!(
        offenders, 0,
        "spec §3 rule 1: a `drive_at`/`hunger_at` call ran with a reset in its future, \
         so the unfiltered lookup and a filtered one are NOT byte-equivalent on a reached \
         path -- STOP, and ledger it as a question before any further tenant lands"
    );
}

// ---------------------------------------------------------------------------
// Steps 2-4: the sustenance oracle, and the three equivalence properties.
// ---------------------------------------------------------------------------

/// `drank`'s exact on-disk spelling, a literal for [`AGENT_AT`]'s reason.
const DRANK: &str = "drank";

/// A VERBATIM COPY of `liveness.rs`'s `rise_at` body and its five authored
/// constants — the oracle's own rate function.
///
/// Copied rather than called because it is private, and because an oracle that
/// shares code with the thing under test cannot falsify it. If The Kindling's
/// coupling is ever retuned, this copy diverges and the FOLD-equals-SCAN tests
/// below go red — which is the correct outcome: they assert that
/// `sustenance_at` computes what `integrate_thirst` computed on the day the
/// migration happened, and a retune is a deliberate act that must move this
/// copy too.
const THERMONEUTRAL_C: f64 = 25.0;
/// See [`THERMONEUTRAL_C`].
const HEAT_SCALE_C: f64 = 20.0;
/// See [`THERMONEUTRAL_C`].
const ENDOTHERM_HEAT_K: f64 = 1.0;
/// See [`THERMONEUTRAL_C`].
const ECTOTHERM_K: f64 = 1.5;
/// See [`THERMONEUTRAL_C`].
const ECTOTHERM_FLOOR: f64 = 0.2;

/// See [`THERMONEUTRAL_C`] — the copied `rise_at`.
fn rise_at_copy(temp: f64, class: ThermalStrategy, p: &DriveParams) -> f64 {
    let base = p.rise;
    if !temp.is_finite() {
        return base;
    }
    match class {
        ThermalStrategy::Endothermic => {
            let excess = (temp - THERMONEUTRAL_C).max(0.0);
            base * (1.0 + ENDOTHERM_HEAT_K * excess / HEAT_SCALE_C)
        }
        ThermalStrategy::Ectothermic => {
            let factor = 1.0 + ECTOTHERM_K * (temp - THERMONEUTRAL_C) / HEAT_SCALE_C;
            base * factor.max(ECTOTHERM_FLOOR)
        }
        ThermalStrategy::Unmodelled | ThermalStrategy::Absent => base,
    }
}

/// A VERBATIM COPY of `liveness.rs`'s `integrate_thirst` body — the SCAN half
/// of FOLD equals SCAN for the sustenance tenants, and (with [`scan_oracle`]
/// above) the frozen reference for what `sustenance_at` replaced.
///
/// The production function is DELETED, so this copy is now the only statement
/// of the old arithmetic anywhere. That is the point: it cannot drift toward
/// the implementation, because nothing regenerates it.
#[allow(clippy::too_many_arguments)]
fn integrate_thirst_oracle(
    sightings: &[(f64, Facet)],
    home: &Facet,
    last_drank: f64,
    t: f64,
    terrain: &dyn Terrain,
    class: ThermalStrategy,
    p: &DriveParams,
) -> f64 {
    if t <= last_drank {
        return 0.0;
    }
    let mut bounds: Vec<f64> = vec![last_drank];
    for (d, _) in sightings {
        if *d > last_drank && *d < t {
            bounds.push(*d);
        }
    }
    bounds.push(t);
    bounds.dedup();
    let mut total = 0.0_f64;
    for w in bounds.windows(2) {
        let (s, e) = (w[0], w[1]);
        let pos = sightings
            .iter()
            .rev()
            .find(|(d, _)| *d <= s)
            .map(|(_, r)| r)
            .unwrap_or(home);
        let rate = rise_at_copy(
            terrain.temperature(
                pos,
                WorldTime::from_std_days(s).expect("a day value is finite"),
            ),
            class,
            p,
        );
        total += rate * (e - s);
    }
    total.clamp(0.0, 1.0)
}

/// A terrain whose temperature varies with BOTH room and day.
///
/// Both dependencies are load-bearing for the traps this fixture exists to
/// catch (the Tailrace's §2, traps 1-3): a room dependence makes a wrong
/// governing POSITION change the answer, and a day dependence makes a wrong
/// segment START change it. A flat terrain would pass every assertion below
/// with either bug present.
struct RippleTerrain;

impl Terrain for RippleTerrain {
    fn elevation(&self, _room: &Facet) -> f64 {
        0.0
    }
    fn is_fresh_water(&self, _room: &Facet) -> bool {
        false
    }
    fn temperature(&self, room: &Facet, day: WorldTime) -> f64 {
        // Deliberately NOT smooth and NOT symmetric: a per-room offset plus a
        // per-day ramp, so no two (room, day) pairs this fixture reaches share
        // a temperature by accident.
        let room_offset =
            f64::from(room.face) * 7.0 + room.path.iter().map(|p| f64::from(*p) * 3.0).sum::<f64>();
        20.0 + room_offset + day.as_std_days() * 1.5
    }
}

/// The sustenance fixture: one entity, a trail that crosses a reset, a
/// same-day tie, and a sighting EXACTLY at a reset day (trap 3).
///
/// Returns `(ledger, entity, home)`.
fn sustenance_fixture() -> (Ledger, EntityId, Facet) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    reg.register_predicate(DRANK, false, "drank").unwrap();
    let mut l = Ledger::default();
    let e = l.mint_entity(test_lineage(0));
    let home = room(3, &[2]);

    // `(day ticks, face, path)` for the sightings. 100_000 ticks == 1 day.
    let sightings: &[(i64, u8, &[u8])] = &[
        (50_000, 0, &[0]),  // before the first reset
        (200_000, 1, &[1]), // EXACTLY at reset 1 (day 2.0) -- trap 3
        (250_000, 2, &[0]), // between resets
        (250_000, 0, &[3]), // a same-day tie, committed out of room order
        (400_000, 1, &[2]), // between resets
        (600_000, 2, &[2]), // EXACTLY at reset 2 (day 6.0) -- trap 3 again
        (730_000, 3, &[1]), // after the last reset
        (910_000, 0, &[1]), // after the last reset
    ];
    for (ticks, face, path) in sightings {
        l.commit(
            Fact {
                subject: e,
                predicate: AGENT_AT.to_string(),
                object: Value::Text(room_text(&room(*face, path))),
                place: None,
                day: Some(WorldTime::from_ticks(*ticks)),
                provenance: "t".to_string(),
            },
            &reg,
        )
        .unwrap();
    }
    // Three resets: day 2.0, day 6.0, day 8.5. The first two land exactly on a
    // sighting; the third lands between two.
    for ticks in [200_000_i64, 600_000, 850_000] {
        l.commit(
            Fact {
                subject: e,
                predicate: DRANK.to_string(),
                object: Value::Flag(true),
                place: None,
                day: Some(WorldTime::from_ticks(ticks)),
                provenance: "t".to_string(),
            },
            &reg,
        )
        .unwrap();
    }
    (l, e, home)
}

/// Every instant this fixture is probed at: every reset, every sighting, every
/// midpoint between consecutive ones, plus one before genesis-of-history and
/// one past the last fact.
fn probe_instants() -> Vec<WorldTime> {
    let mut v: Vec<i64> = vec![
        0, 25_000, 50_000, 125_000, 200_000, 225_000, 250_000, 325_000,
    ];
    v.extend([
        400_000, 500_000, 600_000, 665_000, 730_000, 790_000, 850_000,
    ]);
    v.extend([880_000, 910_000, 1_000_000, 1_500_000]);
    v.into_iter().map(WorldTime::from_ticks).collect()
}

/// Guards the fixture from degenerating into one every assertion passes
/// vacuously: the traps must actually be present.
#[test]
fn the_sustenance_fixture_actually_contains_its_traps() {
    let (l, e, _home) = sustenance_fixture();
    let days: Vec<WorldTime> = l.facts_of(e, AGENT_AT).filter_map(|f| f.day).collect();
    let resets: Vec<WorldTime> = l.facts_of(e, DRANK).filter_map(|f| f.day).collect();
    assert_eq!(resets.len(), 3, "three resets, or step 4 probes nothing");
    let ties = days
        .iter()
        .filter(|d| **d == WorldTime::from_ticks(250_000))
        .count();
    assert_eq!(
        ties, 2,
        "the same-day tie must be present, or the tie-break is never exercised"
    );
    assert!(
        resets.iter().any(|r| days.contains(r)),
        "a sighting must land EXACTLY on a reset day (trap 3), or that trap is untested"
    );
    // And the terrain must actually discriminate, or a wrong position or day
    // could not change any answer below.
    let t = RippleTerrain;
    let a = room(0, &[0]);
    let b = room(1, &[1]);
    let d0 = WorldTime::from_ticks(0);
    let d1 = WorldTime::from_ticks(300_000);
    assert_ne!(
        t.temperature(&a, d0),
        t.temperature(&b, d0),
        "room must matter"
    );
    assert_ne!(
        t.temperature(&a, d0),
        t.temperature(&a, d1),
        "day must matter"
    );
}

/// Which reset instant a sweep feeds to BOTH halves of the comparison.
///
/// The rule is a parameter because the two live semantics differ and the
/// campaign is migrating one of them: `drive_at`/`hunger_at` take the
/// unfiltered maximum over every committed reset (the Tailrace's trap 5),
/// while `catch_up` — and a fold at the position `t` implies — takes the
/// latest reset at or before `t`. Both halves of a sweep always receive the
/// SAME instant, so a sweep compares the fold against the scan and never one
/// reset rule against the other.
#[derive(Clone, Copy, Debug)]
enum ResetRule {
    /// Today's `drive_at`/`hunger_at` lookup: the maximum over every reset,
    /// with no bound on `t`.
    Unfiltered,
    /// `last_fact_day_at_or_before`'s filter, and `catch_up`'s.
    AtOrBefore,
}

/// One whole FOLD-equals-SCAN sweep: every probe instant × every thermal
/// class, `sustenance_at` against the verbatim
/// `agent_sightings` + `integrate_thirst` oracle, under one [`ResetRule`].
///
/// Returns `(probes, probes that returned a STRICTLY POSITIVE integral,
/// the instants at which at least one class did)`.
///
/// **The second and third of those exist because the first draft of this test
/// was three-quarters vacuous and its own guard could not see it.** It swept
/// under [`ResetRule::Unfiltered`] only, which for this fixture is always day
/// 8.5 (tick 850 000) whatever `t` is — so every probe at or before that
/// instant took `sustenance_at`'s `t <= last_reset` short-circuit and asserted
/// `0.0 == 0.0`. Fifteen of nineteen probes proved nothing, and every one of
/// the fixture's traps lives inside that dead zone: the same-day tie at tick
/// 250 000 and both sightings that land exactly on a reset (200 000, 600 000)
/// were never inside an integrated interval at all. The anti-vacuity guard was
/// `segments_integrated() > 0`, which the four live probes satisfied — and
/// which the short-circuit ALSO feeds, since it records a zero-segment read.
/// Counting non-zero RESULTS is the denominator that actually discriminates,
/// the same shape the rule-1 witness needed.
fn sweep_against_the_oracle(rule: ResetRule) -> (usize, usize, std::collections::BTreeSet<i64>) {
    let (l, e, home) = sustenance_fixture();
    let terrain = RippleTerrain;
    let mut store = ResidentFolds::new();
    let mut witness = ReadWitness::default();
    let mut probes = 0usize;
    let mut nonzero = 0usize;
    let mut live: std::collections::BTreeSet<i64> = std::collections::BTreeSet::new();

    for class in [
        ThermalStrategy::Endothermic,
        ThermalStrategy::Ectothermic,
        ThermalStrategy::Unmodelled,
    ] {
        for t in probe_instants() {
            let (trail, resets, memo, _w) = store.trail_and_thirst(&l);
            // The ONE reset instant both halves see. The `GENESIS` floor is
            // what `drive_at`'s old `fold(0.0, f64::max)` did and what
            // `catch_up` names at its own call site for `None`.
            let reset = match rule {
                ResetRule::Unfiltered => resets.last_reset(e),
                ResetRule::AtOrBefore => resets.last_reset_at_or_before(e, t),
            }
            .unwrap_or(WorldTime::GENESIS)
            .max(WorldTime::GENESIS);

            let got = sustenance_at(
                trail,
                e,
                &home,
                reset,
                &[],
                t,
                &terrain,
                class,
                &SUSTENANCE,
                memo,
                &mut witness,
            );
            // The scan half: the exact pair of functions production used, fed
            // the identical reset.
            let sightings = scan_oracle(&l, e, t.as_std_days());
            let expected = integrate_thirst_oracle(
                &sightings,
                &home,
                reset.as_std_days(),
                t.as_std_days(),
                &terrain,
                class,
                &SUSTENANCE,
            );
            assert_eq!(
                got, expected,
                "sustenance_at must equal the integrate_thirst oracle bit for bit \
                 at {t:?} for {class:?} under {rule:?}"
            );
            probes += 1;
            if got > 0.0 {
                nonzero += 1;
                live.insert(t.ticks());
            }
        }
    }
    (probes, nonzero, live)
}

/// Step 2: FOLD equals SCAN for the sustenance read, at every probe instant,
/// against the verbatim `agent_sightings` + `integrate_thirst` oracle.
///
/// `==` on `f64`, not an epsilon: the arithmetic is meant to be the identical
/// sequence of operations on the identical values, so any difference at all is
/// a defect rather than a rounding budget.
///
/// **Swept twice, under both live reset rules** — see [`ResetRule`] for why
/// there are two, and [`sweep_against_the_oracle`] for what the unfiltered
/// sweep alone could and could not see. The floors below are the point of the
/// second sweep: they are asserted on the number of probes that returned a
/// strictly POSITIVE integral, so the sweep cannot go quiet if the fixture's
/// resets move.
#[test]
fn sustenance_at_equals_the_integrate_thirst_oracle_at_every_instant() {
    // The fixture's own landmarks, named so the floors below are readable.
    const FIRST_RESET: i64 = 200_000;
    const SAME_DAY_TIE: i64 = 250_000;
    // A probe whose integrated interval STARTS at a reset that a sighting
    // lands exactly on (trap 3) and CONTAINS the same-day tie (trap 4).
    const SPANS_BOTH_TRAPS: i64 = 325_000;

    let (probes_u, nonzero_u, live_u) = sweep_against_the_oracle(ResetRule::Unfiltered);
    let (probes_f, nonzero_f, live_f) = sweep_against_the_oracle(ResetRule::AtOrBefore);
    println!("--- FOLD equals SCAN sweep coverage ---");
    println!("Unfiltered: {nonzero_u} of {probes_u} probes integrated a non-zero interval");
    println!("AtOrBefore: {nonzero_f} of {probes_f} probes integrated a non-zero interval");
    println!("AtOrBefore live instants (ticks): {live_f:?}");

    assert_eq!(probes_u, probes_f, "both sweeps must probe the same grid");
    // The unfiltered sweep's own floor. It is LOW on purpose and is not the
    // load-bearing one: with a single unfiltered reset, only probes past the
    // last reset in the whole fixture can integrate anything at all.
    assert!(
        nonzero_u >= 12,
        "the unfiltered sweep must integrate a real interval at least 12 times \
         (4 probes x 3 classes), or it is asserting 0.0 == 0.0 throughout: got \
         {nonzero_u} of {probes_u}"
    );
    // The load-bearing floor: under the filtered rule every probe strictly
    // between two resets integrates, so the traps land inside the sweep.
    assert!(
        nonzero_f >= 45,
        "the filtered sweep must integrate a real interval at least 45 times \
         (15 probes x 3 classes), or the fixture's traps have drifted out of \
         every integrated interval: got {nonzero_f} of {probes_f}"
    );
    assert!(
        live_f.contains(&SPANS_BOTH_TRAPS),
        "tick {SPANS_BOTH_TRAPS} must integrate a real interval: it is the probe whose \
         interval starts at the reset a sighting lands exactly on (tick {FIRST_RESET}, \
         trap 3) and contains the same-day tie (tick {SAME_DAY_TIE}, trap 4). Without it \
         neither trap is inside this sweep at all, which is exactly the hole the \
         unfiltered-only sweep had: {live_f:?}"
    );
    assert!(
        !live_u.contains(&SPANS_BOTH_TRAPS),
        "sanity: under the UNFILTERED rule that probe is short-circuited, which is why \
         the second sweep exists -- if this ever stops holding the fixture has changed \
         and both floors above need re-deriving"
    );
}

/// Step 4: a PAST-INSTANT read, taken with the reset a fold at that position
/// would have seen — the checkpoint semantics decision 0237 describes, and the
/// shape the hazard path's `affect_of(m, past_day)` will need in stage 2.
#[test]
fn a_past_instant_read_between_two_resets_equals_the_scan_at_that_instant() {
    let (l, e, home) = sustenance_fixture();
    let terrain = RippleTerrain;
    let mut store = ResidentFolds::new();
    let mut witness = ReadWitness::default();

    // Strictly between reset 1 (day 2.0) and reset 2 (day 6.0), and again
    // between reset 2 and reset 3 (day 8.5).
    for t in [
        WorldTime::from_ticks(430_000),
        WorldTime::from_ticks(700_000),
    ] {
        let (trail, resets, memo, _w) = store.trail_and_thirst(&l);
        let reset = resets
            .last_reset_at_or_before(e, t)
            .expect("both probes lie after the first reset");
        let got = sustenance_at(
            trail,
            e,
            &home,
            reset,
            &[],
            t,
            &terrain,
            ThermalStrategy::Endothermic,
            &SUSTENANCE,
            memo,
            &mut witness,
        );
        let sightings = scan_oracle(&l, e, t.as_std_days());
        let expected = integrate_thirst_oracle(
            &sightings,
            &home,
            reset.as_std_days(),
            t.as_std_days(),
            &terrain,
            ThermalStrategy::Endothermic,
            &SUSTENANCE,
        );
        assert_eq!(
            got, expected,
            "the past-instant read must equal the scan at {t:?}"
        );
        assert!(
            got > 0.0,
            "the probe at {t:?} must accrue SOMETHING, or this test cannot tell a \
             correct answer from a zeroed one"
        );
    }
}

/// Step 3: the overlay. Holding back the last `k` `agent-at` facts and passing
/// them as the read-side overlay must give the answer the full ledger gives.
///
/// The overlay is deliberately handed over SHUFFLED — reversed, and then
/// rotated — because `decide_step` builds it from `out` in emission order and
/// then sorts by `(day, room)`. An implementation that concatenated without
/// sorting would pass on an already-ordered overlay and fail here.
#[test]
fn holding_facts_back_as_an_overlay_equals_the_scan_over_the_full_ledger() {
    let (full, e, home) = sustenance_fixture();
    let terrain = RippleTerrain;
    let mut witness = ReadWitness::default();

    // Every `agent-at` fact of `e`, in commit order.
    let all: Vec<(WorldTime, Facet)> = full
        .facts_of(e, AGENT_AT)
        .filter_map(|f| {
            let d = f.day?;
            match &f.object {
                Value::Text(s) => Some((d, room_from_text_copy(s))),
                _ => None,
            }
        })
        .collect();

    for k in 1..=4usize {
        // A ledger with the last k `agent-at` facts withheld, resets intact.
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(AGENT_AT, false, "pos").unwrap();
        reg.register_predicate(DRANK, false, "drank").unwrap();
        let mut partial = Ledger::default();
        let e2 = partial.mint_entity(test_lineage(0));
        assert_eq!(e2, e, "the same fixed lineage mints the same id");
        let mut seen = 0usize;
        for f in full.iter() {
            if f.predicate == AGENT_AT {
                seen += 1;
                if seen > all.len() - k {
                    continue;
                }
            }
            partial.commit(f.clone(), &reg).unwrap();
        }

        // Deliberately out of `(day, room)` order. `reverse()` alone is a
        // real permutation for every `k >= 2`; the rotate is only added above
        // that, because `reverse` then `rotate_left(1)` on a PAIR returns the
        // original list -- which is what an earlier draft did, making the
        // shuffle a no-op at exactly the two smallest overlays.
        let mut overlay: Vec<(WorldTime, Facet)> = all[all.len() - k..].to_vec();
        overlay.reverse();
        if k >= 3 {
            overlay.rotate_left(1);
        }
        if k >= 2 {
            let mut sorted = overlay.clone();
            sorted.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
            assert_ne!(
                overlay, sorted,
                "the overlay handed to the read must NOT already be in (day, room) order, \
                 or this test cannot tell a sorting implementation from a concatenating \
                 one (k = {k})"
            );
        }

        let mut store = ResidentFolds::new();
        for t in probe_instants() {
            let (trail, resets, memo, _w) = store.trail_and_thirst(&partial);
            let reset = resets
                .last_reset(e)
                .unwrap_or(WorldTime::GENESIS)
                .max(WorldTime::GENESIS);
            // The overlay carries only facts at or before `t`, exactly the
            // filter `decide_step` applies when it builds one from `out`.
            let visible: Vec<(WorldTime, Facet)> =
                overlay.iter().filter(|(d, _)| *d <= t).cloned().collect();
            let got = sustenance_at(
                trail,
                e,
                &home,
                reset,
                &visible,
                t,
                &terrain,
                ThermalStrategy::Endothermic,
                &SUSTENANCE,
                memo,
                &mut witness,
            );
            let sightings = scan_oracle(&full, e, t.as_std_days());
            let expected = integrate_thirst_oracle(
                &sightings,
                &home,
                reset.as_std_days(),
                t.as_std_days(),
                &terrain,
                ThermalStrategy::Endothermic,
                &SUSTENANCE,
            );
            assert_eq!(
                got, expected,
                "with the last {k} facts held back as a SHUFFLED overlay, the read at \
                 {t:?} must equal the scan over the full ledger"
            );
        }
    }
}

/// The sustenance tenants owe the same two chaos schedules [`Trail`] does:
/// discarding the accumulated state and rebuilding it from the ledger is
/// unobservable, at every position and at every third.
///
/// **The read-side accumulator is inside this schedule, not beside it** (The
/// Pawl, Task 5b). It is not a [`hornvale_kernel::fold::LedgerFold`] — it is a
/// function of terrain as well as the ledger — so comparing the tenants' reset
/// lists alone would leave the one piece of state that carries `f64`
/// arithmetic entirely untested against discard. So each prefix also takes a
/// full [`sustenance_at`] read through BOTH stores and compares them with `==`
/// on `f64`: the discarded store rebuilds its accumulator from scratch, the
/// resident one resumes from a checkpoint, and the two must agree bit for bit.
/// One terrain for both stores, which is the invariant the memo states.
fn sustenance_discard_schedule(every: usize) {
    let (l, e, home) = sustenance_fixture();
    let terrain = RippleTerrain;
    let mut witness = ReadWitness::default();
    let mut resident = ResidentFolds::new();
    let mut chaotic = ResidentFolds::new();
    // The anti-vacuity counter: a schedule in which every read short-circuited
    // to zero would compare `0.0 == 0.0` throughout and prove nothing about
    // the accumulator, which is the exact hole the FOLD-equals-SCAN sweep
    // found in its own first draft.
    let mut nonzero = 0usize;

    // Prefixes are built by replaying, since `Ledger` cannot be truncated.
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    reg.register_predicate(DRANK, false, "drank").unwrap();
    let facts: Vec<Fact> = l.iter().cloned().collect();
    for n in 0..=facts.len() {
        let mut prefix = Ledger::default();
        let _ = prefix.mint_entity(test_lineage(0));
        for f in facts.iter().take(n) {
            prefix.commit(f.clone(), &reg).unwrap();
        }
        let _ = resident.sustenance_thirst(&prefix);
        if n % every == 0 {
            chaotic = ResidentFolds::new();
        }
        assert_eq!(
            chaotic.sustenance_thirst(&prefix).resets(e),
            resident.sustenance_thirst(&prefix).resets(e),
            "the thirst resets diverged at prefix {n} under a discard-every-{every} schedule"
        );
        assert_eq!(
            chaotic.position(),
            resident.position(),
            "position diverged at prefix {n} under a discard-every-{every} schedule"
        );
        // And the accumulator itself, at every probe instant, under both reset
        // rules — the state a reset-list comparison cannot see.
        for t in probe_instants() {
            for rule in [ResetRule::Unfiltered, ResetRule::AtOrBefore] {
                let mut read = |store: &mut ResidentFolds| -> f64 {
                    let (trail, resets, memo, _w) = store.trail_and_thirst(&prefix);
                    let reset = match rule {
                        ResetRule::Unfiltered => resets.last_reset(e),
                        ResetRule::AtOrBefore => resets.last_reset_at_or_before(e, t),
                    }
                    .unwrap_or(WorldTime::GENESIS)
                    .max(WorldTime::GENESIS);
                    sustenance_at(
                        trail,
                        e,
                        &home,
                        reset,
                        &[],
                        t,
                        &terrain,
                        ThermalStrategy::Endothermic,
                        &SUSTENANCE,
                        memo,
                        &mut witness,
                    )
                };
                let (discarded, kept) = (read(&mut chaotic), read(&mut resident));
                if kept > 0.0 {
                    nonzero += 1;
                }
                assert_eq!(
                    discarded, kept,
                    "the sustenance accumulator diverged at prefix {n}, instant {t:?}, \
                     rule {rule:?}, under a discard-every-{every} schedule"
                );
            }
        }
    }
    assert!(
        nonzero >= 100,
        "the discard schedule must compare a strictly POSITIVE integral at least 100 \
         times, or it is asserting 0.0 == 0.0 throughout: got {nonzero}"
    );
}

#[test]
fn discarding_the_sustenance_tenants_at_every_position_is_unobservable() {
    sustenance_discard_schedule(1);
}

#[test]
fn discarding_the_sustenance_tenants_at_every_third_position_is_unobservable() {
    sustenance_discard_schedule(3);
}

/// The two reset lookups answer what their scan counterparts answer.
///
/// `last_reset` is the UNFILTERED maximum `drive_at`/`hunger_at`/
/// `WalkState::begin` fold today; `last_reset_at_or_before` is
/// `last_fact_day_at_or_before`'s filter. Both are checked against a direct
/// scan of the ledger, at every probe instant.
#[test]
fn the_reset_lookups_equal_a_direct_scan_of_the_ledger() {
    let (l, e, _home) = sustenance_fixture();
    let mut store = ResidentFolds::new();
    let resets = store.sustenance_thirst(&l);

    let scanned: Vec<WorldTime> = l.facts_of(e, DRANK).filter_map(|f| f.day).collect();
    assert_eq!(
        resets.last_reset(e),
        scanned.iter().copied().max(),
        "the unfiltered lookup is the maximum over every committed reset"
    );
    for t in probe_instants() {
        assert_eq!(
            resets.last_reset_at_or_before(e, t),
            scanned.iter().copied().filter(|d| *d <= t).max(),
            "the filtered lookup at {t:?} is `last_fact_day_at_or_before`'s own answer"
        );
    }
    // An entity with no resets at all.
    let stranger = EntityId::new(9_999).expect("9999 is non-zero");
    assert_eq!(resets.last_reset(stranger), None);
    assert_eq!(
        resets.last_reset_at_or_before(stranger, WorldTime::from_ticks(1)),
        None
    );
}

// ---------------------------------------------------------------------------
// Step 5: the cost witness, and the currency invariant on the SESSION's own
// store.
// ---------------------------------------------------------------------------

/// The currency invariant on the store production actually owns: **a READER
/// never observes a fold behind the ledger it is GIVEN**, and the seam is at
/// read rather than at commit.
///
/// `the_store_is_current_with_a_real_sessions_ledger` above builds its OWN
/// store beside the session, so it can only ever prove that a fresh store
/// agrees with a ledger — never that the session's did. This one asks the
/// session's, through [`Session::resident_position`].
///
/// **The ledger a turn's readers are given is `frozen`, the PRE-tick ledger,
/// and that is the whole content of this assertion.** `Ledger::commit` gained
/// no hook (spec §2.2), so a turn ends with the session's store standing at
/// exactly the fact count the turn STARTED with: every read the walk made —
/// `WalkState::begin`'s reset lookups, `decide_step`'s two integrals,
/// `catch_up`'s filtered lookups, and all of them a second time when
/// `Session::wait` evaluates the same walk again for the facts it commits —
/// saw `frozen`, and each was current with it. Nothing observes the lag: the
/// next turn's reads advance over it, and the equality below is taken every
/// turn, so a lag that ever grew by more than one turn's commits would fail.
///
/// An earlier draft asserted currency against the POST-tick count and failed
/// with 21642 against 21647 — the five facts that turn had just committed,
/// correctly not yet absorbed. A second draft then tried to close the gap by
/// calling `Session::snapshot`, on the belief that a snapshot reads the store;
/// it does not on this world, and the reason is the finding recorded on
/// `rule_one_witness_no_read_runs_before_a_reset_of_the_same_entity`: seed 42's
/// possession is never co-located with another creature, so `snapshot` reaches
/// `affect_of_memo_occupied` for nobody and performs no read at all.
#[test]
fn the_sessions_own_store_stands_exactly_at_the_ledger_each_turns_readers_were_given() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");

    let mut lagged_at_least_once = false;
    let mut frozen_len = session.committed_fact_count();
    for turn in 0..5 {
        session.handle("wait");
        let committed = session.committed_fact_count();
        let position = session.resident_position() as usize;
        assert!(
            position <= committed,
            "a fold can never run AHEAD of its ledger (turn {turn}: {position} > {committed})"
        );
        assert_eq!(
            position, frozen_len,
            "turn {turn}'s readers were handed the PRE-tick ledger, so the store must \
             stand exactly at its length -- neither behind it (a stale read) nor past it \
             (an absorb from a ledger no reader was given)"
        );
        lagged_at_least_once |= position < committed;

        // Reading the position twice moves nothing, and neither does asking
        // the witness: an accessor is not a read of the folds.
        let before = session.resident_position();
        let _ = session.resident_segments_integrated();
        assert_eq!(session.resident_position(), before);

        frozen_len = committed;
    }
    assert!(
        lagged_at_least_once,
        "the store must have been behind its ledger at least once between a commit and \
         the next read, or this test never exercised the advance-on-read seam at all"
    );
}

/// Step 5, the cost witness: the live read is O(sightings since the previous
/// reset), not O(history).
///
/// Stated as a COUNT rather than a duration on purpose — a timing here would
/// measure the box. `ReadWitness` counts the integral SEGMENTS every read
/// sums, per creature, so the per-turn difference is exactly the work the
/// sustenance reads did for that creature on that turn.
///
/// **Per creature, and the choice cannot be made in advance.** A creature that
/// has never drunk integrates from genesis by construction, so its segment
/// count grows with its own history — that is a property of the integral, not
/// of the store, and it is unchanged by this campaign. The claim is about a
/// creature that HAS reset: past its first reset, its reads walk the interval
/// since that reset and nothing older. So the roster is sampled every turn and
/// the subject is chosen afterwards, from the ledger.
#[test]
fn segments_integrated_per_turn_does_not_grow_with_the_tick_index_for_a_creature_that_drinks() {
    const TURNS: usize = 200;
    const BAND: usize = 50;

    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");

    let mut samples: Vec<std::collections::BTreeMap<EntityId, u64>> = Vec::with_capacity(TURNS + 1);
    samples.push(session.resident_segments_by_entity());
    for _ in 0..TURNS {
        session.handle("wait");
        let _ = session.snapshot().expect("seed 42's session snapshots");
        samples.push(session.resident_segments_by_entity());
    }

    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let mut drinkers: std::collections::BTreeSet<EntityId> = std::collections::BTreeSet::new();
    for f in ledger.iter() {
        if f.predicate == DRANK {
            drinkers.insert(f.subject);
        }
    }

    // Among the creatures that drank, the one whose reads did the most work —
    // the hardest case for the claim, not the easiest.
    let last = samples.last().expect("TURNS + 1 samples exist");
    let subject = drinkers
        .iter()
        .copied()
        .max_by_key(|e| last.get(e).copied().unwrap_or(0));
    println!("--- cost witness: segments integrated per turn (seed 42, {TURNS} waits) ---");
    println!(
        "{} creatures drank at least once; {} creatures were read at all",
        drinkers.len(),
        last.len()
    );
    let Some(subject) = subject else {
        panic!(
            "no creature drank in {TURNS} turns of seed 42, so this witness has no subject \
             and measured nothing -- the fixture, not the property, is what failed"
        );
    };

    let per_turn: Vec<u64> = samples
        .windows(2)
        .map(|w| {
            w[1].get(&subject).copied().unwrap_or(0) - w[0].get(&subject).copied().unwrap_or(0)
        })
        .collect();
    let mean = |xs: &[u64]| xs.iter().sum::<u64>() as f64 / xs.len() as f64;
    let first = mean(&per_turn[..BAND]);
    let last_band = mean(&per_turn[per_turn.len() - BAND..]);
    println!(
        "subject {subject:?}: first {BAND} turns mean {first:.3} segments/turn, \
         last {BAND} turns mean {last_band:.3} -- ratio {:.3}",
        if first > 0.0 {
            last_band / first
        } else {
            f64::NAN
        }
    );
    println!(
        "the same subject's endpoints: turn 1 {} segments, turn {TURNS} {} segments \
         (single turns swing hard with what the creature was doing, which is why the \
         claim is on 50-turn band means and not on either endpoint)",
        per_turn.first().copied().unwrap_or(0),
        per_turn.last().copied().unwrap_or(0)
    );

    assert!(
        first > 0.0,
        "the subject must integrate something in the first {BAND} turns, or the ratio \
         below is vacuous"
    );
    assert!(
        last_band <= first * 1.5,
        "segments integrated per turn must not grow with the tick index for a creature \
         that has drunk: first {BAND} turns mean {first}, last {BAND} mean {last_band} \
         (allowance 1.5x)"
    );
}

/// Task 5b's cost witness: the read is O(what the ledger newly determined),
/// not O(history) — measured on the population the campaign's first readout
/// found it had never measured.
///
/// **The Task 3 witness above measures the busiest DRINKER, and that is the
/// wrong population for this claim.** A creature that resets integrates from
/// its last reset, so its read is bounded by the interval since that reset
/// whatever the store does — which is why that witness read 0.889 (an 11%
/// constant-factor saving) and could not see that a creature which NEVER
/// resets was still paying O(history) on every read. Spec §11.7 measured what
/// that costs on the real bench: 21 of 50 agents commit zero `drank` facts
/// across 200 ticks, including the probe agent H2's decisive column is taken
/// on, and for those `S` equals `H` forever.
///
/// So the subject here is chosen from the complement: a creature with NO
/// committed reset of either sustenance drive. Both quantities the read
/// spends are asserted, and both are stated as counts rather than durations
/// for the Task 3 witness's reason (a timing measures the box):
///
/// 1. **Segments integrated per turn** must not grow with the tick index.
/// 2. **Terrain samples per turn** must not either — the quantity spec §11.7
///    named as the leading candidate for the ~200x ecological-versus-synthetic
///    gap, counted rather than inferred.
/// 3. **No read may sample terrain more times than the ledger grew for that
///    creature since the previous read of the same drive**, plus the in-tick
///    overlay's own boundaries, plus the one open segment. This is the
///    per-read form of the same property, taken on the production path by
///    [`ReadWitness::note_sustenance_read`], with its denominator asserted
///    beneath it.
#[test]
fn the_cost_of_a_read_does_not_grow_with_the_tick_index_for_a_creature_that_never_resets() {
    const TURNS: usize = 200;
    const BAND: usize = 50;
    const EATEN: &str = "eaten";

    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");

    let mut segments: Vec<std::collections::BTreeMap<EntityId, u64>> =
        Vec::with_capacity(TURNS + 1);
    let mut samples: Vec<std::collections::BTreeMap<EntityId, u64>> = Vec::with_capacity(TURNS + 1);
    segments.push(session.resident_segments_by_entity());
    samples.push(session.resident_terrain_samples_by_entity());
    for _ in 0..TURNS {
        session.handle("wait");
        let _ = session.snapshot().expect("seed 42's session snapshots");
        segments.push(session.resident_segments_by_entity());
        samples.push(session.resident_terrain_samples_by_entity());
    }

    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let mut resetters: std::collections::BTreeSet<EntityId> = std::collections::BTreeSet::new();
    for f in ledger.iter() {
        if f.predicate == DRANK || f.predicate == EATEN {
            resetters.insert(f.subject);
        }
    }

    let last = segments.last().expect("TURNS + 1 samples exist");
    println!("--- Task 5b cost witness: the NON-RESETTING population (seed 42, {TURNS} waits) ---");
    println!(
        "{} creatures reset at least once; {} creatures were read at all",
        resetters.len(),
        last.len()
    );
    // The hardest case for the claim among creatures that never reset — the
    // one whose reads have done the most work, chosen from the ledger AFTER
    // the run rather than named in advance.
    let subject = last
        .iter()
        .filter(|(e, _)| !resetters.contains(e))
        .max_by_key(|(_, segs)| **segs)
        .map(|(e, _)| *e);
    let Some(subject) = subject else {
        panic!(
            "every creature read in {TURNS} turns of seed 42 reset at least once, so this \
             witness has no subject and measured nothing -- the fixture, not the property, \
             is what failed"
        );
    };

    let per_turn = |xs: &[std::collections::BTreeMap<EntityId, u64>]| -> Vec<u64> {
        xs.windows(2)
            .map(|w| {
                w[1].get(&subject).copied().unwrap_or(0) - w[0].get(&subject).copied().unwrap_or(0)
            })
            .collect()
    };
    let mean = |xs: &[u64]| xs.iter().sum::<u64>() as f64 / xs.len() as f64;

    let seg_turns = per_turn(&segments);
    let sam_turns = per_turn(&samples);
    let seg_first = mean(&seg_turns[..BAND]);
    let seg_last = mean(&seg_turns[seg_turns.len() - BAND..]);
    let sam_first = mean(&sam_turns[..BAND]);
    let sam_last = mean(&sam_turns[sam_turns.len() - BAND..]);
    println!(
        "subject {subject:?} (zero `drank`, zero `eaten`): segments/turn first {BAND} mean \
         {seg_first:.3}, last {BAND} mean {seg_last:.3} -- ratio {:.3}",
        seg_last / seg_first
    );
    println!(
        "subject {subject:?}: terrain samples/turn first {BAND} mean {sam_first:.3}, \
         last {BAND} mean {sam_last:.3} -- ratio {:.3}",
        sam_last / sam_first
    );

    let reads = session
        .resident_sustenance_reads_by_entity()
        .get(&subject)
        .copied()
        .unwrap_or(0);
    let unbounded = session
        .resident_unbounded_reads_by_entity()
        .get(&subject)
        .copied()
        .unwrap_or(0);
    println!("subject {subject:?}: {reads} integrating reads, {unbounded} of them unbounded");
    if let Some((entity, taken, allowed)) = session.resident_first_unbounded_read() {
        println!(
            "first unbounded read anywhere in the roster: entity {entity:?} sampled terrain \
             {taken} times against an allowance of {allowed}"
        );
    }

    assert!(
        seg_first > 0.0 && sam_first > 0.0,
        "the subject must integrate something in the first {BAND} turns, or the ratios \
         above are vacuous: segments {seg_first}, samples {sam_first}"
    );
    assert!(
        seg_last <= seg_first * 1.5,
        "segments integrated per turn must not grow with the tick index for a creature \
         that never resets: first {BAND} mean {seg_first}, last {BAND} mean {seg_last} \
         (allowance 1.5x)"
    );
    assert!(
        sam_last <= sam_first * 1.5,
        "terrain samples per turn must not grow with the tick index for a creature that \
         never resets: first {BAND} mean {sam_first}, last {BAND} mean {sam_last} \
         (allowance 1.5x)"
    );
    assert!(
        reads > 0,
        "the subject must actually be read, or a verdict of zero unbounded reads is zero \
         out of zero and says nothing"
    );
    assert_eq!(
        unbounded, 0,
        "every read of a creature that never resets must cost the segments the ledger \
         newly determined for it (plus this tick's overlay and the one open segment), not \
         the whole history: {unbounded} of {reads} reads exceeded that allowance"
    );
}

// ---------------------------------------------------------------------------
// The Pawl, Task 4: `KnownWater` — the belief tenant.
//
// Step 1 is the rule-6 witness; steps 2 and 3 are FOLD equals SCAN against a
// VERBATIM copy of `believed_water`'s own set-building loop, plus both chaos
// schedules. The oracle is a copy for `scan_oracle`'s reason: the production
// loop is deleted by this task, and an oracle that shares code with the thing
// under test cannot falsify it.
// ---------------------------------------------------------------------------

/// A terrain whose water rooms are a fixed set — [`RippleTerrain`] reports
/// nothing wet, so every `is_water` filter over it would pass vacuously.
struct PoolTerrain {
    /// The rooms `is_fresh_water` says yes to.
    wet: std::collections::BTreeSet<Facet>,
}

impl Terrain for PoolTerrain {
    fn elevation(&self, _room: &Facet) -> f64 {
        0.0
    }
    fn is_fresh_water(&self, room: &Facet) -> bool {
        self.wet.contains(room)
    }
    fn temperature(&self, _room: &Facet, _day: WorldTime) -> f64 {
        20.0
    }
}

/// A VERBATIM COPY of `liveness.rs`'s `believed_water` set-building loop — the
/// SCAN half of FOLD equals SCAN for [`KnownWater`].
///
/// Only the loop: the `plan_to_room` ranking below it is untouched by this
/// migration and is not what the tenant replaced. Copied rather than called
/// because the production loop is GONE as of this task, so this is now the
/// only statement of the old set-building anywhere, and nothing regenerates
/// it.
fn known_water_scan_oracle(
    ledger: &Ledger,
    entity: EntityId,
    t: WorldTime,
    terrain: &dyn Terrain,
) -> std::collections::BTreeSet<Facet> {
    let mut seen: std::collections::BTreeSet<Facet> = std::collections::BTreeSet::new();
    for f in ledger.facts_of(entity, AGENT_AT) {
        let sighted = f.day.map(|d| d <= t).unwrap_or(false);
        if sighted && let Value::Text(s) = &f.object {
            let room = room_from_text_copy(s);
            if hornvale_vessel::liveness::is_water(&room, terrain) {
                seen.insert(room);
            }
        }
    }
    seen
}

/// The fold half for [`KnownWater`], reached through `absorb_at` one fact at a
/// time — the path independent of `advance_to`, for the reason
/// [`fold_one_by_one`] states.
fn fold_known_water_one_by_one(ledger: &Ledger) -> Folded<KnownWater> {
    let mut f: Folded<KnownWater> = Folded::new();
    for (i, fact) in ledger.iter().enumerate() {
        f.absorb_at(i as u64, fact);
    }
    f
}

/// The fixture's wet rooms: SOME of the rooms [`SCRIPT`] visits, never all of
/// them, so the `is_water` filter is exercised in both directions.
///
/// [`SCRIPT`] posts to `(face 0, [0])`, `(face 0, [1])`, `(face 1, [2])` and
/// `(face 0, [3])` across its two entities; three are wet here and one — plus
/// a room nobody ever stood in — is not.
fn pool_terrain() -> PoolTerrain {
    PoolTerrain {
        wet: [room(0, &[0]), room(1, &[2]), room(0, &[3]), room(5, &[7])]
            .into_iter()
            .collect(),
    }
}

/// Guards every `is_water` assertion below from being vacuous the way
/// [`the_hand_built_ledger_is_not_already_in_sorted_order`] guards the
/// ordering ones: the fixture must visit both wet and dry rooms, or a fold
/// that ignored terrain entirely would pass.
#[test]
fn the_fixture_visits_both_wet_and_dry_rooms() {
    let (l, a, b) = hand_built();
    let terrain = pool_terrain();
    let far = WorldTime::from_ticks(9_999_999);
    let visited: std::collections::BTreeSet<Facet> = [a, b]
        .into_iter()
        .flat_map(|e| {
            l.facts_of(e, AGENT_AT)
                .filter(|f| f.day.is_some())
                .map(|f| match &f.object {
                    Value::Text(s) => room_from_text_copy(s),
                    other => panic!("an agent-at object is always text, got {other:?}"),
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let wet: Vec<&Facet> = visited
        .iter()
        .filter(|r| hornvale_vessel::liveness::is_water(r, &terrain))
        .collect();
    let dry: Vec<&Facet> = visited
        .iter()
        .filter(|r| !hornvale_vessel::liveness::is_water(r, &terrain))
        .collect();
    assert!(
        !wet.is_empty() && !dry.is_empty(),
        "the fixture must visit at least one wet and one dry room, or the water filter \
         is never exercised: wet {wet:?}, dry {dry:?}"
    );
    // And the oracle must actually drop something at the whole-history instant
    // -- compared against THAT ENTITY's own visited rooms, not the union over
    // both, which is a larger denominator and would let the assertion pass on
    // an entity whose rooms are all wet.
    for e in [a, b] {
        let mine: std::collections::BTreeSet<Facet> = l
            .facts_of(e, AGENT_AT)
            .filter(|f| f.day.is_some())
            .map(|f| match &f.object {
                Value::Text(s) => room_from_text_copy(s),
                other => panic!("an agent-at object is always text, got {other:?}"),
            })
            .collect();
        let scanned = known_water_scan_oracle(&l, e, far, &terrain);
        assert!(
            scanned.len() < mine.len(),
            "the oracle must drop at least one of {e:?}'s OWN {} visited rooms as dry, or \
             the filter is vacuous for it: kept {scanned:?}",
            mine.len()
        );
    }
}

#[test]
fn known_water_folded_one_fact_at_a_time_equals_the_scan_oracle() {
    let (l, a, b) = hand_built();
    let terrain = pool_terrain();
    let folded = fold_known_water_one_by_one(&l);
    let far = WorldTime::from_ticks(9_999_999);

    for e in [a, b] {
        let scanned = known_water_scan_oracle(&l, e, far, &terrain);
        assert_eq!(
            folded.state().water_at(e, far, &terrain),
            scanned.into_iter().collect::<Vec<_>>(),
            "the fold's water set for {e:?} must equal the scan oracle's, ascending"
        );
    }
}

#[test]
fn known_water_at_every_past_instant_equals_the_oracle_at_that_instant() {
    let (l, a, b) = hand_built();
    let terrain = pool_terrain();
    let folded = fold_known_water_one_by_one(&l);

    for e in [a, b] {
        // Every instant the script names, plus one strictly before the first
        // and one strictly after the last, so the empty and full answers are
        // both covered — and the ones in between are exactly where an
        // unfiltered set read would differ.
        let mut probes: Vec<WorldTime> = SCRIPT
            .iter()
            .map(|(_, t, _, _)| WorldTime::from_ticks(*t))
            .collect();
        probes.push(WorldTime::from_ticks(0));
        probes.push(WorldTime::from_ticks(9_999_999));
        // The floor below counts these: a probe where the oracle returned a
        // NON-EMPTY set. Comparing two empty sets is agreement about nothing,
        // and at the early probes that is exactly what happens.
        let mut non_empty = 0_usize;
        for t in probes {
            let scanned: Vec<Facet> = known_water_scan_oracle(&l, e, t, &terrain)
                .into_iter()
                .collect();
            if !scanned.is_empty() {
                non_empty += 1;
            }
            assert_eq!(
                folded.state().water_at(e, t, &terrain),
                scanned,
                "the fold's water set for {e:?} at {t:?} must equal the oracle's at that \
                 instant -- this is spec §3 rule 6's first-visit filter"
            );
        }
        // MEASURED against this fixture: 8 of the 10 probes for the first
        // entity, 5 of 10 for the second (taken by raising this floor until it
        // reported each entity's own count). The floor is 4 — under the
        // smaller of the two, so a fixture edit that costs a probe is not a
        // red, and one that empties the comparison is.
        assert!(
            non_empty >= 4,
            "at least 3 of {e:?}'s probes must compare NON-EMPTY sets, or the equality \
             above is two empty sets agreeing: got {non_empty}"
        );
    }
}

#[test]
fn a_room_visited_twice_keeps_its_first_instant() {
    // The read admits a room when SOME sighting of it is at or before `t`,
    // which is the MINIMUM over its sightings. A tenant that kept the latest
    // would forget the room at every instant between the two visits.
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    let mut l = Ledger::default();
    let e = l.mint_entity(test_lineage(0));
    let wet = room(0, &[0]);
    for ticks in [500_000_i64, 100_000] {
        l.commit(
            Fact {
                subject: e,
                predicate: AGENT_AT.to_string(),
                object: Value::Text(room_text(&wet)),
                place: None,
                day: Some(WorldTime::from_ticks(ticks)),
                provenance: "t".to_string(),
            },
            &reg,
        )
        .unwrap();
    }
    let terrain = PoolTerrain {
        wet: [wet.clone()].into_iter().collect(),
    };
    let folded = fold_known_water_one_by_one(&l);
    assert_eq!(
        folded.state().of(e).get(&wet),
        Some(&WorldTime::from_ticks(100_000)),
        "the LATER commit of an EARLIER instant must win: the first visit is the minimum"
    );
    assert_eq!(
        folded
            .state()
            .water_at(e, WorldTime::from_ticks(200_000), &terrain),
        vec![wet],
        "a room first seen at 100_000 is known at 200_000, whichever order the two \
         sightings committed in"
    );
}

#[test]
fn an_entity_that_never_moved_knows_no_water() {
    let (l, _a, _b) = hand_built();
    let terrain = pool_terrain();
    let folded = fold_known_water_one_by_one(&l);
    let stranger = EntityId::new(9_999).expect("9999 is non-zero");
    assert!(folded.state().of(stranger).is_empty());
    assert!(
        folded
            .state()
            .water_at(stranger, WorldTime::from_ticks(9_999_999), &terrain)
            .is_empty()
    );
}

#[test]
fn discarding_known_water_at_every_position_is_unobservable() {
    let (l, _a, _b) = hand_built();
    let resident = fold_known_water_one_by_one(&l);

    let mut chaotic: Folded<KnownWater> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        chaotic = Folded::rebuild_upto(&l, chaotic.position());
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}

#[test]
fn discarding_known_water_at_every_third_position_is_unobservable() {
    let (l, _a, _b) = hand_built();
    let resident = fold_known_water_one_by_one(&l);

    let mut chaotic: Folded<KnownWater> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        if i % 3 == 0 {
            chaotic = Folded::rebuild_upto(&l, chaotic.position());
        }
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}

// ---------------------------------------------------------------------------
// Task 4, step 1: the rule-6 witness.
// ---------------------------------------------------------------------------

/// Spec §3 rule 6, executed on the real thing: does any `believed_water` read
/// run at an instant STRICTLY BEFORE a committed sighting of the same entity?
///
/// That is the exact condition under which an unfiltered set fold — one
/// holding a `BTreeSet<Facet>` of every water room ever visited, advanced to
/// the ledger's end — would answer differently from `believed_water`'s own
/// `day <= t` loop. The coarser question the spec words the rule with ("`t`
/// strictly before the ledger's last committed day") is a proxy for it: it
/// fires on reads where nothing about the answer could have changed, because
/// the entity gained no sighting in the interval. Both are printed; the
/// EXACT one is what the branch was taken on, and it is the one asserted.
///
/// **The branch taken, stated exactly, because the number alone would be read
/// two ways.** On the paths seed 42 actually REACHES — the session's own 568
/// belief lookups, and the present-instant `affect_of_memo_occupied` shape —
/// the exact count is ZERO. [`KnownWater`] carries a first-visit instant
/// anyway, for two reasons neither of which is that number:
///
/// 1. `emitter_arousal` (`liveness.rs`) replays `affect_of` at a creature's
///    own PAST visit day, and that call reaches `believed_water` at that past
///    instant. It is production code, not a test shape; it is merely gated
///    behind a non-empty emitter scan, and seed 42 has no primary-afraid
///    emitter. The sweep below runs exactly that shape and every one of its
///    reads is a past-instant read.
/// 2. `believed_water` is a public function keyed on `t`, and its own
///    `believed_water_only_counts_sightings_at_or_before_t` test commits a
///    sighting in the read's future. A plain set could only answer that by
///    rebuilding the fold to the position `t` implies — the O(history)
///    per-call rebuild spec §2.2 refuses, on the production path, to serve a
///    test.
///
/// So the assertion below is on the SWEEP, not on the session: it says the
/// first-visit filter is exercised in anger somewhere, because a filter that
/// no test ever drives is indistinguishable from dead code.
#[test]
fn rule_six_witness_belief_reads_run_at_past_instants() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..40 {
        session.handle("wait");
        let _ = session.snapshot().expect("seed 42's session snapshots");
    }

    println!("--- rule 6 witness: belief reads (seed 42, 40 waits + snapshots) ---");
    println!(
        "the SESSION itself: {} facts absorbed, {} belief lookups, {} at an instant before \
         a committed sighting",
        session.resident_position(),
        session.resident_belief_lookups(),
        session.resident_beliefs_in_the_past()
    );
    assert!(
        session.committed_fact_count() > 0,
        "the 40-wait seed-42 script must commit facts, or this witness measured nothing"
    );

    // The shape `windows/lab`'s `run_simulation` uses: `affect_of_memo_occupied`
    // per body over one store — first at the PRESENT instant, then at each
    // body's own past visit days, which is the instant `emitter_arousal`
    // (`liveness.rs`, the `hazard_memory_memo -> frightened_at -> alarm_at ->
    // alarm_field -> emitter_arousal -> affect_of` chain) replays affect at.
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let bodies: Vec<hornvale_vessel::body::Body> = session.bodies().to_vec();
    let terrain = RippleTerrain;
    let now = session.day();
    let last_committed_day = ledger.iter().filter_map(|f| f.day).max();

    let present = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        for npc in &bodies {
            let _ = affect_of_memo_occupied(
                &ledger,
                npc,
                &bodies,
                now,
                &terrain,
                &mut afraid,
                None,
                &mut mesh,
                &mut nav,
                &present,
            );
        }
    }
    let present_lookups = present.borrow().witness().belief_lookups();
    let present_past = present.borrow().witness().beliefs_in_the_past();
    println!(
        "the PRESENT read shape (`affect_of_memo_occupied` at {now:?}, {} bodies, ledger's \
         last committed day {last_committed_day:?}): {present_lookups} belief lookups, \
         {present_past} at a past instant",
        bodies.len()
    );

    let past = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    let mut past_calls_before_last_committed_day = 0_u64;
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        for npc in &bodies {
            let days: Vec<WorldTime> = ledger
                .facts_of(npc.entity, AGENT_AT)
                .filter_map(|f| f.day)
                .collect();
            for day in days.into_iter().step_by(37) {
                if last_committed_day.is_some_and(|last| day < last) {
                    past_calls_before_last_committed_day += 1;
                }
                let _ = affect_of_memo_occupied(
                    &ledger,
                    npc,
                    &bodies,
                    day,
                    &terrain,
                    &mut afraid,
                    None,
                    &mut mesh,
                    &mut nav,
                    &past,
                );
            }
        }
    }
    let past_lookups = past.borrow().witness().belief_lookups();
    let past_offenders = past.borrow().witness().beliefs_in_the_past();
    println!(
        "the PAST read shape (the same call at each body's own visit days): {past_lookups} \
         belief lookups, {past_offenders} at an instant before a committed sighting of the \
         same entity; {past_calls_before_last_committed_day} of this sweep's OUTER calls \
         were at a `t` strictly before the ledger's last committed day (the spec's coarser \
         proxy)"
    );
    if let Some((entity, t, sighting)) = past.borrow().witness().first_belief_in_the_past() {
        println!(
            "first past-instant read: entity {entity:?} read at {t:?} with a sighting at {sighting:?}"
        );
    }
    println!(
        "--- verdict: spec §3 rule 6 {} ---",
        if present_past + past_offenders == 0 {
            "found NO past-instant belief read on a reached path"
        } else {
            "FIRED -- a production belief read runs at a past instant, so KnownWater carries \
             a first-visit instant and filters by it"
        }
    );
    println!(
        "NOT counted here, and named so the number is not read as complete: the belief reads \
         NESTED inside `emitter_arousal`'s own `affect_of` go to the THROWAWAY store \
         `affect_of_memo` builds (liveness.rs, `affect_of_memo`), so their witness dies with \
         it. They are past-instant by construction -- `emitter_arousal` passes the visit day \
         it is replaying -- and they are unreached on seed 42, which has no primary-afraid \
         emitter"
    );

    assert!(
        past_lookups > 0,
        "the sweep must actually REACH `believed_water`, or a verdict of zero past-instant \
         reads is zero out of zero and says nothing"
    );
    assert!(
        past_offenders > 0,
        "spec §3 rule 6's branch: `KnownWater` carries a first-visit instant BECAUSE a \
         production read runs at an instant before a committed sighting. If this is ever \
         zero, that filter is unexercised and the tenant is carrying a day map nothing \
         proves it needs"
    );
}

/// Spec §3 rule 6 for the OTHER set-shaped read: does any `hazard_memory_memo`
/// call run at an instant strictly before a committed sighting of the same
/// entity?
///
/// **This is measured here rather than inferred from the belief witness above,
/// and the first draft of this campaign's report did infer it.** That report
/// argued the two counts must agree "call for call" because
/// `affect_of_memo_occupied` passes its `day` to both. That is true of ONE of
/// `hazard_memory_memo`'s five callers. The other four are:
///
/// - `believed_hazard_memo` — a public entry point with its own `t`; no
///   caller anywhere in the tree today.
/// - `hazard_memory` and `believed_hazard` (which delegates to it) — public
///   entry points with their own `t`; every call site in the tree is inside
///   `liveness.rs`'s own test module.
/// - `DriveMovements::step_with_occupancy`'s per-creature preamble and
///   `step_one_with_controller`, both at `t = self.from`.
///
/// And the counts are measured unequal, in the direction the inference did not
/// predict either: on the seed-42 session, 520 hazard lookups against 568
/// belief lookups. (Arithmetically consistent with one hazard read and one
/// `own` belief read per walk, plus 48 extra belief reads from
/// `shared_believed_water`'s per-co-located-peer loop, which makes no hazard
/// read — stated as consistency, not as a separately measured decomposition.)
///
/// The number `LatestVisit` (Task 5) branches on is this one, so it is taken
/// on its own counter. The belief counts are printed beside it precisely so
/// the two caller sets can be seen NOT to agree.
#[test]
fn rule_six_witness_hazard_memory_reads_run_at_past_instants() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..40 {
        session.handle("wait");
        let _ = session.snapshot().expect("seed 42's session snapshots");
    }

    println!("--- rule 6 witness: hazard-memory reads (seed 42, 40 waits + snapshots) ---");
    println!(
        "the SESSION itself: {} facts absorbed, {} hazard lookups, {} at an instant before a \
         committed sighting (and {} belief lookups beside them, {} past-instant -- the two \
         counts are NOT equal, which is the point)",
        session.resident_position(),
        session.resident_hazard_lookups(),
        session.resident_hazards_in_the_past(),
        session.resident_belief_lookups(),
        session.resident_beliefs_in_the_past()
    );
    if let Some((entity, t, sighting)) = session.resident_first_hazard_in_the_past() {
        println!(
            "first past-instant hazard read in the session: entity {entity:?} read at {t:?} \
             with a sighting at {sighting:?}"
        );
    }
    assert!(
        session.resident_hazard_lookups() > 0,
        "the seed-42 session must REACH `hazard_memory_memo` -- it is called from the walk \
         preamble of every tick -- or a verdict of zero past-instant reads is zero out of \
         zero and says nothing"
    );

    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let bodies: Vec<hornvale_vessel::body::Body> = session.bodies().to_vec();
    let terrain = RippleTerrain;
    let now = session.day();

    // Shape 2: `affect_of_memo_occupied` per body at the PRESENT instant --
    // the `windows/lab` `run_simulation` shape. This is the one caller that
    // does pair a hazard read with a belief read at the same `t`.
    let present = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        for npc in &bodies {
            let _ = affect_of_memo_occupied(
                &ledger,
                npc,
                &bodies,
                now,
                &terrain,
                &mut afraid,
                None,
                &mut mesh,
                &mut nav,
                &present,
            );
        }
    }
    println!(
        "the PRESENT read shape (`affect_of_memo_occupied` at {now:?}, {} bodies): {} hazard \
         lookups, {} at a past instant (belief: {} / {})",
        bodies.len(),
        present.borrow().witness().hazard_lookups(),
        present.borrow().witness().hazards_in_the_past(),
        present.borrow().witness().belief_lookups(),
        present.borrow().witness().beliefs_in_the_past()
    );
    assert!(
        present.borrow().witness().hazard_lookups() > 0,
        "the present-instant sweep must reach `hazard_memory_memo`, or its zero says nothing"
    );

    // Shape 3: the same call at each body's own PAST visit days -- the instant
    // `emitter_arousal` replays affect at.
    let past = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        for npc in &bodies {
            let days: Vec<WorldTime> = ledger
                .facts_of(npc.entity, AGENT_AT)
                .filter_map(|f| f.day)
                .collect();
            for day in days.into_iter().step_by(37) {
                let _ = affect_of_memo_occupied(
                    &ledger,
                    npc,
                    &bodies,
                    day,
                    &terrain,
                    &mut afraid,
                    None,
                    &mut mesh,
                    &mut nav,
                    &past,
                );
            }
        }
    }
    let past_lookups = past.borrow().witness().hazard_lookups();
    let past_offenders = past.borrow().witness().hazards_in_the_past();
    println!(
        "the PAST read shape (the same call at each body's own visit days): {past_lookups} \
         hazard lookups, {past_offenders} at a past instant (belief: {} / {})",
        past.borrow().witness().belief_lookups(),
        past.borrow().witness().beliefs_in_the_past()
    );

    // Shape 4: the three public entry points that take their own `t` and have
    // NO production caller. They are unreachable from shapes 1-3, so their
    // count there is a measured zero with a denominator, not an absence of
    // evidence -- and this drives one directly to show the counter does fire
    // on them when something calls them.
    let entry = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    let mut entry_probe_calls = 0_u64;
    for npc in &bodies {
        let first_day = ledger
            .facts_of(npc.entity, AGENT_AT)
            .filter_map(|f| f.day)
            .min();
        if let Some(day) = first_day {
            entry_probe_calls += 1;
            let _ = hornvale_vessel::liveness::hazard_memory(
                &ledger, &entry, npc, day, &terrain, &bodies,
            );
        }
    }
    println!(
        "the ENTRY-POINT shape (`hazard_memory` at each body's FIRST visit day -- a shape \
         nothing in production performs, driven here so the counter is shown to fire): \
         {entry_probe_calls} calls made, {} hazard lookups, {} at a past instant",
        entry.borrow().witness().hazard_lookups(),
        entry.borrow().witness().hazards_in_the_past()
    );

    println!(
        "--- verdict: spec §3 rule 6 for the latest-visit map {} ---",
        if session.resident_hazards_in_the_past()
            + present.borrow().witness().hazards_in_the_past()
            + past_offenders
            == 0
        {
            "found NO past-instant hazard read on any shape measured"
        } else {
            "FIRED -- a hazard read runs at an instant before a committed sighting, so an \
             unfiltered latest-visit map would answer differently there"
        }
    );

    assert!(
        past_lookups > 0,
        "the past-day sweep must REACH `hazard_memory_memo`, or its verdict is zero out of \
         zero"
    );
    assert!(
        entry.borrow().witness().hazard_lookups() == entry_probe_calls,
        "every `hazard_memory` entry-point call must be counted once: {entry_probe_calls} \
         calls made, {} counted",
        entry.borrow().witness().hazard_lookups()
    );
}

// ---------------------------------------------------------------------------
// Task 5, step 1: LatestVisit -- FOLD equals SCAN, the past-instant sweep, and
// the two chaos schedules.
// ---------------------------------------------------------------------------

/// A VERBATIM COPY of `liveness.rs`'s `hazard_memory_memo` `latest` loop — the
/// SCAN half of FOLD equals SCAN for [`LatestVisit`].
///
/// Copied rather than called, for [`scan_oracle`]'s reason and one more: the
/// production loop is GONE as of this task, so this is the only statement of
/// it left anywhere, and nothing regenerates it. The `f64` day is the copy's
/// own — the loop really did fold standard days, and comparing the tenant's
/// tick-keyed answer against it is what pins the retype as well as the fold.
fn latest_visit_scan_oracle(
    ledger: &Ledger,
    entity: EntityId,
    t: WorldTime,
) -> std::collections::BTreeMap<Facet, f64> {
    let mut latest: std::collections::BTreeMap<Facet, f64> = std::collections::BTreeMap::new();
    for f in ledger.facts_of(entity, AGENT_AT) {
        if let Some(fday) = f.day.filter(|d| *d <= t).map(WorldTime::as_std_days)
            && let Value::Text(s) = &f.object
        {
            latest
                .entry(room_from_text_copy(s))
                .and_modify(|d| {
                    if fday > *d {
                        *d = fday;
                    }
                })
                .or_insert(fday);
        }
    }
    latest
}

/// The fold half for [`LatestVisit`], reached through `absorb_at` one fact at
/// a time — the path independent of `advance_to`.
fn fold_latest_visit_one_by_one(ledger: &Ledger) -> Folded<LatestVisit> {
    let mut f: Folded<LatestVisit> = Folded::new();
    for (i, fact) in ledger.iter().enumerate() {
        f.absorb_at(i as u64, fact);
    }
    f
}

/// The tenant's answer lifted to the oracle's `f64`-day shape, so the two
/// compare directly.
fn latest_as_days(
    latest: std::collections::BTreeMap<Facet, WorldTime>,
) -> std::collections::BTreeMap<Facet, f64> {
    latest
        .into_iter()
        .map(|(r, d)| (r, d.as_std_days()))
        .collect()
}

/// Guards the latest-wins assertions from being vacuous: the fixture must
/// visit at least one room TWICE for the same entity, or a fold that kept the
/// FIRST instant (which is exactly what [`KnownWater`] does, one tenant over)
/// would pass every comparison below.
#[test]
fn the_fixture_visits_at_least_one_room_twice() {
    let (l, a, b) = hand_built();
    let mut revisited = 0;
    for e in [a, b] {
        let mut per_room: std::collections::BTreeMap<Facet, usize> =
            std::collections::BTreeMap::new();
        for f in l.facts_of(e, AGENT_AT) {
            if f.day.is_some()
                && let Value::Text(s) = &f.object
            {
                *per_room.entry(room_from_text_copy(s)).or_default() += 1;
            }
        }
        revisited += per_room.values().filter(|n| **n > 1).count();
    }
    assert!(
        revisited > 0,
        "the fixture must visit some room more than once, or LatestVisit's latest-wins \
         rule is never exercised and a first-wins fold would pass every test below"
    );
}

#[test]
fn latest_visit_folded_one_fact_at_a_time_equals_the_scan_oracle() {
    let (l, a, b) = hand_built();
    let folded = fold_latest_visit_one_by_one(&l);
    let far = WorldTime::from_ticks(i64::MAX / 4);
    for e in [a, b] {
        assert_eq!(
            latest_as_days(folded.state().latest_at(e, far)),
            latest_visit_scan_oracle(&l, e, far),
            "the fold's latest-visit map must equal the scan's for {e:?}"
        );
    }
}

/// The past-instant sweep, which is the whole reason this tenant is a VISIT
/// LIST rather than a latest-day map (spec §3 rule 6's fallback, taken on the
/// number `rule_six_witness_hazard_memory_reads_run_at_past_instants`
/// measured: 9 of 9 hazard reads at past instants on the emitter-gated replay
/// shape).
///
/// A latest-day map advanced to the ledger's end cannot answer at any earlier
/// `t` at all; this asserts the list does, at EVERY instant the fixture can
/// distinguish — each committed day, and the instants either side of it.
#[test]
fn latest_visit_at_every_past_instant_equals_the_oracle_at_that_instant() {
    let (l, a, b) = hand_built();
    let folded = fold_latest_visit_one_by_one(&l);

    let mut instants: Vec<WorldTime> = Vec::new();
    for f in l.iter() {
        if let Some(d) = f.day {
            for delta in [-1_i64, 0, 1] {
                instants.push(WorldTime::from_ticks(d.ticks() + delta));
            }
        }
    }
    instants.push(WorldTime::GENESIS);
    instants.sort();
    instants.dedup();

    let mut non_empty = 0;
    let mut distinct_answers: std::collections::BTreeSet<Vec<(Facet, i64)>> =
        std::collections::BTreeSet::new();
    for e in [a, b] {
        for t in &instants {
            let got = folded.state().latest_at(e, *t);
            if !got.is_empty() {
                non_empty += 1;
            }
            distinct_answers.insert(got.iter().map(|(r, d)| (r.clone(), d.ticks())).collect());
            assert_eq!(
                latest_as_days(got),
                latest_visit_scan_oracle(&l, e, *t),
                "the fold's latest-visit map at {t:?} must equal the scan truncated there \
                 for {e:?}"
            );
        }
    }
    assert!(
        non_empty >= instants.len(),
        "the sweep must reach non-empty answers on most of its {} instants, or it is \
         comparing two empty maps: only {non_empty} were non-empty",
        instants.len()
    );
    assert!(
        distinct_answers.len() >= 4,
        "the sweep must see the map GROW across its instants, or a fold ignoring `t` \
         entirely would pass: {} distinct answers",
        distinct_answers.len()
    );
}

#[test]
fn a_room_visited_twice_reports_its_latest_visit_not_its_first() {
    let (l, a, _b) = hand_built();
    let folded = fold_latest_visit_one_by_one(&l);
    // `SCRIPT` posts entity `a` to (face 0, [0]) at ticks 700_000 and 300_000.
    let twice = room(0, &[0]);
    let far = WorldTime::from_ticks(i64::MAX / 4);
    let latest = folded.state().latest_at(a, far);
    assert_eq!(
        latest.get(&twice).copied(),
        Some(WorldTime::from_ticks(700_000)),
        "the LATEST visit wins, not the first — the whole staleness rule the hazard fold \
         reads this map for"
    );
    // And at an instant between the two visits, the EARLIER one is the answer.
    let between = WorldTime::from_ticks(500_000);
    assert_eq!(
        folded.state().latest_at(a, between).get(&twice).copied(),
        Some(WorldTime::from_ticks(300_000)),
        "at an instant between the two visits the earlier one is the most recent"
    );
    // The distinct-rooms read agrees about membership at both instants.
    assert!(folded.state().rooms_at(a, between).contains(&twice));
    assert!(
        !folded
            .state()
            .rooms_at(a, WorldTime::from_ticks(200_000))
            .contains(&twice),
        "a room is not in the set before its FIRST visit"
    );
}

#[test]
fn an_entity_that_never_moved_has_no_visits() {
    let (l, _a, _b) = hand_built();
    let folded = fold_latest_visit_one_by_one(&l);
    let stranger = EntityId(std::num::NonZeroU64::new(9_999_999).expect("non-zero"));
    assert!(folded.state().of(stranger).is_empty());
    assert!(
        folded
            .state()
            .latest_at(stranger, WorldTime::from_ticks(i64::MAX / 4))
            .is_empty()
    );
    assert!(
        folded
            .state()
            .rooms_at(stranger, WorldTime::from_ticks(i64::MAX / 4))
            .is_empty()
    );
}

#[test]
fn discarding_latest_visit_at_every_position_is_unobservable() {
    let (l, _a, _b) = hand_built();
    let resident = fold_latest_visit_one_by_one(&l);

    let mut chaotic: Folded<LatestVisit> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        chaotic = Folded::rebuild_upto(&l, chaotic.position());
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}

#[test]
fn discarding_latest_visit_at_every_third_position_is_unobservable() {
    let (l, _a, _b) = hand_built();
    let resident = fold_latest_visit_one_by_one(&l);

    let mut chaotic: Folded<LatestVisit> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        if i % 3 == 0 {
            chaotic = Folded::rebuild_upto(&l, chaotic.position());
        }
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}

/// The retype the hazard fold's day made when it stopped round-tripping
/// through `f64` standard days.
///
/// The old loop stored `WorldTime::as_std_days()` and handed the result back
/// to `WorldTime::from_std_days` at every use; the new one carries the instant
/// itself. The two agree only if that round trip is the identity, and it is at
/// every reachable magnitude — `from_std_days` rounds `(days × 100 000)` to
/// the nearest tick, and the division-then-multiplication error stays far
/// below half a tick until `|ticks|` approaches `2^53`. This asserts it on the
/// instants the sim actually produces (a real session's committed days, which
/// run to ~7×10^10 ticks) rather than on a hand-picked few.
#[test]
fn the_tick_to_standard_day_round_trip_is_exact_at_every_instant_the_hazard_fold_reads() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..5 {
        session.handle("wait");
    }
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let days: Vec<WorldTime> = ledger.iter().filter_map(|f| f.day).collect();
    assert!(
        days.len() > 100,
        "the session must commit enough dated facts for this to mean anything: {}",
        days.len()
    );
    let biggest = days.iter().map(|d| d.ticks().abs()).max().unwrap_or(0);
    assert!(
        biggest > 1_000_000_000,
        "the sweep must reach the magnitudes the sim really uses, or it proves the round \
         trip only near genesis: largest |ticks| seen was {biggest}"
    );
    for d in days {
        assert_eq!(
            WorldTime::from_std_days(d.as_std_days()).expect("a session day is finite"),
            d,
            "the tick -> standard-day -> tick round trip must be exact at {d:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// Task 5, step 2: the one place Trail's order and the ledger's could part.
// ---------------------------------------------------------------------------

/// `build_emitter_scan`'s timeline used to be sorted by day ALONE, stably, so
/// sightings sharing an instant kept COMMIT order; `Trail` sorts by
/// `(day, room)`, so they are in ROOM order there. `position_at` reads the last
/// entry with `day <= q`, so the two answers differ exactly when one entity has
/// two sightings at the SAME instant in DIFFERENT rooms.
///
/// This pins that the divergence is REAL rather than hypothetical, on the
/// hand-built fixture that deliberately commits such a pair out of room order
/// — so nobody has to rediscover it — and the test one below measures that the
/// walk cannot produce one.
#[test]
fn a_same_day_pair_committed_in_descending_room_order_is_where_the_two_orders_part() {
    let (l, a, _b) = hand_built();
    let day = WorldTime::from_ticks(300_000);

    // Commit order at that instant, which is what the old day-only stable sort
    // preserved and what `agent_position` still reads.
    let commit_order: Vec<Facet> = l
        .facts_of(a, AGENT_AT)
        .filter(|f| f.day == Some(day))
        .filter_map(|f| match &f.object {
            Value::Text(s) => Some(room_from_text_copy(s)),
            _ => None,
        })
        .collect();
    assert!(
        commit_order.len() > 1,
        "the fixture must commit more than one sighting of {a:?} at {day:?}, or this test \
         measures nothing: {commit_order:?}"
    );

    let trail = fold_one_by_one(&l);
    let upto = trail.state().prefix_len(a, day);
    let trail_last = trail.state().of(a)[upto - 1].1.clone();
    let commit_last = commit_order.last().expect("checked non-empty").clone();
    assert_ne!(
        trail_last, commit_last,
        "the fixture must commit its same-instant pair OUT of room order, or the two \
         orderings agree here by accident and the divergence this test names is untested"
    );
    assert_eq!(
        trail_last,
        commit_order
            .iter()
            .max()
            .expect("checked non-empty")
            .clone(),
        "the trail's answer at a same-instant tie is the LARGEST room, because it sorts \
         by (day, room)"
    );
}

/// The other half: the walk cannot commit such a pair, so the divergence above
/// is unreachable in production.
///
/// `WalkState` advances `st.day` by `clock::cost_of` — which floors at one
/// tick — before every emitted `agent-at`, so a walker's sightings are strictly
/// increasing in day. That is a structural argument; this measures it, with
/// the denominator, on a real session.
#[test]
fn the_walk_never_commits_two_sightings_of_one_entity_at_one_instant() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    for _ in 0..20 {
        session.handle("wait");
    }
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");

    let subjects: std::collections::BTreeSet<EntityId> = ledger
        .iter()
        .filter(|f| f.predicate == AGENT_AT)
        .map(|f| f.subject)
        .collect();
    let mut pairs = 0_u64;
    let mut same_instant = 0_u64;
    let mut same_instant_different_room = 0_u64;
    for e in &subjects {
        let sightings: Vec<(WorldTime, Facet)> = ledger
            .facts_of(*e, AGENT_AT)
            .filter_map(|f| match (&f.day, &f.object) {
                (Some(d), Value::Text(s)) => Some((*d, room_from_text_copy(s))),
                _ => None,
            })
            .collect();
        for w in sightings.windows(2) {
            pairs += 1;
            if w[0].0 == w[1].0 {
                same_instant += 1;
                if w[0].1 != w[1].1 {
                    same_instant_different_room += 1;
                }
            }
        }
    }
    println!(
        "--- Trail order versus commit order (seed 42, 20 waits, {} subjects) ---\n\
         {pairs} adjacent sighting pairs, {same_instant} at the same instant, \
         {same_instant_different_room} at the same instant in DIFFERENT rooms",
        subjects.len()
    );
    assert!(
        pairs > 100,
        "the script must commit enough sightings for this to be a measurement: {pairs} \
         adjacent pairs"
    );
    assert_eq!(
        same_instant_different_room, 0,
        "a walker committed two sightings at one instant in different rooms, so the \
         Trail's (day, room) order and the ledger's commit order CAN disagree on the \
         emitter scan's `position_at` — see the test above for what that costs"
    );
}

// ---------------------------------------------------------------------------
// Task 5, step 4: H4's cost witness.
// ---------------------------------------------------------------------------

/// One measurement of a hazard read's integration cost: the segments summed,
/// the past-day replays performed, and the ledger position it was taken at.
struct HazardCost {
    /// Integral segments summed inside the hazard read.
    segments: u64,
    /// Past-day affect replays performed inside it.
    replays: u64,
    /// How many facts the store had absorbed when it was taken.
    position: u64,
    /// How many facts the ROSTER has committed by then — the history the fold
    /// would have walked, and the quantity the scaling claim is really about.
    ///
    /// The store's `position` is NOT that quantity and reading it as one is a
    /// trap this witness fell into first: a fresh session's ledger already
    /// holds ~23,000 facts of world history before a single creature moves, so
    /// 150 turns of walking move it by under 2%, and a guard demanding the
    /// ledger double would fail on a session whose walk history quadrupled.
    trail_facts: usize,
}

/// Walk `session` to `turns`, then take ONE whole-roster hazard read and
/// measure what it integrated.
///
/// **A roster pass rather than a single `hazard_memory_memo` call, and the
/// difference is deliberate.** The scaling question is the same either way,
/// and a single call would have to name its creature BEFORE the run — but
/// which body reaches the past-day replay is a property of the world, not
/// something a test can choose in advance, so pinning one would be a good way
/// to measure a body that never enters the path. The pass shares one
/// `PrimaryAfraidMemo` across the roster exactly as the tick does, so it is
/// also the shape the sim actually pays for.
fn hazard_cost_at(session: &mut Session<'_>, turns: usize) -> HazardCost {
    for _ in 0..turns {
        session.handle("wait");
    }
    let trail_facts: usize = session
        .bodies()
        .iter()
        .map(|b| session.committed_fact_count_for(b.entity))
        .sum();
    let before_segments = session.resident_segments_integrated();
    let before_replays = session.resident_alarm_replays();
    let _ = session.hazard_memories();
    HazardCost {
        segments: session.resident_segments_integrated() - before_segments,
        replays: session.resident_alarm_replays() - before_replays,
        position: session.resident_position(),
        trail_facts,
    }
}

/// H4's cost witness: the hazard fold's integration work does not scale with
/// the tick index.
///
/// The Tailrace measured `hazard_memory_memo` at 73–97 ms/call at its final
/// band with elasticity 1.06–1.21 against history — the most expensive fold in
/// the stack, and history-proportional. Two mechanisms produced that (spec §1):
/// a `latest` map rebuilt from every `agent-at` fact the creature ever
/// committed, and an emitter scan that rebuilt every roster member's whole
/// timeline. Both are now reads off the resident store, so what remains is the
/// PAST-DAY affect replay, whose own sustenance reads resume from a reset.
///
/// Stated as a COUNT rather than a duration, for the reason the Task-3 witness
/// beside it gives: a timing here would measure the box.
///
/// **Both shapes are measured and only one is asserted on.** Seed 42 never
/// reaches the past-day replay at all (see `EMITTER_SEED`'s doc in
/// `ledger_hash_witness.rs` for what it does and does not have), so its
/// number would be a claim about the terrain-only path. The assertion is on
/// the emitter-bearing world, with the replay count asserted non-zero at both
/// tick indices so the ratio cannot be a ratio of two untaken branches.
#[test]
fn the_hazard_folds_integration_does_not_grow_with_the_tick_index() {
    const EARLY: usize = 50;
    const LATE: usize = 200;

    println!("--- H4 cost witness: segments integrated inside one hazard read ---");

    // Shape 1, printed not asserted: seed 42, the terrain-only path.
    let plain = common::build(42).expect("seed 42 always builds a world");
    let (mut plain_session, _) =
        Session::start(&plain, &PossessOpts::default()).expect("seed 42 always starts a session");
    let plain_early = hazard_cost_at(&mut plain_session, EARLY);
    let plain_late = hazard_cost_at(&mut plain_session, LATE - EARLY);
    println!(
        "seed 42 (NO past-day replay): turn {EARLY} {} segments over {} roster facts \
         ({} replays); turn {LATE} {} segments over {} roster facts ({} replays) -- \
         segment ratio {:.3} against a {:.2}x longer roster history",
        plain_early.segments,
        plain_early.trail_facts,
        plain_early.replays,
        plain_late.segments,
        plain_late.trail_facts,
        plain_late.replays,
        plain_late.segments as f64 / plain_early.segments.max(1) as f64,
        plain_late.trail_facts as f64 / plain_early.trail_facts.max(1) as f64
    );

    // Shape 2, the one asserted on: the emitter-bearing world, whose hazard
    // fold really does replay an emitter's affect at a past visit day.
    let (seed, world) = common::world_where(
        "the hazard fold replays an emitter's affect at a past visit day",
        |session| {
            for _ in 0..8 {
                session.handle("wait");
            }
            session.resident_alarm_replays() > 0
        },
    );
    let (mut session, _) = Session::start(&world, &PossessOpts::default())
        .expect("the found world always starts a session");
    let early = hazard_cost_at(&mut session, EARLY);
    let late = hazard_cost_at(&mut session, LATE - EARLY);
    println!(
        "seed {seed} (past-day replay LIVE): turn {EARLY} {} segments over {} roster \
         facts ({} replays); turn {LATE} {} segments over {} roster facts ({} replays) -- \
         segment ratio {:.3} against a {:.2}x longer roster history (store positions {} \
         and {}, most of which is world history that predates the walk)",
        early.segments,
        early.trail_facts,
        early.replays,
        late.segments,
        late.trail_facts,
        late.replays,
        late.segments as f64 / early.segments.max(1) as f64,
        late.trail_facts as f64 / early.trail_facts.max(1) as f64,
        early.position,
        late.position
    );

    // THE GROWTH GUARD, and its threshold is stated rather than round. The
    // claim's axis is the TICK INDEX, which is 4x here by construction (50
    // against 200) and so guards nothing on its own; what has to be true for
    // the comparison to mean anything is that real history accrued in
    // between. An earlier draft demanded the roster's history DOUBLE and
    // failed on a world whose creatures simply walk less (1.48x over the same
    // 150 turns) — a fact about that world, not about the fold. So the guard
    // is an absolute floor on facts accrued, which does not assume how busy a
    // given world's roster is, and the ratio is printed beside it.
    const MIN_FACTS_ACCRUED: usize = 200;
    assert!(
        late.trail_facts >= early.trail_facts + MIN_FACTS_ACCRUED,
        "the roster must commit at least {MIN_FACTS_ACCRUED} more facts between the two \
         samples, or 'does not scale with the tick index' is untested: {} facts then {}",
        early.trail_facts,
        late.trail_facts
    );
    assert!(
        early.replays > 0 && late.replays > 0,
        "the measured hazard read must enter the PAST-DAY replay at BOTH tick indices, or \
         the ratio compares two untaken branches: {} replays early, {} late",
        early.replays,
        late.replays
    );
    assert!(
        early.segments > 0,
        "the measured hazard read must integrate something early, or the ratio is vacuous"
    );
    assert!(
        late.segments as f64 <= early.segments as f64 * 1.5,
        "the hazard fold's integration must not scale with the tick index: {} segments at \
         turn {EARLY} against {} at turn {LATE} (allowance 1.5x), with the roster's own \
         history {:.2}x longer",
        early.segments,
        late.segments,
        late.trail_facts as f64 / early.trail_facts.max(1) as f64
    );
}
