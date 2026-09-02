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
use hornvale_vessel::resident::{KnownWater, ReadWitness, ResidentFolds, Trail};
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
/// # The finding this witness produced first
///
/// **The seed-42 possession never calls `drive_at` or `hunger_at` at all.**
/// Their only production caller is `affect_of_memo_occupied`, which
/// `Session::snapshot` and `Session::needs` reach once per CO-LOCATED
/// creature — and seed 42's flagship stands alone: `possess --seed 42` with a
/// `needs` in the script answers "No one else is here to read." The session's
/// tick walks nine creatures and commits twenty-two thousand facts, but every
/// one of those reads goes through `decide_step`, which carries the walk's own
/// reset local and makes no unfiltered lookup. So the session is measured
/// here, and its zero is REPORTED rather than asserted on.
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
    assert_eq!(
        session.resident_reset_lookups(),
        0,
        "the finding this witness recorded: seed 42's possession is never co-located with \
         another creature, so `affect_of_memo_occupied` -- the ONLY production caller of \
         `drive_at`/`hunger_at` -- never runs. If this ever becomes non-zero the session \
         has gained a reached path and the sweep below is no longer the whole answer"
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
    // visit day, which is where an unfiltered reset lookup would differ -- but that
    // chain is gated behind a non-empty emitter scan, and seed 42 is
    // emitter-free, so no such call happens today. Stage 2 threads the store
    // through that chain; this line records, now, what it will find.
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
            let (trail, resets, _w) = store.trail_and_thirst(&l);
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
        let (trail, resets, _w) = store.trail_and_thirst(&l);
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
            let (trail, resets, _w) = store.trail_and_thirst(&partial);
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
fn sustenance_discard_schedule(every: usize) {
    let (l, e, _home) = sustenance_fixture();
    let mut resident = ResidentFolds::new();
    let mut chaotic = ResidentFolds::new();

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
    }
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
    // And the oracle must actually drop something at the whole-history instant.
    for e in [a, b] {
        let scanned = known_water_scan_oracle(&l, e, far, &terrain);
        assert!(
            scanned.len() < visited.len(),
            "the oracle must drop at least one visited room as dry for {e:?}, or the \
             filter is vacuous"
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
        for t in probes {
            let scanned: Vec<Facet> = known_water_scan_oracle(&l, e, t, &terrain)
                .into_iter()
                .collect();
            assert_eq!(
                folded.state().water_at(e, t, &terrain),
                scanned,
                "the fold's water set for {e:?} at {t:?} must equal the oracle's at that \
                 instant -- this is spec §3 rule 6's first-visit filter"
            );
        }
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
