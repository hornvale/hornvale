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
use hornvale_vessel::resident::{ResidentFolds, Trail};
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
        }
    }

    // And after the whole script (plus its two ignorable trailing facts), both
    // agree with a from-scratch fold of the full ledger.
    let scan = fold_one_by_one(&full);
    let _ = resident.trail(&full);
    let _ = chaotic.trail(&full);
    for e in [a, b] {
        assert_eq!(resident.trail(&full).of(e), scan.state().of(e));
        assert_eq!(chaotic.trail(&full).of(e), scan.state().of(e));
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
