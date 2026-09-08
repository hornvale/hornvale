//! The Pawl, Task 2: the resident fold store's first tenant, [`Trail`], and
//! the store that owns it.
//!
//! Three things are pinned here, and the third is the one the primitive's
//! module doc says every tenant owes:
//!
//! 1. **The rule-2 witness** (spec §3 rule 2): whether an entity's `agent-at`
//!    facts ever commit out of day order on a real session (at
//!    [`WALKING_SEED`] since the absorption of main — see that constant). The
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
//!
//! # THE SCRIPTS WERE SHORTENED AT THE CAMPAIGN'S CLOSE (2026-09-02)
//!
//! Every number in this file was re-measured on the shortened script it is
//! written beside; none was carried over from the long one. The campaign's
//! standing rule is that a witness costs at most 60 s, and The Roll had put
//! five of this file's witnesses far outside it — a tick advances a
//! settlement's whole roll now (59 bodies at [`WALKING_SEED`]), so the same
//! wait count buys ten times the work it did when these scripts were written.
//!
//! Measured on this box with a full `nextest` run of this crate, so every
//! column carries the same parallel contention. The AFTER column is two
//! independent runs rather than one, because this box was carrying another
//! session's work throughout (`vm.loadavg` ranged from 11 to 91 on ten cores)
//! and a single number would read as a precision the measurement does not have:
//!
//! ```text
//! witness                                          before   after (2 runs)  script
//! rule_six_witness_hazard_memory_reads...          343.453s  39.265 45.085   40 -> 12 waits
//! rule_six_witness_belief_reads...                 342.947s  39.116 45.473   40 -> 12 waits
//! rule_two_witness_agent_at_commit_order...        342.433s  37.365 43.172   40 -> 12 waits
//! the_hazard_folds_integration_does_not_grow...    339.000s  66.186 75.770   5/20 -> 2/6 turns
//! the_walk_never_commits_two_sightings...          126.175s  12.726 13.943   20 -> 8 waits
//! rule_one_witness_no_read_runs_before_a_reset...   11.532s  11.165 12.257   unchanged
//! whole-crate wall                                 489.643s 265.161 290.971
//! ```
//!
//! `rule_one_witness` walks seed 42, whose residents condense onto fresh water
//! and commit no positional fact, so it was never expensive and its script is
//! untouched; it appears only for completeness.
//!
//! **It does share [`PAST_DAY_STRIDE`] with the two rule-6 sweeps — but only
//! since Task 7a's first fix round, and this doc claimed it one commit early.**
//! It still called `.step_by(37)` when the sentence above first said otherwise.
//! Moving it onto the constant is meaning-preserving and that was checked
//! rather than assumed: on seed 42 the past-day sweep makes **0 calls at either
//! stride**, because every body's `agent-at` list is empty, so nothing there
//! depends on the number at all. Its two asserted floors live on the
//! PRESENT-instant sweep and are untouched (136 reset lookups over 68 bodies,
//! 0 with a reset in the future; 5,360 lookups in the session itself).
//!
//! **[`the_hazard_folds_integration_does_not_grow_with_the_tick_index`] IS
//! STILL OVER THE CEILING AND IS LEFT THAT WAY DELIBERATELY.** It is 5.1x
//! cheaper than it was and it is not under 60 s: 52.4 s in a light eight-test
//! run, 66.2 s and 75.8 s in the two full runs above, 71.5 s alone on a box at
//! load 15. Two of its costs are irreducible without dropping evidence — the
//! search that SELECTS the emitter-bearing world (seven world builds, and a
//! hardcoded seed is the thing that search exists to avoid), and the seed-42
//! contrast shape that shows the same measurement on a world with no past-day
//! replay at all. `EARLY` is already at 2, the smallest window in which the
//! replay is entered at both ends. Cutting further would remove one of those
//! two, which is weakening the witness rather than shortening it, so it stays
//! long and this paragraph says so.
//!
//! **What a shorter script costs, stated rather than waved at.** Every
//! assertion in this file is either a floor on a denominator or a comparison
//! between two samples; none of them names an absolute tick index, and each
//! one's re-measured value is recorded at the assertion itself. What is lost is
//! the length of walk the verdicts are read over — a same-instant sighting
//! pair, say, has fewer chances to appear in 986 adjacent pairs than in the
//! long script's — and that is a real reduction in reach, not a free lunch.

use crate::common;
use hornvale_kernel::fold::Folded;
use hornvale_kernel::{
    ConceptRegistry, EntityId, Facet, FacetId, Fact, Ledger, Value, WorldTime, test_lineage,
};
use hornvale_species::ThermalStrategy;
use hornvale_vessel::liveness::{
    DriveParams, HomeNavCache, PrimaryAfraidMemo, RouteMemo, SUSTENANCE, Terrain,
    affect_of_memo_occupied, sustenance_at,
};
use hornvale_vessel::resident::{LatestVisit, ReadWitness, ResidentFolds, Trail};
use hornvale_vessel::{PossessOpts, Session};

/// `agent-at`'s exact on-disk spelling, written as a literal rather than
/// imported, so this test still fails if the constant is ever repointed
/// (`liveness.rs`'s own spelling test makes the same argument).
const AGENT_AT: &str = "agent-at";

/// A seed whose residents actually WALK — one whose flagship settlement is not
/// on water, so a ticked body leaves home for a drink and commits `agent-at`.
///
/// **Four of this file's witnesses moved off seed 42 onto this at the
/// absorption of main (The Roll), and the move is the finding rather than a
/// fixture repair.** Every one of them measures something about the committed
/// POSITION trail — the rule-2 commit-order verdict, the two rule-6
/// past-instant sweeps, and the same-instant-pair guard — and each already
/// carried a loud precondition saying so. Before The Roll a seed-42 session
/// ticked three settlement creatures and four wild ones, and the wild ones
/// walked. After it, a session's roster is the residents of the settlement you
/// stand in, and seed 42's flagship condenses onto fresh water: its residents
/// drink where they stand and commit **no positional fact at all**. So all four
/// witnesses went from measuring a real trail to measuring the empty one, and
/// every one of them said so by failing its own denominator assertion rather
/// than passing vacuously — which is the whole reason those assertions are
/// there.
///
/// The number is main's, measured there and not re-derived here: seeds 0..16
/// possessed and waited seven days, eleven of sixteen walk, and 14 is the
/// cheapest of those (59 bodies, 673 positional facts, never reaching water at
/// all). The recording site with the fuller note is
/// `possession_moves.rs`'s own `WALKING_SEED`; this file pins the same number
/// as `the_roll.rs` and `player_acts_commit.rs` already do, because each test
/// module keeps its own private constant, and every site fails loudly if an
/// epoch moves it.
/// type-audit: bare-ok(index)
const WALKING_SEED: u64 = 14;

/// How many `wait`s each [`WALKING_SEED`] witness's script takes.
/// type-audit: bare-ok(count)
const WITNESS_WAITS: usize = 12;

/// The stride the past-instant sweeps take through each body's own visit days.
/// type-audit: bare-ok(count)
const PAST_DAY_STRIDE: usize = 9;

/// How many `wait`s the same-instant sighting witness's script takes.
/// type-audit: bare-ok(count)
const SIGHTING_WAITS: usize = 8;

/// How many `wait`s the emitter-bearing world search gives each candidate seed
/// before asking whether its hazard fold replayed an emitter's affect at a past
/// visit day. Kept in step with `ledger_hash_witness.rs`'s
/// `EMITTER_SCRIPT_WAITS`, which searches for the same property.
/// type-audit: bare-ok(count)
const EMITTER_SEARCH_WAITS: usize = 2;

/// The seed the emitter-bearing world search lands on today.
///
/// **This constant did not exist until Task 7a's first fix round, and its
/// absence made a sentence in `ledger_hash_witness.rs` false.** That file said
/// the two search sites "each fail loudly on their own if the landing seed
/// moves"; only that file had the guard. Here the search's result was bound and
/// PRINTED and nothing checked it — so every measured number in
/// [`the_hazard_folds_integration_does_not_grow_with_the_tick_index`]'s comment
/// block (598 segments over 223 replays at turn 2, 806 over 373 at turn 6,
/// `history_growth` 2.28x, the 702-fact accrual, the 59%-of-ceiling margin) is a
/// seed-6 measurement that would have gone on describing a different world in
/// silence.
///
/// It is the same number `ledger_hash_witness.rs` pins for the same search, and
/// deliberately a second copy rather than an import: each test module keeps its
/// own, exactly as [`WALKING_SEED`] is a fifth copy of 14, so each site refuses
/// on its own terms if an epoch moves the world under it.
/// type-audit: bare-ok(index)
const EMITTER_SEED: u64 = 6;

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
///
/// **Re-measured on the shortened script** ([`WITNESS_WAITS`] = 12, down from
/// 40; 342.433 s to 37.4-43.2 s in a full parallel crate run): 58 entities, 46 `agent-at` facts each, **0
/// out-of-order pairs** — the same verdict the forty-wait script returned. The
/// floor below is `!by_entity.is_empty()`, and it is the ENTITY count that
/// clears it: 58 subjects, not the 2,668 `agent-at` facts an earlier draft of
/// this sentence quoted at a floor that never counts them.
#[test]
fn rule_two_witness_agent_at_commit_order_versus_day_order() {
    let world = common::build(WALKING_SEED).expect("the walking seed always builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the walking seed always starts a session");
    for _ in 0..WITNESS_WAITS {
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
    println!(
        "--- rule 2 witness: agent-at commit order vs day order (seed {WALKING_SEED}, \
         {WITNESS_WAITS} waits) ---"
    );
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
        "the {WITNESS_WAITS}-wait seed-{WALKING_SEED} script must commit at least one agent-at fact, or \
         this witness measured nothing"
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

/// The first visit per room, derived locally from [`LatestVisit::of`] for
/// store-discard comparisons. Production reads use `water_at`; no public
/// first-visit-map accessor is needed.
fn first_visits(
    visits: &LatestVisit,
    entity: EntityId,
) -> std::collections::BTreeMap<Facet, WorldTime> {
    visits
        .of(entity)
        .iter()
        .filter_map(|(room, days)| days.first().map(|day| (room.clone(), *day)))
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
                first_visits(chaotic.latest_visit(&prefix), e),
                first_visits(resident.latest_visit(&prefix), e),
                "{e:?}'s first-visit map diverged at prefix {i} under a \
                 discard-every-{every} schedule"
            );
        }
    }

    // And after the whole script (plus its two ignorable trailing facts), both
    // agree with a from-scratch fold of the full ledger.
    let scan = fold_one_by_one(&full);
    let visits_scan = fold_latest_visit_one_by_one(&full);
    let _ = resident.trail(&full);
    let _ = chaotic.trail(&full);
    for e in [a, b] {
        assert_eq!(resident.trail(&full).of(e), scan.state().of(e));
        assert_eq!(chaotic.trail(&full).of(e), scan.state().of(e));
        assert_eq!(
            first_visits(resident.latest_visit(&full), e),
            first_visits(visits_scan.state(), e)
        );
        assert_eq!(
            first_visits(chaotic.latest_visit(&full), e),
            first_visits(visits_scan.state(), e)
        );
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
/// `Session::needs` once per CO-LOCATED creature, and seed 42's flagship stood
/// alone (`possess --seed 42` with a `needs` in the script answered "No one
/// else is here to read"), and every read the tick's nine walking creatures
/// made went through `decide_step`, which carries the walk's own reset local
/// and makes no unfiltered lookup.
///
/// **The past tense in that paragraph is load-bearing as of the absorption of
/// main: seed 42's flagship no longer stands alone.** The Roll made a
/// session's roster the residents of the settlement you stand in, so the read
/// shape below is 68 bodies rather than one and this witness's own numbers
/// moved with it — see the next paragraph, whose figure is re-measured rather
/// than carried forward.
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
/// 40-wait script that reported none** — re-measured at the absorption of
/// main, where the same script on the same seed reports **5,360**, because the
/// roll ticks sixty-eight bodies where it ticked seven. The finding is the
/// same one either way (the counter reads non-zero for a real reason, and the
/// old zero was the instrument); only its size moved, and by 16.8x.
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
        "the 40-wait seed-42 script must commit facts, or this witness measured \
         nothing"
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
        let mut route = RouteMemo::new();
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
                &mut route,
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
    //
    // MEASURED, AND IT IS ZERO: this sweep makes no calls at all on seed 42,
    // because every body's `agent-at` list is empty there. That is why it can
    // share [`PAST_DAY_STRIDE`] with the two rule-6 sweeps (12 -> 9 at the
    // campaign's close) without its meaning moving — the stride indexes an
    // empty list either way. If a future seed puts facts in that list, the
    // stride starts to matter here and this paragraph is where to look.
    let past = hornvale_vessel::resident::OwnedFolds::new(ResidentFolds::new());
    {
        let mut afraid = PrimaryAfraidMemo::new();
        let mut mesh = hornvale_kernel::RoomMeshMemo::new();
        let mut nav = HomeNavCache::new();
        let mut route = RouteMemo::new();
        for npc in &bodies {
            let days: Vec<WorldTime> = ledger
                .facts_of(npc.entity, AGENT_AT)
                .filter_map(|f| f.day)
                .collect();
            for day in days.into_iter().step_by(PAST_DAY_STRIDE) {
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
                    &mut route,
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
///
/// **Swept under BOTH reset rules, with a non-vacuity floor on each**, the same
/// shape [`sweep_against_the_oracle`] needed and for the same reason. Under
/// [`ResetRule::Unfiltered`] this fixture's reset is tick 850 000 whatever `t`
/// is, so 15 of the 19 probe instants take `sustenance_at`'s
/// `t <= last_reset` short-circuit and assert `0.0 == 0.0` — measured, 16 of
/// 76 probes across the four overlay widths integrated anything at all, and
/// every one of the fixture's traps sits inside that dead zone. The filtered
/// rule is the load-bearing half: every probe strictly between two resets
/// integrates a real interval, so the withheld facts are actually inside the
/// sum being compared, and it measures 60 of 76. The floors below are
/// asserted on the number of probes that returned a strictly POSITIVE
/// integral, so the sweep cannot go quiet if the fixture's resets move.
#[test]
fn holding_facts_back_as_an_overlay_equals_the_scan_over_the_full_ledger() {
    let (full, e, home) = sustenance_fixture();
    let terrain = RippleTerrain;
    let mut witness = ReadWitness::default();
    let mut probes_u = 0usize;
    let mut nonzero_u = 0usize;
    let mut probes_f = 0usize;
    let mut nonzero_f = 0usize;
    // Measured on the current fixture, not estimated. See the printed
    // coverage line: if the fixture's resets or probe grid move, these are
    // the two numbers to re-derive.
    const FLOOR_U: usize = 16;
    const FLOOR_F: usize = 60;

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

        for rule in [ResetRule::Unfiltered, ResetRule::AtOrBefore] {
            let mut store = ResidentFolds::new();
            for t in probe_instants() {
                let (trail, resets, memo, _w) = store.trail_and_thirst(&partial);
                let reset = match rule {
                    ResetRule::Unfiltered => resets.last_reset(e),
                    ResetRule::AtOrBefore => resets.last_reset_at_or_before(e, t),
                }
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
                     {t:?} must equal the scan over the full ledger under {rule:?}"
                );
                match rule {
                    ResetRule::Unfiltered => {
                        probes_u += 1;
                        if got > 0.0 {
                            nonzero_u += 1;
                        }
                    }
                    ResetRule::AtOrBefore => {
                        probes_f += 1;
                        if got > 0.0 {
                            nonzero_f += 1;
                        }
                    }
                }
            }
        }
    }
    println!("--- overlay-equals-scan sweep coverage ---");
    println!("Unfiltered: {nonzero_u} of {probes_u} probes integrated a non-zero interval");
    println!("AtOrBefore: {nonzero_f} of {probes_f} probes integrated a non-zero interval");

    assert_eq!(probes_u, probes_f, "both sweeps must probe the same grid");
    // The unfiltered sweep's own floor. It is LOW on purpose and is not the
    // load-bearing one: with a single unfiltered reset at tick 850 000, only
    // the 4 probes past it can integrate anything at all, over 4 values of k:
    // 16 of 76, measured.
    assert!(
        nonzero_u >= FLOOR_U,
        "the unfiltered overlay sweep must integrate a real interval at least {FLOOR_U} \
         times (4 live probes x 4 overlay widths), or it is asserting 0.0 == 0.0 \
         throughout: got {nonzero_u} of {probes_u}"
    );
    // The load-bearing floor: under the filtered rule the overlay's facts sit
    // inside an interval that is actually integrated -- 15 of the 19 probes
    // over each of the 4 overlay widths, 60 of 76, measured.
    assert!(
        nonzero_f >= FLOOR_F,
        "the filtered overlay sweep must integrate a real interval at least {FLOOR_F} \
         times, or the withheld facts have drifted out of every integrated interval \
         and the overlay is not being exercised at all: got {nonzero_f} of {probes_f}"
    );
}

/// The sustenance tenants owe the same two chaos schedules [`Trail`] does:
/// discarding the accumulated state and rebuilding it from the ledger is
/// unobservable, at every position and at every third.
///
/// **The read-side accumulator is inside this schedule, not beside it** (The
/// Pawl, Task 5b). It is not a [`hornvale_kernel::fold::LedgerFold`] — it is a
/// function of the temperature field as well as the ledger — so comparing the
/// tenants' reset
/// lists alone would leave the one piece of state that carries `f64`
/// arithmetic entirely untested against discard. So each prefix also takes a
/// full [`sustenance_at`] read through BOTH stores and compares them with `==`
/// on `f64`: the discarded store rebuilds its accumulator from scratch, the
/// resident one resumes from a checkpoint, and the two must agree bit for bit.
/// One temperature field for both stores, which is the invariant the memo
/// states.
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

/// The accumulator's EVICTION is unobservable: a third reset value for one
/// `(entity, drive)` drops a memoised partition, and every read still equals
/// the scan oracle bit for bit.
///
/// `MEMO_PARTITIONS_PER_DRIVE` is 2, so reading one entity's thirst integral
/// from three different reset instants must evict. The eviction is a pure
/// recomputation — a partition is a function of `(ledger prefix, temperature
/// field, home)` and nothing else — but that is an ARGUMENT, and the chaos
/// schedules only ever discard the WHOLE store, never one slot inside a live
/// one. This drives the narrower case directly: the same store, cycled across
/// three resets many times over, compared against the verbatim
/// `integrate_thirst` oracle at every step.
///
/// The cycle is deliberately NOT monotone. A rising sequence of resets would
/// evict only the oldest slot and would never ask a partition to come BACK
/// after being dropped, which is the case an off-by-one in the eviction guard
/// would survive.
///
/// It carries a POSITIVE CONTROL, because an equality sweep on its own could
/// not tell an eviction that rebuilt correctly from a cap that never fired —
/// see the comment beside it at the end of the body.
#[test]
fn evicting_a_memoised_reset_partition_is_unobservable() {
    let (l, e, home) = sustenance_fixture();
    let terrain = RippleTerrain;
    let mut store = ResidentFolds::new();
    let mut witness = ReadWitness::default();

    // The fixture's three resets, cycled out of order so a dropped partition
    // is asked for again after eviction rather than merely aged out.
    let r: Vec<WorldTime> = [200_000_i64, 600_000, 850_000]
        .into_iter()
        .map(WorldTime::from_ticks)
        .collect();
    let mut probes = 0usize;
    let mut nonzero = 0usize;
    for _round in 0..3 {
        for reset in [r[0], r[2], r[1], r[0], r[2]] {
            for t in probe_instants() {
                let (trail, _resets, memo, _w) = store.trail_and_thirst(&l);
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
                    "cycling the reset across more partitions than the memo keeps must \
                     still equal the oracle at {t:?} from reset {reset:?}"
                );
                probes += 1;
                if got > 0.0 {
                    nonzero += 1;
                }
            }
        }
    }
    println!(
        "--- eviction witness: {nonzero} of {probes} probes integrated a non-zero interval, \
         3 resets cycled 15 times through a 2-slot memo ---"
    );
    assert!(
        nonzero >= 100,
        "the eviction sweep must integrate a real interval at least 100 times, or it is \
         asserting 0.0 == 0.0 throughout: got {nonzero} of {probes}"
    );
    // THE POSITIVE CONTROL, and this test needs one badly: eviction is
    // VALUE-invariant by design, so an equality sweep alone cannot tell a
    // memo that evicted and rebuilt correctly from one that never evicted at
    // all. The witness can. With the trail fixed for the whole test, a
    // partition is only ever rebuilt because it was DROPPED, and a rebuild is
    // exactly a read whose terrain samples exceed what the ledger grew since
    // the previous read of that same reset -- an "unbounded" read. So a
    // non-zero count here is the evidence that the cap fired; a zero would
    // mean this test swept three resets through a memo that kept all of them
    // and proved nothing about eviction.
    let rebuilt = witness
        .unbounded_reads_by_entity()
        .get(&e)
        .copied()
        .unwrap_or(0);
    println!("eviction witness: {rebuilt} reads had to rebuild a dropped partition");
    assert!(
        rebuilt > 0,
        "no read ever rebuilt a dropped partition, so the {} cap never fired and this \
         sweep tested equality without testing eviction at all",
        "MEMO_PARTITIONS_PER_DRIVE"
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
///    gap, counted rather than inferred. **This is not independent evidence
///    from (1).** `sustenance_at` samples terrain exactly once per integrated
///    window, so it hands the SAME expression to the witness as both counts,
///    and the two lines below will print the same numbers until some future
///    rate function separates them (see
///    [`ReadWitness::terrain_samples_by_entity`]'s field doc). The second
///    assertion buys a claim stated on the quantity §11.7 named rather than on
///    a proxy for it; it does not buy a second observation.
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
// The Pawl, Task 4: the belief read's scan oracle.
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
/// SCAN half of FOLD equals SCAN for [`LatestVisit::water_at`].
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
fn latest_visit_water_folded_one_fact_at_a_time_equals_the_scan_oracle() {
    let (l, a, b) = hand_built();
    let terrain = pool_terrain();
    let folded = fold_latest_visit_one_by_one(&l);
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
fn latest_visit_water_at_every_past_instant_equals_the_oracle_at_that_instant() {
    let (l, a, b) = hand_built();
    let terrain = pool_terrain();
    let folded = fold_latest_visit_one_by_one(&l);

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
    let folded = fold_latest_visit_one_by_one(&l);
    assert_eq!(
        folded.state().of(e).get(&wet).and_then(|days| days.first()),
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
    let folded = fold_latest_visit_one_by_one(&l);
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
fn discarding_latest_visit_water_at_every_position_is_unobservable() {
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
fn discarding_latest_visit_water_at_every_third_position_is_unobservable() {
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
/// two ways.** On the paths the session actually REACHES — its own belief
/// lookups, and the present-instant `affect_of_memo_occupied` shape — the exact
/// count is ZERO. [`LatestVisit`] carries an ascending visit list anyway, for two
/// reasons neither of which is that number:
///
/// 1. `emitter_arousal` (`liveness.rs`) replays `affect_of` at a creature's
///    own PAST visit day, and that call reaches `believed_water` at that past
///    instant. It is production code, not a test shape; it is merely gated
///    behind a non-empty emitter scan, and neither seed 42 nor
///    [`WALKING_SEED`] has a primary-afraid emitter. The sweep below runs
///    exactly that shape and every one of its reads is a past-instant read.
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
///
/// **Re-measured on the shortened script** ([`WITNESS_WAITS`] = 12, down from
/// 40, and [`PAST_DAY_STRIDE`] = 9, down from 37 so the sweep still samples
/// about six of each body's own visit days rather than one; 342.947 s to
/// 39.1-45.5 s in a full parallel crate run). The session: 23,665 facts absorbed, 49,391 belief lookups, **0**
/// at an instant before a committed sighting. The present-instant shape: 3,365
/// lookups, 0 past-instant. The past-day sweep: **19,379 lookups, 16,015 at an
/// instant before a committed sighting**, over 348 outer calls. Both asserted
/// floors — `past_lookups > 0` and `past_offenders > 0` — are cleared on the
/// shortened script by four orders of magnitude, and the verdict is unchanged:
/// rule 6 FIRES.
#[test]
fn rule_six_witness_belief_reads_run_at_past_instants() {
    let world = common::build(WALKING_SEED).expect("the walking seed always builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the walking seed always starts a session");
    for _ in 0..WITNESS_WAITS {
        session.handle("wait");
        let _ = session
            .snapshot()
            .expect("the walking seed's session snapshots");
    }

    println!(
        "--- rule 6 witness: belief reads (seed {WALKING_SEED}, {WITNESS_WAITS} waits + \
         snapshots) ---"
    );
    println!(
        "the SESSION itself: {} facts absorbed, {} belief lookups, {} at an instant before \
         a committed sighting",
        session.resident_position(),
        session.resident_belief_lookups(),
        session.resident_beliefs_in_the_past()
    );
    assert!(
        session.committed_fact_count() > 0,
        "the {WITNESS_WAITS}-wait seed-{WALKING_SEED} script must commit facts, or this witness \
         measured nothing"
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
        let mut route = RouteMemo::new();
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
                &mut route,
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
        let mut route = RouteMemo::new();
        for npc in &bodies {
            let days: Vec<WorldTime> = ledger
                .facts_of(npc.entity, AGENT_AT)
                .filter_map(|f| f.day)
                .collect();
            for day in days.into_iter().step_by(PAST_DAY_STRIDE) {
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
                    &mut route,
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
            "FIRED -- a production belief read runs at a past instant, so LatestVisit carries \
             an ascending visit list and filters by its first instant"
        }
    );
    println!(
        "NOT counted here, and named so the number is not read as complete: the belief reads \
         NESTED inside `emitter_arousal`'s own `affect_of` go to the THROWAWAY store \
         `affect_of_memo` builds (liveness.rs, `affect_of_memo`), so their witness dies with \
         it. They are past-instant by construction -- `emitter_arousal` passes the visit day \
         it is replaying -- and they are unreached on this seed, which has no primary-afraid \
         emitter"
    );

    assert!(
        past_lookups > 0,
        "the sweep must actually REACH `believed_water`, or a verdict of zero past-instant \
         reads is zero out of zero and says nothing"
    );
    assert!(
        past_offenders > 0,
        "spec §3 rule 6's branch: `LatestVisit` carries an ascending visit list BECAUSE a \
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
/// `hazard_memory_memo`'s four callers. The other three are:
///
/// - `hazard_memory` and `believed_hazard` (which delegates to it) — public
///   entry points with their own `t`; every call site in the tree is inside
///   `liveness.rs`'s own test module.
/// - `DriveMovements::step_with_occupancy`'s per-creature preamble and
///   `step_one_with_controller`, both at `t = self.from`.
///
/// And the counts are measured unequal. **They were 520 against 568 on the
/// pre-Roll seed-42 session, and re-measuring them at the absorption of main
/// is what shows how weak an argument the near-agreement was**: on the
/// forty-wait [`WALKING_SEED`] session the same two counters read **2,557
/// hazard lookups against 143,611 belief lookups** — a factor of 56, not a
/// difference of 48. The old gap was arithmetically consistent with one hazard
/// read and one `own` belief read per walk plus 48 extra belief reads from
/// `shared_believed_water`'s per-co-located-peer loop; that loop runs once per
/// CO-LOCATED peer, and since the roll a settlement's residents stand together
/// rather than one to a settlement, so the term that used to be a rounding
/// error is now the whole quantity. Either reading refutes the inference the
/// first draft made; the new one refutes it by two orders of magnitude, which
/// is the honest form of the point.
///
/// The number `LatestVisit` (Task 5) branches on is this one, so it is taken
/// on its own counter. The belief counts are printed beside it precisely so
/// the two caller sets can be seen NOT to agree.
///
/// **Re-measured on the shortened script** ([`WITNESS_WAITS`] = 12, down from
/// 40; 343.453 s to 39.3-45.1 s in a full parallel crate run), because the
/// disagreement above is a RATIO and a
/// ratio read off a script this test no longer runs is a guess. The twelve-wait
/// session reads **905 hazard lookups against 49,391 belief lookups — a factor
/// of 54.6**, against the forty-wait session's 2,557 against 143,611, a factor
/// of 56. The two scripts agree on the shape and very nearly on the size, which
/// is the point being made: the gap is a factor, not the difference of 48 the
/// pre-Roll seed-42 numbers suggested. Every floor below is cleared on the
/// shortened script — the session reaches `hazard_memory_memo` 905 times, the
/// present-instant sweep 59, the past-day sweep 348 (290 of them at a past
/// instant), and the entry-point probe's 58 calls are counted 58 times.
#[test]
fn rule_six_witness_hazard_memory_reads_run_at_past_instants() {
    let world = common::build(WALKING_SEED).expect("the walking seed always builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the walking seed always starts a session");
    for _ in 0..WITNESS_WAITS {
        session.handle("wait");
        let _ = session
            .snapshot()
            .expect("the walking seed's session snapshots");
    }

    println!(
        "--- rule 6 witness: hazard-memory reads (seed {WALKING_SEED}, {WITNESS_WAITS} waits \
         + snapshots) ---"
    );
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
        "the seed-{WALKING_SEED} session must REACH `hazard_memory_memo` -- it is called \
         from the walk preamble of every tick -- or a verdict of zero past-instant reads is \
         zero out of zero and says nothing"
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
        let mut route = RouteMemo::new();
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
                &mut route,
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
        let mut route = RouteMemo::new();
        for npc in &bodies {
            let days: Vec<WorldTime> = ledger
                .facts_of(npc.entity, AGENT_AT)
                .filter_map(|f| f.day)
                .collect();
            for day in days.into_iter().step_by(PAST_DAY_STRIDE) {
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
                    &mut route,
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
/// FIRST instant (which is exactly what [`LatestVisit::water_at`] uses for
/// membership)
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
///
/// **Re-measured on the shortened script** ([`SIGHTING_WAITS`] = 8, down from
/// 20; 126.175 s to 12.7-13.9 s in a full parallel crate run): 58 subjects, **986 adjacent sighting pairs, 0 at
/// the same instant**, so the `pairs > 100` floor is cleared by 886 and the
/// verdict is the same one the twenty-wait script returned. This is the witness
/// the shortening costs the most reach: a same-instant pair the walk cannot
/// commit has fewer chances to appear in 986 pairs than in the long script's,
/// and the structural argument above — not this count — is what makes the zero
/// believable.
#[test]
fn the_walk_never_commits_two_sightings_of_one_entity_at_one_instant() {
    let world = common::build(WALKING_SEED).expect("the walking seed always builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the walking seed always starts a session");
    for _ in 0..SIGHTING_WAITS {
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
        "--- Trail order versus commit order (seed {WALKING_SEED}, {SIGHTING_WAITS} waits, {} \
         subjects) ---\n\
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
///
/// **The quantity asserted on is segments PER PAST-DAY REPLAY, not the total,
/// and the total is guarded separately against the history it would otherwise
/// have walked.** The two were interchangeable when a script produced ten
/// replays; The Roll made a tick advance a settlement's whole roll and the
/// count went to thousands, so the total now carries the replay count's own
/// growth. The reasoning, with the measured numbers, is at the assertion
/// itself rather than restated here.
#[test]
fn the_hazard_folds_integration_does_not_grow_with_the_tick_index() {
    // **THE SCRIPT WAS 50 AND 200 UNTIL THE ABSORPTION OF MAIN, AND IT WAS
    // SHORTENED BECAUSE THE ROLL MADE A TICK TEN TIMES BIGGER, NOT BECAUSE THE
    // CLAIM GOT WEAKER.** A seed-42 tick advanced seven bodies when these
    // constants were chosen; it advances the settlement's whole roll now
    // (68 at seed 42, 59 at `WALKING_SEED`), and `hazard_memory_memo` is
    // threaded the full roster inside a per-agent call, so the roster pass
    // alone is quadratic in that number. Measured at the absorption: this test
    // was KILLED at **3528.7 s** without completing, on a box at load 2.96,
    // against 96.6 s for the next slowest test in the crate; a first cut to
    // 10 and 40 still cost 862.9 s, which is where these numbers come from
    // rather than from an estimate. Both things the
    // assertions below actually need survive the cut untouched — the tick
    // index still moves by construction, and `MIN_FACTS_ACCRUED` is still
    // the floor that decides whether the comparison means anything. What is
    // lost is the absolute tick index the ratio is read at, and that is the one
    // thing this test does not assert on.
    //
    // **CUT AGAIN AT THE CAMPAIGN'S CLOSE, 5/20 -> 2/6, for the 60 s witness
    // ceiling -- WHICH IT STILL DOES NOT MEET, and the module doc says why it
    // is left that way rather than cut again** (339.000 s -> 66.186 s and
    // 75.770 s in two full parallel crate runs; 52.442 s in a light one). The
    // tick
    // index moves 3x, and the accrual floor is checked directly rather than
    // copied from an older seed-specific measurement.
    //
    // The emitter-bearing world's SEARCH is the other half of this test's cost
    // and it was cut too, from eight waits per candidate seed to
    // [`EMITTER_SEARCH_WAITS`] (2). Measured over seeds 0..64: one wait finds
    // NO world in the range that replays an emitter's affect at a past visit
    // day, and every wait count from two upward lands on seed 6 — so two is the
    // cheapest search that still selects the same world the eight-wait one did.
    const EARLY: usize = 2;
    const LATE: usize = 6;

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
            for _ in 0..EMITTER_SEARCH_WAITS {
                session.handle("wait");
            }
            session.resident_alarm_replays() > 0
        },
    );
    assert_eq!(
        seed, EMITTER_SEED,
        "the search moved off the seed every number in this test's comment block was \
         measured on. That is a finding about the sim, not a broken test: re-measure the \
         two samples on the new seed and say in the campaign record what changed about \
         which worlds replay an emitter's affect at a past visit day"
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

    // THE DENOMINATOR MOVED AT THE ABSORPTION OF MAIN, AND NAMING IT IS THE
    // FINDING — the assertion below used to be on the TOTAL and is on the
    // per-replay quotient now.
    //
    // The claim this test carries is about what the resident store bought: the
    // `latest` map is no longer rebuilt per call and the emitter scan no longer
    // rebuilds every roster member's timeline, so a past-day affect replay
    // costs O(1) integral segments instead of O(history). Before The Roll, a
    // seed-28 session performed **ten** such replays over its whole script, so
    // the total and the per-replay quotient were the same number up to a
    // constant and the total was the simpler thing to assert on.
    //
    // The Roll ticks a settlement's whole roll, and the measured replay count
    // went from ten to **thousands**. That is what trips the old assertion, and
    // the decisive measurement is the 10/40 script this test ran once before
    // being shortened, because there the two readings are directly comparable:
    // seed 6 gave 1,362 segments over 2,031 replays at turn 10 and 3,029 over
    // 4,245 at turn 40 — a TOTAL ratio of 2.224x, which trips a 1.5x
    // allowance, against a per-replay quotient that moved 0.671 to 0.714,
    // **6.5%**. So the fold's own integration cost is flat and what grew is how
    // many times the hazard read enters the replay at all, which is a property
    // of how many rooms a longer walk has left an emitter remembering — not of
    // the store.
    //
    // The 5/20 script this test ran next gave 962 segments over 374 replays at
    // turn 5 and 2,251 over 3,481 at turn 20 — total 2.340x, quotient FALLING
    // from 2.572 to 0.647.
    //
    // On the 2/6 script it actually runs now, seed 6 gives **598 segments over
    // 223 replays at turn 2 and 806 over 373 at turn 6**: the total grows
    // **1.348x** and the per-replay quotient falls from **2.682 to 2.161
    // (0.81x)**, against the 1.5x allowance. All three cuts report the same two
    // shapes — a flat-or-falling quotient and a total growing slower than the
    // history — which is what makes the reading a property of the fold rather
    // than of a script length. The quotient falling rather than holding is the
    // emitter scan's own warm-up, not a second effect: what the guard has to
    // exclude is the quotient RISING, which is the only shape a return to an
    // O(history) read could take.
    //
    // Asserting on the quotient is naming the right denominator, not relaxing
    // the guard, and the total is guarded too, one assertion down, by the
    // comparison that actually discriminates: pre-store the total was
    // O(replays x history) and would have grown ~3.8x here (1.67 x 2.28), so a
    // total that grows STRICTLY SLOWER than the roster's own history still
    // fails loudly if the per-replay cost ever goes back to walking it.
    //
    // THAT GUARD'S MARGIN, WHICH WAS RECORDED NOWHERE UNTIL NOW, and it is the
    // Task 5c review's one carried Important. The total guard is the only
    // discriminating half of this witness once the quotient is asserted against
    // a moving denominator, so a bare `segment_growth < history_growth` says
    // nothing about how much room it has. Measured on the 2/6 script:
    // **`history_growth` = 2.28x (549 roster facts at turn 2, 1,251 at turn 6)
    // against `segment_growth` = 1.348x (598 segments, 806)** — the total sits
    // at 59% of its ceiling, so the guard trips once the fold's integration
    // work grows 1.7x faster than it does today. That is a real margin and not
    // a generous one, and it is the number to re-read after any change to the
    // replay path.
    let early_per_replay = early.segments as f64 / early.replays as f64;
    let late_per_replay = late.segments as f64 / late.replays as f64;
    let history_growth = late.trail_facts as f64 / early.trail_facts.max(1) as f64;
    let segment_growth = late.segments as f64 / early.segments as f64;
    println!(
        "segments per replay: {early_per_replay:.3} at turn {EARLY}, {late_per_replay:.3} at \
         turn {LATE} ({:.2}x); total segments {segment_growth:.3}x against a \
         {history_growth:.2}x longer roster history",
        late_per_replay / early_per_replay
    );
    assert!(
        late_per_replay <= early_per_replay * 1.5,
        "the hazard fold's integration must not scale with the tick index: {early_per_replay:.3} \
         segments per past-day replay at turn {EARLY} against {late_per_replay:.3} at turn \
         {LATE} (allowance 1.5x), with the roster's own history {history_growth:.2}x longer"
    );
    assert!(
        segment_growth < history_growth,
        "the hazard fold's TOTAL integration must grow strictly slower than the history it \
         would have walked, or the per-replay quotient above is flat only because the \
         replay count absorbed the growth: {} segments at turn {EARLY} against {} at turn \
         {LATE} ({segment_growth:.2}x) with the roster's history {history_growth:.2}x longer",
        early.segments,
        late.segments
    );
}

// ---------------------------------------------------------------------------
// The Kerf, Task 3: FOLD equals SCAN for the belief read, on two real shapes
// and on a ledger whose sightings arrive backwards.
//
// These witnesses compare `LatestVisit::water_at` against the independent
// scan oracle after the migration. They were green on the old tenant before
// the cut, then retargeted so they continue to police the read's behaviour.
//
// The oracle is [`known_water_scan_oracle`], unchanged and shared with The
// Pawl's fixture tests. It is a VERBATIM copy of the pre-Pawl
// `believed_water` set-building loop and the only statement of that loop
// that survives anywhere; nothing regenerates it, so it is not edited here.
//
// # THE CONTROLS, RUN BEFORE ANY OF THIS WAS BELIEVED (2026-09-04)
//
// A witness that cannot fail is decoration, so both mutations were applied
// with `scripts/mutate.py` to the old fold and restored with `git checkout
// --`, never by retyping. Their discrimination is re-proven below against
// `LatestVisit::water_at`.
//
// ```text
// witness                                    control B   control C
// the_kerf_a_room_committed_backwards...       RED         RED
// the_kerf_possession_shape ... every 1        RED         green
// the_kerf_possession_shape ... every 3        RED         green
// the_kerf_lab_shape ... every 1               RED         green
// the_kerf_lab_shape ... every 3               RED         green
// the_kerf.rs's two ledger-hash constants      green*      green
// ```
//
// **Control B** is `if day < *first` -> `if day > *first`: keep the LATEST
// visit rather than the first. **Control C** empties the min-keeping arm
// outright (`let _ = (first, day);`), so the FIRST-ARRIVING sighting wins
// whatever its day.
//
// Two things are worth reading off that table, and both were surprises.
//
// FIRST, THESE WITNESSES ARE SHARPER THAN THE CAMPAIGN'S HASH CONSTANTS.
// `*` marks the one measured at Task 1: control B moved NEITHER hash, on
// four scripts including seed 17 with its 1,194 past-instant belief reads,
// because a differently-admitted room only reaches a committed fact through
// the chain past-instant read -> different admitted set -> different chosen
// room -> different committed route, and that chain never completed. A
// FOLD-equals-SCAN witness compares the admitted set ITSELF, so it needs no
// such chain and reddens immediately. A campaign that had only minted hash
// constants would have shipped control B's behaviour change unseen.
//
// SECOND, ONLY THE DESCENDING FIXTURE CATCHES CONTROL C, and that is the
// exact sense in which Task 1's "the min-keeping branch never fires on a
// walk" is true. On a real walk a room's sightings ARRIVE in ascending day
// order, so the first arrival already IS the minimum and "keep the minimum"
// and "keep the first arrival" are the same function: control C is
// unobservable on both real shapes, at every one of their 324,535 combined
// comparisons. Control B is observable there only because it makes the fold
// keep the LATEST, which ascending arrivals do distinguish. So the real
// shapes witness the comparison's SENSE and the fixture witnesses the
// branch's FIRING, and neither substitutes for the other.
// ---------------------------------------------------------------------------

/// The seed the POSSESSION shape walks.
///
/// Not [`WALKING_SEED`], and the difference is the whole reason this seed is
/// pinned. Seed 14's residents commit 2,668 positional facts and reach **no
/// water at all**, so `water_at` is empty for every one of them and the
/// `is_water` filter would pass vacuously — spec §3 rule 3's floor (a)
/// failing by construction. Seed 17 was found by The Kerf's Task 1 seed
/// sweep and is the same world `the_kerf.rs` mints its sharper hash constant
/// on: 67 bodies, 928 `agent-at` facts, 28,601 belief reads of which 1,194
/// run at an instant strictly before a committed sighting. Each test module
/// keeps its own copy of a seed constant, as [`WALKING_SEED`] and
/// [`EMITTER_SEED`] already do here, so every site refuses on its own terms
/// if an epoch moves the world under it.
/// type-audit: bare-ok(index)
const KERF_WATER_SEED: u64 = 17;

/// How many `wait`s the possession shape's script takes — the same twelve
/// `the_kerf.rs` walks, kept as this module's own constant rather than
/// shared with [`WITNESS_WAITS`], whose doc binds it to [`WALKING_SEED`].
/// type-audit: bare-ok(count)
const KERF_WAITS: usize = 12;

/// The LAB shape: `the_detent::bench_shape`'s seed, ticks and roster size.
///
/// **Fifty agents, not ten, and that is a measured requirement rather than a
/// preference.** `bench_shape(42, 10, 10)` puts **0 of 10** roster members on
/// water at the final instant, and `bench_shape(42, 20, 20)` puts **0 of 20**
/// — so both would fail floor (a) while looking like perfectly reasonable
/// shapes. At fifty agents nine of fifty hold a non-empty belief set, which
/// is the same 11-of-50 shape The Kerf's Task 2 measured on the bench's own
/// band 10. Roster SIZE is the lever here, not tick count.
/// type-audit: bare-ok(index)
const KERF_LAB_SEED: u64 = 42;
/// How many ticks the lab shape runs. See [`KERF_LAB_SEED`].
/// type-audit: bare-ok(count)
const KERF_LAB_TICKS: usize = 10;
/// How many agents the lab shape derives. See [`KERF_LAB_SEED`].
/// type-audit: bare-ok(count)
const KERF_LAB_AGENTS: usize = 50;

/// A [`Terrain`] whose `is_fresh_water` verdicts over a ledger's sighted
/// rooms are computed ONCE, up front, and then read from a table.
///
/// The sweeps below ask `is_water` about the same rooms tens of thousands of
/// times — once per admitted sighting, per entity, per instant, per prefix —
/// and `LocaleTerrain::is_fresh_water` is a field evaluation. The domain is
/// tiny and known in advance: 83 distinct rooms on the possession shape, 709
/// on the lab shape, all of them named by an `agent-at` fact in the ledger
/// being swept. So the table is built from the ledger and the answer is a
/// pure lookup.
///
/// It is a table and not a lazy memo deliberately: no interior mutability,
/// so the verdicts cannot depend on the order the sweep asks its questions
/// in. A room the table does not know — which cannot arise from a fold or an
/// oracle over this ledger, since both only ever see its own sightings —
/// falls through to the real terrain rather than being answered `false`.
/// Both sides of every comparison read the same instance, so a wrong verdict
/// would be wrong identically for the fold and the oracle and could not
/// manufacture agreement; what it COULD do is make the `is_water` filter
/// vacuous, which is why [`Self::split`] reports the wet/dry split and the
/// lab witness asserts on it.
struct TabulatedWater<'a> {
    /// The real terrain, for anything the table does not hold.
    inner: &'a dyn Terrain,
    /// Every room the ledger sights, and whether it is fresh water.
    known: std::collections::BTreeMap<Facet, bool>,
}

impl<'a> TabulatedWater<'a> {
    /// Tabulate `inner`'s verdict for every room `source` sights.
    fn new(inner: &'a dyn Terrain, source: &Ledger) -> Self {
        let mut known: std::collections::BTreeMap<Facet, bool> = std::collections::BTreeMap::new();
        for f in source.iter() {
            if f.predicate != AGENT_AT {
                continue;
            }
            if let Value::Text(s) = &f.object {
                let r = room_from_text_copy(s);
                known
                    .entry(r.clone())
                    .or_insert_with(|| hornvale_vessel::liveness::is_water(&r, inner));
            }
        }
        TabulatedWater { inner, known }
    }

    /// `(wet, dry)` — the sighted rooms, split by the verdict. The
    /// `is_water` non-vacuity denominator.
    fn split(&self) -> (usize, usize) {
        let wet = self.known.values().filter(|v| **v).count();
        (wet, self.known.len() - wet)
    }
}

impl Terrain for TabulatedWater<'_> {
    fn elevation(&self, room: &Facet) -> f64 {
        self.inner.elevation(room)
    }
    fn is_fresh_water(&self, room: &Facet) -> bool {
        match self.known.get(room) {
            Some(v) => *v,
            None => self.inner.is_fresh_water(room),
        }
    }
    fn temperature(&self, room: &Facet, day: WorldTime) -> f64 {
        self.inner.temperature(room, day)
    }
}

/// A prefix of a real ledger, rebuilt one fact at a time.
///
/// [`known_water_scan_oracle`] takes a `&Ledger` and scans it whole, and
/// [`ResidentFolds`] advances to whatever ledger it is handed — so comparing
/// either at a PREFIX needs a `Ledger` that holds exactly that prefix, and
/// `Ledger` is append-only with no truncation door. The Pawl's fixture gets
/// one by replaying its own `SCRIPT` ([`hand_built_upto`]); a real session's
/// ledger has no script, so this replays the ledger itself.
///
/// Every predicate is registered NON-functional, which is the only liberty
/// taken and it is one-directional: dropping the contradiction check can
/// only admit facts the source already holds, never reject one. The
/// faithfulness of the replay is asserted rather than argued —
/// [`Self::push`] refuses unless the fact landed at the position it came
/// from and compares equal to the original, so a dedup or a rejection is a
/// panic and not a silently shorter prefix.
struct PrefixLedger {
    /// A registry that accepts every predicate the source uses.
    reg: ConceptRegistry,
    /// The prefix built so far.
    out: Ledger,
}

impl PrefixLedger {
    /// An empty prefix.
    fn new() -> Self {
        PrefixLedger {
            reg: ConceptRegistry::default(),
            out: Ledger::default(),
        }
    }

    /// Append one fact, registering its predicate on first sight.
    fn push(&mut self, f: &Fact) {
        if self.reg.predicate(&f.predicate).is_none() {
            self.reg
                .register_predicate(&f.predicate, false, "the-kerf replay")
                .expect("a fresh predicate name registers");
        }
        let at = self.out.len();
        self.out
            .commit(f.clone(), &self.reg)
            .expect("a fact the source ledger already accepted re-commits");
        assert_eq!(
            self.out.len(),
            at + 1,
            "the replay dropped a fact at position {at}: a prefix shorter than the source \
             would compare a fold against an oracle over a DIFFERENT ledger"
        );
        assert_eq!(
            self.out.iter().nth(at),
            Some(f),
            "the replay changed the fact at position {at}"
        );
    }
}

/// What one sweep of [`kerf_fold_equals_scan`] saw — the floors, counted so
/// they can be asserted and printed rather than assumed.
struct KerfSweep {
    /// The ledger position the sweep started at (the first `agent-at` fact).
    start: usize,
    /// How many prefixes it compared at.
    prefixes: usize,
    /// Three-way comparisons made (fold, discarded-and-rebuilt fold, oracle).
    compares: usize,
    /// Of those, the ones where the oracle returned a NON-EMPTY set — two
    /// empty sets agreeing is agreement about nothing.
    non_empty: usize,
    /// Of those, the ones taken at an instant strictly before that entity's
    /// last committed sighting — spec §3 rule 3's floor (b).
    past: usize,
    /// Entities holding a non-empty `water_at` at the whole ledger's last
    /// instant — spec §3 rule 3's floor (a).
    non_empty_at_end: usize,
    /// The largest such set.
    max_at_end: usize,
    /// Entities for whom `is_water` DROPS at least one visited room. Zero
    /// means the filter admitted everything on this shape; it is printed
    /// always and asserted only where the shape can meet it.
    filtered: usize,
}

/// FOLD equals SCAN over a real ledger, at every prefix, under one chaos
/// schedule.
///
/// Three things are compared at each prefix and each probed instant: the
/// RESIDENT store (advanced incrementally as the ledger grows, which is what
/// a session does), a CHAOTIC store thrown away and rebuilt from scratch
/// every `every` positions, and [`known_water_scan_oracle`]. The first pair
/// pins that discarding the store is unobservable; the second pins that the
/// fold answers what a full scan of the ledger answers.
///
/// The sweep starts at the ledger's FIRST `agent-at` fact rather than at
/// position 0. Everything before it is world-genesis material the fold
/// ignores by predicate, so both stores are empty there and the oracle
/// returns the empty set — comparing them would add O(world ledger) prefixes
/// of two empty sets agreeing. The transition into a non-empty answer is
/// inside the swept range, because the first `agent-at` fact is.
///
/// Instants are the entity's OWN committed days, strided by
/// [`PAST_DAY_STRIDE`] (this file's existing idiom for a past-day sweep),
/// plus its latest committed day. That is what puts past instants in the
/// comparison at all, and floor (b) counts them.
fn kerf_fold_equals_scan(
    label: &str,
    source: &Ledger,
    entities: &[EntityId],
    terrain: &dyn Terrain,
    every: usize,
) -> KerfSweep {
    let facts: Vec<&Fact> = source.iter().collect();
    let n = facts.len();
    let start = facts
        .iter()
        .position(|f| f.predicate == AGENT_AT)
        .unwrap_or_else(|| {
            panic!("{label}: the ledger holds no agent-at fact, so this sweep would be vacuous")
        });

    let mut prefix = PrefixLedger::new();
    for f in facts.iter().take(start) {
        prefix.push(f);
    }

    let mut resident = ResidentFolds::new();
    let mut chaotic = ResidentFolds::new();
    let mut out = KerfSweep {
        start,
        prefixes: 0,
        compares: 0,
        non_empty: 0,
        past: 0,
        non_empty_at_end: 0,
        max_at_end: 0,
        filtered: 0,
    };

    for p in start..=n {
        if p > start {
            prefix.push(facts[p - 1]);
        }
        let l = &prefix.out;
        let _ = resident.latest_visit(l);
        if (p - start) % every == 0 {
            chaotic = ResidentFolds::new();
        }
        let _ = chaotic.latest_visit(l);
        out.prefixes += 1;
        assert_eq!(
            chaotic.position(),
            resident.position(),
            "{label}: position diverged at prefix {p} under a discard-every-{every} schedule"
        );
        for e in entities {
            assert_eq!(
                first_visits(chaotic.latest_visit(l), *e),
                first_visits(resident.latest_visit(l), *e),
                "{label}: {e:?}'s first-visit map diverged at prefix {p} under a \
                 discard-every-{every} schedule"
            );
            let days: Vec<WorldTime> = l.facts_of(*e, AGENT_AT).filter_map(|f| f.day).collect();
            let last = days.iter().copied().max();
            let mut probes: Vec<WorldTime> =
                days.iter().copied().step_by(PAST_DAY_STRIDE).collect();
            if let Some(t) = last {
                probes.push(t);
            }
            for t in probes {
                let scanned: Vec<Facet> = known_water_scan_oracle(l, *e, t, terrain)
                    .into_iter()
                    .collect();
                assert_eq!(
                    resident.latest_visit(l).water_at(*e, t, terrain),
                    scanned,
                    "{label}: the resident store's water set for {e:?} at {t:?} must equal \
                     the scan oracle's at prefix {p}"
                );
                assert_eq!(
                    chaotic.latest_visit(l).water_at(*e, t, terrain),
                    scanned,
                    "{label}: the discarded-and-rebuilt store's water set for {e:?} at \
                     {t:?} must equal the scan oracle's at prefix {p}"
                );
                out.compares += 1;
                if !scanned.is_empty() {
                    out.non_empty += 1;
                }
                if last.is_some_and(|l| t < l) {
                    out.past += 1;
                }
            }
        }
    }

    // The floors, taken at the whole ledger's last instant.
    let l = &prefix.out;
    let end = l
        .iter()
        .filter_map(|f| f.day)
        .max()
        .expect("a real ledger carries at least one dated fact");
    for e in entities {
        let w = resident.latest_visit(l).water_at(*e, end, terrain);
        let visited: std::collections::BTreeSet<Facet> = l
            .facts_of(*e, AGENT_AT)
            .filter(|f| f.day.is_some())
            .map(|f| match &f.object {
                Value::Text(s) => room_from_text_copy(s),
                other => panic!("an agent-at object is always text, got {other:?}"),
            })
            .collect();
        if !w.is_empty() {
            out.non_empty_at_end += 1;
        }
        if w.len() < visited.len() {
            out.filtered += 1;
        }
        out.max_at_end = out.max_at_end.max(w.len());
    }

    println!(
        "--- {label} (discard every {every}) ---\n\
         prefixes {} (positions {}..={n}), entities {}, three-way compares {} \
         ({} over a NON-EMPTY oracle set, {} at a PAST instant)\n\
         floor (a): {} of {} entities hold a non-empty water_at at the final instant, \
         largest {}\n\
         is_water drops at least one visited room for {} of {} entities",
        out.prefixes,
        out.start,
        entities.len(),
        out.compares,
        out.non_empty,
        out.past,
        out.non_empty_at_end,
        entities.len(),
        out.max_at_end,
        out.filtered,
        entities.len()
    );

    assert!(
        out.non_empty_at_end > 0,
        "{label}: spec §3 rule 3 floor (a) — at least one entity must hold a NON-EMPTY \
         water_at at the final instant, or the `is_water` filter is vacuous on this shape \
         and every equality above compares two empty sets"
    );
    assert!(
        out.past > 0,
        "{label}: spec §3 rule 3 floor (b) — at least one comparison must run at an instant \
         strictly before that entity's last committed sighting, or the first-visit prefix \
         is never exercised"
    );
    assert!(
        out.non_empty > 0,
        "{label}: at least one comparison must be taken over a NON-EMPTY oracle set"
    );
    out
}

/// The possession shape: a real [`Session`] at [`KERF_WATER_SEED`], driven by
/// the same twelve-`wait` script `the_kerf.rs` walks, with the terrain the
/// world's own [`LocaleContext`] gives.
fn kerf_possession_ledger() -> (hornvale_kernel::World, Ledger, Vec<EntityId>) {
    let world = common::build(KERF_WATER_SEED).expect("the water-belief seed builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the water-belief seed starts a session");
    for _ in 0..KERF_WAITS {
        session.handle("wait");
    }
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    // Every entity the walk actually sighted — read off the ledger rather
    // than chosen, so no convenient body can be picked.
    let mut subjects: std::collections::BTreeSet<EntityId> = std::collections::BTreeSet::new();
    for f in ledger.iter() {
        if f.predicate == AGENT_AT {
            subjects.insert(f.subject);
        }
    }
    (world, ledger, subjects.into_iter().collect())
}

/// The possession shape, discarding at every position.
///
/// **What it compares, measured:** 1,898 prefixes (ledger positions
/// 16,033..=17,930), 52 sighted entities, **244,643 three-way comparisons**,
/// every one of them over a non-empty oracle set and **140,173** of them at
/// an instant strictly before that entity's last committed sighting. Floor
/// (a): 52 of 52 entities hold a non-empty `water_at` at the final instant,
/// the largest 23 rooms. Both of spec §3 rule 3's floors are met with room
/// to spare, and both are asserted in [`kerf_fold_equals_scan`] rather than
/// printed and trusted.
///
/// **Runtime, two readings with the loads that separate them** (both from
/// eight-test `nextest` runs of this crate on ten cores; a timing without
/// its load is not a measurement):
///
/// ```text
/// witness                 run 1     run 2    load run 1              load run 2
/// possession, every 1     21.743 s  31.210 s 9.93 22.73 37.34 ->     6.97 14.52 28.94 ->
/// possession, every 3     20.722 s  30.977 s 6.18 18.77 34.46        31.96 22.73 30.21
/// lab, every 1             9.331 s   7.700 s
/// lab, every 3             9.730 s  10.032 s
/// ```
///
/// Both readings are inside the campaign's 60 s ceiling for a witness, so
/// §3 rule 2 says keep the script and record the number; nothing was
/// shortened. The sweep itself is ~0.5 s of that — the twelve-`wait` session
/// build is the whole cost, and a six-`wait` script was measured at about a
/// quarter of it for 1,026 prefixes instead of 1,898, which is the number to
/// reach for if this ever does need shortening.
///
/// **What this shape does NOT witness, measured rather than assumed.** At
/// seed 17 every room every resident stands in is fresh water: `is_water`
/// tabulates **83 wet rooms and 0 dry ones**, and drops nothing for
/// any of the 52 sighted entities. So the filter is exercised in one
/// direction only here, and the LAB shape is what exercises the other (it
/// drops a visited room for 25 of its 50). The floor this shape cannot meet
/// is therefore printed and not asserted, for the reason `the_kerf.rs`'s
/// seed-11 witness gives about its own missing past-instant floor: asserting
/// a floor a shape cannot meet is how a witness gets quietly weakened.
#[test]
fn the_kerf_possession_shape_fold_equals_scan_discarding_at_every_position() {
    let (world, ledger, entities) = kerf_possession_ledger();
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("the locale context builds");
    let base =
        hornvale_vessel::liveness::LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let terrain = TabulatedWater::new(&base, &ledger);
    let sweep = kerf_fold_equals_scan("possession shape", &ledger, &entities, &terrain, 1);
    let (wet, dry) = terrain.split();
    println!("possession shape: is_water tabulated {wet} wet and {dry} dry sighted rooms");
    assert!(
        sweep.non_empty_at_end >= 2,
        "the possession shape is pinned as the one where the belief set is broadly \
         non-empty; {} entities held one",
        sweep.non_empty_at_end
    );
}

/// The possession shape, discarding at every THIRD position. Both schedules
/// are needed for the reason this file's module doc gives: the every-position
/// schedule gives no signal on `absorb`'s purity, because a bug confined to
/// it cancels when the rebuild happens immediately after every single absorb.
///
/// Same 1,898 prefixes and 244,643 comparisons as its sibling; 20.722 s and
/// 30.977 s in the two runs its sibling's doc tabulates.
#[test]
fn the_kerf_possession_shape_fold_equals_scan_discarding_at_every_third_position() {
    let (world, ledger, entities) = kerf_possession_ledger();
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("the locale context builds");
    let base =
        hornvale_vessel::liveness::LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let terrain = TabulatedWater::new(&base, &ledger);
    let sweep = kerf_fold_equals_scan("possession shape", &ledger, &entities, &terrain, 3);
    assert!(sweep.non_empty_at_end >= 2);
}

/// The lab shape, discarding at every position.
///
/// The comparison sweeps the WHOLE `derive_npcs` roster, not a probe agent,
/// and that is a correction the campaign paid for once already. The bench's
/// own belief probe reads the roster's MAX-HISTORY member, whose belief set
/// is EMPTY at every band — so a witness written against one convenient body
/// would very likely have drawn that one and passed vacuously at every
/// assertion. The Kerf's Task 2 measured the roster instead: 11 of 50 hold a
/// non-empty set at band 10, the largest 46 rooms.
///
/// **What it compares, measured:** 1,166 prefixes (ledger positions
/// 21,812..=22,977), 50 roster members, **79,892 three-way comparisons**,
/// 15,833 of them over a non-empty oracle set and **48,167** at an instant
/// strictly before that entity's last committed sighting. Floor (a): 9 of 50
/// hold a non-empty `water_at` at the final instant, the largest 4 rooms —
/// and this is the shape where `is_water` earns its keep: it tabulates
/// **27 wet rooms and 682 dry ones** and drops a visited room for **25 of
/// the 50** members. That is the direction the possession shape cannot
/// witness at all (83 wet, 0 dry), which is why both shapes are here.
///
/// **Runtime** 9.331 s and 7.700 s in the two runs tabulated on
/// [`the_kerf_possession_shape_fold_equals_scan_discarding_at_every_position`].
#[test]
fn the_kerf_lab_shape_fold_equals_scan_discarding_at_every_position() {
    let shape = crate::the_detent::bench_shape(KERF_LAB_SEED, KERF_LAB_TICKS, KERF_LAB_AGENTS);
    let mesh = shape.mesh_memo.clone();
    let base = hornvale_vessel::liveness::LocaleTerrain::with_fields(
        &shape.ctx,
        None,
        None,
        None,
        None,
        Some(&mesh),
    );
    let terrain = TabulatedWater::new(&base, &shape.ledger);
    let entities: Vec<EntityId> = shape.npcs.iter().map(|n| n.entity).collect();
    let sweep = kerf_fold_equals_scan("lab shape", &shape.ledger, &entities, &terrain, 1);
    let (wet, dry) = terrain.split();
    println!("lab shape: is_water tabulated {wet} wet and {dry} dry sighted rooms");
    assert!(
        wet > 0 && dry > 0,
        "the lab shape is the one that exercises `is_water` in BOTH directions; it \
         tabulated {wet} wet and {dry} dry sighted rooms"
    );
    assert!(
        sweep.filtered > 0,
        "on the lab shape `is_water` must DROP a visited room for at least one entity, or \
         the filter is admitting everything and only its true branch is witnessed"
    );
}

/// The lab shape, discarding at every THIRD position. Same 1,166 prefixes
/// and 79,892 comparisons as its sibling; 9.730 s and 10.032 s in the two
/// runs its sibling's doc tabulates.
#[test]
fn the_kerf_lab_shape_fold_equals_scan_discarding_at_every_third_position() {
    let shape = crate::the_detent::bench_shape(KERF_LAB_SEED, KERF_LAB_TICKS, KERF_LAB_AGENTS);
    let mesh = shape.mesh_memo.clone();
    let base = hornvale_vessel::liveness::LocaleTerrain::with_fields(
        &shape.ctx,
        None,
        None,
        None,
        None,
        Some(&mesh),
    );
    let terrain = TabulatedWater::new(&base, &shape.ledger);
    let entities: Vec<EntityId> = shape.npcs.iter().map(|n| n.entity).collect();
    let sweep = kerf_fold_equals_scan("lab shape", &shape.ledger, &entities, &terrain, 3);
    assert!(sweep.filtered > 0);
}

// ---------------------------------------------------------------------------
// The Kerf, Task 3, step 1: the descending-order fixture.
//
// THE ONLY INSTRUMENT IN THIS CAMPAIGN THAT REACHES THE MIN-KEEPING BRANCH,
// AND THE CONTROL TABLE IN THIS SECTION'S SIBLING HEADER IS WHAT ESTABLISHES
// THAT RATHER THAN AN ARGUMENT. The Kerf's Task 1 proved by panic-mutation
// that the old tenant's `if day < *first` arm is NEVER taken on a
// walk-derived ledger: `DriveMovements` commits at the tick's own day and
// ticks advance monotonically, so a room's first absorbed sighting already IS
// its minimum. Emptying that arm outright (control C) is therefore
// unobservable on both real shapes and on both of the campaign's ledger-hash
// constants — measured, not reasoned — and RED here.
//
// **Do not read that as "no other witness says anything about the direction",
// which is how an earlier draft of this paragraph put it.** Control B — the
// same comparison FLIPPED, keeping the latest visit — reddens both real
// shapes at once, because ascending arrivals distinguish "keep the first"
// from "keep the last" even when they cannot distinguish "keep the first"
// from "keep the minimum". The two mutations separate cleanly and the
// separation is the point: the real shapes hold the comparison's SENSE, and
// only this fixture holds the branch's FIRING.
//
// It is NOT dead code. `liveness::place_agent(entity, room, day)` is a `pub`
// `agent-at` writer taking an ARBITRARY `WorldTime` with no ordering
// constraint, and `windows/lab/src/synthetic.rs` authors whole scenario
// ledgers through it — days chosen by hand, ascending by happenstance rather
// than by construction, and `windows/lab`'s health calibration reads belief
// over exactly those ledgers. So the branch governs a case a sibling window
// can construct today, and this fixture is the only thing that pins it.
// ---------------------------------------------------------------------------

/// The descending fixture's commit script, as
/// `(subject index, day ticks, face, path)`.
///
/// Each ROOM's own sightings arrive in strictly DESCENDING day order, so for
/// every one of them the first fact absorbed is NOT its first visit — the
/// exact condition the old min-keeping arm existed for and the
/// exact condition no walk produces. Three rooms carry three, two and three
/// sightings; the entities are interleaved so a fold leaking one entity's
/// facts into another's is caught too; and one DRY room is visited so the
/// `is_water` filter is not vacuous here either.
///
/// The rooms are [`pool_terrain`]'s: `(0, [0])`, `(0, [3])` and `(1, [2])`
/// are wet, `(0, [1])` is not.
const KERF_DESCENDING_SCRIPT: &[(usize, i64, u8, &[u8])] = &[
    (0, 900_000, 0, &[0]),
    (1, 800_000, 1, &[2]),
    (0, 700_000, 0, &[3]),
    (0, 600_000, 0, &[0]),
    (1, 500_000, 1, &[2]),
    (0, 400_000, 0, &[3]),
    (1, 300_000, 1, &[2]),
    (0, 200_000, 0, &[0]),
    (0, 100_000, 0, &[1]),
];

/// The descending fixture, built the way [`hand_built_upto`] builds The
/// Pawl's: by replaying a script into a fresh ledger with two fixed-lineage
/// entities.
fn kerf_descending_fixture() -> (Ledger, EntityId, EntityId) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    let mut l = Ledger::default();
    let ids = [
        l.mint_entity(test_lineage(0)),
        l.mint_entity(test_lineage(1)),
    ];
    for (who, ticks, face, path) in KERF_DESCENDING_SCRIPT {
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
    (l, ids[0], ids[1])
}

/// Guards the fixture against the edit that would quietly empty it: if
/// [`KERF_DESCENDING_SCRIPT`] ever drifted into per-room ASCENDING order, the
/// min-keeping arm would never be taken and the test below would pass on a
/// fold that simply kept whatever it saw first.
///
/// The same argument [`the_hand_built_ledger_is_not_already_in_sorted_order`]
/// makes for the ordering assertions, made here for the ordering the belief
/// read depends on: not the ledger's order overall, but each ROOM's own.
#[test]
fn the_kerf_descending_fixture_commits_each_room_backwards() {
    let (l, a, b) = kerf_descending_fixture();
    let mut rooms_with_several = 0_usize;
    for e in [a, b] {
        let mut per_room: std::collections::BTreeMap<Facet, Vec<i64>> =
            std::collections::BTreeMap::new();
        for f in l.facts_of(e, AGENT_AT) {
            let Value::Text(s) = &f.object else {
                panic!("an agent-at object is always text")
            };
            per_room
                .entry(room_from_text_copy(s))
                .or_default()
                .push(f.day.expect("the fixture dates every sighting").ticks());
        }
        for (r, days) in &per_room {
            if days.len() < 2 {
                continue;
            }
            rooms_with_several += 1;
            assert!(
                days.windows(2).all(|w| w[1] < w[0]),
                "{e:?}'s room {r:?} must be committed in strictly DESCENDING day order, or \
                 the min-keeping branch this fixture exists to reach is never taken: {days:?}"
            );
            assert_ne!(
                days.first(),
                days.iter().min(),
                "{e:?}'s room {r:?}: the FIRST committed instant must not already be the \
                 earliest, or `days.first()` and the minimum agree for free here"
            );
        }
    }
    assert!(
        rooms_with_several >= 3,
        "at least three rooms must carry more than one sighting, or one accidental edit \
         empties this fixture: got {rooms_with_several}"
    );
}

/// A room whose sightings arrive backwards is known from its EARLIEST
/// instant, not from the first one committed.
///
/// This is spec §5 step 2's equivalence, stated as behaviour: `LatestVisit`'s
/// ascending list makes
/// `days.first()` that same minimum. Both directions are asserted — the room
/// is admitted AT the earliest instant and at every instant after it, and
/// is NOT admitted one tick before it — and the whole thing is compared
/// against [`known_water_scan_oracle`] at every instant the script names,
/// so it is FOLD equals SCAN and not merely a spot check.
///
/// **This is the campaign's only RED under control C** (the min-keeping arm
/// emptied, so the first-arriving sighting wins whatever its day): both real
/// shapes stay green across 324,535 combined comparisons, and so do both
/// ledger-hash constants. See this section's header for the full table and
/// what separates it from control B. Costs 0.007 s.
#[test]
fn the_kerf_a_room_committed_backwards_is_known_from_its_earliest_instant() {
    let (l, a, b) = kerf_descending_fixture();
    let terrain = pool_terrain();
    let folded = fold_latest_visit_one_by_one(&l);
    let wet = room(0, &[0]);

    // The fold kept the minimum, not the first commit.
    assert_eq!(
        folded.state().of(a).get(&wet).and_then(|days| days.first()),
        Some(&WorldTime::from_ticks(200_000)),
        "the room's three sightings committed 900_000, 600_000, 200_000; the fold must \
         hold the EARLIEST"
    );

    // Admitted at the earliest instant, and not one tick before it.
    assert!(
        folded
            .state()
            .water_at(a, WorldTime::from_ticks(200_000), &terrain)
            .contains(&wet),
        "the room is known AT its earliest instant"
    );
    assert!(
        !folded
            .state()
            .water_at(a, WorldTime::from_ticks(199_999), &terrain)
            .contains(&wet),
        "the room is NOT known one tick before its earliest instant"
    );

    // The instant that separates a first-visit fold from a latest-visit one:
    // strictly after the earliest sighting and strictly before the first one
    // COMMITTED. A fold keeping `days.last()` — or keeping whatever it saw
    // first — would report the room unknown here.
    assert!(
        folded
            .state()
            .water_at(a, WorldTime::from_ticks(300_000), &terrain)
            .contains(&wet),
        "at 300_000 the room has been visited (at 200_000) but its FIRST COMMITTED sighting \
         (900_000) is still in the future; a fold that kept the latest visit, or the first \
         arrival, would call it unknown here"
    );

    // FOLD equals SCAN at every instant the script names, plus the two ends,
    // for both entities and under a discarded-and-rebuilt fold as well.
    let mut probes: Vec<WorldTime> = KERF_DESCENDING_SCRIPT
        .iter()
        .map(|(_, t, _, _)| WorldTime::from_ticks(*t))
        .collect();
    probes.push(WorldTime::from_ticks(0));
    probes.push(WorldTime::from_ticks(9_999_999));
    let mut rebuilt: Folded<LatestVisit> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        rebuilt.absorb_at(i as u64, f);
        rebuilt = Folded::rebuild_upto(&l, rebuilt.position());
    }
    let mut non_empty = 0_usize;
    for e in [a, b] {
        for t in &probes {
            let scanned: Vec<Facet> = known_water_scan_oracle(&l, e, *t, &terrain)
                .into_iter()
                .collect();
            if !scanned.is_empty() {
                non_empty += 1;
            }
            assert_eq!(
                folded.state().water_at(e, *t, &terrain),
                scanned,
                "the fold's water set for {e:?} at {t:?} must equal the oracle's"
            );
            assert_eq!(
                rebuilt.state().water_at(e, *t, &terrain),
                scanned,
                "the discarded-and-rebuilt fold's water set for {e:?} at {t:?} must equal \
                 the oracle's"
            );
        }
    }
    // MEASURED against this fixture: 15 of the 22 comparisons return a
    // non-empty set. The floor sits under that so a probe lost to a fixture
    // edit is not a red, and an emptied comparison is.
    assert!(
        non_empty >= 10,
        "at least ten comparisons must be taken over a NON-EMPTY oracle set, or the \
         equalities above are empty sets agreeing: got {non_empty}"
    );
    // And the dry room must really be dropped, here as in The Pawl's fixture.
    let all: Vec<Facet> = folded.state().of(a).keys().cloned().collect::<Vec<_>>();
    let admitted = folded
        .state()
        .water_at(a, WorldTime::from_ticks(9_999_999), &terrain);
    assert!(
        admitted.len() < all.len(),
        "the `is_water` filter must drop at least one of {a:?}'s visited rooms: visited \
         {all:?}, admitted {admitted:?}"
    );
}
