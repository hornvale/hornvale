//! The Latch — restricted passage; The Chattel's Task 8 rebuilt it on the
//! object model, so the folds tested here read `openness` facts about a
//! promoted cave-mouth thing rather than the retired `passage-cleared`
//! predicate (decision 0396). Task 1 is a measurement, not a guard.
//!
//! Discharges the spec's top risk (a time-varying room graph against the
//! pure-function nav caches) by grep, not by argument: `delve` is a session
//! verb (`windows/vessel/src/action.rs` has no `Delve` `Action` variant) and
//! the catch-up path (`windows/vessel/src/liveness.rs`) never mentions
//! `delve`. Both greps returned no matches (2026-08-28), so the gate lands
//! on a mode change into the chamber lattice, never on an edge the NPC
//! catch-up path walks, and the room graph never becomes time-varying.
//!
//! Builds a real seed-42 world through `build_world_to_with_artifacts` (the
//! idiom `windows/vessel/tests/suite/lantern_fabric.rs` uses), the same
//! reason that file gives: `hornvale-vessel` is the shallowest crate that
//! can see both `hornvale-worldgen`'s `barrier_of` and a built
//! `GeneratedTerrain`.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BarrierPins, BarrierState, BuildDepth, SettlementPins, WorldComponents, barrier_of,
    build_world_to_with_artifacts,
};

/// Task 1's probe: how many cave-bearing vertices in seed 42's terrain carry
/// each barrier state. Prints a census and asserts only that a barred vertex
/// EXISTS — if none does, acceptance criterion 1 is unreachable and the cut
/// must move.
///
/// Measured 2026-08-28 on seed 42's terrain (`BuildDepth::Settlements`,
/// `Band::Undercroft`, branch 0, non-ocean cave-bearing vertices only):
/// sealed=215 warded=209 thin=215 open=235 (639 barred of 874 total). Close
/// to the quarter-per-state prediction the brief made ahead of the run.
#[test]
fn seed_42_places_at_least_one_barred_cave_mouth() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed 42 failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .as_ref()
        .expect("BuildDepth::Settlements produces terrain");
    let pins = BarrierPins::default();

    let mut sealed = 0usize;
    let mut warded = 0usize;
    let mut thin = 0usize;
    let mut open = 0usize;

    for vertex in terrain.geosphere().vertices() {
        if terrain.is_ocean(vertex) {
            continue;
        }
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        match barrier_of(Seed(42), vertex, Band::Undercroft, 0, &pins) {
            BarrierState::Sealed => sealed += 1,
            BarrierState::Warded => warded += 1,
            BarrierState::Thin => thin += 1,
            BarrierState::Open => open += 1,
        }
    }

    println!(
        "barrier census (seed 42, Undercroft, branch 0): sealed={sealed} warded={warded} thin={thin} open={open}"
    );
    assert!(
        sealed + warded + thin > 0,
        "no barred cave mouth exists in seed 42 — acceptance criterion 1 is \
         unreachable and the campaign's cut must move"
    );
}

/// The address encoding must be injective across every field of
/// `ChamberAddr` — two different addresses must never collide on one key, or
/// clearing one passage would silently clear another.
///
/// MUTATION this must fail against: drop `branch` from `addr_key`'s format
/// string. Both addresses below then produce the same key and the assertion
/// fires.
///
/// Confirmed 2026-08-28: `assertion `left == right` failed: addr_key
/// collided: ["7/Undercroft/0", "8/Undercroft/0", "7/Undercroft/0",
/// "7/Undercroft/1"] left: 3 right: 4` — `base` and `by_branch` collided,
/// as expected once `branch` drops out of the key.
#[test]
fn addr_key_distinguishes_every_field() {
    use hornvale_kernel::{Band, Vertex};
    use hornvale_vessel::passage::addr_key;
    use hornvale_worldgen::chamber::ChamberAddr;

    let base = ChamberAddr {
        vertex: Vertex(7),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let by_vertex = ChamberAddr {
        vertex: Vertex(8),
        ..base
    };
    let by_branch = ChamberAddr { branch: 1, ..base };
    let by_level = ChamberAddr { level: 1, ..base };

    let keys = [
        addr_key(&base),
        addr_key(&by_vertex),
        addr_key(&by_branch),
        addr_key(&by_level),
    ];
    let unique: std::collections::BTreeSet<&String> = keys.iter().collect();
    assert_eq!(unique.len(), keys.len(), "addr_key collided: {keys:?}");
}

/// The lineage key a cave mouth derives from is a SAVE-FORMAT CONTRACT, and
/// the injectivity test above cannot see it move. Injectivity survives any
/// renaming: `addr_key` spells the band with `{:?}`, so renaming a `Band`
/// variant keeps every key distinct while changing what every key SAYS.
///
/// **This test REPLACES `addr_key_spelling_is_the_permanent_on_disk_key`,
/// and it is deliberately the wider of the two rather than its deletion.**
/// The Latch pinned `addr_key` because it was the `Value::Text` object of a
/// `passage-cleared` fact: a changed spelling made `effective_state` miss
/// every committed fact and silently re-bar a cleared passage. Task 8
/// retired that predicate, so that exact hazard is gone — and the string did
/// not stop being a save-format contract, it got a STRONGER one. `addr_key`
/// is now the address leg of `cave_mouth_role`, which is the `role` of a
/// `Lineage`, which is an input to a derived `EntityId`. A changed spelling
/// therefore renumbers every cave mouth in every saved world: every
/// `instance-of` and every `openness` fact about one is orphaned at once, not
/// merely missed by one lookup.
///
/// So the literal pinned here CONTAINS the literal the old test pinned, and
/// pinning the outer one is what a reader of `cave_mouth_role` actually
/// needs: `thing@passage/` is load-bearing too (it is what keeps a cave
/// mouth's derivation space apart from a room thing's), and no assertion in
/// the tree watched it before this one. Deleting the old test and stopping
/// there would have removed a guard and replaced it with nothing.
///
/// This is the same shape `thing::room_key`'s
/// `a_rooms_key_is_the_permanent_on_disk_spelling` and `thing::thing_role`'s
/// `the_thing_role_spelling_is_the_permanent_lineage_key` already have, and
/// it is stated the same way: reddening here is the intended outcome of a
/// rename, not an obstacle to one — the fix is an epoch (root CLAUDE.md:
/// "deliberate regeneration uses an epoch suffix, never a rename"), and the
/// choice should be made deliberately rather than discovered by a player.
///
/// **THE FIXTURE'S FOUR FIELDS ARE PAIRWISE DISTINCT ON PURPOSE (fix round
/// 1, m2).** It inherited `branch: 0, level: 0` from The Latch, and two
/// equal fields make a literal blind to the one mutation that reorders them:
/// transposing `branch` and `level` in `addr_key`'s format string left all
/// 831 vessel tests green, and `addr_key_distinguishes_every_field` cannot
/// see it either — injectivity survives any permutation of the fields, which
/// is precisely the blind zone this test exists to cover. With `branch: 2,
/// level: 5` the transposition moves the literal, so the pair of tests now
/// covers renaming AND reordering between them.
///
/// MUTATION this must fail against: rename `Band::Undercroft` (any variant
/// reachable from a cave entrance address will do). Confirmed 2026-08-29 by
/// the cheaper equivalent that perturbs the same output — swapping
/// `addr_key`'s `{:?}` band field for `{}`-formatted `addr.band as u8`,
/// which is what a `Debug`-spelling change amounts to on the wire:
///
/// ```text
/// assertion `left == right` failed: the cave mouth's lineage role changed
/// spelling — this is a save-format contract; see the doc comment
///   left: "thing@passage/7/1/0/0/cave-mouth"
///  right: "thing@passage/7/Undercroft/0/0/cave-mouth"
/// ```
///
/// SECOND MUTATION, the one the old fixture could not see: transpose
/// `branch` and `level` in `addr_key`'s argument list. Confirmed 2026-08-29
/// against the new fixture:
///
/// ```text
/// assertion `left == right` failed: the cave mouth's lineage role changed
/// spelling — this is a save-format contract; see the doc comment
///   left: "thing@passage/7/Undercroft/5/2/cave-mouth"
///  right: "thing@passage/7/Undercroft/2/5/cave-mouth"
/// ```
///
/// Genuine behavioural reds, not compile errors; restored and re-run green.
#[test]
fn the_cave_mouth_role_spelling_is_the_permanent_lineage_key() {
    use hornvale_kernel::{Band, Vertex};
    use hornvale_vessel::passage::cave_mouth_role;
    use hornvale_worldgen::chamber::ChamberAddr;

    // Four distinct field values: see the doc above — equal fields cannot
    // witness a transposition.
    let addr = ChamberAddr {
        vertex: Vertex(7),
        band: Band::Undercroft,
        branch: 2,
        level: 5,
    };
    assert_eq!(
        cave_mouth_role(&addr),
        "thing@passage/7/Undercroft/2/5/cave-mouth",
        "the cave mouth's lineage role changed spelling — this is a \
         save-format contract; see the doc comment"
    );
}

/// A cave mouth is a THING, not a private key: the entity `cave_mouth_id`
/// derives must be the entity the ledger mints when the mouth is promoted, or
/// `effective_state` would fold over facts about an entity nothing ever
/// wrote to.
///
/// The two agree by construction — both route through `thing::id_for_role`
/// with `cave_mouth_role` — and that is exactly why it is asserted here
/// rather than assumed: `thing.rs`'s own `thing_lineage` doc records that a
/// duplicated `Lineage` literal was once a LIVE seam in this crate, with
/// mutating one copy leaving the whole vessel suite green.
///
/// MUTATION this must fail against: change `cave_mouth_id`'s hardcoded
/// ordinal from `0` to `1`, leaving `set_openness`'s at `0`. The derived id
/// and the promoted id then differ.
///
/// Confirmed 2026-08-29:
///
/// ```text
/// assertion `left == right` failed: cave_mouth_id must derive the entity
/// set_openness promotes
///   left: EntityId(2024263159753080833)
///  right: EntityId(2024263159753080832)
/// ```
#[test]
fn cave_mouth_id_derives_the_entity_promotion_mints() {
    use hornvale_kernel::{Band, ConceptRegistry, Ledger, Vertex, WorldTime};
    use hornvale_vessel::passage::{cave_mouth_id, set_openness};
    use hornvale_vessel::thing::{OPENNESS, OPENNESS_DOC};
    use hornvale_worldgen::chamber::ChamberAddr;

    let addr = ChamberAddr {
        vertex: Vertex(7),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_kernel::INSTANCE_OF, false, "t")
        .expect("instance-of registers");
    reg.register_predicate(OPENNESS, false, OPENNESS_DOC)
        .expect("openness registers");
    let mut ledger = Ledger::default();
    let day = WorldTime::from_std_days(3.0).expect("3 days is in range");

    let promoted =
        set_openness(&mut ledger, &reg, &addr, true, day).expect("the mouth promotes and opens");
    assert_eq!(
        cave_mouth_id(&addr),
        promoted,
        "cave_mouth_id must derive the entity set_openness promotes"
    );
    assert!(
        ledger
            .facts_about(promoted)
            .any(|f| f.predicate == hornvale_kernel::INSTANCE_OF),
        "a promoted cave mouth must carry its instance-of fact: a thing the \
         ledger cannot name its kind for has not joined the object model"
    );
}

/// **The Chattel's own headline for this task: the latch is no longer
/// monotone.** Decision 0367 ruled that a cleared passage stays clear and
/// named the cost — *"a monotone latch cannot express a trap … the first
/// thing anyone will want"*. Decision 0396 supersedes it, and this is what
/// supersession means in behaviour: a `Thin` mouth opened on day 3 and closed
/// on day 6 is Open at day 4 and BARRED again at day 7.
///
/// Nothing in the shipped verb surface closes a passage yet (`open`/`close`
/// are Task 11), so the close is written through `passage::set_openness`
/// directly — the same door `Session::clear_passage_at` uses, one argument
/// different. The fold is the subject of this test; the verb is not.
///
/// MUTATION this must fail against: restore monotonicity in
/// `effective_state` by asking whether ANY openness fact at or before the day
/// says open, rather than the LATEST one — i.e. replace the
/// `thing::is_open(..) == Some(true)` test with a scan over
/// `ledger.facts_of(cave_mouth_id(addr), OPENNESS)` accepting any
/// `Value::Flag(true)` dated at or before `day`. That is precisely The
/// Latch's rule, and it type-checks.
///
/// Confirmed 2026-08-29:
///
/// ```text
/// assertion `left == right` failed: a cave mouth closed on day 6 must be
/// barred again on day 7 — decision 0396 retires the monotone latch
///   left: Open
///  right: Thin
/// ```
///
/// A genuine behavioural red, not a compile error; restored and re-run green.
#[test]
fn a_cave_mouth_closed_after_it_was_opened_bars_again() {
    use hornvale_kernel::{Band, ConceptRegistry, Ledger, Seed, Vertex, WorldTime};
    use hornvale_vessel::passage::{effective_state, set_openness};
    use hornvale_vessel::thing::{OPENNESS, OPENNESS_DOC};
    use hornvale_worldgen::chamber::ChamberAddr;
    use hornvale_worldgen::{BarrierPins, BarrierState};

    // A barrier the seed makes `Thin`, forced through the pin so this test
    // does not depend on which vertex the terrain happens to bar.
    let pins = BarrierPins {
        state: Some(BarrierState::Thin),
    };
    let addr = ChamberAddr {
        vertex: Vertex(1),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_kernel::INSTANCE_OF, false, "t")
        .expect("instance-of registers");
    reg.register_predicate(OPENNESS, false, OPENNESS_DOC)
        .expect("openness registers");
    let mut ledger = Ledger::default();

    let day = |d: f64| WorldTime::from_std_days(d).expect("a small day is in range");
    set_openness(&mut ledger, &reg, &addr, true, day(3.0)).expect("the mouth opens");
    set_openness(&mut ledger, &reg, &addr, false, day(6.0)).expect("the mouth closes");

    assert_eq!(
        effective_state(&ledger, Seed(42), &addr, day(1.0), &pins),
        BarrierState::Thin,
        "before the opening, the seeded barrier stands"
    );
    assert_eq!(
        effective_state(&ledger, Seed(42), &addr, day(4.0), &pins),
        BarrierState::Open,
        "between the opening and the closing, the mouth is open"
    );
    assert_eq!(
        effective_state(&ledger, Seed(42), &addr, day(7.0), &pins),
        BarrierState::Thin,
        "a cave mouth closed on day 6 must be barred again on day 7 — \
         decision 0396 retires the monotone latch"
    );
}

/// The asymmetry decision 0396 states, and the half a reader is likeliest to
/// assume the other way: closing a mouth the seed drew as `Open` reads `Open`
/// still. `effective_state` falls back to `barrier_of`, so a close does not
/// INVENT a barrier — it withdraws an opening. A trap needs a seeded barrier
/// to fall back to.
///
/// Worth its own test rather than a sentence, because it is the one input
/// under which "closed" and "open" produce the same answer, and a future
/// implementer reaching for `Sealed` on a close would pass every other
/// assertion in this file.
///
/// MUTATION this must fail against: make `effective_state` return
/// `BarrierState::Sealed` when `is_open` is `Some(false)`, instead of falling
/// through to `barrier_of`.
///
/// Confirmed 2026-08-29:
///
/// ```text
/// assertion `left == right` failed: closing a mouth the seed drew Open must
/// leave it Open — a close withdraws an opening, it does not invent a barrier
///   left: Sealed
///  right: Open
/// ```
#[test]
fn a_closed_cave_mouth_falls_back_to_the_barrier_the_seed_drew() {
    use hornvale_kernel::{Band, ConceptRegistry, Ledger, Seed, Vertex, WorldTime};
    use hornvale_vessel::passage::{effective_state, set_openness};
    use hornvale_vessel::thing::{OPENNESS, OPENNESS_DOC};
    use hornvale_worldgen::chamber::ChamberAddr;
    use hornvale_worldgen::{BarrierPins, BarrierState};

    let pins = BarrierPins {
        state: Some(BarrierState::Open),
    };
    let addr = ChamberAddr {
        vertex: Vertex(1),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_kernel::INSTANCE_OF, false, "t")
        .expect("instance-of registers");
    reg.register_predicate(OPENNESS, false, OPENNESS_DOC)
        .expect("openness registers");
    let mut ledger = Ledger::default();
    let day = |d: f64| WorldTime::from_std_days(d).expect("a small day is in range");

    set_openness(&mut ledger, &reg, &addr, false, day(2.0)).expect("the mouth closes");

    assert_eq!(
        effective_state(&ledger, Seed(42), &addr, day(5.0), &pins),
        BarrierState::Open,
        "closing a mouth the seed drew Open must leave it Open — a close \
         withdraws an opening, it does not invent a barrier"
    );
}

/// An opening fact must not open the passage for days BEFORE it. This is what
/// separates a time-correct fold from a mutable flag, and it is what lets any
/// replay evaluating a past instant stay honest.
///
/// **The `<= day` filter moved house with Task 8 and did not weaken.** It
/// used to live in `effective_state`'s own `find`; it now lives in
/// `thing::latest_object_at_or_before`, which `thing::is_open` is made of and
/// which `thing::location_of` shares. So this test and `thing.rs`'s own
/// as-of-day tests now guard one implementation between them rather than two
/// that agree — which is why the mutation below is stated against that fold
/// rather than against `effective_state`.
///
/// MUTATION this must fail against: neutralise the `if d > day { continue; }`
/// guard in `thing::latest_object_at_or_before`, so every dated fact is
/// accepted regardless of its day. Applied as `if false && d > day` rather
/// than by deletion — same behaviour, and it keeps `d` bound so the red is a
/// behavioural one rather than a compile error, which is the distinction this
/// project requires a mutation record to make. The day-1 assertion below then
/// reports Open.
///
/// Confirmed 2026-08-29: `assertion `left == right` failed: a passage opened
/// on day 5 must be barred on day 1 left: Open right: Sealed`.
#[test]
fn an_opening_fact_does_not_open_the_passage_before_it_happened() {
    use hornvale_kernel::{Band, ConceptRegistry, Ledger, Seed, Vertex, WorldTime};
    use hornvale_vessel::passage::{effective_state, set_openness};
    use hornvale_vessel::thing::{OPENNESS, OPENNESS_DOC};
    use hornvale_worldgen::chamber::ChamberAddr;
    use hornvale_worldgen::{BarrierPins, BarrierState};

    // A barrier the seed makes non-Open, forced through the pin so this test
    // does not depend on which vertex the terrain happens to bar.
    let pins = BarrierPins {
        state: Some(BarrierState::Sealed),
    };
    let addr = ChamberAddr {
        vertex: Vertex(1),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };

    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_kernel::INSTANCE_OF, false, "t")
        .expect("instance-of registers");
    reg.register_predicate(OPENNESS, false, OPENNESS_DOC)
        .expect("openness registers");
    let mut ledger = Ledger::default();

    let opened_on = WorldTime::from_std_days(5.0).expect("5 days is in range");
    set_openness(&mut ledger, &reg, &addr, true, opened_on).expect("the mouth opens");

    let day1 = WorldTime::from_std_days(1.0).expect("1 day is in range");
    let day9 = WorldTime::from_std_days(9.0).expect("9 days is in range");
    let before = effective_state(&ledger, Seed(42), &addr, day1, &pins);
    let after = effective_state(&ledger, Seed(42), &addr, day9, &pins);

    assert_eq!(
        before,
        BarrierState::Sealed,
        "a passage opened on day 5 must be barred on day 1"
    );
    assert_eq!(
        after,
        BarrierState::Open,
        "a passage opened on day 5 must be open on day 9"
    );
}
/// `set_openness`'s idempotence is `Ledger::commit`'s dedup of an identical
/// fact, and `Fact` derives `PartialEq` over `day` too — so it holds WITHIN a
/// day and not across one (fix round 1, m4). Two calls on different days
/// leave one entity and TWO `instance-of` facts saying the same thing.
///
/// **Pinned rather than merely documented because Task 11's `open`/`close`
/// is the caller that meets it.** `clear` cannot reach this shape (it writes
/// only for `Thin`, and a second `clear` sees `Open` and writes nothing), so
/// nothing in the crate would notice the day the caveat stopped being true —
/// in either direction. Reading `2` here is the current, deliberate
/// behaviour; a future dedup that made promotion day-independent should
/// change this number on purpose, not discover it.
#[test]
fn promoting_a_cave_mouth_on_a_second_day_repeats_its_instance_of() {
    use hornvale_kernel::{Band, ConceptRegistry, INSTANCE_OF, Ledger, Vertex, WorldTime};
    use hornvale_vessel::passage::set_openness;
    use hornvale_vessel::thing::{OPENNESS, OPENNESS_DOC};
    use hornvale_worldgen::chamber::ChamberAddr;

    let addr = ChamberAddr {
        vertex: Vertex(3),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(INSTANCE_OF, false, "t")
        .expect("instance-of registers");
    reg.register_predicate(OPENNESS, false, OPENNESS_DOC)
        .expect("openness registers");
    let mut ledger = Ledger::default();

    let day3 = WorldTime::from_std_days(3.0).expect("3 days is in range");
    let day6 = WorldTime::from_std_days(6.0).expect("6 days is in range");
    let first = set_openness(&mut ledger, &reg, &addr, true, day3).expect("the mouth opens");
    let same_day = set_openness(&mut ledger, &reg, &addr, true, day3).expect("the mouth opens");
    assert_eq!(
        ledger.find(INSTANCE_OF).count(),
        1,
        "within one day the promotion is idempotent: the second commit is the \
         identical fact and dedups"
    );

    let later = set_openness(&mut ledger, &reg, &addr, true, day6).expect("the mouth opens");
    assert_eq!(
        ledger.find(INSTANCE_OF).count(),
        2,
        "across days it is NOT: `day` is one of the fields `Fact` compares, so \
         the second promotion is a different fact and commits"
    );
    assert_eq!(
        (first, same_day),
        (later, later),
        "one address is one entity on every day; only the FACT count moves"
    );
}
