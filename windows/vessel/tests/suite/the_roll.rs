//! The Roll (spec §8): the company probe (M1), the roll's own properties
//! (M3), dormancy (§3.7), and presence at scale (§4).

// The Tidemark, Task 3: the DEMO subject, chosen rather than inherited —
// `village_info` is "the first `is-settlement` fact in ledger order", which
// was never a claim about where a walk stands. See
// `hornvale_worldgen::land_settlement`'s own doc.
use hornvale_vessel::{PossessOpts, Session};
use hornvale_worldgen::land_settlement as village_info;

use crate::common;
use crate::session::{open_staged_dragons_session, say};

/// One seed's reading for M1: does a fresh flagship possession have anyone in
/// `sensed.present` on its first look, and could it — is the home
/// settlement's `population >= 2`?
fn company_at(seed: u64) -> Option<(bool, bool)> {
    let world = common::build(seed)?;
    let village = hornvale_worldgen::land_settlement(&world)?;
    let (session, _) = Session::start(&world, &PossessOpts::default()).ok()?;
    let snap = session.snapshot().ok()?;
    Some((!snap.sensed.present.is_empty(), village.population >= 2))
}

/// M1, the preregistered count (spec §8): across `SIGHT_SEEDS`, the number of
/// seeds with company on the first look, beside the number whose home
/// settlement could hold company at all. Prediction: equal after the roll.
/// Today's reading, measured at Task 1: seeds built 64, with company 3,
/// population>=2 64 — most fresh possessions start alone even though every
/// built seed's home settlement could hold company; seed 42 was one of the 61
/// that started alone. Task 7 inverted that for seed 42 — see
/// `seed_42_starts_in_company` below, which replaced the
/// `seed_42_starts_alone_today` pin this sentence used to point at.
///
/// **The Task 14 reading, 2026-09-02, after the roll: seeds built 64, with
/// company 64, population>=2 64.** The prediction held exactly — every seed
/// whose home settlement can hold company now does, and there is no seed at
/// which the two numbers disagree.
///
/// Ignored for cost: 64 world builds is ~4 min (`ooc_objective.rs:572`
/// measured a full miss at 233.72 s). Run by hand at Task 1 and Task 14.
///
/// No `MUTATION THIS MUST FAIL AGAINST:` line, and the absence is deliberate:
/// this probe carries no assertion at all — it prints a count. Its instrument
/// is the print, and the reading is recorded in the campaign ledger and the
/// chronicle, where a wrong number is caught by a reader rather than by a
/// mutation.
///
/// claim: rate(M1: company-on-first-look, and population>=2, over SIGHT_SEEDS)
#[test]
#[ignore = "cost: 64 world builds (~4 min); run by hand at stage boundaries, results in the ledger"]
fn company_across_the_sight_seeds() {
    let mut with_company = 0usize;
    let mut could = 0usize;
    let mut built = 0usize;
    for seed in common::SIGHT_SEEDS {
        let Some((has, can)) = company_at(seed) else {
            continue;
        };
        built += 1;
        with_company += usize::from(has);
        could += usize::from(can);
    }
    println!("M1: seeds built {built}, with company {with_company}, population>=2 {could}");
}

/// One seed's reading for M5: after three unattended days at the flagship, do
/// two residents of the home settlement stand in different rooms?
///
/// The observable is co-location with the driven body, which is itself a
/// resident (ordinal 0, decision 0227): `Session::colocated_entities` is the
/// unfiltered "who is at my room" read — `agent_position(..) == position()`,
/// the same predicate `sensed.present` keys on, without sight's narrowing —
/// so a resident missing from it stands somewhere the driven one does not.
/// That is "two residents in different rooms" stated in the only terms an
/// integration test can ask: the session's own ledger is private, so
/// `liveness::agent_position` cannot be called from out here.
///
/// `None` for a seed that does not build or has no flagship village;
/// `Some(false)` for a settlement of one, which cannot separate at all.
fn separated_at(seed: u64) -> Option<bool> {
    let world = common::build(seed)?;
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).ok()?;
    // Three bare `wait`s: one world day each (`Session::wait`'s empty-argument
    // default), so this is spec §8's "by the end of day 3".
    for _ in 0..3 {
        let _ = session.handle("wait");
    }
    let driven = session.driven_body().entity;
    let home = session.driven_body().village.as_ref()?.id;
    let residents: Vec<_> = session
        .bodies()
        .iter()
        .filter(|b| b.village.as_ref().is_some_and(|v| v.id == home))
        .map(|b| b.entity)
        .collect();
    if residents.len() < 2 {
        return Some(false);
    }
    let here = session.colocated_entities();
    Some(residents.iter().any(|e| *e != driven && !here.contains(e)))
}

/// M5, the preregistered count (spec §8): across `SIGHT_SEEDS`, the number of
/// seeds in which the drawn per-resident deviation (§3.3) has visibly pulled
/// the settlement apart by the end of day 3 — two residents of the home
/// settlement standing in different rooms.
///
/// **No prediction beyond `> 0`, and a zero is a finding rather than a
/// failure**: the mechanism perturbs deliberation and boldness, not
/// destination, so it may well be that the dials alone do not separate a
/// blob. Task 7 already measured one reason a seed can read zero that has
/// nothing to do with individuation — seed 42's flagship condenses onto fresh
/// water, so its residents drink in place and never commit a position at all.
///
/// **The reading, 2026-09-02: seeds built 64, residents-separated 16.** The
/// null did not occur; a quarter of the probe's worlds show a settlement that
/// has come apart across a room boundary within three days. What the count
/// does NOT establish is attribution — it is separation, which is what §8
/// preregistered, not a controlled comparison against an unperturbed roll.
///
/// Ignored for the same cost as `company_across_the_sight_seeds` above (64
/// world builds; the three waits per seed are ~0.2 s beside a ~4 s build) and
/// carrying the identical reason string, so the frozen untokenised-ignore
/// roster (`cli/tests/suite/heavy_tier.rs`) is a set this does not widen.
///
/// No `MUTATION THIS MUST FAIL AGAINST:` line, and the absence is deliberate:
/// this probe carries no assertion at all — it prints a count. Its instrument
/// is the print, and the reading is recorded in the campaign ledger and the
/// chronicle, where a wrong number is caught by a reader rather than by a
/// mutation.
///
/// claim: rate(M5: two residents in different rooms by day 3, over SIGHT_SEEDS)
#[test]
#[ignore = "cost: 64 world builds (~4 min); run by hand at stage boundaries, results in the ledger"]
fn separation_across_the_sight_seeds() {
    let mut separated = 0usize;
    let mut built = 0usize;
    for seed in common::SIGHT_SEEDS {
        let Some(apart) = separated_at(seed) else {
            continue;
        };
        built += 1;
        separated += usize::from(apart);
    }
    println!("M5: seeds built {built}, residents-separated {separated}");
}

/// Seed 42's world, locale context, world components, and flagship village —
/// the fixture Task 4's tests share. Named construction site (decision
/// 0092): `WorldComponents::assemble()` is called here, once per test.
#[allow(clippy::disallowed_methods)]
fn residents_fixture() -> (
    hornvale_kernel::World,
    hornvale_locale::LocaleContext,
    hornvale_worldgen::WorldComponents,
    hornvale_settlement::VillageInfo,
) {
    let world = common::build(42).expect("seed 42 builds");
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("a context builds");
    let wc = hornvale_worldgen::WorldComponents::assemble()
        .expect("canonical registries are well-formed");
    let village = village_info(&world).expect("seed 42 places a flagship");
    (world, ctx, wc, village)
}

/// A frozen "now" for Task 4's tests — arbitrary but far enough from day 0
/// that a drawn age never pushes a resident's birth implausibly negative.
fn frozen_now() -> hornvale_kernel::WorldTime {
    hornvale_kernel::WorldTime::from_std_days(20_000.0).expect("finite")
}

/// Ordinal 0 of the roll IS today's body: the same lineage, the same
/// `EntityId` `derive_npcs` mints, so a possession selects the same creature
/// it did before this campaign (decision 0227, spec §2.1).
///
/// MUTATION THIS MUST FAIL AGAINST: change `role: "npc"` to `role:
/// "resident"` in `derive_residents`; every id moves and this reddens.
#[test]
fn ordinal_zero_is_the_body_derive_npcs_mints() {
    let (world, ctx, wc, village) = residents_fixture();

    let mut npc_ledger = world.ledger.clone();
    let npc = hornvale_vessel::liveness::derive_npcs(&world, &ctx, &mut npc_ledger, 1, village.id)
        .into_iter()
        .next()
        .expect("derive_npcs yields the home settlement's body");

    let mut resident_ledger = world.ledger.clone();
    let residents = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut resident_ledger,
        &wc,
        &village,
        frozen_now(),
    );

    assert_eq!(
        residents[0].entity, npc.entity,
        "ordinal 0's entity must be the lineage derive_npcs has always minted"
    );
}

/// A settlement's roll is exactly its committed population, no more (coarse
/// constrains fine, spec §2.2).
///
/// MUTATION THIS MUST FAIL AGAINST: `village.population + 1` in the loop
/// bound of `resident_draws`.
#[test]
fn the_roll_is_the_population() {
    let (world, ctx, wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();
    let residents = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        frozen_now(),
    );
    assert_eq!(residents.len(), village.population as usize);
}

/// Re-deriving over a ledger that already holds the residents mints nothing
/// new and commits nothing new: the derivation is idempotent, which is what
/// lets a saved session reload (`reuse_or_mint_entity`'s contract).
///
/// MUTATION THIS MUST FAIL AGAINST: replace `reuse_or_mint_entity` with
/// `mint_entity`; the second derivation panics on the collision.
#[test]
fn deriving_twice_is_idempotent() {
    let (world, ctx, wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();
    let now = frozen_now();

    let first =
        hornvale_vessel::residents::derive_residents(&world, &ctx, &mut ledger, &wc, &village, now);
    let len_after_first = ledger.len();

    let second =
        hornvale_vessel::residents::derive_residents(&world, &ctx, &mut ledger, &wc, &village, now);

    let first_entities: Vec<_> = first.iter().map(|b| b.entity).collect();
    let second_entities: Vec<_> = second.iter().map(|b| b.entity).collect();
    assert_eq!(
        first_entities, second_entities,
        "the same call yields the same roster of entities"
    );
    assert_eq!(
        ledger.len(),
        len_after_first,
        "a second derivation over the same ledger commits nothing new"
    );
}

/// A name the ledger already carries wins over a fresh draw (`NAME` is
/// functional; a recommit with a different value would contradict). Commit a
/// NAME for ordinal 1 by hand first, then derive: the body's label is the
/// hand-committed one.
///
/// MUTATION THIS MUST FAIL AGAINST: drop the `ledger.text_of(entity, NAME)`
/// read in `derive_residents` and always use the draw; the commit returns a
/// `Contradiction` and the `expect` panics.
#[test]
fn a_committed_name_wins_over_the_draw() {
    let (world, ctx, wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();

    let entity = ledger.reuse_or_mint_entity(hornvale_kernel::Lineage {
        parent: Some(village.id),
        role: "npc",
        ordinal: 1,
    });
    ledger
        .commit(
            hornvale_kernel::Fact {
                subject: entity,
                predicate: hornvale_kernel::NAME.to_string(),
                object: hornvale_kernel::Value::Text("Hand-Named".to_string()),
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &world.registry,
        )
        .expect("a hand-committed NAME on a freshly minted entity commits");

    let residents = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        frozen_now(),
    );

    assert_eq!(
        residents[1].label, "Hand-Named",
        "the ledger's own name wins over the draw"
    );
}

/// Every resident is a person: `is-person` and `person-born` are committed
/// on it, with `place` = the settlement and `person-born`'s day = `now` less
/// its drawn age.
///
/// MUTATION THIS MUST FAIL AGAINST: delete the `PERSON_BORN` commit.
#[test]
fn a_resident_is_a_person() {
    let (world, ctx, wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();
    let residents = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        frozen_now(),
    );

    for r in &residents {
        assert_eq!(
            ledger.value_of(r.entity, hornvale_person::IS_PERSON),
            Some(&hornvale_kernel::Value::Flag(true)),
            "{}: every resident is a committed person",
            r.label
        );
        assert!(
            ledger
                .value_of(r.entity, hornvale_person::PERSON_BORN)
                .is_some(),
            "{}: every resident carries a person-born day",
            r.label
        );
    }
}

/// Two residents of one settlement differ: on at least one dial, and in
/// label. (The blob finding, spec §1.)
///
/// MUTATION THIS MUST FAIL AGAINST: keep `body_at`'s dials instead of the
/// draw's (delete the three field overrides); every dial is the species
/// mean and this reddens on the dial half.
#[test]
fn residents_of_one_settlement_are_not_copies() {
    let (world, ctx, wc, mut village) = residents_fixture();
    village.population = village.population.max(2);
    let mut ledger = world.ledger.clone();
    let residents = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        frozen_now(),
    );

    assert!(residents.len() >= 2, "precondition: at least two residents");
    let a = &residents[0];
    let b = &residents[1];
    assert_ne!(a.label, b.label, "two residents share a label");
    let dial_differs = a.boldness != b.boldness
        || a.deliberation_latency != b.deliberation_latency
        || a.time_horizon != b.time_horizon;
    assert!(dial_differs, "residents 0 and 1 share every mind dial");
}

/// A resident's birth is fixed at its FIRST derivation: re-deriving over the
/// same ledger at a later `now` does not redraw `person-born` — the ledger
/// wins for a resident's birth day exactly as it does for their name
/// (controller ruling, Task 4 fix round 1).
///
/// MUTATION THIS MUST FAIL AGAINST: delete the
/// `ledger.value_of(entity, PERSON_BORN).is_none()` guard in
/// `derive_residents`; the second derivation (at `now_b`) recommits a
/// `person-born` that differs from the first (at `now_a`), which is a
/// functional `Contradiction`, and the `expect` panics.
#[test]
fn a_residents_birth_is_fixed_at_first_derivation() {
    let (world, ctx, wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();
    let now_a = frozen_now();
    let now_b =
        hornvale_kernel::WorldTime::from_std_days(now_a.as_std_days() + 30.0).expect("finite");

    let first = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        now_a,
    );
    let first_born: Vec<_> = first
        .iter()
        .map(|b| {
            ledger
                .value_of(b.entity, hornvale_person::PERSON_BORN)
                .cloned()
        })
        .collect();

    let second = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        now_b,
    );
    let second_born: Vec<_> = second
        .iter()
        .map(|b| {
            ledger
                .value_of(b.entity, hornvale_person::PERSON_BORN)
                .cloned()
        })
        .collect();

    let first_entities: Vec<_> = first.iter().map(|b| b.entity).collect();
    let second_entities: Vec<_> = second.iter().map(|b| b.entity).collect();
    assert_eq!(
        first_entities, second_entities,
        "re-deriving at a later day yields the same roster of entities"
    );
    assert_eq!(
        first_born, second_born,
        "a resident's committed person-born day does not move when now moves"
    );
}

/// A herd's identity does not depend on the order herds are listed in or on
/// which other herds are present: deriving `[a, b]` and `[b]` yields the same
/// entities for `b`.
///
/// MUTATION THIS MUST FAIL AGAINST: key the role on the herd's INDEX in
/// `herds` instead of its vertex; `b`'s ids move.
#[test]
fn a_herds_identity_is_its_attractor_and_species() {
    let (world, ctx, _wc, _village) = residents_fixture();
    let herd_a = hornvale_worldgen::herds::WildHerd {
        species: "wolf".to_string(),
        position: [1.0, 0.0, 0.0],
        vertex: 10,
        headcount: 2,
    };
    let herd_b = hornvale_worldgen::herds::WildHerd {
        species: "elk".to_string(),
        position: [0.0, 1.0, 0.0],
        vertex: 20,
        headcount: 3,
    };

    let mut ledger_ab = world.ledger.clone();
    let bodies_ab = hornvale_vessel::liveness::derive_wild_herds(
        &world,
        &ctx,
        &mut ledger_ab,
        &[herd_a, herd_b.clone()],
    );
    let b_entities_from_ab: Vec<_> = bodies_ab
        .iter()
        .filter(|b| b.species == "elk")
        .map(|b| b.entity)
        .collect();

    let mut ledger_b = world.ledger.clone();
    let bodies_b =
        hornvale_vessel::liveness::derive_wild_herds(&world, &ctx, &mut ledger_b, &[herd_b]);
    let b_entities_from_b: Vec<_> = bodies_b.iter().map(|b| b.entity).collect();

    assert_eq!(
        b_entities_from_ab, b_entities_from_b,
        "herd b's entities are identical whether or not herd a is present"
    );
}

/// Two herds of the SAME species at two DIFFERENT attractors are two herds,
/// not one: their bodies must be disjoint entity sets. A test with two
/// DIFFERENT species (as above) cannot see a regression that keys identity
/// on species alone, dropping the vertex — both herds' roles would still
/// differ (by species), so an all-different-species test stays green even
/// after that regression.
///
/// MUTATION THIS MUST FAIL AGAINST: drop `herd.vertex` from the role,
/// i.e. `format!("wild/{}", herd.species)` in place of
/// `format!("wild/{}/{}", herd.species, herd.vertex)`; both wolf herds then
/// share the role `"wild/wolf"` and their ordinal-0..headcount entities
/// collide, so the two sets are no longer disjoint.
#[test]
fn two_herds_of_one_species_at_different_vertices_are_disjoint() {
    let (world, ctx, _wc, _village) = residents_fixture();
    let herd_at_10 = hornvale_worldgen::herds::WildHerd {
        species: "wolf".to_string(),
        position: [1.0, 0.0, 0.0],
        vertex: 10,
        headcount: 2,
    };
    let herd_at_20 = hornvale_worldgen::herds::WildHerd {
        species: "wolf".to_string(),
        position: [0.0, 1.0, 0.0],
        vertex: 20,
        headcount: 2,
    };

    let mut ledger = world.ledger.clone();
    let bodies = hornvale_vessel::liveness::derive_wild_herds(
        &world,
        &ctx,
        &mut ledger,
        &[herd_at_10, herd_at_20],
    );
    assert_eq!(bodies.len(), 4, "precondition: both herds mint");

    let entities: std::collections::BTreeSet<_> = bodies.iter().map(|b| b.entity).collect();
    assert_eq!(
        entities.len(),
        bodies.len(),
        "two herds of one species at two attractors must mint two disjoint entity sets, \
         not collide into one"
    );
}

/// A herd yields exactly `headcount` bodies (coarse constrains fine).
///
/// MUTATION THIS MUST FAIL AGAINST: `0..=herd.headcount`.
#[test]
fn a_herd_is_its_headcount() {
    let (world, ctx, _wc, _village) = residents_fixture();
    let herd = hornvale_worldgen::herds::WildHerd {
        species: "wolf".to_string(),
        position: [1.0, 0.0, 0.0],
        vertex: 7,
        headcount: 5,
    };
    let mut ledger = world.ledger.clone();
    let bodies = hornvale_vessel::liveness::derive_wild_herds(&world, &ctx, &mut ledger, &[herd]);
    assert_eq!(bodies.len(), 5, "a herd yields exactly its headcount");
}

// ---------------------------------------------------------------------------
// Task 7: the roll itself, and the session that reads it.
// ---------------------------------------------------------------------------

use hornvale_kernel::RoomMeshMemo;
use hornvale_vessel::roll::{ROLL_BUDGET, ROLL_HOPS, RollKeyStatic, roll_of, rooms_within};

/// Unwrap a turn's text; a possession under test never releases.
fn out(turn: hornvale_vessel::Turn) -> String {
    match turn {
        hornvale_vessel::Turn::Out(text) => text,
        hornvale_vessel::Turn::Released(text) => {
            panic!("the possession released unexpectedly: {text}")
        }
    }
}

/// A seed whose flagship roll actually WALKS: its residents must leave home
/// for water, so a tick that reaches them commits `agent-at` facts and a tick
/// that does not commits none. Seed 42 cannot serve — its flagship stands on
/// a river and its residents drink in place — and the difference is exactly
/// what makes `a_body_off_the_roll_is_frozen`'s mutation non-null. Measured
/// over seeds 0..16 (possess, wait seven days, read
/// `committed_agent_at_count`): eleven walk, and seed 14 is the cheapest of
/// those whose residents never reach water at all (59 bodies, 673 positional
/// facts, no drink) — which matters here specifically, because a settlement
/// that walks ONCE and then drinks is dormant on the second wait whether or
/// not the roll filtered it, and that is a null mutation. A settlement that
/// keeps wandering is the one where "ticked" and "frozen" differ every tick.
/// `possession_moves.rs` pins the same number with the fuller note.
/// type-audit: bare-ok(index)
const WALKING_SEED: u64 = 14;

/// A live seed-42 flagship possession with the world's fauna on — the fixture
/// Task 7's session-level tests share.
fn flagship_session(world: &hornvale_kernel::World) -> Session<'_> {
    Session::start(world, &PossessOpts::default())
        .expect("seed 42 builds and possesses")
        .0
}

/// The entity of every body in `bodies`, in roster order — the sequence two
/// sessions of one world must agree on.
fn entities(bodies: &[hornvale_vessel::body::Body]) -> Vec<hornvale_kernel::EntityId> {
    bodies.iter().map(|b| b.entity).collect()
}

/// The same, for a borrowed roster (what `Session::on_roll` returns).
fn borrowed_entities(bodies: &[&hornvale_vessel::body::Body]) -> Vec<hornvale_kernel::EntityId> {
    bodies.iter().map(|b| b.entity).collect()
}

/// The heading that undoes `dir`.
fn opposite(dir: &str) -> &'static str {
    match dir {
        "n" => "s",
        "s" => "n",
        "e" => "w",
        _ => "e",
    }
}

/// M3, the purity half: the roll is a function of (the bodies' homes, the
/// observer's room) and of NOTHING ELSE — measured across two independent
/// SESSIONS, not across two calls.
///
/// **The two-call version this replaces was vacuous, and worth naming as
/// such.** It called `roll_of` twice with identical arguments and asserted
/// the results matched, which is satisfied by any deterministic function of
/// its arguments — including one that read the whole ledger, as long as it
/// read the same ledger twice. Purity is a claim about what the roll may
/// depend on, so the instrument has to be two states that differ in
/// everything the roll may NOT read while agreeing on the two things it may.
///
/// So the second possession reaches the same room by a detour — one step out
/// and one step back — which leaves its ledger two `agent-at` facts longer,
/// its day advanced by two walks and its turn counter four higher, while
/// every settlement and every herd stands exactly where it did. `bodies()`
/// and `on_roll()` must agree entity-for-entity after each of three waits.
///
/// MUTATION THIS MUST FAIL AGAINST: make the mask read the ledger — append
/// `let n = self.on_roll.len(); if n > 0 { self.on_roll[self.ledger.len() % n] = false; }`
/// to `Session::recompute_roll_mask_at`.
#[test]
fn the_roll_agrees_across_two_independent_sessions() {
    let world = common::build(42).expect("seed 42 builds");
    let mut still = flagship_session(&world);
    let mut detoured = flagship_session(&world);

    let home = detoured.position();
    let mut stepped = false;
    for dir in ["n", "e", "s", "w"] {
        let _ = out(detoured.handle(&format!("go {dir}")));
        if detoured.position() != home {
            let _ = out(detoured.handle(&format!("go {}", opposite(dir))));
            if detoured.position() == home {
                stepped = true;
                break;
            }
        }
    }
    assert!(
        stepped,
        "precondition: the second possession must reach the same room by a \
         different route, or the two sessions differ in nothing and this test \
         is the vacuous one it replaces"
    );
    assert_ne!(
        still.committed_agent_at_count(),
        detoured.committed_agent_at_count(),
        "precondition: and the detour must actually have moved the ledger"
    );
    assert_eq!(
        still.position(),
        detoured.position(),
        "precondition: while leaving both possessions in the same room"
    );

    for step in 0..3 {
        let _ = out(still.handle("!wait 1"));
        let _ = out(detoured.handle("!wait 1"));
        assert_eq!(
            entities(still.bodies()),
            entities(detoured.bodies()),
            "wait {step}: the roster is a function of the world, not of the route"
        );
        assert_eq!(
            borrowed_entities(&still.on_roll()),
            borrowed_entities(&detoured.on_roll()),
            "wait {step}: and so is the roll"
        );
    }
}

/// M3, the bounded half: no settlement contributes more than its population,
/// and never more than the asked-for budget in total.
///
/// The herd half of "bounded" is held one layer down, by Task 6's
/// `a_herd_is_its_headcount`: a herd yields exactly `headcount` bodies, so a
/// roll cannot contain more of one than the world says stand there. Asserting
/// it again here would need the herd key a `Body` deliberately does not
/// carry, and would re-measure `derive_wild_herds` rather than `roll_of`.
///
/// MUTATION THIS MUST FAIL AGAINST: `budget + 1` in the truncation.
#[test]
fn the_roll_is_bounded() {
    let world = common::build(42).expect("seed 42 builds");
    let session = flagship_session(&world);
    let observer = session.position();

    let mut memo = RoomMeshMemo::new();
    let full = roll_of(
        session.bodies(),
        session.roll_keys(),
        &observer,
        ROLL_HOPS,
        ROLL_BUDGET,
        &mut memo,
    );
    assert_eq!(
        full.len(),
        session.bodies().len(),
        "the mask covers the roster"
    );

    // No settlement contributes more than its committed population.
    let mut per_settlement: std::collections::BTreeMap<hornvale_kernel::EntityId, usize> =
        std::collections::BTreeMap::new();
    for (body, on) in session.bodies().iter().zip(&full) {
        if !on {
            continue;
        }
        if let Some(village) = body.village.as_ref() {
            *per_settlement.entry(village.id).or_default() += 1;
        }
    }
    for body in session.bodies() {
        if let Some(village) = body.village.as_ref() {
            let held = per_settlement.get(&village.id).copied().unwrap_or(0);
            assert!(
                held <= village.population as usize,
                "{}: the roll holds {held} of a settlement whose population is {}",
                village.name,
                village.population
            );
        }
    }

    // The budget truncates. A budget of three is well under seed 42's
    // flagship roster, so this arm actually exercises the cut rather than
    // passing vacuously.
    let mut memo_tight = RoomMeshMemo::new();
    let tight = roll_of(
        session.bodies(),
        session.roll_keys(),
        &observer,
        ROLL_HOPS,
        3,
        &mut memo_tight,
    );
    assert_eq!(
        tight.iter().filter(|on| **on).count(),
        3,
        "a budget of three admits exactly three bodies"
    );
    assert!(
        full.iter().filter(|on| **on).count() <= ROLL_BUDGET,
        "the roll never exceeds ROLL_BUDGET"
    );
}

/// A wild body's place in the order is its herd's (species, attractor
/// vertex), never its position in the roster — so two herds tied on hop
/// distance keep the same order whichever of them was derived first.
///
/// **The tie is the whole test.** Two herds AT ONE VERTEX of DIFFERENT
/// species agree on every earlier term of the key (hops, wild, parent), so
/// only `species` can separate them; drop it and they tie on the whole key
/// and fall through to the body's roster INDEX, which is derivation order —
/// which is the order they entered the observer's window, which is the route
/// the player walked. Under a binding budget that makes the MASK
/// route-dependent, and spec §3.2 says a wild body is keyed "never by its
/// position in a list".
///
/// MUTATION THIS MUST FAIL AGAINST: drop `species` from the key —
/// `species: statics.species` becomes `species: String::new()` in
/// `roll_of`'s `RollKey` construction.
#[test]
fn two_herds_at_one_vertex_do_not_depend_on_derivation_order() {
    let (world, ctx, _wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();
    let anchor =
        hornvale_vessel::liveness::derive_npcs(&world, &ctx, &mut ledger, 1, village.id)[0].clone();
    let room = anchor.home.clone();

    // Both herds stand at ONE attractor, so they agree on hops and on parent.
    let vertex = 4242u32;
    let wolf = hornvale_worldgen::herds::WildHerd {
        species: "wolf".to_string(),
        position: room.centroid(),
        vertex,
        headcount: 1,
    };
    let elk = hornvale_worldgen::herds::WildHerd {
        species: "elk".to_string(),
        position: room.centroid(),
        vertex,
        headcount: 1,
    };

    // Derive each herd on its own ledger clone, so neither derivation order
    // can influence the other's entities either.
    let derive = |herd: &hornvale_worldgen::herds::WildHerd| {
        let mut l = world.ledger.clone();
        hornvale_vessel::liveness::derive_wild_herds(
            &world,
            &ctx,
            &mut l,
            std::slice::from_ref(herd),
        )
        .remove(0)
    };
    let wolf_body = derive(&wolf);
    let elk_body = derive(&elk);
    let wolf_key = RollKeyStatic::herd_member(wolf.vertex, &wolf.species, 0);
    let elk_key = RollKeyStatic::herd_member(elk.vertex, &elk.species, 0);
    assert_eq!(
        wolf_body.home.pack().ok(),
        elk_body.home.pack().ok(),
        "precondition: both herds stand in one room, so they tie on hops"
    );

    let admitted = |bodies: &[hornvale_vessel::body::Body], keys: &[RollKeyStatic]| {
        let mut memo = RoomMeshMemo::new();
        let mask = roll_of(bodies, keys, &room, ROLL_HOPS, 1, &mut memo);
        assert_eq!(
            mask.iter().filter(|on| **on).count(),
            1,
            "a budget of one admits exactly one body"
        );
        bodies
            .iter()
            .zip(&mask)
            .find(|(_, on)| **on)
            .map(|(b, _)| b.entity)
            .expect("one body is admitted")
    };

    let wolf_first = admitted(
        &[wolf_body.clone(), elk_body.clone()],
        &[wolf_key.clone(), elk_key.clone()],
    );
    let elk_first = admitted(&[elk_body.clone(), wolf_body.clone()], &[elk_key, wolf_key]);
    assert_eq!(
        wolf_first, elk_first,
        "the same herd is admitted whichever order the two were derived in"
    );
}

/// The append path (spec §3.2): a settlement whose room comes within call has
/// its residents derived ONCE, appended to every parallel vector, and read by
/// the mask that same refresh.
///
/// **Through the `refresh_roll_at` test seam, because no reachable session
/// exercises this.** A possession begins standing in its own settlement's
/// room and the nearest other settlement on seed 42 is a hundred-odd rooms
/// away, so every other session test in this campaign runs where the roster
/// never grows at all — the guards and the append would be untested code
/// sitting inside a green suite. See the seam's own doc.
///
/// MUTATION THIS MUST FAIL AGAINST: delete the `derived_settlements` guard in
/// `refresh_roll_at` (the `!self.derived_settlements.contains(&village.id)`
/// test); the second call appends the settlement a second time.
// Named construction site (decision 0092): sculpts seed 42 and fits its
// climate and demography ONCE, solely to reconstruct the same herd set
// `Session::start` builds for `herd_rooms`, so this test's isolation
// precondition can exclude herds as well as settlements (see the comment
// inside). Never a second, independent draw the sim depends on -- the world
// under test is `common::build(42)` above, and this derivation is read-only
// with respect to it.
#[allow(clippy::disallowed_methods)]
#[test]
fn a_settlement_coming_within_call_is_derived_once_and_only_appended() {
    let world = common::build(42).expect("seed 42 builds");
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("a context builds");
    let flagship = village_info(&world).expect("seed 42 places a flagship");
    // **THE NEIGHBOUR IS FOUND BY MEASUREMENT, NEVER BY POPULATION RANK, and
    // both campaigns in this merge learned that the hard way.** The selection
    // used to be simply "the most populous other settlement", which carried a
    // silent premise: that its room is within call of it ALONE. Ten new
    // peoples re-placed seed 42 from two directions at once (The Tidemark's
    // six marine, the Underworld Peoples' four subterranean) and the most
    // populous neighbour's room turned out to sit within call of a second
    // settlement too — the exact-population assertion below then failed by 15
    // bodies while the property it guards (derived ONCE, only appended) was
    // perfectly intact.
    //
    // Both branches replaced the rank premise with a search for a settlement
    // whose room window really does hold exactly one settlement. This is
    // main's form of that search, kept because it scans the WHOLE roster
    // rather than the twelve most populous, so it cannot run out of
    // candidates as the roster grows again.
    //
    // Selecting by the measurement rather than re-pinning a number keeps the
    // assertion below EXACT, which is what makes the deletion mutation named
    // above fail: a `>=` would pass under a double-append.
    let settlements = hornvale_settlement::all_settlements(&world);
    let mut mesh_memo = RoomMeshMemo::new();

    // THE ISOLATION PRECONDITION COVERS HERDS AS WELL AS SETTLEMENTS SINCE
    // 2026-09-12 (The Trencher's repair pass, ledger #25/#26), and the
    // omission is what this test died of rather than anything about the
    // append path.
    //
    // `refresh_roll_at` derives TWO populations from its window -- every
    // underived settlement in it AND every underived herd standing in it --
    // and this selection only ever excluded the first. On the pre-merge world
    // no herd happened to stand near the candidate it picked, so the
    // assertion below held by luck. On the merged world (Task 4's
    // per-metabolite supply change plus the absorbed four underworld peoples)
    // fourteen do, and the roster grew by 65 rather than 51.
    //
    // **MEASURED, because the shape of that failure is exactly the shape a
    // real invariant break would have.** Instrumenting the append (temporary
    // probe, run once, not committed) showed the 65 arrivals partition as 51
    // desert-dwarf -- the settlement's population, EXACTLY as this test
    // claims -- plus one body each for fourteen wild herds (black-dragon,
    // carrion-crawler, dire-wolf, giant-constrictor-snake, giant-crocodile,
    // giant-elk, giant-hyena, giant-scorpion, otyugh, owlbear, red-dragon,
    // rhinoceros, white-dragon, woolly-mammoth). The append path is correct
    // and the property this test names still holds; what had lapsed was the
    // precondition that isolates it. The herds share the settlement's room,
    // so they cannot be told apart by `home` -- the fix has to be in the
    // SUBJECT, not in a filter applied after the fact.
    let herd_rooms: std::collections::BTreeSet<_> = {
        let wc = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
        let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
        let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");
        let report = hornvale_worldgen::demography_report_from(&world, &wc, &terrain, &climate)
            .expect("seed 42 reports demography");
        hornvale_worldgen::herds::wild_herds_near(&wc, &report, |_| true)
            .into_iter()
            .filter_map(|herd| {
                hornvale_kernel::Facet::containing(herd.position, hornvale_locale::walk_depth(&ctx))
                    .pack()
                    .ok()
            })
            .collect()
    };

    let neighbour = settlements
        .iter()
        .filter(|v| v.id != flagship.id)
        .find(|candidate| {
            let mut probe = world.ledger.clone();
            let candidate_body =
                hornvale_vessel::liveness::derive_npcs(&world, &ctx, &mut probe, 1, candidate.id)
                    [0]
                .clone();
            let window = rooms_within(&candidate_body.home, ROLL_HOPS, &mut mesh_memo);
            if window.iter().any(|room| herd_rooms.contains(room)) {
                return false;
            }
            settlements
                .iter()
                .filter(|settlement| {
                    let mut probe = world.ledger.clone();
                    let body = hornvale_vessel::liveness::derive_npcs(
                        &world,
                        &ctx,
                        &mut probe,
                        1,
                        settlement.id,
                    )[0]
                    .clone();
                    window.contains(&body.home.pack().expect("derived room packs"))
                })
                .count()
                == 1
        })
        .cloned()
        .expect(
            "seed 42 places a settlement within the roll radius that is isolated from BOTH \
             other settlements and wild herds -- if this is what fails, the append path is \
             untestable on this world and that is a coverage finding, not a licence to drop \
             the herd half of the precondition",
        );

    // That settlement's own room, and the entity `derive_npcs` mints for it —
    // read off a THROWAWAY ledger, so nothing here is what the session under
    // test derives.
    let mut probe = world.ledger.clone();
    let probe_body =
        hornvale_vessel::liveness::derive_npcs(&world, &ctx, &mut probe, 1, neighbour.id)[0]
            .clone();
    let room = probe_body.home.clone();

    let mut session = flagship_session(&world);
    let before = session.bodies().len();
    let before_keys = session.roll_keys().len();
    assert_eq!(
        before, before_keys,
        "precondition: the roster and its keys start in step"
    );

    session.refresh_roll_at(&room);
    assert_eq!(
        session.bodies().len(),
        before + neighbour.population as usize,
        "a settlement coming within call appends exactly its population"
    );
    assert_eq!(
        session.roll_keys().len(),
        session.bodies().len(),
        "and its keys are appended in the same breath"
    );
    assert_eq!(
        session.bodies()[before].entity,
        probe_body.entity,
        "the first body appended is that settlement's ordinal 0 — the same \
         lineage `derive_npcs` mints"
    );
    // The mask was recomputed AT that room, so it holds the newcomers and not
    // the flagship's own residents, a hundred rooms away — plus the driven
    // body, whose slot is forced true wherever it stands.
    assert_eq!(
        session.roll_len(),
        neighbour.population as usize + 1,
        "the refresh's own mask reads the bodies it just appended"
    );

    let after_first = session.bodies().len();
    session.refresh_roll_at(&room);
    assert_eq!(
        session.bodies().len(),
        after_first,
        "a second refresh at the same room appends nothing"
    );
    assert_eq!(
        session.roll_keys().len(),
        after_first,
        "and adds no key either"
    );
}

/// The order is (distance, residents first, parent, species, ordinal): with a budget
/// of two at a room holding both residents and a herd, the roll is the two
/// lowest-ordinal residents, not the herd.
///
/// The herd is placed at the settlement's own room so that it TIES the
/// residents on hop distance. Without the tie the distance term would decide
/// the order by itself and the residents-first term would be untested — a
/// null mutation dressed as a passing test.
///
/// MUTATION THIS MUST FAIL AGAINST: sort wild before settled (swap the
/// `wild` field's sense in `RollKey`'s construction).
#[test]
fn the_roll_orders_home_first() {
    let (world, ctx, wc, village) = residents_fixture();
    let mut ledger = world.ledger.clone();
    let residents = hornvale_vessel::residents::derive_residents(
        &world,
        &ctx,
        &mut ledger,
        &wc,
        &village,
        frozen_now(),
    );
    assert!(
        residents.len() >= 2,
        "precondition: the flagship holds at least two residents"
    );
    let room = residents[0].home.clone();

    let herd = hornvale_worldgen::herds::WildHerd {
        species: "wolf".to_string(),
        position: room.centroid(),
        vertex: 4242,
        headcount: 3,
    };
    let wild = hornvale_vessel::liveness::derive_wild_herds(&world, &ctx, &mut ledger, &[herd]);
    assert_eq!(
        wild[0].home.pack().ok(),
        room.pack().ok(),
        "precondition: the staged herd shares the settlement's room, so it \
         ties the residents on hop distance"
    );

    let mut bodies: Vec<hornvale_vessel::body::Body> = residents.clone();
    let mut keys: Vec<RollKeyStatic> = residents
        .iter()
        .enumerate()
        .map(|(i, b)| RollKeyStatic::of(b, i as u16))
        .collect();
    // The wild bodies go FIRST in the roster after the residents' own two, so
    // that a mask that merely followed roster order could not pass either.
    for (i, body) in wild.iter().enumerate() {
        bodies.insert(i, body.clone());
        keys.insert(i, RollKeyStatic::of(body, i as u16));
    }

    let mut memo = RoomMeshMemo::new();
    let mask = roll_of(&bodies, &keys, &room, ROLL_HOPS, 2, &mut memo);
    let chosen: Vec<&hornvale_vessel::body::Body> = bodies
        .iter()
        .zip(&mask)
        .filter(|(_, on)| **on)
        .map(|(b, _)| b)
        .collect();
    assert_eq!(chosen.len(), 2, "a budget of two admits two bodies");
    assert!(
        chosen.iter().all(|b| b.village.is_some()),
        "residents sort before wild at equal distance; got {:?}",
        chosen.iter().map(|b| b.label.as_str()).collect::<Vec<_>>()
    );
    assert_eq!(
        chosen.iter().map(|b| b.entity).collect::<Vec<_>>(),
        vec![residents[0].entity, residents[1].entity],
        "and within a settlement the order is by ordinal"
    );
}

/// Seed 42's flagship possession starts in company (M1, inverted from Task
/// 1's pin): `sensed.present` is non-empty on the first look, and it holds
/// every OTHER resident of the home settlement — `population - 1` of them,
/// since everyone is home at day 0.
///
/// The assertion is stated as containment plus that count rather than as
/// `present.len() == population - 1`, because `sensed.present` is the whole
/// co-located roster: a wild herd standing on the settlement's own room would
/// legitimately make it longer, and an equality would then be pinning where
/// seed 42's fauna happens to sit rather than that the player has company.
///
/// MUTATION THIS MUST FAIL AGAINST: derive `1` resident in `start_held`
/// (`derive_npcs(world, ctx, &mut ledger, 1, village.id)` in place of
/// `derive_residents`).
#[test]
fn seed_42_starts_in_company() {
    let world = common::build(42).expect("seed 42 builds");
    let village = village_info(&world).expect("seed 42 places a flagship");
    let session = flagship_session(&world);
    let snap = session.snapshot().expect("a live session snapshots");

    assert!(
        village.population >= 2,
        "precondition: seed 42's flagship could hold company"
    );
    assert!(
        !snap.sensed.present.is_empty(),
        "a fresh possession at seed 42 no longer stands alone"
    );

    let driven = session.driven_body().entity;
    let housemates: Vec<u64> = session
        .bodies()
        .iter()
        .filter(|b| b.entity != driven)
        .filter(|b| b.village.as_ref().is_some_and(|v| v.id == village.id))
        .map(|b| b.entity.0.get())
        .collect();
    assert_eq!(
        housemates.len(),
        village.population as usize - 1,
        "the flagship's roll is its population, less the body you are"
    );
    let present: std::collections::BTreeSet<u64> =
        snap.sensed.present.iter().map(|p| p.entity).collect();
    for who in &housemates {
        assert!(
            present.contains(who),
            "every other resident is present on the first look; {who} is not"
        );
    }
}

/// Dormancy (§3.7): a body off the roll commits no `agent-at` across a wait.
/// The observer walks until its settlement's room is outside `ROLL_HOPS`,
/// which takes the settlement's residents off the roll; a resident's
/// committed `agent-at` count is then unchanged by a wait that would
/// otherwise have moved it.
///
/// **At `WALKING_SEED`, not 42, and the reason is what makes the mutation
/// below real.** Seed 42's flagship stands on a river: its residents drink in
/// place and commit no positional fact whether they are ticked or not, so the
/// same assertions pass with the roll's filter REMOVED — a null mutation
/// dressed as a passing test, measured directly before this test was moved. A
/// world whose residents must leave home for water is the only one where
/// "frozen" and "ticked" have different observable consequences. The first
/// wait below is what establishes that they are the ticking kind.
///
/// MUTATION THIS MUST FAIL AGAINST: hand `other_bodies` unfiltered to
/// `DriveMovements` in `wait`.
#[test]
fn a_body_off_the_roll_is_frozen() {
    let world = common::build(WALKING_SEED).expect("the walking seed builds");
    let mut session = flagship_session(&world);
    let home = session.driven_body().home.clone();
    let driven = session.driven_body().entity;
    let resident = session
        .bodies()
        .iter()
        .find(|b| b.entity != driven && b.village.is_some())
        .expect("the flagship's roll holds a second resident")
        .entity;

    // Cross the sustenance seek threshold (world day ~5.667) while everyone is
    // still within call, so the roster is demonstrably the ticking kind.
    let _ = out(session.handle("!wait 7"));
    assert!(
        session.committed_agent_at_count() >= 1,
        "precondition: seed {WALKING_SEED}'s residents walk when they are \
         ticked; if this is 0 an epoch has moved the seed"
    );

    // Walk out of the window. Every direction is tried in turn so that one
    // blocked heading does not strand the walk; the assertion below is what
    // makes a failed walk loud rather than vacuous.
    let mut memo = RoomMeshMemo::new();
    let window = rooms_within(&home, ROLL_HOPS, &mut memo);
    for _ in 0..12 {
        if !window.contains(&session.position().pack().expect("a walk-band room packs")) {
            break;
        }
        for dir in ["n", "e", "s", "w"] {
            let before = session.position();
            let _ = out(session.handle(&format!("go {dir}")));
            if session.position() != before {
                break;
            }
        }
    }
    assert!(
        !window.contains(&session.position().pack().expect("a walk-band room packs")),
        "precondition: the observer walked outside ROLL_HOPS of the flagship"
    );

    let before = session.committed_agent_at_count_for(resident);
    let session_before = session.committed_agent_at_count();
    let _ = out(session.handle("!wait 7"));
    assert_eq!(
        before,
        session.committed_agent_at_count_for(resident),
        "a resident whose settlement is out of call commits nothing across a wait"
    );
    assert_eq!(
        session_before,
        session.committed_agent_at_count(),
        "and neither does any of its neighbours — the whole settlement is dormant"
    );
    assert!(
        !session.on_roll().iter().any(|b| b.entity == resident),
        "and it is off the roll the wait read"
    );
}

/// Handles are stable: `bodies()` never reorders across waits — it only ever
/// grows at the end — and the driven index never moves
/// (`possessing_a_creature_does_not_renumber_other_bodies_handles`'s lesson,
/// session.rs:809-818). `wait`'s `before`/`narrate_motion` zip depends on it.
///
/// MUTATION THIS MUST FAIL AGAINST: `self.bodies.reverse()` at the end of
/// `refresh_roll`.
///
/// **Not the brief's "sort `self.bodies` by `on_roll`", because that is a
/// NULL here and the null is worth recording.** At seed 42's flagship every
/// derived body is within call, so the mask is all-`true`, and a stable sort
/// on a constant key permutes nothing at all — the test would pass against a
/// `refresh_roll` that really did reorder by the roll. Reversing is the same
/// defect stated in a form the world can actually exhibit.
#[test]
fn the_roster_never_reorders() {
    let world = common::build(42).expect("seed 42 builds");
    let mut session = flagship_session(&world);
    let before: Vec<String> = session.bodies().iter().map(|b| b.label.clone()).collect();
    let driven = session.driven_body().entity;

    for _ in 0..3 {
        let _ = out(session.handle("!wait 1"));
    }

    let after: Vec<String> = session.bodies().iter().map(|b| b.label.clone()).collect();
    assert!(
        after.len() >= before.len(),
        "the roster is append-only: it may grow, never shrink"
    );
    assert_eq!(
        &after[..before.len()],
        &before[..],
        "no body ever changes index; a wait only appends"
    );
    assert_eq!(
        session.driven_body().entity,
        driven,
        "the driven index never moves"
    );
}

// ---------------------------------------------------------------------------
// Task 9: the presence line (spec §4, "Presence at scale").
// ---------------------------------------------------------------------------

/// [`Session::N_NAMED`]'s own value, mirrored here — `session` is a private
/// module (`mod session;` in `lib.rs`), so the constant is not reachable
/// through the crate's public surface even though it is `pub` (the same
/// `pub`-for-type-audit shape `DISPOSITION_SHIFT`/`POSSESSED_BY` already
/// have, neither of which any integration test references either). A drift
/// between this literal and `session.rs`'s own would show up immediately —
/// every test below fails the moment the two disagree, since both derive
/// their expectations from the SAME sensed-roster count read off the live
/// session, not from a second hardcoded total.
const N_NAMED: usize = 4;

/// MERGE RE-PIN (The Trencher absorbing The Tidemark, 2026-09-15): a single
/// named group is no longer always the whole `Here: …` line.
/// `Session::observed_here` joins MULTIPLE groups with `"; "` — the
/// named-individuals group, then one per-species tally group per
/// additional fauna kind present (see `windows/vessel/src/session.rs`'s
/// `Some(format!("Here: {}.", rendered.join("; ")))`) — and the merged
/// world now has enough distinct fauna at this room to populate those
/// trailing groups where it did not before. [`total_sensed`] sums every
/// group's own count rather than assuming the first is the entire line.
///
/// How many items `chamber_prose::listed`'s output names — the inverse of
/// that function's join, over its two separators (`", "` between all but
/// the last pair, `" and "` between the last pair). A single item carries
/// no `" and "` at all.
fn count_listed(s: &str) -> usize {
    if !s.contains(" and ") {
        return 1;
    }
    s.matches(", ").count() + 2
}

/// One group's own headcount — a resident group (named individuals, with an
/// optional `", and K others"` tail) or a wild group (a single label, or
/// `"N species"`). See `Session::observed_here`'s `rendered` construction
/// for the exact three shapes this inverts.
fn group_count(group: &str) -> usize {
    if let Some((heads, tail)) = group.rsplit_once(", and ")
        && let Some(n) = tail
            .strip_suffix(" others")
            .and_then(|n| n.parse::<usize>().ok())
    {
        return count_listed(heads) + n;
    }
    if let Some((num, _rest)) = group.split_once(' ')
        && let Ok(n) = num.parse::<usize>()
    {
        return n;
    }
    count_listed(group)
}

/// The WHOLE `Here: …` line's headcount — every group's own count, summed.
/// This is what the sensed roster's `len()` is actually compared against;
/// `others_count` alone only ever covered the first (named) group, which
/// silently equalled the whole line until a second group could appear.
fn total_sensed(here_line: &str) -> usize {
    let body = here_line
        .strip_prefix("Here: ")
        .unwrap_or(here_line)
        .strip_suffix('.')
        .unwrap_or(here_line);
    body.split("; ").map(group_count).sum()
}

/// The one `Here: …` line of `text`, or `None` if it names nobody.
fn here_line(text: &str) -> Option<&str> {
    text.lines().find(|l| l.starts_with("Here: "))
}

/// `look` at seed 42's flagship — a crowd of `sensed.present.len()` (67,
/// measured at Task 9) — names exactly [`N_NAMED`] residents and counts the
/// rest: the line reads `Here: A, B, C and D, and {P} others.` where
/// `P + N_NAMED` is the sensed roster's own size, read off the live session
/// rather than hardcoded, so this test tracks the roll's own population
/// rather than pinning today's seed-42 headcount a second time.
///
/// MUTATION THIS MUST FAIL AGAINST: `N_NAMED + 1` in `presence_line`'s
/// `labels.iter().take(N_NAMED)`. Performed by hand: changed the take to
/// `N_NAMED + 1`, ran this test alone, and it went red on the separator
/// count, not the total (the `others` count stays consistent with a wider
/// take, since both read `labels.len() - N_NAMED` off the SAME unwidened
/// constant — the total-only assertion is a null against this exact
/// mutation, which is why the separator check exists beside it):
/// `Here: Dvoashngashngo, Qvoshngavngo, Shngovngo, Shngoqvo and Vngaobvo, and
/// 63 others.` — `assertion `left == right` failed: exactly N_NAMED names
/// must be listed before the count`, left 4, right 3 (5 names shown, 4
/// separators, one more than `N_NAMED - 1`) — then reverted the edit.
#[test]
fn look_names_a_few_and_counts_the_rest() {
    let world = common::build(42).expect("seed 42 builds");
    let (session, opening) = Session::start(&world, &PossessOpts::default()).expect("possesses");
    let sensed = session
        .snapshot()
        .expect("a live session snapshots")
        .sensed
        .present
        .len();
    assert!(
        sensed > N_NAMED,
        "precondition: this test exercises the `, and N others` tail, which \
         needs more than N_NAMED present. Got {sensed}"
    );
    let line = here_line(&opening).unwrap_or_else(|| panic!("no presence line: {opening:?}"));
    assert_eq!(
        total_sensed(line),
        sensed,
        "named + counted must equal the whole sensed roll: {line:?}"
    );
    // The named half really is capped at N_NAMED, not merely "some prefix":
    // count the separators between names (each is either ", " or the one
    // " and " before the last), which for N_NAMED items is N_NAMED - 1.
    let heads = line
        .strip_prefix("Here: ")
        .and_then(|rest| rest.split(", and ").next())
        .expect("a Here: line strips its own prefix");
    let separators = heads.matches(", ").count() + heads.matches(" and ").count();
    assert_eq!(
        separators,
        N_NAMED - 1,
        "exactly N_NAMED names must be listed before the count: {line:?}"
    );
}

/// Nobody present, no line at all. **Not attempted via a fresh possession's
/// own first look**, which is the natural first reading and turned out to be
/// a dead end worth recording: `common::world_where` searched all of
/// `SIGHT_SEEDS` (0..64) for a seed whose fresh flagship possession senses
/// nobody and found none — Task 7's roll change (M1's own prediction: company
/// on first look should track `population >= 2`, which is 64 of 64 built
/// seeds) means every fresh flagship possession in range now starts in
/// company. So this test WALKS to an empty room instead: a dormant
/// resident's `agent-at` fact never moves off the roll (§3.7), so
/// `colocated_npcs` — keyed on the possession's CURRENT position — finds
/// nobody the instant the possession no longer stands where the residents
/// are. Cheaper than the search besides: one world build and at most a
/// handful of steps, not up to 64 builds.
///
/// MUTATION THIS MUST FAIL AGAINST: `presence_line` returning
/// `Some("Here: .".to_string())` unconditionally instead of `None` on an
/// empty roll. Performed by hand: changed the `if roll.is_empty() { return
/// None; }` guard to always fall through and render an empty group list, ran
/// this test alone, and it went red on `!looked.contains("Here:")` — the
/// look reply carried a literal `Here: .` line — then reverted the edit.
#[test]
fn an_empty_room_says_nothing_about_company() {
    let world = common::build(42).expect("seed 42 builds");
    let mut session = flagship_session(&world);
    assert!(
        !session
            .snapshot()
            .expect("a live session snapshots")
            .sensed
            .present
            .is_empty(),
        "precondition: the flagship itself must start in company — otherwise \
         walking away tests nothing this task did not already have"
    );
    let mut looked = String::new();
    let mut empty = false;
    'walk: for _ in 0..3 {
        for dir in ["n", "e", "s", "w"] {
            let before = session.position();
            looked = out(session.handle(&format!("go {dir}")));
            if session.position() == before {
                continue;
            }
            empty = session
                .snapshot()
                .expect("a live session snapshots")
                .sensed
                .present
                .is_empty();
            if empty {
                break 'walk;
            }
        }
    }
    assert!(
        empty,
        "precondition: a short walk away from the flagship must reach an \
         empty room. Last look: {looked:?}"
    );
    assert!(
        !looked.contains("Here:"),
        "an empty room names no company: {looked:?}"
    );
    assert!(
        looked.starts_with("[room "),
        "the room's own rendering must still be intact with nobody present: {looked:?}"
    );
}

/// The presence line's count is the SENSED roster's, never the merely
/// co-located one: seed 42's flagship chamber, one companion placed
/// deliberately out of the shadowcast (`Session::place_creature_out_of_my_sight`,
/// the same seam `the_objective_needs_reads_a_creature_the_body_cannot_sense`
/// in `ooc_objective.rs` uses for exactly this precondition). Indoors that
/// makes `colocated_entities().len()` and `snapshot().sensed.present.len()`
/// disagree, and the presence line must track the second.
///
/// MUTATION THIS MUST FAIL AGAINST: `presence_line` reading
/// `self.colocated_npcs()` instead of `self.perceived_npcs(how)`. Performed
/// by hand: changed `let roll = self.perceived_npcs(how);` to
/// `let roll = self.colocated_npcs();`, ran this test alone, and it went
/// red on the EARLIER precondition, not the total: `Here: Dvoashngashngo,
/// Qvoshngavngo, Shngovngo and Shngoqvo, and 63 others.` — the hidden
/// companion is roll-order-first, so an unfiltered roster names it right in
/// the first four, tripping `!looked.contains(&hidden)` before the total
/// comparison is ever reached. Worth recording as-observed rather than
/// as-guessed: the total-equals-sensed assertion further down would have
/// caught this mutation too (63 others off an unfiltered 67 vs. the sensed
/// 65), but the precondition on `hidden`'s name firing first is what this
/// run actually showed. Reverted the edit afterward.
#[test]
fn the_count_is_the_sensed_roster() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let _ = out(session.handle("wait"));
    let _ = out(session.handle("enter"));
    let hidden = session.bodies()[1].label.clone();
    let companion = session.bodies()[1].entity;
    assert!(
        session.place_creature_out_of_my_sight(companion),
        "precondition: the entered chamber must have an anchor outside its own \
         shadowcast to place `{hidden}` on"
    );
    let colocated = session.colocated_entities().len();
    let sensed = session
        .snapshot()
        .expect("a live session snapshots")
        .sensed
        .present
        .len();
    assert!(
        sensed < colocated,
        "precondition: hiding `{hidden}` must actually separate the sensed \
         roll from the colocated one, else this test cannot distinguish \
         the two. Got sensed={sensed}, colocated={colocated}"
    );
    let looked = out(session.handle("look"));
    assert!(
        !looked.contains(&hidden),
        "precondition: `look` must withhold the hidden companion's name \
         (the sight gate `!needs`/`!examine` already enforce): {looked:?}"
    );
    let line = here_line(&looked).unwrap_or_else(|| panic!("no presence line: {looked:?}"));
    let total = total_sensed(line);
    assert_eq!(
        total, sensed,
        "the presence line's total must equal the SENSED roster, not the \
         merely co-located one: {line:?}"
    );
}

/// The wild-group render branch (`presence_line`'s `resident == false` arm —
/// collapse to a bare `"{species}"` for one, `"{n} {species}"` for
/// more) and the multi-group join/ordering (resident group first, `"; "`
/// between groups), in one test (review fix round 1 of Task 9).
///
/// **The singular form lost its `"a wild "` prefix under The Ken, Task 4
/// (round two).** It used to read `"a wild {species}"`, built fresh from
/// `species` — a fresh possession never actually reads that string, because
/// a wild body's `label` (what `examine` matches) is now the same bare
/// `species` this line renders, and `derive_wild_herds` — the derivation an
/// ordinary possession actually walks through — mints that label directly.
/// Before that round, `derive_wild_herds` still minted `"a wild {species}"`
/// labels, so this line's old species-built string coincidentally matched
/// them; the "coincidentally" is exactly what let the label carry the
/// double-article defect `liveness.rs` records ("The a wild
/// carrion-crawler looks lost") into real play undetected. This test's own
/// intent survives unchanged: a lone wild body is still ONE un-joined
/// clause following the resident group, not merged into it and not
/// pluralised — only the literal expected string moved.
///
/// **The plural form lost its own "wild" too (controller ruling, ledger
/// #7, The Ken, Task 5).** Task 4's article drop left the singular form
/// bare (`{species}`) while the plural form still read `"{n} wild
/// {species}"`, so one "Here:" line mixed both forms — a bare singleton
/// beside a "wild"-qualified group. This campaign introduced that
/// inconsistency, so it is this task's to close: the count clause now
/// reads `"{n} {species}"`, uniformly bare.
///
/// **No natural seed conveniently isolates ONE wild group beside
/// residents at a fresh look**, so this test builds the scene by hand
/// rather than searching further. Seed 1 is the first current witness from
/// the same search.
/// `possession_moves.rs`) was the first candidate tried and rejected: its
/// fresh flagship possession already shows all FOURTEEN of its wild
/// species simultaneously (measured: `Here: <4 named>, and 52 others;
/// black-dragon; carrion-crawler; …` — fifteen groups), which exercises the
/// collapse but not a clean single-semicolon ordering check. So instead:
/// walk seed 1's possession to an empty room (the same technique
/// `an_empty_room_says_nothing_about_company` uses — a dormant body's
/// `agent-at` never follows), then place exactly the bodies wanted with
/// `Session::place_creature_at_me`. Seed 1 was chosen because its roster
/// derives seven bodies of the SAME wild species within call — found by
/// scanning seeds 0..64 for a species appearing at least twice among
/// `village.is_none()` bodies, since a herd's individual members are not
/// otherwise guaranteed to survive the roll's own within-call filter (most
/// of seed 3's fourteen wild species have exactly one member within call).
///
/// MUTATION THIS MUST FAIL AGAINST: the `resident == false` arm's `n == 1`
/// case rendering the plural form unconditionally, i.e. swapping
/// `labels[0].to_string()` for `format!("1 wild {species}")`. Performed by
/// hand: made that swap, ran this test alone, and it went red —
/// `assertion `left == right` failed: a single wild body must collapse to
/// `a wild {species}` …`, left `"Here: Kmompmon; 1 wild otyugh."`, right
/// `"Here: Kmompmon; otyugh."` (species and resident name are seed-0-specific
/// and read off the live session, not hardcoded) — then reverted the edit.
#[test]
fn a_wild_group_collapses_and_follows_the_residents() {
    let world = common::build(1).expect("seed 1 builds");
    let mut session = flagship_session(&world);
    // Two bodies of one wild species within call — the precondition this
    // test needs to exercise the `n > 1` collapse. If this seed's roll ever
    // stops deriving a same-species pair, that is a finding about the sim,
    // not a flaky fixture (the same discipline `common::world_where` states
    // for its own searches).
    let wild_species = session
        .bodies()
        .iter()
        .find(|a| {
            a.village.is_none()
                && session
                    .bodies()
                    .iter()
                    .filter(|b| b.village.is_none() && b.species == a.species)
                    .count()
                    >= 2
        })
        .map(|b| b.species.clone())
        .unwrap_or_else(|| {
            panic!(
                "precondition: seed 1 must derive at least two wild bodies of                  the same species within call; if it does not, an epoch has                  moved the seed"
            )
        });
    let same_species: Vec<hornvale_kernel::EntityId> = session
        .bodies()
        .iter()
        .filter(|b| b.village.is_none() && b.species == wild_species)
        .map(|b| b.entity)
        .collect();
    assert!(
        same_species.len() >= 2,
        "precondition: at least two `{wild_species}` bodies within call"
    );

    // Walk to an empty room — exactly `an_empty_room_says_nothing_about_company`'s
    // technique — so the scene below is built entirely by the seam, not by
    // whatever the roll happens to sense at the flagship itself.
    let mut empty = false;
    'walk: for _ in 0..3 {
        for dir in ["n", "e", "s", "w"] {
            let before = session.position();
            let _ = out(session.handle(&format!("go {dir}")));
            if session.position() == before {
                continue;
            }
            empty = session
                .snapshot()
                .expect("a live session snapshots")
                .sensed
                .present
                .is_empty();
            if empty {
                break 'walk;
            }
        }
    }
    assert!(
        empty,
        "precondition: a short walk away from the flagship must reach an          empty room to build a controlled scene on"
    );

    let driven = session.driven_body().entity;
    let resident_idx = session
        .bodies()
        .iter()
        .position(|b| b.village.is_some() && b.entity != driven)
        .expect("seed 0 has a resident besides the driven body");
    let resident = session.bodies()[resident_idx].entity;
    let resident_label = session.bodies()[resident_idx].label.clone();
    session.place_creature_at_me(resident);

    // One resident, no wild yet: a single group, no separator.
    let looked = out(session.handle("look"));
    let line = here_line(&looked).unwrap_or_else(|| panic!("no presence line: {looked:?}"));
    assert_eq!(
        line,
        format!("Here: {resident_label}."),
        "one resident alone must be a single un-joined group"
    );

    // One wild body of the target species: the singular collapse, and the
    // FIRST group boundary — residents must precede it.
    session.place_creature_at_me(same_species[0]);
    let looked = out(session.handle("look"));
    let line = here_line(&looked).unwrap_or_else(|| panic!("no presence line: {looked:?}"));
    assert_eq!(
        line,
        format!("Here: {resident_label}; {wild_species}."),
        "a single wild body must collapse to its bare label ({{species}}), joined          after the resident group by exactly one `; `"
    );
    assert_eq!(
        line.matches("; ").count(),
        1,
        "two groups must be joined by exactly one separator: {line:?}"
    );
    assert!(
        line.find(&resident_label) < line.find(&wild_species),
        "the resident group must precede the wild group: {line:?}"
    );

    // A second body of the SAME species: the plural collapse — count first,
    // the species word never pluralised.
    session.place_creature_at_me(same_species[1]);
    let looked = out(session.handle("look"));
    let line = here_line(&looked).unwrap_or_else(|| panic!("no presence line: {looked:?}"));
    assert_eq!(
        line,
        format!("Here: {resident_label}; 2 {wild_species}."),
        "two wild bodies of one species must collapse to `{{n}} {{species}}`          (no \"wild\" — controller ruling, ledger #7), count first, the          species word unpluralised"
    );
    assert_eq!(
        line.matches("; ").count(),
        1,
        "still exactly one separator with two groups: {line:?}"
    );
}

// ---------------------------------------------------------------------------
// Task 10: names reach the verbs. Residents are labelled by their drawn
// names now, and nothing until this task proved each of the four matchers
// that read a label actually resolves a resident BY NAME, indoors and
// outdoors, through the listing, through provenance, and through a
// substring collision. This section is tests only unless one reddens.
// ---------------------------------------------------------------------------

/// `examine <name>` answers the resident's `creature_datum` line outdoors
/// (via the chart legend's word match) and indoors (via `examine_chamber`'s
/// exact matcher), and the two lines are byte-equal —
/// `the_blocking.rs::a_creatures_noun_answers_the_same_line_on_both_sides_of_a_doorway`'s
/// contract, now specifically for a body labelled by a drawn NAME rather
/// than the old `"<species> of <village>"` template.
///
/// MUTATION THIS MUST FAIL AGAINST: lowercase only one side of the exact
/// match in `examine_chamber`'s creature arm (`session.rs`, the
/// `.find(|npc| npc.label.to_lowercase() == wanted)` line) — drop the
/// `.to_lowercase()` off `npc.label`.
#[test]
fn examine_resolves_a_resident_by_name_on_both_bands() {
    let w = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    session.handle("wait");
    let companion = session.bodies()[1].entity;
    let label = session.bodies()[1].label.clone();
    assert!(
        !label.contains(" of "),
        "precondition: a resident's label is a drawn name, not the old \
         '<species> of <village>' template: {label:?}"
    );
    session.place_creature_at_me(companion);

    let outdoors = out(session.handle(&format!("examine {label}")));
    assert!(
        !outdoors.starts_with("You see no"),
        "precondition: the name answers OUT of doors: {outdoors}"
    );

    let reply = out(session.handle("enter"));
    assert!(
        reply.starts_with("[chamber "),
        "the possession did not get indoors, so nothing below is tested: {reply}"
    );
    common::deepen_until_the_plan_draws(&mut session, companion);
    assert!(
        common::marks_of(&session).iter().any(|m| m.noun == label),
        "precondition: '{label}' must be drawn on the plan indoors — without a \
         mark, examine refusing it is correct, not a bug"
    );
    let indoors = out(session.handle(&format!("examine {label}")));
    assert_eq!(
        indoors, outdoors,
        "'{label}' is drawn on the plan inside; examine must answer it by \
         name, and answer it with the SAME line the walk band gives"
    );
}

/// `npcs` lists a resident's drawn name, never the old
/// `"<species> of <village>"` template, and `why <name>` resolves the same
/// resident — a resident's `NAME`/`is-person`/`person-born` facts (`the-
/// roll`'s own commits, `derive_residents`) are themselves recorded
/// provenance, so `why` answers `recount`'s "{label}:\n- …" narration
/// rather than the "nothing recorded" refusal, and it must be THIS body's
/// narration: it names the label on its very first line.
///
/// MUTATION THIS MUST FAIL AGAINST: in `derive_residents`, keep `body_at`'s
/// label instead of the drawn name (delete the `body.label = name;` line) —
/// `list_npcs` and `why` then answer the generic species template.
#[test]
fn the_roster_listing_and_why_use_names() {
    let w = common::build(42).expect("seed 42 builds");
    let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let label = session.bodies()[1].label.clone();
    assert!(
        !label.contains(" of "),
        "precondition: a resident's label is a drawn name: {label:?}"
    );

    let listed = session.npc_labels();
    assert!(
        listed.contains(&label.as_str()),
        "npcs' own label roster must carry the drawn name '{label}': {listed:?}"
    );
    assert!(
        !listed.iter().any(|l| l.contains(" of ")),
        "no listed label may be the old '<species> of <village>' template: {listed:?}"
    );

    let mut session = session;
    let why = out(session.handle(&format!("!why {label}")));
    assert!(
        why.starts_with(&format!("{label}:\n")),
        "why must resolve '{label}' to the SAME body the roster names it \
         for, narrated by its own drawn name: {why:?}"
    );
}

/// Two residents whose names share a prefix ("Tosk", "Toska") do not
/// collide in the substring matchers: typing the SHORTER name ("tosk") must
/// resolve the resident actually named "Tosk", not the resident named
/// "Toska" whose longer label also contains "tosk" as a substring.
///
/// **Route taken**: seed 42's own draw has no such pair (residents 1/2 in
/// Task 4's fixture draw ordinary generated names), so the pair is
/// constructed with the seam `a_committed_name_wins_over_the_draw` shows —
/// a NAME fact committed by hand, before derivation, for two ordinals. This
/// has to happen on the LEDGER, before `Session::start` derives labels
/// (`Body.label` is set once, at derivation, inside `derive_residents`) —
/// `Session::start`'s signature takes only `world: &World` and clones
/// `world.ledger` internally, so the pre-seeded facts are committed onto a
/// clone of the built world's own ledger, and that modified `World` (not
/// the original) is handed to `Session::start`. The entity for a given
/// ordinal is computed the same way `derive_residents` computes it —
/// `reuse_or_mint_entity` derives an id purely from `Lineage`, independent
/// of ledger state, so minting it on a throwaway clone first and committing
/// NAME there, then folding that ledger into the `World` passed to
/// `Session::start`, yields exactly the entity `derive_residents` will
/// later resolve to the same lineage — this is a full integration-level
/// test through `Session::handle`, not a bare call to the matcher helper.
///
/// Placed with Toska at the LOWER ordinal (1, so roster position 0 of
/// `other_bodies`) and Tosk at the higher one (2, roster position 1): a
/// first-match-in-roster-order matcher checks Toska before Tosk, and
/// "toska".contains("tosk") is true, so a query of "tosk" wrongly matches
/// Toska first under that shape — the collision this test is built to
/// catch.
///
/// MUTATION THIS MUST FAIL AGAINST: in `colocated_npc`'s (a.k.a.
/// `resolve_here`) substring fallback, sort matches by label length
/// ascending and prefer the shortest — i.e. always prefer the shorter
/// label over roster order, which behaves identically to correct
/// exact-match-first resolution for the "tosk" query used here (since
/// "Tosk" is both the exact match and the shorter label) and so is NOT
/// this test's discriminator; the mutation this test actually catches is
/// leaving `.find()` at plain roster order with no exact-match preference,
/// which is exactly what a shared `body_by_needle` helper (exact match
/// first, then longest) fixes.
#[test]
fn a_prefix_name_does_not_shadow_a_longer_one() {
    let mut w = common::build(42).expect("seed 42 builds");
    let village = village_info(&w).expect("seed 42 places a flagship");
    assert!(
        village.population >= 3,
        "need at least two other residents besides the driven body: population {}",
        village.population
    );

    let mut ledger = w.ledger.clone();
    let toska = ledger.reuse_or_mint_entity(hornvale_kernel::Lineage {
        parent: Some(village.id),
        role: "npc",
        ordinal: 1,
    });
    ledger
        .commit(
            hornvale_kernel::Fact {
                subject: toska,
                predicate: hornvale_kernel::NAME.to_string(),
                object: hornvale_kernel::Value::Text("Toska".to_string()),
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &w.registry,
        )
        .expect("a hand-committed NAME on a freshly minted entity commits");
    let tosk = ledger.reuse_or_mint_entity(hornvale_kernel::Lineage {
        parent: Some(village.id),
        role: "npc",
        ordinal: 2,
    });
    ledger
        .commit(
            hornvale_kernel::Fact {
                subject: tosk,
                predicate: hornvale_kernel::NAME.to_string(),
                object: hornvale_kernel::Value::Text("Tosk".to_string()),
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &w.registry,
        )
        .expect("a hand-committed NAME on a freshly minted entity commits");
    w.ledger = ledger;

    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    assert_eq!(
        session.bodies()[1].label,
        "Toska",
        "precondition: ordinal 1 carries the committed name 'Toska'"
    );
    assert_eq!(
        session.bodies()[2].label,
        "Tosk",
        "precondition: ordinal 2 carries the committed name 'Tosk'"
    );

    session.place_creature_at_me(toska);
    session.place_creature_at_me(tosk);

    let why = out(session.handle("!why tosk"));
    assert!(
        why.starts_with("Tosk:\n"),
        "'why tosk' must resolve the resident actually named 'Tosk', not \
         'Toska' merely because 'toska' also contains 'tosk': {why:?}"
    );
}

/// The Ken: with three dragons present, `examine dragon` answered
/// "black-dragon — a black-dragon of this world, alive and moving." — the
/// longest-label tiebreak silently picking one of three. Confidently wrong
/// with no signal to the player is the worst available behaviour; refusing
/// and naming the candidates is both honester and more useful.
#[test]
fn an_ambiguous_needle_is_refused_and_names_its_candidates() {
    let (mut session, _) = open_staged_dragons_session();
    let answer = say(&mut session, "examine dragon");
    assert!(
        !answer.contains("alive and moving"),
        "an ambiguous needle resolved to one creature: {answer:?}"
    );
    assert!(
        answer.contains("black-dragon") && answer.contains("red-dragon"),
        "a refusal must name what would have worked: {answer:?}"
    );
}
