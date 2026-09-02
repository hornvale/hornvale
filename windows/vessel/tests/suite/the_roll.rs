//! The Roll (spec §8): the company probe (M1), the roll's own properties
//! (M3), dormancy (§3.7), and presence at scale (§4).

use hornvale_settlement::village_info;
use hornvale_vessel::{PossessOpts, Session};

use crate::common;

/// One seed's reading for M1: does a fresh flagship possession have anyone in
/// `sensed.present` on its first look, and could it — is the home
/// settlement's `population >= 2`?
fn company_at(seed: u64) -> Option<(bool, bool)> {
    let world = common::build(seed)?;
    let village = village_info(&world)?;
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
/// Ignored for cost: 64 world builds is ~4 min (`ooc_objective.rs:572`
/// measured a full miss at 233.72 s). Run by hand at Task 1 and Task 14.
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

/// M3: the roll is a function — two calls agree — and it is bounded: no
/// settlement contributes more than its population, and never more than the
/// asked-for budget in total.
///
/// The herd half of "bounded" is held one layer down, by Task 6's
/// `a_herd_is_its_headcount`: a herd yields exactly `headcount` bodies, so a
/// roll cannot contain more of one than the world says stand there. Asserting
/// it again here would need the herd key a `Body` deliberately does not
/// carry, and would re-measure `derive_wild_herds` rather than `roll_of`.
///
/// MUTATION THIS MUST FAIL AGAINST: `budget + 1` in the truncation.
#[test]
fn the_roll_is_pure_and_bounded() {
    let world = common::build(42).expect("seed 42 builds");
    let session = flagship_session(&world);
    let observer = session.position();

    let mut memo_a = RoomMeshMemo::new();
    let mut memo_b = RoomMeshMemo::new();
    let a = roll_of(
        session.bodies(),
        session.roll_keys(),
        &observer,
        ROLL_HOPS,
        ROLL_BUDGET,
        &mut memo_a,
    );
    let b = roll_of(
        session.bodies(),
        session.roll_keys(),
        &observer,
        ROLL_HOPS,
        ROLL_BUDGET,
        &mut memo_b,
    );
    assert_eq!(a, b, "the roll is a function of (homes, observer room)");
    assert_eq!(
        a.len(),
        session.bodies().len(),
        "the mask covers the roster"
    );

    // No settlement contributes more than its committed population.
    let mut per_settlement: std::collections::BTreeMap<hornvale_kernel::EntityId, usize> =
        std::collections::BTreeMap::new();
    for (body, on) in session.bodies().iter().zip(&a) {
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
    let mut memo_c = RoomMeshMemo::new();
    let tight = roll_of(
        session.bodies(),
        session.roll_keys(),
        &observer,
        ROLL_HOPS,
        3,
        &mut memo_c,
    );
    assert_eq!(
        tight.iter().filter(|on| **on).count(),
        3,
        "a budget of three admits exactly three bodies"
    );
    assert!(
        a.iter().filter(|on| **on).count() <= ROLL_BUDGET,
        "the roll never exceeds ROLL_BUDGET"
    );
}

/// The order is (distance, residents first, parent, ordinal): with a budget
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
