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
/// built seed's home settlement could hold company; seed 42 is one of the 61
/// that starts alone (see `seed_42_starts_alone_today` below).
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

/// Today's state, pinned so that Task 8 has a red to turn green: seed 42's
/// flagship possession starts with nobody else present. When the roll lands
/// this assertion inverts (see `seed_42_starts_in_company`, Task 8).
///
/// MUTATION THIS MUST FAIL AGAINST: call
/// `session.place_creature_at_me(session.bodies()[1].entity)` before the
/// snapshot inside `company_at`; `present` turns non-empty and this reddens.
#[test]
fn seed_42_starts_alone_today() {
    let (has, can) = company_at(42).expect("seed 42 builds and starts");
    assert!(can, "seed 42's flagship holds at least two people");
    assert!(
        !has,
        "today a fresh possession at seed 42 stands alone; 3 of 64 seeds have \
         company, The Roll Task 1"
    );
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
