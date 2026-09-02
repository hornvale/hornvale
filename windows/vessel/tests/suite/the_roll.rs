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
        "today a fresh possession stands alone (The Hand, 0/64)"
    );
}
