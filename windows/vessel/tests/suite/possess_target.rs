//! `PossessTarget` selects WHICH SETTLEMENT the driven body comes from.
//! The flagship path must be byte-identical to before it existed.

use hornvale_vessel::{PossessOpts, PossessTarget, Session, WorldContext};

/// Identical to the helper in `tests/world_context.rs`.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds")
}

/// The default is the flagship, and it is unchanged.
#[test]
fn the_default_target_is_the_flagship() {
    assert_eq!(PossessOpts::default().target, PossessTarget::Flagship);
}

/// Selecting a target must actually change which settlement you are driven
/// from. Asserted on the settlement, NOT on prose — prose could coincide.
///
/// BOTH arms are asserted. An earlier version started the flagship session and
/// then discarded it with `let _ = a;`, so the test passed identically with the
/// flagship arm deleted and its name overstated what it proved.
#[test]
fn each_target_is_honoured_independently() {
    let w = world();
    let ctx = WorldContext::build(&w).unwrap();

    let flag = PossessOpts {
        target: PossessTarget::Flagship,
        ..Default::default()
    };
    let popular = PossessOpts {
        target: PossessTarget::MostPopulousSettlement,
        ..Default::default()
    };

    let (a, _) = Session::start_in(&ctx, &flag).unwrap();
    let (b, _) = Session::start_in(&ctx, &popular).unwrap();

    // They MAY coincide on a given seed — that is a legitimate world, not a
    // bug. What must hold is that each selection is honoured, on its own
    // independently-derived expectation.
    let flagship = hornvale_settlement::village_info(&w).expect("seed 42 has settlements");
    let popular_expected =
        hornvale_vessel::most_populous_settlement(&w).expect("seed 42 has settlements");
    assert_eq!(
        a.driven_body()
            .village
            .as_ref()
            .expect("a settlement-derived driven body carries Some(village)")
            .id,
        flagship.id,
        "flagship target not honoured"
    );
    assert_eq!(
        b.driven_body()
            .village
            .as_ref()
            .expect("a settlement-derived driven body carries Some(village)")
            .id,
        popular_expected.id,
        "most-populous target not honoured"
    );
}

/// Determinism: the same target on the same seed gives the same body.
#[test]
fn a_target_is_seed_stable() {
    let w = world();
    let ctx = WorldContext::build(&w).unwrap();
    let opts = PossessOpts {
        target: PossessTarget::MostPopulousSettlement,
        ..Default::default()
    };
    let (a, _) = Session::start_in(&ctx, &opts).unwrap();
    let (b, _) = Session::start_in(&ctx, &opts).unwrap();
    assert_eq!(a.agent_entity(), b.agent_entity());
}

/// **Neither target mints any more (The Hand, Task 3) — both SELECT the
/// roster's own home-settlement entry.** This test's pre-Hand name was
/// `both_targets_mint_a_fresh_agent`, asserting the opposite: that each
/// target's identity was a fresh stream draw (`mint_at`'s `AgentId`)
/// distinct from anything `derive_npcs` already produced. Decision 0168
/// (and `Agent`/`AgentId`/`mint_at` no longer existing at all) makes that
/// premise impossible to even state now, so this asserts the replacement
/// invariant: the driven body's identity IS its own roster entry, and that
/// entry belongs to the selected settlement.
#[test]
fn both_targets_select_the_rosters_own_settlement_entry() {
    let w = world();
    let ctx = WorldContext::build(&w).unwrap();

    for (target, village) in [
        (
            PossessTarget::Flagship,
            hornvale_settlement::village_info(&w).unwrap(),
        ),
        (
            PossessTarget::MostPopulousSettlement,
            hornvale_vessel::most_populous_settlement(&w).unwrap(),
        ),
    ] {
        let opts = PossessOpts {
            target,
            ..Default::default()
        };
        let (s, _) = Session::start_in(&ctx, &opts).unwrap();
        assert_eq!(
            s.agent_entity(),
            s.driven_body().entity,
            "{target:?}: identity is the driven body's own entity, not a separate id"
        );
        assert_eq!(
            s.driven_body().village.as_ref().map(|v| v.id),
            Some(village.id),
            "{target:?}: the driven body must belong to the selected settlement"
        );
    }
}
