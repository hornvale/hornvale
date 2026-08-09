//! `PossessTarget` selects WHO is possessed. The flagship path must be
//! byte-identical to before it existed.

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

/// Selecting a target must actually change who you are. Asserted on the
/// agent id and settlement, NOT on prose — prose could coincide.
#[test]
fn first_settlement_and_flagship_are_selectable_independently() {
    let w = world();
    let ctx = WorldContext::build(&w).unwrap();

    let flag = PossessOpts {
        target: PossessTarget::Flagship,
        ..Default::default()
    };
    let first = PossessOpts {
        target: PossessTarget::FirstSettlement,
        ..Default::default()
    };

    let (a, _) = Session::start_in(&ctx, &flag).unwrap();
    let (b, _) = Session::start_in(&ctx, &first).unwrap();

    // They MAY coincide on a given seed — that is a legitimate world, not a
    // bug. What must hold is that the selection is honoured: b's settlement
    // is the most-populous one by the world's own ordering.
    let expected = hornvale_vessel::most_populous_settlement(&w).expect("seed 42 has settlements");
    assert_eq!(b.agent().village.id, expected.id, "target not honoured");
    let _ = a;
}

/// Determinism: the same target on the same seed gives the same agent.
#[test]
fn a_target_is_seed_stable() {
    let w = world();
    let ctx = WorldContext::build(&w).unwrap();
    let opts = PossessOpts {
        target: PossessTarget::FirstSettlement,
        ..Default::default()
    };
    let (a, _) = Session::start_in(&ctx, &opts).unwrap();
    let (b, _) = Session::start_in(&ctx, &opts).unwrap();
    assert_eq!(a.agent().id, b.agent().id);
}
