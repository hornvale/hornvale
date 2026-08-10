//! `PossessTarget` selects WHICH SETTLEMENT the commanded agent is minted at.
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

/// Selecting a target must actually change which settlement you are minted at.
/// Asserted on the settlement, NOT on prose — prose could coincide.
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
        a.agent().village.id,
        flagship.id,
        "flagship target not honoured"
    );
    assert_eq!(
        b.agent().village.id,
        popular_expected.id,
        "most-populous target not honoured"
    );
}

/// Determinism: the same target on the same seed gives the same agent.
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
    assert_eq!(a.agent().id, b.agent().id);
}

/// BOTH targets mint: neither adopts an agent the world already derived.
/// Recorded as a test because decision 0116 originally claimed the opposite,
/// and the doctrine gap it leaves open is only meaningful if this stays true
/// until something closes it. The minted id is derived from the agent's own
/// room, so it must equal a fresh `mint_at` at the selected settlement — a
/// path `derive_npcs` never produces an id through.
#[test]
fn both_targets_mint_a_fresh_agent() {
    let w = world();
    let ctx = WorldContext::build(&w).unwrap();
    let lctx = hornvale_locale::LocaleContext::build(&w).unwrap();

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
        let minted = hornvale_vessel::mint_at(&w, &lctx, village).unwrap();
        assert_eq!(
            s.agent().id,
            minted.id,
            "{target:?} must be a fresh mint at its settlement"
        );
    }
}
