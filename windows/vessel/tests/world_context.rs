//! A `WorldContext` is reusable across possessions, and a session built
//! through it is indistinguishable from one built the old way.

use hornvale_vessel::{PossessOpts, Session, WorldContext};

/// Build seed 42 exactly as `cmd_possess` does. Verified against
/// `cli/src/main.rs:490` and `windows/worldgen/src/lib.rs:7323`; all three
/// pin structs derive `Default`.
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

/// Two sessions started from ONE context must produce byte-identical
/// openings and snapshots to two sessions each started from scratch.
/// This is the whole safety claim of the hoist.
#[test]
fn a_reused_context_produces_identical_sessions() {
    let world = world();
    let opts = PossessOpts::default();

    let (mut a, open_a) = Session::start(&world, &opts).unwrap();
    let snap_a = hornvale_vessel::snapshot_json(&a.snapshot().unwrap());

    let ctx = WorldContext::build(&world).unwrap();
    let (mut b, open_b) = Session::start_in(&ctx, &opts).unwrap();
    let snap_b = hornvale_vessel::snapshot_json(&b.snapshot().unwrap());
    let (c, open_c) = Session::start_in(&ctx, &opts).unwrap();
    let snap_c = hornvale_vessel::snapshot_json(&c.snapshot().unwrap());

    assert_eq!(open_a, open_b, "start_in must match start");
    assert_eq!(open_b, open_c, "a reused context must not drift");
    assert_eq!(snap_a, snap_b, "start_in must match start");
    assert_eq!(snap_b, snap_c, "a reused context must not drift");

    // The agreement has to hold for the whole session, not only at `start`.
    // Every turn reads the world-scoped derivations back out of the held
    // context, so an owned hold and a borrowed hold must answer a VERB
    // identically too — `a` owns its context, `b` shares one. This is the
    // half that can actually break: any `&self` read that came to depend on
    // which `HeldContext` variant it is looking at would pass every
    // assertion above and fail here.
    let _ = a.handle("look");
    let _ = b.handle("look");
    let after_a = hornvale_vessel::snapshot_json(&a.snapshot().unwrap());
    let after_b = hornvale_vessel::snapshot_json(&b.snapshot().unwrap());
    assert_eq!(
        after_a, after_b,
        "a borrowed context must drive a turn exactly as an owned one does"
    );

    // What is NOT asserted here, deliberately: that two sessions sharing one
    // context do not alias each other's turn state. That is guaranteed by the
    // TYPE, not by this test — `WorldContext` holds no interior mutability
    // (no `Cell`/`RefCell`/atomic anywhere in it) and `start_in` takes it by
    // `&`, so no session can write through it at all; every mutable thing a
    // turn touches (the ledger clone, the registry clone, the NPC roster, the
    // turn counter, the accumulated knowledge) is owned by `Session`. An
    // earlier draft asserted `after_b != after_c` for this, which passed only
    // because `b` had taken a turn and `c` had not — it would have gone on
    // passing whatever aliasing existed. Better a stated invariant than a
    // check that cannot fail; if interior mutability is ever added to
    // `WorldContext`, this comment is the thing that has to change with it.
}
