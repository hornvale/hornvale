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

    let (a, open_a) = Session::start(&world, &opts).unwrap();
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

    // And the sessions must be independently drivable — a shared context
    // must not alias session state.
    let _ = b.handle("look");
    let after_b = hornvale_vessel::snapshot_json(&b.snapshot().unwrap());
    let after_c = hornvale_vessel::snapshot_json(&c.snapshot().unwrap());
    assert_ne!(after_b, after_c, "sessions must not share turn state");
}
