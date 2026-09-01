//! H1: the rename changes nothing enterable.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session, Turn};

/// The seed-42 flagship is enterable before and after the gate swap. This is
/// the whole of H1 at this task: a real world, the real verb, the real answer.
///
/// Asserts a POSITIVE signal — the chamber's own `Ways on:` signature — rather
/// than the absence of the old refusal string: the old wording made the
/// negative assertion trivially true, so it could never have caught a broken
/// gate. This one goes red if `enter` stops descending at all.
#[test]
fn the_flagship_is_still_enterable_after_the_gate_swap() {
    let world = hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let reply = match s.handle("enter") {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("enter must not release the session: {t}"),
    };
    assert!(
        reply.contains("Ways on"),
        "the flagship must stay enterable across the gate swap: {reply}"
    );
}
