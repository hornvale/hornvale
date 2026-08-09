//! The Lintel's headline: `enter` at a built locale reaches a chamber and `out`
//! returns. Observed through a real `Session`, not demonstrated in a unit test —
//! the campaign's whole point is that descent EXISTS.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// Seed 42 — the canonical world, and it has a settlement (its village is
/// `Vngoashshngaoshshngoogootao`), so possession succeeds. Setup copied from
/// `windows/vessel/tests/the_purview.rs`.
fn world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// The refusal reserved for the COARSE direction. Byte-pinned in the galleries.
const COARSE_REFUSAL: &str = "The grain of the world resists";

/// Walk until `enter` succeeds, up to `limit` steps, returning the reply that
/// worked. Scouts rather than assuming the start is built: which locales carry
/// settlement territory is a geography accident, and The Snapshot's plan
/// established that a driver must scout rather than hardcode.
fn enter_somewhere_built(session: &mut Session<'_>, limit: usize) -> Option<String> {
    for step in 0..limit {
        let reply = out(session.handle("enter"));
        if !reply.starts_with("Nothing here is built") {
            return Some(reply);
        }
        // Not built here — step along and try again.
        let moved = out(session.handle(if step % 2 == 0 { "go n" } else { "go ne" }));
        if moved.starts_with("No way") {
            let _ = session.handle("back");
        }
    }
    None
}

#[test]
fn enter_reaches_a_chamber_and_out_returns() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    let entered = enter_somewhere_built(&mut session, 12)
        .expect("seed 42 has settlement territory within a dozen steps of its flagship");

    assert!(
        !entered.contains(COARSE_REFUSAL),
        "fine-ward enter must not hit the coarse refusal: {entered:?}"
    );
    for terrain_word in ["biome", "elevation", "moisture", "regime"] {
        assert!(
            !entered.contains(terrain_word),
            "a chamber must not be described as terrain ({terrain_word}): {entered:?}"
        );
    }

    let before_out = out(session.handle("look"));
    let left = out(session.handle("out"));
    assert!(!left.is_empty(), "leaving says something");
    let after_out = out(session.handle("look"));
    assert_ne!(
        before_out, after_out,
        "look inside and look outside must differ, or `out` did nothing"
    );
}

#[test]
fn exit_is_still_refused_coarse_ward() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    assert!(
        out(session.handle("exit")).contains(COARSE_REFUSAL),
        "the coarse-ward refusal is byte-pinned in the galleries"
    );
}

#[test]
fn entering_where_nothing_is_built_gives_a_physical_reason() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    // Walk away from the settlement until a locale reports nothing built.
    //
    // The walk is NORTHWARD-BIASED across three compass points rather than a
    // bare `go n`, and that is load-bearing, not tidying. This mesh is
    // triangular: every cell offers exactly one of two exit triads, `{N, SW,
    // SE}` or `{NE, NW, S}`, so a single fixed direction is absent from half
    // the cells outright. The Tense flipped the parity of the starting cell
    // (the same flip `the_purview.rs` records at both its rungs), which left
    // the old `go n` loop answering "No way n from here." twelve times and the
    // walker standing exactly where it began — a search that had quietly
    // stopped searching. `the_water_column_is_a_place_you_can_be` already
    // carries this warning in its own comment; this test had the bug and no
    // guard. Biasing over `n`/`ne`/`nw` means at least one point is always
    // available whichever triad the cell offers.
    let mut refusal = None;
    for _ in 0..12 {
        let reply = out(session.handle("enter"));
        if reply.starts_with("Nothing here is built") {
            refusal = Some(reply);
            break;
        }
        // That `enter` SUCCEEDED, so the possession is now inside a structure —
        // and lateral movement is refused indoors (§1b.6). Step back out of
        // doors before walking, or the loop would stand still for a dozen turns.
        let _ = session.handle("out");
        for dir in ["n", "ne", "nw"] {
            if !out(session.handle(&format!("go {dir}"))).starts_with("No way ") {
                break;
            }
        }
    }
    let refusal = refusal.expect("wilderness lies within a dozen steps of a village");
    assert!(!refusal.contains(COARSE_REFUSAL), "{refusal:?}");
    assert!(
        !refusal.to_lowercase().contains("error"),
        "a physical reason, not an error: {refusal:?}"
    );
}

#[test]
fn entering_and_leaving_commits_nothing() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let before = session.committed_agent_at_count();
    // `expect`, not `let _`: the guard is only meaningful if a descent actually
    // happened. Discarding the result would pass just as well on a session that
    // never got inside anything.
    enter_somewhere_built(&mut session, 12).expect("a descent must actually happen to be guarded");
    let _ = session.handle("out");
    assert_eq!(
        session.committed_agent_at_count(),
        before,
        "a band change is session state; nothing commits"
    );
}
