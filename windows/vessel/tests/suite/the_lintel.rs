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
        if !reply.starts_with("There is nothing here to enter") {
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
    // bare `go n` — HISTORICAL reasoning, kept for the record rather than
    // silently deleted (final review F4: this comment used to describe live
    // behaviour and stopped being true when The Rhumb shipped). At the time
    // this was written, the mesh's exit-triad model meant every room offered
    // exactly one of two labelled triads, `{N, SW, SE}` or `{NE, NW, S}`, so a
    // single fixed direction was absent from half the rooms outright: The
    // Tense flipped the parity of the starting room (the same flip
    // `the_purview.rs` records at both its rungs), which left the old `go n`
    // loop answering "No way n from here." twelve times and the walker
    // standing exactly where it began — a search that had quietly stopped
    // searching. Biasing over `n`/`ne`/`nw` was the fix, on the theory that at
    // least one point was always available whichever triad the room offered.
    //
    // Since decision 0141, `go` resolves all eight compass points from every
    // walk-band room rather than exact-matching a labelled triad — there is no
    // longer a triad a direction can be "absent" from, so the loop below's
    // `starts_with("No way ")` branch (and the identical one in
    // `enter_somewhere_built` above) is dead: `go` never emits that sentence
    // outdoors. Left as three directions rather than trimmed back to a bare
    // `go n`, because the loop still needs to reach wilderness and the bias is
    // harmless, not because it is still load-bearing.
    //
    // **The MECHANISM in that sentence has changed and the conclusion has not**
    // (The Pavement). It read "via a carried rhumb course"; there is no course
    // any more — a compass word names one of the room's own eight neighbours
    // directly (spec section 3.4). `go` gained exactly one refusal in the
    // exchange, `CORNER_BEARING_REFUSAL` at the 24 cube-corner rooms, and it
    // is not "No way ..." either, so the dead branch stays dead.
    //
    // **CORNERS: 8. ROOMS TOUCHING THEM: 24.** Three quads meet at each of the
    // cube's eight corners, so eight corners are touched by twenty-four rooms.
    // Collapsing those two numbers into one is what produced seven wrong sites
    // across three generations of this campaign's own documents — this line said
    // "the eight cube-corner rooms" until fix round 2 — so the distinction is
    // written out here, where the number lives, rather than left to be re-derived.
    // `all_twenty_four_cube_corner_rooms_refuse_exactly_one_bearing_each`
    // (`session.rs`) asserts the 24.
    let mut refusal = None;
    for _ in 0..12 {
        let reply = out(session.handle("enter"));
        if reply.starts_with("There is nothing here to enter") {
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

/// The session's total fact count taken IMMEDIATELY BEFORE the `enter` that
/// succeeds — scouting exactly as [`enter_somewhere_built`] does, but
/// reporting the baseline from inside the loop.
///
/// **A separate scout, for a reason a shorter test would have hidden (The
/// Deed, Task 7).** Since a walk-band step now commits an `agent-at`, a
/// baseline taken before the scout would fold the scout's own commits into
/// this guard. The obvious repair — walk first, leave, then re-enter and
/// re-leave with the baseline in between — is WORSE THAN VACUOUS: `enter` and
/// `out` charge no time, so the second pair's envelope would be identical to
/// the first's, and `Ledger::commit` dedups an exact full-envelope match. A
/// mutant that committed on `out` was measured to leave this test GREEN under
/// that shape. Taking the baseline inside the loop is what makes any commit
/// here novel, and therefore visible.
fn fact_count_before_a_successful_enter(session: &mut Session<'_>, limit: usize) -> Option<usize> {
    for step in 0..limit {
        let before = session.committed_fact_count();
        let reply = out(session.handle("enter"));
        if !reply.starts_with("There is nothing here to enter") {
            return Some(before);
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
fn entering_and_leaving_commits_nothing() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    // `expect`, not `let _`: the guard is only meaningful if a descent actually
    // happened. Discarding the result would pass just as well on a session that
    // never got inside anything.
    let before = fact_count_before_a_successful_enter(&mut session, 12)
        .expect("a descent must actually happen to be guarded");
    let _ = session.handle("out");

    // `committed_fact_count`, not `committed_agent_at_count` (The Deed, Task
    // 7). Its own doc argues the total is the right instrument here: a
    // per-predicate accessor can only falsify a commit it was told to expect,
    // and THIS campaign is the "later campaign introducing a new predicate"
    // that doc names — it adds commit paths to the session, so the weaker
    // guard is exactly the one it could walk past.
    assert_eq!(
        session.committed_fact_count(),
        before,
        "a band change is session state; nothing commits"
    );
}
