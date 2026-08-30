//! **The strongbox, reached by walking to it** — decision 0398's own witness.
//!
//! The Chattel's Task 11 shipped a container, a lock and a key, and then
//! measured that no session in any world could stand in front of one:
//! `the-strongbox` carried `needs_populous: true`, and across three worlds
//! plus a 48-seed sweep of the flagship a possession actually starts at, not
//! one living occupation cleared `HAMLET_POPULATION_CEILING` (max alive peak
//! 84–87 against 150). The gate was a good claim about social scale and a
//! false one about reachability; 0398 relaxed it.
//!
//! **A relaxation asserted is worth nothing; this file is the measurement.**
//! Everything below drives `Session::handle` — the same commands a player
//! types — rather than calling `selection_for` and reading a `Vec<&str>`.
//! `pattern.rs`'s unit tests already do the latter, and they were GREEN on the
//! day the capability was unreachable, which is the exact failure this file
//! exists to make impossible to repeat.
//!
//! **The 48-seed sweep itself is NOT kept as a test, and that is a cost
//! decision rather than an oversight.** It builds 48 worlds (~195 s measured
//! on the Mac, 2026-08-29) and the only tier that would run it is the heavy
//! one, which decision 0426 has just put back on every merge after The
//! Governor cut it 3.52x — a 195 s addition there is a 44% regression in a
//! number a campaign was spent on, bought for a RATE that a decision record
//! already carries with its date. So the rate lives in decision 0398 and the
//! WITNESS lives here, at one world: what a permanent test has to hold is
//! that the count is not zero, and one seed holds that for ~4 s. The sweep is
//! reproducible from the recipe in that decision if the rate is ever wanted
//! again.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// The refusal `open` gives a locked thing when nothing in the body's hands
/// would open it. Duplicated from `session.rs`'s private `const` on purpose —
/// this file is an integration test and reads the crate exactly as a player
/// does, through the reply text. A rewording that broke this line would be a
/// change to what the player is told, which is a thing worth reddening.
const LOCKED: &str = "It is locked, and you are carrying nothing that would open it.";

fn world_at(seed: u64) -> World {
    build_world(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("the seed builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// Walk a fresh possession in through the front door and then as far in as the
/// place goes, returning each chamber's nouns in depth order.
///
/// Panics rather than returning empty if the possession never gets indoors: a
/// reachability test that silently tested nothing would be the same defect one
/// layer up from the one it is here to witness.
fn nouns_by_depth(session: &mut Session) -> Vec<Vec<String>> {
    let reply = out(session.handle("enter"));
    assert!(
        reply.starts_with("[chamber "),
        "the possession never got indoors, so nothing below is tested: {reply}"
    );
    let mut per_chamber = vec![session.chamber_nouns_here()];
    // `MAX_CHAMBERS` is 4, so four steps is one more than any structure has;
    // the loop stops on the far-end reply rather than on the count.
    for _ in 0..4 {
        if !out(session.handle("enter further in")).starts_with("[chamber ") {
            break;
        }
        per_chamber.push(session.chamber_nouns_here());
    }
    per_chamber
}

/// **A player walks into a room and a strongbox is standing in it, with a key
/// inside** — the headline of decision 0398, driven end to end.
///
/// Seed 1 rather than seed 42, and the reason is the finding itself: seed 42's
/// flagship structure does not draw enough chambers for its possession to
/// reach a `Store` role, so the gallery's own world is not a witness. 8 of the
/// 48 swept seeds are; 1 is the lowest of them.
///
/// The `open` reply is asserted because it is the second thing 0398 made real.
/// [`LOCKED`] was measured unreachable through `Session::handle` on the day it
/// was written — the strongbox that carries `Lockable` was in no room a
/// session could enter — so the lock could only be tested by driving its two
/// halves separately (`session.rs`'s
/// `a_lockable_thing_opens_only_with_the_key_in_custody`). It is a played
/// refusal now.
///
/// MUTATION THIS MUST FAIL AGAINST — the property is *that the strongbox is
/// drawn at all*: put `needs_populous: true` back on `the-strongbox` in
/// `INVENTORY`, which is decision 0398 itself undone. Confirmed 2026-08-29,
/// unfiltered over the whole crate (`850 tests run: 847 passed, 3 failed`):
///
/// ```text
/// FAIL interior::pattern::tests::a_hamlet_composes_a_strongbox_with_a_key_inside_it
/// FAIL interior::pattern::tests::the_populous_gate_still_works_though_no_authored_pattern_uses_it
/// FAIL suite::strongbox_reachability::a_possession_walks_to_a_strongbox_and_finds_it_locked
///
/// no chamber of seed 1's structure composes a strongbox, so the capability
/// is unreachable again: [["a doorway", "a screen"], ["a doorway", "an
/// alcove"], ["a doorway", "a water jar", "a loom"], ["a doorway", "a water
/// jar"]]
/// ```
///
/// **Three failures, and the third one is the shape of the whole argument.**
/// The first is the unit assertion over `selection_for`; the second is the
/// synthetic-inventory test's own PREMISE guard firing ("an INVENTORY pattern
/// is population-gated again"), which is what it is for. This one is the only
/// failure of the three that a player could have observed, and it is the only
/// one that was green through the entire period the capability was
/// unreachable — because it did not exist. That is the reason it exists now.
///
/// The reply pasted above is also the fourth chamber's own reading, and it is
/// worth a second look: `["a doorway", "a water jar"]` is a `Store` chamber
/// with the water jar in it and nothing to lock. The gate never removed the
/// room; it removed the only thing the room was for.
#[test]
fn a_possession_walks_to_a_strongbox_and_finds_it_locked() {
    let world = world_at(1);
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 1 possesses");
    let per_chamber = nouns_by_depth(&mut session);
    assert!(
        per_chamber.len() > 1,
        "seed 1's structure has one chamber, so nothing here walks anywhere: \
         {per_chamber:?}"
    );

    let holds = |n: &str| per_chamber.iter().any(|c| c.iter().any(|x| x == n));
    assert!(
        holds("a strongbox"),
        "no chamber of seed 1's structure composes a strongbox, so the \
         capability is unreachable again: {per_chamber:?}"
    );
    assert!(
        holds("a key"),
        "a strongbox composed with nothing inside it, so `open` has nothing \
         to report: {per_chamber:?}"
    );
    // The two travel together, in one chamber — a key in the third room and a
    // strongbox in the fourth would satisfy both assertions above and be a
    // different world than the one the grammar composes.
    assert!(
        per_chamber
            .iter()
            .any(|c| c.iter().any(|x| x == "a strongbox") && c.iter().any(|x| x == "a key")),
        "the strongbox and the key are in different rooms: {per_chamber:?}"
    );

    // The session is standing in the deepest chamber, which is the one that
    // holds them (asserted above by construction of the walk, and again here
    // by the reply the verb gives).
    assert_eq!(
        out(session.handle("open a strongbox")),
        LOCKED,
        "the lock's refusal is not what a player standing here is told"
    );
}
