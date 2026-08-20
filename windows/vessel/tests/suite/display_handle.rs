//! The Signet, Task 4: the `npcs` listing prints a short, typeable handle —
//! the NPC's 1-based position in the session's own derived roster — instead
//! of the wide, lineage-derived `EntityId` the mint now produces. The number
//! a player types back is display input, never an identity; see
//! `SIG-agentid-entityid-confusion` for the sibling defect this task exists
//! to avoid repeating.
use hornvale_vessel::{PossessOpts, Session, Turn};

/// Identical to the helper duplicated across this crate's other integration
/// tests (`possession_moves.rs`, `the_first_mark.rs`, `possess_target.rs`):
/// each integration test file is its own crate, so the seed-42 fixture world
/// is rebuilt verbatim rather than shared.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// Identical to the helper in `the_first_mark.rs`.
fn out_text(t: Turn) -> String {
    match t {
        Turn::Out(s) => s,
        Turn::Released(s) => panic!("npcs/why never releases: {s}"),
    }
}

#[test]
fn the_npc_listing_prints_a_short_handle_not_a_raw_entity_id() {
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let listing = out_text(session.handle("!npcs"));
    assert!(
        listing.contains("[1]"),
        "the listing must offer a short, typeable handle: {listing}"
    );
    for line in listing.lines().filter(|l| l.contains('[')) {
        let n: u64 = line
            .split(['[', ']'])
            .nth(1)
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(|| panic!("a handle line must carry a bracketed number: {line}"));
        assert!(
            n < 1000,
            "a printed handle must be typeable, not a derived entity id: {line}"
        );
    }
}

#[test]
fn a_short_handle_addresses_the_same_npc_the_label_does() {
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let listing = out_text(session.handle("!npcs"));
    let first_line = listing
        .lines()
        .nth(1)
        .expect("npcs lists at least one [handle] label line");
    let first_label = first_line
        .split_once("] ")
        .map(|(_, label)| label)
        .expect("a handle line carries a label after the bracketed handle");

    let by_handle = out_text(session.handle("!why 1"));
    let by_label = out_text(session.handle(&format!("!why {first_label}")));
    assert_eq!(
        by_handle, by_label,
        "handle and label must address the same NPC"
    );
}
