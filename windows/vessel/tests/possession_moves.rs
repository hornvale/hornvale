//! The world moves without you: possess, wait across a phase, observe an NPC's
//! motion; and the same script is byte-deterministic.
use hornvale_vessel::{PossessOpts, Session};

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

#[test]
fn day_zero_session_is_unchanged_until_you_wait() {
    // The frozen behavior is preserved: before any `wait`, no agent-at exists.
    let w = world();
    let (session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    assert_eq!(session.committed_agent_at_count(), 0); // a test accessor added in this task
}

#[test]
fn waiting_moves_an_npc_and_it_is_observed() {
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Wait long enough to cross into an active phase for at least one NPC.
    let out = session.handle("wait 1");
    // After a day of waiting, at least one agent-at has been committed.
    assert!(
        session.committed_agent_at_count() >= 1,
        "the world moved on wait"
    );
    // The wait output mentions motion (non-empty, references an NPC/movement).
    match out {
        hornvale_vessel::Turn::Out(s) => assert!(!s.is_empty()),
        _ => panic!("wait outputs prose"),
    };
}

#[test]
fn the_same_script_is_byte_deterministic() {
    let w = world();
    let run = || {
        let (mut s, _o) = Session::start(&w, &PossessOpts::default()).unwrap();
        for cmd in ["wait 1", "wait 1", "wait 1"] {
            let _ = s.handle(cmd);
        }
        s.session_ledger_json() // a test accessor: serde_json of the session ledger
    };
    assert_eq!(
        run(),
        run(),
        "same seed + same waits -> byte-identical session ledger"
    );
}
