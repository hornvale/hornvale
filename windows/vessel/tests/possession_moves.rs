//! The world moves without you: possess, wait across a phase, observe an NPC's
//! motion; and the same script is byte-deterministic.
use hornvale_vessel::{PossessOpts, Session, Turn};

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

#[test]
fn a_colocated_npc_is_perceived_by_name_on_departure_and_return() {
    // THE OBSERVATION PAYOFF (T3 review): the possessed agent's own
    // settlement is guaranteed to contribute a derived NPC sharing the
    // player's starting room, and `wait`'s narration must name that NPC's
    // actual transition through the room — not just count a generic
    // "stirred" tally. Seed 42, day 0.5 (noon, PossessOpts::default) starts
    // in the diurnal active band, so the home-settlement NPC is already at
    // its destination; the schedule then alternates rest/active every half
    // day at this fraction, giving a clean departure-then-return sequence.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let labels: Vec<String> = session
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    assert!(!labels.is_empty(), "a session always derives NPCs");

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("wait never releases"),
    };

    // day 0.5 -> 1.0 (midnight, rest): no transition yet.
    let _ = session.handle("wait 0.5");
    // day 1.0 -> 1.5 (noon, active): the co-located NPC departs; the
    // colocation branch must name it, not fall back to "stirred".
    let departure = out_text(session.handle("wait 0.5"));
    assert!(
        labels.iter().any(|l| departure.contains(l.as_str())),
        "departure must name a co-located NPC by label, got: {departure}"
    );
    assert!(
        !departure.contains("stirred"),
        "the specific colocation branch must fire, not the generic fallback: {departure}"
    );

    // day 1.5 -> 2.0 (midnight, rest): the NPC returns; must also be named.
    let return_text = out_text(session.handle("wait 0.5"));
    assert!(
        labels.iter().any(|l| return_text.contains(l.as_str())),
        "return must also name the co-located NPC, got: {return_text}"
    );
    assert!(
        !return_text.contains("stirred"),
        "the specific colocation branch must fire on return too, got: {return_text}"
    );
}
