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
    // The drive model (the-wanting): an NPC starts away from its resource
    // with drive 0 at world day 0, rising at SUSTENANCE's 0.15/day. Starting
    // at day 0.5 (PossessOpts::default), the seek threshold (0.85) is
    // crossed at world day ~5.667 and the return (sated 0.15) at ~6.833 — a
    // "wait 7" spans one full genuine drive cycle (departure + return).
    let out = session.handle("wait 7");
    // After a full drive cycle, at least one agent-at has been committed.
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
    // "stirred" tally. The drive model (the-wanting) replaces the old
    // fixed half-day schedule: every derived NPC starts away from its
    // resource with drive 0 at world day 0, rising at SUSTENANCE's
    // 0.15/day, so (from day 0.5, PossessOpts::default) the seek threshold
    // (0.85) is crossed at world day ~5.667 (departure) and the return
    // (sated 0.15, falling at 0.6/day) at ~6.833 — giving a clean
    // departure-then-return sequence across three waits.
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

    // day 0.5 -> 5.5: still short of the ~5.667 departure crossing: no
    // transition yet.
    let _ = session.handle("wait 5");
    // day 5.5 -> 6.5: crosses the ~5.667 seek threshold: the co-located NPC
    // departs; the colocation branch must name it, not fall back to "stirred".
    let departure = out_text(session.handle("wait 1"));
    assert!(
        labels.iter().any(|l| departure.contains(l.as_str())),
        "departure must name a co-located NPC by label, got: {departure}"
    );
    assert!(
        !departure.contains("stirred"),
        "the specific colocation branch must fire, not the generic fallback: {departure}"
    );

    // day 6.5 -> 8.5: crosses the ~6.833 return threshold (and no further
    // crossing until ~11.5): the NPC returns; must also be named.
    let return_text = out_text(session.handle("wait 2"));
    assert!(
        labels.iter().any(|l| return_text.contains(l.as_str())),
        "return must also name the co-located NPC, got: {return_text}"
    );
    assert!(
        !return_text.contains("stirred"),
        "the specific colocation branch must fire on return too, got: {return_text}"
    );
}

#[test]
fn why_recounts_an_npcs_dated_agent_at_history_after_it_moves() {
    // THE PROVENANCE READ (the-quickening T4): a committed `agent-at` is a
    // dated, provenanced fact, so the world remembers — `why <npc>` must
    // recount it with the day it was asserted, not just that it happened.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let labels: Vec<String> = session
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    let label = labels.first().expect("a session always derives NPCs");

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("why never releases"),
    };

    // Before any wait, the NPC has no committed agent-at yet (day-0 pin):
    // recounting it either says nothing is recorded, or (since the NPC
    // entity was minted this session) never mentions "day".
    let before = out_text(session.handle(&format!("why {label}")));
    assert!(
        !before.contains("day"),
        "before any wait, no dated agent-at exists to recount: {before}"
    );

    // Advance across a full drive cycle (the-wanting: ~5.667 days to the
    // seek crossing) so the tick commits at least one agent-at.
    session.handle("wait 7");
    assert!(session.committed_agent_at_count() >= 1, "the NPC moved");

    let recount = out_text(session.handle(&format!("why {label}")));
    assert!(
        recount.contains(label.as_str()),
        "the recount leads with the NPC's own name: {recount}"
    );
    assert!(
        recount.contains("day"),
        "the recount names the day the position was asserted: {recount}"
    );
    assert!(
        !recount.contains("No one here answers"),
        "the label must resolve to the NPC that actually moved: {recount}"
    );
}

#[test]
fn why_resolves_by_numeric_id_and_reports_an_unknown_target() {
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let listing = match session.handle("npcs") {
        Turn::Out(s) => s,
        _ => panic!("npcs must not release"),
    };
    let id: u64 = listing
        .lines()
        .nth(1)
        .and_then(|l| l.split(['[', ']']).nth(1))
        .and_then(|s| s.parse().ok())
        .expect("npcs lists at least one [id] label line");
    // Advance across a full drive cycle (the-wanting) so the id-resolved
    // NPC has a committed, dated agent-at to recount.
    session.handle("wait 7");
    match session.handle(&format!("why {id}")) {
        Turn::Out(s) => assert!(s.contains("day"), "id-resolved recount names a day: {s}"),
        _ => panic!("why must not release"),
    }
    match session.handle("why nobody-by-this-name") {
        Turn::Out(s) => assert!(s.contains("No one here answers")),
        _ => panic!("why must not release"),
    }
}
