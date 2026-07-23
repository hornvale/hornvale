//! The First Mark, one-hop forward integration: a grieved NPC's `wait`-tick
//! consequence. Provoking a co-located NPC past `HOSTILITY_THRESHOLD`
//! (Task 2's grievance fold) fires a discrete, idempotent `turned-hostile`
//! fact toward the possessed player on the very `wait` that crosses it — the
//! world remembers the player's own acts, not just its own systems'.
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

/// The seed-42 settled NPC guaranteed co-located with the possessed agent at
/// `PossessOpts::default()`'s starting room (day 0.5, before any `go`) — see
/// `possession_moves.rs`'s `GRIEVANCE_NPC` (Task 2).
const GRIEVANCE_NPC: &str = "bugbear of Qvooshtvoagootao";

fn out_text(t: Turn) -> String {
    match t {
        Turn::Out(s) => s,
        Turn::Released(s) => panic!("wait/provoke never releases: {s}"),
    }
}

#[test]
fn provoked_npc_turns_hostile_on_the_next_wait_but_an_unprovoked_one_does_not() {
    let w = world();

    // control: only waits, never provokes -> no hostility.
    let (mut control, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    for _ in 0..4 {
        control.handle("wait");
    }
    assert_eq!(
        control.committed_hostility_count(),
        0,
        "no provocation, no hostility"
    );

    // treatment: antagonize across three days, then wait -> the NPC turns
    // hostile. Same-day dedup (Task 1) means each provoke must be separated
    // by a wait to land as a distinct day's grievance.
    let (mut treat, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    treat.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 0.5: grievance 1
    treat.handle("wait");
    treat.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 1.5: grievance 2
    treat.handle("wait");
    treat.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 2.5: grievance 3
    out_text(treat.handle("wait")); // grievance 3 crosses threshold; the tick fires the consequence
    assert_eq!(
        treat.committed_hostility_count(),
        1,
        "the provoked NPC turned hostile"
    );
}

#[test]
fn a_second_wait_past_the_threshold_does_not_double_fire() {
    // IDEMPOTENCY: the functional predicate + the `value_of` guard mean the
    // consequence fires exactly once, even as further waits keep passing
    // with the NPC still past threshold.
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    assert_eq!(s.committed_hostility_count(), 1, "fires exactly once");
    s.handle("wait");
    s.handle("wait");
    assert_eq!(
        s.committed_hostility_count(),
        1,
        "further waits do not refire the same NPC's hostile act"
    );
}

#[test]
fn same_action_trace_is_byte_identical() {
    let w = world();
    let run = |script: &[&str]| {
        let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
        for line in script {
            s.handle(line);
        }
        s.session_ledger_json()
    };
    let script = [
        format!("provoke {GRIEVANCE_NPC}"),
        "wait".to_string(),
        format!("provoke {GRIEVANCE_NPC}"),
        "wait".to_string(),
        format!("provoke {GRIEVANCE_NPC}"),
        "wait".to_string(),
    ];
    let script: Vec<&str> = script.iter().map(String::as_str).collect();
    assert_eq!(
        run(&script),
        run(&script),
        "same seed + same trace -> identical ledger"
    );
}
