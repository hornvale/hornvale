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

#[test]
fn played_world_persists_the_mark_across_reload() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait"); // grievance 3 crosses the threshold; the tick fires the consequence
    let played = s.into_played_world(w.seed);

    // round-trip through JSON exactly as save/load would.
    let json = serde_json::to_string(&played).unwrap();
    let reloaded: hornvale_kernel::World = serde_json::from_str(&json).unwrap();

    let player_facts = reloaded.ledger.find("disposition-shift").count();
    assert_eq!(player_facts, 3, "the player's marks survive reload");
    let hostility_facts = reloaded.ledger.find("turned-hostile").count();
    assert_eq!(hostility_facts, 1, "the consequence survives reload too");
    // the played world carries MORE facts than the pristine one.
    assert!(
        reloaded.ledger.len() > w.ledger.len(),
        "the world remembers"
    );
}

#[test]
fn played_world_is_deterministic_for_the_same_action_script() {
    let w = world();
    let run = |script: &[&str]| {
        let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
        for line in script {
            s.handle(line);
        }
        let played = s.into_played_world(w.seed);
        serde_json::to_string(&played).unwrap()
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
        "same seed + same trace -> byte-identical played world"
    );
}

#[test]
fn into_played_world_never_mutates_the_input_world() {
    let w = world();
    let before = serde_json::to_string(&w).unwrap();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    let _played = s.into_played_world(w.seed);
    let after = serde_json::to_string(&w).unwrap();
    assert_eq!(before, after, "the input world is never mutated in place");
}

/// The campaign's riskiest cross-task seam, proven rather than reasoned:
/// reload a played world into a *fresh* session and wait again — the
/// persisted `turned-hostile` consequence must NOT double-fire. It holds by
/// two independent mechanisms: `register_predicate` is idempotent on an
/// identical def (re-registering the persisted predicates does not panic),
/// and `next_entity` is serialized, so the re-derived NPCs mint fresh, higher
/// ids and carry grievance 0 (their `disposition-shift` facts point at the old
/// ids), leaving the persisted consequence the only one.
#[test]
fn a_reloaded_played_world_does_not_re_fire_the_consequence_on_a_fresh_wait() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}"));
    s.handle("wait"); // grievance 3 crosses the threshold; the consequence fires
    let played = s.into_played_world(w.seed);

    // round-trip through JSON exactly as save/load would.
    let json = serde_json::to_string(&played).unwrap();
    let reloaded: hornvale_kernel::World = serde_json::from_str(&json).unwrap();
    assert_eq!(
        reloaded.ledger.find("turned-hostile").count(),
        1,
        "the played world carries exactly one consequence"
    );

    // Start a NEW session on the reloaded world and wait again.
    let (mut s2, _opening) = Session::start(&reloaded, &PossessOpts::default()).unwrap();
    let before = s2.committed_hostility_count();
    s2.handle("wait");
    s2.handle("wait");
    assert_eq!(
        before, 1,
        "the persisted consequence is present in the reloaded session"
    );
    assert_eq!(
        s2.committed_hostility_count(),
        1,
        "waiting on a reloaded played world never re-fires a duplicate consequence"
    );
}

/// The butterfly-reader: `why` must recount the SESSION's evolving ledger —
/// the one holding the just-committed player facts and the fired
/// consequence — not the frozen input world, or the player's own hand
/// (and its fallout) would be invisible to the very verb built to trace it.
#[test]
fn why_traces_the_fired_consequence_back_to_the_players_hand() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 0.5: grievance 1
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 1.5: grievance 2
    s.handle("wait");
    s.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 2.5: grievance 3
    s.handle("wait"); // grievance 3 crosses the threshold; the tick fires the consequence

    let text = out_text(s.handle(&format!("why {GRIEVANCE_NPC}")));
    assert!(
        text.contains("player: provoke"),
        "the recount names the player's own act; got: {text}"
    );
    assert!(
        text.to_lowercase().contains("provoked"),
        "the fired consequence's provenance ('player-provoked') is traceable too; got: {text}"
    );
}

/// Control: an un-provoked NPC's recount is its ordinary history — `why`
/// never fabricates a player-authored chain where none was played.
#[test]
fn why_over_an_unprovoked_npc_names_no_player_hand() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("wait");
    s.handle("wait");

    let text = out_text(s.handle(&format!("why {GRIEVANCE_NPC}")));
    assert!(
        !text.contains("player: provoke"),
        "no provocation was played; got: {text}"
    );
    assert!(
        !text.contains("player-provoked"),
        "no consequence fired; got: {text}"
    );
}
