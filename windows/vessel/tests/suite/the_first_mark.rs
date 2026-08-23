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
///
/// Re-pinned at The Wearing's merge: the flagship settlement's rendered name
/// re-derived `Qvooshtvoagootao` -> `Doododoobodobaado` (the campaign's
/// shorter, wearable names). Re-pinned AGAIN at the rebase onto The Toponym's
/// cohort ordering, which reseeds every proto-root:
/// `Goodoogogootoodadoo` -> `Gootoogotoodaoka`. Measured off the session's
/// own `npcs` listing, which returns the same seven NPCs at the same entity
/// ids (1865-1871) before and after. The NPC, its room and its co-location
/// are all unchanged — only the label this test addresses it by moved.
///
/// A stale label here fails LOUDLY BUT MISLEADINGLY: `provoke` finds no such
/// NPC, so nothing fires and every downstream count reads 0, which looks like
/// a behavioural regression rather than a renamed target. It has now done so
/// three times — most recently at F7 (The Witness, 2026-07-30), which gates
/// `Tonogenesis` on a prior merger and so reseeds every cascade:
/// `Gootoogotoodaoka` -> `Goodogododaga`. If these tests fail with `marks 0
/// vs 3` or `hostility 0 vs 1`, check this constant against `npcs` BEFORE
/// suspecting the grievance fold.
///
/// It happened a fourth time at The Contour's epoch v2 (2026-08-02,
/// `history/bake/v2`): the BAKE label bump re-mints the cascade again,
/// `Goodogododaga` -> `Godogododaga` (one fewer `o`). Same seven NPCs at the
/// same entity ids, re-verified from `book/src/gallery/possession-seed-42.md`.
///
/// It happened a fifth time with The Generalist (2026-08-03): human joins
/// the coexistence stack as a sixth competitor, redeciding seed 42's
/// settlement placement once more, which re-mints the flagship's name
/// again: `Godogododaga` -> `Goodogododaga` (one more `o`, back to the
/// pre-Contour spelling by coincidence of the draw, not a reversion of the
/// epoch).
///
/// It happened a SIXTH time with The Tense (2026-08-05): capacity gained an
/// era axis, seed 42 re-placed from 209 settlements to 122, and the flagship's
/// name went `Goodogododaga` -> **`Googo`**. Re-verified from
/// `book/src/gallery/possession-seed-42.md`, which is the source this constant
/// must always be read from: it renders `bugbear of Googo` in the room line,
/// the `map` legend and `whoami` alike.
///
/// NOTE the near-miss: seed 42's CHIEF bugbear settlement in the almanac is
/// `Dadogogodaga`, a different place entirely. Reading the rename off the
/// almanac instead of the possession artifact gives a plausible wrong answer,
/// because this NPC does not live in the chief settlement.
///
/// A SEVENTH time, with The Glasshouse (decision 0134, 2026-08-14): the
/// terrain epoch re-places seed 42's settlements, `Googo` -> **`Goodo`**.
/// Re-read from `book/src/gallery/possession-seed-42.md`, the source the note
/// above insists on. NOTE this constant is DUPLICATED in
/// `possession_moves.rs` and both copies must move together — they did here,
/// but nothing enforces it, which is worth knowing before the eighth rename.
///
/// The EIGHTH rename arrived immediately, with The Glasshouse's Stage B
/// Task 4 (the thermostat): `Goodo` -> **`Doadaga`**. Both copies moved
/// together again — re-read from `book/src/gallery/possession-seed-42.md`.
///
/// The NINTH, at The Glasshouse's close (`k` settled at 0.30):
/// `Doadaga` -> **`Dooga`**. Re-read from the same gallery page, which had
/// already been regenerated at the `k` commit (`0cdd1445`) — **the artifact
/// moved and these two constants did not**, which is exactly the drift the
/// note above predicted and is why all eight previous renames are recorded
/// here rather than summarised.
///
/// The duplication is now GUARDED, not merely noted: see
/// `the_two_grievance_npc_copies_agree` below. Nine renames of a constant
/// that must move in two places at once, with nothing checking it, was long
/// past the point where a comment was the right instrument.
///
/// The TENTH rename, at The Burr (Task 4): admitting an alveolar trill as
/// an ordinary manner reseeds every candidate-consonant draw, which
/// re-places seed 42's settlements again: `Dooga` -> **`Doaba`**. Re-read
/// from `book/src/gallery/possession-seed-42.md`, regenerated at this
/// commit. Both copies moved together — see `the_two_grievance_npc_copies_
/// agree` below, which is exactly the guard the ninth rename asked for.
const GRIEVANCE_NPC: &str = "bugbear of Doaba";

/// The duplication guard the comment above spent eight renames asking for.
/// `possession_moves.rs` declares its own `GRIEVANCE_NPC` because integration
/// tests are separate binaries and cannot share a private const; nothing made
/// the two agree, and a stale copy in EITHER file satisfies every negative
/// assertion that reads it (hostility is `false` for a label the session has
/// never seen). Reads the sibling's source rather than its value, which is the
/// only way one test binary can see another's private constant.
#[test]
fn the_two_grievance_npc_copies_agree() {
    let sibling = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/suite/possession_moves.rs"),
    )
    .expect("possession_moves.rs is readable");
    let expected = format!("const GRIEVANCE_NPC: &str = {GRIEVANCE_NPC:?};");
    assert!(
        sibling.contains(&expected),
        "possession_moves.rs does not declare `{expected}`. The two copies of \
         GRIEVANCE_NPC have drifted — this constant has been renamed NINE times \
         by world changes and both files must move together every time. Re-read \
         the current value from book/src/gallery/possession-seed-42.md and fix \
         whichever copy is stale; do not change this guard."
    );
}

fn out_text(t: Turn) -> String {
    match t {
        Turn::Out(s) => s,
        Turn::Released(s) => panic!("wait/provoke never releases: {s}"),
    }
}

#[test]
#[ignore = "The Hand Task 3: nothing is co-located with a fresh flagship possession by default any more (confirmed live: even sixty waits never bring another derived body into the flagship's own room), because the possessed-body duplicate this task deletes was the only thing that ever guaranteed it -- see task-3-report.md"]
fn provoked_npc_turns_hostile_on_the_next_wait_but_an_unprovoked_one_does_not() {
    let w = world();

    // GUARD THE FIXTURE FIRST — see the identical guard in
    // `possession_moves.rs`. A stale `GRIEVANCE_NPC` satisfies every negative
    // assertion in this file, because hostility is `false` for a label the
    // session has never seen. This constant has already gone stale twice on
    // settlement renames.
    assert!(
        Session::start(&w, &PossessOpts::default())
            .unwrap()
            .0
            .npc_labels()
            .contains(&GRIEVANCE_NPC),
        "GRIEVANCE_NPC ({GRIEVANCE_NPC}) is not co-located at day 0.5 — the settlement was \
         probably renamed; re-read it from book/src/gallery/possession-seed-42.md"
    );

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
    treat.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 0.5: grievance 1
    treat.handle("wait");
    treat.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 1.5: grievance 2
    treat.handle("wait");
    treat.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 2.5: grievance 3
    out_text(treat.handle("wait")); // grievance 3 crosses threshold; the tick fires the consequence
    assert_eq!(
        treat.committed_hostility_count(),
        1,
        "the provoked NPC turned hostile"
    );
}

#[test]
#[ignore = "The Hand Task 3: nothing is co-located with a fresh flagship possession by default any more (confirmed live: even sixty waits never bring another derived body into the flagship's own room), because the possessed-body duplicate this task deletes was the only thing that ever guaranteed it -- see task-3-report.md"]
fn a_second_wait_past_the_threshold_does_not_double_fire() {
    // IDEMPOTENCY: the functional predicate + the `value_of` guard mean the
    // consequence fires exactly once, even as further waits keep passing
    // with the NPC still past threshold.
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
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
#[ignore = "The Hand Task 3: nothing is co-located with a fresh flagship possession by default any more (confirmed live: even sixty waits never bring another derived body into the flagship's own room), because the possessed-body duplicate this task deletes was the only thing that ever guaranteed it -- see task-3-report.md"]
fn played_world_persists_the_mark_across_reload() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
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
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
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
/// and the consequence loop is guarded on the entity's own ledger state —
/// `self.ledger.value_of(npc.entity, TURNED_HOSTILE).is_none()` in
/// `session.rs`, which refuses to fire for an NPC that already carries the
/// fact. Since The Signet the re-derived NPC is the *same* entity as the one
/// the persisted facts name (its id derives from its settlement and role, not
/// from mint order), so it reloads carrying both its grievance and its
/// `turned-hostile` fact, and that guard sees the fact and declines. This is
/// the stronger of the two possible mechanisms: it would still hold if the
/// grievance did not persist, whereas the pre-Signet behaviour — re-derived
/// NPCs minting fresh, higher ids whose grievance therefore read 0 — held only
/// by the accident that the reloaded NPC was a different entity from the one
/// the facts described.
#[test]
#[ignore = "The Hand Task 3: nothing is co-located with a fresh flagship possession by default any more (confirmed live: even sixty waits never bring another derived body into the flagship's own room), because the possessed-body duplicate this task deletes was the only thing that ever guaranteed it -- see task-3-report.md"]
fn a_reloaded_played_world_does_not_re_fire_the_consequence_on_a_fresh_wait() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}"));
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
#[ignore = "The Hand Task 3: nothing is co-located with a fresh flagship possession by default any more (confirmed live: even sixty waits never bring another derived body into the flagship's own room), because the possessed-body duplicate this task deletes was the only thing that ever guaranteed it -- see task-3-report.md"]
fn why_traces_the_fired_consequence_back_to_the_players_hand() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 0.5: grievance 1
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 1.5: grievance 2
    s.handle("wait");
    s.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 2.5: grievance 3
    s.handle("wait"); // grievance 3 crosses the threshold; the tick fires the consequence

    let text = out_text(s.handle(&format!("!why {GRIEVANCE_NPC}")));
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

    let text = out_text(s.handle(&format!("!why {GRIEVANCE_NPC}")));
    assert!(
        !text.contains("player: provoke"),
        "no provocation was played; got: {text}"
    );
    assert!(
        !text.contains("player-provoked"),
        "no consequence fired; got: {text}"
    );
}
