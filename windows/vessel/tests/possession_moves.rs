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
    // Peoples-only (The Wilding): this test isolates the settled on-water
    // settlement's invariant (its NPCs drink in place, never walk). The wild
    // beasts DO walk — that is The Quarry waking — and their motion is covered
    // by `a_wild_beast_walks_away_from_water_and_is_observed` below, not here.
    let opts = PossessOpts {
        wild_agents: false,
        ..PossessOpts::default()
    };
    let (mut session, _opening) = Session::start(&w, &opts).unwrap();
    // THE CONFLUENCE, MEASURED: pre-Confluence, an NPC started away from its
    // resource and a full drive cycle committed at least one `agent-at` (a
    // real walk). Settlement condensation now pulls seed 42's flagship
    // settlement (and its derived NPCs) directly onto fresh water (see
    // `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — 0 moves, drinks in place), so "the world moved on wait" is no longer
    // provable via `agent-at`: it is now provable via `drank`. Starting at
    // day 0.5 (PossessOpts::default), the seek threshold (0.85) is crossed
    // at world day ~5.667 — "wait 7" spans that crossing.
    let out = session.handle("wait 7");
    assert_eq!(
        session.committed_agent_at_count(),
        0,
        "measured: the on-water flagship settlement's NPCs never need to \
         walk to reach fresh water"
    );
    assert!(
        session.committed_drank_count() >= 1,
        "the world moved on wait (a drank fact committed even though no \
         one walked)"
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
fn a_colocated_npcs_drinking_in_place_is_not_narrated_as_a_false_departure() {
    // THE OBSERVATION PAYOFF, RE-MEASURED (T3 review originally proved this
    // via a genuine departure; The Confluence changes what "genuine" means
    // here): the possessed agent's own settlement is guaranteed to
    // contribute a derived NPC sharing the player's starting room, and
    // `wait`'s narration must never CLAIM a departure/arrival that did not
    // actually happen — only a real positional transition earns the named
    // branch; anything else falls to the generic "stirred" tally.
    //
    // Pre-Confluence, the co-located NPC departed on its first exploration
    // step (proven by name) but then explored indefinitely without
    // returning (measured in `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — a real, settlement-placement gap, not the belief mechanism).
    // The Confluence's settlement condensation moves seed 42's flagship
    // settlement (and its two derived neighbors) directly ONTO fresh water
    // (0 moves, drinks in place — the SAME measurement above, re-run after
    // the freshwater re-point): the co-located NPC now never leaves the
    // room at all, so neither a departure NOR an arrival is ever the true
    // event across a full drive cycle. This test asserts that reality
    // honestly: `wait` across the seek crossing (~5.667 days from day 0.5)
    // commits a `drank` (the world genuinely moved) but narrates it as the
    // generic "stirred" sensing, never inventing a named departure for an
    // NPC that stayed exactly where it was.
    //
    // The mechanism this test used to prove end-to-end (naming a REAL
    // departure/arrival) is not exercised here — seed 42's flagship
    // settlement structurally cannot produce one any more (`village_info`
    // always resolves to the same, now on-water, settlement). That
    // end-to-end coverage gap is captured as a followup (decision-ledger);
    // the naming logic itself (`Session::narrate_motion`) is unchanged code,
    // reviewed at the point The Confluence stopped touching it.
    //
    // Peoples-only (The Wilding): the wild beasts DO leave their rooms (The
    // Quarry, live); this test isolates the settled on-water NPC's stay-put
    // narration. The wild motion path has its own coverage below.
    let w = world();
    let opts = PossessOpts {
        wild_agents: false,
        ..PossessOpts::default()
    };
    let (mut session, _opening) = Session::start(&w, &opts).unwrap();
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

    // Cross the seek threshold (~5.667 days from day 0.5): a `drank` commits
    // (measured: `committed_drank_count() >= 1`), but no one's position
    // changed.
    let crossing_wait = out_text(session.handle("wait 7"));
    assert_eq!(
        session.committed_agent_at_count(),
        0,
        "measured: the on-water settlement's NPCs never leave the room"
    );
    assert!(
        session.committed_drank_count() >= 1,
        "the world genuinely moved (a drank fact committed)"
    );
    assert!(
        !labels.iter().any(|l| crossing_wait.contains(l.as_str())),
        "no NPC actually departed or arrived, so none may be named: {crossing_wait}"
    );
    assert!(
        crossing_wait.contains("stirred"),
        "the generic sensing fallback must fire for a same-room drink event: {crossing_wait}"
    );

    // A subsequent wait, still sated, must stay quiet (no spurious
    // departure/arrival, and no further "stirred" noise once nothing at all
    // is committed).
    let still_here = out_text(session.handle("wait 1"));
    assert!(
        !labels.iter().any(|l| still_here.contains(l.as_str())),
        "a co-located NPC that never left must not be named as departing/arriving: {still_here}"
    );
}

#[test]
fn why_recounts_an_npcs_dated_history_after_it_drinks() {
    // THE PROVENANCE READ (the-quickening T4): a committed `agent-at` or
    // `drank` is a dated, provenanced fact, so the world remembers —
    // `why <npc>` must recount it with the day it was asserted, not just
    // that it happened. (Renamed from "...after_it_moves": The Confluence's
    // on-water flagship settlement never moves at all — see below.)
    //
    // Peoples-only (The Wilding): scoped to the settled flagship NPC's own
    // dated history; the wild beasts' motion is covered separately below.
    let w = world();
    let opts = PossessOpts {
        wild_agents: false,
        ..PossessOpts::default()
    };
    let (mut session, _opening) = Session::start(&w, &opts).unwrap();
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
    // seek crossing) so the tick commits a dated fact. THE CONFLUENCE,
    // MEASURED: this NPC (the flagship settlement's own) now condenses
    // directly onto fresh water (see `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — 0 moves, drinks in place), so the dated fact the crossing commits is
    // a `drank`, never an `agent-at`.
    session.handle("wait 7");
    assert_eq!(
        session.committed_agent_at_count(),
        0,
        "measured: this NPC's own settlement is on-water; it never walks"
    );
    assert!(
        session.committed_drank_count() >= 1,
        "the NPC satisfied its sustenance goal"
    );

    let recount = out_text(session.handle(&format!("why {label}")));
    assert!(
        recount.contains(label.as_str()),
        "the recount leads with the NPC's own name: {recount}"
    );
    assert!(
        recount.contains("day"),
        "the recount names the day the drank was asserted: {recount}"
    );
    assert!(
        !recount.contains("No one here answers"),
        "the label must resolve to the NPC that actually drank: {recount}"
    );
    // THE CONFLUENCE'S PAYOFF, RE-MEASURED: The Foresight/Surmise era pinned
    // that this exact settlement's NPC never reaches water and only ever
    // recounts as "wandered, having found no water yet (thirst)" — an
    // IGNORANT explore step, never the believer's beeline or a drink.
    // Settlement condensation resolves that gap by moving the settlement,
    // not by making the agent smarter: the NPC now stands on fresh water
    // from the start, so the FIRST crossing is a `drank`, not a move at
    // all — the recount's provenance is the drink's own reason.
    // Mutation-verify: blanking `DriveMovements::step`'s "drank from the
    // river (thirst sated)" string in `liveness.rs` reds this assertion
    // while leaving the day/name/resolution checks above green (they don't
    // touch provenance text).
    assert!(
        recount.contains("drank from the river (thirst sated)"),
        "the recount names the drink's own reason: {recount}"
    );
}

#[test]
fn needs_reports_a_colocated_npcs_felt_state_and_it_differs_across_the_drive_cycle() {
    // THE FELT-STATE READ (the-wanting T4): `needs` renders a co-located
    // NPC's drive as diegetic prose, never a number, and that prose must
    // actually track the drive over time — not a static line. The
    // possessed agent's own settlement guarantees a co-located NPC at the
    // starting room (the-quickening T3 review), and every derived NPC
    // starts away from its resource with drive 0 at world day 0, rising at
    // SUSTENANCE's 0.15/day (act 0.85, sated 0.15).
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("needs never releases"),
    };

    // Day 0.5 (PossessOpts::default, before any wait): re-derived at the
    // the-living-community merge. The history-driven re-placement seats a
    // different co-located home-settlement NPC (the bugbear of
    // Qvooshtvoagootao) whose merged diurnal/fatigue physics put it on a REST
    // phase at day 0.5 — it reads "settles down to rest", not the old
    // "seems content". (A PLACEMENT/PHYSICS behavior change, not a moved value:
    // the drive-cycle-differs intent is preserved by re-pinning against a day
    // where the felt state genuinely moves — see below.)
    let early = out_text(session.handle("needs"));
    assert!(
        early.contains("settles down to rest"),
        "the co-located NPC reads as resting at day 0.5: {early}"
    );
    assert!(
        !early.contains("No one else is here"),
        "the home settlement's NPC must be co-located at the start: {early}"
    );

    // Wait to day 5.5: thirst has now risen past its restlessness threshold and
    // momentarily dominates the fatigue-rest baseline, so the co-located NPC
    // casts about for water — the felt state has moved off "settles down to
    // rest" to a thirst-restlessness read (measured: day 5.5 is where this
    // NPC's drive competition flips).
    session.handle("wait 5");
    let later = out_text(session.handle("needs"));
    assert!(
        later.contains("casts about for water"),
        "a thirsty NPC casts about for water, not resting: {later}"
    );

    // THE MUTATION-VERIFIED ASSERTION: the felt state DIFFERS across the
    // drive cycle. Fixing the drive to a constant (e.g. always returning
    // 0.0) would make `early == later` and red this line.
    assert_ne!(
        early, later,
        "the felt state must differ across the drive cycle: {early} / {later}"
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

#[test]
fn provoke_commits_one_player_authored_disposition_fact() {
    // THE FIRST PLAYER-AUTHORED FACT: `provoke` commits a disposition-shift
    // fact about a co-located NPC into the session-owned ledger, distinct
    // from every fact the world's own systems commit (the `player:`
    // provenance is what tells the two apart). The possessed agent's own
    // settlement guarantees a co-located NPC at the starting room
    // (the-quickening T3 review), so no `go` is needed first.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let before = session.committed_disposition_count();
    let turn = session.handle("provoke");
    let after = session.committed_disposition_count();
    match turn {
        Turn::Out(s) => assert!(
            s.to_lowercase().contains("provoke") || s.contains("bristle"),
            "diegetic acknowledgement, got: {s}"
        ),
        Turn::Released(s) => panic!("expected Out, got Released({s})"),
    }
    assert_eq!(after, before + 1, "exactly one disposition fact committed");
}

#[test]
fn possession_with_no_act_leaves_session_ledger_unchanged() {
    // BYTE-IDENTITY GUARD: a read-only verb (`look`) must commit nothing —
    // the session ledger is byte-identical to a fresh, untouched session.
    let w = world();
    let a = Session::start(&w, &PossessOpts::default())
        .unwrap()
        .0
        .session_ledger_json();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let _ = s.handle("look"); // a read-only verb
    assert_eq!(a, s.session_ledger_json(), "read-only verbs commit nothing");
}

#[test]
fn a_wild_beast_walks_away_from_water_and_is_observed() {
    // THE WILDING, LIVE — the settled tests' inverse. `PossessOpts::wild_agents`
    // (on by default) appends the world's wild beast agents to the peopled
    // NPCs. Unlike seed 42's on-water flagship settlement — whose peoples drink
    // in place and never walk (the peoples-only tests above) — the wild beasts
    // are placed at their concentrations (a herd, a lair) away from fresh water,
    // so crossing a full drive cycle commits real `agent-at` walks. This is the
    // population that DOES move: The Quarry's predator niche and the drive layer,
    // finally exercised by a live agent in possession.
    let w = world();
    let (mut wild_session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();

    // Wild agents enlarge the roster over the peoples-only session, and read as
    // beasts ("a wild <species>").
    let peopled_count = {
        let opts = PossessOpts {
            wild_agents: false,
            ..PossessOpts::default()
        };
        let (s, _o) = Session::start(&w, &opts).unwrap();
        s.npc_labels().len()
    };
    let labels: Vec<String> = wild_session
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    assert!(
        labels.len() > peopled_count,
        "wild agents enlarge the roster: {} vs peopled {peopled_count}",
        labels.len()
    );
    assert!(
        labels.iter().any(|l| l.contains("wild")),
        "at least one appended agent reads as a wild beast: {labels:?}"
    );

    // Cross the seek crossing (~5.667 days from day 0.5): the wild beasts,
    // unlike the on-water peoples, commit real walks — a dated, provenanced
    // `agent-at` the world remembers.
    let out = match wild_session.handle("wait 7") {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("wait never releases"),
    };
    assert!(
        wild_session.committed_agent_at_count() >= 1,
        "a wild beast placed away from water committed at least one real walk"
    );
    assert!(!out.is_empty(), "the wait narrates the world's motion");
}
