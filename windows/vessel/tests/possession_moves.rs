//! The world moves without you: possess, wait across a phase, observe an NPC's
//! motion; and the same script is byte-deterministic.
use hornvale_vessel::{PossessOpts, Session, Turn};

/// Did THIS NPC commit a positional (`agent-at`) fact? Read from its own
/// `why` recount, the per-NPC provenance channel.
///
/// The three stay-put claims below used to read `Session::
/// committed_agent_at_count() == 0` instead. That accessor sums the whole
/// derived roster (the flagship settlement plus its two most populous
/// neighbours), so it can only stand in for "the NPC under test stayed put"
/// while EVERY derived NPC happens to sit on fresh water. The Tumult's
/// predation epoch ended that coincidence: at seed 42 the flagship
/// (`Doododoobodobaado`, rendered `Qvooshtvoagootao` before The Wearing) is
/// still on water and still drinks in place, but the neighbour then rendered
/// `Vootkeonoagootoaneo` was reseated off the river and its NPC now
/// wanders 18 steps looking for water. The proxy went nonzero while every
/// claim these tests actually make stayed true — so the proxy is narrowed to
/// the claim rather than re-pinned to 18, which would have frozen an
/// unrelated neighbour's water access into three tests that say nothing
/// about it.
fn walked(session: &mut Session, label: &str) -> bool {
    match session.handle(&format!("why {label}")) {
        Turn::Out(s) => s.contains("position on a day"),
        Turn::Released(_) => panic!("why never releases"),
    }
}

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

/// claim: structural(seed: none — world() fixture) — false-positive
/// seed-loop flag; `s` binds a &str label
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
    // provable via the FLAGSHIP's `agent-at`: it is now provable via `drank`.
    // (The Tumult, 2026-07-26: the flagship is STILL on water and still
    // drinks in place; predation reseated a NEIGHBOUR off the river, so the
    // session-wide agent-at count is no longer 0 even though this claim is
    // unchanged. See `walked` at the top of this file.) Starting at
    // day 0.5 (PossessOpts::default), the seek threshold (0.85) is crossed
    // at world day ~5.667 — "wait 7" spans that crossing.
    let flagship = session
        .npc_labels()
        .first()
        .map(|s| (*s).to_string())
        .expect("a session always derives NPCs");
    let out = session.handle("wait 7");
    assert!(
        session.committed_drank_count() >= 1,
        "the world moved on wait (a drank fact committed even though the \
         flagship's own NPC never walked)"
    );
    assert!(
        !walked(&mut session, &flagship),
        "measured: the on-water flagship settlement's own NPC never needs to \
         walk to reach fresh water"
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
    // settlement directly ONTO fresh water (0 moves, drinks in place — the
    // SAME measurement above, re-run after the freshwater re-point): the
    // co-located NPC now never leaves the room at all, so neither a
    // departure NOR an arrival is ever the true event across a full drive
    // cycle. (The Confluence also put both derived NEIGHBOURS on water; The
    // Tumult's predation epoch, 2026-07-26, reseated one of them —
    // `Vootkeonoagootoaneo` — off the river, and its NPC now wanders. That
    // is invisible to the player's room and does not touch this claim, which
    // is about the CO-LOCATED NPC only.) This test asserts that reality
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
    // The premise the whole test rests on, asserted last so the narration
    // reads above are taken on an unperturbed session: the co-located NPC
    // (the flagship's own, always first in the derived roster) really did
    // stay put, so "no departure happened" is a fact about it and not an
    // artifact of nobody being derived.
    let colocated = labels.first().expect("a session always derives NPCs");
    assert!(
        !walked(&mut session, colocated),
        "measured: the co-located on-water NPC never leaves the room"
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
    // a `drank`, never an `agent-at`. (The Tumult, 2026-07-26: still true of
    // THIS NPC after the predation epoch; a neighbour's NPC now walks, which
    // is why the check below is per-NPC rather than session-wide.)
    session.handle("wait 7");
    assert!(
        session.committed_drank_count() >= 1,
        "the NPC satisfied its sustenance goal"
    );
    assert!(
        !walked(&mut session, label),
        "measured: this NPC's own settlement is on-water; it never walks"
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
    // different co-located home-settlement NPC (the bugbear of the flagship,
    // rendered Doododoobodobaado since The Wearing) whose merged
    // diurnal/fatigue physics put it on a REST
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
    // The Tense re-measure (2026-08-05): the flip moved from day 5.5 to day
    // 10.5. Re-measured rather than re-pinned to the weaker reading it now
    // gives at 5.5 ("grows restless"), because that would have silently traded
    // away what this test is FOR. Day-by-day over the first fortnight:
    //
    //   +1 +2 rest · +3 +4 +5 restless · +6..+9 rest · +10 CASTS ABOUT FOR
    //   WATER · +11 restless · +12 rest · +13 restless · +14 rest
    //
    // "Grows restless" is the fatigue baseline being disturbed; "casts about
    // for water" is thirst actually WINNING the drive competition, and it is
    // the latter this assertion exists to witness. It happens once in fourteen
    // days now, so the pin is narrow — if it moves again, re-measure the same
    // way rather than accepting a restlessness read in its place.
    // The Tense re-measure (2026-08-05). THIRST is no longer reachable here:
    // swept `wait 1..=25` from a fresh session each time and "casts about for
    // water" appears at none of them. What the NPC does instead, by wait:
    //
    //   1-4 rest · 5 restless · 6-8 rest · 9-10 EATS ITS FILL · 11 content ·
    //   12-13 restless · 14-16 rest · 17-18 restless · 19-22 eats its fill · …
    //
    // Re-pinned to `wait 9` / "eats its fill" rather than to the "grows
    // restless" this now gives at 5, and the distinction is the point: "grows
    // restless" is the fatigue baseline being disturbed, while "eats its fill"
    // is a drive actually WINNING the competition and being acted on — the same
    // KIND of reading the thirst pin was, just hunger instead of thirst.
    //
    // Coverage cost, recorded rather than absorbed: this assertion used to
    // witness THIRST beating the fatigue-rest baseline, and now witnesses
    // hunger doing it. The thirst limb of the drive competition is no longer
    // exercised anywhere in this test. Sweeping a wider range, or waiting in
    // day-sized steps (thirst does surface at ten successive `wait 1`s, which
    // is NOT the same state as one `wait 10`), would restore it.
    session.handle("wait 9");
    let later = out_text(session.handle("needs"));
    assert!(
        later.contains("eats its fill"),
        "a hungry NPC eats its fill, not resting: {later}"
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
fn a_repeat_same_day_provoke_is_a_ledger_no_op_and_the_narration_says_so() {
    // SAME-DAY DEDUP IS INTENTIONAL: one disposition shift per (NPC, day,
    // direction) — escalation is gated on time passing (a `wait`), not on
    // repeating the verb. Two `provoke`s on the same NPC with no
    // intervening `wait` produce a byte-identical `Fact` envelope, so
    // `Ledger::commit` returns `Ok(false)` (idempotent no-op) the second
    // time: `committed_disposition_count` must not double-count, and the
    // narration must be honest that nothing further landed.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(s) => panic!("provoke never releases: {s}"),
    };

    let first = out_text(session.handle("provoke"));
    assert_eq!(
        session.committed_disposition_count(),
        1,
        "the first provoke commits one fact"
    );
    assert!(
        first.to_lowercase().contains("provoke") || first.contains("bristle"),
        "the first provoke is the effect narration: {first}"
    );

    let second = out_text(session.handle("provoke"));
    assert_eq!(
        session.committed_disposition_count(),
        1,
        "a same-day repeat is a ledger no-op: the count must not double-count"
    );
    assert!(
        second.contains("already"),
        "a same-day repeat must not claim a fresh effect: {second}"
    );
    assert_ne!(
        first, second,
        "the repeat narration must differ from the effect narration"
    );
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

/// The seed-42 settled NPC guaranteed co-located with the possessed agent at
/// `PossessOpts::default()`'s starting room (day 0.5, before any `go`) — see
/// `probe_colocated_npc_label_at_day_zero` in this file's history and the
/// task-2 report for how this was discovered. It never walks (the
/// on-water flagship settlement's peoples drink in place), so it stays
/// co-located across every `wait` in these tests too.
///
/// Re-pinned at The Wearing's merge: the flagship's rendered name re-derived
/// `Qvooshtvoagootao` -> `Doododoobodobaado`. Re-pinned AGAIN at the rebase
/// onto The Toponym's cohort ordering, which reseeds every proto-root:
/// `Goodoogogootoodadoo` -> `Gootoogotoodaoka`. Re-pinned a third time at F7
/// (The Witness, 2026-07-30), which gates `Tonogenesis` on a prior merger
/// and reseeds every cascade: `Gootoogotoodaoka` -> `Goodogododaga`.
/// Measured off the session's own `npcs` listing, which returns the same
/// seven NPCs at the same entity ids (1865-1871) before and after — the NPC
/// and its co-location are unchanged; only the label moved.
///
/// Re-pinned once more at The Contour's epoch v2 (2026-08-02,
/// `history/bake/v2`): the BAKE label bump re-mints the cascade again,
/// `Goodogododaga` -> `Godogododaga` (one fewer `o`). Same seven NPCs at the
/// same entity ids, re-verified from `book/src/gallery/possession-seed-42.md`.
///
/// Re-pinned once more by The Generalist (2026-08-03): human joins the
/// coexistence stack as a sixth competitor, redeciding seed 42's settlement
/// placement once more, which re-mints the flagship's name again:
/// `Godogododaga` -> `Goodogododaga` (one more `o`, back to the pre-Contour
/// spelling by coincidence of the draw, not a reversion of the epoch).
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
const GRIEVANCE_NPC: &str = "bugbear of Googo";

#[test]
fn grievance_accumulates_across_waits_and_crosses_the_hostility_threshold() {
    // GUARD THE FIXTURE FIRST. `would_turn_hostile` answers `false` for a
    // label it has never seen, so the NEGATIVE assertions below are satisfied
    // by a stale or misspelt `GRIEVANCE_NPC` just as well as by a real one —
    // only a positive assertion can fail. This constant has already gone
    // stale twice on settlement renames. Assert the NPC exists, so the next
    // rename fails HERE, saying so.
    assert!(
        Session::start(&world(), &PossessOpts::default())
            .unwrap()
            .0
            .npc_labels()
            .contains(&GRIEVANCE_NPC),
        "GRIEVANCE_NPC ({GRIEVANCE_NPC}) is not co-located at day 0.5 — the settlement was \
         probably renamed; re-read it from book/src/gallery/possession-seed-42.md"
    );

    // THE GRIEVANCE FOLD (Task 2, direct social consequence, not an ambient
    // drive tip): an un-provoked NPC carries zero grievance and is never
    // hostile.
    let w = world();
    let (a, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    assert!(
        !a.would_turn_hostile(GRIEVANCE_NPC),
        "un-provoked NPC is neutral"
    );

    // Provoking across three distinct days climbs grievance past the
    // threshold. Same-day repeats dedup (Task 1), so each provoke here is
    // separated by a `wait` — three distinct days of antagonism.
    let (mut b, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    b.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 0.5: grievance 1
    b.handle("wait");
    b.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 1.5: grievance 2
    b.handle("wait");
    assert!(
        !b.would_turn_hostile(GRIEVANCE_NPC),
        "two provokes is below threshold"
    );
    b.handle(&format!("provoke {GRIEVANCE_NPC}")); // day 2.5: grievance 3
    assert!(
        b.would_turn_hostile(GRIEVANCE_NPC),
        "three provokes crosses the threshold"
    );

    // soothe pulls back below the threshold (intent vs outcome).
    b.handle("wait");
    b.handle(&format!("soothe {GRIEVANCE_NPC}")); // day 3.5: grievance 2
    assert!(
        !b.would_turn_hostile(GRIEVANCE_NPC),
        "soothe pulls the NPC back below hostile"
    );
}

#[test]
fn unprovoked_npcs_have_zero_grievance() {
    // BYTE-IDENTITY GUARD: with no player facts, every derived NPC's
    // grievance fold is exactly zero — an unplayed world (or a session that
    // never provokes/soothes) is byte-identical by construction.
    let w = world();
    let s = Session::start(&w, &PossessOpts::default()).unwrap().0;
    for label in s.npc_labels() {
        assert_eq!(
            s.npc_grievance(label),
            Some(0.0),
            "un-provoked NPC {label} must carry exactly zero grievance"
        );
    }
}
