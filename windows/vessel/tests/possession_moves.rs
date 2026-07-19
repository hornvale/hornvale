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
    // "stirred" tally.
    //
    // Every derived NPC starts away from its resource with drive 0 at world
    // day 0, rising at SUSTENANCE's 0.15/day, so (from day 0.5,
    // PossessOpts::default) the seek threshold (0.85) is crossed at world
    // day `crossing` = act/rise ≈ 5.667. Pre-Surmise (`resource_room`) water
    // was ALWAYS exactly one mesh hop from home, so a full seek-drink-return
    // round trip completed in ~0.2 days and this test could compute BOTH a
    // departure and a return window from the authored constants alone.
    // Under The Surmise's real, terrain-derived exploration, that guarantee
    // is gone: even after the T5 re-wire to FRESH water (The Freshet), the
    // flagship's own home settlement's greedy-downhill walk never reaches a
    // river within any wait a player would plausibly issue (measured in
    // `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — a real, settlement-placement-dependent gap in the exploration
    // POLICY, not in the belief mechanism, and not a regression this test
    // should paper over). The co-located NPC therefore explores
    // indefinitely and never returns, so
    // only the DEPARTURE half is provable end-to-end here; the "return"
    // narration branch (and the full round-trip's provenance, "went down to
    // the river it knew (thirst)" / "drank from the river (thirst sated)")
    // is proven instead at the mechanism level, decoupled from real
    // reachability, by
    // `liveness.rs`'s `the_recount_surfaces_the_drives_own_provenance_for_the_full_round_trip`.
    //
    // The very FIRST exploration step still departs at exactly
    // `crossing + MOVE_DURATION` regardless of whether it is a real
    // "went down to the river it knew" step or an ignorant "wandered,
    // having found no water yet" explore step (both cost one
    // `MOVE_DURATION`), so the departure math below is unchanged from the
    // pre-Surmise model.
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

    // liveness.rs's authored action duration (MOVE_DURATION); not exported,
    // so mirrored here as the model's known constant (as SUSTENANCE's own
    // rise/act already are, below).
    const MOVE_DURATION: f64 = 0.1;
    let p = hornvale_vessel::liveness::SUSTENANCE;
    let start_day = PossessOpts::default().day.day;
    let crossing = p.act / p.rise; // the world day the drive first reaches `act`
    let first_step = crossing + MOVE_DURATION;
    let second_step = first_step + MOVE_DURATION;

    // Land squarely inside [first_step, second_step) — the co-located NPC
    // has taken its first exploration step away from home (and not yet a
    // second), so it is observed as DEPARTED from the player's room.
    let land_departed = (first_step + second_step) / 2.0;
    let departure = out_text(session.handle(&format!("wait {}", land_departed - start_day)));
    assert!(
        labels.iter().any(|l| departure.contains(l.as_str())),
        "departure must name a co-located NPC by label, got: {departure}"
    );
    assert!(
        !departure.contains("stirred"),
        "the specific colocation branch must fire, not the generic fallback: {departure}"
    );

    // A subsequent wait must not spuriously re-report a departure/arrival —
    // the NPC keeps exploring away, so the room stays quiet (no double-count,
    // no "stirred" fallback firing from noise).
    let still_away = out_text(session.handle("wait 1"));
    assert!(
        !labels.iter().any(|l| still_away.contains(l.as_str())),
        "an NPC that stays away must not be re-named as departing/arriving again: {still_away}"
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
    // THE FORESIGHT T3, UPDATED FOR THE SURMISE: the planned move's
    // provenance must surface through the SAME recount, not just an
    // undifferentiated "it moved" — the drive is what gives the routine a
    // WHY. Under the belief model, an agent that has never stood in water is
    // IGNORANT and takes the EXPLORE branch ("wandered, having found no
    // water yet (thirst)"), not the believer's beeline ("went down to the
    // river it knew (thirst)") — and on the real seed-42 world, the
    // flagship's own home settlement's greedy-downhill exploration never
    // reaches fresh water within any wait a player would plausibly issue,
    // even after the T5 re-wire to FRESH water (measured:
    // `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — a real, settlement-placement-dependent gap in the exploration
    // POLICY), so this NPC never stands in water and never leaves the
    // explore branch. Mutation-verify: blanking `DriveMovements::step`'s
    // "wandered, having found no water yet (thirst)" string in `liveness.rs`
    // reds this assertion while leaving every other assertion in this test
    // green (the day/name/resolution checks above don't touch provenance
    // text at all).
    assert!(
        recount.contains("wandered, having found no water yet (thirst)"),
        "the recount names the drive's own reason for the move: {recount}"
    );
    // THE FORESIGHT T4's ORIGINAL CLAIM — that the SAME recount also names
    // the `drank` fact the journey was FOR — no longer holds for THIS real
    // settlement (it never reaches water, so it never drinks; see the
    // finding above): a full round trip's "went down to the river it knew
    // (thirst)" / "drank from the river (thirst sated)" provenance is
    // instead proven, decoupled from real-world reachability, by
    // `liveness.rs`'s
    // `the_recount_surfaces_the_drives_own_provenance_for_the_full_round_trip`.
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

    // Day 0.5 (PossessOpts::default, before any wait): the co-located
    // home-settlement NPC's drive is barely risen (0.5 * 0.15 = 0.075),
    // at or below the `sated` threshold (0.15) -> "seems content".
    let early = out_text(session.handle("needs"));
    assert!(
        early.contains("seems content"),
        "a freshly derived NPC reads content at day 0.5: {early}"
    );
    assert!(
        !early.contains("No one else is here"),
        "the home settlement's NPC must be co-located at the start: {early}"
    );

    // Wait to day 5.5: still short of the ~5.667 seek crossing (still away
    // from the resource, still co-located at home), but the drive has
    // risen into the dead-band (5.5 * 0.15 = 0.825, between sated and act)
    // -> "could do with a drink", not "seems content" anymore.
    session.handle("wait 5");
    let later = out_text(session.handle("needs"));
    assert!(
        later.contains("could do with a drink"),
        "a risen drive reads as wanting, not content: {later}"
    );

    // THE MUTATION-VERIFIED ASSERTION: the felt state DIFFERS across the
    // drive cycle. Fixing the drive to a constant (e.g. always returning
    // 0.0) would make `early == later` (both "seems content") and red this
    // line.
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
