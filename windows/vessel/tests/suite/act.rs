//! Acts — addressable without being stored (Task 7, decision 0580, spec
//! §4.5). The session-facing half of `windows/vessel::act`'s guarantee:
//! `windows/vessel/src/act.rs`'s own unit tests pin the pure arithmetic
//! (handle determinism, distinctness, the degenerate all-zero-shaped case);
//! this file pins the claim that actually needs a live [`Session`] —
//! generating many acts off real session state never commits a fact, and
//! the ledger-comparison this relies on is capable of detecting a fact if
//! one WERE committed (an empty-diff test needs a positive control, or it is
//! not evidence of anything).

use hornvale_kernel::{EntityId, Seed, World};
use hornvale_vessel::act::{
    Act, act_occurred_on, act_precedes, anyone_present, deed_of, present_at, witnessed,
};
use hornvale_vessel::liveness::AGENT_AT;
use hornvale_vessel::{PossessOpts, Session, Turn};

fn world() -> World {
    hornvale_worldgen::build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// Build one [`Act`] straight off a live session's own public surface —
/// [`Session::day`] and [`Session::agent_entity`], the two constituents the
/// module doc names — plus a caller-supplied `deed`/`patient` pair. This is
/// what a live verb dispatch would call to reify an act at zero ledger
/// cost.
fn act_off_session(session: &Session<'_>, deed: &'static str, patient: Option<EntityId>) -> Act {
    Act {
        actor: session.agent_entity(),
        deed,
        patient,
        day: session.day(),
    }
}

/// **1. A pure function of its constituents, grounded in real session
/// data.** Two `Act`s built off the SAME session at the same moment, same
/// deed, same patient, must hash identically — and `act_off_session` reads
/// only `Session::day`/`Session::agent_entity`, never anything private, so
/// this is also the demonstration that those two accessors are sufficient
/// to build an addressable act.
#[test]
fn an_acts_handle_off_real_session_data_is_a_pure_function_of_its_constituents() {
    let w = world();
    let (session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    let a = act_off_session(&session, "avow", None);
    let b = act_off_session(&session, "avow", None);
    assert_eq!(
        a.handle(),
        b.handle(),
        "two acts built off the same session, same deed, same patient must \
         hash identically"
    );

    let different_deed = act_off_session(&session, "provoke", None);
    assert_ne!(
        a.handle(),
        different_deed.handle(),
        "varying the deed alone must change the handle even off real session data"
    );
}

/// **3. Nothing is committed generating many acts — and the comparison
/// used to prove that can actually detect a fact.** Captures the session's
/// serialized ledger and total fact count before deriving several hundred
/// acts (handles, `witnessed`, `present_at`, `deed_of`, `act_precedes`,
/// `act_occurred_on`, `anyone_present` over the walk-band chart's own
/// marks) purely off `Session::day`/`agent_entity`/`purview`, asserts
/// byte-identity after, and only THEN performs a real in-character walk (the
/// same committing action `player_acts_commit.rs` exercises) to prove the
/// comparison is not vacuously equal by construction — an empty diff needs
/// a positive control.
#[test]
fn generating_many_acts_commits_nothing_and_the_ledger_comparison_can_detect_a_real_commit() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    let ledger_before = session.session_ledger_json();
    let facts_before = session.committed_fact_count();

    let deeds = ["provoke", "soothe", "avow", "witness", ""];
    let mut handles = std::collections::BTreeSet::new();
    for i in 0..400u64 {
        let deed = deeds[(i as usize) % deeds.len()];
        let patient = if i % 3 == 0 {
            None
        } else {
            EntityId::new(i % 97 + 1)
        };
        let act = act_off_session(&session, deed, patient);
        handles.insert(act.handle());

        // Exercise every derived read this task ships, over the SAME live
        // session, none of which may touch the ledger.
        let _ = deed_of(&act);
        let _ = act_occurred_on(&act);
        let other = act_off_session(&session, "avow", patient);
        let _ = act_precedes(&act, &other);
        let pool = [session.agent_entity()];
        let _ = present_at(session.agent_entity(), &pool);
        let _ = witnessed(&act, session.agent_entity(), &pool);

        let scene = session
            .purview(0)
            .expect("purview reads at the outdoor start");
        let marks = scene
            .cells // lexicon: SurroundsCell is a chart AREA unit, never a mesh vertex
            .iter()
            .find(|c| c.room == scene.observer.room)
            .map(|c| c.marks.as_slice())
            .unwrap_or(&[]);
        let _ = anyone_present(marks);
    }
    assert!(
        handles.len() > 1,
        "the swept deed/patient grid should not degenerate to one handle"
    );

    assert_eq!(
        session.committed_fact_count(),
        facts_before,
        "deriving act handles and reads must not change the session's fact count"
    );
    assert_eq!(
        session.session_ledger_json(),
        ledger_before,
        "deriving act handles and reads must not move the session's ledger by a single byte"
    );

    // **Positive control.** Nothing above proves the comparison CAN see a
    // real commit — only that it saw none. Perform an ordinary in-character
    // walk (the same committing action `player_acts_commit.rs` pins) and
    // confirm both the count and the serialized ledger actually move.
    let walked = out(session.handle("go n"));
    assert!(
        !walked.starts_with("Go where?") && !walked.starts_with("error:"),
        "the walk must actually happen for the positive control to mean anything: {walked}"
    );
    assert!(
        session.committed_fact_count() > facts_before,
        "the positive control's walk must move the fact count — otherwise the \
         byte-identity assertions above proved nothing"
    );
    assert_ne!(
        session.session_ledger_json(),
        ledger_before,
        "the positive control's walk must move the serialized ledger"
    );

    // **4. A snapshot DOES persist a committed fact (decision 0368).** The
    // walk above committed an ordinary `agent-at` fact — no act-derived read
    // ever commits one, so there is nothing act-specific to carry — but the
    // `--out`/`into_played_world` mechanism decision 0368 documents must
    // still carry it through, and this is the executable proof that it
    // does, in the same session this test already built.
    let agent_at_in_session = session.committed_agent_at_count();
    assert!(
        agent_at_in_session > 0,
        "precondition: the walk must have committed an agent-at fact"
    );
    let played = session.into_played_world(Seed(42));
    let agent_at_in_saved = played.ledger.find(AGENT_AT).count();
    assert_eq!(
        agent_at_in_saved, agent_at_in_session,
        "a fact committed during play must survive into_played_world when a \
         snapshot (--out) asks for it (decision 0368) — even though no act-\
         derived read in this task ever produces one to carry"
    );
}

/// The walk-band chart's own marks, read straight off a live
/// [`Session::purview`] call, plug directly into [`anyone_present`] with no
/// adaptation — the concrete demonstration that this module's session
/// surface (day/agent_entity/purview) is what it claims to be, not merely
/// type-compatible with it. Deliberately re-derives the same boolean two
/// ways (the function, and the raw predicate its own doc states) rather
/// than asserting a fixed truth value: which way seed 42's outdoor start
/// happens to fall is not the property under test.
#[test]
fn anyone_present_reads_directly_off_a_live_sessions_purview() {
    let w = world();
    let (session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let scene = session
        .purview(0)
        .expect("purview reads at the outdoor start");
    let observer_square = scene
        .cells // lexicon: SurroundsCell is a chart AREA unit, never a mesh vertex
        .iter()
        .find(|c| c.room == scene.observer.room)
        .expect("the observer's own chart square is always present in its own chart");

    assert_eq!(
        anyone_present(&observer_square.marks),
        observer_square.marks.iter().any(|m| m.kind == "agent"),
        "anyone_present must agree with a direct scan of the same marks"
    );
}

/// A sanity check that [`WorldTime`] round-trips through [`act_occurred_on`]
/// unchanged when built off a real session's own clock, not just a
/// hand-constructed one — `act.rs`'s own unit test already covers the pure
/// case.
#[test]
fn act_occurred_on_matches_the_sessions_own_clock() {
    let w = world();
    let (session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let act = act_off_session(&session, "avow", None);
    assert_eq!(act_occurred_on(&act), session.day());
}
