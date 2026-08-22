//! The Deed's acceptance test (Task 7): a player's in-character acts charge
//! time against the body's own mass and post facts into the session ledger,
//! so the trail they leave is indistinguishable from a creature's.
//!
//! The four claims, each with its own test:
//!
//! 1. a walk commits `agent-at` and moves the clock;
//! 2. the committed facts have the SHAPE a creature's do — same predicate,
//!    same object arity, same provenance kind, and nothing naming the driver;
//! 3. those facts survive `into_played_world` unfiltered (spec §2.4);
//! 4. a sleeping body refuses in-character acts and permits out-of-character
//!    ones (the gate, spec §3.3).
//!
//! Decision 0069's half — that entering, leaving and stepping within a room
//! commit nothing — is guarded where it already was, in `the_lintel` and
//! `the_blocking`, rather than restated here.

use hornvale_vessel::liveness::AGENT_AT;
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

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// The day a walk-band room line reports: `[room <id>, day <d>]`.
///
/// Read out of the prose rather than off an accessor deliberately — it is the
/// number a PLAYER sees, and it is what the gallery transcripts freeze, so a
/// test that watches it is watching the observable rather than a field.
fn day_of(text: &str) -> f64 {
    let (_, after) = text.split_once(", day ").unwrap_or_else(|| {
        panic!("no room line to read a day from in: {text}");
    });
    let digits: String = after
        .chars()
        .take_while(|c| c.is_ascii_digit() || *c == '.' || *c == '-' || *c == 'e')
        .collect();
    digits
        .parse()
        .unwrap_or_else(|e| panic!("day '{digits}' does not parse: {e}"))
}

#[test]
fn a_players_walk_leaves_an_agent_at_trail_and_charges_time() {
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    let before = s.committed_agent_at_count();
    let day_before = day_of(&out(s.handle("look")));

    let walked = out(s.handle("go n"));
    assert!(
        !walked.starts_with("Go where?") && !walked.starts_with("error:"),
        "the walk must actually happen for this test to mean anything: {walked}"
    );

    assert!(
        s.committed_agent_at_count() > before,
        "an in-character walk must post agent-at, as a creature's does \
         ({before} facts before, {} after)",
        s.committed_agent_at_count()
    );

    let day_after = day_of(&out(s.handle("look")));
    assert!(
        day_after > day_before,
        "an in-character walk must charge time: day {day_before} -> {day_after}"
    );
}

/// `text` with the day it reports elided, so two renderings taken at
/// different moments can be compared for everything EXCEPT the clock. Used
/// where the clock is expected to have moved and nothing else is.
fn without_day(text: &str) -> String {
    let mut out = String::new();
    let mut rest = text;
    while let Some((before, after)) = rest.split_once("day ") {
        out.push_str(before);
        out.push_str("day <elided>");
        let skipped = after
            .trim_start_matches(|c: char| c.is_ascii_digit() || c == '.' || c == '-' || c == 'e');
        rest = skipped;
    }
    out.push_str(rest);
    out
}

/// The player's `agent-at` facts, and one NPC's, read back out of the
/// session's own serialized ledger — the only surface an integration test has
/// on a `Fact`'s SHAPE.
fn agent_at_facts(session: &Session<'_>) -> Vec<serde_json::Value> {
    let ledger: serde_json::Value =
        serde_json::from_str(&session.session_ledger_json()).expect("the ledger is JSON");
    ledger
        .get("facts")
        .and_then(|f| f.as_array())
        .expect("a ledger serializes its facts as an array")
        .iter()
        .filter(|f| f.get("predicate").and_then(|p| p.as_str()) == Some(AGENT_AT))
        .cloned()
        .collect()
}

#[test]
fn a_players_trail_is_shaped_exactly_like_a_creatures() {
    // "Indistinguishable" is the acceptance test's own word, and a COUNT does
    // not establish it. This compares the envelopes: same predicate, same
    // object arity, same field set, and a provenance that names an act in the
    // world rather than the mind that chose it (spec §3.1).
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    // One tick, so the NPC layer has committed a creature's own `agent-at`
    // facts to compare against.
    let _ = s.handle("!wait 1");
    let creature_facts = agent_at_facts(&s);
    assert!(
        !creature_facts.is_empty(),
        "precondition: a creature must have walked, or there is nothing to \
         compare the player's trail against"
    );

    let before = creature_facts.len();
    let _ = s.handle("go n");
    let all = agent_at_facts(&s);
    assert!(
        all.len() > before,
        "precondition: the player's walk must have committed"
    );
    let players: Vec<&serde_json::Value> = all.iter().skip(before).collect();

    let creature = &creature_facts[0];
    for fact in &players {
        assert_eq!(
            fact.as_object().map(|o| o.keys().collect::<Vec<_>>()),
            creature.as_object().map(|o| o.keys().collect::<Vec<_>>()),
            "a player's fact carries a different field set from a creature's: \
             {fact} vs {creature}"
        );
        // Same object arity and kind: a room, as text.
        assert_eq!(
            fact.get("object")
                .and_then(|o| o.as_object())
                .map(|o| o.keys().cloned().collect::<Vec<_>>()),
            creature
                .get("object")
                .and_then(|o| o.as_object())
                .map(|o| o.keys().cloned().collect::<Vec<_>>()),
            "a player's agent-at object is not shaped like a creature's"
        );
        let provenance = fact
            .get("provenance")
            .and_then(|p| p.as_str())
            .expect("every fact carries a provenance");
        assert!(
            !provenance.is_empty(),
            "an in-character act still owes a reason"
        );
        for tell in ["player", "operator", "possess", "driver", "you"] {
            assert!(
                !provenance.to_lowercase().contains(tell),
                "provenance '{provenance}' names the driver ('{tell}'), which \
                 makes the player's trail distinguishable from a creature's \
                 (spec §3.1)"
            );
        }
    }
}

#[test]
fn a_players_facts_survive_into_the_saved_world() {
    // Spec §2.4: a player's trail is NOT filtered at `into_played_world`. The
    // session holds its ledger in memory, so no test above can see a filter
    // added at that boundary.
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let _ = s.handle("go n");
    let in_session = s.committed_agent_at_count();
    assert!(
        in_session > 0,
        "precondition: the walk must have committed, or this compares 0 == 0"
    );

    let played = s.into_played_world(hornvale_kernel::Seed(42));
    let saved = played.ledger.find(AGENT_AT).count();
    assert_eq!(
        saved, in_session,
        "player facts must reach the saved world unfiltered (spec §2.4)"
    );
}

#[test]
fn a_sleeping_body_refuses_in_character_and_permits_out_of_character() {
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    // The awake answer to the out-of-character verb, captured BEFORE the body
    // goes under: asserting only that the sleeping answer is non-empty would
    // pass on the unknown-verb refusal, which is exactly the vacuity this
    // campaign keeps finding.
    let awake_answer = out(s.handle("!whoami"));

    let slept = out(s.handle("sleep"));
    assert!(
        !slept.starts_with("No verb"),
        "`sleep` must be a verb for this test to mean anything: {slept}"
    );

    match s.handle("go n") {
        Turn::Out(t) => assert!(
            t.contains("asleep"),
            "a sleeping body must refuse an in-character act, naming the \
             reason (decision 0007): {t}"
        ),
        Turn::Released(_) => panic!("a refusal must not end the possession"),
    }
    match s.handle("look") {
        Turn::Out(t) => assert!(
            t.contains("asleep"),
            "`look` is in character too (spec §3.2 group B): {t}"
        ),
        Turn::Released(_) => panic!("a refusal must not end the possession"),
    }

    match s.handle("!whoami") {
        Turn::Out(t) => assert_eq!(
            without_day(&t),
            without_day(&awake_answer),
            "an out-of-character act bypasses the body's state entirely \
             (spec §2.2), so it must answer exactly as it did awake — only \
             the clock may have moved, and only because `sleep` charged it"
        ),
        Turn::Released(_) => panic!("`!whoami` must not end the possession"),
    }
    // Session control is not an act at all (spec §3.2 group D) and must stay
    // reachable — a body you cannot let go of is a hang.
    assert!(matches!(s.handle("release"), Turn::Released(_)));
}

#[test]
fn a_sleeping_body_charges_and_commits_nothing_for_a_refusal() {
    // A refusal is not an act: it must cost neither time nor a fact. Without
    // this, a gate that charged BEFORE it refused would pass every test above.
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let _ = s.handle("sleep");

    let facts_before = s.committed_fact_count();
    let day_before = s.day();
    for refused in ["go n", "go e", "back", "enter", "look", "wait 3"] {
        let reply = out(s.handle(refused));
        assert!(
            reply.contains("asleep"),
            "`{refused}` must be refused while asleep, got: {reply}"
        );
    }
    assert_eq!(
        s.committed_fact_count(),
        facts_before,
        "a refused act committed a fact"
    );
    assert_eq!(
        s.day(),
        day_before,
        "a refused act charged time: {day_before:?} -> {:?}",
        s.day()
    );
}

#[test]
fn the_body_wakes_when_its_own_cycle_says_so() {
    // The gate would be a trap rather than a capability if nothing could
    // reopen it — spec §3.4's whole argument for `!wait` is that observing a
    // state you cannot act in requires a clock you can still advance.
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let _ = s.handle("sleep");
    assert!(
        out(s.handle("look")).contains("asleep"),
        "precondition: the body must be asleep"
    );
    let _ = s.handle("!wait 2");
    let awake = out(s.handle("look"));
    assert!(
        !awake.contains("asleep"),
        "two days of out-of-character waiting must reach the next waking: {awake}"
    );
}
