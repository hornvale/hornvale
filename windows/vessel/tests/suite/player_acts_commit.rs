//! The Deed's acceptance test (Task 7): a player's in-character acts charge
//! time against the body's own mass and post facts into the session ledger,
//! so the trail they leave is indistinguishable from a creature's.
//!
//! **THE PROVENANCE KINDS NOW DIFFER, AND THAT IS DELIBERATE (The Warrant,
//! Task 3).** A creature's step carries the producer name `vessel/liveness`;
//! a player's still carries authored prose (`session.rs`'s
//! `WALKED_PROVENANCE`/`RETRACED_PROVENANCE`), because a player has no `Mode`
//! to promote onto an errand predicate — spec §7.4 accepts and names the
//! asymmetry. So claim 2's "same provenance kind" is no longer true in
//! substance, and the tests below never asserted it: they assert the
//! provenance is non-empty and names no driver, both of which still hold on
//! both sides. Read the sameness claim as being about predicate, object arity
//! and driver-anonymity, not about the shape of the provenance string.
//!
//! The four claims, each with its own test:
//!
//! 1. a walk commits `agent-at` and moves the clock;
//! 2. the committed facts have the SHAPE a creature's do — same predicate,
//!    same object arity, and nothing naming the driver (the provenance kind
//!    is the deliberate exception above);
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
    world_at(42)
}

fn world_at(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// A seed whose flagship roll actually WALKS — at least one resident commits
/// an `agent-at` across a seven-day wait.
///
/// **New at The Roll (Task 7).** Claim 2 below compares a creature's `agent-at`
/// ENVELOPE against the player's, so it needs a creature that walked, and
/// since the roll a session's roster is the residents of the settlement you
/// stand in rather than one body from each of three settlements plus four
/// world-top wild beasts. Seed 42's flagship stands on a river: its residents
/// drink in place and commit no positional fact at all, so the comparison has
/// nothing to compare against there. The measurement and the reason this seed
/// rather than another are recorded once, in `possession_moves.rs`'s
/// `WALKING_SEED`; this file pins the same number because a test binary cannot
/// read a sibling's private constant, and the precondition below fails loudly
/// if an epoch moves it.
/// type-audit: bare-ok(index)
const WALKING_SEED: u64 = 14;

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// The day a walk-band room line used to report: `[room <id>, day <d>]`.
///
/// **Retired by The Ken's Task 3.** The header carried the number a PLAYER
/// saw, and this parsed the observable rather than a field on purpose — but
/// Task 3 removed the day (and the id) from the header entirely, so there is
/// no longer a rendered day here to watch. `s.day()` is the only surface
/// left, and this test now reads it directly; the walk's `agent-at` count
/// above is still read off the render-independent ledger, so this file loses
/// no coverage, only the one channel that stopped existing.
fn day_of(s: &Session<'_>) -> f64 {
    s.day().as_std_days()
}

#[test]
fn a_players_walk_leaves_an_agent_at_trail_and_charges_time() {
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    let before = s.committed_agent_at_count();
    let _ = out(s.handle("look"));
    let day_before = day_of(&s);

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

    let _ = out(s.handle("look"));
    let day_after = day_of(&s);
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
    // world rather than the mind that chose it (spec §3.1). The two sides'
    // provenance STRINGS diverged with The Warrant's Task 3 — a creature's
    // names its producer, a player's is still authored prose — and the
    // assertions below were always about non-emptiness and driver-anonymity
    // rather than about that string's shape; see the module doc and The
    // Warrant spec §7.4 for why the asymmetry is deliberate.
    // At `WALKING_SEED` rather than 42: this comparison needs a creature that
    // WALKED, and seed 42's flagship residents drink in place (see the
    // constant's own note).
    let w = world_at(WALKING_SEED);
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    // A full drive cycle, so the NPC layer has committed a creature's own
    // `agent-at` facts to compare against. Seven days rather than one: the
    // sustenance seek threshold is crossed at world day ~5.667, and before
    // that a content creature holds.
    let _ = s.handle("!wait 7");
    let creature_facts = agent_at_facts(&s);
    assert!(
        !creature_facts.is_empty(),
        "precondition: a creature must have walked, or there is nothing to \
         compare the player's trail against; if this is empty an epoch has \
         moved WALKING_SEED and it must be re-measured"
    );

    let before = creature_facts.len();
    // Whichever way out of this room is walkable — the claim is about the
    // SHAPE of what a walk commits, never about a compass direction, and a
    // pinned `go n` would tie it to one world's geometry.
    for dir in ["n", "e", "s", "w"] {
        let _ = s.handle(&format!("go {dir}"));
        if agent_at_facts(&s).len() > before {
            break;
        }
    }
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

#[test]
fn a_band_change_charges_time_and_commits_nothing() {
    // Fix round 1, B1. Decision 0069 forbids the COMMIT for a band change, not
    // the CHARGE — its own consequence paragraph says "the only thing spent is
    // turns", and `clock::base_ticks` has priced a within-room step at a tenth
    // of a room-to-room move all along. Before this fix `enter`/`out` spent
    // nothing at all, so walking through a doorway into a different chamber
    // was free while stepping ONE CELL inside a chamber cost time, and
    // `out`→`enter` was an unbounded free loop.
    //
    // Both halves are asserted per verb, because they fail independently: a
    // charge added inside a handler that also commits would satisfy the clock
    // assertion and violate 0069.
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    // Each step names the band line its reply MUST carry, so a refusal ("There
    // is no way to anywhere from here.", "Nothing here is built") can never be
    // mistaken for a free act that correctly charged nothing.
    // `enter the hearth` rather than `enter further in` (The Cruck, Task 3):
    // seed 42's flagship is the backroom, `T{ H{ W }, S }`, so the threshold
    // forks and `further in` REFUSES there — and a refusal charges nothing, so
    // the "must actually change band" guard below would have fired with
    // exactly the message it was written to print. The hearthroom is a chamber
    // every built structure has, so naming it costs this test nothing; what it
    // asserts is unchanged, that a band change charges time and commits no
    // fact.
    for (verb, expected) in [
        ("enter", "[chamber "),
        ("enter the hearth", "[chamber "),
        ("out", "[room "),
    ] {
        let facts_before = s.committed_fact_count();
        let day_before = s.day().as_std_days();
        let reply = out(s.handle(verb));
        assert!(
            reply.contains(expected),
            "`{verb}` must actually change band for this to guard anything — \
             expected a `{expected}` line, got: {reply}"
        );
        assert!(
            s.day().as_std_days() > day_before,
            "`{verb}` is an in-character act and must charge time \
             (decision 0069 forbids the commit, not the charge): \
             day {day_before} -> {}",
            s.day().as_std_days()
        );
        assert_eq!(
            s.committed_fact_count(),
            facts_before,
            "`{verb}` must commit nothing: fine position is never serialized \
             (decision 0069)"
        );
    }
}

#[test]
fn a_nonsense_token_is_an_unknown_verb_even_while_asleep() {
    // Fix round 1, B3. The gate stands in front of ACTS, not in front of verb
    // RESOLUTION. A token that resolves to nothing is not an act the body
    // could be too asleep to perform, and answering "You cannot — you are
    // asleep." to `whoami` tells a player that a RETIRED group-A bare form is
    // a real in-character verb merely blocked by body state (spec §3.2: "a
    // bare group-A verb is an ordinary unknown-verb refusal").
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let _ = s.handle("sleep");
    assert!(
        out(s.handle("look")).contains("asleep"),
        "precondition: the body must be asleep, or this proves nothing"
    );
    for token in ["xyzzy", "whoami", "npcs", "why", "provoke"] {
        let reply = out(s.handle(token));
        assert!(
            reply.to_lowercase().contains("no verb"),
            "`{token}` resolves to no verb, so a sleeping body must refuse it \
             exactly as an awake one does — as unknown, not as gated: {reply}"
        );
    }
}

#[test]
fn sleep_refuses_an_argument_rather_than_swallowing_it() {
    // Fix round 1, B4. `session.rs`'s own `map` arm states the principle: "an
    // ignored argument is how a player comes to believe they asked for
    // something and got it." `sleep 5` used to lie down for an unrelated
    // length of time and say nothing about the 5.
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    for arg in ["sleep 5", "sleep forever"] {
        let reply = out(s.handle(arg));
        assert!(
            !reply.contains("You lie down"),
            "`{arg}` must not silently lie down as if the argument were not \
             there: {reply}"
        );
        assert!(
            reply.to_lowercase().contains("sleep"),
            "the refusal must name what `sleep` does instead: {reply}"
        );
        assert!(
            !out(s.handle("look")).contains("asleep"),
            "a refused `{arg}` must not have put the body under"
        );
    }
    // ...and the bare form still works, so the refusal is about the argument
    // rather than about the verb.
    assert!(
        out(s.handle("sleep")).contains("You lie down"),
        "bare `sleep` must still lie down"
    );
}
