//! The Deed, Task 5: group A's bare forms are retired. `why`, `npcs`,
//! `help`, `eyes`, `whoami`, `provoke`, and `soothe` answer only behind a
//! leading `!`; the bare spelling is now an ordinary unknown-verb refusal.
use hornvale_vessel::{PossessOpts, Session, Turn};

/// Identical to the helper duplicated across this crate's other integration
/// tests (`possession_moves.rs`, `the_first_mark.rs`, `display_handle.rs`):
/// each integration test file is its own crate, so the seed-42 fixture
/// world is rebuilt verbatim rather than shared.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// A retired bare group-A verb must refuse exactly like any other unknown
/// verb, not merely stop answering usefully — this is what makes the
/// retirement checkable rather than merely asserted in prose.
///
/// Note on the brief's own illustrative assertion
/// (`text.contains("no verb") || text.contains("know")`): it does not match
/// this crate's actual refusal wording, `"No verb '{other}'
/// ('!help' lists them)."` — capital `N`, and no `"know"` substring
/// anywhere. Lower-cased here so the check is case-insensitive rather than
/// permanently red; the intent (assert this is the unknown-verb refusal, not
/// some other `Turn::Out`) is unchanged from what the brief asked for. The
/// brief also named a `test_session`/`session_for` helper that does not
/// exist anywhere in this crate's tests
/// (`grep -rn "fn test_session\|fn session_for" windows/vessel/tests/`
/// returns nothing) — every sibling integration test instead builds the
/// world and calls `Session::start` inline (see `display_handle.rs`), which
/// is what this file does too.
#[test]
fn a_bare_group_a_verb_is_no_longer_a_command() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    match s.handle("whoami") {
        Turn::Out(text) => assert!(
            text.to_lowercase().contains("no verb"),
            "a retired bare form must refuse as an unknown verb, got: {text}"
        ),
        Turn::Released(_) => panic!("`whoami` must not end the possession"),
    }
}

/// Every one of the seven group-A verbs, not just `whoami`: the bare form
/// refuses and the sigil form answers. `eyes` is checked with an argument
/// too, since it carries two dispatch arms (report / set) that both moved.
#[test]
fn every_group_a_verb_retired_its_bare_form() {
    let cases: &[&str] = &[
        "why nobody-by-this-name",
        "npcs",
        "help",
        "eyes",
        "whoami",
        "provoke",
        "soothe",
    ];
    let w = world();
    for bare_line in cases {
        let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
        match s.handle(bare_line) {
            Turn::Out(text) => assert!(
                text.to_lowercase().contains("no verb"),
                "bare `{bare_line}` must refuse as an unknown verb, got: {text}"
            ),
            Turn::Released(_) => panic!("`{bare_line}` must not end the possession"),
        }

        let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
        let sigilled = format!("!{bare_line}");
        match s.handle(&sigilled) {
            Turn::Out(text) => assert!(
                !text.is_empty() && !text.to_lowercase().contains("no verb"),
                "`{sigilled}` must be a recognised out-of-character verb that actually \
                 answers, not merely a non-refusal (an empty string would pass the \
                 refusal check alone), got: {text}"
            ),
            Turn::Released(_) => panic!("`{sigilled}` must not end the possession"),
        }
    }
}

/// `eyes` specifically: both its report and set arms moved under the sigil,
/// and both must still work there (the argument-presence branch that used
/// to live in the bare `"eyes"` arms).
#[test]
fn the_sigil_eyes_still_has_both_its_report_and_set_arms() {
    let w = world();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let report = match s.handle("!eyes") {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("`!eyes` must not end the possession"),
    };
    assert!(
        !report.is_empty() && !report.to_lowercase().contains("no verb"),
        "bare `!eyes` must actually report, not merely answer with a non-empty \
         refusal (fix round 1, Finding 2 — the earlier `!report.is_empty()` alone \
         could not fail: this crate's refusal text is itself non-empty), got: {report}"
    );

    let (mut s2, _opening2) = Session::start(&w, &PossessOpts::default()).unwrap();
    match s2.handle("!eyes off") {
        Turn::Out(text) => assert!(!text.to_lowercase().contains("no verb"), "{text}"),
        Turn::Released(_) => panic!("`!eyes off` must not end the possession"),
    }
}

/// An unclassified `!<in-character verb>` must not silently alias to the
/// bare form's behaviour — the sigil selects a namespace, and a verb with no
/// classified out-of-character half must refuse there exactly as an unknown
/// verb would in the bare namespace.
///
/// **This test used to probe `!look`, and cannot any more.** Task 6 withheld
/// `!look`/`!knows` on the grounds that neither has a renderer gate to relax;
/// Task 7's fix round shipped both, because the gate they discriminate on
/// turned out to be the BODY's rather than a renderer's (spec §2.2/§3.4 — see
/// `ooc_objective.rs`). So the probe moved to spec §3.2's **group C**, whose
/// members have no out-of-character half at all and are not waiting for one:
/// `consult` and `write` are in-world literacy acts, and an operator reading
/// the Book over a sleeping body's shoulder is a capability nobody has
/// specified.
///
/// Two verbs rather than one, and both chosen for having no side effect on
/// the session (a fresh session each way regardless), so the comparison is
/// about dispatch and nothing else.
#[test]
fn an_unclassified_sigil_verb_refuses_rather_than_aliasing_to_the_bare_form() {
    let w = world();
    for verb in ["consult", "write"] {
        let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
        let bare = match s.handle(verb) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("`{verb}` must not end the possession"),
        };
        assert!(
            !bare.to_lowercase().contains("no verb"),
            "precondition: bare `{verb}` must BE a verb, or the comparison below \
             is between two identical refusals: {bare}"
        );
        let (mut s2, _opening2) = Session::start(&w, &PossessOpts::default()).unwrap();
        let sigilled = match s2.handle(&format!("!{verb}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("`!{verb}` must not end the possession"),
        };
        assert!(
            sigilled.to_lowercase().contains("no verb"),
            "`!{verb}` has no classified out-of-character half (spec §3.2 group C) \
             and must refuse, not silently reuse `{verb}`'s own answer; got: \
             {sigilled}"
        );
        assert_ne!(
            bare, sigilled,
            "the sigil must not have fallen through to the bare arm's behaviour"
        );
    }
}
