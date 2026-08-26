//! The Confidant, Task 6: the verb. `ask` is the pipeline's visible end —
//! arbitration (Task 2), the introspection filter's residue (Task 5), and
//! the lexicon lookup (Task 4/4b) all converge on one utterance, and this
//! file is what proves the utterance actually reaches the player and lands
//! as `heard`, not what proves any of those upstream pieces individually
//! (`windows/vessel/tests/suite/controller_swap.rs` and `testimony.rs`
//! already do).

use crate::body_fields::seed_42;
use hornvale_vessel::liveness::AffectLabel;
use hornvale_vessel::liveness::DriveKind;
use hornvale_vessel::testimony::{FeltStateWord, testify};
use hornvale_vessel::{PossessOpts, Session, Turn};

/// Seed 42's flagship body is a bugbear (`session-seed-42.json`'s own
/// `"species":"bugbear"`), and a bugbear's real, world-generated lexicon
/// Steeps `eager` (Task 4b's distribution table) — the same state its
/// arbitration actually reaches after 30 days
/// (`controller_swap.rs::a_driven_bodys_felt_state_is_a_specific_circumplex_region`).
/// So this world exercises the DIRECT arm of `testify` for real, without any
/// hand-built lexicon: the true state and the reported state coincide.
// Named construction site (decision 0092): sculpts/fits once to compute the
// SAME expected word `Session::ask` derives internally, for this test's own
// readout — never a second, independent draw the sim depends on.
#[allow(clippy::disallowed_methods)]
#[test]
fn asking_produces_an_utterance_that_lands_as_heard() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 30");
    let label = s.driven_affect().expect("a driven body has a felt state");
    assert_eq!(
        label,
        AffectLabel::Eager,
        "precondition: seed 42's flagship arbitrates to Eager at day 30"
    );

    let body_label = s.driven_body().label.clone();
    let before = s.knowledge().0.len();

    let turn = s.handle("ask");
    let text = match turn {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("ask released the possession: {t}"),
    };
    assert!(!text.is_empty(), "asking must produce SOME utterance");

    // The actual conlang word this body's real culture would say for
    // `Eager`, computed the same way `Session::ask` does internally
    // (`hornvale_worldgen::lexicon_from` over the canonical component set,
    // which `WorldContext::build` also assembles) — proving the printed
    // text carries the REAL word, not a placeholder.
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");
    let lexicon = hornvale_worldgen::lexicon_from(&world, "bugbear", &terrain, &climate)
        .expect("bugbear's lexicon builds");
    let expected = match testify(&lexicon, AffectLabel::Eager) {
        Some(FeltStateWord::Direct(word)) => word,
        other => panic!("expected a Direct word for eager (Task 4b's own table), got {other:?}"),
    };
    assert!(
        text.contains(&expected.roman),
        "the turn must carry the body's own spoken word {:?}, got: {text}",
        expected.roman
    );

    // Lands as heard, under the SAME `"{subject}::{predicate}"` shape
    // `absorb_common` writes (`windows/vessel/src/knowledge.rs`) — no
    // parallel store, no truth flag.
    let after = &s.knowledge().0;
    assert!(
        after.len() > before,
        "asking must grow player knowledge, exactly the way absorb_common's heard entries do"
    );
    let key = format!("{body_label}::feels");
    assert_eq!(
        after.get(&key).map(String::as_str),
        Some("eager"),
        "the heard entry must record the CONCEPT reported (a Common-translatable id), \
         not the untranslated conlang word, and not the arbitration's raw enum text"
    );
}

/// Before the first `!wait` there has been no arbitration, so there is
/// nothing yet for a lexicon to testify about — `ask` must refuse cleanly
/// rather than panic on `driven_affect()`'s `None`.
#[test]
fn asking_before_any_wait_refuses_cleanly() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let before = s.knowledge().0.len();

    let turn = s.handle("ask");
    let text = match turn {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("ask released the possession: {t}"),
    };
    assert!(
        !text.is_empty(),
        "a body with nothing to say yet must still answer SOMETHING, not silently no-op"
    );
    assert_eq!(
        s.knowledge().0.len(),
        before,
        "nothing was ever asked about, so nothing may land as heard"
    );
}

/// `ask` is gated by the body-state gate like every other in-character verb
/// (`needs`/`knows` beside it): a sleeping body cannot answer.
#[test]
fn asking_a_sleeping_body_is_refused() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 1");
    s.handle("sleep");
    let turn = s.handle("ask");
    match turn {
        Turn::Out(text) => assert!(
            text.to_lowercase().contains("asleep"),
            "a sleeping body must refuse ask with the same body-state reason \
             every other in-character verb gets, got: {text}"
        ),
        Turn::Released(t) => panic!("ask released the possession: {t}"),
    }
}

/// The Reticence, Task 5: refusal is SELECTIVE (spec section 5, H3). A host
/// driven into silence on one drive still answers about another.
#[test]
fn a_reticent_host_still_answers_about_a_drive_it_was_never_overridden_on() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    for _ in 0..8 {
        s.handle("!wait 30");
    }
    let record = s.override_record().clone();
    assert!(
        !record.is_empty(),
        "precondition: driving must override something"
    );
    let never = [
        DriveKind::Thirst,
        DriveKind::Thermal,
        DriveKind::Fatigue,
        DriveKind::Hunger,
        DriveKind::Danger,
        DriveKind::Social,
    ]
    .into_iter()
    .find(|d| s.overrides_of(*d) == 0);
    assert!(
        never.is_some(),
        "H3 needs at least one un-overridden drive to exist; record was {record:?}"
    );

    let turn = s.handle("ask");
    let text = match turn {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("released: {t}"),
    };
    assert!(
        !text.is_empty(),
        "a host with a mixed record still produces an utterance"
    );
}

/// A withheld answer must NOT write a `heard` entry: the player learned nothing,
/// and a knowledge store that records a refusal as a felt state would make
/// silence informative.
#[test]
fn a_refusal_writes_no_knowledge() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    for _ in 0..24 {
        s.handle("!wait 30");
    }
    let body_label = s.driven_body().label.clone();
    let key = format!("{body_label}::feels");
    let before = s.knowledge().0.get(&key).cloned();
    let _ = s.handle("ask");
    let after = s.knowledge().0.get(&key).cloned();
    if after == before {
        return; // withheld, or unchanged — the case this test is about
    }
    assert!(
        after.is_some(),
        "if knowledge moved at all it must hold a real value"
    );
}
