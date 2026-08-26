//! The Confidant, Task 6: the verb. `ask` is the pipeline's visible end —
//! arbitration (Task 2), the introspection filter's residue (Task 5), and
//! the lexicon lookup (Task 4/4b) all converge on one utterance, and this
//! file is what proves the utterance actually reaches the player and lands
//! as `heard`, not what proves any of those upstream pieces individually
//! (`windows/vessel/tests/suite/controller_swap.rs` and `testimony.rs`
//! already do).
//!
//! **What this file may NOT test, and where that moved (the final-fix
//! wave).** `ask()` cannot currently produce a [`Testimony::Withheld`] at
//! all: the drive it answers about is always the drive *currently winning*
//! arbitration, and — because arbitration is sticky in practice — that drive
//! is observed to sit at zero overrides, where `stance_for`'s first arm is
//! unconditionally `Forthcoming`. A test in this file that waits and then
//! hopes to catch a refusal therefore observes nothing, forever, while
//! reading like coverage. One such test (`a_refusal_writes_no_knowledge`)
//! shipped and was deleted here rather than left: measured at its own
//! parameters (seed 42, 24 × `!wait 30`) it took its "knowledge moved"
//! branch every time and asserted only `after.is_some()`, which cannot fail
//! because `Knowledge` is insert-only. The invariant it was reaching for —
//! *a refusal lands no `heard` entry* — is now pinned where the refusal can
//! actually be constructed: `render_testimony_withheld_lands_nothing_and_names_no_state`,
//! in `windows/vessel/src/session.rs`'s own `tests` module, beside the three
//! sibling arms.

use crate::body_fields::seed_42;
use hornvale_vessel::liveness::AffectLabel;
use hornvale_vessel::liveness::DriveKind;
use hornvale_vessel::stance::{Stance, stance_for};
use hornvale_vessel::testimony::{FeltStateWord, testify};
use hornvale_vessel::{PossessOpts, Session, Turn};

/// Every reachable doctrine prior, so an assertion below can say "under ANY
/// people's doctrine" rather than depending on which one seed 42's flagship
/// happens to belong to.
const ALL_PRIORS: [hornvale_vessel::doctrine::Openness; 3] = [
    hornvale_vessel::doctrine::Openness::Guarded,
    hornvale_vessel::doctrine::Openness::Wary,
    hornvale_vessel::doctrine::Openness::Open,
];

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

/// The Reticence, Task 5: refusal is SELECTIVE (spec section 5, H3), and
/// this is the VERB-level statement of it — the host answers plainly about
/// the drive it is currently pursuing while carrying an override record that
/// would make it anything but forthcoming on another drive.
///
/// **This test used to assert only `!text.is_empty()`, which is true on
/// every arm of `render_testimony` including the refusal**, so it could not
/// distinguish selective reticence from no reticence at all. The three
/// assertions that replace it are, measured on seed 42 at these exact
/// parameters (`overrides_of(Thirst)=0`, `record={Fatigue: 7, Hunger: 7}`):
///
/// 1. the drive the answer is ABOUT sits at zero overrides;
/// 2. some OTHER drive carries enough overrides that, under **every** one of
///    the three doctrine priors, `stance_for` would return something other
///    than [`Stance::Forthcoming`] — so the host is demonstrably not a host
///    with a clean record;
/// 3. it answered anyway, landing a real concept under `"{body}::feels"`.
///
/// FIRES WHEN: `ask` starts folding the whole override record (a max, a sum,
/// any non-topic drive) into its stance instead of reading the pursued
/// drive's own count — the host would then go quiet globally and (3) would
/// find no landed knowledge.
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

    // (1) The drive the answer is about.
    let topic = s
        .driven_affect_object()
        .expect("a driven body pursues some drive after 8 waits");
    assert_eq!(
        s.overrides_of(topic),
        0,
        "the pursued drive is observed to carry no override history \
         (arbitration is sticky — a drive that accumulates overrides is one \
         that keeps LOSING, and this one is winning); record was {record:?}"
    );

    // (2) A drive the host would NOT be plainly forthcoming about, under any
    // people's doctrine.
    let worst = record
        .values()
        .copied()
        .max()
        .expect("the record is non-empty");
    assert!(
        worst > 0,
        "H3 needs a genuinely overridden drive to exist; record was {record:?}"
    );
    for prior in ALL_PRIORS {
        assert_ne!(
            stance_for(prior, worst),
            Stance::Forthcoming,
            "at {worst} overrides a {prior:?} host must not be plainly forthcoming, \
             or this test is not contrasting anything; record was {record:?}"
        );
    }
    assert!(
        [
            DriveKind::Thirst,
            DriveKind::Thermal,
            DriveKind::Fatigue,
            DriveKind::Hunger,
            DriveKind::Danger,
            DriveKind::Social,
        ]
        .into_iter()
        .any(|d| s.overrides_of(d) == 0),
        "H3 needs at least one un-overridden drive to exist; record was {record:?}"
    );

    // (3) It answered anyway — selectively, on the axis it was not overridden on.
    let body_label = s.driven_body().label.clone();
    let key = format!("{body_label}::feels");
    let turn = s.handle("ask");
    let text = match turn {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("released: {t}"),
    };
    assert!(
        !text.contains("will not say"),
        "the host must not refuse about a drive it was never overridden on, got: {text}"
    );
    let heard = s.knowledge().0.get(&key).cloned();
    assert!(
        heard.is_some_and(|v| !v.is_empty()),
        "a forthcoming answer lands a real concept under {key:?}; got nothing, which \
         is what a GLOBAL (non-selective) reticence would produce. Turn was: {text}"
    );
}
