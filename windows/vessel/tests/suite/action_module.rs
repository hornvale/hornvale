//! The action layer is reachable as its own module, not through `liveness`.

use hornvale_kernel::room::Facet;
use hornvale_vessel::action::{Action, is_movement, plan_to_water};

#[test]
fn the_action_layer_has_its_own_module() {
    let here = Facet {
        face: 0,
        path: vec![0],
    };
    assert!(is_movement(&Action::MoveTo(here.clone())));
    assert!(!is_movement(&Action::Drink));
    // The planner is reachable here too: standing on the water, the plan is
    // the single `Drink` with no move before it.
    let plan = plan_to_water(&here, &here, 64, &std::collections::BTreeSet::new())
        .expect("standing on the water, a plan always exists");
    assert_eq!(plan, vec![Action::Drink]);
}

#[test]
fn every_action_variant_still_carries_a_concept_name() {
    // The Actants' contract survives the move: each variant answers to a
    // concept, and the roster stays exhaustive.
    for a in Action::all() {
        assert!(!a.concept_name().is_empty(), "{a:?} has no concept name");
    }
    assert!(Action::all().len() >= 5);
}

// ---------------------------------------------------------------------------
// THE THREE ACTS, ASSERTED APART (The Wicket, Task 8, spec §6b)
//
// Nathan's ruling is three distinctions, not one: **waiting** merely passes
// time; **resting** leaves the body conscious and watchful and restores some
// fatigue; **sleeping** renders it unconscious and restores more. Each gets its
// own test so a failure names WHICH distinction broke — a single test over all
// three would report "the acts differ" and leave the reader to find out how.
// ---------------------------------------------------------------------------

use hornvale_kernel::{ConceptRegistry, EntityId, Ledger, Lineage, TickSpan, WorldTime};
use hornvale_vessel::liveness::{
    RESTED, SLEPT, SleepTraits, fatigue_at, record_rest, record_sleep, renders_unconscious,
};

/// `fatigue_at`'s species argument (The Wicket, Task 9 — see
/// `tests/suite/fatigue_stock.rs`'s own doc for the full story; The Pallet,
/// Task 4 made it a struct): human's rows in
/// `hornvale_species::fatigue_rise_registry` and `sleep_grade_registry`, the
/// same `0.3` and `1.5` the old `FATIGUE_RISE` and `AFFORDED_REST_GAIN`
/// constants carried for every kind alike. Every call below also passes
/// `day: None` and `sites: None`, so the arithmetic is unchanged in shape
/// from before either task and `afforded_gain` is never consulted at all —
/// it is spelled out rather than left at some arbitrary literal so a reader
/// can see the calls are UNGRADED by their `sites` argument, not by a gain
/// quietly set to `1.0`.
///
/// `substrate` mirrors `hornvale_species::substrate_response`'s SURFACE curve
/// — the realm every kind absent from `habitat_realm_registry` carries — and
/// is never consulted either, for the same reason `afforded_gain` is not: a
/// bout with no `sites` argument is graded `Bare` before any surface is
/// looked at (The Tenon).
const TRAITS: SleepTraits = SleepTraits {
    rise: 0.3,
    afforded_gain: 1.5,
    substrate: hornvale_kernel::ConditionResponse {
        optimum: 0.0,
        width: 0.5,
        devotion: 1.0,
    },
};

/// A fresh ledger with one body in it, and a registry that knows both bout
/// predicates. `role` keeps the two bodies of the sleeping test distinguishable
/// in a failure dump.
fn bout_body(role: &'static str) -> (Ledger, EntityId, ConceptRegistry) {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(
            RESTED,
            false,
            "an agent rested on a day, for this many ticks",
        )
        .expect("a fresh registry accepts the predicate");
    registry
        .register_predicate(SLEPT, false, "an agent slept on a day, for this many ticks")
        .expect("a fresh registry accepts the predicate");
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity(Lineage {
        parent: None,
        role,
        ordinal: 0,
    });
    (ledger, e, registry)
}

fn at(days: f64) -> WorldTime {
    WorldTime::from_std_days(days).expect("a finite day")
}

fn span(days: f64) -> TickSpan {
    TickSpan::from_std_days(days).expect("a finite span")
}

/// **RESTING leaves the body conscious, and restores some fatigue.**
///
/// Two claims, and both halves are asserted because either alone would be
/// satisfied by the wrong act: an act that restored fatigue without leaving the
/// body conscious is a sleep, and an act that left it conscious without
/// restoring anything is a wait.
///
/// "Some" is asserted against the body's OWN debt at the moment it lay down —
/// the debt must have gone DOWN over the bout, not merely risen more slowly.
///
/// **THE FIRST VERSION OF THIS TEST WAS VACUOUS AND ITS MUTATION CAUGHT IT.**
/// It compared the resting body against an idle one read at the same later
/// instant, and asserted the rester owed less. That passes at
/// `REST_FALL = 0.0`: the fold treats a bout as an interval where no fatigue
/// ACCRUES, so a rest that repays nothing at all still leaves its body below an
/// idle one that spent the same quarter-day on its feet. The old construction
/// measured suppressed accrual and reported it as recovery. The lay-down
/// comparison below cannot: at `REST_FALL = 0.0` the debt at waking equals the
/// debt at lying down exactly, and the strict `<` reddens.
#[test]
fn resting_leaves_the_body_conscious_and_restores_some_fatigue() {
    assert!(
        !renders_unconscious(&Action::Rest),
        "a resting body is watchful — that is the whole distinction from Sleep"
    );

    let (mut ledger, e, registry) = bout_body("wicket-rester");
    // Two days awake from genesis, then a quarter-day rest, read at the moment
    // it gets up. Reading AT the wake instant is deliberate: it removes the
    // awake segment that would otherwise follow, so what the comparison sees is
    // the bout alone.
    let lay_down = at(2.0);
    let wakes = at(2.25);
    let owed_lying_down = fatigue_at(&ledger, e, lay_down, TRAITS, None, None);
    assert!(
        owed_lying_down > 0.0 && owed_lying_down < 1.0,
        "the fixture must sit strictly inside the clamps or a reduction cannot \
         be seen: {owed_lying_down}"
    );
    ledger
        .commit(record_rest(e, lay_down, span(0.25)), &registry)
        .expect("`rested` is non-functional");
    let after = fatigue_at(&ledger, e, wakes, TRAITS, None, None);
    assert!(
        after < owed_lying_down,
        "a rest must REPAY: the debt at waking must be strictly below the debt \
         the body carried when it lay down, not merely below an idle body's — \
         {after} against {owed_lying_down}"
    );
    assert!(
        after > 0.0,
        "and it must not clear the debt outright — that is the flag model, and \
         it is what makes sleep worth having: {after}"
    );
}

/// **SLEEPING renders the body unconscious, and restores strictly more than
/// resting FOR THE SAME SPAN.**
///
/// The span is held identical on purpose. Sleeping is also the longer act in
/// practice (`SLEEP_BOUT` against `REST_BOUT`), so a comparison that let the
/// spans differ would pass on duration alone and say nothing about the two
/// acts — it would still be green if both repaid at one rate. Holding the span
/// fixed leaves the RATE as the only thing that can differ, which is the claim.
///
/// This is the test that would have passed vacuously before Task 7: under the
/// flag model any bout zeroed the debt, so "both restore something" was true of
/// both acts and of a single act wearing two names. The strict inequality is
/// what needs a stock underneath it.
#[test]
fn sleeping_renders_the_body_unconscious_and_restores_strictly_more_than_resting() {
    assert!(
        renders_unconscious(&Action::Sleep),
        "a sleeping body is under — that is the whole distinction from Rest"
    );

    let bout = span(0.25);
    let lay_down = at(2.0);
    let wakes = at(2.25);

    let (mut rest_ledger, r, rest_reg) = bout_body("wicket-rester");
    rest_ledger
        .commit(record_rest(r, lay_down, bout), &rest_reg)
        .expect("`rested` is non-functional");
    let rested = fatigue_at(&rest_ledger, r, wakes, TRAITS, None, None);

    let (mut sleep_ledger, s, sleep_reg) = bout_body("wicket-sleeper");
    sleep_ledger
        .commit(record_sleep(s, lay_down, bout), &sleep_reg)
        .expect("`slept` is non-functional");
    let slept = fatigue_at(&sleep_ledger, s, wakes, TRAITS, None, None);

    assert!(
        slept > 0.0 && rested < 1.0,
        "neither reading may sit on a clamp, or the inequality below is an \
         artifact of the clamp rather than of the rates: slept={slept}, \
         rested={rested}"
    );
    assert!(
        slept < rested,
        "for the SAME span from the SAME instant, read at the SAME wake \
         moment, the body that slept must owe STRICTLY less than the body that \
         merely rested: slept={slept}, rested={rested}"
    );
}

/// **WAITING changes nothing about the body that time alone would not** —
/// The Wicket, Task 8, step 4.
///
/// `wait` is the one of the three acts this task deliberately did NOT touch: it
/// parses a span, advances the clock, runs the NPC tick, and has no `Action`
/// variant at all, which is the right shape for an act that transforms nothing.
/// This asserts that rather than trusting it, because "we changed nothing here"
/// is a claim about code that the split could easily have falsified — the body
/// runs its own arbitration inside a `wait`, and an act it chose there would be
/// committed under the possession's own subject.
///
/// The discriminating assertion is the third: fatigue at the post-wait instant,
/// read off the POST-wait ledger, must be BIT-identical to what the PRE-wait
/// ledger already predicted for that same instant. If waiting had put the body
/// down for so much as a tick, the two folds would differ — and the check is
/// exact rather than approximate, because there is no tolerance at which a
/// spurious bout would be acceptable.
#[test]
fn waiting_changes_nothing_about_the_body_that_time_alone_would_not() {
    use hornvale_vessel::{PossessOpts, Session};

    let world = crate::common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    let body = session.agent_entity();

    let before_day = session.day();
    let before: Ledger =
        serde_json::from_str(&session.session_ledger_json()).expect("a ledger round-trips");
    let bouts = |l: &Ledger| l.facts_of(body, RESTED).count() + l.facts_of(body, SLEPT).count();
    let bouts_before = bouts(&before);

    session.handle("wait");

    let after_day = session.day();
    let after: Ledger =
        serde_json::from_str(&session.session_ledger_json()).expect("a ledger round-trips");

    assert!(
        after_day > before_day,
        "a wait moves the clock — {before_day:?} to {after_day:?}"
    );
    assert_eq!(
        bouts(&after),
        bouts_before,
        "waiting is not a recovery act: it may commit no `rested` and no \
         `slept` for the possessed body"
    );

    let predicted = fatigue_at(&before, body, after_day, TRAITS, None, None);
    let actual = fatigue_at(&after, body, after_day, TRAITS, None, None);
    assert_eq!(
        actual.to_bits(),
        predicted.to_bits(),
        "the fatigue after a wait must be exactly what the pre-wait ledger \
         already predicted for that instant: {actual} against {predicted}"
    );

    // Anti-vacuity in both directions. Time really did accrue (so the equality
    // above is not two identical zeroes), and the reading is off a clamp (so it
    // is not two identical ceilings either).
    let started_at = fatigue_at(&after, body, before_day, TRAITS, None, None);
    assert!(
        actual > started_at,
        "and time alone DOES accrue fatigue, or this test compares a constant \
         with itself: {actual} against {started_at} a moment earlier"
    );
    assert!(
        actual < 1.0,
        "the reading must be off the ceiling, or the equality above holds \
         because both sides clamped: {actual}"
    );
}
