//! Fatigue is a recovery STOCK, not a flag any rest clears (The Wicket,
//! Task 7) — the six properties P1-P6 the task froze before the model was
//! written.
//!
//! **What the old model was.** `fatigue_at` was
//! `FATIGUE_RISE * (t - last_rested)` clamped: time since the most recent
//! `rested` fact. Any rest, of any length, zeroed the debt, and nothing in a
//! world recorded how much sleep a body actually got. "Rest gives some benefit,
//! sleep gives more" is not a sentence that model can say — the two acts would
//! differ only in their prose.
//!
//! **What it is now.** Fatigue rises at `FATIGUE_RISE` per day awake and falls
//! at `FATIGUE_FALL` per day asleep, folded along the creature's own timeline
//! of `rested` facts, each of which carries the span the body was down for.
//! Both constants are private to `liveness`, so every assertion here is
//! written in terms the public surface can see — ordering, monotonicity and
//! bounds — never a hard-coded constant this file would have to chase.
//!
//! **P5 is deliberately not here.** "The read and the mover agree exactly" is
//! a claim about `decide_step`, which is `pub(crate)`, and the task named its
//! home: `liveness.rs`'s own
//! `a_fatigue_read_matches_the_walks_own_fatigue_arithmetic`, extended rather
//! than duplicated. Writing a second, weaker version of it out here would give
//! a false impression of coverage — the interesting half is bit-identity, and
//! this file cannot reach the mover to test it.
//!
//! **The rate and the day length are now explicit arguments (The Wicket,
//! Task 9).** `fatigue_at` no longer has an implicit constant rate: the
//! production rate is per-species, resolved from `hornvale_species::
//! fatigue_rise_registry`, and the accrual now converts against the world's
//! LOCAL day rather than the kernel's standard one. Every call below passes
//! [`RATE`] (human's row, unchanged by Task 9) and `day: None` — the
//! tidally-locked/no-calendar convention `windows/vessel::clock::
//! ticks_per_local_day` documents, which is also what a bare `Ledger` with no
//! world behind it actually is here — so `to_local_days` reduces to exactly
//! `TickSpan::as_std_days` and every property below is unchanged in shape
//! from before this task; only the call signature grew.
//!
//! **The site grade is a sixth argument, and P1-P6 pass `None` (The Wicket,
//! Task 10).** A bout is now graded by what the room it was taken in offered
//! the body that took it, which needs a terrain and a body neither a bare
//! `Ledger` nor any of the six properties below has. `None` means UNGRADED —
//! every bout reads as bare ground, which is exactly the arithmetic P1-P6
//! were frozen against — so those six are unchanged in meaning as well as in
//! shape. P7 below is the one property that supplies a world, and it pins
//! that bare ground and `None` agree BIT for bit, so the two readings of
//! "ungraded" cannot drift apart.

use hornvale_kernel::{
    ConceptRegistry, ConditionResponse, EntityId, Facet, Ledger, Lineage, ResourceVector, TickSpan,
    WorldTime,
};
use hornvale_vessel::affordance::{OfferedVerb, offered_to};
use hornvale_vessel::body::Body;
use hornvale_vessel::interior::interior_of;
use hornvale_vessel::liveness::{
    AGENT_AT, RESTED, RestSites, SLEPT, Terrain, ThreatNiche, fatigue_at, place_agent, record_rest,
    record_sleep,
};

/// The rate every fixture below folds against — human's row in
/// `hornvale_species::fatigue_rise_registry`, and the same `0.3` the old
/// `FATIGUE_RISE` constant carried before Task 9 moved it into that table.
/// This file asserts ordering/monotonicity/bounds, never the constant's
/// magnitude, so its exact value is not load-bearing here — only that it is
/// shared and nonzero.
const RATE: f64 = 0.3;

/// A fresh ledger with one entity in it, the entity, and a registry that knows
/// the one predicate this file writes.
fn body() -> (Ledger, EntityId, ConceptRegistry) {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(
            RESTED,
            false,
            "an agent rested on a day, for this many ticks",
        )
        .expect("a fresh registry accepts the predicate");
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity(Lineage {
        parent: None,
        role: "fatigue-stock-subject",
        ordinal: 0,
    });
    (ledger, e, registry)
}

/// Rests go through `Ledger::commit`, never a back door: the whole point of P1
/// is that fatigue is a fold over COMMITTED facts, and commit is also where a
/// numeric object meets the quantizer (decision 0033) — a span that could not
/// survive that round trip would be the wrong carrier, and this file would be
/// the last place to notice.
fn push_rest(ledger: &mut Ledger, registry: &ConceptRegistry, e: EntityId, start: f64, span: f64) {
    let day = WorldTime::from_std_days(start).expect("a finite day");
    let span = TickSpan::from_std_days(span).expect("a finite span");
    ledger
        .commit(record_rest(e, day, span), registry)
        .expect("`rested` is non-functional, so a rest never contradicts one");
}

fn at(d: f64) -> WorldTime {
    WorldTime::from_std_days(d).expect("a finite day")
}

/// P1 — fatigue is still a pure FOLD over committed facts.
///
/// Two claims, and they are different. The first is that the read holds no
/// state: asking twice at the same instant gives the same answer, and asking
/// out of chronological order does not disturb it — a memo or an accumulator
/// hiding behind the function would show up here. The second is that a world
/// SURVIVES a round trip: serialize the ledger, read it back, and the fatigue
/// is bit-identical. That is the property that would break if a rest's span
/// lived anywhere but in the ledger, which is exactly the temptation a stock
/// creates (the old model kept a `last_rested` field in the walk; this one
/// deliberately keeps nothing).
/// NO MUTATION MARKER, AND THE ABSENCE IS ITSELF THE FINDING (fix round 1,
/// Minor 4). Eight mutations to `liveness.rs` were run against this file while
/// authoring the markers below — drop the committed `d <= t` filter; remove the
/// `[0, 1]` floor; remove the ceiling; credit a bout's whole span the moment
/// the fact exists; revert `to_local_days` to `as_std_days`; make recovery a
/// flat constant; fold only the most recent bout; make any bout zero the debt —
/// and **this test stayed green under every one of them.** That is not a defect
/// in it: both sides of both its assertions move together under any change to
/// the arithmetic, because it compares the fold against ITSELF. It is a guard
/// against a class of implementation — hidden state, or a span that does not
/// survive serialization — neither of which is expressible as a one-line edit
/// to the module. Real, and not coverage of what fatigue computes; do not read
/// a green P1 as evidence about the numbers.
#[test]
fn p1_fatigue_is_a_pure_fold_over_committed_facts() {
    let (mut ledger, e, reg) = body();
    push_rest(&mut ledger, &reg, e, 1.0, 0.4);
    push_rest(&mut ledger, &reg, e, 2.5, 0.2);
    push_rest(&mut ledger, &reg, e, 4.0, 0.5);

    // Stateless: repeated and out-of-order reads agree with themselves.
    let probes = [0.5, 6.0, 1.2, 3.0, 4.25, 2.0];
    let first: Vec<u64> = probes
        .iter()
        .map(|&d| fatigue_at(&ledger, e, at(d), RATE, None, None).to_bits())
        .collect();
    for _ in 0..3 {
        let again: Vec<u64> = probes
            .iter()
            .rev()
            .map(|&d| fatigue_at(&ledger, e, at(d), RATE, None, None).to_bits())
            .collect();
        let again: Vec<u64> = again.into_iter().rev().collect();
        assert_eq!(
            first, again,
            "fatigue must be a function of (ledger, entity, t) alone"
        );
    }

    // Survives a reload: the whole model lives in the committed facts.
    let json = serde_json::to_string(&ledger).expect("a ledger serializes");
    let reloaded: Ledger = serde_json::from_str(&json).expect("and deserializes");
    for &d in &probes {
        assert_eq!(
            fatigue_at(&ledger, e, at(d), RATE, None, None).to_bits(),
            fatigue_at(&reloaded, e, at(d), RATE, None, None).to_bits(),
            "fatigue at day {d} must survive a save/load round trip"
        );
    }
}

/// P2 — fatigue still ranges `[0, 1]` and still rises with time awake.
///
/// The clamp is the half that a stock could plausibly lose: an unbounded
/// integral would sail past 1 for a creature awake long enough, and the drive
/// stack's thresholds (`FATIGUE_ACT`, `FATIGUE_CEIL`) are all expressed against
/// a `[0, 1]` urgency.
/// MUTATION THIS MUST FAIL AGAINST: delete the `.min(1.0)` ceiling from
/// `fatigue_from_rests`'s trailing awake term. Red observed:
///
/// ```text
/// thread 'fatigue_stock::p2_fatigue_stays_in_range_and_rises_while_awake'
/// panicked: fatigue must stay in [0, 1] ...
/// ```
#[test]
fn p2_fatigue_stays_in_range_and_rises_while_awake() {
    let (ledger, e, _reg) = body();
    // No rest at all: a monotone ramp from genesis, saturating at 1.
    let mut previous = f64::NEG_INFINITY;
    for step in 0..60 {
        let d = step as f64 * 0.25;
        let f = fatigue_at(&ledger, e, at(d), RATE, None, None);
        assert!(
            (0.0..=1.0).contains(&f),
            "fatigue left [0, 1] at day {d}: {f}"
        );
        assert!(
            f >= previous,
            "fatigue must not fall while the body is awake: day {d} gave {f} \
             after {previous}"
        );
        previous = f;
    }
    assert!(
        fatigue_at(&ledger, e, at(0.0), RATE, None, None)
            < fatigue_at(&ledger, e, at(1.0), RATE, None, None),
        "a day awake must cost something"
    );
    assert_eq!(
        fatigue_at(&ledger, e, at(1000.0), RATE, None, None),
        1.0,
        "and the ceiling must hold however long it stays up"
    );
}

/// P3 — A SHORT REST DOES NOT ZERO THE DEBT. **The discriminating property.**
///
/// This is the one the old model could not express, and the one that makes the
/// act split in Task 8 mean anything. Its mutation is stated in the task brief
/// and was run: reverting `fatigue_at` to the old
/// `(FATIGUE_RISE * (t - last_rested)).clamp(0.0, 1.0)` shape reddens this
/// test, because under that shape a body five days awake reads fatigue 0 the
/// instant it takes a six-minute nap. If this test ever passes under that
/// mutation, it is not testing what it claims.
///
/// Two assertions, and the second exists because of the first. After a short
/// rest the debt must still be LARGE — not merely nonzero, but still over half,
/// so a model that subtracts an epsilon would not satisfy it. That alone would
/// be satisfied by a model that ignored the rest entirely, which is the
/// opposite failure and the cheapest way to make the first line pass; so the
/// second assertion requires the nap to have moved the number at all. P6 covers
/// the floor.
/// MUTATION THIS MUST FAIL AGAINST, and it is the old model expressed as one
/// line rather than as a whole reverted function: replace the fall term with
/// `(fatigue - 1000.0).max(0.0)`, so ANY bout of any length clears the debt
/// outright. That is exactly the behaviour the pre-Task-7 flag had. Red
/// observed (alongside `p4`, `p7`, `a_rest_in_progress` and `a_full_cycle`,
/// which is the right blast radius for reverting the whole model).
#[test]
fn p3_a_short_rest_does_not_zero_the_debt() {
    // Five days awake, then a rest of one twentieth of a day (the finest jump
    // `next_awake_day`'s scan can produce: a real, if minimal, nap).
    let (mut ledger, e, reg) = body();
    let exhausted = fatigue_at(&ledger, e, at(5.0), RATE, None, None);
    assert_eq!(exhausted, 1.0, "five days awake is a saturated debt");
    push_rest(&mut ledger, &reg, e, 5.0, 0.05);
    let after_nap = fatigue_at(&ledger, e, at(5.05), RATE, None, None);

    assert!(
        after_nap > 0.5,
        "a nap must leave an exhausted body still deeply in debt; the old \
         model read exactly 0 here, which is the whole reason this task \
         exists. Got {after_nap}"
    );
    assert!(
        after_nap < exhausted,
        "but the nap must actually have done something: {after_nap} vs \
         {exhausted}"
    );
}

/// P4 — a LONGER rest restores strictly more than a shorter one.
///
/// The monotonicity that gives "rest a little, sleep a lot" its meaning.
///
/// **READ AT THE WAKE INSTANT, and that is the whole design of this test.** The
/// obvious construction — same rest start, same later read instant, longer
/// spans — is VACUOUS, and it was written that way first. A longer rest ending
/// later also leaves LESS TIME AWAKE before the read, so fatigue falls with
/// span whether or not recovery depends on span at all: a mutation replacing
/// `FATIGUE_FALL * (time asleep)` with a flat `FATIGUE_FALL * 0.1` left that
/// version passing. Reading each body at the moment it wakes removes the awake
/// segment entirely — every body accrued the same debt over the same two days
/// and differs only in how much sleep it then got — so the comparison is
/// recovery against recovery and nothing else. The flat-recovery mutation
/// reddens it.
/// MUTATION THIS MUST FAIL AGAINST: replace the span-proportional fall term
/// with a flat one — `(fatigue - kind.fall() * site.gain() * 0.1).max(0.0)` —
/// so every bout repays the same amount whatever its length. Red observed
/// (with `a_rest_in_progress`, and nothing else).
#[test]
fn p4_a_longer_rest_restores_strictly_more() {
    let mut previous = f64::INFINITY;
    // Day 3 rather than day 2: the debt has to be deep enough that even the
    // longest rest in the ladder does not repay all of it, or the last rung
    // lands on the floor clamp and compares nothing (the assertion after the
    // loop is what caught that, at day 2).
    for span in [0.05_f64, 0.1, 0.2, 0.4, 0.8] {
        let (mut ledger, e, reg) = body();
        push_rest(&mut ledger, &reg, e, 3.0, span);
        let f = fatigue_at(&ledger, e, at(3.0 + span), RATE, None, None);
        assert!(
            f < previous,
            "a {span}-day rest must leave STRICTLY less debt at the moment it \
             ends than the shorter one before it: got {f} after {previous}"
        );
        previous = f;
    }
    assert!(
        previous > 0.0,
        "the ladder must not bottom out on the clamp, or the last comparison \
         proved nothing"
    );
}

/// P4, second half — recovery accumulates across separate rests.
///
/// A stock has memory: two nights are not one night wasted. This is the
/// property that distinguishes the model from one that only ever looks at the
/// MOST RECENT rest — the old flag with a duration bolted on, and the most
/// likely wrong turn.
///
/// **The fixture is shaped so the wrong model overshoots rather than ties.** An
/// earlier version put the two rests a day apart and read a day after the
/// second; under a most-recent-only mutation both bodies landed on the same
/// value to within a few ULPs, and the strict `<` passed by luck. Here the
/// second rest is a short nap taken just before the read, so a model that
/// forgets the FIRST rest must also forget a day and a half of repayment and
/// reads clearly HIGHER than the single-rest body, not equal to it. The
/// `rests.iter().rev().take(1)` mutation reddens it.
/// MUTATION THIS MUST FAIL AGAINST: fold only the most recent bout —
/// `for &(start, span, kind, site) in rests.iter().rev().take(1)` — the "flag
/// with a duration bolted on" this test exists to refuse. Red observed, and it
/// is the ONLY test in this file that reddens under it.
#[test]
fn p4b_two_rests_restore_more_than_either_alone() {
    let read_at = at(3.0);
    let (mut one, e1, reg1) = body();
    push_rest(&mut one, &reg1, e1, 1.0, 0.2);
    let (mut two, e2, reg2) = body();
    push_rest(&mut two, &reg2, e2, 1.0, 0.2);
    push_rest(&mut two, &reg2, e2, 2.9, 0.05);
    assert!(
        fatigue_at(&one, e1, read_at, RATE, None, None) < 1.0
            && fatigue_at(&two, e2, read_at, RATE, None, None) > 0.0,
        "both bodies must sit strictly inside the clamps for this comparison \
         to mean anything: {} and {}",
        fatigue_at(&one, e1, read_at, RATE, None, None),
        fatigue_at(&two, e2, read_at, RATE, None, None)
    );
    assert!(
        fatigue_at(&two, e2, read_at, RATE, None, None)
            < fatigue_at(&one, e1, read_at, RATE, None, None),
        "the second rest must count ON TOP of the first: {} vs {}",
        fatigue_at(&two, e2, read_at, RATE, None, None),
        fatigue_at(&one, e1, read_at, RATE, None, None)
    );
}

/// P6 — fatigue never goes negative however much a body sleeps.
///
/// The floor is not decoration: a stock that could bank credit would let a
/// creature sleep for a week and then stay awake for a fortnight without ever
/// crossing `FATIGUE_ACT`, which is a behaviour nobody chose. Probed three
/// ways — one absurdly long rest, many rests in a row, and a rest that is still
/// running when the read happens.
/// MUTATION THIS MUST FAIL AGAINST: delete the `.max(0.0)` floor from
/// `fatigue_from_rests`'s fall term, so a long bout banks credit below zero.
/// Red observed, and it is the ONLY test in this file that reddens under it.
#[test]
fn p6_fatigue_never_goes_negative() {
    let (mut ledger, e, reg) = body();
    push_rest(&mut ledger, &reg, e, 0.5, 40.0);
    for d in [0.5, 1.0, 10.0, 40.0, 40.5, 41.0] {
        let f = fatigue_at(&ledger, e, at(d), RATE, None, None);
        assert!(f >= 0.0, "a 40-day sleep drove fatigue to {f} at day {d}");
    }
    let (mut many, e2, reg2) = body();
    for n in 0..50 {
        push_rest(&mut many, &reg2, e2, n as f64 * 0.5, 0.45);
    }
    for step in 0..60 {
        let d = step as f64 * 0.5;
        let f = fatigue_at(&many, e2, at(d), RATE, None, None);
        assert!(
            (0.0..=1.0).contains(&f),
            "fifty back-to-back rests left fatigue at {f} on day {d}"
        );
    }
}

/// A rest that has BEGUN but not finished credits only the sleep already had.
///
/// Not one of P1-P6, and it is here because it is the property that makes P4's
/// "read after both woke" caveat necessary — and because the alternative
/// (crediting the whole span the moment the fact exists) would let a body wake
/// refreshed from a sleep it has not had yet, which is a stock that runs
/// backwards in time.
/// MUTATION THIS MUST FAIL AGAINST: `let woke = end.min(t)` becomes
/// `let woke = end`, crediting a bout's whole span the moment its fact exists.
/// Red observed, and it is the ONLY test in this file that reddens under it —
/// which is what makes it worth keeping beside P1-P6 rather than folding into
/// P4.
#[test]
fn a_rest_in_progress_credits_only_the_sleep_already_had() {
    let (mut ledger, e, reg) = body();
    push_rest(&mut ledger, &reg, e, 2.0, 1.0);
    let start = fatigue_at(&ledger, e, at(2.0), RATE, None, None);
    let quarter = fatigue_at(&ledger, e, at(2.25), RATE, None, None);
    let half = fatigue_at(&ledger, e, at(2.5), RATE, None, None);
    let done = fatigue_at(&ledger, e, at(3.0), RATE, None, None);
    assert!(
        start > quarter && quarter > half && half >= done,
        "fatigue must fall monotonically THROUGH a rest, not jump at its \
         start: {start} {quarter} {half} {done}"
    );
    assert!(
        start > 0.0,
        "and lying down is not itself rest — the debt at the instant sleep \
         begins is whatever the waking day built"
    );
}

/// **THE DISCRIMINATING TEST (The Wicket, Task 9).** Two worlds whose day
/// lengths differ, the same species (so the same rate), and the same elapsed
/// STANDARD time must accrue DIFFERENT sleep debt — and the ratio between
/// them must be exactly the ratio of their day lengths.
///
/// **This is the test the old model could not pass and would not have been
/// written for.** `FATIGUE_RISE` used to multiply `TickSpan::as_std_days()`,
/// so under that shape the two worlds below would read IDENTICAL fatigue —
/// the day length never entered the computation, only the kernel's own fixed
/// 100,000-ticks-per-day lattice did. `to_local_days` (`liveness.rs`) is what
/// replaces it, and the mutation this test is named for is exactly that
/// revert: swap `to_local_days(span, day)` back for `span.as_std_days()` in
/// `fatigue_from_rests`'s two RISE terms, and `debt_short`/`debt_long` below
/// become bit-identical — this test reddens on both the ratio assertion and
/// the "not equal" one.
///
/// No rests are pushed, so this is a pure ramp from genesis: `fatigue_from_
/// rests` reduces to exactly `RATE * to_local_days(t - GENESIS, day)`, clamped
/// (never reached here — see the strictly-inside-the-clamps assertion below).
/// That makes the predicted numbers exact, not merely ordered.
///
/// **What this test does NOT witness (fix round 1 review).** `day` here is a
/// synthetic `TickSpan` handed straight to `fatigue_at` — this file never
/// touches `LocaleTerrain`, `Calendar`, or a real world, so this test would
/// stay GREEN even if `Terrain::day_ticks` never reached a real calendar at
/// all (a seam bug entirely upstream of this fold). The reviewer checked
/// that seam separately: mutating `LocaleTerrain::day_ticks` to always
/// return `None` reddens
/// `session_snapshot::the_client_fixtures_are_current`, so the committed
/// `snapshot-seed-0-chamber-occupied.json` golden — which reads as ordinary
/// artifact drift — is this task's real witness that the calendar reaches
/// the fold in production, not this test.
/// MUTATION THIS MUST FAIL AGAINST: the revert the paragraph above names,
/// applied at the one place both terms cross —
/// `fn to_local_days(span, day) { span.as_std_days() }`. Red observed, here and
/// in `a_full_cycle_lands_at_the_same_debt_regardless_of_the_local_day_length`,
/// and in nothing else.
#[test]
fn the_debt_scales_with_the_worlds_own_local_day_not_the_standard_one() {
    let (ledger, e, _reg) = body();
    // The elapsed STANDARD time both worlds share: exactly 200,000 kernel
    // ticks (two standard days) since genesis — the SAME instant on the
    // SAME kernel lattice, read against two different planets' calendars.
    let t = WorldTime::from_ticks(200_000);

    // World A: an Earth-like local day (100,000 ticks = one standard day).
    // `to_local_days` here is exactly `TickSpan::as_std_days`, so this is
    // also what the pre-Task-9 arithmetic would have answered for BOTH
    // worlds — the common case Task 9 must leave unmoved.
    let short_day = TickSpan::from_ticks(100_000);
    // World B: a local day four times as long. The same creature, the same
    // elapsed standard time, but a quarter as many local days have actually
    // turned — so it must owe a quarter of the debt.
    let long_day = TickSpan::from_ticks(400_000);

    let debt_short = fatigue_at(&ledger, e, t, RATE, Some(short_day), None);
    let debt_long = fatigue_at(&ledger, e, t, RATE, Some(long_day), None);

    assert!(
        debt_short > 0.0 && debt_short < 1.0 && debt_long > 0.0 && debt_long < 1.0,
        "both readings must sit strictly inside the clamps or the ratio below \
         proves nothing: short={debt_short}, long={debt_long}"
    );
    // The exact predicted values: RATE * 2.0 std days over a 1-std-day local
    // day, and RATE * 0.5 std days over a 4-std-day local day.
    assert!(
        (debt_short - RATE * 2.0).abs() < 1e-12,
        "the short (Earth-like) day world must read exactly RATE * 2.0: got \
         {debt_short}"
    );
    assert!(
        (debt_long - RATE * 0.5).abs() < 1e-12,
        "the long (4x) day world must read exactly RATE * 0.5: got {debt_long}"
    );
    assert!(
        (debt_short - 4.0 * debt_long).abs() < 1e-9,
        "the short-day world's debt must be exactly 4x the long-day world's \
         — the ratio of their day lengths — for the SAME elapsed standard \
         time: short={debt_short}, long={debt_long}"
    );
    assert_ne!(
        debt_short, debt_long,
        "under the old `as_std_days()` conversion these two worlds would \
         read IDENTICAL fatigue; discriminating on the local day length is \
         the whole point of this task"
    );
}

/// **THE FALL-TERM INVARIANT (fix round 1, Important 2).** A full accrue-
/// and-recover cycle — 0.9 of a LOCAL day awake, then 0.1 of a LOCAL day
/// spent resting — must land at the SAME net fatigue regardless of how long
/// the local day actually is in standard days, once both the RISE and the
/// FALL terms convert through `to_local_days`.
///
/// **Why 0.9/0.1 and not a 0.5/0.5 "normal night".** Tried first, and it is
/// vacuous: `RATE * 0.5 = 0.15` accrued against `REST_FALL * 0.5 = 0.25`
/// repaid clamps to EXACTLY `0.0` on both worlds, so the equality holds for a
/// reason that has nothing to do with `L`-invariance — the clamp, not the
/// arithmetic, is doing the agreeing. An unbalanced cycle (`RATE * 0.9 =
/// 0.27` accrued against `REST_FALL * 0.1 = 0.05` repaid, net `0.22`) lands
/// strictly inside both clamps, so the equality below is a real property of
/// the fold.
///
/// **This is the test that would have caught the rotation-dependent
/// recovery bug fix round 1 found.** The first cut of this task left the
/// fall terms on `TickSpan::as_std_days()`, reasoned as a scope boundary
/// (`FATIGUE_FALL`/`REST_FALL` are not named by Nathan's ruling). That
/// reasoning missed that a sleep bout is not a standard-day span: the walk's
/// own `act_span` runs a sleep to roughly half a LOCAL day. With `L` the
/// local day in standard days, the waking phase accrues `RATE * 0.9`
/// (`L`-invariant, once RISE converts) while an unconverted fall term repays
/// `REST_FALL * 0.1 * L` — a recovery rate that SCALES with `L` while the
/// accrual it must outpace does not. Break-even was at `L = 0.3` standard
/// days (7.2 h); `RotationPin::PeriodHours` admits 4-100 h, so a legal
/// `--day-hours 4` world could accrue faster than it could ever recover.
/// Converting both terms restores the fixed margin at every `L` — the
/// property the pre-Task-9 model had for free, because both terms carried
/// the SAME (kernel) day.
///
/// Two worlds with very different local days — Earth-like (1 standard day)
/// and 20x longer — the same species, one full cycle each, read at the
/// moment the body wakes. Name the mutation: reverting the fall term's
/// `to_local_days` back to `as_std_days` reddens this — the long-day world
/// repays far more than the short-day one for the identical FRACTION (0.1)
/// of a local day spent resting, because an unconverted fall term repays
/// per STANDARD day, and 0.1 of a 20-standard-day local day is a much bigger
/// standard-day span than 0.1 of a 1-standard-day one.
/// MUTATION THIS MUST FAIL AGAINST: `fn to_local_days(span, day) {
/// span.as_std_days() }` — the same revert
/// `the_debt_scales_with_the_worlds_own_local_day_not_the_standard_one` names,
/// and the two are complementary: that one pins that the debt MOVES with the
/// day length, this one that a full accrue-and-recover cycle does NOT, which
/// is the property the fall terms' own conversion buys.
#[test]
fn a_full_cycle_lands_at_the_same_debt_regardless_of_the_local_day_length() {
    // Earth-like: one standard day. 20x longer: twenty standard days — "very
    // different" per the brief. This is the FOLD's own invariant, not a
    // claim about which worlds worldgen can mint, so pin legality is beside
    // the point.
    let short_day = TickSpan::from_ticks(100_000);
    let long_day = TickSpan::from_ticks(2_000_000);

    let debt_short = one_full_cycle(short_day);
    let debt_long = one_full_cycle(long_day);

    assert!(
        debt_short > 0.0 && debt_short < 1.0 && debt_long > 0.0 && debt_long < 1.0,
        "both readings must sit strictly inside the clamps or the comparison \
         below proves nothing: short={debt_short}, long={debt_long}"
    );
    assert!(
        (debt_short - debt_long).abs() < 1e-9,
        "a full accrue-and-recover cycle (0.9 of a LOCAL day awake, 0.1 \
         resting) must land at the SAME net debt on both worlds — the rise \
         and fall terms must scale with the SAME local day: \
         short={debt_short} (day={short_day:?}), long={debt_long} \
         (day={long_day:?})"
    );
}

/// One full accrue-and-recover cycle on a world whose local day is
/// `local_day`: 0.9 of a local day awake from genesis, then a `rested` bout
/// spanning the remaining 0.1, read at the moment the body wakes. Ticks are
/// derived directly from `local_day` (never through a standard-day float),
/// so the fractions are exact on every `local_day` this is called with.
fn one_full_cycle(local_day: TickSpan) -> f64 {
    let (mut ledger, e, reg) = body();
    let lay_down_ticks = local_day.ticks() * 9 / 10;
    let span_ticks = local_day.ticks() - lay_down_ticks;
    let lay_down = WorldTime::from_ticks(lay_down_ticks);
    let span = TickSpan::from_ticks(span_ticks);
    ledger
        .commit(record_rest(e, lay_down, span), &reg)
        .expect("`rested` is non-functional");
    let woke = lay_down + span;
    fatigue_at(&ledger, e, woke, RATE, Some(local_day), None)
}

// --- P7: the room grades the bout (The Wicket, Task 10) ------------------

/// A world of exactly two rooms, told apart by `is_built` alone.
///
/// `interior_of` reads exactly two things — `is_built` and `is_cold` — so a
/// terrain that answers them is a complete fixture for the composition that
/// decides what a room offers, and a real world would cost a full genesis to
/// answer two booleans. Everything is COLD (`-20.0`, well under
/// `FURNISHING_COLD_C`), so `furnished` composes the built+cold locale
/// vocabulary — which is the one that draws `the-fireside-bed` — while the
/// other room, unbuilt, composes wilderness and draws no bed at all.
struct OneFurnishedRoom {
    /// The one room this terrain calls built.
    furnished: Facet,
}

impl Terrain for OneFurnishedRoom {
    fn elevation(&self, _room: &Facet) -> f64 {
        0.0
    }
    fn is_fresh_water(&self, _room: &Facet) -> bool {
        false
    }
    fn temperature(&self, _room: &Facet, _day: WorldTime) -> f64 {
        -20.0
    }
    fn is_built(&self, room: &Facet) -> bool {
        *room == self.furnished
    }
}

/// A `Body` fixture varying nothing that matters here, assembled the same way
/// `tests/suite/affordance.rs`'s own `body_with_mass` is — locally, from
/// public constructors, because `liveness.rs`'s test bodies are private to
/// that module. `mass_kg` is the reference mass, well under the ceiling
/// `affordance::body_can_use` applies to `SupportsRest`, so the body-relative
/// half of the offer passes and the ROOM is the only thing varying.
fn resting_body(entity: EntityId, home: Facet) -> Body {
    Body {
        entity,
        home: home.clone(),
        resource: home,
        species: "human".into(),
        activity: hornvale_species::ActivityCycle::Diurnal,
        temperature_niche: ConditionResponse {
            optimum: 15.0,
            width: 10.0,
            devotion: 0.5,
        },
        deliberation_latency: 0.5,
        time_horizon: 0.0,
        thermal_strategy: hornvale_species::ThermalStrategy::Endothermic,
        niche: ResourceVector::new(&[]).expect("the empty niche is valid"),
        boldness: 0.5,
        threat_niche: ThreatNiche {
            uncanny: 1.0,
            heat: 0.0,
            cold: 0.0,
            predator: 0.5,
        },
        mass_kg: 70.0,
        label: "rest-site-subject".into(),
        perception: hornvale_species::PerceptionVector::MANIKIN,
        village: None,
    }
}

/// Does any anchor in `room` offer this body somewhere to sleep? The test's
/// own reading of the fixture, written out here rather than borrowed from
/// `liveness`, so the assertions below are checking that the two rooms
/// genuinely differ in the way the task claims — the POSITIVE CONTROL for
/// every comparison in `p7`. It pins nothing about the fold: a
/// `room_affords_rest` that always answered `false` would leave this check
/// green and redden the fatigue comparison, which is the direction that
/// matters.
fn room_offers_sleep(room: &Facet, body: &Body, terrain: &dyn Terrain) -> bool {
    let interior = interior_of(room, terrain);
    interior
        .ids()
        .iter()
        .any(|&a| offered_to(interior.anchor(a).kind, body).contains(&OfferedVerb::Sleep))
}

/// A ledger with the three predicates a graded bout needs registered, and two
/// entities: one that will bed down in the furnished room, one in the road.
fn two_bodies() -> (Ledger, EntityId, EntityId, ConceptRegistry) {
    let mut registry = ConceptRegistry::default();
    for (p, doc) in [
        (AGENT_AT, "an agent's room position on a day"),
        (RESTED, "an agent rested on a day, for this many ticks"),
        (SLEPT, "an agent slept on a day, for this many ticks"),
    ] {
        registry
            .register_predicate(p, false, doc)
            .expect("a fresh registry accepts the predicate");
    }
    let mut ledger = Ledger::default();
    let mut mint = |ordinal| {
        ledger.mint_entity(Lineage {
            parent: None,
            role: "rest-site-subject",
            ordinal,
        })
    };
    let bedded = mint(0);
    let roadside = mint(1);
    (ledger, bedded, roadside, registry)
}

/// P7 — **the same species, the same span, two rooms: the one with a
/// `SupportsRest` anchor present restores strictly more** (The Wicket, Task
/// 10; spec §6a's object grade, Nathan's ruling that a creature must be able
/// to pass out in the road and prefer a bed where it can get one).
///
/// Asserted for BOTH acts, because the grade multiplies the act's own rate
/// rather than replacing it: a graded conscious rest must still repay less
/// than a graded sleep would, and neither act may be the only one the room
/// reaches.
///
/// Four things are pinned here, in the order they are asserted:
///
/// 1. the fixture's two rooms really do differ in what they offer
///    (`room_offers_sleep`) — without this the comparison could be measuring
///    anything;
/// 2. both readings sit strictly inside the `[0, 1]` clamp, so "strictly
///    less" is a real inequality and not two saturated values;
/// 3. the roadside body's debt is BIT-identical to the ungraded (`sites:
///    None`) reading — the grade must not fire in a room that offers nothing.
///    Observed red against `room_affords_rest` forced to `true`;
/// 4. the furnished room's body carries strictly LESS debt — the claim.
///    Observed red against `SiteGrade::Afforded => 1.0` (the recovery ignores
///    the room) and against grading every bout `Afforded`.
///
/// **What (3) does NOT pin, said plainly: that bare ground's own multiplier is
/// `1.0`.** `sites: None` and a room that offers nothing both route through
/// the same `SiteGrade::Bare` arm, so the two sides of (3) move together and
/// no value of that multiplier can separate them — `SiteGrade::Bare => 1.2`
/// leaves this whole test green. That claim is pinned where the fold's
/// arithmetic is written out by hand instead of called:
/// `liveness::tests::a_fatigue_read_matches_the_walks_own_fatigue_arithmetic`,
/// which computes `awake - fall * days` from `REST_FALL`/`FATIGUE_FALL`
/// directly and compares BIT for bit. That test was run against
/// `SiteGrade::Bare => 1.2` and reddens.
///
/// (3) and (4) are both live, and a mutation that breaks both reports
/// whichever is asserted first — which is why (3) is asserted first: the
/// mutations it alone catches (a permissive `room_affords_rest`) also break
/// (4), while the reverse is not true.
///
/// MUTATION THIS MUST FAIL AGAINST — five, all run against `liveness.rs`, each
/// naming the part of the design it neutralises:
///
/// 1. **the recovery ignores the room**: `SiteGrade::Afforded => 1.0`. Red at
///    (4): `bed=0.44999999999999996, road=0.44999999999999996`.
/// 2. **the room-reading code never fires**: `room_affords_rest` forced to
///    `false` — the vacuity check, and the reason this test is not the shape
///    Task 9's discriminating test had. Red at (4), same values.
/// 3. **the grade fires where nothing is offered**: `room_affords_rest` forced
///    to `true`. Red at (3):
///    `left: 4600427019358961664  right: 4601778099247172812`.
/// 4. **the position trail is not read at all**: `let room =
///    sites.body.home.clone()`, deleting the `position_timeline` merge in
///    effect. Red at (4). **This is the mutation the first draft of this test
///    survived** — its fixture gave each body a `home` equal to the room it
///    slept in, so the trail read and the home fallback answered the same
///    thing and the whole merge was pinned by nothing (campaign ledger #45).
///    The bodies live in a third room now, which is what makes this red
///    possible.
/// 5. **the grade is read at the query instant, not the bout's**: `while
///    cursor < positions.len()` (drop the `<= bout.0` bound), so every bout is
///    graded by the LATEST position. Red at the permanence assertion:
///    `walking off a bed must not retroactively un-repay the night spent on
///    it: 0.15000000000000002 became 0.3`.
#[test]
fn p7_a_bout_in_a_room_that_affords_rest_restores_strictly_more() {
    let furnished = Facet::containing([0.10, 0.10, 0.0], 6);
    let road = Facet::containing([-0.40, -0.40, 0.0], 6);
    assert_ne!(furnished, road, "the two rooms must be distinct");
    let terrain = OneFurnishedRoom {
        furnished: furnished.clone(),
    };

    // THE HOME IS A THIRD ROOM, and that is the fixture's load-bearing detail
    // (fix round 1, Important 2). `rest_timeline` falls back to `body.home` for
    // a bout with no committed position at or before it — the same default
    // `agent_position` applies — so a body whose home IS the room it sleeps in
    // makes the trail read and the home fallback agree at every assertion, and
    // the whole `position_timeline` merge can be deleted with the test still
    // green. The reviewer proved exactly that: replacing the merge's result
    // with `sites.body.home.clone()` left P7, all of `fatigue_stock` and all
    // 603 vessel lib tests passing (campaign ledger #45). Neither body lives in
    // either room it is graded in, so the fallback and the trail can no longer
    // be confused.
    let elsewhere = Facet::containing([0.30, -0.30, 0.0], 6);
    assert!(
        elsewhere != furnished && elsewhere != road,
        "the bodies' home must be a THIRD room, or the trail read and the \
         `body.home` fallback answer the same thing and neither is pinned"
    );
    let (mut ledger, bedded, roadside, reg) = two_bodies();
    let bedded_body = resting_body(bedded, elsewhere.clone());
    let roadside_body = resting_body(roadside, elsewhere.clone());

    // (1) THE FIXTURE DISCRIMINATES. Checked before anything is folded: if
    // both rooms offered the same thing, every comparison below would be a
    // null dressed as a result.
    assert!(
        room_offers_sleep(&furnished, &bedded_body, &terrain),
        "the built, cold room must compose an anchor that offers Sleep — \
         `the-fireside-bed` is the locale band's only `SupportsRest` carrier, \
         and without it this test measures nothing"
    );
    assert!(
        !room_offers_sleep(&road, &roadside_body, &terrain),
        "the unbuilt room must offer nowhere to lie down (fixture precondition)"
    );

    let lay_down = at(2.0);
    let span = TickSpan::from_std_days(0.3).expect("a finite span");
    let woke = lay_down + span;
    for (act, build) in [
        (
            "rested",
            record_rest as fn(EntityId, WorldTime, TickSpan) -> _,
        ),
        (
            "slept",
            record_sleep as fn(EntityId, WorldTime, TickSpan) -> _,
        ),
    ] {
        let mut ledger = ledger.clone();
        for (e, room) in [(bedded, &furnished), (roadside, &road)] {
            ledger
                .commit(place_agent(e, room, WorldTime::GENESIS), &reg)
                .expect("`agent-at` is non-functional");
            ledger
                .commit(build(e, lay_down, span), &reg)
                .expect("a bout predicate is non-functional");
        }
        let on_a_bed = fatigue_at(
            &ledger,
            bedded,
            woke,
            RATE,
            None,
            Some(&RestSites {
                terrain: &terrain,
                body: &bedded_body,
            }),
        );
        let in_the_road = fatigue_at(
            &ledger,
            roadside,
            woke,
            RATE,
            None,
            Some(&RestSites {
                terrain: &terrain,
                body: &roadside_body,
            }),
        );

        // (2) Both strictly inside the clamp: a comparison between two
        // saturated values would pass for any gain at all.
        assert!(
            on_a_bed > 0.0 && on_a_bed < 1.0 && in_the_road > 0.0 && in_the_road < 1.0,
            "both {act} readings must land strictly inside [0, 1] or the \
             inequality proves nothing: bed={on_a_bed}, road={in_the_road}"
        );
        // (3) The grade must not fire where nothing is offered: a bout in
        // the road folds exactly what an UNGRADED read folds.
        assert_eq!(
            in_the_road.to_bits(),
            fatigue_at(&ledger, roadside, woke, RATE, None, None).to_bits(),
            "a {act} bout on bare ground must fold exactly the ungraded \
             arithmetic — the grade is a bonus a room can give, never \
             something every roadside body is quietly given too"
        );
        // (4) THE CLAIM.
        assert!(
            on_a_bed < in_the_road,
            "a {act} bout of the same span, by the same species, must leave \
             LESS sleep debt where the room afforded somewhere to lie down: \
             bed={on_a_bed}, road={in_the_road}"
        );
    }

    // THE GRADE IS PERMANENT, not a property of where the body is NOW.
    // A body that slept in the furnished room and then walked out into the
    // road must keep what the bed repaid: the site is read off the position
    // the ledger records at the BOUT, so a later move cannot un-repay it.
    ledger
        .commit(place_agent(bedded, &furnished, WorldTime::GENESIS), &reg)
        .expect("`agent-at` is non-functional");
    ledger
        .commit(record_sleep(bedded, lay_down, span), &reg)
        .expect("`slept` is non-functional");
    let at_the_bed = fatigue_at(
        &ledger,
        bedded,
        woke,
        RATE,
        None,
        Some(&RestSites {
            terrain: &terrain,
            body: &bedded_body,
        }),
    );
    ledger
        .commit(place_agent(bedded, &road, woke), &reg)
        .expect("`agent-at` is non-functional");
    let after_walking_out = fatigue_at(
        &ledger,
        bedded,
        woke,
        RATE,
        None,
        Some(&RestSites {
            terrain: &terrain,
            body: &bedded_body,
        }),
    );
    assert_eq!(
        at_the_bed.to_bits(),
        after_walking_out.to_bits(),
        "walking off a bed must not retroactively un-repay the night spent \
         on it: {at_the_bed} became {after_walking_out}"
    );
}
