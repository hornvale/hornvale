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

use hornvale_kernel::{ConceptRegistry, EntityId, Ledger, Lineage, TickSpan, WorldTime};
use hornvale_vessel::liveness::{RESTED, fatigue_at, record_rest};

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
        .map(|&d| fatigue_at(&ledger, e, at(d), RATE, None).to_bits())
        .collect();
    for _ in 0..3 {
        let again: Vec<u64> = probes
            .iter()
            .rev()
            .map(|&d| fatigue_at(&ledger, e, at(d), RATE, None).to_bits())
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
            fatigue_at(&ledger, e, at(d), RATE, None).to_bits(),
            fatigue_at(&reloaded, e, at(d), RATE, None).to_bits(),
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
#[test]
fn p2_fatigue_stays_in_range_and_rises_while_awake() {
    let (ledger, e, _reg) = body();
    // No rest at all: a monotone ramp from genesis, saturating at 1.
    let mut previous = f64::NEG_INFINITY;
    for step in 0..60 {
        let d = step as f64 * 0.25;
        let f = fatigue_at(&ledger, e, at(d), RATE, None);
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
        fatigue_at(&ledger, e, at(0.0), RATE, None) < fatigue_at(&ledger, e, at(1.0), RATE, None),
        "a day awake must cost something"
    );
    assert_eq!(
        fatigue_at(&ledger, e, at(1000.0), RATE, None),
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
#[test]
fn p3_a_short_rest_does_not_zero_the_debt() {
    // Five days awake, then a rest of one twentieth of a day (the finest jump
    // `next_awake_day`'s scan can produce: a real, if minimal, nap).
    let (mut ledger, e, reg) = body();
    let exhausted = fatigue_at(&ledger, e, at(5.0), RATE, None);
    assert_eq!(exhausted, 1.0, "five days awake is a saturated debt");
    push_rest(&mut ledger, &reg, e, 5.0, 0.05);
    let after_nap = fatigue_at(&ledger, e, at(5.05), RATE, None);

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
        let f = fatigue_at(&ledger, e, at(3.0 + span), RATE, None);
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
#[test]
fn p4b_two_rests_restore_more_than_either_alone() {
    let read_at = at(3.0);
    let (mut one, e1, reg1) = body();
    push_rest(&mut one, &reg1, e1, 1.0, 0.2);
    let (mut two, e2, reg2) = body();
    push_rest(&mut two, &reg2, e2, 1.0, 0.2);
    push_rest(&mut two, &reg2, e2, 2.9, 0.05);
    assert!(
        fatigue_at(&one, e1, read_at, RATE, None) < 1.0
            && fatigue_at(&two, e2, read_at, RATE, None) > 0.0,
        "both bodies must sit strictly inside the clamps for this comparison \
         to mean anything: {} and {}",
        fatigue_at(&one, e1, read_at, RATE, None),
        fatigue_at(&two, e2, read_at, RATE, None)
    );
    assert!(
        fatigue_at(&two, e2, read_at, RATE, None) < fatigue_at(&one, e1, read_at, RATE, None),
        "the second rest must count ON TOP of the first: {} vs {}",
        fatigue_at(&two, e2, read_at, RATE, None),
        fatigue_at(&one, e1, read_at, RATE, None)
    );
}

/// P6 — fatigue never goes negative however much a body sleeps.
///
/// The floor is not decoration: a stock that could bank credit would let a
/// creature sleep for a week and then stay awake for a fortnight without ever
/// crossing `FATIGUE_ACT`, which is a behaviour nobody chose. Probed three
/// ways — one absurdly long rest, many rests in a row, and a rest that is still
/// running when the read happens.
#[test]
fn p6_fatigue_never_goes_negative() {
    let (mut ledger, e, reg) = body();
    push_rest(&mut ledger, &reg, e, 0.5, 40.0);
    for d in [0.5, 1.0, 10.0, 40.0, 40.5, 41.0] {
        let f = fatigue_at(&ledger, e, at(d), RATE, None);
        assert!(f >= 0.0, "a 40-day sleep drove fatigue to {f} at day {d}");
    }
    let (mut many, e2, reg2) = body();
    for n in 0..50 {
        push_rest(&mut many, &reg2, e2, n as f64 * 0.5, 0.45);
    }
    for step in 0..60 {
        let d = step as f64 * 0.5;
        let f = fatigue_at(&many, e2, at(d), RATE, None);
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
#[test]
fn a_rest_in_progress_credits_only_the_sleep_already_had() {
    let (mut ledger, e, reg) = body();
    push_rest(&mut ledger, &reg, e, 2.0, 1.0);
    let start = fatigue_at(&ledger, e, at(2.0), RATE, None);
    let quarter = fatigue_at(&ledger, e, at(2.25), RATE, None);
    let half = fatigue_at(&ledger, e, at(2.5), RATE, None);
    let done = fatigue_at(&ledger, e, at(3.0), RATE, None);
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

    let debt_short = fatigue_at(&ledger, e, t, RATE, Some(short_day));
    let debt_long = fatigue_at(&ledger, e, t, RATE, Some(long_day));

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
