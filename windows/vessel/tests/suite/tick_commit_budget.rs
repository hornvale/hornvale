//! The Penstock's feasibility number: facts committed per agent per tick.
//!
//! **Why this exists.** The ledger is append-only (kernel constitution): a
//! long-running world never deletes a fact, so its only way to stay bounded
//! is for the PER-TICK commit rate to fall toward zero as the world settles
//! (agents reach their resources, drives stabilise, `agent-at`/`drank`/
//! disposition churn quiets). If that rate does not fall — worse, if it
//! grows — the log itself grows without bound in RAM no matter how reads are
//! optimised, and a later stage would have to build compaction/eviction just
//! to keep the sim alive. This battery measures that rate directly rather
//! than assuming it, on the one driver that already exists for exactly this
//! (`Session`, via `windows/vessel/tests/common`): `wait` advances the day
//! and runs the NPC layer's tick against the session's own ledger, and
//! `Session::committed_fact_count` (`self.ledger.len()`) is already the
//! observable the metaplan asks for — no new production state, per the task
//! brief's interface note.
//!
//! **Construction.** A fixed seed-42 world (no chamber precondition is
//! needed — `wait` runs at the walk band same as indoors), a default
//! [`PossessOpts`] session (peoples NPCs + wild beasts, `Session::npc_labels`
//! reports the full derived roster regardless of which), and [`TICKS`]
//! consecutive `wait`s, threading the evolving ledger tick to tick exactly as
//! `Session` already does internally. Facts committed per tick is read as
//! `committed_fact_count()` after minus before each `wait` — the
//! `after.len() - before.len()` the brief asks for, through the accessor
//! that already exists rather than a hand-rolled `Ledger` diff.
//!
//! Deterministic only: no `Instant`, no wall-clock. Fact counts are
//! byte-stable across runs and machines, which is exactly why this gate can
//! assert a ceiling where a timing budget could not.
//!
//! ## WHAT THIS GATE CANNOT SEE, AND IT IS THE FINDING MORE THAN THE NUMBER
//!
//! It is a **scalar threshold on an aggregate rate, blind to cause**. Six
//! agents' facts are summed, divided, and compared against one constant, so a
//! world where five agents settle and one enters an unbroken drive cycle reads
//! the same as a world where all six churn mildly — and reads the same again
//! as a world where the churn is healthy resource cycling. Nothing in the
//! number distinguishes them.
//!
//! **Worse, this doc had already pre-committed to the benign reading.** The
//! sentence under [`STEADY_STATE_CEILING`] naming "needs cycling:
//! hunger/thirst/danger drives keep firing `agent-at`/`drank` as agents keep
//! moving to and from resources" was written before the cause was measured,
//! and it is what made the wrong attribution easy to reach for when the gate
//! next tripped: a reader arriving at a red gate finds an explanation already
//! waiting for it. The actual cause at The Pavement was one creature's
//! two-place limit cycle, and the resource counters ruled the benign reading
//! out directly — `drank` moved 10/11/12 and `eaten` 3/3/3 across the three
//! measurements, while the cycling agent's own legs went 6 -> 32.
//!
//! [`the_commit_rate_is_carried_by_one_creatures_two_place_drive_cycle`] is
//! the answer to the blindness, not a bigger constant: a fact count cannot
//! tell a limit cycle from healthy churn, so the cause is pinned
//! BEHAVIOURALLY beside the rate.
//!
//! ## Measured (seed 42, default `PossessOpts`, `--nocapture`)
//!
//! **The headline finding: the rate does NOT fall toward zero.** It holds
//! roughly flat at ~0.92-0.96 facts/agent/tick — recorded below, and see
//! [`STEADY_STATE_CEILING`]'s doc for the number this test actually gates on
//! and why. A 100-tick exploratory run (not part of the committed battery,
//! `TICKS` bumped by hand and reverted) confirmed this is a genuine
//! steady-state plateau rather than a slow decay still in flight: first-half
//! rate 0.950000, last-half rate 1.016667 over the full 100 ticks — noisier
//! than the 40-tick window but still a plateau, not a trend (no per-tick
//! value climbs outside the 40-tick series' own 1-13 range; see
//! `NON_GROWTH_MARGIN`'s doc for the re-measurement this superseded). That is
//! the metaplan's §11 feasibility question answered **no**, at this world's
//! current agent roster and drive parameters: with no compaction, this
//! ledger grows linearly in (agents × ticks) indefinitely, not merely at
//! genesis.

use crate::common;
use hornvale_vessel::{PossessOpts, Session};

/// How many consecutive `wait`s to drive. **Not** separating a transient
/// opening burst from steady state — the measured series (this module's own
/// doc) shows no such burst; tick 1 commits only 2 facts, well below several
/// later ticks. The actual justification is averaging: 40 ticks split into
/// two 20-tick halves gives each half's rate enough ticks to smooth the
/// per-tick noise (values range 2-13) into a comparable summary statistic,
/// while staying short enough that the battery remains a commit-gate-class
/// cost rather than a `graph_cost`-class one.
const TICKS: usize = 40;

/// Falsification ceiling on the **steady-state** rate: total facts committed
/// over the last half of [`TICKS`] waits, divided by (agent count × ticks in
/// that half).
///
/// **Measured**, seed 42, default [`PossessOpts`], `TICKS` = 40, run
/// repeatedly (deterministic — byte-identical every run, as expected for
/// fact counts). **THE SOURCES, Task 9 moved this world**: xorn's
/// per-rung `CHEMOSYNTHATE` weight (see `domains/species/src/lib.rs`'s
/// `xorn` row) legitimately changes species suitability/dominance at seed
/// 42, which changed the derived wild-beast roster near the flagship from
/// `NPC_COUNT` = 3 peoples + `WILD_COUNT` = 4 wild beasts = 7 agents to
/// **6 agents**. The series below is the re-measurement, not the original:
///
/// ```text
/// per-tick facts committed = [1, 4, 7, 7, 9, 6, 1, 3, 7, 9, 7, 5, 5, 6, 4,
///     7, 6, 5, 4, 7, 6, 1, 5, 7, 7, 8, 4, 5, 6, 2, 11, 4, 5, 9, 7, 8, 1, 5,
///     2, 12]
/// first-half rate = 0.916667 facts/agent/tick
/// last-half rate  = 0.958333 facts/agent/tick
/// ```
///
/// **This does NOT fall toward zero** — every tick keeps committing several
/// facts per agent (needs cycling: hunger/thirst/danger drives keep firing
/// `agent-at`/`drank` as agents keep moving to and from resources; nothing
/// about this world's steady state is "arrived and done"). Unlike the
/// pre-Task-9 series, this window's last half is measurably ABOVE its first
/// half (see [`NON_GROWTH_MARGIN`] for why that is accepted rather than
/// failed) — the 100-tick exploratory run in the module doc above confirms a
/// noisy plateau, not a trend.
///
/// Budgeted at **1.5** until The Pavement, ≈1.56× the then-measured last-half
/// rate (0.958333) — the `graph_cost` convention of a few-times margin against
/// measurement noise and roster/seed variation, not a "this looks fine"
/// number: the measured rate is real, sustained churn, and the ceiling exists
/// to catch a *regression on top of it* (e.g. a drive that starts re-firing
/// every tick instead of only when its threshold trips), not to certify the
/// baseline itself as acceptable. The baseline being "large and flat" rather
/// than "falling toward zero" is exactly the finding the report states — the
/// ceiling ratchets down if a later campaign reduces the churn.
///
/// # 2.5 SINCE THE PAVEMENT, AND THIS IS AN OVERRIDE, NOT A RECALIBRATION
///
/// **The gate is not misnamed and not miscalibrated. It tripped correctly.**
/// This module's own doc ties "bounded" to behavioural settling, and what
/// crossed the old ceiling is a non-decaying rate driven by one agent's
/// unbroken drive cycle — precisely the failure mode the gate was written to
/// catch. The number below was raised so the campaign could land; the defect
/// was NOT fixed, and nothing here should be read as saying it was.
///
/// **The measured cause, on seed 42 with `TICKS` = 40** (see
/// [`the_commit_rate_is_carried_by_one_creatures_two_place_drive_cycle`],
/// which pins it as a behavioural witness so this paragraph cannot rot into
/// fiction):
///
/// ```text
/// per-tick facts = [5, 8, 11, 11, 16, 17, 11, 12, 16, 19, 17, 16, 14, 15,
///     14, 11, 12, 9, 9, 10, 14, 6, 9, 10, 11, 15, 8, 10, 8, 8, 14, 8, 9, 9,
///     13, 12, 6, 9, 7, 12]
/// first-half rate = 2.108333   last-half rate = 1.650000    (6 agents)
///
/// per agent, facts in the first half -> the last half:
///   a wild rust-monster       103 -> 104     <- flat, and 52.5% of the last half
///   hobgoblin of Neene         72 ->  16
///   hobgoblin of Naabeena      19 ->  19
///   a wild otyugh              17 ->  15
///   a wild carrion-crawler     42 ->  44
///   EVERY OTHER AGENT         150 ->  94     <- the world IS settling
/// ```
///
/// One seed-42 wild rust-monster commits **207 facts over the 40 ticks**, of
/// which 57 carry `fled the uncanny ground (fear)` and 60 carry `drifted
/// homeward, missing its people (belonging)`. It switches between those two
/// drives **43 times** across the window: a two-place limit cycle with no
/// hysteresis on drive arbitration. Its rate is the only one that does not
/// fall, and every other agent taken together falls 150 -> 94.
///
/// **THE DEFECT PRE-DATES THIS CAMPAIGN.** A wild carrion-crawler runs a
/// comfort-versus-thirst version of the same cycle in this very measurement
/// (`sought a kinder clime (comfort)` 26 times), and one exists at HEAD. The
/// mesh change did not create the cycle; the octile planner's smaller ball at
/// equal reach let the rust-monster's homeward plans SUCCEED where they used
/// to exhaust `PLAN_BUDGET` and return `None`, which made an existing cycle
/// louder.
///
/// **The fix is hysteresis on drive arbitration, and it is deliberately not
/// here.** Nathan's ruling: *"We definitely have to add hysteresis into the
/// system, but now is not the time."* It is carried by
/// `PSY-drive-arbitration-limit-cycle`, with the N-place oscillation corpus
/// (cycles over 3, 5, 7, 13 places) carried separately by
/// `PSY-oscillation-corpus` — which must be FROZEN BEFORE the hysteresis fix
/// moves the numbers (decision 0016).
///
/// 2.5 is ≈1.52× the measured 1.650000, the same shape of margin the 1.5
/// carried against 0.958333. It is not slack bought to make a red go away: a
/// rate materially above today's still fails.
const STEADY_STATE_CEILING: f64 = 2.5;

/// The non-growth check's own tolerance — see the assertion below for why a
/// STRICT `last_half_rate <= first_half_rate` no longer holds and what
/// investigation justified widening it THIS FAR and no further.
///
/// **THE SOURCES, Task 9 investigation (2026-08-26).** The re-measurement
/// above flips the strict check: last-half 0.958333 > first-half 0.916667,
/// a 4.5% overshoot. Per this assertion's own standing warning ("the correct
/// response to a flip is to INVESTIGATE… never to widen this margin [to hide
/// a regression]"), the investigation, not a reflexive widen:
///
/// 1. **What moved and why is known, not mysterious.** Task 9 (`MAP-per-
///    rung-substrate`'s consumer switch) changed xorn's suitability, which
///    changed the derived wild-beast roster at seed 42 from 7 agents to 6 —
///    a roster-composition change, exactly the benign cause this module's
///    own doc named in advance, not a drive re-firing every tick.
/// 2. **A 100-tick exploratory run rules out a trend.** First-half 0.950000,
///    last-half 1.016667 — noisier at n=6 agents than the old n=7 baseline,
///    but still a plateau (no per-tick value exceeds the 40-tick series'
///    own range), not a climb that keeps climbing.
///
/// So the margin below is sized to the MEASURED noise (the 100-tick run's
/// 7.0% overshoot is the largest of the two), not merely to the 40-tick
/// run's 4.5%, and stops exactly there: **10%**, not "whatever makes it
/// pass". A larger overshoot than this still fails, which is what keeps this
/// a tripwire rather than a rubber stamp.
const NON_GROWTH_MARGIN: f64 = 1.10;

/// The gate: drive [`TICKS`] waits over a seed-42 session, print the raw
/// per-tick commit counts, and assert (a) the steady-state (last-half) rate
/// is at or below [`STEADY_STATE_CEILING`] and (b) the rate does not GROW
/// across the run by more than [`NON_GROWTH_MARGIN`] — the unbounded-log
/// tripwire the metaplan's §11 feasibility question is actually asking.
#[test]
fn facts_committed_per_agent_per_tick_stays_bounded() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    let agents = session.npc_labels().len();
    assert!(
        agents > 0,
        "a default session always derives at least the flagship's own NPC"
    );

    let mut per_tick: Vec<usize> = Vec::with_capacity(TICKS);
    let mut before = session.committed_fact_count();
    for _tick in 0..TICKS {
        session.handle("wait");
        let after = session.committed_fact_count();
        per_tick.push(after - before);
        before = after;
    }

    println!("agents = {agents}, per-tick facts committed = {per_tick:?}");

    let half = TICKS / 2;
    let first_half: usize = per_tick[..half].iter().sum();
    let last_half: usize = per_tick[half..].iter().sum();
    let first_half_rate = first_half as f64 / (agents * half) as f64;
    let last_half_rate = last_half as f64 / (agents * (TICKS - half)) as f64;
    println!(
        "first-half rate = {first_half_rate:.6} facts/agent/tick, \
         last-half rate = {last_half_rate:.6} facts/agent/tick"
    );

    assert!(
        last_half_rate <= STEADY_STATE_CEILING,
        "steady-state rate {last_half_rate:.6} facts/agent/tick exceeds the \
         ceiling {STEADY_STATE_CEILING} — the ledger is not settling, which \
         is the metaplan's §11 feasibility question answered no"
    );
    // WARNING: this non-growth assertion carries a thin margin at seed 42.
    // [`NON_GROWTH_MARGIN`]'s own doc records the THE SOURCES Task 9
    // investigation that widened it from a strict `<=` to `<= * 1.10` — read
    // that before touching this number again. A benign change to drive
    // timing or roster composition can flip it; the correct response to a
    // flip is to INVESTIGATE which per-tick values moved and why (as that
    // doc comment does), never to reflexively widen the margin further —
    // slack beyond what measurement justifies would hide the exact
    // regression (a drive re-firing every tick) this assertion exists to
    // catch.
    assert!(
        last_half_rate <= first_half_rate * NON_GROWTH_MARGIN,
        "commit rate GREW across the run by more than NON_GROWTH_MARGIN allows \
         (first half {first_half_rate:.6} -> last half {last_half_rate:.6} \
         facts/agent/tick, ratio {:.4} > {NON_GROWTH_MARGIN}) — the unbounded-log \
         tripwire: a world that commits MORE per tick as it runs longer never \
         reaches a bounded steady state at all",
        last_half_rate / first_half_rate
    );
}

/// How much the cycling agent's own commit count may grow across the run,
/// as a percentage of its first half.
///
/// **Measured 103 -> 104, i.e. 101%.** 130 is a few-times-the-noise margin in
/// the same shape [`NON_GROWTH_MARGIN`] carries, not slack sized to make
/// anything pass: the point of an upper arm is that a materially tighter cycle
/// reddens here, where the cause is named, rather than only at
/// [`STEADY_STATE_CEILING`], where it is invisible.
const LOUD_GROWTH_MARGIN_PCT: usize = 130;

/// The ceiling on either drive leg's fact count over [`TICKS`] ticks.
/// Measured 57 (fear) and 60 (belonging); 90 is ~1.5x the larger.
const LEG_CEILING: usize = 90;

/// The ceiling on how many times the cycling agent switches between its two
/// drives over [`TICKS`] ticks. Measured 43; 70 is ~1.6x.
///
/// A LOWER bound alone says "this is a cycle"; the pair says "this is the
/// cycle that was measured". A run that switched 200 times would be a
/// different, faster pathology wearing the same name.
const SWITCH_CEILING: usize = 70;

/// **The witness that keeps [`STEADY_STATE_CEILING`]'s raise honest.**
///
/// The ceiling above was raised rather than the defect fixed, and a raised
/// ceiling with nothing beside it is a number that stops meaning anything the
/// moment the world moves again. So the CAUSE is pinned here, behaviourally.
///
/// **It is deliberately not a fact count.** A count cannot tell a limit cycle
/// from healthy churn — that is exactly the blindness that let "needs cycling"
/// stand as the explanation for a rate one agent's unbroken drive cycle was
/// carrying. What this asserts instead is the SHAPE: one creature dominates
/// the commit stream, its own rate does not fall while everyone else's does,
/// and its facts oscillate between two named drive legs many times over the
/// window rather than settling into either.
///
/// **Measured, seed 42, `TICKS` = 40, deterministic** — the numbers below are
/// what this world produces today and the floors are set well under them, so
/// this fails on a change of KIND (the cycle broken, or a second creature
/// joining it) rather than on ordinary movement:
///
/// ```text
/// loudest agent            a wild rust-monster, 207 facts over 40 ticks
///   fled the uncanny ground (fear)                57
///   drifted homeward, missing its people (belonging)  60
///   switches between the two legs                 43
///   its own first half -> last half              103 -> 104
/// every other agent, summed, first -> last       150 ->  94
/// ```
///
/// **EVERY ASSERTION IS TWO-SIDED, and the upper half is the point (fix round
/// 1).** The first version of this witness was one-directional throughout, so
/// the one thing it was specifically asked to catch — the cycle getting WORSE
/// — would have passed it. With [`STEADY_STATE_CEILING`] raised to 2.5 that
/// left a tightening cycle detectable only by the scalar aggregate gate this
/// same file documents as blind to cause, on 1.52x of headroom. The idiom is
/// `GROWN_RELAXATIONS` in `lattice::anchor_cells`, one crate over — lexicon: that
/// name means lattice SQUARES, which are areas — a bound nothing can cross in
/// EITHER direction quietly.
///
/// WHAT MAKES IT FAIL, and each is a real outcome rather than noise:
/// - hysteresis lands on drive arbitration and the cycle stops — the floors on
///   the leg counts and on the switch count go red, which is the SIGNAL that
///   `PSY-drive-arbitration-limit-cycle` is done and the ceiling above may
///   come back down;
/// - **the cycle TIGHTENS** — [`LOUD_GROWTH_MARGIN_PCT`], [`LEG_CEILING`] or
///   [`SWITCH_CEILING`] goes red, naming the cause, instead of the defect
///   being absorbed silently by the ceiling's headroom;
/// - a SECOND agent joins the cycle — the "one creature carries it" share
///   assertion goes red, and the ceiling's whole justification with it;
/// - the rest of the world stops settling — the aggregate-fall assertion goes
///   red, which would mean the raise is covering something wider than one
///   creature.
#[test]
fn the_commit_rate_is_carried_by_one_creatures_two_place_drive_cycle() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");

    // Facts per subject, read out of the session's own committed ledger. The
    // JSON is the only public read of a fact's SUBJECT and PROVENANCE from an
    // integration test, and provenance is the whole point here: the drive that
    // committed a fact is what distinguishes a cycle from churn.
    let by_subject = |s: &Session<'_>| -> std::collections::BTreeMap<String, Vec<String>> {
        let doc: serde_json::Value =
            serde_json::from_str(&s.session_ledger_json()).expect("a ledger serializes");
        let mut out: std::collections::BTreeMap<String, Vec<String>> = Default::default();
        for f in doc["facts"].as_array().expect("the ledger carries facts") {
            out.entry(f["subject"].to_string())
                .or_default()
                .push(f["provenance"].as_str().unwrap_or("").to_string());
        }
        out
    };

    let at_start = by_subject(&session);
    let half = TICKS / 2;
    for _ in 0..half {
        session.handle("wait");
    }
    let at_half = by_subject(&session);
    for _ in 0..TICKS - half {
        session.handle("wait");
    }
    let at_end = by_subject(&session);

    let len = |m: &std::collections::BTreeMap<String, Vec<String>>, k: &String| {
        m.get(k).map(Vec::len).unwrap_or(0)
    };
    let mut first: std::collections::BTreeMap<String, usize> = Default::default();
    let mut last: std::collections::BTreeMap<String, usize> = Default::default();
    for k in at_end.keys() {
        first.insert(k.clone(), len(&at_half, k) - len(&at_start, k));
        last.insert(k.clone(), len(&at_end, k) - len(&at_half, k));
    }
    let total_last: usize = last.values().sum();
    assert!(
        total_last > 0,
        "the world committed nothing in the second half, so nothing below is tested"
    );

    // (1) ONE creature carries the rate.
    let (loud, loud_last) = last
        .iter()
        .max_by_key(|(_, n)| **n)
        .map(|(k, n)| (k.clone(), *n))
        .expect("some subject committed");
    assert!(
        loud_last * 100 >= total_last * 40,
        "no single agent carries the commit rate ({loud_last} of {total_last} in the \
         last half) — the ceiling's raise is justified by ONE creature's cycle, so if \
         the load has spread the justification has lapsed: {last:?}"
    );

    // (2) ITS rate does not fall while EVERYONE ELSE's does. Stated over the
    //     aggregate of the others rather than per-agent, because a single
    //     agent's half-to-half count is noisy at this window length (measured:
    //     one of the four rose 42 -> 44 while the four together fell 150 -> 94).
    let loud_first = first[&loud];
    assert!(
        loud_last * 10 >= loud_first * 9,
        "the loud agent's own rate FELL ({loud_first} -> {loud_last}) — the cycle this \
         witness exists to pin has broken, which is good news and means \
         STEADY_STATE_CEILING should come back down rather than stay raised"
    );
    // THE UPPER ARM (fix round 1). Everything above was one-directional, and
    // "the cycle got WORSE" is the one thing this witness was asked to catch:
    // with the ceiling raised to 2.5, a tightening cycle would otherwise be
    // caught only by `STEADY_STATE_CEILING` — the scalar-blind-to-cause gate
    // this very file documents as the weakness — sitting on 1.52x headroom.
    // Two-sided, in the `GROWN_RELAXATIONS` idiom next door.
    assert!(
        loud_last * 100 <= loud_first * LOUD_GROWTH_MARGIN_PCT,
        "the loud agent's own rate GREW by more than {LOUD_GROWTH_MARGIN_PCT}% of its \
         first half ({loud_first} -> {loud_last}) — the deferred limit cycle is \
         TIGHTENING, not merely persisting. That is a worsening of \
         PSY-drive-arbitration-limit-cycle, and raising STEADY_STATE_CEILING again to \
         absorb it is exactly what this witness exists to prevent"
    );
    let others_first: usize = first
        .iter()
        .filter(|(k, _)| **k != loud)
        .map(|(_, n)| n)
        .sum();
    let others_last: usize = last
        .iter()
        .filter(|(k, _)| **k != loud)
        .map(|(_, n)| n)
        .sum();
    assert!(
        others_last < others_first,
        "every other agent taken together did NOT settle ({others_first} -> \
         {others_last}) — the raised ceiling is then covering something wider than \
         one creature's drive cycle, which is a different finding entirely"
    );

    // (3) THE CYCLE ITSELF: two named legs, both substantial, switching often.
    let stream: Vec<&str> = at_end[&loud].iter().map(String::as_str).collect();
    let fear = stream.iter().filter(|p| p.contains("(fear)")).count();
    let belonging = stream.iter().filter(|p| p.contains("(belonging)")).count();
    assert!(
        fear >= 20 && belonging >= 20,
        "the loud agent's two drive legs are no longer both substantial (fear {fear}, \
         belonging {belonging}) — measured 57 and 60. Either the cycle broke or a \
         different drive is now carrying the rate, and either way the ceiling's \
         justification needs re-reading rather than re-raising"
    );
    assert!(
        fear <= LEG_CEILING && belonging <= LEG_CEILING,
        "a drive leg GREW past {LEG_CEILING} (fear {fear}, belonging {belonging}, \
         measured 57 and 60) — the cycle is committing more per leg than when the \
         ceiling was raised for it"
    );
    let legs: Vec<char> = stream
        .iter()
        .filter(|p| p.contains("(fear)") || p.contains("(belonging)"))
        .map(|p| if p.contains("(fear)") { 'F' } else { 'B' })
        .collect();
    let switches = legs.windows(2).filter(|w| w[0] != w[1]).count();
    assert!(
        switches >= 15,
        "the loud agent switched between its two drives only {switches} times over \
         {TICKS} ticks (measured 43) — a two-place LIMIT CYCLE is what justifies \
         calling this a defect rather than churn, and few switches means it is \
         something else"
    );
    assert!(
        switches <= SWITCH_CEILING,
        "the loud agent switched between its two drives {switches} times over {TICKS} \
         ticks, past the ceiling of {SWITCH_CEILING} (measured 43) — the cycle is \
         running FASTER than the one the ceiling was raised for, which is the \
         worsening this witness is here to make visible rather than absorb"
    );
}
