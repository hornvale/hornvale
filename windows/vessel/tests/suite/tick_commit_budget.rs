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
//! ## Measured (seed 42, default `PossessOpts`, `--nocapture`)
//!
//! **The headline finding: the rate does NOT fall toward zero.** It holds
//! roughly flat at ~0.93-0.95 facts/agent/tick — recorded below, and see
//! [`STEADY_STATE_CEILING`]'s doc for the number this test actually gates on
//! and why. A 100-tick exploratory run (not part of the committed battery,
//! `TICKS` bumped by hand and reverted) confirmed this is a genuine
//! steady-state plateau rather than a slow decay still in flight: first-half
//! and last-half rates were both **0.948571** over the full 100 ticks, i.e.
//! no visible settling even at 2.5× this test's horizon. That is the
//! metaplan's §11 feasibility question answered **no**, at this world's
//! current agent roster and drive parameters: with no compaction, this
//! ledger grows linearly in (agents × ticks) indefinitely, not merely at
//! genesis.

use crate::common;
use hornvale_vessel::{PossessOpts, Session};

/// How many consecutive `wait`s to drive. Long enough to separate a
/// transient opening burst (every NPC's first tick commits at least one
/// `agent-at`) from a settled steady state, short enough that the battery
/// stays a commit-gate-class cost rather than a `graph_cost`-class one.
const TICKS: usize = 40;

/// Falsification ceiling on the **steady-state** rate: total facts committed
/// over the last half of [`TICKS`] waits, divided by (agent count × ticks in
/// that half).
///
/// **Measured**, seed 42, default [`PossessOpts`] (`NPC_COUNT` = 3 peoples +
/// `WILD_COUNT` = 4 wild beasts = 7 agents), `TICKS` = 40, run repeatedly
/// (deterministic — byte-identical every run, as expected for fact counts):
///
/// ```text
/// per-tick facts committed = [2, 5, 8, 7, 11, 6, 2, 5, 8, 10, 8, 5, 7, 6, 5,
///     9, 9, 6, 5, 9, 7, 2, 6, 9, 8, 9, 5, 6, 7, 3, 7, 6, 5, 11, 8, 4, 3, 6,
///     5, 13]
/// first-half rate = 0.950000 facts/agent/tick
/// last-half rate  = 0.928571 facts/agent/tick
/// ```
///
/// **This does NOT fall toward zero** — every tick keeps committing several
/// facts per agent (needs cycling: hunger/thirst/danger drives keep firing
/// `agent-at`/`drank` as agents keep moving to and from resources; nothing
/// about this world's steady state is "arrived and done"). It also does not
/// meaningfully grow across this window (0.928571 ≤ 0.950000, within the
/// series' own tick-to-tick noise — confirmed flat rather than transient by
/// the 100-tick exploratory run in the module doc above).
///
/// Budgeted at **1.5**, ≈1.6× the measured last-half rate (0.928571) — the
/// `graph_cost` convention of a few-times margin against measurement noise
/// and roster/seed variation, not a "this looks fine" number: the measured
/// rate is real, sustained churn, and the ceiling exists to catch a
/// *regression on top of it* (e.g. a drive that starts re-firing every tick
/// instead of only when its threshold trips), not to certify the baseline
/// itself as acceptable. The baseline being "large and flat" rather than
/// "falling toward zero" is exactly the finding the report states — the
/// ceiling ratchets down if a later campaign reduces the churn.
const STEADY_STATE_CEILING: f64 = 1.5;

/// The gate: drive [`TICKS`] waits over a seed-42 session, print the raw
/// per-tick commit counts, and assert (a) the steady-state (last-half) rate
/// is at or below [`STEADY_STATE_CEILING`] and (b) the rate does not GROW
/// across the run (last-half ≤ first-half) — the unbounded-log tripwire the
/// metaplan's §11 feasibility question is actually asking.
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
    assert!(
        last_half_rate <= first_half_rate,
        "commit rate GREW across the run (first half {first_half_rate:.6} -> \
         last half {last_half_rate:.6} facts/agent/tick) — the unbounded-log \
         tripwire: a world that commits MORE per tick as it runs longer never \
         reaches a bounded steady state at all"
    );
}
