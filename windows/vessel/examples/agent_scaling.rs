//! The Penstock stage 1, Task 5: does the plan (GOAP/A*) term swamp query
//! cost as agent count grows, and what is the query / plan / commit split?
//!
//! INFORMATIVE, NEVER A GATE — the same standing as
//! `windows/vessel/examples/turn_cost.rs` and
//! `kernel/examples/query_scaling.rs`. This is the bench the metaplan's §11
//! feasibility question hinges on: if planning dominates tick cost, stages
//! 2-8 (a read-optimised query/cache/index layer) would be optimising a term
//! that barely moves the total, and the metaplan says to stop rather than
//! build it.
//!
//! Run: `cargo run --release -p hornvale-vessel --example agent_scaling`
//! ALWAYS `--release`: a debug build measures the optimizer, not the code.
//!
//! ## Construction
//!
//! One seed-42 world (`build_world`, default pins), one [`LocaleContext`]
//! (`LocaleContext::build`, the sanctioned entry point, decision 0092), one
//! bench-owned ledger clone. Agents are REAL, not hand-built:
//! `hornvale_vessel::liveness::derive_npcs` is `pub` and takes the agent
//! count `k` as a parameter, truncating the settlement list ordered by
//! population (home settlement pulled to the front) — so **`k` is the sweep
//! variable, one agent per settlement, up to the world's settlement
//! count**. Wild agents (`derive_wild_npcs`) are deliberately NOT included
//! in the sweep, so the driven agent count is exactly `k`, never `k +
//! WILD_COUNT` — a clean denominator for every per-agent rate below.
//!
//! `home_settlement` (the id `derive_npcs` guarantees a spot for,
//! regardless of population rank) is the flagship's settlement —
//! `mint_flagship(&world, &ctx)?.village.id` — copied from `Session::
//! start`'s own default-possession construction.
//!
//! The tick driver is `DriveMovements::step_with_occupancy`, called
//! directly with a CALLER-OWNED `HomeNavCache` and `RoomMeshMemo` that
//! persist across every tick — the same construction `Session::wait` uses
//! to recover its own `Occupancy` read. **This deliberately differs from
//! `Session::wait`'s full turn**, which evaluates the walk a SECOND time
//! through `hornvale_kernel::tick` for the facts it actually commits: the
//! generic `TickSystem::step` trait signature cannot carry a caller's
//! memo/cache (see `DriveMovements`'s own `TickSystem::step` impl doc), so
//! that second evaluation pays a fresh, THROWAWAY `HomeNavCache` on every
//! tick, discarding whatever cross-tick warmth the session's own cache had
//! built. Reusing ONE cache across every tick, as this bench does, is
//! exactly the cross-tick reuse `HomeNavCache` exists for (its own doc:
//! "the campaign's scaling bar needs a cache that survives across calls to
//! this very function, not merely across one call's own creatures/pops") —
//! so this bench measures the plan cost the design actually intends, not
//! the throwaway-per-tick cost `Session::wait` pays today as a side effect
//! of the kernel scheduler's generic signature. The facts
//! `step_with_occupancy` returns are committed into the bench's own ledger
//! by hand (`ledger.commit(fact, &registry)`), exactly what
//! `hornvale_kernel::tick` does internally for a single system — so the
//! commit term below costs precisely what production pays per fact.
//!
//! ## The query / plan / commit split, and which term is a RESIDUAL
//!
//! - **plan** = the delta in `HomeNavCache::searches()` over the run — one
//!   unit per real `plan_to_room` (A*) search, exactly the metaplan's
//!   plan-cost witness, deterministic and byte-stable across machines.
//! - **commit** = facts actually appended to the ledger this run (the delta
//!   in `ledger.len()`) — the cost `Ledger::commit` pays per fact
//!   (quantize, the idempotency/contradiction check, the push).
//! - **query is NOT independently instrumented.** Nothing in this driver
//!   exposes a query counter the way `HomeNavCache` exposes a plan counter
//!   — drive evaluation (thirst/hunger/danger/thermal), the per-tick alarm
//!   field build, `WalkState` folds, and occupancy bookkeeping are all
//!   mixed into the same wall-clock span with no internal boundary this
//!   bench can see from outside the crate. **query is therefore reported as
//!   the RESIDUAL**: the total wall time for the rung, printed alongside
//!   `plan`/`commit` but with NO subtraction performed — this bench does
//!   not know how to apportion that wall time between query and the two
//!   measured terms, so it reports the whole span under `query` rather than
//!   inventing a split it cannot support. A residual silently absorbs
//!   everything unmeasured — including any real query-shaped cost this
//!   driver has no counter for — so it must never be read as though query
//!   itself were measured. See `## Measured`'s own discussion of what the
//!   superlinear total implies about where that residual likely sits.
//!
//! ## bytes-per-agent
//!
//! Deterministic, never RSS (RSS is not reproducible and cannot be compared
//! across machines): `ledger.len() * size_of::<Fact>()` plus the heap each
//! committed `Fact` owns — `predicate.len() + provenance.len()` (both
//! `String`) plus a `Value::Text` object's own `.len()` where the fact's
//! object is text (`0` for `Number`/`Entity`/`Flag`, none of which own
//! heap). `.len()`, not `.capacity()`: capacity depends on the allocator's
//! growth history for that particular `String`, not on its byte content, so
//! it is not reproducible across runs or allocators the way a fact's actual
//! byte length is.
//!
//! ## Measured
//!
//! Date: 2026-08-22. Box: `ambrose` (`hostname -s`). Profile: `--release`.
//! Verbatim output:
//!
//! ```text
//! agent_scaling: seed 42 has 221 settlements (the ceiling derive_npcs's k can reach)
//!   agents    ms/tick   facts/a/tick  search/a/tick      bytes/agent    total_bytes      facts   searches
//!       10     68.216         2.2150         1.7750         172065.4        1720654        443        355
//!       50    503.724         2.8250         2.2790          42121.4        2106071       2825       2279
//!      100   1267.741         2.6780         2.0675          25122.9        2512286       5356       4135
//!      200   5722.748         2.8960         2.2822          17597.7        3519536      11584       9129
//!
//! fitted log-log slope, ms/tick vs agent count (1.0 linear, 2.0 quadratic): 1.43
//! marginal bytes/agent (total_bytes[200]-total_bytes[10])/(agents[200]-agents[10]) = 9467.8
//!
//! query/plan/commit split at the largest rung run (agents=200, 20 ticks):
//!   plan   (HomeNavCache::searches delta) = 9129 searches
//!   commit (ledger.len() delta)           = 11584 facts
//!   query  = RESIDUAL (this driver exposes no query counter --
//!            see the module doc's "which term is a RESIDUAL" section).
//!            Total wall for this rung: 114454.968 ms over 20 ticks.
//! ```
//!
//! **Reading `bytes/agent`.** The single-rung `bytes/agent` column falls as
//! agent count rises (172065 -> 17598) NOT because more agents cost less —
//! it is `total_bytes / agents`, and `total_bytes` is dominated by the
//! fixed, already-large seed-42 GENESIS ledger (terrain/settlement/species
//! facts that exist regardless of `agents`), so dividing a near-constant
//! numerator by a growing denominator falls even if nothing about the
//! NPC-driven cost changed at all. The **marginal** figure above nets that
//! fixed term out by differencing `total_bytes` between the smallest and
//! largest rung actually run: **~9,468 bytes per additional agent** over 20
//! ticks — a small, plausible NPC's-worth of `NAME`/`agent-at`/`drank`/
//! `rested`/`eaten` facts, and the number the metaplan's memory arithmetic
//! should actually use, not the single-rung column.
//!
//! **Search count and facts committed do not fall toward zero as agent
//! count grows** — `search/a/tick` holds in a narrow ~1.8-2.3 band and
//! `facts/a/tick` in a narrow ~2.2-2.9 band across a 20x range of agent
//! counts (10 -> 200), the same qualitative shape the sibling
//! `tick_commit_budget.rs` battery found for facts alone (~0.93-0.95, flat,
//! not falling). Two independent counters now agree: this world's steady
//! state is genuine sustained churn, not a decaying transient, on both the
//! commit axis and the plan axis.
//!
//! **The superlinear total is NOT explained by either measured term
//! alone.** Both `searches_delta` and `facts_delta` grow almost exactly
//! LINEARLY with agent count (roughly 5x and 2x for 5x/2x agent-count
//! steps, matching the flat per-agent-per-tick rates above) — so if `plan`
//! (search count) or `commit` (fact count) were the whole story, the fitted
//! `ms/tick` slope would land near 1.0. It measures **1.43** instead:
//! distinctly superlinear. Since the two DIRECTLY MEASURED terms are each
//! linear, the excess above linear lives entirely in the **residual**
//! (query, in this bench's framing) — the population-wide per-creature
//! reads `step_with_occupancy` performs once per creature per tick
//! (`hazard_memory_memo`/`alarm_field_memo`, each threaded `&self.npcs`,
//! the full roster) are the most likely site, since a per-creature read
//! over the full population is an O(agents) cost paid `agents` times, i.e.
//! O(agents^2) — exactly the ledger-scan shape Task 2/3 diagnosed and
//! fixed on the READ side. This bench cannot confirm that attribution
//! directly (no counter separates it from search/commit), which is
//! precisely why it is reported as a residual rather than a measurement —
//! but the arithmetic argument (measured terms linear, total superlinear)
//! is real and does not depend on that attribution being right.

#![allow(
    clippy::disallowed_types,
    reason = "std::time::Instant times an out-of-sim profiling EXAMPLE (decision 0001 bans wall-clock IN a world; nothing here builds one) -- the same scoped posture windows/vessel/examples/turn_cost.rs and kernel/examples/query_scaling.rs already use"
)]

use hornvale_kernel::{EntityId, Fact, Ledger, RoomMeshMemo, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_vessel::liveness::{
    DriveMovements, HomeNavCache, LocaleTerrain, SUSTENANCE, derive_npcs,
};
use hornvale_vessel::mint_flagship;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
use std::time::Instant;

/// Agent-count sweep candidates. Filtered at runtime against the seed-42
/// world's actual settlement count (`derive_npcs`'s own ceiling — its `k`
/// truncates the settlement list, so `k` cannot exceed how many
/// settlements exist) rather than assumed; whichever candidates survive
/// the filter are what actually runs, and the ceiling itself is printed.
const RUNGS: &[usize] = &[10, 50, 100, 200];

/// Ticks driven at each rung. Long enough to separate the opening burst
/// (every NPC's first tick pays a cold `HomeNavCache` search) from the
/// cache's steady state; short enough that the whole sweep stays
/// release-mode-informative rather than `heavy`-tier-class, matching
/// `tick_commit_budget.rs`'s own `TICKS` framing but doubled since this
/// bench also pays A* search cost tick_commit_budget never measures.
const TICKS: usize = 20;

/// Least-squares slope of log(y) against log(x) — the scaling exponent.
/// Copied from `kernel/examples/query_scaling.rs`'s helper of the same
/// name (a shared module is not worth it for two benches, per the task
/// brief) rather than factored into a new shared crate.
fn log_log_slope(xs: &[f64], ys: &[f64]) -> f64 {
    let n = xs.len() as f64;
    let lx: Vec<f64> = xs.iter().map(|v| hornvale_kernel::math::ln(*v)).collect();
    let ly: Vec<f64> = ys.iter().map(|v| hornvale_kernel::math::ln(*v)).collect();
    let mx = lx.iter().sum::<f64>() / n;
    let my = ly.iter().sum::<f64>() / n;
    let num: f64 = lx.iter().zip(&ly).map(|(x, y)| (x - mx) * (y - my)).sum();
    let den: f64 = lx.iter().map(|x| (x - mx) * (x - mx)).sum();
    num / den
}

/// Deterministic byte estimate for `ledger`: the flat `Fact` size times the
/// fact count, plus the heap each fact's `String`/`Value::Text` fields
/// actually own (byte length, not capacity — see the module doc).
fn ledger_bytes(ledger: &Ledger) -> usize {
    let flat = ledger.len() * std::mem::size_of::<Fact>();
    let heap: usize = ledger
        .iter()
        .map(|f| {
            f.predicate.len()
                + f.provenance.len()
                + match &f.object {
                    Value::Text(s) => s.len(),
                    _ => 0,
                }
        })
        .sum();
    flat + heap
}

/// One rung's measured outcome.
struct Row {
    agents: usize,
    ms_per_tick: f64,
    facts_per_agent_per_tick: f64,
    searches_per_agent_per_tick: f64,
    bytes_per_agent: f64,
    /// The raw (undivided) deterministic byte estimate for the WHOLE ledger
    /// at the end of this rung — printed alongside `bytes_per_agent` because
    /// that per-agent figure is dominated by the fixed genesis ledger size
    /// at small `agents` (dividing a near-constant numerator by a small
    /// denominator), so a reader wanting the MARGINAL cost of one more
    /// agent needs to difference this raw total across two rungs rather
    /// than read `bytes_per_agent` at a single rung as if it were marginal.
    /// See `## Measured`'s own worked marginal figure.
    total_bytes: usize,
    facts_delta: usize,
    searches_delta: u64,
    wall_ms: f64,
}

/// Drive `agents` NPCs (derived at `home_settlement`'s settlement roster,
/// truncated to `agents`) through `TICKS` `DriveMovements` ticks over a
/// fresh ledger clone, threading ONE `HomeNavCache`/`RoomMeshMemo` across
/// the whole run (see the module doc for why that, not `Session::wait`'s
/// literal double-evaluation, is the right instrument here).
fn run_rung(
    world: &World,
    ctx: &LocaleContext,
    home_settlement: EntityId,
    day_length_std: Option<f64>,
    agents: usize,
) -> Row {
    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    // The four predicates the NPC drive stack actually writes — copied
    // verbatim from `Session::start`'s own registration block (`session.rs`
    // ~652-671). `DISPOSITION_SHIFT`/`TURNED_HOSTILE` are player-possession
    // predicates with no NPC-drive writer, so this bench (no possessed
    // player) never needs them.
    registry
        .register_predicate(
            hornvale_vessel::liveness::AGENT_AT,
            false,
            "an agent's position on a day",
        )
        .expect("AGENT_AT registers identically every run");
    registry
        .register_predicate(
            hornvale_vessel::liveness::DRANK,
            false,
            "an agent satisfied its sustenance goal",
        )
        .expect("DRANK registers identically every run");
    registry
        .register_predicate(
            hornvale_vessel::liveness::RESTED,
            false,
            "an agent rested (eased its fatigue) on a day",
        )
        .expect("RESTED registers identically every run");
    registry
        .register_predicate(
            hornvale_vessel::liveness::EATEN,
            false,
            "an agent ate (eased its hunger) on a day",
        )
        .expect("EATEN registers identically every run");

    let npcs = derive_npcs(world, ctx, &mut ledger, agents, home_settlement);
    let n = npcs.len();

    let terrain = LocaleTerrain::new(ctx);
    let mut mesh_memo = RoomMeshMemo::new();
    let mut home_nav_cache = HomeNavCache::new();
    let mut day = WorldTime::new(0.5).expect("0.5 is finite");

    let facts_before = ledger.len();
    let searches_before = home_nav_cache.searches();
    let t0 = Instant::now();
    for _ in 0..TICKS {
        let from = day;
        day = WorldTime::new(day.day() + 1.0).expect("day advance stays finite");
        let sys = DriveMovements {
            npcs: npcs.clone(),
            from,
            to: day,
            params: SUSTENANCE,
            day_length_std,
            terrain: &terrain,
        };
        let (facts, _occupancy) =
            sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
        for fact in facts {
            ledger
                .commit(fact, &registry)
                .expect("a real drive-movements fact always commits");
        }
    }
    let wall = t0.elapsed();
    let facts_after = ledger.len();
    let searches_after = home_nav_cache.searches();

    let facts_delta = facts_after - facts_before;
    let searches_delta = searches_after - searches_before;
    let wall_ms = wall.as_secs_f64() * 1000.0;

    let total_bytes = ledger_bytes(&ledger);
    Row {
        agents: n,
        ms_per_tick: wall_ms / TICKS as f64,
        facts_per_agent_per_tick: facts_delta as f64 / (n as f64 * TICKS as f64),
        searches_per_agent_per_tick: searches_delta as f64 / (n as f64 * TICKS as f64),
        bytes_per_agent: total_bytes as f64 / n as f64,
        total_bytes,
        facts_delta,
        searches_delta,
        wall_ms,
    }
}

fn main() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 must build under default pins");
    let ctx = LocaleContext::build(&world).expect("seed 42 locale context must build");

    let settlement_count = hornvale_settlement::all_settlements(&world).len();
    println!(
        "agent_scaling: seed 42 has {settlement_count} settlements (the ceiling derive_npcs's k can reach)"
    );

    let home_settlement = mint_flagship(&world, &ctx)
        .expect("seed 42's flagship always mints")
        .village
        .id;

    // The planet's rotation period, exactly the read `Session::start` makes
    // (`self.calendar.as_ref().and_then(|c| c.day_length()).map(|d|
    // d.get())`) — `None` on a tidally-locked world.
    let day_length_std = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned())
        .and_then(|c| c.day_length())
        .map(|d| d.get());

    let mut rows: Vec<Row> = Vec::new();
    println!(
        "{:>8} {:>10} {:>14} {:>14} {:>16} {:>14} {:>10} {:>10}",
        "agents",
        "ms/tick",
        "facts/a/tick",
        "search/a/tick",
        "bytes/agent",
        "total_bytes",
        "facts",
        "searches"
    );
    for &k in RUNGS {
        if k > settlement_count {
            println!(
                "agent_scaling: SKIPPING k={k} — exceeds the world's {settlement_count} settlements"
            );
            continue;
        }
        let row = run_rung(&world, &ctx, home_settlement, day_length_std, k);
        println!(
            "{:>8} {:>10.3} {:>14.4} {:>14.4} {:>16.1} {:>14} {:>10} {:>10}",
            row.agents,
            row.ms_per_tick,
            row.facts_per_agent_per_tick,
            row.searches_per_agent_per_tick,
            row.bytes_per_agent,
            row.total_bytes,
            row.facts_delta,
            row.searches_delta,
        );
        rows.push(row);
    }

    let xs: Vec<f64> = rows.iter().map(|r| r.agents as f64).collect();
    let ys: Vec<f64> = rows.iter().map(|r| r.ms_per_tick).collect();
    println!();
    println!(
        "fitted log-log slope, ms/tick vs agent count (1.0 linear, 2.0 quadratic): {:.2}",
        log_log_slope(&xs, &ys)
    );

    // The MARGINAL bytes/agent between the smallest and largest rung run --
    // `bytes_per_agent` at any single rung is dominated by the fixed
    // genesis ledger size divided by a small `agents`, so it is NOT the
    // right number for "what does one more agent cost"; this difference,
    // by contrast, nets out that fixed numerator.
    if rows.len() >= 2 {
        let first = &rows[0];
        let last = rows.last().expect("len >= 2");
        let marginal = (last.total_bytes as f64 - first.total_bytes as f64)
            / (last.agents as f64 - first.agents as f64);
        println!(
            "marginal bytes/agent (total_bytes[{}]-total_bytes[{}])/(agents[{}]-agents[{}]) = {:.1}",
            last.agents, first.agents, last.agents, first.agents, marginal
        );
    }

    if let Some(last) = rows.last() {
        // The query/plan/commit split at the largest rung actually run.
        // `plan` and `commit` are measured directly (see the module doc);
        // `query` is the RESIDUAL of the total wall time -- it is not an
        // independent measurement and absorbs everything this driver has
        // no counter for (drive evaluation, the alarm field build,
        // `WalkState` folds, occupancy bookkeeping).
        println!();
        println!(
            "query/plan/commit split at the largest rung run (agents={}, {} ticks):",
            last.agents, TICKS
        );
        println!(
            "  plan   (HomeNavCache::searches delta) = {} searches",
            last.searches_delta
        );
        println!(
            "  commit (ledger.len() delta)           = {} facts",
            last.facts_delta
        );
        println!(
            "  query  = RESIDUAL (this driver exposes no query counter --\n\
             \x20          see the module doc's \"which term is a RESIDUAL\" section).\n\
             \x20          Total wall for this rung: {:.3} ms over {} ticks.",
            last.wall_ms, TICKS
        );
    } else {
        println!(
            "agent_scaling: no rung ran -- every RUNGS candidate exceeded the settlement ceiling"
        );
    }
}
