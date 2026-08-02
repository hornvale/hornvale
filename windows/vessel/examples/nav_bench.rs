//! The nav bench (the-waymark, Task 7): massive-scale synthetic subjects
//! walking the NAV SEAM ONLY (no drives/affect, no `Session`) against the
//! kernel's [`hornvale_kernel::Solver`] shelf, per backend, at rungs
//! 10..1,000,000. INFORMATIVE, NEVER A GATE — its purpose is to convert the
//! packed-address question and the D*-Lite/HPA* shelf (see the campaign
//! spec) into measured cases, and it is EXPECTED to hit real limits at the
//! top rungs; a rung that cannot complete in a sane budget is itself the
//! finding (recorded, not hidden).
//!
//! Run: `cargo run --release -p hornvale-vessel --example nav_bench`
//! (always `--release` — a debug-mode A* at N=1,000,000 measures the
//! optimizer, not the algorithm).
//!
//! ## Design
//!
//! **One real seed-42 world.** `build_world` (the same call `profile_build`
//! makes) plus [`LocaleContext::build`] — the sanctioned derivation site
//! (decision 0092) — give a real `walk_depth` (`globe_level() + 6`, the
//! production constant [`hornvale_vessel::walk_depth`] computes) so the
//! synthetic room mesh this bench walks is the SAME depth production nav
//! addresses live at, not an arbitrary one. Neither `build_world` nor
//! `LocaleContext::build` is itself one of the three 0092-banned methods
//! (`terrain_of`/`climate_from`/`demography_report_from`) — `LocaleContext::
//! build` already carries its own scoped allow at ITS call site inside
//! `windows/locale`, so calling it from here needs no `#[allow]` of our own;
//! verified empirically (`cargo clippy --all-targets -- -D warnings` is
//! clean with none added — see the task report).
//!
//! **Synthetic subjects, not `HomeNavCache`/`NavSpace` directly.** The
//! production nav space (`NavSpace`) and the memo-threading entry point
//! (`plan_to_room_memo`) are `windows/vessel::liveness` private items (only
//! `plan_to_room`, `HomeNavCache::new`, and the `Solver`/`SearchSpace`
//! kernel traits are `pub`) — an example is a separate crate compiled
//! against the public API only, same as an external consumer, so it cannot
//! reach them. [`BenchNavSpace`] below is a from-scratch, deliberately
//! FAITHFUL reimplementation: same state (`RoomAddr`), same action shape
//! (the destination room), same uniform cost (`1`, matching `move_cost`
//! with an always-empty avoid set — `NavSpace`'s own doc: "Empty ⇒
//! byte-identical"), same zero heuristic, same `successors_memo` override
//! consulting a caller `RoomMeshMemo`. The only thing NOT reproduced is the
//! avoid-set/remembered-danger cost bump, which this bench never exercises
//! (every subject here has an empty avoid set), so cost differences below
//! trace to the solver/caching STRATEGY alone, matching every backend's own
//! framing.
//!
//! **Subject placement: a bounded local random walk from one shared hub.**
//! `dest` is one fixed room (`RoomAddr::containing([1.0, 0.0, 0.0],
//! walk_depth)`) — every subject in every backend shares it, which is what
//! backend D's "single-source per shared goal" framing means concretely.
//! Each subject's `start` is a `[HOP_MIN, HOP_MAX]`-hop walk from `dest`,
//! stepping to a uniformly-picked one of the 3 same-depth neighbours each
//! hop, drawn from a dedicated bench-local stream
//! (`Seed(42).derive(StreamLabel::dynamic("nav_bench/subjects"))` — NOT the
//! world's own genesis stream, so this draws nothing any save-format
//! contract owns; mirrors `kernel/examples/first_light.rs`'s use of
//! `StreamLabel::dynamic` in exactly this "an example, not a domain" role).
//! `HOP_MAX = 20` keeps every subject within `PLAN_BUDGET`'s own local-
//! journey scale (`windows/vessel::liveness::PLAN_BUDGET = 1_000`, mirrored
//! here as [`BUDGET`]) — this bench measures SUBJECT COUNT scaling (the
//! campaign's actual question), not search-distance scaling, so search
//! distance is deliberately kept representative-and-fixed rather than
//! grown alongside `N`. All 1,000,000 subjects are generated ONCE up front
//! from one stream draw sequence; each rung reads the first `N` of that
//! same list, so rungs nest (the N=100 subjects are a prefix of the
//! N=1,000 subjects) and results across rungs are directly comparable.
//!
//! **Four backends, same [`BenchNavSpace`]/subjects, differing only in
//! solver + caching strategy:**
//! - `astar-fresh` — [`AStarSolver`], `memo: None`, one call per subject.
//!   The uncached floor: every call recomputes `RoomAddr::neighbors()` from
//!   scratch (an O(depth) icosphere-subdivision walk) on every expansion.
//! - `astar-memo` — [`AStarSolver`] with ONE [`RoomMeshMemo`] shared across
//!   all `N` subjects' searches (Task 6's win: a memo hit skips the
//!   subdivision walk entirely for a room another subject's search already
//!   visited — subjects cluster within `HOP_MAX` hops of the same hub, so
//!   the memo's key space is bounded regardless of `N`).
//! - `cached-mix` — a bench-local reimplementation of `HomeNavCache`'s KEY
//!   IDEA (not the private type itself — see [`run_cached_mix`]'s own doc):
//!   [`CACHE_TICKS`] simulated ticks; tick 0 is a cold miss for every
//!   subject, ticks 1.. hold 90% of subjects STATIONARY (a cache hit, no
//!   search at all) and move the other 10% one more hop (a cache miss, a
//!   real search) — the marginal-cost story `HomeNavCache` exists for.
//! - `field` — [`FieldSolver`], otherwise identical to `astar-memo`. Per
//!   its own kernel doc, `FieldSolver` explores the WHOLE region reachable
//!   within `budget` before returning (never stopping early at the first
//!   goal pop the way `AStarSolver` does), so at this bench's local
//!   `HOP_MAX`-scale searches it pays close to the FULL `budget` on every
//!   call regardless of how close the goal actually is — this is expected
//!   to read WORSE than `astar-memo`, not better, and that is the finding:
//!   `FieldSolver`'s one-build-answers-many potential (Task 5's disabled
//!   `ReverseField`, rooted at the destination with edges reversed) is not
//!   what this `Solver::solve(..., start, ...)` signature exposes — it
//!   only ever answers ONE `start` per call, so sharing a goal across
//!   subjects buys nothing through this API. That is precisely why
//!   `home_nav` stays on `AStarSolver` alone (kernel/src/astar.rs's
//!   `FieldSolver` doc, "the equivalence null").
//!
//! **Timing: `std::time::Instant`, scoped-allowed.** `clippy.toml` bans it
//! workspace-wide (wall-clock must never leak into a WORLD — decision
//! 0001); this file times an out-of-sim EXAMPLE the same way
//! `windows/worldgen/src/lib.rs`'s `stage` fn (the `profile_build`
//! machinery) already does, with the identical scoped-allow shape. A
//! file-level allow is used here (every fn in this file is profiling
//! machinery, unlike `worldgen/src/lib.rs` where `stage` is one function
//! amid production code) — the same "test fixture, blanket allow" posture
//! decision 0092's own survey applies to a whole test module.
#![allow(
    clippy::disallowed_types,
    reason = "std::time::Instant times an out-of-sim profiling EXAMPLE (decision 0001 bans wall-clock IN a world; nothing here builds one) — the same scoped posture windows/worldgen/src/lib.rs's `stage` fn already uses for profile_build"
)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{
    AStarSolver, FieldSolver, RoomAddr, RoomMeshMemo, SearchSpace, Seed, Solver, Stream,
};
use hornvale_locale::LocaleContext;
use hornvale_terrain::TerrainPins;
use hornvale_vessel::walk_depth;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
use std::time::{Duration, Instant};

/// Subject-count rungs (the campaign's own ladder: 10 through 1,000,000).
const RUNGS: &[usize] = &[10, 100, 1_000, 10_000, 100_000, 1_000_000];

/// The search budget, mirroring production's `PLAN_BUDGET`
/// (`windows/vessel::liveness::PLAN_BUDGET = 1_000`, private — duplicated
/// here as a literal since the bench cannot import a private const; keep
/// the two numbers in sync by hand if `PLAN_BUDGET` ever changes).
const BUDGET: usize = 1_000;

/// Minimum hops a subject's start is walked from the shared hub.
const HOP_MIN: u32 = 1;

/// Maximum hops a subject's start is walked from the shared hub — well
/// inside `BUDGET`, so every backend can find every subject's plan; this
/// bench measures SUBJECT-COUNT scaling, not search-distance scaling.
const HOP_MAX: u32 = 20;

/// Simulated ticks for the `cached-mix` backend (tick 0 = cold fill for
/// every subject; ticks 1.. run the 90%-stationary/10%-moving mix).
const CACHE_TICKS: usize = 5;

/// A rung whose PROJECTED wall (extrapolated from the previous rung's
/// measured per-subject cost) exceeds this is skipped rather than run —
/// the "sane budget" the brief names at 10 minutes.
const PER_RUNG_CEILING_SECS: f64 = 600.0;

/// Stop a backend's ladder entirely once measured `VmHWM` crosses this
/// fraction of the box's total memory — "approaching memory limits" made
/// concrete, checked after every rung actually run.
const MEM_CEILING_FRACTION: f64 = 0.80;

/// A from-scratch, deliberately faithful stand-in for `windows/vessel::
/// liveness::NavSpace` (private to that crate — see the module doc for
/// why this bench cannot use the real one). Goal is arrival at `dest`;
/// every move costs 1 (the real `NavSpace`'s cost with an always-empty
/// avoid set, which is all this bench ever exercises).
struct BenchNavSpace {
    /// The single shared destination every subject in every backend plans
    /// toward (backend D's "single-source per shared goal").
    dest: RoomAddr,
}

impl SearchSpace for BenchNavSpace {
    type State = RoomAddr;
    type Action = RoomAddr;

    fn successors(&self, s: &RoomAddr) -> Vec<(RoomAddr, RoomAddr, u64)> {
        s.neighbors()
            .into_iter()
            .map(|n| (n.clone(), n, 1))
            .collect()
    }

    fn successors_memo(
        &self,
        s: &RoomAddr,
        memo: Option<&mut RoomMeshMemo>,
    ) -> Vec<(RoomAddr, RoomAddr, u64)> {
        let neighbors = match memo {
            Some(m) => s.neighbors_memo(m),
            None => s.neighbors(),
        };
        neighbors.into_iter().map(|n| (n.clone(), n, 1)).collect()
    }

    fn goal(&self, s: &RoomAddr) -> bool {
        *s == self.dest
    }

    fn heuristic(&self, _s: &RoomAddr) -> u64 {
        0
    }
}

/// Generate `n` subjects' start addresses: each is a `[HOP_MIN, HOP_MAX]`-hop
/// walk from `dest`, one uniformly-picked same-depth neighbour per hop, all
/// draws from `stream` (a dedicated bench-local stream — see the module
/// doc). Sequential and deterministic: the same `stream` state produces the
/// same `n` subjects every run.
fn generate_subjects(dest: &RoomAddr, n: usize, stream: &mut Stream) -> Vec<RoomAddr> {
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        let hops = stream.range_u32(HOP_MIN, HOP_MAX);
        let mut cur = dest.clone();
        for _ in 0..hops {
            let ns = cur.neighbors();
            cur = stream
                .pick(&ns)
                .expect("neighbors() always returns 3")
                .clone();
        }
        out.push(cur);
    }
    out
}

/// `VmHWM` from `/proc/self/status`, in KiB — the process's peak resident
/// set size SO FAR (Linux-only; `None` elsewhere). This is a WATERMARK
/// across the whole process, not an isolated per-rung reading: every
/// backend/rung in this one run shares a process, so a later rung's
/// reading can never be smaller than an earlier one's even if that rung
/// itself frees memory. Read as "peak RSS up to and including this point",
/// not "this rung's own footprint".
fn read_vm_hwm_kb() -> Option<u64> {
    let status = std::fs::read_to_string("/proc/self/status").ok()?;
    for line in status.lines() {
        if let Some(rest) = line.strip_prefix("VmHWM:") {
            return rest.split_whitespace().next()?.parse().ok();
        }
    }
    None
}

/// `MemTotal` from `/proc/meminfo`, in KiB (Linux-only; `None` elsewhere) —
/// the basis for [`MEM_CEILING_FRACTION`].
fn read_mem_total_kb() -> Option<u64> {
    let meminfo = std::fs::read_to_string("/proc/meminfo").ok()?;
    for line in meminfo.lines() {
        if let Some(rest) = line.strip_prefix("MemTotal:") {
            return rest.split_whitespace().next()?.parse().ok();
        }
    }
    None
}

/// One backend/rung's measured (or projected) outcome.
struct BenchResult {
    /// Total wall time for every query this call made.
    wall: Duration,
    /// Total per-subject-per-tick queries (`N` for the three single-tick
    /// backends; `N * CACHE_TICKS` for `cached-mix`) — the marginal-cost
    /// denominator.
    queries: u64,
    /// How many real `Solver::solve` calls were actually executed (a cache
    /// hit in `cached-mix` costs a query but not a search).
    searches: u64,
    /// How many of those searches found a plan within `BUDGET`.
    found: u64,
}

/// Backend A — the uncached floor: one fresh `AStarSolver` call per
/// subject, no memo, no cache. Every call recomputes `RoomAddr::neighbors`
/// from scratch on every expansion.
fn run_astar_fresh(space: &BenchNavSpace, subjects: &[RoomAddr]) -> BenchResult {
    let mut searches = 0u64;
    let mut found = 0u64;
    let t0 = Instant::now();
    for s in subjects {
        searches += 1;
        if AStarSolver.solve(space, s.clone(), BUDGET, None).is_some() {
            found += 1;
        }
    }
    BenchResult {
        wall: t0.elapsed(),
        queries: subjects.len() as u64,
        searches,
        found,
    }
}

/// Backend B — `AStarSolver` sharing ONE `RoomMeshMemo` across all
/// subjects' searches (Task 6's win).
fn run_astar_memo(space: &BenchNavSpace, subjects: &[RoomAddr]) -> BenchResult {
    let mut memo = RoomMeshMemo::new();
    let mut searches = 0u64;
    let mut found = 0u64;
    let t0 = Instant::now();
    for s in subjects {
        searches += 1;
        if AStarSolver
            .solve(space, s.clone(), BUDGET, Some(&mut memo))
            .is_some()
        {
            found += 1;
        }
    }
    BenchResult {
        wall: t0.elapsed(),
        queries: subjects.len() as u64,
        searches,
        found,
    }
}

/// Backend D — `FieldSolver`, otherwise identical call shape to
/// `run_astar_memo` (see the module doc for why this measures, and is
/// expected to fail to realize, an "amortized" story).
fn run_field(space: &BenchNavSpace, subjects: &[RoomAddr]) -> BenchResult {
    let mut memo = RoomMeshMemo::new();
    let mut searches = 0u64;
    let mut found = 0u64;
    let t0 = Instant::now();
    for s in subjects {
        searches += 1;
        if FieldSolver
            .solve(space, s.clone(), BUDGET, Some(&mut memo))
            .is_some()
        {
            found += 1;
        }
    }
    BenchResult {
        wall: t0.elapsed(),
        queries: subjects.len() as u64,
        searches,
        found,
    }
}

/// Backend C — a bench-local reimplementation of `HomeNavCache`'s KEY IDEA
/// (per-entity "did pos/goal change since last time" memoization), NOT the
/// private type itself (its one real method, `home_nav`, is not `pub` —
/// only `HomeNavCache::new` is, so nothing outside `windows/vessel::
/// liveness` can invoke the actual cache; hence "-style" in the backend's
/// name, matching the task brief's own wording). Simulates `CACHE_TICKS`
/// ticks: tick 0 is a cold miss for every subject; ticks 1.. hold 90% of
/// subjects stationary (`i % 10 != 0`, a cache hit — the position is
/// unchanged since the last real search, so no search runs at all) and
/// move the other 10% one further hop (a cache miss, a real search) — the
/// marginal-cost story the real cache exists for ("a stationary,
/// unchanged-belief creature pays ZERO searches on ticks after its
/// first"). Shares one `RoomMeshMemo` across every real search, same as
/// `run_astar_memo`, since production's `HomeNavCache` threads one through
/// too.
fn run_cached_mix(
    space: &BenchNavSpace,
    subjects: &[RoomAddr],
    move_stream: &mut Stream,
) -> BenchResult {
    let n = subjects.len();
    let mut memo = RoomMeshMemo::new();
    let mut positions: Vec<RoomAddr> = subjects.to_vec();
    let mut cached_pos: Vec<Option<RoomAddr>> = vec![None; n];
    let mut searches = 0u64;
    let mut found = 0u64;
    let t0 = Instant::now();
    for tick in 0..CACHE_TICKS {
        for i in 0..n {
            if tick > 0 && i % 10 == 0 {
                let ns = positions[i].neighbors();
                positions[i] = move_stream
                    .pick(&ns)
                    .expect("neighbors() always returns 3")
                    .clone();
            }
            if cached_pos[i].as_ref() == Some(&positions[i]) {
                continue; // cache hit: no search at all
            }
            searches += 1;
            if AStarSolver
                .solve(space, positions[i].clone(), BUDGET, Some(&mut memo))
                .is_some()
            {
                found += 1;
            }
            cached_pos[i] = Some(positions[i].clone());
        }
    }
    BenchResult {
        wall: t0.elapsed(),
        queries: (n * CACHE_TICKS) as u64,
        searches,
        found,
    }
}

/// One printed/ledgered row — either a real measurement or a recorded skip.
struct Row {
    backend: &'static str,
    n: usize,
    wall_s: Option<f64>,
    marginal_us: Option<f64>,
    searches: Option<u64>,
    found: Option<u64>,
    vm_hwm_kb: Option<u64>,
    note: String,
}

/// Drive one backend across [`RUNGS`]: run for real while the projected
/// cost (extrapolated from the last rung actually run) stays under
/// [`PER_RUNG_CEILING_SECS`], skip-and-record once it doesn't (skips only
/// grow more certain at larger `N`, so nothing after the first skip is
/// attempted), and stop the whole backend early if `VmHWM` crosses
/// [`MEM_CEILING_FRACTION`] of the box's memory.
fn drive_ladder<F>(
    backend: &'static str,
    subjects_all: &[RoomAddr],
    mem_ceiling_kb: Option<u64>,
    mut run: F,
) -> Vec<Row>
where
    F: FnMut(&[RoomAddr]) -> BenchResult,
{
    let mut rows = Vec::new();
    let mut prev: Option<(usize, f64)> = None;
    for &n in RUNGS {
        if let Some((pn, pw)) = prev
            && pw > 0.0
        {
            let projected = pw / pn as f64 * n as f64;
            if projected > PER_RUNG_CEILING_SECS {
                println!(
                    "nav_bench: {backend:<12} N={n:>8} SKIPPED — projected {projected:.1}s exceeds the {PER_RUNG_CEILING_SECS:.0}s per-rung ceiling (extrapolated from N={pn} at {pw:.3}s)"
                );
                rows.push(Row {
                    backend,
                    n,
                    wall_s: None,
                    marginal_us: None,
                    searches: None,
                    found: None,
                    vm_hwm_kb: None,
                    note: format!(
                        "SKIPPED: projected {projected:.1}s > {PER_RUNG_CEILING_SECS:.0}s ceiling"
                    ),
                });
                continue;
            }
        }
        let subjects = &subjects_all[..n];
        let result = run(subjects);
        let wall_s = result.wall.as_secs_f64();
        let marginal_us = wall_s / result.queries as f64 * 1e6;
        let vm_hwm = read_vm_hwm_kb();
        let vm_hwm_str = vm_hwm
            .map(|k| format!("{:.1}MB", k as f64 / 1024.0))
            .unwrap_or_else(|| "n/a".to_string());
        println!(
            "nav_bench: {backend:<12} N={n:>8} wall={wall_s:>10.3}s marginal={marginal_us:>9.3}us searches={:>9} found={:>9} vm_hwm={vm_hwm_str}",
            result.searches, result.found
        );
        let mut note = String::new();
        let mut abort = false;
        if let (Some(ceiling_kb), Some(hwm)) = (mem_ceiling_kb, vm_hwm)
            && hwm > ceiling_kb
        {
            note = format!(
                "ABORTED further rungs: VmHWM {hwm}KB exceeds the {:.0}% memory ceiling ({ceiling_kb}KB)",
                MEM_CEILING_FRACTION * 100.0
            );
            println!("nav_bench: {backend} N={n} {note}");
            abort = true;
        }
        rows.push(Row {
            backend,
            n,
            wall_s: Some(wall_s),
            marginal_us: Some(marginal_us),
            searches: Some(result.searches),
            found: Some(result.found),
            vm_hwm_kb: vm_hwm,
            note,
        });
        if abort {
            break;
        }
        prev = Some((n, wall_s));
    }
    rows
}

fn main() {
    let overall_start = Instant::now();
    println!("nav_bench: building the seed-42 world (default pins, full depth)...");
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 must build under default pins");
    // Named construction site (decision 0092): `LocaleContext::build` is
    // itself the sanctioned entry point and carries its own scoped allow at
    // ITS call site inside windows/locale — nothing here calls
    // terrain_of/climate_from/demography_report_from directly, so no
    // `#[allow(clippy::disallowed_methods)]` is needed at THIS call (see
    // the module doc's "Design" section for the empirical check).
    let ctx = LocaleContext::build(&world).expect("seed 42 locale context must build");
    let depth = walk_depth(&ctx);
    println!(
        "nav_bench: globe_level={} walk_depth={depth} (production nav depth)",
        ctx.globe_level()
    );

    let dest = RoomAddr::containing([1.0, 0.0, 0.0], depth);
    let max_n = *RUNGS.iter().max().expect("RUNGS is non-empty");
    let mut subject_stream = Seed(42)
        .derive(StreamLabel::dynamic("nav_bench/subjects"))
        .stream();
    println!(
        "nav_bench: generating {max_n} synthetic subjects ({HOP_MIN}-{HOP_MAX}-hop local walks from the shared hub)..."
    );
    let gen_t0 = Instant::now();
    let all_subjects = generate_subjects(&dest, max_n, &mut subject_stream);
    println!(
        "nav_bench: subject generation took {:.3}s",
        gen_t0.elapsed().as_secs_f64()
    );

    let space = BenchNavSpace { dest: dest.clone() };
    let mem_total_kb = read_mem_total_kb();
    let mem_ceiling_kb = mem_total_kb.map(|t| (t as f64 * MEM_CEILING_FRACTION) as u64);
    match (mem_total_kb, mem_ceiling_kb) {
        (Some(t), Some(c)) => {
            println!("nav_bench: MemTotal={t}KB memory ceiling={c}KB ({MEM_CEILING_FRACTION:.0})")
        }
        _ => println!(
            "nav_bench: /proc/meminfo unavailable — no memory ceiling armed (non-Linux host?)"
        ),
    }

    let mut all_rows: Vec<Row> = Vec::new();

    println!("\n== backend: astar-fresh (uncached floor) ==");
    all_rows.extend(drive_ladder(
        "astar-fresh",
        &all_subjects,
        mem_ceiling_kb,
        |s| run_astar_fresh(&space, s),
    ));

    println!("\n== backend: astar-memo (shared RoomMeshMemo) ==");
    all_rows.extend(drive_ladder(
        "astar-memo",
        &all_subjects,
        mem_ceiling_kb,
        |s| run_astar_memo(&space, s),
    ));

    println!("\n== backend: cached-mix (HomeNavCache-style, 90% stationary) ==");
    let mut move_stream = Seed(42)
        .derive(StreamLabel::dynamic("nav_bench/cache-moves"))
        .stream();
    all_rows.extend(drive_ladder(
        "cached-mix",
        &all_subjects,
        mem_ceiling_kb,
        |s| run_cached_mix(&space, s, &mut move_stream),
    ));

    println!("\n== backend: field (FieldSolver, single-source per shared goal) ==");
    all_rows.extend(drive_ladder("field", &all_subjects, mem_ceiling_kb, |s| {
        run_field(&space, s)
    }));

    println!(
        "\nnav_bench: total wall for the whole ladder: {:.3}s",
        overall_start.elapsed().as_secs_f64()
    );

    println!("\n| backend | N | wall_s | marginal_us | searches | found | vm_hwm_mb | note |");
    println!("|---|---|---|---|---|---|---|---|");
    for row in &all_rows {
        println!(
            "| {} | {} | {} | {} | {} | {} | {} | {} |",
            row.backend,
            row.n,
            row.wall_s
                .map(|v| format!("{v:.3}"))
                .unwrap_or_else(|| "-".into()),
            row.marginal_us
                .map(|v| format!("{v:.3}"))
                .unwrap_or_else(|| "-".into()),
            row.searches
                .map(|v| v.to_string())
                .unwrap_or_else(|| "-".into()),
            row.found
                .map(|v| v.to_string())
                .unwrap_or_else(|| "-".into()),
            row.vm_hwm_kb
                .map(|v| format!("{:.1}", v as f64 / 1024.0))
                .unwrap_or_else(|| "-".into()),
            row.note,
        );
    }
}
