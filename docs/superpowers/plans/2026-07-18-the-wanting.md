# The Wanting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give The Quickening's NPCs a *why*: a homeostatic drive (a need with memory, derived as a fold over the `agent-at` history), a hysteresis controller, and an agent that moves to close the gap — the destination becomes a *resource*, the trip carries a dated reason, and the player can feel its own drive. GOAP's seams are reserved (interface shape only) so the future planner is a body-swap.

**Architecture:** New game-layer code extending `windows/vessel/src/liveness.rs` (The Quickening's home). The drive-state is *derived* (a fold over the committed `agent-at` log — never stored, the seam rule). Movement is decided by `decide(npc, view, t) -> Intent` — the degenerate one-goal case of goal-selection-then-planning — and committed at **closed-form threshold crossings** (a discrete-event tick, exact, Lorenz-safe) with drive-naming provenance. Genesis byte-identical, census-free, domains untouched. Spec: `docs/superpowers/specs/2026-07-18-the-wanting-design.md` (G3-approved; the reserved-seams refinement, decision #9, is in scope).

**Tech Stack:** Rust edition 2024, std + serde/serde_json only. `cargo nextest`.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only.
- **No new dependencies**; **no wall-clock time**; **no RNG** (the drive dynamics, crossings, and moves are pure functions of seed + committed history + time — surprise is epistemic).
- **Domains untouched** (The Walk §11): all new code is in `windows/vessel`; `cli/tests/architecture.rs` enforces layering — do not modify any `domains/*` crate.
- **Reserved seams are INTERFACE SHAPE ONLY (decision #9):** a `Goal` enum with one variant, one `decide` fn, one `view` param. **Do NOT build a planner, action graph, cost function, or arbitration.** Over-reserving (a speculative GOAP framework) is a plan failure — the seam must read as an obvious minimal shape, not a half-built engine.
- **Every public item gets a one-line doc comment + a `type-audit:` verdict tag** (struct/fn-level, not per-field — the c5 convention); type-audit runs in `make gate`.
- **`cargo fmt` last before every commit**; clippy `-D warnings` clean. Commit trailer: `Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm`.
- **Byte-identical genesis (spec §3):** the campaign commits NO drive fact and no drive-driven `agent-at` at world build; genesis worlds, the census, and all gallery/laboratory artifacts stay byte-identical. After each task, `git diff --stat book/src/laboratory/ cli/tests/fixtures/` must be empty (the ONLY expected book change is Task 4's re-baselined *game-layer* over-time transcript — never a census/laboratory/fixture drift). If a genesis artifact drifts, STOP.
- **Mutation-verify every new test** before counting it done (break the guarded code, see RED, restore, see GREEN); report it.
- **The NO-THRASH guard (the campaign's characteristic risk):** a memoryless controller thrashes (a move every tick). A test MUST assert a long `wait` commits only the *few* genuine drive-cycle crossings, and mutation-verify by collapsing the two thresholds (`SATED = ACT`) → the test reds.
- Worktree: `.claude/worktrees/the-wanting`, branch `worktree-the-wanting`.

---

### Task 0: Baseline gate

**Files:** none modified.

- [ ] **Step 1: Preflight**

Run: `make preflight`
Expected: GO. If main moved, merge main INTO `worktree-the-wanting`, re-run the gate, note the absorption.

- [ ] **Step 2: Baseline suite**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-w-baseline.txt`
Expected: exit 0. If red, STOP and diff against main.

- [ ] **Step 3: fmt + clippy baseline**

Run: `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: clean. No commit.

---

### Task 1: The resource anchor + the drive-state fold (pure, additive)

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (add `resource_room`, `Drive`, `drive_at`; do NOT change `Npc`/`AgentMovements`/`scheduled_position` yet — additive only)

**Interfaces:**
- Consumes: The Quickening's `AGENT_AT`, `room_to_text`/`room_from_text`, `Npc` (read-only); `hornvale_kernel::{Ledger, RoomAddr, WorldTime, EntityId, Value}`; `hornvale_locale::LocaleContext` with `describe(&RoomAddr, WorldTime) -> Result<Locale, _>` where `Locale.fields.elevation_m: f64`.
- Produces:
  - `pub fn resource_room(home: &RoomAddr, ctx: &LocaleContext) -> RoomAddr` — the lowest-elevation of `home`'s three mesh neighbors ("toward water"), ties broken by `RoomAddr` order.
  - `pub struct DriveParams { pub rise: f64, pub fall: f64, pub act: f64, pub sated: f64, pub initial: f64 }` with an authored `pub const SUSTENANCE: DriveParams`.
  - `pub fn drive_at(ledger: &Ledger, entity: EntityId, resource: &RoomAddr, t: WorldTime, p: &DriveParams) -> f64` — the drive in `[0,1]`, a fold over the entity's committed `agent-at` history up to `t`: rises at `p.rise`/day while away from `resource`, falls at `p.fall`/day while at it, clamped `[0,1]`, starting from `p.initial` before any move.

- [ ] **Step 1: Write the failing tests** (`windows/vessel/src/liveness.rs` tests)

```rust
#[test]
fn drive_rises_while_away_and_is_clamped() {
    // No agent-at history: the entity has been "away" since day 0 at rate rise.
    let p = DriveParams { rise: 0.1, fall: 0.5, act: 0.8, sated: 0.2, initial: 0.0 };
    let ledger = Ledger::default();
    let e = EntityId::new(1).unwrap();
    let res = RoomAddr::containing([1.0, 0.0, 0.0], 6).neighbors()[0].clone();
    assert!((drive_at(&ledger, e, &res, WorldTime { day: 3.0 }, &p) - 0.3).abs() < 1e-9);
    // clamps at 1.0
    assert_eq!(drive_at(&ledger, e, &res, WorldTime { day: 100.0 }, &p), 1.0);
}

#[test]
fn drive_falls_while_at_the_resource() {
    // History: at the resource since day 2. Drive rose to 0.2 by day 2, then falls.
    let p = DriveParams { rise: 0.1, fall: 0.5, act: 0.8, sated: 0.2, initial: 0.0 };
    let mut ledger = Ledger::default();
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    let e = ledger.mint_entity();
    let res = RoomAddr::containing([1.0, 0.0, 0.0], 6).neighbors()[0].clone();
    // commit: at resource on day 2
    ledger.commit(hornvale_kernel::Fact {
        subject: e, predicate: AGENT_AT.to_string(), object: Value::Text(room_to_text(&res)),
        place: None, day: Some(2.0), provenance: "test".into(),
    }, &reg).unwrap();
    // by day 2 drive was ~0.2 (rose from 0 at 0.1/day); at day 3 (1 day at resource) it fell 0.5 -> ~0 (floored)
    let d = drive_at(&ledger, e, &res, WorldTime { day: 3.0 }, &p);
    assert!(d <= 0.001, "drive should fall to ~0 after a day at the resource, got {d}");
}

#[test]
fn drive_at_is_deterministic_and_reload_stable() {
    // Fold determinism: same ledger + t -> same value; and serialize->reload of
    // the ledger yields the identical drive (the DRIVE == FOLD contract).
    let p = SUSTENANCE;
    let mut ledger = Ledger::default();
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    let e = ledger.mint_entity();
    let res = RoomAddr::containing([1.0, 0.0, 0.0], 6).neighbors()[0].clone();
    for day in [1.0, 4.0, 9.0] {
        ledger.commit(hornvale_kernel::Fact {
            subject: e, predicate: AGENT_AT.to_string(), object: Value::Text(room_to_text(&res)),
            place: None, day: Some(day), provenance: "t".into(),
        }, &reg).unwrap();
    }
    let t = WorldTime { day: 12.3 };
    let a = drive_at(&ledger, e, &res, t, &p);
    let b = drive_at(&ledger, e, &res, t, &p);
    assert_eq!(a, b);
    let json = serde_json::to_string(&ledger).unwrap();
    let reloaded: Ledger = serde_json::from_str(&json).unwrap();
    assert_eq!(drive_at(&reloaded, e, &res, t, &p), a, "drive re-derives identically after reload");
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib liveness::tests::drive`
Expected: compile error (`drive_at`, `DriveParams`, `resource_room` missing).

- [ ] **Step 3: Implement** (in `liveness.rs`)

```rust
/// The homeostatic-drive parameters (authored constants; §4.2/§4.3): the rise
/// rate while away from the resource, the fall (satiety) rate while at it, and
/// the hysteresis thresholds `act` (seek) and `sated` (leave), plus the
/// pre-history initial value. Dimensionless; the drive lives in [0, 1].
/// type-audit: bare-ok(ratio: rise), bare-ok(ratio: fall), bare-ok(ratio: act), bare-ok(ratio: sated), bare-ok(ratio: initial)
#[derive(Clone, Copy, Debug)]
pub struct DriveParams {
    /// Drive gained per day while away from the resource.
    pub rise: f64,
    /// Drive lost per day while at the resource (satiety).
    pub fall: f64,
    /// The seek threshold: drive >= act -> go to the resource.
    pub act: f64,
    /// The leave threshold: drive <= sated -> return home.
    pub sated: f64,
    /// The drive before any committed move.
    pub initial: f64,
}

/// The one authored sustenance drive (thirst/foraging). act > sated (the
/// hysteresis dead-band); rates chosen so a cycle spans a few days.
pub const SUSTENANCE: DriveParams =
    DriveParams { rise: 0.15, fall: 0.6, act: 0.85, sated: 0.15, initial: 0.0 };

/// The resource cell the drive seeks: the lowest-elevation of `home`'s three
/// mesh neighbors ("toward water"), ties broken by `RoomAddr` order for
/// determinism. Reads the locale elevation field.
/// type-audit: bare-ok(return)
pub fn resource_room(home: &RoomAddr, ctx: &LocaleContext) -> RoomAddr {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in home.neighbors() {
        let elev = ctx
            .describe(&n, WorldTime { day: 0.0 })
            .map(|loc| loc.fields.elevation_m)
            .unwrap_or(f64::INFINITY);
        match &best {
            Some((ba, be)) if (elev, &n) >= (*be, ba) => {}
            _ => best = Some((n, elev)),
        }
    }
    best.expect("a room has three neighbors").0
}

/// The drive at `t`: a fold over the entity's committed `agent-at` history.
/// Rises `p.rise`/day away from `resource`, falls `p.fall`/day at it, clamped to
/// [0, 1], starting from `p.initial` before any committed move. A pure derived
/// view over the ledger (never stored) — DRIVE == FOLD.
/// type-audit: bare-ok(ratio: return)
pub fn drive_at(
    ledger: &Ledger,
    entity: EntityId,
    resource: &RoomAddr,
    t: WorldTime,
    p: &DriveParams,
) -> f64 {
    // The entity's dated agent-at history, ascending by day (commit order is
    // ascending day within a session; sort defensively).
    let mut hist: Vec<(f64, RoomAddr)> = ledger
        .find(AGENT_AT)
        .filter(|f| f.subject == entity)
        .filter_map(|f| match (&f.object, f.day) {
            (Value::Text(s), Some(day)) => Some((day, room_from_text(s))),
            _ => None,
        })
        .collect();
    hist.sort_by(|a, b| a.0.total_cmp(&b.0));

    // Walk the timeline from day 0, integrating the piecewise-linear drive.
    let mut drive = p.initial;
    let mut cursor = 0.0_f64;
    // Segment before the first move: position is the NPC's *home* (unknown here);
    // treat "away from resource" as the default (drive rises) unless the first
    // fact is at the resource. We reconstruct position per segment from history.
    let mut at_resource = false; // NPCs start at home (not the resource)
    for (day, pos) in hist.iter().filter(|(d, _)| *d <= t.day) {
        drive = integrate(drive, at_resource, day - cursor, p);
        cursor = *day;
        at_resource = pos == resource;
    }
    // Final segment to t.
    integrate(drive, at_resource, t.day - cursor, p)
}

fn integrate(drive: f64, at_resource: bool, dt: f64, p: &DriveParams) -> f64 {
    let delta = if at_resource { -p.fall * dt } else { p.rise * dt };
    (drive + delta).clamp(0.0, 1.0)
}
```

**Implementer notes:** confirm `LocaleContext::describe` returns a `Locale` with `.fields.elevation_m` (grep `windows/locale/src/lib.rs`); adjust the call if the signature differs. The `drive_falls_while_at_the_resource` test's expected value depends on the exact integration — compute it by hand from the params and assert with a tolerance; if your integration convention differs, fix the *test's arithmetic*, not the physics, and say so. `room_from_text`/`room_to_text` are The Quickening's (private — you're in the same module).

- [ ] **Step 4: Run + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel --lib liveness::`
Expected: PASS. Mutation-verify: flip `integrate`'s sign (rise while at resource) → `drive_falls_while_at_the_resource` reds. Restore.

```bash
git diff --stat book/src/laboratory/ cli/tests/fixtures/   # MUST be empty
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): the resource anchor + the drive-state fold (pure, derived) (the-wanting T1)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 2: The reserved seam — `Goal`/`Intent`/`view` + `decide` (hysteresis) + closed-form crossings

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`Goal`, `Intent`, `Perceived`, `decide`, `next_crossing`)

**Interfaces:**
- Consumes: Task 1's `drive_at`/`DriveParams`/`resource_room`; `RoomAddr`, `WorldTime`.
- Produces:
  - `pub enum Goal { AtResource }` — the desired world-state (the sole goal this slice; decision #9).
  - `pub struct Perceived { pub position: RoomAddr, pub drive: f64 }` — what the agent perceives (the `view`; ground-truth body now, belief later).
  - `pub enum Intent { GoTo(RoomAddr), Hold }` — the decision's output; the tick depends only on this.
  - `pub fn decide(home: &RoomAddr, resource: &RoomAddr, view: &Perceived, p: &DriveParams) -> Intent` — the degenerate one-goal case of goal-selection-then-planning: seek `resource` when `view.drive >= act` and not there; return `home` when at `resource` and `view.drive <= sated`; else `Hold`.
  - `pub fn next_crossing(from_day: f64, drive0: f64, at_resource: bool, p: &DriveParams) -> Option<f64>` — the closed-form day the drive next reaches the relevant threshold (`act` if rising toward it, `sated` if falling), or `None` if it never will in this segment.

- [ ] **Step 1: Write the failing tests** (append to `liveness.rs` tests)

```rust
fn addr(seed: f64) -> RoomAddr { RoomAddr::containing([seed, 0.0, 0.0], 6) }

#[test]
fn decide_seeks_when_parched_and_returns_when_sated() {
    let p = SUSTENANCE;
    let home = addr(1.0);
    let resource = home.neighbors()[0].clone();
    // parched (drive >= act), at home -> go to resource
    let v = Perceived { position: home.clone(), drive: 0.9 };
    assert_eq!(decide(&home, &resource, &v, &p), Intent::GoTo(resource.clone()));
    // at resource, sated (drive <= sated) -> go home
    let v = Perceived { position: resource.clone(), drive: 0.1 };
    assert_eq!(decide(&home, &resource, &v, &p), Intent::GoTo(home.clone()));
    // in the dead-band (sated < drive < act) -> hold, wherever you are
    let v = Perceived { position: home.clone(), drive: 0.5 };
    assert_eq!(decide(&home, &resource, &v, &p), Intent::Hold);
    let v = Perceived { position: resource.clone(), drive: 0.5 };
    assert_eq!(decide(&home, &resource, &v, &p), Intent::Hold);
}

#[test]
fn next_crossing_is_closed_form_and_exact() {
    let p = DriveParams { rise: 0.1, fall: 0.5, act: 0.8, sated: 0.2, initial: 0.0 };
    // away, drive 0.0, rises at 0.1/day -> reaches act 0.8 at day 8.
    assert!((next_crossing(0.0, 0.0, false, &p).unwrap() - 8.0).abs() < 1e-9);
    // at resource, drive 0.8, falls at 0.5/day -> reaches sated 0.2 at day 1.2.
    assert!((next_crossing(0.0, 0.8, true, &p).unwrap() - 1.2).abs() < 1e-9);
    // already past the threshold in the direction of travel -> immediate (day = from_day)
    assert_eq!(next_crossing(5.0, 0.9, false, &p), Some(5.0));
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib liveness::tests::decide`
Expected: compile error.

- [ ] **Step 3: Implement**

```rust
/// A desired world-state — the agent's active goal. This slice generates exactly
/// one goal (the drive's setpoint); it is named so that "the drive *generates a
/// goal*" is explicit. The future GOAP planner selects among many and plans to
/// reach the chosen one (decision #9 — reserved seam, not built here).
/// type-audit: bare-ok(return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Goal {
    /// Be at the resource (satisfy the sustenance drive).
    AtResource,
}

/// What the agent perceives of the world — the `view` the decision reads. Today
/// its contents are ground truth; PSY-6's "plan over belief, not truth" (UNI-16)
/// is later a change to what fills this, not to the seam.
/// type-audit: bare-ok(ratio: drive)
#[derive(Clone, Debug)]
pub struct Perceived {
    /// The agent's current room.
    pub position: RoomAddr,
    /// The agent's perceived drive level.
    pub drive: f64,
}

/// The decision's output: where the agent intends to be. The tick depends ONLY
/// on this — never on the drive internals — so the decision body can be replaced
/// (by the GOAP planner) without touching the caller.
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    /// Move toward this room.
    GoTo(RoomAddr),
    /// Stay put (the hysteresis dead-band).
    Hold,
}

/// The reactive controller (decision #9): the degenerate one-goal case of
/// goal-selection-then-planning. Generate the sole goal when the drive crosses
/// `act`, plan the one-step route to it (go to the resource), and return home
/// when sated — with a dead-band (`sated < act`) between to prevent thrashing.
/// type-audit: bare-ok(return)
pub fn decide(home: &RoomAddr, resource: &RoomAddr, view: &Perceived, p: &DriveParams) -> Intent {
    let at_resource = &view.position == resource;
    if !at_resource && view.drive >= p.act {
        Intent::GoTo(resource.clone()) // goal generated: AtResource; one-step plan
    } else if at_resource && view.drive <= p.sated {
        Intent::GoTo(home.clone()) // goal satisfied; return
    } else {
        Intent::Hold // dead-band
    }
}

/// The closed-form day the drive next reaches its governing threshold from
/// `(from_day, drive0)`: `act` while away (rising), `sated` while at the
/// resource (falling). `Some(from_day)` if already past it in the travel
/// direction; `None` if the rate is zero and it never arrives.
/// type-audit: bare-ok(return)
pub fn next_crossing(from_day: f64, drive0: f64, at_resource: bool, p: &DriveParams) -> Option<f64> {
    if at_resource {
        if drive0 <= p.sated {
            return Some(from_day);
        }
        if p.fall <= 0.0 {
            return None;
        }
        Some(from_day + (drive0 - p.sated) / p.fall)
    } else {
        if drive0 >= p.act {
            return Some(from_day);
        }
        if p.rise <= 0.0 {
            return None;
        }
        Some(from_day + (p.act - drive0) / p.rise)
    }
}
```

- [ ] **Step 4: Run + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel --lib liveness::`
Expected: PASS. Mutation-verify: in `decide`, drop the dead-band (return `GoTo(resource)` whenever `!at_resource`, ignoring `act`) → `decide_seeks_when_parched...`'s hold case reds. Restore.

```bash
git diff --stat book/src/laboratory/ cli/tests/fixtures/   # empty
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): the decide() seam (Goal/Intent/view) + hysteresis + closed-form crossings — GOAP reserved (the-wanting T2)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 3: The drive-driven tick + provenance; supersede the clock model; wire the session

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`DriveMovements` TickSystem; `derive_npcs` sets the resource anchor; remove the superseded clock `scheduled_position`/`AgentMovements` + their now-obsolete unit tests, keeping `active_phase` if unused-remove it too)
- Modify: `windows/vessel/src/session.rs` (use `DriveMovements`; NPCs carry their resource)
- Modify: `windows/vessel/tests/possession_moves.rs` (the observed-movement tests, now drive-driven)

**Interfaces:**
- Consumes: Tasks 1–2. The Quickening's session-owned evolving `ledger`/`registry`/`npcs`, the `wait` verb, `agent_position`.
- Produces: `pub struct DriveMovements { pub npcs: Vec<Npc>, pub from: WorldTime, pub to: WorldTime, pub params: DriveParams }` impl `TickSystem` — over `(from, to]`, computes each NPC's threshold crossings in order (via `next_crossing` + `decide`) and emits a dated `agent-at` per crossing with provenance naming the drive. `Npc` gains `pub resource: RoomAddr` (set in `derive_npcs`).

- [ ] **Step 1: Write the failing tests** (append to `liveness.rs` tests + edit `possession_moves.rs`)

```rust
#[test]
fn a_long_wait_commits_only_the_few_genuine_crossings_not_one_per_tick() {
    // THE NO-THRASH GUARD (the campaign's characteristic risk). One NPC, a 40-day
    // wait. With SUSTENANCE (rise 0.15, fall 0.6, act 0.85, sated 0.15): a cycle
    // is ~ (0.85/0.15) away + (0.7/0.6) at-resource ≈ 5.67 + 1.17 ≈ 6.8 days, so
    // ~40/6.8 ≈ 5-6 cycles -> ~10-12 crossings, NOT 40. Assert the bound.
    let p = SUSTENANCE;
    let mut ledger = Ledger::default();
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    let e = ledger.mint_entity();
    let home = addr(1.0);
    let resource = home.neighbors()[0].clone();
    let npc = Npc { entity: e, home: home.clone(), resource: resource.clone(),
                    activity: hornvale_species::ActivityCycle::Diurnal, label: "herder".into() };
    let sys = DriveMovements { npcs: vec![npc.clone()], from: WorldTime { day: 0.0 },
                               to: WorldTime { day: 40.0 }, params: p };
    let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
    let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
    assert!(moves >= 6 && moves <= 16, "expected a few genuine crossings, got {moves}");
    // consecutive moves alternate resource<->home (no resource->resource thrash)
    let positions: Vec<RoomAddr> = next.find(AGENT_AT).filter(|f| f.subject == e)
        .filter_map(|f| match &f.object { Value::Text(s) => Some(room_from_text(s)), _ => None })
        .collect();
    for w in positions.windows(2) { assert_ne!(w[0], w[1], "no repeated position (thrash)"); }
    // post-tick position matches decide at t=40 (jump coherence, extended)
    let drive40 = drive_at(&next, e, &resource, WorldTime { day: 40.0 }, &p);
    let intent = decide(&home, &resource, &Perceived { position: agent_position(&next, &npc, WorldTime{day:40.0}), drive: drive40 }, &p);
    // if intent is Hold, position is already coherent; if GoTo, it equals the target
    if let Intent::GoTo(target) = intent {
        assert_eq!(agent_position(&next, &npc, WorldTime { day: 40.0 }), target);
    }
    let _ = ledger;
}

#[test]
fn moves_carry_drive_naming_provenance() {
    let p = SUSTENANCE;
    let mut ledger = Ledger::default();
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    let e = ledger.mint_entity();
    let home = addr(1.0); let resource = home.neighbors()[0].clone();
    let npc = Npc { entity: e, home: home.clone(), resource: resource.clone(),
                    activity: hornvale_species::ActivityCycle::Diurnal, label: "herder".into() };
    let sys = DriveMovements { npcs: vec![npc], from: WorldTime{day:0.0}, to: WorldTime{day:10.0}, params: p };
    let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
    let first = next.find(AGENT_AT).find(|f| f.subject == e).unwrap();
    assert!(first.provenance.contains("thirst") || first.provenance.contains("water") || first.provenance.contains("sustenance"),
            "provenance names the drive: {}", first.provenance);
    let _ = ledger;
}
```

Update `possession_moves.rs`: the existing "waiting moves an NPC and it is observed" tests should still pass under the drive model (the co-located NPC still departs/returns on a `wait`), but the exact `wait` duration to observe a move may change — adjust the wait interval so a drive cycle completes, and keep the assertions (a move happens; the co-located NPC is perceived by name; determinism). Preserve the day-0-unchanged and byte-determinism tests.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel liveness::tests::a_long_wait`
Expected: compile error (`DriveMovements`, `Npc.resource` missing).

- [ ] **Step 3: Implement**

**Rename `Npc.destination` → `Npc.resource`** (semantically it IS the resource now — no vestigial field); in `derive_npcs`, set `resource: resource_room(&home, ctx)` (the lowest-elevation neighbor) instead of `deterministic_destination`. Update every `Npc { … }` literal (in `liveness.rs` + `possession_moves.rs`) to the renamed field. Implement `DriveMovements`:

```rust
/// The drive-driven movement system (spec §4.4): over (from, to], advance each
/// NPC through its closed-form drive-threshold crossings in order, committing a
/// dated `agent-at` at each crossing with provenance naming the drive. A
/// discrete-event tick (exact, no integrator). Run through c6's `tick`.
/// type-audit: bare-ok(return)
pub struct DriveMovements {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The interval start (the session's previous day).
    pub from: WorldTime,
    /// The interval end (the session's new day).
    pub to: WorldTime,
    /// The drive parameters.
    pub params: DriveParams,
}

impl hornvale_kernel::TickSystem for DriveMovements {
    fn label(&self) -> &'static str { "drive-movements" }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        let mut out = Vec::new();
        for npc in &self.npcs {
            // Reconstruct the NPC's state at `from` from the frozen history.
            let mut pos = agent_position(frozen, npc, self.from);
            let mut day = self.from.day;
            let mut drive = drive_at(frozen, npc.entity, &npc.resource, self.from, &self.params);
            // Advance crossing by crossing until past `to`.
            loop {
                let at_resource = pos == npc.resource;
                let Some(cross) = next_crossing(day, drive, at_resource, &self.params) else { break };
                if cross > self.to.day { break; }
                // At the crossing the drive is exactly at its threshold; decide.
                let drive_at_cross = if at_resource { self.params.sated } else { self.params.act };
                let intent = decide(&npc.home, &npc.resource,
                    &Perceived { position: pos.clone(), drive: drive_at_cross }, &self.params);
                match intent {
                    Intent::GoTo(target) if target != pos => {
                        out.push(Fact {
                            subject: npc.entity, predicate: AGENT_AT.to_string(),
                            object: Value::Text(room_to_text(&target)),
                            place: None, day: Some(cross),
                            provenance: "sought water (thirst)".to_string(),
                        });
                        pos = target;
                        drive = drive_at_cross; // reset to threshold at the move
                        day = cross;
                    }
                    _ => break, // Hold at a crossing shouldn't happen; guard against a loop
                }
            }
        }
        out
    }
}
```

**Supersession:** the session (`session.rs`) switches from `AgentMovements`/
`scheduled_position` (the clock model) to `DriveMovements`. Remove
`AgentMovements`, `scheduled_position`, and their now-dead unit tests (keep
`active_phase` only if the drive still uses it as a waking gate — the spec §4.3
made the gate optional; **for this first slice, drop the gate: the drive is the
sole mover**, so remove `active_phase` too if nothing else references it). In
`session.rs`'s `wait`: build `DriveMovements { npcs, from: <old day>, to:
<new day>, params: SUSTENANCE }` and run it through `tick`. The narration
(co-located NPC by name) is preserved from The Quickening — it keys off position
change, which still happens.

- [ ] **Step 4: Run + full suite + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel` then `cargo nextest run --workspace 2>&1 | tee /tmp/hv-w-t3.txt`
Expected: PASS. Mutation-verify the NO-THRASH guard: set `params.sated = params.act` (collapse the dead-band) → `a_long_wait_commits_only_the_few...` reds (thrash). Restore.

```bash
git diff --stat book/src/laboratory/ cli/tests/fixtures/   # MUST be empty (genesis untouched)
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): the drive-driven tick — discrete-event crossings, drive provenance; supersede the clock model (the-wanting T3)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 4: The felt state + why + close

**Files:**
- Modify: `windows/vessel/src/session.rs` (a `needs`/`self` verb rendering the possessed agent's felt drive; the `why` provenance already surfaces via `recount`)
- Create: the felt-state test (in `possession_moves.rs` or a session test)
- Re-baseline: `book/src/gallery/possession-over-time-seed-42.md` (game-layer transcript — drive movement + why + felt state)
- Create: `book/src/chronicle/the-wanting.md` (+ SUMMARY.md); `docs/retrospectives/the-wanting.md`
- Modify: `book/src/frontier/idea-registry.md` (PSY-6 re-score), stale "fixed clock schedule" book lines, the spec header (STATUS)

- [ ] **Step 1: The felt-state read (of the NPCs, not the player)**

Add a `needs` verb to the session's `match verb` that renders the **co-located
NPCs'** felt drive as diegetic prose (never a number): for each NPC the possessed
agent perceives, `drive >= act` → "The {label} looks parched." ; `drive <= sated`
→ "The {label} seems content." ; else → "The {label} could do with a drink."
Compute each via `drive_at(&self.ledger, npc.entity, &npc.resource, self.day,
&SUSTENANCE)`.

**Why the NPCs and not the player (a plan correction to spec §4.5):** the
possessed agent's own moves are NOT committed as `agent-at` (only NPC moves are;
player movement is committed only under Campaign IV, deferred). So `drive_at` for
the player would fold an empty history — the player would be *eternally parched*,
a meaningless drive. The honest first slice makes the drive visible on the
**NPCs** (whose moves ARE committed, so their drive is real); the player's own
felt drive is a followup that rides player-acts-mutate (Campaign IV). This still
delivers "the drive is visible" and pairs with the `why` recount.

Test that `needs` after a `wait` reports a co-located NPC's felt state, and that
the state DIFFERS across a drive cycle (parched before its trip, content after).
Mutation-verify: fix the drive to a constant → the "differs across a cycle"
assertion reds.

- [ ] **Step 2: `why` surfaces the drive**

Confirm `why <npc>` (the existing `recount`) now shows the drive-driven moves with their provenance ("sought water (thirst)"). Add/extend a test asserting the recount of a moved NPC names the drive reason. Mutation-verify (blank the provenance → the assertion reds).

- [ ] **Step 3: Re-baseline the over-time transcript + final byte-identity**

```bash
bash scripts/regenerate-artifacts.sh
git status --porcelain
```
Expected: ONLY `book/src/gallery/possession-over-time-seed-42.md` changed (the game-layer transcript now shows drive-driven movement + `needs` + `why`). If any census/laboratory/fixture/almanac artifact drifts, STOP (genesis must be byte-identical). Then:

```bash
git diff $(git merge-base HEAD main)..HEAD --stat -- book/src/laboratory/ cli/tests/fixtures/ book/src/gallery/almanac-seed-42-sky.md | cat   # EMPTY
make gate 2>&1 | tail -20
cargo nextest run --workspace 2>&1 | tee /tmp/hv-w-final.txt
```
Expected: `make gate` green; full suite exit 0; the genesis diff empty.

- [ ] **Step 4: Chronicle** (`book/src/chronicle/the-wanting.md`)

At the book's altitude: the puppet becomes an agent — the first NPC that moves because it *wants*, not because a clock says. The drive as a fold over history (a derived view, like belief); the thermostat dead-band; the destination becoming a resource; the discrete-event tick (jump to the next crossing); the reserved GOAP seam (the planner is a body-swap away — name what's deferred: arbitration, A*, planning-over-belief, the psychology-vector cost function); the shadow posture (drive lives only in the possess session; genesis byte-identical). Add the SUMMARY.md line. `mdbook build book` clean.

- [ ] **Step 5: Freshness sweep + PSY-6 re-score + retro**

- `grep -rn "fixed clock schedule\|clock schedule\|no reason\|stateless\|activity-cycle" book/src/` — update the vessel/liveness chapters that describe NPC movement as a fixed clock (now it is drive-driven).
- `book/src/frontier/idea-registry.md`: re-score **PSY-6** — the *drive rung* shipped (goal-generation from a homeostatic setpoint), the *goal rung* (A* planner, arbitration, cost function) deferred; the reserved `decide`/`view` seams. Note UNI-20 gains the drive as a derived view; cross-ref UNI-16 (the `view` becomes belief later). Edit ONLY this worktree's copy.
- `book/src/open-questions.md`: check the "someone walks in it" / living-world bet — the first inner-state/motivation slice likely MOVES it; re-score per decision 0030 if so, and SAY SO (or say why not).
- `cargo test -p hornvale --test docs_consistency` — PASS.
- Retrospective `docs/retrospectives/the-wanting.md` (one page, process): the GOAP-boundary reasoning (cardinality-1 → no planner); ideonomy's two passes (drive = fold = belief; the thermostat dead-band); the reserved-seam discipline (interface shape, not a speculative engine); the clock→drive supersession; the 7 followup rows. Spec header STATUS line.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A && git commit -m "docs(the-wanting): close — felt state, why, chronicle, PSY-6 re-score, retrospective (the-wanting T4)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

**STOP after this task: G6 is a hard stop.** Present the post-G3 ledger digest (the determinism entry #7 + the reserved-seam #9 leading) to Nathan; the FF, push, pull, and teardown are his calls, under `closing-a-campaign`.

---

## Execution notes for the dispatcher

- Subagent-driven (dispatching-hornvale-subagents): sonnet floor, dispatch
  preamble verbatim, `cd` into the worktree, challenge-response on return.
  **Watch for parking** — resume via SendMessage.
- `make preflight` at every task boundary; absorb main INTO the branch on NO-GO.
- Stages 1–2 are pure/additive (non-breaking); Stage 3 is the integration +
  supersession of The Quickening's clock model (the churn concentrates there);
  Stage 4 re-baselines only the game-layer transcript (genesis byte-identical).
- The reserved seam (`decide`/`Goal`/`view`) is INTERFACE SHAPE ONLY — a reviewer
  should reject any speculative planner/action-graph/cost machinery as scope creep.
