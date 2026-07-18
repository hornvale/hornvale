# The Quickening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The world's first autonomous motion: while you possess an agent and `wait`, a few derived NPCs walk their deterministic daily routes (home settlement ⇌ a neighbor room, on their species' activity-cycle), each move committed as a dated `agent-at` event via c6's tick, perceived on re-query — genesis byte-identical, census-free.

**Architecture:** New game-layer code in `windows/vessel` only (domains untouched, The Walk §11). NPC positions over time are a *derived* pure schedule (the reversible routine); the discrete position *changes* are *committed* as dated `agent-at` facts via the kernel's `tick` (the irreversible, remembered happenings — the seam rule). The possess session gains an owned, evolving `Ledger` + `ConceptRegistry` (its `wait` verb was a no-op; now it advances `WorldTime`, runs the movement tick, and re-focalizes). Spec: `docs/superpowers/specs/2026-07-18-the-quickening-design.md` (G3-approved; actor revised to NPC movement per ledger #8).

**Tech Stack:** Rust edition 2024, std + serde/serde_json only. `cargo nextest`.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (clippy-enforced).
- **No new dependencies**; **no wall-clock time**; **no RNG** (routes are pure functions of seed + time — surprise is epistemic, not stochastic, The Walk §3.3).
- **Domains untouched** (The Walk §11): all new code is in `windows/vessel`; `cli/tests/architecture.rs` enforces layering — do not modify any `domains/*` crate.
- **Every public item gets a one-line doc comment** + a `type-audit:` verdict tag (struct/fn-level, NOT per-field — the c5 convention); type-audit runs in `make gate`.
- **`cargo fmt` last before every commit**; clippy `-D warnings` clean. Commit trailer: `Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm`.
- **Byte-identical genesis (spec §3):** the campaign commits NO `agent-at` fact at world build; genesis worlds, the census, and all gallery/laboratory artifacts stay byte-identical. After each task, `git diff --stat book/ cli/tests/fixtures/` must be empty (the ONLY exception is Task 4's *new* over-time transcript file — never a diff of an existing one). If an existing artifact drifts, STOP.
- **Mutation-verify every new test** before counting it done (break the guarded code, see RED, restore, see GREEN); report it.
- **The anti-inert guard (ledger #8):** the demography actor died because it was inert (no movement). A test MUST assert the derived NPCs actually move (position differs between rest and active phase). Do not ship an actor that doesn't move.
- Worktree: `.claude/worktrees/the-quickening`, branch `worktree-the-quickening`.

---

### Task 0: Baseline gate

**Files:** none modified.

- [ ] **Step 1: Preflight**

Run: `make preflight`
Expected: GO. If main moved, merge main INTO `worktree-the-quickening`, re-run the gate, note the absorption.

- [ ] **Step 2: Baseline suite**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-q-baseline.txt`
Expected: exit 0. If red, STOP and diff against main.

- [ ] **Step 3: fmt + clippy baseline**

Run: `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: clean. No commit.

---

### Task 1: NPC derivation + the daily route (pure, derived)

**Files:**
- Create: `windows/vessel/src/liveness.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod liveness;`)

**Interfaces:**
- Consumes: `hornvale_kernel::{World, RoomAddr, WorldTime, EntityId}`; `crate::agent::{Agent, mint_flagship, walk_depth}`; `crate::locale::LocaleContext` (however the session builds it — mirror `Session::start`); `hornvale_species::ActivityCycle`; the possessed agent carries `perception.activity: ActivityCycle`.
- Produces:
  - `struct Npc { pub entity: EntityId, pub home: RoomAddr, pub destination: RoomAddr, pub activity: hornvale_species::ActivityCycle, pub label: String }`
  - `pub fn active_phase(activity: ActivityCycle, t: WorldTime) -> bool` — true iff the species is active at the day-fraction of `t`.
  - `pub fn scheduled_position(npc: &Npc, t: WorldTime) -> RoomAddr` — `destination` if active, else `home`.
  - `pub fn derive_npcs(world: &World, ctx: &LocaleContext, ledger: &mut hornvale_kernel::Ledger, k: usize) -> Vec<Npc>` — mint `k` NPC entities from the top-`k` settlements (by `POPULATION`), each home = its settlement cell's room, destination = a deterministic neighbor, activity from its species.

- [ ] **Step 1: Write the failing tests** (`windows/vessel/src/liveness.rs` `tests`)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, World, WorldTime};

    fn diurnal_npc() -> Npc {
        // A hand-built NPC for the pure-schedule tests (no world needed).
        let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
        let destination = home.neighbors()[0];
        Npc {
            entity: hornvale_kernel::EntityId::new(1).unwrap(),
            home,
            destination,
            activity: hornvale_species::ActivityCycle::Diurnal,
            label: "herder".to_string(),
        }
    }

    #[test]
    fn active_phase_diurnal_is_day_not_night() {
        // day fraction 0.5 = noon (active for diurnal); 0.0 = midnight (rest).
        assert!(active_phase(hornvale_species::ActivityCycle::Diurnal, WorldTime { day: 3.5 }));
        assert!(!active_phase(hornvale_species::ActivityCycle::Diurnal, WorldTime { day: 3.0 }));
    }

    #[test]
    fn scheduled_position_moves_between_home_and_destination() {
        // THE ANTI-INERT GUARD (ledger #8): the actor must actually move.
        let npc = diurnal_npc();
        let at_noon = scheduled_position(&npc, WorldTime { day: 3.5 });
        let at_midnight = scheduled_position(&npc, WorldTime { day: 3.0 });
        assert_eq!(at_noon, npc.destination);
        assert_eq!(at_midnight, npc.home);
        assert_ne!(at_noon, at_midnight, "the NPC must move between phases");
    }

    #[test]
    fn scheduled_position_is_deterministic_and_total() {
        let npc = diurnal_npc();
        // splitmix over t: every t yields a defined position, equal on repeat.
        let mut st = 1u64;
        for _ in 0..200 {
            st = st.wrapping_mul(6364136223846793005).wrapping_add(1);
            let t = WorldTime { day: (st % 100_000) as f64 / 1000.0 };
            assert_eq!(scheduled_position(&npc, t), scheduled_position(&npc, t));
        }
    }

    #[test]
    fn derive_npcs_are_distinct_and_placed() {
        let world = World::new(Seed(42)); // an empty world has no settlements...
        // Use the real worldgen build for a populated world:
        let world = hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap();
        let ctx = crate::locale::LocaleContext::build(&world).unwrap();
        let mut ledger = world.ledger.clone();
        let npcs = derive_npcs(&world, &ctx, &mut ledger, 3);
        assert_eq!(npcs.len(), 3);
        // distinct entities, and each moves (home != destination)
        let ids: std::collections::BTreeSet<_> = npcs.iter().map(|n| n.entity).collect();
        assert_eq!(ids.len(), 3);
        for n in &npcs {
            assert_ne!(n.home, n.destination, "NPC {} must have a real route", n.label);
        }
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib liveness::`
Expected: compile error (types missing). That is the red.

- [ ] **Step 3: Implement** (`windows/vessel/src/liveness.rs`)

```rust
//! The Quickening: the world's first autonomous motion. NPCs derived like the
//! possessed agent walk deterministic daily routes; their position over time is
//! a pure schedule (derived, reversible — the routine), and the discrete
//! changes are committed as dated `agent-at` events via c6's tick (the
//! remembered happenings). New game-layer code; domains untouched (The Walk §11).
use crate::agent::walk_depth;
use crate::locale::LocaleContext;
use hornvale_kernel::{EntityId, Ledger, RoomAddr, World, WorldTime};
use hornvale_species::ActivityCycle;

/// A derived non-player agent: a minted entity, a home and a destination room,
/// and its species' activity-cycle. Derived from the genesis world, never
/// stored (re-derivable).
/// type-audit: bare-ok(identifier-text: label)
#[derive(Clone, Debug)]
pub struct Npc {
    /// The NPC's minted ledger entity (subject of its `agent-at` facts).
    pub entity: EntityId,
    /// Where the NPC rests (its home settlement's room).
    pub home: RoomAddr,
    /// Where the NPC goes when active (a deterministic neighbor room).
    pub destination: RoomAddr,
    /// The species activity-cycle driving the routine.
    pub activity: ActivityCycle,
    /// A short human label for prose ("the herder").
    pub label: String,
}

/// Is the species active at the day-fraction of `t`? Diurnal → active in the
/// daylight half [0.25, 0.75); nocturnal → the complement; crepuscular → the
/// twilight bands. A pure, periodic function (Lorenz-safe).
/// type-audit: bare-ok(flag: return)
pub fn active_phase(activity: ActivityCycle, t: WorldTime) -> bool {
    let frac = t.day - t.day.floor(); // day fraction in [0, 1); 0.5 == noon
    match activity {
        ActivityCycle::Diurnal => (0.25..0.75).contains(&frac),
        ActivityCycle::Nocturnal => !(0.25..0.75).contains(&frac),
        // crepuscular: active at the dawn/dusk bands
        ActivityCycle::Crepuscular => {
            (0.20..0.30).contains(&frac) || (0.70..0.80).contains(&frac)
        }
    }
}

/// The NPC's scheduled position at `t`: its destination when active, else home.
/// Pure and deterministic — the derived routine (never committed).
/// type-audit: bare-ok(return)
pub fn scheduled_position(npc: &Npc, t: WorldTime) -> RoomAddr {
    if active_phase(npc.activity, t) {
        npc.destination
    } else {
        npc.home
    }
}

/// Derive `k` NPCs from the `k` most-populous settlements. Each NPC is minted in
/// `ledger` (a session-owned clone), homed at its settlement's cell room, with a
/// deterministic destination neighbor and its species' activity-cycle.
/// type-audit: bare-ok(count: k)
pub fn derive_npcs(world: &World, ctx: &LocaleContext, ledger: &mut Ledger, k: usize) -> Vec<Npc> {
    // Settlements + populations from the ledger (POPULATION facts), top-k by pop,
    // ties broken by the settlement's EntityId for determinism.
    let mut settlements: Vec<(EntityId, f64)> = world
        .ledger
        .find(hornvale_settlement::POPULATION)
        .filter_map(|f| match f.object {
            hornvale_kernel::Value::Number(p) => Some((f.subject, p)),
            _ => None,
        })
        .collect();
    settlements.sort_by(|a, b| {
        b.1.total_cmp(&a.1).then(a.0.cmp(&b.0)) // pop desc, then id asc (deterministic)
    });
    settlements.truncate(k);

    settlements
        .into_iter()
        .map(|(settlement, _pop)| {
            let home = settlement_room(world, ctx, settlement);
            let destination = deterministic_destination(home);
            let species = hornvale_settlement::species_of(world, settlement)
                .unwrap_or_else(|| "goblin".to_string());
            let activity = species_activity(world, &species);
            let entity = ledger.mint_entity();
            Npc {
                entity,
                home,
                destination,
                activity,
                label: format!("{species} of the settlement"),
            }
        })
        .collect()
}

/// The room containing a settlement's cell at walk depth (mirrors `mint_flagship`).
fn settlement_room(world: &World, ctx: &LocaleContext, settlement: EntityId) -> RoomAddr {
    // Read the settlement's committed cell position (lat/long or cell-id -> [f64;3]).
    // Reuse whatever `mint_flagship` uses to turn a settlement into a [f64;3]
    // position, then `RoomAddr::containing(pos, walk_depth(ctx))`.
    // (Plan-detail: the implementer factors the position lookup out of
    // mint_flagship or calls the same helper; the point is the SAME derivation.)
    let pos = crate::agent::settlement_position(world, settlement); // factor this out of mint_flagship
    RoomAddr::containing(pos, walk_depth(ctx))
}

/// A deterministic destination neighbor of `home` — the smallest of its three
/// mesh neighbors by address (a fixed, seed-independent, total-order pick).
fn deterministic_destination(home: RoomAddr) -> RoomAddr {
    home.neighbors().into_iter().min().expect("a room has three neighbors")
}

/// The species' activity-cycle, from its committed `SPECIES_ACTIVITY_CYCLE` fact.
fn species_activity(world: &World, species: &str) -> ActivityCycle {
    // Resolve the species entity, read its activity-cycle fact; default Diurnal.
    // (Plan-detail: mirror how `mint_flagship` reads the species' perception —
    // `hornvale_species::species_entity(world, species)` then the activity fact,
    // or read `perception.activity` from the species' perception vector.)
    hornvale_species::species_entity(world, species)
        .and_then(|e| match world.ledger.value_of(e, hornvale_species::SPECIES_ACTIVITY_CYCLE) {
            Some(hornvale_kernel::Value::Text(t)) => Some(parse_activity(t)),
            _ => None,
        })
        .unwrap_or(ActivityCycle::Diurnal)
}

fn parse_activity(t: &str) -> ActivityCycle {
    match t {
        "nocturnal" => ActivityCycle::Nocturnal,
        "crepuscular" => ActivityCycle::Crepuscular,
        _ => ActivityCycle::Diurnal,
    }
}
```

**Implementer notes:** `RoomAddr` must be `Ord` for `.min()` and the `BTreeSet` —
if it is not, order by a derived key (`.neighbors()` returns `[RoomAddr; 3]`;
pick index 0 if no order exists, still deterministic). Factor
`settlement_position(world, settlement) -> [f64; 3]` out of `mint_flagship`
(the lat/long → unit-vector it already does) rather than duplicating it —
**that refactor of `agent.rs` is in scope** and must keep `mint_flagship`
byte-identical (the frozen possess transcript must not drift). Confirm the exact
`ActivityCycle` variant names and the activity-fact text encoding against
`domains/species/src/lib.rs`; adjust `parse_activity` to match.

- [ ] **Step 4: Run to pass + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel --lib liveness::`
Expected: PASS. Mutation-verify the anti-inert guard: make `scheduled_position` always return `home` → `scheduled_position_moves_between_phases` and `active_phase_diurnal...` must RED. Restore.

```bash
git diff --stat book/ cli/tests/fixtures/   # MUST be empty (mint_flagship refactor kept byte-identical)
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): derived NPCs + deterministic daily route (pure) (the-quickening T1)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 2: The movement `TickSystem` + `agent_position` read + genesis-zero pin

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`AGENT_AT`, `AgentMovements`, `agent_position`)
- Test: `windows/vessel/tests/liveness_genesis.rs` (the genesis-zero pin)

**Interfaces:**
- Consumes: Task 1's `Npc`/`scheduled_position`; `hornvale_kernel::{TickSystem, Fact, Value, Ledger, WorldTime, EntityId}`.
- Produces:
  - `pub const AGENT_AT: &str = "agent-at";` (non-functional; registered by the session, not genesis).
  - `pub struct AgentMovements { pub npcs: Vec<Npc>, pub at_time: WorldTime }` impl `TickSystem`.
  - `pub fn agent_position(ledger: &Ledger, npc: &Npc, t: WorldTime) -> RoomAddr` — latest committed `agent-at` for `npc` ELSE `scheduled_position(npc, t)`.

- [ ] **Step 1: Write the failing tests** (append to `liveness.rs` `tests`)

```rust
fn registry_with_agent_at() -> hornvale_kernel::ConceptRegistry {
    let mut r = hornvale_kernel::ConceptRegistry::default();
    r.register_predicate(AGENT_AT, false, "an agent's position on a day").unwrap();
    r
}

#[test]
fn tick_commits_agent_at_when_the_npc_moves() {
    let r = registry_with_agent_at();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
    let npc = Npc { entity: e, home, destination: home.neighbors()[0],
                    activity: hornvale_species::ActivityCycle::Diurnal, label: "herder".into() };
    // Advance to noon (active) -> the NPC is at its destination -> one agent-at fact.
    let sys = AgentMovements { npcs: vec![npc.clone()], at_time: WorldTime { day: 0.5 } };
    let next = hornvale_kernel::tick(&ledger, &[&sys], &["agent-movements"], &r).unwrap();
    assert_eq!(agent_position(&next, &npc, WorldTime { day: 0.5 }), npc.destination);
    assert_eq!(next.find(AGENT_AT).count(), 1);
    ledger = next;
    // Advance to midnight (rest) -> back home -> a second agent-at fact.
    let sys2 = AgentMovements { npcs: vec![npc.clone()], at_time: WorldTime { day: 1.0 } };
    let n2 = hornvale_kernel::tick(&ledger, &[&sys2], &["agent-movements"], &r).unwrap();
    assert_eq!(agent_position(&n2, &npc, WorldTime { day: 1.0 }), npc.home);
    assert_eq!(n2.find(AGENT_AT).count(), 2);
}

#[test]
fn tick_commits_nothing_when_position_unchanged() {
    let r = registry_with_agent_at();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
    let npc = Npc { entity: e, home, destination: home.neighbors()[0],
                    activity: hornvale_species::ActivityCycle::Diurnal, label: "herder".into() };
    // Two ticks both at rest (midnight) -> position never changes -> at most one fact.
    let s1 = AgentMovements { npcs: vec![npc.clone()], at_time: WorldTime { day: 1.0 } };
    let n1 = hornvale_kernel::tick(&ledger, &[&s1], &["agent-movements"], &r).unwrap();
    let s2 = AgentMovements { npcs: vec![npc.clone()], at_time: WorldTime { day: 2.0 } };
    let n2 = hornvale_kernel::tick(&n1, &[&s2], &["agent-movements"], &r).unwrap();
    // home == genesis default, so the first tick may emit 0 (already home); the
    // key invariant: no SPURIOUS second fact for an unchanged position.
    assert!(n2.find(AGENT_AT).count() <= 1, "no spurious agent-at for an unchanged position");
    let _ = ledger; let _ = e;
}

#[test]
fn jump_past_a_phase_lands_coherent() {
    // THE JUMP CASE (spec §5.2): a single wait from day 0.2 (rest) to day 1.9
    // (rest again, after a full active phase) must leave the ledger coherent:
    // the NPC's latest committed position equals its scheduled position at 1.9.
    let r = registry_with_agent_at();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = hornvale_kernel::RoomAddr::containing([1.0, 0.0, 0.0], 6);
    let npc = Npc { entity: e, home, destination: home.neighbors()[0],
                    activity: hornvale_species::ActivityCycle::Diurnal, label: "herder".into() };
    let sys = AgentMovements { npcs: vec![npc.clone()], at_time: WorldTime { day: 1.9 } };
    let next = hornvale_kernel::tick(&ledger, &[&sys], &["agent-movements"], &r).unwrap();
    assert_eq!(
        agent_position(&next, &npc, WorldTime { day: 1.9 }),
        scheduled_position(&npc, WorldTime { day: 1.9 }),
        "after a multi-day jump, the committed position matches the schedule at t_new"
    );
    ledger = next; let _ = ledger;
}
```

And the genesis-zero pin (`windows/vessel/tests/liveness_genesis.rs`):

```rust
//! Genesis commits NO agent-at fact (spec §3): the world moves only in a
//! possess session. If this reddens, liveness leaked into world build — a
//! save-format event, not a bug fix.
#[test]
fn a_genesis_world_has_no_agent_at_facts() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap();
    assert_eq!(world.ledger.find(hornvale_vessel::liveness::AGENT_AT).count(), 0);
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel liveness`
Expected: compile error (`AGENT_AT`, `AgentMovements`, `agent_position` missing).

- [ ] **Step 3: Implement** (in `liveness.rs`)

```rust
use hornvale_kernel::{Fact, TickSystem, Value};

/// A game-layer predicate: an agent's room position on a day. Non-functional
/// (position changes over sim time — c5's kind-change shape); the current
/// position is the latest committed one. Registered by the possess session, NOT
/// at genesis (spec §3).
/// type-audit: bare-ok(identifier-text)
pub const AGENT_AT: &str = "agent-at";

/// The movement system (spec §4.3): for each NPC, derive its scheduled position
/// at `at_time` and, if it differs from the last committed position (else the
/// NPC's genesis home), emit a dated `agent-at` fact. Run through c6's `tick`.
/// type-audit: bare-ok(return)
pub struct AgentMovements {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The world-time being advanced to.
    pub at_time: WorldTime,
}

impl TickSystem for AgentMovements {
    fn label(&self) -> &'static str {
        "agent-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        self.npcs
            .iter()
            .filter_map(|npc| {
                let want = scheduled_position(npc, self.at_time);
                let current = latest_committed_position(frozen, npc).unwrap_or(npc.home);
                if want == current {
                    None
                } else {
                    Some(Fact {
                        subject: npc.entity,
                        predicate: AGENT_AT.to_string(),
                        object: Value::Text(room_to_text(want)),
                        place: None,
                        day: Some(self.at_time.day),
                        provenance: "the-quickening".to_string(),
                    })
                }
            })
            .collect()
    }
}

/// The NPC's current position: latest committed `agent-at` ELSE the derived
/// scheduled position at `t`.
/// type-audit: bare-ok(return)
pub fn agent_position(ledger: &Ledger, npc: &Npc, t: WorldTime) -> RoomAddr {
    latest_committed_position(ledger, npc).unwrap_or_else(|| scheduled_position(npc, t))
}

fn latest_committed_position(ledger: &Ledger, npc: &Npc) -> Option<RoomAddr> {
    match ledger.latest_value_of(npc.entity, AGENT_AT) {
        Some(Value::Text(s)) => Some(room_from_text(s)),
        _ => None,
    }
}

// RoomAddr <-> Text: use the existing RoomAddr string encoding if one exists
// (grep RoomAddr Display / to_string / parse in kernel/src/room.rs); else encode
// the address components. MUST round-trip: room_from_text(room_to_text(r)) == r.
fn room_to_text(r: RoomAddr) -> String { /* existing encoding — plan-detail */ }
fn room_from_text(s: &str) -> RoomAddr { /* existing decoding — plan-detail */ }
```

**Implementer note:** find `RoomAddr`'s existing serialization
(`kernel/src/room.rs` — it is a save-format id, decision 0006, so it round-trips
somehow; reuse that exact encoding, do not invent a new one). `latest_value_of`
is c5's kernel read (latest-wins over a non-functional predicate) — confirm the
signature. Numbers in `Value` quantize at commit; a `RoomAddr` text encoding
sidesteps float drift.

- [ ] **Step 4: Run + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel liveness` then `cargo test -p hornvale-vessel --test liveness_genesis`
Expected: PASS. Mutation-verify: (a) in `step`, drop the `want == current` guard (always emit) → `tick_commits_nothing_when_position_unchanged` reds; (b) temporarily commit an `agent-at` in `build_world` → the genesis-zero pin reds; revert. Report both.

```bash
git diff --stat book/ cli/tests/fixtures/   # MUST be empty
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): agent-movements TickSystem + agent-at events + genesis-zero pin (the-quickening T2)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 3: The `wait` verb made real + session time-advance + observation

**Files:**
- Modify: `windows/vessel/src/session.rs` (owned evolving ledger/registry; the real `wait`; the observation line)
- Test: `windows/vessel/tests/possession_moves.rs` (session determinism + observation integration)

**Interfaces:**
- Consumes: Tasks 1–2. The `Session` struct (currently holds `world: &'w World`, `day: WorldTime`, etc.).
- Produces: a `wait <interval>` that advances time, runs `AgentMovements`, and re-focalizes; a session-owned `ledger: Ledger` + `registry: ConceptRegistry` + `npcs: Vec<Npc>`.

- [ ] **Step 1: Write the failing test** (`windows/vessel/tests/possession_moves.rs`)

```rust
//! The world moves without you: possess, wait across a phase, observe an NPC's
//! motion; and the same script is byte-deterministic.
use hornvale_vessel::{PossessOpts, Session};

fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    ).unwrap()
}

#[test]
fn day_zero_session_is_unchanged_until_you_wait() {
    // The frozen behavior is preserved: before any `wait`, no agent-at exists.
    let w = world();
    let (session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    assert_eq!(session.committed_agent_at_count(), 0); // a test accessor added in this task
}

#[test]
fn waiting_moves_an_npc_and_it_is_observed() {
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Wait long enough to cross into an active phase for at least one NPC.
    let out = session.handle("wait 1");
    // After a day of waiting, at least one agent-at has been committed.
    assert!(session.committed_agent_at_count() >= 1, "the world moved on wait");
    // The wait output mentions motion (non-empty, references an NPC/movement).
    match out { hornvale_vessel::Turn::Out(s) => assert!(!s.is_empty()), _ => panic!("wait outputs prose") };
}

#[test]
fn the_same_script_is_byte_deterministic() {
    let w = world();
    let run = || {
        let (mut s, _o) = Session::start(&w, &PossessOpts::default()).unwrap();
        for cmd in ["wait 1", "wait 1", "wait 1"] { let _ = s.handle(cmd); }
        s.session_ledger_json() // a test accessor: serde_json of the session ledger
    };
    assert_eq!(run(), run(), "same seed + same waits -> byte-identical session ledger");
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test possession_moves`
Expected: compile error (accessors / owned ledger missing).

- [ ] **Step 3: Implement the evolving session**

In `windows/vessel/src/session.rs`: give `Session` an owned `ledger: Ledger`, `registry: ConceptRegistry`, and `npcs: Vec<Npc>`. In `start`: clone `world.ledger` into `self.ledger`, clone `world.registry` into `self.registry`, register `AGENT_AT` into `self.registry`, and `derive_npcs(world, &ctx, &mut self.ledger, K)` (K a small authored const, e.g. 3). Replace the `wait` body:

```rust
fn wait(&mut self, rest: &str) -> Turn {
    let days: f64 = rest.trim().parse().unwrap_or(1.0);
    self.day = WorldTime { day: self.day.day + days };
    let sys = crate::liveness::AgentMovements { npcs: self.npcs.clone(), at_time: self.day };
    match hornvale_kernel::tick(&self.ledger, &[&sys], &["agent-movements"], &self.registry) {
        Ok(next) => {
            let moved = next.len() - self.ledger.len();
            self.ledger = next;
            // re-absorb the (possibly changed) here into knowledge, then narrate.
            let _ = self.absorb_here();
            let line = self.narrate_motion(moved);
            Turn::Out(line)
        }
        Err(e) => Turn::Out(format!("Time falters: {e}")),
    }
}

fn narrate_motion(&self, moved: usize) -> String {
    if moved == 0 {
        "Time passes; the world keeps its shape.".to_string()
    } else {
        // Name the NPCs the possessed agent would perceive at/near its position.
        // (Plan-detail: for each NPC whose agent_position == self.agent.position
        //  or an adjacent room, add a line. Keep it deterministic and simple.)
        format!("Time passes. You sense movement nearby ({moved} stirred).")
    }
}
```

Add the test accessors (behind `#[doc(hidden)]` or plain `pub`):
`pub fn committed_agent_at_count(&self) -> usize { self.ledger.find(crate::liveness::AGENT_AT).count() }`
and `pub fn session_ledger_json(&self) -> String { serde_json::to_string(&self.ledger).unwrap() }`.

**Reads that must now use the session ledger:** anything rendering NPC positions
reads `self.ledger` (the evolving one), not `self.world.ledger`. The possessed
agent's own frozen reads (`observable(self.world, …)`) are unchanged — the
frozen genesis world still backs the static scenery; only the NPC layer evolves.
Confirm `day_zero_session_is_unchanged_until_you_wait` stays green (no `wait` →
no `agent-at`).

- [ ] **Step 4: Run + mutation-verify + full suite + commit**

Run: `cargo test -p hornvale-vessel --test possession_moves`
Expected: PASS. Mutation-verify: make `wait` NOT run the tick (just advance `self.day`) → `waiting_moves_an_npc...` reds. Restore.

```bash
cargo nextest run --workspace 2>&1 | tee /tmp/hv-q-t3.txt
git diff --stat book/ cli/tests/fixtures/   # MUST be empty
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): the wait verb moves the world — evolving session ledger + agent-movements tick (the-quickening T3)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 4: Provenance + over-time transcript + book + close

**Files:**
- Modify: `windows/vessel` or `windows/explain` (a history read that recounts a dated `agent-at`) — mirror the existing `explain`/`why` path
- Create: a NEW gallery transcript artifact (a "possession over time" — the world moving on `wait`) + its generator/drift-check wiring (mirror the existing frozen possession transcript)
- Create: `book/src/chronicle/the-quickening.md` (+ `book/src/SUMMARY.md`)
- Create: `docs/retrospectives/the-quickening.md`
- Modify: `book/src/frontier/idea-registry.md` (The Walk M2 / PSY-6 / UNI-20 rows), any stale "the world is frozen" book chapter, the spec header (STATUS)

- [ ] **Step 1: Provenance read + test**

Add/extend a history read so `explain`/`why` over an NPC recounts its dated
`agent-at` facts ("the herder was at the river on day 1"). Mirror the existing
`explain` window's recount path. Test: after a `wait` that moved an NPC, the
recount names the dated position. Mutation-verify (drop the day from the
recount → the test reds).

- [ ] **Step 2: The over-time transcript artifact**

Find how the frozen possession transcript gallery artifact is generated and
drift-checked (grep `possession` in `scripts/regenerate-artifacts.sh` and
`book/src/gallery/`). Add a NEW artifact — a fixed possess script that `wait`s
across phases and captures the moving world — as a new file (NOT a diff of the
frozen one). Wire it into the regen script + the drift-check. Run
`bash scripts/regenerate-artifacts.sh` and confirm: the new file appears, the
frozen transcript and all other artifacts are UNCHANGED (`git status`).

- [ ] **Step 3: Final byte-identity + full gate**

```bash
git status --porcelain
make gate 2>&1 | tail -20
cargo nextest run --workspace 2>&1 | tee /tmp/hv-q-final.txt
git diff $(git merge-base HEAD main)..HEAD --stat -- book/src/laboratory/ cli/tests/fixtures/ | cat
```
Expected: `make gate` green; full suite exit 0; the census/fixtures diff EMPTY (genesis byte-identical). The only new book file is the over-time transcript + the chronicle.

- [ ] **Step 4: Chronicle** (`book/src/chronicle/the-quickening.md`)

At the book's altitude: the world moves for the first time; the actor pivot
(demography was inert — the substrate is a static equilibrium, measured, so
liveness had to come from agent action); the derived routine vs the committed
happening (the seam rule); genesis-is-tick-0 paying off (c6's tick, c5's
latest-wins, the Milestone-1 agent spine all running together); the shadow
posture (the world moves only when possessed; genesis byte-identical). Add the
SUMMARY.md line. `mdbook build book` — clean.

- [ ] **Step 5: Freshness sweep + registry + retro**

- `grep -rn "frozen\|does not move\|world stays still\|static cross-section" book/src/` — update vessel/possess chapters that call the world frozen (now it moves on `wait`).
- `book/src/frontier/idea-registry.md`: open The Walk Milestone 2 (the living world) / note the first liveness slice; re-point PSY-6 (setpoint idea named but planner deferred), UNI-20 (a new derived view: the moving-agent layer). Edit ONLY this worktree's copy.
- `book/src/open-questions.md`: check for a moved bet (grep liveness/agent/walk terms; a first-slice liveness may touch the "someone walks in it" bet — SAY SO if it does, re-score per decision 0030; if not, say why).
- `cargo test -p hornvale --test docs_consistency` — PASS.
- Retrospective `docs/retrospectives/the-quickening.md`: the measured demography-inert-actor finding + the pivot (the measure-don't-narrate win — verifying real data caught the false premise before a plan was written); the 7 followup rows; the c7→liveness pivot context; whatever execution taught. Spec header STATUS line.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A && git commit -m "docs(the-quickening): close — provenance, over-time transcript, chronicle, registry, retrospective (the-quickening T4)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

**STOP after this task: G6 is a hard stop.** Present the post-G3 ledger digest (the actor-pivot #8 and the determinism entry #5 leading) to Nathan; the FF, push, and teardown are his calls, under `closing-a-campaign`.

---

## Execution notes for the dispatcher

- Subagent-driven (dispatching-hornvale-subagents): sonnet floor, dispatch
  preamble verbatim, `cd` into the worktree, challenge-response on return.
  **Watch for parking** (two agents parked in c6) — resume via SendMessage.
- `make preflight` at every task boundary; absorb main INTO the branch on NO-GO.
- No task regenerates census artifacts; Tasks 1–3 confirm the empty book/fixtures
  diff; Task 4 adds exactly one new book file (the over-time transcript) + docs.
- Task 1's `mint_flagship` refactor (factoring out `settlement_position`) must
  keep the frozen possess transcript byte-identical — verify the empty diff.
