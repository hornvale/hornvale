# The Surmise Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Agents stop magically knowing where water is — what an agent knows of the world becomes *belief*, a derived cache over the one ledger, populated by perception; the planner plans over belief, not truth.

**Architecture:** All work is in `windows/vessel/` (domains untouched — The Walk §11). Water is a region read from the existing elevation field (`is_water` = elevation ≤ `WATER_LEVEL`), abstracted behind a `Terrain` trait so pure tests plant synthetic terrain. `believed_water` is a pure fold over the agent's existing `agent-at` history ∩ `is_water` — **no new committed predicate**. The reserved `Perceived` view gains `believed_water` + `explore_step`; `decide` reads belief and, when ignorant, explores; the c6 `DriveMovements` tick builds the view each step from the frozen ledger and walks the belief-driven plan.

**Tech Stack:** Rust (edition 2024), std + serde only. Kernel primitives: `RoomAddr`, `Ledger`, `Fact`, `Value`, `WorldTime`, `EntityId`, `astar`/`SearchSpace`, `tick`/`TickSystem`. `cargo nextest`, `cargo test --doc`, `cargo clippy`, `cargo fmt`, `tools/type-audit`.

## Global Constraints

- **No new committed predicate; no epoch; genesis byte-identical.** Belief re-derives from existing `agent-at` facts ∩ static `is_water`. The genesis-zero pins (no `agent-at`, no `drank`) must stay green.
- **Determinism:** no `HashMap`/`HashSet` (use `BTreeMap`/`BTreeSet`/`Vec`); no RNG; no wall-clock; all float ordering via `total_cmp` with a deterministic tie-break (never native `<`/`>=` on `f64`).
- **`windows/vessel/` only.** Kernel, domains, and other windows are untouched.
- **Every public item, field, and variant gets a one-line doc comment** (`#![warn(missing_docs)]`); every primitive at a `pub` boundary carries a `type-audit:` tag.
- **`decide`'s seam is preserved:** the tick depends only on the `Intent` output; the body-swap changes what *fills* the view (belief), not the tick's dependence on `Intent`.
- **Run `cargo fmt` and `type-audit check` before every commit.** Cost-order the gate: fmt + clippy first, then `cargo test -p hornvale-vessel`.
- **Census-free:** nothing here runs a census or needs AWS. `make gate` (no heavy tier) is the bar.

Authored constants (spec §8): `WATER_LEVEL: f64` (tuned in Task 5 for the seed-42 demo), perception radius 0 (occupy the room), exploration = lowest-elevation *unvisited* neighbor, `MAX_STEPS`/`PLAN_BUDGET` reused from the existing module.

---

## File Structure

- `windows/vessel/src/liveness.rs` (modify) — the whole mechanism: `Terrain` trait, `WATER_LEVEL`, `is_water`, `nearest_water`, `downhill_step`, `believed_water`; `Perceived` gains `believed_water` + `explore_step`; `decide` reads belief + explore branch; `DriveMovements` gains a `Terrain` and builds the belief view each step. `resource_room` is superseded by `nearest_water`.
- `windows/vessel/src/session.rs` (modify) — a `LocaleTerrain` adapter over `LocaleContext`; `wait` constructs `DriveMovements` with it; `derive_npcs`'s resource uses `nearest_water`; `why`/`needs` prose narrates discovery and the believed source.
- `windows/vessel/tests/liveness_genesis.rs` (unchanged, must stay green) — the genesis-zero pins.
- Tests live inline in `liveness.rs` `#[cfg(test)]` (as today) plus the existing `windows/vessel/tests/*.rs` integration files where they already are.

---

## Task 1: Terrain trait + water geography (`is_water`, `nearest_water`, `downhill_step`)

**Files:**
- Modify: `windows/vessel/src/liveness.rs`
- Test: inline `#[cfg(test)] mod tests` in the same file.

**Interfaces:**
- Consumes: `RoomAddr` (`neighbors()`, `Ord`, `containing`), `total_cmp` discipline.
- Produces:
  - `pub trait Terrain { fn elevation(&self, room: &RoomAddr) -> f64; }`
  - `pub const WATER_LEVEL: f64`
  - `pub fn is_water(room: &RoomAddr, terrain: &dyn Terrain) -> bool`
  - `pub fn downhill_step(from: &RoomAddr, terrain: &dyn Terrain) -> RoomAddr` (lowest-elevation neighbor)
  - `pub fn nearest_water(from: &RoomAddr, terrain: &dyn Terrain, budget: usize) -> Option<RoomAddr>`
  - a test helper `struct PlantedTerrain` (a `BTreeMap<RoomAddr, f64>`, default `f64::INFINITY`).

- [ ] **Step 1: Write the failing tests** (append to the inline `tests` module)

```rust
/// A synthetic elevation field for pure tests: planted heights, INFINITY elsewhere
/// (INFINITY = "not water, never chosen downhill" — mirrors resource_room's
/// undescribable-room fallback).
struct PlantedTerrain(std::collections::BTreeMap<RoomAddr, f64>);
impl Terrain for PlantedTerrain {
    fn elevation(&self, room: &RoomAddr) -> f64 {
        self.0.get(room).copied().unwrap_or(f64::INFINITY)
    }
}

#[test]
fn is_water_is_the_elevation_threshold() {
    let low = raddr(1.0);
    let high = raddr(2.0);
    let t = PlantedTerrain([(low.clone(), WATER_LEVEL - 1.0), (high.clone(), WATER_LEVEL + 1.0)]
        .into_iter().collect());
    assert!(is_water(&low, &t));
    assert!(!is_water(&high, &t));
}

#[test]
fn downhill_step_picks_the_lowest_neighbor_deterministically() {
    let home = raddr(1.0);
    let ns = home.neighbors();
    // Make ns[1] strictly lowest; others high.
    let mut m = std::collections::BTreeMap::new();
    for (i, n) in ns.iter().enumerate() {
        m.insert(n.clone(), if i == 1 { 0.0 } else { 100.0 });
    }
    let t = PlantedTerrain(m);
    assert_eq!(downhill_step(&home, &t), ns[1]);
}

#[test]
fn nearest_water_finds_the_closest_water_room_by_hops() {
    // home (dry) -> a neighbor that is water: 1 hop.
    let home = raddr(1.0);
    let near = home.neighbors()[0].clone();
    let t = PlantedTerrain([(home.clone(), 100.0), (near.clone(), WATER_LEVEL - 1.0)]
        .into_iter().collect());
    assert_eq!(nearest_water(&home, &t, 10_000), Some(near));
}

#[test]
fn nearest_water_returns_from_itself_when_already_on_water() {
    let here = raddr(1.0);
    let t = PlantedTerrain([(here.clone(), WATER_LEVEL - 1.0)].into_iter().collect());
    assert_eq!(nearest_water(&here, &t, 10_000), Some(here));
}

#[test]
fn nearest_water_gives_up_within_budget_when_no_water() {
    let home = raddr(1.0); // all-INFINITY terrain: no water anywhere
    let t = PlantedTerrain(std::collections::BTreeMap::new());
    assert_eq!(nearest_water(&home, &t, 50), None);
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib is_water_is_the_elevation_threshold`
Expected: FAIL (`Terrain`/`is_water` not found).

- [ ] **Step 3: Implement** (add near `resource_room` in `liveness.rs`)

```rust
/// The elevation field the belief/exploration logic reads, abstracted so pure
/// tests plant synthetic terrain without building a world. The session backs it
/// with a `LocaleContext` (see `session.rs::LocaleTerrain`).
pub trait Terrain {
    /// The room's elevation in metres (INFINITY for an undescribable room —
    /// never water, never chosen downhill).
    fn elevation(&self, room: &RoomAddr) -> f64;
}

/// The elevation at or below which a room is water (spec §8, tuned for the
/// seed-42 demo in the closing task). A room is water iff it lies this low.
/// type-audit: bare-ok(quantity: metres)
pub const WATER_LEVEL: f64 = 0.0;

/// Water-truth (L0): a room is water iff its elevation is at or below
/// `WATER_LEVEL`. Pure over the terrain field; the low ground is water, so
/// sources scatter naturally by terrain (many, not one).
pub fn is_water(room: &RoomAddr, terrain: &dyn Terrain) -> bool {
    terrain.elevation(room).total_cmp(&WATER_LEVEL).is_le()
}

/// The single steepest-descent neighbour ("water lies low" — the prior an
/// ignorant agent explores along). `total_cmp` with an ascending-`RoomAddr`
/// tie-break (the constitutional no-native-float-cmp rule), exactly
/// `resource_room`'s existing rule. Always a neighbour (never `from` itself).
pub fn downhill_step(from: &RoomAddr, terrain: &dyn Terrain) -> RoomAddr {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        let elev = terrain.elevation(&n);
        let keep_existing = match &best {
            Some((ba, be)) => elev.total_cmp(be).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep_existing {
            best = Some((n, elev));
        }
    }
    best.expect("a room has three neighbors").0
}

/// The true nearest water room to `from` (ground-truth-best) — a deterministic
/// breadth-first walk over the mesh to the closest `is_water` room, frontier
/// processed in `RoomAddr` order, capped at `budget` expansions (`None` if no
/// water within it). The agent does not know this until it has PERCEIVED it.
pub fn nearest_water(from: &RoomAddr, terrain: &dyn Terrain, budget: usize) -> Option<RoomAddr> {
    let mut visited: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    let mut frontier: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    frontier.insert(from.clone());
    let mut expansions = 0usize;
    while let Some(room) = frontier.iter().next().cloned() {
        frontier.remove(&room);
        if !visited.insert(room.clone()) {
            continue;
        }
        if is_water(&room, terrain) {
            return Some(room);
        }
        expansions += 1;
        if expansions >= budget {
            return None;
        }
        for n in room.neighbors() {
            if !visited.contains(&n) {
                frontier.insert(n);
            }
        }
    }
    None
}
```

Note: the BFS pops the *smallest-`RoomAddr`* frontier element each step, so among equal-hop water rooms the smallest `RoomAddr` wins — deterministic. (Not strictly hop-monotone, but adequate for "nearest source"; the belief fold in Task 2 uses `plan_to_room` path length for the precise nearest-to-home ordering.)

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel --lib -- nearest_water is_water downhill_step`
Expected: PASS (5 tests).

- [ ] **Step 5: fmt + type-audit + commit**

```bash
cargo fmt
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): Terrain trait + water geography (is_water/nearest_water/downhill_step) — the-surmise T1"
```

---

## Task 2: `believed_water` — the nearest-known-of-several belief fold

**Files:**
- Modify: `windows/vessel/src/liveness.rs`
- Test: inline `tests` module.

**Interfaces:**
- Consumes: `is_water`, `Terrain`, `PlantedTerrain` (Task 1); `AGENT_AT`, `room_from_text`, `plan_to_room`, `Npc` (existing).
- Produces: `pub fn believed_water(ledger: &Ledger, npc: &Npc, t: WorldTime, terrain: &dyn Terrain, budget: usize) -> Option<RoomAddr>`.

The keystone this task lands: **BELIEF ≡ FOLD-OVER-PERCEIVED** — belief is a pure deterministic function of the agent's committed `agent-at` history ∩ water-truth, reload-stable and per-agent-isolated, exactly as `drive_at` is over `drank`.

- [ ] **Step 1: Write the failing tests**

```rust
// Helper: commit an agent-at at `room` on `day` for `entity`.
fn commit_agent_at(ledger: &mut Ledger, reg: &ConceptRegistry, entity: EntityId, room: &RoomAddr, day: f64) {
    ledger.commit(agent_at_fact(entity, room, day, "test"), reg).unwrap();
}
fn agent_at_reg() -> ConceptRegistry {
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    reg
}

#[test]
fn believed_water_is_none_until_the_agent_has_stood_in_water() {
    let reg = agent_at_reg();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = raddr(1.0);
    let water = home.neighbors()[0].clone();
    let t = PlantedTerrain([(water.clone(), WATER_LEVEL - 1.0)].into_iter().collect());
    let npc = Npc { entity: e, home: home.clone(), resource: water.clone(),
        activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
    // no agent-at yet -> ignorant
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), None);
    // stood in the water room on day 2 -> now believes it
    let mut ledger = ledger; commit_agent_at(&mut ledger, &reg, e, &water, 2.0);
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), Some(water));
}

#[test]
fn believed_water_ignores_dry_rooms_the_agent_stood_in() {
    let reg = agent_at_reg();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = raddr(1.0);
    let dry = home.neighbors()[0].clone();
    let t = PlantedTerrain([(dry.clone(), WATER_LEVEL + 1.0)].into_iter().collect());
    let npc = Npc { entity: e, home: home.clone(), resource: home.clone(),
        activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
    commit_agent_at(&mut ledger, &reg, e, &dry, 2.0);
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), None);
}

#[test]
fn believed_water_keeps_the_nearest_to_home_of_several_known_sources() {
    // THE MULTI-SOURCE FOLD: the agent has stood in a NEAR and a FAR water room;
    // belief is the near one (fewer hops from home).
    let reg = agent_at_reg();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = raddr(1.0);
    let near = home.neighbors()[0].clone();                 // 1 hop
    let far = near.neighbors().iter().find(|n| **n != home).unwrap().clone(); // 2 hops
    let t = PlantedTerrain([(near.clone(), WATER_LEVEL - 1.0), (far.clone(), WATER_LEVEL - 1.0)]
        .into_iter().collect());
    let npc = Npc { entity: e, home: home.clone(), resource: near.clone(),
        activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
    commit_agent_at(&mut ledger, &reg, e, &far, 2.0);   // discovered far first
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), Some(far.clone()));
    commit_agent_at(&mut ledger, &reg, e, &near, 3.0);  // later discovers the nearer one
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), Some(near),
        "belief switches to the nearer known source");
}

#[test]
fn believed_water_only_counts_sightings_at_or_before_t() {
    let reg = agent_at_reg();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = raddr(1.0);
    let water = home.neighbors()[0].clone();
    let t = PlantedTerrain([(water.clone(), WATER_LEVEL - 1.0)].into_iter().collect());
    let npc = Npc { entity: e, home: home.clone(), resource: water.clone(),
        activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
    commit_agent_at(&mut ledger, &reg, e, &water, 9.0); // sighting in the future
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), None);
}

#[test]
fn believed_water_is_deterministic_reload_stable_and_per_agent() {
    // BELIEF == FOLD: same ledger+t -> same value; reload-stable; another agent's
    // sightings never leak in (subject-scoped).
    let reg = agent_at_reg();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let other = ledger.mint_entity();
    let home = raddr(1.0);
    let water = home.neighbors()[0].clone();
    let t = PlantedTerrain([(water.clone(), WATER_LEVEL - 1.0)].into_iter().collect());
    let npc = Npc { entity: e, home: home.clone(), resource: water.clone(),
        activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
    commit_agent_at(&mut ledger, &reg, other, &water, 2.0); // OTHER stood in water, not e
    assert_eq!(believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000), None,
        "another agent's sighting does not become e's belief");
    commit_agent_at(&mut ledger, &reg, e, &water, 3.0);
    let a = believed_water(&ledger, &npc, WorldTime { day: 5.0 }, &t, 10_000);
    let json = serde_json::to_string(&ledger).unwrap();
    let reloaded: Ledger = serde_json::from_str(&json).unwrap();
    assert_eq!(believed_water(&reloaded, &npc, WorldTime { day: 5.0 }, &t, 10_000), a);
    assert_eq!(a, Some(water));
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib believed_water`
Expected: FAIL (`believed_water` not found).

- [ ] **Step 3: Implement** (add after `drive_at` in `liveness.rs`)

```rust
/// Belief (L1): the agent's nearest KNOWN water — a pure fold over its committed
/// `agent-at` history ∩ water-truth. Among the water rooms the agent has stood in
/// at or before `t`, the one nearest to `npc.home` by planned hop-distance (ties
/// by ascending `RoomAddr`), else `None` (ignorant). BELIEF == FOLD-OVER-PERCEIVED:
/// no stored belief — it re-derives from facts already committed (the matrix
/// verdict; UNI-20). Nearness anchors to home (nearest-to-current is a followup).
pub fn believed_water(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    budget: usize,
) -> Option<RoomAddr> {
    let mut seen: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) {
        if f.day.map(|d| d <= t.day).unwrap_or(false) {
            if let Value::Text(s) = &f.object {
                let room = room_from_text(s);
                if is_water(&room, terrain) {
                    seen.insert(room);
                }
            }
        }
    }
    seen.into_iter()
        .filter_map(|r| plan_to_room(&npc.home, &r, budget).map(|p| (p.len(), r)))
        .min_by(|(la, ra), (lb, rb)| la.cmp(lb).then_with(|| ra.cmp(rb)))
        .map(|(_, r)| r)
}
```

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel --lib believed_water`
Expected: PASS (5 tests).

- [ ] **Step 5: fmt + type-audit + commit**

```bash
cargo fmt && cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): believed_water — the nearest-of-several belief fold (BELIEF==FOLD) — the-surmise T2"
```

---

## Task 3: `Perceived` gains belief + `decide` plans over belief (and explores when ignorant)

This is the seam body-swap. `Perceived` gains `believed_water` + `explore_step`; `decide` drops its ground-truth `water` argument (it now lives in the view as belief) and gains the explore branch; `DriveMovements` gains a `Terrain` and builds the belief view each step. Existing `decide`/tick tests update to the belief model.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`Perceived`, `decide`, `DriveMovements`, `DriveMovements::step`, and the existing tests that construct them).

**Interfaces:**
- Consumes: `believed_water`, `is_water`, `downhill_step`, `Terrain` (T1/T2); `plan_to_water`, `plan_to_room`, `Action`, `Intent`, `agent_at_fact`, `drank_fact`, `agent_position` (existing).
- Produces:
  - `Perceived { position, drive, believed_water: Option<RoomAddr>, explore_step: Option<RoomAddr> }`
  - `pub fn decide(view: &Perceived, home: &RoomAddr, p: &DriveParams, budget: usize) -> Intent`
  - `pub struct DriveMovements<'a> { npcs, from, to, params, terrain: &'a dyn Terrain }`

- [ ] **Step 1: Write the failing pure decision-divergence test**

```rust
#[test]
fn decide_plans_to_believed_water_or_explores_when_ignorant() {
    // BELIEF DRIVES THE DECISION: two views identical but for belief produce
    // different first moves — the believer A*-steps toward its known water; the
    // ignorant one takes the explore step. (Water two hops away so the A* first
    // step differs from an arbitrary explore step.)
    let p = SUSTENANCE;
    let home = raddr(1.0);
    let mid = home.neighbors()[0].clone();
    let water = mid.neighbors().iter().find(|n| **n != home).unwrap().clone();
    let explore = home.neighbors()[2].clone(); // a different direction
    // believer, thirsty, at home -> first A* step toward water (== mid)
    let believer = Perceived { position: home.clone(), drive: 0.9,
        believed_water: Some(water.clone()), explore_step: Some(explore.clone()) };
    assert_eq!(decide(&believer, &home, &p, 10_000), Intent::Do(Action::MoveTo(mid.clone())));
    // ignorant, thirsty, at home -> the explore step (not toward water)
    let ignorant = Perceived { position: home.clone(), drive: 0.9,
        believed_water: None, explore_step: Some(explore.clone()) };
    assert_eq!(decide(&ignorant, &home, &p, 10_000), Intent::Do(Action::MoveTo(explore.clone())));
    assert_ne!(mid, explore, "the two beliefs must yield different moves for this to prove anything");
    // ignorant with nowhere new to explore -> Hold
    let stuck = Perceived { position: home.clone(), drive: 0.9,
        believed_water: None, explore_step: None };
    assert_eq!(decide(&stuck, &home, &p, 10_000), Intent::Hold);
    // not thirsty, away from home -> plan home (unchanged behavior)
    let sated_away = Perceived { position: water.clone(), drive: 0.1,
        believed_water: Some(water.clone()), explore_step: None };
    assert!(matches!(decide(&sated_away, &home, &p, 10_000), Intent::Do(Action::MoveTo(_))));
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib decide_plans_to_believed_water`
Expected: FAIL (compile error — `Perceived` has no `believed_water`; `decide` arity).

- [ ] **Step 3: Implement the seam change**

Replace `Perceived`:

```rust
/// What the agent perceives of the world — the `view` the decision reads. Splits
/// SELF-knowledge (position, drive — always true) from world-BELIEF
/// (`believed_water` — a cache that may be absent/ignorant) and immediate
/// perceived affordance (`explore_step`). PSY-6's "plan over belief, not truth"
/// (UNI-16), realized: the ground-truth `water` argument `decide` once took now
/// lives here as belief.
/// type-audit: bare-ok(ratio: drive)
#[derive(Clone, Debug)]
pub struct Perceived {
    /// The agent's current room (self-knowledge — always true).
    pub position: RoomAddr,
    /// The agent's perceived drive level (self-knowledge — always true).
    pub drive: f64,
    /// The nearest water the agent KNOWS of (belief), or `None` (ignorant).
    pub believed_water: Option<RoomAddr>,
    /// The next exploration move for an ignorant agent (lowest-elevation
    /// unvisited neighbour), or `None` (nowhere new to look → Hold).
    pub explore_step: Option<RoomAddr>,
}
```

Replace `decide` (drop the `water` argument; add the explore branch):

```rust
/// The planner (The Foresight), now planning over BELIEF (The Surmise): thirsty
/// and knows water -> A* to it and drink; thirsty and ignorant -> take the
/// explore step (or Hold if nowhere new); not thirsty and away -> plan home; else
/// Hold. Preserves the seam: `decide` reads `view` (now carrying belief), the tick
/// depends only on `Intent`.
/// type-audit: bare-ok(count: budget)
pub fn decide(view: &Perceived, home: &RoomAddr, p: &DriveParams, budget: usize) -> Intent {
    if view.drive >= p.act {
        match &view.believed_water {
            Some(w) => match plan_to_water(&view.position, w, budget).and_then(|pl| pl.into_iter().next()) {
                Some(a) => Intent::Do(a),
                None => Intent::Hold, // known water unreachable within budget
            },
            None => match &view.explore_step {
                Some(step) => Intent::Do(Action::MoveTo(step.clone())),
                None => Intent::Hold, // ignorant and nowhere new to explore
            },
        }
    } else if &view.position != home {
        match plan_to_room(&view.position, home, budget).and_then(|pl| pl.into_iter().next()) {
            Some(a) => Intent::Do(a),
            None => Intent::Hold,
        }
    } else {
        Intent::Hold
    }
}
```

Add the `Terrain` to `DriveMovements` and rebuild `step` to compute the belief view each iteration. Replace the struct + `impl TickSystem`:

```rust
/// The drive-driven movement system (The Foresight → The Surmise): each NPC steps
/// through its belief-driven plan — exploring while ignorant, beelining once it
/// knows water — committing a dated `agent-at`/`drank` at each executed step.
/// Holds a `Terrain` to compute belief and exploration mid-walk.
/// type-audit: bare-ok(return)
pub struct DriveMovements<'a> {
    /// The NPCs this tick advances.
    pub npcs: Vec<Npc>,
    /// The interval start (the session's previous day).
    pub from: WorldTime,
    /// The interval end (the session's new day).
    pub to: WorldTime,
    /// The drive parameters.
    pub params: DriveParams,
    /// The elevation field belief and exploration read.
    pub terrain: &'a dyn Terrain,
}

impl<'a> TickSystem for DriveMovements<'a> {
    fn label(&self) -> &'static str {
        "drive-movements"
    }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        let mut out = Vec::new();
        for npc in &self.npcs {
            let mut pos = agent_position(frozen, npc, self.from);
            let mut day = self.from.day;
            let mut last_drank = frozen
                .find(DRANK)
                .filter(|f| f.subject == npc.entity)
                .filter_map(|f| f.day)
                .fold(0.0_f64, f64::max);
            // Belief and exploration state, evolved locally across the walk (the
            // fold includes this tick's own emitted moves). Seed belief from the
            // pre-tick history; grow it whenever the agent stands in water.
            let mut believed = believed_water(frozen, npc, self.from, self.terrain, PLAN_BUDGET);
            let mut visited: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
            visited.insert(pos.clone());
            let mut steps = 0usize;
            loop {
                if day > self.to.day || steps >= MAX_STEPS {
                    break;
                }
                steps += 1;
                // Standing in water forms/updates belief (nearest-to-home wins).
                if is_water(&pos, self.terrain) {
                    believed = nearer_to_home(&npc.home, believed.take(), pos.clone(), PLAN_BUDGET);
                }
                let drive = (self.params.rise * (day - last_drank)).clamp(0.0, 1.0);
                let explore_step = lowest_unvisited_neighbor(&pos, &visited, self.terrain);
                let view = Perceived {
                    position: pos.clone(),
                    drive,
                    believed_water: believed.clone(),
                    explore_step,
                };
                let seeking_water = drive >= self.params.act;
                match decide(&view, &npc.home, &self.params, PLAN_BUDGET) {
                    Intent::Do(Action::MoveTo(n)) => {
                        day += MOVE_DURATION;
                        if day > self.to.day {
                            break;
                        }
                        let provenance = if seeking_water && believed.is_some() {
                            "walking to water (thirst)"
                        } else if seeking_water {
                            "seeking water (thirst)" // exploring, ignorant
                        } else {
                            "walking home (sated)"
                        };
                        out.push(agent_at_fact(npc.entity, &n, day, provenance));
                        visited.insert(n.clone());
                        pos = n;
                    }
                    Intent::Do(Action::Drink) => {
                        out.push(drank_fact(npc.entity, day, "drank (thirst sated)"));
                        last_drank = day;
                    }
                    Intent::Hold => {
                        let next_act = last_drank + self.params.act / self.params.rise;
                        if next_act <= day || next_act > self.to.day {
                            break;
                        }
                        day = next_act;
                    }
                }
            }
        }
        out
    }
}

/// The nearer-to-home of an existing belief and a newly-perceived water room
/// (ties keep the existing). Small helper for the tick's incremental fold.
fn nearer_to_home(home: &RoomAddr, current: Option<RoomAddr>, found: RoomAddr, budget: usize) -> Option<RoomAddr> {
    let d = |r: &RoomAddr| plan_to_room(home, r, budget).map(|p| p.len());
    match current {
        None => Some(found),
        Some(c) => match (d(&c), d(&found)) {
            (Some(dc), Some(df)) => Some(if df < dc { found } else { c }),
            (None, Some(_)) => Some(found),
            _ => Some(c),
        },
    }
}

/// The lowest-elevation neighbour not yet visited this walk (the directed-
/// exploration step), or `None` if every neighbour is visited. Terminating: the
/// visited set only grows.
fn lowest_unvisited_neighbor(from: &RoomAddr, visited: &std::collections::BTreeSet<RoomAddr>, terrain: &dyn Terrain) -> Option<RoomAddr> {
    let mut best: Option<(RoomAddr, f64)> = None;
    for n in from.neighbors() {
        if visited.contains(&n) {
            continue;
        }
        let elev = terrain.elevation(&n);
        let keep = match &best {
            Some((ba, be)) => elev.total_cmp(be).then_with(|| n.cmp(ba)).is_ge(),
            None => false,
        };
        if !keep {
            best = Some((n, elev));
        }
    }
    best.map(|(r, _)| r)
}
```

- [ ] **Step 4: Update the existing `decide`/tick tests to the belief model**

The old `decide_plans_to_water_when_thirsty_and_home_when_not` and `decide_holds_when_the_plan_is_unreachable_within_budget` pass `water` and a two-field `Perceived`; rewrite their `Perceived` literals to the four-field form (`believed_water: Some(water)`, `explore_step: None`) and drop the `water` argument from `decide` calls. The tick tests (`a_thirsty_agent_plans_to_water_and_the_tick_walks_it`, `thirsty_but_unreachable_water_gives_up_quickly_not_at_max_steps`, `a_degenerate_zero_rise_drive_terminates_via_the_max_steps_cap_not_a_hang`, `moves_carry_drive_naming_provenance`) construct `DriveMovements { npcs, from, to, params }`; add `terrain: &t` where `t` is a `PlantedTerrain` placing the NPC's `resource` room below `WATER_LEVEL` (so the agent can discover and drink) and `home` above it. For the unreachable/degenerate tests, use an all-INFINITY (no water) terrain so the agent never drinks — matching their existing "no facts committed / bounded" assertions (adjust the unreachable test to assert the run is bounded rather than exactly zero facts, since an ignorant agent now explores; keep the termination assertion as the load-bearing one). Show each rewrite in full when editing.

- [ ] **Step 5: Run to verify pass**

Run: `cargo test -p hornvale-vessel --lib`
Expected: PASS (all inline tests, including the new decision-divergence test and the updated tick tests).

- [ ] **Step 6: fmt + clippy + type-audit + commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): decide plans over belief; ignorant agents explore; tick builds the belief view — the-surmise T3"
```

---

## Task 4: session wiring + the multi-source & discovery keystones + genesis pins

Wire `DriveMovements`'s terrain through the session, switch `derive_npcs` to `nearest_water`, and land the end-to-end acceptance tests.

**Files:**
- Modify: `windows/vessel/src/session.rs` (the `LocaleTerrain` adapter; `wait`; `derive_npcs`'s resource).
- Modify: `windows/vessel/src/liveness.rs` (the end-to-end keystone tests; keep the genesis pins green).
- Verify: `windows/vessel/tests/liveness_genesis.rs` still green.

**Interfaces:**
- Consumes: `LocaleContext::describe(&RoomAddr, WorldTime) -> Result<Locale, _>` with `.fields.elevation_m`; `DriveMovements<'a>` (T3); `nearest_water` (T1).
- Produces: `pub struct LocaleTerrain<'a> { pub ctx: &'a LocaleContext }` impl `Terrain`.

- [ ] **Step 1: Write the failing end-to-end keystone tests** (inline `liveness.rs` tests)

```rust
#[test]
fn two_agents_believe_different_sources_from_their_histories_and_beeline_differently() {
    // THE MULTI-SOURCE KEYSTONE (destination divergence): two NPCs, same home,
    // thirst, world — differing ONLY in a pre-seeded agent-at (perceived source).
    // A knows the near source W1; B knows the far source W2. Each beelines to its
    // OWN believed source. Belief ignored ⇒ both go to the same true-nearest ⇒
    // this fails.
    let reg = { let mut r = agent_at_reg(); r.register_predicate(DRANK, false, "drank").unwrap(); r };
    let home = raddr(1.0);
    let w1 = home.neighbors()[0].clone();                        // near source
    let w2 = home.neighbors()[1].neighbors().iter().find(|n| **n != home).unwrap().clone(); // far source
    let terrain = PlantedTerrain([(home.clone(), 100.0), (w1.clone(), WATER_LEVEL - 1.0),
        (w2.clone(), WATER_LEVEL - 1.0)].into_iter().collect());
    let run = |seed_room: &RoomAddr| -> Vec<RoomAddr> {
        let mut ledger = Ledger::default();
        let e = ledger.mint_entity();
        // The prior sighting (day 0), THEN a return-home (day 0.5): history holds
        // the sighting (→ belief) but the agent's current position is home, not
        // the water. (Position = latest agent-at; belief = the fold over history.)
        commit_agent_at(&mut ledger, &reg, e, seed_room, 0.0);
        commit_agent_at(&mut ledger, &reg, e, &home, 0.5);
        let npc = Npc { entity: e, home: home.clone(), resource: w1.clone(),
            activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
        // from > both seed days so the frozen ledger holds no future facts and the
        // agent starts at home, not yet thirsty.
        let sys = DriveMovements { npcs: vec![npc], from: WorldTime { day: 1.0 },
            to: WorldTime { day: 41.0 }, params: SUSTENANCE, terrain: &terrain };
        let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
        // the rooms it drank at (its believed destinations)
        next.find(DRANK).filter(|f| f.subject == e).filter_map(|f| f.day)
            .filter_map(|d| next.find(AGENT_AT).filter(|g| g.subject == e)
                .filter(|g| g.day.map_or(false, |gd| gd <= d))
                .filter_map(|g| match &g.object { Value::Text(s) => Some((g.day.unwrap(), room_from_text(s))), _ => None })
                .max_by(|a, b| a.0.total_cmp(&b.0)).map(|(_, r)| r))
            .collect()
    };
    let a_dests = run(&w1);
    let b_dests = run(&w2);
    assert!(a_dests.iter().all(|r| *r == w1), "A (knows W1) drinks at W1: {a_dests:?}");
    assert!(b_dests.iter().all(|r| *r == w2), "B (knows W2) drinks at W2: {b_dests:?}");
    assert_ne!(w1, w2);
}

#[test]
fn an_ignorant_agent_discovers_water_then_later_beelines() {
    // DISCOVERY: a fresh NPC (no perceived water) explores downhill, finds water,
    // drinks; belief now formed, a later thirst cycle beelines. The first journey
    // (exploration) differs from the later (beeline).
    let reg = { let mut r = agent_at_reg(); r.register_predicate(DRANK, false, "drank").unwrap(); r };
    // A downhill chain home(100) -> a(50) -> water(below level); other neighbors high.
    let home = raddr(1.0);
    let a = home.neighbors()[0].clone();
    let water = a.neighbors().iter().find(|n| **n != home).unwrap().clone();
    let mut m = std::collections::BTreeMap::new();
    m.insert(home.clone(), 100.0); m.insert(a.clone(), 50.0); m.insert(water.clone(), WATER_LEVEL - 1.0);
    let terrain = PlantedTerrain(m);
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let npc = Npc { entity: e, home: home.clone(), resource: water.clone(),
        activity: hornvale_species::ActivityCycle::Diurnal, label: "h".into() };
    let sys = DriveMovements { npcs: vec![npc.clone()], from: WorldTime { day: 0.0 },
        to: WorldTime { day: 40.0 }, params: SUSTENANCE, terrain: &terrain };
    let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &reg).unwrap();
    // It drank at least twice (multiple cycles) and reached the water room.
    let drinks = next.find(DRANK).filter(|f| f.subject == e).count();
    assert!(drinks >= 2, "discovered water and drank across cycles: {drinks}");
    // Belief formed: after the run, believed_water is the discovered source.
    assert_eq!(believed_water(&next, &npc, WorldTime { day: 40.0 }, &terrain, PLAN_BUDGET), Some(water));
    let _ = ledger;
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib -- two_agents_believe_different an_ignorant_agent_discovers`
Expected: FAIL (compile: these already use T3's `DriveMovements` with `terrain`; they fail because the assertions exercise behavior not yet wired — or pass if T3 is complete. If they compile-pass, that is fine — they are the acceptance evidence for T3's mechanism; keep them here as the keystone.)

- [ ] **Step 3: Wire the terrain adapter** (`liveness.rs` for the adapter, `session.rs` for `wait`)

Define the adapter ONCE in `liveness.rs` (which already `use`s `LocaleContext`), so both `derive_npcs` and the session share it:

```rust
/// A `Terrain` backed by a `LocaleContext` — the elevation field the belief/
/// exploration logic reads in a live session (tests use a planted terrain
/// instead). Mirrors `resource_room`'s undescribable-room fallback (INFINITY).
/// type-audit: bare-ok(return)
pub struct LocaleTerrain<'a> {
    /// The locale context whose elevation field is read.
    pub ctx: &'a LocaleContext,
}
impl<'a> Terrain for LocaleTerrain<'a> {
    fn elevation(&self, room: &RoomAddr) -> f64 {
        self.ctx
            .describe(room, WorldTime { day: 0.0 })
            .map(|l| l.fields.elevation_m)
            .unwrap_or(f64::INFINITY)
    }
}
```

In `derive_npcs` (`liveness.rs`), replace `let resource = resource_room(&home, ctx);` with:

```rust
let resource = nearest_water(&home, &LocaleTerrain { ctx }, PLAN_BUDGET).unwrap_or_else(|| home.clone());
```

In `session.rs`'s `wait`, before building `sys` (use the shared adapter via `hornvale_vessel::liveness::LocaleTerrain` or the crate path already imported):

```rust
let terrain = liveness::LocaleTerrain { ctx: &self.ctx };
let sys = DriveMovements {
    npcs: self.npcs.clone(),
    from: /* existing from-day */,
    to: /* existing to-day */,
    params: SUSTENANCE,
    terrain: &terrain,
};
```

`resource_room` is now unused by `derive_npcs`; leave it in place only if another caller needs it (grep first — if none, remove it and its test to avoid dead code).

- [ ] **Step 4: Run the vessel suite + the genesis pins**

Run: `cargo test -p hornvale-vessel`
Expected: PASS — inline tests, `tests/liveness_genesis.rs` (both genesis-zero pins green — no new predicate, genesis untouched), `tests/session.rs`, `tests/possession_moves.rs`, `tests/walker_battery.rs`.

If a `tests/*.rs` integration test constructs `DriveMovements` or calls `decide`, update it to the new signatures (terrain + four-field `Perceived`), same as Task 3's inline updates.

- [ ] **Step 5: fmt + clippy + type-audit + commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/
git commit -m "feat(vessel): session terrain wiring + multi-source & discovery keystones; genesis pins green — the-surmise T4"
```

---

## Task 5: discovery prose, `WATER_LEVEL` demo tuning, book chronicle, close

**Files:**
- Modify: `windows/vessel/src/session.rs` (`why`/`recount`/`needs` prose).
- Modify: `windows/vessel/src/liveness.rs` (`WATER_LEVEL` final value).
- Create: `book/src/chronicle/the-surmise.md`; wire into `book/src/SUMMARY.md`.
- Create: `docs/retrospectives/the-surmise.md`.
- Regenerate: the possess-session transcript artifact (if committed) and any generated book pages the change touches.

- [ ] **Step 1: Narrate discovery in `why`/`recount`.** When an NPC's `agent-at` provenance is `"seeking water (thirst)"` (exploring, ignorant) vs `"walking to water (thirst)"` (beelining, knows), the recount should read the difference ("wandered, having found no water" vs "went down to the mere it knew"). Write a `session.rs` test asserting a possessed-session `why <npc>` transcript contains the discovery phrasing after an ignorant NPC's first trip. (Follow the existing `recount`/`why` test pattern in `tests/session.rs`.)

- [ ] **Step 2: Tune `WATER_LEVEL` for the seed-42 demo.** Add an `#[ignore]`-free `session.rs` (or `liveness.rs`) test on the real seed-42 world asserting the possessed agent's village has water within a short hop budget of home (so the demo shows discovery, not a waterless Hold): build the world + `LocaleContext`, derive the home NPC, assert `nearest_water(&npc.home, &LocaleTerrain{ctx:&ctx}, 200).is_some()` and that its hop-distance is small (e.g. ≤ 8). Adjust `WATER_LEVEL` until this passes with a sensible discovery walk. Keep the value a named constant with a comment recording the tuning.

- [ ] **Step 3: Regenerate committed artifacts and confirm genesis byte-identity.**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/   # expect: clean (worldgen unchanged)
```

Only the possess-session transcript artifact (if the repo commits one) changes; regenerate it per its documented command and review the discovery behavior. If no transcript artifact is committed, this step asserts drift-clean.

- [ ] **Step 4: Book chronicle + retrospective.** Write `book/src/chronicle/the-surmise.md` (the perception→belief story: belief as a cache over the ledger, the map/territory framing, multi-source nearest-known, discovery, BELIEF≡FOLD, and what is reserved — staleness, the L2 credulity threshold). Wire it into `SUMMARY.md`. Write `docs/retrospectives/the-surmise.md` (process lessons). Promote `.superpowers/sdd/followups.md` into the retro's follow-up section.

- [ ] **Step 5: Full gate + commit.**

```bash
make gate
git add windows/vessel/ docs/ book/
git commit -m "docs(the-surmise): discovery prose, WATER_LEVEL tuning, chronicle + retrospective (close) — the-surmise T5"
```

- [ ] **Step 6: STOP — G6 is a hard stop.** Present the post-G3 ledger digest (the no-new-predicate determinism story #7, the cache/belief representation #5, and the multi-source revision leading) to Nathan; the FF, push, pull, teardown are his calls, under `closing-a-campaign`.

---

## Self-review notes (author)

- **Spec coverage:** §3.1 truth → T1; §3.2 perception + §3.3 belief → T2; §3.4 planner/view → T3; §5 keystones (1 decision-divergence → T3; 2 multi-source destination → T4; 3 discovery → T4; 4 fold determinism/reload/isolation → T2; 5 termination → T3's updated degenerate test; 6 genesis-zero → T4) all covered; §6 determinism (no new predicate) enforced by the genesis pins in T4 + the Global Constraints; §7 non-goals reserved (no staleness/threshold code); §8 constants in T1/T5.
- **No new predicate:** grep guard — no `register_predicate` call is added anywhere; `believed_water` reads only `AGENT_AT`. The genesis pins in T4 are the tripwire.
- **Type consistency:** `decide(view, home, p, budget)` (4 args, no `water`) is used identically in T3's tests, T3's `DriveMovements::step`, and T4. `Perceived` is the four-field form everywhere after T3. `DriveMovements<'a>` carries `terrain` from T3 on. `believed_water(ledger, npc, t, terrain, budget)` and `nearest_water(from, terrain, budget)` signatures match across T2/T3/T4/T5.
