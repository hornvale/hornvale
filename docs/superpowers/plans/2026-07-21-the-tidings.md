# The Tidings Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Co-located creatures circulate their spatial water-belief so a lost, ignorant creature learns of water a co-located neighbour knows — the band as error-correcting unit — demonstrated and measured in the synthetic health harness.

**Architecture:** One pure function, `shared_believed_water`, pools the water rooms known to every agent co-located with a creature (same current room) and re-ranks that union by the creature's own nearest-to-home metric — a monotonic set-union join that is order-independent by construction (no RNG). It replaces the plain `believed_water` seed at the two production belief-consumption sites (`DriveMovements::step` and `affect_of`). Live NPCs are one-per-settlement (never co-located), so the change is a no-op over the live population (verified byte-identical); it takes effect only where bands exist — the synthetic health scenarios.

**Tech Stack:** Rust 2024, workspace crates `hornvale-vessel` (`windows/vessel`) and `hornvale-lab` (`windows/lab`), std-only. Gate: `cargo nextest run --workspace`, `cargo test --workspace --doc`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`.

## Global Constraints

- **Dependencies:** `serde` + `serde_json` only, workspace-wide. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`).
- **No wall-clock time** — time is `WorldTime { day: f64 }`.
- **`#![warn(missing_docs)]`** — every public item, field, and variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.**
- **type-audit:** primitives at a `pub` boundary carry a `type-audit:` verdict tag. `shared_believed_water`'s `budget: usize` is `bare-ok(count: budget)` (identical to `believed_water`); its return `Option<RoomAddr>` is a typed value (no tag needed).
- **Determinism (constitutional):** no stream draw, no epoch, no new predicate, **zero save-format cost** (belief stays a UNI-20 derived view — sharing only widens the candidate set the fold ranks over). **Genesis byte-identical.** **Live health goldens and possession galleries byte-identical** — verified in Task 5 (live NPCs never co-locate → the share is a live no-op). Determinism is a pure function of ledger + co-location, seed-tiebroken by ascending `RoomAddr`.
- **Band-consistency invariant:** the `band` slice passed to `affect_of` MUST be the same set of NPCs the paired `DriveMovements` moves — the mover and the affect sampler must share belief over the same band, or a sampled affect will not reflect the belief the creature acted on.
- **Work on branch `worktree-the-tidings`, never main.** (Already in the campaign worktree.)

---

### Task 0: Baseline gate

**Files:** none (verification only).

- [ ] **Step 1: Establish a green baseline**

Run: `cargo nextest run -p hornvale-vessel -p hornvale-lab`
Expected: PASS (0 failures). This scopes the baseline to the two crates this plan touches; the full-workspace gate is the pre-merge step, not each task.

- [ ] **Step 2: Confirm fmt/clippy clean on the two crates**

Run: `cargo fmt --check && cargo clippy -p hornvale-vessel -p hornvale-lab --all-targets -- -D warnings`
Expected: PASS. If either fails on untouched code, STOP and report — the baseline is not clean.

---

### Task 1: The share law — `shared_believed_water` (pure)

The union-fill join. No wiring yet — a standalone pure function beside `believed_water` in `windows/vessel/src/liveness.rs`.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (add `shared_believed_water` just after `believed_water`, ~line 628; add `#[cfg(test)]` cases in the existing test module)

**Interfaces:**
- Consumes: `believed_water(&Ledger, &Npc, WorldTime, &dyn Terrain, usize) -> Option<RoomAddr>` (existing, `liveness.rs:607`); `agent_position(&Ledger, &Npc, WorldTime) -> RoomAddr` (existing, used at `liveness.rs:1986`); `plan_to_room(&RoomAddr, &RoomAddr, usize) -> Option<Vec<Action>>` (existing, `liveness.rs:2795`).
- Produces: `pub fn shared_believed_water(frozen: &Ledger, npc: &Npc, band: &[Npc], t: WorldTime, terrain: &dyn Terrain, budget: usize) -> Option<RoomAddr>` — `npc`'s pooled water belief: the nearest-to-`npc.home` water room known to `npc` or to ANY agent in `band` co-located with `npc` at `t` (same current room), ties by ascending `RoomAddr`; `None` if the whole co-located band is ignorant. A strict generalization of `believed_water`: with an empty band (or a band whose only co-located member is `npc`), it returns exactly `believed_water(npc)`.

- [ ] **Step 1: Write the failing tests**

Add to the `#[cfg(test)] mod tests` in `liveness.rs`. These reuse the test helpers already in that module (`PlantedTerrain`, `place_agent`/`AGENT_AT`, `mint`ing NPCs); mirror the construction in the existing `believed_water` tests around `liveness.rs:2894`.

```rust
#[test]
fn shared_belief_fills_an_ignorant_colocated_creature() {
    // Two creatures in the SAME room `here`; `knower` has stood at `water`,
    // `lost` never has. Both homed at `here`.
    let t = PlantedTerrain::with_water(&[water.clone()]);
    let mut ledger = Ledger::default();
    let registry = test_registry();
    let knower = mint_npc(&mut ledger, /*home*/ here.clone());
    let lost = mint_npc(&mut ledger, /*home*/ here.clone());
    // knower's perception history: stood at water, now back at `here`.
    commit_at(&mut ledger, &registry, knower.entity, &water, 0.0);
    commit_at(&mut ledger, &registry, knower.entity, &here, 1.0);
    // lost has only ever been at `here`.
    commit_at(&mut ledger, &registry, lost.entity, &here, 1.0);
    let band = [knower.clone(), lost.clone()];
    let now = WorldTime { day: 1.0 };

    // Alone, `lost` is ignorant.
    assert_eq!(believed_water(&ledger, &lost, now, &t, 10_000), None);
    // Co-located with `knower`, it learns the water.
    assert_eq!(
        shared_believed_water(&ledger, &lost, &band, now, &t, 10_000),
        Some(water.clone())
    );
}

#[test]
fn shared_belief_is_order_independent() {
    // Same setup; permuting the band yields identical results.
    // ... (build `knower`, `lost`, `water`, `here` as above) ...
    let now = WorldTime { day: 1.0 };
    let ab = [knower.clone(), lost.clone()];
    let ba = [lost.clone(), knower.clone()];
    assert_eq!(
        shared_believed_water(&ledger, &lost, &ab, now, &t, 10_000),
        shared_believed_water(&ledger, &lost, &ba, now, &t, 10_000),
    );
}

#[test]
fn shared_belief_is_a_noop_when_alone_or_band_empty() {
    // A lone knower's shared belief equals its own belief; an empty band is a no-op.
    let now = WorldTime { day: 1.0 };
    assert_eq!(
        shared_believed_water(&ledger, &knower, &[], now, &t, 10_000),
        believed_water(&ledger, &knower, now, &t, 10_000),
    );
}

#[test]
fn shared_belief_ignores_a_knower_in_a_different_room() {
    // `knower` knows water but stands in a DIFFERENT room than `lost` → no share.
    // Place knower's latest agent-at at `elsewhere` (≠ `here`).
    let now = WorldTime { day: 1.0 };
    assert_eq!(
        shared_believed_water(&ledger, &lost, &band, now, &t, 10_000),
        None
    );
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-vessel shared_belief 2>&1 | tail -20`
Expected: FAIL — `cannot find function shared_believed_water`.

- [ ] **Step 3: Implement `shared_believed_water`**

Insert after `believed_water` (after `liveness.rs:628`):

```rust
/// The BAND's water belief for `npc` (The Tidings; anchoring split per
/// decision #8). With NO co-located peer, returns `believed_water(npc)`
/// verbatim — the home-anchored nearest water it remembers — an exact no-op
/// (this is what keeps the live one-per-settlement population byte-identical).
/// With a co-located peer, pools `npc`'s and every co-located peer's
/// `believed_water` and returns the one nearest to `npc`'s CURRENT position
/// (ties: ascending `RoomAddr`), `None` if the pool is empty. Current-position
/// anchoring is the semantics of hearsay — "water near HERE" — and is what lets
/// a stranded creature adopt a here-reachable water its home-anchored memory
/// could never admit. Order-independent by construction (`BTreeSet` union +
/// deterministic `min`); no RNG. BELIEF == FOLD (UNI-20): stores nothing.
/// type-audit: bare-ok(count: budget)
pub fn shared_believed_water(
    frozen: &Ledger,
    npc: &Npc,
    band: &[Npc],
    t: WorldTime,
    terrain: &dyn Terrain,
    budget: usize,
) -> Option<RoomAddr> {
    let own = believed_water(frozen, npc, t, terrain, budget);
    let here = agent_position(frozen, npc, t);
    let mut pool: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
    let mut has_peer = false;
    // Co-located OTHERS (never npc itself) contribute what they know of water.
    for other in band {
        if other.entity != npc.entity && agent_position(frozen, other, t) == here {
            has_peer = true;
            if let Some(w) = believed_water(frozen, other, t, terrain, budget) {
                pool.insert(w);
            }
        }
    }
    // ALONE: home-anchored memory, unchanged — the byte-identical no-op.
    if !has_peer {
        return own;
    }
    // CO-LOCATED: rank the pooled beliefs (npc's + peers') by nearness to npc's
    // CURRENT position (ties: ascending RoomAddr) — act on what's reachable HERE.
    if let Some(w) = own {
        pool.insert(w);
    }
    pool.into_iter()
        .filter_map(|r| plan_to_room(&here, &r, budget).map(|p| (p.len(), r)))
        .min_by(|(la, ra), (lb, rb)| la.cmp(lb).then_with(|| ra.cmp(rb)))
        .map(|(_, r)| r)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel shared_belief 2>&1 | tail -20`
Expected: PASS (4 tests).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): shared_believed_water — the union-fill share law (The Tidings task 1)"
```

---

### Task 2: Wire the share law into the mover (`DriveMovements::step`)

The band moves using its pooled belief: a co-located lost creature now plans toward the shared water. `DriveMovements` already carries `self.npcs` (`liveness.rs` struct; constructed at `health.rs:71`, `session.rs:335`) — the band is in hand.

**Files:**
- Modify: `windows/vessel/src/liveness.rs:2217` (the belief seed inside `DriveMovements::step`)
- Test: `#[cfg(test)]` in `liveness.rs` (a DriveMovements-level test)

**Interfaces:**
- Consumes: `shared_believed_water` (Task 1); `self.npcs: Vec<Npc>` (the band the system moves).
- Produces: no signature change — an internal call-site swap.

- [ ] **Step 1: Write the failing test**

A band of two NPCs placed in the SAME room, one knowing water; step the system one tick; assert the lost creature's emitted move heads toward the shared water (mirror the DriveMovements tests around `liveness.rs:3236`).

```rust
#[test]
fn a_colocated_lost_creature_moves_toward_shared_water() {
    // knower + lost in room `here`; knower has water history; homes at `here`.
    // Build the ledger/registry/terrain as in the DriveMovements tests.
    let sys = DriveMovements {
        npcs: vec![knower.clone(), lost.clone()],
        from: WorldTime { day: 1.0 },
        to: WorldTime { day: 2.0 },
        params: SUSTENANCE,
        terrain: &t,
    };
    let next = tick(&ledger, &[&sys], &["drive-movements"], &registry).expect("tick");
    // The lost creature, previously ignorant, now has a target and steps off `here`.
    let lost_pos_after = agent_position(&next, &lost, WorldTime { day: 2.0 });
    assert_ne!(lost_pos_after, here, "shared belief gave the lost creature a water target to pursue");
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel a_colocated_lost 2>&1 | tail -20`
Expected: FAIL — the lost creature holds (no belief) and does not move off `here`.

- [ ] **Step 3: Swap the belief seed to the shared law**

At `liveness.rs:2217`, change:

```rust
            let mut believed = believed_water(frozen, npc, self.from, self.terrain, PLAN_BUDGET);
```

to:

```rust
            // The Tidings: seed from the BAND's pooled belief (co-located
            // members share what they know), not the creature's alone.
            let mut believed =
                shared_believed_water(frozen, npc, &self.npcs, self.from, self.terrain, PLAN_BUDGET);
```

(The subsequent local growth at `liveness.rs:2232` — `believed = nearer_to_home(...)` when standing in water — is unchanged; it now grows from the shared seed.)

- [ ] **Step 4: Run the test (and the crate's belief/mover tests) to verify green**

Run: `cargo test -p hornvale-vessel 2>&1 | tail -25`
Expected: PASS. Existing single-agent DriveMovements tests are unaffected (a singleton band's shared belief == its own belief).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): the band moves on shared belief in DriveMovements (The Tidings task 2)"
```

---

### Task 3: Wire the share law into the affect sampler (`affect_of`)

`affect_of` is single-npc; it gains a `band` parameter so a sampled felt state reflects the same shared belief the creature acted on (the band-consistency invariant).

**Files:**
- Modify: `windows/vessel/src/liveness.rs:1985` (add `band` param) and `:1992` (use the shared law)
- Modify callers: `windows/lab/src/health.rs:92`; `windows/vessel/src/session.rs:505`; the test callers in `liveness.rs` (`:4768`, `:5810`, `:5819`, `:5857`, `:5863`)
- Test: `#[cfg(test)]` in `liveness.rs`

**Interfaces:**
- Consumes: `shared_believed_water` (Task 1).
- Produces: `pub fn affect_of(frozen: &Ledger, npc: &Npc, band: &[Npc], day: WorldTime, terrain: &dyn Terrain) -> Affect` — `band` inserted after `npc` (the subject, then its cohort, then the moment).

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_colocated_lost_creature_feels_relief() {
    // knower + lost co-located in `here`; lost is ignorant alone (chronic thirst).
    // Alone, lost reads a distress label (Frustrated/Lost); with the band it does not.
    let now = WorldTime { day: 1.0 };
    let alone = affect_of(&ledger, &lost, &[], now, &t);
    let in_band = affect_of(&ledger, &lost, &[knower.clone(), lost.clone()], now, &t);
    assert!(is_distress(alone.label), "alone and ignorant, the creature is in distress");
    assert!(!is_distress(in_band.label), "the shared water belief relieves it");
    // (import `is_distress` or assert on the concrete labels the module exposes)
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel a_colocated_lost_creature_feels 2>&1 | tail -20`
Expected: FAIL — `affect_of` takes 4 args, not 5 (compile error), or the belief is unshared.

- [ ] **Step 3: Add the `band` param and use the shared law**

At `liveness.rs:1985` change the signature:

```rust
pub fn affect_of(frozen: &Ledger, npc: &Npc, band: &[Npc], day: WorldTime, terrain: &dyn Terrain) -> Affect {
```

At `liveness.rs:1992` change the belief line:

```rust
    let believed = shared_believed_water(frozen, npc, band, day, terrain, PLAN_BUDGET);
```

- [ ] **Step 4: Update all callers**

- `windows/lab/src/health.rs:92` — pass the full band (the same `npcs` the paired `DriveMovements` at `:71` moves):
  ```rust
              traces[i].push(affect_of(&ledger, npc, npcs, now, terrain));
  ```
- `windows/vessel/src/session.rs:505` — pass the NPC roster the session's `DriveMovements` (`:335`) moves (bind it to a local `let band = ...;` if not already in scope; NPCs are one-per-settlement, so this is a no-op today and keeps the mover/sampler consistent):
  ```rust
                  let affect = affect_of(&self.ledger, npc, &band, self.day, &terrain);
  ```
- The five test callers in `liveness.rs` (`:4768`, `:5810`, `:5819`, `:5857`, `:5863`) — pass `&[]` (single-agent tests; empty band == own belief, byte-identical):
  ```rust
          let a = affect_of(&ledger, &base, &[], WorldTime { day: 0.5 }, &terrain);
  ```

- [ ] **Step 5: Run the crate + lab tests to verify green**

Run: `cargo test -p hornvale-vessel -p hornvale-lab 2>&1 | tail -25`
Expected: PASS. The single-agent affect tests are byte-identical (empty band).

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs windows/lab/src/health.rs windows/vessel/src/session.rs
git commit -m "feat(vessel,lab): affect_of samples shared belief over the band (The Tidings task 3)"
```

---

### Task 4: The harness band scenario + the paired health result

Demonstrate and MEASURE the law with a MATCHED PAIR that isolates co-location as the only variable: the same stricken creature and the same knowledgeable neighbour, run once **co-located** (sharing fires) and once **apart** (no sharing). Co-located, the band's aggregate distress drops.

**The affect mechanism you MUST design around (confirmed against `liveness.rs:1966-2005` during Task 3):**
- The health metric counts only DISTRESS labels — `Frustrated`/`Lost`/`Helpless` (`health.rs::is_distress`). `Searching` and `Eager` are NOT distress.
- An **ignorant** creature (no `believed_water`) reads **`Searching`**, never distress: `affect_of` always has an exploration step available, so it is never "blocked." **So an ignorant creature is USELESS as the distressed subject — it will not register in the health metric.**
- **`Frustrated`** arises only when the creature KNOWS a water (`believed_water = Some`) but cannot reach it within budget (blocked → Hold). This is the distress the demo needs.
- Therefore the stricken creature must KNOW a FAR, UNREACHABLE water (→ chronic `Frustrated`), and sharing must supply a NEARER, REACHABLE one so it flips to `Eager`/`Searching`.

**Reachability facts (from `synthetic.rs:230` + the mesh):**
- `water_and_a_far_exile()` returns `(spring, exile)` at antipodal points — `spring` is UNREACHABLE from `exile` (this is exactly what makes the existing `stranded_from_known_water`/`a_stricken_and_a_healthy_people` chronically `Frustrated`).
- A 1-hop neighbour `exile.neighbors()[0]` is REACHABLE from `exile` and reachable within a single tick's walk (`MAX_STEPS = 10_000`). This is the "near" water.
- A `Frustrated` creature that KNOWS a water does NOT fall back to exploration — it commits and Holds — so the stricken will NOT wander onto the near water on its own; it stays `Frustrated` until told.
- Belief is a per-tick fold (not stored). Sharing SEEDS the belief at tick start; because the near water is 1 hop away, the stricken walks to it and DRINKS within that same tick, so it thereafter knows the near water from its own visit history — co-location is only needed to seed the first tick.

**Files:**
- Modify: `windows/lab/src/synthetic.rs` (add a private `a_stranded_pair` builder + `pub fn a_band_that_shares_water`, near `a_stricken_and_a_healthy_people` at `:502`)
- Test: `#[cfg(test)]` in `windows/lab/src/synthetic.rs` (or the lab's health test module)

**Interfaces:**
- Consumes: `Scenario`, `creature`, `place_agent`, `SyntheticTerrain`, `water_and_a_far_exile`, `health_report`, `HealthReport`, `RoomAddr::neighbors` (all existing in `windows/lab`/kernel).
- Produces: `pub fn a_band_that_shares_water() -> Scenario` — the co-located treatment; plus a private `fn a_stranded_pair(colocated: bool) -> Scenario` the test also calls with `false` for the matched null.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_colocated_band_is_healthier_than_the_same_band_apart() {
    // Matched pair: identical stricken + knower; the ONLY difference is whether
    // the knower stands with the stricken (sharing) or apart (no sharing).
    let shared = health_report(&a_stranded_pair(true).simulate(HEALTH_TICKS));   // co-located
    let apart = health_report(&a_stranded_pair(false).simulate(HEALTH_TICKS));   // separated null
    assert!(
        shared.prevalence < apart.prevalence,
        "co-located sharing heals the stricken creature: {} < {}",
        shared.prevalence, apart.prevalence
    );
    assert!(shared.chronicity <= apart.chronicity, "sharing never worsens chronicity");
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-lab a_colocated_band 2>&1 | tail -20`
Expected: FAIL — `cannot find function a_stranded_pair`.

- [ ] **Step 3: Implement the matched-pair builder + the public treatment**

Add near `synthetic.rs:502`. The `colocated` flag changes ONLY the knower's current room (with the stricken at `exile`, or apart at the near water):

```rust
/// A stranded pair (The Tidings; decision #8). The **stricken** creature is
/// HOMED at `spring` (so its home-anchored `believed_water` genuinely holds
/// `spring`) but is marooned far away at `exile`, from which `spring` is
/// unreachable — so alone it reads chronic `Frustrated` (it KNOWS water it
/// cannot reach). The **knower** is homed at `exile` and has stood at a `near`
/// water one hop from `exile` (reachable-from-`exile`), so it believes `near`.
/// When `colocated`, the knower stands with the stricken at `exile`; the share
/// law (current-position-anchored for co-located agents) pools `near` into the
/// stricken's belief, `near` is reachable from `exile`, so the stricken walks
/// there within the tick, drinks, and is relieved. When apart, the knower sits
/// on `near` and the stricken stays `Frustrated`. The knower's HOME is `exile`
/// in BOTH cases — only its current position (and thus co-location) differs.
fn a_stranded_pair(colocated: bool) -> Scenario {
    let (spring, exile) = water_and_a_far_exile();
    let near = exile.neighbors()[0].clone(); // one hop from exile → reachable-from-exile
    let mut ledger = Ledger::default();
    let registry = harness_registry();

    // The stricken: homed at spring, stood there, then marooned at exile. Its
    // home-anchored belief = spring (unreachable from exile) → chronic Frustrated.
    let stricken = ledger.mint_entity();
    ledger.commit(place_agent(stricken, &spring, WorldTime { day: 0.0 }), &registry)
        .expect("stricken once at spring");
    ledger.commit(place_agent(stricken, &exile, WorldTime { day: 0.5 }), &registry)
        .expect("stricken marooned at exile");

    // The knower: homed at exile, stood at the near water (so it KNOWS a source
    // reachable from exile). It then either joins the stricken at exile
    // (colocated) or stays on the near water (apart). Home is exile either way.
    let knower = ledger.mint_entity();
    ledger.commit(place_agent(knower, &near, WorldTime { day: 0.0 }), &registry)
        .expect("knower at near water");
    let knower_now = if colocated { exile.clone() } else { near.clone() };
    ledger.commit(place_agent(knower, &knower_now, WorldTime { day: 0.5 }), &registry)
        .expect("knower placed");

    let npcs = vec![
        creature(stricken, spring.clone(), spring.clone(), "kobold", MILD_NICHE),
        creature(knower, exile.clone(), near.clone(), "goblin", MILD_NICHE),
    ];
    Scenario {
        ledger,
        registry,
        npcs,
        terrain: SyntheticTerrain {
            fresh: [spring, near].into_iter().collect(),
            temps: BTreeMap::new(),
            calm_after: None,
            forage: BTreeMap::new(),
            threat: BTreeMap::new(),
        },
    }
}

/// The co-located treatment (The Tidings headline result): a band whose
/// knowledgeable member heals its stranded one by circulating a reachable water.
/// Its matched null is `a_stranded_pair(false)` — the same pair standing apart.
pub fn a_band_that_shares_water() -> Scenario {
    a_stranded_pair(true)
}
```

- [ ] **Step 4: Run the test — and VERIFY the signal is real (do not fake it)**

Run: `cargo test -p hornvale-lab a_colocated_band 2>&1 | tail -20`
Expected: PASS.

If it does NOT pass, the construction — not the assertion — is wrong; iterate on it. Confirm the two halves independently before trusting the pass: the null (`a_stranded_pair(false)`) MUST show the stricken chronically `Frustrated` (non-zero `prevalence` and `by_cause["thirst"] > 0`), and the treatment MUST relieve it (near-zero `prevalence`). If you cannot make the null distressed AND the treatment relieved with a deterministic construction, STOP and reply `BLOCKED:` with what you observed — never weaken the assertion to make a hollow test pass. (Common fixes: the day-0.5 placement must make each creature's LATEST position correct at tick start; `near` must actually be fresh water and one hop; the stricken must not itself know `near`.)

- [ ] **Step 5: Add the monotone-improvement null-control (the ratchet)**

```rust
#[test]
fn sharing_never_increases_band_distress() {
    // v1 shares only TRUE beliefs → sharing can only help: the co-located band is
    // never worse than the same band apart, on prevalence or chronicity.
    let shared = health_report(&a_stranded_pair(true).simulate(HEALTH_TICKS));
    let apart = health_report(&a_stranded_pair(false).simulate(HEALTH_TICKS));
    assert!(shared.prevalence <= apart.prevalence);
    assert!(shared.chronicity <= apart.chronicity);
}
```

Run: `cargo test -p hornvale-lab sharing_never_increases 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/lab/src/synthetic.rs
git commit -m "feat(lab): a_band_that_shares_water — the measured band result (The Tidings task 4)"
```

---

### Task 5: Lab study, artifact regeneration, and determinism verification

Preregister the result as a study (data), regenerate artifacts, and PROVE the live no-op (the byte-identity claim).

**Files:**
- Create: `studies/the-tidings.study.json` (a study preregistering the shared-vs-separated comparison) — follow the shape of an existing `studies/*.study.json` and the lab CLAUDE.md "studies preregister their hypotheses" rule.
- Modify (regenerate, do not hand-edit): whatever `git diff` flags under `book/src/`.

- [ ] **Step 1: Author the study**

Create `studies/the-tidings.study.json` mirroring an existing study's schema (seeds × pins × metrics/scenarios), preregistering the hypothesis "a co-located sharing band shows lower prevalence and no-worse chronicity than the separated null." If the existing study schema has no seam for a synthetic-scenario comparison (studies target world metrics), SKIP a new study and instead rely on the Task 4 tests as the preregistered evidence — `log` this choice in the commit message rather than forcing a study shape that doesn't fit. (Do not invent a schema; check `cargo run -p hornvale -- lab list-metrics` and an existing study first.)

- [ ] **Step 2: Regenerate artifacts and inspect drift**

Run the artifact commands from the CI "Artifacts are current" step (root `CLAUDE.md`), then:

Run: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json && cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md`
Then: `git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/`

- [ ] **Step 3: Verify the live no-op (the determinism claim)**

Expected: **no drift** in the live possession galleries and the almanac (live NPCs are one-per-settlement → the share is a live no-op). If `git diff` shows drift in a LIVE artifact (not a new lab study), STOP: it means two live NPCs co-located during the sweep, which contradicts the spec's byte-identity claim. That drift is *correct behaviour* but a real finding — record it in `.superpowers/sdd/decision-ledger.md` (a determinism-contract entry, leads the G6 digest) and surface it to Nathan before regenerating the live golden.

- [ ] **Step 4: Run the full workspace gate**

Run: `make gate`
Expected: PASS (fmt + clippy + nextest + doctests; heavy tier skipped). This is the pre-merge gate; run it once, inspect the file if needed (`… 2>&1 | tee /tmp/hv-test.txt`).

- [ ] **Step 5: Commit**

```bash
git add studies/ book/ 2>/dev/null; git commit -m "build(lab): the-tidings study + artifact freshness; live byte-identity verified (The Tidings task 5)"
```

---

### Task 6: Close (via `closing-a-campaign`)

Not a code task — the campaign-close DoD. Use the `closing-a-campaign` skill, which covers: the chronicle entry (`book/src/chronicle/the-tidings.md`); the retrospective (`docs/retrospectives/the-tidings.md`, promoting `.superpowers/sdd/followups.md`); flipping the `SOC-belief-sharing` registry row `raw → shipped` (recording the harness-only-until-population-field scope) and re-scoring any Confidence-Gradient bet it touches; the stale-chapter freshness sweep; and the G6 ledger digest for Nathan (determinism/save-format entries leading). The population-field substrate, false-belief correction, `SOC-information-economy`, and The Discernment stay captured as followups, not built here.

## Self-Review

**Spec coverage:** §1 share law → Task 1. §2 harness demonstration → Tasks 2–4. §3 band health metric (paired comparison) → Task 4. §4 live/possession unchanged → Task 3 caller updates + Task 5 Step 3 verification. Determinism section → Global Constraints + Task 5. Model card deferrals → Task 6 (captured, not built). Test plan → Tasks 1, 3, 4 tests. All sections map to a task.

**Placeholder scan:** No TBD/TODO. Every code step shows the code; test bodies show real assertions (the scenario/ledger construction references the exact existing helpers and line numbers to copy from, per the "repeat the code / point at the source" rule for test scaffolding that mirrors existing tests).

**Type consistency:** `shared_believed_water(frozen, npc, band, t, terrain, budget) -> Option<RoomAddr>` is defined in Task 1 and consumed with that exact signature in Tasks 2 (`&self.npcs`) and 3 (`band`). `affect_of` gains `band: &[Npc]` in the same argument position (after `npc`) at every one of its call sites listed in Task 3. `PLAN_BUDGET` is the existing constant used at both original call sites. The band-consistency invariant (mover's `npcs` == sampler's `band`) is stated in Global Constraints and honoured at both production call sites in Task 3.
