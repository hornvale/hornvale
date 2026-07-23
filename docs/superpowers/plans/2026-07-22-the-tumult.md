# The Tumult (Predation) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make conflict emerge from *coveting value down a strength gradient*, not from crowding: a community raids the reachable neighbour whose land it covets when it can win; lossy war and the death of broken remnants supply the dissipation; the displaced roll downhill; the cascade-size distribution is measured against a power law.

**Architecture:** Rewrite `history_bake.rs`'s conflict logic. Strength = `population × tech`; coveted value = the existing per-cell `capacity`. A new `maybe_raid` (covet + dominance, deterministic) replaces the `pressure >= 1.0` trigger; the climate path reverts to migrate-or-die; `relocate` becomes the conflict-driven roll-downhill (displace a *beatable* occupant, die below a viable minimum). SOC is *measured*, not engineered. Salvages T1–T3 infra (`nearest_occupied`, `relocate`, the cascade histogram, `history_for`, the gate scaffolding) — this plan **edits forward from the current branch**, it does not revert it.

**Tech Stack:** Rust 2024, `hornvale-worldgen` (`history_bake.rs`), `hornvale-history`, kernel. `cargo nextest` + doctests; `make gate` / `make gate-full`.

## Global Constraints

- **Determinism (constitutional):** same seed + pins ⇒ byte-identical skeleton. `BTreeMap`/`BTreeSet`/`Vec` only. Every float compare via `f64::total_cmp`. No RNG beyond kernel `Seed`/`Stream`; no wall-clock. **No new seed draw.** The raid is a total, deterministic function of frozen epoch state (no agent choice).
- **Lorenz-safe:** the raid reads state (strength, value, graph reach); it does NOT forward-integrate a chaotic pressure variable. Bounded cascade depth (`CASCADE_DEPTH_CAP`, exists).
- **Density dropped:** crowding/pressure is NEVER a conflict trigger. Pressure governs only growth (logistic) and Famine (collapse). The raid trigger is covetousness (`capacity(target) > capacity(raider)`) + dominance (`strength(raider) > strength(target) × RAID_MARGIN`).
- **No new committed field, predicate, or stream label.** A raid is a chain of existing `CauseOfEnd::Fled` / `Ended::By` records; strength/value read existing state.
- **type-audit:** new pub-boundary primitives carry a `type-audit:` verdict tag. New consts (`RAID_MARGIN`, `WAR_LOSS`, `VIABLE_MIN`, tech weights) are `f64`/count → tagged.
- **measure-don't-narrate:** every gate is a real assertion; the power-law metric is a falsification headline — **either outcome ships**. If conflict stays inert or the map depopulates, that is a calibration finding for Nathan (fidelity carve-out), never a floor.
- **Census regen is LOCAL on `lefford` (0063)** — census regen + keystone refreeze at the G6 close.

---

## File Structure

- `windows/worldgen/src/history_bake.rs` — **modified.** Add `fn strength(&self, idx) -> f64` (population × tech weight) + tech-weight consts; add `const RAID_MARGIN`, `WAR_LOSS`, `VIABLE_MIN`; add `fn maybe_raid(&mut self, idx, era, year)` (the covet+dominance opportunistic raid, lossy plunder); rewrite `step_community` (climate path → migrate-or-die; drop the `pressure >= 1.0` raid; grow then `maybe_raid`); repurpose `relocate`'s displace branch to pick a *beatable weaker* occupant (dominance) and die below `VIABLE_MIN`. Remove the now-dead `raid`/`raid_target` (or repurpose) as the plan directs.
- `windows/worldgen/tests/history_bake.rs` — **modified.** Value-driven-raid-fires-with-land-to-spare; lossy; the conflict cascade fires + dissipates + terminates; byte-identity stays green.
- `windows/worldgen/tests/history_tumult.rs` — **modified.** Re-point `conflict_fires_at_volume` at the value-driven raids (seed-42 now fights); the power-law metric re-adjudicated on the new model.
- `windows/worldgen/tests/history_gates.rs` / `history_placement.rs` — **re-measure** (re-pin if a floor moved, labelled).
- `cli/tests/graph_cost.rs` — **modified.** The conflict-bake cost gate (reuse the T4 shape).

Close (G6, `closing-a-campaign`): census regen on lefford, cascade re-pins, keystone refreeze, artifact drift, chronicle (incl. the crowding→predation reframe), retrospective, Confidence Gradient re-score, registry flip (SOC-criticality → elaborated/slice-1 with the measured result), full gate.

---

### Task 1: Predation — strength, coveted value, and the raid rule

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Test: `windows/worldgen/tests/history_bake.rs`

**Interfaces:**
- Consumes: `self.communities[idx].{population, tech, site}`; `self.capacity: &CellMap<f64>`; `traversable_neighbors(self.cur(), site)`; `self.node_index`; `TechHorizon` (`Neolithic < Bronze < Iron < Classical`); `relocate` (T2 repurposes it).
- Produces: `fn strength(&self, idx: usize) -> f64`; `const RAID_MARGIN: f64`; `const WAR_LOSS: f64`; `const VIABLE_MIN: f64`; `fn maybe_raid(&mut self, raider: usize, era: &EraClimate, year: f64)`.

- [ ] **Step 1: Write the value-driven-raid test (failing).**

Add to `history_bake.rs`'s `#[cfg(test)] mod tests`: a fixture with **land to spare** (mostly vacant), a STRONG community on POOR land adjacent (over the graph) to a WEAKER community on RICH land. Assert the strong one RAIDS the weak-rich one (`census(&h).raided > 0`) — proving conflict fires on value×strength *with vacant land available* (density is not the trigger). Construct via a small direct `Bake` or a saturating-optional fixture; keep it deterministic.

```rust
#[test]
fn a_strong_community_raids_a_weaker_richer_neighbour_with_land_to_spare() {
    // Geosphere::new(1); MOST cells vacant+habitable. Seed a STRONG community
    // (high pop, higher tech) on a LOW-capacity cell adjacent to a WEAK
    // community (low pop) on a HIGH-capacity cell. Bake a few epochs.
    // Assert census.raided > 0 (conflict fired despite vacant land everywhere).
    // ... construct, bake, assert ...
}
```

- [ ] **Step 2: Run — expect FAIL** (current code raids only under `pressure >= 1.0`; with land to spare nobody is over-pressure, so `raided == 0`).

Run: `cargo test -p hornvale-worldgen --test history_bake a_strong_community_raids 2>&1 | tail -20`
Expected: FAIL — `raided` is 0.

- [ ] **Step 3: Implement `strength` + `maybe_raid` + rewrite `step_community`.**

Add the strength function and consts:

```rust
/// The tech multiplier on raw population when reckoning a community's raiding
/// strength — Iron beats Bronze beats Neolithic. Monotone in `TechHorizon`.
fn tech_weight(t: TechHorizon) -> f64 {
    match t {
        TechHorizon::Neolithic => 1.0,
        TechHorizon::Bronze => 1.5,
        TechHorizon::Iron => 2.25,
        TechHorizon::Classical => 3.0,
    }
}

/// A community's raiding strength: population scaled by its tech horizon.
/// Heterogeneous strength is the fuel of predation; equals do not prey.
fn strength(&self, idx: usize) -> f64 {
    let c = &self.communities[idx];
    c.population * tech_weight(c.tech)
}

/// How much stronger a raider must be than its target to attack (dominance
/// margin). Save-format constant. type-audit: bare-ok(ratio)
const RAID_MARGIN: f64 = 1.5;
/// Fraction of the loser's population destroyed in a raid (war is lossy — the
/// primary dissipation). type-audit: bare-ok(ratio)
const WAR_LOSS: f64 = 0.3;
/// Population below which a broken/displaced remnant dies out rather than
/// cascading further (the avalanche cutoff — the second dissipation).
/// type-audit: bare-ok(count)
const VIABLE_MIN: f64 = 2.0;
```

Add `maybe_raid` (opportunistic, covet + dominance, lossy plunder):

```rust
/// Opportunistic predation (The Tumult): a community raids the reachable
/// occupied neighbour whose land is worth MORE than its own (covetousness)
/// and that it can beat (dominance) — decoupled from its own crowding. It
/// plunders (seizes population; a fraction is destroyed — lossy war); if the
/// target is broken below `VIABLE_MIN` it is driven off (`Fled`) and rolls
/// downhill via `relocate`. Deterministic: most-valuable target, tie-broken by
/// weakest then lowest `CellId`.
fn maybe_raid(&mut self, raider: usize, era: &EraClimate, year: f64) {
    let raider_site = self.communities[raider].site;
    let raider_str = self.strength(raider);
    let raider_val = *self.capacity.get(raider_site);
    let mut best: Option<(usize, f64, f64, CellId)> = None; // (target_idx, value, strength, cell)
    for n in traversable_neighbors(self.cur(), raider_site) {
        let Some(&t) = self.node_index.get(&n) else { continue };
        let t_val = *self.capacity.get(n);
        let t_str = self.strength(t);
        if t_val <= raider_val { continue; }                 // covet only BETTER land
        if raider_str <= t_str * RAID_MARGIN { continue; }   // dominance: can win
        let better = match best {
            None => true,
            Some((_, bv, bs, bc)) => t_val.total_cmp(&bv)
                .then(bs.total_cmp(&t_str))   // among equal value, the WEAKEST
                .then(bc.cmp(&n))             // then lowest CellId
                .is_lt(),
        };
        if better { best = Some((t, t_val, t_str, n)); }
    }
    let Some((target, _, _, _)) = best else { return };
    self.tally.raided += 1;
    let seized = self.communities[target].population * RAID_SEIZE;
    let loss = self.communities[target].population * WAR_LOSS;
    self.communities[raider].population += seized;
    self.communities[target].population -= seized + loss;
    self.touch(raider, year);
    // Broken below viability ⇒ driven off, rolls downhill; else survives (weakened).
    if self.communities[target].population < VIABLE_MIN {
        let (people, remaining, lineage, offset, target_id) = {
            let c = &self.communities[target];
            (self.records[c.record].people, c.population.max(0.0), c.lineage, c.tech_offset, c.id)
        };
        let raider_id = self.communities[raider].id;
        let flee_site = self.communities[target].site;
        self.close(target, year, CauseOfEnd::Fled, Ended::By(raider_id));
        self.tally.fled += 1;
        match self.relocate(people, remaining, lineage, target_id, offset, flee_site, era, year, 0) {
            Relocation::Settled { cascade: 0 } => self.tally.resettled += 1,
            Relocation::Settled { cascade } => self.tally.record_cascade(cascade),
            Relocation::Lost => self.tally.collapsed += 1,
        }
    }
}
```

Rewrite `step_community`: revert the climate path to migrate-or-die (no crowding-cascade), drop the `pressure >= 1.0` raid, and grow-then-`maybe_raid`:

```rust
fn step_community(&mut self, idx: usize, era: &EraClimate, year: f64) {
    if !self.communities[idx].alive { return; }
    let site = self.communities[idx].site;
    let eff = self.eff_capacity(era, site);
    // Climate eviction: migrate to a vacant refuge, or starve. (No conflict here.)
    if eff == 0.0 {
        let (record, pop, lineage, offset, migrant_id) = {
            let c = &self.communities[idx];
            (c.record, c.population, c.lineage, c.tech_offset, c.id)
        };
        let people = self.records[record].people;
        match self.nearest_dest(era, site) {
            Some(dest) => {
                self.close(idx, year, CauseOfEnd::Migrated, Ended::Nature);
                let ni = self.open(people, dest, year, pop * MIGRATE_SURVIVAL, Founding::From(migrant_id), Some(lineage), offset);
                self.touch(ni, year);
                self.tally.migrated += 1;
            }
            None => { self.close(idx, year, CauseOfEnd::Famine, Ended::Nature); self.tally.collapsed += 1; }
        }
        return;
    }
    let pressure = self.communities[idx].population * NEED / eff;
    if pressure >= COLLAPSE_PRESSURE {
        self.close(idx, year, CauseOfEnd::Famine, Ended::Nature);
        self.tally.collapsed += 1;
        return;
    }
    self.grow(idx, era, year, pressure);
    // Opportunistic predation — decoupled from pressure (density is NOT the trigger).
    if self.communities[idx].alive {
        self.maybe_raid(idx, era, year);
    }
}
```

Delete the now-unused `raid` and `raid_target` (or leave `raid_target` if `nearest_occupied`/`maybe_raid` fully replace it — remove dead code to keep clippy clean).

- [ ] **Step 4: Run — expect PASS** (+ existing tests green).

Run: `cargo test -p hornvale-worldgen --test history_bake 2>&1 | tail -25`
Expected: `a_strong_community_raids_a_weaker_richer_neighbour_with_land_to_spare` PASSES; `same_seed_bakes_byte_identical_history` and the all-land/displacement tests still PASS.

- [ ] **Step 5: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/history_bake.rs windows/worldgen/tests/history_bake.rs
git commit -m "feat(history): predation — raid for coveted value down a strength gradient, density dropped (the-tumult T1)"
```

---

### Task 2: The conflict-driven roll-downhill (dissipation + cutoff)

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Test: `windows/worldgen/tests/history_bake.rs`

**Interfaces:**
- Consumes: `relocate` (existing, from T2 of the first design — its displace branch), `nearest_occupied`, `strength`, `VIABLE_MIN`, `WAR_LOSS`.
- Produces: `relocate` modified so its displace branch targets a *beatable weaker* occupant (dominance), applies war-loss per hop, and returns `Relocation::Lost` when the roller falls below `VIABLE_MIN` (the cutoff).

- [ ] **Step 1: Write the cascade-dissipates test (failing).**

Add a test: a displaced strong-ish group rolls downhill through a chain of progressively weaker occupied neighbours, losing population each hop, until a remnant falls below `VIABLE_MIN` and DIES (the cascade terminates by dissipation, not only by the depth cap). Assert: a cascade fires (`cascade_hist` populated), it terminates well short of `CASCADE_DEPTH_CAP`, and the total population strictly decreases across the chain (lossy). Deterministic.

- [ ] **Step 2: Run — expect FAIL** (current `relocate` displaces the nearest occupant regardless of strength and never dies below a viable minimum — cascades run to the cap).

- [ ] **Step 3: Modify `relocate`'s displace branch.**

In `relocate` (the existing recursion): (a) at entry, if `pop < VIABLE_MIN` return `Relocation::Lost` (the remnant died — the cutoff); (b) the displace branch chooses, among reachable occupied cells, the nearest the roller can BEAT (`strength_of_roller > strength(occupant) × RAID_MARGIN` — dominance; shit rolls *downhill*, onto the weaker), not merely the nearest occupied; if none beatable, `Relocation::Lost` (nowhere weaker to go — it dissipates); (c) apply `WAR_LOSS` to the roller's `pop` on each displacement hop (lossy). The victim is closed `Fled`/`By(displacer)` and recurses (open-before-close ordering unchanged). The roller's strength must be recomputed from its carried `pop` (it has no live community during the roll — pass strength as a value or compute from `pop × tech_weight` using the carried tech).

(Note: `relocate` already carries `people, pop, lineage, predecessor, offset`; add the carried `tech` or compute the roller's strength from `pop` and a carried tech horizon so the dominance test works mid-roll.)

- [ ] **Step 4: Run — expect PASS** (+ byte-identity + Task-1 raid test green).

Run: `cargo test -p hornvale-worldgen --test history_bake 2>&1 | tail -25`

- [ ] **Step 5: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/history_bake.rs windows/worldgen/tests/history_bake.rs
git commit -m "feat(history): the roll-downhill dissipates — displace the beatable, die below viable (the-tumult T2)"
```

---

### Task 3: Re-measure the falsification gates on the predation model

**Files:**
- Modify: `windows/worldgen/tests/history_tumult.rs`
- Re-measure: `windows/worldgen/tests/history_gates.rs`, `history_placement.rs`

- [ ] **Step 1: Re-point `conflict_fires_at_volume` at the predation raids.**

`conflict_fires_at_volume` now asserts seed-42's `census.raided` (or the cascade total) rises from ZERO — proving conflict fires on **value × strength** where the crowding model fired nothing. Set `MIN` clear below the measured value (Step 3). If seed-42 STILL fires no conflict, that is a raid-margin calibration finding for Nathan (fidelity carve-out), reported `DONE_WITH_CONCERNS` — do NOT lower a floor to hide it.

- [ ] **Step 2: Keep the not-depopulated + power-law gates; re-adjudicate the shape.**

`cascades_do_not_depopulate_the_world` unchanged in intent (lossy war + downhill must not empty the map; ceiling above measured). The heavy `cascade_sizes_are_measured_and_the_shape_adjudicated` re-runs on the predation model: **measure the pooled histogram and REPORT it; do NOT tune toward power-law.** If heavy-tailed → pin a measured shape (SOC confirmed). If bell/spike/geometric → assert only the honest floor and reply `DONE_WITH_CONCERNS` with the histogram (documented falsification → diagnoses the dominance-hierarchy next slice). Either ships.

- [ ] **Step 3: Measure and set thresholds.**

Build seed-42 + the 1..=30 sample via `history_for`; print `census` (raided/fled/collapsed/alive) + the pooled cascade histogram. Set the `conflict_fires` floor below measured; adjudicate the shape per Step 2. If conflict is inert or the map depopulates, `DONE_WITH_CONCERNS` with the numbers (a calibration decision for Nathan on `RAID_MARGIN`/`WAR_LOSS`/`VIABLE_MIN`).

- [ ] **Step 4: Re-pin the existing seed-42 gates if a floor moved (labelled), commit.**

Run `history_gates`/`history_placement`; re-pin moved floors (`// The Tumult (predation) re-pin: …`) if the phenomenon still fires; STOP and report if it goes inert / depopulates.

```bash
cargo fmt
git add windows/worldgen/tests/history_tumult.rs windows/worldgen/tests/history_gates.rs
git commit -m "test(history): re-measure conflict-fires-on-value + power-law falsification on predation (the-tumult T3)"
```

---

### Task 4: The conflict-bake cost gate (heavy tier)

**Files:**
- Modify: `cli/tests/graph_cost.rs`

- [ ] **Step 1: Update/confirm the heavy cost gate** builds seed-42 to `BuildDepth::Settlements` (now the predation bake) under budget, and (if instrumented) the max cascade depth stays below `CASCADE_DEPTH_CAP` (a real avalanche is not clipped — with the viable-minimum cutoff it should terminate well short). Reuse the existing `graph_cost.rs` heavy test shape (rename to `tumult_predation_bake_stays_within_budget`).

- [ ] **Step 2: Run it (opt-in), commit.**

```bash
cargo test -p hornvale --test graph_cost -- --ignored tumult 2>&1 | tail -15
cargo fmt && git add cli/tests/graph_cost.rs
git commit -m "test(cli): predation-bake wall-time + max-depth cost gate, heavy tier (the-tumult T4)"
```

---

## Close (G6 — `closing-a-campaign`, Nathan-authorized)

Census regen on `lefford` (0063); cascade re-pins; keystone refreeze; artifact drift; DoD docs (chronicle — the crowding→predation reframe + the six-pass ideonomy pivot; retrospective; freshness sweep; Confidence Gradient re-score; registry flip SOC-criticality → elaborated/slice-1 with the measured power-law-or-falsification result + the deferred dominance-hierarchy/cohesion/captives/grievance rows); full gate + artifact drift; fast-forward main.

---

## Self-Review

**Spec coverage:** §4.1 strength+value → Task 1 (`strength`, `capacity`). §4.2 raid rule (covet+dominance, density dropped) → Task 1 (`maybe_raid`, `step_community` rewrite). §4.3 lossy war + viable-death + roll-downhill → Task 1 (WAR_LOSS/plunder) + Task 2 (the downhill dissipation). §4.4 determinism → Tasks 1–2 (total_cmp, no new draw, bounded). §5 falsification metric → Task 3. §8 gates → Task 3 + Task 4. §7 epoch (no new field) → existing Fled records; census at close. §9 non-goals — no dominance-hierarchy/tribute/captives/grievance/cohesion introduced. §11 salvage → edits forward (relocate, histogram, history_for, nearest_occupied reused).

**Placeholder scan:** `RAID_MARGIN`/`WAR_LOSS`/`VIABLE_MIN`/tech weights are named starting values (calibrated by measurement in Task 3, per measure-don't-narrate); the `conflict_fires` floor + the power-law shape are measured-then-set/adjudicated in Task 3. No TBDs.

**Type consistency:** `strength(idx) -> f64` used in `maybe_raid` and `relocate`'s dominance test; `maybe_raid(usize, &EraClimate, f64)` called from `step_community`; `relocate` keeps its `Relocation::{Settled{cascade}, Lost}` return; the consts are `f64`. `census`/`cascade_sizes`/`history_for` reused from T1/T3 unchanged.
