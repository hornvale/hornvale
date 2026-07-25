# The Shudder — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: use
> `superpowers:subagent-driven-development` to implement this plan task-by-task
> (and `dispatching-hornvale-subagents` for every dispatch). Steps use checkbox
> (`- [ ]`) syntax for tracking.

The visceral felt phobia (PSY-11's reserved felt half of The Phantom). Spec:
`docs/superpowers/specs/2026-07-24-the-shudder-design.md`. Split the hazard
memory by **provenance**, feed the *transient* half back into the Danger drive
as remembered alarm, and give it an affordance so the creature steps off rather
than freezing. No epoch, no new predicate, no new constant.

**Goal:** A creature standing on ground that only *memory* marks as dangerous
feels fear there (`Affect { object: Danger, arousal ≥ DANGER_ACT }`), steps off
it rather than holding in distress, and — having stood there unharmed — loses
the fear; while every current world stays byte-identical.

**Architecture:** `believed_hazard_memo`'s existing terrain-shortcut branch
already separates terrain-frightening cells from alarm-tipped ones. Record the
second branch instead of discarding it (`HazardMemory { shunned, dread }`), pass
`dread` into `Danger` as a second additive alarm term at the creature's own
cell, and make `serviceability`/`flee_step` read the same combined field.

**Tech stack:** Rust 2024, `windows/vessel` (`hornvale-vessel`), `windows/lab`
(`hornvale-lab`). std only.

## Global Constraints

- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float order via
  `f64::total_cmp` with deterministic tie-breaks.
- No wall-clock time. `f64` transcendentals via `hornvale_kernel::math`.
- Every new `pub` item, field and variant gets a one-line doc comment
  (`#![warn(missing_docs)]`); every primitive at a `pub` boundary carries a
  `type-audit:` tag (`bare-ok(ratio: …)` for the dread magnitudes — they are
  dimensionless felt-threat ratios, exactly like `alarm`).
- **No new constant.** The dread rides `ALARM_SCALE` — it *is* an alarm term
  (spec §2, ledger #4). Do not introduce `DREAD_SCALE`.
- **Byte-identity:** `new --seed 42`, the seed-42 possession galleries and every
  committed artifact are byte-for-byte unchanged. The mechanism is structural:
  `hazard_memory_memo`'s **emitter-free fast path returns before any dread is
  recorded**, and seed 42 has no primary-afraid emitter. Unlike The Haunt, **no
  scoped drift is expected** — verify the galleries are CLEAN, not merely scoped.
- **Determinism:** no seed draw, no new predicate, **no epoch**, nothing new
  serialized. `dread` is a per-read `BTreeMap` folded from committed `agent-at`
  facts plus the existing pure replay. Stream consumption order untouched.
- **The recursion break and the contagion block are the same structural fact:**
  `alarm_field_memo` builds emission through `affect_of(band = &[])`; an empty
  band ⇒ an empty roster ⇒ an empty emitter scan ⇒ an **empty dread map**. Never
  add a flag or a guard to achieve either — verify the structure holds.
- **The fear must stay falsifiable** (spec §4): a creature's own dread must
  never feed back into `hazard_memory`. If it did, `believed_hazard_clears_a_
  disproven_phantom` would go red — that test is the invariant's tripwire.
- Run `cargo fmt` as the final step of every task; `cargo clippy --workspace
  --all-targets -- -D warnings` must be clean.

---

### Task 1: `HazardMemory` — the fold returns both provenances

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the `believed_hazard` fold,
  ~lines 985–1150; tests in the same file's `mod tests`)

**Interfaces:**
- Consumes: nothing new.
- Produces:
  - `pub struct HazardMemory { pub shunned: BTreeSet<RoomAddr>, pub dread: BTreeMap<RoomAddr, f64> }`
  - `pub fn hazard_memory(ledger: &Ledger, npc: &Npc, t: WorldTime, terrain: &dyn Terrain, roster: &[Npc]) -> HazardMemory`
  - `pub fn hazard_memory_memo(ledger: &Ledger, npc: &Npc, t: WorldTime, terrain: &dyn Terrain, roster: &[Npc], memo: &mut PrimaryAfraidMemo) -> HazardMemory`
  - `believed_hazard` / `believed_hazard_memo` keep their exact current
    signatures and become one-line wrappers returning `.shunned`.

- [ ] **Step 1: Write the failing tests**

Add to `mod tests` in `windows/vessel/src/liveness.rs`, next to
`believed_hazard_clears_a_disproven_phantom` (~line 4265). They reuse that
test's fixtures verbatim: `agent_at_reg()`, `raddr()`, `haunt_npc()`,
`commit_agent_at()`, `PlantedTerrain::hazard()`.

```rust
    #[test]
    fn hazard_memory_splits_static_from_transient() {
        // PROVENANCE. Two shunned cells for two different reasons:
        //   H — frightening for its own TERRAIN (The Haunt). Shunned, NOT dreaded:
        //       the present cell already frightens the creature, so there is
        //       nothing remembered-but-absent about it.
        //   X — terrain-SAFE, tipped over `act` only by emitter B's re-derived
        //       alarm (The Phantom). Shunned AND dreaded, carrying the remembered
        //       alarm magnitude.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone(); // E: frightens the emitter B (and A, if A stands there)
        let x = ns[1].clone(); // X: terrain-safe, inside B's one-hop halo
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        // Emitter B: beside X on day 0.5 (primary-afraid — E is its neighbour).
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        // A (coward) stood on BOTH the transient cell X and the terrain hazard E.
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &hazard, 0.5);

        let mem = hazard_memory(&ledger, &a, WorldTime { day: 10.0 }, &terrain, &[b]);
        assert!(mem.shunned.contains(&x), "the phantom cell is shunned");
        assert!(mem.shunned.contains(&hazard), "the terrain hazard is shunned");
        assert!(
            mem.dread.contains_key(&x),
            "the phantom cell is DREADED (transient provenance): {:?}",
            mem.dread
        );
        assert!(
            !mem.dread.contains_key(&hazard),
            "a terrain hazard is not a phantom — it is present danger, not memory"
        );
        assert!(
            mem.dread[&x] > 0.0,
            "the dread carries the remembered alarm magnitude"
        );
    }

    #[test]
    fn hazard_memory_dread_is_empty_with_an_empty_roster() {
        // THE STRUCTURAL GUARANTEE, asserted: an empty roster ⇒ an empty emitter
        // scan ⇒ an empty dread map. This one fact is simultaneously The
        // Phantom's recursion base case, seed 42's byte-identity, and the block
        // on superstition contagion (the emission read is bandless).
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let hazard = d_cell.neighbors()[0].clone();
        let x = d_cell.neighbors()[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &hazard, 0.5);

        let mem = hazard_memory(&ledger, &a, WorldTime { day: 10.0 }, &terrain, &[]);
        assert!(mem.dread.is_empty(), "no roster ⇒ no phantom: {:?}", mem.dread);
        assert!(
            mem.shunned.contains(&hazard),
            "the terrain memory is unaffected by the empty roster"
        );
    }

    #[test]
    fn believed_hazard_is_hazard_memory_shunned() {
        // The wrapper is exactly the shunned half — the old entry point keeps
        // its meaning, so The Haunt's planner reads what it always read.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let hazard = d_cell.neighbors()[0].clone();
        let x = d_cell.neighbors()[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);

        let now = WorldTime { day: 10.0 };
        let roster = [b];
        assert_eq!(
            believed_hazard(&ledger, &a, now, &terrain, &roster),
            hazard_memory(&ledger, &a, now, &terrain, &roster).shunned
        );
    }
```

- [ ] **Step 2: Run the tests and watch them fail**

```bash
cargo test -p hornvale-vessel --lib hazard_memory 2>&1 | tail -20
```

Expected: FAIL — `cannot find function 'hazard_memory' in this scope`.

- [ ] **Step 3: Implement the split fold**

In `windows/vessel/src/liveness.rs`, immediately above `believed_hazard`:

```rust
/// The Haunt/Phantom hazard memory, split by PROVENANCE — the one fold, read
/// two ways. `shunned` is what the PLANNER routes around (both provenances);
/// `dread` is the TRANSIENT subset alone, the ground a creature's own reading
/// of the present terrain calls safe and only a remembered alarm makes
/// frightening. The Shudder's load-bearing distinction: a felt term reading
/// `shunned` would drift the canonical world (wild fauna carry a non-empty
/// static set on seed 42), while `dread` is EMPTY there by construction — no
/// primary-afraid emitter, so the emitter-free fast path returns before a
/// single entry is recorded.
/// type-audit: bare-ok(ratio: dread)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct HazardMemory {
    /// Every remembered-frightening cell, both provenances — the planner's
    /// finite route-cost set (exactly the historical `believed_hazard`).
    pub shunned: std::collections::BTreeSet<RoomAddr>,
    /// The TRANSIENT subset, keyed to the remembered ALARM magnitude at that
    /// cell: ground whose terrain alone never crossed `DANGER_ACT`, tipped over
    /// it only by the re-derived alarm of a herd that has long since moved on.
    /// A subset of `shunned`'s keys. Empty ⇒ no phobia (the settled worlds).
    pub dread: std::collections::BTreeMap<RoomAddr, f64>,
}
```

Rename the body of `believed_hazard_memo` to `hazard_memory_memo`, changing its
return type to `HazardMemory` and making these three edits inside it:

```rust
    // (a) the accumulator, replacing `let mut shunned: BTreeSet<..> = ..;`
    let mut mem = HazardMemory::default();

    // (b) the emitter-free fast path — unchanged logic, and the reason `dread`
    //     is empty on every settled world: this returns before any dread is
    //     ever recorded, so byte-identity costs not one instruction.
    if scan.emitters.is_empty() {
        for (cell, day) in latest {
            if frightened_at(&cell, npc, terrain, WorldTime { day }, &[], ledger) {
                mem.shunned.insert(cell);
            }
        }
        return mem;
    }

    // (c) inside the per-cell loop: the terrain shortcut inserts into `shunned`
    //     only (present danger, not a phantom), and the alarm-tipped branch
    //     records the remembered magnitude as well.
        if feels_frightening(terrain_threat, 0.0, npc.boldness) {
            mem.shunned.insert(cell);
            continue;
        }
        // … the existing alarm re-derivation, unchanged …
        let alarm = alarm.clamp(0.0, 1.0);
        if feels_frightening(terrain_threat, alarm, npc.boldness) {
            // TRANSIENT provenance by construction: control only reaches here
            // when terrain ALONE did not frighten (the shortcut above `continue`d
            // otherwise), so a cell shunned here is shunned BECAUSE of a
            // remembered alarm. That is the whole isolation — no second pass.
            mem.shunned.insert(cell.clone());
            mem.dread.insert(cell, alarm);
        }
    }
    mem
```

Then the three public entry points:

```rust
/// [`hazard_memory_memo`] with a throwaway memo — a lone read gains nothing
/// from caching (the hot sim paths thread a shared one).
pub fn hazard_memory(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
) -> HazardMemory {
    let mut memo = PrimaryAfraidMemo::new();
    hazard_memory_memo(ledger, npc, t, terrain, roster, &mut memo)
}

// … and the two historical entry points become wrappers over `.shunned`,
// keeping their existing doc comments (append one line: "The planner half of
// [`hazard_memory`]; the transient half is [`HazardMemory::dread`]."):
pub fn believed_hazard(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
) -> std::collections::BTreeSet<RoomAddr> {
    hazard_memory(ledger, npc, t, terrain, roster).shunned
}

pub fn believed_hazard_memo(
    ledger: &Ledger,
    npc: &Npc,
    t: WorldTime,
    terrain: &dyn Terrain,
    roster: &[Npc],
    memo: &mut PrimaryAfraidMemo,
) -> std::collections::BTreeSet<RoomAddr> {
    hazard_memory_memo(ledger, npc, t, terrain, roster, memo).shunned
}
```

Note the existing loop clamps the alarm inline at the `feels_frightening` call
(`alarm.clamp(0.0, 1.0)`); hoist that clamp to a `let` as shown so the *same*
clamped value is both judged and recorded — the recorded dread must be the
magnitude that produced the verdict, or the memory and the feeling disagree.

- [ ] **Step 4: Run the tests and verify they pass**

```bash
cargo test -p hornvale-vessel --lib hazard_memory 2>&1 | tail -20
cargo test -p hornvale-vessel --lib believed_hazard 2>&1 | tail -20
```

Expected: PASS, including the pre-existing `believed_hazard_*` tests (the
wrappers preserve their behaviour) — most importantly
`believed_hazard_clears_a_disproven_phantom`, this campaign's falsifiability
tripwire.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): HazardMemory — split the hazard memory by provenance (The Shudder T1)"
```

---

### Task 2: `Danger.dread` — the remembered alarm, felt and dischargeable

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the `Danger` struct ~2067–2086, its
  `Drive` impl ~2192–2247, `flee_step` ~2255, and the 18 `Danger {` literals)

**Interfaces:**
- Consumes: `HazardMemory::dread` from Task 1.
- Produces: `Danger.dread: Option<&'a BTreeMap<RoomAddr, f64>>` (a new public
  field — **every one of the 18 `Danger {` literals in the crate must gain
  `dread: None`**, which is the byte-identical default).

- [ ] **Step 1: Write the failing tests**

Add to `mod tests`, beside `danger_urgency_reads_the_cell_threat_and_defaults_safe`
(~line 6684). `view_at` is unchanged — dread rides the drive, not the view.

```rust
    #[test]
    fn danger_urgency_reads_remembered_dread_on_now_safe_ground() {
        // THE SHUDDER: a cell with NO hazard anywhere near it — present threat 0 —
        // frightens a creature that remembers a herd's alarm there. Fear of
        // nothing present. `None` dread on the same cell reads calm, so the term
        // is additive-latent: byte-identical wherever the map is empty.
        let safe = raddr(-1.0); // neither it nor its neighbours carry any hazard
        let t = PlantedTerrain::hazard(std::iter::empty(), std::iter::empty());
        let mut dread = std::collections::BTreeMap::new();
        dread.insert(safe.clone(), 0.8);

        let calm = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: None,
        };
        assert_eq!(
            calm.urgency(&view_at(safe.clone())),
            0.0,
            "without the memory the ground is unremarkable"
        );

        let haunted = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: Some(&dread),
        };
        let felt = haunted.urgency(&view_at(safe));
        assert_eq!(felt, 0.8, "the remembered alarm is felt as the alarm it was");
        assert!(felt >= DANGER_ACT, "and it crosses act — the drive engages");
    }

    #[test]
    fn danger_discharges_dread_by_stepping_off_the_haunted_cell() {
        // THE AFFORDANCE (spec §2, ledger #1). A phantom cell is now-SAFE ground,
        // so terrain offers no gradient to flee down: without a dread-aware
        // serviceability the creature would Hold and read `Lost` — a distress
        // tick for a feature that is a feeling, not a pathology. With it, every
        // neighbour is an improvement and the creature steps off.
        let here = raddr(-1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), std::iter::empty());
        let mut dread = std::collections::BTreeMap::new();
        dread.insert(here.clone(), 0.8);
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: Some(&dread),
        };
        let view = view_at(here.clone());
        let step = danger
            .affordance(&view, PLAN_BUDGET)
            .expect("dread on flat ground still offers a step off it");
        let Action::MoveTo(to) = step else {
            panic!("fleeing is a MoveTo");
        };
        assert!(here.neighbors().contains(&to), "it steps to a neighbour");
        assert!(
            danger.serviceability(&Action::MoveTo(to), &view, PLAN_BUDGET) > 0.0,
            "stepping off the dreaded cell positively serves the drive"
        );
    }

    #[test]
    fn danger_without_dread_is_unchanged_on_flat_ground() {
        // The byte-identity half of the same seam: `dread: None` on hazard-free
        // ground still offers NO flee step (nowhere is strictly safer) — today's
        // behaviour exactly.
        let here = raddr(-1.0);
        let t = PlantedTerrain::hazard(std::iter::empty(), std::iter::empty());
        let danger = Danger {
            terrain: &t,
            threat_niche: mortal_threat_niche(),
            boldness: BOLDNESS_STEADY,
            alarm: None,
            dread: None,
        };
        assert_eq!(danger.affordance(&view_at(here), PLAN_BUDGET), None);
    }
```

- [ ] **Step 2: Run the tests and watch them fail**

```bash
cargo test -p hornvale-vessel --lib danger_ 2>&1 | tail -20
```

Expected: FAIL — `struct 'Danger' has no field named 'dread'`.

- [ ] **Step 3: Implement the drive term**

Add the field to `Danger` (doc comment required):

```rust
    /// The remembered DREAD map (The Shudder): the TRANSIENT subset of this
    /// creature's hazard memory — cells whose present terrain is safe but where
    /// a herd's alarm once frightened it — keyed to the remembered alarm
    /// magnitude. Read at the creature's OWN cell and folded into the same
    /// additive slot as [`Danger::alarm`], because it IS an alarm term: the
    /// alarm as it was, not as it is. `None` ⇒ no phobia — byte-identical.
    /// Provenance is the only difference from `alarm`: that one is SENSED
    /// (present, external, a per-tick field), this one is BELIEVED (past,
    /// internal, a fold over committed history).
    pub dread: Option<&'a std::collections::BTreeMap<RoomAddr, f64>>,
```

Add the lookup helper beside `Danger::threat_at`:

```rust
impl<'a> Danger<'a> {
    /// The remembered dread at `room` (`0.0` when unremembered or `None`).
    /// type-audit: bare-ok(ratio: return)
    fn dread_at(&self, room: &RoomAddr) -> f64 {
        self.dread
            .and_then(|m| m.get(room))
            .copied()
            .unwrap_or(0.0)
    }

    /// The creature's total felt threat at `room` — present terrain PLUS
    /// remembered dread, the field `serviceability` and the flee gradient read.
    /// Unlike the borrowed alarm (whose halo always lies within one hop of
    /// terrain that genuinely frightens its emitter, so a terrain gradient
    /// always exists), dread sits on now-SAFE ground: without it in the
    /// gradient a dreading creature has nowhere to go and reads `Lost`.
    /// type-audit: bare-ok(ratio: return)
    fn felt_threat_at(&self, room: &RoomAddr) -> f64 {
        self.threat_at(room) + ALARM_SCALE * self.dread_at(room)
    }
}
```

In `urgency`, sum the two alarm sources before the boldness scaling:

```rust
        // THE SHUDDER: the REMEMBERED alarm at this cell joins the BORROWED one
        // in the same additive slot — the dread is an alarm term, so it needs no
        // scale of its own. Feeding back the very magnitude that recorded the
        // memory reproduces the verdict that created it: the memory and the
        // feeling agree.
        let remembered = self.dread_at(&view.position);
        let felt = base + ALARM_SCALE * (borrowed + remembered);
        (felt * mettle_factor(self.boldness)).clamp(0.0, 1.0)
```

In `serviceability` and `affordance`, read the combined field:

```rust
            Action::MoveTo(n) => self.felt_threat_at(&view.position) - self.felt_threat_at(n),
```

```rust
        flee_step(&view.position, self.terrain, &self.threat_niche, self.dread)
            .map(Action::MoveTo)
```

And give `flee_step` the dread argument (its one call site is the line above):

```rust
fn flee_step(
    from: &RoomAddr,
    terrain: &dyn Terrain,
    niche: &ThreatNiche,
    dread: Option<&std::collections::BTreeMap<RoomAddr, f64>>,
) -> Option<RoomAddr> {
    let threat = |room: &RoomAddr| {
        threat_value(niche, &terrain.hazards(room))
            + ALARM_SCALE * dread.and_then(|m| m.get(room)).copied().unwrap_or(0.0)
    };
```

The rest of `flee_step` is unchanged (the same three-neighbour scan,
`total_cmp`-then-ascending-`RoomAddr` tie-break, and strictly-safer test). Update
its doc comment to say the gradient is over *felt* threat — terrain plus
remembered dread — so a creature can flee ground that is only frightening in
memory.

Then add `dread: None` to all 18 `Danger {` literals:

```bash
grep -n "Danger {" windows/vessel/src/liveness.rs
```

- [ ] **Step 4: Run the tests and verify they pass**

```bash
cargo test -p hornvale-vessel --lib danger_ 2>&1 | tail -20
cargo test -p hornvale-vessel 2>&1 | tail -20
```

Expected: PASS, with every pre-existing Danger/Alarm/Haunt/Phantom test still
green (`dread: None` is the identity).

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): Danger.dread — the remembered alarm, felt and dischargeable (The Shudder T2)"
```

---

### Task 3: Wire it — the mover feels it, the narration shows it, the field never spreads it

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`DriveMovements::step` ~3100,
  `affect_of_memo` ~2747–2786, `alarm_field_memo`'s gate comment ~2889–2899)

**Interfaces:**
- Consumes: `hazard_memory_memo` (T1), `Danger.dread` (T2).
- Produces: no new signatures — `affect_of` and `DriveMovements::step` keep
  theirs.

- [ ] **Step 1: Write the failing tests**

```rust
    #[test]
    fn affect_of_feels_the_phantom_on_now_safe_ground() {
        // THE FELT HALF, through the public read the narration and the health
        // metric both use. A creature standing where a herd's alarm once caught
        // it reads Danger — on ground whose PRESENT terrain threat is below act.
        // A never-alarmed control on the same cell reads no danger at all.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone(); // E: frightens the emitter B
        let x = ns[1].clone(); // X: terrain-safe, in B's halo
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        // B: primary-afraid beside X on day 0.5, then far away by day 9.5.
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        commit_agent_at(&mut ledger, &reg, b_e, &raddr(-1.0), 9.5);
        // A (coward): stood at X while B panicked beside it, and is there now.
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &x, 9.5);
        // C (coward): never stood at X before — it is there now for the first time.
        let c_e = ledger.mint_entity();
        let mut c = haunt_npc(c_e, x.clone());
        c.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, c_e, &x, 9.5);

        let now = WorldTime { day: 9.6 };
        let band = [a.clone(), b.clone(), c.clone()];
        let felt = affect_of(&ledger, &a, &band, now, &terrain);
        assert_eq!(
            felt.object,
            Some(DriveKind::Danger),
            "the rememberer is afraid on now-safe ground: {felt:?}"
        );
        assert!(felt.arousal >= DANGER_ACT, "and the fear is felt: {felt:?}");
        let control = affect_of(&ledger, &c, &band, now, &terrain);
        assert_ne!(
            control.object,
            Some(DriveKind::Danger),
            "a creature with no memory of this ground feels nothing here: {control:?}"
        );
    }

    #[test]
    fn a_dread_afraid_creature_raises_no_alarm() {
        // NO SUPERSTITION CONTAGION (spec §3, ledger #6) — and not by a guard:
        // the emission read is BANDLESS, so its hazard memory has no emitters and
        // its dread map is empty. A creature shuddering at a phantom is quiet.
        // Same fixture as above; B is long gone, so the ONLY possible emitter is
        // A's remembered dread — and the field must be empty at X.
        let reg = agent_at_reg();
        let mut ledger = Ledger::default();
        let d_cell = raddr(1.0);
        let ns = d_cell.neighbors();
        let hazard = ns[0].clone();
        let x = ns[1].clone();
        let terrain = PlantedTerrain::hazard(std::iter::empty(), [(hazard.clone(), 0.8)]);
        let b_e = ledger.mint_entity();
        let b = haunt_npc(b_e, d_cell.clone());
        commit_agent_at(&mut ledger, &reg, b_e, &d_cell, 0.5);
        commit_agent_at(&mut ledger, &reg, b_e, &raddr(-1.0), 9.5);
        let a_e = ledger.mint_entity();
        let mut a = haunt_npc(a_e, x.clone());
        a.boldness = 0.0;
        commit_agent_at(&mut ledger, &reg, a_e, &x, 0.5);
        commit_agent_at(&mut ledger, &reg, a_e, &x, 9.5);

        let field = alarm_field(&ledger, &[a, b], &terrain, WorldTime { day: 9.6 });
        assert!(
            !field.contains_key(&x),
            "remembered dread is felt, never broadcast: {field:?}"
        );
    }
```

- [ ] **Step 2: Run the tests and watch them fail**

```bash
cargo test -p hornvale-vessel --lib affect_of_feels_the_phantom 2>&1 | tail -20
```

Expected: FAIL — the rememberer's affect object is not `Danger` (the drive is
constructed with `dread: None`).

- [ ] **Step 3: Wire the two read sites**

In `affect_of_memo` (~line 2747), take the whole memory and hand the drive its
transient half:

```rust
    let memory = hazard_memory_memo(frozen, npc, day, terrain, band, memo);
    let view = Perceived {
        position: pos,
        drive,
        fatigue,
        believed_water: believed,
        believed_hazard: memory.shunned.clone(),
        explore_step,
    };
```

```rust
    let danger = Danger {
        terrain,
        threat_niche: npc.threat_niche,
        boldness: npc.boldness,
        // The instantaneous affect read is alarm-free (terrain-sourced only) —
        // this is the read `alarm_field` builds over, so it MUST NOT see borrowed
        // alarm (else secondary transmission, a self-sustaining stampede).
        alarm: None,
        // THE SHUDDER: it DOES see remembered dread, because this is the read the
        // narration and the health metric observe — a fear that never reaches
        // `Affect` is not a feeling, only a second behavioural term. Safe against
        // the same stampede: the alarm-field's emission read passes `band = &[]`,
        // whose bandless memory has no emitters and therefore an EMPTY dread map,
        // so a dread-afraid creature can never emit. One structural fact — the
        // bandless replay — gives termination, byte-identity, and no contagion.
        dread: Some(&memory.dread),
    };
```

In `DriveMovements::step` (~line 3100), the same swap: bind `let memory =
hazard_memory_memo(frozen, npc, self.from, self.terrain, &self.npcs, &mut
afraid_memo);`, use `memory.shunned.clone()` where `believed_hazard.clone()` is
used for the view, and give the per-step `Danger` literal `dread:
Some(&memory.dread)` beside its existing `alarm: Some(&alarm)`.

Finally, sharpen `alarm_field_memo`'s gate comment (~line 2894) so a later edit
cannot open contagion by accident — replace the sentence "This is a NECESSARY
condition, not the decision" with:

```rust
        // This is a NECESSARY condition, not the decision — and it stays exact
        // under The Shudder: the read it guards is the BANDLESS `affect_of`,
        // whose hazard memory has no emitters and therefore no dread, so its
        // Danger urgency really is `threat_field × mettle_factor`. Remembered
        // dread is felt but never emitted; contagious superstition is reserved.
        // Widening this gate to admit dread-only creatures would open it.
```

- [ ] **Step 4: Run the tests and verify they pass**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -20
```

Expected: PASS — both new tests, plus every existing vessel test. If
`believed_hazard_clears_a_disproven_phantom` fails, dread has leaked into the
memory: the falsifiability invariant is broken, stop and re-read spec §4.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): the mover and the affect read feel the phantom (The Shudder T3)"
```

---

### Task 4: The shudder end-to-end, and the evidence

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`mod tests`, beside
  `the_phantom_detours_around_a_passed_alarm_then_relearns_the_ground_safe`
  ~line 6059)

**Interfaces:**
- Consumes: everything from T1–T3. Produces: nothing new.

- [ ] **Step 1: Write the failing end-to-end test**

Reuse The Phantom's e2e geometry verbatim (start → c1 → c2 → c3 → water, X the
interior cell, D its off-path neighbour, E the hazard beside D) — copy that
setup block from
`the_phantom_detours_around_a_passed_alarm_then_relearns_the_ground_safe`
rather than re-deriving it, then assert the felt arc:

```rust
    #[test]
    fn the_shudder_is_felt_on_the_phantom_then_discharged_then_disproven() {
        // THE SHUDDER, end-to-end: the full arc The Phantom could only plan.
        //   (1) FELT     — standing on X, where herd-mate B once panicked beside
        //                  it, the creature reads Danger though X's own terrain
        //                  is safe and B is long gone. Fear of nothing present.
        //   (2) DISCHARGED — it is not stuck: its affect is not a distress label,
        //                  and its intent is a step OFF X, not a Hold.
        //   (3) DISPROVEN — having stood there and come to no harm, the cell
        //                  leaves both the shunned set and the dread map: the
        //                  fear the avoidance had been protecting is undone by
        //                  the one experience that can undo it.
        // (Setup: copy the geometry + registry block from
        // `the_phantom_detours_around_a_passed_alarm_then_relearns_the_ground_safe`
        // above — start/c1/c2/c3/water, X = path_cells[1], D = X's off-path
        // neighbour, hazard_e = D's off-path neighbour, `terrain` planted with
        // water at `water` and hazard 0.8 at `hazard_e`.)

        // A (coward) stood at X on the early day when B panicked at D, and is
        // standing at X now. A never-alarmed control C is at X now only.
        // …commit_agent_at for B at d_cell (early) then `far` (late);
        //   for A at x (early) and x (now); for C at x (now)…

        let band = [a.clone(), b.clone(), c.clone()];

        // (1) FELT.
        let felt = affect_of(&ledger, &a, &band, now, &terrain);
        assert_eq!(felt.object, Some(DriveKind::Danger), "{felt:?}");
        assert!(felt.arousal >= DANGER_ACT, "{felt:?}");
        assert!(
            threat_field(&x, &a.threat_niche, &terrain) * mettle_factor(a.boldness) < DANGER_ACT,
            "X's PRESENT terrain is not frightening — the fear is memory, not sense"
        );

        // (2) DISCHARGED — a feeling, not a pathology.
        assert!(
            !matches!(
                felt.label,
                AffectLabel::Lost | AffectLabel::Frustrated | AffectLabel::Helpless
            ),
            "dread with an outlet is wariness, not distress: {felt:?}"
        );
        let memory = hazard_memory(&ledger, &a, now, &terrain, &band);
        let danger = Danger {
            terrain: &terrain,
            threat_niche: a.threat_niche,
            boldness: a.boldness,
            alarm: None,
            dread: Some(&memory.dread),
        };
        assert!(
            matches!(danger.affordance(&view_at(x.clone()), PLAN_BUDGET), Some(Action::MoveTo(_))),
            "it has somewhere to go"
        );

        // (3) DISPROVEN — run the real tick, then re-read the memory. A stood at
        // X on `now` with no emitter anywhere near, so its most-recent verdict
        // there is SAFE and the phantom is gone from both halves.
        let sys = DriveMovements {
            npcs: band.to_vec(),
            from: now,
            to: WorldTime { day: now.day + 1.0 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let next = tick(&ledger, &[&sys], &["drive-movements"], &reg).expect("tick");
        let after = hazard_memory(&next, &a, WorldTime { day: now.day + 1.0 }, &terrain, &band);
        assert!(
            !after.dread.contains_key(&x),
            "standing there unharmed disproves the dread: {:?}",
            after.dread
        );
        assert!(
            !after.shunned.contains(&x),
            "and clears the shun with it — the phobia is falsifiable"
        );
    }
```

Fill the elided setup and `now` binding from the neighbouring Phantom test; do
not invent new geometry — the point is that this is the *same* world The Phantom
could only route around.

- [ ] **Step 2: Run it and watch it fail, then pass**

```bash
cargo test -p hornvale-vessel --lib the_shudder 2>&1 | tail -30
```

Expected: FAIL before T1–T3 land (it will not compile against the old API);
PASS after. If assertion (3) fails, check whether A's tick actually kept it at
or returned it to X — if the walk carries it away before a fact is committed at
X on `now`, commit A's `agent-at` at X for `now` explicitly (as the setup above
does) rather than weakening the assertion.

- [ ] **Step 3: The byte-identity evidence**

```bash
bash scripts/regenerate-artifacts.sh 2>&1 | tail -5
git diff --stat
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ && echo "ARTIFACTS CLEAN"
```

Expected: `ARTIFACTS CLEAN` — **no drift at all**, not scoped drift. Any
possession-gallery movement means the transient set is non-empty on seed 42,
which contradicts The Phantom's premise: stop, report the diff, do not
regenerate over it.

- [ ] **Step 4: The health battery — the real exposure surface, timed**

```bash
/usr/bin/time -f "health battery: %e s" cargo test -p hornvale-lab --test health_calibration 2>&1 | tail -30
```

Expected: PASS, and the wall-clock in the same band as The Phantom's post-fix
~371 s (a large jump is a regression — the campaign's timing probe is this
battery, never the possession walk). Record the number in the commit message.
`the_null_control_reads_no_chronic_distress` and
`the_null_control_holds_across_a_seed_sweep` must stay green: unlike seed 42's
peoples, the health sim's wild fauna *do* contain primary-afraid emitters, so
this is where a non-empty dread map can genuinely move the metric. If
chronicity moves off `0.0`, investigate before merging — do not loosen a bound.

- [ ] **Step 5: The full gate, then commit**

```bash
make gate 2>&1 | tail -20
```

```bash
cargo fmt
git add -A
git commit -m "test(vessel,lab): the shudder end-to-end — felt, discharged, disproven (The Shudder T4)

Byte-identity verified (artifacts clean, no scoped drift); health battery green at <N> s."
```

---

## Close (G6)

Not a task — the `closing-a-campaign` skill owns it: chronicle entry
(`book/src/chronicle/the-shudder.md`), the PSY-11 registry row (flip the
visceral-felt-phobia reservation to SHIPPED and add the four new reserved
clauses recorded in `.superpowers/sdd/decision-ledger.md`'s capture manifest),
the book freshness sweep, the campaign retrospective, and `make gate-full`.

## Self-Review

**Spec coverage.** §1 `HazardMemory` → T1. §2 the drive term, the shared
`ALARM_SCALE`, and the serviceability/flee asymmetry → T2. §3 the three wiring
sites (mover, affect read, bandless emission) and the sharpened gate comment →
T3. §4 falsifiability → guarded in T1's step 4 and T3's step 4 by the existing
`believed_hazard_clears_a_disproven_phantom`, and asserted directly by T4's
assertion (3). Determinism → the Global Constraints plus T4 step 3. Cost → T4
step 4. Every success criterion has a home: transient-subset split (T1),
empty-roster dread (T1), felt (T3/T4), discharged (T2/T4), extinguished (T4),
no alarm emitted (T3), byte-identity (T4.3), health null-control and timing
(T4.4).

**Placeholders.** T4 step 1 deliberately elides a setup block with an explicit
instruction to copy it verbatim from a named neighbouring test — the alternative
is reproducing ~40 lines of geometry derivation that must match exactly. Every
other step carries its real code.

**Type consistency.** `hazard_memory` / `hazard_memory_memo` / `HazardMemory {
shunned, dread }` / `Danger.dread: Option<&BTreeMap<RoomAddr, f64>>` /
`dread_at` / `felt_threat_at` / `flee_step(from, terrain, niche, dread)` are
used under those exact names in T2–T4. `believed_hazard` and
`believed_hazard_memo` keep their existing signatures throughout.
