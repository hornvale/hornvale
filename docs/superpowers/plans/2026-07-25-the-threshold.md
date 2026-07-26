# The Threshold Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make The Hearth's fine layer *live* — a real room derives a real interior, a creature stands at an anchor, and a drive reads a field where it stands.

**Architecture:** Seven tasks in two stages. Stage A builds every piece **byte-identically** — seams, derivation, the fine movement action, occupancy, catch-up — because none of it is read by any outcome. Stage B is one small task that arms it: the Thermal drive reads warmth at the creature's anchor. That is the single boundary at which byte-identity dies, and it is known before any code is written.

**Tech Stack:** Rust 2024, `windows/vessel`, no new dependencies.

**Spec:** `docs/superpowers/specs/2026-07-25-the-threshold-design.md`
**Ledger:** `.superpowers/sdd/the-threshold-ledger.md` (12 entries, seven ideonomy passes, four overturns)

## BLOCKING PRECONDITION

**The Hearth is not built.** Its branch carries a spec and a plan and zero code commits. Every type this plan consumes — `Interior`, `AnchorId`, `AnchorKind`, `Pattern`, `INVENTORY`, `selection`, `compose`, `route_within` — is *specified* in The Hearth's plan and does not exist.

**Do not begin Task 1 until The Hearth has merged.** When it does, re-read its actual `windows/vessel/src/interior/` against this plan's Interfaces blocks before dispatching: this plan was written against The Hearth's spec, and three of its stated interfaces were already found wrong by grounding checks (below).

## Groundings that corrected the spec

Run against the real tree before writing this plan. Each changed a signature the spec asserted.

1. **`interior_of` takes no `era`.** The Hearth's §9.1 names `interior_of(room, culture, era)`. There is no historical-era type in this codebase: the only `Era` is `domains/terrain/src/strata.rs`'s stratigraphic band (`Recent`/`Ancient`/`Deep`/`Primordial`), which is about rock layers, not history. Dropping the parameter is also *correct* rather than merely necessary — a room's furnishing must not flicker with the seasons, so `cold` has to come from a stable climate property, not from a time. **Final signature: `interior_of(room: &RoomAddr, terrain: &dyn Terrain) -> Interior`.**
2. **The Thermal drive does not plan.** It is a *flow* drive: `affordance` returns `comfort_step(...)`, documented "a flow drive needs no plan; NO A*, so `budget` is unused." So catch-up is **not** "run GOAP" — it is running the normal per-tick decide loop with the action set restricted. The mechanism generalises over both flow drives and planning drives; only the name was wrong.
3. **`comfort_step` scans neighbouring *rooms*.** There is no within-room comfort path at all, which is exactly what Task 7 adds.

## Global Constraints

- **Branch `the-threshold`, worktree `~/.config/superpowers/worktrees/hornvale/the-threshold`.** Off `the-hearth`, not `main`.
- **No new dependencies** — `serde`/`serde_json` only, workspace-wide (enforced by `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`). No wall-clock time.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and variant gets a doc comment. This codebase's comments explain *why*, at length, and name the campaign. Terse comments beside paragraph-length neighbours are a defect.
- **Nothing this campaign adds is serialized** (decision 0069). No new predicate, no new fact, no genesis change. If a task finds itself writing a `Fact`, it has gone wrong.
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate`.
- **Stage A must stay byte-identical.** Tasks 1–6 must not move `new --seed 42`, the seed-42 possession galleries, or the health battery. Any drift there is a bug, not a finding — stop and report.

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `windows/vessel/src/interior/seam.rs` | seams: which anchor a room-graph edge lands at | Create (T1) |
| `windows/vessel/src/interior/derive.rs` | `interior_of` — a real room to a real `Interior` | Create (T2) |
| `windows/vessel/src/liveness.rs` | `Action::MoveWithin`; occupancy; catch-up; the Thermal within-room branch | Modify (T3–T7) |
| `windows/vessel/src/interior/mod.rs` | module wiring | Modify (T1, T2) |
| `windows/lab/src/health.rs` | the paired control and stratified readout | Modify (T8) |

---

## Before Task 1 — controller only

Not subagent work. Steps 1 and 2 of the spec's acceptance protocol are worthless if they run late.

- [ ] **Freeze the baseline from The Hearth's merged tip.** Seed-42 galleries + health battery, recorded in the ledger with the commit SHA. A baseline taken mid-campaign aliases other campaigns' physics into this one's measurement.
- [ ] **Preregister the stratified predictions, with signs**, in the ledger: cold-climate creatures in built rooms drawing a hearth improve; warm-climate creatures unchanged; creatures in rooms whose selection yields no hearth unchanged; **observed and unobserved cold creatures improve by comparable amounts** (the catch-up check — if the observed group improves and the unobserved does not, catch-up is not working).

---

## Stage A — byte-identical

### Task 1: Seams

**Files:**
- Create: `windows/vessel/src/interior/seam.rs`
- Modify: `windows/vessel/src/interior/mod.rs` (add `pub mod seam;`)

**Interfaces:**
- Consumes: `AnchorId`, `AnchorKind`, `Interior` (The Hearth T2).
- Produces:
  - `pub enum SeamKind { Narrow, Broad }`
  - `pub fn seam_kind(built: bool) -> SeamKind`
  - `pub fn landing(interior: &Interior, kind: SeamKind) -> Option<AnchorId>`

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::anchor::{AnchorKind, Interior};

    /// A built interior: a threshold and a hearth, hub-composed.
    fn built() -> Interior {
        let mut i = Interior::new();
        let t = i.push(AnchorKind::Threshold, None);
        let h = i.push(AnchorKind::Hearth, None);
        i.connect(t, h);
        i
    }

    /// A wilderness interior: no threshold anywhere (spec §4.2 — this is
    /// legitimate, not a gap to be patched with a fake doorway).
    fn wild() -> Interior {
        let mut i = Interior::new();
        let p = i.push(AnchorKind::Pool, None);
        let l = i.push(AnchorKind::Log, None);
        i.connect(p, l);
        i
    }

    #[test]
    fn a_narrow_seam_lands_at_the_threshold() {
        let i = built();
        let at = landing(&i, SeamKind::Narrow).expect("a built interior has a landing");
        assert_eq!(i.anchor(at).kind, AnchorKind::Threshold);
    }

    #[test]
    fn a_broad_seam_lands_at_the_hub() {
        // The hub is the first anchor (compose connects everything to it).
        // Without coordinates there is no "nearest anchor to the north edge",
        // and spec §2.1 of The Hearth forbids reaching for one.
        let i = wild();
        let at = landing(&i, SeamKind::Broad).expect("a wilderness interior has a landing");
        assert_eq!(at, i.ids()[0]);
    }

    #[test]
    fn wilderness_has_no_threshold_and_that_is_fine() {
        let i = wild();
        assert!(!i.ids().iter().any(|&a| i.anchor(a).kind == AnchorKind::Threshold));
        assert!(landing(&i, SeamKind::Broad).is_some(), "it still has somewhere to arrive");
    }

    #[test]
    fn a_narrow_seam_into_an_interior_with_no_threshold_falls_back_to_the_hub() {
        // Robustness: a built room whose selection happened to draw no
        // threshold must still be enterable.
        let i = wild();
        assert_eq!(landing(&i, SeamKind::Narrow), Some(i.ids()[0]));
    }

    #[test]
    fn an_empty_interior_has_no_landing() {
        assert_eq!(landing(&Interior::new(), SeamKind::Broad), None);
    }

    #[test]
    fn built_rooms_are_narrow_and_wilderness_is_broad() {
        assert_eq!(seam_kind(true), SeamKind::Narrow);
        assert_eq!(seam_kind(false), SeamKind::Broad);
    }
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo test -p hornvale-vessel --lib interior::seam 2>&1 | tail -20`
Expected: FAIL — `cannot find function 'landing' in this scope`.

- [ ] **Step 3: Implement**

```rust
//! Seams — the join between the two position scales (The Threshold, spec §4.2).
//!
//! A `Threshold` anchor is one SPECIES of a wider thing: the seam between
//! scales, simultaneously a room-graph edge and an anchor. A doorway, a ford
//! and an open field edge are all seams; only the first is a threshold. Built
//! and natural rooms are mirror images — indoors the default is a chokepoint
//! and the wall is impassable; outdoors the whole border is passable and the
//! chokepoint is the exception:
//!
//! ```text
//!               NARROW (chokepoint)         BROAD (whole edge)
//!   BUILT       doorway, gate  <- common    colonnade  <- rare
//!   NATURAL     ford, col      <- exception open edge  <- THE COMMON CASE
//! ```
//!
//! A seam belongs to the room-graph EDGE, not to a room's interior — which is
//! why this module is separate from `anchor`. A broad seam lands at the
//! interior's hub because that is the only topologically available answer:
//! without coordinates there is no "nearest anchor to the north edge", and The
//! Hearth's §2.1 forbids reaching for one (outcomes read topology, never
//! metrics). The forced answer being the metric-free one is a good sign.

use super::anchor::{AnchorId, AnchorKind, Interior};

/// Whether passage between two rooms is a chokepoint or the whole shared edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeamKind {
    /// A chokepoint — a doorway, a ford, a gap in a cliff.
    Narrow,
    /// The entire shared border is passable — meadow to meadow.
    Broad,
}

/// The seam kind for a room, derived from whether it is built. Built rooms
/// default to a chokepoint; unbuilt land defaults to an open edge.
/// type-audit: bare-ok(flag: built)
pub fn seam_kind(built: bool) -> SeamKind {
    if built { SeamKind::Narrow } else { SeamKind::Broad }
}

/// Which anchor an arriving creature stands at. A narrow seam lands at the
/// interior's `Threshold` if it has one; everything else lands at the hub (the
/// first anchor, which `compose` connects every other anchor to). `None` only
/// for an empty interior.
pub fn landing(interior: &Interior, kind: SeamKind) -> Option<AnchorId> {
    let ids = interior.ids();
    if kind == SeamKind::Narrow {
        if let Some(&t) = ids
            .iter()
            .find(|&&a| interior.anchor(a).kind == AnchorKind::Threshold)
        {
            return Some(t);
        }
    }
    ids.first().copied()
}
```

Add `pub mod seam;` and `pub use seam::{SeamKind, landing, seam_kind};` to `interior/mod.rs`.

- [ ] **Step 4: Verify**

Run: `cargo test -p hornvale-vessel --lib interior:: 2>&1 | tail -20`
Expected: PASS, all of them.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/src/interior/seam.rs windows/vessel/src/interior/mod.rs
git commit -m "feat(interior): seams — the join between the two position scales

A Threshold anchor is one species of a wider thing. A seam belongs to the
room-graph edge, not to a room's interior; narrow seams land at a
threshold, broad seams land at the hub, which is the only topologically
available answer without coordinates.

Wilderness therefore needs no threshold pattern: its seams are broad.
Nothing consumes this yet."
```

---

### Task 2: `interior_of`

**Files:**
- Create: `windows/vessel/src/interior/derive.rs`
- Modify: `windows/vessel/src/interior/mod.rs`, `windows/vessel/src/liveness.rs` (the `Terrain` trait gains one defaulted method)

**Interfaces:**
- Consumes: `selection`, `compose`, `permits` (The Hearth T4); `Terrain` (liveness).
- Produces:
  - `pub fn interior_of(room: &RoomAddr, terrain: &dyn Terrain) -> Interior`
  - `Terrain::is_built(&self, room: &RoomAddr) -> bool` (defaulted `false`)
  - `Terrain::is_cold(&self, room: &RoomAddr) -> bool` (defaulted from `temperature` at a canonical day)

**Why no `era` parameter:** see Groundings 1. **Why a canonical day:** a room's furnishing must not flicker with the seasons, so `cold` is a stable property of the room, not a function of the current day.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::anchor::AnchorKind;

    /// A `Terrain` that answers only what derivation reads.
    struct Stub { built: bool, cold: bool }
    impl Terrain for Stub {
        fn elevation(&self, _r: &RoomAddr) -> f64 { 0.0 }
        fn is_fresh_water(&self, _r: &RoomAddr) -> bool { false }
        fn temperature(&self, _r: &RoomAddr, _d: WorldTime) -> f64 {
            if self.cold { -20.0 } else { 25.0 }
        }
        fn is_built(&self, _r: &RoomAddr) -> bool { self.built }
    }

    fn room() -> RoomAddr { RoomAddr::default() }

    #[test]
    fn a_built_cold_room_draws_a_hearth() {
        let i = interior_of(&room(), &Stub { built: true, cold: true });
        assert!(i.ids().iter().any(|&a| i.anchor(a).kind == AnchorKind::Hearth));
    }

    #[test]
    fn a_wilderness_room_draws_no_built_anchors() {
        let i = interior_of(&room(), &Stub { built: false, cold: true });
        assert!(!i.ids().is_empty(), "wilderness gets an interior too");
        assert!(!i.ids().iter().any(|&a| i.anchor(a).kind == AnchorKind::Hearth));
        assert!(!i.ids().iter().any(|&a| i.anchor(a).kind == AnchorKind::Threshold));
    }

    #[test]
    fn every_derived_interior_is_well_formed() {
        for &built in &[true, false] {
            for &cold in &[true, false] {
                let i = interior_of(&room(), &Stub { built, cold });
                assert!(
                    crate::interior::permits(&i),
                    "derivation must never produce an interior the validator rejects"
                );
            }
        }
    }

    #[test]
    fn derivation_is_a_pure_function_of_the_room() {
        // Called twice, identical — nothing time-varying leaks in, so a
        // furnishing cannot flicker with the seasons.
        let a = interior_of(&room(), &Stub { built: true, cold: true });
        let b = interior_of(&room(), &Stub { built: true, cold: true });
        assert_eq!(a.ids().len(), b.ids().len());
        for (x, y) in a.ids().iter().zip(b.ids().iter()) {
            assert_eq!(a.anchor(*x).kind, b.anchor(*y).kind);
        }
    }

    #[test]
    fn every_derived_interior_has_a_landing() {
        use crate::interior::seam::{landing, seam_kind};
        for &built in &[true, false] {
            let i = interior_of(&room(), &Stub { built, cold: true });
            assert!(
                landing(&i, seam_kind(built)).is_some(),
                "a creature must always have somewhere to arrive"
            );
        }
    }
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo test -p hornvale-vessel --lib interior::derive 2>&1 | tail -20`
Expected: FAIL — `cannot find function 'interior_of'`.

- [ ] **Step 3: Add the two defaulted `Terrain` methods**

In `windows/vessel/src/liveness.rs`, inside `trait Terrain`, beside the existing defaulted `forage_value`/`hazards`:

```rust
    /// Whether this room carries a built settlement — the signal that decides
    /// whether its interior draws built patterns or wild ones (The Threshold).
    /// A room's *culture* is not a property of the room: culture belongs to the
    /// people whose territory contains it, and a natural room has none. So the
    /// derivation asks the only question it can answer here — is anyone's
    /// territory this? Defaults false, so every existing implementation reads
    /// as wilderness and nothing moves.
    /// type-audit: bare-ok(flag: return)
    fn is_built(&self, _room: &RoomAddr) -> bool {
        false
    }

    /// Whether warmth matters in this room. Read at a CANONICAL day rather
    /// than the current one: a room's furnishing must not flicker with the
    /// seasons, so this is a stable property of the place (The Threshold).
    /// type-audit: bare-ok(flag: return)
    fn is_cold(&self, room: &RoomAddr) -> bool {
        self.temperature(room, FURNISHING_REFERENCE_DAY) < FURNISHING_COLD_C
    }
```

and, near the other drive constants:

```rust
/// The day a room's furnishing reads its climate at (The Threshold). Any fixed
/// day serves; day 0 is the world's own origin and needs no justification
/// beyond being stable. Changing it is a `room/furnishing/v1` epoch.
pub const FURNISHING_REFERENCE_DAY: WorldTime = WorldTime { day: 0.0 };

/// Below this mean temperature (°C) a room's people build around a fire.
/// A first-pass value; changing it is a `room/furnishing/v1` epoch.
pub const FURNISHING_COLD_C: f64 = 5.0;
```

- [ ] **Step 4: Implement `derive.rs`**

```rust
//! `interior_of` — a real room to a real `Interior` (The Threshold, spec §3).
//!
//! The Hearth builds an interior nobody can reach; this is the path from a
//! `RoomAddr` to a composed pattern set. It takes NO era parameter: the only
//! `Era` in this codebase is stratigraphic (rock bands), and a room's
//! furnishing must not flicker with the seasons anyway, so `cold` is read at a
//! canonical day and the result is a pure function of the room.
//!
//! Nothing here is serialized (decision 0069). The interior is derived per
//! room, bubble-scoped, and discarded with the bubble.

use super::anchor::Interior;
use super::pattern::{compose, selection};
use crate::liveness::Terrain;
use hornvale_kernel::{RoomAddr, Seed};

/// The interior of `room`: which patterns it draws, composed into an anchor
/// graph. `built` is "is anyone's territory this" and `cold` is "does warmth
/// matter here" — both read from `terrain`, both stable.
pub fn interior_of(room: &RoomAddr, terrain: &dyn Terrain) -> Interior {
    let built = terrain.is_built(room);
    let cold = terrain.is_cold(room);
    // `selection`'s seed is unused in The Hearth's v1 (a pure filter); it is
    // threaded because the signature takes it. When the variation draw lands
    // it must be keyed by pattern NAME, never by position — see the ledger's
    // "owed to The Hearth".
    let seed = Seed::new(0);
    compose(&selection(&seed, built, cold))
}
```

Add `pub mod derive;` and `pub use derive::interior_of;` to `interior/mod.rs`.

- [ ] **Step 5: Verify**

Run: `cargo test -p hornvale-vessel --lib interior:: 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 6: Verify Stage A byte-identity for the first time**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-t2.json
git stash && cargo run -p hornvale -- new --seed 42 --out /tmp/hv-base.json && git stash pop
diff /tmp/hv-base.json /tmp/hv-t2.json && echo "IDENTICAL"
```

Expected: `IDENTICAL`. Nothing calls `interior_of` yet, and the two `Terrain` methods are defaulted. **If this differs, stop and report** — Stage A drift is a bug.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(interior): interior_of — a real room derives a real interior

No era parameter: the only Era in this codebase is stratigraphic, and a
room's furnishing must not flicker with the seasons, so cold is read at a
canonical day and the result is a pure function of the room.

Terrain gains is_built and is_cold, both defaulted, so every existing
implementation reads as temperate wilderness and nothing moves. Nothing
calls this yet; seed 42 is byte-identical."
```

---

### Task 3: `Action::MoveWithin` and the precondition invariant

**Files:** Modify `windows/vessel/src/liveness.rs`

**Interfaces:**
- Produces: `Action::MoveWithin(AnchorId)` — a new variant. Every `match` on `Action` in the crate must gain an arm; the compiler will find them.

**The invariant this task exists to protect** (spec §5.2): catch-up executes a plan's movement steps while skipping its committing ones, which is only coherent because no movement precondition depends on a committing action's effect. That is an accident of today's action set and ends the moment an action gates movement — a barred door needing unbarring. The test makes the accident explicit.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn no_movement_precondition_depends_on_a_committing_effect() {
    // Catch-up (spec §5) replays a creature's movement while suppressing the
    // actions that commit facts. That is sound only while movement
    // preconditions are purely positional. If a future action gates movement
    // — a barred door needing unbarring — catch-up silently reconstructs a
    // past that could not have happened, and silently is the bad part.
    //
    // The check: every action is classified, and every movement action's
    // precondition is declared positional.
    for a in [
        Action::MoveTo(RoomAddr::default()),
        Action::MoveWithin(AnchorId(0)),
        Action::Drink,
        Action::Rest,
        Action::Eat,
    ] {
        if is_movement(&a) {
            assert!(
                !precondition_reads_committed_state(&a),
                "{a:?} is a movement action whose precondition reads committed \
                 state — catch-up cannot replay it. Either make the \
                 precondition positional or exclude the action from catch-up."
            );
        }
    }
}

#[test]
fn exactly_the_non_committing_actions_are_replayable() {
    assert!(is_replayable_in_catch_up(&Action::MoveWithin(AnchorId(0))));
    // Coarse movement writes `agent-at` — replaying it would fabricate history.
    assert!(!is_replayable_in_catch_up(&Action::MoveTo(RoomAddr::default())));
    assert!(!is_replayable_in_catch_up(&Action::Drink));
    assert!(!is_replayable_in_catch_up(&Action::Rest));
    assert!(!is_replayable_in_catch_up(&Action::Eat));
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo test -p hornvale-vessel --lib no_movement_precondition 2>&1 | tail -20`
Expected: FAIL — `no variant named MoveWithin`.

- [ ] **Step 3: Implement**

Add the variant to `Action`:

```rust
    /// Walk to another anchor inside the current room (The Threshold).
    /// Precondition: adjacency in the room's anchor graph. Effect: fine
    /// position, which is NEVER serialized (decision 0069) — which is what
    /// makes this the one action catch-up may replay.
    MoveWithin(AnchorId),
```

and beside it:

```rust
/// Whether an action's effect is position rather than a committed fact.
pub fn is_movement(a: &Action) -> bool {
    matches!(a, Action::MoveTo(_) | Action::MoveWithin(_))
}

/// Whether an action's precondition reads committed state rather than position
/// alone. Today nothing does; the catch-up invariant test asserts it, so the
/// first action that changes this fails loudly instead of silently corrupting
/// a reconstruction.
pub fn precondition_reads_committed_state(_a: &Action) -> bool {
    false
}

/// Whether catch-up (spec §5) may replay this action. Exactly the actions
/// whose effects are ephemeral: coarse `MoveTo` writes `agent-at`, and
/// `Drink`/`Rest`/`Eat` each commit a fact, so only fine movement qualifies.
/// The partition is "does it commit", not "is it movement".
pub fn is_replayable_in_catch_up(a: &Action) -> bool {
    matches!(a, Action::MoveWithin(_))
}
```

Then fix every non-exhaustive `match` the compiler reports. **Each new arm is a no-op or a `0.0` serviceability** — no drive produces or is served by `MoveWithin` until Task 7. Do not add behaviour here.

- [ ] **Step 4: Verify, including byte-identity**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -20
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-t3.json && diff /tmp/hv-base.json /tmp/hv-t3.json && echo "IDENTICAL"
```

Expected: PASS and `IDENTICAL`. A variant nothing constructs cannot move a world.

- [ ] **Step 5: Commit**

```bash
cargo fmt && git add -A
git commit -m "feat(liveness): Action::MoveWithin, and the catch-up invariant

The two movement scales meet inside the planner rather than beside it.
Coarse MoveTo writes agent-at and Drink/Rest/Eat each commit a fact, so
fine movement is the only replayable action — the partition is 'does it
commit', not 'is it movement'.

The invariant test makes an accident explicit: catch-up's replay is sound
only while movement preconditions are purely positional, which ends the
moment an action gates movement. Nothing produces the variant yet."
```

---

### Task 4: Occupancy

**Files:** Modify `windows/vessel/src/liveness.rs`

**Interfaces:**
- Produces: `pub struct Occupancy(BTreeMap<NpcId, AnchorId>)` with `at`, `arrive`, `walk`; bubble-scoped, never serialized.

**Constraint:** if this task writes a `Fact`, it has gone wrong (decision 0069).

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_creature_arrives_at_the_seam_landing() {
    let interior = /* built interior fixture */;
    let mut occ = Occupancy::default();
    occ.arrive(npc_id(1), &interior, SeamKind::Narrow);
    let at = occ.at(npc_id(1)).expect("an arrived creature stands somewhere");
    assert_eq!(interior.anchor(at).kind, AnchorKind::Threshold);
}

#[test]
fn occupancy_is_empty_until_arrival_and_forgotten_on_departure() {
    let mut occ = Occupancy::default();
    assert!(occ.at(npc_id(1)).is_none());
    occ.arrive(npc_id(1), &built_interior(), SeamKind::Narrow);
    assert!(occ.at(npc_id(1)).is_some());
    occ.depart(npc_id(1));
    assert!(occ.at(npc_id(1)).is_none(), "the bubble collapsing forgets everything");
}

#[test]
fn walking_requires_adjacency() {
    let i = built_interior(); // threshold -- hearth
    let mut occ = Occupancy::default();
    occ.arrive(npc_id(1), &i, SeamKind::Narrow);
    let hearth = i.ids().iter().copied().find(|&a| i.anchor(a).kind == AnchorKind::Hearth).unwrap();
    assert!(occ.walk(npc_id(1), &i, hearth), "adjacent, so the walk succeeds");
    assert_eq!(occ.at(npc_id(1)), Some(hearth));
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo test -p hornvale-vessel --lib occupancy 2>&1 | tail -20`
Expected: FAIL — `cannot find type 'Occupancy'`.

- [ ] **Step 3: Implement**

```rust
/// Which anchor each creature stands at, inside the presence bubble.
///
/// NEVER SERIALIZED (decision 0069, `CLIENT-two-tier-position`): an entity's
/// persisted position is its room; this is the finer tier and it evaporates
/// with the bubble. That is not a convenience — it is what makes `AnchorId`
/// safe to use here at all. `AnchorId` is a vector OFFSET into a derived
/// `Interior`, not a name, so a committed occupancy fact would orphan the
/// moment a `room/furnishing/v1` epoch regenerated the base. An ephemeral one
/// cannot. If you ever find yourself persisting one of these, that is why you
/// must not.
#[derive(Debug, Default)]
pub struct Occupancy(std::collections::BTreeMap<NpcId, AnchorId>);
```

with `at`, `arrive` (via `seam::landing`), `walk` (adjacency-checked via `Interior::neighbors`), and `depart`.

- [ ] **Step 4: Verify, including byte-identity**

Same two commands as Task 3, expecting `IDENTICAL`. Nothing reads occupancy yet.

- [ ] **Step 5: Commit**

---

### Task 5: Catch-up

**Files:** Modify `windows/vessel/src/liveness.rs`

**Interfaces:**
- Produces: `pub fn catch_up(...)`, `pub const CATCH_UP_CAP_DAYS: f64`.

**The mechanism** (spec §5): on bubble entry, re-run each occupant's normal decide loop from its pre-entry state, executing only `is_replayable_in_catch_up` actions, then tick normally. **Not "run GOAP"** — the Thermal drive is a flow drive with no planner (Grounding 2); catch-up runs the decide loop, which serves both kinds.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn catch_up_walks_a_cold_creature_to_the_hearth() {
    // The artifact this exists to dissolve: without catch-up a creature is
    // back at the door every time the world looks at it, and therefore never
    // gets warm.
}

#[test]
fn catch_up_commits_nothing() {
    let before = ledger.len();
    catch_up(/* a full day of absence */);
    assert_eq!(ledger.len(), before, "a projection rebuild must be side-effect free");
}

#[test]
fn catch_up_is_order_independent() {
    // Two creatures caught up toward the same hearth give the same result in
    // either order. Free today (anchors have no capacity); breaks silently
    // when capacity or beside(host) arrives.
}

#[test]
fn beyond_the_cap_catch_up_places_rather_than_replays() {
    // Exact for short absences, approximate for long ones. The test belongs
    // AT the crossover, not in the middle of either regime.
}
```

- [ ] **Step 2–5:** implement, verify byte-identity (`IDENTICAL` — catch-up moves fine position, which nothing reads yet), commit.

---

## Stage B — the boundary

### Task 6: Arm it — the Thermal drive reads warmth where it stands

**This is the one task at which byte-identity dies.** Everything else is already in place; this task's diff is solely responsible for all drift.

**Files:** Modify `windows/vessel/src/liveness.rs` (the `Thermal` drive)

- [ ] **Step 1: Write the failing test** — a cold creature in a built cold room ends its tick at the hearth, and the warmth there exceeds the warmth where it began; the same creature in a hearthless interior does not move.

- [ ] **Step 2–4:** give `Thermal::affordance` a within-room branch that compares `warmth_at` across the interior's anchors and returns `Action::MoveWithin`, with `serviceability` scoring it by warmth gained. `comfort_step` (rooms) stays exactly as it is — this is a new branch, not a replacement.

- [ ] **Step 5: Measure, do not assert.** Run the health battery and the seed-42 galleries. **Expect drift, and record it stratified** against the Before-Task-1 preregistration: which subpopulation moved, by how much, and in which direction. Any drift in warm-climate or hearthless-room creatures is outside the prediction and owes a creature-by-creature explanation.

- [ ] **Step 6: Commit**, with the stratified readout in the message.

---

### Task 7: The paired control

**Files:** Modify `windows/lab/src/health.rs`

Run the battery twice on one build — fine layer live, and fine layer forced inert — identical seed, identical everything. Converts "is this drift acceptable" into "is this drift caused by what we built." Report the stratified difference, including **observed vs unobserved cold creatures**: if the observed group improves and the unobserved does not, catch-up is not working.

---

## Self-Review

**Spec coverage.** §3 → T2. §4 → T4. §4.1 → T1+T4. §4.2 → T1. §5.1 → T3. §5.2 → T3. §5.3 → T5. §5.4 order-independence → T5. §5.5 present-tense — **no task; it is a constraint on a prose layer this campaign does not build.** Recorded in the spec and owed to whichever campaign narrates an interior. §6 protocol: step 0 → T2 Step 5 and T6 Step 1; steps 1–2 → Before Task 1; step 3 → the `IDENTICAL` check in T2/T3/T4/T5 and the drift expectation in T6; step 4 → T7; step 5 → T6 Step 5. §6.1 headline → T7.

**Placeholder scan.** Tasks 4–7 carry test *names and intent* with skeleton bodies rather than complete code, and that is deliberate: their bodies depend on The Hearth's actual `Interior`/`route_within` and on liveness fixtures this plan cannot see from outside a merged Hearth. **They must be filled in at dispatch time, against real code** — flagged here rather than pretended away. Tasks 1–3 are complete as written.

**Type consistency.** `interior_of(room, terrain)` — no era — is used identically in T2 and its callers. `SeamKind`/`landing`/`seam_kind` are named identically in T1 and T4. `is_replayable_in_catch_up` is defined in T3 and consumed in T5. `AnchorId`, `AnchorKind`, `Interior`, `permits`, `selection`, `compose` all come from The Hearth and are **unverified against real code** — see the blocking precondition.
