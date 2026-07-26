# The Phantom — Implementation Plan

**Status: COMPLETE (shipped 2026-07-23).** T1 537ee52b, T2 2e02d6e2, T3 7ac5e289.
Fully byte-identical (no scoped drift). The re-derive-at-read-time cost detonated on
the health battery (>710s); resolved (decision-ledger #4, 2 ideonomy passes) by a
session-level append-only `primary_afraid(entity, day)` memo + terrain-shortcut,
reaching parity with believed_water (~371s). Felt phobia = next campaign.

Transient-danger memory (PSY-11's capstone of The Haunt + The Alarm). Spec:
`docs/superpowers/specs/2026-07-22-the-phantom-design.md`. Generalize
`frightened_at(cell) → frightened_at(cell, day, roster)` folding terrain +
**re-derived** alarm over visited (cell, day) pairs; the most-recent-visit
staleness rule goes live; read by The Haunt's existing planner. No epoch;
byte-identical on seed 42 (no primary-afraid emitter).

**Goal:** A creature alarm-frightened at a cell later detours around it even
after the alarm has died and the cell is safe (the phobia), and stops detouring
once it safely revisits it (the fear disproved) — while every current world
stays byte-identical.

## Global Constraints

- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float order via
  `f64::total_cmp` with deterministic tie-breaks; A* costs `u64`.
- No wall-clock time. `f64` transcendentals via `hornvale_kernel::math`.
- **Byte-identity:** `new --seed 42` (genesis) and the seed-42 possession
  galleries are byte-for-byte unchanged (no primary-afraid emitter on seed 42 ⇒
  the re-derived alarm is `0` everywhere ⇒ `frightened_at(cell, day)` collapses to
  The Haunt's terrain-only verdict ⇒ `believed_hazard` returns The Haunt's exact
  set). The gate's artifact-drift check enforces it. **UNLIKE The Haunt, no scoped
  drift is expected** — verify the possession galleries are CLEAN, not merely
  scoped.
- **Determinism:** the transient memory is a fold over committed `agent-at` facts
  + a re-derived (pure-over-frozen) alarm field — no seed draw, no new predicate,
  **no epoch**. Deterministic replay; `BTreeMap`/`BTreeSet` accumulation.
- **Recursion break (load-bearing):** `alarm_field`'s internal `affect_of` already
  passes an EMPTY band (`&[]`). If `believed_hazard` reads that same slice as its
  roster, the replay's `frightened_at` re-derives `alarm_field` over an EMPTY roster
  → empty field → terrain-only → NO re-entry. The break is structural — verify it,
  do not add a flag.
- Every crate `#![warn(missing_docs)]`; type-audit tags on pub-boundary primitives.
- Dependencies unchanged. All work in `windows/vessel` (+ a lab null-control check
  in Task 3). `cargo fmt` final before each commit; `make gate` the final step.

---

### Task 1: `frightened_at(cell, day, roster)` — the alarm as it was, re-derived

Implements spec §1. Self-contained: test by planting a history with a primary-
afraid emitter and re-deriving the alarm at a past day.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`frightened_at`; a private `alarm_at`
  helper)
- Test: `windows/vessel/src/liveness.rs` test module (beside `frightened_at_matches_the_danger_drive`)

**Interfaces:**
- `frightened_at(room: &RoomAddr, npc: &Npc, terrain: &dyn Terrain, day: WorldTime, roster: &[Npc]) -> bool`
  — gains `day` + `roster`. Body:
  ```
  let transient = ALARM_SCALE * alarm_at(room, day, roster, terrain, /* frozen */);
  (threat_field(room, &npc.threat_niche, terrain) + transient)
      * mettle_factor(npc.boldness) >= DANGER_ACT   (clamped as now)
  ```
  Needs the frozen ledger to re-derive the alarm — thread `frozen: &Ledger` in too
  (`frightened_at` is pure over it, like `alarm_field`). Confirm the arg list
  against the call sites in Task 2.
- `alarm_at(room, day, roster, terrain, frozen) -> f64` (private): reads
  `alarm_field(frozen, roster, terrain, day)` and returns its value at `room`
  (`0.0` if absent). **Short-circuit:** if `roster.is_empty()` return `0.0`
  immediately (the recursion base case AND the seed-42 fast path). Do NOT rebuild
  the whole field per call if it can be hoisted — but a correct O(roster) version
  first; optimize in Task 2 against timing.
- `ALARM_SCALE` already exists (The Alarm) — reuse it; the transient memory scales
  the borrowed alarm exactly as the live drive did.

**Steps (TDD):**
- [ ] **Step 1: Write failing unit tests:**
  - `frightened_at_is_terrain_only_with_empty_roster`: with `roster = &[]`,
    `frightened_at(cell, day, &[])` equals the old terrain-only verdict for every
    day (the recursion base case / seed-42 path).
  - `frightened_at_fires_on_re_derived_past_alarm`: plant a primary-afraid emitter
    (a creature on a strong-hazard cell, committed there on `day`) one hop from a
    SAFE cell; assert `frightened_at(safe_cell, day, roster)` is TRUE (the re-derived
    alarm pushed it over `act`) though `frightened_at(safe_cell, day, &[])` (terrain
    only) is FALSE.
  - `frightened_at_is_false_after_the_alarm_passes`: same safe cell, a LATER day on
    which the emitter has moved away (no longer primary-afraid nearby) →
    `frightened_at(safe_cell, later_day, roster)` is FALSE (the alarm is gone — the
    memory's time-awareness).
- [ ] **Step 2: Run — verify fail.**
- [ ] **Step 3: Implement** `frightened_at` (day + roster + frozen) and `alarm_at`
  with the empty-roster short-circuit. Reuse `alarm_field` verbatim (one source of
  truth with the live drive).
- [ ] **Step 4: Run — verify pass;** existing danger/haunt tests still green (they
  pass `&[]`/terrain-only, so byte-identical).
- [ ] **Step 5: fmt + clippy + type-audit, commit**
  (`feat(vessel): frightened_at(cell, day) — the re-derived transient alarm (The Phantom T1)`).

---

### Task 2: `believed_hazard` — most-recent-visit staleness + roster threading

Implements spec §2. The staleness rule goes live; the roster threads to every
caller; byte-identity verified.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`believed_hazard`; the tick + `affect_of`
  view assembly)
- Test: `windows/vessel/src/liveness.rs` test module

**Interfaces:**
- `believed_hazard(ledger, npc, t, terrain, roster: &[Npc]) -> BTreeSet<RoomAddr>`
  gains `roster`. The fold changes from "any visit frightened" to **MOST-RECENT
  visit**: iterate the creature's `agent-at` facts (day ≤ t), and for each cell keep
  the frightened-verdict (`frightened_at(cell, WorldTime{day: fday}, roster, …)`) of
  its LATEST visit; include a cell iff its latest visit was frightened. (Sort by day,
  or keep a `BTreeMap<RoomAddr, (latest_day, frightened)>` and update when a later
  day is seen; deterministic.)
- **Roster threading (the recursion break):**
  - The tick (`DriveMovements::step`): call `believed_hazard(frozen, npc, self.from,
    self.terrain, &self.npcs)` — the FULL roster.
  - `affect_of`: pass its existing `band` slice as the roster
    (`believed_hazard(…, band)`). Because `alarm_field`'s internal `affect_of` passes
    `band = &[]`, the replay gets an empty roster → terrain-only → no re-entry. This
    is the whole recursion fix; **verify `alarm_field` still passes `&[]`** (it does —
    liveness.rs:2538).
  - Any other `believed_hazard` caller / `Perceived` builder: pass the appropriate
    roster (`&[]` where terrain-only is intended / no population in scope).

**Steps (TDD):**
- [ ] **Step 1: Write failing tests:**
  - `believed_hazard_clears_a_disproven_phantom`: a history where the creature is
    alarm-frightened at cell X on day `t1` (emitter nearby) then stands SAFELY at X on
    day `t2 > t1` (emitter gone) → `believed_hazard` does NOT contain X (most-recent
    visit safe). Contrast: without the later safe visit, X IS shunned.
  - `believed_hazard_is_terrain_only_with_empty_roster`: with `roster = &[]`,
    `believed_hazard` equals The Haunt's set exactly (byte-identity guard).
- [ ] **Step 2: Run — verify fail.**
- [ ] **Step 3: Implement** the most-recent-visit fold + roster threading at all call
  sites (grep `believed_hazard(`).
- [ ] **Step 4: Run the new tests + full vessel suite** — green; existing behaviour
  intact.
- [ ] **Step 5: BYTE-IDENTITY CHECK.** Regenerate seed-42 artifacts + possession
  galleries; `git diff --exit-code book/src/gallery/ book/src/reference/
  book/src/laboratory/` must be **CLEAN** (no primary-afraid emitter on seed 42 ⇒
  transient set empty ⇒ identical). Any drift is a BLOCKED — report; unlike The Haunt,
  NO drift is expected here.
- [ ] **Step 6: HOT-PATH re-time.** Time `new --seed 42` and the possession walk
  before/after. The transient fold must short-circuit to near-free on seed 42 (empty
  alarm). Paste timings; if it regresses, hoist the per-day `alarm_field` (cache within
  the fold) and skip days with no primary-afraid emitter.
- [ ] **Step 7: fmt + clippy + type-audit, commit**
  (`feat(vessel): believed_hazard remembers passed danger, most-recent-visit staleness (The Phantom T2)`).

---

### Task 3: The phantom (end-to-end) + health null-control

Implements the spec's e2e + null-control. Close docs handled at G6.

**Steps:**
- [ ] **Step 1: End-to-end "the phantom" test** driving the real `DriveMovements`
  tick. Construct: creature A (the rememberer) and a primary-afraid emitter B beside a
  cell X that lies on A's straight path to water, with a viable detour. Over the ticks,
  B's alarm frightens A at X; then B leaves (no longer primary-afraid). Assert:
  (a) after the alarm has passed and X is safe, A's committed path to water DETOURS
  around X (the phantom — avoidance of a now-safe cell), where a control creature never
  alarmed at X goes straight through; (b) once A safely revisits X (a scenario that
  routes it through), the detour ceases on later trips (the fear cleared). Both reach
  water; A never trapped.
- [ ] **Step 2: Health null-control.** `cargo test -p hornvale-lab --test
  health_calibration` (foreground, ~min) — `chronicity == 0.0` still holds (detouring
  around a phantom still reaches water; routing is a seek). A seed showing chronic
  distress is a REAL finding: STOP, `BLOCKED:` — do not edit the test.
- [ ] **Step 3: Run `cargo test -p hornvale-vessel -p hornvale-lab`** green.
- [ ] **Step 4: `make gate` (full) green, then commit**
  (`test(vessel): the phantom — remembered-passed-danger detour + disproof end-to-end (The Phantom T3)`).
  Then the controller presents the G6 merge package and STOPs; chronicle/retro/PSY-11
  flip run under `closing-a-campaign`.

---

## Self-Review

- **Spec coverage:** §1 (`frightened_at(cell, day)` + re-derived alarm + recursion
  break) → T1; §2 (`believed_hazard` most-recent-visit + roster threading) → T2; §3
  (byte-identity/bounded/cheap) → T2 (construction) + T3 (e2e). All 5 criteria mapped
  (frightened_at re-derive test → T1; staleness-clear test → T2 S1; byte-identity → T2
  S5; e2e phantom+disproof → T3 S1; health null-control → T3 S2).
- **Ordering:** `frightened_at` (T1) before the fold + threading (T2) so each task
  compiles and tests green independently. Byte-identity verified when the roster goes
  live (T2 S5).
- **The recursion break** is the campaign's one true risk; it is structural (empty band
  → empty roster → terrain-only) and verified in T2 (confirm `alarm_field` passes `&[]`;
  the empty-roster tests in T1/T2 assert terrain-only).
- **Cost** guarded by the empty-field short-circuit (free on seed 42), measured T2 S6.
- **No schema/epoch/save-format change** — re-derived fold, no new predicate.
