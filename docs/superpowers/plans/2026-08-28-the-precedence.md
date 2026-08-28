# The Precedence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the vessel scheduler's emitted fact stream chronological, so the ledger's ordering stops being an accident of the queue's pop order — then land the retype that defect was blocking.

**Architecture:** The queue pops creatures in action-START order; every fact is stamped at action-END. Those orderings differ by the population's cost spread. Since pop order provably affects nothing observable except emission order, the fix is a stable sort by day at the point of production, guarded by three tests. The retype of `WalkState`'s instants follows once that unblocks it.

**Tech Stack:** Rust 2024, `cargo nextest`, no new dependencies (workspace allowlist is `serde`/`serde_json`/`libm` only).

**Spec:** `docs/superpowers/specs/2026-08-27-the-precedence-design.md`

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only. No new crates.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`).
- No wall-clock time. Instants are `WorldTime` (exact `i64` ticks); durations are `TickSpan`.
- Every crate sets `#![warn(missing_docs)]` — every public item, field and variant gets a one-line doc comment.
- Run `cargo fmt` as the final step before every commit. Fmt-gate skips are the most common review finding.
- `make gate-commit` runs the **sub-floor tier only**. It is not a green crate. Run `cargo test -p <crate>` in full before believing anything.
- **Never run two `cargo test` invocations in one shell command.** Capture once to a log, then grep the log. A pre-commit guard enforces this.
- `windows/vessel/tests/fixtures/` are byte goldens only `REBASELINE=1` writes, deliberately. An unexpected move there is a finding, not a rebaseline.
- Work happens in the worktree `.claude/worktrees/the-precedence` on branch `campaign/the-precedence`.

---

## File Structure

| file | responsibility in this plan |
| --- | --- |
| `windows/vessel/src/liveness.rs` | the scheduler (`step_with_occupancy`), the walk (`advance_one`), `WalkState`, the four fact helpers, and the `mod tests` that guards them. Every change in stages 1 and 3 lands here. |
| `docs/decisions/` | one new decision record (Task 1.7) — the ledger's chronology is not the pop order. |
| `book/src/frontier/idea-registry.md` | correct the two `TOOL-` rows that misattribute the defect (Task 1.7). |

No new files. `liveness.rs` is large (15k lines) but is the established home for all of this; splitting it is out of scope and would swamp the diff.

---

# Stage 1 — the ordering fix

**Goal:** emissions are chronological; the invariant asserts it with no tolerance.
**Success Criteria:** both mass orders of `interleaving_fixture` green; zero inverted ticks on seed 42; no tolerance term left in the file.
**Status:** Not Started

---

### Task 1.1: The window precondition — written FIRST, before the sort

The sort's correctness rests on a tick being a closed window. **This is unverified.** If it goes red, the design in spec §3 is wrong and the campaign re-specs — do not relax this test to make it pass.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (in `mod tests`, immediately before `fn a_faster_creature_acts_more_often_between_a_slower_ones_actions`)

**Interfaces:**
- Consumes: `interleaving_fixture(&[f64]) -> (Ledger, PlantedTerrain, Vec<Body>)`; `DriveMovements { npcs, from, to, params, day_ticks, terrain }`; `SUSTENANCE`.
- Produces: nothing later tasks depend on.

- [ ] **Step 1: Write the test**

```rust
    #[test]
    fn every_emitted_fact_is_dated_inside_the_tick_that_emitted_it() {
        // THE SORT'S PRECONDITION (The Precedence, spec §5 guard 2). Making
        // the emitted stream chronological by sorting it is correct only
        // because a tick is a CLOSED WINDOW: no fact may be dated outside
        // `[from, to]`, so reordering within the window can never need to
        // reach back past a fact an earlier tick already emitted. That is the
        // watermark the whole design rests on, and it was previously assumed
        // rather than asserted.
        //
        // DIRECTION THIS ENFORCES: `emitted` is a subset of the window. It is
        // blind to a fact the walk DECLINED to emit — a step past `to` returns
        // early and commits nothing, which this cannot see and does not claim
        // to.
        //
        // Three masses, so the population genuinely falls out of step and the
        // jump arms (`hold_step`'s closed form, `next_awake_day`'s sleep
        // jump) are actually reached rather than merely present.
        let (ledger, terrain, npcs) = interleaving_fixture(&[4.375, 70.0, 1_120.0]);
        let from = WorldTime::from_std_days(1.0).expect("a day value is finite");
        let to = WorldTime::from_std_days(20.0).expect("a day value is finite");
        let sys = DriveMovements {
            npcs,
            from,
            to,
            params: SUSTENANCE,
            day_ticks: None,
            terrain: &terrain,
        };
        let facts = sys.step(&ledger);
        assert!(
            !facts.is_empty(),
            "the fixture emitted nothing; it cannot pin a window"
        );
        for f in &facts {
            let d = f.day.expect("every emitted fact is dated");
            assert!(
                d >= from && d <= to,
                "`{}` at {d:?} fell outside the tick's window [{from:?}, {to:?}] — \
                 the sort in `step_with_occupancy` is only sound inside a closed \
                 window, so this is a design refutation, not a test to relax",
                f.predicate
            );
        }
    }
```

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-vessel --lib every_emitted_fact_is_dated_inside_the_tick_that_emitted_it`

**This is a branch, not a prediction — the plan author did not run it:**
- **PASS** → the precondition holds; continue to Task 1.2.
- **FAIL on the window bound** → **STOP.** Record the failing predicate, its day, and the window. The sort's justification is refuted; report back before writing any more code.
- **FAIL on `expect("every emitted fact is dated")`** → **STOP.** An undated fact means the sort key `Option<WorldTime>` has a `None` to place, which the design did not account for.
- **FAIL on `!facts.is_empty()`** → the fixture no longer emits; fix the fixture before proceeding, the test is vacuous otherwise.

- [ ] **Step 3: Commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs
git commit -m "test(vessel): assert a tick is a closed window, before relying on it"
```

---

### Task 1.2: Make the monotonicity invariant fail, honestly

The existing test passes only because `interleaving_fixture` hands the *fast* creature the lower entity id. Run both orders.

**Files:**
- Modify: `windows/vessel/src/liveness.rs`, `fn a_faster_creature_acts_more_often_between_a_slower_ones_actions`

**Interfaces:**
- Consumes: as Task 1.1.
- Produces: nothing.

- [ ] **Step 1: Replace the test body**

Replace the whole of `a_faster_creature_acts_more_often_between_a_slower_ones_actions` with this. The comment block is deliberately rewritten: the old one attributed the disorder to sub-tick `f64` noise, which is false.

```rust
    #[test]
    fn a_faster_creature_acts_more_often_between_a_slower_ones_actions() {
        // INTERLEAVING, OBSERVABLY. Two creatures sixteen-fold apart in mass are
        // exactly two-fold apart in tempo (`16 ^ 0.25 == 2`), so the lighter one
        // takes two actions in the time the heavier takes one. Under a shared
        // clock its facts must appear BETWEEN the heavier one's; under the old
        // sequential loop they appeared entirely before them.
        //
        // BOTH MASS ORDERS (The Precedence). `interleaving_fixture` mints entity
        // ids in the order the masses are listed, so the list order IS the
        // queue's tie-break order. This test used to run only `[4.375, 70.0]`,
        // which hands the FAST creature the lower id — so at every tie the
        // cheaper action was emitted first and the inversion below could not
        // show. Swapping the order was worth 9,925 ticks against a one-tick
        // tolerance. The fixture must not depend on which creature is lighter.
        for masses in [[4.375_f64, 70.0], [70.0, 4.375]] {
            let (ledger, terrain, npcs) = interleaving_fixture(&masses);
            let sys = DriveMovements {
                npcs,
                from: WorldTime::from_std_days(1.0).expect("a day value is finite"),
                to: WorldTime::from_std_days(20.0).expect("a day value is finite"),
                params: SUSTENANCE,
                day_ticks: None,
                terrain: &terrain,
            };
            let facts = sys.step(&ledger);
            let seq: Vec<EntityId> = facts
                .iter()
                .filter(|f| f.predicate == AGENT_AT)
                .map(|f| f.subject)
                .collect();
            // The assertion counts SWITCHES of subject along the emitted
            // sequence. The sequential loop scores exactly one (all of A, then
            // all of B) for any pair, however far apart in tempo; a scheduler
            // scores many. `>= 2` is the smallest threshold the old loop
            // cannot reach.
            let switches = seq.windows(2).filter(|w| w[0] != w[1]).count();
            assert!(
                switches >= 2,
                "the two creatures never interleave (masses={masses:?}, \
                 switches={switches}, seq={seq:?}) — the queue is not \
                 scheduling, it is still walking each in turn"
            );
            // And the emitted days run forward on ONE timeline, EXACTLY.
            //
            // This assertion carried a one-tick tolerance until The Precedence,
            // justified as "creatures tied at the same rounded tick are
            // separated by entity id and their exact `f64` days then differ
            // within that tick." That rationale was false, and the tolerance
            // was never bounding the quantity that actually varies: the queue
            // pops by when an action BEGINS while every fact is stamped at the
            // instant it ENDS, so an emission could precede the running max by
            // the population's whole COST SPREAD — 9,925 ticks in this very
            // fixture, and 10,014 on the real seed-42 population.
            //
            // `step_with_occupancy` now sorts its emissions by day, so this is
            // exact and needs no allowance. A tolerance here would silently
            // re-admit the defect.
            let mut prev: Option<WorldTime> = None;
            for f in &facts {
                let d = f.day.expect("every emitted fact is dated");
                if let Some(p) = prev {
                    assert!(
                        d >= p,
                        "`{}` at {d:?} went back past {p:?} (masses={masses:?}) — \
                         the emitted stream is not chronological",
                        f.predicate
                    );
                }
                prev = Some(prev.map_or(d, |p: WorldTime| p.max(d)));
            }
        }
    }
```

- [ ] **Step 2: Run it and confirm it FAILS**

Run: `cargo test -p hornvale-vessel --lib a_faster_creature_acts_more_often_between_a_slower_ones_actions`

Expected: **FAIL** on the `[70.0, 4.375]` arm with "went back past". This is the red that proves the test is real.

If it PASSES: **STOP.** Either the sort was already applied, or the fixture changed. Do not proceed to Task 1.3 — a green here would make Task 1.3 unverifiable.

- [ ] **Step 3: Do NOT commit a red test.** Proceed directly to Task 1.3, which turns it green, and commit both together there.

---

### Task 1.3: Sort the emitted stream by day

**Files:**
- Modify: `windows/vessel/src/liveness.rs:4932` (the `(out, occupancy)` return of `step_with_occupancy`)

**Interfaces:**
- Consumes: `out: Vec<Fact>`, `Fact.day: Option<WorldTime>`.
- Produces: the chronological guarantee every later task and both later stages rely on.

- [ ] **Step 1: Insert the sort**

Find the single occurrence of this line (it is the return of `step_with_occupancy`):

```rust
        (out, occupancy)
```

Replace with:

```rust
        // THE LEDGER'S CHRONOLOGY IS NOT THE POP ORDER (The Precedence,
        // decision <NNNN>, spec section 3). The queue pops creatures by when
        // an action BEGINS; every fact above is stamped at the instant its
        // action ENDS. Those two orderings differ by the population's cost
        // spread whenever creatures act at different tempos — measured at
        // 10,014 ticks on the real seed-42 population, against an invariant
        // that allowed one tick.
        //
        // Sorting here is not a patch on the queue's output: it IS the
        // queue's only cross-entity product. Pop order affects nothing else
        // observable — every `occupancy` access is keyed by the creature's OWN
        // entity, and perception is built from `frozen` before anyone moves —
        // which `the_queues_tie_break_decides_nothing_but_order` asserts, and
        // which will fail loudly the day that stops being true.
        //
        // Sound because a tick is a CLOSED WINDOW: nothing may be dated
        // outside `[from, to]`, so reordering within it can never need to
        // reach past a fact an earlier tick emitted. That watermark is
        // asserted by `every_emitted_fact_is_dated_inside_the_tick_that_emitted_it`.
        //
        // `sort_by_key` on `Option<WorldTime>` is a STABLE, EXACT INTEGER
        // comparison — `WorldTime` is an `i64` tick count with a derived `Ord`
        // (decision 0186), so no float enters the ordering. Stability is
        // load-bearing twice over: a creature's own facts are already
        // monotone and stay in place, and cross-entity ties keep the entity-id
        // order the queue chose, which is what keeps the emitted sequence a
        // pure function of the frozen ledger rather than of the input vector.
        // `decide_step` reads this vector mid-tick, but filters to its own
        // subject and sorts what it finds, so it cannot observe this at all.
        out.sort_by_key(|f| f.day);
        (out, occupancy)
```

Replace `<NNNN>` with the decision number allocated in Task 1.7. If Task 1.7 has not run yet, leave the literal text `decision <NNNN>` and fix it there — **do not guess a number** (`max+1` is forbidden by convention; see Task 1.7).

- [ ] **Step 2: Run the two tests from Tasks 1.1 and 1.2**

Run: `cargo test -p hornvale-vessel --lib every_emitted_fact_is_dated_inside_the_tick_that_emitted_it a_faster_creature_acts_more_often_between_a_slower_ones_actions`

Expected: both PASS, including the `[70.0, 4.375]` arm that failed in Task 1.2.

- [ ] **Step 3: Re-measure the real-world effect — do not inherit it**

The spec's 62-to-0 figure was measured with a **float** sort (`total_cmp` on `as_std_days()`). This task ships an **integer** sort (`sort_by_key` on `Option<WorldTime>`). Those orderings agree wherever every fact is dated, but that is an argument, not a measurement, and the two differ on `None` (float maps to NaN and sorts last; the key form sorts `None` first).

Add this temporary probe to `windows/lab/src/health.rs`, run it, record the numbers in the commit message, then delete it:

```rust
#[cfg(test)]
mod precedence_probe {
    /// TEMPORARY (The Precedence, Task 1.3). Delete before committing.
    #[test]
    fn probe_real_world_is_chronological() {
        let world = hornvale_worldgen::build_world(
            hornvale_kernel::Seed(42),
            &Default::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 42 builds");
        let traces = super::simulate_world(&world);
        let rendered: String = traces
            .iter()
            .map(|t| format!("{}|{:?};", t.species, t.affects))
            .collect();
        let mut h: u64 = 0xcbf29ce484222325;
        for b in rendered.as_bytes() {
            h ^= *b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        eprintln!("PROBE len={} hash={h:016x} traces={}", rendered.len(), traces.len());
    }
}
```

Run: `cargo test -p hornvale-lab --lib probe_real_world_is_chronological -- --nocapture`

Branch on the hash:
- **`len=32587 hash=adb21c8768eb26c5`** → matches the pre-fix baseline exactly; the census does not move. Record it and continue.
- **any other hash** → **STOP.** The integer sort moved a census value the float sort did not. That is a real difference between the two orderings and needs naming before anything lands.

- [ ] **Step 4: Delete the probe**

```bash
git checkout -- windows/lab/src/health.rs
```

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs
git commit -m "fix(vessel): the ledger's chronology is not the scheduler's pop order"
```

The commit body should state: the mechanism (start-ordered pops, end-stamped facts), the measured 62 inverted ticks / worst 10,014 on seed 42, that the invariant's stated rationale was false, and the recorded census hash showing the traces are unchanged.

---

### Task 1.4: Guard that the tie-break is not load-bearing

This is the guard the whole design rests on, promoted from a throwaway probe. It is what fails when creatures start observing each other mid-tick and the sort stops being sufficient.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (in `mod tests`, after the test from Task 1.2)

**Interfaces:**
- Consumes: `interleaving_fixture`, `DriveMovements`.
- Produces: nothing.

- [ ] **Step 1: Write the test**

The fixture's list order is its entity-id order, so reversing the mass list reverses which creature wins a tie. The emitted **multiset** must not change.

```rust
    #[test]
    fn the_queues_tie_break_decides_nothing_but_order() {
        // THE PRECEDENCE'S LOAD-BEARING PRECONDITION. Sorting the emitted
        // stream by day is a sufficient fix ONLY because pop order affects
        // nothing else observable: every `occupancy` access is keyed by the
        // creature's own entity, and perception (`alarm`, hazard memory,
        // belief seeding) is built from `frozen` BEFORE anyone moves. So two
        // populations that differ only in which creature wins a tie must
        // produce the SAME FACTS.
        //
        // `interleaving_fixture` mints entity ids in list order, so reversing
        // the masses reverses the tie-break while keeping the same two
        // creatures. Compare as a MULTISET keyed by (mass, predicate, tick):
        // the two runs assign the ids oppositely, so comparing by raw
        // `EntityId` would report a difference that is only a relabelling.
        //
        // WHEN THIS FAILS, DO NOT RELAX IT. It means a creature has begun to
        // observe another's mid-tick state, pop order has become semantically
        // load-bearing, and the sort is no longer sufficient — the
        // begin/complete event queue named in the spec is then required.
        let run = |masses: [f64; 2]| -> Vec<(u64, String, i64)> {
            let (ledger, terrain, npcs) = interleaving_fixture(&masses);
            let mass_of: std::collections::BTreeMap<EntityId, u64> = npcs
                .iter()
                .map(|n| (n.entity, (n.mass_kg * 1000.0).round() as u64))
                .collect();
            let sys = DriveMovements {
                npcs,
                from: WorldTime::from_std_days(1.0).expect("a day value is finite"),
                to: WorldTime::from_std_days(20.0).expect("a day value is finite"),
                params: SUSTENANCE,
                day_ticks: None,
                terrain: &terrain,
            };
            let mut rows: Vec<(u64, String, i64)> = sys
                .step(&ledger)
                .iter()
                .map(|f| {
                    (
                        *mass_of
                            .get(&f.subject)
                            .expect("every emitter is in the roster"),
                        f.predicate.clone(),
                        f.day.expect("every emitted fact is dated").ticks(),
                    )
                })
                .collect();
            rows.sort();
            rows
        };
        let forward = run([4.375, 70.0]);
        let reversed = run([70.0, 4.375]);
        assert!(
            !forward.is_empty(),
            "the fixture emitted nothing; it cannot pin a tie-break"
        );
        assert_eq!(
            forward, reversed,
            "reversing the queue's tie-break changed WHAT happened, not just \
             the order it was reported in — pop order has become semantically \
             load-bearing and sorting the emissions is no longer a sufficient fix"
        );
    }
```

(`WorldTime::ticks()` is `pub const fn ticks(self) -> i64` and is already used in this test module — verified, not assumed.)

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-vessel --lib the_queues_tie_break_decides_nothing_but_order`

Expected: PASS.

If it FAILS: **STOP and report.** This contradicts a measurement taken during the spec (reversing the tie-break left the seed-42 affect traces bit-identical). Either the fixture reaches a path the real population does not, or the measurement was wrong — both are findings that must be understood before Stage 3.

- [ ] **Step 3: Prove the guards can actually fail**

A guard that cannot fail is worse than no guard, and a no-op mutation produces false evidence. Temporarily delete the `out.sort_by_key(|f| f.day);` line from Task 1.3 and re-run the vessel lib tests.

Expected: `a_faster_creature_acts_more_often_between_a_slower_ones_actions` goes RED. Confirm that specific test is the one that reddens — this tie-break test compares a sorted multiset and is deliberately insensitive to ordering, so it may well stay green; its job is to catch a change in *what happened*, not in what order it was reported.

Then restore the line.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs
git commit -m "test(vessel): guard that the queue's tie-break decides nothing but order"
```

---

### Task 1.5: Full suites, both crates

`gate-commit` runs the sub-floor tier only. This is the check that should precede any submission.

**Files:** none modified.

- [ ] **Step 1: Run the vessel suite in full, capture once**

```bash
cargo test -p hornvale-vessel > /tmp/hv-vessel.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-vessel.log
grep -E "FAILED|panicked" /tmp/hv-vessel.log
```

Expected: three `test result: ok` lines, 0 failed. The pre-stage baseline was 455 and 220 passing.

- [ ] **Step 2: Run the lab suite in full, capture once — a SEPARATE command**

```bash
cargo test -p hornvale-lab > /tmp/hv-lab.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-lab.log
grep -E "FAILED|panicked" /tmp/hv-lab.log
```

Expected: 0 failed. The pre-stage baseline was 357, 3 and 130 passing.

- [ ] **Step 3: If anything is red, STOP and report the failure list.** Do not fix opportunistically — a red here is information about the sort's blast radius and the campaign wants to see it whole. Add `--no-fail-fast` to get the complete list in one pass.

- [ ] **Step 4: Check spec criterion 2 by COUNTING, not by recollection**

The spec requires no tolerance term to survive anywhere in the file. A criterion phrased as an absolute is checkable by counting and should be counted.

```bash
grep -n "prev - tick\|- tick\b\|TickSpan::from_ticks(1).as_std_days()" windows/vessel/src/liveness.rs
```

Expected: **no hits.** Any hit is either the old tolerance surviving, or a new one someone reintroduced — inspect each and remove it. If a hit is a legitimate unrelated use of the identifier `tick`, say so explicitly in the task report rather than waving it through.

---

### Task 1.6: Artifact drift — adjudicate by branch, do not predict

**Files:** determined by the run, not by this plan.

- [ ] **Step 1: Regenerate and diff**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); echo "drift-exit=$?"
git status --short
```

- [ ] **Step 2: Adjudicate. This is a branch table, not an expectation — the plan author did not run this.**

- **Nothing moved** (`drift-exit=0`, clean status) → continue to Task 1.7.
- **Only `docs/audits/` moved** → the type-audit report drifting. Stage 1 changes no `pub` signature, so this is mildly surprising but benign; inspect the diff, then `git add docs/audits/` and commit it alongside Task 1.7.
- **`book/src/domesday/` or a census CSV moved** → **STOP.** Those are pure reads over the committed census, which Task 1.3 measured as unchanged. A contradiction of a measurement is a finding.
- **`windows/vessel/tests/fixtures/` moved** → **STOP.** Only `REBASELINE=1` writes those.
- **Anything else moved** → **STOP** and name it before regenerating anything.

- [ ] **Step 3: If `make rebaseline` itself reports a failed parallel job**, re-run it. A generator that loses the cargo build lock can truncate an artifact and report only "a parallel job failed" (a known defect, a Foliot follow-up). Verify the artifact's length is plausible before committing it.

---

### Task 1.7: The decision record and the registry correction

**Files:**
- Create: `docs/decisions/<NNNN>-the-ledgers-chronology-is-not-the-pop-order.md`
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `windows/vessel/src/liveness.rs` (fill in `<NNNN>` in the Task 1.3 comment)

- [ ] **Step 1: Allocate a decision number properly**

```bash
make decision-block
```

Use a number from the reserved block it prints. **Do not use `max+1`** — the convention forbids it and a previous campaign minted a colliding number that way.

- [ ] **Step 2: Write the decision record**

Follow the format of a recent record in `docs/decisions/`. It must state:
- the defect (pop order is start-ordered, stamps are end-ordered);
- the fix, and why it is root-cause rather than a patch (pop order's only cross-entity product IS the emitted order);
- the precondition (a tick is a closed window) and where it is asserted;
- **the trigger that retires it** — when creatures observe each other mid-tick, the begin/complete event queue becomes necessary, and `the_queues_tie_break_decides_nothing_but_order` is the test that will say so;
- that widening the invariant's tolerance was considered and rejected, because it would enshrine a false rationale and grow with the mass band.

- [ ] **Step 3: Correct the two registry rows**

In `book/src/frontier/idea-registry.md`:
- `TOOL-liveness-accumulates-f64-days` — it is **not** blocked by the retype's own arithmetic. Record that the blocker was this pre-existing ordering defect, now fixed, and that the row is unblocked.
- Add or update a row for the deferred begin/complete queue, citing the trigger.

Do not delete rows. Supersede, never edit away.

- [ ] **Step 4: Fill in the decision number in the source comment**

Replace `decision <NNNN>` in the `step_with_occupancy` comment with the allocated number.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "decision: the ledger's chronology is not the scheduler's pop order"
```

---

### Task 1.8: Land stage 1

Stage 1 is self-contained and independently valuable. Landing it now means stages 2 and 3 do not pay its absorption toll repeatedly.

- [ ] **Step 1: Submit to the sluice**

```bash
git push -u origin campaign/the-precedence
make sluice BRANCH=campaign/the-precedence REF=$(git rev-parse HEAD)
```

Use the full SHA, never a branch name. Then follow with `make sluice-status` and `make sluice-log`.

- [ ] **Step 2: If the mouth refuses (exit 21/23/24)**, absorb main locally and resubmit. A conflict is refused in milliseconds before the box is taken — that is the signal, not a failure.

- [ ] **Step 3: Post the technique to the board**

```bash
make board-post KIND=technique NOTE='The vessel scheduler pops by action START and stamps facts at action END; the emitted stream was not chronological across creatures (62 inverted ticks on seed 42, worst 10014). Fixed by sort_by_key(day) at the point of production. The one-tick tolerance in the monotonicity invariant was masking it, and its stated rationale (sub-tick f64 noise) was false.' PATHS='windows/vessel/'
```

---

# Stage 2 — prove the retype is unblocked, with a stop condition

**Goal:** establish, before committing to Stage 3, that the retype's blocker is gone.
**Success Criteria:** a recorded branch outcome naming which of The Foliot's two failures survive.
**Status:** Not Started

A fix task cannot audit its own premise. Stage 3 exists to land the retype; this stage exists to prove Stage 3 is possible, and it is allowed to conclude that it is not.

---

### Task 2.1: Throwaway retype spike

**Files:**
- Modify (throwaway, reverted at the end): `windows/vessel/src/liveness.rs`

- [ ] **Step 1: Apply the minimal retype as a spike**

Change `WalkState.day` (`windows/vessel/src/liveness.rs:4973`) from `f64` to `WorldTime` and follow the compiler until the crate builds. **Do not** retype `last_drank`/`last_rested`/`last_ate` in this spike — the point is to reproduce The Foliot's exact attempt, which left them continuous, and see whether its failure survives Stage 1.

Convert at each site with the kernel's named hatch, and do not attempt to make the result elegant. This code is being thrown away.

- [ ] **Step 2: Run the full vessel suite, capture once**

```bash
cargo test -p hornvale-vessel > /tmp/hv-spike.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-spike.log
grep -E "FAILED|panicked" /tmp/hv-spike.log
```

- [ ] **Step 3: Adjudicate against The Foliot's two recorded failures**

- **`a_faster_creature_acts_more_often_between_a_slower_ones_actions` is GREEN** → the ordering defect was the blocker and Stage 1 removed it. Proceed.
- **It is still RED** → **STOP and report.** Record the two day values and whether they are tick-aligned. Stage 3's premise is false and the campaign re-specs rather than pressing on. This is the whole reason this stage exists.
- **`the_hoisted_walk_emits_exactly_what_the_loop_emitted` is RED by ONE tick** → expected and legitimate. The Foliot identified this as a genuine golden move (the drift being removed), not a defect. Record it; it becomes a deliberate golden update in Stage 3, never a silent rebaseline.
- **Any OTHER test is red** → record it. It is new information neither campaign has seen, and Stage 3's task list must account for it before starting.

- [ ] **Step 4: Revert the spike completely**

```bash
git checkout -- windows/vessel/src/liveness.rs
git status --short
```

The status must be clean.

- [ ] **Step 5: Record the findings**

Write the branch outcome, the exact failing test names, and the one-tick golden's before/after values into `.superpowers/sdd/followups.md`. Stage 3 reads this. Nothing is committed to the repo's tracked tree from this stage — it produces knowledge, not code.

---

# Stage 3 — the retype

**Goal:** every *instant* in the walk is a `WorldTime`; continuous integrals keep `f64` and name the crossing.
**Success Criteria:** zero `day: f64` at any instant site; full vessel and lab suites green; artifact drift adjudicated.
**Status:** Not Started — gated on Stage 2

---

### Task 3.1: Classify the instant surface — no code change

The Foliot retyped `day` alone and left its three siblings continuous, which is what made the seam feel unbounded. Place the seam on a principle first, and **count rather than recollect**.

**Files:**
- Create: `.superpowers/sdd/instant-classification.md` (campaign scratch, dies with the worktree — promote anything durable into the retrospective)

- [ ] **Step 1: Enumerate every candidate site**

```bash
grep -n "day: f64\|last_drank: f64\|last_rested: f64\|last_ate: f64\|entry_day: f64\|horizon: f64\|-> f64" windows/vessel/src/liveness.rs > /tmp/hv-sites.txt
wc -l /tmp/hv-sites.txt
```

At the time of writing there were 14 `day: f64` declarations. **Do not trust that number** — re-count and use your count.

- [ ] **Step 2: Classify each site, by reading it, into exactly one of three**

- **INSTANT** — a point on the time axis. Becomes `WorldTime`. (Expected: `WalkState.{day,last_drank,last_rested,last_ate}`, the four fact helpers' `day`, `last_fact_day_at_or_before`'s parameter and return, `catch_up`'s `entry_day`, `decide_step`'s `day` and `last_*`, `hold_step`'s `day`, `next_awake_day`'s `day`.)
- **DURATION** — a span. Becomes `TickSpan`, or stays `f64` if it is a continuous physical quantity. (Expected: `anticipation_lead`'s return, `AnticipationX.horizon`.)
- **RATIO** — dimensionless. Stays bare `f64`, per project convention.

`catch_up`'s `horizon` is an INSTANT (it is "now", the ceiling) while `anticipation_lead`'s `horizon` is a DURATION. **They share a name and are different kinds.** Classify by reading the use, never by the identifier.

- [ ] **Step 3: Write the table** to `.superpowers/sdd/instant-classification.md` as `file:line | identifier | INSTANT|DURATION|RATIO | why`.

- [ ] **Step 4: Stop condition.** If any site cannot be confidently classified, **STOP and report it** rather than guessing. A misclassified instant is exactly the defect decision 0126 exists to prevent — an untyped day cost a campaign a predicate no world could commit.

---

### Task 3.2: Retype `WalkState`'s four instants and the four fact helpers

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — `WalkState` (`:4969`), `agent_at_fact` (`:4124`), `place_agent` (`:4144`), `drank_fact` (`:4148`), `rested_fact` (`:4161`), `eaten_fact` (`:4174`)

**Interfaces:**
- Produces, for Task 3.3:
  - `agent_at_fact(entity: EntityId, target: &Facet, day: WorldTime, provenance: &str) -> Fact`
  - `drank_fact(entity: EntityId, day: WorldTime, provenance: &str) -> Fact`
  - `rested_fact(entity: EntityId, day: WorldTime, provenance: &str) -> Fact`
  - `eaten_fact(entity: EntityId, day: WorldTime, provenance: &str) -> Fact`
  - `WalkState { day: WorldTime, last_drank: WorldTime, last_rested: WorldTime, last_ate: WorldTime, .. }`

- [ ] **Step 1: Retype the four fact helpers**

Each currently converts at the boundary. After the change the conversion is gone, because `Fact.day` is already `WorldTime` (decision 0126) — this is the seam the spec places. `agent_at_fact` becomes:

```rust
pub(crate) fn agent_at_fact(
    entity: EntityId,
    target: &Facet,
    day: WorldTime,
    provenance: &str,
) -> Fact {
    Fact {
        subject: entity,
        predicate: AGENT_AT.to_string(),
        object: Value::Text(room_to_text(target)),
        place: None,
        day: Some(day),
        provenance: provenance.to_string(),
    }
}
```

Apply the identical shape to `drank_fact`, `rested_fact` and `eaten_fact`. Each loses its `WorldTime::from_std_days(day).expect("simulated day is finite")` — **that `expect` disappearing is the point of the change**, not incidental: an instant that was already exact can no longer fail to be constructed.

`place_agent` currently round-trips through `f64`:

```rust
pub fn place_agent(entity: EntityId, room: &Facet, day: WorldTime) -> Fact {
    agent_at_fact(entity, room, day.as_std_days(), "harness-placement")
}
```

It becomes a straight pass-through — delete the `.as_std_days()`.

- [ ] **Step 2: Retype the four `WalkState` instants**

Change `day`, `last_drank`, `last_rested` and `last_ate` to `WorldTime`, and update each field's doc comment to say it is an instant.

- [ ] **Step 3: Build and follow the compiler**

```bash
cargo check -p hornvale-vessel --all-targets > /tmp/hv-check.log 2>&1; echo "exit=$?"
grep -c "^error" /tmp/hv-check.log
```

Every error is a call site that must convert or be retyped. **At each one, decide from Task 3.1's table** whether the other side is an instant (retype it, Task 3.3) or a continuous quantity (convert explicitly through the kernel's named hatch, and name the crossing in a comment at the call site).

Do not paper over an error with a conversion whose direction you have not classified. That is how the seam became unprincipled the first time.

- [ ] **Step 4: Do not commit until Task 3.3 builds clean.** These two tasks are one compiling unit.

---

### Task 3.3: Retype the reader and decider surface

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — `learned_helplessness` (`:174`), `next_awake_day` (`:2218`), `room_entry_day` (`:4212`), `hold_step` (`:4259`), `decide_step` (`:4335`), `last_fact_day_at_or_before` (`:4500`), `catch_up` (`:4552`), and every call site the compiler names.

**Interfaces:**
- Consumes: the signatures Task 3.2 produced.
- Produces: a crate where every INSTANT from Task 3.1's table is `WorldTime`.

- [ ] **Step 1: Retype each site the classification table marks INSTANT**

`last_fact_day_at_or_before` is the shape worth stating, because its `fold` currently uses `0.0` as an identity and that is wrong for a signed instant (negative ticks are legal, decision 0126):

```rust
fn last_fact_day_at_or_before(
    ledger: &Ledger,
    predicate: &str,
    entity: EntityId,
    day: WorldTime,
) -> Option<WorldTime> {
    ledger
        .facts_of(entity, predicate)
        .filter_map(|f| f.day)
        .filter(|&d| d <= day)
        .max()
}
```

Returning `Option` rather than folding to a `0.0` sentinel is the honest signature: "no such fact" and "a fact at genesis" are different answers, and the `f64` version could not tell them apart. **Each caller must then say which it means** — that is the intended work of this step, not incidental churn. A caller that genuinely wants a default writes `.unwrap_or(WorldTime::GENESIS)` at the call site, where the choice is visible.

- [ ] **Step 2: At every crossing into a continuous integral, convert explicitly and comment it**

The thirst and fatigue integrals are continuous physical quantities and stay `f64`. Where an instant feeds one, convert at the call site and state the direction:

```rust
// The thirst integral is continuous (a temperature-weighted rate over a
// span), so the instant crosses to `f64` standard days HERE, at the
// integral's own edge, rather than the walk carrying a float clock to suit
// it. Lossless below ~2.47e8 years (decision 0186).
let day_continuous = st.day.as_std_days();
```

- [ ] **Step 3: Build clean**

```bash
cargo check -p hornvale-vessel --all-targets > /tmp/hv-check2.log 2>&1; echo "exit=$?"
grep -c "^error" /tmp/hv-check2.log
```

Expected: `0`.

- [ ] **Step 4: Run the full vessel suite, capture once**

```bash
cargo test -p hornvale-vessel > /tmp/hv-retype.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-retype.log
grep -E "FAILED|panicked" /tmp/hv-retype.log
```

Branch, using Stage 2's recorded findings:
- **All green** → continue to Task 3.4.
- **Only `the_hoisted_walk_emits_exactly_what_the_loop_emitted` is red, by ONE tick** → the golden move Stage 2 predicted. Update it as a **deliberate, commented** change stating the old value, the new value, and that the difference is accumulated `f64` drift being removed. Do not rebaseline it silently.
- **The monotonicity invariant is red** → **STOP.** Stage 2 asserted this could not happen. Report before touching anything.
- **Anything else** → STOP and report.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/liveness.rs
git commit -m "refactor(vessel): every instant in the walk is a WorldTime"
```

The commit body should state the seam principle (instants become `WorldTime`; continuous integrals keep `f64` and name the crossing), why all four `WalkState` instants move rather than only `day`, and the `last_fact_day_at_or_before` signature change.

---

### Task 3.4: Artifact adjudication and close

**Files:** determined by the run.

- [ ] **Step 1: Run the vessel suite, capture once**

```bash
cargo test -p hornvale-vessel > /tmp/hv-v.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED" /tmp/hv-v.log
```

- [ ] **Step 2: Run the lab suite, capture once — a SEPARATE command**

```bash
cargo test -p hornvale-lab > /tmp/hv-l.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED" /tmp/hv-l.log
```

- [ ] **Step 3: Regenerate and adjudicate — branch table, not a prediction**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); echo "drift-exit=$?"
```

- **Only `docs/audits/` moved** → expected. Stage 3 changes `pub` signatures (`agent_at_fact`, `rested_fact`, `place_agent`), and the type-audit report is a whole-repo aggregate that drifts on any `pub` boundary change. Regenerate and commit in the same commit.
- **Nothing moved** → also fine; commit as-is.
- **A census artifact or `book/src/domesday/` moved** → **STOP.** Report which metric and by how much before deciding anything.
- **`windows/vessel/tests/fixtures/` moved** → **STOP.** Only `REBASELINE=1` writes those.

- [ ] **Step 4: Re-verify the type-audit tags**

Retyping a `pub` boundary invalidates its `type-audit:` verdict tag, and a stale tag on a changed signature is a known footgun.

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
```

Every primitive that left the boundary should no longer need a tag; every one that remains needs a current verdict. Fix tags rather than adding waivers.

- [ ] **Step 5: Submit**

```bash
git push origin campaign/the-precedence
make sluice BRANCH=campaign/the-precedence REF=$(git rev-parse HEAD)
```

- [ ] **Step 6: Close the campaign** using the `closing-a-campaign` skill — chronicle entry, book freshness sweep, retrospective, census re-pin decision.

The retrospective's central lesson is already known and should not be lost: **a fixture's own construction masked a live defect for a whole campaign.** `interleaving_fixture` minted entity ids in mass order, which made the list order the queue's tie-break order, which made the published fixture the one arrangement where the bug could not appear. Three attempts reasoned about the failure; one measurement in the other arrangement settled it.

---

## Notes for the executor

**The three stop conditions are the point of this plan.** Task 1.1 can refute the design; Task 2.1 can refute Stage 3; Task 3.1 can refute the seam. Each exists because the preceding campaign failed by reasoning past exactly that question. If you hit one, stop and report — do not work around it.

**Do not trust counts in this document.** The 14 `day: f64` sites, the 455/220 vessel test counts, the `adb21c8768eb26c5` census hash: all were measured on `672edba22` and are given so you can notice a discrepancy, not so you can skip measuring.

**`make gate-commit` is not a green crate.** It runs the sub-floor tier only. Run the crate's own suite.
