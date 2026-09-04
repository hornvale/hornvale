# The Minute Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A possessed body's own walk during `wait` commits what it does — drinks, meals, rests, sleeps, moves — so its ledger, its felt state and its position column agree, and the wait line tells the watching player what the body did.

**Architecture:** One call site changes: `Session::wait` in `windows/vessel/src/session.rs` stops discarding `step_one_with_controller`'s facts and commits them through the same loop shape the population's facts use, then writes the driven slot's position and felt together. Two rules ride on it: off the walk band (indoors, underwater, underground) the held body's walk is asked through the Holding `PlayerController`; and the wait narration minutes the driven body's own committed facts. Nothing in `liveness.rs`'s walk, `controller.rs`'s controllers or the roster's storage changes shape.

**Tech Stack:** Rust 2024, `hornvale-vessel` (the possession window), `hornvale-kernel` (`Ledger`, `Fact`, `WorldTime`, `TickSpan`). Tests: in-module `#[cfg(test)]` in `session.rs` where private state is needed, one new integration module `windows/vessel/tests/suite/the_minute.rs` otherwise. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-09-03-the-minute-design.md` — read it first; every task cites its section. Ledger: `docs/superpowers/ledgers/2026-09-03-the-minute.md`. Decision block 0656–0665.

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only (decision 0004/0041). No new crates.
- No `HashMap`/`HashSet`; `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock time; no `Instant` in tests.
- Every `pub` item gets a one-line doc comment (`#![warn(missing_docs)]`). Every primitive at a `pub` boundary carries a `type-audit:` tag; a new `pub` surface drifts `docs/audits/type-audit-report.md`, which `make gate-commit` regenerates and diffs.
- Determinism: the driven facts commit AFTER the population's facts and BEFORE the First Mark's `turned-hostile` loop, every tick, in the walk's own order (spec §3.1).
- The free path is byte-identical: no committed fixture under `docs/generated-paths.txt` may move except `docs/audits/` (spec §4 P4). The decision rule: `clients/game/core/tests/fixtures/`, `windows/vessel/tests/fixtures/` or a gallery transcript moving → STOP and report; only `docs/audits/` moving → commit it in the same commit; nothing moving → expected.
- `cargo fmt` is the last step before every commit; the pre-commit hook runs `make gate-commit` on any Rust path. Commit messages end with the `Claude-Session:` trailer the session carries; write the message to a file and use `git commit -F` (an apostrophe inside a `$(cat <<'EOF' …)` heredoc breaks the shell).
- Work in the worktree `.claude/worktrees/the-minute` on branch `campaign/the-minute`. Push at every task boundary.
- Renamed or new tests are not in `docs/timings/subfloor-roster.tsv` until a green stage gate rewrites it, so `make gate-commit` compiles them but does not run them. Run them by name (`cargo test -p hornvale-vessel …`) before claiming green.
- The word `cell` is guarded by `cli/tests/suite/lexicon_guard.rs`; do not introduce it in a new identifier.

---

## File structure

| file | responsibility in this campaign |
|---|---|
| `windows/vessel/src/session.rs` — `Session::wait` (≈ lines 8294–8675) | the commit of the driven walk's facts; the driven slot write; the off-band controller choice; `wake_at` from a walk-committed sleep; the call into `narrate_motion` |
| `windows/vessel/src/session.rs` — `narrate_motion` (≈ 8720–8790) | the wait line, now minuting the driven body's own facts |
| `windows/vessel/src/session.rs` — new free functions `wake_after`, `minutes_of`, `enum Minute` (private) | pure helpers, unit-tested |
| `windows/vessel/src/session.rs` — `#[cfg(test)] mod tests` (from line 10571) | the red witness, the free-path control, the off-band test, helper unit tests |
| `windows/vessel/src/roster.rs` — `Roster::write`/`resolve` (≈ 264–305) | `resolve` deleted (its only caller was the discard); `write`'s doc amended |
| `windows/vessel/src/controller.rs` (≈ 40–200) | doc freshness only: the discard is no longer a fact |
| `windows/vessel/src/liveness.rs` — `step_one_with_controller` doc (≈ 7530–7600) | doc freshness only |
| `windows/vessel/tests/suite/the_minute.rs` (new) + `mod the_minute;` in `tests/suite.rs` | P1, P2, P7 as integration tests over public API |
| `windows/vessel/tests/suite/the_rack.rs` — `a_possessed_sessions_columns_are_the_ledgers_too` | P3: the VIEW ≡ SCAN sweep gains the eight-wait script |
| `docs/decisions/0656…0658-*.md`, `docs/decisions/README.md` | the three decision records |
| `book/src/chronicle/the-minute.md`, `book/src/SUMMARY.md`, `book/src/chronicle/the-coercion.md`, `book/src/chronicle/the-rack.md`, `book/src/frontier/idea-registry.md` | chronicle, freshness sweep, registry flips |
| `docs/retrospectives/the-minute.md`, `docs/superpowers/ledgers/2026-09-03-the-minute.md` | process record, campaign ledger |

Names used across tasks (so a task's implementer knows what a neighbour relies on):

- `fn wake_after(facts: &[Fact], now: WorldTime) -> Option<WorldTime>` (Task 3), private, in `session.rs`.
- `enum Minute { Moved, Drank, Eaten, Rested }` and `fn minutes_of(facts: &[Fact]) -> Vec<Minute>` (Task 4), private, in `session.rs`.
- `fn narrate_motion(&self, moved: usize, before: &[Facet], sensed_before: &BTreeSet<EntityId>, how: Perceiving, driven_before: &Facet, minutes: &[Minute]) -> String` (Task 4).
- Test helper `fn driven_facts_named(session: &Session<'_>, predicate: &str) -> usize` (Task 1), in `the_minute.rs`.

---

### Task 1: The red witnesses and the free-path control

Spec §1, §4 (P1, P2, P4's control). Tests first; the ones that must be red today are named as such. Nothing in `src/` changes in this task except the deletion of one obsolete test.

**Files:**
- Modify: `windows/vessel/src/session.rs` — `#[cfg(test)] mod tests` (append at the end of the module, before its final `}`); delete `a_possessed_walk_ends_where_the_ledger_never_recorded` (≈ lines 20844–20960, the last test in the file) and its doc comment.
- Create: `windows/vessel/tests/suite/the_minute.rs`
- Modify: `windows/vessel/tests/suite.rs` — add `mod the_minute;` in alphabetical position (after `mod the_lintel;`-style neighbours; the list is alphabetical).

**Interfaces:**
- Consumes: `Session::start`, `handle`, `possessor`, `position`, `driven_mode`, `driven_affect`, `committed_fact_count_for`, `session_ledger_json`, `agent_entity` (all `pub`); in-module: `session.ledger`, `session.wctx`, `session.folds`, `DriveMovements`, `step_one_with_controller`, `LocaleTerrain::with_fields`, `agent_position` (all already used by the test being replaced — copy its construction verbatim).
- Produces: `driven_facts_named`, used by Tasks 3 and 4's tests.

- [ ] **Step 1: Replace the obsolete in-module test with the red witness**

Delete `a_possessed_walk_ends_where_the_ledger_never_recorded` and its doc comment. In its place, at the same spot, add:

```rust
    /// The Minute, spec §3.2: under possession the driven body's solo walk
    /// ENDS WHERE THE LEDGER RECORDED, because `wait` now commits that walk's
    /// facts (spec §3.1) and writes the column from the same walk.
    ///
    /// This replaces `a_possessed_walk_ends_where_the_ledger_never_recorded`,
    /// which pinned the defect: it asserted the walk's end differed from the
    /// ledger's fold. Every assertion here is the inverse of one there, and
    /// the first is still the NON-VACUITY guard: if the imposed walk stopped
    /// acting, the agreement below would hold for a reason that has nothing
    /// to do with the commit.
    ///
    /// Seed 7, because seed 42's flagship never leaves its room (measured: 0
    /// `agent-at` across 500 days) and the whole point is a walk that moves.
    ///
    /// RED BEFORE TASK 2 (observed while writing it): `the walk's end must be
    /// the ledger's fold` — left is the room the walk reached, right is the
    /// origin room the ledger still names.
    #[test]
    fn a_possessed_walk_ends_where_the_ledger_recorded() {
        let world = build_world(
            Seed(7),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 7 builds");
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let _ = session.handle("!possess");
        assert!(
            session.possessor().is_some(),
            "possession must be open, or the walk is asked through \
             PlayerController and cannot move at all"
        );
        let _ = session.handle("!wait 1");
        let frozen = session.ledger.clone();
        let from = session.day;
        let before = session.committed_fact_count_for(session.agent_entity());
        let _ = session.handle("!wait 5");
        let to = session.day;
        let after = session.committed_fact_count_for(session.agent_entity());

        let terrain = LocaleTerrain::with_fields(
            &session.wctx.ctx,
            session.calendar.as_ref(),
            session.predator.as_ref(),
            session.prey.as_ref(),
            Some(&session.built),
            Some(&session.mesh_memo),
        )
        .with_ground(&session.ground);
        let sys = DriveMovements {
            npcs: Vec::new(),
            from,
            to,
            params: SUSTENANCE,
            day_ticks: session.day_ticks(),
            terrain: &terrain,
            folds: &session.folds,
        };
        let driven_body = session.driven_body().clone();
        let (driven_facts, driven_written) = sys.step_one_with_controller(
            &frozen,
            &driven_body,
            &mut hornvale_kernel::RoomMeshMemo::new(),
            &mut HomeNavCache::new(),
            &mut ImposedController::new(),
        );
        let driven = session.roster.driven();
        let scanned = agent_position(&session.ledger, &driven_body, session.day);

        // NON-VACUITY: the imposed walk really did act.
        assert!(
            !driven_facts.is_empty(),
            "the imposed walk must actually act, or there is nothing to minute"
        );
        assert_ne!(
            driven_written.position,
            agent_position(&frozen, &driven_body, from),
            "the imposed walk must MOVE, or the position half is untested"
        );
        // THE WALK'S FACTS REACHED THE LEDGER — all of them, appended.
        assert_eq!(
            after - before,
            driven_facts.len(),
            "every fact the driven walk emitted must have been appended"
        );
        // THE WALK'S END IS THE LEDGER'S FOLD.
        assert_eq!(
            driven_written.position, scanned,
            "the walk's end must be the ledger's fold"
        );
        // AND THE COLUMN IS BOTH.
        assert_eq!(
            session.roster.positions()[driven.0],
            scanned,
            "the driven slot's column follows the ledger, which now knows the walk"
        );
        assert_eq!(
            session.roster.felts()[driven.0],
            driven_written.felt,
            "the driven slot's felt IS that same walk's resolution"
        );
        assert!(
            session.roster.resolved_felt(driven).is_some(),
            "…and the tick flipped the slot's `written` flag doing it"
        );
    }

    /// The Minute, spec §4 P4's POSITIVE CONTROL, measured before the spec
    /// was written and pinned here so it cannot silently stop being true: a
    /// FREE body's solo walk, asked through a `PlayerController` with nothing
    /// queued, emits NO facts and ends in the column's own room. This is the
    /// whole reason `wait` may commit the driven walk's facts unconditionally
    /// (spec §3.1) without moving a byte of any free-session fixture.
    ///
    /// Green before and after Task 2. If it ever goes red, the free path is
    /// no longer inert and every session golden is suspect.
    #[test]
    fn a_free_walk_emits_nothing_and_ends_in_the_column() {
        for seed in [42u64, 7u64] {
            let world = if seed == 42 {
                seam_world()
            } else {
                build_world(
                    Seed(seed),
                    &SkyPins::default(),
                    SkyChoice::Generated,
                    &TerrainPins::default(),
                    &SettlementPins::default(),
                )
                .expect("seed 7 builds")
            };
            let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
            assert!(session.possessor().is_none(), "this is the FREE control");
            for i in 0..4 {
                let frozen = session.ledger.clone();
                let from = session.day;
                let before = session.committed_fact_count_for(session.agent_entity());
                let _ = session.handle("!wait 5");
                let to = session.day;
                let after = session.committed_fact_count_for(session.agent_entity());
                let terrain = LocaleTerrain::with_fields(
                    &session.wctx.ctx,
                    session.calendar.as_ref(),
                    session.predator.as_ref(),
                    session.prey.as_ref(),
                    Some(&session.built),
                    Some(&session.mesh_memo),
                )
                .with_ground(&session.ground);
                let sys = DriveMovements {
                    npcs: Vec::new(),
                    from,
                    to,
                    params: SUSTENANCE,
                    day_ticks: session.day_ticks(),
                    terrain: &terrain,
                    folds: &session.folds,
                };
                let body = session.driven_body().clone();
                let (facts, written) = sys.step_one_with_controller(
                    &frozen,
                    &body,
                    &mut hornvale_kernel::RoomMeshMemo::new(),
                    &mut HomeNavCache::new(),
                    &mut PlayerController::new(),
                );
                assert!(
                    facts.is_empty(),
                    "seed {seed} wait#{i}: a Holding walk must emit nothing, got {facts:?}"
                );
                assert_eq!(
                    written.position,
                    session.position(),
                    "seed {seed} wait#{i}: a Holding walk ends where the column says"
                );
                assert_eq!(
                    after, before,
                    "seed {seed} wait#{i}: a free body commits nothing during wait"
                );
            }
        }
    }
```

- [ ] **Step 2: Run both in-module tests**

Run: `cargo test -p hornvale-vessel --lib -- a_possessed_walk_ends_where_the_ledger_recorded a_free_walk_emits_nothing_and_ends_in_the_column`
Expected: `a_free_walk_emits_nothing_and_ends_in_the_column` PASS; `a_possessed_walk_ends_where_the_ledger_recorded` FAIL at `every fact the driven walk emitted must have been appended` (left 0). Paste the failing assertion text into the test's doc comment where it says "observed while writing it".

- [ ] **Step 3: Create the integration module**

Create `windows/vessel/tests/suite/the_minute.rs`:

```rust
//! The Minute — a held body's acts are minuted (spec
//! `docs/superpowers/specs/2026-09-03-the-minute-design.md`, §4).
//!
//! P1 and P2 are the campaign's preregistered measurements, frozen before
//! the code. P7 (Task 4) joins this file. Everything here reads public API;
//! the in-module tests in `session.rs` hold the halves that need the ledger.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::liveness::{AffectLabel, Mode};
use hornvale_vessel::{PossessOpts, Session};

fn world_at(seed: u64) -> World {
    if seed == 42 {
        return hornvale_worldgen::seed_42_world();
    }
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("the seed builds")
}

/// How many committed facts with `predicate` name the driven body as
/// subject. Read through the ledger's serialized form because the ledger
/// itself is private to the session; `session_ledger_json` is the
/// determinism accessor and serializes `facts` as an array of `Fact`s.
pub fn driven_facts_named(session: &Session<'_>, predicate: &str) -> usize {
    let json: serde_json::Value =
        serde_json::from_str(&session.session_ledger_json()).expect("a ledger is JSON");
    let me = serde_json::to_value(session.agent_entity()).expect("an id serializes");
    json["facts"]
        .as_array()
        .expect("a ledger has a facts array")
        .iter()
        .filter(|f| f["subject"] == me && f["predicate"] == predicate)
        .count()
}

fn possessed(seed: u64) -> (World, ()) {
    (world_at(seed), ())
}

/// P1 — seed 42, the minuted drink. Before Task 2 (measured 2026-09-03): the
/// walk emitted 29 facts in 40 days, `drank` on every tick from the second,
/// and 0 reached the ledger while the felt state read `Content`. After: the
/// drinks are on the ledger and the felt state is unchanged — it was right,
/// the ledger was wrong.
///
/// RED BEFORE TASK 2: `seed 42's held body must have its drinks minuted`
/// (left 0).
#[test]
fn p1_a_held_bodys_drinks_reach_the_ledger_and_its_felt_state_stands() {
    let (world, ()) = possessed(42);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    assert!(session.possessor().is_some(), "possession must be open");
    for _ in 0..8 {
        let _ = session.handle("!wait 5");
    }
    assert!(
        driven_facts_named(&session, "drank") >= 7,
        "seed 42's held body must have its drinks minuted: got {}",
        driven_facts_named(&session, "drank")
    );
    assert_eq!(session.driven_mode(), Some(Mode::Idle));
    assert_eq!(
        session.driven_affect(),
        Some(AffectLabel::Content),
        "the felt state was already right; only the ledger moves"
    );
}

/// P2 — seed 7, progress accumulates. Before Task 2: the walk sought water
/// for 14 then 15 rooms, restarted from the origin every tick, and the body
/// read `Helpless` from day 20 with 0 `drank`. The mechanism half (the
/// column moves on the first acting wait) is asserted unconditionally. The
/// PREDICTION half — at least one `drank` by day 36 — is the preregistered
/// bet, and the plan's decision rule for a red result is in Task 2.
///
/// RED BEFORE TASK 2 at the first assertion: the column does not move.
#[test]
fn p2_a_held_bodys_walk_accumulates_across_ticks() {
    let (world, ()) = possessed(7);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    assert!(session.possessor().is_some(), "possession must be open");
    let _ = session.handle("!wait 1");
    let origin = session.position();
    let _ = session.handle("!wait 5");
    assert_ne!(
        session.position(),
        origin,
        "the held body's first seeking wait must move the column"
    );
    for _ in 0..6 {
        let _ = session.handle("!wait 5");
    }
    assert!(
        driven_facts_named(&session, "drank") >= 1,
        "PREREGISTERED PREDICTION (spec §4 P2): a walk that resumes reaches water \
         a walk that restarts could not; got 0 drank by day 36 — a red here is \
         the null finding, see the plan's Task 2 decision rule"
    );
}
```

Then register it: in `windows/vessel/tests/suite.rs` add `mod the_minute;` in alphabetical order among the existing `mod` lines.

- [ ] **Step 4: Run the integration tests, expect red**

Run: `cargo test -p hornvale-vessel --test suite -- the_minute`
Expected: both FAIL — P1 at "must have its drinks minuted: got 0", P2 at "must move the column". If P1's helper panics on JSON shape, print `session_ledger_json()`'s first 300 bytes and adjust the field names (`facts`, `subject`, `predicate`) to what the ledger actually serializes; the shape is `Ledger { facts: Vec<Fact>, next_entity }` with `Fact { subject, predicate, object, place, day, provenance }`.

- [ ] **Step 5: Commit (tests only; the pre-commit gate will run because a Rust path is staged)**

```bash
cargo fmt
git add windows/vessel/src/session.rs windows/vessel/tests/suite/the_minute.rs windows/vessel/tests/suite.rs
printf '%s\n' "test(the-minute): the red witnesses and the free-path control" "" "Replaces a_possessed_walk_ends_where_the_ledger_never_recorded (which pinned the defect) with its inverse, adds the free-walk control that licenses an unconditional commit, and P1/P2 over public API. Red by design until Task 2." "" "Claude-Session: https://claude.ai/code/session_01GY2VCS1s4Hp4ftr2TcZZqy" > /tmp/hv-minute-msg.txt
git commit -F /tmp/hv-minute-msg.txt -- windows/vessel/src/session.rs windows/vessel/tests/suite/the_minute.rs windows/vessel/tests/suite.rs
git push
```

Note: `make gate-commit` runs only the sub-floor roster, which does not contain these tests, so a red witness does not block the commit. That is the design (coverage is the stage gate's job).

---

### Task 2: Commit the driven walk's facts and write the driven slot

Spec §3.1, §3.2. This is the fix.

**Files:**
- Modify: `windows/vessel/src/session.rs` — `Session::wait`, the block from `let driven_npc = self.driven_body().clone();` through `self.roster.resolve(driven_slot, driven_written.felt);` (≈ 8510–8625).
- Modify: `windows/vessel/src/roster.rs` — delete `Roster::resolve` (≈ 288–305), inline its body into `write`; amend `write`'s doc.
- Modify: `windows/vessel/src/session.rs` — `driven_felt_state_can_move_under_an_imposed_controller_during_wait` (≈ 20228–20300): amend the doc and add a ledger assertion.
- Modify: `windows/vessel/tests/suite/the_rack.rs` — `a_possessed_sessions_columns_are_the_ledgers_too` (≈ 321–345): extend the script.

**Interfaces:**
- Consumes: `step_one_with_controller(...) -> (Vec<Fact>, Written)`; `Ledger::commit(Fact, &ConceptRegistry) -> Result<bool, LedgerError>`; `Roster::write(Slot, Facet, Felt)`.
- Produces: the local `driven_facts: Vec<Fact>` (no longer underscored), consumed by Tasks 3 and 4 in the same function.

- [ ] **Step 1: Rename the binding and commit the facts**

In `Session::wait`, change `let (_driven_facts, driven_written) = sys.step_one_with_controller(` to `let (driven_facts, driven_written) = sys.step_one_with_controller(`. Then, directly AFTER the population commit loop (the `for fact in facts { match self.ledger.commit(fact, &self.registry) { … } }` block) and BEFORE `self.occupancy = occupancy;`, insert:

```rust
        // THE MINUTE (spec §3.1): the driven body's own walk is committed,
        // UNCONDITIONALLY on the controller. Free, the walk was asked through
        // a `PlayerController` with nothing queued, whose intent is `Hold`,
        // and a Holding walk emits nothing — pinned by
        // `a_free_walk_emits_nothing_and_ends_in_the_column` — so this loop
        // is a no-op for every free session and every committed fixture.
        // Held, the `ImposedController` acts, and what it did now reaches the
        // ledger through the same constructors a creature's walk uses
        // (decision 0168): the drink it took, the room it reached.
        //
        // AFTER the population's facts and BEFORE the First Mark's
        // `turned-hostile` loop, every tick — a determinism contract from the
        // day it landed (spec §3.1), not a preference. Same failure shape as
        // the loop above: an error leaves the facts before it in place and
        // ends the turn.
        //
        // Before this campaign the binding was `_driven_facts` and this loop
        // did not exist; the walk's drinks were discarded and a held body's
        // ledger thirst grew monotonically while its felt state read
        // `Content` (spec §1, measured).
        let driven_minuted = driven_facts.len();
        for fact in driven_facts {
            match self.ledger.commit(fact, &self.registry) {
                Ok(true) | Ok(false) => {}
                Err(e) => return Turn::Out(format!("Time falters: {e}")),
            }
        }
        let _ = driven_minuted; // Task 4 reads this for the wait line.
```

(`driven_minuted` is replaced by the `minutes_of` summary in Task 4; leaving the count bound now keeps the compiler quiet without a warning suppression. If clippy objects to the `let _`, delete both lines — nothing in this task depends on the count.)

- [ ] **Step 2: Write the driven slot's position and felt together**

Replace the block that ends the driven-walk handling:

```rust
        let driven_slot = self.roster.driven();
        self.roster.resolve(driven_slot, driven_written.felt);
```

with:

```rust
        // The driven body's own walk — position AND felt, through `write`,
        // because its facts were committed a few lines above (The Minute,
        // spec §3.2). `driven_written.position` is the walk's own `st.pos`,
        // and every move that advanced it emitted an `agent-at` the loop
        // just committed, so the column and `agent_position(&ledger)` agree
        // by construction — `the_rack.rs::a_possessed_sessions_columns_are_
        // the_ledgers_too` holds them to it. Not in `written` above:
        // `on_roll_others` excludes the driven slot by construction, so this
        // is that slot's only writer on a tick.
        //
        // Before this campaign this was `resolve` (felt only), because the
        // position was a view of a walk the ledger never heard about; The
        // Rack found that writing it broke VIEW ≡ SCAN at seed 7. Now the
        // ledger has heard, and the felt-only write would be the lie.
        let driven_slot = self.roster.driven();
        self.roster
            .write(driven_slot, driven_written.position, driven_written.felt);
```

Also rewrite the two long comments above the `step_one_with_controller` call in `wait` — the one beginning "**What actually keeps the LEDGER clean either way is the next line**" and the one beginning "**The ledger is inert to this swap; the driven slot's `felt` is NOT**" — so they no longer describe a discard. Keep them short: the controller choice paragraph stays (it is still true); replace the ledger paragraphs with one that says the facts are committed below (spec §3.1) and that the felt-state observation The Coercion made still holds and is now one of two consequences rather than the only one.

- [ ] **Step 3: Delete `Roster::resolve`; amend `write`**

In `windows/vessel/src/roster.rs` delete the `resolve` method and its doc (the paragraph beginning "Write one slot's `felt` column and NOTHING else"), and change `write` to:

```rust
    /// Write one slot's tick-owned columns — the position the tick left the
    /// body at, and the felt state its resolution expressed.
    ///
    /// **For a walk whose facts were COMMITTED, which is now every walk the
    /// tick runs** (The Minute, spec §3.2). This is [`Self::place`] plus the
    /// felt write, and the pairing is honest because the ledger was told
    /// about the move: the population's facts and the driven body's own are
    /// both committed by `Session::wait` before it calls this, so the
    /// position written here is one `agent_position` will agree with. There
    /// used to be a felt-only `resolve` beside this for the driven body,
    /// whose walk facts `wait` discarded; that discard was the defect The
    /// Minute repaired, and the method went with it.
    ///
    /// Static columns (`bodies`, `keys`) are deliberately not writable: a
    /// body's identity, home and roll key are settled at derivation.
    ///
    /// # Panics
    ///
    /// If `slot` is not a slot of this roster — a `Slot` can only come from
    /// [`Self::push`] or [`Self::slot_of`], so an out-of-range one is a
    /// caller mixing two rosters, which has no honest recovery.
    pub fn write(&mut self, slot: Slot, position: Facet, felt: Felt) {
        self.place(slot, position);
        self.felt[slot.0] = felt;
        self.written[slot.0] = true;
    }
```

Grep for any remaining `resolve(` caller or doc reference in `windows/vessel/` (`grep -rn "\.resolve(\|Roster::resolve\|\`resolve\`" windows/vessel/src windows/vessel/tests`) and repair each mention: the `place` doc's sentence "The driven slot's `position` moves only through `place`, from `Session::commit_agent_at`" becomes "…through `place` (from `Session::commit_agent_at`) and through `write` (from the tick)"; `the_rack.rs`'s test docs that name the `resolve` mutation keep their history but say the method is gone.

- [ ] **Step 4: Run the Task 1 tests and the Rack's possessed sweep**

Run: `cargo test -p hornvale-vessel --lib -- a_possessed_walk_ends_where_the_ledger_recorded a_free_walk_emits_nothing_and_ends_in_the_column`
Expected: both PASS.

Run: `cargo test -p hornvale-vessel --test suite -- the_minute the_rack`
Expected: `p1_…` PASS. `p2_…`: the first assertion PASSES. If the `drank >= 1` assertion is RED: that is the preregistered null, not a bug. Decision rule: do NOT retune anything; change the assertion to record what was measured (`assert_eq!(driven_facts_named(&session, "drank"), N, "…the null: …")`), record the count and the body's `driven_affect()` at day 36 in the test doc as the finding, and report it in the task summary so the chronicle carries it. `the_rack` tests all PASS.

- [ ] **Step 5: Extend P3's sweep and the Coercion's felt-state test**

In `the_rack.rs::a_possessed_sessions_columns_are_the_ledgers_too`, replace `for verb in ["!wait 1", "!wait 5", "!wait 30"]` with `for verb in ["!wait 1", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 30"]` and add to its doc: "The Minute extended the script to the eight-wait shape its P2 measures, so the column is checked after every accumulating step of the walk, not just three."

In `session.rs::driven_felt_state_can_move_under_an_imposed_controller_during_wait`, delete the doc paragraph beginning "**The ledger stays untouched by this test's own construction**" and replace it with: "**The ledger moves too, since The Minute:** the held walk's first tick at seed 42 emits a `slept`, and it is committed, so the held session carries one more fact than the free one. Asserted below; it is the smallest ledger consequence of the swap and the one this seed can show." Then append to the test body:

```rust
        assert_eq!(
            held.committed_fact_count_for(held.agent_entity()),
            free.committed_fact_count_for(free.agent_entity()) + 1,
            "seed 42's held body sleeps on its first wait and the sleep is minuted; \
             the free body, Holding, commits nothing"
        );
```

Run: `cargo test -p hornvale-vessel --lib -- driven_felt_state_can_move_under_an_imposed_controller_during_wait` and `cargo test -p hornvale-vessel --test suite -- a_possessed_sessions_columns_are_the_ledgers_too`. Expected: PASS. If the `+ 1` is wrong, the walk emitted more than `slept` on wait#0 at seed 42; the 2026-09-03 probe recorded exactly one (`slept`). Print both counts, correct the literal to the measured difference, and say so in the doc.

- [ ] **Step 6: Mutation check (must fail against the fix)**

Temporarily restore `self.roster.write(driven_slot, driven_written.position, driven_written.felt)` to write `driven_written.felt` only through `place`-less code — concretely, replace the `write` call with `self.roster.place(driven_slot, self.position_of(driven_slot)); self.roster.write(driven_slot, self.position_of(driven_slot), driven_written.felt);` is NOT a mutation (it is still correct). The mutation that matters is skipping the commit loop: comment out the `for fact in driven_facts { … }` body so nothing commits. Run `cargo test -p hornvale-vessel --lib -- a_possessed_walk_ends_where_the_ledger_recorded`. Expected: FAIL at "every fact the driven walk emitted must have been appended". Restore the loop. Confirm the restore with `git diff --stat` showing only intended files.

- [ ] **Step 7: Rebaseline and apply the drift decision rule**

Run: `make rebaseline` then `git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`.
Decision rule (Global Constraints): only `docs/audits/` moved → stage it with this commit; anything under `clients/game/core/tests/fixtures/`, `windows/vessel/tests/fixtures/`, or `book/src/gallery/` moved → STOP, do not commit, report the diff — the free path is not inert and spec §3.1's premise failed; nothing moved → expected (no `pub` surface changed).

- [ ] **Step 8: Commit**

```bash
cargo fmt
git add -u windows/vessel docs/audits
printf '%s\n' "feat(the-minute): commit the driven walk's facts and write the driven slot" "" "Session::wait no longer discards step_one_with_controller's facts: they commit after the population's, and the driven slot is written through Roster::write (position and felt). Roster::resolve is deleted with its only caller. A free body's Holding walk emits nothing, so every free-session fixture is byte-identical (a_free_walk_emits_nothing_and_ends_in_the_column). P1 green at seed 42; P2 at seed 7 <state the drank count and whether the prediction held>." "" "Claude-Session: https://claude.ai/code/session_01GY2VCS1s4Hp4ftr2TcZZqy" > /tmp/hv-minute-msg.txt
git commit -F /tmp/hv-minute-msg.txt
git push
```

Edit the message's angle-bracket clause to the measured result before committing.

---

### Task 3: Off the walk band a held body holds; a walk-committed sleep sets `wake_at`

Spec §3.3, §3.5 (as amended: `body_state` reads `Session.wake_at`, a field the `sleep` verb sets — it does not fold `slept` facts, so a walk-committed sleep must set it the same way).

**Files:**
- Modify: `windows/vessel/src/session.rs` — the controller choice in `wait` (≈ 8512–8518); a new private fn `wake_after`; a `wake_at` update after the driven commit loop; tests in the in-module test module.

**Interfaces:**
- Produces: `fn wake_after(facts: &[Fact], now: WorldTime) -> Option<WorldTime>`.
- Consumes: `renders_unconscious(&Action) -> bool` (already imported), `SLEPT`, `TickSpan::from_ticks`, `Value::Number`.

- [ ] **Step 1: Unit tests for `wake_after` (red: the function does not exist)**

Append to `session.rs`'s test module:

```rust
    fn slept_at(day: i64, span_ticks: i64) -> Fact {
        Fact {
            subject: EntityId(std::num::NonZeroU64::new(7).unwrap()),
            predicate: SLEPT.to_string(),
            object: Value::Number(span_ticks as f64),
            place: None,
            day: Some(WorldTime { ticks: day }),
            provenance: "test".to_string(),
        }
    }

    /// The Minute, spec §3.5: a walk-committed sleep that ends AFTER the
    /// tick's end leaves the body asleep, exactly as the `sleep` verb's
    /// own `wake_at` does — `body_state` reads the field, not the ledger.
    #[test]
    fn a_walk_sleep_that_outlasts_the_tick_sets_the_wake() {
        let now = WorldTime::from_ticks(1_000_000);
        let ends_later = slept_at(900_000, 250_000); // wakes at 1_150_000
        assert_eq!(
            wake_after(&[ends_later], now),
            Some(WorldTime::from_ticks(1_150_000))
        );
    }

    #[test]
    fn a_walk_sleep_already_over_sets_no_wake() {
        let now = WorldTime::from_ticks(1_000_000);
        let over = slept_at(500_000, 100_000); // woke at 600_000
        assert_eq!(wake_after(&[over], now), None);
    }

    #[test]
    fn a_rest_is_not_a_sleep_for_the_wake() {
        let now = WorldTime::from_ticks(1_000_000);
        let mut rest = slept_at(900_000, 250_000);
        rest.predicate = RESTED.to_string();
        assert_eq!(wake_after(&[rest], now), None);
    }

    #[test]
    fn the_latest_outlasting_sleep_wins() {
        let now = WorldTime::from_ticks(1_000_000);
        let a = slept_at(900_000, 150_000); // 1_050_000
        let b = slept_at(950_000, 300_000); // 1_250_000
        assert_eq!(
            wake_after(&[a, b], now),
            Some(WorldTime::from_ticks(1_250_000))
        );
    }
```

Run: `cargo test -p hornvale-vessel --lib -- wake_after a_walk_sleep the_latest_outlasting_sleep_wins a_rest_is_not_a_sleep`. Expected: compile error, `wake_after` not found. (`WorldTime`'s `ticks` field is private; `WorldTime::from_ticks(i64)` in `kernel/src/field.rs` is the constructor, verified.)

- [ ] **Step 2: Implement `wake_after` and wire it**

Add near `Session::sleep` (a free function, private):

```rust
/// When a tick's committed facts leave the body asleep past `now`: the
/// latest end of any `slept` fact among them that ends after `now`, else
/// `None` (The Minute, spec §3.5).
///
/// `body_state` reads `Session::wake_at`, a field the `sleep` VERB sets
/// from the span it committed — it does not fold `slept` facts. A held
/// body's walk commits its own `slept` (decision 0168: the sleep is the
/// body's whoever chose it), and the walk may sleep past the tick's end
/// (`advance_one` advances `st.day` by the span and stops when it passes
/// `to`), so the field must follow the same rule the verb applies or a
/// released body would be awake at the gate while its ledger says asleep.
/// Pure over the facts, so the verb and the tick cannot disagree.
fn wake_after(facts: &[Fact], now: WorldTime) -> Option<WorldTime> {
    facts
        .iter()
        .filter(|f| f.predicate == SLEPT)
        .filter_map(|f| {
            let Value::Number(ticks) = f.object else {
                return None;
            };
            let start = f.day?;
            Some(start + TickSpan::from_ticks(ticks as i64))
        })
        .filter(|end| *end > now)
        .max()
}
```

In `wait`, `driven_facts` is consumed by the commit loop; compute the wake BEFORE the loop:

```rust
        let woke = if renders_unconscious(&Action::Sleep) {
            wake_after(&driven_facts, self.day)
        } else {
            None
        };
```

and AFTER the loop:

```rust
        if let Some(wake) = woke {
            // The same rule `Session::sleep` applies to the verb's own span.
            self.wake_at = Some(match self.wake_at {
                Some(current) if current > wake => current,
                _ => wake,
            });
        }
```

Run the four unit tests. Expected: PASS.

- [ ] **Step 3: Off the walk band, the walk is asked through the Holding controller**

Replace the controller choice in `wait`:

```rust
        let driven_controller: &mut dyn Controller = if self.possessor().is_some() {
            &mut imposed_controller
        } else {
            &mut player_controller
        };
```

with:

```rust
        // OFF THE WALK BAND A HELD BODY HOLDS (The Minute, spec §3.3).
        // `inside`, `submerged` and `underground` are session-only frames:
        // the body's ledger position stays at the walk band throughout a
        // descent, and `out`/`surface`/`climb` return the player to the room
        // the frame was entered from. The creature walk has no model of a
        // lattice, a chamber index or a stratum, so a mesh move it committed
        // while a frame is open would strand the frame — the frame naming a
        // house the body no longer stands at. So while a frame is open the
        // held body's walk is asked through the Holding `PlayerController`:
        // arbitration still runs and the felt state is still written (the
        // co-present host, decision 0226), but nothing commits. The cost —
        // a held body indoors does not drink on its own — is a fidelity cut
        // recorded on `PLAY-held-body-off-the-band-holds`, and it is honest
        // where the old discard was not: the felt state agrees with the
        // ledger.
        let off_the_band =
            self.inside.is_some() || self.submerged.is_some() || self.underground.is_some();
        let driven_controller: &mut dyn Controller =
            if self.possessor().is_some() && !off_the_band {
                &mut imposed_controller
            } else {
                &mut player_controller
            };
```

- [ ] **Step 4: The off-band test (P5)**

Append to the in-module test module. `world_at(14)` is the seed-14 world helper the custody tests already use (`fn world_at(seed: u64) -> Option<World>` in the same module); `enter` from the flagship's home room steps into its dwelling — copy the exact `enter` line the custody tests use if it differs.

```rust
    /// The Minute, spec §3.3 / §4 P5: a held body INDOORS commits nothing
    /// during `wait` and its frame survives. The positive control is
    /// `the_minute.rs::p1_…`, which holds the same body's outdoor walk to
    /// committing on the very same mechanism; this is its opposite arm.
    ///
    /// `enter` FIRST — it is in-character and would refuse once held.
    #[test]
    fn a_held_body_indoors_holds_and_keeps_its_frame() {
        let world = world_at(14).expect("seed 14 builds");
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let _ = session.handle("enter");
        assert!(session.inside.is_some(), "the premise: the body is indoors");
        let _ = session.handle("!possess");
        assert!(session.possessor().is_some(), "possession must be open");
        let before = session.committed_fact_count_for(session.agent_entity());
        let column = session.position();
        let _ = session.handle("!wait 5");
        assert_eq!(
            session.committed_fact_count_for(session.agent_entity()),
            before,
            "off the walk band the held walk Holds and commits nothing"
        );
        assert!(session.inside.is_some(), "the frame survives the wait");
        assert_eq!(session.position(), column, "and the column did not move");
        assert!(
            session.roster.resolved_felt(session.roster.driven()).is_some(),
            "arbitration still ran: the felt state was written (co-present, 0226)"
        );
    }
```

Run: `cargo test -p hornvale-vessel --lib -- a_held_body_indoors_holds_and_keeps_its_frame`. Expected: PASS. If `enter` does not put seed 14's body indoors from its starting room, read how `session.rs`'s custody tests reach the dwelling (they use `enter` and sometimes `enter further in`) and copy that prefix exactly; state which in the test doc.

Mutation: temporarily drop `&& !off_the_band`; rerun; expected FAIL at "commits nothing" (the held walk emits at least a `slept` in 5 days at seed 14 — if it does not, the mutation is null: switch the test to `!wait 10`, and if still null, record in the test doc that the P5 negative arm at seed 14 is vacuous over 10 days and rely on P1 as the control). Restore.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo test -p hornvale-vessel --lib -- wake_after a_walk_sleep the_latest_outlasting_sleep_wins a_rest_is_not_a_sleep a_held_body_indoors_holds_and_keeps_its_frame a_possessed_walk_ends_where_the_ledger_recorded a_free_walk_emits_nothing_and_ends_in_the_column
git add -u windows/vessel
printf '%s\n' "feat(the-minute): off the walk band a held body holds; a walk sleep sets wake_at" "" "The frames (inside, submerged, underground) are player-only session state the creature walk has no model of, so while one is open the held body's walk is asked through the Holding PlayerController. A walk-committed slept that outlasts the tick sets wake_at by the same rule Session::sleep applies, since body_state reads the field and not the ledger." "" "Claude-Session: https://claude.ai/code/session_01GY2VCS1s4Hp4ftr2TcZZqy" > /tmp/hv-minute-msg.txt
git commit -F /tmp/hv-minute-msg.txt
git push
```

---

### Task 4: The wait line minutes what the body did

Spec §3.4, §4 P7.

**Files:**
- Modify: `windows/vessel/src/session.rs` — new private `enum Minute` + `fn minutes_of`; `narrate_motion` signature and body; the `wait` call site (`Turn::Out(self.narrate_motion(moved, &before, &sensed_before, how))`) and the capture of the driven body's before-position.
- Modify: `windows/vessel/tests/suite/the_minute.rs` — P7.

**Interfaces:**
- Produces: `enum Minute { Moved, Drank, Eaten, Rested }`, `fn minutes_of(facts: &[Fact]) -> Vec<Minute>`; `narrate_motion(&self, moved, before, sensed_before, how, driven_before: &Facet, minutes: &[Minute]) -> String`.

- [ ] **Step 1: Unit test for `minutes_of` (red)**

```rust
    fn fact_named(predicate: &str) -> Fact {
        Fact {
            subject: EntityId(std::num::NonZeroU64::new(7).unwrap()),
            predicate: predicate.to_string(),
            object: Value::Number(0.0),
            place: None,
            day: Some(WorldTime::from_ticks(0)),
            provenance: "test".to_string(),
        }
    }

    /// The Minute, spec §3.4: one clause per predicate present, in first
    /// appearance order, `rested` and `slept` folded into one; `agent-at`
    /// is the move, named first regardless of where it appeared.
    #[test]
    fn minutes_are_one_per_kind_in_first_appearance_order_with_the_move_first() {
        let facts = [
            fact_named(RESTED),
            fact_named(DRANK),
            fact_named(AGENT_AT),
            fact_named(SLEPT),
            fact_named(DRANK),
            fact_named(EATEN),
        ];
        assert_eq!(
            minutes_of(&facts),
            vec![Minute::Moved, Minute::Rested, Minute::Drank, Minute::Eaten]
        );
        assert!(minutes_of(&[]).is_empty());
    }
```

Run: `cargo test -p hornvale-vessel --lib -- minutes_are_one_per_kind`. Expected: compile error.

- [ ] **Step 2: Implement `Minute`, `minutes_of`, and the narration**

Near `narrate_motion`:

```rust
/// One thing the driven body's own walk did this tick, for the wait line
/// (The Minute, spec §3.4). `Moved` is `agent-at`; `Rested` covers both
/// `rested` and `slept`, one clause.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Minute {
    Moved,
    Drank,
    Eaten,
    Rested,
}

/// The minutes of the driven walk: one entry per kind present among
/// `facts`, `Moved` first if present, the rest in the order their kind
/// first appeared. Pure; unit-tested.
fn minutes_of(facts: &[Fact]) -> Vec<Minute> {
    let mut out: Vec<Minute> = Vec::new();
    let mut moved = false;
    for fact in facts {
        let kind = match fact.predicate.as_str() {
            AGENT_AT => {
                moved = true;
                continue;
            }
            DRANK => Minute::Drank,
            EATEN => Minute::Eaten,
            RESTED | SLEPT => Minute::Rested,
            _ => continue,
        };
        if !out.contains(&kind) {
            out.push(kind);
        }
    }
    if moved {
        out.insert(0, Minute::Moved);
    }
    out
}
```

Change `narrate_motion`'s signature to add `driven_before: &Facet, minutes: &[Minute]` and, at the top of its body (before the `if moved == 0` early return), add:

```rust
        // THE MINUTE (spec §3.4). The held body's own acts are named before
        // the population's comings and goings, and a room change SUPPRESSES
        // the arrival/departure comparison: `before` was copied in the room
        // the body has since left, so comparing it against `here` would
        // narrate everyone in the old room as gone and everyone in the new
        // one as arrived. `!look` answers for the new room. The act is
        // attributed to the possessor's will — under decision 0168 it is the
        // body's act and under 0226 the choice was not the player's. A free
        // body has no minutes (its walk Holds), so its line is byte-identical
        // to the line before this campaign.
        let here_now = &self.roster.positions()[self.roster.driven().0];
        let minute_line = minute_sentence(minutes);
        if here_now != driven_before {
            return match minute_line {
                Some(line) => format!("Time passes. {line}"),
                None => "Time passes. The will that holds you walks this body elsewhere.".to_string(),
            };
        }
        let minute_prefix = minute_line.map(|l| format!(" {l}")).unwrap_or_default();
```

and thread `minute_prefix` into the two existing returns and the early return:

```rust
        if moved == 0 {
            return format!("Time passes; the world keeps its shape.{minute_prefix}");
        }
        …
        if parts.is_empty() {
            format!("Time passes. You sense movement nearby ({moved} stirred).{minute_prefix}")
        } else {
            format!("Time passes. {}{minute_prefix}", parts.join(" "))
        }
```

Add the sentence builder beside `minutes_of`:

```rust
/// The sentence for a tick's minutes, or `None` when there are none.
/// "The will that holds you walks this body elsewhere, drinks and rests."
fn minute_sentence(minutes: &[Minute]) -> Option<String> {
    if minutes.is_empty() {
        return None;
    }
    let clauses: Vec<&str> = minutes
        .iter()
        .map(|m| match m {
            Minute::Moved => "walks this body elsewhere",
            Minute::Drank => "drinks",
            Minute::Eaten => "eats",
            Minute::Rested => "rests",
        })
        .collect();
    let joined = match clauses.len() {
        1 => clauses[0].to_string(),
        n => format!("{} and {}", clauses[..n - 1].join(", "), clauses[n - 1]),
    };
    Some(format!("The will that holds you {joined}."))
}
```

In `wait`: capture `let driven_before = self.roster.positions()[self.roster.driven().0].clone();` beside the existing `before` copy (before the tick); compute `let minutes = minutes_of(&driven_facts);` BEFORE the driven commit loop consumes `driven_facts` (replace the `driven_minuted` placeholder from Task 2); and change the final line to `Turn::Out(self.narrate_motion(moved, &before, &sensed_before, how, &driven_before, &minutes))`.

Run: `cargo test -p hornvale-vessel --lib -- minutes_are_one_per_kind narrate_motion`. Expected: PASS (existing `narrate_motion` tests are unchanged in outcome: a free body passes an empty `minutes` and an unchanged `driven_before`; update their call sites to pass `&session.position()` and `&[]` if any call `narrate_motion` directly — grep `narrate_motion(` in the test module).

- [ ] **Step 3: P7 integration test (red until the wording lands)**

Append to `the_minute.rs`:

```rust
/// P7 — the line. Seed 7's first seeking wait names the move and does not
/// count the body among the stirred; seed 42's second wait names the drink;
/// a free body's line carries no minutes.
#[test]
fn p7_the_wait_line_minutes_the_held_bodys_acts() {
    let (world, ()) = possessed(7);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    let _ = session.handle("!wait 1");
    let hornvale_vessel::Turn::Out(line) = session.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        line.contains("walks this body elsewhere"),
        "seed 7's first seeking wait must name the move: {line:?}"
    );
    assert!(
        !line.contains("stirred") && !line.contains("You watch") && !line.contains("You notice"),
        "a room change suppresses the arrival/departure comparison: {line:?}"
    );

    let (world, ()) = possessed(42);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    let _ = session.handle("!wait 5");
    let hornvale_vessel::Turn::Out(line) = session.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        line.contains("drinks"),
        "seed 42's second wait must name the drink: {line:?}"
    );

    let (mut free, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = free.handle("!wait 5");
    let hornvale_vessel::Turn::Out(line) = free.handle("!wait 5") else {
        panic!("wait narrates")
    };
    assert!(
        !line.contains("The will that holds you"),
        "a free body has no minutes: {line:?}"
    );
}
```

Run: `cargo test -p hornvale-vessel --test suite -- the_minute`. Expected: all PASS. If seed 7's line lacks the move, the walk did not move on that tick — P2 asserts it does, so both would be red together; read P2's result first.

- [ ] **Step 4: Rebaseline and check the gallery transcripts are byte-identical**

Run: `make rebaseline` then `git status --short -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`. Decision rule as in Global Constraints. The gallery transcripts (`book/src/gallery/`, generated from `scripts/possession-*.txt`, none of which possess) are the surface this task could move if a free body's line changed — nothing may move.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -u windows/vessel docs/audits
printf '%s\n' "feat(the-minute): the wait line minutes the held body's acts" "" "narrate_motion names what the driven walk committed — the move first, then one clause per kind — and a room change suppresses the arrival/departure comparison whose before-copy was taken in a room the body has left. A free body's line is byte-identical: its walk Holds and has no minutes." "" "Claude-Session: https://claude.ai/code/session_01GY2VCS1s4Hp4ftr2TcZZqy" > /tmp/hv-minute-msg.txt
git commit -F /tmp/hv-minute-msg.txt
git push
```

---

### Task 5: Doc freshness in code, the commit gate, the stage gate

Spec §6 (the code half of the freshness sweep). Prose only in `src/`; then the campaign's one plan-stage boundary.

**Files:**
- Modify: `windows/vessel/src/controller.rs` — `PlayerController` doc (≈ 56–75) and `ImposedController` doc (≈ 113–142).
- Modify: `windows/vessel/src/liveness.rs` — `step_one_with_controller` doc, the paragraph beginning "Returns the facts this body's OWN walk would commit".
- Modify: `windows/vessel/src/session.rs` — any remaining comment that says the driven walk's facts are discarded (`grep -n "discard" windows/vessel/src/session.rs | grep -i "driven\|walk\|facts"`), including the `wait` header comment and the `Roster::place` cross-reference.

- [ ] **Step 1: Rewrite the three docs**

`PlayerController`: delete the paragraph beginning "**This is NOT what keeps the driven body's own walk out of the ledger**" through "…spec §1 'Does not ship: the host speaking')." and replace with:

```rust
/// What this controller guarantees is narrow and real: a driven body's own
/// walk (`Session::wait` constructs a fresh one every tick) never
/// autonomously acts on its own drives, because its intent is
/// unconditionally `Hold` until a verb routes a real action through
/// [`queue`](Self::queue) — and a Holding walk emits no facts
/// (`a_free_walk_emits_nothing_and_ends_in_the_column`). Since The Minute
/// `Session::wait` COMMITS whatever the driven walk returns, so the moment
/// something queues an action here, that act reaches the ledger through the
/// same path a creature's does; nothing queues one yet (today's in-character
/// verbs — `go`, `sleep`, … — still commit directly; there is no `drink`
/// verb at all, see `PLAY-free-body-cannot-drink`).
```

`ImposedController`: replace the sentence "**The ledger stays untouched either way** — `Session::wait` discards that walk's facts unconditionally, so no committed fact ever differs — but the body's OWN felt-state read … is not similarly inert:" with "**Both the ledger and the felt state move** (The Minute repaired the half that did not): `Session::wait` commits that walk's facts, so the held body's drinks, meals, rests and moves are on the record through the same constructors a creature's are (decision 0168), and the body's own felt-state read (`Session::driven_mode`/`driven_affect`/`driven_suppressed`) is read back from the last decision point of that walk, so". Keep the rest of the paragraph.

`step_one_with_controller`: change "Returns the facts this body's OWN walk would commit (empty under [`crate::controller::PlayerController`] with nothing queued — see that controller's own doc for why nothing here ever double-moves a body the player drives through the verb loop)" to "Returns the facts this body's OWN walk committed — `Session::wait` commits them since The Minute (empty under [`crate::controller::PlayerController`] with nothing queued, which is why a free body's session is unchanged by that commit)".

- [ ] **Step 2: Grep for stragglers and fix each**

Run: `grep -rn "discard" windows/vessel/src/session.rs windows/vessel/src/roster.rs windows/vessel/src/controller.rs windows/vessel/src/liveness.rs | grep -i "driven\|walk\|_driven_facts\|ledger-inert"`. Every hit that describes the discard as current gets rewritten to past tense with "(The Minute)". Hits about the felt column's "discarded ranks" (suppressed drives) are a different sense and stay.

- [ ] **Step 3: Commit gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -u windows/vessel docs/audits
printf '%s\n' "docs(the-minute): the discard is history in every doc that described it" "" "Claude-Session: https://claude.ai/code/session_01GY2VCS1s4Hp4ftr2TcZZqy" > /tmp/hv-minute-msg.txt
git commit -F /tmp/hv-minute-msg.txt
git push
```

- [ ] **Step 4: The stage gate (the plan's one stage boundary)**

Absorb main first: `git fetch origin && git merge origin/main` (resolve any conflict; a conflicted merge runs the commit gate through `pre-commit`). Push. Then submit `make sluice-stage BRANCH=campaign/the-minute REF=$(git rev-parse HEAD)` and read the result with `make sluice-status` / `make sluice-log`. Green is required before Task 6; a red names the test — fix it in a fix-round commit and resubmit. Record the stage gate's SHA and verdict in the ledger.

---

### Task 6: Close — decisions, registry, chronicle, retrospective, merge

Spec §5, §6. Follow `closing-a-campaign` for the merge; this task lists the artifacts.

**Files:**
- Create: `docs/decisions/0656-a-held-bodys-walk-commits-what-it-does.md`, `docs/decisions/0657-off-the-walk-band-a-held-body-holds.md`, `docs/decisions/0658-the-wait-line-minutes-the-held-bodys-acts.md`; append three rows to the table in `docs/decisions/README.md` (format: `| [0656](0656-….md) | A held body's walk commits what it does, unconditionally on the controller | Accepted |`).
- Create: `book/src/chronicle/the-minute.md`; add `- [The Minute](./chronicle/the-minute.md)` after The Rack's line in `book/src/SUMMARY.md`.
- Modify: `book/src/chronicle/the-coercion.md` (§"What the ledger cannot tell", the paragraph "Wiring it there is ledger-inert…"), `book/src/chronicle/the-rack.md` (≈ line 197 and the "still discarded" paragraph ≈ 257), `book/src/frontier/idea-registry.md` (`PLAY-imposed-controller-diverges-felt-state` Where cell: "closed by The Minute"; the two raw rows' Where cells gain the chronicle link).
- Create: `docs/retrospectives/the-minute.md`.
- Modify: `docs/superpowers/ledgers/2026-09-03-the-minute.md` — task sections and the followups promoted.

- [ ] **Step 1: The three decision records**

Each in the house format (`# NNNN. Title`, `**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Relates:** …`, "In the context of … we decided that … accepting that …", `## Context`, `## Consequences`). Substance:

- 0656: a held body's walk commits what it does, and the commit is unconditional on the controller — relates 0168, 0226, 0228; context is spec §1's measurement; consequence: a Holding walk emitting nothing is now load-bearing and pinned.
- 0657: off the walk band a held body holds — relates 0226 and the frames' docs; consequence: `PLAY-held-body-off-the-band-holds`.
- 0658: the wait line minutes the held body's acts, attributed to the possessor's will, and a room change suppresses the arrival/departure comparison — relates 0168, `PLAY-host-is-a-narrator`.

- [ ] **Step 2: Chronicle, freshness sweep, registry, retrospective**

Chronicle: written at the book's altitude (technical, comprehensible without the code), carrying spec §1's two tables, P1/P2's post-fix readings (state whether P2's prediction held, and the null if it did not), the off-band cut and why, and what stays open (a frames-aware walk; a free body's `drink`). Freshness: The Coercion's "ledger-inert" half is corrected in place with a dated note, not deleted; The Rack's parked-finding paragraph points here. Registry: the divergence row's status stays `elaborated`; its Where cell says "closed by The Minute" with the chronicle link; the two raw rows link the chronicle. Retrospective: process, not product — at minimum, the spec's §3.5 claim that the gate would read `Asleep` "without further work", written from reasoning about a fold that turned out to be a session field (ledger #6), and how the plan caught it.

- [ ] **Step 3: Regenerate the aggregates and commit**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

Expected: `docs/digest/` moves (three new decisions); nothing else. Then:

```bash
git add -u; git add docs/decisions book/src/chronicle/the-minute.md docs/retrospectives/the-minute.md
printf '%s\n' "close(the-minute): decisions 0656-0658, chronicle, retrospective, freshness sweep" "" "Claude-Session: https://claude.ai/code/session_01GY2VCS1s4Hp4ftr2TcZZqy" > /tmp/hv-minute-msg.txt
git commit -F /tmp/hv-minute-msg.txt
git push
```

- [ ] **Step 4: Merge**

Invoke `closing-a-campaign`: absorb main, regenerate, the G6 ledger digest for Nathan, then `make sluice BRANCH=campaign/the-minute REF=<full-sha>` with the authored `Sluice-Headline` trailer, and the census queued at close only if a world artifact moved (none is expected: this is a session-only change).

---

## Self-review

- **Spec coverage:** §3.1 → Task 2; §3.2 → Task 2 (and `resolve` deletion, spec amended); §3.3 → Task 3; §3.4 → Task 4; §3.5 → Task 3 (`wake_at`, spec amended) and the `Occupancy` followup stays a followup; §4 P1/P2 → Task 1/2, P3 → Task 2 step 5, P4 → Task 1's control + every task's rebaseline rule, P5 → Task 3, P6 → untouched tests (verified green by the stage gate), P7 → Task 4; §5 capture → Task 6; §6 DoD → Tasks 5–6.
- **Placeholders:** none; every step carries code or an exact command. Two measured literals (`+ 1` at seed 42's first wait; `>= 7` drinks) carry their decision rule if the tree disagrees.
- **Type consistency:** `wake_after(&[Fact], WorldTime) -> Option<WorldTime>`; `minutes_of(&[Fact]) -> Vec<Minute>`; `minute_sentence(&[Minute]) -> Option<String>`; `narrate_motion(…, driven_before: &Facet, minutes: &[Minute])`; `driven_facts_named(&Session, &str) -> usize`. `WorldTime::from_ticks(i64)` is the constructor used in tests (`ticks` is private; verified in `kernel/src/field.rs:49-73`).
