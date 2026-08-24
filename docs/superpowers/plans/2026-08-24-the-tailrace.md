# The Tailrace Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the incremental-ledger-fold primitive — a value that advances with the log rather than being invalidated by it — plus the two instruments that size and verify the problem it solves.

**Architecture:** A new kernel module, `kernel/src/fold.rs`, defines a `LedgerFold` trait (the state IS the fold: `empty()` plus `absorb(&mut self, &Fact)`) and a `Folded<S>` holder carrying the state alongside the ledger position it is valid at. The tick's O(1) door is `absorb_at(position, fact)`, which asserts the caller's position matches — making both double-absorb and skip loud rather than silent. `rebuild`/`rebuild_upto`/`resume` are the verification and checkpoint doors. `Ledger` is not modified: the tick already holds the facts before it appends them, so the caller advances the fold explicitly.

**Tech Stack:** Rust edition 2024, `hornvale-kernel`, `cargo nextest`, `samply` for the profile. No new dependencies — the workspace allowlist is `serde`, `serde_json`, `libm` only.

**Spec:** `docs/superpowers/specs/2026-08-24-the-tailrace-design.md` (G3-approved 2026-08-24)

## Global Constraints

- **Scope is stages 1–2 only.** The spec's stages 3–5 rewrite `windows/vessel/src/liveness.rs`, which `campaign/the-escapement` holds off with an unmerged `WorldTime` epoch (f64 fractional days to i64 ticks) touching the same functions. They get a **second plan**, written after that lands, because every `f64` day in their code would be wrong today.
- **Nothing here enters the save.** Serializing a fold state would be a new decision, not an implementation detail (spec §7).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced workspace-wide by `clippy.toml` `disallowed-types`. Float sorting uses `total_cmp`.
- **No wall-clock time in sim code.** A benchmark harness may use `std::time::Instant` behind a scoped `#[allow(clippy::disallowed_types)]` with a comment — the pattern `agent_scaling.rs` and `session_length_scaling.rs` already use.
- **`#![warn(missing_docs)]`** is set on every crate: every public item, field and variant needs a one-line doc comment.
- **Every primitive at a `pub` boundary needs a `type-audit:` tag** (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`). `kernel/src/derived.rs` uses `/// type-audit: bare-ok(count: Ledger.position)` for exactly this shape — follow it.
- **`cargo fmt` is the last step before every commit.** Fmt-gate skips are the most common review finding.
- **Run `make gate-commit` before any commit that touches Rust.** On this Mac the warm floor is ~110–155 s and a cold run after an absorb is 450–700 s; the documented 10–16 s does not hold. Read costs from `docs/timings.md` (field 4 is `wall_s`), per host.
- **One test invocation per question.** This workspace's suite time is dominated by test runtime, so capture once and grep the file rather than re-running to ask a second thing: `cargo test ... > /tmp/hv.log 2>&1; echo "exit=$?"` then grep `/tmp/hv.log`.

---

### Task 1: The synthetic depth sweep

Spec §4 has one acknowledged weakness: it *observes* history rather than *controlling* it, so the sampled range is only 2.48x, `C` is not identifiable, and history depth is perfectly correlated with wall-clock order. This task builds the complement — chosen depths, measured **interleaved** so depth and elapsed time are uncorrelated by construction.

It is a new file under `windows/vessel/examples/`, which cannot conflict with The Escapement's `liveness.rs` edits.

**Files:**
- Create: `windows/vessel/examples/fold_depth_sweep.rs`

**Interfaces:**
- Consumes: `hornvale_vessel::liveness::{drive_at, SUSTENANCE, Terrain, AGENT_AT, DRANK}`, `hornvale_kernel::{EntityId, Fact, Ledger, Value, WorldTime, RoomAddr}`, `hornvale_kernel::registry::ConceptRegistry`, `hornvale_species::MetabolicClass`. `drive_at`'s signature is `pub fn drive_at(ledger: &Ledger, entity: EntityId, home: &RoomAddr, t: WorldTime, p: &DriveParams, terrain: &dyn Terrain, class: MetabolicClass) -> f64`.
- Produces: nothing other code consumes. An informative bench, never a gate.

- [ ] **Step 1: Read the two things this file imitates**

Read `windows/vessel/examples/session_length_scaling.rs` in full — this task reuses its `binomial_tail`, its band/warm-band discipline, and its declared-exclusion style. Then read `kernel/examples/query_scaling.rs`, which is the same *construction* this task needs (synthetic ledger, swept variable, `--release`) for the same reason.

Then read the `Terrain` trait definition (`windows/vessel/src/liveness.rs:341`) and list every method you must implement.

- [ ] **Step 2: Write the constant-terrain stand-in**

A synthetic sweep must not pay genesis. Implement `Terrain` directly in the example, returning fixed values, so the only thing varying across the sweep is ledger depth.

```rust
/// A `Terrain` that answers the same thing everywhere — so the only variable in
/// this sweep is the ledger's depth.
///
/// The thirst integral reads `temperature` once per segment and nothing else
/// that varies, so a constant field makes every segment cost the same and the
/// measured difference between two depths is the *number* of segments alone.
/// That is the point: `LocaleTerrain` would fold real terrain reads (and a
/// `RoomMeshMemo` whose warmth changes over a run) into the number.
struct FlatTerrain;
```

Implement every method the trait requires. For any method whose return type you cannot construct trivially, return the trait's own default if it has one; if it does not, return the simplest valid value and say so in a comment. **Do not guess at a method list from this plan — read the trait.**

- [ ] **Step 3: Write the synthetic-ledger builder**

```rust
/// A ledger holding exactly `depth` `agent-at` facts for `entity`, one per day,
/// plus the one `drank` fact that bounds the thirst integral's window.
///
/// Days ascend by 1.0 so the integral has `depth` segments to walk. The rooms
/// cycle through a small fixed set rather than being constant, because a
/// constant room would let a future optimisation collapse the segments and
/// silently flatter the fold.
fn synthetic_ledger(entity: EntityId, depth: usize, registry: &ConceptRegistry) -> Ledger {
    let mut ledger = Ledger::default();
    ledger
        .commit(drank_at(entity, 0.0), registry)
        .expect("a synthetic drank fact commits");
    for i in 0..depth {
        ledger
            .commit(agent_at(entity, i), registry)
            .expect("a synthetic agent-at fact commits");
    }
    ledger
}
```

Write `agent_at(entity, i)` and `drank_at(entity, day)` alongside it. `agent_at` builds a `Fact` with `predicate: AGENT_AT.to_string()`, `day: Some(WorldTime::new(i as f64 + 1.0)...)`, and `object: Value::Text(...)` holding a packed `RoomAddr`.

**Read how `liveness.rs` encodes a room** — `room_to_text` is `r.pack()` rendered as a decimal `u64` string — and produce the same encoding. `drive_at` calls `room_from_text` on it, so a wrong encoding either panics or silently reads a different room, and the second failure mode would produce a plausible number.

Register `AGENT_AT` and `DRANK` on the registry first, exactly as `session_length_scaling.rs` does; `commit` refuses an unregistered predicate.

- [ ] **Step 4: Write the interleaved sweep**

```rust
/// The depths swept. Spans ~1000x so the affine fit is well-conditioned and
/// `C` is identifiable — the range `session_length_scaling.rs` cannot reach,
/// because it grows history by ticking and only ever spans 2.48x.
const DEPTHS: &[usize] = &[10, 32, 100, 320, 1_000, 3_200, 10_000];

/// Passes over `DEPTHS`. Each pass visits every depth; the ORDER reverses on
/// odd passes, so every depth is measured both early and late in the run.
///
/// THIS IS THE WHOLE POINT OF THIS BENCH. In `session_length_scaling.rs`
/// history can only grow, so depth and elapsed wall-clock time are perfectly
/// correlated and any drift in machine availability arrives disguised as a
/// history effect — measured there as three runs disagreeing about the SIGN.
/// Here depth is set, not grown, so alternating the direction decorrelates the
/// two, and a median across passes is robust to a disturbance hitting one.
const PASSES: usize = 6;

/// Back-to-back `drive_at` calls averaged into one reading.
const FOLD_REPS: u32 = 50;
```

For each pass, iterate `DEPTHS` forward on even passes and reversed on odd ones. Build the ledger once per (pass, depth), then time `FOLD_REPS` calls of `drive_at`. Collect every `(depth, us_per_call)` sample.

- [ ] **Step 5: Report the median per depth, then fit**

Print a table of depth then median µs/call across passes, plus min and max so the spread is visible. Then fit `us = C + k * depth` by ordinary least squares over the medians and print `C`, `k` and `r^2`.

Reuse `session_length_scaling.rs`'s `report_affine` shape, and reuse its lesson: **print `r^2` beside `k`, and print no log-log exponent.** Copy `binomial_tail` if you report monotonicity — a shared module is not worth it for a fourth bench, and `agent_scaling.rs`, `query_scaling.rs` and `session_length_scaling.rs` each carry their own copy of their helpers and each says so.

Take the **median**, not the mean, across passes: robustness to one disturbed pass is the entire reason for multiple passes, and a mean does not deliver it.

- [ ] **Step 6: Lint and run**

```bash
cargo fmt -p hornvale-vessel
cargo clippy --release -p hornvale-vessel --example fold_depth_sweep -- -D warnings
cargo run --release -p hornvale-vessel --example fold_depth_sweep
```

Expected: clippy clean; a table over all seven depths.

**Read the output before believing it.** Three branches — take the one that matches:
- `k > 0`, `r^2` >= 0.9, `C` positive: the expected result. Record `C`, `k`, `r^2` in your report.
- `k > 0` but `r^2` low, or `C` negative: the fit still has not settled. Report the table as measured and say so; do **not** widen `DEPTHS` to chase a nicer number without saying you did.
- `k` not distinguishable from zero: **a real finding that contradicts spec §4.** Stop. Do not proceed to Task 3. §4 measured `drive_at` in situ; if a synthetic ledger shows no history term the two instruments disagree, and that must be resolved before anything is built on either.

- [ ] **Step 7: Commit**

```bash
git add windows/vessel/examples/fold_depth_sweep.rs
git commit -m "feat(vessel): an interleaved synthetic depth sweep for the history term

session_length_scaling.rs observes history rather than controlling it, so
its range is 2.48x, C is not identifiable, and depth is perfectly
correlated with wall-clock order -- which is why three runs of its
whole-tick column disagreed about the sign.

This sets depth instead of growing it, over ~1000x, and visits each depth
in both directions across six passes so depth and elapsed time are
uncorrelated by construction. Medians across passes, not means, because
robustness to one disturbed pass is the entire purpose.

Informative, never a gate."
```

---

### Task 2: Attribute the history term across the six folds

Spec §4 establishes that `drive_at` is proportional to its history and that the history term is 70–80% of a tick. It does not say which of the other five folds carry the rest. Stage 4's entry gate is exactly that answer.

**Files:**
- Modify: `docs/superpowers/specs/2026-08-24-the-tailrace-design.md` (§4, adding the attribution)

**Interfaces:**
- Consumes: Task 1's confirmation that the history term is real.
- Produces: the attribution table stage 4's gate reads.

- [ ] **Step 1: Build the profiling binary**

```bash
cargo build --profile profiling -p hornvale-vessel --example session_length_scaling
```

`[profile.profiling]` is declared in the root `Cargo.toml` at line 21 and inherits release with `debug = true`, so symbols survive. `samply` is installed at `/opt/homebrew/bin/samply`.

- [ ] **Step 2: Record the profile**

Check the flags first rather than guessing:

```bash
samply record --help
samply record -o /tmp/tailrace.json.gz ./target/profiling/examples/session_length_scaling
```

- [ ] **Step 3: Attribute the six folds**

Read inclusive time for each fold named in spec §2: `agent_sightings`, `integrate_thirst` (via `drive_at`), `hunger_at`, `fatigue_at`, `believed_water`, `hazard_memory_memo`, `build_emitter_scan`.

**Symbolication caveat, learned on this project:** an inlined function may not appear as its own frame, so **a missing symbol is not a zero measurement.** If a fold is absent from the profile, check whether it was inlined into its caller before concluding it is free. `#[inline(never)]` applied temporarily and reverted is the way to force a frame.

- [ ] **Step 4: Write the attribution into the spec**

Add a subsection to §4, "Attribution across the six folds": a table of fold then inclusive % of the run, and one sentence per fold on whether it carries a material share.

Then answer stage 4's gate explicitly, in the spec, with one of:
- belief and hazard carry a material share, so **stage 4 is entered**;
- they do not, so **stage 4 is not entered** — and the spec records that migrating them would be unmotivated memory for no measured gain, the same judgement metaplan §6.5 made against its own stage 2.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/specs/2026-08-24-the-tailrace-design.md
git commit -m "spec(the-tailrace): attribute the history term across the six folds

Stage 1's remaining deliverable. Section 4 measured that drive_at is
proportional to its history and that the history term is 70-80% of a
tick; this says which folds carry it, which is stage 4's entry gate."
```

---

### Task 3: The `LedgerFold` trait and `Folded<S>`, with FOLD equals SCAN

The campaign's core deliverable. **The state IS the fold** — no marker type, no associated type, no `PhantomData`.

**Files:**
- Create: `kernel/src/fold.rs`
- Modify: `kernel/src/lib.rs` (one line: `pub mod fold;` between `pub mod field;` and `pub mod geosphere;`)
- Create: `kernel/tests/suite/fold.rs`
- Modify: `kernel/tests/suite.rs` (two lines declaring the module)

**Interfaces:**
- Consumes: `crate::ledger::{Fact, Ledger}`.
- Produces:
  - `pub trait LedgerFold: Sized + PartialEq + core::fmt::Debug { fn empty() -> Self; fn absorb(&mut self, fact: &Fact); }`
  - `pub struct Folded<S: LedgerFold>` with `pub fn new() -> Self`, `pub fn resume(state: S, position: u64) -> Self`, `pub fn state(&self) -> &S`, `pub fn position(&self) -> u64`, `pub fn absorb_at(&mut self, position: u64, fact: &Fact)`, `pub fn advance_to(&mut self, ledger: &Ledger)`, `pub fn rebuild(ledger: &Ledger) -> Self`, `pub fn rebuild_upto(ledger: &Ledger, position: u64) -> Self`
  - `impl<S: LedgerFold> Default for Folded<S>`

`derived` is **not** re-exported from `lib.rs`'s `pub use` block — it is reached as `hornvale_kernel::derived::Derived` — so `fold` follows the same convention and **adds no `pub use` line.** That also keeps this task's `lib.rs` edit at line 17, far from the `pub use units::{...}` block at line 77 that `campaign/the-escapement` modifies, so there is nothing to conflict.

- [ ] **Step 1: Write the failing tests**

Create `kernel/tests/suite/fold.rs`:

```rust
//! The incremental-fold primitive: advancing over a ledger prefix gives the
//! same answer as folding it from scratch (FOLD equals SCAN), and the position
//! guard makes a skipped or repeated fact loud rather than silent.

use hornvale_kernel::fold::{Folded, LedgerFold};
use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// An ORDER-SENSITIVE test fold. `count` alone would pass even if facts were
/// absorbed in the wrong order, which is precisely the bug class this
/// primitive can have — so `rolling` mixes each fact's predicate length in a
/// way that does not commute.
#[derive(Debug, PartialEq)]
struct Probe {
    count: u64,
    rolling: u64,
}

impl LedgerFold for Probe {
    fn empty() -> Self {
        Probe {
            count: 0,
            rolling: 1,
        }
    }
    fn absorb(&mut self, fact: &Fact) {
        self.count += 1;
        self.rolling = self
            .rolling
            .wrapping_mul(31)
            .wrapping_add(fact.predicate.len() as u64);
    }
}

/// Two predicates of DIFFERENT name length, so `rolling` distinguishes their
/// order. Equal lengths would make the fold commutative and every test below
/// vacuous.
const SHORT: &str = "a";
const LONGER: &str = "abcd";

fn registry() -> ConceptRegistry {
    let mut r = ConceptRegistry::default();
    r.register_predicate(SHORT, false, "test").unwrap();
    r.register_predicate(LONGER, false, "test").unwrap();
    r
}

fn fact(predicate: &str, n: u64) -> Fact {
    Fact {
        subject: EntityId::new(n).expect("n is non-zero"),
        predicate: predicate.to_string(),
        object: Value::Flag(true),
        place: None,
        day: None,
        provenance: "test".to_string(),
    }
}

/// A ledger whose predicate sequence is deliberately mixed, so an order bug
/// moves `rolling`.
fn ledger_of(len: u64) -> Ledger {
    let reg = registry();
    let mut l = Ledger::default();
    for i in 1..=len {
        let p = if i % 3 == 0 { LONGER } else { SHORT };
        l.commit(fact(p, i), &reg).expect("a test fact commits");
    }
    l
}

#[test]
fn the_probe_fold_is_order_sensitive() {
    // Guards every other test in this file from being vacuous: a commutative
    // probe would pass them all under an ordering bug.
    let mut forward = Probe::empty();
    forward.absorb(&fact(SHORT, 1));
    forward.absorb(&fact(LONGER, 2));

    let mut backward = Probe::empty();
    backward.absorb(&fact(LONGER, 2));
    backward.absorb(&fact(SHORT, 1));

    assert_ne!(forward, backward, "the probe fold must not commute");
}

#[test]
fn advancing_in_two_steps_equals_folding_from_scratch() {
    let l = ledger_of(20);
    let scan: Folded<Probe> = Folded::rebuild(&l);

    // Advance in two bites, the way a tick does.
    let mut incremental: Folded<Probe> = Folded::new();
    let half = ledger_of(9);
    incremental.advance_to(&half);
    incremental.advance_to(&l);

    assert_eq!(incremental.state(), scan.state());
    assert_eq!(incremental.position(), scan.position());
}

#[test]
fn one_fact_at_a_time_equals_folding_from_scratch() {
    let l = ledger_of(20);
    let scan: Folded<Probe> = Folded::rebuild(&l);

    let mut one_by_one: Folded<Probe> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        one_by_one.absorb_at(i as u64, f);
    }

    assert_eq!(one_by_one.state(), scan.state());
    assert_eq!(one_by_one.position(), scan.position());
}
```

Then declare it in `kernel/tests/suite.rs`, keeping that file's existing ordering and its `#[path]` convention (its own doc comment explains why the attribute is needed):

```rust
#[path = "suite/fold.rs"]
mod fold;
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cargo test -p hornvale-kernel --test suite -- fold > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^error|unresolved" /tmp/hv.log | head
```

Expected: FAIL to compile, `unresolved import hornvale_kernel::fold`.

Note the test-binary consolidation: every crate's integration tests live behind one `tests/suite.rs` binary named `suite`, so a former per-file `--test <name>` is now `--test suite -- <filter>`.

- [ ] **Step 3: Write the module**

Create `kernel/src/fold.rs`:

```rust
//! An incremental fold over a ledger prefix — a derived value that ADVANCES
//! with the log rather than being invalidated by it.
//!
//! # Why this is not [`crate::derived::Derived`]
//!
//! `Derived` with [`crate::derived::Validity::Ledger`] is a MEMO: an entry goes
//! stale once a fact touching a watched `DepKey` commits after the entry's
//! recorded position, and a stale entry is evicted on read and recomputed from
//! scratch. That is the right shape for a value read far more often than its
//! dependencies change.
//!
//! It is the wrong shape — structurally, not marginally — for a fold whose
//! dependency is touched on **every** tick. The motivating case is
//! `windows/vessel`'s drive stack: a fold over one agent's `agent-at` history
//! watches `(that agent, agent-at)`, and the tick commits exactly that every
//! time the agent moves. So the entry would be stale every tick, every read
//! would be a miss, and the recomputation would be the whole-history walk the
//! cache existed to avoid — a cache with a structurally guaranteed 100% miss
//! rate.
//!
//! **Invalidation and accumulation are different operations, not two policies
//! over one operation.** That is why this is a sibling module rather than a
//! third [`crate::derived::Validity`] variant: there is nowhere in `Derived` to
//! put an update function, and adding one would change what the type is.
//!
//! # The state IS the fold
//!
//! [`LedgerFold`] is implemented by the accumulated state itself, not by a
//! marker type with an associated `State`. That keeps [`Folded`] free of a
//! `PhantomData`, and keeps a tenant's code reading as "my state, plus how it
//! absorbs a fact" rather than as two types that must be kept in step.
//!
//! # The obligations a tenant owes
//!
//! - **Commit order.** [`Folded::absorb_at`] takes the position the caller
//!   believes it is at and asserts it matches. Absorbing one fact twice, and
//!   skipping one, are the two bugs this primitive can have, and both are
//!   otherwise silent — so the guard is an `assert!`, not a `debug_assert!`.
//! - **FOLD equals SCAN.** Advancing incrementally must equal folding the same
//!   prefix from scratch. The primitive cannot prove this of a tenant's
//!   `absorb` — one that read a clock, or that depended on how many facts
//!   arrived in a batch, would break it — so every tenant owes the property
//!   test. [`Folded::rebuild`] exists to make writing it cheap.
//! - **Determinism.** A state holding a `HashMap` would put an unstable
//!   iteration order under a byte-identity guarantee. `clippy.toml`'s
//!   `disallowed-types` already refuses that workspace-wide; it is restated
//!   here because a fold state is exactly where someone would reach for one.
//!
//! # What this does NOT do
//!
//! It does not go backwards. A fold is a one-directional accumulation, so a
//! query about an earlier position is served by [`Folded::rebuild_upto`], which
//! is O(position). A tenant with a natural checkpoint — a reset event returning
//! the state to a known value, such as a `drank` fact zeroing a thirst
//! integral — can do far better by pairing [`Folded::resume`] with a narrowed
//! range. That choice is the tenant's, because only the tenant knows where its
//! checkpoints are.

use crate::ledger::{Fact, Ledger};

/// A value accumulated by absorbing a ledger's facts in commit order.
///
/// Implemented by the state itself. `PartialEq` and `Debug` are required
/// because every tenant owes a FOLD-equals-SCAN property test, and that test
/// needs to compare two states and print them when they differ.
pub trait LedgerFold: Sized + PartialEq + core::fmt::Debug {
    /// The state before any fact has been absorbed — the fold's identity
    /// element.
    fn empty() -> Self;

    /// Absorb one fact. Called once per fact, in commit order.
    ///
    /// Must be a pure function of `(self, fact)`. In particular it must not
    /// depend on how many facts arrive in a batch, or on anything outside the
    /// ledger, or FOLD equals SCAN does not hold.
    fn absorb(&mut self, fact: &Fact);
}

/// A [`LedgerFold`] state together with the ledger position it is valid at.
/// type-audit: bare-ok(count: Ledger.position)
#[derive(Debug, PartialEq)]
pub struct Folded<S: LedgerFold> {
    state: S,
    position: u64,
}

impl<S: LedgerFold> Folded<S> {
    /// An empty fold, valid at position 0.
    pub fn new() -> Self {
        Folded {
            state: S::empty(),
            position: 0,
        }
    }

    /// Resume from a state known to be valid at `position` — the checkpoint
    /// seam. The caller asserts that `state` is exactly what folding the
    /// ledger's first `position` facts would produce; nothing here can check
    /// that, which is why it is the tenant's obligation and not this type's.
    /// type-audit: bare-ok(count: position)
    pub fn resume(state: S, position: u64) -> Self {
        Folded { state, position }
    }

    /// The accumulated state, valid as of [`Self::position`].
    pub fn state(&self) -> &S {
        &self.state
    }

    /// The number of facts absorbed — equivalently, the length of the ledger
    /// prefix this state folds.
    /// type-audit: bare-ok(count: return)
    pub fn position(&self) -> u64 {
        self.position
    }

    /// Absorb the fact at `position`, which must be the position this fold is
    /// currently at.
    ///
    /// The O(1) door, for a caller already holding the fact it is about to
    /// commit — which is the tick's situation: the drive systems return their
    /// facts before the ledger appends them, so nothing needs to scan.
    ///
    /// # Panics
    ///
    /// If `position` is not [`Self::position`]. Absorbing twice and skipping
    /// are the only two bugs available here and both corrupt the state
    /// silently, so this is an `assert!` in every build, not a
    /// `debug_assert!`.
    /// type-audit: bare-ok(count: position)
    pub fn absorb_at(&mut self, position: u64, fact: &Fact) {
        assert_eq!(
            position, self.position,
            "a fold must absorb each fact exactly once, in commit order"
        );
        self.state.absorb(fact);
        self.position += 1;
    }

    /// Absorb every fact `ledger` has gained since [`Self::position`].
    ///
    /// The catch-up door, for a caller holding a ledger rather than the
    /// individual facts. A ledger shorter than this fold's position absorbs
    /// nothing: an append-only log cannot shrink, so that means the caller
    /// passed a different ledger, and folding nothing is a better failure than
    /// folding a prefix of the wrong log.
    pub fn advance_to(&mut self, ledger: &Ledger) {
        for fact in ledger.iter().skip(self.position as usize) {
            self.state.absorb(fact);
            self.position += 1;
        }
    }

    /// Fold `ledger` from scratch — the SCAN half of FOLD equals SCAN, and the
    /// reference every tenant's property test compares against.
    pub fn rebuild(ledger: &Ledger) -> Self {
        let mut folded = Self::new();
        folded.advance_to(ledger);
        folded
    }

    /// Fold only the ledger's first `position` facts — the general answer to a
    /// query about an earlier position, at O(position).
    /// type-audit: bare-ok(count: position)
    pub fn rebuild_upto(ledger: &Ledger, position: u64) -> Self {
        let mut folded = Self::new();
        for fact in ledger.iter().take(position as usize) {
            folded.state.absorb(fact);
            folded.position += 1;
        }
        folded
    }
}

impl<S: LedgerFold> Default for Folded<S> {
    fn default() -> Self {
        Self::new()
    }
}
```

Add `pub mod fold;` to `kernel/src/lib.rs` between `pub mod field;` and `pub mod geosphere;`. Add no `pub use` line.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cargo test -p hornvale-kernel --test suite -- fold > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

Expected: PASS, 3 tests.

- [ ] **Step 5: Regenerate the type-audit report**

This task adds `pub` items carrying primitives, so the committed report drifts.

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat -- docs/audits/
```

Branches — take the one that matches, do not predict:
- `check` fails: a `pub`-boundary primitive is untagged. Tag it and re-run; `check` is default-deny.
- `check` passes, `docs/audits/` shows a diff: expected. Commit it **in the same commit** as the code.
- `check` passes, `docs/audits/` shows no diff: you added no `pub`-boundary primitive, which for this task would mean the module is not what this plan describes. Re-read Step 3.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
make gate-commit
git add kernel/src/fold.rs kernel/src/lib.rs kernel/tests/suite/fold.rs kernel/tests/suite.rs docs/audits/
git commit -m "feat(kernel): an incremental ledger fold that advances rather than invalidating

Derived's Validity::Ledger is a memo, and for a fold whose dependency is
touched every tick it is structurally wrong: the entry is stale every
tick, every read is a miss, and the recomputation is the whole-history
walk the cache existed to avoid. Invalidation and accumulation are
different operations, so this is a sibling module rather than a third
Validity variant -- there is nowhere in Derived to put an update function.

The state IS the fold: LedgerFold is implemented by the accumulated state,
which keeps Folded free of a PhantomData and keeps a tenant reading as one
type rather than two kept in step.

absorb_at asserts the caller's position in every build, not just debug.
Absorbing twice and skipping are the only two bugs available here, and
both corrupt the state silently.

The probe fold in the tests is deliberately non-commutative, with its own
test proving it -- a commutative probe would let the FOLD-equals-SCAN
tests pass under an ordering bug."
```

This is a kernel-layer edit, so the gate rebuilds a great deal. Budget from `docs/timings.md`, not from any prose figure.

---

### Task 4: The checkpoint seam and the past-position door

`Folded::resume` and `rebuild_upto` exist after Task 3 but nothing pins their contract. This task pins it, because the whole reason stages 3–5 can bound the drives is that a reset event is a checkpoint.

**Files:**
- Modify: `kernel/tests/suite/fold.rs` (append)

**Interfaces:**
- Consumes: Task 3's `Folded<S>` and `LedgerFold`, exact signatures as listed there, plus its `Probe`, `ledger_of`, `fact`, `registry`, `SHORT`, `LONGER` helpers from the same file.
- Produces: no new API. Tests only.

- [ ] **Step 1: Write the tests**

Append to `kernel/tests/suite/fold.rs`:

```rust
#[test]
fn rebuilding_upto_a_position_equals_the_prefix_it_names() {
    let l = ledger_of(20);
    let upto = Folded::<Probe>::rebuild_upto(&l, 7);

    // The same prefix, reached the long way round.
    let prefix = ledger_of(7);
    let scan: Folded<Probe> = Folded::rebuild(&prefix);

    assert_eq!(upto.state(), scan.state());
    assert_eq!(upto.position(), 7);
}

#[test]
fn resuming_from_a_checkpoint_reaches_the_same_place_as_folding_throughout() {
    let l = ledger_of(20);

    // The checkpoint: whatever the state was at position 7.
    let checkpoint = Folded::<Probe>::rebuild_upto(&l, 7);
    let carried = Probe {
        count: checkpoint.state().count,
        rolling: checkpoint.state().rolling,
    };

    let mut resumed: Folded<Probe> = Folded::resume(carried, checkpoint.position());
    resumed.advance_to(&l);

    let scan: Folded<Probe> = Folded::rebuild(&l);
    assert_eq!(resumed.state(), scan.state());
    assert_eq!(resumed.position(), scan.position());
}

#[test]
fn rebuilding_upto_beyond_the_ledger_stops_at_the_ledger() {
    let l = ledger_of(5);
    let over = Folded::<Probe>::rebuild_upto(&l, 500);
    let all: Folded<Probe> = Folded::rebuild(&l);
    assert_eq!(over.state(), all.state());
    assert_eq!(over.position(), all.position());
}

#[test]
#[should_panic(expected = "exactly once")]
fn absorbing_the_same_position_twice_panics() {
    let l = ledger_of(3);
    let mut f: Folded<Probe> = Folded::new();
    let first = l.iter().next().expect("the ledger has facts");
    f.absorb_at(0, first);
    f.absorb_at(0, first); // the repeat bug, made loud
}

#[test]
#[should_panic(expected = "exactly once")]
fn skipping_a_position_panics() {
    let l = ledger_of(3);
    let mut f: Folded<Probe> = Folded::new();
    let second = l.iter().nth(1).expect("the ledger has 3 facts");
    f.absorb_at(1, second); // position 0 was never absorbed
}
```

- [ ] **Step 2: Run them**

```bash
cargo test -p hornvale-kernel --test suite -- fold > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv.log
```

**If everything passes on the first run, that is the expected outcome and not a problem.** These tests pin behaviour Task 3 built deliberately. The failing-first cycle applies to new behaviour; this task adds none — it adds the assertions that stop Task 3's contract from being changed by accident later. Say so in your report rather than inventing a reason to make them red.

If something fails, it is a real defect in Task 3 and this is the task that found it. Report which test and why before fixing.

- [ ] **Step 3: Fix anything that failed**

One likely defect and its tell: if `rebuilding_upto_beyond_the_ledger_stops_at_the_ledger` fails, `rebuild_upto` was implemented with indexing rather than `Iterator::take`, which clamps. Use `take`.

- [ ] **Step 4: Run the whole kernel suite, not just this filter**

```bash
cargo test -p hornvale-kernel --test suite > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED" /tmp/hv.log
```

Expected: PASS, including the pre-existing `derived` and `determinism` modules — a new `pub mod` in the kernel is exactly the kind of change that can perturb an unrelated golden.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate-commit
git add kernel/tests/suite/fold.rs
git commit -m "test(kernel): pin the fold's checkpoint seam and its position guard

resume plus a narrowed range is how a tenant with a reset event -- a drank
fact zeroing a thirst integral -- answers a past-position query in bounded
time instead of rebuild_upto's O(position). That is the mechanism stages
3-5 depend on, so it gets assertions rather than a doc comment.

Both position-guard directions are pinned: absorbing twice, and skipping.
A one-directional test would have let the other through."
```

---

### Task 5: The chaos-rebuild harness

Metaplan §7's correctness ladder, rung 3, with the subject changed from views to folds: discard the accumulated state at every legal opportunity and assert the output is unchanged. If a fold is genuinely a fold, no schedule of discards is observable.

**Files:**
- Modify: `kernel/tests/suite/fold.rs` (append)

**Interfaces:**
- Consumes: Task 3's `Folded<S>`, and Task 3's `Probe`/`ledger_of` helpers.
- Produces: no new API. Tests only.

- [ ] **Step 1: Write the tests**

Append to `kernel/tests/suite/fold.rs`:

```rust
/// CHAOS-REBUILD: metaplan §7's chaos-eviction rung, for folds.
///
/// Walk the ledger one fact at a time and, at EVERY position, throw the
/// accumulated state away and rebuild it from the ledger prefix — then carry
/// on. If the fold is genuinely a fold, the end state is identical to never
/// having discarded anything, for every possible discard schedule. This is the
/// most aggressive schedule there is.
#[test]
fn discarding_the_state_at_every_position_is_unobservable() {
    let l = ledger_of(24);
    let resident: Folded<Probe> = Folded::rebuild(&l);

    let mut chaotic: Folded<Probe> = Folded::new();
    for (i, fact) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, fact);
        // Throw it all away and come back from the ledger.
        chaotic = Folded::rebuild_upto(&l, chaotic.position());
    }

    assert_eq!(
        chaotic.state(),
        resident.state(),
        "a discard schedule must not be observable"
    );
    assert_eq!(chaotic.position(), resident.position());
}

/// The same property under a SPARSER schedule. Not redundant: a bug can cancel
/// out under the every-step schedule — where the rebuilt state is recomputed
/// immediately after every single absorb — and survive one that lets several
/// absorbs accumulate between discards.
#[test]
fn discarding_the_state_at_every_third_position_is_unobservable() {
    let l = ledger_of(24);
    let resident: Folded<Probe> = Folded::rebuild(&l);

    let mut chaotic: Folded<Probe> = Folded::new();
    for (i, fact) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, fact);
        if i % 3 == 0 {
            chaotic = Folded::rebuild_upto(&l, chaotic.position());
        }
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}
```

- [ ] **Step 2: Run them**

```bash
cargo test -p hornvale-kernel --test suite -- fold > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED" /tmp/hv.log
```

Expected: PASS. As in Task 4, passing immediately is the intended outcome — this rung asserts the primitive holds no hidden state, and if it does hold some, these go red.

- [ ] **Step 3: Prove the harness can actually fail**

A green chaos test proves nothing if the harness is incapable of going red. Break the fold temporarily, confirm red, then revert.

**Find the mutation yourself, in `kernel/src/fold.rs`.** The property under test is that the state is a pure function of the absorbed prefix; a mutation that makes it depend on anything else should turn these tests red. You can see which line carries that once you have read the module — a plan author guessing from outside would pick a worse one.

Two rules, both learned the hard way on this project:

- **Assert the mutation applied.** Confirm the text you meant to replace was actually found and replaced (`git diff` shows the hunk) before trusting the red. A no-op mutation produces a green that looks exactly like a robust implementation, which is worse than not testing at all.
- **A red from a compile error proves nothing** about whether an assertion would have caught the behaviour. The mutation must still type-check.

Record in your report: the mutation, and which tests went red. **If the chaos tests stay green under a mutation that genuinely breaks purity, the harness is not testing what it claims** — report that as a finding rather than working around it.

- [ ] **Step 4: Revert and confirm the tree is clean**

```bash
git diff -- kernel/src/fold.rs
cargo test -p hornvale-kernel --test suite -- fold > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

Expected: an empty diff, and PASS. Do not commit with the mutation in the tree.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate-commit
git add kernel/tests/suite/fold.rs
git commit -m "test(kernel): chaos-rebuild -- a discard schedule must be unobservable

Metaplan section 7's chaos-eviction rung with the subject changed from
views to folds. Two schedules, not one: discard at every position, and at
every third, because a bug can cancel out under the aggressive schedule
and survive the sparse one.

The harness was checked against a mutation rather than trusted for being
green -- a chaos test that cannot fail proves nothing."
```

---

### Task 6: Close stage 2 — decisions, spec, registry, stage gate

**Files:**
- Create: `docs/decisions/0236-a-fold-advances-it-is-not-invalidated.md`
- Create: `docs/decisions/0237-the-reset-event-is-the-checkpoint.md`
- Modify: `docs/digest/decisions-in-force.md` (generated)
- Modify: `docs/superpowers/specs/2026-08-24-the-tailrace-design.md`
- Modify: `book/src/frontier/idea-registry.md` (the `TOOL-incremental-ledger-fold` row)

**Interfaces:**
- Consumes: Tasks 1–5.
- Produces: the record stages 3–5's plan is written against.

- [ ] **Step 1: Mint the two decision records**

The reserved block is **0236–0245** (`make decision-block NAME=the-tailrace`, already run — do not re-run it and do not pick numbers outside the range). Read three recent records in `docs/decisions/` for the format rather than inventing one.

- `0236-a-fold-advances-it-is-not-invalidated.md` — the distinction between this primitive and `Derived`'s memo, and the general rule under it: a dependency touched on every tick makes a memo useless by construction, so invalidation and accumulation are different operations rather than two policies over one.
- `0237-the-reset-event-is-the-checkpoint.md` — a past-position read is served from the last reset at or before it, which is what bounds a fold whose history is otherwise unbounded; and why the primitive exposes `resume` rather than trying to know where a tenant's checkpoints are.

`docs/digest/decisions-in-force.md` is generated and drift-checked, so regenerate it in the same commit:

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
git diff --stat -- docs/digest/
```

Branches: a diff is expected, since you added two records — commit it. **No diff means nothing was regenerated.** Every `render` subcommand prints to stdout and it is the `>` redirect that writes the file, so a bare `render` leaves the artifact untouched while the drift check that follows reports an empty diff and reads as "no drift".

`no_gaps_in_the_decision_log` does **not** exist as a check — main itself carries four gaps while green. A contiguity guard for *duplicates* does exist in `cli/tests/suite/docs_consistency.rs`; stay inside the reserved range and gaps inside it cost nothing.

- [ ] **Step 2: Record what shipped, in the spec**

Add a §11, "What stage 2 shipped", naming the module path, the trait and holder, and the properties now pinned: FOLD equals SCAN in two forms, both position-guard directions, the checkpoint round-trip, `rebuild_upto` clamping, and both chaos schedules — plus the mutation that proved the chaos harness can fail.

State explicitly that **`Ledger` was not modified** and that **nothing entered the save**. Those are the two determinism contracts in §7 a reader will want to confirm without reading the diff.

- [ ] **Step 3: Update the registry row**

`TOOL-incremental-ledger-fold` currently reads as a proposal. Move its status cell from `raw` to whatever this repo uses for shipped-with-a-deferred-half — read three neighbouring rows to find the convention rather than guessing — and put the deferred half (stages 3–5) in the **Where** cell. Root `CLAUDE.md` says a shipped row's Where cell is where a deferred half is recorded.

**The Idea cell is capped at 600 characters** by `cli/tests/suite/docs_consistency.rs`, whose failure message names the remedy: a row is an index entry, not an essay, and the argument belongs in the spec the Where cell links. Check before committing:

```bash
cargo test -q -p hornvale --test suite docs_consistency > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|chars|FAILED" /tmp/hv.log
```

Branches:
- PASS: proceed.
- FAIL naming your row and a character count: trim the Idea cell.
- FAIL on a duplicate row ID: you added a row instead of editing one. Edit the existing row.

- [ ] **Step 4: Commit**

```bash
cargo fmt
make gate-commit
git add docs/decisions/ docs/digest/ docs/superpowers/specs/2026-08-24-the-tailrace-design.md book/src/frontier/idea-registry.md
git commit -m "spec(the-tailrace): stage 2 shipped; decisions 0236-0237

Records the properties pinned, that Ledger was not modified, and that
nothing entered the save -- the two determinism contracts a reader wants
to check without reading the diff.

Stages 3-5 remain deferred behind campaign/the-escapement's WorldTime
epoch and get their own plan once it lands."
```

- [ ] **Step 5: Submit the stage gate**

```bash
git push
make sluice-status
make sluice-stage BRANCH=campaign/the-tailrace REF=<full-sha-of-HEAD>
```

A **full SHA**, never a branch name. Predict a hold with `git merge-tree` against the peer ahead of you in `make sluice-status`, not against `main`. A conflict is refused at the mouth in milliseconds — that is the signal to absorb main locally and resubmit, not a failure of the work.

A merge and a stage gate run the same four phases (`artifacts`, `outboard`, `gate`, `clients`), differing only in the push; `heavy` is a separate `make heavy-remote` dispatch, not a chamber phase.

---

## Self-Review

**1. Spec coverage.** §0's re-carve is recorded by Task 6. §1's motivation is Tasks 1–2. §2's six folds are Task 2's attribution; §2's four traps belong to stages 3–5, not here. §3's mechanism and its "why not `Derived`" argument is Task 3's module doc. §3's correctness ladder: rung 1 (type-level — `state()` hands out `&S`, so a read cannot advance what it reads) Task 3; rung 2 (FOLD equals SCAN) Task 3; rung 3 (advance-exactly-once) Task 4; rung 4 (chaos) Task 5; rung 5 (master oracle — the drift check) Task 6 Step 5. §4's limitations note is Task 1. §5's H1 is already met and H2/H3 belong to stage 5, out of scope. §6's stages 1–2 are all six tasks; stages 3–5 are explicitly deferred in Global Constraints. §7's contracts are Task 6 Step 2. §9's decisions 1–2 are Task 6 Step 1; decision 3 ("the trail's provenance is content") belongs to 7c.

**Gap found and closed:** an earlier draft had no task writing a `docs/decisions/` record, so the reserved 0236–0245 block would have gone unused and §9 unsatisfied. Added as Task 6 Step 1, with the `render`-writes-nothing hazard spelled out because a bare `render` leaves the artifact untouched and the drift check then reads as clean.

**2. Placeholder scan.** No "TBD", no "handle edge cases", no "similar to Task N", no step that describes without showing. Two steps deliberately refuse to specify from outside the code — Task 1 Step 2 (the `Terrain` method list) and Task 5 Step 3 (the mutation) — and both say why. That is the "never prescribe a mutation from outside the code" rule, not a placeholder: a plan author does not know which line carries the property, and the implementer does after reading.

**3. Type consistency.** `LedgerFold::empty`/`absorb` and `Folded::{new, resume, state, position, absorb_at, advance_to, rebuild, rebuild_upto}` appear with exactly those names in Tasks 3, 4 and 5. `Probe { count, rolling }` is constructed in Task 4 with both fields, matching Task 3's definition. `ledger_of`, `fact`, `registry`, `SHORT` and `LONGER` are defined once in Task 3 Step 1 and reused from the same file in Tasks 4–5, which is why both later tasks list them under Consumes.

**4. Ordering.** Tasks 1–2 (measurement) and 3–5 (the primitive) are independent and could run in either order; 3 must precede 4 and 5. Task 6 consumes all of them. If Task 1 returns "`k` not distinguishable from zero", **stop** — that contradicts spec §4 and must be resolved before Task 3 builds anything.
