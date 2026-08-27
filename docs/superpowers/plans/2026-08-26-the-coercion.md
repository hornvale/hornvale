# The Coercion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A body driven by someone other than the player — the player's in-character verbs refuse, out-of-character verbs still work, and the facts the body commits are indistinguishable from ones it would have committed freely.

**Architecture:** Two committed predicates (`possessed-by` / `possession-ended`) folded into a live possessor by a single pass over the body's own facts. `BodyState` gains one variant, which the existing compile-time tripwire forces every consumer to classify. The imposition seam is an out-of-character verb, because no creature has the capability yet.

**Tech Stack:** Rust 2024, no new external crates (decision 0004), no new internal dependency — every surface this touches is already in `hornvale-vessel`.

**Spec:** `docs/superpowers/specs/2026-08-26-the-coercion-design.md`

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml` `disallowed-types`).
- **No wall-clock time** anywhere, including tests.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag.** A `pub const … &str` predicate name takes `bare-ok(identifier-text)` — see `TURNED_HOSTILE` (`windows/vessel/src/session.rs:301-306`) for the exact shape.
- **Determinism is constitutional.** The fold must be a single ordered pass over ledger order — never a sort, never a map iteration.
- **ONE WORD: `possess`.** Do not introduce `dominate`, `usurp`, `control`, or `charm` as a synonym anywhere — identifier, doc comment, or refusal string. Settled in spec §2.1 by count (possess 747 in code / 1642 in prose; usurp 0).
- **`possess` is reserved for body-control.** If you need the have-a-thing sense, the word is `carried` (spec §2.2).
- **Do NOT build charm, command, or a general `Control` trait** (spec §4). Unifying them would force charm into the controller stack, where a charmed creature would stop running its own arbitration — which is exactly what charm must not do.
- **Do NOT rename the arc.** G3 kept "The Coercion" even though spec §4's taxonomy argues coercion names the *command* row. If the name reads wrong in prose you are writing, report it; do not fix it.
- `cargo fmt` final before each commit; `make gate-commit` at task end (`timeout: 3600000`).

## Reference: surfaces this plan consumes, all verified at plan time

```rust
// kernel/src/ledger.rs
pub struct Fact { pub subject: EntityId, pub predicate: String, pub object: Value,
                  pub place: Option<EntityId>, pub day: Option<WorldTime>,
                  pub provenance: String }                                   // :71
pub enum Value { Entity(EntityId), Text(String), Number(f64), Flag(bool) }   // :55
pub fn facts_about(&self, subject: EntityId) -> impl Iterator<Item = &Fact>; // :352
pub fn value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value>;// :399
pub fn commit(&mut self, fact: Fact, registry: &ConceptRegistry) -> Result<..>;// :309

// windows/vessel/src/gate.rs   (82 lines total)
pub enum BodyState { Awake, Asleep }
impl BodyState { pub fn all() -> Vec<BodyState> }
fn body_state_variants_must_all_be_rostered(s: &BodyState) -> &'static str;  // NO wildcard arm
pub enum Verdict { Permitted, Refused(String) }
pub fn verdict(state: BodyState, mood: Mood) -> Verdict;                     // NO wildcard arm

// windows/vessel/src/session.rs
fn body_state(&self) -> BodyState;              // :1910 — ONE caller, at :1940
pub fn agent_entity(&self) -> EntityId;         // :1676 — returns driven_body().entity
fn handle_ooc(&mut self, verb: &str, rest: &str) -> Turn;  // OOC match, arms at :2218+
fn refused_by_the_body(&self, verb: &str) -> Option<String>; // the gate consult, :1940
// predicate registration block: :955-:992  (registry.register_predicate(NAME, functional, doc))

// windows/vessel/src/controller.rs
pub trait Controller { fn intend(&mut self, body: &Body, resolution: &Resolution) -> Intent; }
pub struct DefaultController;   pub struct PlayerController;
```

Test scaffolding: `use crate::body_fields::seed_42;` gives `(World, WorldContext)`; `Session::start(&world, &PossessOpts::default())`; `s.handle("!wait 30")`. New integration tests go in `windows/vessel/tests/suite/` **and** need a `#[path]` line in `windows/vessel/tests/suite.rs` — a new top-level `tests/*.rs` fails `cli/tests/suite/test_binary_ratchet.rs`.

---

### Task 1: The two predicates and the possessor fold

**Files:**
- Modify: `windows/vessel/src/session.rs` — consts beside `TURNED_HOSTILE` (`:301-306`), registration in the block at `:955-992`, and a new free function
- Test: `windows/vessel/tests/suite/possession_facts.rs` + a `#[path]` line in `suite.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `session::POSSESSED_BY`, `session::POSSESSION_ENDED`,
  `session::possessor_of(ledger: &Ledger, body: EntityId) -> Option<EntityId>`,
  and `Session::possessor(&self) -> Option<EntityId>`.

- [ ] **Step 1: Write the failing test**

`windows/vessel/tests/suite/possession_facts.rs`:

```rust
//! The Coercion, Task 1: possession is an OPEN/CLOSE fact pair, because the
//! ledger is append-only and both terminators are events rather than
//! schedules. Sleep — the gate's only other row — sidesteps this by being
//! derived and self-terminating; a committed fact does not.

use crate::body_fields::seed_42;
use hornvale_kernel::{Fact, Value};
use hornvale_vessel::session::{POSSESSED_BY, POSSESSION_ENDED, possessor_of};
use hornvale_vessel::{PossessOpts, Session};

/// Commit a raw fact onto the session's ledger for test setup.
fn put(s: &mut Session, subject: hornvale_kernel::EntityId, predicate: &str, object: Value) {
    let fact = Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: None,
        provenance: "test".to_string(),
    };
    s.commit_for_test(fact);
}

#[test]
fn a_body_with_no_facts_has_no_possessor() {
    let (world, _ctx) = seed_42();
    let (s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert_eq!(s.possessor(), None);
}

#[test]
fn the_pair_opens_and_closes_and_reopens() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let body = s.agent_entity();
    let holder = s.other_body_entity_for_test();

    put(&mut s, body, POSSESSED_BY, Value::Entity(holder));
    assert_eq!(s.possessor(), Some(holder));

    put(&mut s, body, POSSESSION_ENDED, Value::Text("released".into()));
    assert_eq!(s.possessor(), None, "closed");

    put(&mut s, body, POSSESSED_BY, Value::Entity(holder));
    assert_eq!(
        s.possessor(),
        Some(holder),
        "a body may be possessed again after release — this is why the state is \
         a fold and not a single latest value"
    );
}
```

Add to `windows/vessel/tests/suite.rs` in alphabetical position:

```rust
#[path = "suite/possession_facts.rs"]
mod possession_facts;
```

**RESOLVED BEFORE DISPATCH — add NO test-only accessors.** The plan drafted three
`*_for_test` methods as a hedge. Verified against the tree, none is needed and two
already exist:

- **`Session` has no public ledger reader and no public commit**, and it should not
  gain one. `possessor_of` is a pure function over a `&Ledger`, so its own tests
  belong **in-module** (`#[cfg(test)] mod tests` in `session.rs`, which already
  holds 50 tests and can reach `self.ledger` directly). Move the fold's tests
  there and drop the `put` helper — commit `Fact`s straight onto the ledger.
- **`Session::bodies() -> &[Body]`** (`:1129`) already gives a second body:
  pick one whose index is not `driven`. No `other_body_entity_for_test`.
- **`Session::committed_fact_count() -> usize`** (`:1646`) is `self.ledger.len()`.
  Use it for the no-op-release assertion. No `ledger_for_test`.

Add ONE public read instead, which the later tasks want anyway:

```rust
    /// Who currently holds the driven body, if anyone (The Coercion) — the
    /// session-level read over [`possessor_of`]'s fold. `None` for a free body,
    /// which is every body until an imposition seam opens one.
    pub fn possessor(&self) -> Option<EntityId> {
        possessor_of(&self.ledger, self.agent_entity())
    }
```

The integration tests in this task use `s.possessor()`; the fold's own open/close/
reopen coverage lives in-module. Say in your report if anything here did not hold.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- possession_facts`
Expected: FAIL to compile — `POSSESSED_BY` and `possessor_of` unresolved.

- [ ] **Step 3: Add the predicate constants**

Beside `TURNED_HOSTILE` (`session.rs:301-306`), matching its shape exactly:

```rust
/// The entity currently holding this body (The Coercion). **NOT functional**,
/// unlike [`TURNED_HOSTILE`]: a body may be possessed, released, and possessed
/// again over its life, so the live state is [`possessor_of`]'s open/close fold
/// and never a single latest value. Committed only for an IMPOSED possession —
/// the player's own possession is the session's premise, not a world fact, so
/// an open `possessed-by` always means someone other than the player holds this
/// body (spec §3.1).
/// type-audit: bare-ok(identifier-text)
pub const POSSESSED_BY: &str = "possessed-by";

/// Closes the possession opened by the most recent [`POSSESSED_BY`] (The
/// Coercion). The object is the reason: `"released"` today, and `"died"` once
/// mortality exists — see the spec §6, which asserts the second is unreachable.
/// type-audit: bare-ok(identifier-text)
pub const POSSESSION_ENDED: &str = "possession-ended";
```

- [ ] **Step 4: Register them**

In the registration block (`session.rs:955-992`), following the existing calls' shape:

```rust
            .register_predicate(POSSESSED_BY, false, "the entity holding this body")?
            .register_predicate(POSSESSION_ENDED, false, "a possession ended, with its reason")?
```

Match the surrounding code's actual chaining and error handling — the block uses `.expect(...)` on a registration that "registers identically every session". Follow what is there rather than this sketch.

- [ ] **Step 5: Write the fold**

```rust
/// Who currently holds `body`, if anyone (The Coercion).
///
/// A single pass in LEDGER ORDER over the body's own facts: a
/// [`POSSESSED_BY`] opens, a [`POSSESSION_ENDED`] closes, and the last word
/// wins. Ledger order is the ordering — never a sort on `day`, which is
/// `Option<WorldTime>` and absent on facts that carry no instant.
pub fn possessor_of(ledger: &Ledger, body: EntityId) -> Option<EntityId> {
    let mut held = None;
    for fact in ledger.facts_about(body) {
        match fact.predicate.as_str() {
            POSSESSED_BY => {
                if let Value::Entity(who) = fact.object {
                    held = Some(who);
                }
            }
            POSSESSION_ENDED => held = None,
            _ => {}
        }
    }
    held
}
```

- [ ] **Step 6: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite -- possession_facts`
Expected: PASS, both tests.

- [ ] **Step 7: Mutation-prove the fold actually folds**

`mutate.py <file> <old> <new>` — the plain three-argument form. `--to` means something else entirely (it writes to a DIFFERENT file and leaves the original untouched, which is for mutating something executed from a path, not something imported).

```bash
python3 scripts/mutate.py windows/vessel/src/session.rs \
    'POSSESSION_ENDED => held = None,' \
    'POSSESSION_ENDED => {}'
```

Re-run the filter. Expected: **RED** on `the_pair_opens_and_closes_and_reopens`.

**Restore from a scratch copy — never `git checkout -- <file>`**, which reverts your uncommitted work along with the mutation. Then re-run and confirm green with a real recompile in the output; a restored mutation can leave a stale binary and that direction is a false GREEN.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/session.rs windows/vessel/tests/suite/possession_facts.rs windows/vessel/tests/suite.rs
git commit -m "feat(the-coercion): possessed-by and possession-ended, folded to a live possessor"
```

---

### Task 2: The gate row

**Files:**
- Modify: `windows/vessel/src/gate.rs` (all of it — 82 lines)
- Test: `windows/vessel/tests/suite/gate_table.rs` (append; the file already has a cross-product sweep)

**Interfaces:**
- Consumes: nothing from Task 1 — this task is pure gate vocabulary.
- Produces: `gate::BodyState::PossessedByAnother`.

**The compiler does most of this task.** `body_state_variants_must_all_be_rostered` and `verdict` are both exhaustive matches with **no wildcard arm**, deliberately, so adding the variant fails to compile until both are revisited. Do not add a `_` arm to silence it — that is the tripwire the metaplan built this table for.

- [ ] **Step 1: Write the failing test**

Append to `windows/vessel/tests/suite/gate_table.rs`:

```rust
/// The Coercion: the row's whole point. The name says `ByAnother` because the
/// state is relational in a way no other row is — `Awake`, `Asleep`, and the
/// spec's future `dead`/`blind` are true of the body regardless of who asks.
/// A flat `Possessed` would refuse the player's own acts on a body they hold.
#[test]
fn in_character_is_refused_while_possessed_by_another() {
    assert!(matches!(
        verdict(BodyState::PossessedByAnother, Mood::InCharacter),
        Verdict::Refused(_)
    ));
    assert_eq!(
        verdict(BodyState::PossessedByAnother, Mood::OutOfCharacter),
        Verdict::Permitted,
        "OOC is what lets you observe your own possession — if this refuses, \
         being possessed is indistinguishable from the game having hung"
    );
}

/// The refusal names the condition and NOT the possessor: the body does not
/// know who holds it (spec §3.4 — the ledger records the imposition, the body
/// has no introspective access to it).
#[test]
fn the_refusal_does_not_name_the_possessor() {
    let Verdict::Refused(reason) = verdict(BodyState::PossessedByAnother, Mood::InCharacter)
    else {
        panic!("in-character must be refused while possessed by another");
    };
    assert!(!reason.is_empty(), "a refusal fails loudly (decision 0007)");
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- gate_table`
Expected: FAIL to compile — no variant `PossessedByAnother`.

- [ ] **Step 3: Add the variant, and let the compiler find the rest**

In `gate.rs`, add to `BodyState`:

```rust
    /// The body is held by someone other than the player; an in-character act
    /// is refused. **Relational, unlike every other row** — `Awake` and
    /// `Asleep` are true of the body whoever asks, and this is not. The name
    /// carries the relation because the derivation cannot: the player has no
    /// ledger identity of its own to compare against (spec §3.1), so an open
    /// `possessed-by` fact always means someone else.
    PossessedByAnother,
```

Then run `cargo check -p hornvale-vessel` and fix **every** error it reports. There will be at least two — `BodyState::all` and `body_state_variants_must_all_be_rostered` — plus `verdict`'s match. Add:

```rust
        (BodyState::PossessedByAnother, Mood::InCharacter) => {
            Verdict::Refused("another will holds this body".to_string())
        }
        (BodyState::PossessedByAnother, Mood::OutOfCharacter) => Verdict::Permitted,
```

**Do not stop at the first error.** `cargo check` reports what it reaches; a run that fails early has enumerated nothing. Keep running it until it is clean, and say in your report how many sites it actually found.

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite -- gate_table`
Expected: PASS. The pre-existing `out_of_character_is_permitted_in_every_body_state` sweeps `BodyState::all()`, so it now covers the new row for free — confirm it does by checking `all()` includes the variant.

- [ ] **Step 5: Mutation-prove the sweep is not vacuous**

The pre-existing sweep only means something if `all()` really lists the new row.

```bash
python3 scripts/mutate.py windows/vessel/src/gate.rs \
    'vec![BodyState::Awake, BodyState::Asleep, BodyState::PossessedByAnother]' \
    'vec![BodyState::Awake, BodyState::Asleep]'
```

Re-run the filter. Expected: **RED**, or if it stays green, the sweep is not actually covering the new row and you should say so — an `all()` that silently omits a variant is exactly the vacuous-guard shape this project has shipped twice and caught twice.

Restore from a scratch copy, re-run, confirm green with a real recompile.

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/gate.rs windows/vessel/tests/suite/gate_table.rs
git commit -m "feat(the-coercion): BodyState::PossessedByAnother joins the gate table"
```

---

### Task 3: Wire the derivation, so the gate actually refuses

**Files:**
- Modify: `windows/vessel/src/session.rs:1910` (`fn body_state`)
- Test: `windows/vessel/tests/suite/possession_facts.rs` (append)

**Interfaces:**
- Consumes: `possessor_of` (Task 1), `BodyState::PossessedByAnother` (Task 2).
- Produces: no new public API; `fn body_state` changes behaviour and `refused_by_the_body` inherits it.

- [ ] **Step 1: Write the failing test**

Append to `windows/vessel/tests/suite/possession_facts.rs`:

```rust
#[test]
fn an_imposed_possession_refuses_in_character_and_permits_out_of_character() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let body = s.agent_entity();
    let holder = s.other_body_entity_for_test();

    // Baseline: an in-character verb works before anyone takes the body.
    let before = s.handle("look");
    assert!(matches!(before, hornvale_vessel::Turn::Out(ref t) if !t.is_empty()));

    put(&mut s, body, POSSESSED_BY, Value::Entity(holder));

    // In-character now refuses...
    let refused = match s.handle("look") {
        hornvale_vessel::Turn::Out(t) => t,
        hornvale_vessel::Turn::Released(t) => panic!("possession must not release: {t}"),
    };
    assert_ne!(
        refused,
        match before { hornvale_vessel::Turn::Out(t) => t, _ => unreachable!() },
        "an in-character verb must not behave identically once the body is held"
    );

    // ...and out-of-character still works, which is the whole point: without
    // it, being possessed is indistinguishable from the game having hung.
    let ooc = match s.handle("!whoami") {
        hornvale_vessel::Turn::Out(t) => t,
        hornvale_vessel::Turn::Released(t) => panic!("!whoami must not release: {t}"),
    };
    assert!(!ooc.is_empty(), "OOC must still answer while possessed");
}
```

`look` is used because it is in-character and commits nothing, so the test asserts on the gate rather than on a side effect. If `look` turns out to be OOC-only or otherwise unsuitable, pick another in-character verb from `IN_CHARACTER_VERBS` and say which in your report.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- possession_facts`
Expected: FAIL — the in-character verb still works, because nothing derives the new state yet.

- [ ] **Step 3: Teach `body_state` the new row**

`session.rs:1910`. **Order matters and is a real decision:** check possession BEFORE sleep. A body held by another is refused in-character whether or not it is also asleep, and reporting "you are asleep" to a player whose body has been taken would be actively misleading.

```rust
    fn body_state(&self) -> BodyState {
        // The Coercion: checked FIRST. A held body is refused in-character
        // regardless of whether it also happens to be asleep, and reporting
        // sleep to a player whose body was taken names the wrong condition.
        if possessor_of(&self.ledger, self.agent_entity()).is_some() {
            return BodyState::PossessedByAnother;
        }
        match self.wake_at {
            Some(wake) if self.day < wake => BodyState::Asleep,
            _ => BodyState::Awake,
        }
    }
```

- [ ] **Step 4: Run the whole vessel suite**

Run: `cargo test -p hornvale-vessel --test suite`
Expected: PASS.

**If a pre-existing test goes red, STOP and report the failing assertion verbatim.** Do not adjust the ordering above or weaken the new test to make it pass — a red here means the gate now fires somewhere it should not, which is a finding about the derivation, not a test to tune.

- [ ] **Step 5: Prove the ordering is deliberate, not incidental**

Add a test where the body is BOTH asleep and possessed, asserting the refusal names possession rather than sleep. Then mutate the order (move the possession check below the `match`) and confirm that test reddens. Restore from a scratch copy, re-run, confirm green.

If you cannot construct a both-asleep-and-possessed state through the shipped surface, say so — that is a reachability finding worth the same weight as the test, and this campaign's spec §6 already carries one of those.

- [ ] **Step 6: Check for artifact drift, and read the branch table rather than predicting**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

- **only `docs/audits/` moved** — expected, a `pub` boundary changed. Commit it here.
- **`book/src/gallery/` moved** — a transcript changed. Read the diff, confirm it is the gate firing, commit.
- **`clients/game/core/tests/fixtures/` or `cli/tests/fixtures/` moved** — STOP. Those are byte-goldens; `make rebaseline` does not write them and the chamber will not fix them. Report before doing anything.
- **nothing moved** — fine, and SAY SO. Do not read an empty diff as proof; `git diff --exit-code` is vacuous against an unindexed path.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate-commit
git add -A
git commit -m "feat(the-coercion): a held body refuses in-character verbs"
```

---

### Task 4: The imposed controller and the OOC seam

**Files:**
- Modify: `windows/vessel/src/controller.rs` (add `ImposedController`)
- Modify: `windows/vessel/src/session.rs` — new arms in `handle_ooc`'s match (arms live at `:2218+`)
- Test: `windows/vessel/tests/suite/possession_facts.rs` (append)

**Interfaces:**
- Consumes: Tasks 1-3.
- Produces: `controller::ImposedController`, and the OOC verbs that open and close a possession.

**Why the seam is out-of-character.** No creature can possess another today, and metaplan §5 defers that biology to a species-domain campaign. The metaplan independently argues OOC is the right channel: *"If a mind flayer takes your body, IC commands refuse; that is the point. But you are still there, watching. OOC is what lets you observe your own domination."* The observation channel and the imposition seam are the same channel, for this arc only.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_ooc_seam_opens_and_closes_a_possession() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let body = s.agent_entity();
    assert_eq!(s.possessor(), None, "starts free");

    let _ = s.handle("!possess-me");
    assert!(
        s.possessor().is_some(),
        "the seam must commit a possessed-by fact"
    );

    let _ = s.handle("!release-me");
    assert_eq!(
        s.possessor(),
        None,
        "release closes it — the possessor's option, spec section 6"
    );
}

/// Release is idempotent: closing an already-closed possession must not
/// commit a second `possession-ended`, or a body could accumulate unbounded
/// terminators for one opening.
#[test]
fn releasing_a_free_body_commits_nothing() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let before = s.committed_fact_count();
    let _ = s.handle("!release-me");
    assert_eq!(s.committed_fact_count(), before, "no fact for a no-op release");
}
```

**Verb names are a placeholder you must resolve, not accept.** `!possess-me` / `!release-me` read badly and the plan author is not the right person to name a player-facing verb. Pick names that fit the existing OOC roster's voice (read the arms at `session.rs:2218+`), use them consistently, and **state in your report what you chose and why**. Do not introduce `dominate`, `usurp`, `control` or `charm` — Global Constraints, spec §2.1.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- possession_facts`
Expected: FAIL — the verbs are unknown, so `handle_ooc` refuses them.

- [ ] **Step 3: Write `ImposedController`**

`windows/vessel/src/controller.rs`:

```rust
/// A controller supplied by whoever holds the body (The Coercion) — the
/// generalisation The Hand's spec predicted: "`driven: usize` generalises to a
/// controller map in Arc III without the body type changing."
///
/// It wraps another controller rather than inventing intents, because
/// decision 0168 puts the effect with the BODY and not the driver: an imposed
/// driver selects WHICH act, never WHAT the act does. Today it delegates to
/// [`DefaultController`], which is what makes a possessed body's fact trail
/// indistinguishable from a free one (spec §3.4) — a later campaign giving a
/// creature real intent swaps the inner controller and nothing else.
pub struct ImposedController {
    inner: DefaultController,
}
```

Implement `Controller` for it by delegating `intend` to `inner`. Add the doc comment on every public item.

- [ ] **Step 4: Add the OOC arms**

In `handle_ooc`'s match, following the shape of the arms at `:2218+`. The open arm commits a `POSSESSED_BY` fact; the close arm commits a `POSSESSION_ENDED` with `Value::Text("released")`, **guarded** so a no-op release commits nothing — follow `TURNED_HOSTILE`'s idempotence pattern (`session.rs:4136`), which guards on `Ledger::value_of` rather than a separate dedup flag.

The `possessed-by` object needs an entity to name as the holder. Use another body from the roster; if none is available in a given world, refuse with a legible reason rather than panicking (decision 0007: a refusal fails loudly).


**TWO THINGS TASK 1 LEARNED THAT BIND YOU.**

**(a) `Session::possessor()` is the public read** — Task 1 added it. There is no
public ledger accessor and you must not add one. `Session::committed_fact_count()`
(`:1646`) is `ledger.len()` for the no-op assertion.

**(b) COMMITTING AN IDENTICAL `Fact` TWICE IS A SILENT NO-OP.** `Ledger::commit`
narrows via `FactIndex::contains_full`, which does full `Fact` equality *including
`provenance`*. So a release-then-repossess whose second `possessed-by` matches the
first in every field — same subject, predicate, object, `place: None`, same `day`,
same provenance — **commits nothing at all**, and `possessor_of` correctly returns
`None` while your verb reports success. Task 1 hit this in a test and it cost it a
false RED that misdiagnosed a working fold.

Your verbs must therefore make each commit distinguishable. `day: Some(self.day)`
does it whenever the day has advanced; it does **not** when two verbs run on the
same day, which is exactly what a test does. Give the provenance strings something
that varies, and **write a test that possesses, releases, and re-possesses without
advancing the day** — if that reopen silently no-ops, the bug is live in the verb,
not just in a fixture.

- [ ] **Step 5: Run the tests**

Run: `cargo test -p hornvale-vessel --test suite`
Expected: PASS, whole crate.

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate-commit
git add -A
git commit -m "feat(the-coercion): an imposed controller, opened and closed out of character"
```

---

### Task 5: The preregistered readouts

**Files:**
- Test: `windows/vessel/tests/suite/coercion_calibration.rs` + a `#[path]` line in `suite.rs`

The four hypotheses are frozen in spec §7. **Do not edit that section.** Write one test per hypothesis, run them, report the numbers — including a falsified one, which is a finding rather than a failure.

Any `#[ignore]` must carry a reason naming a cost or citing a decision number.

**H1 and H2 are written out below because their instruments are knowable from outside the code. H3 and H4 name the property and leave the instrument to you** — deliberately. A plan author does not know which fact shapes a given seed emits, and both times a Hornvale plan prescribed a specific probe from outside, the prescribed probe was a null and the implementer found a discriminating one by reading.

- [ ] **Step 1: H1 — the gate's cross product**

```rust
#[test]
fn h1_the_new_row_refuses_in_character_and_permits_out_of_character() {
    // Frozen before the code: exactly one new (state, mood) pair is Refused.
    use hornvale_vessel::action::Mood;
    use hornvale_vessel::gate::{BodyState, Verdict, verdict};
    let refused = BodyState::all()
        .into_iter()
        .flat_map(|s| [Mood::InCharacter, Mood::OutOfCharacter].map(move |m| (s, m)))
        .filter(|(s, m)| matches!(verdict(*s, *m), Verdict::Refused(_)))
        .count();
    assert_eq!(refused, 2, "asleep+IC and possessed+IC, and nothing else");
}
```

If `Mood` or `BodyState::all` are not importable at those paths, adapt and say so.

**H1 IS ALSO A RATCHET ON `BodyState::all()`, AND THAT IS LOAD-BEARING.** Task 2
surfaced a pre-existing vacuous guard: `all()` (`gate.rs:46`) is a hand-written
`vec![]` with no compiler link to the enum, so the exhaustive matches catch an
*added* variant while nothing catches `all()` *omitting* one — mutation-proved
twice, the pre-existing sweep stays GREEN when a variant is dropped.

H1 closes that gap by accident of its shape: it counts over `all()` and asserts
an exact total, so an omission takes the count to 1 and reddens it.

Therefore: **do not "simplify" H1 into three direct `assert!(matches!(verdict(...)))`
lines.** That reads like a clarification and silently removes the only check on
`all()`'s completeness — the exact shape decision 0261 names, where the
cheapest-looking repair deletes the detector. Keep the count-over-`all()` form,
and say in its doc comment that it is doing two jobs.

**Mutation-prove both jobs:** flip the new `verdict` arm (H1 must redden), and
separately drop the variant from `all()` (H1 must also redden). Two mutations,
two reds, restored from a scratch copy each time.

- [ ] **Step 2: H2 — the death terminator is unreachable**

```rust
#[test]
fn h2_no_shipped_verb_can_end_a_possession_by_death() {
    // Frozen: a body cannot die today (spec section 6). This assertion is
    // DESIGNED TO TURN RED when mortality ships — that is the point of writing
    // it now rather than after. If it reddens, the arm is already correct and
    // the spec's section 6 wants updating; do not delete the test.
}
```

Fill it in by driving a session through every shipped verb and asserting no `possession-ended` carries `Text("died")`. **State the verb-roster size in the assertion message** — a null whose denominator is unstated is unreadable.

- [ ] **Step 3: H3 — the act trail is indistinguishable**

Name the property: a body driven by `ImposedController` and the same body driven by `DefaultController`, same seed, same tick span, commit facts differing only in *which* acts were chosen — never in shape, cost, or subject. Find the instrument. Since `ImposedController` currently delegates to `DefaultController`, consider whether the trails should be **byte-identical** rather than merely same-shaped, and if so assert that, which is far stronger.

- [ ] **Step 4: H4 — the null this campaign is prepared to report**

Count distinct fact shapes emitted under imposition versus free running, over a stated denominator. **If the sets are identical, that is the headline** — possession would be invisible in *consequence* and not merely in provenance, a stronger claim than spec §3.4 makes anywhere, and it would want its own decision record. Report the count either way. Do not tune anything to avoid it.

- [ ] **Step 5: Commit, and put all four numbers in your report**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "measure(the-coercion): the four preregistered readouts"
```

---

### Task 6: Definition of Done

**Files:**
- Create: `book/src/chronicle/the-coercion.md` + a `book/src/SUMMARY.md` line
- Create: `docs/retrospectives/the-coercion.md` + a `docs/retrospectives/README.md` line
- Create: three decision records from spec §8
- Modify: `book/src/frontier/idea-registry.md`, `book/src/open-questions.md`, and the metaplan

- [ ] **Step 1: Reserve the decision numbers**

```bash
make decision-block NAME=the-coercion
```

**Never `max+1`.** Numbers are reserved in blocks on the canonical box; a hand-picked next number collides silently with a parallel campaign's, and this repo's local tree routinely lags main's ceiling by several campaigns.

- [ ] **Step 2: Write the three decision records** from spec §8. A record's title must match its filename (`docs_consistency` enforces it).

- [ ] **Step 3: Chronicle entry**, name-only slug (decision 0026), wired into `SUMMARY.md`. **Carries no registry IDs and no process vocabulary** — `docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary` fails on them.

- [ ] **Step 4: Retrospective** — process lessons, not product, with a **"Deferred, with homes"** section naming a committed file per item. Read `.superpowers/sdd/decision-ledger.md` in full first: it is git-ignored, dies with this worktree, and contains the campaign's own record — including the spec amendment found by grepping `agent_entity` before writing Task 1.

- [ ] **Step 5: Update the metaplan's own table.** `docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md:164` renders a `dominated` row this campaign does not ship. Correct it to the shipped name and note the arc's word was settled by count. This is the one place the old word survives.

- [ ] **Step 6: Registry flips and the freshness sweep**

```bash
grep -rln 'possess\|gate table\|controller' book/src/ | grep -v chronicle
```

Re-read each hit, fix the lag, re-score any `open-questions.md` bet that moved (decision 0030). Add a registry row for the deferred **inverse-power duration** (spec §6) — a stronger creature harder to hold — carrying the measurement that motivates it if you have one.

- [ ] **Step 7: Regenerate, verify, commit, push**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
cargo test -p hornvale --test suite -- docs_consistency
make gate-commit
git add -A
git commit -m "docs(the-coercion): chronicle, retrospective, decisions, registry, sweep"
git push origin campaign/the-coercion
```

- [ ] **Step 8: STOP. Do not merge.**

The merge is the owner's decision and the controller submits it separately. Do not run any `sluice` command. Pushing the campaign branch is expected; pushing `main` is refused by a hook and would be wrong regardless.
