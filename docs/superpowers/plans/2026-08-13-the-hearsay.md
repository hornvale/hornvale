# The Hearsay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give decision 0100's *myth* register a channel — a derived, holder-bearing claim with a provenance grade — and measure whether apparent corroboration in a Hornvale world is independent or echoed.

**Architecture:** Two kernel types (`Provenance`, `Claim`) that are derived and never serialized, plus a new read-only window `windows/hearsay` that folds committed history facts into claim sets and computes stemmatic independence. Nothing draws from a seed; nothing is committed; no save-format contract is created.

**Tech Stack:** Rust 2024, workspace deps limited to `serde`/`serde_json`/`libm`, `cargo nextest` as the gate runner.

**Spec:** `docs/superpowers/specs/2026-08-13-the-hearsay-design.md`

## Global Constraints

- **No new external crates.** The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`: `serde`, `serde_json`, `libm` only.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only (`clippy.toml` `disallowed-types`, decision 0005).
- **No wall-clock time** — no `std::time::Instant` or `SystemTime`, in test code either. Time is `WorldTime`.
- **Every crate sets `#![warn(missing_docs)]`**; every `pub` item, field and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag** (`bare-ok(<class>)` / `waiver(<reason>)`).
- **Layering:** `kernel → domains/* → windows/* → cli`. A window may depend on domains and on other windows; nothing may depend on a window except windows and `cli`.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the most common review finding.
- **Float ordering uses `total_cmp`** with a deterministic tie-break; no `partial_cmp().unwrap()`.
- **Myth reads fact; fact never reads myth** (0100 rule 1). No task in this plan writes to `Ledger`, `Fact`, or the concept registry.

---

### Task 1: `Provenance` — the epistemic grade

**Files:**
- Create: `kernel/src/provenance.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod provenance;` and a re-export)
- Test: in-module `#[cfg(test)]` in `kernel/src/provenance.rs`

**Interfaces:**
- Consumes: nothing.
- Produces: `hornvale_kernel::Provenance` with variants `Witnessed`, `Taught`, `Inferred`, and `Provenance::on_transmission(self) -> Provenance`.

**Why this is in the kernel, not the window** (do not "simplify" it into `windows/hearsay`): campaign 2 introduces `domains/transmission`, and a domain may not depend on a window. See spec §3.1.

**Do not touch `domains/language`'s `Evidential`.** It is a *grammatical* category; this is the epistemic fact underneath it. Sharing three variant names is a coincidence of English. See spec §3.2.

- [ ] **Step 1: Write the failing test**

In a new `kernel/src/provenance.rs`, at the bottom:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn witnessing_downgrades_to_taught_when_retold() {
        assert_eq!(Provenance::Witnessed.on_transmission(), Provenance::Taught);
    }

    #[test]
    fn retelling_a_taught_claim_stays_taught() {
        assert_eq!(Provenance::Taught.on_transmission(), Provenance::Taught);
    }

    #[test]
    fn retelling_an_inference_stays_inferred() {
        assert_eq!(Provenance::Inferred.on_transmission(), Provenance::Inferred);
    }

    #[test]
    fn no_grade_ever_transmits_up_to_witnessed() {
        for g in [Provenance::Witnessed, Provenance::Taught, Provenance::Inferred] {
            let after = g.on_transmission();
            assert!(
                after != Provenance::Witnessed || g == Provenance::Witnessed,
                "{g:?} transmitted UP to Witnessed; hearsay is never testimony"
            );
        }
    }
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-kernel provenance`
Expected: FAIL — `kernel/src/provenance.rs` is not yet a module, or `Provenance` is undefined.

- [ ] **Step 3: Write the minimal implementation**

Above the test module in `kernel/src/provenance.rs`:

```rust
//! The epistemic grade a claim carries: how its holder came to hold it.
//!
//! This is the *fact* of provenance. Whether a tongue can grammatically
//! express it is a separate, language-domain question — see
//! `domains/language`'s `Evidential` and the frontier row
//! `KNOW-evidential-invisibility`, which turns on exactly that split.

/// How a holder came to hold a claim. Ordered by epistemic strength, and
/// transmission only ever moves DOWN it (see [`Provenance::on_transmission`]).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Provenance {
    /// The holder was present at the event.
    Witnessed,
    /// The holder was told, by someone who held it on some other grade.
    Taught,
    /// The holder derived it from other claims rather than being told.
    Inferred,
}

impl Provenance {
    /// The grade a hearer receives when a holder of `self` tells them.
    ///
    /// `Witnessed` becomes `Taught`: being told that someone saw a thing is
    /// not seeing it. The reverse transition does not exist, and that
    /// anti-symmetry is what makes a rumour decay across retellings instead
    /// of strengthening — see the frontier row `SOC-reputation-provenance`.
    pub fn on_transmission(self) -> Provenance {
        match self {
            Provenance::Witnessed => Provenance::Taught,
            other => other,
        }
    }
}
```

In `kernel/src/lib.rs`, add `pub mod provenance;` in alphabetical position (between `png` and `quantize`) and add `pub use provenance::Provenance;` to the re-export block.

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-kernel provenance`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/provenance.rs kernel/src/lib.rs
git commit -m "feat(kernel): Provenance, the epistemic grade a claim carries"
```

---

### Task 2: `Claim` — the myth register's unit

**Files:**
- Create: `kernel/src/claim.rs`
- Modify: `kernel/src/lib.rs`
- Test: in-module `#[cfg(test)]` in `kernel/src/claim.rs`

**Interfaces:**
- Consumes: `Provenance` (Task 1); `EntityId`, `Value` from `kernel::ledger`.
- Produces: `hornvale_kernel::Claim { holder, subject, predicate, object, grade, hops }` and `Claim::inherited_by(&self, holder: EntityId) -> Claim`.

**The load-bearing property is what this type does NOT do.** `Claim` derives no `Serialize`/`Deserialize`. Myth is *derived; free; evictable* (0100), so a serialized claim would be a committed balance, which 0100 rule 5 forbids. Do not add serde derives "for convenience" — that silently converts this campaign from zero-risk to a save-format change.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{EntityId, Value};

    fn eid(n: u64) -> EntityId {
        EntityId::new(n).expect("nonzero")
    }

    fn witnessed() -> Claim {
        Claim {
            holder: eid(1),
            subject: eid(99),
            predicate: "occ-ended".to_string(),
            object: Value::Number(63918.75),
            grade: Provenance::Witnessed,
            hops: 0,
        }
    }

    #[test]
    fn inheriting_a_claim_downgrades_its_grade() {
        let heir = witnessed().inherited_by(eid(2));
        assert_eq!(heir.grade, Provenance::Taught);
    }

    #[test]
    fn inheriting_a_claim_increments_hops() {
        assert_eq!(witnessed().inherited_by(eid(2)).hops, 1);
        assert_eq!(witnessed().inherited_by(eid(2)).inherited_by(eid(3)).hops, 2);
    }

    #[test]
    fn inheriting_a_claim_moves_the_holder_and_nothing_else() {
        let original = witnessed();
        let heir = original.inherited_by(eid(2));
        assert_eq!(heir.holder, eid(2));
        assert_eq!(heir.subject, original.subject);
        assert_eq!(heir.predicate, original.predicate);
        assert_eq!(heir.object, original.object);
    }
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-kernel claim`
Expected: FAIL — `Claim` is undefined.

- [ ] **Step 3: Write the minimal implementation**

```rust
//! A claim: what somebody holds to be true, in decision 0100's *myth*
//! register. Derived, never serialized, never contradiction-checked.

use crate::ledger::{EntityId, Value};
use crate::provenance::Provenance;

/// One claim held by one holder.
///
/// **This type is deliberately not `Serialize`.** 0100 puts myth in the
/// derived register — free, evictable, and not required to be coherent with
/// fact or with other myth — and rule 5 forbids committing a balance.
/// A claim set is recomputed from committed facts, never stored.
///
/// The `holder` field is 0100 rule 2 in the type system: a myth without a
/// holder is malformed, so there is no way to build one here.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(count: hops)
#[derive(Clone, Debug, PartialEq)]
pub struct Claim {
    /// Who holds this claim. Never optional (0100 rule 2).
    pub holder: EntityId,
    /// What the claim is about.
    pub subject: EntityId,
    /// The predicate asserted, named as the concept registry names it.
    pub predicate: String,
    /// The value asserted for (subject, predicate).
    pub object: Value,
    /// How this holder came to hold it.
    pub grade: Provenance,
    /// Retellings between the original witness and this holder. 0 = witness.
    pub hops: u32,
}

impl Claim {
    /// The claim as a new holder receives it: same content, downgraded grade,
    /// one more hop. Content is carried unchanged — distortion is deliberately
    /// out of scope for this campaign (spec §2).
    pub fn inherited_by(&self, holder: EntityId) -> Claim {
        Claim {
            holder,
            subject: self.subject,
            predicate: self.predicate.clone(),
            object: self.object.clone(),
            grade: self.grade.on_transmission(),
            hops: self.hops.saturating_add(1),
        }
    }
}
```

Add `pub mod claim;` and `pub use claim::Claim;` to `kernel/src/lib.rs`.

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-kernel claim`
Expected: PASS, 3 tests.

- [ ] **Step 5: Prove no world bytes moved**

Run: `cargo test -p hornvale --test lens_purity`
Expected: PASS. This is the world-identity guard — the seed-42 world's committed JSON. Adding derived types must not move a single byte. **If this goes red, stop and report:** it means something in Tasks 1–2 reached the committed path, which is an epoch-class event this campaign has no mandate for.

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/claim.rs kernel/src/lib.rs
git commit -m "feat(kernel): Claim, the myth register's holder-bearing unit"
```

---

### Task 3: `windows/hearsay` and the lineage reader

**Files:**
- Create: `windows/hearsay/Cargo.toml`, `windows/hearsay/src/lib.rs`, `windows/hearsay/src/lineage.rs`
- Create: `windows/hearsay/tests/common/mod.rs` (shared by all three test files — Tasks 4 and 5 declare `mod common;` rather than copying it)
- Test: `windows/hearsay/tests/lineage.rs`
- Regenerate: `book/src/reference/layering-generated.md`

**Interfaces:**
- Consumes: `hornvale_kernel::{Claim, Provenance}`; `hornvale_history::OCC_FOUNDED_FROM`.
- Produces: `hornvale_hearsay::lineage::{Lineage, lineage_of}` with `Lineage::parent(EntityId) -> Option<EntityId>`, `Lineage::ancestry(EntityId) -> Vec<EntityId>` (self first, root last), `Lineage::roots() -> &[EntityId]`.

**The one thing to get right.** `occ-founded-from` is a sum type
(`windows/worldgen/src/history_emit.rs:282`):

```
  Founding::Genesis(cell)  ->  Value::Number(cell_id)     NO parent: a ROOT
  Founding::From(entity)   ->  Value::Entity(parent_occ)  a real parent link
```

A `Number` is a *site id*, not an ancestor. Reading it as a parent counted 46
roots as edges during spec drafting. On seed 42 the truth is 46 roots and 658
real parent links.

- [ ] **Step 1: Write the failing test**

First the shared helper, `windows/hearsay/tests/common/mod.rs`. Tasks 4 and 5
reuse this file rather than copying it — three near-identical builders is a
defect a reviewer will flag once per copy.

```rust
//! Hand-built ledgers for the hearsay tests. A world build at Settlements
//! depth is minutes; these are milliseconds and pin the shape exactly.

use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// `EntityId` from a small integer.
pub fn eid(n: u64) -> EntityId {
    EntityId::new(n).expect("nonzero")
}

/// A ledger holding one `occ-founded-from` per entry: `Some(parent)` is a
/// `Founding::From` edge, `None` is a `Founding::Genesis` root (Number-valued,
/// a site id — NOT an ancestor).
pub fn ledger_with(chain: &[(u64, Option<u64>)]) -> Ledger {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_history::OCC_FOUNDED_FROM, true, "founding")
        .expect("register");
    let mut led = Ledger::default();
    for (child, parent) in chain {
        let object = match parent {
            Some(p) => Value::Entity(eid(*p)),
            None => Value::Number(7449.0),
        };
        led.commit(
            Fact {
                subject: eid(*child),
                predicate: hornvale_history::OCC_FOUNDED_FROM.to_string(),
                object,
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &reg,
        )
        .expect("commit");
    }
    led
}

/// Commit one extra fact onto an existing ledger, registering its predicate.
pub fn put(led: &mut Ledger, subject: u64, predicate: &str, object: Value) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(predicate, true, "test predicate")
        .expect("register");
    led.commit(
        Fact {
            subject: eid(subject),
            predicate: predicate.to_string(),
            object,
            place: None,
            day: None,
            provenance: "test".to_string(),
        },
        &reg,
    )
    .expect("commit");
}
```

Then `windows/hearsay/tests/lineage.rs`:

```rust
mod common;

use common::{eid, ledger_with};
use hornvale_hearsay::lineage::lineage_of;

#[test]
fn a_number_valued_founding_is_a_root_not_a_parent() {
    let led = ledger_with(&[(1, None)]);
    let lin = lineage_of(&led);
    assert_eq!(lin.parent(eid(1)), None, "Genesis(cell) is not a parent link");
    assert_eq!(lin.roots(), &[eid(1)]);
}

#[test]
fn an_entity_valued_founding_is_a_parent() {
    let led = ledger_with(&[(1, None), (2, Some(1))]);
    let lin = lineage_of(&led);
    assert_eq!(lin.parent(eid(2)), Some(eid(1)));
    assert_eq!(lin.roots(), &[eid(1)]);
}

#[test]
fn ancestry_runs_self_first_root_last() {
    let led = ledger_with(&[(1, None), (2, Some(1)), (3, Some(2))]);
    let lin = lineage_of(&led);
    assert_eq!(lin.ancestry(eid(3)), vec![eid(3), eid(2), eid(1)]);
}

#[test]
fn two_roots_stay_two_lineages() {
    let led = ledger_with(&[(1, None), (2, Some(1)), (10, None), (11, Some(10))]);
    let lin = lineage_of(&led);
    let mut roots = lin.roots().to_vec();
    roots.sort();
    assert_eq!(roots, vec![eid(1), eid(10)]);
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test lineage`
Expected: FAIL — the crate does not exist.

- [ ] **Step 3: Create the crate**

`windows/hearsay/Cargo.toml`:

```toml
[package]
name = "hornvale-hearsay"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale hearsay window: derive held claims from committed history."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-history = { path = "../../domains/history" }

# Task 6's heavy battery builds a real world. These are DEV-only on purpose:
# the library reads a ledger and nothing else, and a read-only window must not
# depend on the composition root at runtime.
[dev-dependencies]
hornvale-worldgen = { path = "../worldgen" }
hornvale-astronomy = { path = "../../domains/astronomy" }
hornvale-terrain = { path = "../../domains/terrain" }
```

The workspace picks it up automatically — `members = ["kernel", "domains/*", "windows/*", "cli"]`.

`windows/hearsay/src/lib.rs`:

```rust
//! The hearsay window: what a community holds to be true, derived from the
//! committed ledger and nothing else.
//!
//! Decision 0100 puts myth in the derived register and says it "has no channel
//! today". This crate is that channel's read side. It draws nothing, commits
//! nothing, and owns no seed labels — it is a window, not a domain.
#![warn(missing_docs)]

pub mod lineage;
```

- [ ] **Step 4: Implement the lineage reader**

`windows/hearsay/src/lineage.rs`:

```rust
//! The community tree, read from `occ-founded-from`.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeMap;

/// The founding tree: who was founded from whom.
#[derive(Clone, Debug, Default)]
pub struct Lineage {
    parent: BTreeMap<EntityId, EntityId>,
    roots: Vec<EntityId>,
}

impl Lineage {
    /// The occupation this one was founded from, or `None` for a root.
    pub fn parent(&self, of: EntityId) -> Option<EntityId> {
        self.parent.get(&of).copied()
    }

    /// Occupations founded at a site rather than from another community,
    /// ascending. Each is an independent origin by construction.
    pub fn roots(&self) -> &[EntityId] {
        &self.roots
    }

    /// `of` first, then each ancestor, root last. Cycle-guarded: the ledger
    /// cannot express a founding cycle, but a hand-built one can, and an
    /// infinite loop in a census metric is a very expensive way to find out.
    pub fn ancestry(&self, of: EntityId) -> Vec<EntityId> {
        let mut out = vec![of];
        let mut seen = std::collections::BTreeSet::new();
        seen.insert(of);
        let mut cursor = of;
        while let Some(next) = self.parent(cursor) {
            if !seen.insert(next) {
                break;
            }
            out.push(next);
            cursor = next;
        }
        out
    }
}

/// Read the founding tree out of a ledger.
///
/// `occ-founded-from` is a sum type: `Value::Entity` is a parent link,
/// `Value::Number` is `Founding::Genesis(cell)` — a SITE id, and therefore a
/// root with no ancestor. Reading a Number as a parent is the mistake this
/// function exists to make impossible.
pub fn lineage_of(ledger: &Ledger) -> Lineage {
    let mut out = Lineage::default();
    for fact in ledger.find(hornvale_history::OCC_FOUNDED_FROM) {
        match &fact.object {
            Value::Entity(p) => {
                out.parent.insert(fact.subject, *p);
            }
            _ => out.roots.push(fact.subject),
        }
    }
    out.roots.sort();
    out.roots.dedup();
    out
}
```

- [ ] **Step 5: Run the tests and verify they pass**

Run: `cargo test -p hornvale-hearsay --test lineage`
Expected: PASS, 4 tests.

- [ ] **Step 6: Regenerate the layering artifact**

Adding a crate changes the enforced dependency graph, and
`cli/tests/architecture.rs` renders that graph into
`book/src/reference/layering-generated.md` — a committed, drift-checked
artifact. Regenerate it in **this** commit; a generated artifact left for later
goes stale on someone else's commit.

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Decision rule for that diff:
- `book/src/reference/layering-generated.md` moved, nothing else → expected; stage it.
- `book/src/gallery/` moved → **STOP.** A world changed. That is an epoch event this campaign has no mandate for; report it.
- `docs/audits/type-audit-report.md` moved → expected if you added a `pub` primitive; stage it.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-hearsay --all-targets -- -D warnings
git add windows/hearsay book/src/reference/layering-generated.md docs/audits/type-audit-report.md
git commit -m "feat(hearsay): the window, and the founding tree read correctly"
```

---

### Task 4: DEFERRED TO CAMPAIGN 2 — do not implement

The independence test (`independent_witnesses`, stemmatics' *eliminatio
codicum descriptorum*) was specified here, approved at G3, and then found to
measure nothing on this campaign's substrate. Spec §5 carries the full finding.

The short version: claims reach holders by one route only — witness, then
inherit — so an event's holder set is a single subtree and the function returns
1 every time. Independence needs a claim to arrive by **two** routes, and two
routes is diffusion, which is this campaign's first non-goal. It is a scope
error, not a hard measurement.

**Nothing here is to be built.** The function is correct and campaign 2 will
want it verbatim; it is preserved in spec §7 and in the frontier row
`KNOW-independence`, which stays `spec'd` against this document. Task numbering
is left alone so the briefs, ledger and commits already referring to Tasks 5
and 6 keep pointing at the same work.


### Task 5: deriving claim sets from committed history

**Files:**
- Create: `windows/hearsay/src/derive.rs`
- Modify: `windows/hearsay/src/lib.rs`, `windows/hearsay/src/lineage.rs` (adds `descendants_of`)
- Test: `windows/hearsay/tests/derive.rs`

**Interfaces:**
- Consumes: `Lineage` (Task 3), `Claim`/`Provenance` (Tasks 1–2).
- Produces: `hornvale_hearsay::derive::claims_about(&Ledger, &Lineage, subject: EntityId, predicate: &str) -> Vec<Claim>`, ascending by holder.

**The two rules** (spec §4.3): an occupation present at an event holds it `Witnessed`; an occupation founded from a holder inherits it, downgraded, `hops + 1`. Content never changes — distortion is campaign 2.

- [ ] **Step 1: Write the failing test**

`windows/hearsay/tests/derive.rs`:

```rust
mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::derive::claims_about;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_kernel::ledger::{Ledger, Value};
use hornvale_kernel::provenance::Provenance;

/// 1 (root) ends on day 100; 2 founded from 1; 3 founded from 2.
fn world() -> Ledger {
    let mut led = ledger_with(&[(1, None), (2, Some(1)), (3, Some(2))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    led
}

#[test]
fn the_subject_of_an_event_witnessed_it() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let own = claims.iter().find(|c| c.holder == eid(1)).expect("self holds it");
    assert_eq!(own.grade, Provenance::Witnessed);
    assert_eq!(own.hops, 0);
}

#[test]
fn descendants_inherit_it_taught_with_growing_hops() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let two = claims.iter().find(|c| c.holder == eid(2)).expect("2 holds it");
    let three = claims.iter().find(|c| c.holder == eid(3)).expect("3 holds it");
    assert_eq!(two.grade, Provenance::Taught);
    assert_eq!(two.hops, 1);
    assert_eq!(three.grade, Provenance::Taught);
    assert_eq!(three.hops, 2);
}

#[test]
fn content_is_carried_unchanged() {
    let led = world();
    let lin = lineage_of(&led);
    for c in claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED) {
        assert_eq!(c.object, Value::Number(100.0), "distortion is campaign 2");
        assert_eq!(c.subject, eid(1));
    }
}

#[test]
fn an_unrelated_lineage_holds_nothing_about_it() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    assert!(claims.iter().all(|c| c.holder != eid(99)));
}
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-hearsay --test derive`
Expected: FAIL — `derive` module does not exist.

- [ ] **Step 3: Write the implementation**

`windows/hearsay/src/derive.rs`:

```rust
//! Turning committed facts into held claims.

use crate::lineage::Lineage;
use hornvale_kernel::ledger::{EntityId, Ledger};
use hornvale_kernel::provenance::Provenance;
use hornvale_kernel::Claim;
use std::collections::BTreeMap;

/// Every claim held about `(subject, predicate)`, ascending by holder.
///
/// Two rules, and no others (spec §4.3): the subject of a committed event
/// witnessed it, and an occupation founded from a holder inherits it at one
/// more hop and a downgraded grade. Nothing here draws, mutates content, or
/// consults anything but the ledger.
pub fn claims_about(
    ledger: &Ledger,
    lineage: &Lineage,
    subject: EntityId,
    predicate: &str,
) -> Vec<Claim> {
    let Some(object) = ledger.value_of(subject, predicate) else {
        return Vec::new();
    };
    let witness = Claim {
        holder: subject,
        subject,
        predicate: predicate.to_string(),
        object: object.clone(),
        grade: Provenance::Witnessed,
        hops: 0,
    };
    // Walk every occupation the lineage knows and keep those descending from
    // the witness. BTreeMap keeps the result ascending and deterministic.
    let mut held: BTreeMap<EntityId, Claim> = BTreeMap::new();
    held.insert(subject, witness.clone());
    for holder in lineage.descendants_of(subject) {
        let hops = lineage
            .ancestry(holder)
            .iter()
            .position(|a| *a == subject)
            .unwrap_or(0) as u32;
        let mut c = witness.clone();
        c.holder = holder;
        c.grade = witness.grade.on_transmission();
        c.hops = hops;
        held.insert(holder, c);
    }
    held.into_values().collect()
}
```

This needs one addition to `Lineage` in `windows/hearsay/src/lineage.rs`:

```rust
impl Lineage {
    /// Every occupation descending from `of`, ascending. Excludes `of` itself.
    pub fn descendants_of(&self, of: EntityId) -> Vec<EntityId> {
        let mut out: Vec<EntityId> = self
            .parent
            .keys()
            .copied()
            .filter(|k| *k != of && self.ancestry(*k).contains(&of))
            .collect();
        out.sort();
        out
    }
}
```

- [ ] **Step 4: Run the tests and verify they pass**

Run: `cargo test -p hornvale-hearsay --test derive`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-hearsay --all-targets -- -D warnings
git add windows/hearsay/src/derive.rs windows/hearsay/src/lineage.rs windows/hearsay/src/lib.rs windows/hearsay/tests/derive.rs
git commit -m "feat(hearsay): derive held claims from committed history"
```

---

### Task 6: transmission depth, measured on a real world

**Files:**
- Create: `windows/hearsay/tests/hop_depth_seed42.rs`
- Modify: `windows/hearsay/src/lib.rs`, `windows/hearsay/src/lineage.rs` (adds `all`), `windows/lab/src/metrics.rs`, `windows/lab/Cargo.toml`

**Interfaces:**
- Consumes: `claims_about` (Task 5), `Lineage` (Task 3).
- Produces: `hornvale_hearsay::hops_about(&Ledger, &Lineage, subject, predicate) -> Vec<u32>` and `hornvale_hearsay::median_hops(&Ledger, &Lineage, predicate) -> Option<f64>`, plus the lab metric `history-myth-hop-median`.

**What is being measured, and why it matters** (spec §6): nothing in this
campaign forgets, so the hop distribution is the **ceiling** on transmission
depth — the shape myth takes when nothing opposes it. Campaign 2's forgetting
is a shift against this curve. The prediction is frozen in spec §6 and was
committed before any measurement; do not adjust the population to move it.

**Measure the metric's cost BEFORE registering it.** Nine studies declare
`"metrics": "all"`, `study.rs` resolves `MetricSelection::All(_) => Ok(reg)`,
and `Metric` carries no opt-out flag, so a slow metric is a permanent
~2000-world cost. See `windows/lab/CLAUDE.md`.

- [ ] **Step 1: Add the two library functions**

In `windows/hearsay/src/lineage.rs`:

```rust
impl Lineage {
    /// Every occupation this tree knows — roots and descendants alike —
    /// ascending and deduplicated. Includes roots, so callers must not chain
    /// `roots()` onto it.
    pub fn all(&self) -> Vec<EntityId> {
        let mut out: Vec<EntityId> = self
            .parent
            .keys()
            .copied()
            .chain(self.parent.values().copied())
            .chain(self.roots.iter().copied())
            .collect();
        out.sort();
        out.dedup();
        out
    }
}
```

In `windows/hearsay/src/lib.rs`:

```rust
/// The hop count of every holder of a claim about `(subject, predicate)`.
/// Empty when the subject holds no such committed fact.
pub fn hops_about(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    subject: hornvale_kernel::ledger::EntityId,
    predicate: &str,
) -> Vec<u32> {
    derive::claims_about(ledger, lineage, subject, predicate)
        .iter()
        .map(|c| c.hops)
        .collect()
}

/// Median hop count over every (event, holder) pair in the world for
/// `predicate`, or `None` when there are no pairs at all.
///
/// The median of an even-length population takes the lower of the two central
/// values — a deterministic tie-break, never an average, so the result is
/// always an observed hop count and never an interpolated one.
pub fn median_hops(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    predicate: &str,
) -> Option<f64> {
    let mut hops: Vec<u32> = lineage
        .all()
        .into_iter()
        .flat_map(|s| hops_about(ledger, lineage, s, predicate))
        .collect();
    if hops.is_empty() {
        return None;
    }
    hops.sort_unstable();
    Some(f64::from(hops[hops.len() / 2]))
}
```

- [ ] **Step 2: Write the seed-42 battery, `#[ignore]`d into the heavy tier**

The ignore reason is compared **verbatim**, not by prefix
(`cli/tests/heavy_tier.rs`). Use exactly this string.

`windows/hearsay/tests/hop_depth_seed42.rs`:

```rust
//! The preregistered readout (spec §6). Live worldgen; heavy tier only.

use hornvale_hearsay::{hops_about, lineage::lineage_of, median_hops};

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn transmission_depth_on_seed_42_has_a_population_and_a_median() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let lin = lineage_of(&world.ledger);
    let mut all: Vec<u32> = Vec::new();
    for s in lin.all() {
        all.extend(hops_about(&world.ledger, &lin, s, hornvale_history::OCC_ENDED));
    }
    assert!(
        all.len() >= 500,
        "NO VERDICT: {} (event, holder) pairs, spec section 6 requires >= 500",
        all.len()
    );
    all.sort_unstable();
    let median = all[all.len() / 2];
    let tail = all.iter().filter(|h| **h >= 10).count() as f64 / all.len() as f64;
    println!(
        "hops: pairs={} median={median} tail_ge_10={tail:.4} max={}",
        all.len(),
        all[all.len() - 1]
    );
    assert_eq!(
        median_hops(&world.ledger, &lin, hornvale_history::OCC_ENDED),
        Some(f64::from(median)),
        "median_hops must agree with the battery's own computation"
    );
}
```

- [ ] **Step 3: Run it and record the real numbers**

Run: `cargo nextest run -p hornvale-hearsay --run-ignored all -E 'test(transmission_depth)' --no-capture`
Expected: PASS, with the printed line. **Paste the numbers verbatim into the
report** — they are the campaign's readout, and spec §6's four-branch decision
rule is applied to the median. All four branches are legitimate outcomes.

For reference, a Python pass over the committed seed-42 ledger gave
`pairs=5453 median=5 tail_ge_10=0.1883 max=21`. **If your figures differ, that
is a finding, not a nuisance** — report the difference rather than reconciling
it, because it would mean the Rust derivation and the direct ledger read
disagree about what a holder is.

- [ ] **Step 4: Register the metric, then gate it on measured cost**

**Read this ordering note before doing anything.** An earlier draft told you to
measure the metric's cost *before* registering it. That is impossible: a lab
study can only name a metric that is already in `registry()`, and the cost
cannot be timed inside a test instead because `std::time::Instant` is banned in
workspace test code by `clippy.toml`. So the order is register → probe →
**revert the registration if it is too slow**. Nothing slow ever reaches a
commit, which is what the gate was for.

First add the dependency to `windows/lab/Cargo.toml`:

```toml
hornvale-hearsay = { path = "../hearsay" }
```

Then, in `windows/lab/src/metrics.rs`'s `registry()`, in the `Domain::History`
family block (beside `vestige-density`, whose shape this matches):

```rust
Metric {
    name: "history-myth-hop-median",
    doc: "Median inheritance depth of a held claim about a historical ending: \
          0 = the witnessing community itself, higher = the claim is carried by \
          communities further down the founding tree. The no-decay ceiling on \
          transmission depth (The Hearsay, spec section 6)",
    summary: SummaryKind::Numeric {
        bucket_edges: &[1.0, 2.0, 3.0, 5.0, 8.0],
    },
    domain: Domain::History,
    role: Role::Descriptor,
    extract: Extractor::Full(|v: &FullView| {
        let ledger = &v.world().ledger;
        let lin = hornvale_hearsay::lineage::lineage_of(ledger);
        match hornvale_hearsay::median_hops(ledger, &lin, hornvale_history::OCC_ENDED) {
            // Absent, never a sentinel: a world with no held claim has no
            // median, and 0.0 would read as "every claim is first-hand".
            None => MetricValue::Absent,
            Some(m) => MetricValue::Number(m),
        }
    }),
},
```

Now probe the cost on three worlds:

```bash
cat > /tmp/hv-cost.study.json <<'JSON'
{ "name": "hearsay-cost-probe", "description": "cost probe, throwaway",
  "seeds": [42, 43, 44], "pin_sets": [], "metrics": ["history-myth-hop-median"] }
JSON
time cargo run --release -p hornvale -- lab run /tmp/hv-cost.study.json
```

Subtract the world-build cost, which dominates and is not yours: run the same
study again with `"metrics": ["settlement-count"]` (an existing cheap metric)
and take the difference. **Report both numbers**, not just the difference.

Decision rule on the *difference* in per-world wall time:

```
  < 0.5 s/world   -> KEEP the registration. The census absorbs it.
  0.5-3 s/world   -> KEEP it, and state the projected census cost in the
                     commit message: multiply by ~2000 worlds AND by the nine
                     studies that declare "metrics": "all".
  > 3 s/world     -> REVERT the registration (git checkout the two lab files).
                     Keep the heavy battery, report the readout from there, and
                     say so plainly in your report. The Mire declined three
                     metrics at ~3.5 s/world for exactly this reason. This is
                     a legitimate outcome, not a failure.
```


Then `make rebaseline` and review the drift. **Expect `book/src/laboratory/`
to move if you kept the metric** — a new metric changes every
`"metrics": "all"` study's schema, and the calibration binaries go red until
the census fixtures refresh on lefford. That refresh is a G6 close activity
requiring Nathan's authorization, not this task's.

- [ ] **Step 5: Regenerate artifacts, then commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/hearsay windows/lab/src/metrics.rs windows/lab/Cargo.toml book/src/laboratory book/src/reference docs/audits
git commit -m "feat(hearsay): transmission depth, measured on seed 42"
```


## Close (G6 — hard stop, do not self-approve)

- [ ] `make gate` green on the branch. Stagger it: three other campaigns share this box, and two concurrent gates cost ~30 min each rather than 15.
- [ ] `make gate-full` for the heavy tier, dispatched with `make heavy-remote REF=<full-sha>` — it is an authoring path and carries a canonical-host guard.
- [ ] Census refresh on lefford **only if** Task 6 registered a metric (`ssh lefford … scripts/census-run.sh`, full SHA, never a branch name). Requires Nathan's explicit authorization.
- [ ] Chronicle entry, book freshness sweep, retrospective in `docs/retrospectives/`, plus a one-line row in `docs/retrospectives/README.md`'s index.
- [ ] Re-score `book/src/open-questions.md` if the readout moved a Confidence Gradient bet.
- [ ] Flip `KNOW-independence` to `shipped`; file whatever campaign 2 inherited.
