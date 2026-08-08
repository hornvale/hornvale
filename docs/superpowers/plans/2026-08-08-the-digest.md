# The Digest Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generate `make doctor`'s self-map and an in-force decision index from a compacted, non-`World` fact ledger, so neither can drift from the source it describes.

**Architecture:** A crate at `tools/digest/`, outside the cargo workspace (precedent: `tools/type-audit/`, decisions 0027/0028), path-depending on `hornvale-kernel` for `Fact`/`ConceptRegistry`. Authored facts are asserted into a JSONL store; functional predicates make assertion *replace* its predecessor, which is the compaction. Derived facts are scanned from source at render time and never stored. Views regenerate through `scripts/regenerate-artifacts.sh` and drift-check with `git diff --exit-code`.

**Tech Stack:** Rust 2024, `serde`/`serde_json`, `hornvale-kernel` (path dep). No workspace membership, so `ALLOWED_EXTERNAL` does not bind this crate — but do not add dependencies beyond `serde`/`serde_json` without a decision.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-08-08-the-digest-design.md`. Where this plan and the spec disagree, the spec governs.
- **`tools/digest/Cargo.toml` MUST contain an empty `[workspace]` table.** Without it, a checkout nested under `.claude/worktrees/` makes cargo walk past this package's own workspace root and bind to the outer one. `tools/type-audit/Cargo.toml` documents this; the campaign itself runs in such a worktree.
- **Add `"tools/digest"` to the root `Cargo.toml` `exclude` list**, alongside `tools/type-audit`.
- **The ledger carries no time.** `Fact.place` and `Fact.day` are **always `None`**. Never store a date, a commit SHA, or any wall-clock value. Project time is git's. This is the constitutional premise of the whole campaign (spec §4.2).
- **Serialization is JSONL, one `Fact` per line, stable-ordered by `(subject, predicate)`.** A single assertion must produce a one-line diff. `Fact` derives `Serialize`/`Deserialize` but **not** `Ord` (it holds an `f64`), so sorting requires an explicit comparator.
- **`EntityId` wraps `NonZeroU64`.** Project entity ids start at 1.
- Rust edition 2024. `#![warn(missing_docs)]`; every public item gets a one-line doc comment.
- Run `cargo fmt` as the final step before every commit. Formatting-gate skips are this repo's most common review finding.
- Commit messages end with: `Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU`

## File Structure

| File | Responsibility |
|---|---|
| `tools/digest/Cargo.toml` | Package manifest; empty `[workspace]`; path dep on kernel |
| `tools/digest/src/lib.rs` | Crate root, module wiring, docs |
| `tools/digest/src/store.rs` | `ProjectLedger`: assert with functional-replace, JSONL read/write |
| `tools/digest/src/vocabulary.rs` | The project `ConceptRegistry` and its predicates |
| `tools/digest/src/scan/decisions.rs` | Parse `docs/decisions/*.md` into facts |
| `tools/digest/src/scan/capability.rs` | Derive `ALLOWED_EXTERNAL` and the layer list from source |
| `tools/digest/src/render/decisions.rs` | The in-force decision index |
| `tools/digest/src/render/doctor.rs` | The repo self-map |
| `tools/digest/src/render/delta.rs` | The intent≠reality report |
| `tools/digest/src/main.rs` | CLI: `scan`, `render <view>`, `assert`, `check` |
| `docs/digest/facts.jsonl` | The committed store (asserted facts only) |

---

### Task 1: The crate skeleton and the compacting store

**Files:**
- Create: `tools/digest/Cargo.toml`, `tools/digest/src/lib.rs`, `tools/digest/src/store.rs`
- Modify: `Cargo.toml` (root — add `"tools/digest"` to `exclude`)

**Interfaces:**
- Consumes: `hornvale_kernel::ledger::{Fact, Value, EntityId}`, `hornvale_kernel::registry::ConceptRegistry`
- Produces: `ProjectLedger::new(ConceptRegistry) -> Self`, `ProjectLedger::assert(&mut self, Fact) -> Result<(), StoreError>`, `ProjectLedger::facts(&self) -> &[Fact]`

- [ ] **Step 1: Write the failing test**

Create `tools/digest/src/store.rs` with only the test module:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::ledger::{EntityId, Value};
    use std::num::NonZeroU64;

    fn eid(n: u64) -> EntityId {
        EntityId(NonZeroU64::new(n).expect("nonzero"))
    }

    fn registry() -> hornvale_kernel::registry::ConceptRegistry {
        let mut r = hornvale_kernel::registry::ConceptRegistry::default();
        r.register_predicate("status", true, "a decision's current status")
            .expect("register");
        r.register_predicate("supersedes", false, "this decision supersedes another")
            .expect("register");
        r
    }

    #[test]
    fn asserting_a_functional_predicate_replaces_its_predecessor() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(1), "status", Value::Text("accepted".into())))
            .expect("first assert");
        led.assert(fact(eid(1), "status", Value::Text("superseded".into())))
            .expect("second assert");
        assert_eq!(led.facts().len(), 1, "functional predicate must compact");
        assert_eq!(
            led.facts()[0].object,
            Value::Text("superseded".into()),
            "the later assertion wins"
        );
    }

    #[test]
    fn a_non_functional_predicate_accumulates() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(1), "supersedes", Value::Text("0026".into())))
            .expect("a");
        led.assert(fact(eid(1), "supersedes", Value::Text("0043".into())))
            .expect("b");
        assert_eq!(led.facts().len(), 2, "non-functional predicates accumulate");
    }

    #[test]
    fn an_unregistered_predicate_is_rejected() {
        let mut led = ProjectLedger::new(registry());
        let err = led
            .assert(fact(eid(1), "invented", Value::Flag(true)))
            .expect_err("unregistered predicate must be rejected");
        assert!(matches!(err, StoreError::UnknownPredicate(_)));
    }

    #[test]
    fn a_fact_carrying_time_is_rejected() {
        let mut led = ProjectLedger::new(registry());
        let mut f = fact(eid(1), "status", Value::Text("accepted".into()));
        f.day = Some(1.0);
        let err = led.assert(f).expect_err("time must be rejected");
        assert!(matches!(err, StoreError::TimeIsGits));
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml`
Expected: FAIL — `ProjectLedger`, `StoreError`, and `fact` are not defined.

- [ ] **Step 3: Write the manifest and root**

`tools/digest/Cargo.toml`:

```toml
[package]
name = "digest"
version = "0.1.0"
edition = "2024"
license = "MIT"
publish = false

# Empty table: this package is deliberately outside the main workspace
# (spec §4.6; precedent decisions 0027/0028). An empty [workspace] here (not
# just the main Cargo.toml's `exclude`) stops cargo's ancestor search at this
# manifest unconditionally — without it, a checkout nested under another Cargo
# workspace (e.g. a worktree under `.claude/worktrees/`) makes cargo walk past
# this package's own workspace root and bind to the outer one.
[workspace]

[lib]
name = "digest"
path = "src/lib.rs"

[[bin]]
name = "digest"
path = "src/main.rs"

[dependencies]
hornvale-kernel = { path = "../../kernel" }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
```

`tools/digest/src/lib.rs`:

```rust
//! The Digest — the project's compacted knowledge of itself.
//!
//! A non-`World` fact ledger in `hornvale_kernel`'s shape. Asserting a fact
//! whose predicate is *functional* replaces its predecessor, so the committed
//! store always holds only what is currently true; superseded facts leave the
//! artifact and survive in git alone (spec §4.3, decision 0088).
//!
//! The ledger carries NO time. `Fact::place` and `Fact::day` are always
//! `None`; project time is git's (spec §4.2).
#![warn(missing_docs)]

pub mod store;
```

Add `"tools/digest"` to the root `Cargo.toml` `exclude` array, which becomes:

```toml
exclude = ["tools/type-audit", "tools/earth-mask", "tools/digest", "clients/vessel/wasm", "clients/world-wasm"]
```

- [ ] **Step 4: Write the minimal store**

Prepend to `tools/digest/src/store.rs`:

```rust
//! The compacting fact store.

use hornvale_kernel::ledger::{EntityId, Fact, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// Why an assertion was refused.
#[derive(Debug, PartialEq)]
pub enum StoreError {
    /// The predicate is not in the project vocabulary.
    UnknownPredicate(String),
    /// The fact carried a `place` or `day`. Project time is git's (spec §4.2).
    TimeIsGits,
}

/// Build a project fact. `place` and `day` are always `None` by construction.
pub fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: None,
        provenance: "asserted".to_string(),
    }
}

/// A compacted, time-free ledger of facts about the project.
pub struct ProjectLedger {
    registry: ConceptRegistry,
    facts: Vec<Fact>,
}

impl ProjectLedger {
    /// A ledger over the given project vocabulary.
    pub fn new(registry: ConceptRegistry) -> Self {
        Self {
            registry,
            facts: Vec::new(),
        }
    }

    /// Assert a fact. A *functional* predicate replaces any existing fact for
    /// the same `(subject, predicate)` — that replacement IS the compaction.
    /// A non-functional predicate accumulates.
    pub fn assert(&mut self, f: Fact) -> Result<(), StoreError> {
        if f.place.is_some() || f.day.is_some() {
            return Err(StoreError::TimeIsGits);
        }
        let def = self
            .registry
            .predicate(&f.predicate)
            .ok_or_else(|| StoreError::UnknownPredicate(f.predicate.clone()))?;
        if def.functional {
            self.facts
                .retain(|e| !(e.subject == f.subject && e.predicate == f.predicate));
        }
        self.facts.push(f);
        Ok(())
    }

    /// Every fact currently in force.
    pub fn facts(&self) -> &[Fact] {
        &self.facts
    }
}
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test --manifest-path tools/digest/Cargo.toml`
Expected: PASS, 4 tests.

- [ ] **Step 6: Confirm the workspace is undisturbed**

Run: `cargo metadata --format-version 1 --no-deps > /dev/null && echo WORKSPACE_OK`
Expected: `WORKSPACE_OK`. This proves `tools/digest` did not join the workspace.

- [ ] **Step 7: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest Cargo.toml
git commit -m "feat(digest): the compacting, time-free project fact store

Asserting a functional predicate replaces its predecessor — that
replacement is the compaction (spec 4.3). A fact carrying place or day
is rejected outright: project time is git's (spec 4.2).

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 2: JSONL serialization with a proven one-line diff

**Files:**
- Modify: `tools/digest/src/store.rs`

**Interfaces:**
- Produces: `ProjectLedger::to_jsonl(&self) -> String`, `ProjectLedger::from_jsonl(&str, ConceptRegistry) -> Result<Self, StoreError>`

This task proves **spec §6 S3**: a single assertion produces a one-line diff. That is the whole justification for compaction, because 0088 rules that a store whose rows churn on noise is not archaeology.

- [ ] **Step 1: Write the failing test**

Add to `tools/digest/src/store.rs`'s test module:

```rust
    #[test]
    fn jsonl_is_stable_ordered_by_subject_then_predicate() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(2), "status", Value::Text("b".into()))).unwrap();
        led.assert(fact(eid(1), "supersedes", Value::Text("x".into()))).unwrap();
        led.assert(fact(eid(1), "status", Value::Text("a".into()))).unwrap();
        // Bind the String first: `led.to_jsonl().lines().collect()` borrows
        // from a temporary whose drop scope ends at the `let`, which is E0716.
        let text = led.to_jsonl();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
        assert!(lines[0].contains("\"subject\":1") && lines[0].contains("\"status\""));
        assert!(lines[1].contains("\"subject\":1") && lines[1].contains("\"supersedes\""));
        assert!(lines[2].contains("\"subject\":2"));
    }

    #[test]
    fn replacing_one_fact_changes_exactly_one_line() {
        let mut led = ProjectLedger::new(registry());
        for n in 1..=20u64 {
            led.assert(fact(eid(n), "status", Value::Text("accepted".into())))
                .unwrap();
        }
        let before: Vec<String> = led.to_jsonl().lines().map(str::to_string).collect();

        led.assert(fact(eid(7), "status", Value::Text("superseded".into())))
            .unwrap();
        let after: Vec<String> = led.to_jsonl().lines().map(str::to_string).collect();

        assert_eq!(before.len(), after.len(), "compaction must not grow the file");
        let changed = before.iter().zip(&after).filter(|(a, b)| a != b).count();
        assert_eq!(changed, 1, "a single assertion must be a one-line diff (S3)");
    }

    #[test]
    fn jsonl_round_trips() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(1), "status", Value::Text("accepted".into()))).unwrap();
        let text = led.to_jsonl();
        let back = ProjectLedger::from_jsonl(&text, registry()).expect("round trip");
        assert_eq!(back.facts(), led.facts());
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml`
Expected: FAIL — `to_jsonl` / `from_jsonl` not defined.

- [ ] **Step 3: Implement**

Add to `impl ProjectLedger`:

```rust
    /// Serialize as JSONL, one fact per line, stable-ordered by
    /// `(subject, predicate)`. `Fact` has no `Ord` (it holds an `f64`), so the
    /// comparator is explicit. Stable order is what keeps a single assertion a
    /// one-line diff, which is what keeps `git log -p` readable (decision 0088).
    pub fn to_jsonl(&self) -> String {
        let mut sorted: Vec<&Fact> = self.facts.iter().collect();
        sorted.sort_by(|a, b| {
            a.subject
                .cmp(&b.subject)
                .then_with(|| a.predicate.cmp(&b.predicate))
        });
        let mut out = String::new();
        for f in sorted {
            out.push_str(&serde_json::to_string(f).expect("Fact serializes"));
            out.push('\n');
        }
        out
    }

    /// Parse JSONL produced by [`ProjectLedger::to_jsonl`].
    pub fn from_jsonl(text: &str, registry: ConceptRegistry) -> Result<Self, StoreError> {
        let mut led = Self::new(registry);
        for line in text.lines().filter(|l| !l.trim().is_empty()) {
            let f: Fact = serde_json::from_str(line).map_err(|_| StoreError::Malformed)?;
            led.assert(f)?;
        }
        Ok(led)
    }
```

Add the variant to `StoreError`:

```rust
    /// A JSONL line did not parse as a `Fact`.
    Malformed,
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml`
Expected: PASS, 7 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest
git commit -m "feat(digest): JSONL with a proven one-line diff (S3)

Stable order by (subject, predicate) with an explicit comparator — Fact
has no Ord because it holds an f64. The one-line-diff test is the
measurement 0088's churn corollary demands, not an assumption.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 3: The project vocabulary

**Files:**
- Create: `tools/digest/src/vocabulary.rs`
- Modify: `tools/digest/src/lib.rs` (add `pub mod vocabulary;`)

**Interfaces:**
- Produces: `vocabulary::project_registry() -> ConceptRegistry`

Predicate names are governed by spec §11: **no epoch suffix, but a rename is its own commit touching nothing else.** Put that rule in the module doc so the next reader finds it.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_v1_predicate_is_registered_with_the_right_arity() {
        let r = project_registry();
        for (name, functional) in [
            ("decision-title", true),
            ("decision-status", true),
            ("superseded-by", true),
            ("supersession-scope", true),
            ("intends", false),
            ("provides", false),
            ("archival", true),
        ] {
            let def = r
                .predicate(name)
                .unwrap_or_else(|| panic!("{name} must be registered"));
            assert_eq!(def.functional, functional, "{name} arity");
            assert!(!def.doc.is_empty(), "{name} needs a doc");
        }
    }

    #[test]
    fn supersession_scope_is_functional_so_it_compacts() {
        // 0026 is superseded FOR DECISION RECORDS ONLY; its registry-row
        // provision still stands. The scope must be replaceable in place.
        let r = project_registry();
        assert!(r.predicate("supersession-scope").expect("registered").functional);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml vocabulary`
Expected: FAIL — `project_registry` not defined.

- [ ] **Step 3: Implement**

```rust
//! The project vocabulary.
//!
//! Predicate names carry **no epoch suffix** (unlike world stream labels,
//! decision 0006 — those version because a rename corrupts every world; these
//! corrupt nothing). But a rename rewrites every fact carrying the predicate,
//! producing a whole-file diff that destroys `git log -p` for that compaction.
//! So: **a predicate rename is its own commit, touching nothing else**
//! (spec §11).

use hornvale_kernel::registry::ConceptRegistry;

/// The v1 project vocabulary.
pub fn project_registry() -> ConceptRegistry {
    let mut r = ConceptRegistry::default();
    for (name, functional, doc) in [
        ("decision-title", true, "a decision's title line"),
        ("decision-status", true, "accepted | proposed | superseded"),
        ("superseded-by", true, "the decision that supersedes this one"),
        (
            "supersession-scope",
            true,
            "which provisions the supersession covers; absent means all of them",
        ),
        ("intends", false, "a rule the project intends to hold"),
        ("provides", false, "a capability a crate provides"),
        ("archival", true, "this document is history, never governing"),
    ] {
        r.register_predicate(name, functional, doc)
            .expect("v1 vocabulary registers cleanly");
    }
    r
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml vocabulary`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest
git commit -m "feat(digest): the v1 project vocabulary

supersession-scope is functional and separate from superseded-by,
because 0026 is superseded for decision records only and its
registry-row provision still stands.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 4: The decision scanner

**Files:**
- Create: `tools/digest/src/scan/mod.rs`, `tools/digest/src/scan/decisions.rs`
- Modify: `tools/digest/src/lib.rs` (add `pub mod scan;`)

**Interfaces:**
- Consumes: `store::fact`, `vocabulary::project_registry`
- Produces: `scan::decisions::parse(id: &str, text: &str) -> DecisionRecord` where `pub struct DecisionRecord { pub id: String, pub title: String, pub status: Status, pub superseded_by: Option<String> }` and `pub enum Status { Accepted, Proposed, Superseded }`

**AMENDED 2026-08-08 (Nathan's ruling, ledger Task 4).** `scope` is NOT scanned. The trailing parenthetical after "Superseded by" carries three different meanings in the live corpus — a rationale (0063), a genuine scope (0043), and a date (0099) — and they are not syntactically distinguishable. Spec §4.4 already places supersession in the **asserted** column; the scanner extracts `superseded-by` only, and `supersession-scope` is an authored fact. **Decision entities are keyed `EntityId(n)` where `n` is the decision's number** (0026 → `EntityId(26)`), which is deterministic and needs no id table.

**All four real status formats present in the corpus must parse.** Measured at `64e8c667`: 107 `Accepted`, 2 `Proposed`, and 4 superseded across the forms `Superseded by 0063`, `Superseded by 0043 (…)`, `Superseded by [0099]`, and a bare `Superseded by` continuing on the next line.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_an_accepted_decision() {
        let text = "# 0110. The census is the suite's shared world-building pass\n\n\
                    **Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan\n";
        let d = parse("0110", text);
        assert_eq!(d.title, "The census is the suite's shared world-building pass");
        assert_eq!(d.status, Status::Accepted);
        assert_eq!(d.superseded_by, None);
    }

    #[test]
    fn parses_a_plain_supersession() {
        let text = "# 0029. CI checks 500-seed censuses\n\n\
                    **Status:** Superseded by 0063 · **Decider:** Nathan\n";
        let d = parse("0029", text);
        assert_eq!(d.status, Status::Superseded);
        assert_eq!(d.superseded_by.as_deref(), Some("0063"));
    }

    #[test]
    fn parses_a_bracketed_supersession() {
        let text = "# 0098. Hornvale is single-player\n\n\
                    **Status:** Superseded by [0099](0099-worlds-are-version-locked.md)\n";
        let d = parse("0098", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0099"));
    }

    #[test]
    fn a_trailing_parenthetical_is_never_read_as_scope() {
        // THE case this campaign exists for: 0026 reads as superseded to any
        // grepping reader, but its registry-row provision still stands, and
        // the registry violates it 1,402 times. The SCANNER must not try to
        // infer that — the same slot holds a rationale in 0063 and a date in
        // 0099. Scope is asserted, not scanned (spec §4.4).
        let text = "# 0026. Slugs, not numbers\n\n\
                    **Status:** Superseded by 0043 (for decision records; the \
                    study/chronicle/registry-row provisions stand) · **Decider:** Nathan\n";
        let d = parse("0026", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0043"));
    }

    #[test]
    fn a_date_parenthetical_does_not_corrupt_the_superseder_id() {
        // 0099's form: a bracket link followed by a DATE parenthetical.
        let text = "# 0082. A thing\n\n\
                    **Status:** Superseded by [0099](0099-worlds-are-version-locked.md) (2026-08-04) ·\n";
        let d = parse("0082", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0099"));
    }

    #[test]
    fn a_rationale_parenthetical_does_not_corrupt_the_superseder_id() {
        // 0063's form: a prose rationale, not a scope.
        let text = "# 0029. CI checks 500-seed censuses\n\n\
                    **Status:** Superseded by 0063 (The Local Census made the full census a ~7-min\n\
                    local run) · **Decider:** Nathan\n";
        let d = parse("0029", text);
        assert_eq!(d.superseded_by.as_deref(), Some("0063"));
    }

    #[test]
    fn every_committed_decision_parses() {
        let dir = repo_root().join("docs/decisions");
        let mut n = 0;
        for entry in std::fs::read_dir(&dir).expect("decisions dir") {
            let path = entry.expect("entry").path();
            let name = path.file_name().expect("name").to_string_lossy().to_string();
            if !name.ends_with(".md") || name == "README.md" {
                continue;
            }
            let id = name.split('-').next().expect("id prefix").to_string();
            let text = std::fs::read_to_string(&path).expect("read");
            let d = parse(&id, &text);
            assert!(!d.title.is_empty(), "{name} produced an empty title");
            n += 1;
        }
        assert!(n >= 112, "expected at least 112 decisions, found {n}");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml decisions`
Expected: FAIL — `parse`, `DecisionRecord`, `Status`, `repo_root` not defined.

- [ ] **Step 3: Implement**

`tools/digest/src/scan/mod.rs`:

```rust
//! Derived facts — scanned from source at render time, never stored.
//!
//! PROC-11's rule governs what belongs here: store only geological-rate
//! facts; derive-on-read anything faster (spec §4.4).

// NOTE: `pub mod capability;` is added by Task 6, which creates that file.
// Declaring a module before its file exists does not compile.
pub mod decisions;

use std::path::PathBuf;

/// The repository root, resolved from this crate's manifest directory.
pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("tools/digest sits two levels below the repo root")
        .to_path_buf()
}
```

`tools/digest/src/scan/decisions.rs`:

```rust
//! Parse `docs/decisions/*.md` into decision records.

pub use super::repo_root;

/// A decision's lifecycle state.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Status {
    /// In force.
    Accepted,
    /// Not yet ratified.
    Proposed,
    /// Replaced, wholly or in part.
    Superseded,
}

/// One parsed decision record.
#[derive(Debug, PartialEq, Clone)]
pub struct DecisionRecord {
    /// Zero-padded numeric id, e.g. `"0026"`.
    pub id: String,
    /// Title text from the `# NNNN. Title` line.
    pub title: String,
    /// Lifecycle state.
    pub status: Status,
    /// The superseding decision's id, if any.
    pub superseded_by: Option<String>,
}

/// Parse one decision file.
pub fn parse(id: &str, text: &str) -> DecisionRecord {
    let title = text
        .lines()
        .find(|l| l.starts_with("# "))
        .and_then(|l| l.split_once(". "))
        .map(|(_, t)| t.trim().to_string())
        .unwrap_or_default();

    // The status line may wrap, so join until the first `·` or blank line.
    let start = text.find("**Status:**").map(|i| i + "**Status:**".len());
    let raw = start
        .map(|i| {
            let rest = &text[i..];
            let end = rest.find("\n\n").unwrap_or(rest.len());
            rest[..end].replace('\n', " ")
        })
        .unwrap_or_default();
    let status_field = raw.split('·').next().unwrap_or("").trim().to_string();

    let (status, superseded_by) = if status_field.starts_with("Superseded") {
        // Take the FIRST run of ascii digits after "by", wherever it sits:
        // bare (`by 0063`), bracketed (`by [0099](0099-...md)`), or on the
        // next line (0006). Everything after it — rationale, scope, or date —
        // is deliberately ignored: those three are not distinguishable
        // syntactically, and scope is asserted rather than scanned (spec §4.4).
        let after = status_field
            .split_once("by")
            .map(|(_, a)| a.trim())
            .unwrap_or("");
        let mut sup = None;
        let mut run = String::new();
        for c in after.chars() {
            if c.is_ascii_digit() {
                run.push(c);
            } else if !run.is_empty() {
                sup = Some(run.clone());
                break;
            }
        }
        if sup.is_none() && !run.is_empty() {
            sup = Some(run);
        }
        (Status::Superseded, sup)
    } else if status_field.starts_with("Proposed") {
        (Status::Proposed, None)
    } else {
        (Status::Accepted, None)
    };

    DecisionRecord {
        id: id.to_string(),
        title,
        status,
        superseded_by,
    }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml decisions`
Expected: PASS, 7 tests — including all 112+ committed decisions parsing.

- [ ] **Step 5: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest
git commit -m "feat(digest): parse decisions, including partial supersession

All four status formats in the corpus parse, and scope is retained:
0026 is superseded FOR DECISION RECORDS ONLY and its registry-row
provision still stands. Collapsing that to a boolean is what makes the
registry's 1,402 numeric ids look sanctioned.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 5: The in-force decision index

**Files:**
- Create: `tools/digest/src/render/mod.rs`, `tools/digest/src/render/decisions.rs`
- Modify: `tools/digest/src/lib.rs` (add `pub mod render;`)

**Interfaces:**
- Consumes: `scan::decisions::{DecisionRecord, Status, parse}`
- Produces: `render::decisions::index(records: &[DecisionRecord], scopes: &BTreeMap<String, String>) -> String`

Proves **spec §6 S4**: superseding removes a decision from the in-force index.

**AMENDED 2026-08-08 (Nathan's ruling).** `scope` is no longer on `DecisionRecord`. It arrives as `scopes`, a map from decision id to the surviving-provisions text, sourced from **asserted** facts (`supersession-scope`, subject `EntityId(n)` where `n` is the decision number). For v1 exactly one entry exists: `"0026"`. A decision superseded with no scope entry is wholly superseded and must not appear.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::scan::decisions::{DecisionRecord, Status};

    fn rec(id: &str, title: &str, status: Status, by: Option<&str>) -> DecisionRecord {
        DecisionRecord {
            id: id.into(),
            title: title.into(),
            status,
            superseded_by: by.map(str::to_string),
        }
    }

    fn no_scopes() -> BTreeMap<String, String> {
        BTreeMap::new()
    }

    #[test]
    fn a_superseded_decision_falls_out_of_the_index() {
        let out = index(
            &[
                rec("0029", "CI checks 500-seed censuses", Status::Superseded, Some("0063")),
                rec("0063", "Census regen is local again", Status::Accepted, None),
            ],
            &no_scopes(),
        );
        assert!(!out.contains("0029"), "wholly superseded decisions must not appear");
        assert!(out.contains("0063"));
    }

    #[test]
    fn a_partially_superseded_decision_stays_with_its_asserted_scope() {
        let mut scopes = BTreeMap::new();
        scopes.insert(
            "0026".to_string(),
            "for decision records; the study/chronicle/registry-row provisions stand".to_string(),
        );
        let out = index(
            &[rec("0026", "Slugs, not numbers", Status::Superseded, Some("0043"))],
            &scopes,
        );
        assert!(out.contains("0026"), "a partial supersession still governs in part");
        assert!(out.contains("registry-row provisions stand"), "the surviving scope must be shown");
        assert!(out.contains("0043"), "the superseder must be named");
    }

    #[test]
    fn a_superseded_decision_with_no_asserted_scope_is_dropped_even_if_its_file_had_a_parenthetical() {
        // 0029's status line carries a RATIONALE parenthetical and 0082's a
        // DATE. Neither is a scope, neither is asserted, so both must drop.
        let out = index(
            &[rec("0082", "A thing", Status::Superseded, Some("0099"))],
            &no_scopes(),
        );
        assert!(!out.contains("0082"), "absent scope means wholly superseded");
    }

    #[test]
    fn proposed_decisions_are_marked_not_dropped() {
        let out = index(&[rec("0061", "A proposed thing", Status::Proposed, None)], &no_scopes());
        assert!(out.contains("0061"));
        assert!(out.to_lowercase().contains("proposed"));
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml render`
Expected: FAIL — `index` not defined.

- [ ] **Step 3: Implement**

`tools/digest/src/render/mod.rs`:

```rust
//! Generated views. Every view is a pure function of the collection; none is
//! hand-edited. Regenerated by `scripts/regenerate-artifacts.sh` and
//! drift-checked with `git diff --exit-code`.

// NOTE: `pub mod doctor;` is added by Task 6 and `pub mod delta;` by Task 8,
// each alongside the file it creates. Declaring a module before its file
// exists does not compile.
pub mod decisions;
```

`tools/digest/src/render/decisions.rs`:

```rust
//! The in-force decision index.

use crate::scan::decisions::{DecisionRecord, Status};
use std::collections::BTreeMap;

/// Render the decisions currently in force.
///
/// A wholly superseded decision does not appear — that absence is the point
/// (spec §4.3): a stale record left in the file WILL be found by a grepping
/// reader. A PARTIALLY superseded decision does appear, with the provisions
/// that survive, because it still governs them.
///
/// `scopes` maps a decision id to its surviving-provisions text and comes from
/// ASSERTED facts, never from parsing (spec §4.4): the trailing parenthetical
/// in a status line holds a rationale, a scope, or a date depending on the
/// decision, and those are not distinguishable syntactically.
pub fn index(records: &[DecisionRecord], scopes: &BTreeMap<String, String>) -> String {
    let mut sorted: Vec<&DecisionRecord> = records.iter().collect();
    sorted.sort_by(|a, b| a.id.cmp(&b.id));

    let mut out = String::from(
        "# Decisions in force\n\n\
         GENERATED by `digest render decisions` — do not edit. Wholly \
         superseded decisions are absent by design; git holds them.\n\n",
    );
    for r in sorted {
        match (r.status, scopes.get(&r.id).map(String::as_str)) {
            (Status::Superseded, None) => continue,
            (Status::Superseded, Some(scope)) => {
                let by = r.superseded_by.as_deref().unwrap_or("?");
                out.push_str(&format!(
                    "- **{}** {} — *partly superseded by {}; still in force: {}*\n",
                    r.id, r.title, by, scope
                ));
            }
            (Status::Proposed, _) => {
                out.push_str(&format!("- **{}** {} — *proposed, not ratified*\n", r.id, r.title));
            }
            (Status::Accepted, _) => {
                out.push_str(&format!("- **{}** {}\n", r.id, r.title));
            }
        }
    }
    out
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml render`
Expected: PASS, 3 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest
git commit -m "feat(digest): the in-force decision index (S4)

Wholly superseded decisions are ABSENT, not deprioritised — a stale
record left in the file will be found. Partially superseded ones stay,
carrying the provisions that survive.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 6: The capability scanner and the generated self-map

**Files:**
- Create: `tools/digest/src/scan/capability.rs`, `tools/digest/src/render/doctor.rs`
- Modify: `scripts/doctor.sh`

**Interfaces:**
- Consumes: `scan::repo_root`
- Produces: `scan::capability::allowed_external() -> Vec<String>`, `scan::capability::layers() -> Vec<String>`, `render::doctor::self_map(&[String], &[String]) -> String`

Proves **spec §6 S1**: the generated map states `libm, serde, serde_json`, derived from `ALLOWED_EXTERNAL`, with no hard-coded allowlist. Today `scripts/doctor.sh:19` hard-codes `serde, serde_json` and has been wrong since decision 0041.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_allowlist_is_derived_from_the_enforcing_test() {
        let got = allowed_external();
        assert_eq!(
            got,
            vec!["libm".to_string(), "serde".to_string(), "serde_json".to_string()],
            "must match ALLOWED_EXTERNAL in cli/tests/architecture.rs, including libm (0041)"
        );
    }

    #[test]
    fn the_generated_map_names_libm() {
        let out = crate::render::doctor::self_map(&allowed_external(), &layers());
        assert!(out.contains("libm"), "S1: the drift doctor.sh has today must be gone");
    }

    #[test]
    fn no_allowlist_is_hard_coded_in_the_generator() {
        let src = include_str!("../render/doctor.rs");
        assert!(
            !src.contains("serde_json\""),
            "S1: the renderer must not name an allowlist member literally"
        );
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml capability`
Expected: FAIL — `allowed_external` not defined.

- [ ] **Step 3: Implement**

`tools/digest/src/scan/capability.rs`:

```rust
//! Capability facts, derived from source at render time and never stored.

use super::repo_root;

/// The externally-allowlisted crates, read from the test that ENFORCES the
/// rule rather than from any prose restatement of it. `scripts/doctor.sh` used
/// to restate it and drifted: it still omitted `libm` long after decision 0041
/// admitted it.
pub fn allowed_external() -> Vec<String> {
    let src = std::fs::read_to_string(repo_root().join("cli/tests/architecture.rs"))
        .expect("architecture.rs is readable");
    let line = src
        .lines()
        .find(|l| l.contains("ALLOWED_EXTERNAL"))
        .expect("ALLOWED_EXTERNAL is declared");
    // Split on `=` FIRST. The declaration is
    //   const ALLOWED_EXTERNAL: &[&str] = &["libm", "serde", "serde_json"];
    // so bracket-matching the whole line lands on the `&[&str]` type
    // annotation's `[`, not the value literal's, and corrupts the first entry.
    let initializer = line
        .split_once('=')
        .map(|(_, r)| r)
        .expect("ALLOWED_EXTERNAL has an initializer");
    let inner = initializer
        .split_once('[')
        .and_then(|(_, r)| r.rsplit_once(']'))
        .map(|(i, _)| i)
        .expect("ALLOWED_EXTERNAL is a slice literal");
    let mut out: Vec<String> = inner
        .split(',')
        .map(|s| s.trim().trim_matches('"').to_string())
        .filter(|s| !s.is_empty())
        .collect();
    out.sort();
    out
}

/// The layer names, in dependency order.
pub fn layers() -> Vec<String> {
    ["kernel", "domains/*", "windows/*", "cli"]
        .iter()
        .map(|s| s.to_string())
        .collect()
}
```

`tools/digest/src/render/doctor.rs`:

```rust
//! The repo self-map.

/// Render the layering and dependency-allowlist section of the self-map.
///
/// Every value here is DERIVED. Nothing in this function may name an
/// allowlist member literally — that is what drifted before (spec §1.1).
pub fn self_map(allowed: &[String], layers: &[String]) -> String {
    format!(
        "== Layering (enforced: cli/tests/architecture.rs; picture: book/src/reference/layering.md)\n  \
         {}\n  \
         a domain depends on the kernel and NOTHING else; windows/worldgen is the\n  \
         composition root; external deps allowlist: {}\n",
        layers.join(" -> "),
        allowed.join(", ")
    )
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml`
Expected: PASS, all tests.

- [ ] **Step 4b: Declare the two new modules**

Add `pub mod capability;` to `tools/digest/src/scan/mod.rs` and `pub mod doctor;` to `tools/digest/src/render/mod.rs`, each replacing the NOTE comment left by Tasks 4 and 5. Re-run `cargo test --manifest-path tools/digest/Cargo.toml` and confirm it still compiles before continuing.

- [ ] **Step 5: Replace the drifted line in `doctor.sh`**

In `scripts/doctor.sh`, delete the hard-coded layering block (the `echo` lines around line 19 that state the allowlist) and replace with:

```sh
cargo run --quiet --manifest-path tools/digest/Cargo.toml -- render doctor
```

- [ ] **Step 6: Prove the drift is gone**

Run: `make doctor | grep -A1 "external deps allowlist"`
Expected: the line reads `external deps allowlist: libm, serde, serde_json`.

- [ ] **Step 7: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest scripts/doctor.sh
git commit -m "feat(digest): derive the self-map's allowlist from its enforcer (S1)

doctor.sh had hard-coded 'serde, serde_json' since before 0041 admitted
libm — the repo self-map was wrong about the repo. The allowlist is now
read from ALLOWED_EXTERNAL in cli/tests/architecture.rs, and a test
forbids the renderer from naming a member literally.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 7: The CLI, the drift check, and the mutation proof

**Files:**
- Create: `tools/digest/src/main.rs`, `docs/digest/facts.jsonl`
- Modify: `scripts/regenerate-artifacts.sh`

**Interfaces:**
- Consumes: everything above
- Produces: `digest render <doctor|decisions|delta>`, `digest check`

Proves **spec §6 S2**: mutating one fact makes the drift check exit non-zero. **The check must be demonstrated RED on command.** A green drift-check that cannot fail is this repo's documented recurring failure mode; per the campaign-autopilot rule, a mutation test must also prove it actually mutated.

**ALSO PROVES S6 — moved here 2026-08-08 (controller, after Task 6's review).** The plan's coverage table promised S6 from Task 6, but Task 6's text only removed the layering/allowlist block. **13 fact-asserting `echo` lines remain in `scripts/doctor.sh`** — lines 22-25 (determinism contracts), 28-32 (committed generated artifacts), 36-39 (documentation map). They are the same drift shape as the `libm` bug this campaign exists to fix, and no other task touched them. Task 7 owns `docs/digest/` and the CLI, so it finishes the job.

**How.** Spec §6 S6 requires that any remaining prose be "an assembled text object stored in the ledger." So:

- Register one new predicate in `tools/digest/src/vocabulary.rs`: `("self-map-line", true, "one authored line of the repo self-map")`. Functional, because each line's subject holds exactly one line.
- Assert one fact **per line**, each with its own subject id, so JSONL's `(subject, predicate)` sort reproduces line order. Use `EntityId(100..103)` for the determinism lines, `EntityId(200..204)` for the artifact lines, `EntityId(300..303)` for the doc-map lines. **Do not put multiple lines under one subject** — tie order between facts sharing a `(subject, predicate)` key is unspecified (a Task 2 review finding), so it would scramble.
- Extend `render::doctor::self_map` to emit those sections from the ledger, and delete the corresponding `echo` lines from `scripts/doctor.sh`.
- Line 37's `${decision_count}` is already derived from source — keep it derived, do not freeze the count into a stored fact. It is a fast-drifting fact and PROC-11's rule forbids storing it.
- Leave lines 14, 42, 74, 78 alone: 14 is a heading, 42 and 74/78 are derived from git and the filesystem.

**Verification:** after the edit, `grep -c '^echo "  - ' scripts/doctor.sh` must return `0`, and `make doctor` must still print all three sections with the same content.

- [ ] **Step 1: Write `main.rs`**

```rust
//! The Digest CLI.

use digest::render;
use digest::scan;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("render") => match args.get(2).map(String::as_str) {
            Some("doctor") => print!(
                "{}",
                render::doctor::self_map(&scan::capability::allowed_external(), &scan::capability::layers())
            ),
            Some("decisions") => print!("{}", render::decisions::index(&all_decisions())),
            // NOTE: the `delta` arm is added by Task 8, which creates that
            // module. Referencing it here would not compile.
            _ => {
                eprintln!("usage: digest render <doctor|decisions>");
                std::process::exit(2);
            }
        },
        _ => {
            eprintln!("usage: digest render <view>");
            std::process::exit(2);
        }
    }
}

fn all_decisions() -> Vec<scan::decisions::DecisionRecord> {
    let dir = scan::repo_root().join("docs/decisions");
    let mut out = Vec::new();
    let mut entries: Vec<_> = std::fs::read_dir(&dir)
        .expect("decisions dir")
        .filter_map(Result::ok)
        .map(|e| e.path())
        .collect();
    entries.sort();
    for path in entries {
        let name = path.file_name().expect("name").to_string_lossy().to_string();
        if !name.ends_with(".md") || name == "README.md" {
            continue;
        }
        let id = name.split('-').next().expect("id prefix").to_string();
        let text = std::fs::read_to_string(&path).expect("read");
        out.push(scan::decisions::parse(&id, &text));
    }
    out
}
```

- [ ] **Step 2: Generate the committed views**

```bash
mkdir -p docs/digest
cargo run --quiet --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
```

- [ ] **Step 3: Wire into the regeneration script**

Add to `scripts/regenerate-artifacts.sh`, next to the other artifact regenerations:

```sh
cargo run --quiet --manifest-path tools/digest/Cargo.toml -- render decisions \
  > docs/digest/decisions-in-force.md
```

- [ ] **Step 4: Prove the drift check goes GREEN when clean**

Run: `bash scripts/regenerate-artifacts.sh && git diff --exit-code docs/digest/`
Expected: exit 0, no output.

- [ ] **Step 5: Prove the drift check goes RED on a mutation (S2)**

```bash
# Assert the target exists BEFORE mutating — a no-op mutation produces
# evidence that looks exactly like a robust implementation.
grep -q "^# 0110\." docs/decisions/0110-the-census-is-the-suites-shared-world-building-pass.md \
  || { echo "TARGET NOT FOUND — mutation would be a no-op"; exit 1; }

cp docs/decisions/0110-the-census-is-the-suites-shared-world-building-pass.md /tmp/0110.bak
sed -i '' 's/^\*\*Status:\*\* Accepted/**Status:** Superseded by 0111/' \
  docs/decisions/0110-the-census-is-the-suites-shared-world-building-pass.md

bash scripts/regenerate-artifacts.sh
if git diff --exit-code docs/digest/ > /dev/null; then
  echo "S2 FAILED: the drift check did not go red"; exit 1
else
  echo "S2 PASSED: drift check went red on a single mutated fact"
fi

cp /tmp/0110.bak docs/decisions/0110-the-census-is-the-suites-shared-world-building-pass.md
bash scripts/regenerate-artifacts.sh
git diff --exit-code docs/digest/ && echo "restored clean"
```

Expected: `S2 PASSED`, then `restored clean`.

- [ ] **Step 6: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest docs/digest scripts/regenerate-artifacts.sh
git commit -m "feat(digest): CLI, committed views, and a drift check proven red (S2)

The mutation step asserts its target exists before substituting, so a
no-op mutation cannot masquerade as a passing guard.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 8: The intent≠reality delta report

**Files:**
- Create: `tools/digest/src/render/delta.rs`

**Interfaces:**
- Consumes: `scan::decisions::{DecisionRecord, Status}`, `scan::repo_root`
- Produces: `render::delta::report(&[DecisionRecord]) -> String`

Proves **spec §6 S5**. This is the view that keeps the campaign from becoming what it exists to prevent (spec §4.9): generated docs cannot disagree with the code, so the disagreement must be *computed and reported*.

The known instance: **0026's surviving registry-row provision says slugs, and the registry carries 1,402 numeric ids.**

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::scan::decisions::{DecisionRecord, Status};

    fn scopes_with_0026() -> BTreeMap<String, String> {
        let mut m = BTreeMap::new();
        m.insert(
            "0026".to_string(),
            "for decision records; the study/chronicle/registry-row provisions stand".to_string(),
        );
        m
    }

    #[test]
    fn reports_the_registry_id_gap() {
        let recs = vec![DecisionRecord {
            id: "0026".into(),
            title: "Slugs, not numbers".into(),
            status: Status::Superseded,
            superseded_by: Some("0043".into()),
        }];
        let out = report(&recs, &scopes_with_0026());
        assert!(out.contains("0026"), "the partially-surviving decision must be named");
        assert!(out.contains("numeric"), "the measured reality must be reported");
    }

    #[test]
    fn the_report_is_not_vacuous_on_real_repo_state() {
        // S5: if this ever returns 'no gaps' on the live repo, either the
        // repo became perfect or the view stopped working. Assume the latter.
        let out = report(&crate::scan::decisions::all_for_test(), &scopes_with_0026());
        assert!(out.contains("0026"), "S5: the known live gap must be found");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml delta`
Expected: FAIL — `report` not defined.

- [ ] **Step 3: Implement**

```rust
//! The intent ≠ reality report.
//!
//! Generated documentation CANNOT disagree with the code, and disagreement is
//! diagnostic (spec §4.9). So intent is asserted, reality is scanned, and the
//! gap is computed rather than silently collapsed.

use crate::scan::decisions::{DecisionRecord, Status};
use crate::scan::repo_root;

/// Count identifiers of the form `PREFIX-123` in the idea registry.
fn numeric_registry_ids() -> usize {
    let text = std::fs::read_to_string(repo_root().join("book/src/frontier/idea-registry.md"))
        .unwrap_or_default();
    text.split_whitespace()
        .filter(|w| {
            let w = w.trim_matches(|c: char| !c.is_ascii_alphanumeric() && c != '-');
            match w.split_once('-') {
                Some((p, s)) => {
                    !p.is_empty()
                        && p.chars().all(|c| c.is_ascii_uppercase())
                        && !s.is_empty()
                        && s.chars().all(|c| c.is_ascii_digit())
                }
                None => false,
            }
        })
        .count()
}

/// Report every place authored intent and scanned reality disagree.
///
/// `scopes` is the same asserted map [`crate::render::decisions::index`] takes:
/// decision id to surviving-provisions text. A decision only counts as
/// partially superseded if a scope was ASSERTED for it.
pub fn report(records: &[DecisionRecord], scopes: &BTreeMap<String, String>) -> String {
    let mut out = String::from(
        "# Intent vs reality\n\n\
         GENERATED by `digest render delta` — do not edit. Each row is a rule \
         the project says it holds, next to what the repo actually does.\n\n",
    );
    let mut gaps = 0usize;

    for r in records {
        let Some(scope) = scopes.get(&r.id) else {
            continue;
        };
        if r.status != Status::Superseded {
            continue;
        }
        if r.title.to_lowercase().contains("slug") {
            let n = numeric_registry_ids();
            if n > 0 {
                gaps += 1;
                out.push_str(&format!(
                    "- **{} {}** still governs registry rows ({}), but the registry \
                     carries {} numeric identifiers.\n",
                    r.id, r.title, scope, n
                ));
            }
        }
    }

    if gaps == 0 {
        out.push_str("- No gaps found. Verify the view still works before believing this.\n");
    }
    out
}
```

Add to `scan/decisions.rs` for the non-vacuity test:

```rust
/// Every committed decision, parsed. Test support for the delta view.
pub fn all_for_test() -> Vec<DecisionRecord> {
    let dir = repo_root().join("docs/decisions");
    let mut out = Vec::new();
    let mut paths: Vec<_> = std::fs::read_dir(&dir)
        .expect("decisions dir")
        .filter_map(Result::ok)
        .map(|e| e.path())
        .collect();
    paths.sort();
    for path in paths {
        let name = path.file_name().expect("name").to_string_lossy().to_string();
        if !name.ends_with(".md") || name == "README.md" {
            continue;
        }
        let id = name.split('-').next().expect("id prefix").to_string();
        out.push(parse(&id, &std::fs::read_to_string(&path).expect("read")));
    }
    out
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml delta`
Expected: PASS, 2 tests.

- [ ] **Step 4b: Declare the module and add the CLI arm**

Add `pub mod delta;` to `tools/digest/src/render/mod.rs`, replacing the NOTE comment left by Task 5. In `tools/digest/src/main.rs`, restore the `delta` arm and its usage string:

```rust
            Some("delta") => print!("{}", render::delta::report(&all_decisions())),
```

and change the two usage lines to `usage: digest render <doctor|decisions|delta>`. Re-run `cargo test --manifest-path tools/digest/Cargo.toml` and confirm it compiles.

- [ ] **Step 5: Wire it into regeneration and commit**

```bash
cargo run --quiet --manifest-path tools/digest/Cargo.toml -- render delta > docs/digest/intent-vs-reality.md
# add the same line to scripts/regenerate-artifacts.sh
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest docs/digest scripts/regenerate-artifacts.sh
git commit -m "feat(digest): the intent-vs-reality report (S5)

0026's registry-row provision survived its supersession and says slugs;
the registry carries 1,402 numeric ids. Generated docs cannot disagree
with the code, so the disagreement is computed instead.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 9: The MCP read/write surface

**Files:**
- Create: `tools/digest/src/mcp.rs`
- Modify: `tools/digest/src/lib.rs`, `tools/digest/src/main.rs`

**Interfaces:**
- Consumes: `store::{ProjectLedger, fact}`, `vocabulary::project_registry`
- Produces: `digest mcp` (stdio JSON-RPC), tools `digest_query`, `digest_assert`

In scope per spec §4.8: if asserting a decision is onerous, decisions stop being asserted. **MCP is ergonomics, not substrate** — every operation must remain possible by editing `docs/digest/facts.jsonl` in an editor.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_then_query_round_trips_through_the_store_file() {
        let dir = std::env::temp_dir().join("digest-mcp-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        let _ = std::fs::remove_file(&path);

        handle_assert(&path, 1, "decision-status", "accepted").expect("assert");
        handle_assert(&path, 1, "decision-status", "superseded").expect("re-assert");

        let text = std::fs::read_to_string(&path).expect("read back");
        assert_eq!(text.lines().count(), 1, "the functional predicate compacted on disk");
        assert!(text.contains("superseded"));

        let hits = handle_query(&path, Some(1), None).expect("query");
        assert_eq!(hits.len(), 1);
    }

    #[test]
    fn the_store_is_plain_text_editable_without_the_tool() {
        let dir = std::env::temp_dir().join("digest-mcp-test2");
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        std::fs::write(
            &path,
            "{\"subject\":1,\"predicate\":\"decision-status\",\"object\":{\"Text\":\"accepted\"},\
             \"place\":null,\"day\":null,\"provenance\":\"asserted\"}\n",
        )
        .expect("hand-write");
        let hits = handle_query(&path, Some(1), None).expect("query a hand-written store");
        assert_eq!(hits.len(), 1, "MCP is ergonomics, not substrate");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --manifest-path tools/digest/Cargo.toml mcp`
Expected: FAIL — `handle_assert` / `handle_query` not defined.

- [ ] **Step 3: Implement the handlers**

```rust
//! The MCP surface: ergonomic read/write over the JSONL store.
//!
//! MCP is ERGONOMICS, NOT SUBSTRATE (spec §4.8). Every operation here must
//! remain possible by editing `docs/digest/facts.jsonl` by hand; the
//! round-trip test asserts exactly that.

use crate::store::{ProjectLedger, StoreError, fact};
use crate::vocabulary::project_registry;
use hornvale_kernel::ledger::{EntityId, Fact, Value};
use std::num::NonZeroU64;
use std::path::Path;

fn load(path: &Path) -> Result<ProjectLedger, StoreError> {
    let text = std::fs::read_to_string(path).unwrap_or_default();
    ProjectLedger::from_jsonl(&text, project_registry())
}

/// Assert one fact and rewrite the store, compacted.
pub fn handle_assert(path: &Path, subject: u64, predicate: &str, object: &str) -> Result<(), StoreError> {
    let mut led = load(path)?;
    led.assert(fact(
        EntityId(NonZeroU64::new(subject).expect("subject is nonzero")),
        predicate,
        Value::Text(object.to_string()),
    ))?;
    std::fs::write(path, led.to_jsonl()).expect("store is writable");
    Ok(())
}

/// Query facts by subject and/or predicate.
pub fn handle_query(path: &Path, subject: Option<u64>, predicate: Option<&str>) -> Result<Vec<Fact>, StoreError> {
    let led = load(path)?;
    Ok(led
        .facts()
        .iter()
        .filter(|f| subject.is_none_or(|s| f.subject.0.get() == s))
        .filter(|f| predicate.is_none_or(|p| f.predicate == p))
        .cloned()
        .collect())
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --manifest-path tools/digest/Cargo.toml mcp`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt --manifest-path tools/digest/Cargo.toml
git add tools/digest
git commit -m "feat(digest): MCP read/write over the JSONL store

A test asserts a hand-written store parses, because MCP is ergonomics
and not substrate — if the only way to read project state were a running
server, this would be more fragile than the prose it replaces.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 10: Close — gate, book, retrospective

**Files:**
- Modify: `book/src/chronicle/the-digest.md` (create), `docs/retrospectives/the-digest.md` (create), `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Run the full commit gate**

Run: `make gate`
Expected: green. Note `tools/digest` is outside the workspace, so the gate does not build it — run its tests explicitly too:
Run: `cargo test --manifest-path tools/digest/Cargo.toml`

- [ ] **Step 2: Confirm no artifact drift**

Run: `make rebaseline && git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/`
Expected: exit 0.

- [ ] **Step 3: Evaluate the falsification clause (spec §6)**

Read the generated `make doctor` output start to finish. If a fresh session would orient *worse* from it than from the hand-written map, that is the campaign's headline finding: report it, stop at v1, do not proceed to `CLAUDE.md`. Record the verdict either way in the retrospective.

- [ ] **Step 4: Write the chronicle entry and retrospective**

Chronicle: what codification is, why the ~50% position was the missing one, and the two live drift instances that motivated it. Retrospective: process lessons only — including that the campaign's own brainstorm re-derived PROC-11 from scratch because 2.07M words made it unfindable.

- [ ] **Step 5: Update registry rows**

Set UNI-29 / UNI-21 / UNI-28 / PROC-11 statuses to reflect what shipped, and add a row for the codification position if it earns one (Nathan's call at G6).

- [ ] **Step 6: Commit and present the G6 package**

---

## Self-Review

**Spec coverage.** §4.1 collection → Task 1. §4.2 no time → Task 1 (`TimeIsGits`). §4.3 compaction → Task 1. §4.4 stored vs scanned → Tasks 4/6. §4.5 serialization → Task 2. §4.6 placement → Task 1. §4.7 regeneration → Task 7. §4.8 MCP → Task 9. §4.9 delta → Task 8. §5 v1 scope → Tasks 5–7. §6 S1→Task 6, S2→Task 7, S3→Task 2, S4→Task 5, S5→Task 8, S6→Task 6. §11 rename rule → Task 3 module doc. **§7 migration is NOT a task** — asserting the initial fact set is authoring, not implementation; it happens as Task 7 Step 2 and grows organically. Flagged rather than hidden.

**Placeholder scan.** No TBD/TODO. Every code step carries real code.

**Type consistency.** `DecisionRecord`/`Status` defined in Task 4 and used unchanged in Tasks 5 and 8. `ProjectLedger::assert`/`facts`/`to_jsonl`/`from_jsonl` defined in Tasks 1–2 and used unchanged in Task 9. `fact()` helper defined once in Task 1. `repo_root()` defined in Task 4's `scan/mod.rs` and reused in Tasks 6 and 8.

**Known gap, deliberately left:** `render::delta::report` special-cases the slug rule by title match. That is honest for v1 with one known gap, and it will not generalize. Generalizing it needs an `intends` fact vocabulary, which is why `intends` is registered in Task 3 but unused until v2.
