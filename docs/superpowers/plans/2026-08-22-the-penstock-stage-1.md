# The Penstock Stage 1: The Call Sites and the Instruments

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the quadratic ledger read in `liveness.rs`, give `Ledger` the
public indexed (subject, predicate) query it lacks, and build the instruments
that decide whether stages 2–8 of the metaplan are worth building at all.

**Architecture:** Nothing here is a cache. Task 1 adds one kernel query method
backed by the SPO index that already exists. Task 2 repoints two call sites at
it. Tasks 3–5 are measurement: a synthetic-ledger generator and a
query-scaling bench that reports a fitted log-log slope (so an accidental
quadratic is caught at any scale, on any machine, deterministically), a gated
facts-per-agent-per-tick counter, and an agent-scaling bench that splits tick
cost into query / plan / commit.

**Tech Stack:** Rust edition 2024, std only (plus `serde`, `serde_json`,
`libm`). `cargo nextest` for tests, `cargo run --release --example` for
benches.

**Spec:** `docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`
(especially §4 measured baseline, §6 stage table, §7 standing gate).

## Global Constraints

Every task's requirements implicitly include all of these.

- **Dependencies are frozen.** `serde`, `serde_json`, `libm` only, workspace
  wide (decision 0004/0041). No new crates for any reason, including
  benchmarking. Enforced by `cli/tests/suite/architecture.rs`.
- **No `HashMap` / `HashSet`.** `BTreeMap` / `BTreeSet` / `Vec` only
  (decision 0005), enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock** (`std::time::Instant`, `std::time::SystemTime`) —
  decision 0001, same clippy enforcement. **The one exemption** is a
  measurement harness, which takes a scoped `#[allow(clippy::disallowed_types)]`
  plus this standing justification comment, copied from
  `cli/tests/suite/session_cost.rs`:
  ```rust
  // The measurement harness times derivation calls for a diagnostic (never sim
  // logic, never a fact, never seeded from wall-clock) -- exempt from the
  // wall-clock ban (clippy.toml / decision 0001), same pattern as
  // `cli/tests/suite/scene_cost.rs` and `cli/tests/suite/graph_cost.rs`.
  #[allow(clippy::disallowed_types)]
  use std::time::Instant;
  ```
- **Never add a top-level `<crate>/tests/*.rs`.** Every integration test goes
  in `<crate>/tests/suite/<name>.rs` with a `#[path = "suite/<name>.rs"] mod
  <name>;` line in `<crate>/tests/suite.rs`. A new top-level file is a new
  compilation unit and reddens
  `cli/tests/suite/test_binary_ratchet.rs`. Kernel unit tests go in the
  `#[cfg(test)] mod tests` block of the source file, beside their subject.
- **`#![warn(missing_docs)]` is on in every crate.** Every `pub` item, field
  and variant needs a doc comment.
- **Every primitive at a `pub` boundary needs a `type-audit:` verdict tag**
  (decisions 0027/0028), or `cargo run --manifest-path
  tools/type-audit/Cargo.toml -- check` fails the gate. A `&str` predicate
  parameter takes `/// type-audit: bare-ok(identifier-text)`, copying `find`.
- **`cargo fmt` is the final step before every commit**, and every commit must
  pass `make gate-commit`.
- **Byte-identity is the master oracle.** Nothing in this plan may move a
  committed artifact. See Task 2 Step 6 for the branch table.

---

### Task 1: `Ledger::facts_of` — the missing indexed multi-fact query

**Files:**
- Modify: `kernel/src/ledger.rs` (add `naive_facts_of` beside the other naive
  reference impls at ~line 230; add `facts_of` after `facts_about` at ~line
  340; add tests to the `#[cfg(test)] mod tests` block)

**Interfaces:**
- Consumes: `crate::fact_index::FactIndex::positions_for_subject_predicate`
  (already `pub(crate)`, signature
  `fn(&self, subject: EntityId, predicate: &str) -> impl Iterator<Item = usize> + '_`).
  **It does not yield ascending positions** — it flat-maps a BTree range over
  object keys, so positions arrive in object order. It must be collected and
  sorted, exactly as `positions_for_predicate` already does with
  `sort_unstable`.
- Produces: `Ledger::facts_of(&self, subject: EntityId, predicate: &str) ->
  impl Iterator<Item = &Fact>`, yielding every matching fact in **commit
  order**. Task 2 depends on this name, signature, and ordering.

- [ ] **Step 1: Write the failing tests**

Add to the `#[cfg(test)] mod tests` block at the end of `kernel/src/ledger.rs`.
These sit beside the existing `index_equals_scan_*` battery and follow its
shape. Read one of those first for the local helper conventions.

```rust
/// claim: invariant(forall-seed) — indexed `facts_of` agrees with a naive
/// scan on BOTH contents and ORDER. Order is the half a count-only
/// assertion misses, and Task 2 repoints two folds onto this method.
#[test]
fn facts_of_equals_scan_in_commit_order() {
    let mut r = ConceptRegistry::default();
    r.register_predicate("p", false, "").unwrap();
    r.register_predicate("q", false, "").unwrap();
    let mut l = Ledger::default();
    let a = l.mint_entity(test_lineage(0));
    let b = l.mint_entity(test_lineage(1));

    // Interleave subjects, predicates, and DESCENDING objects, so index-key
    // order and commit order genuinely differ (the SPO index orders by object
    // within a (subject, predicate) pair).
    for (subj, pred, obj) in [
        (a, "p", 3.0),
        (b, "p", 9.0),
        (a, "q", 8.0),
        (a, "p", 2.0),
        (b, "q", 1.0),
        (a, "p", 1.0),
    ] {
        l.commit(
            Fact {
                subject: subj,
                predicate: pred.to_string(),
                object: Value::Number(obj),
                place: None,
                day: None,
                provenance: "t".to_string(),
            },
            &r,
        )
        .unwrap();
    }

    for (subj, pred) in [(a, "p"), (a, "q"), (b, "p"), (b, "q"), (a, "absent")] {
        let indexed: Vec<&Fact> = l.facts_of(subj, pred).collect();
        let scanned: Vec<&Fact> = l
            .iter()
            .filter(|f| f.subject == subj && f.predicate == pred)
            .collect();
        assert_eq!(
            indexed, scanned,
            "facts_of({subj:?}, {pred}) must equal the scan, in commit order"
        );
    }
    // The discriminating case: (a, "p") holds objects 3, 2, 1 committed in
    // that order, so a result sorted by object would read 1, 2, 3.
    let objs: Vec<f64> = l
        .facts_of(a, "p")
        .filter_map(|f| match f.object {
            Value::Number(n) => Some(n),
            _ => None,
        })
        .collect();
    assert_eq!(objs, vec![3.0, 2.0, 1.0], "commit order, not object order");
}

/// claim: invariant — `facts_of` agrees with the scan on a ledger whose
/// index has never been built (the absent-index path is a separate branch).
#[test]
fn facts_of_agrees_before_the_index_exists() {
    let mut r = ConceptRegistry::default();
    r.register_predicate("p", false, "").unwrap();
    let mut l = Ledger::default();
    let a = l.mint_entity(test_lineage(0));
    l.commit(
        Fact {
            subject: a,
            predicate: "p".to_string(),
            object: Value::Flag(true),
            place: None,
            day: None,
            provenance: "t".to_string(),
        },
        &r,
    )
    .unwrap();
    // Round-trip through JSON: `index` is #[serde(skip)], so the reloaded
    // ledger has none until something calls `ensure_index`.
    let reloaded: Ledger = serde_json::from_str(&serde_json::to_string(&l).unwrap()).unwrap();
    let indexed: Vec<&Fact> = reloaded.facts_of(a, "p").collect();
    let scanned: Vec<&Fact> = reloaded
        .iter()
        .filter(|f| f.subject == a && f.predicate == "p")
        .collect();
    assert_eq!(indexed, scanned);
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib -- facts_of`
Expected: FAIL to compile — `no method named 'facts_of' found for struct 'Ledger'`.

- [ ] **Step 3: Add the naive reference impl**

In `kernel/src/ledger.rs`, in the `--- naive reference impls` block (beside
`naive_facts_about`, ~line 240):

```rust
    pub(crate) fn naive_facts_of(&self, subject: EntityId, predicate: &str) -> Vec<usize> {
        (0..self.facts.len())
            .filter(|&p| self.facts[p].subject == subject && self.facts[p].predicate == predicate)
            .collect()
    }
```

- [ ] **Step 4: Add `facts_of`**

Immediately after `facts_about` (~line 348), matching its shape:

```rust
    /// Every fact for (`subject`, `predicate`), in commit order.
    ///
    /// The multi-fact query the public surface lacked: [`Self::value_of`]
    /// returns the FIRST and [`Self::latest_value_of`] the LAST, so a caller
    /// wanting all of them had to `find(predicate).filter(|f| f.subject ==
    /// e)` — O(every fact with that predicate), and therefore QUADRATIC when
    /// run once per subject per tick. O(log n + k) via the SPO index.
    ///
    /// `positions_for_subject_predicate` yields object-key order, not
    /// position order, so the sort is load-bearing: commit order is the
    /// contract every caller of `facts_about`/`find` already relies on.
    /// type-audit: bare-ok(identifier-text)
    pub fn facts_of(&self, subject: EntityId, predicate: &str) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => {
                let mut v: Vec<usize> = idx.positions_for_subject_predicate(subject, predicate).collect();
                v.sort_unstable();
                v
            }
            None => self.naive_facts_of(subject, predicate),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib -- facts_of`
Expected: PASS, 2 tests.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
make gate-commit
git add kernel/src/ledger.rs
git commit -m "feat(kernel): Ledger::facts_of, the indexed (subject, predicate) query

value_of returns the first and latest_value_of the last; nothing returned
them all, so callers reached for find(pred).filter(subject) -- O(every fact
with that predicate), quadratic when run per subject per tick. Backed by the
SPO index that already existed. positions_for_subject_predicate yields
object-key order, so the sort is load-bearing: commit order is the contract
facts_about and find already keep, and the test pins order, not just
contents."
```

---

### Task 2: Repoint the two quadratic call sites

**Files:**
- Modify: `windows/vessel/src/liveness.rs:958` (`believed_water`) and
  `:1230` (`hazard_memory_memo`)
- Test: `windows/vessel/tests/suite/` — a new file, see Step 1

**Interfaces:**
- Consumes: `Ledger::facts_of` from Task 1.
- Produces: no new API. Behaviour must be **bit-identical**; this is a pure
  performance change.

Both sites currently read:

```rust
for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) {
```

Both fold into order-independent aggregates today (a `BTreeSet<RoomAddr>` and
a max-by-day `BTreeMap`), so ordering is not load-bearing *for them* — but
`facts_of` guarantees commit order anyway, so the change is safe regardless,
and the equivalence test below pins that rather than trusting the reading.

- [ ] **Step 1: Write the failing equivalence test**

Create `windows/vessel/tests/suite/ledger_query_equivalence.rs`:

```rust
//! The Penstock stage 1, Task 2: the two `liveness` folds that used to scan
//! every agent's whole history per agent per tick now use the SPO-indexed
//! `Ledger::facts_of`. This pins that the swap is behaviour-preserving —
//! the old expression and the new one select the same facts, in the same
//! order, on a ledger where the two could differ.

use hornvale_kernel::{ConceptRegistry, Fact, Ledger, Value, test_lineage};

/// The predicate both call sites read. Kept as a literal rather than
/// imported so this test still fails if the constant is repointed.
const AGENT_AT: &str = "agent-at";

#[test]
fn facts_of_selects_what_find_and_filter_selected() {
    let mut r = ConceptRegistry::default();
    r.register_predicate(AGENT_AT, false, "").unwrap();
    let mut l = Ledger::default();
    let a = l.mint_entity(test_lineage(0));
    let b = l.mint_entity(test_lineage(1));

    // Interleaved subjects with descending objects, so index-key order and
    // commit order differ and a same-contents-wrong-order bug is visible.
    for (subj, room) in [
        (a, "r3"),
        (b, "r9"),
        (a, "r2"),
        (b, "r8"),
        (a, "r1"),
    ] {
        l.commit(
            Fact {
                subject: subj,
                predicate: AGENT_AT.to_string(),
                object: Value::Text(room.to_string()),
                place: None,
                day: None,
                provenance: "t".to_string(),
            },
            &r,
        )
        .unwrap();
    }

    for subj in [a, b] {
        let old: Vec<&Fact> = l.find(AGENT_AT).filter(|f| f.subject == subj).collect();
        let new: Vec<&Fact> = l.facts_of(subj, AGENT_AT).collect();
        assert_eq!(old, new, "the swap must be behaviour-preserving for {subj:?}");
    }
}
```

Register it by adding this line to `windows/vessel/tests/suite.rs`, in the
existing alphabetical run of `#[path]` declarations:

```rust
#[path = "suite/ledger_query_equivalence.rs"]
mod ledger_query_equivalence;
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- ledger_query_equivalence`
Expected: FAIL to compile — `facts_of` is Task 1's method; if Task 1 is
already merged this test PASSES immediately, which is correct and expected.
It is a characterization test protecting Step 3, not a red-first test for new
behaviour. Note in your report which it was.

- [ ] **Step 3: Make both swaps**

`windows/vessel/src/liveness.rs`, in `believed_water` (~line 958) and in
`hazard_memory_memo` (~line 1230), replace:

```rust
    for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) {
```

with:

```rust
    for f in ledger.facts_of(npc.entity, AGENT_AT) {
```

Change nothing else in either loop body.

- [ ] **Step 4: Run the vessel suite**

Run: `cargo test -p hornvale-vessel --test suite 2>&1 | tail -30`
Expected: PASS. Any failure here is a real behaviour change — stop and
report it rather than adjusting a test.

- [ ] **Step 5: Prove world identity did not move**

Run: `cargo test -p hornvale --test suite -- lens_purity`
Expected: PASS. This is the seed-42 committed-world golden and it is the
master oracle for "did behaviour change".

- [ ] **Step 6: Check generated-artifact drift, and branch on the result**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Do not predict the output; branch on it:

- **Nothing moved** → proceed to Step 7.
- **Only `docs/audits/` moved** → expected, because Task 1 added a `pub`
  method and the type-audit report drifts on any pub-boundary change. Stage
  it in the same commit.
- **Anything under `book/src/gallery/`, `book/src/domesday/`, or a
  `*-seed-42-*` fixture moved** → STOP. That is a world-identity change and
  this task must not produce one. Report it; do not rebaseline it away.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs windows/vessel/tests/suite.rs \
        windows/vessel/tests/suite/ledger_query_equivalence.rs docs/audits/
git commit -m "perf(vessel): the two agent-history folds use the SPO index

believed_water and hazard_memory_memo each ran find(AGENT_AT).filter(subject
== e) -- a scan of EVERY agent's whole history, per agent, per tick, so cost
was quadratic in session length. Measured on a synthetic 1M-fact ledger:
48,288 ms for one 1000-agent sweep, against 44 ms for the same sweep through
the SPO-backed facts_about. Both folds are order-independent aggregates
today, but the new test pins order as well as contents so a future fold
cannot inherit a silent reordering."
```

---

### Task 3: The synthetic ledger and the query-scaling bench

**Files:**
- Create: `kernel/examples/query_scaling.rs`

**Interfaces:**
- Consumes: `Ledger::facts_of` (Task 1), `Ledger::{commit, iter, find,
  facts_about}`, `ConceptRegistry::register_predicate`, `EntityId::new`.
- Produces: nothing other crates depend on. The deliverable is the table in
  its own module doc.

This is the instrument that catches an accidental quadratic **at any scale,
on any machine**. It reports a fitted log-log slope, because the constant is
machine-dependent noise and the exponent is the finding.

- [ ] **Step 1: Write the bench**

Create `kernel/examples/query_scaling.rs`:

```rust
//! The Penstock stage 1, Task 3: how each ledger read scales with ledger
//! size.
//!
//! INFORMATIVE, NEVER A GATE — the same standing as
//! `windows/vessel/examples/turn_cost.rs`. Its job is to make an accidental
//! quadratic visible as a SLOPE, because a wall-time ceiling cannot: the
//! defect this stage fixes measured 13 ms at seed-42 scale and 48 seconds at
//! 1M facts, so any budget set against realistic data would have passed it
//! forever.
//!
//! Run: `cargo run --release -p hornvale-kernel --example query_scaling`
//! ALWAYS `--release`: a debug build measures the optimizer.
//!
//! ## Measured
//!
//! Paste the verbatim output here, with the date, `hostname -s`, and the
//! profile, the way `turn_cost.rs` does. Wall-times are machine-specific;
//! the SLOPES are the portable part.

// The measurement harness times derivation calls for a diagnostic (never sim
// logic, never a fact, never seeded from wall-clock) -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), same pattern as
// `cli/tests/suite/scene_cost.rs` and `cli/tests/suite/graph_cost.rs`.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, Ledger, Value};

const AGENT_AT: &str = "agent-at";

/// A synthetic ledger: `agents` subjects each holding `history` `agent-at`
/// facts, committed round-robin so subjects interleave in commit order the
/// way a real tick loop produces them.
fn synthetic(agents: u64, history: u64, reg: &ConceptRegistry) -> Ledger {
    let mut l = Ledger::default();
    for h in 0..history {
        for a in 1..=agents {
            l.commit(
                Fact {
                    subject: EntityId::new(a).expect("nonzero"),
                    predicate: AGENT_AT.to_string(),
                    object: Value::Number((h * 7 + a) as f64),
                    place: EntityId::new(1_000_000 + (a % 64)),
                    day: None,
                    provenance: "synthetic".to_string(),
                },
                reg,
            )
            .expect("synthetic commit");
        }
    }
    l
}

/// Least-squares slope of log(y) against log(x) — the scaling exponent.
/// 1.0 means linear in ledger size; 2.0 is the quadratic bug class.
fn log_log_slope(xs: &[f64], ys: &[f64]) -> f64 {
    let n = xs.len() as f64;
    let lx: Vec<f64> = xs.iter().map(|v| hornvale_kernel::math::ln(*v)).collect();
    let ly: Vec<f64> = ys.iter().map(|v| hornvale_kernel::math::ln(*v)).collect();
    let mx = lx.iter().sum::<f64>() / n;
    let my = ly.iter().sum::<f64>() / n;
    let num: f64 = lx.iter().zip(&ly).map(|(x, y)| (x - mx) * (y - my)).sum();
    let den: f64 = lx.iter().map(|x| (x - mx) * (x - mx)).sum();
    num / den
}

fn main() {
    println!("size_of::<Fact>()  = {}", std::mem::size_of::<Fact>());
    println!("size_of::<Value>() = {}", std::mem::size_of::<Value>());
    println!();

    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "synthetic").unwrap();

    // Fixed agent count, growing history: ledger size is the only variable,
    // which is what makes the slope interpretable.
    const AGENTS: u64 = 200;
    let histories = [25u64, 50, 100, 200, 400];

    let mut facts = Vec::new();
    let mut scan_ms = Vec::new();
    let mut index_ms = Vec::new();
    let mut place_ms = Vec::new();

    println!(
        "{:>10} {:>12} {:>12} {:>12}",
        "facts", "scan_ms", "facts_of_ms", "place_scan_ms"
    );
    for h in histories {
        let l = synthetic(AGENTS, h, &reg);
        let n = (AGENTS * h) as f64;

        let t = Instant::now();
        let mut sink = 0usize;
        for a in 1..=AGENTS {
            let e = EntityId::new(a).unwrap();
            sink += l.find(AGENT_AT).filter(|f| f.subject == e).count();
        }
        let scan = t.elapsed().as_secs_f64() * 1e3;
        std::hint::black_box(sink);

        let t = Instant::now();
        let mut sink2 = 0usize;
        for a in 1..=AGENTS {
            sink2 += l.facts_of(EntityId::new(a).unwrap(), AGENT_AT).count();
        }
        let idx = t.elapsed().as_secs_f64() * 1e3;
        std::hint::black_box(sink2);
        assert_eq!(sink, sink2, "INDEX != SCAN");

        // The unindexed axis: `place` is not an index key, so this is the
        // shape stage 2 exists to serve. 64 rooms, one query each.
        let t = Instant::now();
        let mut sink3 = 0usize;
        for room in 0..64u64 {
            let p = EntityId::new(1_000_000 + room);
            sink3 += l.iter().filter(|f| f.place == p).count();
        }
        let place = t.elapsed().as_secs_f64() * 1e3;
        std::hint::black_box(sink3);

        println!("{n:>10.0} {scan:>12.2} {idx:>12.2} {place:>12.2}");
        facts.push(n);
        scan_ms.push(scan);
        index_ms.push(idx);
        place_ms.push(place);
    }

    println!();
    println!("fitted log-log slope vs ledger size (1.0 linear, 2.0 quadratic):");
    println!("  scan          {:.2}", log_log_slope(&facts, &scan_ms));
    println!("  facts_of      {:.2}", log_log_slope(&facts, &index_ms));
    println!("  place scan    {:.2}", log_log_slope(&facts, &place_ms));
}
```

- [ ] **Step 2: Run it**

Run: `cargo run --release -p hornvale-kernel --example query_scaling`
Expected: it completes and prints three slopes. The `scan` slope should be
markedly steeper than the `facts_of` slope — that is the whole point of the
instrument. **Do not assert specific numbers here**; record what you get.

- [ ] **Step 3: Paste the verbatim output into the module doc**

Replace the `## Measured` placeholder text with the real output, plus the
date, `hostname -s`, and `--release`, following `turn_cost.rs`'s format
exactly. An un-filled `## Measured` section is an incomplete task.

- [ ] **Step 4: Gate and commit**

```bash
cargo fmt
make gate-commit
git add kernel/examples/query_scaling.rs
git commit -m "bench(kernel): query scaling as a fitted slope, not a ceiling

A wall-time budget cannot catch an accidental quadratic: the defect this
stage fixes measured 13 ms at seed-42 scale and 48 s at 1M facts, so any
ceiling set against realistic data would have passed it forever. This
reports the log-log slope against ledger size instead, which is
machine-independent and is the property that actually decides whether the
sim scales. Also carries the unindexed place axis, the shape stage 2 exists
to serve."
```

---

### Task 4: Facts committed per agent per tick, as a gated counter

**Files:**
- Create: `windows/vessel/tests/suite/tick_commit_budget.rs`
- Modify: `windows/vessel/tests/suite.rs` (one `#[path]` line)

**Interfaces:**
- Consumes: `hornvale_kernel::tick`, `Ledger::len`. **No new production
  state**: facts committed by a tick is `after.len() - before.len()`, which
  is already observable.
- Produces: nothing other tasks depend on.

This is the metaplan's feasibility number (§11): if facts-per-agent-per-tick
does not fall toward zero in steady state, the log grows without bound and
stage 7 becomes the program.

- [ ] **Step 1: Read the existing driver before writing anything**

The tick harness you need already exists in `windows/vessel/src/liveness.rs`'s
own `#[cfg(test)]` block — search for `hornvale_kernel::tick(&ledger, &[&sys],
&["drive-movements"], &reg)`, which appears at several call sites (e.g. around
lines 7743, 8044, 8736). **Copy a working construction from there rather than
inventing one**; this plan deliberately does not prescribe the roster
construction, because the author did not verify it and the implementer can
read it.

- [ ] **Step 2: Write the failing test**

Create `windows/vessel/tests/suite/tick_commit_budget.rs`. The **contract**
this test must satisfy, which matters more than its exact shape:

1. Build a roster of N creatures and run T consecutive ticks, threading each
   tick's output ledger into the next.
2. Record `after.len() - before.len()` for every tick.
3. Assert the **steady-state** rate — facts per agent over the last half of
   the run — is at or below a `const` falsification ceiling set comfortably
   above what you actually measure, with the measured value written into the
   constant's doc comment. Ceilings ratchet DOWN freely; raising one is an
   explicit reviewed act. This is the convention
   `cli/tests/suite/graph_cost.rs` and `session_cost.rs` establish — read
   `graph_cost.rs`'s module doc for the exact wording to mirror.
4. Assert the rate does **not grow** across the run (last-half rate ≤
   first-half rate), which is the unbounded-log tripwire.

Deterministic only: no `Instant` anywhere in this file. Fact counts are
byte-stable, which is exactly why this one can be a gate when a wall-time
budget cannot.

Register it in `windows/vessel/tests/suite.rs`:

```rust
#[path = "suite/tick_commit_budget.rs"]
mod tick_commit_budget;
```

- [ ] **Step 3: Run it to see the real numbers**

Run: `cargo test -p hornvale-vessel --test suite -- tick_commit_budget -- --nocapture`
Expected: prints the per-tick rates. Set the ceiling from what you observe,
then re-run to green.

- [ ] **Step 4: Report the measured rate in your task report**

The number itself is a deliverable, not just a passing test — it is the input
to the metaplan's stage-7 falsifier. State it explicitly.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/tests/suite.rs windows/vessel/tests/suite/tick_commit_budget.rs
git commit -m "test(vessel): gate facts committed per agent per tick

The Penstock's feasibility number: if this does not fall toward zero in
steady state the ledger grows without bound and no read optimisation can
help, because the log itself is what grows. Deterministic (fact counts, not
wall time), so unlike a timing budget it can actually gate. Also asserts the
rate does not GROW across a run, which is the unbounded-log tripwire."
```

---

### Task 5: The agent-scaling bench and the query / plan / commit split

**Files:**
- Create: `windows/vessel/examples/agent_scaling.rs`
- Modify: `windows/vessel/src/liveness.rs` — add a public read-only accessor
  for `HomeNavCache::searches`

**Interfaces:**
- Consumes: the same tick driver Task 4 used; `HomeNavCache`.
- Produces: `HomeNavCache::searches(&self) -> u64`.

This is the bench that decides whether stages 2–8 are worth building. If plan
cost swamps query cost, the metaplan stops at stage 1 (§11).

- [ ] **Step 1: Widen `HomeNavCache::searches`, deliberately**

`searches` is currently `pub(crate)` with the doc "test-visible within this
crate only — no drive, and no OTHER crate, ever reads it." That narrowing was
deliberate, so **do not simply flip it to `pub`**. Add a read-only accessor
beside it and record why the constraint moved:

```rust
    /// How many real `plan_to_room` searches this cache has run, ever.
    ///
    /// The field stays `pub(crate)` — nothing outside this crate may WRITE
    /// or reset it. This read-only accessor exists because The Penstock's
    /// stage-1 instrument set needs the plan half of the query/plan/commit
    /// split, and that bench lives in `examples/`, which is a separate
    /// crate. A deterministic search count is exactly the witness the
    /// metaplan asks for in preference to a wall-clock proxy.
    /// type-audit: bare-ok(count: return)
    pub fn searches(&self) -> u64 {
        self.searches
    }
```

- [ ] **Step 2: Write the bench**

Create `windows/vessel/examples/agent_scaling.rs`, modelled on
`windows/vessel/examples/turn_cost.rs` (read it first — same module-doc
shape, same `INFORMATIVE, NEVER A GATE` standing, same `--release`
requirement, same exempted-`Instant` comment from the Global Constraints).

The **contract** it must satisfy:

1. For each of several agent counts (e.g. 10, 50, 200), run T ticks and report:
   - wall ms per tick,
   - facts committed per agent per tick,
   - `HomeNavCache::searches` delta per agent per tick (the **plan** term),
   - the fitted log-log slope of ms-per-tick against agent count, reusing the
     `log_log_slope` shape from Task 3 (copy it; a shared helper is not worth
     a new module for two benches),
   - **bytes per agent** — a deterministic estimate, not RSS: `ledger.len() *
     size_of::<Fact>()` plus the heap each `Fact` owns (its `predicate` and
     `provenance` `String`s, and a `Value::Text` object), divided by agent
     count. RSS is not reproducible and cannot be compared across machines;
     this estimate can. Spec §6 lists it as a stage-1 deliverable and it is
     the input to the metaplan's memory arithmetic.
2. Report the **query / plan / commit split** at the largest agent count. Use
   whatever decomposition the driver actually admits — searches for plan,
   committed-fact count for commit, remainder for query — and **state in the
   module doc which term is a residual**, because a residual absorbs
   everything unmeasured and must not be read as if it were measured.

- [ ] **Step 3: Run it and paste the verbatim output into the module doc**

Run: `cargo run --release -p hornvale-vessel --example agent_scaling`
Fill in `## Measured` with date, `hostname -s`, profile, and verbatim output.

- [ ] **Step 4: Write the finding into your task report**

Answer the metaplan's §11 falsifiers explicitly:
- Does plan cost swamp query cost? If yes, **the program stops at stage 1** —
  say so plainly; that is a successful outcome, not a failed task.
- What is the fitted slope of tick cost against agent count?
- Does facts-per-agent-per-tick fall toward zero?

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs windows/vessel/examples/agent_scaling.rs
git commit -m "bench(vessel): agent scaling and the query/plan/commit split

The measurement that decides whether The Penstock's stages 2-8 are worth
building: if GOAP/A* planning dominates tick cost, the read optimisation is
optimising noise and the program stops here. Uses HomeNavCache's existing
deterministic search counter for the plan term rather than a wall-clock
proxy, via a new read-only accessor -- the field stays pub(crate), so
nothing outside the crate can write or reset it."
```

---

## Definition of Done

- [ ] All five tasks committed, each passing `make gate-commit`.
- [ ] `cargo test -p hornvale --test suite -- lens_purity` green — world
      identity did not move.
- [ ] `git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep
      -v '^$')` clean after `make rebaseline` (the type-audit report will have
      been committed in Task 2).
- [ ] Both benches have a filled-in `## Measured` section with real verbatim
      output, date, and hostname. An empty one means the task did not finish.
- [ ] The three §11 falsifier answers are written up — they are the input to
      the decision about stage 2, and this stage exists to produce them.
- [ ] A stage gate submitted: `make sluice-stage BRANCH=campaign/the-penstock
      REF=<full-sha>` (push the branch first; a SHA, never a branch name).

## Deferred from §5.7 on purpose

Spec §5.7 names three counters as belonging to the stage-1 instrument set.
Only one of them is measurable here, and pretending otherwise would produce
two counters that read zero forever:

- **replans per agent per hundred ticks** — Task 5, via
  `HomeNavCache::searches`. Measurable now.
- **invalidations dispatched per tick** — needs the dispatch path, which is
  stage 2. Nothing to count yet.
- **preemption-to-invalidation ratio** — needs both halves. Its denominator
  does not exist until stage 2.

## Out of scope for stage 1

Named so nobody helpfully adds them: any cache or view store; any eviction
policy; `place`- or `day`-keyed indexes; derived components; the condensation
boundary; predicate or provenance interning; anything touching `arbitrate` or
the motivation engine. Stage 1 is a bug fix and a set of instruments, and its
purpose is to tell us whether the rest is worth building.
