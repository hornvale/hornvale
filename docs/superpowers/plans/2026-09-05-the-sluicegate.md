# The Sluicegate Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the merge queue's state machine out of `scripts/sluice-queue.sh` into a unit-tested Rust tool at `tools/sluice`, without changing the on-disk format or any caller's behaviour.

**Architecture:** A library crate whose every verb is a function over an *injected* state directory, plus a thin `main` that parses argv and `HV_SLUICE_DIR` exactly once. `scripts/sluice-queue.sh` becomes a dispatcher that forwards the ported verbs to the binary and keeps the unported ones in bash, so the migration is incremental and reversible at every step. The TSV is untouched throughout.

**Tech Stack:** Rust (pinned 1.96.1 via `rust-toolchain.toml`), `std` only — `std::fs::File::lock()` provides the advisory lock and needs no crate. Tests are `cargo test` with `TempDir`-equivalent scratch dirs built by hand (no `tempfile` dependency).

## Global Constraints

- **No new dependencies.** `std::fs::File::lock()` is available on 1.96.1 and takes the same lock as `flock(1)`. Both facts are measured in the spec's §3.4. If a task seems to need a crate, stop and raise it.
- **The on-disk format does not change.** `queue.tsv` keeps its seven tab-separated columns in order: `when`, `id`, `branch`, `sha`, `state`, `kind`, `note`. `scripts/queue-watch.sh` and `make sluice-status` must remain **unmodified by this campaign** — that is a success criterion, not a nicety.
- **`HV_SLUICE_DIR` defaults to `$HOME/.local/state/hornvale/sluice`**, holds `queue.tsv` and `queue.lock`, and is read **once, in `main()`**. No library function may read an environment variable. This is the campaign's central discipline: the bug that discarded a merge product came from a library-level ambient read.
- **`state` vocabulary (closed):** `queued`, `running`, `held`, `landed`, `reported`, `superseded`, `dropped`.
- **`kind` vocabulary (closed):** `merge`, `stage`, `census`. Empty normalises to `merge` for rows written before the column existed.
- **Note sanitising is exact** (`scripts/sluice-queue.sh` `sanitize_note`): tab, CR and LF each become a single space. Strip, never reject — a queue whose job is durability must not fail closed on cosmetic input.
- **Every commit must pass `make gate-commit`**, and any commit touching `scripts/` must pass `bash scripts/lane-outboard.sh` before submission.

---

### Task 1: The crate, the row, and a lossless TSV round-trip

**Files:**
- Create: `tools/sluice/Cargo.toml`
- Create: `tools/sluice/src/lib.rs`
- Create: `tools/sluice/src/row.rs`
- Create: `tools/sluice/src/store.rs`
- Create: `tools/sluice/tests/suite.rs`
- Modify: `Cargo.toml` (add `tools/sluice` to the workspace `exclude` list)

**Interfaces:**
- Produces: `Row { when: String, id: String, branch: String, sha: String, state: String, kind: String, note: String }`; `Row::parse(&str) -> Option<Row>`; `Row::render(&self) -> String`; `Store::new(dir: PathBuf)`; `Store::read_rows() -> io::Result<Vec<Row>>`; `Store::write_rows(&[Row]) -> io::Result<()>`.

- [ ] **Step 1: Write the failing round-trip test**

```rust
// tools/sluice/tests/suite.rs
use sluice::row::Row;

#[test]
fn a_row_round_trips_through_render_and_parse() {
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\tmerge\ta note";
    let r = Row::parse(line).expect("parses");
    assert_eq!(r.state, "queued");
    assert_eq!(r.note, "a note");
    assert_eq!(r.render(), line);
}

#[test]
fn a_short_line_is_padded_and_kept_never_dropped() {
    // The shell pads and keeps; dropping would delete the row on next write.
    let r = Row::parse("TRUNCATED\tonly\tthree").expect("kept, not dropped");
    assert_eq!(r.when, "TRUNCATED");
    assert_eq!(r.state, "");
    assert_eq!(r.kind, "merge");
}

#[test]
fn an_empty_kind_normalises_to_merge() {
    let line = "2026-09-05T00:00:00Z\treq-abc-1\tcampaign/x\tdeadbeef\tqueued\t\t";
    assert_eq!(Row::parse(line).expect("parses").kind, "merge");
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test --manifest-path tools/sluice/Cargo.toml`
Expected: FAIL — the crate does not exist yet, so this is a compile error. That is the correct red for a crate-creation task; a behavioural red is not available until there is something to call.

- [ ] **Step 3: Create the crate and the row type**

`tools/sluice/Cargo.toml`:

```toml
[package]
name = "sluice"
version = "0.1.0"
edition = "2024"

[dependencies]
```

`tools/sluice/src/row.rs`:

```rust
//! One queue row. The TSV is the durable format and this type is its only
//! parser; see the plan's Global Constraints for the column order.

/// A single request in the queue.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Row {
    /// RFC3339 stamp written when the row was added.
    pub when: String,
    /// `req-<sha12>-<stamp>`, the row's exact identity.
    pub id: String,
    /// The branch the request names.
    pub branch: String,
    /// The full 40-character ref.
    pub sha: String,
    /// One of the closed state vocabulary.
    pub state: String,
    /// One of `merge`, `stage`, `census`.
    pub kind: String,
    /// Free text, already sanitised.
    pub note: String,
}

impl Row {
    /// Parse one TSV line. TOTAL — never returns `None` for a short line.
    ///
    /// The shell it replaces PADS a malformed row out to seven fields and
    /// keeps it; measured 2026-09-05 by feeding `set-state` a three-field
    /// line and watching it survive as `TRUNCATED\tonly\tthree\t\t\tmerge\t`.
    /// Dropping such a line here would make the next `write_rows` delete it
    /// permanently, silently losing a request — which the plan's own Global
    /// Constraints forbid (the format does not change) and which is the exact
    /// opposite of a queue whose first duty is durability.
    pub fn parse(line: &str) -> Option<Row> {
        let f: Vec<&str> = line.split('\t').collect();
        let g = |i: usize| f.get(i).copied().unwrap_or("");
        let kind = if g(5).is_empty() { "merge" } else { g(5) };
        Some(Row {
            when: g(0).to_string(),
            id: g(1).to_string(),
            branch: g(2).to_string(),
            sha: g(3).to_string(),
            state: g(4).to_string(),
            kind: kind.to_string(),
            note: g(6).to_string(),
        })
    }

    /// Render back to one TSV line, no trailing newline.
    pub fn render(&self) -> String {
        format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.when, self.id, self.branch, self.sha, self.state, self.kind, self.note
        )
    }
}
```

`tools/sluice/src/lib.rs`:

```rust
//! The merge queue's state machine.
#![warn(missing_docs)]

pub mod row;
pub mod store;
```

- [ ] **Step 4: Write the store, with the directory INJECTED**

`tools/sluice/src/store.rs`:

```rust
//! The on-disk queue. The state directory is a constructor argument and is
//! never read from the environment here — a library-level ambient read is what
//! let a test address a live chamber and discard a merge product on
//! 2026-09-04.

use crate::row::Row;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// The queue's durable state, rooted at one directory.
pub struct Store {
    dir: PathBuf,
}

impl Store {
    /// Root the store at `dir`. The directory is created if absent.
    pub fn new(dir: PathBuf) -> io::Result<Store> {
        fs::create_dir_all(&dir)?;
        Ok(Store { dir })
    }

    /// `queue.tsv` under the store's directory.
    pub fn queue_path(&self) -> PathBuf {
        self.dir.join("queue.tsv")
    }

    /// `queue.lock` under the store's directory.
    pub fn lock_path(&self) -> PathBuf {
        self.dir.join("queue.lock")
    }

    /// Every parseable row, in file order. A missing file is an empty queue,
    /// not an error: the first `add` on a fresh box must not have to
    /// special-case its own creation.
    pub fn read_rows(&self) -> io::Result<Vec<Row>> {
        let p = self.queue_path();
        if !Path::new(&p).exists() {
            return Ok(Vec::new());
        }
        Ok(fs::read_to_string(&p)?
            .lines()
            .filter_map(Row::parse)
            .collect())
    }

    /// Replace the file with these rows, via a temp file and a rename so a
    /// reader never sees a half-written queue.
    pub fn write_rows(&self, rows: &[Row]) -> io::Result<()> {
        let tmp = self.dir.join(".queue.tmp");
        let body: String = rows
            .iter()
            .map(|r| r.render() + "\n")
            .collect();
        fs::write(&tmp, body)?;
        fs::rename(&tmp, self.queue_path())
    }
}
```

- [ ] **Step 5: Add a store round-trip test**

```rust
// tools/sluice/tests/suite.rs — append
use sluice::store::Store;
use std::path::PathBuf;

fn scratch(name: &str) -> PathBuf {
    let d = std::env::temp_dir().join(format!("sluice-test-{}-{}", name, std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    d
}

#[test]
fn rows_survive_a_write_then_read() {
    let s = Store::new(scratch("roundtrip")).expect("store");
    let r = Row::parse("2026-09-05T00:00:00Z\treq-a-1\tb\tsha\tqueued\tmerge\t").expect("row");
    s.write_rows(&[r.clone()]).expect("write");
    assert_eq!(s.read_rows().expect("read"), vec![r]);
}

#[test]
fn a_missing_queue_file_reads_as_empty_not_an_error() {
    let s = Store::new(scratch("missing")).expect("store");
    assert!(s.read_rows().expect("read").is_empty());
}
```

- [ ] **Step 6: Exclude the crate from the cargo workspace**

Add `"tools/sluice"` to the `exclude` array in the root `Cargo.toml`. Verify it did not join the workspace:

Run: `cargo metadata --format-version 1 --no-deps | grep -c '"name":"sluice"'`
Expected: `0`. A non-zero count means the crate joined the workspace, where the `ALLOWED_EXTERNAL` allowlist in `cli/tests/architecture.rs` would bind it — fix the `exclude` entry before continuing.

- [ ] **Step 7: Run the tests and the commit gate**

Run: `cargo test --manifest-path tools/sluice/Cargo.toml`
Expected: PASS, 4 tests.

Run: `make gate-commit`
Expected: rc=0.

- [ ] **Step 8: Commit**

```bash
git add tools/sluice Cargo.toml
git commit -m "feat(sluice): the queue row and its store, with the state dir injected

Nothing calls this yet. The store takes its directory as a constructor
argument and no library function reads the environment, which is the
discipline the campaign exists to buy: an ambient read at library level is
what let a test address a live chamber and discard a merge product.

Sluice-Headline: the queue's row and store arrive in Rust, untested by any caller yet"
```

---

### Task 2: `set-state` and the lock, including the refusal that matters

**There is deliberately no library `list()`.** `list` is a pure read with no
transaction, so Task 4's `main.rs` implements it directly from
`Store::read_rows()`. An earlier title named this task for both verbs and
specified only one, which is a naming defect rather than a missing deliverable.

**Files:**
- Create: `tools/sluice/src/verbs.rs`
- Modify: `tools/sluice/src/lib.rs` (add `pub mod verbs;`)
- Modify: `tools/sluice/src/store.rs` (add `Store::lock()`, used by `set_state` here and by `claim` in Task 3)
- Modify: `tools/sluice/tests/suite.rs`

**Interfaces:**
- Consumes: `Store`, `Row` from Task 1, and `Store::lock()` — which Task 3 also uses. Implement `Store::lock()` in THIS task (its code is in Task 3's Step 3); `set_state` must hold it across its read-modify-write.
- Produces: `sanitize_note(&str) -> String`; `validate_state(&str) -> bool`; `validate_kind(&str) -> bool`; `set_state(&Store, id: &str, state: &str, note: Option<&str>) -> Result<(), SetStateError>` where `SetStateError::NoSuchRow` is distinct from `SetStateError::BadState`.

- [ ] **Step 1: Write the failing tests**

```rust
// tools/sluice/tests/suite.rs — append
use sluice::verbs::{sanitize_note, set_state, SetStateError};

#[test]
fn a_note_loses_tabs_and_newlines_to_single_spaces() {
    assert_eq!(sanitize_note("a\tb\nc\rd"), "a b c d");
}

#[test]
fn set_state_changes_exactly_the_named_row() {
    let s = Store::new(scratch("setstate")).expect("store");
    s.write_rows(&[
        Row::parse("w\treq-a\tb1\tsha1\tqueued\tmerge\t").unwrap(),
        Row::parse("w\treq-b\tb2\tsha2\tqueued\tmerge\t").unwrap(),
    ]).unwrap();
    set_state(&s, "req-b", "running", Some("mine")).expect("ok");
    let rows = s.read_rows().unwrap();
    assert_eq!(rows[0].state, "queued");
    assert_eq!(rows[1].state, "running");
    assert_eq!(rows[1].note, "mine");
}

#[test]
fn an_unmatched_id_is_a_refusal_and_changes_nothing() {
    let s = Store::new(scratch("nomatch")).expect("store");
    let before = vec![Row::parse("w\treq-a\tb\tsha\tqueued\tmerge\t").unwrap()];
    s.write_rows(&before).unwrap();
    let e = set_state(&s, "req-typo", "held", None).expect_err("must refuse");
    assert!(matches!(e, SetStateError::NoSuchRow));
    assert_eq!(s.read_rows().unwrap(), before);
}
```

The third test is the load-bearing one. `scripts/sluice-queue.sh`'s own comment records why: until 2026-08-19 a mistyped id rewrote every row unchanged and exited 0, so an operator believed a finished run's row had been set terminal, the row stayed `running`, and coalescing then refused to supersede it — a campaign's resubmission queued behind a job that had already exited. One digit, and the queue said nothing.

- [ ] **Step 2: Run and confirm they fail**

Run: `cargo test --manifest-path tools/sluice/Cargo.toml`
Expected: FAIL — `verbs` does not exist.

- [ ] **Step 3a: Add the lock to the store**

```rust
// tools/sluice/src/store.rs — append to impl Store
    /// Take the queue's advisory lock, held until the returned handle drops.
    /// This is the SAME lock `flock(1)` takes, which is what lets bash and
    /// Rust callers coexist during the migration.
    pub fn lock(&self) -> io::Result<fs::File> {
        let f = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(false)
            .open(self.lock_path())?;
        f.lock()?;
        Ok(f)
    }
```

`File::lock` is stable on the repo's pinned 1.96.1 and needs no crate; both
that and its interoperability with `flock(1)` are measured in the spec's §3.4.

- [ ] **Step 3b: Implement the verbs**

```rust
// tools/sluice/src/verbs.rs
//! The queue's verbs. Each takes its `Store` explicitly.

use crate::row::Row;
use crate::store::Store;

/// Why `set_state` refused.
#[derive(Debug)]
pub enum SetStateError {
    /// The state is outside the closed vocabulary.
    BadState,
    /// No row carries that id. NOTHING was changed.
    NoSuchRow,
    /// The store could not be read or written.
    Io(std::io::Error),
}

/// Tab, CR and LF each become one space. Strip, never reject: a tab shifts
/// every later field, and a newline is read as a whole separate row by the
/// next reader and re-emitted forever.
pub fn sanitize_note(s: &str) -> String {
    s.replace(['\t', '\r', '\n'], " ")
}

/// The closed state vocabulary.
pub fn validate_state(s: &str) -> bool {
    matches!(s, "queued" | "running" | "held" | "landed" | "reported" | "superseded" | "dropped")
}

/// The closed kind vocabulary.
pub fn validate_kind(s: &str) -> bool {
    matches!(s, "merge" | "stage" | "census")
}

/// Set one row's state, and its note when `note` is `Some` and non-empty.
pub fn set_state(
    store: &Store,
    id: &str,
    state: &str,
    note: Option<&str>,
) -> Result<(), SetStateError> {
    if !validate_state(state) {
        return Err(SetStateError::BadState);
    }
    // THE LOCK IS NOT OPTIONAL HERE. `scripts/sluice-queue.sh`'s `set-state`
    // takes `with_lock` before its rewrite; an earlier draft of this plan
    // dropped it, which would have let an unlocked set-state race a locked
    // claim through the same read-modify-write and the same temp path.
    let _guard = store.lock().map_err(SetStateError::Io)?;
    let mut rows = store.read_rows().map_err(SetStateError::Io)?;
    let mut matched = false;
    for r in rows.iter_mut() {
        if r.id == id {
            matched = true;
            r.state = state.to_string();
            if let Some(n) = note {
                let n = sanitize_note(n);
                if !n.is_empty() {
                    r.note = n;
                }
            }
        }
    }
    if !matched {
        return Err(SetStateError::NoSuchRow);
    }
    store.write_rows(&rows).map_err(SetStateError::Io)
}
```

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test --manifest-path tools/sluice/Cargo.toml`
Expected: PASS, 8 tests (5 from Task 1 plus 3 here).

- [ ] **Step 5: Mutate the refusal to prove the test can fail**

Change `if !matched { return Err(...) }` to `if false { return Err(...) }`, run the tests, and confirm `an_unmatched_id_is_a_refusal_and_changes_nothing` FAILS. Restore.

Expected: exactly one test fails. A guard never observed failing is not known to work.

- [ ] **Step 6: Commit**

```bash
git add tools/sluice
git commit -m "feat(sluice): list and set-state, with the unmatched-id refusal

The refusal is ported deliberately, not incidentally: a mistyped id used to
rewrite every row unchanged and exit 0, which is how a ghost row was made.
Mutation-tested — reverting the refusal fails exactly the test that names it.

Sluice-Headline: set-state refuses an id that matches no row, in Rust"
```

---

### Task 3: `claim`, the transaction the campaign exists for

**Files:**
- Modify: `tools/sluice/src/verbs.rs`
- Modify: `tools/sluice/src/store.rs` (add `Store::lock()`)
- Modify: `tools/sluice/tests/suite.rs`

**Interfaces:**
- Consumes: `Store`, `Row`, `sanitize_note` from Tasks 1–2.
- Consumes: `Store::lock()` from Task 2. NOTE: `verbs.rs` currently has no `use crate::row::Row;` — Task 2 needed none and an unused import is a clippy error here. `claim`'s signature names `Row`, so add the import in this task. Produces: `claim(&Store, sha: Option<&str>, note: Option<&str>) -> Result<Option<Row>, ClaimError>` with `ClaimError::HeldByAnother` and `ClaimError::NoSuchRow` mapping to exit codes 4 and 5.

- [ ] **Step 1: Write the failing tests, including the concurrency one**

```rust
// tools/sluice/tests/suite.rs — append
use sluice::verbs::{claim, ClaimError};

#[test]
fn claim_marks_the_row_running_in_the_same_call_that_selects_it() {
    let s = Store::new(scratch("claim1")).expect("store");
    s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t").unwrap()]).unwrap();
    let got = claim(&s, None, Some("taken")).expect("ok").expect("a row");
    assert_eq!(got.id, "req-a");
    assert_eq!(s.read_rows().unwrap()[0].state, "running");
}

#[test]
fn claim_by_sha_on_a_held_row_refuses_and_changes_nothing() {
    let s = Store::new(scratch("claim4")).expect("store");
    let before = vec![Row::parse("w\treq-a\tb\tsha1\trunning\tmerge\t").unwrap()];
    s.write_rows(&before).unwrap();
    let e = claim(&s, Some("sha1"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::HeldByAnother));
    assert_eq!(s.read_rows().unwrap(), before);
}

#[test]
fn claim_by_sha_for_an_absent_ref_is_a_different_answer() {
    let s = Store::new(scratch("claim5")).expect("store");
    s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t").unwrap()]).unwrap();
    let e = claim(&s, Some("nope"), None).expect_err("must refuse");
    assert!(matches!(e, ClaimError::NoSuchRow));
}

#[test]
fn racing_claimants_produce_exactly_one_winner_per_queued_row() {
    // 32 rounds x 32 threads = 1024 claim attempts, but never more than 32
    // file descriptors at once. Concurrency is BOUNDED on purpose: this suite
    // gates every merge once Task 4 lands, and macOS defaults `ulimit -n` to
    // 256, so a 1000-thread version would fail on a developer's laptop for a
    // reason that has nothing to do with the property under test. A flaky test
    // in the gate blocks everyone; that happened on 2026-09-05 and once is
    // enough. 32 concurrent is ample: the shell equivalent of this mutation
    // produced 12 winners out of 12.
    for round in 0..32 {
        let s = Store::new(scratch(&format!("claimrace-{round}"))).expect("store");
        s.write_rows(&[Row::parse("w\treq-a\tb\tsha1\tqueued\tmerge\t").unwrap()]).unwrap();
        let winners = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let dir = s.queue_path().parent().expect("dir").to_path_buf();
        let mut hs = Vec::new();
        for _ in 0..32 {
            let d = dir.clone();
            let w = winners.clone();
            hs.push(std::thread::spawn(move || {
                let s = Store::new(d).expect("store");
                if let Ok(Some(_)) = claim(&s, None, None) {
                    w.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                }
            }));
        }
        for h in hs {
            h.join().expect("thread");
        }
        assert_eq!(
            winners.load(std::sync::atomic::Ordering::SeqCst),
            1,
            "round {round}: more than one claimant won the same row"
        );
    }
}
```

The last test is the campaign's whole point and is why this moved out of shell: the equivalent in `scripts/test-sluice.sh` spawns 12 processes and takes seconds, so it can only afford one round. This runs 1024 attempts in milliseconds, across 32 rounds of 32 — bounded so it cannot exhaust file descriptors on a host with a low `ulimit -n`.

- [ ] **Step 2: Run and confirm they fail**

Run: `cargo test --manifest-path tools/sluice/Cargo.toml`
Expected: FAIL — `claim` does not exist.

- [ ] **Step 3: Confirm the lock already exists**

`Store::lock()` was added in Task 2, which needs it for `set_state`. Do not
re-add it. Confirm it is present and used:

Run: `grep -n "pub fn lock" tools/sluice/src/store.rs`
Expected: one match. If there are none, Task 2 is incomplete — stop and say so
rather than adding a second copy.

- [ ] **Step 4: Implement `claim`**

```rust
// tools/sluice/src/verbs.rs — append

/// Why `claim` refused.
#[derive(Debug)]
pub enum ClaimError {
    /// A row for that ref exists but is not `queued` — somebody else has it.
    /// Exit code 4.
    HeldByAnother,
    /// No row at all for that ref. Exit code 5; an ad hoc run, not a race.
    NoSuchRow,
    /// The store could not be read or written.
    Io(std::io::Error),
}

/// Atomically select a `queued` row and mark it `running`, returning it.
///
/// Selecting and marking happen under ONE lock acquisition, which is the whole
/// point: the shell version released its lock between `next` and `set-state`,
/// so two dispatchers both saw an unclaimed row and one merge ran twice.
///
/// With `sha`, claims the row for that ref (an executor, which knows its ref
/// and not its id). Without, claims the first queued row (a dispatcher).
pub fn claim(
    store: &Store,
    sha: Option<&str>,
    note: Option<&str>,
) -> Result<Option<Row>, ClaimError> {
    let _guard = store.lock().map_err(ClaimError::Io)?;
    let mut rows = store.read_rows().map_err(ClaimError::Io)?;
    let mut seen_sha = false;
    let mut idx = None;
    for (i, r) in rows.iter().enumerate() {
        if let Some(want) = sha {
            if r.sha == want {
                seen_sha = true;
            }
        }
        if idx.is_none()
            && r.state == "queued"
            && sha.map(|w| r.sha == w).unwrap_or(true)
        {
            idx = Some(i);
        }
    }
    let Some(i) = idx else {
        if sha.is_some() && seen_sha {
            return Err(ClaimError::HeldByAnother);
        }
        if sha.is_some() {
            return Err(ClaimError::NoSuchRow);
        }
        return Ok(None);
    };
    rows[i].state = "running".to_string();
    if let Some(n) = note {
        let n = sanitize_note(n);
        if !n.is_empty() {
            rows[i].note = n;
        }
    }
    let claimed = rows[i].clone();
    store.write_rows(&rows).map_err(ClaimError::Io)?;
    Ok(Some(claimed))
}
```

- [ ] **Step 5: Run to verify they pass**

Run: `cargo test --manifest-path tools/sluice/Cargo.toml`
Expected: PASS, 11 tests.

- [ ] **Step 6: Mutate the lock away and prove the race test fails**

Delete the `let _guard = store.lock()...;` line, run the tests, and record the winner count the race test reports.

Expected: `racing_claimants_produce_exactly_one_winner_per_queued_row` FAILS, naming the round and a count greater than 1. Restore the line. If it still passes, the test is not exercising the race — raise it rather than proceeding, because the shell equivalent of this mutation produced 12 winners out of 12 and a green here would mean the in-process version is weaker than the one it replaces.

- [ ] **Step 7: Commit**

```bash
git add tools/sluice
git commit -m "feat(sluice): claim selects and marks under one lock

The shell version released its lock between next and set-state, so two
dispatchers both saw an unclaimed row and one merge ran twice (pids
1240741/1253175, two ~800 KB logs for 48aa9373b6f2, both rc=0). Here the
read-modify-write cannot straddle a process boundary because it is not one.

Mutation-tested by deleting the lock: the 1000-claimant race then reports
more than one winner.

Sluice-Headline: claim becomes a transaction rather than a read followed by a write"
```

---

### Task 4: The binary, the hybrid shim, and the gate

**Files:**
- Create: `tools/sluice/src/main.rs`
- Modify: `scripts/sluice-queue.sh` (forward `claim`, `set-state`, `list` — NOT `next`, which stays in bash and is unused by the chamber since `claim` replaced it)
- Modify: `scripts/lane-outboard.sh` (gate `tools/sluice`)

**Interfaces:**
- Consumes: everything from Tasks 1–3.
- Produces: the `sluice` binary with verbs `claim`, `set-state`, `list`; exit codes 0 success, 2 usage, 4 `HeldByAnother`, 5 `NoSuchRow`.

- [ ] **Step 1: Write `main.rs`, the ONLY place the environment is read**

```rust
//! The queue's CLI. `HV_SLUICE_DIR` is parsed here and nowhere else — every
//! library function takes its store explicitly.

use sluice::store::Store;
use sluice::verbs::{claim, set_state, ClaimError, SetStateError};
use std::path::PathBuf;

fn state_dir() -> PathBuf {
    if let Ok(d) = std::env::var("HV_SLUICE_DIR") {
        return PathBuf::from(d);
    }
    let home = std::env::var("HOME").expect("HOME is set");
    PathBuf::from(home).join(".local/state/hornvale/sluice")
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let store = Store::new(state_dir()).expect("state dir");
    match args.first().map(String::as_str) {
        Some("list") => {
            for r in store.read_rows().expect("read") {
                println!("{}", r.render());
            }
        }
        Some("set-state") => {
            let id = args.get(1).expect("usage: set-state <id> <state> [note]");
            let st = args.get(2).expect("usage: set-state <id> <state> [note]");
            match set_state(&store, id, st, args.get(3).map(String::as_str)) {
                Ok(()) => {}
                Err(SetStateError::NoSuchRow) => {
                    eprintln!("sluice: set-state: no row with id '{id}' — NOTHING WAS CHANGED.");
                    std::process::exit(1);
                }
                Err(SetStateError::BadState) => {
                    eprintln!("sluice: set-state: '{st}' is not a known state");
                    std::process::exit(1);
                }
                Err(SetStateError::Io(e)) => {
                    eprintln!("sluice: set-state: {e}");
                    std::process::exit(1);
                }
            }
        }
        Some("claim") => {
            let (sha, note) = if args.get(1).map(String::as_str) == Some("--sha") {
                (args.get(2).map(String::as_str), args.get(3).map(String::as_str))
            } else {
                (None, args.get(1).map(String::as_str))
            };
            match claim(&store, sha, note) {
                Ok(Some(r)) => println!("{}", r.render()),
                Ok(None) => {}
                Err(ClaimError::HeldByAnother) => {
                    eprintln!("sluice: claim: a row for that ref exists but is NOT queued — somebody else has it. NOTHING WAS CHANGED.");
                    std::process::exit(4);
                }
                Err(ClaimError::NoSuchRow) => {
                    eprintln!("sluice: claim: no row at all for that ref.");
                    std::process::exit(5);
                }
                Err(ClaimError::Io(e)) => {
                    eprintln!("sluice: claim: {e}");
                    std::process::exit(1);
                }
            }
        }
        other => {
            eprintln!("sluice: unknown command {other:?}");
            std::process::exit(2);
        }
    }
}
```

- [ ] **Step 2: Forward the three ported verbs from the shell script**

In `scripts/sluice-queue.sh`, immediately after the `cmd` is read and before the existing `case`, insert:

```bash
# THE PORTED VERBS GO TO tools/sluice. `add` stays here for now: it sources
# sluice-headline.sh, resolves three-valued ancestry and coalesces, all of
# which shell out to git, and moving it is Task 6 rather than a side effect
# of this one. The binary is built on demand and the path is resolved from
# this script's own location so a caller's cwd cannot change which one runs.
case "$cmd" in
    claim|set-state|list)
        sluice_bin="$(dirname "$0")/../tools/sluice/target/release/sluice"
        if [ ! -x "$sluice_bin" ]; then
            cargo build --quiet --release --manifest-path \
                "$(dirname "$0")/../tools/sluice/Cargo.toml" >&2
        fi
        exec "$sluice_bin" "$cmd" "$@"
        ;;
esac
```

- [ ] **Step 3: Run the EXISTING shell suite against the Rust implementation**

Run: `bash scripts/test-sluice.sh`
Expected: 220 passed, 0 failed. This is the integration check that the binary and its callers agree, and it is why the shell suite is kept rather than deleted. If a test fails here, the port changed behaviour — fix the Rust, not the test.

- [ ] **Step 4: Gate the new crate — tests AND lint AND fmt**

`make gate-commit` is workspace-scoped and never visits an excluded crate, and
the `outboard` tool lines run `cargo test` only. So without the two extra lines
below, nothing in this repo would ever run clippy or rustfmt against
`tools/sluice` — measured on 2026-09-05, when Task 1's code was committed with
a live `-D warnings` failure and a fmt diff that no gate could see.

In `scripts/lane-outboard.sh`, beside the five existing tool lines (`tools/board` at line 42 onward), add:

```bash
run "tools/sluice"     cargo test --manifest-path tools/sluice/Cargo.toml
run "tools/sluice fmt" cargo fmt --manifest-path tools/sluice/Cargo.toml --check
run "tools/sluice lint" cargo clippy --manifest-path tools/sluice/Cargo.toml --all-targets -- -D warnings
```

- [ ] **Step 5: Run the whole outboard set**

Run: `bash scripts/lane-outboard.sh`
Expected: all suites passed, rc=0.

- [ ] **Step 6: Verify the format contract held**

Run: `git status --porcelain scripts/queue-watch.sh Makefile`
Expected: empty for `queue-watch.sh`. If either file needed changing to make this task pass, the on-disk format moved and that is a stop-and-raise, not a fix-forward.

- [ ] **Step 7: Commit**

```bash
git add tools/sluice scripts/sluice-queue.sh scripts/lane-outboard.sh
git commit -m "feat(sluice): route claim, set-state and list through the Rust binary

A hybrid shim, not a replacement: add stays in bash because it sources
sluice-headline.sh, resolves three-valued ancestry and coalesces, and moving
that is its own task. Every existing caller is unchanged, and the shell suite
now exercises the Rust implementation — 220/0.

Sluice-Headline: the ported queue verbs run in Rust behind the existing script"
```

---

### Task 5: `sluice-run.sh` takes a request ID, and the leaked variable dies

**Files:**
- Modify: `scripts/sluice-run.sh`
- Modify: `scripts/sluice-drain.sh`
- Modify: `scripts/test-sluice.sh`

**Interfaces:**
- Consumes: the `sluice` binary's `list` output from Task 4.
- Produces: `sluice-run.sh <request-id>` reads branch/sha/kind from the row; the positional `<branch> <sha> [kind]` form survives for ad hoc runs with no row.

- [ ] **Step 1: Write the failing test**

```bash
# scripts/test-sluice.sh — append near the other chamber tests
echo "== sluice-run: a request id supplies branch, sha and kind from the row"
: > "$CQ"
row jjj campaign/j jjjjjjjjjjjj queued stage >> "$CQ"
set +e
idout="$(HV_SLUICE_DIR="$cdir" bash "$repo_root/scripts/sluice-run.sh" req-jjj 2>&1)"
set -e
if printf '%s' "$idout" | grep -q "kind=stage"; then
    ok "a request id is resolved to its row's branch, sha and kind"
else
    bad "sluice-run did not resolve req-jjj to kind=stage — an operator-typed kind can still disagree with the row"
fi
```

This is `PROC-stage-request-should-read-its-own-queue-row`, filed after an operator hand-typed `merge` for a `kind=stage` request and landed it on main.

- [ ] **Step 2: Run and confirm it fails**

Run: `bash scripts/test-sluice.sh 2>&1 | grep "request id"`
Expected: FAIL — the positional form treats `req-jjj` as a branch name.

- [ ] **Step 3: Resolve a request id at the top of `sluice-run.sh`**

Immediately after the existing `branch=`/`sha=`/`kind=` assignments, insert:

```bash
# A SINGLE ARGUMENT THAT LOOKS LIKE A REQUEST ID IS ONE. Reading branch, sha
# and kind out of the row removes the class of failure where an
# operator-supplied value disagrees with the row that authorised the run — a
# hand-typed `merge` for a kind=stage request landed on main once already.
# It also removes the need for HV_SLUICE_CLAIMED: the runner now knows WHICH
# ROW authorised it, so nothing has to be exported to tell it.
case "$branch" in
    req-*)
        _row="$(bash "$repo_root/scripts/sluice-queue.sh" list \
                | awk -F'\t' -v i="$branch" '$2==i {print; exit}')"
        if [ -z "$_row" ]; then
            echo "sluice-run: no queue row with id '$branch'" >&2
            exit 2
        fi
        queue_row_id="$branch"
        branch="$(printf '%s' "$_row" | cut -f3)"
        sha="$(printf '%s' "$_row" | cut -f4)"
        kind="$(printf '%s' "$_row" | cut -f6)"
        [ -n "$kind" ] || kind="merge"
        ;;
esac
```

- [ ] **Step 4: Delete `HV_SLUICE_CLAIMED` entirely**

Remove the `export HV_SLUICE_CLAIMED="$ID"` line from `scripts/sluice-drain.sh`, and in `scripts/sluice-run.sh` remove the `if [ -n "${HV_SLUICE_CLAIMED:-}" ]` branch, its `unset`, and the `env -u HV_SLUICE_CLAIMED` guards in `scripts/test-sluice.sh`. Have drain pass the id instead:

```bash
(cd "$repo_root" && bash "$runner" "$ID")
```

- [ ] **Step 5: Verify the variable is gone**

Run: `grep -rn HV_SLUICE_CLAIMED scripts/ tools/ | grep -v '^scripts/CLAUDE.md'`
Expected: no output. A surviving reference means one caller still coordinates through ambient state, which is the defect this task exists to delete.

- [ ] **Step 6: Run both suites under the chamber's own condition**

Run: `bash scripts/test-sluice.sh`
Expected: PASS, 221 tests.

Run: `HV_SLUICE_CLAIMED=req-fake bash scripts/test-sluice.sh`
Expected: PASS, identical count. The variable must now be inert; a difference between these two runs means something still reads it.

- [ ] **Step 7: Commit**

```bash
git add scripts/sluice-run.sh scripts/sluice-drain.sh scripts/test-sluice.sh
git commit -m "feat(sluice): the runner takes a request id, and the exported claim dies

HV_SLUICE_CLAIMED existed only because the runner was told WHAT to run
rather than WHICH ROW authorised it. Given an id there is nothing to export,
inherit or unset — which matters because drain exported it into the whole
phase tree, and a nested sluice-run.sh inherited it and skipped the
interlock entirely.

Closes PROC-stage-request-should-read-its-own-queue-row.

Sluice-Headline: the chamber is told which row authorised it, not what to run"
```

---

### Not planned here: `add`, coalescing, and the pure shim

**`add` is deliberately NOT a task in this plan, and that is a finding from
writing it rather than a scoping preference.** The first draft carried it as
Task 6 and the self-review rejected that draft on the skill's own criteria: it
referenced a `Git` type no task defined, and its central step read "port the
coalescing rules... preserving all three", which is a description of work
rather than the work. A task an implementer cannot execute from its own text
is not a task.

The reason it resisted planning is worth recording, because it is also the
reason it is the riskiest part of the migration. `add` is not a state
transition with some git beside it — it is four coupled behaviours:

- it sources `scripts/sluice-headline.sh` and refuses a merge with no usable
  `Sluice-Headline:` trailer, printing eleven lines of guidance;
- it resolves ancestry **three-valued**, where exit 128 means "this box cannot
  resolve these objects" and must never collapse into "not an ancestor" — it
  did, silently, for two days;
- it coalesces per-branch AND per-kind, and must never supersede a `running`
  row, because that orphans an authoring job mid-write;
- it treats an unresolvable ancestry as "accept and stamp the row", because the
  queue's first duty is durability and refusing a real request over a missing
  object would lose it.

Each of those is a defended incident with a comment explaining it. Porting them
needs its own plan, written **after Task 5 lands**, when the shim's shape and
the subprocess boundary for git are known rather than guessed. Until then
`scripts/sluice-queue.sh` stays a **hybrid** shim: ported verbs to the binary,
`add` in bash. That is a stable, shippable end state, not a half-migration — the
spec's §5 makes step 2 (callers move to the binary) the point at which value is
delivered, and steps 3-4 explicitly optional.

**`scripts/sluice-mouth.sh` is likewise out of scope here.** The spec names it a
second wave; its five-valued exit is a contract `sluice-run.sh` offsets by 20,
and no caller audit has been done. It needs its own compatibility check before
anyone moves it.
