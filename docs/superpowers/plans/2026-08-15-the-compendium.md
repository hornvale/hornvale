# The Compendium Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship a second corpus family, `systems/`, that scores Hornvale against
external catalogues of game-system capability — with Wolverson's Rust roguelike
tutorial as its first column — where every verdict cites a machine-checked
anchor so the artifact reddens instead of rotting.

**Architecture:** Corpus is **data** (`systems/*.system.json`), resolver is
**code** (`cli/src/systems.rs`) — decision 0011. The resolver reads four files
and **builds no world**: unlike `cmd_tropes`, which needs the live concept
registry and pays a full `build_world`, every anchor here resolves against
`docs/digest/decisions-in-force.md`, `book/src/frontier/idea-registry.md`, and
the filesystem. That makes the ratchet nearly free.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only (decision 0004 —
**no new crates**), `std` for everything else.

**Spec:** `docs/superpowers/specs/2026-08-15-the-compendium-design.md`

## Where the tests live, and why

**Task 0 gives `hornvale` a lib target**, which it does not have today. After
it, `cli/tests/*.rs` can `use hornvale::systems::…` directly, and the split is:

| test kind | lives in | reaches the code by |
|---|---|---|
| corpus freeze, anchor audit, surplus | `cli/tests/system_coverage.rs` | `use hornvale::systems` |
| report / matrix goldens | `cli/tests/system_coverage.rs` | `CARGO_BIN_EXE_hornvale` |

The goldens keep driving the **binary** even though the library is now
reachable, and that is deliberate rather than leftover: it exercises the real
dispatch path, which is exactly where `cmd_tropes`'s documented false-pass bug
lived (`tropes --corpus X check` emitted a report and exited 0). A golden test
that called `render` directly would not have caught it.

## Global Constraints

- **No new dependencies.** `serde`, `serde_json`, `libm` are the entire
  workspace allowlist (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only — enforced by
  `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Enforced by the same lint.
- Every crate sets `#![warn(missing_docs)]`; **every** `pub` item, field and
  variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.
  After any change to a `pub` boundary, regenerate the report **in the same
  commit**: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
- `cargo fmt` is the final step before **every** commit. Fmt-gate skips are the
  most common review finding in this repo.
- `docs/audits/` is already declared in `docs/generated-paths.txt` — **no edit
  needed there** — but the first commit introducing each new artifact must
  `git add` it, because `git diff --exit-code` against an untracked path is
  silently vacuous and would pass forever.
- The resolver must **never shell out to `cargo`**. The ratchet test runs
  inside the build; a resolver that invokes `cargo` would build inside a build.

---

### Task 0: Give the CLI a library target

**Files:**
- Create: `cli/src/lib.rs`
- Modify: `cli/src/main.rs` (drop the 8 `mod` declarations, hoist `flag_value`)
- Modify: `cli/src/audio.rs`, `cli/src/proto.rs` (promote `pub(crate)` → `pub`)

**Interfaces:**
- Produces: the `hornvale` library crate, exposing `audio`, `concepts`,
  `dictionary`, `phonology`, `proto`, `repl`, `streams`, `tropes`, and
  `flag_value`.

**Why this is here.** `cli/` is described in CLAUDE.md as "the thin command
surface", but the crate is binary-only, so none of its eight modules can be
reached by a test except through the built binary. This task closes that gap
before adding a ninth module. **It is scope (a) only** — the 49 `cmd_*`
functions stay in `main.rs` for now; moving them is a separate campaign (see
Step 5).

**Measured before planning, not assumed:** a probe lib target compiled with
**0 missing_docs warnings** and 6 warnings total, all `never used` on
`pub(crate)` items that only the binary calls. The modules are already fully
documented. Do not budget for a doc-writing wave.

- [ ] **Step 1: Create the library root**

`cli/src/lib.rs`:

```rust
//! The hornvale CLI as a library: every command module, reachable by tests.
//!
//! `main.rs` is the binary that dispatches into these. The split exists so a
//! command's logic can be tested directly instead of only through a built
//! binary — `cli/` is the thin command surface, and this is what makes that
//! description true of the crate and not merely of its intent.
#![warn(missing_docs)]

pub mod audio;
pub mod concepts;
pub mod dictionary;
pub mod phonology;
pub mod proto;
pub mod repl;
pub mod streams;
pub mod tropes;

/// Read the value following `flag` in `args`, if present.
///
/// Hoisted out of `main.rs` because `audio.rs` reaches it as
/// `crate::flag_value`, and a module moving into the library cannot reach a
/// helper that stayed in the binary.
///
/// **This returns the next token unconditionally**, so it is correct only for
/// flags that take a value. `cmd_concepts` takes `--manifest` with no value;
/// see `cmd_tropes`'s mode scan for what that asymmetry has already cost.
pub fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}
```

- [ ] **Step 2: Rewire `main.rs`**

Delete lines 4–11 (`mod audio;` … `mod tropes;`) and the `flag_value`
definition at `cli/src/main.rs:198-203`. Add `use hornvale::{flag_value, …};`
for each module the binary calls. **Do not leave both a `mod x;` in `main.rs`
and a `pub mod x;` in `lib.rs`** — that compiles each module twice into two
distinct types, and the resulting mismatch errors are confusing out of
proportion to their cause.

- [ ] **Step 3: Resolve the 6 `never used` warnings**

Promote to `pub` (with a doc line each, now required):
- `cli/src/audio.rs`: `cmd_voice`
- `cli/src/proto.rs`: `DEFAULT_FAMILY`

`ESPEAK_VOICE`, `ESPEAK_SPEED`, `record` and `run` are private helpers of
`cmd_voice`; they stop being dead once it is `pub`.

**Decision rule** — after `cargo clippy -p hornvale --all-targets -- -D warnings`:
- *Clean:* proceed.
- *Still `never used` on something the binary calls:* promote that item too.
- *`missing_docs` on an item you did not touch:* the probe said this would not
  happen. Report it rather than bulk-adding doc comments; it means a module is
  less documented than measured and the count is worth knowing.

- [ ] **Step 4: Verify nothing else moved**

```bash
cargo clippy -p hornvale --all-targets -- -D warnings
cargo nextest run -p hornvale 2>&1 | tail -20
```

Expected: clean, and every existing `cli/tests/` test still passing. This task
changes no behaviour — a test that fails here is a real regression, not a
rebaseline candidate. `cli/tests/architecture.rs` enforces the dependency
allowlist and layering; a lib target adds no dependency, so it must stay green.

- [ ] **Step 5: Capture the deferred half, then commit**

Add an idea-registry row for scope (b) — moving the 49 `cmd_*` functions and
dispatch out of `main.rs` (2,497 lines) so the binary becomes a ~20-line
`fn main()`. Status `raw`, and say in the row that Task 0 shipped the lib
target that unblocks it.

```bash
cargo fmt && cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/ book/src/frontier/idea-registry.md
git commit -m "refactor(cli): give the CLI a library target

cli/ is 'the thin command surface' by description but binary-only in fact, so
no module could be reached by a test except through the built binary. Exposes
the eight existing modules and hoists flag_value, which audio.rs reaches as
crate::flag_value.

Measured, not assumed: 0 missing_docs warnings — the modules were already
documented. The 6 'never used' warnings were pub(crate) items only the binary
calls.

Moving the 49 cmd_* functions out of main.rs is deliberately NOT here; it is
a much larger mechanical diff and wants its own campaign."
```

---

### Task 1: The corpus schema and its freeze

**Files:**
- Create: `systems/wolverson-2021.system.json`
- Create: `cli/src/systems.rs`
- Modify: `cli/src/lib.rs` (add `pub mod systems;`)
- Modify: `cli/src/main.rs` (add `use hornvale::systems;`)
- Test: `cli/tests/system_coverage.rs`

**Interfaces:**
- Produces: `systems::Corpus`, `systems::Item`, `systems::Verdict`,
  `systems::load(&str) -> Result<Corpus, String>`, `systems::CORPORA: &[&str]`

**The freeze rule.** The corpus is frozen *before* any verdict is measured
(decision 0016). Every item ships at `absent` — the default-deny posture, and
the honest one: we claim nothing until we can cite an anchor. Task 4 is the
measurement that moves them.

- [ ] **Step 1: Establish the chapter count from the source.**

Fetch `https://bfnightly.bracketproductions.com/rustbook/` and enumerate the
table of contents. The spec's §12 records **4 numbered sections, 73 chapters**
(13 + 7 + 20 + 33) from one fetch on 2026-08-15, plus two front-matter pages
(*Introduction*, *Building for the Web*).

**This is a decision rule, not a prediction.** Branch on what you actually
find:
- *Count matches 73 chapters + 2 front-matter items (75 total):* proceed.
- *Count differs:* the source moved since the spec was written. Use **what you
  find**, record the discrepancy and the fetch date in the corpus's
  `provenance` string, and note it in your task report. Do not "correct" the
  source to match the spec.

- [ ] **Step 2: Write the failing test**

Create `cli/tests/system_coverage.rs`:

```rust
//! The Compendium's ratchet and anchor discipline.

use std::path::PathBuf;

fn workspace_root() -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn load_wolverson() -> hornvale::systems::Corpus {
    let path = workspace_root().join("systems/wolverson-2021.system.json");
    let json = std::fs::read_to_string(&path).expect("corpus is readable");
    hornvale::systems::load(&json).expect("corpus parses")
}

/// The freeze. A corpus's item count is asserted so that changing the
/// catalogue is a deliberate act, never a side effect — the same discipline
/// `tropes/` carries for its situation counts.
#[test]
fn the_wolverson_corpus_is_frozen_at_its_declared_size() {
    let c = load_wolverson();
    assert_eq!(c.items.len(), 75, "the frozen corpus changed size");
    assert!(c.ordered, "Wolverson's chapters are a pedagogical ladder");
    assert_eq!(c.unit, "chapter");
}

/// Provenance is emitted, not documented (decision 0095): a reader cannot
/// reach a score without passing the statement that this is one instrument
/// with a known bias.
#[test]
fn the_corpus_declares_its_provenance_and_freeze() {
    let c = load_wolverson();
    assert!(!c.provenance.is_empty(), "provenance is required");
    assert!(!c.frozen.is_empty(), "the freeze note is required");
}
```

- [ ] **Step 3: Run it to verify it fails**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -20`
Expected: FAIL — `cli/src/systems.rs` does not exist, so the crate does not
compile. **A compile-error red proves nothing about an assertion**, so after
Step 5 you will re-run and see it fail on the *assertion* before it passes.

- [ ] **Step 4: Write the corpus**

`systems/wolverson-2021.system.json`. Every item at `"verdict": "absent"` with
no anchor. Front matter carries `"kind": "front-matter"`, chapters carry
`"kind": "chapter"`.

```json
{
  "corpus": "wolverson-2021",
  "unit": "chapter",
  "ordered": true,
  "provenance": "Herbert Wolverson, Roguelike Tutorial - In Rust (bfnightly.bracketproductions.com/rustbook/), table of contents fetched 2026-08-15. A pedagogical sequence for building one ECS roguelike on bracket-lib, not a specification of what a roguelike is: an instrument with known bias, never a standard. Its ordering is a teaching order, and its later sections are one specific game's content design. Verdicts are authored by Hornvale about itself, which is a weaker authority than the blind mapping tropes/tvtropes-2012 used; the anchor discipline is what constrains it, and it constrains `present` least. Coverage measures reach against this catalogue only.",
  "frozen": "before first measurement, The Compendium",
  "items": [
    {
      "id": "1",
      "kind": "front-matter",
      "title": "Introduction",
      "verdict": "absent"
    },
    {
      "id": "2.6",
      "kind": "chapter",
      "title": "Dealing Damage",
      "verdict": "absent"
    }
  ]
}
```

- [ ] **Step 5: Write the minimal resolver types**

`cli/src/systems.rs`:

```rust
//! The Compendium: score a frozen corpus of game-system capability against
//! Hornvale's own declared state. Sibling family to `tropes/`, never a member
//! of it — see the spec's §3.
//!
//! Unlike `tropes`, this resolver builds **no world**. Every anchor resolves
//! against `docs/digest/decisions-in-force.md`, the idea registry, and the
//! filesystem, so the ratchet costs a few file reads rather than a genesis.

use serde::Deserialize;

/// The declared corpora, in matrix-column order.
pub const CORPORA: &[&str] = &["systems/wolverson-2021.system.json"];

/// How one catalogue item stands against Hornvale.
///
/// Five verdicts, not decision 0095's three. An unmet capability is three
/// different facts — deliberately refused, planned but unbuilt, or a genuine
/// hole — and an instrument that cannot tell them apart reports a deficiency
/// list that is mostly false against a corpus like this one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Verdict {
    /// Hornvale does this. Cites a mechanism anchor.
    Present,
    /// Hornvale deliberately will not. Cites a decision anchor.
    Refused,
    /// Planned, not built. Cites a registry anchor.
    Deferred,
    /// A genuine hole. Cites nothing — the honest red.
    Absent,
    /// About the tutorial's toolchain, not a world capability. Cites a reason.
    Inapplicable,
}

/// One catalogue item as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(identifier-text: kind), bare-ok(prose: title), bare-ok(identifier-text: anchor), bare-ok(prose: note)
#[derive(Debug, Clone, Deserialize)]
pub struct Item {
    /// Corpus-local identifier, e.g. `2.6`.
    pub id: String,
    /// What this row is: `chapter`, `front-matter`, `feature`, `mechanic`.
    pub kind: String,
    /// The item's title as the source gives it.
    pub title: String,
    /// How it stands against Hornvale.
    pub verdict: Verdict,
    /// The anchor backing the verdict, absent only for `absent`.
    #[serde(default)]
    pub anchor: Option<String>,
    /// One line of human context. Never parsed.
    #[serde(default)]
    pub note: String,
}

/// A frozen, provenance-stamped capability corpus.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: unit), bare-ok(prose: provenance), bare-ok(prose: frozen)
#[derive(Debug, Clone, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `wolverson-2021`.
    pub corpus: String,
    /// What the items are: `chapter`, `feature`, …
    pub unit: String,
    /// Whether the items form a meaningful sequence. Gates ordinal readings.
    pub ordered: bool,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement.
    pub frozen: String,
    /// The items themselves, in corpus order.
    pub items: Vec<Item>,
}

/// Parse a corpus from JSON.
/// type-audit: bare-ok(artifact: json), bare-ok(prose: return)
pub fn load(json: &str) -> Result<Corpus, String> {
    serde_json::from_str(json).map_err(|e| format!("corpus parse: {e}"))
}
```

Add `pub mod systems;` to `cli/src/lib.rs`, beside `pub mod tropes;`. Task 0
established the library target, so the integration test reaches it as
`hornvale::systems` with nothing further needed in `main.rs` beyond a `use`.

- [ ] **Step 6: Run the test — expect an ASSERTION failure, not a compile error**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -20`
Expected: FAIL on `the_wolverson_corpus_is_frozen_at_its_declared_size` with a
count mismatch, if you have not yet written all 75 items. Fill the corpus until
it passes. **Do not skip observing this red** — a green that was never red is
the failure mode `five-vacuous-guards-one-campaign` records.

- [ ] **Step 7: Run the test to verify it passes**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -20`
Expected: PASS, 2 tests.

- [ ] **Step 8: Format and commit**

```bash
cargo fmt
git add systems/ cli/src/systems.rs cli/src/main.rs cli/tests/system_coverage.rs
git commit -m "feat(systems): freeze the wolverson-2021 capability corpus

75 items (73 chapters + 2 front-matter), every one at `absent` — the
default-deny posture. The freeze precedes the measurement (0016); Task 4
authors the verdicts."
```

---

### Task 2: Anchor resolution and the four RED conditions

**Files:**
- Modify: `cli/src/systems.rs`
- Test: `cli/tests/system_coverage.rs`

**Interfaces:**
- Consumes: `Corpus`, `Item`, `Verdict`, `load` (Task 1)
- Produces: `systems::Anchor`, `systems::RepoFacts`,
  `RepoFacts::gather(&Path) -> Result<RepoFacts, String>`,
  `systems::Finding`, `systems::audit(&Corpus, &RepoFacts) -> Vec<Finding>`

- [ ] **Step 1: Write the failing tests, one per RED condition**

Append to `cli/tests/system_coverage.rs`:

```rust
use hornvale::systems::{Finding, RepoFacts, audit, load};

/// Build a one-item corpus with the given verdict and anchor.
fn corpus_with(verdict: &str, anchor: Option<&str>) -> hornvale::systems::Corpus {
    let anchor_json = match anchor {
        Some(a) => format!(r#", "anchor": "{a}""#),
        None => String::new(),
    };
    let json = format!(
        r#"{{ "corpus": "fixture", "unit": "chapter", "ordered": true,
              "provenance": "fixture", "frozen": "fixture",
              "items": [ {{ "id": "1.1", "kind": "chapter", "title": "T",
                            "verdict": "{verdict}"{anchor_json} }} ] }}"#
    );
    load(&json).expect("fixture parses")
}

fn facts() -> RepoFacts {
    RepoFacts::gather(&workspace_root()).expect("repo facts gather")
}

/// UNJUSTIFIED: a non-`absent` verdict with no anchor. The tropes family's
/// reasonless-`inapplicable` rule, generalized.
#[test]
fn a_refused_verdict_without_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("refused", None), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED, got {f:?}"
    );
}

/// UNJUSTIFIED also covers the wrong KIND of anchor: `refused` means a
/// decision forbids it, and a path cannot express that.
#[test]
fn a_refused_verdict_anchored_to_a_path_is_unjustified() {
    let f = audit(&corpus_with("refused", Some("path:cli/src/main.rs")), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for a wrong-kind anchor, got {f:?}"
    );
}

/// DANGLING: 0014 was superseded by 0126, so it is absent from
/// `decisions-in-force.md` by construction. A refusal citing it has lost its
/// ground and must not read as settled.
#[test]
fn a_refusal_citing_a_superseded_decision_is_dangling() {
    let f = audit(&corpus_with("refused", Some("decision:0014")), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING for superseded 0014, got {f:?}"
    );
}

/// A refusal citing a decision that IS in force is clean. The positive
/// control: without it, a resolver that flagged everything would pass the
/// test above (`an-empty-diff-needs-a-positive-control`).
#[test]
fn a_refusal_citing_an_in_force_decision_is_clean() {
    let f = audit(&corpus_with("refused", Some("decision:0070")), &facts());
    assert!(f.is_empty(), "0070 is in force; expected no findings, got {f:?}");
}

/// DANGLING for a registry row that does not exist.
#[test]
fn a_deferral_citing_an_unknown_registry_row_is_dangling() {
    let f = audit(
        &corpus_with("deferred", Some("registry:CLIENT-no-such-row-exists")),
        &facts(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::Dangling { .. }]),
        "expected DANGLING, got {f:?}"
    );
}

/// STALE-DEFERRED: seam-guard's STALE-DECL, exactly. A one-directional
/// acknowledgement can only ever be satisfied, so it rots; this fails the
/// moment reality catches up. `CLIENT-action-clock` reads `shipped` today.
#[test]
fn a_deferral_against_a_shipped_registry_row_is_stale() {
    let f = audit(
        &corpus_with("deferred", Some("registry:CLIENT-action-clock")),
        &facts(),
    );
    assert!(
        matches!(f.as_slice(), [Finding::StaleDeferred { .. }]),
        "expected STALE-DEFERRED for a shipped row, got {f:?}"
    );
}

/// An `absent` verdict must carry NO anchor — it is the one verdict that
/// claims nothing, and an anchored `absent` is a miscategorised row.
#[test]
fn an_absent_verdict_carrying_an_anchor_is_unjustified() {
    let f = audit(&corpus_with("absent", Some("decision:0070")), &facts());
    assert!(
        matches!(f.as_slice(), [Finding::Unjustified { .. }]),
        "expected UNJUSTIFIED for an anchored `absent`, got {f:?}"
    );
}

/// The shipped corpus must be clean at all times.
#[test]
fn the_wolverson_corpus_has_no_anchor_findings() {
    let f = audit(&load_wolverson(), &facts());
    assert!(f.is_empty(), "the corpus has anchor findings:\n{f:#?}");
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -30`
Expected: FAIL to compile — `RepoFacts`, `audit`, `Finding` undefined. Then,
after Step 3, each must be seen failing on its **assertion** before it passes.

- [ ] **Step 3: Implement anchor parsing, repo facts, and the audit**

Append to `cli/src/systems.rs`:

```rust
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

/// A verdict's backing evidence, parsed from its `anchor` string.
/// type-audit: bare-ok(identifier-text: Decision.0), bare-ok(identifier-text: Registry.0), bare-ok(identifier-text: Test.0), bare-ok(artifact: Path.0), bare-ok(prose: Reason.0)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Anchor {
    /// `decision:0070` — must be in force.
    Decision(String),
    /// `registry:CLIENT-action-clock` — must exist and not read `shipped`.
    Registry(String),
    /// `test:<crate>::<fn>` — the preferred mechanism anchor.
    Test(String),
    /// `path:<file>` — the weaker mechanism anchor.
    Path(String),
    /// `reason:<prose>` — for `inapplicable` only.
    Reason(String),
}

impl Anchor {
    /// Parse an anchor string. `None` when the prefix is unknown.
    pub fn parse(s: &str) -> Option<Anchor> {
        let (kind, rest) = s.split_once(':')?;
        match kind {
            "decision" => Some(Anchor::Decision(rest.to_string())),
            "registry" => Some(Anchor::Registry(rest.to_string())),
            "test" => Some(Anchor::Test(rest.to_string())),
            "path" => Some(Anchor::Path(rest.to_string())),
            "reason" => Some(Anchor::Reason(rest.to_string())),
            _ => None,
        }
    }
}

/// The live repo state every anchor resolves against. Gathered once.
#[derive(Debug, Clone)]
pub struct RepoFacts {
    /// Decision numbers present in `docs/digest/decisions-in-force.md`.
    in_force: BTreeSet<String>,
    /// Idea-registry row ID → its `status` cell.
    registry: BTreeMap<String, String>,
    /// Repo root, for path and test-symbol resolution.
    root: std::path::PathBuf,
    /// Crate name → its directory, e.g. `hornvale-vessel` → `windows/vessel`.
    crates: BTreeMap<String, String>,
    /// The digest's blob hash, printed so a confusing red is traceable.
    digest_stamp: String,
}
```

Implement `RepoFacts::gather`:
- **Decisions:** read `docs/digest/decisions-in-force.md`; each in-force line
  matches `- **NNNN** …`. Extract the four digits.
- **Registry:** read `book/src/frontier/idea-registry.md`; a row is a `|`-
  prefixed line whose first cell looks like a registry ID (a category prefix, a
  hyphen, then digits or a lowercase slug). **Reuse the shape of
  `looks_like_registry_id` in `cli/tests/docs_consistency.rs`** rather than
  inventing a second rule. Status is cell index 2 (`id | description | status |
  confidence | where`). Verify that indexing against a real row before relying
  on it.
- **Crates:** walk `domains/*/Cargo.toml` and `windows/*/Cargo.toml`, read the
  `name = "…"` line, map name → directory.
- **digest_stamp:** the digest file's content length and a simple checksum is
  enough; do not shell out to `git`.

Then the audit:

```rust
/// One thing wrong with a verdict's evidence. Every variant carries enough
/// to diagnose it WITHOUT knowing this instrument exists — the spec's §5
/// requirement, because the idea registry is now a gated interface and the
/// session that reddens it may have been editing a registry row.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Finding {
    /// A non-`absent` verdict with no anchor, or the wrong kind of anchor.
    Unjustified {
        /// The item's corpus-local id.
        id: String,
        /// What is wrong, in a sentence.
        why: String,
    },
    /// The anchor stopped resolving.
    Dangling {
        /// The item's corpus-local id.
        id: String,
        /// The anchor as authored.
        anchor: String,
        /// What would have caused this, and the two legitimate repairs.
        why: String,
    },
    /// A `deferred` verdict whose registry row now reads `shipped`.
    StaleDeferred {
        /// The item's corpus-local id.
        id: String,
        /// The registry row that shipped.
        row: String,
        /// What to do about it.
        why: String,
    },
}
```

`audit` walks items, maps verdict → permitted anchor kinds
(`Present` → `Test`/`Path`; `Refused` → `Decision`; `Deferred` → `Registry`;
`Inapplicable` → `Reason`; `Absent` → none), and emits findings.

**Failure text is a deliverable of this task, not decoration.** A `Dangling`
on a decision must read like:

```
2.6 cites decision:0014, which is not in docs/digest/decisions-in-force.md.
A decision leaves that file when it is wholly superseded. Either re-verdict
this item against the superseding decision, or restore the anchor if the
supersession was partial. (digest stamp: <stamp>)
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -30`
Expected: PASS, 10 tests.

- [ ] **Step 5: Format, lint, and commit**

```bash
cargo fmt && cargo clippy -p hornvale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add cli/src/systems.rs cli/tests/system_coverage.rs docs/audits/type-audit-report.md
git commit -m "feat(systems): anchor resolution and the four RED conditions

Verdicts cite anchors the resolver verifies: a decision must be in force, a
registry row must exist and not read `shipped`. Each RED was observed failing
before its fix. The type-audit report moves because systems.rs adds pub
boundaries."
```

---

### Task 3: The report, its artifact, and the ratchet

**Files:**
- Modify: `cli/src/systems.rs` (add `render`, `artifact_path`)
- Modify: `cli/src/main.rs` (add `cmd_systems`, help text)
- Modify: `scripts/regenerate-artifacts.sh`
- Create: `docs/audits/system-coverage-wolverson-2021.md` (generated)
- Test: `cli/tests/system_coverage.rs`

**Interfaces:**
- Consumes: everything from Tasks 1–2
- Produces: `systems::render(&Corpus, &RepoFacts) -> String`,
  `systems::artifact_path(&Corpus) -> String`, CLI `hornvale systems report|check`

- [ ] **Step 1: Write the failing ratchet test**

```rust
use std::process::Command;

#[test]
fn committed_system_coverage_matches_the_live_report() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["systems", "--corpus", "systems/wolverson-2021.system.json", "report"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "systems report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/system-coverage-wolverson-2021.md"),
        &live,
        "the system-coverage report drifted from the committed artifact. An item that \
         changed verdict means an anchor moved — a decision was superseded, or a \
         registry row shipped — which is a finding, not a formality. Regenerate \
         deliberately with `make rebaseline` and read the diff.",
    );
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale --test system_coverage committed_system 2>&1 | tail -20`
Expected: FAIL — no `systems` subcommand yet.

- [ ] **Step 3: Implement `render`**

Order is fixed by decision 0095: **provenance before any number.** The report
emits, in order:

1. A generated-file banner naming the regeneration command.
2. The corpus id, `provenance` and `frozen` strings, hard-wrapped at 76
   columns (reuse `tropes::wrap`'s approach — an unwrapped paragraph makes
   every edit a whole-line diff).
3. **The `present`-is-weakly-checked caveat**, verbatim from spec §7, *above*
   the tally. This is a requirement, not a nicety.
4. The tally: counts per verdict.
5. If `ordered`: the **first unmet item** — the corpus's most useful single
   reading. If not `ordered`, this section is omitted entirely (spec §14: an
   unordered catalogue ranked by `id` would manufacture a ladder its source
   never had).
6. The item table: `| id | title | verdict | anchor |`.

Use integer percentages only — no floats in a byte-ratcheted artifact
(`tropes::percent` is the precedent, and decision 0033 is the reason).

- [ ] **Step 4: Wire the CLI**

Add `cmd_systems` to `main.rs`, modelled on `cmd_tropes` but **without
`build_world`** — this resolver needs no world. Copy `cmd_tropes`'s
flag-skipping mode scan verbatim and keep its comment: `args.get(1)` alone
would let `systems --corpus X check` emit a report and exit 0, a false pass
for anything gating on `check`. Add the help lines beside the `tropes` block.

`check` compares live render to the committed artifact and fails with the
regenerate instruction. `check` **also** fails on any `audit` finding, and on
NOVELTY — the `absent` count rising above the committed artifact's.

- [ ] **Step 5: Generate the artifact and wire regeneration**

Add to `scripts/regenerate-artifacts.sh`, beside the `tropes` redirects
(around line 506) — **the `>` redirect is what writes the file**, never the
command alone:

```bash
spawn run -p hornvale -- systems report > docs/audits/system-coverage-wolverson-2021.md
```

Then:

```bash
make rebaseline
```

**Decision rule for the resulting diff:**
- *Only `docs/audits/system-coverage-*.md` and `docs/audits/type-audit-report.md` moved:* expected. `git add` both.
- *`book/src/gallery/` moved:* STOP. That is a byte-identity change and an epoch event; nothing in this task should touch it. Report and do not commit.
- *Anything under `book/src/domesday/` moved:* STOP and report — that reads the census, which this task cannot affect.

- [ ] **Step 6: Run the test to verify it passes**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 7: Format and commit**

```bash
cargo fmt && cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/systems.rs cli/src/main.rs cli/tests/system_coverage.rs \
        scripts/regenerate-artifacts.sh docs/audits/
git commit -m "feat(systems): the coverage report, its artifact, and the ratchet

Provenance and the present-is-weakly-checked caveat print above the tally
(0095). The first-unmet reading is gated on `ordered`. docs/audits/ is
already drift-checked, but the artifact is git-added here because
`git diff --exit-code` against an untracked path is silently vacuous."
```

---

### Task 4: Author the verdicts — the measurement

**Files:**
- Modify: `systems/wolverson-2021.system.json`
- Modify: `docs/audits/system-coverage-wolverson-2021.md` (regenerated)

This is the campaign's least mechanical task and its actual content. Every
item moves from `absent` to a verdict backed by an anchor, or stays `absent`
honestly.

- [ ] **Step 1: Read the source for each item**

Work through the corpus in order. For each chapter, read enough of the
tutorial to know what capability it adds — do not verdict from the title
alone. Several titles are misleading (*Bloodstains* is a rendering-residue
chapter; *Difficulty* is a spawn-weighting chapter).

- [ ] **Step 2: Assign verdicts, cheapest evidence first**

The rules, in the order they apply:

1. Is the chapter about bracket-lib, the toolchain, or the tutorial's own
   scaffolding? → `inapplicable`, `reason:…`.
2. Does a **decision in force** forbid it? → `refused`, `decision:NNNN`.
   Known instances the spec already established: chapter 2.6 *Dealing Damage*
   → `decision:0070` (no stored mutable health value; combat is explicitly
   ordered after it). Alignment-flavoured content → `decision:0021`. Anything
   asking the sim to render → `decision:0022`.
3. Does an **idea-registry row** plan it? → `deferred`, `registry:ROW-ID`.
   The 76 `CLIENT-*` rows are the first place to look. If no row exists but
   the capability is genuinely planned, **create the row** — that is expected
   work for this task, not scope creep.
4. Does Hornvale do it? → `present`, preferring `test:<crate>::<fn>` over
   `path:<file>`. The spec is explicit that `present` is the least-entitled
   verdict; do not reach for it on a resemblance.
5. Otherwise → `absent`, no anchor.

**Do not tune a verdict to improve the tally.** A high `absent` count is a
finding. Several campaigns in this repo ship the null as the headline.

- [ ] **Step 3: Run the audit continuously**

Run: `cargo test -p hornvale --test system_coverage the_wolverson 2>&1 | tail -30`
Expected: PASS — `the_wolverson_corpus_has_no_anchor_findings` is the check
that every verdict you wrote is actually backed.

- [ ] **Step 4: Regenerate and read the diff**

```bash
make rebaseline
git diff docs/audits/system-coverage-wolverson-2021.md
```

Read the whole diff. The first-unmet line is the campaign's headline reading —
**report what it says in your task report**, whatever it says.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add systems/ docs/audits/ book/src/frontier/idea-registry.md
git commit -m "feat(systems): author the wolverson-2021 verdicts

The measurement. Every non-absent verdict cites an anchor the resolver
verifies. New idea-registry rows were minted where a capability was genuinely
planned but had no row."
```

---

### Task 5: The matrix and the surplus read

**Files:**
- Modify: `cli/src/systems.rs` (add `render_matrix`)
- Modify: `cli/src/main.rs` (add the `matrix` mode)
- Modify: `scripts/regenerate-artifacts.sh`
- Create: `docs/audits/system-matrix.md` (generated)
- Test: `cli/tests/system_coverage.rs`

- [ ] **Step 1: Write the failing tests**

```rust
/// The surplus read: a subsystem no `present` verdict cites is one this
/// catalogue has no vocabulary for. Derived, never authored — an authored
/// list is the rot the anchor discipline exists to prevent.
#[test]
fn the_surplus_read_names_subsystems_no_chapter_cites() {
    let surplus = hornvale::systems::surplus(&load_wolverson(), &facts());
    assert!(
        surplus.iter().any(|s| s.contains("language")),
        "no chapter of a roguelike tutorial asks anything domains/language \
         would answer; expected it in the surplus, got {surplus:?}"
    );
}

#[test]
fn committed_system_matrix_matches_the_live_render() {
    let root = workspace_root();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["systems", "matrix"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "systems matrix failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    hornvale_kernel::golden::assert_golden(
        &root.join("docs/audits/system-matrix.md"),
        &live,
        "the system matrix drifted; regenerate with `make rebaseline`",
    );
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale --test system_coverage 2>&1 | tail -20`
Expected: FAIL — `surplus` undefined, `matrix` mode unknown.

- [ ] **Step 3: Implement `surplus` and `render_matrix`**

`surplus(&Corpus, &RepoFacts) -> Vec<String>`: enumerate `domains/*` and
`windows/*` directories; a subsystem is **cited** when some `present` item's
anchor names it — a `path:` anchor whose path starts with that directory, or a
`test:<crate>::…` anchor whose crate maps to it through `RepoFacts::crates`.
Return the uncited ones, sorted.

`render_matrix`: one row per corpus in `CORPORA` (one today), columns being the
five verdict counts, plus the surplus list, plus **the declared limitation
printed next to it** — subsystem granularity is coarse and a domain cited by a
single chapter reads as fully covered (spec §6).

- [ ] **Step 4: Wire, regenerate, verify**

Add `matrix` to `cmd_systems` (taking no `--corpus`, exactly as
`cmd_tropes_matrix` does — the columns are a declared list, not the caller's
choice). Add the redirect:

```bash
spawn run -p hornvale -- systems matrix > docs/audits/system-matrix.md
```

Run: `make rebaseline && cargo test -p hornvale --test system_coverage 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 5: Format and commit**

```bash
cargo fmt && cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/ cli/tests/ scripts/regenerate-artifacts.sh docs/audits/
git commit -m "feat(systems): the matrix and the derived surplus read

The surplus is what the catalogue never thinks to ask for, derived from
uncited subsystems rather than authored. Its coarse granularity is printed
next to it."
```

---

### Task 6: Close — book, decisions, registry, retrospective

**Files:**
- Create: `book/src/chronicle/the-compendium.md`
- Modify: `book/src/SUMMARY.md`
- Create: `docs/decisions/NNNN-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md`
- Create: `docs/decisions/NNNN-a-coverage-verdict-cites-a-checked-anchor.md`
- Modify: `book/src/frontier/idea-registry.md` (close `CLIENT-coverage-matrix`)
- Modify: `CLAUDE.md` (a `systems/` entry in the directory guides)
- Create: `docs/retrospectives/the-compendium.md`

- [ ] **Step 1: Write the chronicle entry**

Follow `book/src/chronicle/the-repertoire.md` and `the-collation.md` — the two
entries this one extends. Carry the reading: the first unmet chapter, the
verdict distribution, and the surplus. Book prose is technical and
mathematical, comprehensible without reading the code it may show.

**Chapter titles are code-generated** — check how `SUMMARY.md` entries are
produced before hand-writing one.

- [ ] **Step 2: Write the two decision records**

Numbers: take the next two free. `make doctor` counts decisions; `ls
docs/decisions/ | tail` gives the current maximum. Content is spec §14.

The second record must state the accepted cost explicitly: **the idea registry
becomes a gated interface** — a row's ID and status are load-bearing for a
committed artifact, ratified by Nathan at G3 on 2026-08-15.

- [ ] **Step 3: Close the registry row and add follow-ups**

Set `CLIENT-coverage-matrix` to `shipped`, with the **Where** cell pointing at
the chronicle. Add rows for the two deferred halves: executable per-chapter
probes (spec §9) and the surplus read's coarse granularity (spec §6).

- [ ] **Step 4: Promote the ledger into the retrospective**

`.superpowers/sdd/decision-ledger.md` is git-ignored and **dies with the
worktree**. Promote its process notes into `docs/retrospectives/the-compendium.md`
*before* teardown, including the `worktree-take`-branched-from-stale-`origin/main`
finding.

- [ ] **Step 5: The full gate**

```bash
make gate 2>&1 | tee /tmp/hv-compendium-gate.txt
make rebaseline && git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Expected: green, and an empty drift diff. Run on a **quiet box** — the duration
alarm cannot see ordinary load, and a red alarm from a busy machine is not
evidence (CLAUDE.md, blind spot 1).

- [ ] **Step 6: Commit and hand back for G6**

```bash
cargo fmt
git add book/ docs/ CLAUDE.md
git commit -m "docs(the-compendium): chronicle, decisions, registry close, retrospective"
```

G6 is a **hard stop**. Present the post-G3 ledger digest and wait for Nathan;
then run `closing-a-campaign` unchanged.

---

## Self-review

**Spec coverage.** §1 → T1–T5. §2 scope → T1 (one corpus), T6 (registry rows
for deferred halves). §3 family separation → T6 decision record. §4 five
verdicts → T1 `Verdict`, T4 authoring. §5 anchor discipline → T2, and the
diagnosability requirement is an explicit deliverable of T2 Step 3. §6 surplus
→ T5, limitation printed. §7 declared bias → T3 Step 3 item 3, above the tally.
§8 freeze and count → T1 Steps 1–2, with a branch table rather than a
prediction. §9 refusal of probes → T6 Step 3 follow-up row. §10a constraints →
Global Constraints. §14 both ratifications → T6 Step 2; `ordered` gating → T3
Step 3 item 5 and T1's `ordered` assertion.

**Placeholders.** None: every code step carries real code, and the two places
where an outcome could not be known in advance (the chapter count, the
rebaseline diff) are written as **branch tables**, not predictions — the
failure mode `imperative-mood-hides-assertions` records.

**Type consistency.** `Corpus`/`Item`/`Verdict`/`Anchor`/`RepoFacts`/`Finding`
are defined in T1–T2 and used with the same names and fields in T3–T5.
`load`/`audit`/`render`/`artifact_path`/`surplus`/`render_matrix` are the whole
public surface, each produced once and consumed by name thereafter.

**Resolved during planning rather than left to the implementer:** whether
`cli/tests/` can reach the resolver at all. It could not — `hornvale` had no
lib target, `cli/src/lib.rs` did not exist, and no `cli/tests/` file contained
`use hornvale::`. That would have made every unit-style test in Tasks 1–2
uncompilable as written. **Task 0 fixes it at the root**, and its cost was
measured with a throwaway probe (0 missing_docs, 6 trivial warnings) rather
than estimated.

**Task 0 is the only task that changes existing behaviour-free code**, so it
is also the only one where a failing existing test means a genuine regression
rather than a moved artifact. Every later task's reds are either assertions
being observed before their fix, or artifacts that legitimately drifted.
