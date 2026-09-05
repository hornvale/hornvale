# The Charter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Assemble useful checked task context from independently enrolled
Thing and census contributors, with explicit quality contracts.

**Architecture:** Extend the outboard Digest with a small protocol package,
generic composition and a controlled subprocess host. Freeze that bootstrap,
then develop the two adapters on separate branches and compose them without
contributor-specific host edits. Existing simulation and gate authority remain.

**Tech Stack:** Existing Rust/Cargo, serde and serde_json. For Unix process
groups/signals, use scoped development-only `nix` and `signal-hook` dependencies
after verifying their resolved APIs. No async runtime or plugin framework.

**Spec:** [The Charter](../specs/2026-09-04-the-charter-design.md), approved
with quality refinement at G3; [ledger](../ledgers/2026-09-04-the-charter.md).

## Global Constraints

- Implementation stays on `codex/the-charter` or its named isolated adopter
  branches, never main. Controller records bases before each dispatch.
- Protocol version is `1`; workspace members are `packages/*` with permanent
  `packages/protocol`. The protocol has no kernel dependency.
- Existing `render doctor`, `render decisions`, and `render delta` stay usable.
  The current ProjectLedger and time-free fact store are not changed.
- “Context runs with `--locked --offline` after the normal dependency/build
  preparation step.” Metadata/build always name the current checkout's Digest
  manifest and compiled target directory; inherited Git paths/target overrides
  must not redirect collection.
- “Bound each contributor's stdout to 1 MiB, stderr capture to 64 KiB,
  execution to 5 s; bound metadata/build machine-output capture to 16 MiB and
  each build/metadata phase to 600 s. Cancellation allows 250 ms for orderly
  termination before forced termination.” Use injectable limits in tests.
- Contributors execute sequentially within one host invocation. No sandbox,
  complete input certificate, atomic source snapshot, evidence reuse, gate
  omission, world construction or live census is claimed by context.
- Treat requirement declarations as policy; changing expected observations
  cannot itself establish satisfaction of the former requirement.
- All existing commit hooks remain enabled. Scope iterative tests to this
  outboard workspace; do not run the full simulation workspace suite locally.
  Canonical stage/merge/census work uses the existing queue and full SHA.
- Every task has a fresh implementer and independent task review. Tasks 3–4
  are intentionally parallel in different worktrees: this user-approved
  independence experiment overrides the generic same-worktree no-parallel
  advice. No concurrent edits to one checkout/index; stagger their commit gates.
- Keep tests in module test blocks or one `tests/suite.rs` per new package.
  Comments state each check's direction and what it does not establish.
- New timing uses scoped `#[allow(clippy::disallowed_types)]` with a development
  instrumentation explanation. Do not weaken the simulation's clippy policy.

## File responsibilities and stage boundaries

| Path | Responsibility |
|---|---|
| `tools/digest/packages/protocol/src/lib.rs` | Types, names/paths, envelope/reference validation |
| `tools/digest/src/context/mod.rs` | Public context entry point, assembled report, errors |
| `tools/digest/src/context/compose.rs` | Validation across envelopes and deterministic Markdown |
| `tools/digest/src/context/discovery.rs` | Metadata parsing, workspace membership, scopes and exact targets |
| `tools/digest/src/context/process.rs` | Bounded IO, deadlines, cancellation, Unix process-group lifetime |
| `tools/digest/src/context/collect.rs` | Checkout resolution, build artifact selection, fresh collection |
| `tools/digest/tests/suite.rs` | CLI/fixture integration; no simulation build or census execution |
| `tools/digest/packages/thing/` | Thing/Settlement observations, instructions and local tests |
| `tools/digest/packages/census-publication/` | Pure lab guard observations, source check and local tests |
| `tools/digest/README.md` | Protocol author guide, context/preparation usage and limits |
| `docs/digest/the-charter-evidence.md` | Durable independent-adoption, quality and cost evidence |
| `Makefile`, `scripts/lane-outboard.sh` | Discoverable entry point and all-package outboard coverage |

Stage 1 is Tasks 1–2. Freeze its reviewed bootstrap SHA and submit its stage
gate. Stage 2 is Tasks 3–4 and their integration; submit the composed stage
gate. Stage 3 is Task 5 and final review. Track these in
`IMPLEMENTATION_PLAN.md`; remove that transient tracker when all stages end.

## Shared interfaces

Task 1 establishes these public protocol names. Derive serde and ordinary
comparison/debug traits as appropriate; use snake_case enum encoding and
reject unknown object fields. Add public API documentation.

```rust
pub const PROTOCOL_VERSION: u32 = 1;

pub struct Contribution {
    pub protocol: u32,
    pub namespace: String,
    pub display_name: String,
    pub scopes: Vec<String>,
    pub requirements: Vec<Requirement>,
    pub observations: Vec<Observation>,
    pub instructions: Vec<Instruction>,
}
pub struct Requirement {
    pub id: String,
    pub statement: String,
    pub sources: Vec<String>,
    pub evidence: Evidence,
}
pub enum Evidence {
    AuthoredOnly,
    Checked { required_observations: Vec<String> },
}
pub enum Outcome { Satisfied, Contradicted, Unknown }
pub struct Observation {
    pub id: String,
    pub method: String,
    pub subject: String,
    pub outcome: Outcome,
    pub details: String,
    pub requirements: Vec<String>,
}
pub struct Instruction {
    pub id: String,
    pub markdown: String,
    pub requirements: Vec<String>,
    pub observations: Vec<String>,
}
pub struct ContractError(pub String);

pub fn validate(contribution: &Contribution) -> Result<(), ContractError>;
pub fn normalize_scope(value: &str) -> Result<String, ContractError>;
pub fn scope_matches(requested: &str, declared: &str) -> bool;
```

IDs use `<namespace>:<local-name>`, with nonempty lowercase ASCII alphanumeric,
dot/hyphen names starting with a letter. No cross-owner references in v1.
Scopes use `/` and normal relative path components; reject absolute paths,
parent traversal, backslashes and empty input. Accept `.` as an explicit
all-enrolled-context request. Match actual descendants and also contributors
under a requested ancestor; `domains/thing-other` does not match
`domains/thing`. Do not sort semantic detail text or source rosters.

Task 1 also establishes the generic host composition API:

```rust
pub struct CheckoutContext {
    pub revision: String,
    pub dirty: bool,
}
pub struct ContextReport {
    pub markdown: String,
    pub successful: bool,
}
pub fn compose(
    checkout: &CheckoutContext,
    contributions: &[Contribution],
) -> Result<ContextReport, ContractError>;
```

Malformed/ambiguous records are `Err`. A structurally valid contradicted or
unknown required observation renders an honest report with `successful=false`.
Every explicit contradiction makes the report unsuccessful, including an
observation outside a checked requirement's required set. An optional unknown
observation alone does not fail the report. Pure authored-only requirements do not become executable failures, but their
unchecked status is explicit. No overall verification badge. Namespace/record
sorting is deterministic and duplicate IDs fail even with equal text.

Task 2 establishes the host entry point:

```rust
pub fn context_from_current_checkout(scope: &str) -> Result<ContextReport, String>;
```

Contributor manifests use this metadata shape (these are proposed exact v1
keys, not existing manifest entries):

```toml
[package.metadata.digest]
role = "contributor"
namespace = "hornvale.thing"
protocol = 1
binary = "digest-thing"
scopes = ["domains/thing"]
```

Contributors implement `collect --repo-root <absolute-path>`, emit one JSON
`Contribution` on stdout and diagnostics on stderr, and exit nonzero if they
cannot perform collection. The host validates metadata/envelope agreement.
The root path is an ordinary argv value, never interpolated into a shell.

### Task 1: Protocol and deterministic composition

**Files:** Create `tools/digest/packages/protocol/Cargo.toml`,
`tools/digest/packages/protocol/src/lib.rs`, `tools/digest/src/context/mod.rs`,
`tools/digest/src/context/compose.rs`. Modify `tools/digest/Cargo.toml`,
`tools/digest/Cargo.lock`, `tools/digest/src/lib.rs`.

**Interfaces:** Consume serde/JSON and the existing outboard package layout.
Produce every shared interface above except `context_from_current_checkout`.
The protocol package is named `digest-protocol`; no simulation dependency.

- [ ] **Step 1: Establish behavioral examples before scaffolding.** Write
  fixture construction helpers in module tests, then tests for omitted
  required observation, wrong-kind reference, duplicate identities, invalid
  scopes, unknown versions and deterministic composition. Compile failures
  while types are absent are scaffolding evidence only; demonstrate a
  behavioral red with a deliberately incomplete validator before implementing
  the validation. The core omission case must have this property:

  ```rust
  let mut c = checked_fixture(); // valid requirement and required observation
  assert!(validate(&c).is_ok());
  assert_eq!(c.observations.len(), 1);
  c.observations.clear();
  assert!(validate(&c).is_err());
  ```

- [ ] **Step 2: Define the workspace and contract.** Keep the root package
  isolated, add `members = ["packages/*"]`, `resolver = "3"`, and a path
  dependency on `digest-protocol`. Implement the named types and validation:
  versions, namespace/name form, nonempty statements/method/subjects,
  nonempty authorities for requirements, valid scopes, unique IDs across
  record kinds, local references of the correct kind, nonempty checked sets,
  and required observation backreferences. Do not infer requirements from
  whichever observations happened to arrive. A contributor must contain at
  least one requirement and one instruction; an empty panel cannot enroll.

- [ ] **Step 3: Compose without contributor knowledge.** Sort outer records,
  reject duplicate namespaces/IDs, render authored instructions separately
  from observations and their limits, escape metadata used in headings, and
  render checkout revision/dirty state. Authored Markdown is trusted prose;
  do not infer its semantic truth. Include the full expected observation set
  in the rendered requirement so a reviewer can see its claimed coverage.

  ```rust
  let a = checked_fixture_in("example.a");
  let b = authored_fixture_in("example.b");
  let context = CheckoutContext { revision: "fixture".into(), dirty: false };
  assert_eq!(compose(&context, &[a.clone(), b.clone()]).unwrap().markdown,
             compose(&context, &[b, a]).unwrap().markdown);
  ```

- [ ] **Step 4: Verify and commit.** Run the focused tool workspace commands,
  inspect all failures, and commit through the normal hook:

  ```bash
  cargo fmt --manifest-path tools/digest/Cargo.toml --all --check
  cargo test --manifest-path tools/digest/Cargo.toml --workspace
  cargo clippy --manifest-path tools/digest/Cargo.toml --workspace --all-targets -- -D warnings
  ```

  Record exact command/results and the commit in the task report. Controller
  performs independent task review before Task 2 begins.

### Task 2: Controlled discovery, build, collection and CLI

**Files:** Create `tools/digest/src/context/discovery.rs`, `process.rs`,
`collect.rs`, `tools/digest/tests/suite.rs`, `tools/digest/README.md`.
Modify `context/mod.rs`, `tools/digest/src/main.rs`, `tools/digest/Cargo.toml`,
`tools/digest/Cargo.lock`, `Makefile`, `scripts/lane-outboard.sh`.

**Interfaces:** Consume `digest_protocol::{Contribution, validate,
normalize_scope, scope_matches}`, `compose` and `ContextReport` from Task 1.
Produce `context_from_current_checkout` and the contributor invocation
contract. No production contributor-specific match arm or package dependency.

- [ ] **Step 1: Verify available APIs, then build lifecycle tests first.**
  Resolve development-only Unix dependencies `nix` (signal/process features)
  and `signal-hook`, and read the resolved source before using them. Their
  justification is safe process-group signaling and cancellation delivery;
  do not implement raw unsafe signal handlers. Use
  `CommandExt::process_group(0)` for each invocation and a cancellation flag
  set by SIGINT/SIGTERM. The public standard-library API is documented at
  <https://doc.rust-lang.org/std/os/unix/process/trait.CommandExt.html>.
  New dependency preparation is allowed here; context itself stays offline.

  Test a normal child, nonzero exit with stderr, output overflow, a child
  retaining a pipe through a grandchild, timeout, and cancellation. Use
  injected small limits and fixture-owned temporary directories, not a
  five-second sleep per test. A useful fixture process shape is:

  ```text
  runner starts new process group
    fixture parent writes parent/grandchild PIDs to fixture-owned files
      grandchild waits while retaining stdout
    parent waits (or exits for the inherited-pipe case)
  test cancels or times out runner
  assert: nonzero diagnosis, readers joined, direct child waited,
          recorded processes no longer running
  ```

  Assert fixture readiness before applying the event. Never signal a process
  discovered outside the fixture. Terminated zombies count as not running;
  do not demand portable adoption/reaping of unrelated grandchildren.

- [ ] **Step 2: Implement bounded process ownership.** A helper takes a
  structured `Command`, limits and a shared cancellation flag, returning
  bounded stdout/stderr, exit status and elapsed duration or a phase error.
  Concurrently drain both pipes using bounded channels/buffers; never collect
  an unbounded stream before checking its size. Poll cancellation/deadline
  while draining and waiting. On error, signal the owned process group,
  escalate after the grace period, wait for the child and finish readers.
  Cleanup also handles a normally exited parent with lingering descendants.
  Scope clock lint exceptions to development instrumentation. Unsupported
  systems return a clear context-only error.

- [ ] **Step 3: Implement discovery from the current checkout.** Resolve
  caller cwd using Git with inherited path variables removed (`GIT_DIR`,
  `GIT_WORK_TREE`, `GIT_COMMON_DIR`, `GIT_INDEX_FILE`, `GIT_OBJECT_DIRECTORY`,
  `GIT_ALTERNATE_OBJECT_DIRECTORIES`). Do not use compile-time
  `CARGO_MANIFEST_DIR` as the new context command's runtime checkout selector;
  keep legacy renderer behavior unchanged. Query:

  ```text
  cargo metadata --manifest-path <root>/tools/digest/Cargo.toml
    --format-version 1 --no-deps --locked --offline
  ```

  Parse only necessary metadata fields with serde structs. Validate workspace
  root, actual member IDs, canonical member manifests under `packages/`, role,
  namespace/version/scopes, and exactly one matching binary target. Refuse
  symlink escapes and ambiguous declarations. Metadata scopes select packages
  before their collection build. A root `.` request selects all enrolled
  packages; no match is an explicit nonzero result. The protocol package has
  no contributor metadata and is skipped.

- [ ] **Step 4: Build and invoke exact artifacts.** Use Cargo's JSON
  compiler-artifact messages, matching package ID and binary target, to find
  the executable produced for each selected contributor. Reject missing or
  multiple matches and artifact paths outside the explicitly selected
  `<root>/tools/digest/target` directory. Build with explicit manifest,
  `--package`, `--bin`, `--target-dir`, `--locked --offline`, and
  `--message-format=json-render-diagnostics`; do not guess a binary filename
  or reuse an earlier report. Set cwd to root, stdin null, and control ambient
  target overrides. Execute `collect --repo-root <root>` through the same
  process helper. Verify envelope agreement and collect all selected results
  before writing final report stdout. Failures identify build versus execution.

  Record checkout revision/dirty state using bounded read-only Git commands;
  state that it is context, not complete provenance or a source snapshot.
  Report phase timing on stderr only. A stale lock/dependency error names the
  preparation command without mutating inputs itself.

- [ ] **Step 5: Wire CLI and ordinary tooling.** Add `context <scope>` to
  existing CLI dispatch, exit nonzero on failed reports, and retain existing
  render paths. Add `make context SCOPE=...` and `context-prepare` to help and
  `.PHONY`. Preparation runs Cargo dependency/build preparation explicitly;
  context uses a locked/offline root `cargo run` and the scoped command.
  Quote Make arguments as data; reject a missing scope. Add `--workspace` to
  the existing Digest outboard test entry. The usage guide documents exact
  metadata, contributor argv, JSON, source authority, limits and manual use.

- [ ] **Step 6: Exercise hermetic CLI discovery and isolation.** Fixtures
  create tiny outboard workspaces with self-contained mock contributors, then
  run the real host from their roots. Cases cover root selection, invalid
  metadata/version/target/scopes, missing observation, malformed JSON,
  unknown/contradicted results, no match, source mutation, and two fixture
  roots under inherited Git and target-dir overrides. Fixtures must reproduce
  Cargo manifest selection rather than stub away the very isolation check.
  Test binary reuse across two caller roots: it must observe each runtime
  checkout. Test a saved old report has no acceptance route into collection.
  Keep CLI fixtures offline after preparing their minimal lockfiles.

  ```rust
  // Required property; fixture helpers own isolated directories and commands.
  let first = run_context(&repo_a, "domains/thing", &host_binary);
  let second = run_context(&repo_b, "domains/thing", &host_binary);
  assert!(first.stdout.contains("fixture-a-marker"));
  assert!(!first.stdout.contains("fixture-b-marker"));
  assert!(second.stdout.contains("fixture-b-marker"));
  assert_eq!(tracked_bytes_before, tracked_bytes_after);
  ```

- [ ] **Step 7: Verify, review and freeze bootstrap.** Run Task 1's scoped
  commands plus `shellcheck scripts/lane-outboard.sh`, `bash
  scripts/check-bash32.sh`, real CLI no-match, and existing render smoke tests.
  Inspect generated diffs using the normal rules if any appear. Commit and
  obtain independent review. Controller records the reviewed SHA, pushes the
  campaign branch normally and submits `make sluice-stage` with its full SHA.
  No queue/operator messages without Nathan's explicit messaging permission.

### Task 3: Thing contributor on its own bootstrap branch

**Files:** Create only `tools/digest/packages/thing/Cargo.toml`,
`src/lib.rs`, `src/main.rs`, `README.md`; update the branch's Digest lockfile
only if dependency resolution requires it.

**Interfaces:** Consume the frozen protocol and host contract. Produce a
package named `digest-thing`, binary `digest-thing`, namespace
`hornvale.thing`, scope `domains/thing`, and a public
`pub fn contribution() -> Result<Contribution, String>` local adapter API.
Dependencies point to `../protocol`, `../../../../kernel`,
`../../../../domains/thing`, `../../../../domains/settlement`; serde_json
serializes the envelope. Verify path resolution before adding dependencies.

- [ ] **Step 1: Establish independent base and read the real APIs.** Controller
  creates `codex/the-charter-thing` in its own worktree from the recorded
  bootstrap SHA, starts controller-owned prewarm and provides the task brief.
  Read `THING_KINDS`, `BORROWED`, `thing_registry`, both registration functions,
  `ComponentStore::ids`, and `ConceptRegistry::concept`. Registration can panic
  for stale borrowing as well as return a registry error; handle/report a
  failed observation without manufacturing success. No production source edits.

- [ ] **Step 2: Implement discriminating local tests first.** Factor the
  comparison over supplied rosters/owners so tests can remove a registration,
  change a lender and add an undeclared registry entry without editing the
  production tree. Include a test of actual API composition, not only fixtures.

  ```text
  actual Settlement registration + actual Thing registration -> agreement
  missing lender -> refused registration/contradicted observation
  wrong lender -> refused registration/contradicted observation
  missing owned kind -> ownership check fails
  extra registry kind -> reverse roster-inclusion check fails
  changed authored statement -> observation outcome does not change
  ```

- [ ] **Step 3: Build the local envelope.** Preserve source roster order in
  details, compare roster and registry sets in both directions, observe actual
  composed owners, and name each required observation before producing results.
  Explain owned versus borrowed concepts and point to the actual APIs and
  domain guide. State the worldgen/save/item-behavior limits explicitly. Export
  JSON via `collect --repo-root <root>`; the binary validates argv and scopes
  but does not shell out, run worldgen or maintain a duplicate kind roster.

- [ ] **Step 4: Validate autonomy and commit.** Run protocol/local tests,
  local clippy and fmt; invoke the frozen host for `domains/thing`. Record
  dependency/lockfile changes. Stop and report a design counterexample if
  host/protocol meaning needs an edit. Commit through ordinary hooks, with the
  controller staggering gates against Task 4. Independent review follows.

### Task 4: Census publication contributor on its own bootstrap branch

**Files:** Create only `tools/digest/packages/census-publication/Cargo.toml`,
`src/lib.rs`, `src/main.rs`, `README.md`; update its Digest lockfile if required.

**Interfaces:** Consume frozen protocol/host and lab's public pure guard.
Produce package/binary `digest-census-publication`, namespace
`hornvale.census-publication`, scopes `windows/lab/src/census_guard.rs`,
`windows/lab/src/publish.rs`, `scripts/census-canonical-host.txt`,
`scripts/sluice-census.sh`. Local API:
`pub fn contribution(repo_root: &std::path::Path) -> Result<Contribution, String>`.
Dependencies: protocol at `../protocol`, lab at `../../../../windows/lab`,
serde_json. Verify Cargo paths and lab API signatures before implementation.

- [ ] **Step 1: Establish independent base and verify scope.** Controller
  creates `codex/the-charter-census` in a separate worktree at the same bootstrap
  SHA as Task 3 and owns its prewarm. Read `census_guard.rs`, `publish.rs`,
  canonical-host file, and current queue request authority. Do not restate
  gate phase rosters. The lab dependency may compile many crates; it must not
  execute their world-build or census tests as part of local observation.

- [ ] **Step 2: Build a finite two-sided guard panel.** Cases use canonical
  host, a guaranteed different hostname, case-normalized host, the exact
  census study and prefix forms, unrelated study, official suffix and an
  unrelated output directory. Read constants from APIs/current source;
  expected allowed/refused behavior is independently authored in the cases.
  Test both an always-accepting and always-refusing supplied guard against
  that same evaluator so the panel demonstrates both failure directions.

  ```text
  official study + official suffix + canonical host -> allowed
  official study + official suffix + different host -> refused
  unrelated study OR unrelated output -> permitted by this predicate
  source host differs from compiled host -> collection error
  ```

- [ ] **Step 3: Produce honest context.** The finite guard requirement names
  all its required observations. The queued-census instruction is a separate
  authored-only requirement with source/authority links. Do not mark queue or
  publication-call execution as observed. Explicitly explain path-suffix
  semantics, finite-case coverage and compiled-host/source agreement limits.
  Compare the current root's host file with `CANONICAL_CENSUS_HOST` and return
  an actionable error on mismatch. No publish/census/queue side effect.

- [ ] **Step 4: Validate autonomy and commit.** Run only this contributor's
  tests, protocol validation, local fmt/clippy and a frozen-host request for
  `windows/lab/src/publish.rs`. Inspect tracked files before/after. Record
  build and run separately, changes and exact branch base. Stop/report any
  necessary semantic host/protocol edit. Commit with staggered controller
  gate timing and obtain independent review.

### Task 5: Compose branches and qualify the actual user surface

**Files:** Integrate the two reviewed package trees and reconciled
`tools/digest/Cargo.lock`; modify `tools/digest/README.md`, `docs/README.md`;
create `docs/digest/the-charter-evidence.md` and `scripts/charter-measure.sh`.
The measurement script is an explicit diagnostic, not a new routine gate.
Fix existing task-owned files
only for reviewed defects; a necessary new semantic host variant is a failed
autonomy experiment, not an integration fix to conceal.

**Interfaces:** Consume reviewed adopter commits and frozen bootstrap, exact
CLI and envelopes. Produce composed task context and quality evidence on both
supported hosts. Do not introduce a third contributor or cached evidence path.

- [ ] **Step 1: Verify branch independence before integrating.** Record the
  bootstrap SHA, each branch merge-base and full changed-file list. The
  controller integrates only reviewed commits into `codex/the-charter`, using
  ordinary non-force Git operations. Reconcile Cargo.lock through Cargo and
  inspect it. If branches differ outside permitted scopes, classify each
  difference; semantic host changes reopen design, ordinary hook artifacts
  need normal reconciliation. Run actual tools after integration; a clean
  merge alone does not establish generated freshness.

  ```bash
  git merge-base codex/the-charter-thing codex/the-charter-census
  git diff --name-only <bootstrap-sha>..codex/the-charter-thing
  git diff --name-only <bootstrap-sha>..codex/the-charter-census
  ```

  The SHA placeholders here are runtime values recorded after Task 2; resolve
  them from Git, never invent or copy a shortened ref into a queue request.

- [ ] **Step 2: Exercise the real composed product.** Prepare dependencies,
  run both selected scopes and `.` through the real CLI; capture output and
  stderr phase costs. Confirm identities, required observations, authored
  queue policy, source links, no tracked mutation and stable re-rendering of
  identical collected envelopes. Change a local input in an isolated fixture
  and verify recollection changes the result. Keep saved reports as examples,
  never inputs. Run all Digest packages' tests/clippy/fmt once after the join.

- [ ] **Step 3: Record quality evidence and usability.** Write a table mapping
  every acceptance property and `charter.*` scenario to exact commands,
  observations and limitations. Measure one cold disposable-target build and
  ten warm invocations for Thing, census and combined workloads per supported
  host, with available peak memory, toolchain, source and load context. Separate
  build, collection, waiting and failures. Canonical measurements run only as
  ordinary serialized canonical work, never contend by a side-channel SSH job.
  To carry the diagnostic through existing queue machinery, the controller
  creates an isolated stage-only measurement branch from the implemented
  candidate. Its only extra integration edit invokes `scripts/charter-measure.sh`
  from that branch's outboard script. Submit it as a stage request, never a
  merge; fetch its completed job log as evidence and record the tested SHA
  and diagnostic-only diff. This temporary branch is not merged back. The
  script prints samples to stdout and writes no committed artifact. It creates
  fixture-owned disposable detached worktrees at its input HEAD for cold
  runs, so their fixed `tools/digest/target` directories are genuinely fresh;
  it never deletes or redirects an active worktree's target cache. Use traps
  to remove only those owned worktrees after their processes have finished.
  Record failed samples too; unavailable logs or failed runs remain failures,
  not implied measurements. No direct competing canonical SSH execution or
  operator messaging is authorized. Propose targets from measured evidence; cap values
  remain the approved policy unless deliberately amended.

  Give an independent reviewer the two actual reports and one failure report:
  can they identify authored claims, observed coverage, failed obligation and
  relevant owner/source without this conversation? Record corrections and
  limits. Link the command from `docs/README.md` and document preparation,
  supported hosts, error behavior, caps, and extension steps in the tool guide.

- [ ] **Step 4: Verify complete scope and prepare G6.** Controller runs the
  current commit gate, scoped tool checks and ordinary queued stage validation
  for the integrated SHA; inspect all results. Use one broad whole-branch
  review, triage its findings in the committed ledger, and close its fixes with
  scoped review. Complete the stage tracker only when evidence is actually
  present. Present the concrete implementation, tests, costs, unresolved limits
  and post-G3 ledger at G6. Do not submit a merge or claim the campaign closed
  before Nathan's close approval and the closing-a-campaign workflow.

## Plan self-review and coverage map

| Spec responsibility | Tasks |
|---|---|
| Protocol, expected observations, identity/reference refusal | 1; exercised by 3–4 |
| Discovery, artifact selection, isolated fresh context | 2; verified on real composition by 5 |
| Determinism and plain-text evidence distinctions | 1–2, reviewed in 5 |
| Thing and census real APIs plus discriminating counterexamples | 3 and 4 respectively |
| Process caps, cancellation, descendants, supported hosts | 2, canonical confirmation in 5 |
| Independent branch extension and permitted lock integration | 3–5 |
| Cost, usability and evidence limits | 5 |
| Existing tooling compatibility and documentation | 2 and 5 |
| Current gates, tracked-input integrity and G6 stop | All tasks; controller owns submissions |

Shared-file/interface checks: Tasks 1→2 share context module/manifest/lock and
are sequential. Tasks 2→3/4 share only frozen protocol/metadata/argv; adopters
must not edit those files. Tasks 3/4 both resolve the Digest lock on independent
branches, explicitly reconciled by 5. Task 5 consumes the exact named interfaces
above. Tasks 1–5 each include behavior tests, verification, report, commit and
independent review; Task 5's controller integration does not bypass review.

No claim of unchanged generated artifacts, successful process cleanup, branch
compatibility or performance is earned by this plan text. The named acceptance
experiments supply that evidence, and their failure branches govern the next
action.
