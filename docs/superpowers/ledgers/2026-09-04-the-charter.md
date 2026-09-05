# The Charter — decision ledger

Campaign: `codex/the-charter` · Base:
`fd3cd159be98e6c07dbb849b780c2712c6a7826f` · Phase: implementation, Stage 1.
Nathan authorized the brainstorm on 2026-09-04 after discussing the
self-describing program, federation, and local evolutionary ratchets.
Nathan approved G3 with the quality refinement; G4 and its rulings are recorded below.

## #1 [G1] — prove independent adoption before granting narrower admission

**Question:** Should the first campaign build the whole federated development
system, improve documentation alone, or establish a contribution boundary
that distinct adopters can use independently?

**Decision:** Establish a small development-tool contribution protocol and
prove it with two heterogeneous adopters, each producing checked observations
and useful scoped instructions. Capture the wider evolution rules in a
program metaplan. Existing merge, gate, census, and approval rules remain
authoritative; this campaign grants no permission to skip validation.

**Why:** The Digest deliberately deferred generated governing text and
language realization; its current fixed vocabulary and renderer dispatch do
not establish independent extension. The next experiment must demonstrate
that useful local contributions compose without a new shared vocabulary or
dispatcher edit for each adopter. User-approved direction: local adoption,
explicit obligations, and a first campaign developed through independent
adopter branches after a shared bootstrap.

Precedents: [The Digest design](../specs/2026-08-08-the-digest-design.md),
[kernel membership](../../decisions/0517-the-kernel-membership-criterion.md),
[open-registry totality](../../decisions/0556-totality-by-registry-replaces-totality-by-compiler.md),
and [evidence doctrine](../../design/evidence-doctrine.md).

**Alternatives discarded:** A universal ontology/runtime plugin platform
before a useful adopter, because it defers the very integration evidence the
campaign needs. A prose-to-JSON relocation, because serialization alone
cannot establish either live truth or independent extension. Immediate gate
reduction, because the proposed effect model has not earned that authority.

**Ideonomy passes / overturns:** Two new passes for this decision. The first
used abstraction, dimensions, negation, a list, and a spectrum: the useful
position lies between descriptive records and autonomous execution, and
side effects must remain with existing operators. The second used
cross-domain reinstantiation and a periodic grid: museum accession separates
independent collections from catalogue rules; the grid of one/many owners
against assertion/observation/view responsibilities exposed no additional
required bootstrap mechanism. No overturn; the second pass added no
material scope. Three preceding exploratory passes are historical input,
not substitutes for these campaign passes.

**Capture actions:** This ledger records the ruling before design proceeds.
The program metaplan will retain the eight evolution rules and deferred
directions. Registry capture will reuse existing homes and add only distinct
federation/contribution/adoption ideas after the category scan.

## #2 [Q] — two adopters with different sources of evidence

**Question:** Which adopters exercise a useful boundary without pulling a
world build or an operator's side effects into the first protocol?

**Decision:** Thing ownership and borrowing, checked by composing the real
Settlement and Thing registration APIs; and the official census publication
host predicate, checked through the public pure Rust guard. The second
contributor also carries the existing queued-census instruction as an
authored requirement, explicitly separate from observed guard behavior.

**Why:** `domains/thing/src/lib.rs` exposes `THING_KINDS`, `BORROWED`,
`thing_registry`, and `register_concepts`; Settlement exposes its real
registration function. A development adapter may depend on both domains
without creating a sibling-domain dependency. `windows/lab/src/census_guard.rs`
exposes `require_canonical_host_for`, which accepts study, output path, and
hostname as inputs. Its scope is a census name and a path suffix, not every
canonical artifact or the whole queue. `publish.rs` calls this guard before
writing; observing the predicate alone does not prove that invocation.

**Alternatives discarded:** Alchemy's trait adapter, because a signature or
source scan would invite a larger purity claim than the check establishes.
A complete queue-policy proof, because claim ownership, dispatch, staging,
and publication are distinct behaviors. A hand-transcribed concept roster,
because the real APIs already supply the observation.

**Ideonomy passes / overturns:** One pass: tree-finding, map, direction,
naturalness. Mapping dependency direction against the distance from a callable
boundary left Thing and the pure guard as the smallest distinct adopters;
no further material improvement or overturn. Each result names the narrow
scope actually observed.

**Capture actions:** Retain Alchemy and publication/queue boundary checks
as follow-ups, not hidden acceptance requirements. Existing worldgen and
publication tests retain their authority. The spec will require meaningful
behavioral counterexamples, including a missing or wrong lender and a guard
that admits an off-host official publication.

## #3 [Q] — independent packages, one small wire envelope

**Question:** How can a second contributor arrive without a new host match
arm, shared ontology edit, or simulation dependency on development tooling?

**Decision:** Extend the existing outboard Digest tool with a small Cargo
workspace under `tools/digest/packages/*`. Put the permanent protocol package
under that glob, and discover contributor packages through Cargo metadata.
Each contributor is a Rust executable returning the same versioned JSON
envelope; its local code owns the domain-specific observations and instruction
text. The host composes records and rejects collisions. No runtime simulation
plugin loader, arbitrary executable path, or new kernel vocabulary is needed.

**Why:** The current Digest is already isolated by its own `[workspace]`
table and has serde/JSON dependencies. Its `main.rs` dispatch is currently
fixed. Development adapters depending on simulation crates preserve the
existing dependency direction. The existing `ProjectLedger` has local numeric
identities and functional assertion replacement; concatenating its stores
would conceal a disagreement instead of composing independent owners.

**Alternatives discarded:** Static host linkage to every contributor, which
would preserve a central enrollment edit; a general dynamic library ABI,
which adds lifetime/compatibility machinery this experiment does not need;
a universal project ontology, which would make local adoption wait on it.

**Ideonomy passes / overturns:** One pass: tree-finding, atlas, longevity,
direction. The ownership and temporal maps separate the durable envelope
from local semantic payloads and run observations. This confirmed the boundary
without another scope addition. One implementation correction came from the
probe below: the glob must contain the permanent protocol at bootstrap.

**Tool-behavior evidence:** Throwaway Cargo projects, outside the repo:
`cargo metadata --offline --no-deps --format-version 1 --manifest-path ...`
returned rc=101 for an empty `contributors/*` glob; adding a member returned
rc=0. A second prototype used a root package plus `packages/*`, initially
containing protocol. Metadata returned rc=0 with host/protocol, then rc=0
with host/protocol/thing/census after only package additions. In both states,
`cargo run --offline --quiet --manifest-path ...` returned rc=0 and
`root-ok`. These are discovery/root-selection probes, not a completed plugin
implementation. Cargo documents the mechanisms in its
[workspace reference](https://doc.rust-lang.org/cargo/reference/workspaces.html)
and [metadata command](https://doc.rust-lang.org/cargo/commands/cargo-metadata.html).

**Capture actions:** Record Cargo.lock as a shared integration artifact,
not a promise of conflict-free branches. The autonomy acceptance exercise
forbids contributor-specific host/protocol edits after bootstrap; a failure
reopens the design. Retain semantic input-closure and richer local schemas
for later campaigns.

## #4 [Q] — useful context now, bounded observations rather than certificates

**Question:** What should the first campaign deliver to an author, and how
much evidence authority should that output carry?

**Decision:** Add an on-demand `digest context <repo-relative-path>` command
and a `make context SCOPE=...` entry point. They assemble scoped instructions,
live check outcomes, and their explicit limits from contributor packages.
The Markdown is directly usable as task context for either agent family;
root prompts and the Book are not replaced in this campaign. Collection
builds through Cargo and invokes contributors afresh. Historical output is
never loaded as current evidence, and no result is a reusable admission
certificate. Report the checkout identity/dirty state and observation method
without claiming they enumerate every build or runtime input.

**Why:** The Digest design explicitly deferred governing text and language
realization. A bounded context command closes part of that gap while leaving
existing authored requirements authoritative. The evidence doctrine and
`PROC-a-parked-finding-carries-no-use-by-date` distinguish observations from
broader judgments; `TOOL-verdict-cache` already owns complete-input reuse.
Existing publication tests and worldgen checks remain separate evidence.

**Alternatives discarded:** Persisted green receipts treated as current on a
different checkout; source anchors mistaken for executable checks; replacing
the root prompts before two local contributors work; complete reproducible
build/input closure as a prerequisite for a read-only context experiment.

**Ideonomy passes / overturns:** One pass: organon-construction, scale,
modularity, naturalness. A scale from named predicate cases through the
publication boundary to the queue separates the claims each instrument can
make. It confirmed the bounded result with no additional mechanism or
overturn. Full provenance closure is recorded as debt, not silently assumed.

**Capture actions:** The program reserves complete input closure, validated
reuse, generated persistent prompt sections, and native language realization
for subsequent adoption. The campaign must label authored instructions and
observations separately and show unknown/failed checks without green success.
Its context command is an explicit user/agent entry point, not automatic
startup integration and not a sandbox for contributor code.

## #5 [G2] — design reviewed; present the G3 package

**Question:** Does the architectural design preserve the agreed scope and
provide a falsifiable route to independent adoption?

**Decision:** Present the proposed program metaplan and first-campaign spec
for Nathan's G3 review. Do not start implementation planning. The spec binds
an actual two-branch composition exercise, useful scoped context, explicit
expected observations, and bounded authority. The program's eight rules
remain proposals for incremental adoption rather than newly enacted policy.

**Why:** Three independent read-only reviews checked Rust/package feasibility,
Thing and census evidence, current gate authority, historical scope, and
capture. The controller checked the actual files and applied four concrete
corrections: expected observation IDs independent of returned results;
discoverable scopes with envelope agreement; explicit Digest manifest paths
for Cargo operations; and process-tree cleanup with build/execution phases
separated. Acceptance cases now cover each correction. The integration/history
review read the corrected evidence schema and found no material blocker.

**Alternatives discarded:** Treating missing checks as authored-only;
discovering applicability only after running every contributor; relying on
the caller's Cargo workspace; killing only a Cargo parent while descendants
continue. These defeat stated requirements rather than offer useful options.

**Ideonomy passes / overturns:** No additional pass required for this G2
self-review. The two G1 passes and one pass for each of the three nontrivial
Q decisions are recorded above. Review sharpened the accepted design rather
than overturning the approach. Broader possibilities retain their program
or existing-registry homes.

**Capture actions:**

- [Program metaplan](../specs/2026-09-04-federated-development-program-metaplan-design.md):
  eight evolution rules, concurrent adoption pattern, later expansions,
  formal verification and orchestration opportunities, measurements, and
  reuse of existing idea homes.
- [The Charter spec](../specs/2026-09-04-the-charter-design.md): package
  boundary, protocol, adopters, user surface, limits, acceptance, and G3 flags.
- [Idea registry](../../../book/src/frontier/idea-registry.md): four new
  `raw` proposals, `PROC-federated-evolution`, `PROC-knowledge-contributions`,
  `PROC-evidence-selects-obligations`, and `PROC-local-adoption-and-activation`.
  No shipped/ratified status or waiver was added.
- The visual companion illustrates bootstrap, parallel adoption, composition,
  and failure/unchecked/no-match cases. It is a review aid outside the repo;
  all binding content is captured in the committed spec and metaplan.

**Verification:** `make docs-tests` returned rc=0: 63 passed, 246 skipped,
3.051 s execution on the draft package. `git diff --check` returned rc=0.
The companion was opened in the browser, inspected visually, and its missing
observation interaction was exercised. Ordinary commit hooks remain enabled.
These checks validate the design documents and example UI, not an implemented
contributor system. The earlier Cargo prototypes establish only the narrow
tool behaviors recorded in #3.

## #6 [Q] — nonfunctional requirements are scoped quality contracts

**Question:** Nathan asks what place nonfunctional requirements have in the
proposed system, before approving the G3 package.

**Decision:** Recommend first-class quality contracts alongside behavioral
requirements. A contract names its subject/owner, scenario and workload,
environment, required response or budget, evaluation method, evidence scope,
and action on breach. Distinguish categorical guarantees, measured budgets,
and review-based qualities. This records a design refinement for the pending
G3 discussion; it does not invent numeric limits, approve new gate policy,
or claim the protocol already implements quality-specific records.

**Why:** The Charter already specifies deterministic composition, errors,
process cleanup, and independent enrollment, but its cost requirement mostly
asks for measurement. Local conformance cannot establish aggregate memory,
latency, contention, or recovery behavior. Quality belongs at three scopes:
local contributor, cross-contributor agreement, and whole-system composition.
The program's existing effect/amendment rules support this distinction.
Scenario-based quality analysis also has an established precedent in the
[SEI quality-attribute workshop](https://www.sei.cmu.edu/library/quality-attribute-workshop-collection/).

**Alternatives discarded:** Boolean “fast/reliable/modular” manifest badges;
a single overall quality score; summing component percentiles as an end-to-end
latency proof; universal thresholds across dissimilar workloads; automatically
relaxing a target whenever a baseline regresses; treating compilation of a
requirement descriptor as proof that its runtime target is achieved.

**Ideonomy passes / overturns:** Two passes. Substitution over size and
homogeneity, captured as a timeline from two small contributors to many
heterogeneous adopters, exposed the need to preserve a workload envelope and
shared composition budgets as enrollment grows. Abstraction-lift over
distribution and visibility, captured as states proposed → measured →
adopted → breached/stale → re-evaluated, confirmed that observation, acceptance,
and breach handling are separate, with no further material mechanism added.
No approach overturn; these requirements sharpen the federation's obligations.

**Capture actions:** Add `PROC-quality-contracts` as a raw registry proposal.
Before G3 approval, refine the campaign's quality scenarios: worktree isolation,
failure/cancellation recovery, compatible extension, plain-text diagnostics,
supported-host operation, and separately measured build/collection costs.
Keep complete provenance, global test-selection authority, and a universal
quality ontology outside this first campaign. Target numbers need justified
workloads and measurements; desired service levels and observed baselines
are different records.

## #7 — G3 approval and quality refinement incorporated

Nathan replied “Sounds great. Let's continue.” after the G3 package and the
quality-contract refinement. Treat that as approval to incorporate the agreed
refinement and proceed to planning, not as a reason to repeat the approval
question. G4 remains a plan self-review; G6 remains the campaign-close stop.

The spec now contains nine named quality scenarios, including controlled
worktree selection, deterministic output, bounded process lifetime, explicit
resource caps, measured costs, compatible extension, diagnostics, portability
and review-based usability. Existing records can carry quality requirements;
no universal quality schema was added. Targets and baselines remain separate.

Implementation elaborations: conservative byte/time caps bound the two small
trusted subprocesses; they are policy choices, not observed performance
claims. Dependency preparation is separate from locked/offline read-only
context collection. Process-group termination and direct-child reaping are
required; portable reaping of arbitrary grandchildren is not promised. These
make the accepted guarantees implementable without claiming a sandbox.

Capture: spec §8 and program §6 carry the requirements; ledger #6's pending
refinement is addressed. No Ideonomy rerun for recording Nathan's approval;
the quality-contract decision's two passes are recorded in #6.

## #8 [G4] — staged plan self-review and execution ruling

**Question:** Does the implementation plan cover the approved spec with
consistent interfaces and genuinely independent adopter work?

**Decision:** Execute the [five-task plan](../plans/2026-09-04-the-charter.md)
in three stages. Protocol then process host are sequential. The two adopters
fork from the reviewed bootstrap into separate worktrees. Composition,
measurements and whole-branch review follow. G6 remains the merge/close stop.

**Why:** The spec's independent-adoption acceptance requires real parallel
branches; it is not satisfied by two agents editing one checkout. This
user-approved experiment supersedes the generic SDD warning against parallel
implementers sharing files. Shared lock resolution is recorded and reviewed.
No agent may change another checkout or dispatch its own reviewer.

**Pre-flight self-review:**

| Tasks / surface | Producer and consumer | Finding / ruling |
|---|---|---|
| 1 internal | Types, validators and composition tests | Required evidence is declared separately; malformed structure differs from contradicted/unknown observations. |
| 2 internal | Discovery metadata, selected build, executable response | Scopes exist before execution; explicit runtime checkout/manifest/target paths avoid compile-time or ambient worktree selection. |
| 3 internal | Real registries and independently authored comparisons | Registration may panic as well as return an error; report failure honestly. No production edits. |
| 4 internal | Finite guard panel and source/compiled host check | Expected allowed/refused outcomes are independent of the implementation; queue policy stays authored-only. |
| 5 internal | Integrated reports and quality evidence | Measurement is explicit and uses disposable owned worktrees; no active cache deletion, no silent absent Linux sample. |
| 1 → 2 | Shared protocol/composition functions; module and manifests | Exact names in the plan; sequential edits. |
| 2 → 3/4 | Metadata, JSON, argv and scope contract | Frozen before fork; semantic core edits invalidate the autonomy experiment. |
| 3 ↔ 4 | Independent packages, shared lockfile ancestry | Separate branches/worktrees; controller staggers gates and reconciles only reviewed dependency artifacts. |
| 3/4 → 5 | Reviewed commits and actual context | Explicit branch-base/diff evidence; generated freshness checked after integration. |

**Alternatives discarded:** Unspecified canonical diagnostic execution,
which would invite contention or a missing result; instead use an isolated
stage-only measurement branch whose outboard command invokes the diagnostic
under the existing queue, and never merge that extra invocation. A direct
competing SSH job and unsolicited operator messaging are excluded.

**Ideonomy passes / overturns:** No new nontrivial architectural question was
opened in this G4 self-review; prior approach and quality-contract passes
remain the basis. The measurement transport is a concrete use of the existing
stage queue, not new gate/admission policy. Runtime values such as bootstrap
SHA and measured costs must be read from execution, never predicted by prose.

**Capture actions:** The plan maps every spec/quality scenario to a task;
`IMPLEMENTATION_PLAN.md` tracks the three stages. The permanent evidence
report will retain the branch experiment and measured quality results.
Task briefs/reviews remain scratch; rulings and follow-ups stay here.

## Task 1 — implementation verified; independent review pending

Base: `6849ccd31c4065656aa08a197b70c67427013ff9`.
Implementation: `adf5c78c91916e223147db2ecd72c40886abcede`.
The new protocol and generic composer are committed; no process host or
adopter is claimed yet. The implementer demonstrated behavioral red/green
for omitted required observations and incomplete report rendering.

Verification reported and read: 40 Digest tests and 12 protocol tests passed,
doctests passed, outboard fmt and clippy passed; normal commit gate returned
rc=0 in 62.911 s with all three subfloor chunks passing. The hook's ordinary
timing row is retained with this controller record.

Ruling: permit exactly the two pre-existing rustfmt-only blocks in
`tools/digest/src/mcp.rs` exposed by the required whole-Digest format check.
The alternative was to leave the mandated check red or narrow it around a
known failure. No behavior change or unrelated cleanup was authorized.
This mechanical compatibility ruling needed no new architectural Ideonomy
pass. The task report records the original failure and resulting diff.

The command guard required its documented `HV_TEST_OK=1` acknowledgement for
the outboard `cargo test --manifest-path tools/digest/Cargo.toml --workspace`
invocation; the command still selected only the Digest workspace and did not
disable tests or commit hooks. The review has the exact task diff and the
recorded evidence. No completion verdict is assumed before it returns.

## Task 1 — fix round 1: explicit contradictions cannot succeed

Independent review: spec compliance failed; code quality passed. The composer
calculates success from required observations only, so an additional explicit
contradiction can be rendered alongside `successful=true`. Spec §6 requires
every contradiction to fail; only unknown outcomes are qualified by whether
the observation is required. The controller checked the predicate in
`context/compose.rs` and accepts the finding.

Ruling: retain required-observation satisfaction and additionally require
that no observation is contradicted. Add behavioral red/green cases for an
optional contradiction and optional unknown, preserving their distinction.
This clarifies the already binding spec; no architectural alternative or
new Ideonomy pass is needed. Resume the original implementer, then obtain a
scoped re-review of this fix. No task-2 dispatch before approval.

## Follow-ups

- Profile the measured Linux `git status --porcelain --untracked-files=normal`
  phase under the existing canonical queue before adopting a latency target;
  the evidence report observes roughly 62–63% of warm request time there,
  without attributing the cause. Repeat over time/load conditions before an SLO.
- Revisit contributor dependency size as enrollment grows: the pure census
  guard currently pulls the lab dependency graph, with measured cold selected
  requests of 11.552 s on Mac and 25.712 s on Linux. Consider a smaller owning
  boundary only with evidence that it preserves the real API and contracts;
  this campaign does not restructure production crates for benchmark gains.

- Broad review M1: `render_code_list` uses heading escaping inside Markdown
  code spans, leaving literal backslashes in copyable path/symbol labels.
  Add a dedicated inline-code renderer with correct backtick handling in
  presentation follow-up work; the current bounded usability review passed.
- The review-package capper samples lines, so a JSONL file with fewer very
  long lines than its sample count can exceed the advertised byte cap. Use
  parsed field/aggregate inspection for such evidence and fix byte-bounded
  sampling in a later tooling change.

- Generated-write capture uses mtime across a run, so concurrent controller
  edits can be counted as generator writes. Preserve that attribution limit
  and keep measured authoring runs free of unrelated edits under their stems.

- Report presentation: put the census pure-predicate qualification next to
  its requirement result; make the distinct Thing observation subjects and
  contributor registration order easier to scan. Independent usability
  passed with the current explicit per-observation limits, so these are
  refinements rather than blockers.
- Richer dirty-state identifiers/diff attribution belong to later provenance
  work. The current revision/dirty pair explicitly does not identify every
  observed input; do not present it as a reproducibility key.

- Reconcile the campaign-reconciliation TSV ledger column with the coverage
  population before using it; Task 5 absorption found parser acceptance and
  coverage rejection of the same optional ledger reference.

- Gate selection and receipt reuse require a later campaign with an explicit
  input-closure model, shadow comparisons, and review under accepted rules.
- Full Book/language realization, broad registry import, and root-prompt
  replacement remain later adopters of the program, not prerequisites for
  this first campaign.
- Preserve the distinction between source locality and semantic effects;
  local files alone cannot establish safe independent activation.
- Later adopters: Alchemy's contract, the actual publication boundary, and
  the queue lifecycle. The first campaign's predicate check proves none of
  those by implication.
- G3 quality refinement addressed in #7 and spec §8. Measured performance
  targets remain distinct from the initial resource caps and must be justified
  by the first composed workload report, not invented from the design.

## Verification and observations

- The isolated worktree was created from the fetched `origin/main` at the
  base above. No implementation files have been changed during this phase.
- `make prewarm` completed with rc=0; its ordinary timing row is retained in
  `docs/timings.md`. This is build preparation, not a claim that tests passed.

## Task 1 — complete after scoped re-review

Reviewed implementation: `8f610af758214d5234fd9e36f21f748251d6fcff`.
Independent reviewer `/root/charter_protocol_review` reports spec compliance
and code quality PASS after the optional-contradiction correction. The fix
retains informational optional unknowns. Behavioral red/green, ten focused
composer tests, fmt, clippy, and the ordinary commit gate (rc=0, 61.778 s)
are recorded in the task report; re-review checked the exact fix range and
`git diff --check` without repeating the suite. The hook's timing row is
retained with this entry. Task 2 now consumes the reviewed protocol.

Task 2 preflight verified actual context/protocol exports, the existing CLI
and outboard command, and cached `nix`/`signal-hook` APIs. A disposable Cargo
fixture emitted exactly one matching executable artifact on both cold and
cached builds, with metadata's opaque package ID preserved. This supports
artifact selection, not complete provenance. Timing decomposition unavailable
from Cargo is to remain explicitly unavailable.

## Task 2 — implemented; independent review pending

Implementation: `b45f274f9152fec6d9c989f8760bada1701fdb1b`, from
`df881bd200c0bf2151c42c24aa6fc0a411c2be60`. The generic host discovers
current-checkout Cargo members, selects exact built artifacts, collects fresh
bounded JSON and renders only after validation. Local evidence: 52 Digest
unit tests, 12 real-Cargo CLI fixtures, 12 protocol tests; fmt, clippy,
shellcheck, Bash 3.2 and legacy render smoke checks passed. Independent
spec/quality review is running against the exact implementation range.

The ordinary hook first refused because its lexical substring inventory
counts `cell` inside cancellation-related identifiers. The task renamed
host-local identifiers to interruption, preserving the signal behavior and
leaving the guard/inventory unchanged. Both the failed gate (36.326 s) and
successful retry (37.102 s, rc=0) remain in the timing ledger. This is an
implementation naming correction, not a weakening of the process contract.

Canonical preflight found Rust's outside-repository default is 1.77.1 while
the tracked toolchain pin selects 1.96.1, matching the Mac. Real Cargo
fixtures copy the pin to test with the intended prepared compiler. Canonical
execution is still unearned. The expanded fixture suite also caught Cargo's
null metadata for an unenrolled package; that case now passes. No stage or
merge result is claimed by these local tests.

## Task 2 — fix round 1: the harness owns its Git commands

Independent review found one Important defect in `tools/digest/tests/suite.rs`:
fixture setup and tracked-file snapshots inherited Git path overrides. The
reviewer reproduced the existing isolation test failing during setup, using
only a disposable outer repository. Controller source inspection confirmed
the unchecked init/add/commit/ls-files calls. Runtime collection already
scrubs these variables; its behavior is not the finding.

Ruling: accept the fixture-ownership defect and correct the fixture helper —
the hermetic test requirement applies before the host subprocess too — the
cost if wrong is a narrow test-harness change and a focused validation run.
Remove all six path overrides from fixture Git commands, retain deliberate
host overrides, and verify the contaminated-environment test without touching
any real checkout or index. Preserve the outer fixture's HEAD/index as a
non-vacuous check. This implements the approved isolation obligation and
requires no protocol or scope amendment.

## Task 2 — complete; bootstrap frozen

Frozen bootstrap: `6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f`. Scoped re-review reports
spec compliance and quality PASS, resolving the sole fixture-ownership finding.
Thirteen CLI tests, fmt, clippy and ordinary gate (rc=0, 34.581 s) passed after
the correction; the reviewer did not repeat those tests. No production host
semantics changed in that fix. Both adopter branches start at this exact SHA.

The controller now submits a stage request for the frozen bootstrap and begins
the two independently reviewed adoption tasks in separate worktrees. The plan
requires freezing and submitting at this boundary; asynchronous implementation
can proceed while the queue works. Stage 1 remains incomplete until its actual
canonical report arrives. A later failure must be resolved before claiming
integration acceptance. Hook timings are retained with this entry.

## Stage 1 queued; independent adoption started

Request `req-6fe554d0ebc6-20260905T032743Z` durably queues the frozen
bootstrap as a **stage** on lefford. Job
`sluice-6fe554d0ebc6-20260905T032853Z` reported mouth ADMIT, then waited
for the shared claim; no canonical success is inferred. The branch was pushed
normally before submission. No operator message or board post was sent.

Both `codex/the-charter-thing` and `codex/the-charter-census` begin at
`6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f`, in distinct worktrees. Their
agents received only local adopter briefs/shared interfaces and verified
preflight findings. Controller-owned prewarm completed rc=0 for each:
419.445 s for Thing, 411.766 s for census, while running concurrently.
These are setup costs, not context latency samples. Agents held additional
Cargo builds during preparation and will stagger ordinary commit gates.

For the enrollment exercise both invoke the same copied bootstrap executable,
SHA256 `53e4f8c26374845db41914c6a3732d5d5fd65c1e69183d859d85dc3bb0337af0`,
from their own roots. This is executable reuse with fresh collection, not saved
evidence reuse. Each adapter's branch diff and actual output will determine
whether enrollment stayed local.

## Task 5 preparation may overlap independent adopters

Ruling: begin authoring the diagnostic harness while the two adopter branches
run — its inputs are the already frozen host CLI, declared workload scopes and
quality scenarios, and its files are disjoint — the cost if an adopter exposes
a design defect is a small harness adjustment before measurement. Task 5 still
waits for reviewed integration before collecting actual reports or costs.
Controller retains integration, index/commit scheduling and canonical dispatch;
no task is marked complete early and all task/final reviews remain required.
This extends the same approved independent-work principle used for Tasks 3–4,
without running competing canonical work or editing their shared contract.

## Task 4 — fix round 1: protected forms need refusal probes

Ruling: accept the census review finding — canonical-host successes cannot
establish that a study/path form is protected, because unrecognized inputs
also return success — the correction is local panel coverage, with no host
or protocol change. Controller checked the actual case table and pure guard.
Add different-host refusal cases for the census-of prefix and exact relative
suffix, and demonstrate that predicates omitting either protected form are
contradicted. Keep positive case-normalization and unrelated-input controls.
All added observations remain required by the checked guard obligation.

## Stage 1 — canonical host suite green; full stage held on documentary drift

Job `sluice-6fe554d0ebc6-20260905T032853Z` acquired the claim after
942 s queued. Canonical outboard finished rc=0 in 72.545 s, including 52
Digest unit tests, 13 CLI fixtures and the protocol suite. This supplies Linux
host-suite evidence. The full stage is **not green**: its gate phase failed
`campaign_reconciliation_covers_every_campaign_record` after 83.794 s.
Main's newer documentary guard requires the Charter plan, Charter spec and
federated-program metaplan in `docs/audits/campaign-reconciliation.tsv`.
The controller is inspecting that new ledger's schema and neighboring records;
the next integrated stage must include correct current-work classifications.
No validation was skipped, and clients were not reached by this stage.

## Task 5 — preparation fix round 1: cleanup ownership and failed evidence

Ruling: accept both Important findings from the preparation review. The
controller inspected `finish_process`, `measure` and `successful`: cleanup
only enumerates the original process group although the host creates nested
groups, and cleanup exceptions escape before a sample is assembled. An
interrupted zero-exit sample is unsuccessful yet loses its stdout. Require
owned-session cleanup before removal and a complete failed sample even when
cleanup fails; retain the owned directory if safe removal cannot be established.
Use behavioral fixtures matching separately grouped children and failed cleanup.
Do not begin real measurements before this fix passes independent review.
The alternative of assuming cooperative host cleanup is rejected by the
reviewer's live nested-group fixture. These are corrections within approved
quality scenarios, with no new policy or design choice; no new ideonomy pass
was run for this task review ruling.

## Stage 1 recovery — absorb the new documentary guard

The controller merged `origin/main` with `--no-commit`; Git reported a clean
merge and left the candidate uncommitted. Added three active reconciliation
records for the plan and two specs named by the canonical failure. No record claims shipment. Regeneration
and the ordinary commit hook will validate this composition before the next
stage request. The independent adopter branches retain their frozen bootstrap
base, so this controller absorption does not erase their independence evidence.

## Tasks 3–4 — independently reviewed adopters ready to compose

Thing at `275bc5d2b31374ec98dfa3bb8fd8936c8b96b5fd` passed both spec
and quality review without fixes. Census at
`11edfaabe918b168e74ac669fe9e851a18b9cbc3` passed both after its first
scoped fix. The latter now supplies eight required satisfied observations;
the two omission mutants are contradicted. Their frozen-host invocations
returned rc=0, and each branch changed only its package and Cargo.lock.
Controller will integrate the reviewed trees and reconcile the lock through
Cargo. Independent branch gates passed; composed and canonical qualification
remain separate obligations.

Absorption verification: `make rebaseline` returned rc=0, wall 251.625 s.
No Book, fixture or audit-content artifact changed. The generated write-set
inventory changed `docs/audits` written-count 15→16 and `docs/digest`
tracked-count 3→4, and its timing row is retained. The written-count
attribution is corrected in the later evidence regeneration note below. This is observed
freshness of this composition, not a prediction from the clean Git merge.

The first absorption commit hook failed the reconciliation coverage test:
no campaign records were missing, but the optional Charter ledger reference
was outside its four-directory audit population. The parser accepts a ledger
column while coverage flattens all five columns against a population excluding
ledgers. Removed that optional reference; the spec already links the ledger.
The failed gate (36.640 s) remains in timings. Follow-up: reconcile the Coda
ledger-column schema with its coverage population before relying on that
column; this campaign does not change the new guard's scope.

## Reviewed adopter integration

Controller applied Thing `275bc5d2b31374ec98dfa3bb8fd8936c8b96b5fd`,
census `b83fb084d28abacaf6b6d21692d004e7875ef1f9`, and its fix
`11edfaabe918b168e74ac669fe9e851a18b9cbc3` with ordinary no-commit
cherry-picks. Only Cargo.lock conflicted. Cargo resolved the combined manifests;
its full regeneration initially upgraded cached syn 3.0.3 to 3.0.4. The
controller detected this by comparing every external package's version, source
and checksum, and used Cargo's precise update to retain 3.0.3. The final
external records exactly match bootstrap. A first test pass on 3.0.4 is retained
as preliminary evidence; final scoped tests run on the retained resolution.
Both integrated package trees compare byte-identical to their reviewed heads.

Git confirms both branch merge bases and their mutual merge base equal
`6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f`. Each full branch diff is
Cargo.lock plus its four package files; there are no semantic host/protocol
edits. Five ordinary setup/gate timing rows from the two adopter worktrees
are retained here, alongside the controller's own gates. The final combined
lock adds 27 local packages, largely because the census guard lives in
`hornvale-lab`. No world construction is invoked by the contributor, but its
transitive compilation cost belongs in the cold measurement.

Composed verification before integration commit: all Digest packages passed
95 tests (52 host unit, 13 host CLI, 12 protocol, and 9 per adopter), with
clippy and fmt green. Explicit dependency preparation completed. Actual Thing,
census and combined context invocations each returned rc=0; reports were
4,078 / 7,756 / 11,738 bytes in this dirty integration checkout. The complete
tracked diff was byte-identical before/after those three invocations. These
are surface checks, not the planned cold/warm samples. Canonical artifacts
and client validation remain pending the composed stage.

Task 5's preparation fix at `9294b9ca765f83e82bd8ac1054053b0aff0163e0`
passed scoped independent spec and quality review; both R1 and R2 closed.
Eight behavioral harness self-tests passed on Mac, including separately grouped
children and retained failed evidence. Python 3.11 runtime qualification remains
for canonical execution; syntax compatibility alone is not that evidence.

## Composed stage and measured diagnostics

Integrated commit `e3355f441db045f0960576ead12b0f8acc56ca7d` passed its
normal commit gate (35.775 s), was pushed normally and submitted as
`req-e3355f441db0-20260905T041405Z`. Job
`sluice-e3355f441db0-20260905T041451Z` admitted it with zero commits
behind main and acquired the claim without waiting. Full result is pending.

A separate diagnostic worktree/branch at that commit adds only five lines
to `scripts/lane-outboard.sh`: after green ordinary outboard checks it runs
the reviewed harness self-tests, then measurements only if those pass.
Syntax and shellcheck passed; its normal hook passed 75 prose-subject tests.
Commit `9ea2dc9abb38970c03a5b536645d9d8429b3d37a` was pushed normally
and submitted stage-only as `req-9ea2dc9abb38-20260905T041951Z`. It is
never a merge candidate. Diagnostic-worktree prewarm cost 202.590 s; retain
that setup cost separately from the measured fresh Digest targets.

The Mac harness completed on the integrated source with rc=0: 36 command
samples (three host builds, three cold selected requests, thirty warm
requests), zero failures, three clean tracked-integrity results, and all
three owned disposable checkouts removed after completion. Controller
verified those assertions against the emitted JSON and filesystem. Canonical
measurements remain pending. The independent reader is evaluating actual
Thing/census reports and a real failed Thing report without the conversation.

## Independent usability — accepted with captured refinements

An unfamiliar reviewer read only the two real scoped reports and their paired
stderr plus the actual failed Thing report. They identified every authored
claim, eight census and three Thing observations, the unchecked queued-authoring
rule, the loom roster contradiction, and the relevant owner/source symbols
without opening code or the conversation. Verdict: PASS for usability,
explicitly not a validation of implementation correctness. Their suggested
summary-level wording and dirty-state attribution improvements are captured
in Follow-ups. Current explicit scope limits were sufficient for the reader
tasks; no new production semantics or provenance promise is introduced.

## Evidence-file ownership correction

Controller inspection of `docs/generated-paths.txt` found that its directory
author applies to every file without a more-specific exception. Charter's
evidence ledger, three report snapshots and Mac sample stream are captured
qualification records, not files the artifact generator recreates. Added
explicit `none(...)` rows under the existing longest-match policy rather
than leave inherited authorship false. The existing overriding-declaration
guard requires a real measured regeneration before accepting those rows.
This is artifact bookkeeping within the approved evidence scope; no gate
selection or publication authority changes. The same review also corrected
the evidence command-list order to match the actual controller invocation.

Evidence regeneration completed rc=0 in 137.310 s. The five explicit
evidence exceptions each measured `written=0, tracked=1`; the parent Digest
row measured two generated files out of eight tracked files. No Book,
fixture or audit-content artifact changed. The audit written-count returned
from 16 to 15: during the first absorption regeneration the controller edited
the reconciliation TSV after the run's marker, so that mtime-based counter
counted a controller write too. The script's counter observes writes during
the interval, not causal authorship. This later run had no concurrent edits
under generated stems. Preserve this limitation when interpreting capture
counts; the first count is not evidence that the generator owns that TSV.

Ruling: start the single broad source review on this code-complete, Mac-evidenced
checkpoint while the canonical jobs finish. The reviewer must mark remaining
Linux/stage evidence pending, not silently waive it; subsequent evidence or
fix deltas receive scoped review. This schedules independent reading earlier
without declaring Task 5 or final qualification complete.

## Stages 1–2 — composed canonical validation complete

Stage job `sluice-e3355f441db0-20260905T041451Z` finished rc=0 in
1209 s. Submitted implementation: `e3355f441db045f0960576ead12b0f8acc56ca7d`.
Initial merge product: `92a5cb2fac190eeb023662acc30786dd3b37b704`.
Final tested tree after ordinary generated/timing commits:
`32e985fc719e05cbe77a2a1990efd444903a8494`. All four phases passed:
artifacts 234.365 s, outboard 96.787 s, gate 732.827 s, clients 138.332 s.
The outboard phase includes all 95 composed Digest tests on Linux; the full
gate includes 5,355 workspace tests and doctests. The new campaign-record
check passed with the corrected active entries. Main stayed exactly
`3007f164ce6c5d94315b68f8abfd38ee7fca258d`; nothing was pushed by this
stage. This composed pass discharges the earlier bootstrap's documentary
stage failure as well as the joined-adopter stage obligation.

The diagnostic stage is now running in the same queue. Its self-tests, Linux
samples, final tested identity and completion remain separate pending evidence.
Task 5 and G6 are not complete.

A read-only canonical Git comparison of submitted source to final tested tree
found only four changed paths: `docs/generated-path-writes.tsv`,
`docs/timings.md`, `docs/timings/subfloor-roster.tsv`, and the lefford duration
baseline. There was no generated Book/fixture/audit-content or production-source
drift. This is the actual composed artifact result, beyond the earlier local
main-absorption regeneration.

## Broad review — source approved, remaining evidence still open

The single fresh whole-branch reviewer approved spec compliance and code
quality at `3560a9228d8646a11700455b491335362d9effff`, with no Critical
or Important findings. They independently checked source, branch/tree
identity, real mutation/recomposition proofs, raw Mac samples and aggregate
claims, legacy rendered bytes, ownership exceptions and completed Stage 2.
One Minor (inline-code locator escaping) is accepted and captured in Follow-ups;
it affects copied presentation, not observed outcomes or branch autonomy.
No source change is made for this nonblocking refinement. Their approval
remains a checkpoint: Linux samples/self-tests, completed diagnostic job and
the resulting evidence delta still require scoped independent review.

Linux diagnostic samples have now completed on
`406a09f6f00e08cccdd7c1698922a288f865d369`: all eight harness self-tests
passed (7.451 s, Python 3.11.2); all 36 samples passed; all three tracked
integrity records were clean. Read-only canonical inspection confirms all
three owned measurement checkouts are absent and unregistered. Its source
diff from the Mac implementation contains only the five-line diagnostic
invocation, generated-write inventory and timings. The full diagnostic
stage is still running, so its final tested-tree identity is not yet claimed.

### Final Linux evidence capture — concurrent authored write detected

The 137.628 s `make rebaseline` completed successfully, but a final evidence-document edit overlapped its mtime-based write interval. The capture therefore reported `docs/digest/` as 3 written / 9 tracked and the authored evidence document as 1 / 1. These are observed writes, not proof of generator authorship. Preserve the timing row and repeat the capture with all contributors idle and the evidence files frozen; do not hand-edit the generated counts or reset mtimes. This applies the existing capture decision rule, with no new design ruling or ideonomy pass.


## Linux qualification — diagnostic stage and quiet capture complete

Diagnostic job `sluice-9ea2dc9abb38-20260905T042052Z` finished rc=0 at
`2026-09-05T00:57:00-04:00`. Initial merge:
`61deae955b1248594bf7eaa711bf629c03a14b13`; measured source:
`406a09f6f00e08cccdd7c1698922a288f865d369`; final tested tree:
`de581c41b535d1f7817213a664b414a5c75660aa`. All four phases passed:
artifacts 227.946 s, outboard 218.040 s, gate 732.887 s, clients 137.715 s.
The gate includes 5,355 passing workspace tests plus passing doctests. All 44
Linux JSON records equal the records in the completed log. Read-only candidate
to final comparison shows only generated-write inventory, timing ledger,
subfloor roster and canonical duration baseline. Measured source to final
changes only the latter three. No production, Book, fixture or audit-content
drift. Main stayed `3007f164ce6c5d94315b68f8abfd38ee7fca258d`; no push
occurred. The diagnostic-only branch remains excluded from integration.

The repeated local `make rebaseline` completed rc=0 in 140.057 s with all
contributors idle and no edits under generated stems. Each of the six evidence
exceptions measured 0 written / 1 tracked, parent Digest 2 / 9; no Book,
fixture or audit-content artifact changed. Both this and the overlapping
137.628 s timing row are preserved. The capture is now fit for its bounded
inventory purpose. Source implementation remains exactly the reviewed/measured
`e3355f441db045f0960576ead12b0f8acc56ca7d`; later changes are evidence and
bookkeeping. Submit this fixed-head evidence delta to the same whole-branch
reviewer. Task 5 final qualification and G6 remain open until that review.


## Task 5 — complete after final scoped review; G6 ready

The same broad reviewer inspected the final evidence delta
`3560a9228d8646a11700455b491335362d9effff..9140e19211f08d0108d814c3b4124cf5ea1e602d`
and returned spec compliance PASS and quality PASS, with no new findings.
Their independent parsing matched all 44 Linux records to the completed job,
recomputed all new aggregates, checked the exact code/measurement/final-tree
identities, verified recorded lifecycle/cleanup and both canonical results,
and inspected the quiet ownership capture and 75/75 evidence commit hook.
The sole broad-review Minor remains captured and deferred. Their final status
correctly noted the controller's uncommitted follow-up additions; those add
profiling/dependency research pointers, not implementation or qualification
claims. No new design question or ideonomy pass was needed for this reviewed
completion ruling.

Mark Task 5 Steps 2–4 complete and remove the completed three-stage
`IMPLEMENTATION_PLAN.md` per the repository convention. Source remains
`e3355f441db045f0960576ead12b0f8acc56ca7d`; later committed evidence and
status bookkeeping are locally gated, not described as separately canonical-
tested code. The campaign reconciliation rows remain active and idea-registry
confidence remains design-approved until authorized close. No merge request,
main push, campaign close or worktree release is authorized by this verdict.

### Post-G3 digest for Nathan

- Executed the approved sequential bootstrap, then two independent adopter
  branches, followed by composition and measured qualification. Both package
  trees joined without a semantic host/protocol edit; Cargo reconciled the
  shared lock while retaining every existing external dependency version.
- Closed reviewed defects in contradiction handling, Git fixture isolation,
  discriminating census refusal probes, and diagnostic process cleanup/failed
  evidence retention. Each received scoped re-review and behavioral evidence.
- Absorbed main's new campaign-reconciliation guard, added active records and
  resolved the initial documentary stage failure with the green integrated
  stage. The optional ledger-column/coverage mismatch is captured for later.
- Qualified isolation, deterministic composition, recovery, resource caps,
  portability, extension and usability through the named checks and actual
  reports. Compiling authored declarations is never presented as proof of
  their truth; queue execution remains explicitly unchecked authored policy.
- Retained all 72 Mac/Linux command samples. They establish initial baselines;
  a steady-state SLO is not earned. Resource caps are unchanged. Canonical
  measurement ran through the existing serialized stage queue on a separate
  diagnostic-only branch, which is excluded from integration.
- Preserved evidence as authored records with explicit generator exceptions.
  Kept both overlapping and quiet regeneration costs and corrected capture
  attribution by rerunning without concurrent writes, never by editing counts.
- Accepted one nonblocking inline-code locator-escaping finding and captured
  presentation/provenance, dependency-size, status-phase profiling, review-
  package byte-capping and write-attribution follow-ups. Broader prompt/Book
  generation, formal guarantees and admission changes remain later campaigns.

No save-format, epoch, determinism-contract or fidelity tradeoff was introduced.
Nathan's G6 review is the remaining authorization boundary; approved close
must still follow the existing campaign-close and sluice workflow.


## G6 approved — close begins

Nathan approved close and merge: “Let's sluice this and then see if we can
think of some ways to close as many of those gaps as possible.” Complete the
existing close walk and queue merge before starting the next brainstorm. This
is authorization for the campaign's merge/close, not for implementing a wider
follow-up architecture. Preserve the pilot's uncertainty and the retrospective
lessons about independent oracles, physical versus semantic boundaries,
measurement ownership and generated-document meaning.

Close-time fetch found 32 newly landed commits through The Sluicegate at
`d17645ea5` (resolve full SHA in the close record). Read its chronicle and
retrospective, including the new request-ID/atomic-claim behavior and the
explicitly prepared queue binary. Absorption produced one conflict: append-only
campaign-reconciliation rows. Preserve both Charter's three active rows and
The Sluicegate's retrospective row. No production semantic conflict occurred.
Regenerate before asserting artifact freshness; a conflict-free source merge
is not evidence about generated bytes.

Absorption regeneration completed rc=0 in 147.623 s. No generated artifact
content changed; only this run's timing row was added after the merge. Main
tip is `d17645ea59cb2c30e53ed2691658b1546bfa7300`. Direct blob comparisons after
regeneration found all 72 tracked fixture files identical to that tip,
including seed-42 world blob `27017c3afa47c72b0e174709c35c90818642d8e9`.
The keystone refreeze therefore needs no byte change; there are no campaign
physics/golden-pin edits to defer. The canonical census is still owed at close
and will be queued on the committed absorbed candidate.


The absorption commit `fe0603cb2545f5dd170481b1f5200dd50265ff9f` passed
its ordinary gate in 43.995 s, with all three subfloor chunks green, then was
pushed normally. Its canonical census is durably queued as
`req-fe0603cb2545-20260905T133959Z` via `make sluice-census`, on that exact
immutable source. No golden movement is asserted before the result arrives.
Documentation close work proceeds locally while the queued run waits/executes.


### Census held for an operator scope question, not a test failure

`make sluice-status` reports the census request held and explicitly not run.
The operator measured zero changed paths under `kernel/`, `domains/`, or
`windows/` and proposed avoiding a roughly twenty-minute null run, asking for
confirmation before proceeding. Root repeated the path comparison and obtained
an empty result. That is evidence about changed paths, not a measured census
result. Current campaign cadence still requires the run, and this campaign
explicitly grants no new gate-omission authority. No census failure, zero-golden
result, or exception is claimed. The operator's request is an unresolved
coordination step; close artifacts can be prepared while it is resolved.
This instance also motivates the already-recorded future work on explicit
input closure and admission evidence; it does not implement that future rule.


Nathan explicitly authorized telling the operator to proceed with the held
census. Posted that confirmation as board notice
`7ebb9325ac38d82ff96aa4eb6cd4a82f2786b207`, naming the existing request
and exact SHA. No tool-only census exception is adopted. This is resolution
of the operator's coordination question, not a second approval of the campaign
or a change to its quality claims.


## Close documentation and survival audit

The close author read both the entire durable ledger and the campaign scratch
reports/reviews/progress/mutation evidence across the primary and two adopter
worktrees. The fixed inventory contains 1,474 files, including 1,251 reproducible
compiler cache/generated metadata entries; these are not 1,474 prose findings.
Forty-six substantive routes identify durable homes below. No new unledgered
ruling was found. Seven measured deferred ideas now have registry homes; the
bounded contribution envelope is prepared as shipped, while the wider program
and its unimplemented authority remain open. The author inspected links,
anchors and row structures, froze all tracked writes, and ran no gate or
regeneration. Root owns final qualification and asks the close reviewer which
finding did not survive the mapping.

Project memory was inspected, including the prior Tackle instrument-independence
lesson. All Charter process lessons have repository homes in the retrospective
and registry; no additional private-memory duplicate is needed and no relevant
memory rule was superseded by this pilot. The standalone self-writing-Book
memory describes a different, historical language program, not this developer
context contribution surface.

The census is now running as
`census-fe0603cb2545-20260905T135333Z`; results remain pending.

### Durable routing map at close preparation

Line locators refer to this prepared close snapshot; the named files and
subjects are the enduring homes. Raw compiler caches and redundant logs remain
reproducible/scratch; samples and actual report snapshots are committed.

| Finding or evidence category | Exact permanent home | Disposition |
|---|---|---|
| Ledger #1–8: bounded context pilot versus broader active program | `docs/superpowers/specs/2026-09-04-the-charter-design.md:353` | Material post-G3 rulings promoted; original G3 choices remain §10. |
| No kernel/ProjectLedger schema, world or save behavior migration | `book/src/chronicle/the-charter.md:88` | Bounded product boundary; historical Digest receives a continuation link. |
| Task1 optional explicit contradiction bug and fix | `docs/superpowers/specs/2026-09-04-the-charter-design.md:365` | Resolved: every explicit contradiction fails; optional unknown remains informational. |
| Task1 compiler-stub RED versus behavioral omission/rendering RED | `docs/retrospectives/the-charter.md:10` | Compilation is structural; behavior is independently exercised. Detailed original runs stay in durable ledger. |
| Task1 permitted preexisting rustfmt-only mcp blocks | `docs/superpowers/ledgers/2026-09-04-the-charter.md:369` | Documented mechanical allowance; no new lesson or unresolved change. |
| Task2 null Cargo metadata and unenrolled package behavior | `docs/digest/the-charter-evidence.md:253` | Final qualification and production fixture coverage; intermediate report superseded by reviewed bootstrap. |
| Task2 Git fixture setup/snapshot path leak with outer index proof | `docs/retrospectives/the-charter.md:31` | Resolved with discriminating RED/GREEN; same boundary promoted in spec §11. |
| Task2 toolchain pin versus outside-repo default; Cargo manifest not cwd | `docs/superpowers/specs/2026-09-04-the-charter-design.md:372` | Pin retained in fixtures and measurements; preflight protocol promoted. |
| Task2 vocabulary substring overmatch workaround | `book/src/frontier/idea-registry.md:2064` | Existing measured row reused, not minted again. Host-local rename fixed hook refusal without waiver. |
| Task2 suspected compiler diagnostic loss | `docs/superpowers/ledgers/2026-09-04-the-charter.md:1062` | Reviewer explicitly refuted suspicion; not an open finding. Complete stderr survives in recorded fixture evidence. |
| Task3 actual public ownership APIs, reverse rosters and frozen-host acceptance | `docs/digest/the-charter-evidence.md:11` | Exact branch, package rosters, 22 unchanged external and 27 local dependency records retained. |
| Task4 non-discriminating canonical-host panel and two omission mutants | `docs/retrospectives/the-charter.md:14` | Resolved; pure predicate scope remains narrower than publication invocation. |
| Task5 disjoint overlap and no sampling before reviewed integration | `docs/superpowers/specs/2026-09-04-the-charter-design.md:379` | Accepted execution sequencing, promoted without a new global rule. |
| Task5 nested process groups survived abnormal parent exit | `docs/retrospectives/the-charter.md:31` | Resolved actual-session topology fixture, not detached hostile sessions. |
| Task5 cleanup sample loss, interrupted-zero stdout and emission-before-abort | `docs/superpowers/specs/2026-09-04-the-charter-design.md:375` | Resolved; preserved failed fixture/intermediate test-scope failure, no selective evidence deletion. |
| Task5 fixture mock scope restored cleanup flag too early | `docs/retrospectives/the-charter.md:34` | Corrected test itself; retained failed intermediate run in scratch and durable ledger/evidence method. |
| Real public compose: both envelope orders, repetition, inner ordering | `docs/digest/the-charter-evidence.md:177` | Exact 11738-byte equality and unchanged observation details retained. |
| Real loom mutation compiles, contradicts and remains red after authored assertion rewrite | `docs/digest/the-charter-evidence.md:177` | Actual commands/results and snapshots durable; not a fictional fixture or compile failure. |
| Legacy renderer compatibility and tracked primary byte preservation | `docs/digest/the-charter-evidence.md:241` | Only actually exercised doctor/decisions/delta and tracked comparison claimed. |
| Functional driver and Rust fixture Cargo scaffolding | `docs/digest/the-charter-evidence.md:177` | Reusable explanation/commands/results retained, ephemeral build cache not promoted. |
| Actual successful Thing/census and failed Thing report bytes | `docs/digest/the-charter-evidence.md:575` | Exact snapshots in docs/digest; original historical revision/dirty headers retained. |
| Independent usability PASS and reader scope limits | `docs/retrospectives/the-charter.md:43` | Not source review; findings routed separately below. |
| All 72 samples, RSS/load/units, failure and missing-subphase attribution | `docs/digest/the-charter-evidence.md:281` | All raw JSONL retained, not only aggregate tables; no SLO or cap change. |
| Ordinary integrated Stage2 green | `docs/digest/the-charter-evidence.md:109` | Exact submitted/initial/final identities and 5355+doctests/95Digest retained. |
| Diagnostic stage-only transport, measured versus final tree and full result | `docs/digest/the-charter-evidence.md:358` | Separate identity chain and diagnostic exclusion retained; no ordinary outboard change merged. |
| Full source review and final evidence delta PASS | `docs/digest/the-charter-evidence.md:542` | Earlier pending reports are historical; final reviewed boundary explicitly supersedes pending qualification. |
| Deferred context-presentation | `book/src/frontier/idea-registry.md:2131` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Deferred context-provenance | `book/src/frontier/idea-registry.md:2132` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Deferred context-status-cost | `book/src/frontier/idea-registry.md:2133` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Deferred contributor-build-boundary | `book/src/frontier/idea-registry.md:2134` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Deferred review-byte-cap | `book/src/frontier/idea-registry.md:2135` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Deferred generated-write-attribution | `book/src/frontier/idea-registry.md:2136` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Deferred reconciliation-ledger-coverage | `book/src/frontier/idea-registry.md:2137` | New raw measured row, exact limits and permanent evidence/retro pointer; not merely ledger-only. |
| Three independence meanings and lab compilation cost | `docs/retrospectives/the-charter.md:22` | File/build/semantic distinction and source-identity discipline; no restructuring undertaken. |
| Prior Tackle instrument-independence/control-scope precedent | `docs/retrospectives/the-charter.md:13` | Cross-reference established precedent; no claim the lesson was invented here. |
| NFRs as scenarios rather than quality adjectives | `docs/retrospectives/the-charter.md:40` | Bounded quality qualification, broad quality system still raw/open in registry. |
| Two contaminated mtime capture intervals and honest reruns | `docs/retrospectives/the-charter.md:57` | Preserve observed costs; causal-attribution gap remains raw; freeze writers before root regeneration. |
| Coda ledger parser/coverage mismatch and documentary stage refusal | `book/src/frontier/idea-registry.md:2137` | Captured independently of ledger; prepared close retains ten columns without optional ledger links. |
| Operator held census on zero changed simulation paths | `docs/retrospectives/the-charter.md:60` | Explicit user authorization resolved coordination; actual result still pending, no cadence exception or null claimed. |
| G6 approval, absorbed main and close responsibilities | `docs/superpowers/ledgers/2026-09-04-the-charter.md:882` | Root owns final census, keystone and merge notes; no edit to its ledger by this agent. |
| Broader federation direction | `book/src/frontier/idea-registry.md:2124` | Retains raw status and active metaplan; bounded pilot achievement named separately. |
| Knowledge contribution envelope | `book/src/frontier/idea-registry.md:2125` | Shipped bounded v1 with qualified evidence pointer; richer semantics remain open. |
| Obligation selection and adoption/activation | `book/src/frontier/idea-registry.md:2126` | Both existing IDs preserved raw and explicitly unimplemented; no gate omission authority. |
| Global and composed quality authority | `book/src/frontier/idea-registry.md:2129` | Pilot scenarios qualified; general system and global budgets remain raw/open. |
| Book freshness | `book/src/chronicle/the-digest.md:228` | Actual search found only historical/context mentions; added bounded continuation, no world chapter edits. |
| Confidence Gradient existing bets | `docs/retrospectives/the-charter.md:68` | Searched headings and Digest/project/development/self-description references. Existing world bets unchanged; no score or new pseudo-bet authored. |



Close-documentation regeneration completed rc=0 in 143.862 s with all authored
writers frozen. The actual unstaged diff after it contains only the run's
`docs/timings.md` row: no generated Book, fixture, audit-content, Digest or
write-inventory byte changed. This is an observed result, not a prediction
from the registry edits. Close documentation now goes through the normal
commit hook and independent close/survival review; the running canonical census
and final merge remain separate obligations.


Close documentation's first commit attempt was refused by the normal prose
suite: 71 passed, one failed, three not run after cancellation. The chronicle
used “worktrees,” which the existing five-term world-prose guard rejects even
in a development-tool chronicle. Root read the actual guard and changed that
sentence to “on separate branches,” preserving the verified independence
claim; checkout mechanics remain in the evidence/retrospective. No guard was
weakened. The failure log is retained and the normal hook must pass on retry.


## Canonical census complete — measured null retained

`census-fe0603cb2545-20260905T135333Z` completed rc=0 on exact absorbed
source `fe0603cb2545f5dd170481b1f5200dd50265ff9f`: **NO GOLDENS MOVED**.
The timed body cost 1070.850 s (29612.125 user, 266.011 sys, CPU ratio 27.90);
the wrapper records 1074 s and the queue 1084 s. These measure different boundaries.
The returned branch `census/fe0603cb2545-20260905T141127Z` at
`6636e56a06773a1cc40d7e561c261557691c484e` has exactly one added
`docs/timings.md` row. Root read the completed log, fetched the branch and
compared the entire commit before joining it with an ordinary non-committing
merge; it merged cleanly. No moved goldens, new epoch, re-pin, or census-source
change is being hidden in close bookkeeping. Main is still
`d17645ea59cb2c30e53ed2691658b1546bfa7300`.

The close-documentation retry passed 75/75 prose tests and committed at
`c0c565efe13b25823d0cfdefd57247ca48d0a1ca`. Final close/survival review is
reading that exact checkpoint; the measured census result and resulting
status updates form its small subsequent completion delta. The local prose
hook incident is also routed to the retrospective, beyond this ledger.


### Task 2 diagnostic hypothesis — permanent refutation record

The original Task 2 independent review exercised a focused disposable real-
Cargo fixture containing broken Rust, specifically to test diagnostic retention
through `--message-format=json-render-diagnostics` and the host's stderr path.
Its recorded result was exit 1, zero stdout bytes, and
`error[E0308]: mismatched types` with the source location and expected/actual
types on stderr. This refuted the suspicion that compiler diagnostics were
lost; it was explicitly not an open finding. This paragraph preserves that
historical review observation, not a fresh compiler run at close.

Close review Minor C1 found that the routing table pointed to an unrelated
Task 1 timing paragraph. Corrected the locator to this explicit Task 2 record.
The ruling already survived in the table's disposition; the correction makes
its evidence retrievable. No code or behavior change was needed.


### Final close review acceptance

The independent close reviewer passed compliance and quality for HEAD
`c0c565efe13b25823d0cfdefd57247ca48d0a1ca` plus the frozen completion
patch (SHA-256 `85be1c89879140ae2afceecad56ee5a60900c83609a0ffdd443067f79ce63c88`).
The census receipt and admission wording were checked against the actual log
and returned timing-only commit. C1 is corrected and independently verified;
no substantive finding was lost in routing. Earlier implementation Minor M1
remains accepted and deferred to `PROC-context-presentation`. This acceptance
does not claim the later regeneration, commit gate, or canonical merge/heavy
result; those are observed at admission.


### Final admission gate — census profiling obligation discovered

The quiet final regeneration completed rc 0 in 202.515 s, with no generated
content drift and one retained timing row. The subsequent explicit commit
gate failed rc 2 in 34.499 s on
`census_duration::a_census_over_the_alarm_threshold_owes_a_profiling_followup`.
The canonical census's 1070.850 s exceeds the existing 1000 s alarm and
requires a run-specific profiling finding. No finding for its timestamp exists
in `docs/timings/census-yellow-log.tsv`. The null golden result remains valid;
performance attribution is a separate outstanding close obligation.

The existing census log contains no perf capture. Earlier Weft profiling is
not a fresh profile of this run, and source changes exist between that profiled
confirmation and this census source. No placeholder acknowledgement, threshold
change or test bypass was made. A concrete bounded profiling request at
`fe0603cb2545f5dd170481b1f5200dd50265ff9f` is prepared for the canonical
operator under ordinary serialization. Sending this new message awaits Nathan's
explicit messaging authorization; the prior permission covered the census
proceed confirmation. The final census merge remains uncommitted and the
campaign has not been submitted for merge admission.


### Nathan's profiling disposition and renewed admission direction

Nathan replied: “Yes, I'm looking into the profiling issue. Go ahead and
sluice again.” This supersedes the pending operator-message prerequisite:
Nathan owns the investigation and explicitly directs admission to continue.
The run-specific yellow-log entry records **NOT PROFILED**, the original
source, timestamp and measured cost, and the authorized deferral. It does
not assert attribution or reuse earlier profiles as this run's evidence.
The normal gate and queue remain unchanged; no profiling message is needed
while Nathan is handling the issue. The performance investigation remains
open even if normal admission succeeds.


### Admission regeneration retry

The first regeneration after Nathan's disposition stalled before application
startup: a one-second macOS sample of its `hornvale scene eclipses` process
showed every sample in `_dyld_start`, with only 96 KiB physical footprint.
The controller terminated that invocation; the failed run's 320.676 s cost
remains recorded. One ordinary retry progressed and passed rc 0 in 201.862 s.
The completed working-tree diff contained only the two timing rows, with no
generated-content change. This is evidence of a startup stall and a successful
retry, not a diagnosis of the loader's underlying cause.
