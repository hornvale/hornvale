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
inventory changed two directory population counts (`docs/audits` 15→16,
`docs/digest` 3→4 total files), and its timing row is retained. This is observed
freshness of this composition, not a prediction from the clean Git merge.

The first absorption commit hook failed the reconciliation coverage test:
no campaign records were missing, but the optional Charter ledger reference
was outside its four-directory audit population. The parser accepts a ledger
column while coverage flattens all five columns against a population excluding
ledgers. Removed that optional reference; the spec already links the ledger.
The failed gate (36.640 s) remains in timings. Follow-up: reconcile the Coda
ledger-column schema with its coverage population before relying on that
column; this campaign does not change the new guard's scope.
