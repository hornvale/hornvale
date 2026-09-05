# The Charter — decision ledger

Campaign: `codex/the-charter` · Base:
`fd3cd159be98e6c07dbb849b780c2712c6a7826f` · Phase: architectural brainstorm.
Nathan authorized the brainstorm on 2026-09-04 after discussing the
self-describing program, federation, and local evolutionary ratchets.
Implementation planning awaits the G3 spec review.

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
