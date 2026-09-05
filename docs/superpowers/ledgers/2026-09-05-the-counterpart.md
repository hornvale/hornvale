# The Counterpart — brainstorming decision ledger

Status: exploration captured on 2026-09-05; G3 review is pending.
Branch: `codex/federation-next`.
Research base: `4f303d3b06d8403f79324755c6adfac9d0394a3d`.
No implementation plan or implementation is authorized by this document.

## Current-state evidence

- `git fetch origin` completed, and `git rev-parse origin/main` returned the
  research base above. The shared checkout remained on `main` at
  `d17645ea5`, clean. This campaign created its own worktree from origin/main.
- `make doctor` and `make board` ran. Peer refs were fetched with
  `git fetch origin '+refs/hornvale/hosts/*:refs/hornvale/peers/*'`;
  lefford's mirrored tip moved from `bfe9e27da` to `369d56956`.
  No board posts or messages to other campaign owners were sent.
- `make sluice-status` reported Charter merge
  `req-aa2de77fbcaa-20260905T161542Z` landed at `4f303d3b0`,
  all merge phases rc=0 in 1653 seconds. The Warp stage row remained held.
  The refreshed board explicitly reports The Warp unblocked by the
  hand-authorship correction; a held historical row is not a new blocker.
- Correction `c610c9363fd9530c68627576d240e0e44ba68b74` is already in
  the research base. It adds the hand-authored override for
  `docs/audits/campaign-reconciliation.tsv` plus measured inventory and
  a timing row. The earlier board warning is history, not work to duplicate.
  It is distinct from the open reconciliation-ledger population mismatch.
- The queue also records `tooling/census-study-split` at
  `0f0865380ecc9e77e77496df9ca9e0597c401f0e`: a reported census,
  rc=0 in 999 seconds, no goldens moved. Its source commit adds separate study
  timing. This is subsequent instrumentation/evidence, not a demonstrated
  explanation or discharge of Charter's profiling alarm.
- Charter's yellow-log entry still says **NOT PROFILED** and records Nathan's
  ownership and authorized deferral. The audit found no newer attribution in
  available Git history. No census or profile was run for this brainstorm.
- Actual command, from this worktree using the retained Charter host:
  `/Users/nathan/.config/superpowers/worktrees/hornvale/the-charter/tools/digest/target/debug/digest context domains/settlement`.
  Exit 1: `context: discovery: no contributors match scope "domains/settlement"`.
  The host's Git/metadata phases ran; no selected contributor build occurred.
  This is evidence about the existing scope selector, not a latency benchmark.
  The Thing contributor nevertheless calls real Settlement registration
  (`tools/digest/packages/thing/src/lib.rs`, `collect`) and declares
  only `domains/thing` in its manifest.

## Evidence audit retained

Three independent read-only research agents inspected implementation, Charter
qualification, and competing designs. All echoed this worktree/branch and made
no edits. Their findings were checked against the cited source:

- [Charter evidence](../../digest/the-charter-evidence.md): two independent
  adopter packages; 22 external dependency records preserved, 27 local ones
  added; 95 Digest tests on each host; 72 command samples, not 72 independent
  workload studies. Each host has three workloads with a host build, one
  selected cold request and ten warm requests each.
- Selected cold census-context requests: 11.552 s Mac / 25.712 s Linux;
  these are observer builds/collections, not censuses. Ten warm observations
  per workload/host establish baselines, no SLO.
- [Charter ledger](2026-09-04-the-charter.md), implementation review:
  positive canonical-host cases did not discriminate missing census recognition.
  Separate wrong-host omission mutants did. Compilation and an agreeing
  declaration did not supply that missing evidence.
- Static challenge, **not an observed defect**: the census adapter builds
  protected-output cases from `CENSUS_GOLDENS_DIR`, which the guard itself
  uses. A wrong shared suffix could move both sides of the comparison.
  A future challenger should independently anchor the actual publication
  destination. We have not executed such a mutation.
- Rejected audit suspicion: a simple removal of a census observation is
  constrained by a separate instruction-reference roster and the count test.
  Do not report it as an established silent missing-check vulnerability.

## Periodic grid of federation boundaries

Ideonomy draw: tree-finding + cross-domain re-instantiation; periodic-grid;
complexity, naturalness, decomposability.

The parent tree is cooperation → separately owned changes → boundary
agreements → individual observations. Siblings of a contract are isolation,
coordination, and post-composition inspection. Moving down the tree replaces
“federate the project” with “which assumption can these two owners violate?”

A rehearsal provides the cross-domain translation: separate performers can
prepare their parts, while cues and the combined performance need a shared
reference. This is an analogy used to generate options, not external evidence.

```
Boundary       Declaration              Independent counter-observation
Files          Owned paths              Actual edit intersection
Build          Package imports          Changed compilation closure/cost
Meaning        Consumer obligation      Same obligation on combined behavior
Resources      Local execution cap      Other invocation survives cancellation
Time           Current source claim     Reconstruct result from retained inputs
```

Every cell is populated by an existing observation or a proposed experiment;
the missing achieved capability is semantic composition under independently
changed assumptions. Fully decomposable work needs little coordination;
irreducible shared meaning remains a sequential question. A universal schema
would increase complexity without resolving that distinction.

## Dictionary and timeline of evidence

Ideonomy draw: organon-construction + abstraction-lift + substitution;
dictionary and timeline; longevity, intentionality, direction, scope, cyclicity.

- **Enrollment:** a host discovers a new contributor.
- **Dependency:** a change alone can affect a consumer.
- **Interaction:** neither change alone violates the frozen obligation, but
  their combination does.
- **Repeatability:** another run reproduces a stated result on retained inputs.
- **Corroboration:** a differently grounded method challenges the same claim.
- **Authority:** an accepted rule permits an action; a passing report is not one.
- **Retirement:** an obligation is explicitly discharged or transferred,
  rather than lost with its old mechanism.

These definitions break the circular equivalence between “the report is fresh”
and “its conclusion is justified.” Abstractly the problem is preservation of a
promise across independently varying causes.

Past: Charter qualified a live read. Present: preserve a bounded experiment.
Possible future: prove input invalidation before reusing evidence. Substituting
an occasional inert dossier for a continuously maintained provenance service
removes a premature prerequisite. Substituting accidental interference for a
deliberately bad declaration requires real behavior to be observed. Expanding
the scope from one agreement to the whole repository changes the claim,
not merely the sample size. Future retirement must revoke the applicable
privilege rather than let evidence accumulate permanent authority.

## Scale of evidence surviving its observer

Independent Ideonomy draw: negation + combination; scale;
visibility, animacy, age.

```
0  Hidden assumption
1  Live scoped observation                 Charter
2  Frozen experiment another owner repeats Proposed next bounded step
3  Independent challenge survives counterexamples
4  Real owners migrate and retire an agreement
5  Named, revocable admission privilege
```

This scale is ordinal, not a probability or mandatory universal sequence.
Negating self-description gives a consumer challenge; negating a permanently
resident observer gives an inert replayable record; negating endless adoption
gives retirement. Crossing owner declarations and independent challenges with
build/meaning/time reproduces the three strongest alternatives.

The disruptive correction: A-alone consumer failure is not an A+B interaction.
A safe composition and a withheld independent challenge are necessary to avoid
a tool that rejects everything or recognizes only its own designed example.

## Map of the next experiment

Ideonomy draw: dimension-identification; map; materiality, connectivity.

```
Concrete input bytes ---- build closure ---- declared effects ---- semantic judgment
       |                      |                  |                    |
  bounded dossier       measurable cost    negative dependencies   admission
       +---------------- one real agreement -------------------------+
```

Distances express increasing distance from directly retained bytes.
Candidate axes: retained/live inputs, sparse/dense dependencies,
local/global resources, observed/declared effects, advisory/authoritative
results, independent/correlated oracle. More edges increase possible
interference, but a graph of declared edges cannot establish absent edges.
This rules out claiming complete provenance from a handpicked file list.

## Graph of the minimum experiment

Final Ideonomy draw: substitution; graph; distribution, size.

- One real agreement **requires** a frozen question.
- Two local authors **propose** independent edits and their effects.
- A separate challenger **tests** shared wrong assumptions.
- Retained inputs **enable** another observer's replay.
- Exact composition **exposes** interaction.
- Adjudication **limits** the claim and **routes** the next obligation.

Concentrating all nodes in one author destroys the oracle comparison.
Distributing them across a fleet adds coordination without increasing the
bounded claim. Scaling down to one real agreement retains the useful graph;
scaling up to every package introduces provenance, scheduler and migration
projects. This pass added no material option, overturned assumption, or
spec-worthy implication beyond the preceding corrections: G1 convergence.

## Follow-ups and capture routing

All speculative directions use existing registry homes after a PROC/TOOL/UNI
category scan; no new registry ID is needed.

- Comparative interference and conservative obligation routing:
  `PROC-federated-evolution`, `PROC-evidence-selects-obligations`.
- Independent challenger and shared question roster:
  `PROC-project-epistemology`, `TOOL-duplicated-rulebook-audit`.
- Bounded replay and longer-lived input evidence:
  `PROC-context-provenance`, `PROC-evidence-dies-with-the-scratch`.
- Build islands: `PROC-contributor-build-boundary`.
- Actual compatible migration, legacy adoption and retirement:
  `PROC-local-adoption-and-activation`, `PROC-suite-life-cycle`.
- Small formal state model: `PROC-13`, `PROC-federated-evolution`.
- Persistent prompts/Book/idea lifecycle: `PROC-codification`,
  `PROC-11`, `UNI-28`, `UNI-29`, `TOOL-registry-query`.
- Concurrent resource isolation and end-to-end budgets:
  `PROC-quality-contracts`; no new scheduler is presumed.
- The seven Charter follow-ups retain their existing rows and open status.
  This campaign does not absorb them as miscellaneous cleanup.
- The Rhumb prose consumer counterexample stays under
  `PROC-prose-consumers-cross-the-workspace-boundary`; it is context for
  later external-consumer work, not a required second pilot here.
- Census performance remains Nathan's investigation. The study-split result
  is a coordination fact, not permission to duplicate it.

Rejected as prerequisites: universal ontology, VCS migration, dynamic plugin
framework, whole-repository provenance capture, automated gate narrowing,
and an invented production schema break just to exercise migration.
Each either expands scope before a demonstrated need or assumes the safety
property the experiment must interrogate.

## #1 [G1] — next campaign

**Question:** Which next step most directly tests safe concurrent work?
**Decision:** Recommend **The Counterpart**, a bounded interference experiment
on the real Thing–Settlement concept-registration boundary. Compare path-based
selection, Cargo dependencies and one explicit consumer agreement in shadow;
run the same frozen obligations on base, A, B and the composed source. Include
a safe pair, a single-change dependency failure, a joint-only interaction and
an independent challenge. Retain a minimal replay dossier. No production
selection rule, migration, or admission privilege is enacted by the experiment.

**Why:** Charter already proves enrollment. Source and the live Settlement
scope probe expose the next uncertainty. Decisions 0094 and 0261 authorize
a shared question roster with independent derivations; 0139 preserves exact
composed-source admission. The metaplan itself calls its sequence questions,
not approved plans. Testing an agreement before building more federation
infrastructure is a justified refinement of its order.

**Alternatives discarded as the next campaign:** build extraction has a
measured cost but does not establish meaning; a general provenance platform
is larger than this experiment needs; version migration lacks a presently
needed incompatible change; prompt/Book automation already has a qualified
on-demand predecessor and does not resolve interference; a formal migration
model has no selected live migration to correspond to; a global scheduler
would answer a different resource question. All remain captured above.

**Ideonomy passes / overturns:** five draws, four by the controller and one
independent challenger, all recorded as organons above. The direction shifted
from strengthening general infrastructure to testing one behavioral boundary
first. The independent pass required distinguishing dependency from joint-only
interaction and retaining a safe control; the final size/distribution pass
found no material improvement. No claim of exhaustive search follows.

**Capture actions:** existing federation, evidence-selection and provenance
rows point here. The spec will carry comparison criteria and response rules;
the seven Charter follow-ups and Nathan's profiling ownership stay open.

## #2 [G2] — design consistency and independent review

**Question:** Does the bounded design test its own claim fairly and preserve
the distinction between valid evidence, correct behavior and new authority?
**Decision:** Adopt the [G3 draft](../specs/2026-09-05-the-counterpart-design.md)
with the following review corrections before presenting it:

- A satisfied check is not shown unnecessary. Report selected/satisfied,
  selected/violating, unselected/violating and unknown separately.
- Compare question IDs through one frozen map to contributors, Cargo packages
  and input subjects. Unknown and no-enrollment fall back to the whole roster;
  include fallback cost. The path comparator is not scored as unsafe merely
  because it honestly declined to answer a Settlement request.
- Freeze the challenger implementation as well as owner records and comparison
  rules before revealing the reserved case. A correction gets a new identity
  and cannot erase the original miss.
- Existing ownership refusal, independent corroboration and earlier selection
  are distinct outcomes. A valid negative specimen is not a safe subject, and
  an uncontrolled harness failure is not behavioral discrimination.

**Why:** two independent spec reviews and the controller's own scoring review
found these ambiguities. Source already contains the collision guard.
The actual existing test command
`cargo test -p hornvale-thing --lib undeclared_collision_panics_instead_of_silently_ceding`
returned rc=0, `1 passed; 0 failed; 10 filtered out`. That confirms its existing
fixture, not the new A/B experiment. The reviewer independently confirmed the
corrected fallback/passing-check language and found no further substantiated
oracle-independence defect. The additional mapping/freeze/refusal corrections
were checked directly in the final draft.

**Alternatives discarded:** scoring unknown as an unsafe omission; treating
every passing selected check as waste; claiming a novel detector from an
existing production guard; changing the checker after seeing the challenge
without preserving the original outcome.

**Ideonomy passes / overturns:** one additional G2 draw, organon-construction
with lattice; hierarchicalness and symmetry. Order selected question sets by
inclusion: empty is bottom, the frozen roster is top, intersection/union are
meet/join. Two proper subsets may be incomparable. Unknown is an epistemic
state outside that lattice, mapped to the top by fallback, not to empty.
Owner A and B have symmetric scoring obligations even though production
registration has a fixed Settlement-then-Thing order. This adds no further
material design change after the corrections; no new capability is inferred
from a smaller set. Total recorded brainstorming draws: six.

**Capture actions:** final spec carries the response rules and G3 flags;
the registry points its three existing program rows to the bounded proposal
without claiming their broader work shipped. The hand-authored reconciliation
table gains an active spec row, with its optional ledger cell empty under the
existing population rule. This does not fix the Charter follow-up.

**Verification boundary:** local spec links and cited fragments resolve;
placeholder scan is clear. The capture and G1 commits each passed all 75
prose-subject hook tests. `make prewarm` completed rc=0 in 623.679 seconds;
its automatically authored timing row is retained, not an experiment sample.
A final fetch still reports the same origin/main research base. All writing
is in this worktree; no implementation source or implementation plan was added.
The final spec commit still receives the ordinary prose-subject hook.

## G3 — Nathan approved proceeding, 2026-09-05

After the plain-language explanation of benefits, direction, and the
possibility that the experiment earns knowledge without immediate throughput
gains, Nathan said: “Great! Let's move forward.” This approves the presented
spec and releases implementation planning. The four G3 qualifications remain
in force. G4 is self-reviewed under campaign-autopilot; G6 remains the next
human stop before merge/close.

## #3 [Q] — reusable execution machinery and experiment identities

**Ruling:** extract Charter's existing Python diagnostic into an importable
module, preserving its wrapper and behavioral tests. Add opt-in retained raw
output and a bound for Counterpart. Keep a tiny Rust raw-observation executable
outboard; the independently authored checker consumes its facts. Preserve
specimen source as Git objects in a bounded bundle, with an explicit committed
base prerequisite. These objects are experiment data, never admission candidates.

**Why:** the inspected Charter `measure` already owns process sessions,
interruption, inherited Git/Cargo scrubbing, timings and unsafe-cleanup retention.
It currently deletes successful stdout after hashing, so reuse needs an explicit
output-retention extension. The existing Rust supervisor's failure result loses
structured stdout. Charter's ledger (Composed stage and measured diagnostics)
provides a stage-only outboard invocation precedent for canonical experiments.

**Cycle of an observation (tree-finding; discovery versus invention; rate):**

- Question -> source -> preparation -> observation -> judgment -> revised question.
- Up: an observation belongs to an experiment, then to an admission argument;
  this experiment has no admission authority.
- Down: preparation, process ownership, raw facts, independent interpretation
  and presentation are different children. Their costs run at different rates;
  compilation must not be charged as interpretation.
- Across: copy a supervisor / import a reviewed one / invoke a long-lived
  service. Import is the bounded sibling; a service invents a new lifetime.
- Between levels: preserve committed source objects rather than invent a
  general provenance graph. Replay closes this cycle only back to the frozen
  question, never to a universal safety claim.
- The fixture is invented; the observed interaction is discovered in that
  fixture. Increasing trial rate does not turn synthetic faults into measured
  natural fault frequency.

**Alternatives discarded:** cloning the supervisor; extending live Digest
protocol v1; a new service; committing intentionally broken simulation changes
on the campaign branch; asserting before/after hashes prove atomic reads.
**Ideonomy passes / overturns:** one actual picker draw, recorded above; no
material improvement beyond the retained-output extension and existing separate
preparation/observation timing. **Cost if wrong:** rework of a development-only
runner and replay records. **Capture:** implementation plan tasks and this ledger;
no new general mechanism or registry row.

## #4 [G4] — implementation plan self-review

**Decision:** execute the three-stage, seven-task plan with independent
question interpretation, two owner authors and a later independent replay.
**Why:** each spec requirement maps to a task in the plan's self-review map.
Independent source authorship follows the approved spec and Charter precedent;
shared question inputs and Git-object data are explicitly disclosed.
**Alternatives discarded:** a generalized federation service, a new supervisor,
unqualified synthetic mutations and production admission of specimen changes.
**Review corrections:** keep checker input separated from candidate output;
require the dossier to contain the panel's full arm list rather than merely
validate emitted rows; retain prerequisite source objects for replay; preserve
the original frozen checker when the reserved challenge is revealed.
**Ideonomy:** architecture Q used one actual draw in #3; G4 is a consistency
review and makes no additional nontrivial Q ruling.
**Capture:** detailed plan, stage tracker and reconciliation row. The optional
reconciliation ledger column remains empty under the existing population rule.
**Preflight:** Tasks 1/2 share facts/question IDs; Tasks 3/4 share only base and
named collision input; Task 5 consumes all records and freezes rules; Task 6
uses that immutable freeze; Task 7 uses committed artifacts alone. All shared
function signatures are stated once in the plan and assigned to producing tasks.

### Task 1 preflight correction — verified APIs and brief context

**Ruling:** use `ConceptRegistry::default()` and
`digest_thing::contribution() -> Result<Contribution, String>`. The plan
named nonexistent `new`/`collect` entry points; the implementer checked the
source and corrected them before implementation. This is a factual plan repair,
not an architectural Q. Cost if wrong: compilation or observation failure,
covered by focused real-API execution. The G4 claim that all signatures had
been read was too broad; these two had not been checked precisely enough.

The task-brief extractor retains only the Task heading/body, omitting the
shared file map. The controller now supplies that map with the shared schema
and constraints to every implementer. Experiment paths retain `the-counterpart`,
while the Rust package path is `packages/counterpart`.

### Stage 1 chronology — question freeze and independent checker dispatch

Before either specimen owner was dispatched, Task 1 wrote `contract.json`
with SHA256 `37ea8fa06f086000f94cf215a391b5bcbd74bef92b9502bdc02ebb0a9ad32b34`.
It fixes the four questions and their input/package/contributor map. The
independent checker task received these bytes and the approved raw schema,
in worktree `counterpart-checker`, branch `codex/counterpart-checker`. It was
instructed not to inspect the Charter verdict derivation. The foundation
author continues separately; these tasks share data types, not checker code.

Checker derivation disclosure lives in `checker-derivation.md` with a later
README link, avoiding simultaneous README authorship. This is a file ownership
adjustment only. The controller launched its ordinary prewarm separately; its
preparation cost is not an experiment observation.

### Task 1 implementation receipt — review pending

Commit `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd` adds the raw observer
and reusable diagnostic. Actual checks: two raw Rust tests, six new Python
tests, all eight retained Charter self-tests, selected clippy, fmt and
shellcheck passed. The ordinary commit gate passed all three subfloor chunks
(1435, 1383, 1227 tests) in 99.504 seconds. The lock diff adds one local
`digest-counterpart` record; no external lock record or simulation source moved.
The original successful-output-retention red was `b'' != b'\x00ok'`; raw API
expectations also failed against a compiling empty observer before implementation.

The independent task review has the entire dispatch-base range, including
two controller documentation commits, and both spec/quality verdicts are
required before Task 1 is considered complete.

### Main refresh before Stage 1

A fresh fetch found `cf95817d3` (The Kerf). Its chronicle and 22-path diff
were read: it removes a redundant resident water index, retaining the read
class and deterministic witnesses. No path in Digest, Charter diagnostic,
Thing or Settlement changed in this new range. Absorption waits until the
active task commits/reviews are complete; the shared main checkout stays untouched.

## #5 [G5] — accept Task 1 after independent review

**Decision:** Task 1 complete at `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`.
The reviewer returned spec compliant / quality approved, with no findings.
**Why:** raw observations, refusal separation, retained bytes, legacy lifecycle,
import/CLI checks and lock isolation are present with focused behavioral
evidence. Reviewer did not repeat already evidenced tests.
**Cross-task checks:** independent checker, owner chronology, all-question runs,
immutable attempts, both hosts, replay and canonical gates are assigned to
Tasks 2–7; none is counted achieved by this task.
**Alternatives:** no scope expansion or weakened check adopted. **Ideonomy:**
not an approach/Q decision; no new draw required at this green G5 checkpoint.
**Capture:** this acceptance, task-state scratch and the retained source base.

The source base for independently authored specimens is the reviewed Task 1
commit above. Checker code has its separate identity; no source specimen
needs the checker implementation in its production-source commit. Owner
worktree preparation may proceed during the separate checker review.

## #6 [G5] — accept and integrate independently derived checker

**Decision:** accept Task 2, authored at
`26c693ce027bbfb8c937e4f36f024949a6b97589`, integrated as `0bd2b4f8f`.
Independent review returned spec compliant / quality approved, no findings.
**Why:** 32 hand-derived cases passed; a compiling removal of the actual owner
comparison failed the wrong-owner case. Its ordinary hook passed 75 tests.
Derivation/disclosure is linked from the experiment README. No Charter verdict
implementation was read by the checker author or reviewer; incidental exposure
to production Thing test comments is explicitly disclosed.

The controller exercised the real observer with this independent checker:
`cargo run --quiet --manifest-path tools/digest/Cargo.toml -p digest-counterpart
--locked --offline`, then `evaluate(raw["facts"], contract)`. All four
questions were satisfied. The contract hash stayed
`37ea8fa06f086000f94cf215a391b5bcbd74bef92b9502bdc02ebb0a9ad32b34`;
checker SHA256 is `ee14515c833ddb43397b25ad9b035dcc18d07c748d7f1a1f3effa6b9f8902c90`.
This is base integration evidence, not a completed specimen panel.

**Cross-task checks:** duplicate JSON keys, exact persisted identities, attempt
completeness and call-boundary isolation belong to Task 5; supported-host and
replay obligations remain Tasks 6–7. **Alternatives/Ideonomy:** no new Q or
approach, no weakened obligation. **Capture:** acceptance, hashes, standalone
derivation and preparation cost. Checker worktree prewarm completed rc=0 in
312.064 seconds; the exact timings row is retained separately from assay cost.

### Stage 1 absorption receipt

`git merge --no-commit origin/main` against `cf95817d3` completed rc=0 with
no unmerged paths. Git reported automatic merges of idea registry,
reconciliation TSV and timings. No manual semantic resolution was needed.
This is an actual merge receipt, not a prediction about future integration.
The upcoming commit runs the ordinary commit gate before branch push/stage
submission. Frozen specimen source remains the separately identified Task 1 base.

### Stage 1 submission and Stage 2 independent preparation

The ordinary gate on the main-absorption commit passed all three subfloor
chunks in 223.968 seconds, rc=0. Candidate
`ab3abc6e011a2b97b02a83d38da8de916e4c6ffd` was pushed normally and submitted
as `req-ab3abc6e011a-20260905T183028Z`, kind stage. Initial readback was queued
behind The Zenith's running merge. This is not yet a green stage result.

The independent source authors share reviewed base `5cc62d8b6`, frozen questions
and the named collision input only. They work in `counterpart-thing` and
`counterpart-settlement`, on matching `codex/` branches, with no shared index.
Neither received the other's implementation or results. Both received the
same plain-list owner field contract and plain concept-name subject convention.
Top-level subject union and per-variant change deltas are explicitly distinguished.
The generic task extractor omitted Task 3's owner schema from Task 4; the
controller supplied that schema directly, without sibling specimen information.

Prewarms completed rc=0: Thing433.952s, Settlement359.107s; exact rows retained.
Source mutation/compilation slots were serial: Thing began after its prewarm
finished, restored all sources, then Settlement's prewarm and source runs
followed. Thing feasibility overlapped the controller's ordinary gate, so its
author timings are explicitly not controlled performance samples.

## #7 [G5] — accept independently authored Thing specimens

**Decision:** accept Task3 author commit
`846f3fa27760480ed5aac7f1f6a30368bd23faa5`, integrated as `d5f43b410`.
Both review verdicts approved with no findings. Three real-source variants
compiled and executed rc=0, produced observable effects, passed patch-application
checks, and restored source hashes. Lock diff was empty. Ordinary artifact hook
passed75 tests with246 skipped by its standard filter.

**Why:** safe addition, prospective joint claim and legitimate extra borrowing
have raw evidence independent of the candidate's declarations. Joint interaction
is still unmeasured; source objects, full-question evaluation and composition
belong Task5. The controller's explicit context/slot ownership receipts support
procedural separation; hashes alone are not atomic-capture proof.
**Scope clarification:** “safe” means the four frozen obligations. These synthetic
patches intentionally leave production FROZEN roster tests unchanged and are
not production admission candidates. No test was disabled to land source code.
**Alternatives/Ideonomy:** no new approach/Q or weakened obligation; the reserved
correlated challenge remains Task6. **Capture:** artifacts, exact author evidence,
this acceptance and the later-task verification obligations.

Task5 begins pure comparator/runner code against frozen interfaces while Task4
artifacts finish review. It may not invent the missing owner record or compose
the real panel before reviewed artifacts are integrated. This overlaps independent
preparation, not semantic integration or a pending review decision. The existing
Charter ledger's diagnostic/adopter overlap provides the same scheduling precedent.

## #8 [G5] — accept independently authored Settlement specimens

**Decision:** accept author commit `9c47f253db7b968152c6bb6dfa8787845779a340`,
integrated as `5c2d83c49`. Spec/quality review approved with no findings.
Three source patches only affect the owned manifest loop as experiment data.
Actual solo additions register successfully; lender rename produces a completed
observer rc=0 with expected Thing refusal. Author judgments are explicitly hand
interpretations pending the independent checker. Source/lock diffs are empty
after restoration and initial/restored raw base bytes agree. Hook passed75 tests.
**Cross-task checks:** immutable source commits, all-question outcomes, composed
arms, selection scoring and replay remain Task5 onward. Context separation and
serial compilation are recorded above, not inferred from hashes.
**Alternatives/Ideonomy:** no new Q or approach at this green review.
**Capture:** originals, raw evidence, review report and this acceptance.

## #9 [Q] — normalize independent declarations without hiding integration work

**Question:** how should the finite comparison consume independently authored
records that use different subject granularity and list semantics?
**Ruling:** keep originals intact and use explicit, versioned per-owner adapters
for this finite experiment. Record normalization mode, original field/quote and
reason; count the intervention as manual integration. Unknown or ambiguous forms
keep full fallback. This is no general prose interpreter and no executable
authority for admission.

**Evidence:** direct JSON inspection found Thing `changed_subjects` names source
locators (`THING_KINDS`, `thing_registry`, `BORROWED`); per-variant supplies are
additions, while its qualified top-level union includes prospective names.
Settlement `changed_subjects` names concepts; its variant supplies are the whole
post-change roster, while top-level supplies are `["home", "hearth"]`.
Thing's named negatives use `unclaimed:<name>` prefixes with explanation;
Settlement names the same kind of assumption in explicit prose. Treating both
supply lists as additions would invent a change to unchanged lender names.

**Chart: autonomy x reversibility (abstraction-lift, autonomy, reversibility):**

```text
                         Original retained          Original replaced
Human interpretation     Explicit reviewed adapter  Untraceable editorial rewrite
Automatic interpretation Known finite forms +       General prose parser whose
                         unknown fallback            assumptions become invisible
```

Lift: translating two declarations to a common observation space is the same
shape as unit conversion or questionnaire coding. A smaller output does not
remove the work of choosing the conversion. Retaining the originals makes a
wrong interpretation reversible and reviewable; imposing one silent meaning
on both lists does not.

**Why/precedent:** approved spec §6 requires one frozen comparison map, explicit
unknown fallback and reporting manual integration analysis. The Charter also
separates typed declarations from their truth; independent authorship does not
mean identical interpretation conventions.
**Alternatives discarded:** rewriting the owner records after seeing results,
a generic prose interpreter, and scoring one owner's full roster as all additions.
**Ideonomy passes / overturns:** one actual draw, reflected above; it reinforces
lossless finite normalization and its disclosed cost, with no further material
design expansion. **Cost if wrong:** biased finite comparator results; all checks
still execute and no admission authority moves. **Capture:** Task5 rule/evidence
requirements, this ledger and eventual retrospective.

## Stage 1 — canonical gate accepted

Request `req-ab3abc6e011a-20260905T183028Z` completed green on merge
product `9aa10c2c4957c7b6251c45a5193cd68f3185d476`, final artifact commit
`4b63c3faa4525e378ca5824f60dcb9daaa6d6899`. All four phases returned
zero: artifacts 311.916 s, outboard 94.742 s, gate 818.361 s, clients
153.921 s. The claim wait was zero; the queue request preceded job start
by 308 seconds, which is a separate cost. Nothing was pushed and main
remained `cf95817d37e8f9eea742d1ae9d2e09266f62970b`. Bounded log receipt
with original log location, size and SHA256 is retained under experiment
`evidence/controller/stage-1.json`. This accepts Stage 1 only; no composed
behavior or later runner correctness follows from this earlier candidate.

Task5 implementation is now committed at `9536a75d8224dd42b4fbc5fc536a9f38685021e2`
and its unreserved panel/bundle at `179a706fd0d521f6239fe1b3d3647175c1263f49`.
The author reports 51 focused tests and source-identity reconstruction for
all 11 arms. Independent review and actual canonical observations remain
pending. Full-panel execution uses the approved stage-only Charter transport;
a Mac derived subset is explicitly scoped and cannot certify the full roster.

## Task 5 — fix round 1

Independent review requested two P2 repairs before accepted observations.
Narrow probes changed stored observation argv/cwd to an unrelated process,
removed or contradicted the copied candidate, and fabricated the imports-only
supplement; the original validator still returned `completed=True`. Require
role-specific invocation and owned source context, exact candidate equality
with raw output, and recomputed supplemental evidence. These repairs bind
accidentally mixed records; they do not authenticate a hostile author.
The implementer is correcting both with behavioral RED/GREEN tests. Frozen
checker, owner records and selection rules remain unchanged, and no full
source-panel observation preceded this review. Original report/review and
implementation identities are retained. This is an existing Task5 requirement,
not a new Q or approach; no ideonomy ruling is needed at this red checkpoint.

Diagnostic worktree preparation completed locally: normal `make prewarm`
returned zero, 409.902 s wall. Its timing row is retained separately from
assay execution; no source observations were collected by this preparation.

Task5 round1 author committed `02796fe0ae719a55f431f8aedc2ab6853c9a2776`
and updated panel identity at `57f030395cac159dc6b93a7989a3df291e4a9a71`.
The report records 14 invocation/context, two candidate and six supplement
behavioral RED cases before repair, then 55 focused tests green in 21.295 s.
Both ordinary hooks passed75 tests. Original panel retained exactly; checker,
rules, owners and source bundle are unchanged. Scoped re-review is pending.
Author/review records for Tasks1–5 are now durable under
`tools/digest/experiments/the-counterpart/evidence/controller/reviews/`.

## Task 5 — code review accepted, observations pending

Scoped re-review closed both P2 findings, with spec and quality PASS for
canonical diagnostic readiness. The 55-test author evidence was inspected;
no redundant suite was rerun by the reviewer. Original review and its
correction receipt are retained. `freeze.json` records the reviewed runner,
checker, rules, question map, owner records, unreserved source roster and
bundle before revealing a reserved challenge. This is a code/input freeze,
not an empirical Task5 pass. The original checker author resumed only to
confirm context availability and explicitly reported no reserved selection
or revelation. Task6 dispatch still waits for the unreserved assay and
normal Stage2 boundary. No new approach or Q arose at this green checkpoint.

## Canonical diagnostic submission and scoped Mac qualification

Diagnostic branch `codex/counterpart-diagnostic` adds ten transport-only lines
to `scripts/lane-outboard.sh` after ordinary checks, following the approved
Charter precedent. The diff is retained as data in `evidence/controller/`;
this script change is never a campaign merge candidate. Syntax and shellcheck
passed. Normal hooks passed75 tests and explicit `make gate-commit` passed
all three chunks in91.809 s. Candidate `0c6a920898c2e86789d2aa60b93edcdbe50f879c`
was pushed normally, then queued stage-only as
`req-0c6a920898c2-20260905T191704Z`. Actual full-panel results remain pending.

The separately committed `panel-mac-base.json` is explicitly one-arm scope,
carrying the full panel's parent SHA256 and its own roster identity. Actual
CLI execution returned zero; base satisfied all four questions. The controller
revalidated the retained dossier. Metadata took0.203/0.157 s, build8.998 s,
observation0.199 s; arm wall13.507 s includes preparation2.469 s. These are
observed local costs, not cold-cache or full-panel performance claims.
43 exact JSON records (5,278,407 bytes before compression) are retained in
`evidence/mac-base/records.tar.gz`, alongside member hashes, command receipt
and readable summary. Owned checkout/target remain at the receipt's path.
This qualifies the Mac command/report path only; no composition result follows.

## #10 [Q] — separate challenge authorship from gated qualification

**Ruling:** after the committed complete input freeze, resume the independent
checker author to preregister and compile its reserved source challenge while
the canonical unreserved assay is queued. Formal scoring/integration acceptance
and dependent qualification still wait for the unreserved result and green
normal Stage2 gate. This revises the stricter dispatch ordering noted above;
it changes scheduling, not the frozen experiment or any stage requirement.

**Why/precedent:** approved spec §5 requires owner records, comparison rules
and checker implementation frozen before reserved selection; that prerequisite
is now committed at `16861a886a7069cf01123d0cde5688364781cff9`. The plan's
Stage2 result is an acceptance dependency for qualification, not information
an independent challenger should consume. Tasks3–4 and the Charter's separate
adopter/diagnostic preparation use the same independent-authoring overlap.

**Organon list — visibility and animacy:**
- Frozen questions and identities: visible informational inputs; author may read.
- Original checker derivation: its own informational context; retained.
- Other owners' patches and unreserved judgments: hidden from task context; do
  not provide them or ask the challenger to compensate for their results.
- Compile/probe process: live activity in its separately owned warm checkout;
  serial locally, bounded, with all attempts retained.
- Formal scoring and acceptance: later activity, gated by Stage2 evidence.

Separating these items exposes the only useful overlap: an independent author
can work from already frozen information without depending on another run's
answer. Treating every activity as the same stage would consume waiting time
without strengthening that independence. **Alternatives rejected:** reveal
ahead of the freeze; tune from unreserved outcomes; count a queued gate as
green; bypass the canonical queue.
**Ideonomy passes / overturns:** one actual organon-construction/list draw
with visibility/animacy; it overturns the blanket dispatch wait, retaining
the acceptance wait. No further material change emerged.
**Cost if wrong:** discard/requalify a challenge if the frozen instrument needs
revision; original identities and attempts remain, and no admission moves.
**Capture:** this ledger, task brief and retrospective scheduling account.
