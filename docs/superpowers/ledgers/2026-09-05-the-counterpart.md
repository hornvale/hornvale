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
