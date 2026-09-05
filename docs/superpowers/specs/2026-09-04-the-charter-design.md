# The Charter — independently contributed development knowledge

Status: implementation and qualification complete; Nathan approved G6 on
2026-09-05. Canonical census qualified; approved for ordinary sluice admission.
The broader program stays active. [Qualified evidence](../../digest/the-charter-evidence.md).
Branch: `codex/the-charter`.
Observed base: `fd3cd159be98e6c07dbb849b780c2712c6a7826f`.
Scope: completed bounded pilot; close documentation prepared for the approved merge.
Design-time base facts below describe the pre-campaign checkout.

Program: [Federated development](2026-09-04-federated-development-program-metaplan-design.md).
Rulings and evidence: [decision ledger](../ledgers/2026-09-04-the-charter.md).
Precedent: [The Digest](2026-08-08-the-digest-design.md) and
[evidence doctrine](../../design/evidence-doctrine.md).

## 1. Outcome and falsifiable hypothesis

An author asks for context about `domains/thing` or the census publication
surface. The Digest returns the relevant authored instructions, observations
from real Rust APIs, source references, and the limits of those observations.
The two contributors can be implemented on independent branches after a
shared bootstrap and composed without changing the host or protocol for
either contributor's meaning.

This is the first working turn of the program: local knowledge, a small
shared contract, checks, and a generated view. Its hypothesis is **independent
extension can produce useful checked context without extending a central
semantic dispatcher**. The acceptance exercise must be able to disprove it.

The campaign does not grant permission to merge untested changes, narrow
gates, refresh artifacts off-host, bypass the Sluice, or change an epoch.
It does not replace root prompts, automate task startup, import the whole
idea registry, generate the whole Book, or implement native NLG. Those remain
named program expansions rather than implicit prerequisites.

## 2. Existing facts that shape the design

- `tools/digest` is a Rust tool outside the simulation workspace with an
  explicit local `[workspace]` and serde/JSON dependencies. Its CLI currently
  dispatches the fixed `render doctor`, `render decisions`, and `render delta`
  commands. Keep those commands compatible.
- The Digest's current `ProjectLedger` uses local numeric identities and
  functional assertion replacement. Its time-free fact store is not a
  federation transport: conflicting assertions must not become whichever
  file was read last. This campaign leaves that store and vocabulary intact.
- Thing exposes its registry, kind roster, borrowing declarations, and
  registration function. Settlement exposes the real lender registration.
  An outboard tool can compose both without a sibling-domain dependency.
- The lab exposes a pure `require_canonical_host_for(study, base_dir, host)`
  guard. Its inputs and return type allow a bounded behavioral check without
  running a census. `publish` invokes the guard before writing; that call
  site is additional source evidence, not something a pure predicate test
  proves by itself.
- `scripts/lane-outboard.sh` currently tests Digest's root package. If Digest
  becomes a workspace, that entry must cover all its members. The artifact
  regeneration script already invokes existing Digest commands; compatibility
  is an integration requirement, not an assumed empty artifact diff.

## 3. Package and ownership boundary

Proposed layout, all development-side:

```text
tools/digest/                  existing CLI plus generic collection/composition
  packages/protocol/           shared Rust types, JSON version, envelope checks
  packages/thing/              Thing/Settlement adapter and local requirements
  packages/census-publication/ lab-guard adapter and local requirements
```

Use `packages/*` as workspace members with the permanent protocol package
present at bootstrap. Contributor packages declare their role, namespace,
protocol version, normalized repository scopes, and executable target in
package metadata. Metadata supplies scope selection before execution; the
returned envelope must agree. The host reads
`cargo metadata --format-version 1 --no-deps`, filters actual workspace
members, and invokes the declared Cargo binary target by structured arguments.
Metadata and contributor Cargo commands always receive an explicit
`--manifest-path` for this checkout's `tools/digest/Cargo.toml`; invocation
from the repository root must not select the simulation workspace instead.
Reject duplicate namespaces, unsupported versions, missing/ambiguous targets,
or declarations inconsistent with the returned envelope. Do not accept shell
commands, arbitrary executable paths, or discovery outside this workspace.

The host selects the exact package and binary target and builds through Cargo
on every collection invocation, then collects a new result. Cargo may reuse
its build cache. Contributor processes emit protocol JSON on stdout and
diagnostics on stderr; malformed, empty, failed, or unsupported responses
are errors, never an empty successful contribution. Protocol version 1 has
a documented response size limit and execution deadline; either limit being
hit reports an error with the contributor identity. Cancellation terminates
and reaps the invocation's process tree, including a compiler or contributor
descendant, rather than leaving work running after its parent exits. Treat
build waiting separately from the contributor execution deadline and expose
both phases. A cold compilation failure is not valid evidence.

This is a development-tool subprocess boundary. Each contributor is ordinary
trusted repository code, with the process's permissions. It is not a sandbox
or a dynamic simulation plugin ABI. Dependencies point from adapters to
simulation APIs; simulation crates never depend on this protocol or host.

The host owns discovery, envelope validation, composition, path selection,
and common Markdown framing. Local contributors own their rules, method
implementations, semantic interpretation, and authored instruction blocks.
The protocol may depend on serde; it does not depend on the simulation kernel.
Do not move project concepts into the kernel to make this adapter compile.

### Evidence for the discovery choice

Two temporary Cargo probes are recorded in ledger #3. An empty member glob
failed with rc=101. A root package with a permanent protocol member under
`packages/*` returned rc=0 both before and after adding two packages; metadata
listed the new packages without a root edit. `cargo run` selected the root
binary and printed `root-ok` in both states. This checks discovery and root
selection only; actual adapter builds and the merge exercise remain owed.

Cargo.lock is a shared dependency-resolution artifact. Reconciliation there
is allowed and measured. The spec promises neither zero conflicts nor zero
additional compilation. Use the existing Cargo toolchain and JSON facilities;
no new plugin framework is justified by these two adapters.

## 4. Protocol version 1

Keep the transport to one typed, versioned envelope. The exact Rust naming
can be settled in the implementation plan; the semantic fields below bind it.

| Record | Required meaning |
|---|---|
| Contributor | Namespace, protocol version, display name, declared repository scopes |
| Requirement | Namespaced stable ID, authored statement, authority/source references; either authored-only or checked with a nonempty set of required observation IDs |
| Observation | Namespaced stable ID, named method, precise subject/scope, result and explanatory details, requirement references where applicable |
| Instruction block | Namespaced stable ID, authored Markdown, requirement references and optional observation references |
| Collection context | Checkout identity and dirty-state disclosure, contributor/package identity; framing that observations were obtained in this invocation |

Observation results distinguish satisfied, contradicted, and unknown. A
transport failure is a collection error, not an observation of satisfaction.
An authored instruction need not have an executable observation; render it
as authored and unchecked rather than inventing one. A checked requirement
declares its expected observation IDs before results are assessed: all must
resolve, refer back to that requirement, and be satisfied for it to count as
checked and satisfied. A missing or unknown required result is nonzero;
absence cannot downgrade the requirement to authored-only. A contributor
changing that declaration is a reviewed policy change, not a successful run
of the former check. The report must
name the method and finite scope; no overall “project verified” indicator.

IDs are unique within the composed collection and prefixed by their owner
namespace. References must resolve to records of the required kind. Scopes
are normalized repository-relative paths, matched at path-component
boundaries, never arbitrary string prefixes. Version 1 permits only local
references within a contributor; cross-contributor semantic imports and
compatibility negotiation are a later contract amendment. Thing's use of
Settlement APIs is an ordinary Cargo dependency, not a protocol import.

Reject duplicate IDs even if their current text is equal: ambiguous ownership
must not become a future last-writer-wins bug. Different contributors may
cover overlapping paths; both appear, each with its own identity. The host
does not claim to detect contradictions in arbitrary natural language.
Structured duplicate identities are detectable; disputes between distinct
authored statements remain review obligations.

Composition sorts by namespace and record ID. Preserve any explicitly
contractual order inside an observation, such as a source roster; sorting the
outer collection must not redefine simulation order. Rendering the same
envelopes in different discovery orders must produce identical Markdown.
No clock or random run ID belongs in deterministic report content.

## 5. The two adopters

### 5.1 Thing ownership and borrowing

The contributor uses `thing_registry`, `THING_KINDS`, `BORROWED`, and the
actual Settlement and Thing `register_concepts` functions. Requirements
explain that Thing's declared kinds must match its registry, owned kinds
must register, and borrowed kinds must resolve to their declared owner.

Observe roster/registry agreement in both directions; a declaration-only
subset check would miss undeclared entries. Compose the real lender and
borrower registrations in a small registry, then observe owned and borrowed
ownership. Derive names and owners from the APIs rather than keeping a
second hand-maintained kind list. Retain source order where it is a contract.

The useful context explains where an author adds Thing knowledge, which
concepts Thing borrows, and which local agreements were checked. It points
to the existing instructions and owning APIs. It does not claim to exercise
world generation's actual registration roster, save compatibility, item
behavior, stream consumption, or every rule in `domains/CLAUDE.md`.

### 5.2 Official census publication host predicate

The contributor reads the public canonical-host constant and invokes the
pure guard. Its named case panel covers official census study names and
the official output suffix, canonical and off-host inputs, case handling,
non-census studies, and non-official output destinations. Check accepted as
well as rejected cases; an always-rejecting guard is also wrong.

The context states exactly which predicate was exercised. The current
predicate uses a path suffix; do not describe it as a filesystem containment
or security boundary. Read the canonical host from its existing source,
`scripts/census-canonical-host.txt`, rather than authoring another hostname
literal. Check that the compiled constant and current source agree and
report a discrepancy as an error; this is one specific stale-input check,
not complete build provenance.

The existing instruction to request a census with
`make sluice-census BRANCH=<requester> REF=<full-sha>` is an authored policy
with current decision/source anchors. It is not “verified” by the host
predicate. Do not duplicate the phase roster. Existing publication and queue
tests continue to check their own boundaries; this campaign runs no live
census or real queue submission as a contributor observation.

## 6. User surface and evidence limits

Proposed commands, not existing capabilities:

```text
cargo run --manifest-path tools/digest/Cargo.toml -- context domains/thing
make context SCOPE=windows/lab/src/publish.rs
```

The scope selects applicable contributor context, with the invoked command
building/collecting those contributors afresh. Explain whether no scopes
matched, a contributor failed, or a named observation was unknown or
contradicted. Empty selection must say “no enrolled context,” never success
at checking the requested subject. Version 1 uses nonzero exit for malformed
collection, unresolved references, contradictions, unknown required checks,
or no match; a rendered diagnostic may still be useful to the author.

Markdown separates authored requirements/instructions, observations, and
limits, and gives repository references. The same output is usable as task
context for Claude or Codex. Add a short durable usage guide and one pointer
from existing developer orientation so authors can find the command. It is
an explicit invocation; root `AGENTS.md`/`CLAUDE.md` retain their existing
authority. Do not silently rewrite their contents or imply startup adoption.

Reports are on-demand views, not committed generated artifacts in version 1.
An author can save stdout, but the tool never reloads that saved report as
current evidence. This avoids introducing another mutable central aggregate
before independent collection works. Persistent generated prompt sections
and Book publication are subsequent adopters with explicit writer/drift rules.

Checkout identity and dirty state provide context, not a complete digest of
compiler, dependencies, environment, and runtime inputs. Version 1 does not
certify reproducibility, atomic observation of a concurrently edited tree,
or equivalence between two runs. Report these limits. Contributor checks are
ordinary reviewed code and can themselves be wrong. None of this evidence
may authorize gate omission or approve a change to its own governing rule.

## 7. Acceptance evidence

Use meaningful behavior tests within the existing tool conventions. Avoid a
test per copy of a rendering template. The following properties are required;
the implementer finds discriminating fixtures or mutations after reading
the code, asserts that each mutation took effect, and distinguishes a
behavioral failure from a compile error.

| Property | Required observation |
|---|---|
| Independent enrollment | Freeze the generic host/protocol bootstrap; add the two real contributors on separate branches. Compose them without a contributor-specific host/protocol/source-dispatch edit. Record branch bases, shared file edits, reconciliation, and results. A necessary semantic core edit fails this experiment and reopens design. |
| Real Thing evidence | Missing/wrong lender, missing registration, and roster disagreement are detected. Changing an authored assertion alone cannot manufacture a satisfied observation. Valid local changes update observed context through the adapter. |
| Guard discrimination | The finite panel detects both over-admission of off-host official publication and over-refusal of allowed inputs. A compiled-host/source mismatch is reported. The output makes no queue execution claim. |
| No vacuous success | Empty/malformed/failed responses, unknown protocol, duplicate ownership/IDs, wrong-kind or unresolved references, metadata/envelope scope disagreement, and no matched scope cannot render a green result. Removing a required observation while leaving the rest of the collection valid must be detected. |
| Compositional determinism | The same valid envelopes in all tested discovery orders produce equal output; path matching distinguishes actual descendants from similarly prefixed names. |
| Fresh invocation boundary | A saved earlier report is not accepted as input to the context command. Changed local requirement/check inputs are recollected; stale compiled-host evidence is not labeled current-source agreement. |
| Existing behavior | Existing Digest commands/tests remain valid; the outboard entry tests all packages; dependency layering remains valid. Invocation from the repository root still discovers only the Digest workspace. Actual generated diffs are inspected rather than predicted empty. |
| Process lifecycle | Deadline, response overflow, and interrupted collection report failure and leave no live invocation descendants or unclosed pipes. Build waiting and contributor execution are reported separately. |
| Useful cost | Measure cold and warm context invocation, splitting build from collection. Report dependency expansion and output size. If either adopter requires world construction or a census, the chosen bounded boundary has failed. |

The two author branches share the bootstrap base and no mutable working
directory. Their own packages, tests, and local documentation are their
ordinary edit scopes. The composition exercise may reconcile Cargo.lock and
generic integration wiring already specified at bootstrap; it may not hide
a new host semantic dependency as “integration.” Existing queued stage and
merge verification still apply to the actual campaign product.

## 8. Quality contracts

Quality requirements have stable IDs and the same distinction between authored
obligation and observed evidence as behavioral requirements. Version 1 can
express them using the existing requirement/observation records; it does not
need a universal quality taxonomy or a new kernel type. A quality scenario
names subject/owner, workload, environment, required response or budget,
evaluation method, and breach action. Keep desired targets separate from
observed baselines. Regenerating measurements must never relax a target.

The first campaign adopts the following scenarios. These are requirements to
verify, not claims that the current tool meets them.

| ID | Scenario and acceptance | Evidence and breach action |
|---|---|---|
| `charter.isolation` | Invoke context from two different worktrees with deliberately different local declarations, including inherited Git path variables and a shared target-dir environment override. Each invocation observes its own source and branch; it does not edit tracked files or select another worktree's contributor executable. | Hermetic fixture repositories and explicit current-checkout manifest/target paths. A mismatch fails collection. Cargo dependency caches may be shared; checkout-specific compiled artifacts may not be selected by an ambient override. |
| `charter.determinism` | Reorder equivalent contribution envelopes, retaining any semantic order inside observations. Markdown stays identical for identical envelopes and checkout context. | Composition tests; execution timing belongs on stderr, outside deterministic Markdown. A mismatch fails the tool suite. |
| `charter.recovery` | A contributor hangs, overproduces output, exits with an error, or receives cancellation while holding its pipes open. The host returns a diagnostic/nonzero status and terminates its invocation process group, waits for its direct child, and drains/closes its readers. | Child/grandchild fixtures and live PID checks on macOS and Linux. No successful partial report; no invocation process remains running. The host cannot reap arbitrary grandchildren on every OS, so require termination and direct-child reaping rather than a portable subreaper guarantee. |
| `charter.resource-bounds` | Run the two enrolled contributors sequentially within an invocation. Bound each contributor's stdout to 1 MiB, stderr capture to 64 KiB, execution to 5 s; bound metadata/build machine-output capture to 16 MiB and each build/metadata phase to 600 s. Cancellation allows 250 ms for orderly termination before forced termination. | These are initial operational caps chosen for small JSON observations, not measured service-level claims. Boundary/overflow/deadline tests use injected smaller limits. Exceeding a cap fails visibly; changing a cap is a reviewed policy change, not an automatic rebaseline. Unbounded contributor fan-out is excluded. |
| `charter.cost-attribution` | Measure both real scopes and their combined collection on the named Mac and canonical Linux host: one cold tool-target build and ten warm invocations per workload, recording toolchain, source, build, collection, elapsed time, and available peak-memory data. | Publish all samples including failures, and report missing measurements honestly. Cold runs use disposable target directories, never delete an active cache. The evidence report must propose a justified steady-state target or explicitly record why no performance claim is earned; a guessed cross-host latency threshold is not acceptance evidence. Operational caps above still apply. |
| `charter.extension` | After bootstrap, independently enroll both pilot packages without semantic host/protocol edits; resolve only permitted dependency artifacts. | Record the fork base, changes and composed result. Necessary core semantics reopen the design. Existing render commands remain supported. |
| `charter.diagnostics` | Empty scope, failed contributor, metadata mismatch, malformed JSON, missing required observation, and contradiction each identify the phase, contributor/requirement where known, and the actionable cause in plain text. | CLI cases capture stdout/stderr/exit status. Do not print a completed success report before validation finishes; no ANSI color or graphical viewer is needed to understand failure. |
| `charter.portability` | The same protocol, path-selection, discovery, and lifecycle suite passes on macOS and canonical Linux. Host-specific census rules stay explicit. | Local scoped tests plus the normal canonical outboard phase. Unsupported operating systems fail clearly for context collection; existing unrelated Digest commands are not deliberately disabled. |
| `charter.usability` | An unfamiliar reader can identify which statements are authored, which named checks ran, what failed, and where to inspect the owning source from a bounded scoped report. | Independent review using both real reports and one failure report; record examples and corrections. This is evaluated evidence, not a compiler guarantee or an overall quality score. |

The host owns shared process/resource policy; contributors own their local
observations. The composed workload owns end-to-end cost evidence. Passing
local limits does not establish a global concurrency or memory budget across
independent host invocations. Those remain program-level obligations before
narrower admission can be earned. The current Sluice serialization rules
remain authoritative for canonical work.

Context runs with `--locked --offline` after the normal dependency/build
preparation step. A missing dependency or stale lockfile is a useful failure
with a preparation instruction, not permission to modify tracked inputs or
fetch silently. Control Git path variables, Cargo target selection, cwd,
stdin and child lifetime explicitly. Execution remains trusted repository
code, not a security sandbox; process-group cleanup assumes contributors
obey the contract and do not detach into independent sessions.

## 9. Integration, risks, and response rules

- Keep current outboard tool placement. Record any proposed new dependency
  and its reason; defer unrelated crate restructuring.
- If an adopter needs a new semantic host variant, stop the autonomy exercise,
  explain the counterexample, and revise the design through the ledger.
- If a public API is insufficient, inspect the smallest development-side
  composition first. A production semantic change or expanded public contract
  requires an explicit spec amendment, not an incidental adapter fix.
- If existing generated artifacts move, identify their inputs and authority.
  Follow normal authoring/gate rules; do not assume docs-only or byte identity.
  Unexpected world/save/epoch movement is outside this campaign's scope.
- If context duplicates an authored rule, retain the source reference and
  label it as an adopted instruction. Semantic equality with arbitrary prose
  is not mechanically established. Broad prompt takeover waits for a later
  migration with explicit ownership and drift checks.
- If collection is too slow, measure compilation versus execution before
  moving code or caching results. Do not introduce unsound evidence reuse to
  make the first timing attractive.

## 10. Approved G3 decisions

1. Adopt a new **development-only JSON protocol schema**, outside the kernel
   and the time-free ProjectLedger; no save-format or simulation schema change.
2. Accept independently compiled Rust contributor processes and Cargo package
   discovery as the smallest tested route to enrollment without host edits.
3. Bound the first useful product to on-demand checked task context from two
   real adopters; persistent prompts, the Book, NLG, and gate selection follow.
4. Accept explicitly incomplete provenance and narrowly scoped observations
   in this read-only product; they confer no admission or activation authority.

5. Make quality scenarios explicit as in §8, preserving the distinction
   between operational caps, observed baselines, and earned service levels.

Nathan approved continuing after reviewing the original package and the
quality-contract refinement at G3. That authorized implementation planning;
the later implementation decisions and separate G6 approval are recorded below.

## 11. Post-G3 implementation decisions and qualification

These promote the material execution rulings from the
[campaign ledger](../ledgers/2026-09-04-the-charter.md), without creating a new
set of global rules or expanding the approved pilot.

- **Independent adoption:** freeze the reviewed protocol/host bootstrap before
  two separate adopter branches. Package-local code and shared Cargo.lock
  reconciliation are permitted; no contributor-specific host semantic change
  is. Integration retains both reviewed trees and the 22 existing external
  dependency records; the 27 added local packages are disclosed build cost,
  not an autonomy counterexample or evidence that a census executed.
- **Failure semantics:** every explicit contradiction fails, including an
  optional observation; only unknown observations outside the required set
  may remain informational. Authored declarations cannot satisfy their own
  observations. The census panel's prefix and relative-suffix forms require
  discriminating wrong-host refusal cases, not canonical-host successes that
  also pass when recognition is missing.
- **Ownership is part of the instrument:** all fixture and diagnostic Git
  commands scrub path overrides, and fixture checkouts preserve the toolchain
  pin. Process tests reproduce actual nested groups and abnormal parent exits.
  The diagnostic finishes its owned session before removing its checkout;
  cleanup uncertainty retains the directory and available raw sample, emits
  the failed attempt, then aborts later work. Interrupted zero-exit attempts
  retain stdout. Trusted contributors may not detach from the applicable
  owned group/session; no sandbox or portable grandchild-reaping claim follows.
- **Preparation and measurement:** disjoint harness preparation may overlap
  adopter work, but actual samples wait for reviewed integration. Fresh owned
  targets measure host build, selected cold request and ten warm requests
  separately; all-member preparation is not a selected-workload cold sample.
  Keep every attempt, raw RSS units, host/toolchain/load, and unavailable
  lock-wait/subphase/aggregate-memory attribution. All 72 samples are retained;
  no steady-state SLO is earned by the short consecutive series. Existing
  operational caps remain unchanged.
- **Canonical evidence:** use the existing stage-only transport for diagnostics,
  not a new benchmark authority or routine outboard invocation. The temporary
  five-line diagnostic addition is excluded from integration. Distinguish
  submitted candidate, initial merge, measured source and final tested tree;
  green measurement steps alone do not establish a completed stage job.
- **Authored evidence and capture:** report snapshots and sample JSONL are
  explicitly authored evidence under `none(...)` exceptions. Regeneration
  supplies write counts; mtime overlap with unrelated writers invalidates a
  causal authorship reading. Preserve the cost and repeat with frozen writers,
  without editing capture counts. Reconciliation rows omit optional ledger
  links until the parser/coverage mismatch is resolved separately.
- **Review and scope:** independent source, final evidence and three-report
  usability reviews passed. M1's inline-code escaping and scope-wording
  refinements are deferred alongside richer provenance and measured cost/tooling
  gaps in the [retrospective](../../retrospectives/the-charter.md) and registry.
  Neither typed declarations, finite observations nor generated prose prove
  truth, coverage sufficiency or semantic independence of arbitrary changes.

G6 authorizes the ordinary close/merge workflow, not omission of its census or
other gates. Implementation/qualification completion does not close the active
federation metaplan. The Confidence Gradient was searched at close: **N/A**, no
existing world bet moved; the Book gains the bounded Digest continuation.
