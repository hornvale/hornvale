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

## Verification and observations

- The isolated worktree was created from the fetched `origin/main` at the
  base above. No implementation files have been changed during this phase.
- `make prewarm` completed with rc=0; its ordinary timing row is retained in
  `docs/timings.md`. This is build preparation, not a claim that tests passed.
