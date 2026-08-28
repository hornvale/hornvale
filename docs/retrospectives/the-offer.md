# The Offer — retrospective

*Arc IV.a of The Bridle. Objects advertise verbs derived from properties,
filtered by the body and by the observer's knowledge. Nine tasks executed of
ten planned, eight decision records (0346–0353). Process lessons only; the
campaign's own account is [the chronicle](../../book/src/chronicle/the-offer.md).*

## Seven vacuous guards, every one born in my prose, none found by reading

This is the campaign's headline and it is not close. Seven checks asserted
things that were true of *any* implementation, or protected code no assertion
reached. **Every one originated in the spec or the plan — my text — and not one
came from an implementer.** And not one was found by re-reading. Each died to a
command somebody ran.

| # | the guard | why it could not fail | how it died |
|---|---|---|---|
| 1 | `extra_properties_do_not_withdraw_an_offer` | asserted a static fact about `required_properties`; never called the query it existed to guard | reviewer changed `is_subset` to `==`; 6/6 stayed green |
| 2 | `no_verb_by_object_table_exists` | scanned for `"=> OfferedVerb::"`, a shape nobody would write — the real table has to build a `BTreeSet` | reviewer wrote both plausible spellings; 0 matches each |
| 3 | `body_relativity_never_withdraws_an_existing_capability` | `offered_to = offered_by.filter(..)`, so the subset assertion re-proves an invariant of `Iterator::filter` | reviewer made the doorway mass-gated — the one thing forbidden by name — and both tests stayed green |
| 4 | the knowledge gate (a whole spec section) | no live session path can reach a room it has not absorbed | pre-dispatch grep over the absorption path, then a reviewer re-checking every citation against source |
| 5 | the containment feature (a second whole spec section) | the property's only carrier is never a container in any authored room | a census over all 60 production gate combinations |
| 6 | `Session::warm`'s success path | both existing tests exercised refusals; one was turned away by the body gate before the method ran | whole-branch reviewer repointed the gate's anchor kind; 700/700 green |
| 7 | the containment production wiring | the pin test was tautological (both sides call the same function) and never reached the room the alcove lives in | whole-branch reviewer reverted the production call; 700/700 green |

Two of the seven are worth separating from the rest because they are a
different animal. Rows 4 and 5 are not assertion gaps — they are **features
that could never fire**, and they were caught *before* the code was written,
by a grep and a census run during pre-dispatch verification. That is the
cheapest possible place to find them, and it worked twice. Rows 1, 2, 3, 6 and
7 all reached committed code first.

## The remedy, adopted mid-campaign and now a decision

After the second one, the common cause was legible: **I wrote each test's
INTENT and let its SHAPE be inferred, and the inferred shape was structurally
incapable of failing.** One of the seven was worse than that — I shipped an
empty test body, a comment describing the intent and no code, so the
implementer wrote it from nothing and reached for the shape my prose implied.

The rule that came out of it, and is now decision 0353: **for a test whose job
is to catch a specific regression, name the MUTATION it must fail against, not
the property it should assert.** A property can be asserted vacuously; a
mutation cannot be failed vacuously. The mutation must type-check, because one
that fails to compile says nothing about whether an assertion would have caught
the behaviour.

Adopting it mid-campaign changed the later tasks visibly: several tests in
`windows/vessel/tests/suite/affordance.rs` now carry their own mutation in their
doc comment, so the next reader re-runs it rather than re-deriving it.

## The discipline compounded across four levels

Implementer → reviewer → fixer → re-reviewer, each one catching the previous
one's imprecision **by running it** rather than by reading it.

- The **implementer** found that my plan's `offered_by` sketch was wrong — it
  returned an empty offer for the eight anchor kinds carrying no properties,
  which would have withheld the universal `examine` from all of them. Found by
  writing a test that failed against my version, not by reading my version.
- The **reviewer** found rows 1, 2 and 3 above, each by mutation.
- The **fixer** proved the replacement source scanner by *injecting* a real
  verb×object table into the file and watching the guard fire — a positive
  control on the guard itself, not on the code it guards.
- The **re-reviewer** then defeated the new scanner two ways (an if/else
  dispatch, and indirection through a helper) and confirmed both are already
  named in the guard's own doc comment. Disclosed limits, not undisclosed gaps.

**The clearest single instance is at the last level.** The final reviewer's own
illustrative mutation for the `warm` fix — repoint the gate from `Hearth` to
`Bed` — turns out to be **non-discriminating**: the authored fireside bed
requires a hearth in the same chamber, so the two anchor kinds are perfectly
co-located in every real interior and the success test stays green under it. The
fixer found that by *running* the reviewer's mutation instead of trusting it,
and substituted a vessel, whose roles exclude the hearthroom outright. That
finding is also *why* the fix needed a structural source scan rather than a
behavioural test: no behavioural test can discriminate two correlated anchor
kinds.

One more level down the same chain: a fix report's own transcript cited a line
number that, after the fix, had become prose. The re-reviewer noticed, relocated
the mutation to its true current site, and re-ran it there.

## A ruling made on good evidence, superseded by better evidence one task later

At Task 1 the implementer refused to pad the property table beyond the carriers
the spec named as certain, and gave code evidence for each exclusion. I accepted
the narrow assignment, on the argument that refusing to invent a datum beats
manufacturing a stronger-looking demonstration out of assignments nothing
supports. I logged the cost at the time: the two-carrier proof would rest on one
property, only just satisfying the next task's threshold.

One task later that ruling was the **root cause** of two of the seven vacuous
guards. Every registered kind carried exactly one property, so no real datum
exercised subset-versus-equality, and the tests written against that data could
not fail.

Recording it as the ledger working, not failing. The ruling was defensible on
its own terms and on the evidence available when it was made; what superseded it
was evidence that did not exist yet. The fix chosen was also the right one — the
query was reshaped to take a trait set directly, making it testable at its
source, rather than the carrier table being padded to rescue the tests. The
implementer re-examined the tempting extra assignment a second time and again
found it unsupported. Two independent refusals to invent a datum, and the design
moved instead.

## The stage-boundary absorption cadence was missed entirely — a controller failure

`CLAUDE.md` requires a campaign branch to absorb main **at every plan-stage
boundary**. This branch absorbed main exactly once, at close, **50 commits
behind**, and it conflicted — on `docs/audits/type-audit-report.md`, an
aggregate artifact that must never be text-merged, and the exact file the
project's own tooling notes name as the recurring case where two sessions drop
opposite halves cleanly.

This is mine. Ten tasks ran with review rounds between them and no absorption
was submitted at any of the boundaries. It cost nothing this time, which is the
part worth distrusting: the same omission is how a campaign discovers at close
that a semantic collision has been sitting in main for a week, and the reason
the cadence exists is that small absorptions keep drift next to its cause. The
right correction is not "remember harder" — the boundaries are already named in
the plan file, and a stage submission is one command that refuses in
milliseconds if the merge would conflict. It was simply never run.

## A reviewer parked on a watcher and left a mutation applied in the source tree

A re-review burned 1275 seconds waiting for a monitor to signal a test run had
finished, and returned having left its mutation **applied and uncommitted** in
`windows/vessel/src/affordance.rs`. It was caught only because the tree was
checked before anything else was dispatched. Had the next task gone out onto
that tree, its implementer would have built on mutated source and every result
would have been quietly wrong.

Both halves of this are already written down — "I'll hold here / wait for the
watcher" is the documented signature of an agent that has parked and whose
children are dead, and "after any kill, sweep" is the standing rule. What is
worth adding is the specific hazard: **a mutation-based review leaves the tree
dirty by construction**, so the sweep is not optional hygiene but the only thing
standing between a killed reviewer and a corrupted successor. Verify the tree
between dispatches, not at the end.

A near-miss in the same family, from a different task: an implementer's scratch
backup was silently dropped by a harness guard (two test-runner calls batched
into one shell block), so no backup existed when it went to restore. It
recovered with a targeted `git show HEAD:<path> > <path>` rather than a blanket
checkout, because real uncommitted edits were live elsewhere in the tree — the
correct instinct, and the blanket version is the trap the parked reviewer's task
had already hit.

## A terminal artifact-sweep task is a plan defect, and the fix strengthened the sweep

The plan gave regeneration its own final task: every earlier task drifts a
generated artifact, and Task 10 sweeps them all at the end. That contradicts the
project's same-commit rule, and the consequence is not cosmetic — the drift check
is `git diff --exit-code` and nothing runs it automatically, so a plan shaped
that way leaves **every intermediate commit red** against the only drift check
that exists, and nobody would find out until the sweep.

Ruled before dispatch: each task regenerates and commits its own artifacts in its
own commit, and Task 10 becomes a *final sweep and verification* — run the
regeneration, run the drift check, and assert it comes back **empty**. That
strictly strengthens the plan rather than merely relocating work, because a
non-empty diff at the end is now a finding about an earlier task rather than
routine labour.

It held: the final sweep's drift check was empty, and it is a **positive** result
rather than a vacuous one, because the same check had fired for real one task
earlier when the type-audit report moved. An empty diff needs a positive control,
and this one had a dated one.

## Three of ten planned tasks changed shape or vanished on contact with the code

- **The knowledge gate** survived, but only after a pre-dispatch investigation
  found the shape I had specified could not be built at all: the knowledge store
  has no key shape for an anchor, and adding one runs against a standing
  decision that anchor identity is positional and never serialized. The gate
  moved to room granularity, and shipped with its unreachable branch stated.
- **The containment section** was rewritten outright after a census showed its
  property's only carrier is never a container. The replacement is the owner's,
  not mine, and it is better than what it replaced.
- **The wire field task was dropped**, after the surface task proved that no
  anchor reaches the wire at all — verified against the committed chamber
  fixture, whose narration carries six walk-band nouns and no anchor.

**Each was found by a census or a grep, not by reviewing the spec.** All three
were checkable in one command before any of them was written, and two of them
were caught that way. The third — the four-surface premise — was not, and it is
the one that killed a task, because I listed four channels as peers without
checking which band each one runs in.

## Deferred, with homes

- **The durable half of the arc.** Committed-state preconditions, mintable
  object entities, the open/closed/transparent state machine, and the catch-up
  replay redesign that depends on them. Home: the arc's own next campaign; the
  exhaustive `precondition_reads_committed_state` match in
  `windows/vessel/src/action.rs`, whose doc already names the collision.
- **The lying object.** This campaign ships the filter seam a lie plugs into
  and ships truthful advertisement through it. Home: decision 0349's own
  consequences, and the spec's §3.5.
- **`interior/field.rs`'s hardcoded hearth check.** A real anchor-kind coupling
  in the tree that the structural guard is scoped not to see, and which the
  guard's own doc names as its worked example. Home: that doc comment.
- **Whether the object model obeys interactive-fiction world rules generally.**
  Banked as a proposed fourth sibling to the trope, system and sentence corpora,
  resolved against the object model rather than the concept registry. Home: the
  idea registry row `MAP-if-world-conformance`.
- **The disconnected second property system.** A full substance/quality
  grammar with mass balance in `domains/alchemy`, whose only consumer is
  worldgen and which no live metric reaches. This campaign's property vocabulary
  is adjacent and deliberately does **not** connect to it. Home: the spec's §10,
  risk 4.
- **The arc-numbering collision**, inherited unresolved from two campaigns back
  and untouched here. Home: the spec's §13, item 4.
- **Affordances as an unreliable narration channel.** Routing the offer through
  the same culture/knowledge filter `ask` uses, so a body whose culture has no
  word for a thing cannot be offered its verbs. Depends on The Reticence's
  doctrine arm, which is G3-declined and unregistered — zero rider concepts
  exist — so this is blocked, not merely unscheduled. Home: this list and
  decision 0349's seam.
- **`BarrierState` ships drawn, seeded, pinnable dials with zero effects**
  (`windows/worldgen/src/character.rs`, whose own doc says "dials only, no
  effects"), and nothing reads `barrier_of` outside its own tests. It is the
  obvious input to the next campaign's restricted passage, and this campaign
  deliberately did **not** become its first consumer. Home: that doc comment,
  and the spec's §9.
- **`Session::delve`'s dead sealed arm** keeps live code on an unreachable path
  — zero of 48,316 caves sealed over thirty worlds — behind a two-directional
  tripwire that reddens the moment sealed becomes possible. The next campaign
  trips it deliberately. Home: `delve_has_two_distinguishable_outcomes` in
  `windows/vessel/src/session.rs`.
