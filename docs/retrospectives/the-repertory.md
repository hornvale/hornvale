# The Repertory — retrospective

Process, not product. The chronicle carries what was built.

## The design came from the owner's sentence, not the controller's framing

The campaign opened with a fork the controller had already framed and was
ready to recommend on: source the corpus from found interactive fiction, or
from authored original scenes. An ideonomy pass killed the fork outright. The
`intentionality` dimension-prompt (designed / emergent) mapped straight onto a
sentence the owner had written an hour earlier — *"we should be able to
explicitly author anything that we want to be able to generate
procedurally"* — and exposed it as a claim about a **gap** rather than one
about a source. Found and authored are positions on one axis, measuring
opposite things.

The lesson is about where to look, not about ideonomy. The load-bearing
sentence was already in the conversation, stated plainly, and the controller
had read past it twice — once summarising it as vision, once as a
justification for a corpus. It became the instrument's whole architecture the
moment it was read as an engineering claim.

## Fixing the listed instance did not immunise its neighbour

The plan's self-review found that the floor-ratchet test iterated a single
corpus file, so a scene in a second file would sit in the floor table with
nothing enforcing it. The plan closed that instance.

During execution the **well-formedness** test turned out to have exactly the
same shape, one screen away in the same file, and the plan had not touched it.
It was found by looking, not by the plan.

This is the recurring failure the project has already named: a carried-findings
list reads as a checklist, and the remedy discharges the listed sites rather
than the class. The durable fix here was structural rather than diligent — a
single `every_committed_scene()` helper both tests iterate, so a third test
cannot reintroduce the hole by being written normally.

## Every assertion was run before it was written down

All four founding scenes were executed against the witness world, and their
pointers compared before and after, **before the plan was drafted**. The plan
therefore states measurements (`/self/room` 890961927 → 890962023 on `go e`;
`/day` 0.0 → 1.0 on `wait 1` with the room unchanged) rather than predictions.

One of those runs paid for itself immediately: `go e` advances the *day* as
well as the room. An assertion that walking leaves the clock alone is the
obvious thing to write and would have been wrong, and the plan carries an
explicit warning against writing it.

## Three cuts, one defect shape

Self-review removed three things, and they only looked unrelated:

- spec section 7's `participants` array — no task read it;
- spec section 9's `PARTIAL` verdict — no code could produce it, since
  evaluation stops at the first failing beat;
- beats 3-7 of `the-orange` — no snapshot surface to assert against.

All three are the same defect: **a record nothing reads is a declaration**,
which is precisely what this corpus family was founded to avoid inheriting
from its sibling. Naming the class found all three; a list of one would have
found one. Worth noting that the spec had shipped all three past a hard-stop
human review, and it was the plan's mechanical *can you point at the task that
implements this* pass that caught them.

## A test can encode a claim about the absence of a name

The resolver's scene-to-verdict function, first called `verdict`, reddened a
guard belonging to a different campaign. That guard's doc comment states its
premise outright: *the bare word `verdict` names no function anywhere in
`cli/`* — which is how it manufactures a symbol that is a strict prefix of a
real one, to prove its own prefix matching is not naive.

Nothing was defective in either file. A new symbol in a new test file
falsified a premise recorded in a comment elsewhere in the crate. Renaming the
newcomer was obviously right; weakening the older guard to accommodate it
would have been the cheap repair that deletes the check. Absence is the one
property a newcomer cannot see they are about to destroy, and the only defence
observed working here was that the premise had been *written down* in the
guard's doc comment.

## A loop over non-seeds trips the seed-loop lint

`claim_shape` flags any test that iterates, and demands a declared claim shape
(decision 0093). Iterating corpus scenes is not iterating seeds, but the lint
cannot tell, and the honest response was to declare the false positive with
its reason — for which precedent already existed in `branches_identity.rs`.
Anticipating it for the remaining tests cost nothing; discovering it cost one
failed commit.

## Followups

- **`REACHED`** — recognition of a scene in an *unscripted* run, its rate over
  seeds, and the second ratchet. The instrument is half-built without it.
- **`PARTIAL`** — needs an evaluator that continues past the first failing
  beat, so "never started" and "beat 4 failed" become distinguishable.
- **The witness search** — witnesses are hand-found today. `scout` is the
  right precedent but scans astronomy pins at genesis and cannot answer
  creature co-location; the search runs at full world cost, and bounding it is
  unsolved.
- **Prose assertions** — v1 is structural only, deliberately, to avoid the
  golden-string trap. A scene needing one is a deliberate extension of the
  closed assertion vocabulary, not an improvisation.
- **Beats 3-7 of `the-orange`** — each needs a snapshot surface before it can
  be encoded at all. Beat 2 (object instances and possession) is the next
  campaign's acceptance criterion.
