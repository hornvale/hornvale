# The Compendium

A compendium is a *weighing together* — Latin *compendere*, the root that also
gives *compensate*. It abridges by putting unlike things on one scale, which
means its whole value rests on the scale being honest about what it can and
cannot weigh.

[The Repertoire](./the-repertoire.md) built a capability probe against Georges
Polti's dramatic situations, and [The Collation](./the-collation.md) gave it a
second column. Both ask one question: can a **world** represent a situation?
This campaign opens a second family asking a different question of a different
subject — does a **program** implement a capability? — and reads its first
catalogue, Herbert Wolverson's *Roguelike Tutorial — In Rust*, backwards, as
seventy-four numbered pages of things a roguelike is expected to do.

## Why the existing resolver could not take it

The trope resolver scores requirement bundles against the **concept registry**:
predicates, concepts, phenomena — the sim's ledger vocabulary. That substrate
cannot carry this catalogue, and the failure would have been quiet rather than
loud.

Roughly a third of the tutorial is renderer work: colour, a sidebar, particle
effects, bloodstains, console layers, a menu drawn from a paint-program asset.
None of it has a concept-registry token and none of it ever will, because
[decision 0022](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0022-sim-emits-data-clients-render.md)
puts rendering outside the ledger deliberately. Resolved against the registry,
every one of those pages would have read *blocked by a dangling requirement* —
a number that would have been plausible, near zero, and a category error.

So the two families are siblings, never members of one another, and
[decision 0134](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0134-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md)
settles it: different subject, different resolution substrate, one resolver
each.

## An unmet capability is not one fact

The trope family needed three verdicts. This one needs five, and the split is
the entire design rather than a refinement of it. An item Hornvale does not do
may be one of three completely different things:

```
  present       Hornvale does this            -> cites a MECHANISM anchor
  refused       Hornvale deliberately won't   -> cites a DECISION anchor
  deferred      planned, not built            -> cites a REGISTRY anchor
  absent        a genuine hole, nothing claimed-> cites NOTHING
  inapplicable  about the tutorial's toolchain -> cites a REASON
```

Chapter 2.6, *Dealing Damage*, is the case that forced it.
[Decision 0070](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0070-wounds-commit-health-folds.md)
rules that no stored, mutable health value may exist anywhere — wounds commit
and vitality folds over them — and closes by ordering combat explicitly *after*
that model, because combat built first would invent the counter. An instrument
with a two- or three-valued vocabulary files that as *missing*. It is not
missing. It is refused, on the record, with a reason and a sequence.

## The anchor is the evidence

Every verdict except `absent` cites an anchor, and the resolver checks that the
anchor still resolves. All three kinds resolve without building anything: a
decision anchor is membership in the generated in-force index, a registry
anchor is a parsed row, a mechanism anchor is a test name or a path found by
scanning sources. Four conditions turn the artifact red:

```
  DANGLING        the anchor stopped resolving — a decision superseded,
                  a registry row deleted or renamed
  STALE-DEFERRED  the verdict says `deferred`, but the cited row now reads
                  `shipped`. The verdict has become a lie.
  UNJUSTIFIED     a non-`absent` verdict with no anchor — a parse error
  NOVELTY         the `absent` count rose against the committed artifact
```

None of the four is new machinery. Superseded decisions are absent from the
in-force index by construction. Registry IDs already parse, for the drift check
that guards the registry itself. `STALE-DEFERRED` is the mutation guard's
stale-declaration verdict, exactly — a one-directional acknowledgement can only
ever be satisfied, so it rots, and the fix is a check that fails the moment
somebody adds the missing thing. `NOVELTY` is the ratchet shape the trope
audit, the type audit and the duration baseline all already use.

What this buys is stated most concretely as a prediction about the future.
When the action clock ships, the row it is deferred against flips to `shipped`
and every page deferred against that row goes red until a human re-reads it.
When decision 0070 is superseded by a combat decision, chapter 2.6's refusal
goes red the same day. A hand-authored coverage table does not go stale so much
as begin to **lie**; this one cannot, because each cell points at something
checkable instead of asserting.

[Decision 0135](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0135-a-coverage-verdict-cites-a-checked-anchor.md)
ratifies that rule and states its price plainly: the idea registry stops being
prose that humans read and becomes a gated interface, where a row's identifier
and status are load-bearing for a committed artifact.

## The reading

Seventy-four numbered pages, four sections, one column:

| verdict | count | share |
|---|---|---|
| present | 26 | 35% |
| refused | 12 | 16% |
| deferred | 21 | 28% |
| absent | 10 | 14% |
| inapplicable | 5 | 7% |

**The first page Hornvale cannot replicate is 2.6, and it reads `refused`.**
The world climbs the tutorial's opening ladder without a gap — entities and
components as ledger subjects and committed facts, a walked map that is a
sculpted planet, symmetric shadowcasting for field of view, creatures derived
from the world's own demography — and stops at a ratified decision rather than
at a hole.

That sentence is the single most useful thing this catalogue produces, and it
is only available because the tutorial declares itself **ordered**. Its items
form a pedagogical ladder where each chapter assumes the one before it. A
feature list has no such order, and *first unmet item* asked of one would be an
artefact of sorting. So the corpus states its own ordering and the resolver
refuses ordinal claims for a catalogue that does not, rather than silently
ranking by identifier.

The rest of the reading is mostly a map of where the project's positions live.
Twelve refusals resolve to just two decisions — 0022 for the renderer third,
0070 for everything downstream of a damage counter. Twenty-one deferrals
concentrate on four planned capabilities the registry already held: an object
genus, admissible derived map builders, underground settlement, and knowledge-lit
mapping. Not one new registry row had to be minted to explain a deferral, which
is a stronger result than it sounds: it means the backlog already knew about
every gap the catalogue found that anybody had planned for.

## The surplus, and what it does not say

The trope matrix's most valuable table is its *demand* read — the catalogues
scored against each other, where disagreement is the finding a single column
cannot carry. With one column here, the analogue runs the other way: what does
the catalogue never think to ask for?

Enumerate the domains and the presenting windows; any subsystem no `present`
verdict cites is **surplus**. Twenty of twenty-five. Astronomy, paleoclimate,
language, religion, culture, history, demography, settlement — the tutorial has
no vocabulary for any of it, and the five subsystems it does reach are the ones
a walked, rendered game touches.

The read is derived on every run from the corpora and the live directory tree,
never authored, so it moves on its own as domains land. It is also coarse, and
the artifact prints that limitation directly above the list rather than
below it: the most-cited subsystem carries thirteen anchors and therefore does
not appear as surplus, which reads as full coverage and is nothing of the kind.
Thirteen anchors are not thirteen anchors' worth of a crate's surface, and this
instrument does not measure that surface at all.

## What the instrument is least entitled to

In the trope family, requirements were mapped onto the bundle vocabulary by a
model reading wiki prose blind — a weak authority, but an *independent* one.
Here the verdicts are authored by us, about ourselves, which invites
self-flattery structurally rather than accidentally.

The anchor requirement mitigates it unevenly, and the report says so above its
own headline. `refused` and `deferred` are strongly checked: a decision must be
in force, a row must exist and must not read `shipped`. `present` is weakly
checked — a path that exists is not a working feature, and a resolvable test
name is not proof this page's capability is met. Preferring a test name over a
path narrows the gap without closing it. So `present` is the verdict this
instrument is least entitled to, printed as such immediately before the tally
it most affects, and eight verdicts carry an explicit *arguable* qualification
that travels into the rendered table rather than living only in the source.

The strongest conceivable version of this instrument would run each chapter as
a scenario against a live session and read the verdict off a transcript —
behaviour rather than assertion. It is refused here on two grounds: it costs
more than the rest of the campaign combined, and it still cannot cover the
renderer third, which has no transcript to read. It is recorded as a follow-up
scoped to the pages where a transcript would actually discriminate, not as a
vague ambition.

## What it does not claim

This is **reach against one catalogue**, and the catalogue is a teaching
sequence for building one particular game on one particular library — not a
specification of what a roguelike is. Its later sections are one game's content
design. A second column is the point of the exercise and is deliberately not
this campaign's work; the corpus format, the resolver and the matrix all take N
columns from the first commit, because a format that could not accept a second
catalogue is the thing this family would most regret shipping.

Thirty-five per cent is therefore not a score. It is one reading through one
declared bias, and the number that matters on this page is not the percentage
but the identifier: the first page this project cannot replicate is 2.6, and
the reason is written down.
