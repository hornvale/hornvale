# The Lot — retrospective

*Process lessons. The chronicle carries the product story; the spec carries the
design; the campaign ledger carries the seventeen rulings.*

**Merged:** 2026-09-05

## One design, three corrections, each from running rather than reading

The person-years tally was corrected three times, and no correction came from
re-reading anything.

1. The plan said "mirror both `deepen` calls". It was asserting completeness,
   and completeness is what an enumeration gets wrong: a community changes seat
   through four further paths that re-open it, and the brief's design left 159
   of seed 42's 1,212 occupations at zero. **What caught it was the brief's own
   zero-count test** — the one thing in that brief that was right by
   construction (ledger #11).
2. The review then found that the six per-site credits *double-count* across a
   same-epoch handoff, and confirmed it on the committed fixture rather than by
   argument: the epoch loop grows every living community before it closes some
   of the same ones, so a community closed at year Y had already been credited
   for the epoch starting at Y, and its successor opened at Y was credited for
   the same window. The design became one sweep at the end of the epoch loop
   (ledger #12).
3. The implementer then found the case that ruling did not cover — 45
   occupations that open and close inside a single epoch, so they have a
   positive tenure and are alive at no sample point at all — and refused to
   proceed until the invariant was restated on epochs survived rather than raw
   tenure.

Each step was correct given what it knew, and each was overturned by an
observation the previous step could not have had. **The general form: a
sampling convention is not a detail of an integral, it is half of what the
number means, and the only way to find its boundary cases is to run it against
a real world.**

## The read-side fan-out, before the plan, overturned three rows of the spec

The spec's slot table named `Occupation.tongue`, `Occupation.deity` and five
climate kinds as sources. All three were asserted from *field names*. Grepping
the readers found that the first two are written as absent by the bake and
never committed by anything — the almanac's own source says so — and that the
climate kinds are phenomenon kinds, not facts on a settlement (ledger #8). The
correction cost an hour before any code existed; discovered during Task 6 it
would have cost a fix round, and shipped it would have produced silences that
are real for the wrong reason and indistinguishable in the output from silences
that are real. **A spec's claim about a data shape is a hypothesis until the
readers have been counted.**

Two per-task disciplines earned their cost independently. The per-task
**read-side fan-out** before writing the plan produced the pre-flight conflict
table that caught a duplicated epoch-length constant and a signature mismatch
before either was written. The **three-minute verification at each dispatch**
caught the plan's own defects one task ahead — the sky-argument shape, the
missing composition-root accessor, the constants that had to be made public
rather than mirrored.

## Measure a gate on the host that enforces it

The exhibit's wasm was built into the shared catalog and measured on the
authoring machine at 6.4% under its size gate. That reading was not wrong; it
was taken on the wrong machine. The canonical box's older compressor emits some
11–13% larger output, which projects the same artifact *over* the gate on the
one machine that actually runs it, and the gate's own comment forbids raising
it. The fix was structural rather than numeric — the exhibit took its own
crate, ungated, on the possession exhibit's established precedent, and the
catalog reverted to its pre-campaign weight (ledger #16, #17). **A
host-dependent threshold measured on the wrong host is a green light with no
bearing on the light that matters**, and the campaign spent one fix round
chasing a dependency edge for a growth that measurement then showed was not
where the edge was.

## A new crate cannot pass the commit gate — a standing process finding

The sub-floor roster coverage guard refuses a crate with no roster row, and only
a green run on the canonical box writes rows. The guard's own doc recommends
adding a new crate *without* a declaration and letting the first such run
populate the roster; that recommendation is unreachable, because the crate
cannot be committed in order to be gated. The declared-absent route is worse
than unreachable: the merge's roster rewrite lands the rows in the same commit
as the declaration, making it declared-and-present, which the guard reddens on
the main line. The working route is hand-authored provisional rows, one per
test, under a comment naming the ruling; the next green run overwrites them
harmlessly, because the roster's only reader turns a row into a test-name
filter (ledger #14). This is filed as a process finding, not a workaround: **a
guard whose recommended remedy does not exist is a guard that teaches people to
route around it.**

## The absorption, and the shape of what it broke

Main gained 209 commits mid-campaign, and the stage gate was refused at the
mouth before the box was ever taken — which is the mechanism working. The
auto-merge was clean and the tree did not compile: four signatures had moved
under the campaign's own new code (a dropped argument at the composition root,
an accessor that no longer returns an option, and the dead branch that option
had existed for). Nothing conflicted, because nothing overlapped textually. The
fallout review came back clean on all six checks. **A clean auto-merge is
evidence about text, never about premises** — the compiler was the enumeration
here, and it was worth running before believing the merge.

## Two smaller things worth keeping

**A transcription error travels.** The plan quoted 1,240 occupations for seed 42
from a comment in the bake's own source; the probe and the world both say
1,212, and the fact-count constant moved by exactly that. It had no code effect
this time. It was still a number in a brief that nothing had checked.

**The controller's shell working directory drifted to the main checkout once**,
and what saved it was a failing assertion in the command it was about to run,
not vigilance. Re-anchoring with `pwd` before every commit and every
verification is cheap; the failure it prevents is an edit landing on the wrong
tree, silently, at a moment when three sessions are running.

## Do differently next time

- When a fact is an integral, write the sampling convention into the spec
  beside the fact, and derive its boundary cases (zero-length tenures,
  handoffs) *there*, before the first implementation.
- Fan out over the *readers* of every field a spec names as a source, before
  the plan, not before the task.
- Take any host-dependent measurement on the enforcing host, or state in the
  same sentence that it was not.
- When a new crate is involved, budget the sub-floor roster rows as a step of
  the crate's first commit rather than discovering them at the gate.

## Deferred minors, and where each landed

Twelve minors were deferred by the per-task reviews (campaign ledger #19,
backfilled at the close — they should have been ledgered as they occurred).
Six were fixed in the final review's wave (the history test's hardcoded
start year, the non-positive-lifespan guard, the undisplayed-longitude
citation, the page's prose caveat, and the two Important-class citation and
rounding defects the wave was for); five are accepted with a reason written
beside each in the ledger; one is carried forward — the exhibit's axis and
pick toggles carry no `aria-pressed`, a five-line fix for the next touch of
`clients/lot/`.
