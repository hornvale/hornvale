# The Governor

*A governor does not make an engine faster. It decides how fast the engine is
allowed to run, and stops it running away.*

The heavy tier is the set of tests too expensive to run on every commit —
live-worldgen batteries that build dozens of worlds each and measure something
about them. At the start of this campaign it ran 118 tests in 1,551 seconds and
ten of them were red. At the end it runs 64 tests in about 449 seconds and
none of them are red. (The measured run was 63 tests in 449.219 seconds; the
campaign's final review restored one more, which passed at 408 seconds on its
own in an earlier run of the same tier. What it adds to a parallel wall whose
pole sets the pace is not that number, and is not yet measured.)

**Neither number was the problem.** The problem was that nothing ran the tier.

## A life cycle with a broken joint

An expensive check has a life: it is authored, deferred out of the commit gate
because it is expensive, dispatched by something on some cadence, read,
attributed to whatever caused it to move, and discharged — after which it decays
back toward the beginning and is authored again in some new form. Decision 0148
removed the heavy tier and the mutation checker from the merge's phase list,
because together they were 80.5% of a merge's wall time. On its evidence that
was plainly right, and the four-phase merges that immediately followed confirmed
it.

It also removed the only automatic thing that ever moved the tier from
*deferred* to *dispatched*. Nothing replaced it. From then on the tier ran when
a human remembered, and the tier's own cost ledger records how often that was:
no bare heavy row reached it between 5 August and this campaign — twenty-seven
runs, invisible to the ledger that exists to record them, because the dispatcher
writes its row into a scratch checkout nothing commits.

By the time someone ran the tier by hand, at the close of the previous
campaign, ten of the 118 were failing. The natural reading of ten failures
arriving at once is that the campaign in front of you caused them. It had caused
one.

So the campaign's question was never *how do we make this fast*. It was **what
re-enters the cycle at the dispatch joint, and what must the tier cost for that
dispatcher to be affordable.** Cost was instrumental, which is why the cost work
came first and the gating decision came last, against a measurement rather than
a projection.

## What the levers were worth

```
                     BEFORE (2f8faf243)   AFTER (da03b576a)
nextest wall           1551.631 s           449.219 s       3.45x
tests                  118                   63*
failures                10                    0
cpu_ratio               13.88                20.87
```

*\* The roster is 64 after the final review restored a byte-golden drift check
that had been demoted; the run above measured the tier as it stood at that
commit.*

One lever was rejected before anything was built. Four probe binaries each
declare the same twelve-seed panel and build the same twelve worlds
independently: forty-eight redundant world builds. Measured against a control
whose per-seed cost is dominated by its build rather than its measurement, a
world build is about 10.7 seconds, so the redundancy is roughly 515 CPU-seconds
of a 12,280-second tier — about 4% — and removing it would require an on-disk
fixture, because the test runner is process-per-test and no in-process cache can
survive between two tests. Worst ratio in the campaign; recorded so it is not
proposed again.

**Front-loading the barrier.** One battery reserves the entire runner, so the
scheduler drains every running test — including the 891-second pole — before
starting it, and then restarts the remaining seventy-one from cold. The optimum
for that roster is the sum of the exclusive tests plus the larger of the pole
and the total CPU work divided by cores; the gap between that optimum and
reality was 484 seconds, stable across two runs, and larger than any single test
except the pole. The fix is a scheduling priority in a configuration file: pay
the barrier at time zero against an empty pool, rather than after the pole
against a full one. It moved the tier from 1,551.6 to 1,206.3 seconds — 345
seconds from a change that touched no code at all.

It also falsified the arithmetic used to predict it. The projection assumed each
test's duration is a constant that scheduling merely rearranges. It is not: with
everything now running concurrently from the start, the long tests got *slower* —
the pole by 15%, its two nearest peers by 18% and 23%. The net saving stood, but
every downstream projection built on fixed per-test durations was optimistic for
the same reason, and the campaign stopped adding stage deltas together.

## The adjudication

The lead lever was not an optimization. Of the 118 tests, most were campaign
instruments: a question a campaign asked once, answered, and pinned — nine of
the ten failures were named as questions, `how_much_…`, `which_way_…`,
`whether_…`. A test named as a question was written to answer that question
once. A cheaper tier already existed for exactly this, decision-backed and
applied to precisely two tests by the same decision that removed the
dispatcher — and then never continued, while 118 accumulated.

There is no mechanical classifier for the distinction, and the campaign refused
to pretend otherwise. The existing tag vocabulary describes whether a seed loop
is a search or a fixed panel — a different axis, and joining it against the
roster confirms it: the five largest failures all carry the same shape tag. So
the rule was an adjudication, applied per test, with a stated question:

> **If this test went red tomorrow, what would we do?** Investigate a regression
> in the world or the program — it is a **witness**, and it stays. Note that the
> number moved and update it, because the question was answered in a campaign
> and the pin is just the answer we recorded — it is a **report**, and it goes.

**The burden was placed on keeping**, deliberately: the tier's failure mode had
been silent accumulation, and a rule whose default is *keep* reproduces it.
Verdicts: 63 keep, 54 demote, one undecided, every one written down with its
reason in a committed table — the kept half being the part a future reader
actually needs, because keeping is the silent choice.

Three disciplines made the demotion something other than a mass silencing.
Every demoted tag names the campaign whose question it answered, or — for the
one test that never answered a campaign's question at all — the convention that
put it in the tier, because a false attribution is worse than a named
convention. Every demoted test that was **currently red** says so in its own
tag, and says why that is acceptable: nine of them do. And no fixture was
regenerated to make a demoted test green, because demotion and repair are
different acts and mixing them would let a real regression leave the tier under
cover of a cost campaign.

**One verdict was reversed after all three populations were closed**, and the
shape of the mistake is worth more than the count it moves.
`occupancy_readout_is_current` was demoted because its failure message says
to rewrite the fixture — the report branch, in the test's own words. But that
reasoning proves too much: the census-fixture comparison that this campaign
kept, and that the gating decision leans on, reads exactly the same way. The
demotion had answered a question about the *message* when the question was
about the *instrument*. It is an exact byte comparison of a freshly rendered
CSV against a committed one — a change detector, not a pinned number — and
its own regenerator calls it "the gate". The decisive fact was structural
rather than interpretive: the fixture it compares against is not a declared
generated path, so nothing else in the tree ever looks at it. The demotion
would have left a committed artifact with no automated witness of any kind,
which was noticed at the time, recorded honestly as a cost, and accepted. On
the final read it was not an acceptable cost, and the verdict rather than the
cost note is what changed.

The keep rate across the three populations ran 17%, 48%, then 90%. The last is
exactly the shape of an adjudication that has stopped pushing, so it was
re-adjudicated blind, including all five of its most expensive members, and
every verdict held with a concrete failing input named for each. Two expensive
keeps in an earlier population did not survive the same treatment: both rested
on harness tautologies — a total that matched because every item lands in
exactly one bucket, a "monotonicity invariant" that holds for any array
including an empty one — and both flipped.

## The two levers do not add

Front-loading bought 345 seconds by reordering a barrier that the demotion then
largely deleted: 177 seconds of exclusive barrier became 29. The levers overlap,
and the campaign's saving is therefore an end-to-end measurement rather than a
sum of stage deltas. That composition is also why the result beat the spec's own
projections — the demotion removed the barrier's *source* rather than merely
scheduling around it, while front-loading still handles what remains.

## Parallelism, and the guard that had to be widened first

The surviving witnesses are mostly serial seed panels: a loop over twelve or
thirty seeds, each building a world. A parallel seed sweep already existed —
scoped threads, results reassembled by seed position rather than completion
order, so output is byte-identical to the serial loop — but it lived inside one
crate's test directory, reachable from nowhere else.

It could not simply be adopted, because a guard asserts a strict two-way
equality between the configuration's whole-runner class and *every* test that
calls the sweep. Adding parallelism to nine panels would have demanded nine
full-runner barriers and made the tier catastrophically worse. The guard was
correct and widening it would have been the wrong repair.

The amendment is a second, **sized** class. The existing class exists because
some batteries sweep two hundred seeds and genuinely want the whole box; a
twelve-seed panel does not — it wants twelve slots. Both classes keep the
two-way agreement property, checked independently in both directions against
their own marker, and the sized class hard-rejects the unbounded setting so it
cannot be quietly widened back into a barrier.

Three panels were converted, each proven byte-identical against its own serial
result by forcing the sweep to a single thread: 7.2x, 7.5x and 6.4x, with the
test runner's own duration line as the only difference — which doubles as
evidence that the comparison was live rather than vacuous. The helper itself
moved out of the test directory into the composition root, the library where all
domains meet, which is what makes it reachable by every remaining candidate.

## The authoring set went from three to one

The heavy tier is not merely expensive: it is an **authoring path**. Some of its
tests write committed artifacts, which is why it is pinned to one machine —
goldens are authored on one enforced host, and a run elsewhere would commit
values that silently disagree with canonical and then pass their drift check
forever.

[The Siding](the-siding.md) is where that was first written down, and it
states the count in the present tense: *"Three committed artifacts and one
host-sensitive comparison, governed by convention alone."* After this campaign's
demotion, **the tier holds one writer and one comparator** — a history battery
that writes its report, and a probe that compares live census seeds against
fixtures authored on the canonical box.

The Siding's sentence is deliberately not edited. A chronicle entry records what
a campaign found; editing it to match a later tree falsifies the record, which
is the same argument that makes this project's decision log append-only —
supersede, never edit. So the correction lands here instead. **The book's latest
word on the tier's authoring set is this entry, and the mechanism by which an
append-only book stays honest is not revising its earlier pages but making sure
its newest one is right.** A reader who stops at the older chapter overstates the
tier by one artifact; a reader who reaches this one does not.

Two artifacts were orphaned by that demotion, and the campaign says so rather
than discovering it later: a scaling-exponent report and a set of sample
biographies now have no producer inside any gated path. Nothing asserts against
them, so nothing reddens; they simply stop being regenerated until someone runs
the demoted test by hand.

## A test freed to stop describing itself falsely

A guard requires every heavy tag's reason to be one verbatim sentence —
*"live-worldgen battery; deferred from the commit gate to the heavy set"* — so
that membership in the tier cannot be blurred by prose. The rule has a cost
nobody had priced: **a test that is not a live-worldgen battery must call itself
one in order to be in the tier at all.**

A wall-time micro-benchmark of the ledger's commit path is such a test. It
builds no world and draws no seed; it measures whether an indexed commit beats a
naive linear scan. Its own source comment says it carries the sentence only
because the guard demands one. The cheaper tier carries no equivalent guard, so
its reasons are free-form, and the benchmark's tag now reads what it is: *a
wall-time micro-bench, not a live-worldgen battery*. Two other demotions from
the same population — a scaling-exponent sweep and a preregistered readout whose
question a named campaign closed — likewise now name themselves rather than
borrowing a description.

Nothing budgeted for this. It is worth stating because a uniformity rule that
buys auditability by forcing false self-descriptions is a trade rather than a
free lunch, and the trade only becomes visible when something leaves.

## The last red had a name, and it was not this campaign's

Nine of the ten failures left the tier as adjudicated demotions, each carrying
in its tag the fact that it was red and why that was acceptable. The tenth
stayed: a test that says in its own name that it is a witness, pinned at twelve
of sixty and reading ten.

Bisection named the cause. Four days earlier a merged campaign bumped a stream
epoch for the deep history bake, because sub-year placement moves *when* within
a year a raid fires — a deliberate, documented consequence of a shipped feature,
which moved a shipped world value. That campaign passed every gate it was asked
to pass. It re-pinned several other witnesses and never saw this one, because
nothing showed it. The witness was re-pinned with the bisection recorded beside
it, and the earlier campaign's claim that the failure predated it was re-derived
rather than inherited.

That is the defect in one sentence: **a campaign can move shipped world values,
pass every gate, and leave the tier red for the next campaign to inherit and
mis-attribute.** It is an author/inheritor asymmetry, not a cost problem, and no
amount of making the tier cheap fixes it.

## What it costs to close the joint

Decision 0426 puts the tier back on the merge phase list, last, and only there.
A merge goes from about 1,130 seconds to about 1,605 — roughly 475 seconds more
per landing that is not pure prose, on the one strictly serial box, which at the
tempo actually measured is on the order of forty-five hours a month. That number
is stated at its true size because the first draft of the record stated it at a
fifth of that, having divided a twelve-day sample by two months.

The cost is not what carries the decision. A predicate that ran the tier only
when a change reaches world-generating code would save roughly a fifth of that,
and was the serious rival — but seven of the 64 surviving tests live in the
crate that holds the tier's own harness, and the harness itself sits outside the
world-generating layers entirely. A predicate written from the layering diagram
would exempt exactly the changes most able to break the tier, and nothing would
say so. Trading a real saving against not building a mechanism that can be wrong
silently is the trade the decision actually makes.

The stage gate deliberately does not run it. The census-fixture comparison
inside the tier expects the committed census to be current, and the census is
refreshed once per campaign at the pre-merge close — so on a stage gate that one
test would red predictably from a campaign's first moved value until its last
day, for a reason that is expected, benign and unfixable at that moment. **A
gate that reds predictably for a known-benign reason trains people to ignore
it**, which is precisely the disease this campaign diagnosed. At merge time the
census has already been refreshed, so the cadence the test assumes and the
moment a merge occupies are the same moment.

Finally the roster itself is frozen in a committed fixture, checked in both
directions, so adding a heavy test now requires editing that fixture in the same
commit — a visible, reviewable act rather than a tag nobody sees. Membership was
chosen over a wall-clock budget on purpose: a committed duration is a claim with
a date and decays, while roster membership does not. Under the old phase list
the ratchet could only make an addition *visible*; a tag nobody's gate ran cost
nobody anything. Under the new one it **prices** an addition, because appending
a line charges every subsequent merge for the test it admits.

## What this leaves open

Three residuals ship knowingly. The sized-sweep class reserves thirty of forty
slots, so its three members cannot co-schedule with each other; that cost is
inside the 449-second measurement rather than separated from it. The repair to
the lost-timings-rows defect cannot be exercised before it merges, because the
dispatcher runs the canonical checkout's copy of its own script — which is
main's — so a fix riding on a branch never runs; the first real test of it is the
first hand-dispatched run after this lands. And the browser-client phase, now the
largest phase of a merge, has no roster ratchet at all: one campaign added
roughly a hundred tests and half again its CPU-seconds to it in a single merge
with nothing remarking on it, and the argument this campaign makes about an
unpriced tag applies there word for word.

The larger open question is whether the tier needs the canonical box at all. The
host guard protects two tests out of 64. Cross-platform byte-identity has
already been measured once — forty worlds, every metric, identical between the
two architectures in use — and if that holds for this tier's pins, the other
sixty-two could run anywhere, concurrently, never taking the serial claim. That
would make the gating decision nearly free. It is the shape of an inherited
constraint that ages into an apparent requirement after its producer is removed,
and it deserves its own probe rather than an assumption.
