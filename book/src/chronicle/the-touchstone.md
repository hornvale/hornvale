# The Touchstone

A touchstone is the assayer's stone. Rub a coin against it and the streak it
leaves tells true gold from fool's gold — a test that gives a different answer
for two things a glance cannot separate. The myth thread has spent three
campaigns holding two very different things a glance could not separate: a
transmission mechanism that rewrites what a large fraction of the world
believes, and one that changes nothing at all. Both left the same streak. This
campaign built a stone that does not.

The thread reports its results through one number. Every holder, derived from
committed facts alone, believes some remembered day for each event; the
**divergence aggregate** — `DivRow::mutually_exclusive` — counts the
cross-people endings on which the victim's people and the raider's people each
hold a day the other holds nowhere. It is the thread's headline, and three
times running it has watched a mechanism rewrite beliefs across thousands of
communities and barely twitched: the [Undertow](./the-undertow.md)'s crossing
penalty changed what was held at 0.10–0.25% of 1.2 million holders
and moved the aggregate by two endings of 421; a selection-rule swap rewrote
the held telling at up to 41.9% of holders and moved it by four of a hundred.
Seen once, that is a fact about a mechanism. Seen three times, it is a fact
about the instrument. This campaign does not add a fourth mechanism. It builds
the measure the last three needed and did not have.

## Why one number is blind, three times over

The aggregate is not merely coarse. It is blind for three reasons that
compound, and naming them separately is what tells you the fix is a new
instrument rather than a bigger sample.

**Homogeneity.** The aggregate is one scalar for a whole panel — a count over
a population. The holders a mechanism moves are a *tail*, and a count that
folds the whole distribution into one integer cancels the tail against the
unmoved majority. This is the Undertow's own scale-probe lesson stated at the
panel level: a ratio of medians cannot see a tail, and neither can a single
count.

**Decomposability.** The aggregate fuses everything a holder might believe
into one cross-people day-set question. Whether a holder now believes through a
different witness, remembers a different day, at a different rung, at a
different depth — all of it collapses into one bit about two peoples' day-sets
agreeing or not. A change can move four of those five components and leave the
fused bit untouched.

**Materiality.** This is the sharpest and least visible of the three. The
aggregate reads only the *emitted* disagreement — the remembered days two
peoples end up holding. But a holder's remembered day is floored through the
ladder of the *originating witness's* people, and two peoples assign different
spans to the same rung. So a change that flips which route wins can re-floor a
holder's remembered day at an unchanged rung index, and can change *which
witness a holder believes through* with no change to the emitted day-set at
all. Route identity is load-bearing even for the day channel — and route
identity is precisely what the shipped walk throws away.

That last fact is what made the measure both necessary and half-built already.

## The measure was buried in a test file

The shipped relaxation, `variants_about_accumulating`, carries the winning
witness as the third element of its ordering key through the entire best-first
walk — and then drops it, returning only the `Claim`. The remembered day, the
rung, the hops and the grade survive; the **witness/route** and the continuous
**width** do not. Because of that discard, the Undertow's readout had already
been forced to ship a *private copy of the whole walk*, whose richer record
carries the route the library dropped, and to pay a standing tax for it: a
heavy-battery control asserting the copied walk matches the shipped walk
holder-for-holder, forever.

So the thread's needed instrument was not missing. It was buried in a test
battery, duplicating determinism-critical code, and paying rent. This campaign
promoted it into the library as first-class, unit-tested code — a **traced
walk** (`hornvale_hearsay::traced`) returning, for every holder, the route and
width alongside the `Claim`; and a **belief-delta** module
(`hornvale_hearsay::touchstone`) that diffs two transmission arms into a
per-holder vector of component-change flags — `{route, day, rung, hops,
width}` — and rolls them up into a distribution over holders with an explicit
tail mass and a people-pair cut. The shipped walk was left byte-identical: the
traced walk is a sibling, guarded by the same agreement test the readout used
to pay, now moved into the library where it belongs. Full unification — making
the shipped walk a projection of the traced one — is the cleaner end state and
is deferred, on the standing principle that a determinism-critical refactor
does not belong inside a measurement campaign.

## The discrimination the instrument had to pass

An instrument that fires on everything is as useless as one that fires on
nothing; both fail to separate. So the touchstone was held to a **preregistered
discrimination**, frozen before the measurement code existed: it is valid iff
it separates a change that rewrites beliefs from one that provably does not, on
the same panel where the aggregate reads near-zero for both. Both controls were
frozen with reachability evidence re-derived on this tree, using a hand-rolled
`Claim`-inequality diff — so nothing in the criterion was tuned by the
instrument that would later be judged against it.

The **positive control** is a working mechanism the aggregate misses: the
selection-rule swap, under the aggregate's own arm and rule. Arm A is today's
shipped selection — smallest accumulated width, then fewest hops, then witness.
Arm B is the `Recency` rule — most hops, then least damage — applied to the
full candidate set the seam delivers to each holder. Re-derived over 3,177
(holder, foreign-ending) pairs, arm B rewrites the held telling at **62.64%**
of holders while the divergence aggregate moves from 10 to 14, a delta of four
of about a hundred. The prediction was a floor of 20%; the churn clears it more
than threefold, an order of magnitude above the aggregate's four percent.

The **negative control** is a genuinely inert change, provably zero by a
theorem rather than merely observed to be small. Under `Contact::Descent` the
only route is descent; the seam is never consulted. For a holder whose entire
ancestry chain is one people, every descent step is within that people, so the
crossing penalty's `from == to` early-return fires on every step under *both*
the `Free` and `ContactWeighted` arms. Identical width follows, and identical
rung, day, hops and route follow from that: the two configurations produce
bit-identical claims for every such holder. This is a code theorem, and it is
non-vacuous — the panel carries **2,014** people-homogeneous-ancestry holders,
and the probe asserts every one of their claims bit-identical across the two
arms. It is also mutation-proven: deleting the `from == to` guard reddens the
theorem, because `ContactWeighted` then charges a full finest rung on every
same-people step, moving the rung and the remembered day. The negative control
is what supplies the ceiling the positive floor needs. An instrument wired to
fire on everything would redden it, so a passing negative is the evidence that a
high positive is signal and not a constant.

## The streak

On the 12-seed panel, over the two frozen controls:

| | reached both | any component moved | aggregate |
|---|---|---|---|
| **positive** (recency swap) | 3,177 | 2,026 — **63.77%** | 10 → 14 (**+4**) |
| **negative** (inert, homogeneous) | 2,014 | 0 — **0.00%** | 15 → 15 (**+0**) |

The positive tail is 0.6377 against a frozen floor of 0.20; the negative tail
is exactly 0.0000 against a frozen ceiling of 0.01. The touchstone separates a
working mechanism from an inert one by more than sixty percentage points, on
the same panel where the divergence aggregate separates them by four endings
against zero — that is, does not separate them at all. It is not a
falsification: the instrument sees exactly the dissociation the aggregate
cannot.

The per-component breakdown of the positive control is the decomposability
argument made real — route moved for 61.72% of holders, day for 38.37%, rung
for 33.71%, hops for 62.64%, width for 59.55%. The day channel moving less than
the hops channel is the materiality effect of §1 turned into a measured number:
a re-floored day at an unchanged rung is a route effect a rung-keyed measure
would miss.

## Two honesties the thread has paid for before

**The instrument reads more than a `Claim`-diff, and that is correct.** The
touchstone's 63.77% slightly exceeds the route-blind `Claim`-diff of 62.64%
that the frozen probe measured over the same population. The excess is not
over-counting. The `Claim` carries the day, rung and hops, so its diff catches
those; it does not carry the route or the width, which live outside it. The 36
extra holders the touchstone flags are exactly the ones whose winning witness
or accumulated width moved while their `Claim` did not — the instrument seeing
what a `Claim`-diff *cannot* see, which is the entire reason it was built. The
day and hops channels reproduce the `Claim`-diff to the digit (1,219 and 1,990
holders); the surplus comes only from the two channels outside the `Claim`.

**The prior 41.9% value signature did not reproduce, and it was not rescued.**
The Undertow measured a selection swap moving the *value* of the held telling at
41.9% of holders. On this tree the strongest value-change is recency's 38.37% —
under 40%. The substrate has moved since that figure was measured; settlement
placement shifted under the thread since 41.9% was last measured. This was recorded as a
finding, not repaired: the frozen 0.20 floor was **not** lowered to flatter the
result, and the discrimination clears the unlowered floor by more than
threefold. The dissociation *property* — a selection swap rewriting a large
fraction of held tellings while the aggregate barely twitches — reproduces
robustly; only the exact magnitude drifted, which is the thread's own standing
lesson that a committed baseline is a claim with a date.

## What this leaves

The blocker on the myth thread's corroboration half was, after the Undertow,
no longer a missing mechanism but a missing measure. The measure now exists,
lives in the library rather than a test file, is proven component-by-component
by mutation, and has passed a preregistered discrimination that the aggregate
provably fails. The thread keeps building mechanisms its own headline number
cannot see; this campaign fixed the number so the next mechanism can be
believed.

The natural next campaign is the one the touchstone is a precondition for —
whether a crossing penalty whose magnitude is *derived* from the world's
contact history does anything a well-chosen constant would not. That question
presupposes an instrument that can see the penalty at all, which is what this
campaign supplies. Three followups are banked as tooling rows: unifying the
traced and shipped walks into one, promoting the discrimination readout into a
committed drift-checked artifact so the numbers live in prose rather than being
re-derived each campaign, and consolidating the three surviving copies of the
enumerate/select machinery that the probes still carry.
