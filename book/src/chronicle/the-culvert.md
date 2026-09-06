# The Culvert

A culvert is the one passage everything above it drains through. Many
surface flows, one pipe, laid once. That is the shape of this campaign: three
hundred creatures asking the same route question four thousand times, and one
search answering all of it.

Nothing a world does changed. Every committed byte — worlds, almanacs, scenes,
fixtures — is identical before and after, and the argument for that is
structural rather than statistical. What changed is how many times the same
question is asked of the same unchanging geometry.

## The function was purer than the brief believed

`believed_water` ranks a creature's remembered water rooms by how far each one
is from its home, and returns the nearest. The ranking calls a budgeted graph
search once per remembered room, once per read, and consumes only the returned
plan's **length**.

The campaign's own brief specified a memo keyed on `(home, water room, hazard
set)` and warned that the hazard set is an input, so the key must carry it. Two
thirds of that sentence was false against the code and a count falsified the
rest.

Three readings, each from the source rather than from the prose around it:

- All three belief folds pass a freshly allocated **empty** avoid set. The only
  production caller that passes a real hazard set is the homing cache, three
  functions away. The belief ranking is hazard-blind.
- `NavSpace` holds exactly two fields, a destination and an avoid set. It never
  consults terrain, the ledger, or the tick. So the search is a pure function of
  **mesh geometry alone**.
- `NavSpace::heuristic` returns `0`. It is Dijkstra, not A\*. The registry row
  that had described it as "an A\* with a 1,000-node budget" was half wrong, and
  is corrected at this close.

A memo of a function that is pure over an unchanging mesh is byte-identical by
construction. That is the campaign's whole determinism argument; the testing
confirms it rather than establishing it.

## The counts chose a different mechanism than the reading did

A throwaway probe counted calls, distinct `(home, destination)` pairs, and node
expansions per roster-wide sweep, on two shapes: a fifty-agent, two-hundred-tick
laboratory run, and a real possession session of sixty-seven residents.

```text
  shape        calls   distinct pairs   node expansions   budget-exhausted
  laboratory     679               83           425,042        404 of 679
  possession   4,060               83           392,391                  0
```

Three things fell out that no amount of reading the code had produced.

**The dominant cost is failure, not distance.** In the laboratory shape 404 of
679 calls exhaust the thousand-node budget and return nothing. At 1,001
expansions each, those failures are 404,404 of 425,042 expansions — **95.1% of
all the work**. The 275 successful calls average 75 expansions and return a
median three-hop plan. So the memo must cache the *negative* result: one that
stored only successes would re-pay 95.1% of the cost forever while reporting an
87.8% hit rate.

**The two shapes need the memo for opposite reasons.** In the laboratory shape
no two agents ever share a `(home, water room)` pair — the within-sweep
duplicate rate is exactly 1.00× at all ten bands — so a per-tick memo buys
nothing and only a session-lived one helps. In the possession shape the
residents are co-located and the same pair is re-planned 6.4 to 9.1 times inside
a single sweep. A design fitted to either shape alone misjudges the other.

**Therefore the memo is shared, not per-entity.** A creature's remembered rooms
are distinct, so it can never duplicate its own pair within a sweep; every
duplicate is a duplicate *across* creatures. The per-entity shape the brief
proposed would have forfeited the whole cross-entity win. The homing cache is
per-entity for reasons that do not transfer — two of its four key components are
per-creature by construction — and copying it would have been copying the wrong
half of a precedent.

**And the population is small and it saturates.** Eighty-three distinct pairs in
each shape, against 679 and 4,060 calls, with zero new pairs added in the
laboratory shape's final band. A hundred entries, not a history.

## What shipped

A caller-owned, session-lived table of `(from, destination, budget) → hop
count`, storing the failures, holding sixteen-byte values rather than plans, and
**taking no avoid parameter at all**. That last is the key hardening done
structurally: a future caller holding a real hazard set cannot reach the memo,
because there is nowhere to pass one. It is a compile error rather than a
silently wrong answer.

Two call sites consume it — the belief ranking and the mid-walk
`nearer_to_home`, which shares the same anchor and must agree with the belief's
distances or the tie-break it was written to match breaks. Sharing one table
makes that agreement structural.

A third, moving-anchor site was **excluded by measurement**. It ranks from the
creature's current position, so its key space is positions × rooms rather than
homes × rooms — the one way the bound above fails. Extending a possession
session to sixty waits, the home-anchored population flattened at 190 while the
here-anchored one was still climbing at 101. But the here-anchored curve also
holds a **five-wait plateau** at 41 that later resumed climbing, longer than any
plateau the reference shows. A plateau in this system has been observed to be
temporary, so neither curve has demonstrated a ceiling in sixty waits; both have
only been shown to pause. The verdict rests on the conservative default under
genuine uncertainty, and says so rather than claiming a clean separation.

## The count saw what the clock could not

The lead criterion was a **count**, preregistered in preference to a wall-clock
proxy, and it is the one that answered.

```text
  searches per roster-wide sweep       before     after   criterion
  laboratory, whole 200-tick run          679        83       <= 100
  possession, whole 12-wait run         4,060        83       <= 100

  node expansions per sweep            before     after   criterion
  laboratory, whole 200-tick run      425,042    57,190   <= 60,000
  possession, whole 12-wait run       392,391    14,474   <= 15,000
```

The memo ends both runs holding exactly 83 entries, and **entries equal
searches** on both — a miss inserts exactly one entry, so any divergence would
mean the table was either re-searching a key it held or holding a key it never
searched.

The expansion figure has a tidier reading than its 4.7% margin suggests. 57,190
is precisely the control's cost at band 10, and band 10 asks 83 occurrences over
83 distinct pairs — a duplicate rate of exactly 1.00×, so the control there *is*
one search per pair. The memo therefore searched each of the 83 pairs **once,
and never twice, across all ten bands**. It is at its structural floor. Nothing
cheaper exists without changing what is searched.

Across reads rather than within a sweep, the session-lived property is sharper
still: on the seed-42 flagship, **2,692 route questions across eight waits
against one real search**.

## The timing criterion failed, and the cause was measured

The campaign also froze an effect-size floor: the slope of the belief fold's
microseconds-per-call against the probe creature's history had to fall tenfold.
It fell **1.13×**. Failed by a factor of 8.8.

The honest statement is not that it fell 1.13× but that **it did not move**: the
two after-runs disagree with each other by 1.28×, more than the before/after
difference, while the two before-runs agree to 1.3%.

The reason is an instrument-scope mismatch, and it was established by
measurement rather than argument. The timing probe constructs a **fresh memo
inside its own repetition loop**, deliberately, so its column stays comparable
with pre-campaign runs. A creature's remembered rooms are distinct, so a fresh
memo never hits within one call. A same-tree control arm — the identical sweep
with a fresh memo per call, run on the campaign's own head — produced 679
searches and 425,042 expansions, and 4,060 and 392,391: **digit-identical to the
pre-memo tree at every band and every wait**. The instrument's call path does
byte-identical search work before and after. A column whose work is unchanged
cannot fall tenfold, and the residual 1.13× is noise.

So a count established what no clock on that instrument could have. The column
that *does* hold a warm memo — the whole tick — moved 1.91× on slope and 1.84×
at the deepest band, and it is reported as context rather than as a result,
because it sits in the same output and would otherwise be read as one.

The control criterion swept a fold that constructs no memo and never reaches
this path. It moved 4.6% and 6.3% in **opposite** directions. It is evidence the
campaign broke nothing; it is not evidence the campaign worked, and it is
reported as the former.

## Three of six timed runs were set aside

The preregistration required all three load averages at both ends of every timed
run and set aside any run whose one-minute average exceeded ten. Three did, at
50.74, 64.46 and 46.05, against valid runs taken between 1.42 and 6.58.

Their readings scatter from **31.838 to 469.840** — an order of magnitude, in
both directions around the valid runs' 313 to 318. The lowest of them was taken
on a *before* tree, and against a valid after-run it reads as a **9.8× rise**: a
catastrophic regression this campaign did not cause. Contention could not have
manufactured a pass — the largest spurious fall available anywhere in the set is
1.92×, nowhere near the tenfold floor — but it very nearly manufactured alarm.
That is what the load rule is for, and on the night these were taken the box's
one-minute average moved between 1.4 and 64.5 under other campaigns' gates.

## The creature that knows the most water can reach none of it

One measurement is out of scope and worth more than the campaign that found it.

The roster member holding the most remembered water — twelve rooms at the first
band, forty-six by the last — has **zero of them reachable** within the plan
budget from its own home, at every band, while members holding four rooms have
all four. The mechanism is causal rather than incidental: a creature accumulates
remembered water by wandering, wandering carries it away from home, and the
belief plans *from home*. The more water a creature has learned, the farther
that water sits from the origin its plan starts at.

So the biggest believer behaves as ignorant of water while holding forty-six
remembered water rooms. Its thirst-memory path is effectively dead. This also
reframes the 95.1% figure above: budget-exhausted failure is not a quirk of
where the budget happens to sit, it concentrates on exactly the creatures that
know the most.

The design choice was already recorded — the fold's own documentation says
"nearness anchors to home (nearest-to-current is a followup)". What nobody had
was its price. Changing the anchor changes which room is chosen, therefore what
a creature does, therefore committed facts: an epoch, and out under this
campaign's byte-identity discipline. It is recorded with its measurement rather
than fixed here.

## Both hash constants moved, and that is the instrument working

Absorbing fifty-four commits of main at the close moved both campaign-time
ledger hashes. The question a moved constant asks is not "is the number wrong"
but "whose change was that", and the answer came from one experiment rather than
a bisect.

Neutralising the memo's cache lookup makes it do exactly what the pre-memo code
did — one fresh search per ask — and both hashes were re-taken on that same
tree:

```text
  constant   cache neutralised      memo live
  seed 42    0x9dd87f4cea554d28     0x9dd87f4cea554d28
  seed 17    0xbde5058309750ca4     0xbde5058309750ca4
```

Identical in both rows. The memo changes no committed byte; what moved was the
world underneath it, and the absorbed range carries a campaign that edits world
generation. A corroborating control was already in hand: after the previous
ninety-nine-commit absorption, with the memo fully wired, both constants matched
their minted values.

Had the constants been absent, the absorption would have been silent and the
campaign would have merged with no evidence either way about whether main had
interacted with its change. A constant's *value* is disposable. The control that
attributes its movement is the thing worth building.

## What remains

**The one-to-many field is the better idea and was the worse mechanism here.**
One search from a home settles every candidate at once, which beats a memo on
the cold read that a memo cannot help at all. It was demoted on its proof
burden: three properties must hold first, and none is established — that the
distance half of an existing disproven-for-another-purpose equivalence still
holds at the real budget rather than the one it was checked at; that equal
octile cost implies equal hop count on this mesh, when `12 × 17 = 17 × 12` is an
equal-cost pair with hop counts seventeen and twelve; and that the budget cutoff
is reproducible from a single run. Its advantage is also shape-dependent in a
way the memo's is not, and the number that would settle the comparison — the
count of distinct **homes** per shape — was never measured. The probe counted
pairs.

**An admissible octile heuristic** would turn this Dijkstra back into a real
A\*, a large win on every miss. It also changes which least-cost path is
returned, therefore the hop count, therefore the chosen room. It is an epoch,
and it is recorded as one.

**And the instruments themselves were the campaign's first task.** Two of the
three benches it preregistered on had been panicking on main for two days,
across two merged campaigns, on a predicate a drive had begun committing that a
hand-maintained list in each bench did not know about. Nothing runs an example:
they are compiled by every gate and executed by none. One unrelated campaign
even edited both files to keep them *compiling*, during this campaign's own spec
review, without noticing that neither *ran*. They are repaired, and the
hand-maintained list is now a source scan that goes red on an addition rather
than only on a removal — which is the direction the defect actually took, twice.
