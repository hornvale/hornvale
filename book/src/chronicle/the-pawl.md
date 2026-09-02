# The Pawl

A pawl is the small catch that rides against a ratchet wheel. It does nothing
when the wheel turns forward and everything when it tries to turn back: what
has been gained is held, and only advance is permitted. It is the cheapest
mechanism in the machine and the reason the machine has a direction.

Hornvale's fact ledger has the same shape by constitution — facts are appended
and never removed — and [The Tailrace](./the-tailrace.md) built the catch that
matches it: a value that *advances* as the log grows rather than being thrown
away by it. What The Tailrace could not do was give that catch anything to
hold. It shipped the primitive with no tenant at all, and named the migration
of the creature-drive stack onto it as work that needed its own plan.

This is that plan. It moves six reads off the raw history and onto five
tenants of one resident store, it moves no committed byte, and it ends with two
readouts that disagree with each other in a way worth reading carefully.

## One store, not six caches

The obvious carve was to migrate each fold where it stood. Thirst grows an
accumulator; hunger grows one; the water belief grows a set; the fear memory
grows a map; the alarm scan grows whatever it needs. Six migrations, each
correct, each finished.

That carve was refused, and the reason is that it produces six private answers
to a single question: *when does this thing catch up with the ledger?* Six
answers is five too many, and the sixth campaign to need one — hysteresis on
drive arbitration, a typed intention's anchor, a threshold-crossing detector —
would have built a seventh.

So what shipped is **the store**, and the folds are its tenants. It lives on
the session beside the navigation cache and the mesh memo, is keyed by the
entity it describes, and knows nothing about what any tenant's state means. A
tenant is an accumulator plus the key it lives under. It is never written to a
save, nothing in the world reaches it, and discarding it between any two turns
is unobservable — which is pinned, per tenant, by throwing the state away and
rebuilding it at every position and then at every third position, because the
more aggressive schedule is the *less* diagnostic one and The Tailrace had
already learned that the hard way.

That is the first concrete instance of an idea the frontier had been carrying
in the abstract: the layer above the ledger is an **adaptive cache over
derived data**, not a second store of truth. The ledger determines everything
in it. Keeping it buys time and can never buy truth.

## The seam is at read

There were three places to put the catch-up, and only one of them survives
contact with the code.

A hook on the write path was refused by The Tailrace on purpose, and this
campaign did not reopen it. An advance driven by the session after each commit
sounds equivalent and is not: the vessel evaluates a single walk **twice** —
once to read where everyone ends up, and once for the facts it actually
commits — so anything advanced per *evaluation* absorbs the same history
twice.

What is left is to advance **on read**, which turns out to be the only one of
the three that is correct under both evaluations. Advancing is idempotent in
position: catching up from where you are to where the ledger is costs one pass
over the facts you have not seen, and nothing is ever absorbed twice. A second
read within a turn is free. A re-read after a snapshot is free. The double
evaluation is free.

One consequence of that had to be decided rather than discovered. The per-turn
snapshot reads through a shared reference and cannot reach the session's own
caches, so today it *builds throwaway ones*. A throwaway store would have been
a whole-history rebuild per creature per turn — the exact cost the campaign
exists to remove, reintroduced on the one path its instrument does not watch.
So the store sits behind interior mutability and advances on read, and the
throwaway is refused in writing.

## What the trail is, and what it is not

The Tailrace ruled that the timeline builder at the centre of the old design
had to be **deleted rather than cached**, because its output is proportional
to history however cheaply you keep it. The store's first tenant is a
per-entity trail of committed sightings, and it would be easy to read that as
the ruling being quietly reversed.

It is not, and the distinction is the whole of a decision record. The old
builder was an object *rebuilt per call*: caching it bounds nothing, since the
first read still walks everything and every consumer downstream still receives
a whole timeline. The trail is the opposite construction — each sighting is
absorbed exactly once, in commit order, at constant cost; nothing is
recomputed; and its consumers do not receive a timeline, they receive a
*range*, found by binary search on the day. Its memory is a strict subset of
the ledger's own.

What justifies keeping it at all is that three things want the same ordered
view at once: the drive integrals' range reads, the alarm scan's positions for
everyone else on the roster, and the replay that catches a creature up after
an absence. One index shared by three readers is a different object from three
private copies of the same fact stream — a distinction the campaign then
promptly caught itself violating, and recorded as work still owed rather than
shipping the fourth copy.

**Five tenants shipped, not the six the design listed**, and the sixth is the
one worth explaining. The design had the alarm scan as a tenant of its own.
Building it revealed that everything the scan accumulates *from the ledger* is
already held by two other tenants — the trail and the per-room visit lists —
and that what remains needs the terrain and the observer's own threat niche,
which no accumulator over facts can supply. So the alarm scan is a **read**
over those two tenants plus a predicate, memoised for the tick as it always
was. A tenant whose absorb step would record exactly what a neighbour records
is a third copy of the same stream, and the campaign already had one of those
it did not want.

## The questions the code answered differently from the plan

The design carried six decision rules, each written as a branch table rather
than a prediction: run the witness, record which branch fired, then act. Three
of them fired the way nobody expected, and all three corrections came from
measurement rather than argument.

**A witness with no denominator reports zero, and zero reads as a clean bill
of health.** The first rule asked whether any drive read ever runs at an
instant *earlier* than a reset of the same creature — the difference between
the rule the project ratified and the rule the code implements. The first
draft of the witness reported no offenders. It had also observed no lookups at
all: on the flagship seed the reads in question have exactly one production
caller, and that caller runs only when another creature is standing in the
same room. Adding a counter that asserted the witness had seen *anything*
turned a comfortable zero into a real reading, and the real reading was that
the divergence is live — six of eighteen reads at past visit days resolve a
reset the read's own instant has not reached.

That mechanism has since stopped being true, which is worth recording beside
it rather than quietly replacing it. A sibling campaign made a session's roster
the residents of the settlement you stand in, and the same script on the same
seed now makes **5,360 of those lookups across 68 bodies** where it made 320
against a creature that stood alone. The conclusion the witness reached is
unaffected; the reason it originally saw so little is a fact about the world as
it was that week.

That same shape appeared twice more within hours: a sweep comparing the
accumulator against the old scan was asserting that zero equals zero across
three quarters of its probes, and a second rule's witness was using a proxy
for "past" so coarse that all five hundred and seventy-five production calls
qualified under it. Each time the repair was the same — count the thing the
assertion is *about*, and put a floor under the count — and each time the
corrected witness changed the question rather than merely confirming the
answer.

**A doc comment answers its author's question, not yours.** Two consecutive
tasks recorded that the flagship seed's world has no fear emitters at all,
each citing a comment in the code that said so. The third task measured it:
twenty of seventy scans on that very session *do* find an emitter. What the
seed lacks is something much narrower — no visited room's halo ever holds an
emitter at that room's last-visit day — which only two of sixty-four seeds
reach at all. Every conclusion the earlier tasks drew from the wrong mechanism
survived; the mechanism itself was wrong, and it had been carried forward
twice on the strength of a sentence written to answer a different question.

## The past-day path, preserved exactly as it is

The most expensive read in the stack asks a question an advancing accumulator
cannot answer directly. For every room a creature has visited, and every
emitter that could reach it, the fear memory evaluates that emitter's state
**at the day the room was last visited** — a read into the past, on the hot
path, inside the costliest fold.

The project's answer to past reads is that a drive's reset event is its
checkpoint: resume from the last reset at or before the instant and advance
from there. That is what shipped. But the code as written does something
subtly different — it takes the *latest* reset in the ledger, with no bound on
the instant at all, so a creature's thirst at a past day can be zeroed by a
drink it has not yet taken.

The campaign preserved that behaviour, deliberately, and wrote a decision
record saying so. The reasoning is not that the unfiltered rule is right: it
is that this campaign's headline constraint is that nothing moves, and the
corrected rule changes a reading on every world with an emitter in it, which
moves derived quantities that the project's calibration measurements assert
against. A behaviour change smuggled inside a performance migration is the one
thing that would make the migration unreviewable. The corrected rule is owed
as its own campaign, with its own reference refresh, and the record exists so
that the next reader finds a decision rather than an inheritance — because an
unexamined behaviour carried forward looks exactly like an examined one.

## The first readout: five criteria of six, not met

The success criteria were inherited **verbatim** from The Tailrace, frozen a
week before any of this code existed. Re-freezing them after seeing the
substrate is what metric-chasing looks like, so they were not re-frozen.

They asked for three things about the thirst read — that its cost stop growing
with history, that a fixed floor become identifiable and positive, and that
the history term fall below a fifth of a tick — and two about the fear memory,
that it flatten and that its enormous absolute cost fall tenfold.

Those criteria compare against The Tailrace's own tables, taken a week earlier
on a differently loaded box and reporting no per-run intercept — and the
falsifier is a claim about the intercept. So a **same-box control** was
measured in the same session from a checkout at the campaign's merge base, with
identical bench constants and no store. It is a control and **not** a
re-freezing of a criterion, and the distinction is the one thing a reader can
get wrong here: every threshold below is the frozen one, unchanged, and every
verdict is stated against the frozen pre-campaign numbers as well as against
the control. What makes the pairing legitimate is that both benches'
deterministic columns are byte-identical across the two trees.

Five of the six came back not met. The thirst read's sensitivity to history
was 1.01 where it needed to be under 0.20; its fitted floor was negative on
all three runs; the whole-tick history share fell from 74% to 62% against a
target of 20%; the fear memory was flat at 0.92 and had fallen 1.5-fold, not
tenfold. Only the byte-identity criterion held, and it held by construction.

And a third instrument said the opposite. A synthetic sweep that drives the
same read over a thousandfold range of history — rather than the 2.6-fold
range an ordinary session reaches — reported that the campaign had **changed
the order of the computation**: in the regime where a creature never drinks,
the exponent fell from 2.11 to 1.16, and the cost at ten thousand facts of
history fell from 19,215 microseconds per call to 194. Ninety-nine-fold.

Both instruments were right, and reconciling them is the campaign's first real
result. At the depth an ordinary session actually reaches, the predicted
saving was seventeen microseconds — smaller than the ecological instrument's
own run-to-run spread on that very column. An effect the size of the noise is
not resolvable, and the frozen criterion had asked that instrument to resolve
it. Which left a much more interesting question: the ecological read cost
about six hundred microseconds where the synthetic one cost three, at the same
depth, and *that* two-hundred-fold gap was unattributed.

The readout named the leading candidate and refused to claim it. The
difference between the two call sites is terrain: the synthetic bench passes a
trivial one, the session passes a real one, and the integral samples
temperature once per segment. That per-segment sample is proportional to the
segments since the last reset — the same order as the walk the store had just
removed, on the same segments, which is exactly the signature the measurement
showed.

## One change after unblinding, and what kind of change it was

The reading of that is that **the implementation had been falsified against
its own design, not the design against the world.** The specification had said
the thirst accumulator's state was a running integral, a last sighting, a
segment start, a last reset, *and* the checkpoint list. What shipped was the
checkpoint list alone, re-integrating from the last reset on every read — and
the review had accepted "no read-side accumulator at all" as vacuously
satisfying the purity clause the design had written to constrain one.

For a creature that never drinks, "the last reset" is the beginning of the
world. Twenty-one of the fifty creatures on the instrument are in exactly that
position, and the probe the decisive measurement follows is one of them,
because that instrument selects the creature with the *most* postings, which
is by construction the one that never arrives anywhere. So the read was still
proportional to history, with a terrain sample per segment, and it was the
unattributed ninety-nine per cent.

So the campaign built the accumulator its own design had described, and
re-measured. That is **one** post-unblinding change to production code, and
the distinction between mechanism-completing and constant-tuning is checkable
rather than rhetorical here: no threshold moved, no constant was retuned, no
criterion was rewritten, and the first readout is reported in full rather than
superseded. It stands exactly as it was measured. The honest cost of the
ordering is stated with it — the second readout is not blind, because it was
taken knowing precisely what had been repaired and where to look.

The accumulator keeps, per creature and per drive and per reset, the running
sum after each sighting, restarting at zero at each reset. A read is one
lookup, one linear term, one clamp. It performs **the same additions in the
same order** as the loop it replaces, which is what makes it identical to the
last bit rather than merely equivalent to within rounding — floating-point
addition is not associative, and a faster summation order would have been a
behaviour change wearing an optimisation's clothes.

## The second readout

| what was asked | threshold | first readout | second readout |
|---|---|---|---|
| thirst read stops growing with history | under 0.20 | 1.01 — **not met** | **0.07** — met |
| its fixed floor becomes identifiable and positive | positive | negative on all runs — **not met** | **+2.35 to +2.51** — met |
| history falls below a fifth of the whole tick | under 20% | 62.4% — **not met** | 58.0% — **not met** |
| no committed byte moves | — | held by construction | held, re-witnessed |
| the fear memory flattens | under 0.20 | 0.92 — **not met** | 0.92 — **not met** |
| the fear memory's cost falls tenfold | tenfold | 1.52-fold — **not met** | 1.57-fold — **not met** |

Two of the five failures became passes; three did not, and nothing was
averaged across a failure to make it look better. The thirst read fell from
620 microseconds per call to 2.70 — a factor of 230 — and the hunger read from
619 to 2.51, a factor of 247. **The two reads the accumulator serves collapsed
together and nothing else did**, which is the signature of that change and of
no other: the fatigue read, the water belief and its shared sibling all sit at
a ratio of 1.00, exactly as they did in the first readout, and the fear memory
sits at 1.57 — moved, but by the store rather than by the accumulator.

The synthetic sweep's never-drinks column, which had been eighty-nine times
slower than its periodic sibling before the campaign, now sits *on top* of
it — 15.5 microseconds against 14.9 at ten thousand facts.

The two results that could have gone against the campaign are worth stating
before the ones that went for it.

**The falsifier did not fire.** The design had said outright that if the
per-fact slope fell but the fixed floor rose by more than the saving at
realistic session lengths, the store would be a pessimisation for short
sessions. The floor did rise — by 45 milliseconds per tick — and the lines
where the two trees cross now sit *below the shallowest history any instrument
samples*, where in the first readout they crossed just inside it. There is no
measured band in which the pre-campaign tree is faster.

**The level moved, and the first readout said it had not.** A second
instrument, which sweeps creature count at a fixed twenty ticks, measured two
paired runs at two hundred creatures: 4.07% and 3.77% faster. Those agree with
each other to within a third of a point, and — the part worth reading twice —
the *other* instrument's fitted lines predicted 4.4% at that depth. A
prediction from one instrument landing within half a point of another
instrument's measurement is the strongest single piece of evidence in the
readout that the two are describing the same thing.

## The remaining quarry

The three criteria that still fail all fail on the same fold, and it is bigger
than the one that was fixed. At the deepest band measured, the fear memory
costs 93,153 microseconds per call: **84% of the six timed reads' total**,
against the thirst and hunger pair's combined 0.005%. It remains proportional
to history at 0.92, and the store moved it by 1.57× — 36% off the same-box
control, and against the frozen criterion's own 73-97 milliseconds per call
**no reduction at all**.

Its remaining cost is legible from the code rather than merely suspected. Per
tick, for every room the observer has visited and every emitter, the emitter's
state at that room's last-visit day is re-evaluated — a recursion into the
same read with an empty roster — and the result is memoised only *within* the
tick, because the memo that holds it is built fresh every tick. A memo that
survived across ticks would fix it, and it is not safe to build one under the
unfiltered reset semantics this campaign deliberately preserved unless it is
keyed by the emitter's own reset partition. That makes it a question about the
derived-component layer rather than about folds, and it leaves the campaign as
a measured handoff with a number on it rather than as another task.

## What arrived while this was being built

Three sibling campaigns landed in the middle: one that made fatigue a stock
folded over the whole rest timeline, one that made a session's roster the
residents of the settlement you are standing in, and one that moved the ledger
and the person domain upstream of everything a session commits. Between them
the branch absorbed 202 commits of main, then 59 more, then 47 at the close.

All three changed things this campaign was measuring, and the roster one
changed them enough to matter. The flagship seed's creature now condenses onto
fresh water, so the residents around it commit no positional facts at all —
which quietly
emptied four of the campaign's own witnesses. Every one of them *failed its
own floor assertion* rather than passing on an empty set, which is the entire
argument for putting floors under witnesses, and all four moved to a seed
whose roster main itself had already measured and pinned in three other
places.

The rest-fold campaign's arrival also settled a question by subtraction. This
campaign had planned to decide whether the fatigue read should migrate; after
the absorption, the read it would have had to decide about no longer exists.
What was left was measured anyway, once, on a quiet box: it is a trail-walker
but it is *not* proportional to history — sensitivity 0.31 against a standing
rule of 0.5 — so it did not migrate, and the number to re-measure against is
recorded rather than the verdict alone.

The identity witnesses were re-recorded **main-first**: a checkout of main
carrying none of this campaign's code was given the same scripts, and the
merged tree was required to reproduce main's numbers exactly, which it did —
down to 127 bodies, 532 shunned rooms and thirteen dread entries agreeing bit
for bit. That ordering matters. Taking the merged tree's numbers first and
then asking whether they look right would have proved nothing about which
campaign moved them.

## The constants retire, and the witnesses stay

The proof that nothing moved could not come from the project's ordinary drift
check, and the campaign said so at the outset rather than citing an instrument
that is structurally blind here: no committed artifact carries a ticked
session ledger, so nothing committed would redden either way. What stood in
for it was a hash of an entire scripted walk's output, asserted against a
committed constant, and shown to work by mutating a fold and watching the hash
move.

That is exactly the right instrument during a migration and exactly the wrong
one after it. The constant equals *the whole walk's behaviour on one seed*, so
any campaign that legitimately changes creature behaviour reddens it — the
roster campaign moved all three of this one's constants without touching a
line of its code. It is a standing tax on everyone else, and an unwinnable
race besides, since what is gated is main merged with the branch rather than
the branch alone.

One casualty is worth naming, because deleting it would have been the tidy
mistake. The old filtered lookup over committed facts lost its last production
caller — two went to the store and the third to the rest-fold campaign — and it
is now compiled only for tests, deliberately, because it is the *scan* half of
the fold-equals-scan comparison that a surviving replay witness uses as its
oracle. Left ungated it would have been a production-dead function still
compiling into the library, which reads to a later reader as a live path.

So the constants retire and the witnesses stay. Each keeps three things: it
runs its script twice on two *fresh* sessions and requires the two to agree;
it keeps every floor that proves it reached the path it claims to witness,
now checked on both runs so that a witness which quietly stopped witnessing
cannot hide behind an agreeing hash; and it keeps the comparison of the fold
against a verbatim copy of the scan it replaced, held in the test file rather
than shared with the code under test. The retired constants and their positive
controls are kept as dated history, with an explicit note that nothing
re-checks them.

## The honest limits

**Both readouts were taken before any of the three absorptions.** The measured
tree is the campaign as it stood at its own merge base plus its own work, and
202, 59 and 47 commits of other people's work landed after the last number was
recorded. That matters unevenly. The folds' own per-call costs are properties
of the read and would be expected to survive — but they are measured on the
pre-roster walk, and the roster campaign changed that walk materially enough
that the same script on the same seed now makes 5,360 of one witness's lookups
where it made 320. **The level and the whole-tick share are pre-absorption
numbers**, and nobody has re-measured either against what actually lands.

**Three of the six criteria were not met and are not explained away.** The
whole-tick history share is 58% against a target of 20%, and both clauses
about the fear memory failed twice. The blunter form of the second: the
criterion asked the fear memory's cost to fall tenfold from 73-97 milliseconds
per call, and against that frozen figure it did not fall **at all** — the
1.57-fold improvement is against a same-box control measured this week, not
against the number the criterion named. The attribution for that is arithmetic
rather than an excuse — a fix to a read that is now five-thousandths of a per
cent of the total cannot move a share that another read dominates at 84% — but
the criteria were written against that other read on purpose, and they failed.

**The second readout is not blind, and the first is reported in full for that
reason.** One change was made between them. It completed a mechanism the
design had specified and the implementation had not built; no threshold and no
constant moved. A reader who wants the version of this campaign that was
measured without knowing where to look should read the first readout, which
stands unedited.

**Two things were not measured at all.** The accumulator bounds how many reset
partitions it keeps per drive, and that eviction path has no direct test and
no production measurement — nothing here says how often it fires in a long
session. And the positive control for the emitter-bearing identity witness was
demonstrated on a seed that the witness no longer selects: the search moved to
a different world when the roster changed, and the control has never been
re-run there. It is kept because what it established is a property of the
instrument rather than of a world, and a control deleted for being old leaves
nothing in its place — but a constant-free witness guarantees determinism plus
its floors and nothing more, and cannot detect a behaviour change by any
means.

**And one redundancy shipped knowingly.** Three per-entity indexes now fold
the same stream of sightings: the trail, the per-room visit lists, and the
water belief — where the second is derivable from the first and the third from
the second. Merging them is recorded as owed rather than optional. The cost of
leaving it is memory and catch-up work, never a wrong answer, which is why it
was allowed to ship and why it is written down.

The headline is that the read side of the log now stands on catches rather
than on walks: the thirst and hunger reads are two hundred and thirty times
cheaper, the whole tick is a quarter cheaper at two hundred ticks, and the
level fell four per cent. The second headline is that the largest fold in the
stack was barely touched, and the campaign ends by pointing at it with a
number instead of a hunch.
