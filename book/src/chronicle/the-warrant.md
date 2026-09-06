# The Warrant

A warrant is two things at once: the authority for an errand and the
justification for it. Until this campaign, a walking creature in Hornvale had
neither written down. It had a *live* reason — a `Mode` computed inside the
tick, naming which drive was winning and whether the creature believed it knew
where the water was — and that reason evaporated at the commit boundary, leaving
behind a sentence of English stamped onto every step the creature took.

The campaign that replaced this began by measuring whether the sentence was
worth keeping. The answer is the most interesting thing it found, and it is not
the answer the project had written down.

## The premise, rendered

[Decision 0238](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0238-stage-7-is-three-stages-and-their-order-is-forced.md)
carved the project's log-bounding work into three stages and fixed their order.
The ordering of the last two rests on a single sentence:

> **7b must precede 7c, and this is a fidelity call.** The trail is *content*,
> not bookkeeping. Each step's `provenance` is authored prose … and it is
> rendered … Replacing per-step commits with per-errand ones before the
> intention carries its own compositional `why?` would delete readable content.

That was read off the code. It had never been rendered. This campaign rendered
it, through the real reader — `hornvale possess --seed N --script <look; wait
12; !why 1>`, which drives the same `recount` the repl's `why` calls — over
forty residents per seed and twelve simulated days, counting maximal runs of
constant provenance in each resident's own trail. The fourth row is a separate,
wider sweep, and it is explained below.

| seed | `agent-at` facts | errands (runs) | distinct prose strings | steps per errand |
|---|---|---|---|---|
| 7 | 2600 | 40 | **1** | 65.00 |
| 14 | 3080 | 40 | **1** | 77.00 |
| 23 | 1364 | 579 | **3** | 2.36 |
| 42 | **0** | 0 | 0 | n/a |

**The premise is half true, and the half that fails is the half the ordering was
built on.** The prose is content: it names the drive, the belief state and the
direction of the errand, and three renderers put it in front of a reader. But
*per-step commits* are not what carry it. A run of identical strings carries the
information of one string. A seed-7 resident's twelve days produce sixty-five
steps bearing **one** sentence; a seed-14 resident's produce seventy-seven,
also bearing one. The busiest regime measured, seed 23, alternates enough to
produce three. On seeds 7 and 14 the repetition is **98.5% of the rendered
lines**.

The fourth row is its own finding and it bounds everything that follows. Seed
42 — the flagship, the seed on which every committed artifact in this repository
is built — commits **no `agent-at` fact at all**: sixty-seven residents, swept
individually, over ninety simulated days, zero. Its roster condenses on water
and never has to walk. So the movement half of this epoch could not move a
single committed seed-42 byte, and did not.

What this does to the ordering is a correction, not a reversal. 7b must still
precede 7c, for a **stronger** reason than 0238 recorded: not "per-step prose is
content that abstention would delete", but *the why is content, and it currently
has no home except a string repeated at every step*. 7b gives it a home; after
that, 7c can drop steps without deleting anything a reader can distinguish. That
is a better precondition than 0238 claimed to be establishing, and it is written
down as [decision 0847](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0847-the-7b-before-7c-ordering-holds-on-the-why-not-the-per-step-prose.md),
which amends the argument and leaves the stage table untouched.

## The unit, and where each of its parts went

The committed unit is the **errand**: a maximal run of constant `Mode`. It was
not invented here. `Mode`'s own doc already called itself "the errand an NPC is
on"; the campaign promoted it from tick-local to committed.

A fact in this project is a dumb envelope — subject, predicate, object, place,
day, provenance — and `Value` is `{Entity, Text, Number, Flag}`, with no
structured variant. So an errand's two components had to be placed rather than
packed, and each placement is an argument.

**The reason lives in the predicate**, drawn from a closed family of eight
`errand/*` keys. Every predicate carries a registry doc string, and that doc is
already the only prose the historiography window renders for a predicate — it is
what produces the leading *"an agent's position on a day:"* on every step line.
Putting the reason there moves the reader-facing words out of the ledger and
into the concept registry, which is the kernel's own producer rule applied to
facts rather than to phenomena: *a producer cannot know who is looking, so a
stored string could only ever be culture-neutral or wrong.* A per-drive
predicate family is established convention here and not a novelty —
`drank`/`eaten`/`rested`/`slept` are already four predicates for one relation.

The eight are `Mode`-shaped rather than `DriveKind`-shaped, and that distinction
came out of an ideonomy pass rather than the code. A shape like `Go(target,
reason)` under-types two of the eight cases: walking home is not a pursuit at
all, and fleeing a threat is repulsion *from* something rather than attraction
*to* something. The mapping is written as an exhaustive `match` with no wildcard
arm, so widening `Mode` is a compile error rather than a silent fall-through
into the wrong errand.

**The object carries the errand's origin, not its target** — and this is a
correction to the design, made after the design was approved. The first draft
put the target there. There is no target to put there. The arbitration seam
exposes `Intent::Do(Action)` and nothing else; `Action::MoveTo(n)` is the *next
step*, and `Drive::proposal` is documented as "the next executable step". No
destination is materialized anywhere near the commit site, so the field as
specified had no possible caller. Surfacing a goal through the seam would have
made an epoch also a behaviour change; a bare flag would have thrown away a fact
that is free and correct. The creature's position at the instant its first step
is charged is always available and never wrong, and it is what makes an errand a
*segment* rather than a point.

That correction improves the result. The recount now says where a creature *got
to*, never where it *meant* to go. For a completed errand the two coincide. For
an errand abandoned partway they do not — and asserting a target would have made
the ledger claim an intention the code never formed.

**The step's provenance becomes `vessel/liveness`**, the producer, like every
other fact in the repository. That restores the truth of a claim `liveness.rs`
had been making about itself for some time — *"`provenance` is free-form prose
no fold may key on"* — which one instrument had quietly been violating.

## What a reader sees

A seed-7 resident, twelve days, before:

```text
- wandered, having found no water yet (thirst): 3874794881 (asserted by vessel/liveness, day 5.83239)
- an agent's position on a day: 3874794977 (asserted by vessel/liveness, day 5.83239)
- an agent's position on a day: 3874798081 (asserted by vessel/liveness, day 5.99811)
   … thirty-six further lines, the parenthetical identical on every one …
- an agent's position on a day: 3874984961 (asserted by vessel/liveness, day 11.96403)
```

After:

```text
- wandered, having found no water yet (thirst): from 3874794881 — 38 steps, days 5.83239 to 11.96403, ending at 3874984961
```

**Forty-two bullet lines become four**, three of which are the creature's name,
personhood and birth. The one thing a reader could not learn before — *why* — is
now the first thing on the line, and the origin, the step count, the day span
and the destination are named once each. The per-step view survives behind
`why <id> --steps`, and it gains something the pre-errand ledger could not
express at all: each step is numbered *within* its errand, `step 12 of 38`.

The compression is not the interesting case; the alternating regime is. Seed 23
at forty waits renders nine errands scattered among the drinks, grazes and
sleeps they happened around — *sought a kinder clime (comfort): from 3610257411
— 5 steps, days 13.66271 to 15.12597, ending at 3610258659*, then a graze, then
a sleep, then *walking home (sated)*. The roll-up removes repetition and nothing
else. Where a creature genuinely does many different things, the account stays
as textured as it was, and reads better because each errand now carries its
origin and its end.

## The preregistration, and the null

Three hypotheses were frozen in the spec before the code that would move them.

**H1 — the fidelity claim, and the one that matters.** For every resident, the
set of distinct reason-glosses rendered, and the day each first appears, is
identical before and after the flip. This is a claim of *exact equality*, and it
is the whole justification for calling the change lossless. It is discharged by
building the tree the campaign branched from, capturing the
pre-flip renderings there, and comparing them against the post-flip run: **252
entities, 1242 run-starts, equal in both directions** across the three
preregistered seeds and the seed-11 fixture that carried the campaign's own
before-image — four seeds in all. No entity in the before-image is missing from
the run and none in the run is absent from the before-image. H1 holds, on the
population it was frozen over.

**H2 — provenance bytes committed per agent per tick fall by at least 50% in the
walking regime, and by exactly 0% on seed 42. H2 IS FALSIFIED, and it is
reported unamended.**

| seed | agents | before (bytes/agent/tick) | after | reduction |
|---|---|---|---|---|
| 7 | 101 | 141.037954 | 71.878713 | **49.04%** |
| 14 | 58 | 196.903736 | 87.912356 | **55.35%** |
| 23 | 109 | 109.123089 | 84.918960 | **22.18%** |
| 42 | 67 | 73.250000 | 73.250000 | **0.00%** |

The second clause holds to the byte: seed 42 commits the same 1407 facts with
the same 58,893 provenance bytes at the same rate, before and after. The first
clause fails on two of the three walking seeds. Nothing was retuned.

**Why it failed is a defect in the preregistration, and saying so must not become
a rescue.** The spec argued its case in §1 about the **`agent-at` trail's**
prose, and then froze §10's floor over **all committed provenance** — a quantity
§1 never discussed. Only `agent-at`'s share of that total can shrink, and the
new errand facts add provenance back. On seed 23 the drive commits 920 errands
against 2524 steps, an errand every 2.7 steps, which is the worst-case regime §1
itself had identified; those errands' own bytes buy back a sixth of what the
steps give up, against an `agent-at` share that was only 58% of the total to
begin with. Measured on the quantity the flip actually governs, the `agent-at`
provenance reduction is **65.72% / 65.82% / 54.56%** — past fifty per cent on
all three. Both numbers are reported here. The second is context for why the
first came out as it did, and it is not a substitute result: a preregistration
that can be re-scoped after unblinding is not one, and the spec's §10 carries a
dated note recording the mismatch rather than a corrected threshold.

**H3 — facts committed per agent per tick stay under the standing ceiling.** The
tick-commit budget reads a last-half rate of **1.785075** facts per agent per
tick against a ceiling of 2.5, non-growing against a first-half 1.855970. Seed
42's per-tick series is byte-identical to the series measured one commit before
the errand fact existed, and identical to six decimal places — the epoch adds
facts, and on the flagship seed it adds exactly none.

The campaign removed no fact. Pre-flip and post-flip fact counts are identical
on every seed (4035 / 3213 / 4870 / 1407), as are the `agent-at` counts
(2968 / 2668 / 2524 / 0). Stage 7b writes; only 7c will erase.

## Two limits, stated rather than left to be discovered

**An errand's endpoint is derived, not stored.** It is the position at the next
errand boundary, read from the trail the store already keeps. That is what makes
the recount honest about abandonment, and it is an asymmetry stage 7c inherits: a
compaction that drops every step of a completed errand must fold the endpoint
into the errand fact first, or the segment loses one of its two ends.

**An errand is opened and never closed, so its span measures elapsed time rather
than time spent walking.** Nothing anywhere sets a creature's errand back to
`None`; an errand ends only implicitly, when the next step computes a different
key. Seed 11's first resident holds `walking home (sated) — 5 steps, days
113.04798 to 116.05578`, which brackets five sleeps and a graze: a step every
fourteen hours where a walking step is four hours apart. The errand is genuinely
one errand. What the span hides is that the creature was mostly not walking
during it, and nothing on the ledger marks the gaps. The widest *continuous*
spans are ordinary by comparison — seed 7's sixty-five steps over 10.266 days,
seed 14's seventy-seven over 9.816.

That limit is stated in this narrow form because a wider one was written down
first and turned out to be false. The original claim was that a creature which
walks for thirst, drinks, sleeps for a month and then walks for thirst again
commits no second errand fact, folding two episodes into one line. It was
plausible and it does not occur: a drink flips the mode to sated, so the next
step computes a different key and commits. Two independent sweeps — one over
seeds 7, 11, 14 and 23, one wider at 249 errand lines and 120 waits — found
**zero** errand spans bracketing a drink. The mechanism was one probe away from
being checked and the probe was eventually run. The consequence for 7c survives
in changed shape: a compaction that drops steps loses the *activity profile*
inside an errand, but not an episode boundary, because a drive discharge already
creates one.

## What the guards learned

Three findings about instruments came out of this campaign, and each is a
different way for a check to stop measuring while staying green.

**A flip that retires a field silences the arms that assert absence over it.**
An existing test asserted, as a *negative control*, that a lone creature never
flees — by counting facts whose provenance contained `"fear"`. The flip makes
that count unconditionally zero. The positive arm beside it went red and
announced itself; the control would have gone on passing for exactly the wrong
reason, a zero meaning "the predicate can never match" wearing the clothes of a
zero meaning "the behaviour is absent". A vacuous positive check reads as
coverage; a vacuous negative control reads as *proof*, which is worse.

**A ceiling mutation cannot fire against a measured zero.** The plan proposed
proving a re-pointed witness alive by lowering its ceiling to zero. Seed 42
commits no fear or belonging errands at all, so `0 <= 0` still passes and the
mutation witnesses nothing. Mutating the witness's *filter* instead — pointing it
at two predicates the run does commit in quantity — turned it red at 2256, which
proves the stronger property: that the check reads the predicate field and
matches on exact key equality, rather than merely that some number sits under
some bound.

**Retiring a transitional guard is a coverage event.** During the two tasks in
which the eight glosses existed in two places, a test held the two copies equal.
When the second copy was deleted the test was deleted with it, correctly — it
could not exist in that form. But it was also the only thing pinning each key to
its own gloss. What survived pinned the `Mode`-to-key mapping, pinned the eight
key spellings, and checked that the eight docs were distinct and non-empty —
**all of which a swap satisfies**. Four of the eight glosses were pinned by
nothing at all, and swapping two of them would have rendered the wrong words on
the right key, permanently, with the whole suite green. The remedy is an
eight-row literal table, and the proof that it earns its place is that the
mutation which introduces the swap leaves six other tests passing and reddens
only this one.

## What stage 7c inherits

Every committed step is now covered by an errand fact naming its reason and its
origin, so dropping steps deletes position and never *why*. An errand fact is
roughly one sixty-fifth the volume of the steps it covers in the walking regime.
Abandonment is on the ledger for the first time, so a compaction can distinguish
an errand whose steps are redundant from one that was cut short — which is
precisely the case where the steps are the only evidence. And the two
asymmetries above travel with all of it.

One option this campaign surfaced and nobody has argued for is now written down
rather than left implicit: an errand **close** fact, carrying the endpoint. It
would make abandonment and resumption legible at the ledger instead of by
inference, and it would hand 7c the endpoint it must otherwise fold in by hand.
It costs a second fact per errand, which is the whole of the argument against
it. The registry carries the row; nothing here decides it.
