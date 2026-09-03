# The Minute

*The minutes of a sitting are taken by someone who did not act. They are not
the meeting; they are the only part of it that survives it.*

## Forty days without a drink

Since [The Coercion](./the-coercion.md), a body a player holds does not stand
still while time passes. It runs its own arbitration every tick — the same
arbitration every other creature in the world runs — and it acts: it drinks,
it eats, it sleeps, it walks toward water. Nobody was taking the minutes. The
one call site that runs that walk bound its facts to a name beginning with an
underscore and dropped them, and the comment beside it said so plainly:
*discarded unconditionally, regardless of what the walk returns.*

Measured before anything was built, at two seeds, with the same script — hold
the body, then wait five days eight times. On each tick the probe re-ran the
same solo walk against the pre-wait ledger to see what had been thrown away,
then read the ledger and the body's felt state:

```
seed 42 (water in the home room)
  wait#  walk emitted                          ledger drank  felt
  0      slept                                 0             Idle / Content
  1      drank, rested, slept                  0             Idle / Content
  2      drank, eaten, rested, rested, slept   0             Idle / Content
  3..7   drank, eaten, (rested), slept         0             Idle / Content
  total  29 facts emitted, 0 committed, 40 days, 0 drank on the ledger
```

```
seed 7 (water is not in the room)
  wait#  walk emitted                          ledger drank  felt
  0      slept                                 0             Idle / Content
  1      slept + 14 agent-at (seeking)         0             Pursuing(Thirst) / Searching
  2      15 agent-at                           0             Pursuing(Thirst) / Searching
  3..7   4 agent-at each                       0             Pursuing(Thirst) / Helpless
  position column: unchanged across all 40 days
```

Two shapes of one defect. At the first seed the body drank in place on nearly
every tick, the drink was thrown away, and the next tick's walk began from a
ledger with no drink in it and drank again — so the felt state, read off the
walk, said *Content* for forty days while every fold over the drink record
described a body that had never drunk. At the second the body sought water
for fourteen rooms, then fifteen, ended somewhere the ledger never heard of,
and restarted from the origin room on the next tick. Nothing accumulated. It
then learned helplessness from a search it had never been allowed to finish.

The report line said `Time passes; the world keeps its shape.` at both seeds,
and the player, watching from inside the body, was told nothing at all.

The discard was never a design. The arc's founding sketch drew *commit what
the walk does* for every body; what shipped asked the solo walk through a
controller whose answer is unconditionally *hold*, and a holding walk emits
nothing, so dropping its facts cost nothing and nobody noticed the drop was
unconditional. Then a controller that **acts** was swapped in underneath it.

## The walk commits what it does

The repair is a commit loop, a wake merge, one changed write and one deletion.
The walk's facts are committed to the session's ledger through the same loop
the population's facts use, in a fixed order — the population first, then the
held body, then the rest of the tick — and the held body's column is then
written from the walk's own ending room, position and felt state together.

**Unconditionally, not "when possessed."** The natural patch asks whether
anyone is holding the body. It produces the same bytes today and it is the
wrong shape, because the controller *is* that question already: a body nobody
is driving answers *hold*, a holding walk emits nothing, and committing
nothing is a commit that costs nothing. Making it conditional would leave the
one path that will matter later — a verb queued through the player's own
controller — to be re-plumbed by whoever needs it, and would make the ledger's
honesty depend on who is driving, which is the one thing the arc's provenance
rule forbids. So a holding walk's empty commit stops being a coincidence and
becomes a guarantee, measured across two seeds and four waits each: zero facts
on every tick, and the walk ending in the column's own room every time.

The felt-only writer that used to serve the held body's column is deleted with
its only caller. A second writer that is correct only when it agrees with the
first is a second way to be wrong.

One thing downstream of the commit is not a fold, and an early draft of the
design said it was. Whether a body is asleep is read from a session field the
*sleep* verb sets, not from the sleep record; and a walk's sleep can run past
the end of the tick that started it. So a walk-committed sleep now sets that
field too, keeping the later of the two wakings, or a body released from a
hold would stand awake at the gate while its own ledger said it was sleeping.

## Off the walk band, a held body holds

Inside a dwelling, underwater, underground: these are frames the session holds
and the ledger does not. A body's recorded position stays out on the walk band
throughout a descent, and the verb that climbs back out returns the player to
the room the frame was entered from. A walk that committed a move to another
room while a frame was open would strand it — the frame would name a house the
body is no longer standing at.

The creature walk has no model of a lattice, a cell or a chamber index. So
while a held body is off the walk band its solo walk is asked through the
holding controller instead: the arbitration still runs, the felt state is still
written, and nothing commits.

This is a fidelity cut and was taken as one, not slipped in. Its cost: a held
body indoors does not drink, eat or sleep on its own while time passes, and its
thirst grows exactly as every body's did before this campaign — but **honestly**
now, because the felt state written by a holding walk agrees with the ledger.
Before, the same body read *Content* over a drink that had been thrown away.
The alternative is a walk that understands the frames, which is the campaign
that gives every creature the same frames, and that is a campaign rather than a
branch.

## The line

A tick that commits what your body did should say so. The report line now
minutes the held body's own committed facts, under two rules.

A **room change is named first and stops the line there.** The comparison that
tells you who arrived and who left was taken in the room your body has since
walked out of; run against the room it is now standing in, it would report
everyone in the old room as departed and everyone in the new one as arrived.
Looking is one keystroke away and already answers for the new room; a second,
half-correct description of it is worse than none.

**Needs served are named as clauses**, one per kind of act present among the
committed facts, in the order those kinds first appear. And the sentence names
the *will that holds you*, never "you": the act's effect belongs to the body,
and the choice was not the player's. What the player did was hold the body
while it happened.

```
Time passes. The will that holds you walks this body elsewhere.
Time passes. You sense movement nearby (201 stirred). The will that holds you drinks and rests.
Time passes. You sense movement nearby (201 stirred).
```

The third is a free body's line at the same tick, byte-identical to what it
read before this campaign — a free body commits no such facts, so its line
cannot move. The population's own movement count is untouched, so *the world
keeps its shape* still means the population did not move, and a tick where
only your own body moved says that instead of reporting one creature stirred.

## What the ledger says now

At the first seed the prediction held exactly: **seven drinks on the held
body's ledger by day forty**, one per tick from the second wait onward, which
is what the walk had been emitting and losing all along — and the felt state
still reads *Idle* and *Content* throughout, unchanged. The felt state was
right the whole time. The ledger was what was wrong.

## The null, and what it turned out to be

The second seed's prediction was that a walk which resumes from where it
stopped would reach water a walk that restarts cannot. Half of it held and
half of it did not, and the half that did not is the campaign's finding.

The mechanism half is green: the column moves on the first seeking wait, and
goes on moving. Measured tick by tick after the repair, cumulative:

```
seed 7, held, eight waits of five days.  The last column is a hop search from
the room the body is standing in on that tick, run to a forty-hop ceiling.

  wait#  moved  agent-at on the ledger  felt                           hops to water
  start      —                       0  —                              > 40
  0      false                       0  Idle / Content                 > 40
  1      true                       29  Pursuing(Thirst) / Searching   > 40
  2      true                       60  Pursuing(Thirst) / Searching   > 40
  3      true                       65  Pursuing(Thirst) / Helpless    > 40
  4      true                       70  Pursuing(Thirst) / Helpless    > 40
  5      true                       74  Pursuing(Thirst) / Helpless    > 40
  6      true                       78  Pursuing(Thirst) / Helpless    > 40
  7      true                       82  Pursuing(Thirst) / Helpless    > 40
```

Twenty-nine rooms on the first seeking wait, thirty-one more on the second —
that is a real journey, and it is the journey the discard used to erase every
tick. And still **zero drinks by day forty**.

The reason is not the repair, and it is not helplessness either. It is that
there is no water. Two searches, and it is worth keeping them apart. From every
room the body actually stood in, tick by tick, there is no fresh water within
**forty hops** — that is the column above. From the body's **home room**, run
much deeper, there is none within **a hundred and twenty hops, across 59,049
rooms searched**. The second is the one that says how far the drought reaches;
the first only says the body never walked out of it.

The body's own resource anchor — the room its thirst is pointed at, resolved
once when the body is derived — falls back to *its own home room*, which is the
documented answer when the search that resolves it comes up empty within its
budget. So the body is thirsty in a place where the thing it is thirsty for
does not exist, and what it performs is genuine exploration rather than a walk
toward a known destination.

Helplessness arrives on top of that. The body first reads *Helpless* at day
20.5 of this script, having covered sixty rooms before it did, and from there
the exploration falls to four or five rooms a tick — the measured deltas after
the second wait are 5, 5, 4, 4 and 4. It is a consequence of the drought, not
its cause.

So the honest report is: **the repair made the seeking real and the seeking
still fails**, because at this seed the settlement stands more than a hundred
and twenty rooms from the nearest river. The body is now helpless thirty-odd
rooms from home instead of helpless in it, and the ledger says so.

One thing the same measurement turned up and did not explain. The control — the
same body at the same seed with nobody holding it — never moves and commits
nothing, as it must; but it reads *Searching* at every one of the eight
five-day waits, while the same free body sampled every **three** days reads
*Helpless* at most of them. Helplessness is not a latch: it lifts for one day in
every five, a deliberate flicker of renewed effort so the state can reverse. A
five-day cadence therefore samples the same phase of that cycle every time. The
cadence-dependence is measured; exactly which phase each cadence lands on is
not, and nothing in this campaign depends on it.

A second unexplained number sits beside it. Before the repair the walk emitted
fourteen rooms of seeking on the first such tick and fifteen on the second;
after it, the committed trail grows by twenty-nine and then thirty-one — about
double, on the same seed and the same script. A resumed walk covering more
ground than a restarting one is the expected direction, and the *factor* is
not something this campaign measured a cause for. The two runs also differ in
what the ledger held going in, since the first tick's sleep is now committed
and the folds that read it move with it. Recorded as measured, not explained.

## What stays open

**A frames-aware walk.** The cut above is the whole of it: a held body indoors,
underwater or underground still holds. Lifting it means teaching the creature
walk what a chamber is, which is the same work as giving every creature the
frames a player has.

**A free body cannot drink.** A body nobody is holding acts only through the
verbs a player types, and there is no *drink* verb — the in-character roster is
shaped around going and sleeping. So a free body's thirst is monotone by
construction, which is why the free control above sits in *Searching* forever
with no way out. The path is already open: a verb queued through the player's
own controller now reaches the ledger through the same commit this campaign
built, and nothing further needs plumbing for it.

**The held walk's within-room seat is still dropped.** The walk works out where
in a room the body settles and that answer goes nowhere, exactly as it did
before. A seat within a room is not a committed fact, so nothing is lost from
the record; it is a fidelity gap, and it is pre-existing.
