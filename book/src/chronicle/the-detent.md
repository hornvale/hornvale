# The Detent

A detent is the catch that holds a mechanism in one position until something
deliberately releases it — the click in a rotary switch, the notch that keeps a
lever where it was put. It is not a ratchet. A ratchet permits advance and
forbids retreat; a detent forbids movement in either direction until a
threshold force arrives.

That is the exact shape of the fact this campaign is built on. A verdict about
the ground — *is this room frightening to this creature* — is a pure function
of a terrain that does not change within a session. Nothing about the passage
of ticks releases it. So it is taken once and held, and the only thing that
could release it is a different terrain.

[The Pawl](./the-pawl.md) built the ledger-derived half of the layer above the
ledger: a resident store of what the *facts* determine, advancing as facts
commit. This campaign adds the world-derived half to the same session: a memo
of what the *terrain* determines, keyed by room, never invalidated. Two
structures, two halves of one idea, on the same session object.

## The finding that chose the campaign

The Pawl closed with one fold still failing three criteria — the fear memory,
at 93 milliseconds per call, sensitivity to history of 0.92, and no meaningful
fall against the frozen figure at all. It left behind a note naming the
mechanism: *per tick, for every visited room and every emitter, the emitter's
affect at that room's latest-visit day is re-evaluated.* Its chronicle called
that mechanism "legible from the code rather than merely suspected."

It was legible. It was also wrong, and the way that was established is the
thing worth carrying out of this campaign.

Before the campaign was chosen, the fold was **counted** — not read — on the
instrument the criterion is measured on. A terrain wrapper counted every
question the fold asked of the world; the store's own witness was read before
and after each probe.

```
seed 42, 50 derived agents, one fear-memory call on the deepest-history probe
                     tick 15   tick 30   tick 60   tick 100   tick 200
  terrain questions   14,004    16,758    22,302    29,097     43,164
  affect replays           0         0         0          0          0
```

The named mechanism is reached **zero times**, at every depth. On a second
seed, chosen because every scan on it *does* find an emitter, the replay count
is still zero. What the ninety-three milliseconds actually were: **static
terrain, re-sampled every tick** — about ninety-five per cent of it in the
emitter scan's second pass, which asks about every room every roster member has
ever stood in, and each room's neighbours, once per member, every tick.

Widen the lens from one call to one whole tick and the shape is plainer still.
Fifty agents walking for one tick asked the world about **44,694 rooms**, of
which about eleven thousand were distinct, in order to commit **31 facts**. The
ground had not moved between any two of those questions, or between any two
ticks.

The zero is only worth anything because it carries denominators. A count of
zero with no floor beside it cannot be told apart from an instrument nobody
wired in — which is, precisely, how a mechanism that is reached zero times came
to be written into a committed record in the first place.

## What shipped

**A room memo.** A session-lived table of the terrain's hazard verdict per
room, at the derived layer's purest validity class: never invalidated, because
nothing in a session can move it. It is owned by the session beside the
navigation cache and the mesh memo — deliberately *not* by the terrain object,
because the benches and the laboratory rebuild their terrain every tick and
would find a terrain-scoped memo cold on exactly the instruments the criteria
run on. It is filled on read rather than prefilled, because the rooms a tick
will ask about are the union of every roster member's visited rooms and their
neighbours, which nothing knows before the reads run.

Its key is the room alone. The derivation also reads the locale context and the
session's predator field, and the obligation that a derived value's key carry
every parameter its derivation reads is a settled one — so the terrain's
identity is supplied by **ownership** instead: one memo per locale context and
predator field, owned by the object that owns both. That is not a new
assumption; it is the sentence the resident store already carries for its own
temperature dependence. It is held by two tests: eviction of the whole memo
after every single read, with the answer required to be unchanged; and a
two-terrain test that hands one memo to a second terrain with a different
predator field and requires the answers to differ — a test that *observes* the
aliasing the rule forbids, rather than assuming it impossible.

**A verdict index.** The memo alone turns samples into lookups without changing
how many are asked for, and the campaign's design said so before it was built.
So the second structure remembers, per creature and per room, the answer to the
one predicate the fear path applies: threat at that room, scaled by the
creature's mettle, against the action threshold. Over a fixed terrain that
answer is a constant. The index keeps the frightening rooms in ascending order
of *first* visit, advanced from the creature's trail by a consumed-prefix
cursor, so the per-tick work is judging the rooms first seen since the last
tick.

The Pawl had ruled that the emitter scan is **not** a tenant of the resident
store, because nothing a fold's absorb step may read can supply a terrain
verdict. That ruling stands unchanged. The index is not a fold: it is a
read-side memo of a pure predicate, which is exactly what the water-belief
tenant's own predicate already was — with the one difference that the answer is
kept rather than re-asked. The absorb step still sees only facts.

Sorting by first visit is what lets a past instant be served without a rebuild.
The laboratory reads each creature at a *waking* instant inside the tick, which
can precede the trail's end; a room is in the scan's domain at that instant
exactly when its first visit is at or before it, so the answer is a prefix of an
already-sorted list.

**A scan that advances.** With both structures in place the emitter scan and the
emitter-free read stop rebuilding themselves. Byte-identity was not assumed for
this; it was proven site by site. The old emitter-free domain — *some* visit at
or before the instant was frightening — equals the index's *first* visit at or
before it, because a room's visit days ascend and its first is the minimum. The
scan's sets are the same sets in a different insertion order into a sorted
container. The one predicate now shared between the two paths agrees with the
scan's old inequality on **every** double-precision value, not merely on the
thirty-thousand-point sweep that was run, because the clamp between them can
only move a value across one or zero and the threshold sits at neither. And the
verbatim pre-rewrite bodies were kept as oracles and compared at every position
and every third position, on two seeds.

## The first readout

Every threshold was inherited verbatim from two campaigns ago, frozen before
any of this code existed, and none of them was re-frozen after seeing the
substrate. Because those thresholds compare against tables taken on a
differently loaded box, a **same-box control** was measured in the same session
from a checkout at the campaign's merge base, interleaved run for run. The
pairing is legitimate because both benches' deterministic columns hash to a
single value across all eighteen runs and both trees — the workload did not
move.

The box was not quiet on demand: two other campaigns were resident on it, one
running a debug binary at 833% of a core. Runs were fired only when a poller
saw no other build's process and a one-minute load average below six — a
stricter gate than the rule asked for — and the one-minute average was sampled
every thirty seconds *during* each run, so a spike that landed and decayed
between the endpoint readings could not hide. Two of twelve runs were set aside,
both controls, both to another campaign's suite starting mid-run.

The decisive column collapsed. The fold's cost at the deepest band fell from
**92.0 milliseconds per call to 0.135** — a factor of 681 against the same-box
control, and 540 to 718 against the frozen figure, where The Pawl had managed
1.57 and, against the frozen figure, nothing at all. One fold collapsed and
nothing else did: the two sustenance reads and the two water-belief reads sat
within three per cent of the control in both directions, with their
sensitivities unchanged to two decimals. That is the signature of these two
structures and of nothing else.

And the criterion that asks the fold's cost to stop tracking history **did not
pass**, by 0.045. It fell from 0.93 to 0.245 against a threshold of 0.20, under
all three readings of the filter the criterion carries.

## One change after unblinding, and the clause that got it wrong twice

The residual had a candidate, and the candidate was legible before any
measurement — which is a sentence this campaign has already been burned by, so
what happened next is the interesting part.

The design had specified the emitter-free read as a **prefix** read over the
index. What shipped was the prefix read with the old per-room latest-visit map
still built *above* it, on every call, because the campaign's own plan text —
written by the controller — had told the implementer to "leave the latest block
where it is." On the wandering probe this bench selects, distinct rooms *are*
history: the map holds exactly as many entries as the creature has postings, at
every depth. So the path the design had made prefix-bounded still carried a term
proportional to history. The implementation had been falsified against its own
design, in plan text, exactly as The Pawl's had been one campaign earlier.

The governing rule permits **one** post-unblinding change to production code,
conditional on verifying the mechanism by measurement first, proving
byte-identity, and reporting the first readout unedited. The verification clause
was written to require the map to be at least half the fresh read's cost.

**It measured 24 per cent, and the implementer stopped and reported rather than
proceeding.** The stop was correct and the clause was wrong: the criterion in
question is about a **slope**, and the clause tested a **level**. The map is a
quarter of a total that the fit had already shown to be intercept-dominated —
and it is **72 to 76 per cent of the history-driven growth**, which is the
quantity the criterion is actually about. The clause was corrected to compare
the right denominator, in a ruling written after the comparison under *both*
readings had been recorded and before the change was made. No threshold, no
constant and no criterion moved. What moved was which quantity a verification
compares.

The change itself is forty-two insertions in one function: the map moves below
the emitter-free return, into the emitter path, where it was always the only
caller that needed it.

## The second readout

Taken on the same control, unchanged and un-rebuilt, on a box contested again
by the same campaign. Three of eleven runs were set aside; a fourth was set
aside for a spike it inherited from its neighbour's tail and is reported anyway,
because its readings agree with the four that were kept.

| what was asked | threshold | first readout | second readout |
|---|---|---|---|
| the fear read stops growing with history | under 0.20 | 0.245 — **not met** | **0.04** — met |
| its cost at the deepest band falls tenfold | tenfold | 540–718× — met | **758–1,008×** — met |
| history falls below a fifth of the whole tick | under 20% | 60.80% — **not met** | 59.95% — **not met** |
| no committed byte moves | — | held by construction | held, re-witnessed |
| a repeat read takes no terrain samples | zero | zero — met | zero — met |
| the tick's terrain questions fall tenfold | ≤ 4,469 | 3,168 — met | 3,168 — met |
| the scan's work per tick is bounded by new sightings | slower growth | margin 2.42× — met | margin 2.42× — met |
| what the two structures hold | no threshold | recorded | recorded, unchanged |

The fold's cost at the deepest band is now **0.096 milliseconds per call**,
against a same-box control of 93.8 — a factor of 975 — and its sensitivity to
history is 0.04 against a control of 0.91. The whole of the remaining cost sits
within six microseconds of the fitted intercept measured on the same runs: **a
cost that equals its own floor has no history term left to have a sensitivity
about.**

The falsifier — that the fixed floor might rise by more than the history term
saves — could not fire, and its margin widened. The floor **fell**, by 25.1
milliseconds per tick, so the two lines cross at a session length no session can
have. Across three readouts of this program that crossover has walked from +26
to −13 to −21.

The level, reported and not predicted: on the agent-scaling instrument at two
hundred agents, four valid pairs give a median of **−9.27 per cent** (the first
readout, on three pairs, gave −8.55). Every rung moved by nine or ten per cent,
and the fitted scaling slope is unchanged at 1.09 on both trees — the campaign
lowered the level without changing the shape, which is what a per-agent read
getting cheaper looks like.

Two instruments still disagree by about a factor of two on how large the saving
should be, and the second readout does not resolve it. The structural
explanation stands unverified: one instrument drives the walk once with
persistent caches, the other evaluates it twice through the full turn, so the
removed fold is paid twice in one and once in the other. That the ratio
reproduced at the same size on an independent session is new information and
makes a structural cause likelier than a noisy one.

## The criterion whose own filter emptied on success

The frozen criterion counts only runs whose fit clears a goodness-of-fit floor.
On the control column it admits four of four. **On the campaign column it admits
none of four** — and not because of noise. The slope is now 0.023 against an
intercept of 90.6, so there is no slope left for a line to explain, and a fit to
a flat scatter has a poor fit *by construction*.

The filter was frozen when this fold read 1.06, and its job was to exclude a
contended run whose fit had fallen apart. It did that job correctly twice in
this campaign, catching exactly the two runs the load rule caught independently.
Applied to a criterion that has succeeded, it empties the sample.

The readout reports it that way rather than resolving it silently in either
direction: every threshold that admits at least one campaign run gives 0.03 to
0.04, far under the frozen 0.20; the printed threshold leaves the statistic
undefined. A goodness-of-fit filter on a criterion whose success destroys the
fit is a filter that fails at the finish line, and this is the second clause in
two days to name the wrong quantity for the same criterion. Both are handed
forward: a criterion about a slope wants an effect-size floor, not a fit floor.

## What is left, and whose it is

The campaign's own quarry is not merely gone; it is the fourth-cheapest of the
six timed reads, behind fatigue. Two attributions replace it, both measured
rather than reasoned.

**The tick's remaining history term belongs almost entirely to the water
belief.** The two water reads are **99.08 per cent** of the six folds' cost at
the deepest band — 8.7 milliseconds per call each, with sensitivities of 0.96
and 0.97 — against the fear read's 0.096. That is why the whole-tick criterion
still fails at 59.95 per cent, and it is now provable rather than arguable:
removing the fear fold *entirely*, at zero cost, would move that share by a
fraction of a point. Their cost is not the 121 terrain samples each call takes —
it is a bounded path search run **per known water room per read**, so it grows
with the belief set rather than with the ground.

**The terrain questions that remain are the walk's own, not the fear path's.**
Of the 3,168 questions a tick still asks, the alarm field takes 450 and one
fresh fear read takes 450; the remaining **2,268 — seventy-two per cent** — are
the danger drive's per-step sampling inside the movement loop, once per
candidate room per step. They cost a table lookup each now rather than a field
blend, which is why the tick takes **zero** field samples at all, but they are
still 2,268 questions asked. The fear fold is 28 per cent of the tick's
curiosity about the ground; the live drive is the rest.

## What arrived while this was being built

Three campaigns landed on main during these nine tasks, and each left a mark
worth recording.

[The Reservoir](./the-reservoir.md) arrived first and moved nothing: a
main-first re-measurement, taken on a checkout carrying none of this campaign's
code, showed all three campaign-time hashes unchanged, and the merged tree
reproduced them. Its clean automatic merge nonetheless regenerated an aggregate
artifact by hand, on the standing principle that a clean merge of an aggregate
is not evidence it merged correctly.

[The Rack](./the-rack.md) arrived at the close and **moved the seed-42 walk**.
The campaign's seed-42 hash was re-recorded to main's own value before the
merge — so the merged tree was required to reproduce a number this campaign did
not produce, which it did. The two emitter hashes did not move, and that is a
statement in its own right: The Rack rewrote how a tick reaches the roster and
did not touch the fear path. The session file itself conflicted and was resolved
by hand, taking The Rack's file verbatim and re-applying this campaign's
sixty-three lines onto it.

[The Plumb](./the-plumb.md) arrived in the same absorption and refused two of
this campaign's constants. Its default-deny constant lint walks files, and the
oracle tests now live in a module declared with a path attribute *in another
file* — so the module's test-only status is invisible to it, and two synthetic
fixture values in a test file were judged as though they were world values. They
carry declarations now whose stated reason is honest and whose declared axis is
a misstatement of what they are. The blind spot is handed forward as the
tool's, not the test's.

One more thing nobody predicted: a **clean** automatic merge duplicated a
registry row, because both campaigns had edited the same cell of it. The
document consistency check caught it, which is the third time that particular
lesson has been paid for.

## The constants retire, and the witnesses stay

This campaign minted three hash constants at its first task — one for the
seed-42 walk, two for an emitter-bearing seed — with a positive control that
moved the verdict threshold and watched the hashes follow. The control is worth
recording for its *asymmetry*: moving the threshold moved both emitter hashes
and left the seed-42 hash untouched, because seed 42's derived residents carry
no fear verdict that ever reaches a route. A green seed-42 witness was never
evidence about the fear path; it witnessed the walk's blast radius, and the
emitter pair was the load-bearing one.

At the close the constants retire, as the governing rule says they must. A
constant here equals "the whole walk's behaviour on one seed," so it reddens on
any behaviour change by any campaign — a tax on work that has nothing to do with
these folds. What remains runs each fixed script **twice**, on two fresh
sessions, and requires the two to agree; every floor the constants rode on is
kept and checked on both runs, and the hashes are printed so a future migration
has the numbers without this file gating on them.

What that gives up is stated rather than implied: a constant-free witness
guarantees determinism plus its floors. **It cannot detect a behaviour change at
all** — a fold that moved every creature's route would move both runs together
and be witnessed by neither. The values, the control and both main-first
re-measurements are kept in the file as a dated record.

## The honest limits

**One criterion is not met and this campaign cannot meet it.** The whole-tick
history share sits at 59.95 per cent against a threshold of 20. The remaining
term belongs to folds this campaign does not touch, and that is attribution, not
excuse — it is now computable rather than arguable.

**The second readout is not blind.** The first was taken without knowing what
the fix would be; the second was taken knowing exactly what had been repaired
and where to look. That is why the first is reported in full rather than
superseded, and why the falsifier and the level — the two results that could
have gone against the campaign — are stated before the favourable ones in both.

**Nothing evicts either structure.** At two hundred ticks on the fifty-agent
shape the room memo holds 18,902 rooms and about 1.46 megabytes; the index holds
4,665 entries and about 215 kilobytes. Both grow monotonically, with
decelerating increments — the roster running out of new ground, not a bound in
the mechanism. That figure is reported without a threshold on purpose: the
lifecycle stage of the wider programme is gated on this stage producing exactly
this number, and this is the number it enters on.

**One decision rule was left unresolved by its own instrument.** The rule that
would have built a cross-tick affect memo asks for the replay's *share of the
hazard read* — a time. Its committed witness counts, because wall-clock
measurement is banned in this project's tests, and a release-mode probe on the
possession shape measured 785 microseconds per read at 2.62 replays per read: a
whole-read cost, not a share. The memo stays unbuilt and the open question is
recorded with its number and its trigger, which is a *timed* share of ten per
cent.

**One decision rule measured only its cheapest case.** The emitter's trail copy
was counted at zero entries at every tick although every scan found an emitter,
and the mechanism is stated rather than guessed: a member becomes an emitter
when its *home* is frightening, before it has committed a single dated sighting,
so there is nothing to copy. That is a real measurement of the easiest case. A
shape that stresses it would need an emitter with history, and none was
constructed.

**And the placement of one map is guarded by prose alone.** The change that
carried the second readout moved a map below an early return. Nothing but a
comment prevents a future edit from moving it back, and the criterion it would
break is one nobody re-runs between campaigns.
