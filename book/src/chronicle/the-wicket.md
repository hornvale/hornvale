# The Wicket

A wicket is a small gate set in front of open ground. This campaign takes one
down — and then finds the same fence built out of a number instead of an enum,
and takes that down too.

Hornvale had been shipping two answers to the question *what is a kind*. For a
creature the answer was composition and had been for a long time: a species is
the string `KindId("goblin")` plus whatever component tables happen to carry a
row at that key. Nothing anywhere holds the definition of a goblin; a goblin is
the join, and a species no table mentions is a key with no consequences.

For an object the answer was the same store behind a fence. The property table
objects reach is `ComponentStore<KindId, ObjectTraits>` — open, string-keyed,
exactly the species shape — but the vocabulary a room's grammar could name was
`AnchorKind`, a closed enum declared by a macro. A kind the enum did not name
could not be placed in a room, however open the store behind it was.

## The fence was smaller than its own advertisement

The essay that named this problem said the enum carried thirty-four variants,
thirty-six exhaustive match sites and seventeen files. Counted at that essay's
own commit, it carried **fifteen** variants, **three** exhaustive match sites,
and eighteen source files. Not drift — the sentence never described any version
of the code. The fence was advertised at roughly 2.3 times its real size, and
the campaign that took it down was scoped against the advertisement rather than
against the fence.

That correction is loud rather than quiet, and in the essay itself, because a
published measured fact that is wrong does not sit inert. It produces wrong
cost estimates from readers acting in good faith, and this campaign was one of
them.

## The deliverable is the guarantee, not the deletion

The enum carried two inherited purposes and only one of them was load-bearing.
It was the grammar's vocabulary, and it was the guarantee that every kind
placeable in a room has a noun, a detail line and a thing-kind row. Closedness
serves the second, and it serves it by forbidding growth — which is the cost.

So the deliverable is stated the other way round: **registry totality replaces
compile-time totality**, and deleting the enum is the consequence. Framed as a
refactor the campaign would have shipped the rename and quietly dropped what
the three exhaustive matches were guaranteeing.

Six default-deny checks take over, and each one states in its own doc comment
which direction it runs. That clause is not decoration. A check asserting
*declared ⊆ resolvable* is structurally blind to over-admission and still reads
as total to the next person, so the direction is written where it is enforced:
every kind the grammar names is a roster row; every roster row has prose; every
prose row is a roster kind; every published handle is a roster row; every
propertied kind is a roster row; and the roster itself is frozen as an *ordered
set* rather than a count, because a size ratchet passes any compensating swap.

The prose pair runs both ways deliberately. The cheapest repair to a one-way
prose check is to delete the offending prose row rather than to add the missing
one, and a check whose cheapest repair deletes the check is not a check.

## A constant where a variant was mandatory

Production predicates must not become stringly typed — `kind ==
KindId("hearht")` compiles — so the thing domain publishes a named constant per
kind that code names. The asymmetry between that constant and the variant it
replaces is the whole campaign in one line. **A variant was mandatory: a kind
with no variant could not exist. A constant is a convenience: a kind with no
constant is a first-class kind that simply has no predicate written against
it.**

The handle list is hand-written, and that is a cost paid on purpose. A macro
generating both the constants and the list would make the list unable to
disagree with them, which is what made the deleted macro's own roster safe and
is exactly what is not wanted here: this list is checked against a third party,
so it must be able to go wrong.

One honest note about that rule: all seventeen rostered kinds happen to carry a
handle today, so nothing in the tree currently demonstrates the absence the
rule permits. It is a licence for the campaign that adds a kind no predicate
names, not an observation about this one.

## The proof is a brazier, and it nearly went in a room no world builds

An open vocabulary that places nothing new is a capability nothing can reach,
which the project had ruled on three days earlier. So the campaign had to place
a kind the enum could not have named.

The first choice was a kneeler in a shrine, and both halves of that were wrong.
The property a kneeler would have carried gates nothing — the sleep verb has
never consulted it — so the kind would have advertised a capability rather than
granting one. And the shrine is a room no reachable world builds: a comment in
the very file the campaign was editing already recorded a forty-eight-seed
sweep finding the role at chamber index 2 to be the loomroom in **24 of 24**
structures that have an index 2, with smithy, hall and shrine at **zero**. That
comment even draws the conclusion in general form — *a gate whose predicate is
false everywhere is not a gate, it is a deletion*. The campaign that cited
unreachability as its reason for placing anything at all would have shipped
unreachability, and the evidence had been sitting in the file the whole time.

So: a **brazier** — a standing pan of coals — beside the loom. It carries the
heat property, and the warm verb is genuinely enforced: it asks every anchor in
the room whether one offers heat and refuses when none does. Until this
campaign a hearth was the only carrier, and a hearth is confined by a
three-link chain to a hearthroom, so no shrine, hall, smithy, storeroom or
loomroom in any world could be warmed at.

The method that enforces the verb had already named this exact future case in
its own comment: *a future heat carrier (a cauldron of coals on
`AnchorKind::Vessel`, say) would have needed an edit HERE as well as a property
row — exactly the M×N dispatcher edit this campaign exists to abolish.* The
brazier is that carrier, arriving to find the edit already unnecessary, which
is a stronger proof than a kind chosen to suit the argument.

That sentence is true of the **verb** and false of the **field**, and the final
review made us say which. Heat has two dispatchers in this crate, not one. The
verb asks the property table and needed no edit — the claim above, in full. The
warmth field, which decides how many degrees an anchor is worth to a creature's
thermal drive, is a separate function one file away, and it is still a literal
comparison against the hearth kind: `kind != kinds::HEARTH`, skip. A brazier
contributes nothing to it, and would not however many property rows the table
grew. What hides the disagreement is the band, not the code — the brazier is a
chamber pattern, and every live reader of the warmth field derives its room in
the walk band, so no body has ever stood beside one while a drive was scored.
Promote the pattern one band up and the offer would say *you may warm yourself
here* over a field reading zero. The campaign abolished one M×N dispatcher and
left its twin standing; naming it is the honest form of the proof.

It reached a real world, and the published transcript is the evidence:

```text
-A small room, holding a doorway, a water jar, a loom and a key.
+A small room, holding a doorway, a water jar, a loom, a key and a brazier.
```

## What it actually cost, which is not what was predicted

The campaign predicted that adding a kind would be five data rows and an
appended pattern — no enum, no match arm, no macro, no dispatcher edit. The
first half held exactly. The second half was too strong, and running found the
difference.

A kind is also a **concept**. The thing domain's documentation lookup ends in
an unreachable arm, so a rostered kind with no doc arm panics *every world
genesis* — a ratchet reachable only at runtime and invisible to any static
enumeration of ratchets. And registering the concept obligated an accession
slot: without a new epoch cohort every genesis would default the kind to epoch
zero and reopen churn a previous campaign had closed.

So the claim that survives is narrower and still worth making: **the room
grammar's kind vocabulary is open, and adding a kind touches no dispatcher, no
enum and no match arm in that path.** The fence came down in the window that
draws rooms; an equivalent one is still standing one layer over, in two other
crates. That is a finding rather than a failure — it is the same shape the
campaign was built to remove, now named and located instead of suspected.

## The same defect wearing a number

Halfway through, the campaign's owner made the generalisation that reframed it:
essentially everything should vary by species, and often by individual, so
watch for constants that ought to be components.

The measured instance was sitting in the creature layer. `FATIGUE_RISE = 0.3`
was one sleep-debt rate for every species in every world, and it was wrong
twice over — one rate for all bodies, and folded against *standard* days when
the quantity is per *planetary* day. A world whose rotation is not one standard
day accrued sleep debt at the wrong rate, and nothing noticed, because an
authored constant is never compared against a local clock.

It is a row now, keyed by species, folded on the planetary day. A species the
table has never heard of reads a documented neutral fallback rather than
silently becoming sleepless — the opposite convention was specified, and the
code path itself refuted it: seven neighbouring species traits all fall back to
a documented neutral, and the inverted rule broke a test inside the very task
that proposed it, when a fixture's placeholder species silently resolved to a
rate of zero with nothing reporting a bad species.

The honest qualification: thirty-nine species rows exist and thirty-eight of
them carry the same number. Only the xorn differs, at zero, because an
ametabolic thing does not sleep — and that row is written out rather than left
to a lookup miss to imply. So the *mechanism* is per-species and the *content*
is not yet differentiated. What shipped is the shape.

## Wait, rest and sleep are three acts, and were one and a half

The owner ruled on the vocabulary too: waiting merely passes time; resting
leaves a body conscious and watchful and returns something; sleeping renders it
unconscious and returns more, and is for most species mandatory.

Waiting was already correct and needed nothing — it advances the clock, runs
the tick, and has no action variant at all, which is the right shape for an act
that transforms nothing. Rest and sleep were one act with two words: a single
variant, with unconsciousness bolted onto the player's path afterwards, so a
creature performing the identical act never went under.

The load-bearing half was arithmetic rather than vocabulary. Fatigue was time
since the most recent rest fact, so *any* rest zeroed it. There was no
representation of how long a body slept, which means "rest gives some benefit,
sleep gives more" could not be said at all — the only value in the system was
debt cleared to zero. Sleep debt is now a **stock**, integrated along the
creature's own timeline: it rises at the species rate per local day awake and
falls, per local day spent down, at the rate the bout's own act repays at.

Two consequences are worth stating because both were found by running rather
than by reasoning.

**The floor was binding in a case nobody had checked.** A sleep runs until the
body's own cycle wakes it, floored at a full bout so a body going under
off-cycle does not merely nap. The doc said that on the ordinary path the cycle
decides and the floor never binds, and named a published golden as its
evidence. Instrumenting that very golden found the floor binding in **8 of 18**
sleeps — cycles of five, twenty, twenty-five and thirty thousand ticks all
overridden — and every binding case was a body bedding down with its night
nearly over and being pushed past dawn, worst case an eight-fold overshoot. The
magnitude claim was right; the frequency claim was wrong; and the fixture cited
as evidence was the disproof. The floor now applies only to a body going under
while awake, which is the case it was written for.

**A regression hid in the boundary between the two halves.** Moving the rise
term to the local clock while leaving the fall terms on standard days looked
like a clean scope boundary and was a bug. A sleep bout is roughly half a
*local* day, so accrual per waking phase became invariant in the day length
while repayment still scaled with it — break-even at a 7.2-hour day, and the
rotation pin admits four to a hundred hours. A legal world existed in which a
saturated creature could never recover, where the pre-campaign model had a
fixed margin on every world because both terms carried the same day. Both terms
convert now.

## The place grades the sleep; it never gates it

Three doc comments described the sleep verb as gated on a rest-affording
object, and one described resting as requiring a home. None of it was true.
The verb refuses a non-empty argument, charges the clock, commits the bout, and
asks nothing whatever about the room; the creature layer already agreed and
said so — a body *sleeps where it is*.

The comments were not inert. An earlier draft of this campaign's own
specification reasoned from them and asserted, on their authority, that a
shrine offers nowhere to rest. That sentence was false and it justified the
wrong proof kind.

The ruling that settles it: **sleep is never gated; the place grades it.** A
creature must be able to pass out in the middle of the road, and prefer a bed,
or a fur, or bracken, when it can get one. So the rest-affording property was a
*grade* misfiled among a vocabulary of gates, and repayment now scales with
what the room offered the body that lay down in it — one on bare ground, so a
body that never reaches furniture folds exactly the arithmetic that shipped
without the grade.

Three design points fell out of it, each declined for a stated reason rather
than for convenience.

The grade asks the room's *offer* rather than comparing kinds, so a future
rest carrier — a fur, bracken — needs only its property row. But it
deliberately does **not** pass through the observer's knowledge, on three
grounds in ascending force: the knowledge gate governs what a body is *told*
and physical restoration is rendered to nobody; the creature path carries no
knowledge structure at all, so the gate could only have been faked or applied
to one of the two routes; and that query's own doc records, measured, that no
live session can present it with a knowledge state that fails — a check that
reads as live and can never fire is worse than an admitted absence.

The site is read at the **bout**, from the position the ledger already records,
rather than at the query instant. A grade read at query time would be
non-monotonic: a body that slept on a bed and then walked into the road would
have the bed's repayment retroactively withdrawn.

And the read and the mover reach the grade through exactly one door. They had
already diverged once on this fold, so the repair was not a test watching two
call sites agree but a single function that builds the whole argument list —
species rate, local day, rest sites — leaving only the one parameter that has
always been the honest difference between reading fatigue and moving it.

## The limit this leaves, which belongs to the owner

The grade is **locale-granular by construction**, and that is a real distance
from the ruling rather than a rough edge. In a built, cold locale the grade
fires everywhere in that locale, so a player who passes out in the street is
repaid exactly as one who found the bed.

It cannot simply be tightened. Grading per anchor would require the fatigue
fold to know which anchor a body occupied, and the project has ruled that fine
position is never serialized: the ledger carries the room, not the spot in it.
So making the grade anchor-granular is a decision about *that* ruling, not a
refinement of this one, and it is left open rather than approximated.

**Refined by [The Pallet](the-pallet.md), 2026-09-03, on the multiplier and not
on the limit.** How much a furnished room is worth is no longer one number for
every creature: it is a table over the species roster, seven distinct values
derived from whether a kind is endothermic (only an endotherm pays the
conductive-heat bill a surface relieves) and whether it is settled (only a
settled kind builds bedding sized to itself). The number this section describes
survives as the table's ceiling, so nothing gains more from a bed than it did
here, and the peoples it was calibrated for keep it exactly. A body also now
*chooses* which anchor it lies on, within the room it is already in, and the
world records the kind it chose. **The limit above is untouched by all of that,
and for the reason this section already gives.** The fold still re-derives from
committed facts, and the finest place any fact names is the room. What changed
is that the durable half of the choice is now in the ledger, so the campaign
that wants the fold to know a bed from a heap of bracken has a fact to read
rather than a position it may not keep.

The inversion one might fear cannot occur: the fireside bed requires a room
that is both built and cold at either band, so there is no world where a
chamber has a bed and its locale does not.

How much of the world this grade can fire in was read off a standing
measurement rather than a throwaway probe. The share of a world's built
settlement rooms that read as cold has been measured at a thousand worlds since
The Range, and the survey reports it: a median near a fifth, a lower quartile
under a tenth, a mean near a quarter, and no world at all without a reading. A
grade confined to cold, built rooms is a grade most rooms do not carry, which
is the intended shape and not a shortfall.

The general lesson is smaller and travels further. The first version of that
argument was a probe written and deleted inside a task — evidence nobody can
re-run, at five seeds. The committed survey had been measuring the same
quantity at a thousand worlds for weeks. When a task needs to establish that
something is reachable, look for the column before writing the probe.

## What the essay's orange is still waiting for

Two of the three additions the object model owes are still unbuilt, and one of
them stopped being hypothetical here. *What a people tends to sleep on* is a
pair of kinds — a species and a thing — which is precisely a kind-to-kind edge,
and it is a better example than the orange tree because something in the world
is now asking the question. *This one just likes a sleeping bag* is the
per-instance half, derived from lineage rather than stored.

After this campaign both are a new table and nothing else: no enum, no match,
no dispatcher. Leaving room for them was free. What would not have been free is
entrenching the wrong shape on the way past, which is why the false comments
were corrected rather than left, and why the ungated behaviour is now pinned by
a test whose stated purpose is to reject the tidy-looking fix.
