# The Chattel

*Chattel is property that moves. The word is the whole campaign: an object
whose identity travels independently of the place it happens to be standing,
so that carrying it somewhere does not make it a different thing.*

Before this campaign, everything in a Hornvale room was furniture. A hearth, a
loom, a water jar — the interior grammar composed them, the prose named them,
and [The Offer](./the-offer.md) had even taught the world which verbs each one
would accept. But none of them were anything. They had no identity the ledger
could hold, so nothing could be said about one that would still be true in the
next room. You could look at a key. You could not pick it up, because there
was no *it* to pick up.

[The Latch](./the-latch.md) had made the first durable state a player could
change — a cave mouth that stays cleared — and had done it with a private,
monotone flag on a passage, because a passage was the only thing in the world
that needed one. The Chattel's thesis is that the flag was the wrong shape and
the passage was the wrong scope: open-and-closed is not a property of
doorways, it is a property of *things*, and a passage is one.

## Identity before existence

The design turns on a single choice, and everything else falls out of it.

A thing's identity is a **pure function of where the grammar puts it**: the
room's packed facet, the kind, and an ordinal. `thing@1734/strongbox` is the
strongbox in that room, and it is that entity in every world derived from that
seed, in a ledger that has never been written to, on a machine that has never
run the game. Nothing is minted to give an object a name.

That is what makes an object **latent**. Every strongbox, every key, every
alcove that any room's grammar will ever compose already has its identity;
what it does not have is any fact. An anchor stays free — costing nothing,
occupying no storage, appearing in no ledger — until something touches it, at
which point it *promotes*: one `instance-of` fact, and from then on it can
carry state.

So the world does not contain a hundred thousand objects. It contains a
hundred thousand *addresses* at which an object would be, and it pays only for
the ones a player reaches for. Task 1 measured the shape before the design
rested on it: across all 60 production gate combinations, a composed interior
offers between two and seven anchors; asking whether one is still free costs
about 90 nanoseconds against a played ledger of nearly 23,000 facts. A play
that drove every verb at every noun through a whole building promoted three of
the sixteen slots it walked past.

## One mechanism, three angles

The second choice is that **durable state is a fold, never a flag**. Whether a
thing is open is not a boolean somewhere; it is the answer to *what was the
last `openness` fact about this thing at or before the instant I am asking
about*. A replayed past sees the world as it was, because the read is dated
and the history is append-only.

Once state is a fold over a thing, a passage stops being special. A cave mouth
becomes a thing of kind `cave-mouth` whose address happens to be a chamber
coordinate rather than a room facet — one derivation, two spellings — and the
monotone latch retires ([decision
0396](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0396-a-passage-is-a-thing-and-openness-is-its-fold.md),
superseding 0367). A container opens, closes and re-opens through the same
fold that answers for the doorway beside it.

The unification paid for itself immediately in a place nobody had aimed at.
[Decision
0369](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0369-a-gate-is-reached-by-addressing-not-by-durability.md)
had recorded, as an unmet acceptance criterion of The Latch, that the
knowledge gate could not deny a player a passage — and had diagnosed the
obstacle correctly as *addressing*: the gate took an `AnchorKind`, and a cave
mouth is not an anchor. 0369 named two remedies and called them owner
decisions. The Chattel took a third neither had seen: re-key the gate to
`KindId`, the currency the object model already speaks, and a cave mouth
becomes expressible without inventing anything ([decision
0397](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0397-the-knowledge-gate-denies-a-passage.md)).
The gate denies a passage now. The residual is named rather than closed: it
still cannot deny through a live session, because knowledge absorption is
unconditional, and that half needs a different lever.

## The lock, and what a lock is for

Six verbs shipped — `take`, `drop`, `put`, `carrying`, `open`, `close`, with
`lock` and `unlock` beside them — and with them the first precondition in
Hornvale that reads a **second object**: a lockable strongbox opens with the
key in custody and refuses without it.

Making that sentence true turned out to require two arguments the campaign had
not planned for, and both of them arrived as corrections from people playing
the thing.

The first was reachability. The strongbox had been gated on population, which
was a good claim about social scale and a false one about whether anyone could
ever stand in front of one: across a 48-seed sweep, not one living settlement
cleared the threshold. A capability nothing can reach is not a capability
([decision
0398](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0398-a-capability-nothing-can-reach-is-not-a-capability.md)).
The gate came off, and the evidence is now a test that *walks a possession to
a strongbox and finds it locked* rather than one that reads a list of pattern
names.

The second was sharper, and it is Nathan's correction rather than the
campaign's. `open` and `close` had shipped as one boolean doing two jobs, so
closing a chest locked it — and a reviewer soft-locked himself in three moves,
shutting the lid on the key he needed. Four remedies were put to him and he
rejected all four for a model correction: *you can close it, but you should
not be able to lock it without the key in the lock, and if the key is in the
lock it is not in the container.* Closed and locked are different states
([decision
0399](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0399-closing-is-not-locking.md)).

That correction had a consequence the campaign nearly shipped without: if a
lid genuinely means something, and the only key in the world is *inside the
box it opens*, then the box is unopenable and 0398's ruling is reverted by a
different route. The lock is only a lock if the key is somewhere else. So a
second key pattern went in, placed structurally rather than for flavour — at a
threshold, a role a strongbox can never occupy, so the grammar itself carries
*a reachable lock implies a reachable key* with no seed sweep needed to
believe it.

## An identity that travels

The claim the campaign is named for is one sentence: a thing taken in one room
is used in another, and the world file written by `possess --out` carries it.

Custody is a `located-in` fact whose object is the driven body — the same
predicate, and the same fold, that answers "which room is this in" and "which
chest is this in". One predicate whose object can be a room key, a container
or a hand; transitivity read in the fold, so a key in a chest in a room is in
that room without any call site restating it.

Because custody is a dated posting rather than an inventory slot, it survives
the save for free, and the round trip that [decision
0368](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0368-a-played-world-is-a-fork.md)
had left as work is now driven end to end: walk in, take the key, carry it
three rooms further, fold to a world, serialize to JSON, read it back, start a
second session and ask. *You are carrying a key.*

It also found the one thing the save does **not** carry, and it is not the
custody. A `World` holds no instant, so a fresh possession starts at noon of
day zero — and custody is an as-of-day fold, so a take committed later in the
day is invisible to a reader asking at an earlier one. The facts are in the
file, correctly; the question was asked at the wrong time. The re-possession
names its day, which is what `possess --world <saved> --day D` already
exposes. The saved world is the durable half and the instant is the reader's,
the same way an almanac is rendered *at* a day.

The wire carries it too. Carried things ride the `self` channel of the session
snapshot, which contradicted a standing statement in shipped client code that
`Snapshot` carries no inventory by design — so it became a record rather than
an edit ([decision
0400](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0400-custody-is-an-observable-not-a-vital.md)):
custody is an observable, not a vital. What you are holding is something a
witness could see.

## What is honestly not here

Nothing burns, breaks, is consumed, or is made. That is a stated non-goal, and
the campaign's last task made it a checked one: a played session that drives
every promoting verb at every noun through a whole building commits no
`instance-of` fact for anything the grammar had not already offered at the
room it was promoted in. Two mutations hold it — a promotion at an ordinal no
room composes, and a taken key burned to ash in the player's hand.

That test also produced a small finding about the non-goal itself. Its two
clauses are not independently observable in a played session: a destroyed
object stops participating, so destruction is detected as an *absence*
downstream — the burned key unlocks nothing three rooms later — as readily as
as a contradiction in place.

Three things a player can still see are recorded rather than repaired. A shut
strongbox still narrates its contents, and a taken key is still listed on the
floor, because the room's prose renders from the grammar with no ledger and no
latency filter — registered as `PLAY-closed-container-conceals-nothing`, whose
bill was priced when it was found. No NPC takes, drops or opens anything,
though the predicates are agent-neutral by construction, so that is a later
flip with no schema change. And verbs are still Rust variants: the
property-chemistry this advances gained six verbs and kept its two-way
property, but it is not yet a language in which a reaction can be authored as
data.

---

*[Retrospective](https://github.com/hornvale/hornvale/blob/main/docs/retrospectives/the-chattel.md)
· Decisions
[0396](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0396-a-passage-is-a-thing-and-openness-is-its-fold.md),
[0397](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0397-the-knowledge-gate-denies-a-passage.md),
[0398](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0398-a-capability-nothing-can-reach-is-not-a-capability.md),
[0399](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0399-closing-is-not-locking.md),
[0400](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0400-custody-is-an-observable-not-a-vital.md).*
