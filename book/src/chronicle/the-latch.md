# The Latch

The previous campaign drew an axis and left a hole in it. Preconditions were
sorted by how much state they read — nothing, position, derived object
properties, the observer's knowledge, the playthrough's daybook, committed
world facts, another mind — and every precondition in the codebase sat at the
same rung: position only. The Offer filled the empty band in the middle and
named the far end as its sequel's.

This campaign takes the far end. A cave mouth is barred; an act clears it; and
the thing that decides which of those is true is a fold over facts anyone could
have committed.

## A door is not a flag

The obvious build is a mutable field. Give the chamber a barrier the verb
writes, and the feature is one line. It is also a second source of truth for
something the seed already answers, and a world here is a seed plus a ledger
with everything else re-derived.

So the barrier is derived, and only the *change* is written:

```
effective_state(ledger, seed, addr, day, pins)
    = Open                              if some committed passage-cleared fact
                                        names addr at a day' <= day
    = barrier_of(seed, addr, pins)      otherwise
```

Half of this already existed and had never been used. `barrier_of` was built by
The Deep Realm, draws one of four states — sealed, warded, thin, open — for any
chamber address, and until now was read only by its own tests. This campaign is
its first production consumer. On seed 42 it bars **639 of 874** cave-bearing
vertices (215 sealed, 209 warded, 215 thin), which makes the refusal the
majority case rather than a corner: the very first cave-bearing vertex in scan
order is warded.

The other half is one predicate. A `passage-cleared` fact carries the clearing
body as its subject and the *address* as its object, rendered as text — places
are values in this ledger, never minted entities, which is the same shape the
position fact has always used. Nothing new is serialized, no entity is created,
genesis is untouched, and the byte-golden world fixture does not move.

**The `day' <= day` filter is the whole difference between this and a flag.** A
fold over the entire history would look chronologically past the instant being
asked about, so a replayed past would show doors standing open that had not yet
been opened. A mutable flag cannot be asked the question at all. Ask this one at
day zero and the passage is shut; ask it after the clearing day and it is open;
the answer is a function of when you ask.

The subject is not consulted, and that is the point. Any body's clearing fact
opens the passage for everyone, which is what makes this *a door someone else
locked* rather than a private diary entry.

## The latch only ever opens

A cleared passage stays clear. Re-closing belongs with doors, lids and
containers — the same mechanism from three angles — and a closing act designed
now would be designed against one of the three.

Monotonicity also paid for itself immediately. The campaign expected its
largest cost to be cache invalidation: two caches memoize reachability, and
reachability had just become time-varying. Under a latch that only opens, a
stale cache routes the long way round instead of routing a body through a
barrier — a degradation, not a correctness failure. The concern turned out to be
doubly moot, because `delve` is not an action the catch-up replay can see at
all, which two greps established before any code was written.

Only the *thin* barrier yields to clearing, and that choice was made from inside
the code rather than specified from outside it. Sealed rock has no rubble for an
act of clearing to move. A ward's own refusal already tells the player "you
cannot force it" — so a verb that then forced it would contradict prose the game
had already printed. Thin rubble's refusal, by contrast, already promises what
the verb delivers: *it looks like it would not take much to clear.*

## Three outcomes, and a tripwire that predicted its own rename

`delve` once had three answers and had quietly fallen to two: an earlier terrain
epoch made every cave-bearing vertex's entrance chamber resolve, so the
unrealized-chamber refusal became unreachable. The test guarding that was
renamed to say so, and its doc comment predicted the campaign that would rename
it back.

It came back through a different door than anyone expected. The third outcome is
now the barrier, not the chamber; the unrealized branch is still live,
unreachable code. And what actually reddened first was not the scan that test
was built around but its *second* outcome — the assertion that had been silently
relying on the first chamber-realized vertex also happening to be unbarred. It
was not. That shared helper was teaching five other tests the same false thing,
and all five broke together the moment the barrier gate landed.

## The gate that still cannot deny

The Offer shipped a knowledge gate with no reachable denying branch, wired
rather than dormant, and predicted its remedy in writing: *this campaign's
durable objects give it a firing case with no rewiring.*

They do not, and the reason is worth more than the feature would have been.
**The prediction was about durability; the obstacle is addressing.** The gate
takes an anchor kind — hearth, bed, strongbox, altar — an enum of things found
*inside a room*. A cave mouth is not an anchor. It is a chamber address, and it
never passes through anything that takes an anchor kind. The passage module
contains no reference to knowledge, anchors, or the gate at all; the chamber
below has no anchor catalogue whatsoever, its contents being two hardcoded
strings.

So the criterion is recorded as unmet, struck through in the specification with
its reason beside it rather than deleted. A criterion quietly dropped teaches a
successor nothing. An unreachable one shows them the wall. Reaching that gate
needs a new anchor kind and a design for what a cave mouth offers, or an
interior system for chambers — decisions belonging to whoever owns the design,
not to an implementation step.

## What the campaign believed about itself, and had wrong

The specification stated, as settled fact, that nothing a possession session
commits is ever persisted — that there is no world-writing path after genesis
anywhere in the program. It was written into the acceptance criteria, into the
module documentation, into an idea-registry row, and into a decision record.

It is false, and one command retired it. Possession takes an `--out` flag. The
session's evolved ledger and its per-session registry are folded into a new
world and saved. A previous campaign built this deliberately and ruled that a
player's acts are *not* filtered on the way out. A three-line script writes a
world carrying the facts a two-step walk committed, and that world can be
possessed again.

**The error is instructive because nothing was lying.** The ledger field's
documentation says it is "never written back," and that sentence is true: it
answers its author's question, which is whether a session mutates the world it
borrowed. It does not — the input file is read-only. The specification read that
sentence as answering a different question, whether these facts can ever be
saved at all, and the two questions have opposite answers. A doc comment answers
its author's question, not the one a later reader brings to it.

What is actually true is better than what was claimed: **a played world is a
fork, not an update.** Play never mutates what it possessed; it produces a new
world, and only when asked. The default really is session lifetime, which is the
original finding's surviving half.

What this campaign proves is the session claim — a cleared passage stays clear
across many turns, a wait tick, and the creature activity it drives. The save
round trip is *not* proved, so it is not claimed. The mechanism that would carry
it demonstrably works for the sibling predicate committed by the same call on
the same ledger, and nothing about this one differs; that is a strong inference
and still an inference. Writing the test is the cheapest real work left behind.

## What stays open

Re-closing, and with it traps and anything a container needs. The save round
trip, which is a test rather than a design. And the knowledge gate, which now
has a documented reason it cannot be reached and two named ways to reach it,
neither of which is wiring.
