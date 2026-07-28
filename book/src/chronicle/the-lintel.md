# The Lintel

One word was doing two jobs, and nobody had checked the arithmetic.

The world's canonical grid resolves at roughly a hundred and ten kilometres per
cell — a figure chosen years of campaigns ago so that a thousand-world census
finishes in an afternoon. A possessed body does not stand on that grid; it
stands six refinements below it, and each refinement halves an edge. Six halvings
of a hundred and ten kilometres is about one and seven tenths. A *room*, the
thing a walking creature commits its position to, is one and seven tenths of a
kilometre across.

That is a deliberate and defensible size. It is the scale at which "something
worth noticing every so often" is the right design target for crossing
wilderness on foot, and the map the possession draws has always been honest
about it. What was not defensible was that *The Hearth* had furnished that same
place with a hearth, a bed, an alcove and a water jar.

Nobody noticed for two campaigns, because nothing in the code says how big
anything is. The number is a product of two constants that live in different
crates and never meet: the grid's level, and the possession's offset from it.
The collision surfaced during a design session, from someone multiplying them
together for an unrelated reason, and it had already produced a wrong design
downstream — a metric lattice of cells, drawn at landscape scale, which would
have been the right picture of the wrong thing. Design caught it. Implementation
would not have; every test would have passed.

## Two bands, one address space

The fix is vocabulary before it is code. A **locale** is a macro place: the
walk band, one and seven tenths of a kilometre, exhaustively tiled — every
direction you can face has one, because the mesh covers the sphere. A
**chamber** is a micro place: human scale, and *sparse*, because most of the
world is not indoors. **Place** is the word for either, when the band does not
matter. "Room", unqualified, is retired from new prose; it will keep appearing
in older entries here, which is what those entries said and what they meant.

The two bands are not two address spaces. An address in this world is a base
icosahedral face plus a path of child indices, and the depth of that path is
just how many times the triangle has been quartered. A chamber is a *longer
path* — the walk band plus nine further refinements, which puts it near three
and a third metres. The ceiling is twenty-nine refinements and the walk band is
twelve, so nine is a comfortable spend of ample headroom.

This is the part worth saying slowly, because it looks like a cheat and is not.
Adding depth does not add machinery. The parent of a place is its path with the
last index dropped; that is already how the situated chart zooms out. Descent is
the same operation read downward. What changes at the band break is not the
addressing but three other things — how existence is decided, how connectivity
is described, and what a step costs.

Above the walk band, existence is a given: the mesh is exhaustive, so every
address *is* a place. Below it, existence is a **predicate**, and this is the
load-bearing asymmetry. Four to the ninth is a quarter of a million descendants
under a single locale, and essentially none of them are chambers. Which few are
is derived from a small brief — what the committed history says stands on this
ground, who holds it, at what technology, with what prominence — and everything
else simply is not a place. Had that gone the other way, had every deep address
been admitted as a place, the failure would have been silent and total: a world
of a quarter-million empty identical closets under every meadow. It is asserted
by test for exactly that reason.

## Descent commits nothing, and that was found rather than designed

The expensive version of this campaign changes what a world writes down. A body
standing in a chamber is a body at a deeper address, so if position were a
committed datum, every existing world's saved position would have to mean
something new — a change of era, with all the ceremony that implies.

Position is not a committed datum. Walking has always mutated session state and
pushed onto a trail without writing a fact; the session's own ledger is written
only by the tick that moves other creatures, and never written back. So the
player's whereabouts have never been part of a world's persistent identity, and
descent needs no schema change, no new predicate, and no epoch. Re-entering a
chamber re-derives it identically because there is nothing else it could do.

The byte-identity this program keeps asking for was therefore not engineered
here. It was inherited from a law written for another reason — that fine
position is never serialized — and the campaign's whole determinism argument is
one paragraph long as a consequence. Every committed artifact came out
unchanged, which is why extending the possession transcript to *show* the new
verbs had to be a separate, deliberate act, isolated in its own commit so the
one diff could be read line by line.

## The footgun in the truncation

A chamber's interior depends on whether anything is built where it stands, and
"built" is decided by asking whether the address belongs to a settlement's
territory. That set is keyed at the walk band. A chamber address is not in it —
cannot be in it, being nine refinements deeper — so the naive read comes back
*false*, and a dwelling's interior furnishes itself out of the wilderness
vocabulary. A house full of ferns, and no test failing.

The rule is that band-aware reads **truncate the address to the walk band
before consulting terrain**. It is the same path-truncation move the situated
chart uses to zoom out, applied downward instead: to ask a coarse question about
a fine place, ask it of the fine place's ancestor. Simple enough once stated;
invisible until someone states it. It is asserted directly rather than left to
convention, because the symptom is prose that reads slightly odd rather than a
crash.

## Doors, not compass bearings

Chambers have no bearing relative to one another. Their addresses are identity,
not shape, so *north* inside a house is a question the model cannot answer and
should not pretend to. Movement between chambers is therefore through named
apertures, and `go north` *was* refused indoors.

> **Reversed by The Blocking (2026-07-28), and not a flip-flop.** The refusal was
> correct for a chamber with no interior; The Blocking gave chambers an interior —
> a cell lattice with walls, floor and doorways — so `go north` now means one
> cell north, and the plan marks where you stand. What changed is the *inference*
> drawn here, that a chamber address carries no bearing to walk along; the band
> law itself (metaplan §1b.6, *lateral movement never changes band*) is
> **unchanged**, because a cell step stays inside the chamber band. The paragraph
> above is still exactly right about what it is about: movement **between**
> chambers is by named aperture, because *chamber addresses* carry no bearing to
> each other. `back` stays refused indoors, since it retraces a walk-band trail
> whatever the interior looks like.

Naming the apertures turned out to be where the campaign's one real functional
defect lived. The first implementation named each aperture after something the
prose had said — enter *the hall*, enter *the alcove* — which is the right
instinct, and the same trick the situated chart uses to guarantee a player is
only ever asked to name a thing they were told about. It failed for a reason
that only becomes visible below: the chambers of a structure describe
themselves identically, so their nouns collide, and a list that removes
duplicates collapses to one. Roughly half of all built locales shipped with
chambers no input could reach, contradicting the structure's own documented
invariant that its chambers are connected.

The remedy was to name apertures by *direction of travel* rather than by
furniture. A chamber sits on a path and has at most two neighbours; one of them
is already spoken for by `out`. So the other is **further in**, and the naming
is unambiguous by the shape of the graph rather than by the contents of the
prose. The fix then introduced a second defect two lines from its own edit — the
help text still promising the mechanism the fix had just demoted — which is a
process lesson recorded elsewhere and a good argument for reading whole blocks
rather than changed lines.

## The band changes only at a threshold

The design session's own first answer to "what happens when you leave a finely
subdivided area" was that a body occupies the finest band that has content, so
the world coarsens behind you. It is an attractive rule and it was overturned by
the session that adopted it.

Automatic band transitions reintroduce thrashing one level up. A player pacing
back and forth across a village boundary would change scale on every step, which
is the demand-paging failure the wider program had already named and refused.
The replacement is one sentence: **the band changes only at a threshold, and
thresholds are always visible.** Descent is a deliberate verb; ascent is a
deliberate verb; lateral movement never changes band. The machinery for it was
built two campaigns ago and had been sitting unused — a threshold anchor is
documented as an anchor that is *also* an edge in the coarser graph, the seam
between the two levels, and it was written before there was anything on the
other side of it to walk into.

The attractive consequence survives, for a better reason than the rule that
first produced it. Fine bands exist only inside structures, so there is no fine
band to be in once you step out through the gate.

## Four doors onto one room

The campaign's headline is that a player can walk between a structure's
chambers. That is literally true and experientially thin, and the transcript in
the gallery shows why: both chambers of the seed-42 dwelling read *a small room,
holding a doorway, an alcove, a water jar and a screen* — the same sentence,
twice.

The composer that furnishes an interior takes two facts about a place — whether
it is built, and whether warmth matters there — and nothing else. No address, no
seed. Every chamber under one locale therefore composes the same interior, and
only the header distinguishes them.

**The sameness is not new. Descent is what made it observable.** Every built,
cold locale in the world has composed that identical interior since *The
Hearth*; there was simply never a way to stand in two of them and compare,
because there was only ever one per place. The new verbs did not create the
degeneracy, they built the instrument that shows it.

It is also not cheaply fixable, and this campaign was forbidden from trying.
Differentiating chambers means drawing per chamber over the pattern set, and the
pattern set is frozen at a level above this campaign's authority: since a
creature now stands at an anchor and reads the warmth there, adding or
reordering a pattern changes what creatures have historically done, which is a
change of era rather than a tweak. Order within the set is load-bearing too,
because a pattern is admitted only once the pattern it depends on is present.

So a per-chamber interior belongs to the campaign that takes that epoch — most
naturally the same one that gives chambers a vocabulary of their own, since
freezing the walk band's vocabulary is what deferred both. The design chose that
freeze knowingly, to buy byte-identity, and it should have said out loud that
this was the price. It says so now.

## What is safe to say afterwards

A structure can be walked into one chamber at a time and is left in a single
step: `out` means leave the building, not retreat one room. In a two-chamber
dwelling — today's common case — the asymmetry is invisible, and the footer
never advertises a way that does not exist. In a four-chamber one a player will
notice, and the answer is a named backward aperture rather than an overloaded
verb, which is a decision and not a tweak.

The vertical verbs remain asymmetric on purpose. Fine-ward, `enter` now
succeeds where something stands and refuses with a physical reason where nothing
does. Coarse-ward, `exit` still answers with the old sentence, unchanged
byte-for-byte: *the grain of the world resists; that way lies another scale of
things.* Possessing a settlement, a culture, a civilization is still a deferred
arc. The chart has been allowed to render scales the body cannot enter since
*The Purview*; this campaign is the first time the body caught up with the chart
in the other direction.

Nothing about a world changed. A door opened.
