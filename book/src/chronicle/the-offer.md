# The Offer

The arc this campaign belongs to has one row and one acceptance test: *objects
advertise verbs; a key says unlock-me-with-this, and no verb×object table
exists anywhere.* A survey of the tree found the row hiding two separate
inventions, the second of them much the larger.

**Hornvale has no objects.** Not a thin object model — none. There is no
item type, no inventory (and its absence is *enforced*: a client test fails if
the word ever appears in a rendered strip), no `take`, `drop`, `open`, `put`
or `use` in any dispatcher, and no name-to-entity lookup at all. A typed word
resolves against a per-turn string catalogue rebuilt on every call and yields
a sentence, never a thing. What reads like an object is an **anchor**: a
derived region inside a room's graph carrying exactly two fields, a
fourteen-variant discriminant and the anchor it lies strictly inside. A
strongbox is that discriminant plus two authored sentences. It cannot be
opened, locked, moved, or contain anything.

So the useful frame is not "objects versus no objects" but **how much state an
affordance's precondition reads**, which is an ordinal axis with an empty band
in it:

```
  reads     position                       example                      status
  ------------------------------------------------------------------------------
   0%   nothing                       look, help                     shipped
  10%   position only                 drink at water, enter          shipped -- ALL of it
  30%   derived object properties     a strongbox affords `open`     EMPTY  <- this campaign
  50%   the observer's knowledge      an unseen key is silent        built, unused here
  70%   the playthrough's daybook     a door YOU opened this run     daybook shipped
  90%   committed world facts         a door someone else locked     the next campaign
 100%   another agent's committed mind a door that opens if believed  far future
```

Every precondition in the codebase sits at 10%, and that is structural rather
than incidental: the predicate asking whether an action's precondition reads
committed state is an exhaustive match with no wildcard arm returning `false`
for every variant, and its own documentation names the case that would end it
— a barred door needing unbarring — because the catch-up replay reconstructs a
past that could have happened only while no movement is gated by a committed
effect. This campaign takes the empty 30–50% band and leaves 90% to its
sequel, together with the replay redesign that must accompany it.

## The mechanism, in one line

```
affordances(object, body, observer)
    = { verb : required properties of verb ⊆ properties of object }
      ∩ what this BODY can do
      ∩ what this OBSERVER knows
```

Authoring cost is M verbs plus N objects, never M×N outcomes. A new object
kind declares properties and gains every qualifying verb; a new verb declares
required properties and appears on every qualifying object. Neither edits the
other, and neither edits a dispatcher.

The vocabulary is five properties against fourteen anchor kinds, and each one
exists because a verb needs it — a property no shipped verb gates was cut
rather than kept. `supports-rest` (bed), `holds-liquid` (pool, vessel),
`affords-passage` (threshold), `encloses` (strongbox, alcove), `radiates-heat`
(hearth). Four of the five retrofit verbs that already ship, which makes the
campaign a unification of existing behaviour rather than a speculative
subsystem, and means the mechanism carries real traffic on day one. One row an
earlier draft carried, `bears-weight → climb`, was removed on inspection:
`climb` is not a general climbing verb but underground egress — *"You are not
underground; there is nothing to climb out of"* — so retrofitting it onto a log
would have been a semantic collision presented as a unification.

Properties live in a kind-keyed table held locally by the window that presents
rooms. That placement is what makes the arc cut structural instead of
disciplinary: a kind-level table has nowhere to put per-instance state, so this
campaign *cannot* accidentally climb the scale (decisions 0346, 0348). Two
anchors of the same kind are indistinguishable to the query. Every alcove
reveals its contents; no alcove can be the locked one.

An affordance is also a **relation**, not a field on either side of it: a bed
does not have rest, a bed affords rest to a body it can hold, and the same
anchor answers differently to two bodies standing in the same room on the same
tick (decision 0347). In this campaign body-relativity is deliberately
*additive* — a bed offering rest is a new place to rest beside the existing
one, and nothing a body could do yesterday becomes refused today. The canonical
Gibsonian case that would break that, a body-relative doorway, was named and
declined: newly *blocking* traversal changes where creatures can go and moves
transcripts, which is exactly the scope this campaign was cut to avoid.

## What a player can actually see, which is three things

The whole player-visible delta is the new `warm` verb, one extra clause on
`examine` when what you are examining holds something, and the line for `warm`
in `HELP`. That is the list; there is nothing else.

It is worth being exact about how much of that the offer query produces, because
the honest answer is *none of it, yet*. The query has two production call sites.
One is `examine`'s anchor gate, and the only way it can refuse is the knowledge
check, which nothing in this campaign can make fail (below). The other is
`warm`'s own gate, and it agrees with the hardcoded comparison it replaced on
every anchor kind that exists, because the hearth is still the only thing that
radiates heat. The containment clause does not go through the query at all — it
reads the `encloses` property directly. So the derivation is correct, wired,
and behaviourally invisible: what changes is not what the world says but what
would have to be edited to make it say something new.

## The gate that cannot deny

The offer passes through the observer's knowledge before it is rendered. A body
that has not encountered a thing is offered nothing by it — the seam a *lying*
object would later plug into.

**It cannot deny anything here, and that was established before it shipped
rather than discovered afterwards.** The session absorbs its current room
unconditionally before returning; the projection that would suppress absorption
in darkness takes a perception argument and never reads it; entering a room
descends from a locale already absorbed. There is no live path on which a body
is offered an object whose room it does not know.

The gate ships anyway, wired rather than dormant, on an explicit ruling. Every
surface that reads an offer reads it through the knowledge-gated query, so it is
live code with an unreachable branch instead of dead code, and the next
campaign's durable objects give it a firing case with no rewiring. The branch is
real — neutralise the knowledge argument and a test goes red — but no seed, no
session, and no transcript in this campaign will ever exercise the denying side
of it. That cost is stated in the specification, in the code, and here, because
a section that reads as delivered and cannot be observed working is exactly the
kind of thing a later reader mistakes for a working feature (decision 0349).

## Four surfaces, of which two were never peers

The campaign's premise was that four existing advertisement channels — the
static `HELP` list, `examine`'s authored sentence, a completion hint on the
wire, and the floor plan's legend — would all come to derive from one query.

**Two of the four never advertised an anchor's verbs and cannot, and finding
that out killed a task.** The first two are chamber-band; the wire hint and the
legend are walk-band, and the legend is additionally blocked indoors by an
assertion protecting an existing creature-visibility discipline. Anchors are
chamber-band. Checked against the committed chamber fixture rather than
argued: its narration carries six nouns — a biome, a terrain regime, a village,
the sky and two moons — and **no anchor reaches the wire at all, even
indoors.**

So the unification is honestly partial, and clause (3) of the acceptance test
is recorded as partial rather than smoothed. One surface genuinely derives from
the query (`examine`'s gate). One is tied to the same registry by test, because
`HELP` is a static constant with no object, body or observer to route. Two
structurally cannot without new scope. The implementer declined to write an
"all four move" test that would have read as full coverage, which was the right
call and not the first time in this campaign that refusing to manufacture a
green number beat producing one.

The same finding removed the wire field the specification had promised. A
completion hint listing an anchor's affordances would have been empty for every
entry the wire actually carries — a third artifact reading as delivered while
doing nothing. **The stated cost: this campaign ships nothing a client can
display.** The advertisement is sim-side only. A player sees it through
`examine`; the browser clients do not see it at all.

## The strongbox is silent, and the reason changed

The specification's first rule drew the containment line at *semantic*
containment (a strongbox contains) against merely *spatial* containment (an
alcove is a recess in a wall), and gave the `encloses` property to the
strongbox alone.

A census over all sixty production room-composition combinations measured what
that meant. The grammar's only `within` relation anywhere is a hearth inside an
alcove, three times over, and **nothing is ever placed inside a strongbox** —
the authored strongbox sits *beside* a vessel, a sibling and not a container.
The semantic line had put the property on the one anchor that never holds
anything, and the feature would have reported nothing, forever.

The rule that replaced it is the interactive-fiction one: **contents are
revealed when a container is open or transparent.** A wall nook is both, so its
hearth shows; a chest is neither, so its contents do not. Both anchors carry the
property; the alcove reports and the strongbox is silent.

It is a better rule than the one it replaced rather than merely a different
one, and the reason is where it puts the arc's cut. Open and closed is *durable
object state* — the 90% rung this campaign defers by construction — so the same
boundary falls out of a principle instead of an accident, and the next campaign
inherits a firing case rather than a redesign (decision 0351). One honesty
remains and is written into the property's own documentation: there is no
open/closed state here to gate on, so every carrier reveals unconditionally.
The strongbox is silent because nothing is within it. The rule that would make
that silence *principled* is the next campaign's to enforce.

## The verb that reintroduced the coupling it exists to abolish

`warm` is the one wholly new verb, and it is the live witness for the harder
half of the acceptance test: a verb that declares a required property and
appears on the hearth without the hearth being edited.

As first written, `Session::warm` asked whether any anchor here *was a hearth*.
That is precisely the per-kind coupling the campaign exists to remove,
reintroduced by the campaign's own new verb, one file away from the guard whose
own documentation says it cannot see there. A cauldron of coals would have
needed an edit in the dispatcher as well as a row in the property table — the
M×N shape, restored by hand.

Two things about how it was caught are worth keeping. The first is that **no
behavioural test could have caught it.** A test that walks a body to a real
hearth and asserts the success line cannot distinguish "gated on hearth" from
"gated on bed", because the authored fireside bed *requires* a hearth in the
same chamber: the two anchor kinds are perfectly co-located in every real
interior. Substituting a bed for a hearth in the gate leaves the success test
green. The mutation that does discriminate is a vessel, whose roles exclude the
hearthroom outright. What actually holds the fix is a structural scan of the
dispatcher's own body, asserting it names no anchor kind at all.

The second is that **the acceptance test's fourth clause had to be narrowed to
stay true.** "No verb×object table exists anywhere" is not what is enforced.
What is enforced is that no such table exists in the offer's own module, in one
syntactic shape, scanned from source. A table in another file, reached through
a helper, or keyed on something other than an anchor kind is invisible to it —
and a real instance is in the tree today: the warmth field's own accumulator
skips every anchor that is not a hearth. Both guards state their own direction
in their documentation, including the one that discloses the evasion a reader
would reach for first: extract the gate into a one-line private helper and the
scan stays green while the coupling returns. A check that overstates itself is
worse than a narrower one that says so (decision 0350).

## What the world can now name

The five properties are registered as concepts, in an appended accession
cohort, so a culture can have — or lack — a word for one; shipping a
vocabulary the world cannot name would recreate one layer down the problem
already solved for verbs. They are `Quality` concepts rather than a newly
minted kind, against two recent precedents that minted: both of those were for
categorically new classes — a thing done, a thing undergone — and a property a
thing *has* is what `Quality` already denotes (decision 0352).

The append was verified line by line rather than by line count: five added
lines, exactly the five new concepts at their alphabetical positions, and every
pre-existing root entry byte-identical. That was the campaign's only
determinism-adjacent moment and it came out clean — appending a cohort
disturbed no existing draw, which is what the accession order is supposed to
guarantee and is now evidenced rather than assumed.

## What stays open

Nothing here survives a save, and nothing here can be changed by a player and
found changed later. The offer is derived at the moment it is asked and never
committed (decision 0346), which is the whole of why this campaign could ship
without touching the replay. A chest you open does not stay open, because
nothing records that you opened it; a thing you put somewhere is gone with the
room's derivation. Durable object state, mintable object entities, restricted
passage, and the state machine the open-or-transparent rule is waiting for all
belong to the next campaign, which owns them together because they arrive
together.

And the object that *lies* — the trapped chest that says "open me" — is the
seam's obvious next tenant and is not built. This campaign ships truthful
advertisement through a filter that could carry an untruth. That makes three
registers in three campaigns for one finding: a host unreliable in ways it
cannot help, a hold invisible in the record and visible in testimony, and now
an object whose offer could be wrong.
