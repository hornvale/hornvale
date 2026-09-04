# The Tenon

A tenon holds because two pieces were each cut to suit the other, and neither
piece names the other. That is the relation this campaign adds: what a surface
is worth depends on the surface and the sleeper, with no table of named pairs.

The predecessor to this work left three deliberate limits. A body chose a
sleepable kind and committed its name, but the choice was a tie-break rather
than a preference. Recovery still asked only whether the room afforded rest.
And one bed was the only rest-supporting object in the world. The record could
say *slept on bed* while the physiology could not make that fact matter.

All three limits move here. Three natural surfaces enter the grammar. The
chooser ranks the surfaces present in its current room. The fatigue fold reads
the kind the body committed and grades the pair.

## An edge with no pair table

The obvious representation is a matrix:

```text
                 bed   rushes   ledge   bracken
  human          ...     ...      ...      ...
  drow           ...     ...      ...      ...
  gully-dwarf    ...     ...      ...      ...
```

It is also the wrong cost model. A new thing requires one row for every
species, and a new species one row for every thing. The one-kind programme had
just made adding a kind cheap; an M×N matrix would make the next addition
expensive again.

Instead each endpoint carries its own traits. A surface supplies an offer, a
substrate, and — for a natural substrate — a hardness. A sleeper supplies the
old best-site grade and a response curve over hardness. The consumer combines
them:

```text
grade = 1 + (species_grade - 1) × offer × fit
```

`fit` is one for a made surface. For a natural surface it is the sleeper's
curve evaluated at the surface's hardness, floored at 0.2 so a poor fit can
make a site less helpful without turning it into a penalty. The two endpoints
are ordinary component rows. Nothing holds a `(species, thing)` cell, and
neither endpoint names the other.

This is a rank-2 relation. A species scalar times a surface scalar is rank 1:
it can make one creature care more than another, but it cannot reverse the
ordering of two surfaces. The extra contrast — substrate response against
hardness — makes a reversal possible. A third contrast is absent because no
consumer asks for one.

The motivating sentence had to change actor. Hornvale's dwarves carry no
subterranean distinction; inventing one to save a sentence would be authoring
a trait for its example rather than its world. Drow already carries the
roster's live subterranean row. In seed 1234, in actual rooms containing both
surfaces:

```text
  gully-dwarf   ledge 1.136008901  <  rushes 1.344455629
  drow          ledge 1.294000000  >  rushes 1.079841873
```

That is the first shipped kind-to-kind edge in this programme: not a graph
object, but a relation derived from both kinds and producing a live reversal.
A general edge graph waits for a second edge kind with a consumer, because
direction is not a detail one generic container can guess. `grows-on` is
directed; `family_of` is symmetric and transitive; rest fit is symmetric while
the payoff goes only to the sleeper.

## The marker and its numbers stay in one row

`SupportsRest` used to be only a marker. The surface scalar could have lived in
a second registry, joined by `KindId`. Hornvale already carries the failure
case for that arrangement: `RadiatesHeat` has hearth and brazier as carriers,
while the warmth field still dispatches only on hearth. The marker grew and
the number beside it did not.

So `RestSurface` lives inside `ObjectTraits`, beside the property it gives
meaning to, with a two-way invariant:

```text
  SupportsRest present  iff  RestSurface present
```

One-way would be a familiar false totality: it catches a missing surface for a
marker and not an orphan surface without the marker. The two-way form catches
both. Its visible type cost is that `ObjectTraits` no longer implements `Eq`,
because a real-valued compute trait does not pretend to have total equality.

## The fold reads what the body did

The stable thing the ledger may record is a kind, not an anchor identity.
`slept-on = ledge` survives reload; *anchor 3 in this room* does not and must
not. Recovery now makes a second ordered merge over those facts alongside the
position merge it already performed, then grades that kind.

The old room boolean remains for a reason. Conscious rests do not commit
`SLEPT_ON`, and neither does any world saved before The Pallet. With no kind
fact, the fold uses the old `Bare` or `Afforded` room result. That is durable
compatibility, not a second mechanism authors may choose between.

This corrects the predecessor's current-tense description without rewriting
its history. Rest is no longer uniformly room-level where a sleep-kind fact
exists. It is still not anchor-level: two ledges in one room are the same kind,
and the fallback remains room-granular.

## Preferring is not travelling

The chooser now takes the highest grade among sleepable anchors in its current
room. Equal grades retain the lowest anchor identity: candidates arrive in
ascending order, and only a strictly greater `total_cmp` result replaces the
incumbent. No floating-point value enters a sort comparator.

This explicitly amends The Pallet's warning that a chooser guaranteed to find
the optimum would destroy the diagnostic. Within-room argmax does guarantee
the room's optimum. The purpose of the warning survives in the boundary the
chooser does not cross: it never searches another room and never proposes
movement. A body can choose bracken here while a bed stands next door, or find
nothing and sleep on the ground. Bad sleep remains a readout on reachability,
layout, movement, and drive order; it is no longer manufactured by ignoring a
preference inside the room.

## Three surfaces, four quadrants

Pattern composition is admissibility, not probability. Any locale pattern
whose gates pass appears in every qualifying room, so an ungated surface for
every built/wild and cold/warm combination would have erased bare ground from
the world. The authored set deliberately fills three cells and leaves one:

```text
                    cold                    warm
  built             bed + rushes + ledge   ledge
  wild              bracken                 bare
```

Rushes and bracken are soft natural surfaces. Ledge is hard. All three have
the preregistered offer `0.7`; their hardness values are `0.1`, `0.85`, and
`0.1`. None was retuned after the measurement.

The concepts, thing rows, prose, patterns, and accession cohort were appended
as data. The epoch was visible where expected: the concept/world registry and
the affect trace changed, while stream manifests, the census, Domesday, and
scene goldens did not.

## What the worlds said

The same 24 seeds, 40 ticks, and 10 bodies per world were measured before and
after the surfaces arrived. The historical probe found 601 afforded bouts of
5,096, a share of **0.1179**. The live probe found 2,926 of 4,859, a share of
**0.6022**. Bare ground remains: 1,933 bouts, **0.3978** of the live total.

All three surfaces reached bodies' walked rooms:

```text
  rushes    34 rooms in 11/24 worlds
  ledge    163 rooms in 24/24 worlds
  bracken  402 rooms in 12/24 worlds
```

A live drow in a room containing a bed and a ledge grades the bed
`1.500000000` above the ledge's `1.294000000`. Every run reached its requested
tick count.

The selection counts are the result that must not be rounded into a success
story:

```text
  bed       311 committed SLEPT_ON facts
  bracken   463
  ledge    1321
  rushes      0
```

Rushes existed in 34 walked rooms across eleven worlds and appeared in **zero**
committed sleeps. The null was not a stop condition, and it was not tuned away.
It says the surface is reachable while the whole choice-producing stack never
selects it under this sweep. That is exactly the kind of distinction the
sleep-site diagnostic was built to expose.

## What remains

The one-kind model has shipped its second addition, but not a universal graph.
This derived edge is the evidence a later abstraction must preserve, not a
claim that all relations share its shape. Per-instance variation derived from
`Lineage` remains the third addition.

The measurement also leaves two narrower debts. `offered_by` still builds the
whole object registry on each call outside the fold path this campaign made
cheap, and laboratory simulation still swallows commit errors by truncating a
run. Both are recorded in the idea registry rather than widened into this
epoch-bearing campaign.

A tenon is useful because the pieces meet without either carrying the other's
name. The world now has that joint — and one soft surface that no sleeper in
the measured worlds chose.
