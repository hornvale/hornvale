# The Cruck

*A cruck is the pair of curved timbers that gives a timber building its
section. Nobody chooses the roofline; it is what the frame allows. This
campaign gives a dwelling's chamber graph a frame.*

The Housemark made a dwelling's threshold carry the people living behind it.
The building around that threshold still carried nothing. A structure's
chambers were counted by a draw from one to four, linked in the order they
were drawn, and named by their position in that line: the first chamber was
the threshold, the second the hearthroom, the third the place's business, the
rest stores. The brief — what stands here, who lives here, whether the
climate is cold — was consulted once, as a gate on whether to build at all.
Its own documentation said so: the brief is a gate and never a parameter of
the draw.

So a bugbear's cold hall and a lizard-folk's warm hut with the same draw were
the same corridor, with the same rooms, in the same order. The world knew
what a building was for and the building did not.

## Form follows use, and the direction is the design

A built structure's chamber graph is now derived. Five rules — threshold,
hearthroom, hall, workroom, store — are walked once against the brief in that
order. Each carries an attachment, either the root or *beside* some other
role, and each carries a condition for being admitted at all. A rule whose
parent role is absent is refused rather than re-hung on the door, because at
this band an attachment is a requirement and not a fallback. What comes out
is a rooted tree at the threshold, and the seed fills exactly what the
derivation leaves free: which facets the chambers stand at, and where the
walls fall inside the plan.

The attachments are read the way space syntax reads a justified permeability
graph — depth from the entry is control — and three readings decide them:

- **Cold nests everything on the hearth.** One fire heats the rooms that open
  off it; a warm-climate plan can afford rooms that open off the door. This
  is physics, and it outranks the social rules under it.
- **Authority sets depth.** A command people's rooms hang off the hearthroom,
  so you pass the head of the house to reach anything. A common people's
  rooms hang off the threshold, so everyone reaches their own room without
  passing another's.
- **Threshold posture decides who reaches the workroom.** An outward posture,
  which offers a guest water at the door, puts the workroom at the door: an
  outsider reaches it without entering the hearth, which is a shop-front. An
  inward posture puts it behind the hearth. A plain posture defers to
  authority.

The store is the household's and never the guest's, so posture does not move
it. No rule reads a people's knowledge basis or its technology, for the
reasons The Housemark and The Staple already gave: one would be a stereotype,
the other is a world clock rather than a place axis.

Four shapes come out of the three axes that vary, and they were written down
before anything measured them. **Deep** nests the workroom and the store on
the hearth. **Bush** hangs the hearth, the workroom and the store all off the
threshold. **Shopfront** puts the workroom at the door and the store behind
the hearth. **Backroom** puts the store at the door and the workroom behind
the hearth. Three of the four have a fork.

The map from axes to shapes is deliberately many-to-one, and the collapses
were stated in advance so that "cold hides authority" would be a prediction
rather than an excuse: cold hides both social axes; a command people hides
posture except at outward; a common people cannot tell plain from outward.

## What a player sees

The flagship dwelling on seed 42 is a backroom. Entering it now reads:

```
> enter
[chamber 978618474755681, day 0.01172]
A small room in Doaba, holding a doorway, a screen, a bench and a stone ledge.
Ways on: out, the hearth, the store.
> enter the hearth
[chamber 978618475628161, day 0.02344]
A small room in Doaba, holding a doorway, an alcove and a stone ledge.
Ways on: out, further in.
```

Two campaigns are visible in four lines. The screen and the bench are the
housemark of an inward, communal people; the ways are this campaign's. On the
seed-14 flagship, which is a bush, the same first line reads `Ways on: out,
the hearth, the loomroom, the store.` — three ways off the door, because a
common people in a warm climate gives every room its own way in.

`Further in` survives where there is one way in, and becomes a refusal that
names the ways where there is more than one. A way is named by its role —
the hearth, the store, the loomroom — never by an index, because an index is
not a reason and the player cannot see it. Siblings never share a role, so
those names are unambiguous by construction.

That naming immediately produced a defect of a kind this project has spent
campaigns mechanizing away: the footer advertised `the hearth` while the
parser accepted only `hearth`. It had been unreachable, because production
had only ever drawn chains and no real session had printed a named way. The
footer's own words are now accepted as typed, pinned in both directions.

## The proof, and the half it could not discriminate

The axes were censused before any production code was written. Over the
1,259 distinct player-addressable built rooms on seeds 42, 13, 7, 1 and 100 —
the same population The Housemark's recovery test accounted for — all twelve
combinations of climate, authority and posture occur, and cold is not
constant: 399 cold rooms against 860 warm. Every one of the four shapes has
living support, from 110 rooms to 611, so no row of the table shipped
unwitnessed.

Measured again after the grammar was built, over the same population: the
derived chambers and links equal the predicted shape for every room's axes,
1,259 of 1,259, and the per-shape totals reproduce the census exactly — 611
deep, 194 bush, 110 shopfront, 344 backroom. Recovering the shape from the
chambers and links **alone**, without consulting the brief, and then asking
whether the room's own axes lie in that shape's preimage, is also 1,259 of
1,259 with no mismatch. Making the store always attach to the threshold
reddens nine of the twelve axis combinations — the six cold rows and the
three warm command rows — and leaves the three warm common rows green,
which is what the campaign predicted in advance and, on the second attempt,
actually observed rather than inferred.

The honest limit is in the first of those two claims. Its roles half compares
production's roles against the same pure function production calls, so it is
tautological; the discriminating half is the links, checked against a table
transcribed by hand from the design. The recovery result is the one that
carries weight, because it never sees the brief.

## What the seed still fills

One draw per chamber for its facet, and one draw per cut for the walls —
the residual degrees of freedom and nothing else. A fully derived topology
leaves the seed nothing at the topology, which is the point.

The two embedders had both assumed a chain in their own documentation, and
both were one substitution away from a tree: the previous chamber becomes the
parent. The rectilinear allocator now cuts a node's region, then cuts the
remainder perpendicular into one strip per child, so every child shares a
wall with its parent by construction. Over every rooted tree on four chambers
or fewer and 256 seeds — 2,560 combinations — it produces a lattice passing
all eight checker rules with no chamber below the minimum span and its
freedom accounting exact.

The growing method does not reach that bar, and the campaign records the
narrowing rather than hiding it. Its parent generalisation realizes the fork
on 2,536 of those 2,560 combinations and drops exactly one doorway on 24 —
fifteen seeds on the three-child root and nine on the node with two children
one level down — where a one-cell root in the interior's corner has two
doorways to give three children. Every remedy tried also moved grown cave
bytes, which this campaign had promised not to move. Production never hands
the growing method a fork, because wild sites are chains and built sites are
allocated; the twenty-four failures are pinned by tree and by seed, so the
set cannot move in either direction unnoticed.

## The cave keeps its chain, by argument

For a built site, form follows use. For a wild one — a cave, an exotic site —
the derivation has always run the other way: the rock made the form and a
people reads it, which is The Plat's finding. A cave has no function, no
notability and no housemark, and cold alone is not a reason for a cave's
shape. So a wild structure keeps today's drawn chain byte for byte, under an
unchanged stream label, and the built path draws its facets under a new one.
The two paths share their addressing code and nothing else, which is why the
split is by method rather than a version bump: bumping the shared label would
have moved every cave in every world for no reason at all.

## What remains outside the door

A structure is a tree, so it has no cycle and no courtyard: a ring of rooms
needs an embedder the current two cannot be, not another grammar row. A
settlement still gets one building at one facet, so districts and plural
buildings remain the third rung of the ladder this campaign is the second of.
A wild chain's roles are still read off an index, which gives a four-chamber
cave a threshold, a hearth and two stores — a duplicate the campaign kept
byte-for-byte rather than quietly correcting; the reading it deserves, an
entry, a heart and a sanctum found by the plan's own metric, is a later
campaign's. A ruin does not remember its builder's grammar: a ruined
dwelling's shape is derived from the brief as it stands, and nothing on that
brief carries who built it. The growing method's twenty-four fork failures
stay as a witness, waiting for the day something built is grown. And the
one-room built dwelling is gone, deliberately: a hearthroom is always
admitted, so every built structure now has a fire, where a one-chamber cold
dwelling used to have none at all.

The Staple's second rung is shipped. A building's shape now has a reason, and
a player can read it from the door.
