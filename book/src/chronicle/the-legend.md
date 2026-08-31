# The Legend

The game client had been assigning display characters one pane at a time for
four campaigns, with no central table and no rule about which pane could
claim which mark. Nobody had gone back and checked what had accumulated.
Checking found three characters each carrying two or three meanings: `.`
meant land, floor, *and* relief band 2; `+` meant threshold, "everything
else in view", *and* a water class; `#` meant wall *and* settlement. The one
character every pane agreed on — `@`, the possessed body — was also the only
one ever assigned by a rule rather than by hand. That asymmetry is the whole
campaign in miniature: a glyph assigned by rule cannot collide with anything,
and a glyph assigned by hand eventually will.

## The register, and the rule it enforces

Decision [0389](../../../docs/decisions/0389-a-glyph-carries-order-or-identity-never-category.md)
states the rule a glyph must obey: it carries **order** (ink ascends with a
quantity — a density ladder like `_ . : ^ A`) or **identity** (the character
is the referent's own initial, like `@`), and never an arbitrary category.
Twenty-two candidate biome glyphs, considered and rejected two campaigns
earlier, were the concrete instance this generalises: nothing about `%`
implies "swamp", so a nominal set needs a legend forever, while an ordinal
ladder needs none — the reader decodes ink density unaided. Colour carries
category and substance instead, weight carries attention, position carries
position; a character means one thing across the whole client, full stop.

That rule now has an enforcement mechanism, not just a sentence: a single
`REGISTER` table (`clients/game/core/src/register.rs`) binds every glyph
this campaign's three renderers use to exactly one population, and a test —
`no_character_is_bound_twice` — fails the build the moment two populations
claim the same character. The three collisions above were resolved before
that guard went in, by rule rather than by taste:

- **`.`** stays the chamber floor and relief band 2 (Ruling K) — the two
  meanings sit at different *scales* (an indoor floor plan, an outdoor
  relief ladder) that a player is never looking at simultaneously, so one
  character can mean both without ever meaning two things in the same view.
- **`+`** becomes threshold alone. The walk band's "everything else" use —
  the fact that a creature, a boulder and an unexplored tile were all `+`
  before this campaign — is gone; a water class moved elsewhere on the
  register.
- **`#`** becomes wall alone. The settlement glyph moves to `o`/`O`
  (lowercase and capital by size), which doubles as the fix for a second
  problem: `o` was also an ADoM-style creature initial, and Ruling AG
  resolves that second collision by *layer* rather than by character — the
  world map keeps its cartographic `o`/`O`, and creature initials are
  claimed only in the walk band and the floor plan, two views where a city
  marker never appears.

## What a session actually draws now

The world map went from two characters to a full ordinal vocabulary: six
relief bands (`abyss`, `shelf`, `lowland`, `upland`, `highland`, `alpine`),
three water classes (ocean `~`, salt basin `=`, river `"`), cave mouths
(`*`), volcanoes (`!`), waterfalls (`|`), and settlements sized by
population (`o`/`O`). The walking view stopped drawing one glyph for
everything in sight and started reading terrain texture: a five-rung
impedance ladder (`_ . : ^ A`), ported unchanged from the sim's own
`surrounds_ascii.rs` so the client is never a second implementation of the
same formula, only a second *caller* of it.

**A creature is no longer the same character as a boulder.** Every creature
draws its own noun's first letter — `creature_glyph`, a pure function of the
noun alone, no authored species table, no per-render bookkeeping. An earlier
version of this rule assigned letters by a greedy per-render collision
search, so a goblin drew `g` alone and `o` the moment a gargoyle also stood
in view — the same species changing its own letter as its company changed,
which is not identity, it is a slot number. Nathan's correction stands as
shipped: **a species always draws its own initial**, full stop. Two
same-initial species — goblin and gnoll, both `g` — now deliberately draw
the same letter. That is not a regression; it is the collision decision
0389 accepts, because the glyph's job is to say *something worth attention
stands here*, and `examine` — reading the mark's own noun — is where the
detail actually lives. Letters are unavailable to anything else in the walk
band and floor plan for exactly this reason: `Creature` owns the whole
`a`-`z`/`A`-`Z` codespace there.

The hearth was the one piece of this campaign that needed sim-side work
rather than a client rewrite. A chamber's furnishings — a hearth, a bed, an
alcove, and eleven more kinds besides — never reached the wire at all;
`windows/vessel`'s session now emits one furnishing mark per lit, unclaimed
anchor, and the client draws every kind under one glyph (`?`) rather than
one per `AnchorKind`. The reasoning is the same division of labour as the
creature rule: the mark says *something is here*, `examine` says what, and
fourteen (and rising) furnishing kinds would either need fourteen more
characters — reintroducing exactly the nominal-set problem decision 0389
exists to prevent — or a shared one, which is what shipped.

## The measurement: extracting a classifier is free

The elevation classifier `plate.rs` used to compute for itself is now one
function, `hornvale_scene::relief_band`, called by both the world map and
`windows/scene`'s tile builder for the `scene/surrounds/v2` wire field. The
preregistered hypothesis was that pulling a decision out from where it was
computed to where it is shared would cost nothing measurable at the rung a
player actually holds a key down to reach — the coarsest, where a redraw
happens on every keystroke of a held pan.

It held. Three separate measurements of a warm 200×200 redraw at that rung,
five replicates each, landed at 0.049–0.068 ms — comfortably inside the
0.20 ms bar (a ~3.5× allowance chosen specifically because a looser one
would have passed a real regression silently) and close to the prior
campaign's own 0.056 ms baseline for the identical quantity. A refactor of
*where* a decision is written, not of *how* a tile is addressed, cost
nothing, as expected — but an expectation is not a measurement, and this
campaign's own working notes record more than one prior case of a plausible
number substituting for one actually taken.

## The coverage audit's first unmet item, closed

`docs/audits/system-coverage-wolverson-2021.md`'s item 2.1, "Entities and
Components", had read `absent` since the audit was first taken, with a note
naming this campaign's own opening problem in so many words: *"a creature
and a boulder are the same character, on every seed, with no flag that
changes it."* It now reads `present`: the sim half was always there (an
entity is a ledger subject, its components are the facts committed about
it), and the render half — the half that was missing — now distinguishes an
entity from terrain at the glyph, in both surfaces where the corpus scores
it. Per-entity colour still does not exist anywhere in the client; that gap
survives as the same secondary shortfall a neighbouring item's `present`
verdict already carries, and it is not this chapter's deliverable.

## A capacity observation, and the design that actually answered it

Two campaigns ago, an idea-registry row (`CLIENT-glyph-carries-biome`)
correctly observed that a cell carries a biome, a relief band and a water
kind, while the client drew only `~` and one land glyph — the largest
unused rendering capacity in the client. Its proposed answer, twenty-two
biome glyphs, was rejected on legibility grounds before this campaign
began. **What actually shipped is a different design, and it is worth
saying plainly: the capacity gap is now answered ordinally, not nominally.**
Six relief bands plus colour on the world map, a five-rung impedance ladder
plus colour on the walk band, three water classes — an ordinal ladder any
reader decodes without a legend, exactly the property the twenty-two-glyph
design lacked and the reason it did not ship.
