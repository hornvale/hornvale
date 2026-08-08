# The Journal — the brief handed to Claude Design

This file is the **artifact**: paste it into a claude.ai/design session. The
reasoning behind it lives in
[`2026-08-07-the-journal-design.md`](2026-08-07-the-journal-design.md); this
file is written to be read cold by someone who has never seen Hornvale.

Everything below the rule is the prompt.

---

## What I am asking for

Design the interface for **Hornvale**, a text-observed world simulation with a
roguelike game client. I want a **design system** — foundations (type, palette,
ink weights, grid metrics, rules) plus previewed components — not a single
mockup image. Give me one coherent visual language I can implement twice: once
as a terminal UI drawn in characters, once as a browser UI that may substitute
sprite tiles for those characters.

I do not have a look in mind. I have a *register*, described below, and a set of
constraints that are not negotiable because they come from the simulation, not
from taste. Within those, the aesthetic judgment is yours — I want it beautiful,
and I want it to work.

## What Hornvale is

A deterministic simulation of an invented planet: astronomy, tectonics, climate,
terrain, species, settlements, languages, religions, individual creatures with
derived beliefs and daily routines. A world is a seed plus a ledger of committed
facts; the same seed produces byte-identical worlds forever, on any machine.

The game is one *client* of that simulation. You **possess** a creature already
living in the world — you do not create a character — and walk around inside it.
The verbs are `look`, `map`, `go <direction>`, `enter`, `out`, `examine <noun>`,
`wait`, `back`, `why`, `knows`, `whoami`, `release`.

Two spatial bands exist. Out of doors you occupy a **locale**, a cell of a
triangular lattice wrapped over a globe, roughly 1.7 km across; the map is a
small neighbourhood of those cells. Indoors you occupy a **chamber**, roughly
3.3 m across; the map is a floor plan of the building. One map surface, two
bands, switching automatically as you enter and leave.

Here is real output from the running program — seed 42, the creature is a
bugbear of a settlement called Googo. Design against this, not against
placeholder text:

```
[room 750518284, day 0]
Tropical rainforest — buttressed canopy, shaded, in a hollow — in the lands of
Googo. The sky above: Night. The vast moon is a smear of light. The small,
distant moon is a smear of light. The sky is a flat overcast.
Ways on: NE, NW, S.

> map
[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]
  +++++
 +++++++
++++@++++
 +++++++
  + + +
  ways on: NE, NW, S
  legend: Googo, bugbear of Googo, buttressed canopy, shaded, in a hollow,
          tropical rainforest

> enter
[chamber 196743868198924, day 0]
A small room, holding a doorway and a screen.
Ways on: out, further in.

> map
[plan: chamber 196743862489068, 2 of 4]
###################
#........#........#
#........#........#
#........+@.......#
#........#........#
#........###+######
#........#....#...#
#........#....+...#
###################
  legend: . the floor, # a wall, + a doorway, @ you

> examine an alcove
A recess cut back from the main space, deep enough to sit in.

> wait 90
Time passes. You sense movement nearby (766 stirred).
```

## The register: a field journal, not a HUD

**The interface is a bound journal the possessed creature keeps.** It is a
diegetic object — a thing inside the world, carried by the character you are
wearing — not a layer of software floating above the world.

It is open to a **two-page spread**:

- **The left page is the plate**: whatever is currently being *drawn*. A sketch
  map of the country, a floor plan of a building, a study of a place, a page of
  a book, an index. This is the large pane and it holds the eye.
- **The right page is the entry**: the written prose, and beneath it the command
  line, which reads as *the next line being written* rather than as a text
  input.
- **Beneath both, the endpaper**: a thin strip of identity — who you are, where
  you are from, what day it is.

Because the map is a *drawing the character made*, it carries no claim to be
true. This matters: it is what lets the map be large without competing with the
prose for authority. The prose is what is happening; the plate is what the
character has recorded about it.

### The palimpsest layer

The simulation tells the client, for every map cell, one of exactly three
states — plus a fourth condition, absence:

| state        | meaning                                   | how it should be drawn                                     |
|--------------|-------------------------------------------|------------------------------------------------------------|
| `here`       | the cell you are standing in              | fresh ink, full weight, the most present thing on the page |
| `sensed`     | you can perceive it right now             | inked, confident, present tense                            |
| `remembered` | you walked it once; you cannot see it now | drawn from memory — lighter stroke, very slightly off-register, hatched rather than filled |
| *not sent*   | never known                               | **unmarked paper** — blank, not black                      |

That last row is the single most important visual decision in this brief.
Every roguelike draws the unknown as darkness. A journal draws it as *paper the
pen has not reached*. Nothing about a journal is unlit; it is unwritten.

When you walk back into somewhere you had only remembered, its cells move from
`remembered` to `sensed` — so the natural motion of the map is a memory sketch
being **gone over in confident ink**. Design that transition; it is the register's
signature gesture, and it is honest, because it is a confirmation rather than a
correction.

### What weight means

Weight — `dim`/`normal`/`bold` in a terminal, the full range in a browser — is
the most important channel in this design, because it is the only one that
survives monochrome, a sixteen-colour terminal, and colour-blindness intact. It
therefore carries the most important axis.

**Weight renders attention: how much a thing commands the character's notice
right now.** One rule, both panes. On the map that is *presence* — the three
epistemic states above. In the prose it is *salience* and *novelty*. A reader
learns the rule once.

Two consequences that are not obvious and are both load-bearing:

- **Weight runs opposite to durability.** `here` lasts one turn. `sensed` lasts
  while you stand there. `remembered` lasts forever — nothing is ever forgotten.
  So the boldest thing on the page is the thing that will be **gone next turn**,
  and the faintest is the thing that will outlast everything. Bold does not mean
  important; it means *perishable*. Design that.
- **There is no weight below the floor — there is absence.** The simulation culls
  an observation once its weighted salience drops under a threshold, and its own
  comment gives the reason: *"a star dimmed to a fiftieth is not a faint star, it
  is a star you cannot see."* So the bottom of the scale is not a fourth, fainter
  ink. It is unmarked paper. Same rule as the unknown map cell, arrived at from
  the other direction.

Colour, correspondingly, carries **substance** — what a thing *is*: its biome,
its water, its relief. Not certainty, not danger, not threat. Colour is the
channel that may fail; it must not be the channel that carries whether you can
trust what you are looking at.

### Reserved, not yet used

Design a full vocabulary for *correction* as well —
erasure, scraping-out, a struck-through mark, a redrawn tile sitting visibly
over an abandoned one — and mark it clearly as reserved. The simulation cannot
yet produce a remembered cell that turns out to be wrong (nothing is ever
forgotten, so memory is always accurate). It will one day. Define the marks now,
show them in the system, and state in the component that they are unused. **Do
not** put them in any composed screen: drawing a feature the simulation does not
have is the specific failure this brief exists to prevent.

## Two laws that constrain every decision

### 1. The cell law

The terminal rendering and the tile rendering **share one grid**. A tile is a
drop-in replacement for exactly one character cell: same box, same metrics, same
position. Switching modes changes *what is in a cell* and never *where cells
are*. Nothing in the layout may depend on sprites existing.

The shipped character vocabulary, which the ASCII rendering must keep: `#` a
wall, `.` a floor, `+` a doorway, `@` you, and on the outdoor chart `+` for a
placed lattice cell.

### 2. The legibility law

This has to be a *good roguelike display* first and a beautiful object second.
Skeuomorphism that costs a glance is a failure. Concretely:

- **Ornament may never occupy a cell that carries information.** Paper grain,
  ink bleed, foxing, the shadow in the gutter, a ribbon — all live strictly
  *behind* glyphs, never over them, and never inside the map grid.
- **Every element is either derived from world state or declared inert.** If a
  mark means something, say what channel it reads. If it is decoration, say so.
  There is no third category.
- **Monochrome at 80×24 is the floor.** If the design only works with paper
  texture, colour, and sprites, it is wrong. Show me the plain-character version
  of the full spread and it must still be usable.
- **Light and dark both ship.** Dark mode here is not an inverted terminal — it
  is *reading by lamplight*: warm, low, the paper still paper.

## What the interface is allowed to show

This is the complete inventory the simulation emits per turn. **Every visible
datum must trace to one of these.** Anything not on this list does not exist and
must not be designed for.

- **Identity** — species, home settlement, that settlement's population, the
  creature's id, the current place's id.
- **Time** — the day (an integer count of standard days) and the turn number.
  There is no clock finer than the day.
- **The sky** — as a written sentence, not as numbers. ("Night. The vast moon
  shows its first-quarter face. The sky is fair, with scattered cumulus.")
- **The place** — its biome (one of 22), its water kind (one of 4), its relief
  band (one of 6, ordered), a fine-grained descriptor, temperature, moisture,
  elevation, height above sea level, and the ways on.
- **Who is present** — each creature's label, and **what it feels**.
- **The knowledge ledger** — every fact the character has come to know, as
  key/value pairs. In practice this holds the places you have stood and the
  settlements you have learned of; a *creature* enters it only if someone told
  you about it.
- **The prose**, and a catalogue of the nouns in it that can be examined, each
  with the text `examine` would print.
- **The map** — cells, with the epistemic state above, plus **marks**: things
  standing on a cell, each with a noun, a kind (`settlement`, `agent`), a line
  of detail, and a rank. Both bands carry marks. Plus a legend.

### The noun is the join key — use it

The noun on a map mark and the noun in the prose catalogue are **the same
namespace, deliberately**. The code says so: the chart's legend entry is "the
same shape as the focalizer's nouns, because that identity is what makes map and
prose two grains of one lens."

So a reader who touches a noun in the written entry can have its mark answer on
the plate, and vice versa, with no new data and no guessing. Design that
linkage — it is the strongest thing the two-page spread can do that a single
pane cannot, and it costs nothing.

### What colour has to carry, and why it cannot be a swatch set

The substance channel is bigger than it looks. Every map cell carries a biome
(**22 of them**), a relief band (**6**, ordered `abyss, shelf, lowland, upland,
highland, alpine`), and a water kind (**4**: `ocean, salt-basin, river,
dry-land`). It also carries that cell's temperature and moisture as numbers.

Twenty-two is past what a categorical palette can hold — eight or so hues are
distinguishable, and a sixteen-colour terminal cannot hold twenty-two at all. So
do not design twenty-two swatches. The biomes are not arbitrary categories:

- **Twelve are terrestrial** and lie on a temperature × moisture classification —
  ice, tundra, taiga, temperate grassland, shrubland, temperate forest,
  temperate rainforest, desert, savanna, tropical seasonal forest, tropical
  rainforest, alpine. Since temperature and moisture are *emitted per cell*, land
  colour should be a **two-dimensional surface over those two axes**, so that
  biomes which are neighbours in the world are neighbours in the palette. That is
  derived rather than authored, and it degrades gracefully when the palette
  shrinks.
- **Ten are marine**, and most are a depth ladder — epipelagic, mesopelagic,
  bathypelagic, abyssal, hadal trench — which wants a **sequential ramp**, not
  hues. The rest (coral reef, kelp forest, hydrothermal vent, upwelling, sea ice)
  are genuine specials and may have their own marks.
- **Relief is ordinal**, so it reads as elevation shading rather than as hue.

**And here is the forced consequence.** In monochrome — which is the floor this
design must meet — colour is gone, so **the glyph is the only channel left that
can carry biome.** Today the client draws two: `~` for water and one for land,
with the source commenting "coarse on purpose." Two glyphs for twenty-two
biomes is the largest unused capacity in the whole interface.

So the character vocabulary is a first-class deliverable, not a fallback: design
a **biome glyph set** that reads at a glance, survives without colour, and has an
obvious sprite counterpart under the cell law. This is where a roguelike's
craft actually lives.

One caution about the lens caption: **there are exactly two lenses on this
surface** (`terrain` and `colour`). Design the caption and a two-position
control, not a lens gallery.

**Note what the mark's rank is not.** Marks carry a *rank* ("lower is more
salient"), not a magnitude. Rank three of four and rank three of forty are
different situations, so **a rank cannot be turned into a weight**. Marks may be
*ordered* by it — which one is drawn on top, which one the legend names first —
and must not be dimmed or emboldened by it.

### What does not exist — do not design it

No hit points. No mana. No stamina, hunger, or fatigue **for the player**. No
inventory. No armour class, no experience, no level, no gold. No quest log. No
minimap claiming full truth. No timer finer than a day.

**And no standing, disposition, or attitude surface** — no reputation bar, no
"they distrust you," no per-creature relationship readout. The simulation does
track how each creature feels toward you, but that channel is *world truth*: it
knows about creatures your character has never met. Filtering it down to what
you actually know would need a join that does not exist yet — creatures are not
in the knowledge ledger. A pane rendering it today would silently be showing you
the world's mind rather than your own, which is the exact failure this whole
brief is built to prevent. Leave it out.

The endpaper is therefore **an identity strip, not a vitals bar**. And note the
asymmetry, which is real and which I would like the design to feel rather than
apologise for: you can read what *other* creatures feel, and you cannot read
what *you* feel. The journal has no page about its own author.

## Components to deliver

**Foundations**
1. **Type** — a text face for the written entry, a monospace for the grid and
   the command line. Full scale, with the relationship between the two faces
   worked out (they share a page).
2. **Palette** — paper and ink, in daylight and lamplight. Colour carries
   **substance**: what a thing is — biome, water, relief. Never danger, never
   certainty. State the channel assignment explicitly in the component: glyph
   carries identity, the grid carries position, colour carries substance, weight
   carries attention.
3. **Ink weights** — the three epistemic states and the attention scale (see
   *What weight means*), plus the reserved correction vocabulary, shown together
   as a specimen sheet. This is the foundation the whole system rests on; give it
   the most care.
4. **Grid metrics** — the cell box, and the proof that a sprite and a character
   occupy the same one.
5. **The glyph vocabulary** — a character per biome (22), the relief and water
   treatments layered on it, the wall/floor/threshold set, and the `@`. Must read
   in monochrome, since without colour this is the only channel carrying what a
   place *is*. Show the sprite counterpart for at least a representative slice.
6. **Rules, margins, the gutter** — the anatomy of the spread.

**Components**
7. **The Spread** — the frame: plate left, entry right, endpaper below.
8. **The Chart Plate** — the outdoor map. A triangular lattice, biome and relief
   legend, epistemic ink weights, the `@` mark, and the lens caption line
   (`[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]`) —
   which is *load-bearing honesty*, not chrome: the client has no true view of
   the world, only named lenses, and the caption says which one you are wearing.
9. **The Plan Plate** — the indoor floor plan, with creature marks and legend.
10. **The Study Plate** — a detailed study of **a place**, drawn with called-out
    labels. This is the one subject with enough behind it to be lavish: a place
    carries its latitude and longitude, its refinement depth, its biome, its
    annual-mean temperature, moisture, elevation and signed height above sea
    level, its water kind, a strangeness descriptor and the noun inside it, a
    strangeness magnitude, its exits, and the **three lattice corner cells with
    their integer blend weights** — a locale is literally a weighted blend of
    three points on the globe, and the plate is where that can be shown.
11. **The Caption** — what an *object* or a *creature* gets instead. All either
    carries is a name and one sentence (what `examine` prints), plus, for a
    creature you are standing beside, what it feels. Make the caption small and
    beautiful. **Do not** build an object plate: a lavish frame around one
    sentence is exactly the failure this brief exists to prevent.
12. **The Leaf** — long scrollable text: a book read inside the world. Running
    head, folio, drop cap, and a scroll model that feels like turning pages.
13. **The Index** — the menu surface: tabbed sections, ribbon markers, keyed
    rows.
14. **The Entry** — the prose column: the passage, examinable nouns marked so a
    reader can tell what is addressable, the ways-on line, the command line.
15. **The Endpaper** — the identity strip.
16. **The Margin** — marginalia: a `why` explanation, an annotation on the plate,
    a noun the reader has touched answering itself. (Not standing toward a
    creature — see the forbidden list.)
17. **States** — unmarked paper (empty/unknown), a refusal ("No way n from
    here."), and genesis (the world deriving from its seed, which takes a few
    seconds and is worth making beautiful).

## Reference points

Aim near: a field naturalist's notebook; Admiralty and portolan charts; Haeckel
and Blaschka plates; Ultima Ratio Regum's item views; Skyrim's in-world books;
Dwarf Fortress's dense keyed menus; Cogmind's craftsmanship with a terminal grid.

Aim away from: fantasy-RPG parchment with rope borders and torn edges; wood
panelling; anything that reads as a video-game frame rather than as an object.
The journal is a working document that someone is *using*, not a treasure.

## Acceptance tests

Show me, explicitly, that the system passes each:

1. The complete spread rendered in monochrome characters at 80×24, still usable.
2. The same spread in tiles, with **no element in a different position**.
3. A trace listing: every visible datum on the composed screen, and which
   channel from the inventory it came from.
4. The unknown region reading as paper rather than as void.
5. Light and lamplight variants, both meeting contrast targets.
6. No ornament overlapping any glyph cell — call this out where it was tempting.
7. The reserved correction marks present in the specimen sheet and absent from
   every composed screen.
8. **The weight scale legible with colour switched off entirely.** If the three
   levels are only distinguishable when colour is present, the channel
   assignment has failed.
9. The noun-join demonstrated: a noun touched in the entry, and its mark
   answering on the plate.
