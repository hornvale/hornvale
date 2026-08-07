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
  map of the country, a floor plan of a building, a study of an object or
  creature, an index. This is the large pane and it holds the eye.
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

**Reserved, not yet used.** Design a full vocabulary for *correction* as well —
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
- **The place** — its biome, its water, its relief band, a fine-grained
  descriptor, temperature, moisture, elevation, height above sea level, and the
  ways on.
- **Who is present** — each creature's label, and **what it feels**.
- **Standing** — per creature, an accumulated grievance and whether it has
  crossed into hostility. *This channel is world truth and is not filtered by
  what your character knows.* Any component rendering it must filter it against
  the knowledge ledger first, or it is a cheat.
- **The knowledge ledger** — every fact the character has come to know, as
  key/value pairs.
- **The prose**, and a catalogue of the nouns in it that can be examined, each
  with the text `examine` would print.
- **The map** — cells, with the epistemic state above, plus a legend.

### What does not exist — do not design it

No hit points. No mana. No stamina, hunger, or fatigue **for the player**. No
inventory. No armour class, no experience, no level, no gold. No quest log. No
minimap claiming full truth. No timer finer than a day.

The endpaper is therefore **an identity strip, not a vitals bar**. And note the
asymmetry, which is real and which I would like the design to feel rather than
apologise for: you can read what *other* creatures feel, and you cannot read
what *you* feel. The journal has no page about its own author.

## Components to deliver

**Foundations**
1. **Type** — a text face for the written entry, a monospace for the grid and
   the command line. Full scale, with the relationship between the two faces
   worked out (they share a page).
2. **Palette** — paper and ink, in daylight and lamplight. The organizing
   principle should be **certainty, not danger**: ink weight and colour encode
   how sure the character is of a thing, not how dangerous it is. State this
   explicitly in the palette component.
3. **Ink weights** — the three epistemic states, plus the reserved correction
   vocabulary, shown together as a specimen sheet.
4. **Grid metrics** — the cell box, and the proof that a sprite and a character
   occupy the same one.
5. **Rules, margins, the gutter** — the anatomy of the spread.

**Components**
6. **The Spread** — the frame: plate left, entry right, endpaper below.
7. **The Chart Plate** — the outdoor map. A triangular lattice, biome and relief
   legend, epistemic ink weights, the `@` mark, and the lens caption line
   (`[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]`) —
   which is *load-bearing honesty*, not chrome: the client has no true view of
   the world, only named lenses, and the caption says which one you are wearing.
8. **The Plan Plate** — the indoor floor plan, with creature marks and legend.
9. **The Study Plate** — a detailed view of one object or creature, drawn with
   called-out labels. This is where the design gets to be lavish.
10. **The Leaf** — long scrollable text: a book read inside the world. Running
    head, folio, drop cap, and a scroll model that feels like turning pages.
11. **The Index** — the menu surface: tabbed sections, ribbon markers, keyed
    rows.
12. **The Entry** — the prose column: the passage, examinable nouns marked so a
    reader can tell what is addressable, the ways-on line, the command line.
13. **The Endpaper** — the identity strip.
14. **The Margin** — marginalia: a `why` explanation, a note of standing toward
    a creature, an annotation on the plate.
15. **States** — unmarked paper (empty/unknown), a refusal ("No way n from
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
