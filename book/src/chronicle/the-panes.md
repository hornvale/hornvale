# The Panes

The question the campaign opened with was *how close are we to a working
combination roguelike and text adventure two-pane view?* The honest answer was
**closer than the absence of any pane suggested**, because both pictures
already rendered. Ask the shipped binary for `map` out of doors and it draws a
hexagonal chart of the surrounding terrain; ask for it inside a building and it
draws a 1:1 floor plan of the chamber you stand in. Neither was missing. Both
were simply serialized into the transcript on demand, one turn at a time, and
then scrolled away.

So the campaign was never "build a map." It was **stop making the map a verb**
— carry the spatial state in the per-turn snapshot and let a client hold it
open beside the prose.

## The map ships uninhabited, and says so

State this first, because it is the campaign's largest deliberate omission and
softening it would be dishonest.

The map draws walls, doorways, terrain, water and `@`. It draws **no creatures
and no field of view**. `windows/vessel/src/lattice/occupancy.rs` opens with
the sentence *"No creature stands in a cell until The Sighting,"* and that
sentence is exactly as true after this campaign as before it. Nothing in the
spatial channel carries an occupant; nothing in either pane can hide a cell
you have not seen.

That ordering was chosen at the opening fork with the cost in view: the seam
is worth proving before the picture is worth staring at. A channel that ships
occupants before it ships a stable shape has to move both at once, and a pane
nobody can mount is not a pane. The empty room is the price of getting the
window in the wall first.

## One channel, a tagged union over the band

`vessel/session/v1` gains one channel, `spatial`, last in key order because
key order is contract and it is the newest arrival. The addition is
**additive**: the schema's own text says additive changes are free and only a
meaning change mints a `v2`, and no existing field's meaning moved. The
shipped client, which refuses any tag that is not `vessel/session/v1`, keeps
working unchanged.

The channel carries **semantic content, never a picture**. The sim emits what
an observer can see, never how to draw it; clients render pixels. The prose
exception does not generalise — prose ships verbatim because prose is the
constitutional primary, and a map is not prose. So the wire tag is `band`,
with values `walk` and `chamber`, and each arm carries cells.

**Out of doors, the channel embeds `scene/surrounds/v1` verbatim.** Nothing
new was designed for the walk band at all. `windows/scene` already defines a
schema-tagged, quantized, structural surrounds scene, and the session already
returns one as a pure read. One schema, one owner — the same move the room
channel already makes with `locale/room/v2`. This was the cheapest half of the
campaign by a wide margin, and the more expensive one at runtime, which is the
inversion the next section is about.

## `vessel/plan/v1`: a palette and a dense index grid

The chamber band had no serializable form at all. `Lattice` derives `Clone`,
`Debug`, `PartialEq` and `Eq` and pointedly not `Serialize`. This is the
campaign's one genuinely new schema surface, and its shape was corrected once,
at the design review, in a way worth recording because the correction
generalises past the objection that produced it.

The first draft encoded each cell as a one-character code in a dense string.
The objection was: *can a string carry colour?* It cannot, and the objection
does not stop at colour. A per-cell string carries exactly one attribute, so
each further attribute becomes another array that must stay length-synced with
the grid — N chances to desync and the totality invariant to re-check N times.
Worse, the attributes actually coming are not one character wide: a colour
triple, an occupant's identifier, a temperature.

The repository had already solved this and the draft had walked past the
solution. `scene/surrounds/v1` carries append-only legend tables with a `u32`
index per cell, an optional colour triple skipped when absent so that *"an
uncoloured document is byte-for-byte what it was before the colour layer
existed,"* and a per-cell marks list for instances. `vessel/plan/v1` mirrors
all three:

```json
{ "schema": "vessel/plan/v1",
  "chamber": 193703028372802,
  "at": 0, "of": 2,
  "extent": { "x": 0, "y": 0, "w": 19, "h": 10 },
  "palette": [ { "kind": "wall",      "chambers": [] },
               { "kind": "floor",     "chambers": [0] },
               { "kind": "threshold", "chambers": [0, 1] } ],
  "cells": [0, 0, 0, 1, 1, "…"],
  "you": { "x": 6, "y": 5 } }
```

Four properties the shape is chosen for, three of which were free and one of
which had to be argued.

**Semantics, not glyphs.** `"wall"`, `"floor"` and `"threshold"` are the
`CellKind` discriminants. The client picks `#`, `.` and `+`. That is what
leaves an atmospheric or deliberately-unreliable map reachable as a pure
client change rather than a schema migration.

**Total, like its source.** `Lattice::cells` is documented total over its
extent precisely so that "absent" cannot mean two things. The projection
preserves it: `cells.len()` is exactly `w × h`, checked rather than asserted
in prose, and a missing cell panics rather than defaulting — an
`unwrap_or(Wall)` would draw a hole as solid rock and hide the upstream bug.

**The doorway keeps both its answers.** A threshold's palette entry carries
`chambers: [a, b]`. `CellKind::serves` is a predicate exactly because "whose is
this doorway" has two right answers, and the retired owner-map could hold only
one. The palette does not reintroduce that, and it needs no separate doorway
list duplicating what the cells already say — the discipline the snapshot
already keeps for exits, where two representations of one truth would drift.

**Types in the palette, individuals elsewhere.** A palette entry is a cell
*type*; an occupant is an individual. Type-level attributes (all walls are
grey) may join an entry; a thing standing on a cell may not. This is what
keeps the palette from quietly becoming the tile catalogue `CellKind`'s own
doc forbids. The campaign shipped **no** marks field rather than an empty one,
on the reasoning that a field nothing writes cannot be seen to be wrong, and
because additive-is-free means the slot costs nothing to add beside its first
writer.

**The payload is bounded by arithmetic, not by estimate.** A structure holds
at most four chambers of side eight, and the extent is
`cols × 8 + (cols + 1)` per axis with `(cols, rows)` at most `(2, 2)`. The
largest plan any structure can produce is therefore **19 × 19 = 361 cells**,
and the palette is bounded by chambers × kinds — at most a dozen entries
today. Seed 42's opening chamber is 19 × 10 with a four-entry palette. It
stays small as attributes accrete, because attributes land on the dozen
palette entries and never on the 361 cells.

## A cell in a snapshot is not a saved position

The projection derives `Serialize`; `Lattice` and `Cell` do not. This matters
because `Cell`'s own doc reads *"never serialized, never a fact's object,"*
and that stays literally true of the type.

The rule it names holds that an entity's **persisted** position is its room,
and no **saved** state may point into the fine layer — with the stated
consequence that nothing stored points there, *"so it may regenerate
differently forever without corrupting a world."* A session snapshot is
derived fresh each turn from a lattice itself re-derived from the seed,
discarded on the next turn, never a fact's object, never read back into the
sim. It has exactly the property the rule wanted.

The constraint that falls out binds a later campaign rather than this one, and
is written into the schema's own module doc where a replay campaign will
actually read it: **`vessel/plan/v1` is emit-only and must never be
persisted.** The moment a snapshot carrying cells is written to disk — a
replay file, a morgue file — that artifact points into the fine layer, and the
rule is broken by the *saving*, not by the emitting. The answer a replay wants
already exists: save a seed and the verb sequence and replay it to regenerate
snapshots, never a recording of them.

## What it costs, measured three times

The design document declined to price the time, and said why rather than
hedging: the CLI harness could not see it, because world load dominates at
350–600 ms per invocation and radius 0 through 8 showed no monotonic trend
against that noise. The scene renderer's own doc reports "~2 ms of this
function's own per-cell work" at an unstated radius, and quoting that against
the 4.75 ms no-op turn floor would have been precisely the extrapolation the
project's own clock inventory forbids in as many words: *re-measure, do not
extrapolate.*

So the plan built the benchmark before it built the pane, and the reading is a
**matched pair** — the same fixed ten-verb sequence, the same box, the same
release profile, before and after the channel exists.

```
                        before        after
snapshot() + json      0.173 ms     1.249 ms      7.22x
walk snapshot bytes    4,235 B     11,582 B     +7,347
chamber snapshot bytes 4,064 B      4,759 B       +695
```

Three independent measurements converge on the walk band's cost. The design
document computed ~7 KB from a standalone scene emit; the fixture step measured
the committed golden growing 16,345 → 45,733 bytes and derived **7,348 bytes**
of chart per turn from the growth; the matched-pair benchmark measured
**+7,347**. Agreement to one byte, from three instruments that share no code.
The chamber band's computed ~700 B landed at +695.

The asymmetry is the opposite of what the campaign's framing suggested: **the
new schema is nearly free and the reused one is what costs.** The chamber plan
adds about 17% to a snapshot; the walk chart roughly triples it.

The per-class split is the more interesting reading. Verb handling varies by
almost two orders of magnitude across classes — 11.6 ms for a verb that moves
the possession, 7.7 ms for one that advances the day, 0.19 ms for one that does
neither — while `snapshot() + json` is **flat** at 1.20, 1.25 and 1.31 ms. The
snapshot's cost is a function of what there is to describe, not of what the
verb did.

## The memoization was specified, priced, and not built

The design document held a mitigation in reserve — memoize scene construction
on room, day and zoom, so the walk chart would be rebuilt only on the minority
of turns that move the possession or the clock — and it was explicit that the
mitigation applied *"if the benchmark says one is needed."*

The benchmark said it was not. 1.249 ms native, against the repository's
measured 3.6–3.8× native-to-wasm ratio, is roughly **4.6 ms of snapshot per
turn** in the browser. A human pressing a key does not perceive five
milliseconds, and the 4.75 ms figure it would double was never a budget — it
was a measurement. Building a memo against an imperceptible cost is the
premature optimization that measure-first discipline exists to prevent, so the
campaign shipped without it and recorded the arithmetic instead of the code.

Note what the memo would *not* have done in any case: the bytes still ship
every turn. Emitting "spatial unchanged since last turn" would break the
invariant that every pane is a pure function of **one** snapshot. A snapshot
that only makes sense beside its predecessor is not self-contained, and the
whole redaction discipline rests on that self-containment. Construction is an
implementation detail and may be cached; the emitted bytes are the contract
and stay whole.

## A clock that was owed since The Action Clock

The turn clock's caveat said the 4.75 ms no-op floor would rise by an
unmeasured amount once per-tick behaviour existed. That trigger fired at The
Action Clock and the re-measurement has been outstanding since, wanting a
session-level benchmark nobody had built.

This campaign built it. Verb handling costs **1.071 ms native** on the
pre-channel baseline; multiplied through the same 3.6–3.8× ratio it is about
**4.0 ms** of wasm turn. That does not contradict the 4.75 ms floor — it
**corroborates** it from an independent instrument built four campaigns later
on a session that now runs an action queue. The debt is paid, and the answer
is that the floor did not move materially.

## One pane, switching with the band

Not two spatial panes side by side, because the sim forbids it. The session
answers `map out` indoors with a refusal, so the walk-band chart is **not
derivable** while the possession is inside a building. Two simultaneous
spatial panes require lifting that refusal, which is a change to the sim and
belongs to whichever campaign wants the two-scale layout. It is registered
rather than smuggled in.

The rejected middle option is worth naming. Keeping a *frozen last-seen* chart
on screen while indoors would present a stale picture as a live one — the
cheat-pane failure mode the snapshot's own documentation warns about at
length. A pane that cannot be honest goes away instead, and the map pane
empties on entering a building rather than lying about the country outside.

The `map` verb survives untouched. It is the CLI's only map, and it is the
subject of a live parity test that walks what the render *claims* to depict
and demands `examine` accept each item — the check that caught The Lintel's
look-named-but-`examine`-denied jar. Deleting the verb would delete the test's
subject.

## The client draws it

Two pure modules, snapshot in and glyph rows out, unit-tested with no DOM, in
the discipline the transcript and snapshot readers already held.

The chamber reader indexes twice per cell — once into the grid, once into the
palette — and switches on the kind. It validates before it draws: the grid
length must equal `w × h`, every index must be a legal palette subscript, and
every palette entry itself must be a non-null object, and it returns **no
pane** rather than a short or clamped one when any of the three fails — the
last of the three closed a defect where a malformed (`null`) entry threw
instead of refusing, found in the final whole-branch review. An unknown
*kind string* on an otherwise well-shaped entry renders as a documented
fallback glyph rather than throwing, matching the client's standing posture
that a reader which cannot understand the structure degrades to prose,
which always works.

The walk reader is not a port. The Rust surrounds renderer is 543 lines, and
nearly all of it is lens tables, colour-disclosure prose, legend text and marks
ranking — machinery the `map` verb still owns and a pane does not need. What
the pane reimplements is the four-line placement and a coarse three-way glyph
choice. Two renderings of one scene for different purposes is exactly what the
client-renders-pixels rule licenses; they are not expected to agree
glyph-for-glyph and no test asserts that they do.

The placement is the part where a wrong answer looks right:

```
row = -w
col = 2v + (up ? 0 : 1) + w
```

The `+ w` term cancels the lattice's row offset. Drop it and an up-triangle's
horizontal-edge neighbour lands down-and-to-the-right instead of directly
below, so a breadth-first ball of cells draws as a right-leaning parallelogram
instead of the symmetric hexagon it actually is — and the result is a
perfectly plausible-looking picture of a shape the world does not have. The
campaign anticipated this and carried a symmetry negative control for it. That
control was itself wrong in an instructive way, which the retrospective covers.

Both panes stay pure functions of one snapshot, so the redaction boundary
stays **structural**: the map pane reads the spatial channel and cannot see
outside it. A snapshot with no spatial channel renders no map pane and prose
still works.

## The diptych

`main.ts` grew a two-column layout and mounts the map beside the transcript.
Its header had anticipated this since The Casement — nothing at module level
holds session state, so *"a future page can mount two casements."* The prose
pane is the existing transcript, untouched, and the byte-identity smoke
confirms it: the wasm opening still matches the committed native transcript
byte for byte, so nothing about the spatial channel leaked into prose. That was
the check worth running, because a moved *transcript* would have meant the
change reached the wrong side of the window.

The `@` is shared deliberately between the two bands, and that was a decision
rather than an accident: a player should not have to learn two marks for one
thing. It is now a client-side choice, which is the entire point of shipping
cells rather than a picture.

## What this leaves

The remembered map turns out to be **band-asymmetric**, and nothing had
recorded that. The walk band already accumulates — the knowledge ledger holds a
room entry per visited locale, and `scene/surrounds/v1` has carried a
`"remembered"` cell state annotated *"written only by a session-owning
consumer"* since it was designed, with no writer ever. So a remembered
overworld minimap is nearly free from data that exists today. The chamber band
accumulates nothing: after entering a chamber, the session's own knowledge
report lists a room, a settlement name and a settlement population, and **no
chamber key of any shape**. A remembered floor plan is therefore not a pane
change at all — it is a new knowledge shape, which is a far larger and
epoch-adjacent commitment. That was verified by running the binary rather than
by reading the code, which is why it is stated as a fact rather than an
expectation.

And the indoor chart refusal is now the single identified blocker between this
campaign and the two-scale layout the metaplan actually wants. It is one
condition in one match arm, and lifting it is a sim change with its own honesty
question attached — what does the chart of the country outside mean while you
stand in a windowless room? That is a design question, not an implementation
one, which is why it was registered rather than answered here.
