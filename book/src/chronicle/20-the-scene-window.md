# Campaign 20: The Scene Window

**July 2026 · outcome: complete, merged — the workspace's first render meant
for something other than the workspace to look at**

## What was attempted

Every picture Hornvale has drawn so far has been a picture the workspace
itself finished: an elevation raster baked to PNG, a biome map colored and
compressed, a planisphere pair of dots and digits, a strip of phonology
audio. Each is a committed, deterministic file, and each is also a dead
end — a reader can look at it, but nothing downstream can ask it a question.
A future viewer that wants to pan across a biome map, or a future game that
wants to know what a traveler standing on a given tile can see, cannot parse
pixels back into elevation and plate and unrest. It needs the numbers
themselves, not a picture of the numbers.

This campaign opens that seam. Rather than drawing one more finished image,
it emits a *scene* — a document that says what a stretch of the world's
surface contains, in the world's own units, and leaves every choice about
color, projection, or style to whatever reads it next. The posture is
deliberate and now settled for good: the simulation emits deterministic
data, and clients render pixels (decision 0022). Nothing in the workspace
gets a paintbrush from here on; anything that wants one lives outside it,
reading committed bytes or a running process's stdout. This first scene
covers only the *cartographic* half of that idea — a fixed lattice over a
world's surface, no observer, no clock, no living creature's eye. A second
half, a scene centered on someone standing somewhere at some hour, is left
for the seam to grow into later.

## What landed

**One index, one home.** Two unrelated corners of the workspace — the
elevation-map renderer and the biome-map renderer — had each, independently,
solved the same small problem: given a latitude and a longitude, which of a
world's tens of thousands of geodesic cells is nearest? Both had arrived at
the same answer, a world sorted into thirty bands of latitude so a lookup
only has to search a neighboring band or two instead of the whole sphere,
and both had written it out by hand, privately, inside their own render
code. A third consumer needing the identical lookup was the moment to stop
duplicating it: the index moved into the kernel, verbatim, not rewritten,
and both existing renderers now call the one copy. Nothing about either
picture was allowed to move in the process — the elevation and biome
rasters already sitting in the book's gallery were regenerated after the
move and checked byte-for-byte against the ones already committed, and they
matched exactly. A shared piece of geometry now has exactly one definition,
and the world's oldest pictures are the proof that moving it changed
nothing.

**A lattice, five layers deep.** The new scene resamples a world's surface
onto an evenly spaced grid of tiles and, at every tile, records five things:
elevation in meters, whether the tile is ocean, which biome it falls in,
which tectonic plate underlies it, and how much tectonic unrest sits there.
Each layer is its own flat array, one entry per tile, all five walking the
same grid in the same order — a reader who only wants elevation never has
to touch biome or plate at all. Biome is stored as a small number rather
than a name, indexing into a legend of the full biome catalog carried once
in the document; the legend's order is fixed by the catalog's
own declared order and only ever grows at the end, so an index means the
same biome in every scene that will ever be produced, forever. On top of
the lattice sit the world's settlements as named points — latitude,
longitude, a name, and a kind that is `"settlement"` for all of them but
one: the world's capital carries `"flagship"` instead, appears exactly
once, and never doubles as a plain settlement in the same list. The whole
document opens like this (arrays elided):

```json
{
  "schema": "scene/tiles/v1",
  "seed": 42,
  "width": 256,
  "height": 128,
  "sea_level_m": 409.0054089516639,
  "elevation_m": [ … ],
  "ocean": [ … ],
  "biome": [ … ],
  "biome_legend": [ … ],
  "plate": [ … ],
  "unrest": [ … ],
  "features": [ … ]
}
```

**A contract, pinned to the byte.** The same seed and the same width must
produce the same document forever, so the scene is held to the standard the
rest of the world's on-disk formats already answer to. A small fixed world's
scene is checked, in the test suite, against an exact copy of its own bytes
committed alongside the test — any change to the document's shape, however
small, is caught the moment it happens, not discovered later by a client
quietly reading the wrong thing. That fixture draws a bright line between
two kinds of change: adding a field, or appending a new entry to the biome
legend, costs nothing and stays the same schema, because an old reader that
looks fields up by name never notices the addition. Changing what an
existing field means, or its order, or its type, is a different document
entirely — a new schema minted alongside the old one, which keeps emitting
exactly what it always emitted. The one committed fixture that pins all of
this is small on purpose, eight tiles by sixteen; the campaign's other
committed example, a full 256-tile scene for the world at seed 42, is not a
test fixture at all but the future's practice data — regenerated by the
same build that regenerates every other committed picture, and checked for
drift on every one of them.

**A door, opened from the command line.** The whole thing is reachable as
one command, printing a scene to standard output for whatever wants to read
it next — a page in this very book, a viewer running entirely outside the
workspace, a script probing one seed's geography. A width the caller does
not like is refused with the bound it broke rather than silently clamped,
and an unrecognized kind of scene is refused by naming the kinds that do
exist. The book's reference section now carries the document's contract in
full — every field, the grid's exact convention, and the rule for what
counts as a safe addition versus a new version — so a reader outside the
workspace never has to go spelunking in source to know what a scene
promises.

## What was learned

- **A duplicated piece of geometry is a debt that compounds quietly.** The
  nearest-cell lookup cost nothing to duplicate the first time, because the
  second copy was a few lines shorter than the trouble of sharing the
  first. By the third need, sharing was obviously cheaper than a third
  copy — but the earlier the shared home is built, the fewer places there
  are to prove unmoved when it finally happens.
- **A legend is worth more than the name it saves.** Storing a small number
  per tile instead of a repeated string costs a document nothing readers
  care about and buys two things at once: a smaller file, and a promise
  that the number means the same thing in every document that will ever
  carry it, because the legend it indexes only ever grows.
- **A contract needs a place to point when it breaks.** A rule written down
  in prose — "this field's meaning never changes in place" — is only as
  real as what enforces it. Pinning a small scene's exact bytes gives the
  rule a tripwire: nobody can loosen the contract by accident, because the
  first accidental loosening fails a test before it fails a reader.

## Deferred, deliberately

No observer and no clock: this scene is the whole fixed lattice seen from
nowhere in particular, not any one traveler's view at any one hour — that is
a second, harder half of the same idea, left whole for its own campaign.
No new layers beyond the five shipped here; moisture and temperature are
both real quantities the world already tracks, and each earns its own place
in a scene only when something asks for it by name, not as a quiet
extension of this one. No compression and no bespoke encoding — a scene is plain,
readable JSON, and if size ever becomes a real problem the answer is to
compress the bytes at rest, never to invent a private format inside the
schema. And no viewer: nothing in this campaign draws the lattice it
emits, on purpose — that is left for something living outside the
workspace entirely, reading exactly the document this campaign now
promises never to change out from under it.

## Artifacts

[Scene Schema: tiles v1](../reference/scene-tiles-v1.md) — the document's
full field-by-field contract, the grid convention, and the stability rules
that govern what counts as an addition versus a new version. The seed-42
example scene, regenerated and drift-checked on every build alongside the
book's other committed pictures. `hornvale scene tiles` — the command that
prints a scene to standard output, refusing loudly on a width or a kind it
does not recognize.
