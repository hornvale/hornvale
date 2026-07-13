# The Locale Window

**July 2026 · outcome: merged — the room mesh gets its first consumer: a
window that turns a bare `RoomAddr` into a described place, and settles how a
room inherits the world beneath it**

## What was attempted

The Room Mesh laid a floor: an addressable, lazily generated finer mesh below
the canonical globe, with coarse-field inheritance hooks that no code yet
called. This campaign builds the first thing to stand on that floor —
`windows/locale` (`hornvale-locale`), which answers one question
deterministically: given a world and a `RoomAddr`, what is it like to stand
there? The answer is a plain, serializable value, the `locale/room/v1`
schema — inherited biome, blended continuous fields, seed-derived texture,
and exits — plus a `hornvale locale` CLI "look" that renders it as prose or
JSON.

The window constructs no providers of its own. It reads the coarse world
through `worldgen` exactly as the Almanac window does — `climate_of`,
`terrain_of` — and composes room-scale detail on top through the one
expensive object the campaign introduces: `LocaleContext`, the coarse world's
geosphere, climate, and terrain, built once and reused across every
`describe` call. A room's locale is a genuinely cheap derived view; the
context is what makes it cheap.

## The inheritance question, settled

The Room Mesh gave a room integer barycentric weights toward the three
canonical-grid corner cells of its coarse ancestor triangle — a room's
`corner_weights`, numerators over `D = 3 · 2^(depth − globe_level)` summing to
`D`. What a room does with those weights was left open, and it splits cleanly
along a line the Crust campaign already drew: some quantities are *pointwise*
fields, resample-anywhere; others are *mesh-bound* truth, computed once and
never re-derived at a different resolution
(decision `identity-computes-on-the-canonical-grid`).

**Continuous fields — temperature, moisture, elevation — blend.** These are
resolution-free by the constitution's own terms, so a room may sample them as
finely as it likes: `field = Σ (weight_i · value_i) / D`, the integer-weighted
average of the three corner cells. This is refinement in the strict sense —
more detail, same truth, always consistent with the coarse value it derives
from.

**Biome does not blend — it inherits, at maximum weight.** Biome is a
*category*, and blending a category is not refinement, it is re-quantization —
computing a new classification at a resolution the canonical grid never
authorized. Decision `identity-computes-on-the-canonical-grid` reserves that
move for an epoch, not a tier, so a room's biome is simply the biome of
whichever corner cell carries the largest weight (ties broken by lowest
`CellId`, deterministic and platform-exact, since the weights themselves are
pure integers). This is the campaign's central technical resolution — the
open Q4 the room-scale design's §14 left for this window to close, and the
room-scale corpus's standing principle of "refine fields, inherit mesh-bound
truth unchanged" made concrete for the first time over real data.

The honest cost of that choice is visible immediately: max-weight inheritance
makes biome a step function of position inside a triangle, so two adjacent
rooms can show a hard biome edge mid-triangle where the dominant corner
flips. This is not a bug to hide — the canonical grid itself has hard cell
boundaries, and pretending otherwise would be the re-quantization the
decision forbids. v1 is deliberately the **low-variety inheritance spine**:
correct, honest, and close to uniform by design. Softening those seams into
ecotones is a later campaign's job (the room-scale corpus's palimpsest and
field-refinement work); true variety and strangeness are the combinatorial
biome bestiary and beyond. What this campaign shipped is the spine everything
else will ride, not the furniture on top of it.

## A change of scale, not a change of floor

A room's exits split into two kinds. The three lateral exits are the room
mesh's own geometric edges, each named by compass bearing off
`addr.bearing_to(&neighbor)`. The vertical exits are `Enter` (descend a
digit, `child(0..4)`) and `Exit` (step back out, `parent()`) — deliberately
not `Up`/`Down`. A room's parent and children are the *same room, seen at a
different level of detail*, not a different place in a stack; `Down` was
reserved because the future Underdark will need it for something that is
actually a different place — a stratum beneath the surface, not a finer view
of the surface. Naming the zoom verb `Enter`/`Exit` now keeps that semantic
space free for when subterranean rooms land.

## The one kernel addition

`LocaleContext::describe` takes a `RoomAddr`, but the CLI's friendly path
takes a coordinate — `--at LAT,LON`. The substrate had no public
coordinate-to-room locator, so the campaign added exactly one:
`RoomAddr::containing(geo, position, depth)`, living beside `coord` and
`corners` in `kernel/src/room.rs` as another float presentation helper. It
introduces no new identity or save-format surface; a boundary-straddling
coordinate may resolve to adjacent rooms on different platforms (the ordinary
float-presentation caveat), but once a room is addressed its content is
integer-exact, as it always was.

## What v1 deliberately is not

`Locale` is a *derived view* in the same discipline as the game layer's
planned readouts: cheap, re-derivable from the seed,
never stored — the ground truth a designer or the sim reads. It is
explicitly not the belief or perceived view a later projection layer will
build; that layer reduces this one, it does not replace it. Everything else
named in the room-scale synthesis and left out of scope stays out on purpose,
with a home already picked: the P1 field-combinator algebra, the P3
weighted-cross-product descriptor grammar (this campaign's `SubCellTexture`
is a deliberately tiny stand-in — a handful of biome-keyed phrases and a
bounded jitter, not content authoring), the `ecology`/`society` domains, the
palimpsest time axis, overlay exits and passability, and any "walk"
(movement) REPL. `describe` already threads `WorldTime`, unused for now
beyond an annual mean, so the coming P8 temporal-phase layer is an
in-version refinement rather than a signature change.

## The road ahead

The room mesh now has a real consumer, and the inheritance question the
substrate's hooks were built for has an answer, exercised end to end and
pinned by tests rather than left as an untested hook. The next layer up is
variety: the combinatorial biome bestiary fills the pool this campaign's
`SubCellTexture` only sketches, the palimpsest gives a room a history to read
back, and the smoothing of hard biome edges into ecotones is deferred,
honestly, to whichever of those campaigns picks it up.
The Locale window is also The Walk's Local-space interface — the game
layer's first walkable surface — though "walkable" still means "look," not
yet "walk": the movement REPL rides this crate's `LocaleContext` when it
lands, unchanged, because the context was built reusable from the start.
