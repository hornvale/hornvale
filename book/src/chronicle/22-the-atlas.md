# Campaign 22: The Atlas

**July 2026 · outcome: complete, merged — the book's first page a reader can
touch**

## What was attempted

The scene window opened a seam and then, on purpose, walked away from it: a
committed document that says what a stretch of the world's surface contains,
with every choice about color or projection left to whatever reads it next,
and nothing in the workspace drawing a single pixel of it. This campaign is
the first thing to walk through that seam from the outside. It builds a
page — the book's own, sitting in `gallery/` beside the static rasters that
came before it — where a reader does not just look at a picture of seed 42's
surface but moves across it: switches what the color means, hovers a tile to
read its numbers back, drags to pan, scrolls to zoom. Nothing about the
scene document changes to make this possible. The whole page is built out of
choices a client makes about bytes it was never trusted to alter.

## What landed

**A second toolchain, and the workspace never felt it.** The no-new-dependencies
rule has held since the kernel's first commit, and it still holds — for the
Rust workspace. What decision 0023 settles is that the rule was always about
the workspace, not about clients standing outside it: a Ring-3 client gets
its own dependency stack and its own toolchain, commits what it builds, and
answers to its own CI job rather than borrowing the Rust gate's. The atlas
lives under `clients/atlas/`, written in TypeScript against a single pinned
binary — Deno 2.9.2, native TypeScript, a built-in test runner and bundler,
nothing installed from a registry at build time. `cargo fmt` and `cargo
clippy` do not know this code exists; `deno fmt`, `deno lint`, and `deno
task test` do, and a new sibling CI job runs all three beside the untouched
Rust gate rather than inside it. Four small modules — reading the scene
document, the pan/zoom viewport math, the layer palettes, and hit-testing a
hover against the nearest tile — carry the tested logic; the page's own glue
code does nothing but wire canvas events to those modules and redraw.

**Five layers, one canvas.** A reader picks among biome, elevation, plate,
unrest, and ocean, and the canvas repaints from the same tile lattice under
whichever one is selected. Biome and elevation are not new colors invented
for the browser — they reproduce, tile for tile, the exact palette the
committed elevation and biome rasters already use, so the atlas's biome
layer and the book's own biome PNG agree on every pixel a reader can compare
them against. Plate and unrest had no prior picture to match: plate wears an
eight-color categorical set built for exactly this job, and unrest — a
quantity the book has tracked since the tectonic globe but never once
shown — gets an orange sequential ramp that, switched on, does something a
reader has not seen before: it lights up in bands that trace the plate
boundaries the lattice was resampled from, tectonic strain rendered visible
for the first time. Settlements sit on top of every layer as ringed dots,
one gold instead of the rest — the world's flagship, never doubling as a
plain settlement in the same list, exactly as the scene document already
promised.

**The same document, opened two ways.** Every fact the page renders — a
tile's elevation, its biome index, which plate it sits on — comes from
[`scene-tiles-seed-42.json`](../gallery/scene-tiles-seed-42.json), fetched by
the page itself, the identical bytes a reader could open directly and read
as plain JSON. Decision 0022 said the simulation emits data and clients
render; this is that sentence made visible rather than merely enforced.
Nothing about the page's pan, zoom, or color choices is privileged over what
a reader could reconstruct by hand from the raw document — the page is
simply a client that already did the work, running entirely in the reader's
own browser, holding no server-side state and asking nothing of the
workspace once the bytes leave it. A network hiccup that leaves the fetch
unanswered gets an honest fallback message rather than a blank canvas,
pointing straight at the same raw file.

**Proven to rebuild to the same bytes.** The page ships a single bundled
file, `atlas.js`, built once and committed rather than assembled by a reader's
browser from source. Two proofs sit behind that commit: rebuilding it from
the checked-out source reproduces it byte for byte, and building it from a
second, differently located copy of the same source produces the identical
bytes again — the bundle's contents depend on nothing about where or when it
was built, only on the source that went into it. CI's new job repeats the
first proof on every push, the same discipline the Rust gate has always
held for every other generated artifact in the book.

## What was learned

- **A protocol proves itself the day something outside the workspace reads
  it.** The scene document's contract — stable field names, an
  only-ever-grows legend, a fixed grid convention — was written and tested
  entirely from the producing side. This campaign is the first real reader,
  and nothing about the contract needed to bend to be read: the atlas
  parses the same document the drift check already pins, and the palette
  choices needed no cooperation from the schema at all.
- **The dependency wall was drawn in the right place the first time.**
  Standing up a second toolchain could have meant relaxing the workspace's
  own rules, or smuggling a client dependency into a shared `Cargo.toml`.
  Neither happened, because decision 0022 already drew the boundary at the
  workspace's edge rather than at "no dependencies anywhere" — decision
  0023 only had to formalize what that boundary already implied for a real
  client.
- **Thin glue is honestly testable, and honestly not tested.** Splitting
  the page into four pure, unit-tested modules and one file of DOM wiring
  meant the hard parts — parsing, projection, palettes, hit-testing — carry
  real test coverage, while the wiring that is left over is short enough
  that its correctness is checked by watching it draw, not by asserting
  against it. Neither half pretends to be the other.

## Deferred, deliberately

No sky, no clock: the atlas renders only the cartographic scene the prior
campaign shipped, seed 42's surface fixed in place, nothing about
`unrest` or any other layer moving with time. A scene centered on an
observer at an hour — the situated pole the scene schema already reserves
room for — has no page yet; scrubbing a sky across a year waits for that
scene kind to exist before any client can draw it. Nor is this the only
client the workspace will ever grow: a terminal viewer that drives
`hornvale scene` as a subprocess instead of fetching committed bytes, and,
further out, a game's own tile view reading a scene kind built for a
traveler's actual knowledge rather than the whole lattice at once — both
are shaped by what this campaign proves works: a small, pinned toolchain,
a handful of tested modules, and a canvas that never has to ask the
workspace for anything it wasn't already given.

## Artifacts

[The Atlas of Seed 42](../gallery/atlas.md) — the interactive page itself,
five switchable layers, hover readout, settlement markers, drag-pan and
wheel-zoom, reading `scene-tiles-seed-42.json` live in the browser.
`clients/atlas/` — the TypeScript client, its four tested modules and thin
DOM glue, verified by its own CI job beside the Rust gate.
