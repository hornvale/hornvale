# Retrospective — The Overworld

The fourth and last campaign of the Orrery view-remake program: rework the flat
pixel-art map into a 16-bit RPG overworld renderer (palette + dithering +
coastlines + outlines). Five tasks, subagent-driven, orrery-only. Process notes.

## What worked

- **Decompose the diffuse aesthetic before touching code.** "The pixel map isn't
  good" is unactionable. Tree-finding split it into seven components (base fill,
  texture, coastline, palette, resolution, outlines, symbols) and named the hidden
  hub — dithering does four jobs at once. That decomposition is what let a
  taste-shaped problem be planned as five concrete, testable tasks instead of an
  open-ended polish session.

- **Determinism forced the honest technique.** The obvious pixel-art move —
  random dithering/noise — is unconstitutional here. Naming that up front (the G3
  package led with it) meant the plan specified *ordered* Bayer dithering from the
  start, and every renderer test asserts identical output for identical input.
  Deterministic pixel-art is entirely achievable; it just has to be chosen
  deliberately, not discovered when a drift check reddens.

- **Each element unit-tested on known pixels, reviewed independently.** The
  renderer is a pure function, so a hand-built mini-region → asserted pixels
  (dither alternation, a coastline band, a biome-boundary line) tested each element
  in isolation. Reviews caught two real correctness issues by inspection: a T2
  test fragility (pixel assertions silently coupled to the very dither constants a
  later task would retune) and a T4 biome comparison by legend *index* instead of
  *name* (a false-positive boundary on a same-name-different-index world).

- **Separating mechanism from look let the campaign close honestly.** The renderer
  is correct and reviewed; the aesthetic is not yet good. Those are different
  claims, and the campaign shipped the first while explicitly deferring the second
  — no pretending a passing test suite means a beautiful map.

## What bit

- **A correct renderer is not a good-looking one, and only the eye knows.** Every
  element passed its tests and the composed map still read flat and pale on the
  first pass. The unit tests guard correctness; they say nothing about whether the
  palette pops or the dither reads. The lesson The Idioms and The Cartographer
  already taught, re-taught: for a render campaign the screenshot is the real
  gate, and "green" is not "good."

- **Resolution is an aesthetic parameter, not just a fidelity one.** 384² sounded
  like "more detail," but displayed across ~720px it made the pixels ~1.9px — too
  fine for the chunky 16-bit read the dithering needs. A pixel-art renderer's
  texture dimension is a *style* knob (chunky vs fine), not a quality dial where
  higher is always better. Flagged for the tuning follow-up.

- **The default showcase was the worst possible ground.** Seed 42 opens the map
  onto open ocean or flat desert — the least flattering subject for a landscape
  renderer, and finding a land-rich, biome-varied region through the globe→map
  navigation proved fiddly (the map region is the globe's center-facing point, and
  seed 42 reads ocean-heavy). A render campaign needs a curated showcase region
  chosen up front, not hunted for at the visual pass.

## Follow-ups
- **HEADLINE: the aesthetic tuning** (owner-deferred to a fresh session with full
  context) — punchier/more-saturated palette, chunkier `OVERWORLD_TEXTURE_DIM`
  (~160–200), bolder outline weight and coastline foam, a curated land-rich
  showcase region, possibly in-map zoom to see the pixels. All named `export
  const`s; pure const-tuning from here.
- Biome motif-stamping (mountains/forests/dunes as procedural texture) — the
  biggest lever left after the core.
- Animated water (Lorenz-safe positional shimmer); overworld river/lake restyling.
- Minor code cleanups triaged at the whole-branch review (a shared neighbor-scan
  helper to DRY the coast/biome loops; a branch-certain shallows test assertion).
