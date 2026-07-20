# Retrospective — The Idioms

A client-only campaign: a swappable render-style layer for the Orrery globe, four
screen-space "delight" skins over the same scene. Orrery-only, no sim/wasm change,
no release. Process lessons only.

## What worked

- **The screenshot visual pass was the real gate, over and over.** Every one of
  the four shaders passed vitest, tsc, and the build, and the first two rendered a
  black globe anyway — the failures lived on the GPU, where a headless test cannot
  look. Green never meant correct for a render change; only rendering the real
  globe through a real browser and *looking* caught it. This is the same lesson The
  Frame Budget closed on (a client-render campaign is not done until it is seen),
  now confirmed four more times in a row. For any shader/render work, budget the
  visual pass as the acceptance test, not a formality.

- **Baking each discovered bug into the NEXT dispatch as a guardrail.** Pixel-art
  cost a long isolation to find a cloned-uniform `setSize` and a composer-alpha
  issue; cel cost another to find that `flat` is a reserved GLSL word that fails
  the whole shader silently. Both lessons went verbatim into the engraving and
  watercolour dispatch prompts as mandatory checklists — and both of those landed
  **first-try**, rendering correctly on the first visual pass. The cost of a bug
  found is a cheap guardrail for every task after it; write the lesson into the
  prompt, not just the retro.

- **Controller-owned inline fixes for the visual loop.** The shader bugs were
  isolated and fixed by the controller directly, not dispatched — correctly, because
  the fix required a rapid render→look→adjust loop only the controller (with the
  screenshot tooling) can run; a subagent cannot see the result it is trying to
  fix. Subagent-driven development's "don't fix manually" rule yields here to the
  reality that a visual defect needs a visual fixer.

- **Deferring the clean styles' per-task reviews into the final whole-branch
  review.** Cel (controller-rewritten) and engraving/watercolour (clean first-try)
  had their per-task reviews folded into one final opus review, which came back
  clean. When the controller has already deeply engaged the code (isolating and
  rewriting), a separate per-task review is redundant with the final one.

## What was hard / notes

- **The black-render class of bug is invisible to headless tests and needs its own
  tripwire.** A shader that fails to compile (a reserved-word variable) or samples
  off-image (a stale resolution uniform) renders a fully black globe with no error
  a unit test can assert on. The mechanised guard is a smoke test that renders each
  style and asserts the frame is neither black (a compile-failed shader yields a
  tiny, highly-compressible PNG) nor identical to photoreal. Every render axis
  should carry one.

- **Shader-bisection discipline.** The way out of a black frame was mechanical:
  replace the shader body with a raw passthrough (is the frame even reaching me?),
  then a single isolated operation (which step blacks it?), then a solid colour
  (does the pass run at all?). Each step is one render and one look; guessing at
  the math wasted more time than the isolation did.

## Follow-ups

- **The "only-this-world" styles** — star-chart (settlements as constellations),
  illuminated mappa-mundi, orrery-schematic, ukiyo-e — the open axis's future
  entries, the ones only a real simulated world could justify. The pipeline accepts
  them; none shipped in v1.
- **The mark-owning cartographic renderer** (the "legibility" fork not taken): the
  self-drawing map with contours, labels and a legend — the visual twin of the
  self-writing Book. A future producer-touching campaign.
- **Deeper data-awareness** (the deferred G-buffer): per-pixel data-keying like
  watercolour pigment pooling at true biome boundaries, or engraving flourishes at
  real ocean gyres. v1 kept to colour + one cheap CPU hook per style.
- **Ocean palette in pixel-art**: the biome-derived palette collapses ocean names
  to grey (only land biomes have legend colours), so ocean-dominant worlds quantize
  bluish water toward grey — a known, deliberate characteristic; revisit if it
  reads poorly.
