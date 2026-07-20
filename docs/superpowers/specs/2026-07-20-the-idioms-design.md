# The Idioms — a render-style layer for the Orrery (design)

**Campaign:** The Idioms · **Repo:** orrery (client only) · **Status:** spec, awaiting review
**Registry:** MAP-60 (this spec is its elaboration)

## Motivation

The Orrery renders one deterministic planet in one style: a photoreal relief
sphere. Its "lens" toggle (natural / topographic / temperature / moisture /
precip / unrest / plates) changes only *which data is colored* — it is a hue
substitution over the same visual grammar. This campaign adds an orthogonal
axis: the **render style** — *how* the world is drawn — and ships several of
them for delight.

The distinctive framing (not a generic filter gallery): every pixel of this
globe has real sim meaning behind it, so a style is not a blind screen-space
filter but a **faithful re-documentation of the same world in a different
representational idiom**. The invariant is the world; the variable is the
tradition looking at it. Each idiom carries an implied maker and era — the
engraving is the world's own Age of Exploration, the watercolor a Romantic
naturalist's field atlas, the pixel-art the world as its own 8-bit era would
render it, the cel/ink a modern illustrated documentary. Switching styles
switches *whose eyes*, never *what is there*.

## Scope

**In (v1) — four styles plus the plumbing:**

- **Pixel-art** — low-resolution render target, palette quantization, nearest-
  neighbour upscale; optional ordered dithering for gradients.
- **Watercolor / painterly** — edge-preserving smoothing, soft colour bleed,
  paper-grain overlay.
- **Cel / ink** — posterized (banded) lighting plus edge-detected ink outlines.
- **Engraving / woodcut** — high-contrast monochrome, hatching/stipple density
  keyed to shading and relief, sepia-on-cream tint.

Photoreal remains the default and is the **identity style** (no passes).

**Signature data hook per style (the "not a dumb filter" proof, kept cheap):**
each v1 style carries *one* hook that reads the world's actual data rather than
just the framebuffer — most prominently, pixel-art's palette is **derived from
this world's own biome mix** (each seed gets its own colours). Engraving's
hachure keyed to elevation+light is the second proof and is intrinsic to the
style, not an add-on.

**Out (deliberate non-goals for v1):**

- **A mark-owning cartographic renderer** (contours, place labels, a legend, a
  self-drawn map). That was the "legibility" fork; it is a larger, on-ethos
  future campaign (the visual twin of the self-writing Book, LANG-35), not this
  one.
- **A full data G-buffer.** v1 styles read the color + depth + normal buffers
  plus cheap CPU-side hooks; per-pixel data-keying (watercolor bleed pooling at
  true biome boundaries, engraving's "here be monsters" flourishes at real
  gyres) is a fast-follow, not v1.
- **The "only-this-world" styles** — star-chart, illuminated mappa-mundi,
  orrery-schematic, ukiyo-e. These are the open axis's future entries; the
  pipeline is built to accept them, but none ships in v1.

## Architecture

A **`RenderStyle`** is an ordered chain of screen-space passes (three.js
`EffectComposer` / `ShaderPass`, from `three/addons` — no new dependency) plus
its parameters. A style selector swaps the composer's active chain. The globe
scene renders once; the style transforms the rendered frame.

Key properties:

- **Photoreal = identity.** The default style is a plain `RenderPass` with no
  effect — today's look, unchanged.
- **Orthogonal to the lens.** The lens sets vertex colours *before* the frame is
  drawn; the style transforms the drawn frame. So any lens × any style composes
  for free, with no combinatorial code.
- **Untouched by the geometry pipeline.** Styles are screen-space only. The LOD
  diff, amortized build, bilinear normals, and region stitch are all upstream of
  the composer and unaffected.
- **Reads color + depth + normal.** The pass chain samples the rendered colour
  buffer, plus depth and a normal buffer where a style needs geometry (cel edge
  detection, engraving relief). No bespoke elevation/biome G-buffer in v1.
- **Cheap CPU-side data hooks.** A style may precompute a small parameter from
  the tiles data once at build (e.g. the biome-derived palette), passed as a
  uniform — not a per-pixel data texture.

**What gets styled.** The style applies to the whole 3D globe scene. Because
markers and place-labels live in that scene, they would be stylized too — a
pixelated label can go illegible. Resolution: render the globe scene through the
style, and if labels/markers suffer, draw them in an un-styled pass composited
on top (world stylized, annotations readable). The HUD is DOM and is never
affected.

## Data flow

```
scene data ──(lens)──> vertex colours ──> globe scene render ──┐
                                                               ├─> EffectComposer
depth + normal buffers ────────────────────────────────────────┘   (style pass chain)
CPU-side hook (e.g. biome palette) ──> uniform ────────────────────>  ──> screen
```

The lens and the geometry pipeline are unchanged; the composer is the only new
stage.

## The four styles — intent and passes

- **Pixel-art.** Render the scene to a low-resolution target (a fraction of the
  canvas); quantize each pixel to a fixed palette **derived once from the
  world's biome distribution**; upscale nearest-neighbour to the canvas.
  Optional ordered (Bayer) dither on the low-res target for banded gradients.
  Reads: colour only.
- **Watercolor / painterly.** An edge-preserving smoothing pass (e.g. Kuwahara
  or a bilateral approximation) to flatten into washes; a subtle colour-bleed at
  high-gradient boundaries; a multiplied paper-grain texture. Reads: colour
  (+ depth to keep the silhouette crisp).
- **Cel / ink.** Posterize the luminance into a few flat bands; detect edges
  from depth + normal (a Sobel/normal-discontinuity pass) and composite a dark
  ink outline. Reads: colour + depth + normal.
- **Engraving / woodcut.** Desaturate; drive a hatching/stipple texture whose
  density tracks shaded luminance (which already encodes relief + light);
  tint sepia-on-cream. Reads: colour (+ normal for cross-hatch direction).

Exact visual tuning of each is settled during implementation by the
controller's visual pass (screenshots), not pinned in this spec.

## Testing and acceptance

- **Unit tests** on the pure, non-visual bits: the biome-derived palette
  (deterministic given a tiles fixture), pass-chain configuration per style,
  the style-selector state machine.
- **Visual acceptance** is the controller's screenshot pass, one per style, at a
  representative far view and a mid-zoom — the same discipline that caught the
  shading regression. Determinism is waived client-side (decisions 0022/0023);
  there is no golden and no drift-check on the rendered pixels.
- The existing smoke e2e stays green; a boot-in-each-style smoke check is added
  (each style renders without error, canvas non-blank).

## Constraints

- **Orrery only. No sim/kernel/wasm change. No world-wasm release.** Everything
  is client-side screen-space rendering; the scene data contract is unchanged.
- **three.js only**, using the existing `three/addons` (EffectComposer /
  ShaderPass / RenderPass) — no new dependency.
- Determinism waived client-side (0022/0023); the render may change freely.
- The build stamp and the perf discipline stand: post-processing adds fullscreen
  passes, cheap on real GPUs; pixel-art (low-res target) is if anything cheaper.
  The controller confirms no per-style frame-budget regression on real hardware.

## Related and deferred directions (captured, not built here)

- **Orrery UI polish (direction A, deferred).** Finer/modern settlement dots; a
  biome-data pane on globe click; a richer settlement pane (race, place-name
  literal meaning, number of souls); zoom-based settlement culling (bigger ones
  first, smaller phased in). Its own future Orrery campaign; the settlement-pane
  enrichment and size-culling depend on whether settlement metadata is in the
  scene or needs a producer addition.
- **Higher local detail (direction B, deferred).** Already mapped in the
  frontier: MAP-49 (sub-cell hash-noise as a scene data mode), MAP-28 / MAP-29 /
  MAP-30 (the room-scale local tier). MAP-26 (uniform adaptive mesh refinement)
  is rejected.
- **The mark-owning cartographic style (future).** The "legibility" fork — the
  self-drawing map, twin of the self-writing Book (LANG-35), part of the
  self-describing-program north star (UNI-29).

## Decisions ledgered in brainstorming

- **G1 — purpose: delight, not legibility.** Styles are screen-space skins for
  aesthetic variety, not a mark-owning cartographic renderer. Lighter, broad
  appeal, decoupled from the geometry pipeline.
- **G1 — batch: the four styles**, engraving serving as the built-in "these are
  data-aware, not filters" proof, so no fifth style is needed for v1.
- **G1 — data-awareness level:** colour + depth + normal + one cheap CPU hook per
  style; no G-buffer.
- **G1 — styling scope:** the whole globe scene; layer un-styled markers/labels
  on top if legibility suffers.
