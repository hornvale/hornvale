# The Overworld — Design

**Campaign:** The Overworld (working name) — rework the Orrery's flat pixel-art
map into a 16-bit RPG overworld aesthetic.
**Registry:** new row (id at CLOSE, per the standing lesson).
**Repos:** Orrery (client-only — **no wasm / producer change**). **Autopilot**
engaged (G3/G6 hard stops).
Fourth and LAST of the view-remake program: A view-switch (MAP-65) → B voxel
globe (MAP-66) → C voxel map (MAP-67) → **D (this)** pixel-art rework.

---

## 1. Goal

Rework the flat map's **Pixel-art** style (the `'pixel'` `MapStyle` from The
Diorama — `regionPixelTexture`/`pixelColorFor`) from its current flat biome-fill
into a **16-bit RPG overworld** look (Final Fantasy / Dragon Quest): a tight
punchy palette, dithered texture inside biomes, crafted outlined coastlines, and
dark landform outlines. Nathan's diagnosis of the current map: flat blocky fills,
hard stair-step coastlines, sparse floating symbols, a washed-out palette. This is
the program's last campaign and the one flagged as hardest — v1 ships the
high-impact core; motifs and animation are deferred.

## 2. Keystones (from the ideonomy pass)

Tree-finding decomposed "a pixel-art map render" into **7 components**: base fill,
texture-within-fill, coastline, palette, resolution, outlines, symbols. Today only
base-fill + sparse symbols exist. Two findings:

1. **Dithering is the hidden hub.** One technique adds within-biome texture,
   crafts coastline foam, harmonizes palette banding, AND fakes detail without
   more data. Ordered (Bayer) dithering + a tight palette is the single
   highest-impact move — it is what turns "biome fill" into "pixel-art." **It is
   also deterministic** (a pure function of output-pixel position + cell data — no
   `Math.random`), which the constitution requires.
2. **The real problem is procedural craft.** We cannot hand-author tiles (the sim
   ships numbers; the client renders deterministically). D must *procedurally
   generate hand-crafted-looking* pixel-art. To have room for that craft, the
   pixel-art renders to its **own higher-resolution texture** (detail computed per
   output pixel), decoupled from the coarse 65×65 region-node grid.

## 3. Architecture — a procedural overworld renderer

Today `regionPixelRGBA` (`mapTexture.ts`) produces one RGBA per region node
(65×65), NearestFilter-scaled to the quad; `pixelColorFor` (`pixelBase.ts`) gives
each node its flat biome/water color; `mapSymbols` overlays 2D sprites.

D adds a new renderer that produces a **higher-resolution procedural RGBA buffer**
(e.g. 384×384, a `const` `OVERWORLD_TEXTURE_DIM`) for the `'pixel'` style:

- For each output pixel `(px, py)`: map to a region `(u, v)`; the biome/elevation/
  water at that point = the **nearest region node's** fields (the data is coarse;
  that is fine — the craft is at render resolution).
- Apply, in order: **palette** quantization → **within-biome dither** (texture) →
  **coastline** band + outline + foam (detected by sampling land/water at
  neighboring output pixels) → **biome-boundary + land-silhouette outlines**.
- The output is a `NearestFilter` `DataTexture` on the map plane (replacing
  `regionPixelTexture` for the pixel style). `mapSymbols` still overlays on top
  (lightly re-tuned if needed).

The **Voxel-2.5D** style (The Diorama) is untouched — this reworks only the pixel
branch's texture. `pixelColorFor` may be kept for the voxel style's per-cell color
(Style ⟂ Lens) and superseded by the palette renderer only for the pixel texture;
or the new palette becomes the shared source — decide in the plan, but do not
regress the voxel style's coloring.

## 4. The v1 elements (the high-impact core)

1. **Tight palette.** A designed, limited, punchy per-biome palette (2–3 tones per
   biome for the dither pair + a dark outline tone + a shallows/beach tone + a foam
   tone). Replaces the washed reference colors. A `const` palette table, keyed by
   the existing biome legend indices (never hardcode legend order — resolve by
   name, as The Freshwater did for water indices).
2. **Within-biome dithering.** An ordered Bayer matrix (4×4 or 8×8) indexed by
   `(px, py)`: each biome fills with a deterministic two-tone dither between its
   light/dark palette tones, giving texture and gentle shading instead of a flat
   block. Threshold may key off elevation (higher = lighter, say) so the dither
   also reads as relief.
3. **Crafted coastlines.** Where an output pixel is water within N pixels of land
   (or vice versa): draw a **shallows/beach band** (a lighter tone just inside the
   water/land edge), a **dark outline** exactly on the boundary, and **foam**
   (a dither of white/light on the water side of the outline). This is where the
   overworld look lives — a hard blocky edge becomes a crafted shore.
4. **Outlines.** A dark outline where adjacent output pixels are different biomes
   (biome boundary) and around the land silhouette — the "drawn map" tell. Thin
   (1 output pixel), from the palette's dark tone.

## 5. Deferred (v2 / followups)

- **Biome motif-stamping** — mountains as ranges, forests as tree-clusters, deserts
  as dunes, drawn INTO the texture as repeated procedural motifs (beyond the
  existing `mapSymbols` sprites). Big; the biggest remaining lever after the core.
- **Animated water** — shimmer/wave scroll (must stay deterministic / Lorenz-safe
  — a positional-phase animation, no chaotic state).
- **Rivers/lakes styling** — the freshwater layer (`RIVER_BASE`/`LAKE_TONE`)
  re-styled to the overworld palette + dithered banks.
- **Modern-indie shading** — the rejected alternate aesthetic; not this program.

## 6. Determinism / save-format

**None touched.** Pure client render — a procedural texture. **No `Math.random`**
anywhere: ordered dithering is a pure function of `(px, py)`; coastline/outline
detection is a pure function of the region field; any "noise" is positional/hashed
deterministically. No new scene fields, no wasm (stays v12), no epoch. Orrery-only,
reversible.

## 7. File structure (Orrery)

- `src/views/styles/overworld.ts` (new) — the procedural renderer:
  `overworldRGBA(region, dim): Uint8Array` and the palette table + Bayer matrix +
  coastline/outline helpers. Pure functions, heavily unit-testable (a known
  mini-region → asserted pixels: dither alternation, a coastline band where land
  meets water, an outline at a biome boundary).
- `src/views/mapTexture.ts` — an `overworldTexture(region)` wrapper (mirrors
  `regionPixelTexture`) producing the `DataTexture` from `overworldRGBA`.
- `src/views/mapView.ts` — the `'pixel'` branch mounts `overworldTexture` instead
  of `regionPixelTexture` (the plane/camera/symbols otherwise unchanged).
- Tests: `overworld.test.ts` (the pure renderer); `mapView` (pixel style still
  mounts a plane + top-down camera, now with the overworld texture); `e2e` (pixel
  style renders, non-blank, no console errors) + the controller visual pass.

## 8. Risks

1. **It's an aesthetic campaign — the tuning IS the work.** Palette, dither
   thresholds, coastline band width, outline weight all need a screenshot visual
   pass (controller-run, reading PNGs, as The Diorama's height tune). The code
   structure must expose these as named `const`s for iteration. Expect 2–3
   screenshot rounds; the first pass will not be final.
2. **Coarse data, fine render.** The region is 65×65; coastlines detected at
   384² will show the underlying 65-node stair-step unless the band/outline
   smooths it. The coastline band width must be enough to read as crafted over the
   blocky data boundary (this is a feature — the band hides the stair-step).
3. **Performance.** 384² = ~147k output pixels per region rebuild, each doing
   neighbor lookups for coastline/outline. Measure (as The Massing did); if a
   rebuild stalls, lower the dim or precompute the land/water mask once per region.
   The map rebuilds only on region/style change (not per frame), so headroom is
   large — but measure, don't assume.
4. **Don't regress the voxel style or the lens coloring.** The voxel diorama and
   the non-pixel lenses must be untouched; the overworld palette applies to the
   pixel texture only (or is shared without changing voxel output — verify).

## 9. DoD / registry

- New registry row (id at close), shipped; **completes the four-campaign Orrery
  view-remake program** — say so, cross-link MAP-65/66/67.
- Chronicle + retro; freshness sweep of the map/Cartographer chapters.
- Confidence Gradient: re-score if a rendering bet moved (likely none — this is
  aesthetic polish, not a capability bet; grep to confirm).
- Orrery deploy (externally visible — confirm at G6); **no wasm release**.
- Followups: biome motif-stamping, animated water, overworld river styling.
