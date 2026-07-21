# Retrospective — The Cartographer

One page of process, not product. This campaign pivoted its medium mid-flight;
most of the lessons are about how that pivot was found and survived.

## What worked

- **The screenshot visual pass is a design instrument, not just a bug-catcher.**
  Every prior render campaign used it to catch black-globe shader failures. Here
  it did something larger: iterating captures with Nathan revealed that pixel-art
  *fights a sphere* (the wave/limb jumble was the tell), and overturned the whole
  medium choice. A campaign's foundational assumption fell to a screenshot. Budget
  the visual pass as a design loop, not a final QA step.

- **Web research broke a taste-deadlock.** Nathan could see the result was wrong
  but "wasn't sure I can envision" the right one. A short research pass surfaced
  the universal industry pattern — flat map zoomed in, globe zoomed out — which
  both *validated his instinct* and handed over a concrete, precedented
  architecture. When aesthetic intuition is stuck, look at how the field already
  solved it before inventing.

- **Isolation debugging cracked "the symbols won't render."** After several
  rounds of guessing why sprites were invisible, a throwaway build forcing every
  sprite to huge bright magenta proved they *did* render — they were just too
  small. That one experiment converted an open-ended hunt into a one-line size
  fix. Isolate before theorizing.

- **Pure-function decomposition was pivot-insurance.** Extraction, budget,
  palette, and sprite glyphs were built as pure modules over plain data. When the
  campaign moved pixel-art off the sphere and onto a flat tier, they transferred
  *wholesale* — the pivot wasted almost nothing. Keeping the derivation pure and
  the rendering thin is what made a medium change cheap.

## What to do differently

- **Compute registry ids at CLOSE, after absorbing main — never at spec-time.**
  This campaign reserved `MAP-61` in its spec; a parallel campaign ("the
  connection graph") took `MAP-61` while this one ran, and the id had to become
  `MAP-62` at merge, rewriting every reference. The registry is a shared,
  racing resource; the only safe time to claim a number is after the branch has
  met current main.

- **The stage-boundary absorption cadence was missed.** This branch's first
  meeting with main was at the close, 20+ commits in — the `MAP-61` collision
  was caught only because the close absorbs main by discipline, not because the
  cadence was followed mid-campaign. A stage-boundary absorb would have surfaced
  the collision (and any semantic drift) next to its cause. Absorb main at every
  plan-stage boundary, as the process says.

- **A mid-campaign re-scope earns a fresh spec + plan, not an ad-hoc redirect.**
  The pivot got its own transition spec (a real G3 stop) and a staged plan. That
  kept a large change legible — the alternative, redirecting the existing plan
  inline, would have buried the decision.

## Nets that caught what tests could not

- The **whole-branch review** (opus) found a vertical-mirror registration bug —
  the symbol overlay flipped against the base texture (`DataTexture.flipY`
  default) — invisible to every unit and e2e test, which assert only counts and
  non-blankness.
- The **e2e suite** caught a real *product* bug the unit tests never could: a
  third canvas added at boot defaulted to `pointer-events:auto` and, being
  topmost, intercepted globe clicks until the first rung transition.

Review, visual pass, and e2e are complementary nets. None of them alone would
have shipped this correctly.

## Follow-ups (promoted from the campaign register)

- **Water tier** — surface `windows/scene`'s per-tile water class (rivers /
  salt-lakes / waterfalls; the data exists at room scale) + a wasm release; then
  a client water symbol tier. Carve-out, Nathan sequences.
- **Land-snap on handoff** — the sub-camera often lands on ocean; snap to the
  nearest landmass so a zoom-in reliably reaches interesting terrain.
- **Internal zoom-LOD on the map** — needs the map rung to grow its own pan/zoom
  controls; then the salience ladder deepens as you lean in.
- **Landmark cities** — needs settlement `features` in the region scene (region
  data currently carries none).
- **Region-neighbour stitching** — pan seamlessly across region-tile boundaries.
- **Resolution / biome-variety tuning** — regions are chunky (~65 px) and modest
  in variety; a visual-pass tuning pass.
- **The engraving map** — the same flat-tier engine with an Obra-Dinn-style
  symbol atlas and palette (the other idiom Nathan loved).
