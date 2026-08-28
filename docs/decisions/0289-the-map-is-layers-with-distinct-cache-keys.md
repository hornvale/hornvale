# 0289. The map is layers with distinct cache keys, and terrain never shares an invalidation key with discovery

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot, spec §3) ·
**Relates:** [0288](0288-fog-of-war-is-band-as-alone.md) (which layer the
discovery gate lives on), [0287](0287-a-zoom-rung-is-a-mesh-depth.md) (what a
tile key names); [The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a world plate that drew terrain and point sites in one pass,
we decided that **the map is drawn as separate layers with distinct cache keys —
terrain keyed on `(frame, rung, tile)`, features keyed on the discovery version,
perception keyed on the turn** — accepting three compositions per frame in
exchange for terrain that no discovery can invalidate.

## Context

`CLIENT-tiles-need-the-overlay-split` recorded the measured reason before this
campaign existed: a tile keyed on the discovery version is invalidated by every
discovery — *the whole pyramid, for one settlement*. A tile cache is not viable
at all under a shared key, so the split is a precondition of the cache rather
than tidiness laid on top of it.

The layers differ in how often their keys move by orders of magnitude. A frame
is fixed once at load from the world's own rotation; a rung moves on a
keystroke; a tile is a fixed subdivision of the chart. Discovery moves when the
player arrives somewhere. The turn moves every turn. Fusing any two of those
prices the slower one at the faster one's rate.

## Consequences

- **The cache became worth building, and the measurement is the argument.** At
  200×200 on the coarsest rung: **0.056 ms warm — 893× under the 50 ms bar** —
  against 79 ms cold, paid once per `(frame, rung)`. A full keystroke round trip
  is 0.119 ms, 419× under. None of that survives a shared invalidation key.
- **A layer may be refused rather than drawn, per band**, and that is a
  property of the layer and not of the caller. The feature layer draws a site
  only once discovered; the perception layer refuses off band B
  ([0290](0290-the-perception-layer-is-drawn-where-the-mesh-is-reachable.md)).
- **The feature layer projects a site's own coordinate** rather than waiting for
  a terrain sample to land on it, which is what dissolved
  `MAP-settlement-glyph-may-be-unreachable-at-any-shipped-zoom`: all 389 of seed
  42's settlements project inside the chart at every one of the seven shipped
  rungs.
- **What we give up:** a single-pass draw, and with it the ability to reason
  about the finished picture in one function. The layers meet at
  `spread::compose`, and a defect in composition is now its own failure mode —
  one of which this campaign found (a degenerate divergence past `virtual_h`
  that nothing pins).

## See also

Spec §3; `clients/game/bin/src/plate.rs`, `clients/game/bin/src/tiles.rs`.
