# 0288. Bands B through E share one epistemic model; fog of war is band A's alone

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (the correction that
reshaped the spec — §0) · **Relates:**
[0160](0160-walk-is-the-clients-third-focus-and-its-default.md) (the
travelled/consulted split this rides on),
[0196](0196-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md);
[The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a client that shows the world at five travel scales — a room
(**A**), wilderness travel (**B**), and three consulted map scales (**C/D/E**) —
we decided that **terrain is known at bands B through E and only *features* are
gated by discovery, and that real fog of war belongs to band A alone**,
accepting that a player sees geography they have never walked.

## Context

A creature native to this world knows its own geology and physical geography.
What it does not know is *features* — cities, interesting places, place names —
until it has been to them. In the owner's words:

> A is really the only scale at which the "Fog of War" really matters […] For
> B, C, D, and E, I don't think the "Fog of War" is remotely important. "Fog of
> Ignorance?" yes, absolutely, we shouldn't have all of the features of a given
> grid position available to us until we have navigated to them via A or B. But
> we don't need to worry about whether to render it or not.

**This record exists because the campaign's first spec draft asserted the
opposite and built on it.** That draft treated band B as egocentric and
knowledge-limited — fog of war, sensed-versus-remembered, an observer-centred
projection — and on that premise raised a fidelity/cost carve-out about widening
the per-turn perception packet from 31 facets to ~976. The premise was the
drafting session's own invention, not anything the project had ratified. It was
plausible, internally consistent, and cited real code; nothing mechanical caught
it, and the owner restating what the game *is* did.

## Consequences

- **The epistemic boundary is A versus everything above it**, never walk versus
  plate. Band A is already a square 4-neighbour lattice with its own sight
  radius — it is already the roguelike view with real fog of war, and this
  campaign did not touch it.
- **One raster renderer serves B through E** as a single zoom ladder
  ([0287](0287-a-zoom-rung-is-a-mesh-depth.md)). Two renderers would have been
  the cost of the premise above.
- **The 31-facet perception packet was never a limit on the terrain layer.** The
  client already holds `terrain`, `geo` and `nearest` and derives the whole world
  plate from them with no wire traffic at all. `PURVIEW_RADIUS` is untouched, and
  the carve-out raised against it is withdrawn.
- **What we give up:** the walking view discloses terrain the character has not
  visited. That is deliberate, and it is what "fog of ignorance, not fog of war"
  buys — the discovery gate moves entirely onto features, where it can be
  reasoned about as one rule instead of two.

## See also

Spec §0, §1, §3; `clients/game/bin/src/plate.rs` (`draw_feature_layer`'s
discovery gate).
