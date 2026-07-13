# 0038. Identity computes on the canonical grid; observation samples fields

**Status:** Accepted (2026-07-11) · **Decider:** Nathan

In the context of the Crust epoch making crust thickness and age *pointwise
fields* (stateless functions of position — decision-free, no per-cell draws)
and raising the geodesic grid to level 6 with a `--globe-level` pin, facing
the question of what "resolution" means for a world's identity, we decided:
**a world's identity-bearing derivations compute once on the world's canonical
grid; everything pointwise is a `Field` that observation may sample at any
resolution.**

**The two halves.**

- **The canonical grid bears identity.** Sea level (an exact percentile),
  drainage and endorheic basins (graph BFS), connected land components,
  settlement placement, and paleoclimate strata are all *mesh-bound* — they
  depend on the set of cells, not just on positions. They compute once, on the
  grid the world's `--globe-level` pin selects (default 6). The pin is part of
  world identity: **the same seed at a different level is deliberately a
  different world**, because these mesh-bound quantities genuinely differ.

- **Fields are resolution-free.** Crust thickness/age (and the render lens's
  coastline refinement before them, Campaign 25) are pure functions of a
  unit-sphere position. Any grid at any level *samples* the same underlying
  field; a finer grid is a finer *observation* of one fixed world, not a
  different world. This is the constitution's coarse-constrains-fine doctrine
  (§2.2) made literal: the field is the coarse truth, the sample is the fine
  view.

**Why.** Before this, resolution was a compile-time constant baked into every
world, so changing it was a silent epoch. Separating the two halves makes
resolution a *first-class, honest* axis: fields cost nothing to resample, so a
future interactive client (the Living Globe) can render terrain at pixel scale
without minting a new world; and the mesh-bound quantities that genuinely
*cannot* be resolution-free are named, isolated, and bound to identity through
the pin rather than through an invisible constant.

**The precedent.** Campaign 25's render-time coastline refinement was the first
instance — a lens that sampled the elevation field below cell scale without
touching world identity. Crust generalizes it from the render layer into the
generator: the crust `Field` is sampled by whatever canonical grid the world
chose, and the "coarse constrains fine" boundary is drawn exactly at the
graph-versus-field line.

**Consequences.** `TerrainPins.globe_level` (legal 4–7) joins world identity and
is recorded in the ledger like every pin. Worldgen holds a per-level geosphere
cache. Cross-level consistency is a tested property: `crust_thickness` byte-
agrees at vertices shared between nested icosphere levels. The Sculpting epoch
(v3) inherits this contract; its relief and erosion fields sample the same
canonical grid.
