# 0667. A placed site is re-sited from its vertex to one facet by a seeded draw

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) ·
**Relates:**
[0039](0039-epochs-replace-tiers-refine.md),
[0102](0102-one-per-cell-was-an-index-artifact.md),
[0141](0141-compass-navigation-is-an-overlay.md),
[0666](0666-the-enterability-gate-is-a-site-not-built.md),
[0669](0669-a-sites-tier-is-placed-or-derived.md)

In the context of caves and exotic sites existing only at level-6 vertices
(110–132 km apart) while the walk band addresses 1.126 km facets, we decided
that **a placed site is re-sited to exactly one facet by a draw seeded on
(vertex, reason)**, accepting one new stream label and explicitly accepting
that this is *not* an epoch.

## Context

Both source models answer per-**vertex**. `GeneratedTerrain::cave_at` takes a
`Vertex`; the 103 exotic sites are vertex-placed. A vertex spans 110–132 km.

Reading "is there a site near me?" off that mesh reproduces
`CLIM-water-label-resolution-vs-walk-band` exactly — the defect where a
per-vertex water label painted a river across a whole 6.49 km band. Every facet
for tens of kilometres would claim the same cave.

So existence and address are different questions. The model answers existence;
nothing answered address.

## Decision

```rust
// windows/worldgen/src/placement.rs
pub fn site_facet_for(vertex: Vertex, reason: SiteReason, seed: Seed,
                      geo: &Geosphere, walk_depth: u32) -> Facet

pub enum SiteReason { Cave, Exotic }
```

Keyed `cell/{vertex}/{reason}` under one new label:

```
SITE_PLACEMENT = "site/placement/v1"
```

`reason` is load-bearing, not decoration: without it a cave and an exotic site
warranted by the same vertex would draw the same facet and collocate every
time.

The draw picks within a quad `PLACEMENT_DEPTH_BELOW_GRID = 2` levels under the
grid cell containing the vertex, then fills the path tail — so a site lands
inside its own vertex's territory rather than anywhere on the sphere.

**Keyed on position, never on a generation ordinal**, per 0102 — the decision
that recorded what happens when a lattice draw keys on the order things were
made in.

## This is not an epoch — a correction

The spec asserted this minted an epoch and that "every world's site placement
moves." Both halves were wrong, on the project's own rule
(`domains/CLAUDE.md`): **new label = safe; changed or reused label = an epoch.**

A new label consumes no draws from any existing stream, so nothing that exists
re-derives. And site placement is *new behaviour* — there is no prior placement
for it to move.

Measured, not reasoned: both pin-isolation suites stayed green (astronomy
20/20, terrain 20/20) and `make rebaseline-goldens` was a no-op. The campaign
cost no epoch at all.

## Consequences

**A site has a real address.** "Somewhere within 120 km" becomes one facet,
which is what makes a site enterable in the walk band at all.

**The label is a permanent contract.** `site/placement/v1` may never be renamed
or reused; a deliberate re-placement is `/v2`.

**A byte-golden pins the draw.** `PLACEMENT_DEPTH_BELOW_GRID`, the digit loop
and the key format are jointly pinned by a literal `Facet`, so changing any of
the three reds a test rather than silently moving every site in every world.

**The price of a new placed kind is one `SiteReason` variant**, and forgetting
it is a collocation bug rather than a compile error — which is why the variant
exists rather than a bare boolean.
