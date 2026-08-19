# 0147. A landscape feature is individuated by traversal, and its name is derived, never committed

**Status:** Accepted (2026-08-19) · **Decider:** Nathan · **Relates:**
[0024](0024-settlement-name-uniqueness-is-reference-time.md),
[0083](0083-a-label-per-algorithm-and-never-in-advance.md),
[0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md)

In the context of a river and a landmass having every piece of the naming
machinery already built and nothing calling it — `NameKind::Landform`'s only
caller was `volcano_name`, and the connected-components walk `shape.rs`
already ran was throwing its cell sets away and returning bare sizes — we
decided that **a landscape feature is individuated by graph traversal over
already-committed terrain, and its name is a pure function of `(seed,
identity, species)` re-derived on read, never committed to the ledger**,
because the alternative — a committed name — cannot survive the one thing
every floor and every sea-level pin can do to a component's numbering.

## Context

**The Watershed's null does not bind here, and that distinction is the
reason this decision exists rather than a repeat of that one.** The Watershed
measured landscape naming as a cure for settlement-name collisions and found
a 44.8% floor, because a settlement's landscape is shared by construction —
its effective discriminator cardinality is a tenth of its nominal one. That
falsified a *criterion*, not the *design*: The Watershed's identity scheme,
size floors and API shapes are reused here substantially unchanged. This
campaign's payoff is a toponymic knowledge layer — a name a character can
learn, lack, or be told — which decision 0024 already licenses independently
of collision rate.

**Identity is the lowest cell id of a component, or a river's terminal
cell.** Canonical, integer, order-independent, no tie-break — and explicitly
**not** stable under anything that moves a coastline: a sea-level change can
renumber every landmass. That fragility is declared rather than discovered
because of what makes it safe: a name is *derived*, never committed, exactly
as `volcano_name` already worked. A terrain change that renumbers features is
already an epoch by other means (0084), so no saved world can carry a name
that contradicts its own terrain, and no epoch is owed to naming itself —
decision 0083 already classifies `river/name/v1` and `landmass/name/v1` as
phantom labels, same algorithm as `language/<species>/name/landform`,
different subject.

**The floors were re-derived against the live tree, not inherited.** Two of
the spec's three committed seed-42 counts were wrong (rivers 106, not the
Watershed's 115 or the chronicle's 123; landmasses 11 at the inherited fixed
floor, not 14), and the salt-lake floor the spec inherited (`>= 20` cells)
yields **zero** salt lakes on the current tree — the largest component is two
cells. Salt lakes ship at floor 1, on a different principle than landmass and
sea: `WaterKind::SaltBasin` is a classification, not a threshold on a
continuous field, so a single-cell salt basin is a real ~112 km salt pan
(Great Salt Lake / Etosha scale at this resolution), not a quantization
artifact the way a one-cell "landmass" is.

## Consequences

- **A feature surfaces every name it has; none is canonical.** Fifteen
  peoples means fifteen names is a real number on a single feature. There is
  no primary-name field anywhere in this campaign, matching decision 0142's
  discipline against collapsing distinct values into one slot.
- **The magnitude ordering is a placement channel, not a measurement axis.**
  Ranking a feature set by size is a claim about what fits on a page, not a
  claim about the world — consistent with 0142's one-axis-per-channel rule,
  which this ordering does not participate in.
- **A session's knowledge of a name is keyed on `(FeatureId, species)`, not
  `FeatureId`.** A character who learns a second name for a place already
  known has learned something, so the fog campaign that reads this decision
  inherits a strictly finer key than "does this character know this place".
- **The volcano class rides the same rule by extension, not by exception.**
  `volcano_at`/`volcano_name` were individuated and named with no surface;
  joining them here was a join, not a new clustering algorithm, and the same
  identity-is-draw-free property covers all five classes.
- **What this decision does not cover:** any feature needing a clustering
  pass rather than a traversal — mountain ranges, bays, capes, straits,
  peninsulas, biome regions — stays out of scope, carried forward in the idea
  registry rather than decided here.

## See also

`domains/terrain/src/landscape.rs` (`components`, `classify`),
`domains/terrain/src/provider.rs` (`FeatureIndex` on `GeneratedTerrain`),
`windows/worldgen/src/gazetteer.rs` (`feature_salt`, `feature_name`,
`gazetteer_features`), `domains/language/src/naming.rs`
(`NameKind::Landform`), `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md`
§3, §10, [the chronicle](../../book/src/chronicle/the-gazetteer.md).
