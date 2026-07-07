# The Cascade at Tier 0

As of Campaign 1b, every domain exists — at the shallowest depth that could
possibly work. This was the plan's central bet about sustainability: build the
*whole* cascade at tier 0 before deepening any part of it, so that every
kernel interface has a real consumer from the first months, and every later
campaign deepens something that already runs rather than bolting on something
new.

The cascade, in dependency order, with its current state:

| Domain | Tier 0 | Consumes | Contributes |
|---|---|---|---|
| [Astronomy](./astronomy.md) | a sun that never sets | — | phenomena (the sky) |
| [Climate](./climate.md) | uniform mildness → banded climate + biomes (tier 1) | — (tier 0); elevation, sea level, seafloor, and the sky's insolation/obliquity/rotation (tier 1) | phenomena (the air); the biome + habitability field (tier 1) |
| [Terrain](./terrain.md) | one hand-placed vale → tectonic globe (tier 1) | — | place facts; the tectonic globe's elevation, boundary, and unrest fields (tier 1) |
| [Settlement](./settlement.md) | one goblin village | a place to stand | settlement facts |
| [Culture](./culture.md) | a fixed caste ladder | a village to structure | caste facts |
| [Religion](./religion.md) | one belief | **phenomena only** | belief facts |

Two things about this table are load-bearing. First, the *Consumes* column
never names a domain — settlement consumes "a place to stand," not "terrain."
Wiring a specific place to a specific village happens in one file, the
composition root, which is the only place in the codebase where all domains
meet. Second, religion's row is the whole thesis in miniature: it consumes
phenomena and nothing else, so the same religion code that mythologizes
today's changeless sun will mythologize Campaign 2's moons without being
edited.

A standing rule from the roadmap: **no domain may lead another by more than
about two tiers.** Depth stays balanced across the cascade, which keeps the
enrichment thesis — that upstream richness produces legible downstream
difference — honest and continuously tested.
