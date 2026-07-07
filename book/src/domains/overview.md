# The Cascade, Tier 0 to Tier 1

As of Campaign 1b, every domain exists — at the shallowest depth that could
possibly work. This was the plan's central bet about sustainability: build the
*whole* cascade at tier 0 before deepening any part of it, so that every
kernel interface has a real consumer from the first months, and every later
campaign deepens something that already runs rather than bolting on something
new.

The cascade, in dependency order, with its current state:

| Domain | Tier 0 | Consumes | Contributes |
|---|---|---|---|
| [Astronomy](./astronomy.md) | a sun that never sets → a generated star system with a two-clock calendar (tier 1) | — | phenomena (the sky) (tier 0); phenomena (day/night, seasons, moon phases, notable neighbors) plus the sky's insolation, obliquity, and rotation fields (tier 1) |
| [Climate](./climate.md) | uniform mildness → banded climate + biomes (tier 1) | — (tier 0); elevation, sea level, seafloor, and the sky's insolation/obliquity/rotation (tier 1) | phenomena (the air); the biome + habitability field (tier 1) |
| [Terrain](./terrain.md) | one hand-placed vale → tectonic globe (tier 1) | — | place facts; the tectonic globe's elevation, boundary, unrest, and drainage fields (tier 1) |
| [Settlement](./settlement.md) | one goblin village → a scatter placed by suitability (tier 1, the Vale retired) | the globe's habitability, freshwater, coast, temperature, and hostility, cell by cell | settlement facts for every placed site, one of them the flagship |
| [Culture](./culture.md) | a fixed caste ladder → subsistence + emergent structure (tier 1) | the flagship's environment (biome, coast, surplus, population, threat) | subsistence + caste facts on the flagship |
| [Religion](./religion.md) | one belief → a pantheon, structured by society (tier 1) | phenomena only (tier 0); phenomena plus a bare society summary — strata count, priesthood presence (tier 1) | belief facts (tier 0); pantheon, `high-god`, and `cult-form` facts (tier 1) |

Two things about this table are load-bearing. First, the *Consumes* column
never names a domain — settlement consumes "habitability, freshwater, coast,
temperature, and hostility, cell by cell," not "terrain and climate," and
culture consumes "the flagship's environment," not "settlement." Wiring
specific fields to a specific settlement, and a specific settlement to a
specific society, happens in one file, the composition root, which is the
only place in the codebase where all domains meet. Second, religion's row is
the whole thesis in miniature: at tier 0 it consumed phenomena and nothing
else, so the same religion code that mythologized the changeless sun
mythologizes Campaign 2's moons and Campaign 5's seasons without being
edited — tier 1 adds one bare society summary, never a dependency on
culture itself.

A standing rule from the roadmap: **no domain may lead another by more than
about two tiers.** Depth stays balanced across the cascade, which keeps the
enrichment thesis — that upstream richness produces legible downstream
difference — honest and continuously tested.

**Year 1: all five domains deepened.** Campaign 5 closes Year 1 with every
row in this table at tier 1 — the last hold-out, religion, now consuming
the flagship's society as well as the sky. The cascade this table tracked
from the first month is, as of this campaign, no longer aspirational: a
change in astronomy (rotation, tidal lock) now measurably reorders climate's
bands, terrain's habitability, settlement's placement, culture's
subsistence and structure, and religion's pantheon and its shape, in one
composition root, with no domain aware of any other. The seed-42
spinning-vs-locked pair, threaded through every gallery page in this book,
is the same claim made visually at each layer; [Campaign 5's
chronicle](../chronicle/campaign-5.md) closes the year with the full
retrospective.
