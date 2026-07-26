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
| [Settlement](./settlement.md) | one goblin village → settlements condensed from a carrying-capacity field → **grown as the last frame of a derived deep history** (*The Living Community*: the field is now substrate, not placer; history places living settlements + standing ruins + separated territories) | the globe's habitability, freshwater, coast, temperature, and hostility, cell by cell, folded through the sibling `demography` domain's field and flow (now ticked per-era by paleoclimate); each species' psychology-derived field weights | settlement + ruin facts grown by history, one flagship (highest population) across every species that placed; a committed, queryable occupation skeleton |
| [Species](./species.md) | (Campaign Y2-1; no tier ladder of its own — authored data, not a generator) | — | a closed three-dimension mind vector and — for the settling peoples — a closed three-dimension society vector (*The Cloister* split the old six-dimension psychology vector so a solitary kind carries a mind but no society), a closed three-dimension perception vector (Campaign 15), a closed six-dimension articulation vector (Campaign Y2-3), and role vocabulary per species, consumed at the composition root to modulate settlement, culture, language, and each species' own religion |
| [Perception](./perception.md) | (Campaign 15; no tier ladder of its own — a lens and an hour, derived, not generated) | a species' authored perception vector | the characteristic hour and salience lens religion observes each species-flagship through |
| [Culture](./culture.md) | a fixed caste ladder → subsistence + emergent structure, modulated by species psychology (tier 1) | each flagship's environment (biome, coast, surplus, population, threat) and its species' psychology vector | subsistence + caste facts on each species' flagship |
| [Language](./language.md) | (Campaign Y2-3; no tier ladder of its own — a phonology and a naming grammar, drawn once per species-culture, then reused) | a species' authored articulation vector | a phoneme inventory and phonotactics per species-culture; generated settlement, deity, and epithet names (roman + IPA); `render_line`, the content→render seam religion's tenets pass through |
| [Religion](./religion.md) | one belief → a pantheon per species-flagship, each observed through its own perception lens (tier 1) | phenomena only (tier 0); each species-flagship's own lensed phenomena plus a bare society summary — strata count, priesthood presence (tier 1) | belief facts (tier 0); pantheon, `high-god`, and `cult-form` facts, one set per species-flagship (tier 1) |

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
culture itself, and Campaign 15 adds a second call site, not a second code
path: the composition root now runs the same religion genesis once per
species-flagship, each time handing it that species' own lensed phenomena,
and religion still cannot tell the difference between being called for a
goblin village or a kobold warren.

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

**Year 2 so far: firm ground, then a second people, then that people's own
eyes, then that people's own voice.** Campaign Y2-0 re-baselined every
census once, on corrected placement, before Year 2 built anything new on
top of it. Campaign Y2-1 then added the species row: `domains/species`
holds authored psychology vectors for goblin (the baseline) and kobold,
consumed by settlement's joint placement and culture's role vocabulary and
thresholds — the same coarse-constrains-fine discipline the
astronomy-to-theology cascade already proved, now widened into a second
axis (which people, not just which sky) that settlement and culture must
stay legible under. Religion did not follow at the time; its
`SocietySummary` input described one flagship's society, goblin only, and
the priesthood check was a literal `"shaman"` string blind to any other
vocabulary. Campaign 15 (The Eyes) closed that gap: a closed
three-dimension perception vector sits alongside the psychology one on
every species definition, a species-specific lens and characteristic hour
replace the single observer-independent salience every prior campaign
assumed, and religion now runs once per species-flagship through its own
eyes — a goblin's sun-headed pantheon and a kobold's moon-headed one stand
side by side on the same globe, at 10,000-seed scale, with zero exceptions
on the exact claims the campaign preregistered. Every name in both
pantheons was still, at that point, drawn from a syllable pool or an
English word list — Campaign Y2-3 (The Tongues) closed that last gap: a new
`domains/language` holds a real phoneme model and a closed articulation
vector per species, draws a phonology and a naming grammar once per
species-culture, and gives religion a permanent content→render seam,
`render_line`, so a belief now commits structured meaning and renders its
voice at display time instead of freezing English prose at genesis — a
goblin's formal, honorific-dense telling and a kobold's repetitive,
descriptive one, both generated, on the same page. [Campaign Y2-1's
chronicle](../chronicle/campaign-y2-1.md), [Campaign 15's
chronicle](../chronicle/15-the-eyes.md), and [Campaign 16's
chronicle](../chronicle/16-the-tongues.md) have the full accounts.
