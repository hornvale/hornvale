# Terrain

**Questions it answers:** What places exist? What is each like — its name,
its biome? (Eventually: what is *between* places, and how does land shape
everything built on it?)

**Tier 0 — one hand-placed vale.** A single place, "the Vale," a temperate
forest, committed to the ledger at genesis as three facts: it exists as a
place, it has a name, it has a biome. Terrain is the first domain that
*writes* — its genesis is the first real exercise of the fact envelope, and
the two predicates it registered (`is-place`, `biome`) were the concept
registry's first domain-contributed vocabulary.

**Why hand-placed is honest.** Tier 0 makes no pretense of generation: the
Vale is authored, exactly one exists, and the `places` query simply reads
the ledger back. What this buys is the *interface* — settlement genesis takes
"a place to stand" as an opaque entity, the almanac lists places without
knowing how many there are, and the REPL's `places` command works — so that
when generation arrives, nothing downstream changes.

**The tier ladder ahead** (Campaign 3, reviving the vision book's deleted
chunking chapters): the region graph — the world as nodes and edges rather
than a grid — with elevation, temperature, and moisture as noise-backed
fields, biomes derived from their intersection, and place names that feel
authored rather than generated. The anti-repetition mandate from the vision
book's dungeon chapter ("every room must feel created with intent") becomes
terrain's standing acceptance test.
