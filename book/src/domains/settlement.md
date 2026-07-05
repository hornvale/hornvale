# Settlement

**Questions it answers:** Where do people live? What is the settlement
called, how many live there, and where does it stand?

**Tier 0 — the goblin village.** Genesis mints one village in whatever place
it is handed, with three generated properties: a name, a population between
forty and eighty, and a location fact tying it to its home. Seed 42 yields
**Gruugish, population 57**.

**How the name happens** — this small feature carries three load-bearing
ideas at once:

1. *Candidate generation.* Two or three syllables drawn from a goblin
   syllable pool, capitalized — the direct descendant of the goblin-mutter
   system in the vision book's language chapter, and the most primitive
   ancestor of the phonaesthetic name generation planned for the language
   domain.
2. *Refinement, used in anger.* The name is chosen through the kernel's
   consistency engine, which rejects any candidate whose commitment would
   contradict a committed fact. Today, with an empty world, nothing ever
   conflicts — but every future settlement will flow through this same
   pathway when the ledger is crowded enough for collisions to be real.
3. *Derivation labels as contracts.* The name draws from the seed chain
   `settlement → name`, the pick from `settlement → name-pick`, the
   population from `settlement → population`. These labels are permanent:
   changing any of them renames every village in every saved world.

**What it emits:** four facts per village (name, is-a-settlement, located-in,
population), all with settlement's provenance, all queryable by anything —
culture and religion take the village's entity id at genesis, and the almanac
and REPL read the rest back through ordinary queries.

**The tier ladder ahead:** settlements *placed* rather than given — sited by
field logic (water, shelter, food) once terrain has fields worth consulting
(Campaign 4), then multiple settlements with relationships, then settlement
histories as the fields-and-ledger model of deep time arrives.
