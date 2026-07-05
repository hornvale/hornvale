# The Four Layers

Hornvale is organized as four layers, from substrate to surface:

**The Kernel** — small, finished early, changes rarely. It provides the six
capabilities every other part of the system depends on: hierarchical seeding,
coherent noise, fields, the fact ledger, refinement, and the trace protocol's
shared vocabularies. The kernel is the *only* thing a domain is allowed to
depend on. As of Campaign 1a, the kernel exists in full (at tier-0 depth) and
is the only layer that does.

**The Domains** — one module per pillar of the vision: astronomy, climate,
terrain, ecology, subsistence, settlement, social structure, language,
religion, historiography. They form an enrichment cascade — a directed
acyclic graph where settlement reads climate, which reads astronomy — but
they communicate exclusively through kernel vocabularies, never directly.
Each domain will exist at multiple fidelity tiers over the years.

**The Windows** — how humans see in. The REPL (interrogate any layer of a
generated world), and the Almanac (generated world documents: calendars,
maps, ethnographies, mythologies — the demonstration artifact each campaign
must produce).

**The Game** — the eventual thin skin: a player character as a situated,
restricted, sensory view of the same query surface every other window uses.
Room descriptions will be focalized renderings of the same queries the REPL
answers plainly. Deferred by design until there is a world worth walking in.

## A world, concretely

A *world* in Hornvale is exactly two things: a **seed** (one 64-bit number
from which all generation flows) and a **ledger** (the append-only record of
everything that has been observed and therefore committed as fact). Everything
else — terrain, weather, the disposition of a goblin village — is *derived*,
on demand, deterministically, from those two. Saving a world means writing
down the seed and the ledger. Loading it means trusting the mathematics to
rebuild everything else identically. Campaign 1a's integration suite proves
this holds: two runs from the same seed serialize to byte-identical worlds.
