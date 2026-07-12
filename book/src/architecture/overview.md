# The Four Layers

Hornvale is organized as four layers, from substrate to surface:

**The Kernel** — small, finished early, changes rarely. It provides the
capabilities every other part of the system depends on: hierarchical seeding,
coherent noise, fields, the fact ledger, refinement, the trace protocol's
shared vocabularies, the geosphere (a spherical region graph), and — below the
globe — the room substrate: an addressable, lazily generated finer mesh that is
the *same* icosphere refined deeper, for local detail at zero global cost. The
kernel is the *only* thing a domain is allowed to depend on. Built in Campaign
1a; gained enumeration interfaces in 1b; the geosphere arrived in Campaign 3;
the room substrate in [The Room Mesh](../chronicle/the-room-mesh.md); otherwise
already stable.

**The Domains** — one module per pillar of the vision: astronomy, climate,
terrain, ecology, subsistence, settlement, social structure, language,
religion, historiography. They form an enrichment cascade — a directed
acyclic graph where settlement reads climate, which reads astronomy — but
they communicate exclusively through kernel vocabularies, never directly.
Each domain will exist at multiple fidelity tiers over the years. As of
Campaign 1b, six domains exist at tier 0 — see
[The Cascade at Tier 0](../domains/overview.md).

**The Windows** — how humans see in. The REPL (interrogate any layer of a
generated world), and the Almanac (generated world documents: calendars,
maps, ethnographies, mythologies — the demonstration artifact each campaign
must produce), both as of Campaign 1b. Campaign L0 added two more: a
composition-root library (`worldgen`, where all domains are wired into a
world — extracted so both the CLI and other windows build worlds the same
way) and the **Laboratory** (`lab`), a batch harness that sweeps thousands
of seeds and publishes measured distributions as drift-checked artifacts.
A window may depend on a domain, or on another window — never the reverse.

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
