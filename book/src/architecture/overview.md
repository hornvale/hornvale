# The Four Layers

Hornvale is organized as four layers, from substrate to surface:

**The Kernel** — small, finished early, changes rarely. It provides the seven
capabilities every other part of the system depends on: hierarchical seeding,
coherent noise, fields, the fact ledger, refinement, the trace protocol's
shared vocabularies, and the geosphere (a spherical region graph). The kernel
is the *only* thing a domain is allowed to depend on. Built in Campaign 1a;
gained enumeration interfaces in 1b; the geosphere arrived in Campaign 3;
otherwise already stable.

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

## Adding a domain

A domain joins the world through one small trait it implements and one line
in the composition root's roster. The trait names the declarative surface
every domain shares: the concepts and predicates it registers into the shared
vocabulary, and — for a domain that draws on the seed — the permanent labels
of its random streams. Its crate name, the key under which its streams appear
in the manifest, is read from the crate itself, so it cannot drift from the
crate it names. Registration and the stream manifest both read the one
roster, so they can never disagree about which domains exist.

What stays out of the trait is as deliberate as what goes in. *Genesis* — how
a domain is actually generated and rebuilt from a seed — is not a trait
member, because its inputs differ from every other domain's and wiring them
together is exactly the composition root's job. Adding a domain therefore
never requires editing another domain; it requires only the domain's own code
and that single line where all domains already meet. The roster is stored in
registration order, because one domain (language) references concepts other
domains own and must register after them; the manifest sorts the same list
alphabetically when it renders.
