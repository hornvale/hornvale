# Hornvale: Long-Term Design & Roadmap

**Date:** 2026-07-05
**Status:** Approved (brainstorming session)
**Scope:** Multi-year vision, architecture, and campaign roadmap for Hornvale — an experimental text adventure and computational social science sandbox.

---

## 1. What Hornvale Is

Hornvale is a multiscalar simulation of invented worlds — astronomical, ecological,
social, mythological — observed through text. The text adventure is the eventual
primary *client* of the simulation, not the simulation itself: a situated, restricted,
sensory view onto a query surface that other tools (a REPL, a document generator,
batch experiment runners) share equally.

Hornvale is its own project. Sibling projects (goldengrove, snakewood, longtable)
are sources of inspiration and lessons, not dependencies. Hornvale shares no code
with them and is not obligated to their designs.

The soul of the project sits between "sim first, game as lens" and "research
instrument": an extremely rich modular core, developed over years, whose
interlocking systems provide services to one another — and whose most interesting
output is *emergent cultural meaning*. The motivating example: given a generated
solar system (suns, moons, wanderers, rings, seasons), what religions develop?
How do goblins on a Tatooine differ from goblins on a Hoth?

## 2. The Constitution

Five principles that settle future design questions. When in doubt, defer to these.

1. **Sim first, game as lens.** Anything the player can experience must be a query
   any tool could make. The game renders queries; it never owns state.

2. **Coarse constrains fine, never the reverse.** The single invariant governing
   space, time, and fidelity. A finer-grained answer (higher provider tier, closer
   zoom, later refinement) may add detail to a coarser answer but may never
   contradict it. This is what keeps seams closed — geometric, temporal, and
   semantic.

3. **Determinism is constitutional.** Seed + coordinates + committed facts → the
   same answer, forever, at every fidelity. This is simultaneously the
   infinite-world mechanism, the save format (a seed plus a ledger), the test
   strategy, and the computational-social-science story (controlled experiments:
   "same world, but the moon is tidally locked").

4. **Every domain is a provider behind a fidelity-agnostic interface.** Domains
   expose query vocabularies, not mechanisms. Multiple providers per domain, tiered
   by fidelity, answer the same questions with different richness. Downstream
   consumers receive *salience-ranked phenomena*, never implementation detail.
   The interfaces are the project's central intellectual artifact; each co-evolves
   with a book chapter.

5. **Nothing exists until it's observable.** Every system, from its first week,
   must be interrogable through the REPL and must contribute to the almanac.
   No abstraction ships without a window.

## 3. Architecture

Four layers, bottom to top.

### 3.1 The Kernel

Small, finished early, changes rarely.

- **Seed & noise substrate.** Hierarchical seeding (world seed → domain seeds →
  per-feature streams). Coherent-noise and hash primitives with three required
  properties: determinism, random access (evaluate any point without evaluating
  neighbors), and local coherence at every zoom (octaves). The one library
  everything shares.
- **Field infrastructure.** Typed, lazily-evaluated, cacheable fields over
  (space × time). Physical fields (elevation, temperature, rainfall) and social
  fields (population pressure, scarcity, migration flow, religious fervor,
  misery, trade intensity). Fields compose into derived fields. Fields are the
  "coarse" in coarse-constrains-fine.
- **The Fact Ledger.** Append-only store of committed observations: named
  entities, events, relationships. Once a goblin king has a name, he has it
  forever. Re-observation is consistent forever. Persistence = seed + ledger.
- **The Refinement Engine.** Constraint-satisfaction machinery that generates new
  detail consistent with both the fields and the accumulated ledger. Starts
  trivially (choose among options that contradict nothing); grows in
  sophistication for years. This is the project's recurring hard problem —
  the semantic seam — and it lives in the kernel because everything uses it.
- **Provider Registry.** How domains declare query interfaces, register tiered
  implementations, and resolve one another's queries at configured fidelity.

### 3.2 The Domains

One crate per pillar; providers as modules within. The enrichment cascade is a
DAG, generated and cached layer by layer:

> astronomy → climate → terrain/hydrology → ecology → subsistence → settlement
> → social structure → language → religion → historiography

Back-edges (e.g., observatories increase celestial salience; belief affecting
reality) are permitted but must be explicit, declared, and rare.

**Provider tiers, illustrated with astronomy** (names are descriptive
placeholders; real naming is deferred):

- Tier 0 `constant-sun`: the sun is always up (Zork).
- Tier 1: 24-hour day/night cycle with visibility effects.
- Tier 2: seasons, moon phases, derived calendar.
- Tier 3: realistic multi-body configurations, constellations as perceived from
  the surface, eclipses, wanderers, rings.

All tiers answer the same query vocabulary (e.g., "enumerate celestial phenomena
salient from this surface, with periodicities and visual character"). A trivial
provider's sparse answer is itself meaningful input downstream (what religion
develops under an eternal noon?).

### 3.3 Deep Time: Fields + Ledger

History is neither a Dwarf Fortress-style forward event log (causally accurate,
unfocalized, boring) nor pure Caves of Qud-style confabulation (charming,
unconstrained). It is both layers of the kernel applied to time:

- **The fields of history** (the prior): continuous field-like quantities over
  (space × time) — pressure, fervor, migration, misery — where coherent noise is
  legitimately the right tool. "The tides of empire in this region across three
  millennia" is answered instantly by reading the field, like reading a climate
  map. Zoomed out, history reads as weather.
- **The event ledger** (the posterior): discrete, causal, named history generated
  *lazily, on observation*, constrained by the field values there-and-then and by
  every previously committed fact. Noise supplies correlation; the ledger
  supplies causation, conservation laws, and identity.

**Historiography, not history.** Inhabitants (and, largely, the player) never
access the objective event layer — they access myths, chronicles, and oral
traditions *focalized* through the tellers' own field values. Every account has
a teller. Triangulating sources is the intended long-term gameplay of the
ethnographer loop.

### 3.4 The Windows

- **The REPL** — the daily driver. Interrogate any layer of a generated world:
  the sky at a place and time, a settlement's social graph, a culture's beliefs,
  and causal queries (`why <fact>`).
- **The Almanac** — generated world documents: star charts, calendars, maps,
  ethnographies, mythologies. The per-campaign showpiece and demo artifact.
  Eventually these same documents exist diegetically as in-world books.

### 3.5 The Game

The thin skin, deferred but never forgotten: a player character as a situated,
restricted, sensory view of the same query surface. Room descriptions are
focalized renderings of field + ledger queries. Every room must feel authored
(the anti-repetition mandate from the book's dungeon chapter). Arrives as a
campaign like any other, once there is a world worth walking in.

## 4. Roadmap

The planning unit is the **campaign**: a 6–10 week themed push ending in three
artifacts — a working increment, an almanac release, and a book chapter.
Cadence assumption: steady work with substantial AI leverage
(subagent-driven implementation between design sessions); roughly 4–5 campaigns
per year with fallow time expected.

Year one is specific; later years are deliberately vaguer, chosen by what the
world most needs once it can be seen.

### Year 1 — the world exists

- **Campaign 1: The Kernel & the First Almanac (~10 weeks).**
  Seed/noise substrate, field infrastructure, fact ledger v0, trivial refinement
  engine, provider registry, REPL — plus tier-0 providers for *every* domain
  (constant sun, uniform climate, one hand-placed biome, one goblin village with
  a stub caste structure, one stub myth).
  **Exit criterion:** `hornvale new --seed 42`, interrogate sky/village/belief in
  the REPL, and `hornvale almanac` emits a one-page world document.
- **Campaign 2: The Sky** (astronomy tiers 1–2). Day/night with visibility
  effects, seasons, moon phases, calendar derivation. Almanac gains a real
  calendar and star chart. First because it is the DAG root and the strongest
  existing intuition (clean-room; no goldengrove code).
- **Campaign 3: The Land** (climate/terrain tier 1). Field-driven elevation,
  temperature, rainfall, biomes; the region-graph chunking model (revived from
  the book's deleted chapters) as the spatial substrate. Almanac gains maps.
- **Campaign 4: The People** (settlement + social structure tier 1). Settlements
  placed by field logic; the goblin village becomes generated rather than
  hand-placed; castes and roles from social-pressure fields. First real test of
  cross-domain enrichment: settlement queries climate queries astronomy.
- **Campaign 5: The Gods** (religion tier 1 + historiography tier 0). Religion
  generation consuming celestial salience, seasonal structure, and social
  structure. First `why <belief>` queries.
  **Year-one exit criterion:** generate two worlds differing only in astronomy
  and get legibly different religions. This is the differentiating demo and the
  test of the enrichment thesis.

### Year 2 — the world has a past and speaks

Candidate campaigns (order chosen at the time): fields-of-history (tides of
empire readable at any zoom); event ledger + lazy retrospective confabulation
(`why is this village fortified?`); language tier 1 (naming, phoneme-level
speech rendering done right, myths rendered in register); ecology/subsistence
deepening; refinement-engine hardening.

### Year 3 — someone walks in it

The Game campaigns: player character as situated query surface; focalized room
description generation; the ethnographer loop as gameplay. Then MUD-ish
liveness (NPCs running daily behavior against the same sim), and beyond that,
multiple worlds — the "alternate universes" of the Zork paper on the wall.

### Structural rules

- **Any campaign can be aborted at a week boundary** without wrecking the whole;
  the tier-0 cascade always works.
- **No domain may lead another by more than ~2 tiers.** Depth stays balanced,
  which keeps the enrichment thesis honest.

## 5. Working Process

- **Book-driven development.** Each campaign opens by writing (or reviving) its
  book chapter — the why, the query vocabulary, the aesthetic constraints —
  before code. The book becomes the design instrument rather than a
  retrospective.
- **Division of labor.** Nathan owns: constitution, interfaces, aesthetics, book
  chapters, taste calls. Claude owns: implementation against agreed specs via
  subagent-driven development, test suites (property tests asserting provider
  contracts and determinism — same seed twice, byte-identical almanac),
  refactors, the mechanical middle of campaigns. Each campaign gets a spec and
  implementation plan in-repo; sessions resume from those.
- **Repo layout.** Cargo workspace in `hornvale/hornvale`:
  `kernel/` (seed, fields, ledger, refine, registry), `domains/` (one crate per
  domain, providers as modules), `windows/` (repl, almanac), later `game/`.
  The book repo gains one chapter per campaign.
- **Guard rails.** Every session ends green (compiles, tests pass). The REPL demo
  is the definition of progress. The almanac gives every campaign a shareable
  ending — the anti-burnout mechanism.

## 6. Testing Strategy

- **Determinism tests:** same seed → byte-identical almanac; ledger replay →
  identical world state.
- **Provider-contract property tests:** written once per domain interface, run
  against every tier ("the sun that rose sets"; answers at tier N+1 refine and
  never contradict tier N on shared queries).
- **Seam tests:** observe → leave → return; refinement must be consistent with
  prior observation. These grow with the refinement engine.
- **Cascade tests:** perturb an upstream domain (e.g., add a moon), assert
  downstream outputs change and remain internally consistent.

## 7. Explicitly Deferred

- **Naming** of providers, tiers, crates, and systems (descriptive placeholders
  throughout this document).
- **Scripting language** (the book leans Lua; irrelevant until the game layer).
- **Multiplayer/MUD networking** (single-player assumed for years; the query
  surface keeps the door open).
- **Any graphical client** (the almanac's rendered documents are the only
  visual output planned).

## 8. Known Risks

- **Refinement-time constraint satisfaction** is the recurring boss fight; it is
  scheduled to start trivial and be hardened in a dedicated Year-2 campaign
  rather than solved up front.
- **Interface design before consumers exist** is mitigated by the tier-0
  full-cascade requirement of Campaign 1: every interface has a consumer from
  the first campaign.
- **The tower without a window** (historical failure mode) is mitigated
  constitutionally: Principle 5, the REPL demo as the definition of progress,
  and campaigns that always end in a shareable artifact.
