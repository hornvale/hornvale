# Room-Scale Variety — Synthesis & Consolidation

**Status:** Consolidation of Cycles 01–03. This is the durable technical
artifact; the three cycle documents are the raw ideation behind it.
**Purpose:** reduce the sixty ideation mechanisms to the small set of
**computational primitives** they actually require, and specify each primitive
concretely — its data structure, its algorithm, the affordances worth borrowing
from other fields, and how it must obey Hornvale's constitution (determinism,
layering, the kernel trace protocol, std-only + serde-only, no `HashMap`, no
wall-clock, quantize-at-the-boundary).

Cycle sources: [01 — mechanisms](cycle-01-ideonomy.md),
[02 — biomes & palimpsest](cycle-02-biomes-and-palimpsest.md),
[03 — the living layer](cycle-03-the-living-layer.md).

---

## 1. The reduction: 60 mechanisms → 8 primitives + 2 cross-cuts

The central finding of the consolidation: the sixty mechanisms are **eight data
structures wearing sixty costumes.** Build the eight well and the sixty fall out
as configurations, not as sixty separate features.

```
  PRIMITIVE                              POWERS (cycle.idea)                                        STATUS
  ------------------------------------   --------------------------------------------------------   ------------
  P1  Composed field stack               1.1 1.2 1.11 1.12 3.2                                       extends Field
  P2  Deterministic subdivision (rooms)  1.9 2.6 (spatial half of 1.16)                              NEW (keystone)
  P3  Weighted cross-product generator   1.6 1.10 1.15 1.17 1.18 2.5 2.9 2.10 3.5 3.6 3.7 3.9 3.10   generalizes Namer
                                          3.11 3.14 3.15
  P4  Palimpsest state-machine           1.5 1.14 2.2 2.3 2.12 2.13 2.14 2.15 2.16 2.20              extends deep-time
  P5  Influence fields & halos           1.3 1.4 1.13 2.11 2.19 3.1 3.3 3.16                          NEW
  P6  Relational lattice / graph         1.16 1.19 3.12 3.13 3.17 3.18 3.19                           extends settlement
  P7  Scalar dials & rarity budget       1.4 1.20 2.1 2.7 2.8 3.4                                      ties to salience
  P8  Temporal phase modulation          1.7 1.8 2.4 2.16 2.18 3.20                                    extends astronomy
  ------------------------------------   --------------------------------------------------------   ------------
  X1  Affordance layer (verbs from stack)  1.12 + every "what can I do here"     cross-cut, derived from P1
  X2  Consequence / persistence contract   3.8 + every irreversible act          cross-cut, = the Fact ledger
```

Two speculative outliers (2.7 informational biomes, 2.17 living biomes, 2.18
wandering biomes, 3.11 the unalive wing) stretch P5/P8/ecology past their
comfortable range; they are flagged Low-confidence in §5 and parked.

**Why this matters:** every primitive below reduces, in Hornvale terms, to a
*producer of one of the three kernel trace types* — a **Field**, a **Fact**, or
a **Phenomenon**. That is the whole cross-domain wiring (§4). The sixty
mechanisms need almost no new inter-domain machinery; they need eight generators
and the composition root to wire them.

---

## 2. The eight primitives, in depth

Each primitive is specified as: **essence · data structure · algorithm ·
borrowed affordances (CS/math, then real-world domains) · Hornvale fit &
determinism.** Pseudocode is illustrative Rust, not a committed API.

### P1 — The Composed Field Stack  *(the master engine)*

**Essence.** A room's content is the ordered evaluation of a fixed *stack* of
independent layers, each a typed function of location and time. Variety is
`M^N` (N layers, M states each), never `M·N`. This is Cycle 01 #1 promoted to
architecture.

**Data structure.** Extends the kernel's existing **Field** protocol (typed
functions over space × time) with composition combinators. Fields are *never
serialized* — like everything in Hornvale they are reconstructed from the seed at
load, so there is no save-format surface here at all.

```rust
struct Locus { cell: CellId, sub: SubAddr /* P2 */, time: WorldTime }
trait Field<T> { fn sample(&self, at: Locus) -> T; }

// combinators — the "map algebra" of the room
Layered(Vec<Field<Cover>>)          // paint in order
Masked{ base: Field<T>, by: Field<bool> }
Blended{ a, b, weight: Field<f64> } // ecotone/gradient
Conditioned{ on: Field<K>, arms: BTreeMap<K, Field<T>> }
```

**Algorithm.** Pure functional sampling. Each leaf field derives its own
`Stream` from a **declared seed-label** and reads noise/positions at the locus.
No mutable global state; sampling the same locus twice is byte-identical by
construction.

**Borrowed affordances.**
- *CS/math:* shader compositing pipelines (layers, blend modes, masks); the
  decorator pattern; pure/lazy evaluation.
- *Domains:* **GIS raster stacks and map algebra** (the literal "atlas of
  overlays"); geological facies models; audio bus mixing.

**Hornvale fit & determinism.** Extends an existing kernel concept; the stack is
assembled by a new *locale* composition root (§4) from fields the domains
publish. Trivially deterministic (pure functions of seed + locus + time). The
ordered pipeline of Cycle 01 #11 *is* the fixed evaluation order of this stack.

---

### P2 — Deterministic Hierarchical Subdivision  *(the keystone: what a "room" is)*

**Essence.** Turn one 240 km geodesic cell into an addressable, lazily-generated
tree of rooms, to arbitrary depth, with byte-identical content per address. Cycle
01 #9. **Nothing at room scale exists without this** — it is the primitive the
whole project is gated on.

**Data structure.** A **hierarchical spatial address** below `Geosphere`:

```rust
struct SubAddr(Vec<u8>);      // path of child indices from the cell root
struct Locus { cell: CellId, sub: SubAddr, time: WorldTime }
// content(locus) is a pure function; only visited rooms are ever materialized.
```

Subdivision aperture: triangle→4 (icosphere-native) or aperture-7 hexagons
(H3-style). The address is a **base-N geocode**; depth d gives `~aperture^d`
rooms per cell (aperture-7, depth 6 ≈ 118k rooms/cell ≈ a room every ~700 m).

**Algorithm.** `room_seed(cell, sub) = kernel_hash(cell_seed, sub_bytes)` — each
room's seed is a *pure hash of its address*, so generation is stateless, lazy,
and reproducible. The field stack (P1) is sampled with that seed. The genuinely
hard part is **neighbor adjacency across cell seams** on an icosphere (12
pentagon cells, non-uniform child counts) — see §6, Open Questions.

**Borrowed affordances.**
- *CS/math:* **quadtree / octree** paths; **Morton (Z-order) and Hilbert
  curves** for locality-preserving linear addressing; wavelet multiresolution.
- *Domains:* **H3 / S2 / geohash** hierarchical geospatial indices (Uber,
  Google) — we cannot take the crates (serde-only), but the *addressing math* is
  the reference design.

**Hornvale fit & determinism.** NEW primitive, most naturally in `kernel` or
`terrain` beneath `Geosphere`. The addressing scheme becomes a **frozen
save-format contract** (like the seed labels): change it and every room in every
world moves. Deterministic by pure hashing; perf is pay-as-you-go because
addresses are only realized when visited.

---

### P3 — The Weighted Cross-Product Generator  *(grammar = matrix = periodic table)*

**Essence.** Combinatorial content from orthogonal, *consequential* axes, with
**rarity = the joint improbability of a coordinate**. One engine underlies the
biome matrix (2.5), the creature grammar (3.6), the encounter grammar (3.7), the
descriptor grammar (1.6), the fluid-substitution and cross-breeder engines (2.9,
2.10), and the *existing* name generator — which this generalizes.

**Data structure.**

```rust
struct Slot<V> {
    label: &'static str,                     // its own seed stream
    values: Vec<(Weight, V)>,                // weighted pool
    condition: Option<fn(&Context, &Draw) -> bool>, // depends on prior slots + fields
}
struct Grammar<Out> { slots: Vec<Slot<..>>, assemble: fn(Draw) -> Out }
```

**Algorithm.** For each slot in fixed order: derive `Stream(room_seed, label)`,
draw a **weighted choice** (cumulative weights + one stream `f64`), then filter
by `condition` against the `Context` (biome, strangeness rung, neighbors, prior
slots). Product of chosen weights gives the combo's probability, so exotic
coordinates (necrotic × crystal × cursed …) are astronomically rare *for free* —
no rarity table. Empty grid cells are Mendeleev-style **predictions** (2.5,
3.14, 3.15): a design-completeness audit, not just content.

**Borrowed affordances.**
- *CS/math:* **context-sensitive & generative grammars** (Chomsky), **L-systems**
  (plants!), Tracery-style expansion; **feature-structure unification** (for
  conditioning); marginal/joint probability.
- *Domains:* **the periodic table's predictive completeness** (chemistry);
  morphological typology (linguistics); combinatorial taxonomy (biology).

**Hornvale fit & determinism.** Best as a **kernel-level combinator over
`Stream`**, usable by every domain, so the existing `Namer` becomes one instance
of it. Per-slot **stream-consumption order is a save-format contract** (exactly
the pin-isolation discipline the astronomy/terrain generators already follow).

---

### P4 — The Palimpsest State-Machine  *(deep-time place history)*

**Essence.** A place is the **current state of a machine that has run since
genesis**; the present room = current state + the *readable residue of the whole
path*. Cycle 02's palimpsest core (2.12–2.16) plus path-dependence (2.14) — the
single highest-leverage anti-oatmeal win, because it mines variety from time the
deep-time sim *already computes*.

**Data structure.**

```rust
struct Transition { from: BiomeState, trigger: Event, to: BiomeState,
                    reversibility: Reversibility, stickiness: f64 }
struct Machine { states: Vec<BiomeState>, edges: Vec<Transition> } // absorbing = no out-edges
struct Epoch { state: BiomeState, entered: WorldTime, via: Event }
type History = Vec<Epoch>;          // the walked path, per cell
struct Trace { of: BiomeState, kind: TraceKind, legibility: f64 } // decays with age/overburden
```

**Algorithm.** Forward-walk the machine from genesis, transitions fired by the
existing tectonic/climate deep-time events. **Sticky** states = high entry
weight, low exit weight → long dwell (a curse falls in a day, recedes over
centuries). **Absorbing** states = salted earth, a dead god's wound. Residue =
fold `History` → `Vec<Trace>`, each past state depositing traces that decay and
compact under later overburden (**law of superposition**). Two cells with the
same present state but different `History` read differently — the path *is* the
content.

**Borrowed affordances.**
- *CS/math:* finite automata / Markov chains (here *deterministic*); **event
  sourcing / append-only log** — which is *literally Hornvale's ledger*.
- *Domains:* **stratigraphy & the law of superposition** (geology, the model
  proper); taphonomy & sedimentation (paleontology).

**Hornvale fit & determinism.** Extends the existing deep-time/paleo sim; the
walked history can be **recorded as Facts** (quantization is safe because reload
re-derives). **⚠ Lorenz guard-rail (constitutional):** the state walk must
re-derive from the full-precision seed — *never* resume a forward chain from
quantized ledger floats. Any chaotic sub-process needs its own full-precision
recomputation, not a checkpoint.

---

### P5 — Influence Fields & Halos  *(scattered-source potential fields)*

**Essence.** A source point radiates an influence with a **core→margin falloff**;
sources compose; the summed field locally decides biome, inhabitants, danger, and
where *frontiers* fall. Powers ranges (3.1), halos both biotic and abiotic (2.11,
3.16), radial purity (2.19), ecotones/frontiers (1.3, 3.3), hazard/sanctuary
(1.13), vistas (1.4).

**Data structure.** Sparse emitters, queried — never a dense per-room store:

```rust
struct Emitter { at: Locus, kind: EmitterKind, strength: f64, support: f64,
                 falloff: Falloff /* linear | gaussian | inverse-sq */ }
// InfluenceField::sample(locus) = Σ over emitters within support of strength·falloff(dist/support)
```

**Algorithm.** For a query locus, gather emitters within their support radius
(spatial neighbor-walk on the Geosphere / P2), sum weighted falloff. **Frontier**
= high gradient magnitude, or two fields comparable (Cycle 01 ecotones). **Core→
margin** = the falloff itself. Emitters are drawn from the **Fact ledger**
(features, settlements, curses), so the influence field is a pure function of
facts + locus.

**Borrowed affordances.**
- *CS/math:* **influence maps / potential fields** (game AI & robot navigation —
  the exact structure); **Worley / cellular noise** (nearest-emitter); **kernel
  density estimation** (statistics); **Voronoi / Delaunay** (territory borders).
- *Domains:* **electrostatic superposition** (physics — sum of point sources);
  **reaction–diffusion / Turing patterns** (chemistry/biology — blight spread,
  biome mottling); home-range estimation (ecology).

**Hornvale fit & determinism.** New but simple; pure over (emitter set, locus).
No serialization (emitters derive from facts). Deterministic given ordered facts.

---

### P6 — The Relational Lattice / Labeled Graph

**Essence.** Culture and allegiance are **partial orders with multiple
inheritance**, not trees; connectivity (rooms, the Underdark) is a **labeled
graph**. Meets and joins are where relationship stakes are generated. Powers
culture (3.17), factions (3.18), feuds (3.19), thralls/sources (3.12),
institutions (3.13), the room/Underdark graphs (1.16, 1.19).

**Data structure.** `BTreeMap`/`BTreeSet` throughout — the `HashMap` ban is not
optional:

```rust
// lattice: a DAG of group generality
parents: BTreeMap<GroupId, BTreeSet<GroupId>>
membership: BTreeMap<PersonId, BTreeSet<GroupId>>
// graph: labeled adjacency
edges: BTreeMap<RoomId, Vec<Edge>>   // Edge { to, kind: TraversalKind, cost }
```

**Algorithm.** `meet(a,b)` = greatest lower bound, `join(a,b)` = least upper
bound, via ancestor-set intersection over the DAG. `relationship(a,b)` = classify
by shared apex / incomparability / opposed branches, weighted by the feud–
alliance **direction field** (accumulate/decay over time from historical facts —
P8). Culture = the **join of a people's parents**: biome ⊕ history ⊕ neighbors ⊕
faith, so two peoples in one biome still diverge.

**Borrowed affordances.**
- *CS/math:* **lattice / order theory** (meet, join); **type lattices &
  subtyping** (PL); lowest-common-ancestor algorithms; social-network analysis.
- *Domains:* **kinship algebra** (anthropology — computing relatedness);
  **RDF / ontologies** — whose subject-predicate-object triple *is already the
  shape of a Hornvale Fact*.

**Hornvale fit & determinism.** Culture/faction extends the existing
settlement/religion/language generators as the lattice's *parents*;
relationships are Facts. Ordered `BTree` traversal is deterministic.

---

### P7 — Scalar Dials & the Rarity Budget

**Essence.** Global and local scalar fields (**strangeness**, **concealment/
visibility**) that gate rarity, generation depth, revelation, and prose register
— governed by a **budget** so the strange stays strange. Powers strangeness
(2.1), secrets & hidden things (1.20, 3.4), distribution (2.8), and the
speculative "field-as-terrain" biomes (2.7).

**Data structure.** `Field<f64>` for the dials, plus a per-world `Budget`
allocator (how much strangeness "mass" a world may spend and how spaced it is).

**Algorithm.** **Budgeted blue-noise placement.** To keep strange sites rare,
spaced, and conditioned on substrate (deep/remote/warded cells favored):
Poisson-disc-style dart-throwing over cells sorted by a seed-derived key, with a
running budget and a mutual-repulsion radius — all std-only (no crates). This is
the mechanism that makes rarity *emergent and spatial* rather than a per-tile dice
roll (the true fix for oatmeal at the "how often is anything special" level).

**Borrowed affordances.**
- *CS/math:* **Poisson-disc / blue-noise sampling** (even spacing); importance &
  rejection sampling (Monte Carlo); simulated annealing for placement.
- *Domains:* loot-tier / encounter-budget design (games); **the existing
  phenomenon *salience* ranking** in Hornvale — strangeness is its terrain-side
  cousin.

**Hornvale fit & determinism.** New global + field; hooks into existing salience.
Sampling is seeded, so placement is reproducible.

---

### P8 — Temporal Phase Modulation

**Essence.** Content = base ∘ modulator(time). Season, hour, moon-phase,
ecological succession, and social calendars re-render the *same* room. Powers
astronomical/seasonal phase (1.8), succession (1.7), ephemeral/periodic biomes
(2.4), magical succession (2.16), wandering biomes (2.18), the calendar of the
living (3.20).

**Data structure.** Pure phase functions + sparse timestamps:

```rust
fn phase(t: WorldTime) -> f64;                 // from the existing sky sim
last_disturbance: BTreeMap<CellId, WorldTime>; // succession clock input
calendars: Vec<Recurring { period, phase, kind }>;
```

**Algorithm.** Succession age = `now − last_disturbance` → index into a sere
sequence. Astronomical phase from the **existing** astronomy domain (moon phases,
season from tilt/orbit). Wandering position = `f(seed, t)` — a seeded drifting
*function*, **not an integrator** (Lorenz guard-rail again). All pure functions
of `WorldTime.day`.

**Borrowed affordances.**
- *CS/math:* phasor/oscillator & LFO composition (DSP / music synthesis —
  envelopes modulating a base signal).
- *Domains:* **ecological succession** (Clements/Gleason sere stages);
  **phenology** (timing of seasonal biological events); orbital mechanics (already
  in-domain).

**Hornvale fit & determinism.** Extends astronomy/time; succession needs
disturbance timestamps (new Facts). Pure over `WorldTime` → deterministic; no
wall-clock (constitutional).

---

## 3. Two cross-cutting concerns

- **X1 — the affordance layer (verbs from the stack).** A room's available verbs
  (forage, mine, ford, climb, track, banish, dismantle…) are *read from* the P1
  stack and the P6/ecology inhabitants, not authored. A new layer automatically
  grants its verb everywhere it appears. This is Cycle 01 #12 / Cycle 03 #11 as a
  derived index, owned by the locale window.
- **X2 — the consequence / persistence contract.** Every consequential act
  (a slain beast, a razed grove, a cured blight) is a **Fact** with a
  reversibility tag (Cycle 03 #8). Persistence *is* the append-only ledger
  Hornvale already has; irreversibility is the default and the source of weight.
  Reversible effects (regrowth, seasons) are re-derived, not stored.

---

## 4. Architecture: how it wires into the constitution

The elegant result of the reduction: **almost every mechanism is a producer of a
Field, a Fact, or a Phenomenon**, and the kernel trace protocol is the *only*
cross-domain channel — so we honor "a domain depends on the kernel and nothing
else" without strain.

```
  kernel        + P2 subdivision, + P3 Grammar<Stream> combinator, Field combinators
  domains/*     each publishes fields/facts/phenomena, depends ONLY on kernel:
     terrain    ── owns geology/landform strata (P4 states, P5 emitters)
     climate    ── owns biome fields (consumed by others AS fields, never by import)
     ecology    ── NEW: fauna/flora/trophic → inhabitants (reads biome via the
                   trace protocol; must not learn which domain produced it)
     society    ── NEW/extends settlement+religion+language: the P6 culture lattice
  windows/*
     worldgen   ── composition root: constructs providers, spends the P7 budget
     locale     ── NEW window: composes the P1 stack + X1 affordances into ROOMS,
                   built through worldgen (windows may depend on windows)
  cli/          ── re-exports; renders rooms
```

Key constitutional checks, each already a project habit:
- **Cross-domain only via facts/phenomena/fields** — ecology reading biome as a
  *field* (not a `climate` import) is exactly what the protocol is for; "consumers
  must never learn which system produced a phenomenon."
- **No new crates** — every algorithm above (Worley, Poisson-disc, LCA, hashing,
  grammar expansion) is hand-rolled std-only; that is a real constraint on P2/P5/P7
  and is already how the kernel's noise/seed live.
- **Save-format contracts** — P2's addressing scheme and every P3 grammar's
  per-slot stream order join the existing frozen set (seed labels, noise
  constants). Deliberate regeneration uses an epoch suffix, never a rename.
- **Determinism** — full-precision compute, quantize only at the emit boundary;
  the P4 and P8 warnings above are the load-bearing subtlety.

---

## 5. Build order & the confidence/value registry

**Keystone spine (build first, in order).** Everything else composes on these:

```
  P2 subdivision  ─┐
  P1 field stack  ─┼─►  P3 grammar  ─►  P8 phase  ─►  P5 halos  ─►  P4 palimpsest  ─►  P6 lattice / P7 budget
                   │        │
          (the room exists) (content is combinatorial)
```

**The ten highest-leverage mechanisms** (high value × high confidence × on the
spine) — the near-term development fuel:

```
  1.9  subdivision / fractal zoom      P2   the room itself
  1.1  stratum stack                   P1   the multiplicative engine
  1.11 the room pipeline               P1   fixed composition order
  2.5  biome generator matrix          P3   biomes as cross-product cells
  1.6  descriptor grammar              P3   anti-oatmeal at the prose level
  1.18 lexicon naming of geography     P3   cheapest, highest perceived-value move
  1.8  astronomical phase              P8   extends existing sky sim; nearly free
  2.11 halo biomes                     P5   strangeness clusters legibly
  2.14 palimpsest path-dependence      P4   biggest variety-from-existing-sim win
  3.7  encounter grammar               P3   a hundred scenes from three creatures
```

**Registry (all 60), grouped by primitive.** V = anti-oatmeal value, C =
confidence it fits determinism/std-only/perf. H/M/L.

```
  P1 COMPOSED FIELD STACK
    1.1  stratum stack ................ V:H C:H   foundational
    1.11 the room pipeline ............ V:H C:H   = the stack's eval order
    1.2  perspective lenses ........... V:M C:H   read-side re-projection
    1.12 generated affordances (X1) ... V:H C:M   verbs read from the stack
    3.2  placed-not-scattered ......... V:H C:H   biome→life lawful mapping (also P3)

  P2 DETERMINISTIC SUBDIVISION
    1.9  fractal zoom / rooms ......... V:H C:M   KEYSTONE; adjacency is the risk (§6)
    2.6  isolation → endemism ......... V:M C:M   divergent sub-seed (also P3)

  P3 CROSS-PRODUCT GENERATOR
    2.5  biome matrix ................. V:H C:H   rarity = coordinate improbability
    1.6  descriptor grammar ........... V:H C:H   prose-level variety
    1.18 lexicon naming ............... V:H C:H   extends Namer; cheap & huge
    3.6  creature grammar ............. V:H C:H   consequential slots
    3.7  encounter grammar ............ V:H C:H   frame varies, not roster
    3.5  modular individuals .......... V:M C:H
    2.9  fluid substitution ........... V:M C:H   exotic aquatic-shaped biomes
    2.10 biome cross-breeder .......... V:M C:H   hybrids at field overlaps (also P5)
    3.9  singular vs swarm ............ V:M C:H   cardinality flip
    3.10 inhabitant taxonomy .......... V:M C:H
    3.14 encounter periodic table ..... V:H C:H   design-completeness audit tool
    3.15 biome→life periodic law ...... V:H C:H   systematic coverage vs sprinkle
    1.10 field-intersection hybrids ... V:M C:H   (also P5)
    1.15 resource fields .............. V:M C:H   seeds the economy (also P5)
    1.17 subterranean biome tree ...... V:M C:H

  P4 PALIMPSEST STATE-MACHINE
    2.14 path-dependence .............. V:H C:H   the jackpot
    2.12 place state-machine .......... V:H C:M   ⚠ Lorenz guard-rail
    2.13 readable strata .............. V:H C:H   archaeology-as-play
    2.15 sticky / absorbing states .... V:H C:H   why curses persist
    1.5  palimpsest rooms ............. V:H C:H   the read surface
    2.2  negative wing ................ V:M C:M   curse/blight/undeath as states
    2.3  engineered + maintainer ...... V:M C:M   decay on maintainer death
    2.16 magical succession ........... V:M C:M   (also P8)
    2.20 reversion ladder ............. V:M C:H   one grammar, reused
    1.14 legible slow processes ....... V:M C:M   (also P8)

  P5 INFLUENCE FIELDS & HALOS
    2.11 halo biomes .................. V:H C:H   features radiate biomes
    3.16 life-halos ................... V:H C:H   features radiate a cast
    3.1  range / territory fields ..... V:H C:H
    3.3  frontier zones ............... V:H C:H   richest encounter ground
    1.13 hazard / sanctuary field ..... V:H C:H   survival loop
    2.19 radial purity gradient ....... V:M C:H   intra-patch arc
    1.3  ecotone rooms ................ V:M C:H   field-boundary room-type
    1.4  vista rooms .................. V:M C:M   see-not-reach (also P7)

  P6 RELATIONAL LATTICE / GRAPH
    3.17 culture as multiple inherit .. V:H C:M   organizes existing generators
    3.18 allegiance lattice ........... V:H C:M   meets/joins → stakes
    3.19 feud / alliance dynamics ..... V:H C:M   palimpsest-seeded (also P8)
    1.16 Underdark 3D graph ........... V:H C:M   labeled traversal edges (also P2)
    1.19 trophic webs ................. V:M C:M   ecology; encounter state
    3.12 thralls / puppeteers ......... V:M C:M   source graph = quests
    3.13 institutions as inhabitants .. V:M C:M   social process, revisitable

  P7 SCALAR DIALS & RARITY BUDGET
    2.1  strangeness axis ............. V:H C:M   global rarity dial
    1.20 secret / hidden rooms ........ V:H C:H   discovery joy
    3.4  hidden inhabitants ........... V:H C:H   the empty-looking room
    2.8  distribution patterns ........ V:M C:H   mega/patch/mosaic
    2.7  informational biomes ......... V:M C:L   SPECULATIVE; parked

  P8 TEMPORAL PHASE MODULATION
    1.8  astronomical phase ........... V:H C:H   extends sky sim; near-free
    1.7  successional clock ........... V:H C:H   time-since-disturbance
    3.20 calendar of the living ....... V:H C:M   world does things on a schedule
    2.4  ephemeral / periodic biomes .. V:M C:H   moon-gated, seasonal
    2.18 wandering biomes ............. V:M C:L   f(seed,t) only; SPECULATIVE
    2.17 living biomes ................ V:M C:L   quasi-organism; SPECULATIVE
    3.11 negate-living-NPC wing ....... V:M C:M   needs bespoke verbs (X1)
    3.8  persistence / reversibility .. V:H C:H   = the ledger (X2)
```

---

## 6. Open questions & risks

1. **Subdivision adjacency (P2) is the hard problem.** Addressing *within* a cell
   is easy; stitching neighbor rooms *across* icosphere cell seams — with 12
   pentagons and non-uniform child counts — is the real design work and gates
   everything. Prototype this first; it may argue for a specific aperture choice.
2. **Palimpsest performance (P4).** A per-cell history walk over deep time × 10,242
   cells × sub-tiles must stay inside the drift-checked artifact budget. Likely
   answer: walk at cell granularity, derive sub-tile residue on demand from the
   cell's history + local noise.
3. **The strangeness budget (P7)** is under-specified: how much, spaced how,
   conditioned on what. It deserves its own cycle — it is the global anti-oatmeal
   governor and the easiest thing to get wrong (too generous → strangeness
   becomes wallpaper; too stingy → the exotic never appears).
4. **Lattice & graph serialization (P6).** These are the first structures here
   heavy enough to want persisting; confirm they re-derive from seed + facts
   (preferred) rather than needing a new save surface, and keep them `BTree`-based.
5. **Speculative outliers** (2.7, 2.17, 2.18, 3.11 partial): high imaginative
   value, uncertain determinism/perf fit. Parked at Low confidence; revisit only
   after the spine exists.

## 7. What to do with this

This synthesis is the hand-off surface. The recommended next step is **not**
another ideation cycle but to graduate the keystone spine (§5) into the frontier
idea-registry with these confidence scores, then spec **P2 (subdivision)** first,
since it gates the rest. The three cycle documents can now be treated as
appendices — the reasoning is captured here.
