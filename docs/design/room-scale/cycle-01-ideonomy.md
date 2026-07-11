# Room-Scale Variety — Cycle 01 (Ideonomy Pass)

**Status:** Ideation. Not yet spec, not yet plan. Feeds future campaigns.
**Method:** Grace Kind / Gunkel *ideonomy*, four randomized method tuples (all
eight operators exercised). Each of the four "movements" below is the organon
one tuple produced; the ideas inside it are that tuple's operators applied to
the dimensions it drew.
**Scope assumption:** worlds are ~Earth-sized (see the scale analysis:
10,242 level-5 geodesic cells, each ~240 km across on an Earth radius). A
"room" is anything from a small enclosed space to a small patch of wilderness
meaningfully distinct from its neighbors — i.e. a *lazily-subdivided sub-tile*
of one 240 km cell. Assume a large Underdark megaregion, many new biomes, and
standard MUD/interactive-fiction furniture are all on the table.

---

## The thesis: multiply, don't enumerate

The failure mode we are designing against is the **"1,000 bowls of oatmeal"**
problem (Compton): a generator can emit a million technically-distinct rooms
that are *perceptually* the same bowl. The defense is not more content — it is
**orthogonal, multiplicative variety that a human can feel and act on**. Three
tests every idea below must pass:

1. **Perceptual** — does the difference register to a person in the room?
2. **Generative** — does it multiply against the other axes, or merely add?
3. **Consequential** — does the difference change what you *do*, not just what
   you read?

The organizing move, stated once here and reused everywhere: a room is a
**stack of independent layers**, each sampled from its own deterministic field,
then composed. If we have `N` layers with ~`M` distinguishable states each,
the room space is `M^N`, not `M·N`. Every idea below is either a new layer, a
new way to compose layers, a new way to *read* the stack, or a new axis
(time, depth, scale, visibility) along which the same stack re-renders.

---

## Movement I — Atlas of a Room

*(substitution + negation, rendered as an atlas; dimensions: modularity,
cardinality, discovery-vs-invention)*

An atlas views one territory through many maps. A room, likewise, is one
substrate seen through many perspectives — and several of the most valuable
room-types come from **negating** a property we assume every room has.

**1. The stratum stack (the core engine).**
A room is a deterministic composition of independent layers, each from its own
field: *bedrock geology → landform → hydrology → biome cover → disturbance/age
→ biota → marks-of-passage → atmosphere → mythic salience*. Each layer is
generated and stored independently; the room is their overlay. This is the
multiplicative engine — adding one new layer multiplies the whole catalogue
rather than adding to it. (Modularity dimension, taken to *fully-modular*:
every layer has a clean interface so new layers slot in without touching old
ones — the constitutional "adding a domain never edits an existing one," at
room scale.)

**2. Perspective lenses.**
The *same* substrate renders differently through a held lens: a **naturalist's**
lens surfaces species and ecology; a **surveyor's** lens surfaces elevation,
ore, and grade; a **historian's** lens surfaces what happened at this cell
across epochs; a **mystic's** lens surfaces phenomena and which deity finds
this place salient; a **cartographer's** lens surfaces names and routes. One
sim, many projections — enormous perceived variety from re-reading, not
re-generating. (The atlas organon, literally.)

**3. Ecotone rooms (negate *boundedness*).**
Most MUD rooms are discrete boxes. Negate that: the transitional edge between
two biomes is its *own* room-type with hybrid content and an unstable,
procedural boundary — the fen-edge where reeds give to open water, the
treeline where forest frays into fell-field. Ecotones are where life
concentrates; they should be where *rooms* concentrate interest.

**4. Vista rooms (negate *traversability*).**
A room whose value is perceptual, not locomotive: from here you *see* a
feature you cannot reach from here — a smoking peak, an aurora, a distant
walled city, a canyon floor a vertical mile down. The visible-but-unreachable
manufactures desire and direction; it turns the flat adjacency graph into a
landscape with a horizon. (Cardinality: these are *few*, deliberately — scarcity
is the point.)

**5. Palimpsest rooms (negate *space → time*; discovery-vs-invention).**
Hornvale already generates a paleo/deep-time record. Expose it: a room can be
*read* for what it used to be — a raised beach 80 m above today's sea, a
glacial scour, a drowned forest, an old battlefield. The room is a **discovered
archive**, not invented decoration. This is the purest expression of "sim
first": the history is already latent in the ledger; we are only surfacing it.

---

## Movement II — The Room Across Time

*(combination + abstraction-lift, rendered as a timeline; dimensions:
homogeneity, naturalness, hierarchicalness)*

Lift room-enrichment to its structural shape — *a field sampled at a location,
composed, then narrativized* — and the timeline reveals that the single richest,
cheapest variety axis is one we get almost for free: **the same room, at a
different time.**

**6. The combinatorial descriptor grammar.**
Prose is generated by a **cross-product grammar**, not templates. Orthogonal
slots — *substance × state × process-verb × sensory-channel × intensity ×
register* — each draw from field-conditioned pools. "The [gypsum] [crusts]
[flake] under a [chalk-pale] light, and the air tastes of [old salt]." Because
the slots are independent, `k` slots with `p` options each yield `p^k`
sentences, and the field conditioning keeps them *true* to the room. This is
the anti-oatmeal defense at the level of language itself. (Combination
operator, applied to text.)

**7. Successional clock.**
A room carries *time-since-last-disturbance* (fire, flood, landslide, blowdown —
all sim-derivable). The same cell reads as burn-scar → fireweed → pioneer
shrub → young stand → old growth depending on when you visit and its
disturbance history. Determinism holds (time is `WorldTime.day`). One location,
a whole trajectory of faces. (Naturalness × the timeline: succession is nature's
own animation.)

**8. Astronomical phase.**
Hornvale has real astronomy — axial tilt, orbit, multiple moons. Room content is
phase-modulated by season and hour: the marsh is frozen / flooded / cracked-dry;
fauna flips diurnal ↔ nocturnal; illumination is the summed light of N moons at
their real phases. The sky sim exists; route it into room rendering. The same
room has a summer-noon face and a three-moon-midnight face. (Timeline as the
astronomical cycle.)

**9. Fractal zoom (hierarchicalness → make it an axis).**
"Enter" is a movement verb. A forest *room* contains a glade contains a fallen
log contains a beetle-gallery. Scale-level is orthogonal to compass direction;
each level is generated on demand from the parent's stack plus a finer field.
This is the multiscalar promise made local, and it turns one cell into an
arbitrarily deep well of rooms without hand-authoring any level.

**10. Emergent hybrid biomes at field intersections.**
Apply the combination operator to the biome set × the climate field: where two
biome-defining fields overlap in a rare configuration, *synthesize* a hybrid —
a cryo-swamp where cold meets waterlogging, a cloud-desert where fog meets
aridity. **Rarity emerges from the math** (rare field-intersections are rare
places), not from a hand-tuned rarity table. Exotic biomes become a *product*
of the world, discovered where the numbers cross.

---

## Movement III — The Generation Procedure

*(organon-construction + dimension-identification, rendered as a procedure;
dimensions: polarity, scope, rate)*

A procedure is an ordered pipeline where order is load-bearing. Writing the
enrichment engine *as* a procedure surfaces the dependencies (inhabitants
depend on features depend on substrate) and shows where each stage can be
extended independently.

**11. The room pipeline (the architecture).**
Formalize enrichment as a deterministic, ordered pipeline: (1) sample substrate
fields → (2) classify biome → (3) roll features conditioned on substrate → (4)
populate biota conditioned on biome+features → (5) apply disturbance/history →
(6) apply phase (season/hour) → (7) narrativize via the grammar. Order matters
(step 4 needs step 3); each stage is a clean extension point. This is the spine
every other idea plugs into — and it keeps the whole thing seed-deterministic
because every stage draws from the kernel's streams.

**12. Generated affordances (verbs from the stack).**
A room *knows* what you can do here by reading its layers: forage (biome →
edible species), mine (substrate → ore), ford/swim (hydrology), shelter
(features), climb (relief), track (fauna), harvest (flora). Affordances are
**derived, not authored** — a new layer automatically grants new verbs
everywhere it appears. (This is the standard MUD "interactable" surface, made
generative.)

**13. The hazard/sanctuary field (polarity, made mechanical).**
Every room carries a *danger* value and a *shelter* value from exposure,
predators, terrain, and weather. An exposed ridge in a storm and a dry cave a
hundred meters away are consequentially different places. Polarity — the
positive/negative valence of a room — becomes a driver of the survival loop and
of route choice, not flavor text.

**14. Legible slow processes (rate-substitution: geological → readable).**
Features that encode slow processes and let the player *read and exploit* them:
a cave actively dissolving (karst), a delta advancing over a drowned field, a
migrating dune burying a road, a retreating glacier exposing new ground. Take
the "geological" rate and make its *traces* legible at human scale. Ties
directly to the deep-time sim and rewards the observant.

**15. Resource & trade-good fields (scope; seeds the economy).**
MUDs need economies. Generate deposits — ores, herbs, game, timber, salt, dyes,
exotic reagents — as fields conditioned on geology + biome, with scope from
*local* (one herb patch) to *regional* (a mining belt, a spice coast).
Combinatorial along *material × quality × accessibility*. Rarity and location
fall out of the sim, seeding trade routes, crafting trees, and "go fetch the
rare thing from the dangerous place" quests without authoring any of them.

---

## Movement IV — The Underdark Graph

*(tree-finding + cross-domain re-instantiation, rendered as a graph;
dimensions: direction, source, visibility)*

A graph is nodes and labeled edges. The surface mesh is one graph; the great
subterranean idea is that beneath it lies **another, differently-shaped graph**
— and several enrichment mechanisms come from re-instantiating structures
Hornvale already has (its language, its ecology) into new domains.

**16. The Underdark as a 3D connectivity graph.**
The subterranean megaregion is not a biome painted on the surface — it is a
**separate graph with its own topology**: cave networks generated by karst,
lava-tube, and fault processes; vertical shafts, chimneys, and sumps linking
depth strata. Edges are *labeled traversal types* — squeeze, climb, wade, swim,
chasm-leap, rope-descent — which is exactly the richer-exits MUD feature,
generated. A whole second world under every surface cell, reachable through cave
mouths, sinkholes, and mines. (Direction dimension: the Underdark's defining
axis is *down*, and depth is anisotropic like time.)

**17. The depth-stratified subterranean biome tree.**
Build the underground biome taxonomy as an explicit tree keyed by depth and
chemistry (see the bestiary appendix): *shallow* (bat caverns, cave pools, ice
caves) → *mid* (fungal forests, crystal galleries, glowworm grottoes) → *deep*
(chemosynthetic lakes, sulfur-vent gardens, sunless seas) → *abyssal-exotic*
(living-rock reefs, resonant voids, the deep melt). **Rarity and exoticness
scale with depth and inaccessibility** — the deepest biomes are the rarest and
strangest precisely because they are hardest to reach. This is where the "many
new, differently-rare biomes" requirement is satisfied wholesale.

**18. The lexicon engine names the world (cross-domain re-instantiation).**
Hornvale already generates phonologies and names settlements and deities.
**Re-instantiate that engine onto geography**: every notable feature, cave,
peak, river, and biome patch earns a culturally-plausible name in the local
language, with a gloss ("Kelmruk — 'the salt-throat', a cave that exhales brine").
Naming is the single highest-leverage anti-oatmeal move known: a *named* place
is a remembered place. The generator exists; we only point it at the map.
(Source dimension: content sourced from the language domain, not from a
geography table.)

**19. Trophic webs → emergent encounters (cross-domain: ecology).**
Re-instantiate a **food web** at room scale: fauna is generated as a
predator–prey–forage graph conditioned on biome, and encounters *emerge from
the graph's state* — you meet wolves *because* there is a deer herd *because*
there is meadow. Populations accumulate, decay, and oscillate over the
timeline (direction dimension → a coarse, deterministic Lotka–Volterra). The
world's life is a consequence of its own structure, so it is never static and
never the same herd twice.

**20. Visibility regimes and secret rooms (visibility; a core MUD joy).**
A *visibility* field gives every room a discovery difficulty: **obvious →
concealed** (rewards searching) **→ secret** (needs a key, tool, or knowledge)
**→ mythic** (opens only under a condition — a moon-phase, a ritual, a
phenomenon). Re-instantiate information-visibility (public / latent / hidden)
as *spatial* secrecy: hidden groves, buried vaults, sealed tombs, a door in the
cliff that is only a door when both moons are full. Secrets are generated
everywhere but *earned* — the joy of discovery, at planetary scale.

---

## How the twenty multiply

The point is not twenty features in a list — it is that they are **orthogonal
axes that compose**. One room is:

```
  stratum stack (#1)
    x perspective lens (#2)
    x successional age (#7)
    x astronomical phase (#8)
    x zoom level (#9)
    x hazard/sanctuary state (#13)
    x visibility regime (#20)
    ... rendered through the descriptor grammar (#6),
        named by the lexicon engine (#18),
        populated by the trophic web (#19),
        and, if underground, placed in the Underdark graph (#16/#17).
```

Seven independent state axes at ~5 distinguishable values each is already
`5^7 ≈ 78,000` felt variations of a *single cell's* content — before biome,
features, or resources. That is the shape of an answer to the oatmeal problem:
not a bigger pot, but more orthogonal spices.

---

## Appendix A — Biome bestiary (suggestions)

Current set: 22 (14 marine, 8 terrestrial). Suggested additions, tiered by
**rarity/exoticness** (common → legendary). These are candidates, not
commitments; several fall out naturally from #10 (field-intersection hybrids).

```
SURFACE — common/uncommon
  Mangrove         Saltmarsh        Peat bog / mire   Fen
  Moor / heath     Chaparral        Steppe            Cold desert
  Loess plain      Floodplain       Riparian gallery  Delta / estuary
  Tidal flat       Seagrass meadow  Montane meadow    Cloud forest

SURFACE — rare/exotic
  Paramo           Fell-field       Polar desert      Sand sea (erg)
  Gibber/reg desert  Badlands       Karst tower field Salt flat / playa
  Volcanic ashland   Lava field     Geyser basin      Mud-volcano field
  Petrified forest   Serpentine barrens  Blue-ice field   Nunatak fields

SURFACE — legendary (single-digit cells per world)
  Tepui sky-island   Bioluminescent bay   Thermal-vent oasis
  Alkali "soda" lake shore   Guano cliff colony

SUBTERRANEAN — shallow  (the Underdark, #16/#17)
  Bat cavern     Cave pool     Ice cave     Sea cave     Root cellar

SUBTERRANEAN — mid
  Fungal forest    Myconid grove   Crystal gallery   Glowworm grotto
  Guano-mound biome   Blindfish stream   Sump lake

SUBTERRANEAN — deep
  Chemosynthetic lake   Sulfur-vent garden   Magma-warmed hall
  Sunless sea   Nitre halls   Black-mold reach

SUBTERRANEAN — abyssal-exotic (rarest, strangest)
  Living-rock reef   Resonant void   Basalt cathedral
  Fossil-light strata (bioluminescent mineral)   The Deep Melt
```

## Appendix B — Standard MUD / IF furniture to generate (not author)

Everything here should be *derived from the stack*, so it appears everywhere the
relevant layers do — the opposite of hand-placement.

```
  Rich exits           labeled traversal edges (#16): squeeze, climb, ford, swim, chasm
  Affordances/verbs    forage, mine, track, harvest, shelter, fish (#12)
  Resources & economy  deposit fields; trade goods; crafting inputs (#15)
  Hazard & shelter     danger/shelter fields; weather exposure (#13)
  Light & darkness     astronomical light (#8); underground = dark by default (#16)
  Fauna & encounters   trophic-web spawns (#19)
  Secret/hidden rooms  visibility regimes (#20)
  Landmarks & vistas   see-but-not-reach features (#4)
  Place names & lore   lexicon engine on geography (#18)
  Weather & season     phase modulation (#8)
  Travel time/encumbrance   terrain-grade + distance cost across the sub-grid
  Cartography          the surveyor/cartographer lens (#2) as an in-world map
  Foraging/crafting    biome edible/material tables from the biota layer
  Camps/shelter        the shelter field (#13) as a build-a-camp affordance
  Quest hooks          "rare resource in dangerous place" falls out of #15 + #13
```

---

## Next cycles

This is Cycle 01. Subsequent cycles should re-run the ideonomy picker (fresh
tuples → operators we under-used here get their turn), drill each movement into
implementable mechanisms, and begin sorting ideas into the frontier
idea-registry with confidence scores before any of them graduate to a spec.
Candidate cycle themes: **the sub-tile subdivision scheme** (how a 240 km cell
lazily explodes into rooms deterministically), **the descriptor-grammar
formalism**, and **the Underdark generation algorithm**.
```
