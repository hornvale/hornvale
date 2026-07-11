# Room-Scale Variety — Cycle 02 (Biomes & the Palimpsest)

**Status:** Ideation. Not yet spec, not yet plan. Feeds future campaigns.
**Method:** ideonomy, five fresh method tuples (organons: scale, list, matrix,
dictionary, state-machine). This cycle is deliberately biome-heavy per the
brief: *strangeness at every level, aquatic not forgotten, artificial /
engineered / magical / faerie / cursed / undead / wasteland / desert*, and a
deeper look at the **palimpsest** idea from Cycle 01 (idea #5 there).
**Builds on:** [Cycle 01](cycle-01-ideonomy.md) — the stratum stack (#1),
succession (#7), field-intersection hybrids (#10), and the depth-stratified
Underdark tree (#17). Biome here is one *layer* of that stack; this cycle
explodes it.

---

## The spine: a biome is a set of defaults, and strangeness is negation

Lift "biome" to its structural shape and it is: *a self-consistent regime of
`(energy source × substrate × dominant life × governing law)`, tiled over space
and persisting in time.* Every dull, interchangeable "bowl of oatmeal" biome
silently shares **nine defaults**. Strangeness is what you get by **negating
one default at a time** — and because the defaults are independent, you can
negate several at once and climb into genuinely alien territory. This single
table is the generating engine for the whole cycle:

```
  DEFAULT (what an ordinary biome assumes)     NEGATE IT ->  BIOME CLASS UNLOCKED
  ------------------------------------------   -----------   --------------------------------
  D1 powered by sunlight                       ->  chemo / geothermal / radiotrophic / necrotic / aetheric
  D2 built on rock & soil                      ->  fluid-substrate seas (sand, fog, tar, glass, ash)
  D3 dominated by plants, then animals         ->  fungal / crystalline / microbial / bone kingdoms
  D4 obeys ordinary, stable physical law       ->  faerie realms, non-Euclidean glades, dream-logic moors
  D5 in rough equilibrium (stable over a life) ->  catastrophe scars, blooms, mid-succession relicts
  D6 spatially fixed                           ->  wandering blooms, drifting fog-forests, roving blights
  D7 hosts life, valence neutral/positive      ->  wastelands, dead zones, blight, cursed & UNDEAD lands
  D8 arose without intent (natural)            ->  engineered / terraformed / cultivated / ruined biomes
  D9 made of ordinary matter                   ->  informational biomes (memory, sound, luck, forgetting)
```

Everything below is either a way to **read** this table (the strangeness
scale), a way to **generate combinatorially** from it (the matrix, the fluid
engine, the cross-breeder, the halo engine), a **catalogue** of what it yields
(the bestiary), or the **time dimension** of it (the palimpsest state-machine).

---

## Movement I — The Strangeness Scale

*(abstraction-lift + negation, rendered as a scale; dimensions: purpose, size,
cyclicity)*

**1. The strangeness axis (master ordering).**
Give every biome a scalar *strangeness* position, 0 → 100, defined by how many
defaults it negates and how far. This is a real, sortable field — it drives how
rare a biome is, how deep/remote it generates, how much a traveller reacts, and
how the prose registers ("unremarkable heath" vs "you should not be seeing
this"). The rungs:

```
  0   MUNDANE        the common 22 + ordinary additions
  15  EXTREME        a real parameter pushed to its physical limit (the deserts, the salt flats)
  30  EXOTIC         a default energy/substrate swapped but still natural (chemo-vents, sand-seas)
  45  RELICT         out of equilibrium: scars, blooms, biomes caught mid-transition
  60  ENGINEERED     made or maintained by a will (terraces, polders, ruin-ecologies, war-forests)
  75  AETHERIC       powered/shaped by a phenomenon field (mana-scars, godsblood fens)
  90  FAERIE         the governing law itself is local and unstable (non-Euclidean, time-skewed)
  --  NEGATIVE WING  crosses all rungs: curse / blight / undeath / wasteland (see idea 2)
```

Most worlds cluster near 0; strangeness is a rarity budget spent sparingly, so
the strange stays strange. (Purpose dimension: rungs 0–45 are *purposeless*,
60 is *purposed*, the negative wing is *anti-purposed* — actively hostile.)

**2. The negative wing (negate *life & benevolence*, D7).**
Curse, blight, undeath, and wasteland are not points on the fertility axis —
they are **off it**, a separate wing reached by negating that a biome hosts life
with neutral valence. They deserve their own generator because they behave
differently from every "positive" biome: they *spread*, they *resist recovery*,
and they have a *source*. A negative biome is `(a source) + (a spread field) +
(a corruption of whatever biome it overwrote)`. Undeath specifically negates D1
too — it is **powered by death**, a necrotic energy source, so an undead fen is
literally a chemosynthetic biome whose substrate is corpses. (See the
state-machine, Movement IV, for why these are *sticky*.)

**3. Engineered biomes = natural biome + purpose + maintainer (D8).**
Any natural biome becomes an artificial one by imposing a *purpose* and a
*maintainer* (a civilization, a cult, a single wizard). Terraced rice on a
drowned hillside, a polder drained below sea level, a sacred grove, a war-forest
planted as a wall, a mine's tailings, an arcology's climate garden. The crucial
generative twist: **when the maintainer dies, the biome does not revert — it
decays**, through ruin-ecology stages, back toward its natural state, leaving
readable traces. Engineered biomes are thus *born already coupled to the
palimpsest*. (Autonomy dimension: externally-driven → the maintainer's death
flips it to self-driven decay.)

**4. Ephemeral & periodic biomes (cyclicity).**
Negate "persists over a human life": biomes that exist only *sometimes*. A
vernal-pool biome that is meadow half the year and pond the other; a monsoon
wetland; a super-bloom desert that flowers for one week a decade; a tidal biome
that only exists between the tides; a biome that manifests **only under a
specific moon-phase** (Hornvale has real, multi-moon astronomy — this is free).
The same cell is two or three entirely different biomes depending on *when* you
stand in it. Enormous variety from the time axis alone.

---

## Movement II — The Generating Engines

*(dimension-identification + combination, rendered as a matrix; dimensions:
distribution, materiality, connectivity)*

The anti-oatmeal payoff is combinatorial. These four mechanisms *manufacture*
biomes rather than listing them.

**5. The biome generator matrix (the core engine).**
Identify the fundamental biome dimensions and treat a biome as a **cell in their
cross-product**, not an entry in a table:

```
  Energy       x  Substrate    x  Dominant       x  Governing      x  Fluid
  source          material        kingdom           law               medium
  -----------     -----------     ------------      -----------       ----------
  sunlight        rock/soil       plants            ordinary          water
  geothermal      sand            fungi             aetheric          air/fog
  chemical        ice             animals           faerie/unstable   brine
  radiant/mana    salt            microbes/mat      cursed/willful    tar/oil
  necrotic        crystal         crystal/mineral   dream-logic       ash/dust
  (a phenomenon)  bone/chitin     none (sterile)    entropic          glass/silica
```

`6 x 6 x 6 x 6 x 6 = 7,776` biome cells, most unnamed, many coherent, some
absurd (absurd cells are *information* — they reveal which axes clash). Rarity
is not a table: it is **the joint improbability of a cell's coordinates**
(necrotic + crystal + fungi + cursed + tar is astronomically rare, so it
generates once per thousand worlds in the deepest, most warded place). This is
the biome-scale expression of Cycle 01's `M^N`. (Combination operator, made the
literal architecture.)

**6. Isolation → endemism (connectivity).**
A cell cut off from its kind — an island, a sky-island tepui, a sealed cave
pocket, a spring in a dune sea — runs a **divergent generation seed**: its biota
drifts from the mainland draw, producing endemic, found-nowhere-else variants.
Connectivity, taken to *isolated*, is a rarity-and-wonder engine for free:
the strangest natural things live where nothing else could reach to homogenize
them. (Distribution dimension: these are *fully concentrated* — one patch,
unique.)

**7. Informational biomes (materiality, D9).**
Negate "made of ordinary matter": a biome whose *medium* is not physical. A
biome of **memory** (where the palimpsest is so dense the past is perceptible),
of **sound** (a resonant canyon that is a different biome to the ear than the
eye), of **luck**, of **forgetting** (a vale where names and histories dissolve),
of **silence**. These sit at the aetheric/faerie rungs and are the bridge
between "biome" and "phenomenon" — they are what a phenomenon field looks like
when it is dense enough to *be* the terrain.

**8. Distribution patterns (mega / patch / mosaic).**
The same biome *kind* reads differently by how it is distributed: one continental
**expanse** (a whole tundra), a scatter of rare **patches** (oases in a reg), or
a fine **mosaic** interwoven with a neighbor at room scale (a fen-and-hummock
braid). Distribution is an independent axis — it multiplies the felt variety of
every biome in the catalogue without adding a single new biome type.

---

## Movement III — The Bestiary Engines

*(substitution + combination, rendered as a dictionary; dimensions:
intentionality, longevity, side-effect)*

Three *substitution/combination* engines that spray named biomes, plus the
catalogue they fill (Appendix A).

**9. The fluid-substitution engine (aquatic, and its dark cousins).**
Take the aquatic template — *a body of fluid, with a surface, a depth, currents,
and a shore* — and **substitute the fluid**. Water gives the ordinary aquatic
biomes (don't neglect them: see the aquatic section of Appendix A). Substitute
the fluid and the whole structure re-instantiates: a **sea of sand** (dunes with
tides and currents), of **fog** (a drowned-in-cloud forest you swim through), of
**tar**, of **glass** (obsidian flats), of **ash**, of **brine**, of **blood**,
of **souls**, of **mercury**, of **milk-pale bioluminescence**. One template ×
a dozen fluids = a dozen exotic biomes that already have coastlines, depths, and
shipping. (Substitution, at its most generative.)

**10. The biome cross-breeder (combination).**
Mash any two biomes and articulate the composite: fungal × tundra →
**frost-mycelium barrens**; reef × cave → **sunless coral vault**; desert ×
faerie → **the mirage-lands that are real where you don't look**; grassland ×
undeath → **the bone-savanna where the herds are skeletons**. The cross-product
of the bestiary with itself is thousands of hybrids; generate them exactly where
two biome fields already overlap (Cycle 01 #10), so the hybrid is *earned* by
the geography, not sprinkled.

**11. Halo biomes (side-effect).**
The highest-leverage combinatorial move: **every major feature radiates its own
biome**. A biome that exists *as the side effect* of something else, attached to
it and generated automatically: the **slag-lands** downwind of a volcano, the
**salt ring** around a drying sea, the **blight halo** around a cursed ruin, the
**fertile crescent** below a dying glacier, the **ghost-fen** where a battle's
dead were never buried, the **gilded rot** downstream of an alchemist's tower,
the **petrified verge** around a basilisk's lair. Features already generate
(Cycle 01 #12/#14); giving each a *halo* means strangeness clusters
meaningfully — you can *smell the volcano before you see it* — instead of
scattering at random. (Side-effect dimension, promoted to a first-class
generator. Longevity varies: some halos are permanent, some heal once the source
is dealt with — a quest reward you can *see* in the landscape.)

---

## Movement IV — The Palimpsest as a State Machine

*(organon-construction + dimension-identification, rendered as a state-machine;
dimensions: animacy, age, decomposability)*

This is the deep-dive you asked for. A place is not a biome — it is the
**current state of a machine that has been running since the world's genesis**,
and Hornvale's deep-time sim already walks that machine. Modelling it explicitly
is where palimpsest stops being flavour and becomes an engine.

**12. The place state-machine.**
Each cell occupies a *biome-state*; **transitions are geological, climatic, and
magical events** — uplift, drowning, glaciation, desertification, burning,
draining, *cursing*, *hallowing*. The deep-time record is the trace of the path
this cell walked from genesis to now. The present room = *current state* +
*legible residue of the whole path*. Drawing the machine (rather than just
naming today's biome) is what unlocks everything below.

**13. The readable stratum stack (palimpsest proper; decomposability).**
The present room carries a **decomposable stack**, one readable trace per past
state: a raised beach 80 m up, a coral reef fossilised into a hillside, a
charcoal horizon from an ancient fire, a salt crust from a vanished sea, a
buried wall from a fallen town. Reading the stack **is** archaeology-as-play — the
surveyor/historian lenses of Cycle 01 #2 given real strata to find. A room's
depth of history becomes a searchable, rewarding surface.

**14. Path-dependence: the present is not enough (the jackpot).**
Two cells that are both "temperate forest" *today* must read **differently**
because they arrived by different histories:

```
  forest-on-drowned-seabed  ->  chalky soil, seashells in the loam, fossil cliffs, sweet springs
  forest-on-old-lava        ->  black earth, basalt tors, iron-red streams, obsidian underfoot
  forest-reclaiming-ruins   ->  orchard-gone-wild, walls under moss, a paved road to nowhere
  forest-over-a-battlefield ->  unnaturally rich growth, bone in the plough, a wrongness at dusk
```

The **path is content.** This multiplies apparent biome variety by the number of
distinct histories that can lead to the same present state — *without inventing a
single new biome*. It is the single most efficient answer to the oatmeal problem
in either cycle: variety mined from time already simulated.

**15. Sticky & absorbing states (why curses persist; age).**
The state-machine's power is **trigger asymmetry**. Some states are *sticky* —
easy to enter, glacially slow to leave: a curse falls in a day and decays over
centuries, its blight-halo shrinking one ring per generation. Some are
*absorbing* — salted earth, a dead god's wound, an undeath that will not lift
without a specific in-world act. This is the mechanism that makes wastelands and
cursed lands *feel permanent and consequential* rather than cosmetic, and it
turns "cleanse the blight" into a real, legible state-transition the player can
drive. (Age dimension: the age of the *current* state is readable — a young scar
is raw, an ancient one has its own weathered ecology of things that adapted to
it.)

**16. Magical & cursed succession (the strange clock).**
Cycle 01 #7 gave biomes a natural successional clock. Extend it into the strange:
a blighted land **heals** through visible stages (blight → ash → lichen →
pioneer → recovered, over centuries), and — the mirror — a normal land **turns**
if a phenomenon or curse persists long enough (a slow creep from healthy →
uneasy → tainted → lost). The clock runs in both directions on the strangeness
axis, so the map is never static: strangeness *advances and recedes*, and a
returning traveller finds the front has moved.

**17. Living biomes (animacy).**
Negate "a biome is an inert backdrop": a biome that behaves like an **organism**
— it grows, heals its wounds, hungers, migrates, or *responds to intruders*. A
forest that closes its paths behind you; a mire that digests; a coral-city that
rebuilds what you break; a fungal kingdom that is one connected mind. Quasi-alive
biomes are both a strangeness rung and a superb encounter engine — the *biome
itself* is the antagonist or ally.

**18. Wandering biomes (negate D6, spatial fixity).**
The top of the strangeness scale: biomes that **move**. A migrating super-bloom,
a drifting fog-forest, a roving blight that eats what it crosses, a faerie realm
that opens in a different valley each season. The biome's *location* is a
time-varying field — `position = f(seed, time)` — so determinism holds perfectly
while the map refuses to sit still. A hunted, rumoured, never-where-you-left-it
kind of wonder.

**19. Radial purity gradient.**
Every strange biome has a **core → margin** structure: the pure heart is small,
rare, and intense (the true faerie, the blight's black center, the deepest
aetheric bloom); the margins are diluted and hybrid, shading into the ordinary
around them. One biome patch thus contains its *own* internal gradient of
strangeness and danger — you feel it intensify as you press inward — which gives
even a single patch a traversal arc.

**20. The reversion ladder (engineered → wild, closing the loop).**
Ties #3 and the palimpsest together into a reusable ladder every engineered or
strange biome descends once its maintainer or source is gone: *maintained →
neglected → feral → ruin-ecology → reclaimed → wild (with traces)*. The same
ladder serves terraces going to jungle, a drained polder re-flooding, a cursed
tower's halo receding, a war-forest outliving its war. One state-transition
grammar, reused across the whole strangeness range — the composability principle
of Cycle 01, applied to time.

---

## Appendix A — The Bestiary (going wild, as requested)

Names are coinable placeholders; definitions are genus-and-differentia. Strange­
ness rung in brackets. Many fall out of the matrix (#5), the fluid engine (#9),
the cross-breeder (#10), or the halo engine (#11) rather than needing bespoke
authoring.

```
AQUATIC — freshwater                                              [0-15]
  Chalk stream        clear alkaline brook over white gravel, cress-choked
  Blackwater swamp    tannin-dark, acidic, slow; drowned hardwoods
  Oxbow marsh         cut-off river bend going to reed and silt
  Tarn                cold high mountain pool in a glacial cirque
  Soda / alkali lake  hypersaline evaporite lake, flamingo-bright microbial mats
  Sinkhole cenote     drowned karst shaft, blue and bottomless
  Subglacial lake     sealed water under ice, sunless, ancient

AQUATIC — marine (extends Cycle 01's set)                         [15-45]
  Anoxic dead-sea     stratified, sulfurous, lifeless below the pycnocline
  Brine-pool lake     a denser-than-seawater "lake" ON the seafloor, with a shore
  Methane-seep garden chemosynthetic mussels & worms over cold hydrocarbon vents
  Whale-fall grove    a decades-long ecosystem blooming on one sunken carcass
  Milky sea           square miles of steady bioluminescence, bright enough to read by
  Sargasso mat        a floating forest of weed, its own drifting continent
  Kelp cathedral      old-growth kelp in cold clear water, cavernous and columned

FLUID-SUBSTITUTION seas (engine #9)                               [30-90]
  Sand sea (erg)      dunes with tides, currents, and drowning
  Fog sea             a valley brim-full of cloud you wade and drown in
  Tar pit barrens     slow black lakes that swallow and preserve
  Glass flats         obsidian sheets from an ancient melt, a frozen sea of black mirror
  Ashen sea           bottomless drifts of volcanic ash, dune-formed, choking
  Quicksilver mere    a mercury pool under a stone sky [aetheric]
  Sea of souls        a faerie "water" of drifting lights that were people [faerie]

DESERT varieties (the EXTREME rung)                               [15-30]
  Erg                 sand sea, dune-formed
  Reg / serir         flat gravel desert, wind-swept pavement
  Hamada              bare rock plateau, stripped to stone
  Yardang field       wind-carved ridges, a fleet of stone hulls
  Playa / salt pan     cracked evaporite floor of a dead lake
  Nabkha field        coppice dunes anchored on hardy shrubs
  Fog / coastal desert lifeless inland, lush with lichen where the sea-fog reaches
  Cold desert          rain-shadow steppe, freezing and arid at once
  Gypsum dunes         blinding white, cool underfoot

FUNGAL & non-plant kingdoms (matrix #5, D3)                       [30-60]
  Giant-fungus forest  toadstool canopy, spore-lit, over rotting deadfall
  Myconid grove        a fungal biome that is one connected, half-aware mind [living]
  Mycelium barrens     surface bare, the whole cell one underground organism
  Lichen steppe        rock-crust ecology on a world too harsh for roots
  Crystal garden       silica/quartz "flora" grown from mineral-rich seeps
  Stromatolite shore   living rock built by microbial mats, oldest life on the world

ENGINEERED / ARTIFICIAL (rung 60, D8)                            [60]
  Terrace-lands        stepped hillsides, sculpted for water and grain
  Polder               dry land held BELOW sea level by will and dyke
  War-forest           a wall of planted trees, ranked and thorned
  Ruin-ecology         a fallen town's stone re-colonised: wall-ferns, feral orchards
  Tailings barrens     poisoned spoil-heaps of an old mine, metal-tolerant weeds
  Sacred grove         a maintained, warded stand around a holy thing
  Feral arcology       a climate-garden gone wild inside a dead megastructure

AETHERIC / MAGICAL (rung 75, D1/D4)                              [75]
  Mana-scar            land where a phenomenon burned through; reality thin, glowing
  Godsblood fen        a mire fed by a dying god's wound, unnaturally fertile & wrong
  Resonant void        a canyon that is a different biome to the ear than the eye [informational]
  Aurora tundra        polar waste under permanent aetheric light, cold-blooming
  Memory-mire          palimpsest so dense the past is visible, walkable [informational]
  Lumen bloom          self-lighting flora over an aether seep, a garden that is its own sun

FAERIE / IMPOSSIBLE (rung 90, D4/D6)                             [90]
  Non-Euclidean glade  a wood whose paths don't compose; you leave by a door you didn't enter
  Time-skewed dell     a valley running fast or slow; an afternoon in, a decade out
  Dream-logic moor     terrain that reorders by association, not geography
  The Uncounted Vale   a faerie realm in a different valley each season [wandering]
  Mirror-lands         real only where unobserved; a desert of things seen from the corner of the eye

NEGATIVE WING — curse / blight / undeath / wasteland (D7)        [neg]
  Blightland           a spreading corruption of whatever biome it overwrote, with a source
  Salt-sown waste      absorbing state: earth killed on purpose, nothing returns for ages
  Ashen wastes         post-cataclysm sterile dust, slowly relichening at the edges
  Bone-savanna         grassland where the herds are skeletons; necrotic-powered [undeath]
  Ghost-fen            a battlefield's unburied dead as a chemosynthetic-of-corpses mire
  Wight-mire           undead wetland; drowned dead walking, cold and lightless
  Petrified verge      the stone-halo around something that turns flesh to rock
  Gilded rot           an alchemist's downstream: beautiful, metallic, lethally poisoned
```

## Appendix B — How this cycle plugs into Cycle 01

- **Biome** is one layer of the stratum stack (C1 #1). This cycle makes that
  layer a *cross-product cell* (#5) rather than an enum pick.
- **Strangeness** (#1) is a new field that feeds rarity, generation depth, and
  prose register — a global dial on how weird a world runs.
- **Palimpsest** (C1 #5) becomes the place state-machine (#12–16): path-dependence
  (#14) is where the biggest, cheapest variety win lives.
- **Halo biomes** (#11) give the feature system (C1 #12/#14) a way to make
  strangeness *cluster* legibly instead of scattering.
- The **negative wing** and **sticky states** (#2/#15) are what let cursed/undead
  lands persist, spread, and be *cured* as real state-transitions — future quest
  and religion hooks.

## Next cycles

Candidate themes: **the strangeness budget** (how a world spends rarity so the
strange stays rare); **the place state-machine formalism** (states, triggers,
and the deep-time walk that drives it); **inhabitants & culture as biome
responses** (who lives in a blightland, who worships a mana-scar); and a pass
that re-runs the picker for the operators this cycle under-used. Before any of
these graduate to a spec, sort the mechanisms into the frontier idea-registry
with confidence scores.
