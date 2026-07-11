# Room-Scale Variety — Cycle 03 (The Living Layer)

**Status:** Ideation. Not yet spec, not yet plan. Feeds future campaigns.
**Method:** ideonomy, five fresh method tuples (organons: map, notation, tree,
periodic-grid, lattice). Cycles 01–02 built the *stage* — rooms, biomes, the
deep-time history under them. This cycle populates it: **who and what inhabits a
room, how they are generated combinatorially, how they relate, and what
happens** when you arrive. The anti-oatmeal target moves up a level: even varied
terrain feels dead without varied *life, stakes, and consequence*.
**Builds on:** [Cycle 01](cycle-01-ideonomy.md) (halos #11, secret rooms #20,
trophic webs #19, phase #8) and [Cycle 02](cycle-02-biomes-and-palimpsest.md)
(strangeness, the negative wing, the palimpsest state-machine). Hornvale already
generates settlements, religions, phonologies, and coarse fauna — this cycle is
the connective tissue that makes them *live in the rooms*.

---

## The thesis: life is a stack too, and it must remember

The stratum-stack logic (C1 #1) applies to inhabitants exactly as it did to
terrain: an individual, an encounter, a culture is a **composition of orthogonal
layers**, so variety is multiplicative. But the living layer adds two demands the
terrain layer didn't have:

1. **Placement, not sprinkling.** Life must *belong* where it is found — derived
   from biome, feature, and history — so the world reads as an ecology and a
   society, not a random-encounter table. (The Map, Movement I.)
2. **Persistence.** A world where the slain beast respawns and the burned grove
   is green next dawn is oatmeal no matter how varied each bowl. Consequence must
   be written to the ledger and *stay* written. (Reversibility, Movement II.)

Everything below serves those two demands with combinatorial engines.

---

## Movement I — The Map of the Living

*(substitution + organon-construction, rendered as a map; dimensions:
complexity, modularity, visibility)*

Position carries information: *who* is here is a function of *where* here is.

**1. Range & territory fields.**
Every creature and people has a **home-range field** with the radial core→margin
structure of Cycle 02 #19: dense and confident at the center of its range,
thinning to nothing at the edges. You are always standing inside *someone's*
range (often several, overlapping). Encounter likelihood and disposition are read
from range membership — you are a trespasser near a core, a curiosity at a
margin. This replaces the flat encounter table with a **living geography of
claims**.

**2. Placed, not scattered (the map's cities and badlands).**
Inhabitants derive lawfully from biome + feature + history, so life *clusters
where it should*: ecotones, oases, reefs, and ruins are **cities of life** (high
density, high diversity); deep desert, blight, and abyss are **badlands** (sparse
— or sparse-but-deadly). The map of life has the same geography as the map of
terrain because one produces the other. A player learns to read the land for what
lives in it.

**3. Frontier zones.**
Where two ranges or two peoples' territories overlap is the richest encounter
ground — raids, trade, tension, hybrids, treaties, feuds. This is Cycle 01's
ecotone idea (#3) re-instantiated for the living layer: the border *between*
claims generates more story than either claim's interior. Deliberately seed
conflict and exchange at range-overlaps.

**4. Visibility: the empty-looking room (modularity + visibility).**
Inhabitants carry a **concealment field** (Cycle 01 #20 applied to life):
obvious → lurking → ambushing → hidden-until-triggered. A room that reads as
empty may be densely inhabited invisibly — revealed by searching, tracking, a
sound, a mistake. Predators ambush; prey hides; cults meet in secret; the dead
wait. Absence of visible life is not absence of life.

**5. Modular individuals (complexity).**
An individual is a stack: *species × role × temperament × condition × appearance
× quirk × personal history*. Most instances are **simple** (a wary deer, a surly
toll-keeper) and cheap; a rare few are **hyper-complex** named figures with deep
histories drawn from the palimpsest. Same multiplicative engine as rooms —
adding one trait-axis multiplies the whole cast.

---

## Movement II — The Notation of Encounters

*(dimension-identification + combination, rendered as a notation; dimensions:
reversibility, cardinality, scope)*

Creatures and encounters get an explicit **grammar**. A notation forces every
required slot to be declared — and the unfilled slots are exactly the ambiguities
a vaguer "spawn a monster" would hide.

**6. The creature grammar.**
A body-plan notation whose slots are all *consequential* (they change how the
thing fights, feeds, and whether it can be reasoned with):

```
  CREATURE := chassis   x  size     x  locomotion  x  diet
            x armament  x  senses   x  sociality   x  habitat-binding
            [ x graft ... ]                          [ x strangeness-rung ]
```

The cross-product is a vast, *lawful* bestiary — and `graft` (the combination
operator) splices two lineages into a chimera **only where two ranges overlap**,
so monsters are earned by geography, not sprayed. Because the axes are
consequential, two creatures that differ on one slot (pack-hunter vs
solitary-ambusher, same everything else) play completely differently. This is the
anti-oatmeal defense at the level of the bestiary.

**7. The encounter notation.**
A room's current event is not "a wolf is here" but a four-slot sentence:

```
  ENCOUNTER := AGENT + STATE + ACTIVITY + STAKE
  e.g.  [starving wolf-pack] + [desperate] + [cornering a wounded elk]
        + [at a contested ford the clans both claim]
```

Each slot is drawn from context (agent from the range field, state from the
trophic clock and season, activity from agent+stake, stake from the room's
features and factions). The *same three creatures* generate a hundred distinct
scenes because the frame around them varies. Encounters are **grammatical, not
tabular**.

**8. Reversibility & persistence.**
Every consequence is a **ledger fact** with a reversibility property: a slain
beast, a razed grove, a freed captive, an insulted lord are **mostly
irreversible** and stay written into the world (this is what makes it a place,
not a ride). A minority are **reversible** — herds regrow via the trophic clock,
seasons turn, feuds cool. Reversibility is per-consequence, and the irreversible
ones are where the weight of the world lives.

**9. Cardinality & scope: the singular and the swarm.**
One creature-spec at *cardinality = one* is a **named legendary beast** —
unique, story-bearing, its range a rumor; the *same spec* at *cardinality = very
many* is a **swarm or herd**, a mass hazard with no individuality. Orthogonally,
*scope* sets the shadow it casts: a local nuisance, a regional menace, a
world-threat whose range is a whole province. One notation, four wildly
different encounters.

---

## Movement III — The Tree of the Living (and the Unalive, and the Made)

*(negation + cross-domain re-instantiation, rendered as a tree; dimensions:
source, autonomy, materiality)*

Walk the taxonomy of inhabitants as a well-formed tree — then **negate its root
assumption** ("an inhabitant is a living creature with a will of its own") and a
whole shadow-forest appears.

**10. The inhabitant taxonomy.**
A well-formed tree (Appendix A) with mutually-exclusive branches: *fauna /
peoples / beasts / flora-that-acts / undead / constructs / spirits / fae*. The
discipline surfaces **empty branches** — kinds of inhabitant we haven't imagined
— and keeps generation rules per branch clean and separable.

**11. Negate the living NPC → the present-but-unalive wing.**
Negating *alive* and *willful* opens the branches that occupy rooms without being
"characters": **ghosts, revenants, echoes** (dead but present — ties to C2's
negative wing), **automata and golems** (present but will-less), **the mad and
the mute** (present, will-bearing, but not conversable), **the hive/collective**
(present, willful, but no individual). Each needs its *own interaction verbs* —
lay-to-rest, banish, decipher, dismantle, appease — not "talk / fight."

**12. Autonomy → thralls, puppets, and their sources.**
Negate *self-driven*: inhabitants moved by an external will — undead thralls,
possessed beasts, bound elementals, enchanted guardians. Their behavior **points
back to a source** (a necromancer, an artifact, a curse from C2 #15). This is a
quest structure for free: kill the puppet or find the puppeteer? The *source*
dimension turns a fight into an investigation.

**13. Cross-domain: institutions as inhabitants.**
Re-instantiate sociology — a room can be "inhabited" by an **institution's
trace** rather than a creature: a toll-post, a shrine with a tending cult, a
bandit tithe, a monster's *lair economy*, a smugglers' cache with rules. The
inhabitant is a **social process** with recurring, rule-bound interactions —
richer and more re-visitable than any single NPC. (Materiality: these are
*embodied-but-abstract* inhabitants.)

---

## Movement IV — The Periodic Grid of Encounters

*(abstraction-lift + cross-domain re-instantiation, rendered as a periodic-grid;
dimensions: naturalness, homogeneity, side-effect)*

Lift "encounter" and "fauna" to their structural shape and lay them on a
**saturated grid** where *empty cells are predictions* — the Mendeleev move. This
turns content generation into a **completeness audit**: the grid tells you what
should exist and doesn't.

**14. The encounter periodic table.**
Two exhausting axes — *agent disposition* × *encounter frame* (Appendix B) —
enumerate the space of what-can-happen. Every filled cell is a recognizable
scene; **every empty cell is an encounter type we owe the world.** Naturalness
splits each cell (the natural vs. contrived version). The grid is a design tool,
not just content: it makes the *gaps in our imagination* visible and fillable.

**15. Biome → life as a periodic law.**
As the periodic table predicts an element from its coordinates, a **biome's
parameters predict its characteristic life**: every biome should field a top
predator, a keystone forager, a decomposer, a people (or its ruins), a menace.
Lay out biome × ecological-niche and fill it; an empty cell says *"this biome has
no apex predator — invent one from its parameters."* Strangeness is periodic too:
an aetheric biome fields aetheric life, a blightland fields blight-things. This
is anti-oatmeal by **systematic coverage** rather than random sprinkling — no
biome is left generically half-populated.

**16. Side-effect inhabitants (life-halos).**
Cycle 02's halo biomes (#11), re-instantiated for the living: **every feature
radiates a cast, not just a biome.** A ruin fields its haunt + its scavengers +
its treasure-guardian + its would-be looters; a mana-scar fields its aberrations
+ its pilgrim-cult; a battlefield fields its ghosts + its carrion-things + its
grave-robbers. Features already generate (C1 #12/#14); giving each a *cast* means
life clusters meaningfully around the world's landmarks. (Side-effect dimension,
promoted to a caster of inhabitants.)

---

## Movement V — The Lattice of Belonging

*(tree-finding + combination, rendered as a lattice; dimensions:
hierarchicalness, direction, cyclicity)*

Culture and allegiance are **not trees** — a person belongs to several overlapping
groups at once, with genuine incomparabilities and conflicts. The right organon
is a partial-order lattice, and its *meets and joins* are where relationship
stakes are generated.

**17. Culture as multiple inheritance.**
A people's character is the **join of its parents**: *biome* (subsistence,
material culture), *history/palimpsest* (memory, grievance, myth — from C2),
*neighbors* (borrowing, rivalry), *faith* (values — Hornvale's religion gen).
Two peoples in the *same biome* diverge because their other parents differ. This
is the anti-oatmeal engine for **cultures**, and it slots directly onto the
existing settlement/religion/language generators as their organizing structure.

**18. The allegiance lattice.**
An individual sits in several overlapping memberships — clan, faith, guild,
settlement, region, secret society — a partial order, not a hierarchy. Two
people's relationship is computed from the **meet/join** of their memberships:
shared apex → kin/ally; incomparable → stranger; opposed branches → enemy.
Loyalties can *conflict* (your guild and your clan want different things), which
is a story generator. Relationship stakes fall out of the structure.

**19. The relationship-direction field (feud & alliance dynamics).**
Inter-group relations **accumulate or decay over time** (direction dimension):
alliances deepen, debts compound, feuds fester, grudges cool. Crucially, the
**palimpsest seeds the present** (C2 #14): "this ford is where the two clans bled
each other three generations back," so the grievance is *readable in the
landscape*. Living politics grown from simulated history, not authored backstory.

**20. The calendar of the living (cyclicity).**
The world *does things on a schedule*: festivals, migrations, raiding seasons,
pilgrimages, spawning runs, a monster's waking cycle, floods that move whole
peoples. Keyed to Hornvale's real astronomical and seasonal clocks (C1 #8), so
returning to the same room at a different time finds **different life doing
different things**. This is the temporal-variety engine applied to society — the
map is never the same map twice because the living layer has a clock.

---

## Appendix A — The inhabitant taxonomy (tree)

Well-formed (one relation type: *kind-of*). Empty branches are invitations.

```
INHABITANT
├── fauna            grazers, predators, scavengers, swarms, migrants, beasts-of-burden
├── flora-that-acts  ambush plants, walking fungi, ent-things, spore-minds
├── peoples          settled, nomadic, seafaring, subterranean, exiled, hidden
├── beasts           the outsized & legendary: apex-uniques, chimeras, region-menaces
├── undead           ghosts, revenants, wights, liches, echoes, restless dead
├── constructs       golems, automata, wards, bound-elementals, clockwork
├── spirits          nature-spirits, ancestor-spirits, godlings, phenomenon-avatars
└── fae              tricksters, court-fae, hollow-folk, changelings, the Uncounted
```

## Appendix B — The encounter periodic table (grid)

Rows = agent disposition; columns = encounter frame. Cells name the scene;
blanks are predictions to fill. (One rendering; both axes are extensible.)

```
             PREDATION    TERRITORY    TRADE        PLEA         OMEN         PUZZLE       AMBUSH
  HOSTILE    hunt-you     drive-off    extortion    threat       curse-sign   deadly-trap  waylay
  WARY       stalk        warn-off     hard-bargain  test         ill-portent  wary-riddle  feint
  INDIFFERENT  --         passing      barter       ignore-you    dumb-sign    obstacle     --
  CURIOUS    circle       inspect      novelty-swap  question     wonder       game         --
  FRIENDLY   --           welcome      fair-trade   offer-aid     good-omen    shared-lore  (none)
  NEEDY      desperate    refugee      beg-trade    cry-for-help  bad-omen     dilemma      lure
```

Empty cells (`--`) are the design gaps the grid makes visible — e.g. *indifferent
× predation* ("something hunts nearby, wholly uninterested in you") or *curious ×
ambush* ("a thing that traps out of fascination, not malice") are encounter
types worth inventing.

## Appendix C — Plugs into prior cycles

- **Halos** (C2 #11) now cast *inhabitants* (#16), not just biomes: every feature
  radiates a biome *and* a cast.
- **Palimpsest** (C2 #12–15) seeds *grievances and lore* (#19): simulated history
  becomes living politics and readable-in-the-land memory.
- **Strangeness** (C2 #1) is *periodic in life* (#15): strange biomes field strange
  inhabitants, at matched rungs.
- **Trophic webs** (C1 #19) drive encounter *state* (#7) and the reversible
  regrowth of populations (#8).
- **Secret rooms / phase** (C1 #20/#8) become *hidden inhabitants* (#4) and the
  *calendar of the living* (#20).
- The **existing** settlement/religion/language generators become the *parents* in
  the culture lattice (#17) — this cycle organizes them rather than replacing them.

## Next cycles

Candidate themes: **the goal & deed engine** (what a player/agent *does* — quests,
crafts, and ambitions grown from the world rather than authored); **the economy &
scarcity layer** (trade, value, and want across the map); **knowledge & rumor
propagation** (what is known where, and how truth and legend diverge); and a pass
re-running the picker for under-used operators. Before graduating any mechanism to
a spec, sort the now-sixty ideas across three cycles into the frontier
idea-registry with confidence scores.
